use std::{collections::HashMap, fmt::Display, marker::PhantomData};

use miette::{bail, miette, Diagnostic, ErrReport};
use num_bigint::BigInt;
use num_traits::Zero;
use rustyline::Editor;

use crate::ast::*;

fn mklist(v: &Vec<impl Display>) -> String {
    match v.len() {
        0 => "none".to_string(),
        1 => v[0].to_string(),
        _ => {
            use std::fmt::Write;

            let mut ret = "one of ".to_string();
            for item in &v[..v.len() - 1] {
                write!(&mut ret, "{item}, ").unwrap();
            }
            write!(&mut ret, "and {}", v.last().unwrap()).unwrap();
            ret
        }
    }
}

#[derive(thiserror::Error, Diagnostic, Debug)]
#[error("Unknown variable '{}'", .name)]
pub struct UnknownVariable {
    #[label("'{}' seen here", .name)]
    span: Location,
    name: String,
}

#[derive(thiserror::Error, Diagnostic, Debug)]
#[error("Type error; found {} but expected {}", .found, mklist(.expected))]
pub struct TypeError {
    #[label("Expected {}, found {}", mklist(.expected), .found)]
    span: Location,
    found: String,
    expected: Vec<String>,
}

#[derive(thiserror::Error, Diagnostic, Debug)]
#[error("Division by zero")]
pub struct DivByZero {
    #[label("This was zero")]
    span: Location,
}

type Builtin =
    for<'a, 'b> fn(&mut Executor, Vec<Value<'a, 'b>>) -> Result<Value<'a, 'b>, miette::Report>;

#[derive(Clone)]
pub enum Value<'a, 'b> {
    Nil,
    Integer(BigInt),
    Lambda(&'b Expression<'a>),
    Builtin(&'a str),
}

impl<'a, 'b> Value<'a, 'b> {
    fn typename(&self) -> &'static str {
        match self {
            Value::Nil => "Nil",
            Value::Integer(_) => "Integer",
            Value::Lambda(_) | Value::Builtin(_) => "Function",
        }
    }
}

impl<'a, 'b> Display for Value<'a, 'b> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Integer(v) => write!(f, "{v}"),
            Value::Lambda(v) => write!(f, "<function>"),
            Value::Builtin(v) => write!(f, "<function>"),
        }
    }
}

pub struct Executor<'a, 'b> {
    builtin_functions: HashMap<String, Builtin>,
    editor: Editor<()>,
    args: Vec<HashMap<String, Value<'a, 'b>>>,
    vars: Vec<HashMap<String, &'b Expression<'a>>>,
}

fn builtin_input<'a, 'b>(
    executor: &mut Executor,
    args: Vec<Value<'a, 'b>>,
) -> miette::Result<Value<'a, 'b>> {
    if let Ok(v) = executor.editor.readline("input: ") {
        executor.editor.add_history_entry(&v);
        Ok(Value::Integer(v.parse().unwrap()))
    } else {
        panic!();
    }
}

fn builtin_print<'a, 'b>(
    executor: &mut Executor,
    args: Vec<Value<'a, 'b>>,
) -> miette::Result<Value<'a, 'b>> {
    if let Some(arg) = args.first() {
        print!("{arg}");
    }
    for arg in args.iter().skip(1) {
        print!(" {arg}");
    }
    println!();
    Ok(Value::Nil)
}

impl<'a, 'b> Executor<'a, 'b> {
    pub fn new() -> Self {
        Self {
            builtin_functions: HashMap::from_iter([
                ("input".to_string(), builtin_input as Builtin),
                ("print".to_string(), builtin_print),
            ]),
            editor: Editor::<()>::new().unwrap(),
            args: vec![],
            vars: vec![],
        }
    }

    fn call_builtin(
        &mut self,
        name: &str,
        args: Vec<Value<'a, 'b>>,
    ) -> miette::Result<Value<'a, 'b>> {
        self.builtin_functions.get(name).unwrap()(self, args)
    }

    pub fn execute(&mut self, expr: &'b Expression<'a>) -> miette::Result<Value<'a, 'b>> {
        let mut pop_vars = false;
        if let Some(where_clause) = &expr.where_clause {
            pop_vars = true;
            self.vars.push(HashMap::new());
            let map = self.vars.last_mut().unwrap();
            for s in &where_clause.stmnts {
                let StatementKind::Assignment { name, expr, .. } = &s.kind;
                map.insert(name.value.to_string(), expr);
            }
        }

        let ret = try {
            match &expr.kind {
                ExpressionKind::Nil(_) => Value::Nil,
                ExpressionKind::Literal(v) => Value::Integer(v.value.clone()),
                ExpressionKind::Variable(name) => {
                    let mut val = None;
                    for varmap in self.vars.iter().rev() {
                        if let Some(expr) = varmap.get(name.value) {
                            val = Some(self.execute(expr));
                            break;
                        }
                    }
                    if val.is_none() && self.builtin_functions.contains_key(name.value) {
                        val = Some(Ok(Value::Builtin(name.value)));
                    }
                    if val.is_none() {
                        for map in self.args.iter().rev() {
                            if let Some(arg) = map.get(name.value) {
                                val = Some(Ok(arg.clone()));
                                break;
                            }
                        }
                    }
                    val.unwrap_or_else(|| {
                        Err(UnknownVariable {
                            span: name.loc,
                            name: name.value.to_string(),
                        }
                        .into())
                    })?
                }
                ExpressionKind::Lambda { .. } => Value::Lambda(expr),
                ExpressionKind::Unary { op, expr } => match op.value {
                    UnaryOp::Neg => match self.execute(expr)? {
                        Value::Integer(v) => Value::Integer(-v),
                        v @ Value::Nil | v @ Value::Lambda(_) | v @ Value::Builtin(_) => {
                            Err(TypeError {
                                span: expr.loc(),
                                found: v.typename().to_string(),
                                expected: vec!["Integer".to_string()],
                            })?
                        }
                    },
                },
                ExpressionKind::Binary { left, op, right } => {
                    let l = self.execute(left)?;
                    let r = self.execute(right)?;
                    match (l, r, &op.value) {
                        (Value::Integer(l), Value::Integer(r), BinaryOp::Add) => {
                            Value::Integer(l + r)
                        }
                        (Value::Integer(l), Value::Integer(r), BinaryOp::Sub) => {
                            Value::Integer(l - r)
                        }
                        (Value::Integer(l), Value::Integer(r), BinaryOp::Mul) => {
                            Value::Integer(l * r)
                        }
                        (Value::Integer(l), Value::Integer(r), BinaryOp::Div) => {
                            if r.is_zero() {
                                Err(DivByZero { span: right.loc() })?
                            }
                            Value::Integer(l / r)
                        }
                        (Value::Integer(l), Value::Integer(r), BinaryOp::Mod) => {
                            Value::Integer(l % r)
                        }
                        (Value::Integer(_), r, _) => Err(TypeError {
                            span: right.loc(),
                            found: r.typename().to_string(),
                            expected: vec!["Integer".to_string()],
                        })?,
                        (l, Value::Integer(_), _) => Err(TypeError {
                            span: left.loc(),
                            found: l.typename().to_string(),
                            expected: vec!["Integer".to_string()],
                        })?,
                        (l, r, _) => Err(TypeError {
                            span: left.loc(),
                            found: l.typename().to_string(),
                            expected: vec!["Integer".to_string()],
                        })?,
                    }
                }
                ExpressionKind::FuncCall {
                    func,
                    args,
                    rbracket,
                } => {
                    let f = self.execute(func)?;
                    let args: Vec<_> = args.iter().map(|e| self.execute(e)).try_collect()?;
                    match f {
                        Value::Lambda(e) => {
                            let ExpressionKind::Lambda { params, body, .. } = &e.kind else { unreachable!() };
                            self.args.push(HashMap::new());
                            let map = self.args.last_mut().unwrap();
                            for (param, arg) in params.iter().zip(args.into_iter()) {
                                map.insert(param.value.to_string(), arg);
                            }
                            let ret = self.execute(body);
                            self.args.pop();
                            ret?
                        }
                        Value::Builtin(name) => self.call_builtin(name, args)?,
                        v @ Value::Nil | v @ Value::Integer(_) => Err(TypeError {
                            span: func.loc(),
                            found: v.typename().to_string(),
                            expected: vec!["Function".to_string()],
                        })?,
                    }
                }
                ExpressionKind::If {
                    cond,
                    case_true,
                    case_false,
                    ..
                } => {
                    let cval = self.execute(cond)?;
                    if match cval {
                        Value::Nil => false,
                        Value::Integer(v) => !v.is_zero(),
                        _ => false,
                    } {
                        self.execute(case_true)?
                    } else {
                        self.execute(case_false)?
                    }
                }
                ExpressionKind::Sequence(v) => {
                    let mut last = None;
                    for v in v {
                        last = Some(self.execute(v)?);
                    }
                    last.unwrap()
                }
            }
        };

        if pop_vars {
            self.vars.pop();
        }

        ret
    }
}
