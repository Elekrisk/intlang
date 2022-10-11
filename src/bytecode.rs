pub mod generator;
pub mod parser;

use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
};

use num_bigint::BigInt;
use num_traits::Zero;
use rustyline::Editor;

const BUILTIN_FUNC_COUNT: usize = 2;

/// Represents an entire program.
#[derive(Debug)]
pub struct Program {
    functions: FunctionStorage,
    values: ValueAllocator,
    entry: FunctionId,
}

impl Program {}

#[derive(Debug)]
pub struct FunctionStorage {
    functions: Vec<Function>,
}

type BuiltinFunc = Box<dyn Fn(&mut Editor<()>, Vec<Value>) -> Value>;

impl FunctionStorage {
    fn new() -> Self {
        let builtins: [(usize, BuiltinFunc); BUILTIN_FUNC_COUNT] = [
            (
                1,
                Box::new(|_, args| {
                    if let [arg, ..] = &args[..] {
                        println!("{arg}");
                        Value::Nil
                    } else {
                        panic!();
                    }
                }),
            ),
            (
                0,
                Box::new(|editor, _| {
                    if let Ok(v) = editor.readline("input: ") {
                        Value::Integer(v.parse().unwrap())
                    } else {
                        panic!();
                    }
                }),
            ),
        ];
        Self {
            functions: builtins
                .into_iter()
                .enumerate()
                .map(|(i, (arg_count, body))| Function {
                    id: FunctionId(i),
                    arg_count,
                    var_count: 0,
                    capture_count: 0,
                    body: FunctionBody::Builtin(body),
                })
                .collect(),
        }
    }

    fn add(&mut self, func: Function) {
        assert_eq!(func.id.0, self.functions.len());
        self.functions.push(func);
    }

    fn get(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id.0)
    }
}

#[derive(Debug)]
pub struct ValueAllocator {
    values: Vec<Option<ValueBacking>>,
    deleted_values: Vec<GlobalValueId>,
}

impl ValueAllocator {
    pub fn new() -> Self {
        Self {
            values: vec![],
            deleted_values: vec![],
        }
    }

    fn allocate_value(&mut self) -> GlobalValueId {
        if let Some(v) = self.deleted_values.pop() {
            self.values[v.0] = Some(ValueBacking {
                count: 1,
                links: vec![],
                value: Value::Nil,
                initialized: false,
            });
            v
        } else {
            let v = self.values.len();
            self.values.push(Some(ValueBacking {
                count: 1,
                links: vec![],
                value: Value::Nil,
                initialized: false,
            }));
            GlobalValueId(v)
        }
    }

    fn get(&self, value_id: GlobalValueId) -> &ValueBacking {
        self.values[value_id.0].as_ref().unwrap()
    }

    fn get_mut(&mut self, value_id: GlobalValueId) -> &mut ValueBacking {
        self.values[value_id.0].as_mut().unwrap()
    }

    fn inc_value(&mut self, value_id: GlobalValueId) {
        self.values[value_id.0].as_mut().unwrap().count += 1;
    }

    fn dec_value(&mut self, value_id: GlobalValueId) {
        // println!("Decrementing counter for value {value_id:?}");
        let backing = self.values[value_id.0].as_mut().unwrap();
        backing.count -= 1;
        if backing.count == 0 {
            // println!("Deleting value {value_id:?}");
            for v in backing.links.clone() {
                self.dec_value(v);
            }
            self.values[value_id.0] = None;
            self.deleted_values.push(value_id);
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct FunctionId(usize);

/// Represents a function.
#[derive(Debug)]
pub struct Function {
    /// The ID of this function.
    id: FunctionId,
    /// How many arguments this function takes.
    arg_count: usize,
    /// How many variables this function allocates.
    var_count: usize,
    /// How many variables this function captures.
    capture_count: usize,
    /// The body of the function.
    body: FunctionBody,
}

impl Function {
    /// Creates a stack frame, complete with program counter,
    /// allocated variables and captures.
    fn create_frame(
        &self,
        values: &mut ValueAllocator,
        captures: Vec<GlobalValueId>,
    ) -> FunctionStack {
        let mut stack = FunctionStack {
            function: self.id,
            pc: 0,
            variables: VariableTranslator::new(),
            stack: vec![],
        };
        // Allocate space for arguments.
        // Initialize to Nil.
        for i in 0..self.arg_count {
            let local = LocalValueId(i);
            let global = values.allocate_value();
            stack.variables.set(local, global);
        }
        // Allocate space for variables.
        for i in 0..self.var_count {
            let local = LocalValueId(self.arg_count + i);
            let global = values.allocate_value();
            stack.variables.set(local, global);
        }
        // Allocate space for captures.
        for i in 0..self.capture_count {
            let local = LocalValueId(self.arg_count + self.var_count + i);
            let global = captures[i];
            stack.variables.set(local, global);
            values.inc_value(global);
        }
        stack
    }
}

// #[derive(Debug)]
pub enum FunctionBody {
    Lambda(Vec<Instruction>),
    Builtin(BuiltinFunc),
}

impl Debug for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lambda(arg0) => f.debug_tuple("Lambda").field(arg0).finish(),
            Self::Builtin(_) => f.debug_tuple("Builtin").field(&"<Function>" as _).finish(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LocalValueId(usize);
impl Display for LocalValueId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.0)
    }
}
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct GlobalValueId(usize);
impl Display for GlobalValueId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Integer(BigInt),
    Function {
        id: FunctionId,
        captures: Vec<GlobalValueId>,
    },
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Integer(v) => write!(f, "{v}"),
            Value::Function { id, captures } => {
                write!(f, "Func [{}] {{", id.0)?;
                let mut captures = captures.iter();
                if let Some(v) = captures.next() {
                    write!(f, "{}", v)?;
                }
                for v in captures {
                    write!(f, ", {}", v)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug)]
pub struct ValueBacking {
    count: usize,
    links: Vec<GlobalValueId>,
    value: Value,
    initialized: bool,
}

fn write_interspersed_with<W: Write, T>(
    w: &mut W,
    f: impl Fn(&mut W, T) -> std::fmt::Result,
    a: impl IntoIterator<Item = T>,
) -> std::fmt::Result {
    let mut iter = a.into_iter();
    if let Some(v) = iter.next() {
        (f)(w, v);
    }
    for v in iter {
        write!(w, ", ")?;
        (f)(w, v)?;
    }
    Ok(())
}

fn write_interspersed(
    w: &mut impl Write,
    a: impl IntoIterator<Item = impl Display>,
) -> std::fmt::Result {
    let mut iter = a.into_iter();
    if let Some(v) = iter.next() {
        write!(w, "{v}");
    }
    for v in iter {
        write!(w, ", {v}")?;
    }
    Ok(())
}

#[derive(Debug)]
pub enum Instruction {
    PushNil,
    PushInteger(BigInt),
    PushFunction {
        function: FunctionId,
        /// Which of the current functions variables this function captures
        captures: Vec<LocalValueId>,
    },

    SetVariable(LocalValueId),
    GetVariable(LocalValueId),
    GetGlobal(GlobalValueId),
    Pop,

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    If(isize),
    Jump(isize),
    Call(usize),
    Ret,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::PushNil => write!(f, "PushNil"),
            Instruction::PushInteger(v) => write!(f, "PushInteger Value: {v}"),
            Instruction::PushFunction { function, captures } => {
                write!(f, "PushFunction ID: [{}] Captures: ", function.0)?;
                write_interspersed(f, captures)
            }
            Instruction::SetVariable(v) => write!(f, "SetVariable ID: {v}"),
            Instruction::GetVariable(v) => write!(f, "GetVariable ID: {v}"),
            Instruction::GetGlobal(v) => write!(f, "GetGlobal ID: {v}"),
            Instruction::Pop => write!(f, "Pop"),
            Instruction::Add => write!(f, "Add"),
            Instruction::Sub => write!(f, "Sub"),
            Instruction::Mul => write!(f, "Mul"),
            Instruction::Div => write!(f, "Div"),
            Instruction::Mod => write!(f, "Mod"),
            Instruction::If(offset) => write!(f, "If Offset: {offset}"),
            Instruction::Jump(offset) => write!(f, "Jump Offset: {offset}"),
            Instruction::Call(argc) => write!(f, "Call ArgumentCount: {argc}"),
            Instruction::Ret => write!(f, "Ret"),
        }
    }
}

#[derive(Debug)]
pub struct FunctionStack {
    function: FunctionId,
    pc: usize,
    variables: VariableTranslator,
    stack: Vec<Value>,
}

impl FunctionStack {
    fn write(&self, w: &mut impl Write) -> std::fmt::Result {
        let id = self.function.0;
        let pc = self.pc;
        write!(w, "Stack [{id}] ({pc}) vars: {{")?;
        write_interspersed_with(
            w,
            |w, (l, g)| write!(w, "{l} : {g}"),
            self.variables
                .as_slice()
                .iter()
                .enumerate()
                .map(|(a, b)| (LocalValueId(a), b)),
        )?;
        write!(w, "}} stack: [")?;
        write_interspersed(w, &self.stack)?;
        write!(w, "]")
    }
}

impl Display for FunctionStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write(f)
    }
}

#[derive(Debug)]
pub struct VariableTranslator {
    variables: Vec<GlobalValueId>,
}

impl VariableTranslator {
    fn new() -> Self {
        Self { variables: vec![] }
    }

    fn translate(&self, local: LocalValueId) -> GlobalValueId {
        self.variables[local.0]
    }

    fn set(&mut self, local: LocalValueId, global: GlobalValueId) {
        assert_eq!(local.0, self.variables.len());
        self.variables.push(global);
    }

    fn as_slice(&self) -> &[GlobalValueId] {
        &self.variables
    }
}

pub struct Executor {
    program: Program,
    function_stack: Vec<FunctionStack>,
    editor: Editor<()>,
}

impl Executor {
    pub fn new(mut program: Program) -> Self {
        Self {
            program,
            function_stack: vec![],
            editor: Editor::new().unwrap(),
        }
    }

    pub fn run(&mut self) {
        let f = self.program.functions.get(self.program.entry).unwrap();
        let frame = f.create_frame(&mut self.program.values, vec![]);
        self.function_stack.push(frame);
        let mut cur_frame = self.function_stack.last_mut().unwrap();
        let mut cur_func = f;
        let mut cur_body = match &cur_func.body {
            FunctionBody::Lambda(ins) => ins,
            FunctionBody::Builtin(_) => todo!(),
        };

        loop {
            // println!("  {}", cur_frame);
            // println!();
            let instruction = &&cur_body[cur_frame.pc];
            // println!("{}", instruction);
            cur_frame.pc += 1;
            match instruction {
                Instruction::PushNil => cur_frame.stack.push(Value::Nil),
                Instruction::PushInteger(v) => cur_frame.stack.push(Value::Integer(v.clone())),
                Instruction::PushFunction { function, captures } => {
                    let mut catures = vec![];
                    for id in captures {
                        let global = cur_frame.variables.translate(*id);
                        catures.push(global);
                    }
                    cur_frame.stack.push(Value::Function {
                        id: *function,
                        captures: catures,
                    });
                }
                Instruction::SetVariable(v) => {
                    let value = cur_frame.stack.pop().unwrap();
                    let global = cur_frame.variables.translate(*v);
                    let backing = self.program.values.get_mut(global);
                    assert!(!backing.initialized);
                    backing.initialized = true;
                    if let Value::Function { id, captures } = &value {
                        let captures = captures.clone();
                        backing.links.extend_from_slice(&captures);
                        backing.value = value;
                        for cap in captures {
                            self.program.values.inc_value(cap);
                        }
                    }
                }
                Instruction::GetVariable(v) => {
                    cur_frame.stack.push(
                        self.program
                            .values
                            .get(cur_frame.variables.translate(*v))
                            .value
                            .clone(),
                    );
                }
                Instruction::GetGlobal(v) => {
                    cur_frame
                        .stack
                        .push(self.program.values.get(*v).value.clone());
                }
                Instruction::Pop => {
                    cur_frame.stack.pop().unwrap();
                }
                Instruction::Add
                | Instruction::Sub
                | Instruction::Mul
                | Instruction::Div
                | Instruction::Mod => {
                    let right = cur_frame.stack.pop().unwrap();
                    let left = cur_frame.stack.pop().unwrap();
                    let val = match (left, right) {
                        (Value::Integer(l), Value::Integer(r)) => match instruction {
                            Instruction::Add => Value::Integer(l + r),
                            Instruction::Sub => Value::Integer(l - r),
                            Instruction::Mul => Value::Integer(l * r),
                            Instruction::Div => Value::Integer(l / r),
                            Instruction::Mod => Value::Integer(l % r),
                            _ => unreachable!(),
                        },
                        _ => panic!(),
                    };
                    cur_frame.stack.push(val);
                }
                Instruction::If(offset) => {
                    if match cur_frame.stack.pop().unwrap() {
                        Value::Nil => false,
                        Value::Integer(v) => !v.is_zero(),
                        _ => true,
                    } {
                        cur_frame.pc = cur_frame.pc.saturating_add_signed(*offset);
                    }
                }
                Instruction::Jump(offset) => {
                    cur_frame.pc = cur_frame.pc.saturating_add_signed(*offset);
                }
                Instruction::Call(count) => {
                    let func = cur_frame.stack.pop().unwrap();
                    if let Value::Function { id, captures } = func {
                        match &self.program.functions.get(id).unwrap().body {
                            FunctionBody::Lambda(_) => {
                                let f = self.program.functions.get(id).unwrap();
                                let args = (0..*count)
                                    .map(|_| cur_frame.stack.pop().unwrap())
                                    .collect::<Vec<_>>();
                                let new_frame = f.create_frame(&mut self.program.values, captures);
                                for (i, arg) in args.into_iter().rev().enumerate() {
                                    let global = new_frame.variables.translate(LocalValueId(i));
                                    let backing = self.program.values.get_mut(global);
                                    assert!(!backing.initialized);
                                    if let Value::Function { id, captures } = &arg {
                                        let captures = captures.clone();
                                        backing.links.extend_from_slice(&captures);
                                        backing.value = arg;
                                        for cap in captures {
                                            self.program.values.inc_value(cap);
                                        }
                                    } else {
                                        backing.value = arg;
                                    }
                                }
                                self.function_stack.push(new_frame);
                                cur_frame = self.function_stack.last_mut().unwrap();
                                cur_func = self.program.functions.get(id).unwrap();
                                cur_body = match &cur_func.body {
                                    FunctionBody::Lambda(ins) => ins,
                                    FunctionBody::Builtin(_) => unreachable!(),
                                };
                            }
                            FunctionBody::Builtin(body) => {
                                let args = (0..*count)
                                    .map(|_| cur_frame.stack.pop().unwrap())
                                    .collect::<Vec<_>>();
                                let args = args.into_iter().rev().collect();
                                let ret_val = (body)(&mut self.editor, args);
                                cur_frame.stack.push(ret_val);
                            }
                        }
                    } else {
                        panic!()
                    }
                }
                Instruction::Ret => {
                    let ret_val = cur_frame.stack.pop().unwrap();
                    for var in cur_frame.variables.as_slice() {
                        self.program.values.dec_value(*var);
                    }
                    self.function_stack.pop().unwrap();
                    if !self.function_stack.is_empty() {
                        cur_frame = self.function_stack.last_mut().unwrap();
                        cur_func = self.program.functions.get(cur_frame.function).unwrap();
                        cur_body = match &cur_func.body {
                            FunctionBody::Lambda(ins) => ins,
                            FunctionBody::Builtin(_) => unreachable!(),
                        };
                        cur_frame.stack.push(ret_val);
                    } else {
                        break;
                    }
                }
            }
        }
    }
}
