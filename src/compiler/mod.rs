use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{PointerType, StructType},
    AddressSpace,
};
use num_bigint::BigInt;

use crate::ast::{
    BinaryOp, Expression, ExpressionKind, Id, Statement, UnaryOp, VariableId, WhereClause, T,
};

pub struct IdGen(usize);

impl IdGen {
    pub fn new(start: usize) -> Self {
        Self(start)
    }

    pub fn next(&mut self) -> usize {
        let r = self.0;
        self.0 += 1;
        r
    }
}

#[derive(Debug)]
pub struct Compiler<'comp, 'a> {
    function_ids: HashMap<Id<Expression<'a>>, Id<Function<'comp, 'a>>>,
    functions: HashMap<Id<Function<'comp, 'a>>, Function<'comp, 'a>>,
    variable_ids: HashMap<Id<()>, VariableId>,
    variable_graph: HashMap<VariableId, Vec<VariableId>>,
}

#[derive(Debug)]
pub struct Function<'comp, 'a> {
    expr: &'comp Expression<'a>,
    arguments: Vec<VariableId>,
    variables: Vec<VariableId>,
    captures: Vec<VariableId>,
}

impl<'comp, 'a> Compiler<'comp, 'a> {
    pub fn compile(expr: &'comp Expression<'a>) {
        let mut s = Self {
            function_ids: HashMap::new(),
            functions: HashMap::new(),
            variable_ids: HashMap::new(),
        };

        let mut func_id_gen = IdGen::new(2);
        let main = Id::new(func_id_gen.next());
        s.functions.insert(
            main,
            Function {
                expr,
                arguments: vec![],
                variables: vec![],
                captures: vec![],
            },
        );
        s.allocate_function_ids(expr, &mut func_id_gen);
        s.get_var_ids(expr, &mut IdGen::new(0));
        s.get_variables(expr, main);

        for (id, func) in &s.functions {
            FunctionBuilder::new(&s, &Context::create()).build_func(*id, func);
        }
    }

    fn allocate_function_ids(&mut self, expr: &'comp Expression<'a>, id_gen: &mut IdGen) {
        FunctionIdAllocator {
            compiler: self,
            id_gen,
        }
        .visit_expr(expr);
    }

    fn get_var_ids(&mut self, expr: &'comp Expression<'a>, id_gen: &mut IdGen) {
        VariableIdCollector {
            compiler: self,
            mapping: &mut vec![vec![HashMap::from_iter([
                ("print", VariableId(id_gen.next())),
                ("input", VariableId(id_gen.next())),
            ])]],
            id_gen,
        }
        .visit_expr(expr);
    }

    fn get_variables(&mut self, expr: &'comp Expression<'a>, main: Id<Function<'comp, 'a>>) {
        let mut c = VariableAllocator {
            compiler: self,
            parent_stack: &mut vec![],
            cur_func: main,
        };

        c.visit_expr(expr);
    }

    fn create_variable_graph(&mut self, expr: &'comp Expression<'a>) {
        
    }
}

struct VariableGraphCreator<'s, 'comp, 'a> {
    compiler: &'s mut Compiler<'comp, 'a>
}

struct FunctionBuilder<'s, 'comp, 'a> {
    compiler: &'s Compiler<'comp, 'a>,
    context: &'s Context,
    module: Module<'s>,

    value_type: PointerType<'s>,
    backing_store_type: StructType<'s>,
    int_data_type: PointerType<'s>,
    lambda_data_type: StructType<'s>,
}

/*

Backing store: {
    count: usize,
    type: u8,
    data: ...
}

Int data: void* (passed into int-manipulating functions)
Lambda data: {
    param_count: usize,
    ptr: void*, // cast to fn(value, value, ...) -> value
    capture_count: usize,
    captures: ...
}


*/

impl<'s, 'comp, 'a> FunctionBuilder<'s, 'comp, 'a> {
    fn new(compiler: &'s Compiler<'comp, 'a>, context: &'s Context) -> Self {
        let module = context.create_module("main");

        let backing_store_type = context.struct_type(
            &[
                context.i64_type().into(), // count
                context.i8_type().into(),  // type
                context.i64_type().into(), // data, cast to type data
            ],
            false,
        );
        let value_type = backing_store_type.ptr_type(AddressSpace::Generic);

        let int_data_type = context.i8_type().ptr_type(AddressSpace::Generic);
        let lambda_data_type = context.struct_type(
            &[
                context.i64_type().into(),                                // param count
                context.i8_type().ptr_type(AddressSpace::Generic).into(), // ptr to function
                context.i64_type().into(),                                // capture count
                context.i64_type().into(),                                // captures, cast to [i8*]
            ],
            false,
        );

        Self {
            compiler,
            context,
            module,
            value_type,
            backing_store_type,
            int_data_type,
            lambda_data_type,
        }
    }
    fn build_func(&mut self, id: Id<Function>, func: &Function) {
        let ty = self.value_type.fn_type(
            &func
                .arguments
                .iter()
                .map(|_| self.value_type.into())
                .collect::<Vec<_>>(),
            false,
        );
        let func = self.module.add_function(&format!("func-{id}"), ty, None);
        let entry = self.context.append_basic_block(func, "entry");
        let builder = self.context.create_builder();
        builder.position_at_end(entry);
    }
}

struct FunctionIdAllocator<'s, 'comp, 'a> {
    compiler: &'s mut Compiler<'comp, 'a>,
    id_gen: &'s mut IdGen,
}

impl<'s, 'comp, 'a> Visitor<'comp, 'a> for FunctionIdAllocator<'s, 'comp, 'a> {
    fn visit_expr_lambda(
        &mut self,
        expr: &'comp Expression<'a>,
        bs: &T<()>,
        params: &Vec<T<&'a str>>,
        arrow: &T<()>,
        body: &'comp Box<Expression<'a>>,
    ) {
        let fid = Id::new(self.id_gen.next());
        self.compiler.function_ids.insert(expr.id, fid);
        self.compiler.functions.insert(
            fid,
            Function {
                expr: body,
                arguments: vec![],
                variables: vec![],
                captures: vec![],
            },
        );
        self.walk_expr_lambda(expr, bs, params, arrow, body);
    }
}

struct VariableAllocator<'s, 'comp, 'a> {
    compiler: &'s mut Compiler<'comp, 'a>,
    parent_stack: &'s mut Vec<Id<Function<'comp, 'a>>>,
    cur_func: Id<Function<'comp, 'a>>,
}

impl<'s, 'comp, 'a> Visitor<'comp, 'a> for VariableAllocator<'s, 'comp, 'a> {
    fn visit_where_clause(&mut self, where_clause: &'comp WhereClause<'a>) {
        for stmnt in &where_clause.stmnts {
            let id = self.compiler.variable_ids[&stmnt.name.id];
            self.compiler
                .functions
                .get_mut(&self.cur_func)
                .unwrap()
                .variables
                .push(id);
        }
        self.walk_where_clause(where_clause);
    }

    fn visit_expr_variable(&mut self, expr: &'comp Expression<'a>, var: &T<&str>) {
        let id = self.compiler.variable_ids[&var.id];
        let func = self.compiler.functions.get_mut(&self.cur_func).unwrap();
        if func.arguments.contains(&id)
            || func.variables.contains(&id)
            || func.captures.contains(&id)
        {
        } else {
            func.captures.push(id);
            for f in self.parent_stack.iter().rev() {
                let func = self.compiler.functions.get_mut(f).unwrap();
                if func.arguments.contains(&id)
                    || func.variables.contains(&id)
                    || func.captures.contains(&id)
                {
                    break;
                } else {
                    func.captures.push(id);
                }
            }
        }
    }

    fn visit_expr_lambda(
        &mut self,
        expr: &'comp Expression<'a>,
        bs: &T<()>,
        params: &Vec<T<&'a str>>,
        arrow: &T<()>,
        body: &'comp Box<Expression<'a>>,
    ) {
        let func_id = self.compiler.function_ids[&expr.id];
        let func = self.compiler.functions.get_mut(&func_id).unwrap();
        for param in params {
            let id = self.compiler.variable_ids[&param.id];
            func.arguments.push(id);
        }
        self.parent_stack.push(self.cur_func);
        self.cur_func = func_id;
        self.walk_expr_lambda(expr, bs, params, arrow, body);
        self.cur_func = self.parent_stack.pop().unwrap();
    }
}

struct VariableIdCollector<'s, 'comp, 'a> {
    compiler: &'s mut Compiler<'comp, 'a>,
    mapping: &'s mut Vec<Vec<HashMap<&'a str, VariableId>>>,
    id_gen: &'s mut IdGen,
}

impl<'s, 'comp, 'a> Visitor<'comp, 'a> for VariableIdCollector<'s, 'comp, 'a> {
    fn visit_expr(&mut self, expr: &'comp Expression<'a>) {
        let needs_pop = if let Some(w) = &expr.where_clause {
            self.mapping.last_mut().unwrap().push(HashMap::new());
            let map = self.mapping.last_mut().unwrap().last_mut().unwrap();
            for stmnt in &w.stmnts {
                let id = VariableId(self.id_gen.next());

                map.insert(stmnt.name.value, id);
                self.compiler.variable_ids.insert(stmnt.name.id, id);
            }
            true
        } else {
            false
        };

        self.walk_expr(expr);

        if needs_pop {
            self.mapping.last_mut().unwrap().pop();
        }
    }

    fn visit_expr_variable(&mut self, expr: &'comp Expression<'a>, var: &T<&str>) {
        let name = var.value;
        if let Some(var_id) = self
            .mapping
            .iter()
            .rev()
            .flat_map(|e| e.iter().rev())
            .filter_map(|v| v.get(name))
            .next()
        {
            self.compiler.variable_ids.insert(var.id, *var_id);
        } else {
            panic!("Unable to find variable {name}");
        }
    }

    fn visit_expr_lambda(
        &mut self,
        expr: &'comp Expression<'a>,
        bs: &T<()>,
        params: &Vec<T<&'a str>>,
        arrow: &T<()>,
        body: &'comp Box<Expression<'a>>,
    ) {
        let mut new_map = HashMap::new();
        for param in params {
            let name = param.value;
            let id = VariableId(self.id_gen.next());
            new_map.insert(name, id);
            self.compiler.variable_ids.insert(param.id, id);
        }
        self.mapping.push(vec![new_map]);
        self.walk_expr_lambda(expr, bs, params, arrow, body);
        self.mapping.pop();
    }
}

trait Visitor<'comp, 'a> {
    fn visit_where_clause(&mut self, where_clause: &'comp WhereClause<'a>) {
        self.walk_where_clause(where_clause)
    }
    fn walk_where_clause(&mut self, where_clause: &'comp WhereClause<'a>) {
        for stmnt in &where_clause.stmnts {
            self.visit_stmnt(stmnt)
        }
    }

    fn visit_stmnt(&mut self, stmnt: &'comp Statement<'a>) {
        self.walk_stmnt(stmnt)
    }
    fn walk_stmnt(&mut self, stmnt: &'comp Statement<'a>) {
        self.visit_ident(stmnt.name);
        self.visit_expr(&stmnt.expr);
    }

    fn visit_ident(&mut self, _ident: T<&str>) {}

    fn visit_expr(&mut self, expr: &'comp Expression<'a>) {
        self.walk_expr(expr);
    }
    fn walk_expr(&mut self, expr: &'comp Expression<'a>) {
        if let Some(w) = &expr.where_clause {
            self.visit_where_clause(w);
        }
        match &expr.kind {
            ExpressionKind::Nil(_) => self.visit_expr_nil(expr),
            ExpressionKind::Literal(val) => self.visit_expr_literal(expr, val),
            ExpressionKind::Variable(var) => self.visit_expr_variable(expr, var),
            ExpressionKind::Lambda {
                backslash,
                params,
                arrow,
                body,
            } => self.visit_expr_lambda(expr, backslash, params, arrow, body),
            ExpressionKind::Unary { op, expr } => self.visit_expr_unary(expr, op, expr),
            ExpressionKind::Binary { left, op, right } => {
                self.visit_expr_binary(expr, left, op, right)
            }
            ExpressionKind::FuncCall {
                func,
                args,
                rbracket,
            } => self.visit_expr_funccall(expr, func, args, rbracket),
            ExpressionKind::If {
                kw_if,
                cond,
                case_true,
                case_false,
                kw_end,
            } => self.visit_expr_if(expr, kw_if, cond, case_true, case_false, kw_end),
            ExpressionKind::Sequence(exprs) => self.visit_expr_sequence(expr, exprs),
        }
    }

    fn visit_expr_nil(&mut self, expr: &'comp Expression<'a>) {}

    fn visit_expr_literal(&mut self, expr: &'comp Expression<'a>, val: &T<BigInt>) {}

    fn visit_expr_variable(&mut self, expr: &'comp Expression<'a>, var: &T<&str>) {}

    fn visit_expr_lambda(
        &mut self,
        expr: &'comp Expression<'a>,
        bs: &T<()>,
        params: &Vec<T<&'a str>>,
        arrow: &T<()>,
        body: &'comp Box<Expression<'a>>,
    ) {
        self.walk_expr_lambda(expr, bs, params, arrow, body);
    }
    fn walk_expr_lambda(
        &mut self,
        expr: &'comp Expression<'a>,
        bs: &T<()>,
        params: &Vec<T<&'a str>>,
        arrow: &T<()>,
        body: &'comp Box<Expression<'a>>,
    ) {
        for param in params {
            self.visit_ident(*param);
        }
        self.visit_expr(body);
    }

    fn visit_expr_lambda_param(&mut self, param: T<&str>) {
        self.walk_expr_lambda_param(param);
    }
    fn walk_expr_lambda_param(&mut self, param: T<&str>) {
        self.visit_ident(param);
    }

    fn visit_expr_unary(
        &mut self,
        expr: &'comp Expression<'a>,
        op: &T<UnaryOp>,
        inner: &'comp Box<Expression<'a>>,
    ) {
        self.walk_expr_unary(expr, op, inner);
    }
    fn walk_expr_unary(
        &mut self,
        expr: &'comp Expression<'a>,
        op: &T<UnaryOp>,
        inner: &'comp Box<Expression<'a>>,
    ) {
        self.visit_expr(inner);
    }

    fn visit_expr_binary(
        &mut self,
        expr: &'comp Expression<'a>,
        left: &'comp Box<Expression<'a>>,
        op: &T<BinaryOp>,
        right: &'comp Box<Expression<'a>>,
    ) {
        self.walk_expr_binary(expr, left, op, right);
    }
    fn walk_expr_binary(
        &mut self,
        expr: &'comp Expression<'a>,
        left: &'comp Box<Expression<'a>>,
        op: &T<BinaryOp>,
        right: &'comp Box<Expression<'a>>,
    ) {
        self.visit_expr(left);
        self.visit_expr(right);
    }

    fn visit_expr_funccall(
        &mut self,
        expr: &'comp Expression<'a>,
        func: &'comp Box<Expression<'a>>,
        args: &'comp Vec<Expression<'a>>,
        rbracket: &T<()>,
    ) {
        self.walk_expr_funccall(expr, func, args, rbracket);
    }
    fn walk_expr_funccall(
        &mut self,
        expr: &'comp Expression<'a>,
        func: &'comp Box<Expression<'a>>,
        args: &'comp Vec<Expression<'a>>,
        rbracket: &T<()>,
    ) {
        self.visit_expr(func);
        for arg in args {
            self.visit_expr(arg);
        }
    }

    fn visit_expr_if(
        &mut self,
        expr: &'comp Expression<'a>,
        kw_if: &T<()>,
        cond: &'comp Box<Expression<'a>>,
        case_true: &'comp Box<Expression<'a>>,
        case_false: &'comp Box<Expression<'a>>,
        kw_end: &T<()>,
    ) {
        self.walk_expr_if(expr, kw_if, cond, case_true, case_false, kw_end);
    }
    fn walk_expr_if(
        &mut self,
        expr: &'comp Expression<'a>,
        kw_if: &T<()>,
        cond: &'comp Box<Expression<'a>>,
        case_true: &'comp Box<Expression<'a>>,
        case_false: &'comp Box<Expression<'a>>,
        kw_end: &T<()>,
    ) {
        self.visit_expr(cond);
        self.visit_expr(case_true);
        self.visit_expr(case_false);
    }

    fn visit_expr_sequence(
        &mut self,
        expr: &'comp Expression<'a>,
        exprs: &'comp Vec<Expression<'a>>,
    ) {
        self.walk_expr_sequence(expr, exprs);
    }
    fn walk_expr_sequence(
        &mut self,
        expr: &'comp Expression<'a>,
        exprs: &'comp Vec<Expression<'a>>,
    ) {
        for expr in exprs {
            self.visit_expr(expr);
        }
    }
}
