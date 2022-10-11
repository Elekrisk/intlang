// use std::collections::HashMap;

// use crate::ast::{self, Expression, ExpressionId, StatementKind};

// use super::{Instruction, LocalValueId};

// pub struct Generator<'a> {
//     functions: HashMap<ExpressionId, Function<'a>>,
//     active_function: ExpressionId,
//     active_block: usize,

//     variable_stack: Vec<HashMap<String, usize>>,
// }

// pub struct Function<'a> {
//     body: &'a ast::Expression<'a>,

//     parameters: HashMap<String, usize>,
//     variables: HashMap<String, usize>,
//     captures: Vec<usize>,
//     blocks: Vec<Block>,
// }

// pub struct Block {
//     instructions: Vec<Instruction>,
// }

// impl<'a> Generator<'a> {
//     pub fn generate(&mut self, expr: &ast::Expression) {}

//     pub fn generate_function(&mut self, params: Vec<String>, body: &ast::Expression) {
//         let mut id = 0;
//         let mut id = || {
//             let r = id;
//             id += 1;
//             r
//         };

//         let mut f = Function {
//             body,

//             parameters: HashMap::new(),
//             variables: HashMap::new(),
//             captures: vec![],
//             blocks: vec![],
//         };

//         for param in params {
//             let id = id();
//             f.parameters.insert(param, id);
//         }

//         let mut block = Block {
//             instructions: vec![],
//         };
//     }

//     fn find_captures(&mut self, var_stack: Vec<Vec<String>>, expr: &ast::Expression) {}

//     fn register_variables(
//         &mut self,
//         parent_stacks: &mut Vec<Vec<Vec<String>>>,
//         stack: &mut Vec<Vec<String>>,
//         f: &mut Function,
//         expr: &ast::Expression,
//     ) {
//         let pop = if let Some(w) = &expr.where_clause {
//             stack.push(
//                 w.stmnts
//                     .iter()
//                     .map(|s| match &s.kind {
//                         ast::StatementKind::Assignment { name, .. } => name.value.to_string(),
//                     })
//                     .collect(),
//             );
//             for stmnt in &w.stmnts {
//                 let StatementKind::Assignment { expr, .. } = &stmnt.kind;
//                 self.register_variables(todo!(), stack, f, expr);
//             }
//             true
//         } else {
//             false
//         };

//         match &expr.kind {
//             ast::ExpressionKind::Nil(_) => todo!(),
//             ast::ExpressionKind::Literal(_) => todo!(),
//             ast::ExpressionKind::Variable(v) => {
//                 for map in stack.iter().rev() {
//                     if map.iter().any(|i| i == v.value) {
//                     } else {
//                         // Either capture or undefined
//                         for (depth, parent) in parent_stacks.iter().rev().enumerate() {
//                             for map in parent.iter().rev() {
//                                 if map.iter().any(|i| i == v.value) {
//                                     // Capture found
//                                     // f.captures.push
//                                 }
//                             }
//                         }
//                     };
//                 }
//             }
//             ast::ExpressionKind::Lambda {
//                 backslash,
//                 params,
//                 arrow,
//                 body,
//             } => {}
//             ast::ExpressionKind::Unary { op, expr } => todo!(),
//             ast::ExpressionKind::Binary { left, op, right } => todo!(),
//             ast::ExpressionKind::FuncCall {
//                 func,
//                 args,
//                 rbracket,
//             } => todo!(),
//             ast::ExpressionKind::If {
//                 kw_if,
//                 cond,
//                 case_true,
//                 case_false,
//                 kw_end,
//             } => todo!(),
//             ast::ExpressionKind::Sequence(_) => todo!(),
//         }
//     }

//     fn build_instruction(&mut self, instr: Instruction) {
//         self.functions
//             .get_mut(&self.active_function)
//             .unwrap()
//             .blocks[self.active_block]
//             .instructions
//             .push(instr);
//     }

//     fn new_block(&mut self) -> usize {
//         let id = self
//             .functions
//             .get(&self.active_function)
//             .unwrap()
//             .blocks
//             .len();
//         self.functions
//             .get_mut(&self.active_function)
//             .unwrap()
//             .blocks
//             .push(Block {
//                 instructions: vec![],
//             });
//         id
//     }
// }

use std::{collections::HashMap, fmt::Display, hash::Hash};

use num_bigint::BigInt;

use crate::{
    ast::{self, Expression, Id, StatementKind, Variable, VariableId},
    bytecode::{FunctionId, GlobalValueId, LocalValueId},
};

pub enum Instruction {
    PushNil,
    PushInteger(BigInt),
    PushFunction {
        function: FunctionId,
        captures: Vec<LocalValueId>,
    },

    SetVariable(LocalValueId),
    GetVariable(LocalValueId),
    GetGlobal(GlobalValueId),
    Pop,

    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Call(usize),
    If {
        true_block: Block,
        false_block: Block,
    },
    Jump(Block),
    Ret,
}

impl Instruction {
    fn is_terminator(&self) -> bool {
        matches!(self, Self::Call(_) | Self::If { .. } | Self::Jump(..))
    }
}

#[derive(Clone, Copy)]
struct Block(usize);

struct FunctionBuilder {
    active_block: Block,
    blocks: Vec<InternalBlock>,
}

impl FunctionBuilder {
    fn new() -> Self {
        Self {
            active_block: Block(0),
            blocks: vec![InternalBlock {
                instructions: vec![],
            }],
        }
    }

    fn current_block(&self) -> Block {
        self.active_block
    }

    fn set_active_block(&mut self, block: Block) {
        self.active_block = block;
    }

    fn new_block(&mut self) -> Block {
        let b = Block(self.blocks.len());
        self.blocks.push(InternalBlock {
            instructions: vec![],
        });
        b
    }

    fn build_instruction(&mut self, ins: Instruction) {
        self.blocks[self.active_block.0].instructions.push(ins);
    }

    fn print(&self, id: FunctionId, func: &Function) {
        println!("Function {}", id.0);
        print!("  Args: ");
        for arg in &func.arguments {
            print!("{}:#{}", func.get_local_id(*arg).unwrap(), arg.0,);
        }
        println!();
        print!("  Vars: ");
        for var in &func.variables {
            print!("{}:#{}", func.get_local_id(*var).unwrap(), var.0,);
        }
        println!();
        print!("  Caps: ");
        for cap in &func.captures {
            print!("{}:#{}", func.get_local_id(*cap).unwrap(), cap.0,);
        }
        println!();
        for (i, block) in self.blocks.iter().enumerate() {
            println!("block[{i}] {{");
            for ins in &block.instructions {
                print!("    ");
                match ins {
                    Instruction::PushNil => println!("push nil"),
                    Instruction::PushInteger(val) => println!("push {val}"),
                    Instruction::PushFunction { function, captures } => {
                        print!("push func[{}] ", function.0);
                        for v in captures
                            .iter()
                            .map(|l| l as &dyn Display)
                            .intersperse(&", ")
                        {
                            print!("{v}");
                        }
                        println!();
                    }
                    Instruction::SetVariable(var) => println!("set {var}"),
                    Instruction::GetVariable(var) => println!("get {var}"),
                    Instruction::GetGlobal(var) => println!("get {var}"),
                    Instruction::Pop => println!("pop"),
                    Instruction::Neg => println!("neg"),
                    Instruction::Add => println!("add"),
                    Instruction::Sub => println!("sub"),
                    Instruction::Mul => println!("mul"),
                    Instruction::Div => println!("div"),
                    Instruction::Mod => println!("mod"),
                    Instruction::Call(count) => println!("call {count}"),
                    Instruction::If {
                        true_block,
                        false_block,
                    } => println!(
                        "if then block[{}] else block[{}]",
                        true_block.0, false_block.0
                    ),
                    Instruction::Jump(block) => println!("jmp block[{}]", block.0),
                    Instruction::Ret => println!("ret"),
                }
            }
            println!("}}");
        }
    }
}

struct InternalBlock {
    instructions: Vec<Instruction>,
}

fn generate_main(expr: &ast::Expression) {}

fn generate_function(
    builder: &mut FunctionBuilder,
    var_ids: &mut HashMap<Id<()>, VariableId>,
    function_map: &HashMap<Id<Expression>, FunctionId>,
    functions: &HashMap<FunctionId, Function>,
    function: &Function,
) {
    generate_variables(
        builder,
        var_ids,
        function_map,
        functions,
        function,
        function.expr,
    );
    generate_expression(
        builder,
        var_ids,
        function_map,
        functions,
        function,
        function.expr,
    );
    builder.build_instruction(Instruction::Ret);
}

fn generate_variables(
    builder: &mut FunctionBuilder,
    var_ids: &mut HashMap<Id<()>, VariableId>,
    function_map: &HashMap<Id<Expression>, FunctionId>,
    functions: &HashMap<FunctionId, Function>,
    function: &Function,
    expr: &Expression,
) {
    if let Some(w) = &expr.where_clause {
        for stmnt in &w.stmnts {
            let StatementKind::Assignment {
                name,
                equals_sign,
                expr,
            } = &stmnt.kind;
            let id = var_ids[&name.id];
            let local_id = function.get_local_id(id).unwrap();

            generate_expression(builder, var_ids, function_map, functions, function, expr);
            builder.build_instruction(Instruction::SetVariable(local_id));
        }
    }

    match &expr.kind {
        ast::ExpressionKind::Nil(_)
        | ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Variable(_)
        | ast::ExpressionKind::Lambda { .. } => {}
        ast::ExpressionKind::Unary { op, expr } => {
            generate_variables(builder, var_ids, function_map, functions, function, expr);
        }
        ast::ExpressionKind::Binary { left, op, right } => {
            generate_variables(builder, var_ids, function_map, functions, function, left);
            generate_variables(builder, var_ids, function_map, functions, function, right);
        }
        ast::ExpressionKind::FuncCall {
            func,
            args,
            rbracket,
        } => {
            generate_variables(builder, var_ids, function_map, functions, function, func);
            for arg in args {
                generate_variables(builder, var_ids, function_map, functions, function, arg);
            }
        }
        ast::ExpressionKind::If {
            kw_if,
            cond,
            case_true,
            case_false,
            kw_end,
        } => {
            generate_variables(builder, var_ids, function_map, functions, function, cond);
            generate_variables(
                builder,
                var_ids,
                function_map,
                functions,
                function,
                case_true,
            );
            generate_variables(
                builder,
                var_ids,
                function_map,
                functions,
                function,
                case_false,
            );
        }
        ast::ExpressionKind::Sequence(exprs) => {
            for expr in exprs {
                generate_variables(builder, var_ids, function_map, functions, function, expr);
            }
        }
    }
}

fn generate_expression(
    builder: &mut FunctionBuilder,
    var_ids: &mut HashMap<Id<()>, VariableId>,
    function_map: &HashMap<Id<Expression>, FunctionId>,
    functions: &HashMap<FunctionId, Function>,
    function: &Function,
    expr: &Expression,
) {
    match &expr.kind {
        ast::ExpressionKind::Nil(_) => builder.build_instruction(Instruction::PushNil),
        ast::ExpressionKind::Literal(val) => {
            builder.build_instruction(Instruction::PushInteger(val.value.clone()))
        }
        ast::ExpressionKind::Variable(name) => {
            let id = var_ids[&name.id];
            let local_id = function.get_local_id(id).unwrap();
            builder.build_instruction(Instruction::GetVariable(local_id));
        }
        ast::ExpressionKind::Lambda {
            backslash,
            params,
            arrow,
            body,
        } => {
            let function_id = function_map[&expr.id];
            let func = &functions[&function_id];
            let mut captures = vec![];
            for capture in &func.captures {
                captures.push(function.get_local_id(*capture).unwrap());
            }
            builder.build_instruction(Instruction::PushFunction {
                function: function_id,
                captures,
            });
        }
        ast::ExpressionKind::Unary { op, expr } => {
            generate_expression(builder, var_ids, function_map, functions, function, expr);
            match op.value {
                ast::UnaryOp::Neg => builder.build_instruction(Instruction::Neg),
            }
        }
        ast::ExpressionKind::Binary { left, op, right } => {
            generate_expression(builder, var_ids, function_map, functions, function, left);
            generate_expression(builder, var_ids, function_map, functions, function, right);
            builder.build_instruction(match op.value {
                ast::BinaryOp::Add => Instruction::Add,
                ast::BinaryOp::Sub => Instruction::Sub,
                ast::BinaryOp::Mul => Instruction::Mul,
                ast::BinaryOp::Div => Instruction::Div,
                ast::BinaryOp::Mod => Instruction::Mod,
            });
        }
        ast::ExpressionKind::FuncCall {
            func,
            args,
            rbracket,
        } => {
            for arg in args {
                generate_expression(builder, var_ids, function_map, functions, function, arg);
            }
            generate_expression(builder, var_ids, function_map, functions, function, func);
            builder.build_instruction(Instruction::Call(args.len()));
        }
        ast::ExpressionKind::If {
            kw_if,
            cond,
            case_true,
            case_false,
            kw_end,
        } => {
            generate_expression(builder, var_ids, function_map, functions, function, cond);
            let true_block = builder.new_block();
            let false_block = builder.new_block();
            let after_block = builder.new_block();
            builder.build_instruction(Instruction::If { true_block, false_block });

            builder.set_active_block(true_block);
            generate_expression(
                builder,
                var_ids,
                function_map,
                functions,
                function,
                case_true,
            );
            builder.build_instruction(Instruction::Jump(after_block));

            builder.set_active_block(false_block);
            generate_expression(
                builder,
                var_ids,
                function_map,
                functions,
                function,
                case_false,
            );
            builder.build_instruction(Instruction::Jump(after_block));

            builder.set_active_block(after_block);
        }
        ast::ExpressionKind::Sequence(exprs) => {
            generate_expression(
                builder,
                var_ids,
                function_map,
                functions,
                function,
                &exprs[0],
            );
            for expr in &exprs[1..] {
                builder.build_instruction(Instruction::Pop);
                generate_expression(builder, var_ids, function_map, functions, function, expr);
            }
        }
    }
}

fn get_var_count(expr: &ast::Expression) -> usize {
    let mut count = 0;
    if let Some(w) = &expr.where_clause {
        for stmnt in &w.stmnts {
            let StatementKind::Assignment { expr, .. } = &stmnt.kind;
            count += 1 + get_var_count(expr);
        }
    }

    count
        + match &expr.kind {
            ast::ExpressionKind::Nil(_)
            | ast::ExpressionKind::Literal(_)
            | ast::ExpressionKind::Variable(_)
            | ast::ExpressionKind::Lambda { .. } => 0,
            ast::ExpressionKind::Unary { expr, .. } => get_var_count(expr),
            ast::ExpressionKind::Binary { left, right, .. } => {
                get_var_count(left) + get_var_count(right)
            }
            ast::ExpressionKind::FuncCall { func, args, .. } => {
                get_var_count(func) + args.iter().map(get_var_count).sum::<usize>()
            }
            ast::ExpressionKind::If {
                cond,
                case_true,
                case_false,
                ..
            } => get_var_count(cond) + get_var_count(case_true) + get_var_count(case_false),
            ast::ExpressionKind::Sequence(s) => s.iter().map(get_var_count).sum(),
        }
}

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

fn allocate_variable_ids<'a>(
    expr: &Expression<'a>,
    var_ids: &mut HashMap<Id<()>, VariableId>,
    functions: &mut HashMap<FunctionId, Function>,
    cur_func: FunctionId,
    mapping: &mut Vec<Vec<HashMap<&'a str, VariableId>>>,
    id_gen: &mut IdGen,
) {
    let needs_pop = if let Some(w) = &expr.where_clause {
        let this_func_mapping = mapping.last_mut().unwrap();
        this_func_mapping.push(HashMap::new());
        let map = this_func_mapping.last_mut().unwrap();
        for stmnt in &w.stmnts {
            let StatementKind::Assignment { name, .. } = &stmnt.kind;
            let id = VariableId(id_gen.next());
            map.insert(name.value, id);
            var_ids.insert(name.id, id);
        }
        for stmnt in &w.stmnts {
            let StatementKind::Assignment { expr, .. } = &stmnt.kind;
            allocate_variable_ids(expr, var_ids, functions, cur_func, mapping, id_gen);
        }
        true
    } else {
        false
    };

    match &expr.kind {
        ast::ExpressionKind::Nil(_) => {}
        ast::ExpressionKind::Literal(_) => {}
        ast::ExpressionKind::Variable(var) => {
            let name = var.value;
            if let Some(var_id) = mapping
                .iter()
                .rev()
                .flat_map(|e| e.iter().rev())
                .filter_map(|v| v.get(name))
                .next()
            {
                var_ids.insert(var.id, *var_id);
            } else {
                panic!("Unable to find variable {name}")
            }
        }
        ast::ExpressionKind::Lambda { params, body, .. } => {
            let mut new_map = HashMap::new();
            for param in params {
                let name = param.value;
                let id = VariableId(id_gen.next());
                new_map.insert(name, id);
                var_ids.insert(param.id, id);
            }
            mapping.push(vec![new_map]);
            allocate_variable_ids(body, var_ids, functions, cur_func, mapping, id_gen);
            mapping.pop();
        }
        ast::ExpressionKind::Unary { op, expr } => {
            allocate_variable_ids(expr, var_ids, functions, cur_func, mapping, id_gen)
        }
        ast::ExpressionKind::Binary { left, op, right } => {
            allocate_variable_ids(left, var_ids, functions, cur_func, mapping, id_gen);
            allocate_variable_ids(right, var_ids, functions, cur_func, mapping, id_gen);
        }
        ast::ExpressionKind::FuncCall { func, args, .. } => {
            allocate_variable_ids(func, var_ids, functions, cur_func, mapping, id_gen);
            for arg in args {
                allocate_variable_ids(arg, var_ids, functions, cur_func, mapping, id_gen);
            }
        }
        ast::ExpressionKind::If {
            cond,
            case_true,
            case_false,
            ..
        } => {
            allocate_variable_ids(cond, var_ids, functions, cur_func, mapping, id_gen);
            allocate_variable_ids(case_true, var_ids, functions, cur_func, mapping, id_gen);
            allocate_variable_ids(case_false, var_ids, functions, cur_func, mapping, id_gen);
        }
        ast::ExpressionKind::Sequence(exprs) => {
            for expr in exprs {
                allocate_variable_ids(expr, var_ids, functions, cur_func, mapping, id_gen);
            }
        }
    }

    if needs_pop {
        mapping.last_mut().unwrap().pop();
    }
}

fn get_variables(
    expr: &ast::Expression,
    var_ids: &mut HashMap<Id<()>, VariableId>,
    function_map: &HashMap<Id<Expression>, FunctionId>,
    functions: &mut HashMap<FunctionId, Function>,
    parent_stack: &mut Vec<FunctionId>,
    cur_func: FunctionId,
) {
    if let Some(w) = &expr.where_clause {
        for stmnt in &w.stmnts {
            let StatementKind::Assignment { name, .. } = &stmnt.kind;
            let id = var_ids[&name.id];
            functions.get_mut(&cur_func).unwrap().variables.push(id);
        }
        for stmnt in &w.stmnts {
            let StatementKind::Assignment { expr, .. } = &stmnt.kind;
            get_variables(
                expr,
                var_ids,
                function_map,
                functions,
                parent_stack,
                cur_func,
            );
        }
    }

    match &expr.kind {
        ast::ExpressionKind::Nil(_) | ast::ExpressionKind::Literal(_) => {}
        ast::ExpressionKind::Variable(name) => {
            let id = var_ids[&name.id];
            let func = functions.get_mut(&cur_func).unwrap();
            if func.arguments.contains(&id)
                || func.variables.contains(&id)
                || func.captures.contains(&id)
            {
            } else {
                func.captures.push(id);
                for f in parent_stack.iter().rev() {
                    let func = functions.get_mut(f).unwrap();
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
        ast::ExpressionKind::Lambda {
            backslash,
            params,
            arrow,
            body,
        } => {
            let func_id = function_map[&expr.id];
            let func = functions.get_mut(&func_id).unwrap();
            for param in params {
                let id = var_ids[&param.id];
                func.arguments.push(id);
            }
            parent_stack.push(cur_func);
            get_variables(
                body,
                var_ids,
                function_map,
                functions,
                parent_stack,
                func_id,
            );
            parent_stack.pop();
        }
        ast::ExpressionKind::Unary { op, expr } => {
            get_variables(
                expr,
                var_ids,
                function_map,
                functions,
                parent_stack,
                cur_func,
            );
        }
        ast::ExpressionKind::Binary { left, op, right } => {
            get_variables(
                left,
                var_ids,
                function_map,
                functions,
                parent_stack,
                cur_func,
            );
            get_variables(
                right,
                var_ids,
                function_map,
                functions,
                parent_stack,
                cur_func,
            );
        }
        ast::ExpressionKind::FuncCall {
            func,
            args,
            rbracket,
        } => {
            get_variables(
                func,
                var_ids,
                function_map,
                functions,
                parent_stack,
                cur_func,
            );
            for arg in args {
                get_variables(
                    arg,
                    var_ids,
                    function_map,
                    functions,
                    parent_stack,
                    cur_func,
                );
            }
        }
        ast::ExpressionKind::If {
            kw_if,
            cond,
            case_true,
            case_false,
            kw_end,
        } => {
            get_variables(
                cond,
                var_ids,
                function_map,
                functions,
                parent_stack,
                cur_func,
            );
            get_variables(
                case_true,
                var_ids,
                function_map,
                functions,
                parent_stack,
                cur_func,
            );
            get_variables(
                case_false,
                var_ids,
                function_map,
                functions,
                parent_stack,
                cur_func,
            );
        }
        ast::ExpressionKind::Sequence(exprs) => {
            for expr in exprs {
                get_variables(
                    expr,
                    var_ids,
                    function_map,
                    functions,
                    parent_stack,
                    cur_func,
                );
            }
        }
    }
}

fn allocate_function_ids<'a>(
    expr: &'a ast::Expression,
    map: &mut HashMap<Id<Expression<'a>>, FunctionId>,
    functions: &mut HashMap<FunctionId, Function<'a>>,
    id_gen: &mut IdGen,
) {
    if let Some(w) = &expr.where_clause {
        for stmnt in &w.stmnts {
            let StatementKind::Assignment {
                name,
                equals_sign,
                expr,
            } = &stmnt.kind;
            allocate_function_ids(expr, map, functions, id_gen);
        }
    }

    match &expr.kind {
        ast::ExpressionKind::Nil(_)
        | ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Variable(_) => {}
        ast::ExpressionKind::Lambda {
            backslash,
            params,
            arrow,
            body,
        } => {
            let fid = FunctionId(id_gen.next());
            map.insert(expr.id, fid);
            functions.insert(
                fid,
                Function {
                    expr: body,
                    arguments: vec![],
                    variables: vec![],
                    captures: vec![],
                },
            );
            allocate_function_ids(body, map, functions, id_gen);
        }
        ast::ExpressionKind::Unary { op, expr } => {
            allocate_function_ids(expr, map, functions, id_gen)
        }
        ast::ExpressionKind::Binary { left, op, right } => {
            allocate_function_ids(left, map, functions, id_gen);
            allocate_function_ids(right, map, functions, id_gen);
        }
        ast::ExpressionKind::FuncCall {
            func,
            args,
            rbracket,
        } => {
            allocate_function_ids(func, map, functions, id_gen);
            for arg in args {
                allocate_function_ids(arg, map, functions, id_gen);
            }
        }
        ast::ExpressionKind::If {
            kw_if,
            cond,
            case_true,
            case_false,
            kw_end,
        } => {
            allocate_function_ids(cond, map, functions, id_gen);
            allocate_function_ids(case_true, map, functions, id_gen);
            allocate_function_ids(case_false, map, functions, id_gen);
        }
        ast::ExpressionKind::Sequence(exprs) => {
            for expr in exprs {
                allocate_function_ids(expr, map, functions, id_gen);
            }
        }
    }
}

#[derive(Debug)]
struct Function<'a> {
    expr: &'a Expression<'a>,
    arguments: Vec<VariableId>,
    variables: Vec<VariableId>,
    captures: Vec<VariableId>,
}

impl<'a> Function<'a> {
    fn get_local_id(&self, var_id: VariableId) -> Option<LocalValueId> {
        for i in 0..self.arguments.len() {
            if self.arguments[i] == var_id {
                return Some(LocalValueId(i));
            }
        }
        for i in 0..self.variables.len() {
            if self.variables[i] == var_id {
                return Some(LocalValueId(i + self.arguments.len()));
            }
        }
        for i in 0..self.captures.len() {
            if self.captures[i] == var_id {
                return Some(LocalValueId(
                    i + self.arguments.len() + self.variables.len(),
                ));
            }
        }
        None
    }
}

pub fn generate(expr: &mut ast::Expression) {
    let builtin_functions = [("print", 0), ("input", 1)];

    let mut function_map = HashMap::new();
    let mut functions = HashMap::new();
    let mut func_id_gen = IdGen::new(builtin_functions.len());
    let main = FunctionId(func_id_gen.next());
    function_map.insert(expr.id, main);
    functions.insert(
        main,
        Function {
            expr,
            arguments: vec![],
            variables: vec![],
            captures: vec![],
        },
    );

    allocate_function_ids(expr, &mut function_map, &mut functions, &mut func_id_gen);

    let mut var_ids = HashMap::new();

    allocate_variable_ids(
        expr,
        &mut var_ids,
        &mut functions,
        main,
        &mut vec![vec![HashMap::from_iter(
            builtin_functions.iter().map(|(a, b)| (*a, VariableId(*b))),
        )]],
        &mut IdGen::new(builtin_functions.len()),
    );

    get_variables(
        expr,
        &mut var_ids,
        &function_map,
        &mut functions,
        &mut vec![],
        main,
    );

    for (&id, func) in &functions {
        let mut builder = FunctionBuilder::new();

        generate_function(&mut builder, &mut var_ids, &function_map, &functions, func);

        builder.print(id, func);
        println!();
    }
}
