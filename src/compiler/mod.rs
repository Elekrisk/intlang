use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target},
    types::{PointerType, StructType},
    values::{FunctionValue, InstructionOpcode},
    AddressSpace, IntPredicate,
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

        let context = Context::create();
        let mut builder = FunctionBuilder::new(&s, &context);

        builder.build_external_func_declarations();
        builder.build_builtin_function_extend_variable_storage();
        builder.build_builtin_function_initialize_variable_storage();
        builder.build_builtin_function_allocate_backing_store();
        builder.build_preamble();

        for (id, func) in &s.functions {
            builder.build_func(*id, func);
        }

        builder.module.print_to_file("generated.ll").unwrap();
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
}

struct VariableGraphCreator<'s, 'comp, 'a> {
    compiler: &'s mut Compiler<'comp, 'a>,
}

struct FunctionBuilder<'s, 'comp, 'a> {
    compiler: &'s Compiler<'comp, 'a>,
    context: &'s Context,
    module: Module<'s>,
    builder: Builder<'s>,

    variable_storage_type: StructType<'s>,
    value_type: StructType<'s>,
    backing_store_type: StructType<'s>,
    int_data_type: PointerType<'s>,
    lambda_data_type: StructType<'s>,

    builtin_functions: HashMap<&'static str, FunctionValue<'s>>,
}

/*

BackingStore: {
    count: usize,
    val: Value
}

struct Value: {
    type: Type,
    val: ValueData
}

enum Type {
    Nil = 0,
    Int = 1,
    Func = 2,
    Var = 3
}

struct ValueData:Var {
    ptr: store*
}

struct ValueData:Int {
    ptr: void*
}

Int data: void* (passed into int-manipulating functions)
Lambda data: {
    param_count: usize,
    ptr: void*, // cast to fn(value, value, ...) -> value
    capture_count: usize,
    captures: BackingStore**
}


*/

impl<'s, 'comp, 'a> FunctionBuilder<'s, 'comp, 'a> {
    fn new(compiler: &'s Compiler<'comp, 'a>, context: &'s Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();

        // Value {
        //     type: u8,
        //     val: [u64; 4]
        // }
        let value_type = context.opaque_struct_type("Value");
        value_type.set_body(
            &[
                /* type: */ context.i8_type().into(),
                /* val:  */ context.i64_type().array_type(4).into(),
            ],
            false,
        );

        // BackingStore {
        //     count: u64,
        //     value: Value
        // }
        let backing_store_type = context.opaque_struct_type("BackingStore");
        backing_store_type.set_body(&[context.i64_type().into(), value_type.into()], false);

        // Int = void*
        let int_data_type = context.i8_type().ptr_type(AddressSpace::Generic);
        // Lambda {
        //     param_count: u64,
        //     fn_ptr: void*,
        //     capture_count: u64,
        //     captures: BackingStore**
        // }
        let lambda_data_type = context.opaque_struct_type("LambdaType");
        lambda_data_type.set_body(
            &[
                context.i64_type().into(),
                context.i8_type().ptr_type(AddressSpace::Generic).into(),
                context.i64_type().into(),
                backing_store_type
                    .ptr_type(AddressSpace::Generic)
                    .ptr_type(AddressSpace::Generic)
                    .into(),
            ],
            false,
        );

        // VariableStorage {
        //     count: u64,
        //     backing_stores: BackingStore**
        // }
        let variable_storage_type = context.opaque_struct_type("VarStorage");
        variable_storage_type.set_body(
            &[
                context.i64_type().into(),
                backing_store_type.ptr_type(AddressSpace::Generic).into(),
            ],
            false,
        );

        Self {
            compiler,
            context,
            module,
            builder,
            value_type,
            backing_store_type,
            int_data_type,
            lambda_data_type,
            variable_storage_type,
            builtin_functions: HashMap::new(),
        }
    }

    fn build_external_func_declarations(&mut self) {
        let variable_storage = self.module.add_global(
            self.variable_storage_type,
            Some(AddressSpace::Generic),
            "variable_storage",
        );
        variable_storage.set_initializer(&self.variable_storage_type.const_zero());

        let mut add_func = |name, ty| {
            self.builtin_functions
                .insert(name, self.module.add_function(name, ty, None))
        };

        add_func(
            "realloc",
            self.context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .fn_type(
                    &[
                        self.context
                            .i8_type()
                            .ptr_type(AddressSpace::Generic)
                            .into(),
                        self.context.i64_type().into(),
                    ],
                    false,
                ),
        );
        add_func(
            "memset",
            self.context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .fn_type(
                    &[
                        self.context
                            .i8_type()
                            .ptr_type(AddressSpace::Generic)
                            .into(),
                        self.context.i32_type().into(),
                        self.context.i64_type().into(),
                    ],
                    false,
                ),
        );
    }

    fn build_builtin_function_initialize_variable_storage(&self) {
        let context = self.context;
        let module = &self.module;
        let builder = &self.builder;
        let builtin_functions = &self.builtin_functions;
        let backing_store_type = self.backing_store_type;

        let global = module.get_global("variable_storage").unwrap();
        let var_ptr = global.as_pointer_value();

        let expand_func = module.add_function(
            "initialize_variable_storage",
            context.void_type().fn_type(&[], false),
            None,
        );
        let entry = context.append_basic_block(expand_func, "entry");
        builder.position_at_end(entry);

        let var_storage = builder
            .build_load(var_ptr, "var-storage")
            .into_struct_value();
        // New size is old-size * 2
        let new_size = context.i64_type().const_int(16, false);
        // The stored size is in number of elements,
        // but realloc takes a byte size.
        let byte_size =
            builder.build_int_mul(new_size, backing_store_type.size_of().unwrap(), "byte-size");
        // New pointer.
        let new_ptr = builder
            .build_call(
                builtin_functions["realloc"],
                &[
                    context
                        .i8_type()
                        .ptr_type(AddressSpace::Generic)
                        .const_null()
                        .into(),
                    byte_size.into(),
                ],
                "new_ptr",
            )
            .try_as_basic_value()
            .unwrap_left()
            .into_pointer_value();

        builder.build_call(
            builtin_functions["memset"],
            &[
                new_ptr.into(),
                context.i32_type().const_zero().into(),
                byte_size.into(),
            ],
            "",
        );

        // Cast the pointer to BackingStore*
        let new_ptr = builder.build_pointer_cast(
            new_ptr,
            backing_store_type.ptr_type(AddressSpace::Generic),
            "ptr-casted-to-backing-store-ptr-ptr",
        );

        let var_storage = builder
            .build_insert_value(var_storage, new_size, 0, "var-storage-with-new-size")
            .unwrap();
        let var_storage = builder
            .build_insert_value(var_storage, new_ptr, 1, "var-storage-with-new-ptr")
            .unwrap();
        builder.build_store(var_ptr, var_storage);
        builder.build_return(None);
    }

    fn build_builtin_function_extend_variable_storage(&mut self) {
        let context = self.context;
        let module = &self.module;
        let builder = &self.builder;
        let builtin_functions = &self.builtin_functions;
        let backing_store_type = &self.backing_store_type;

        let global = module.get_global("variable_storage").unwrap();
        let var_ptr = global.as_pointer_value();

        // void expand-func(void);
        // Doubles the size of the variable storage.
        let expand_func = module.add_function(
            "expand_variable_storage",
            context.void_type().fn_type(&[], false),
            None,
        );
        let entry = context.append_basic_block(expand_func, "entry");
        builder.position_at_end(entry);

        let var_storage = builder
            .build_load(var_ptr, "var-storage")
            .into_struct_value();
        let old_size = builder
            .build_extract_value(var_storage, 0, "old-size")
            .unwrap();
        // New size is old-size * 2
        let new_size = builder.build_int_mul(
            old_size.into_int_value(),
            context.i64_type().const_int(2, false),
            "new-size",
        );
        // The stored size is in number of elements,
        // but realloc takes a byte size.
        let byte_size =
            builder.build_int_mul(new_size, backing_store_type.size_of().unwrap(), "byte-size");

        // The old pointer.
        let ptr = builder
            .build_extract_value(var_storage, 1, "old-ptr")
            .unwrap()
            .into_pointer_value();
        // We need to cast the pointer to i8*
        let ptr = builder.build_pointer_cast(
            ptr,
            context.i8_type().ptr_type(AddressSpace::Generic),
            "ptr-casted-to-void",
        );
        // New pointer.
        // Data has been copied by realloc.
        let new_ptr = builder
            .build_call(
                builtin_functions["realloc"],
                &[ptr.into(), byte_size.into()],
                "new_ptr",
            )
            .try_as_basic_value()
            .unwrap_left()
            .into_pointer_value();
        // Cast the pointer back to BackingStore*
        let new_ptr = builder.build_pointer_cast(
            new_ptr,
            backing_store_type.ptr_type(AddressSpace::Generic),
            "ptr-casted-to-backing-store-ptr-ptr",
        );

        // We need to set all newly allocated backing stores to zero
        let half_ptr =
            unsafe { builder.build_gep(new_ptr, &[old_size.into_int_value()], "half-ptr") };
        let half_ptr = builder.build_pointer_cast(
            half_ptr,
            context.i8_type().ptr_type(AddressSpace::Generic),
            "half-void-ptr",
        );
        let old_byte_size = builder.build_int_mul(
            old_size.into_int_value(),
            backing_store_type.size_of().unwrap(),
            "old-byte-size",
        );
        builder.build_call(
            builtin_functions["memset"],
            &[
                half_ptr.into(),
                context.i32_type().const_zero().into(),
                old_byte_size.into(),
            ],
            "",
        );

        let var_storage = builder
            .build_insert_value(var_storage, new_size, 0, "var-storage-with-new-size")
            .unwrap();
        let var_storage = builder
            .build_insert_value(var_storage, new_ptr, 1, "var-storage-with-new-ptr")
            .unwrap();
        builder.build_store(var_ptr, var_storage);
        builder.build_return(None);
    }

    fn build_builtin_function_allocate_backing_store(&mut self) {
        let context = self.context;
        let module = &self.module;
        let builder = &self.builder;
        let builtin_functions = &self.builtin_functions;
        let backing_store_type = &self.backing_store_type;

        let const_u64 = |val| context.i64_type().const_int(val, false);

        let var_ptr = module
            .get_global("variable_storage")
            .unwrap()
            .as_pointer_value();

        let func = module.add_function(
            "allocate_backing_store",
            backing_store_type
                .ptr_type(AddressSpace::Generic)
                .fn_type(&[], false),
            None,
        );
        let entry = context.append_basic_block(func, "entry");
        builder.position_at_end(entry);

        let var_storage = builder
            .build_load(var_ptr, "var-storage")
            .into_struct_value();
        let size = builder
            .build_extract_value(var_storage, 0, "size")
            .unwrap()
            .into_int_value();
        let backing_list = builder
            .build_extract_value(var_storage, 1, "backing-list")
            .unwrap()
            .into_pointer_value();
        let loop_var = const_u64(0);

        // create blocks
        let loop_block = context.append_basic_block(func, "loop");
        let loop_continued_block = context.append_basic_block(func, "loop-continued");
        let no_unallocated_backing_store_block =
            context.append_basic_block(func, "no-unallocated-backing-store");
        let unallocated_backing_store_found_block =
            context.append_basic_block(func, "unallocated-backing-store-found");

        builder.build_unconditional_branch(loop_block);

        // -- loop --

        builder.position_at_end(loop_block);
        let phi = builder.build_phi(context.i64_type(), "loop_var");
        phi.add_incoming(&[(&loop_var, entry)]);
        let loop_var = phi.as_basic_value().into_int_value();

        let is_at_end_of_list =
            builder.build_int_compare(IntPredicate::UGE, loop_var, size, "is-at-end-of-list");
        builder.build_conditional_branch(
            is_at_end_of_list,
            no_unallocated_backing_store_block,
            loop_continued_block,
        );

        // -- loop-continued --

        builder.position_at_end(loop_continued_block);
        let backing_store_ptr =
            unsafe { builder.build_gep(backing_list, &[loop_var], "backing-store-ptr") };
        let backing_store = builder
            .build_load(backing_store_ptr, "backing-store")
            .into_struct_value();
        let backing_store_count = builder
            .build_extract_value(backing_store, 0, "backing-store-count")
            .unwrap()
            .into_int_value();
        let is_unallocated = builder.build_int_compare(
            IntPredicate::EQ,
            backing_store_count,
            const_u64(0),
            "is-unallocated",
        );
        let incd_loop_val = builder.build_int_add(loop_var, const_u64(1), "incd-loop-var");
        phi.add_incoming(&[(&incd_loop_val, loop_continued_block)]);
        builder.build_conditional_branch(
            is_unallocated,
            unallocated_backing_store_found_block,
            loop_block,
        );

        // -- no-unallocated-backing-store --

        builder.position_at_end(no_unallocated_backing_store_block);
        builder.build_call(
            module.get_function("expand_variable_storage").unwrap(),
            &[],
            "",
        );
        builder.build_unconditional_branch(unallocated_backing_store_found_block);

        // -- unallocated-backing-store-found --

        builder.position_at_end(unallocated_backing_store_found_block);
        let backing_store_ptr =
            unsafe { builder.build_gep(backing_list, &[loop_var], "backing-store-ptr") };
        builder.build_return(Some(&backing_store_ptr));
    }

    // fn build_builtin_functions(&mut self) {
    //     let init_func = self.module.add_function(
    //         "init-variable-storage",
    //         self.context.void_type().fn_type(&[], false),
    //         None,
    //     );

    //     let entry = self.context.append_basic_block(init_func, "entry");
    //     self.builder.position_at_end(entry);

    //     // Start with 8 variable slots.
    //     let realloc = self.builtin_functions["realloc"];
    //     let varslot_size = self.backing_store_type.size_of().unwrap();
    //     let const_8 = self.context.i64_type().const_int(8, false);
    //     let byte_size = varslot_size.const_mul(const_8);
    //     let ptr = self.builder.build_call(
    //         realloc,
    //         &[
    //             self.context
    //                 .i8_type()
    //                 .ptr_type(AddressSpace::Generic)
    //                 .const_null()
    //                 .into(),
    //             byte_size.into(),
    //         ],
    //         "variable-storage-ptr",
    //     );
    //     let ptr = self.builder.build_cast(
    //         inkwell::values::InstructionOpcode::BitCast,
    //         ptr.try_as_basic_value().unwrap_left(),
    //         self.value_type
    //             .ptr_type(AddressSpace::Generic)
    //             .ptr_type(AddressSpace::Generic),
    //         "cast-ptr",
    //     );
    //     self.builder
    //         .build_alloca(self.variable_storage_type, "variable-store");
    //     let var_ptr = variable_storage.as_pointer_value();
    //     self.builder.build_store(var_ptr, ptr);
    //     self.builder.build_return(None);

    //     self.builtin_functions
    //         .insert("init-variable-storage", init_func);
    // }

    fn build_preamble(&mut self) {}

    fn build_func(&mut self, id: Id<Function>, function: &'s Function<'comp, 'a>) {
        // let ty = self.value_type.fn_type(
        //     &function
        //         .arguments
        //         .iter()
        //         .map(|_| self.value_type.into())
        //         .collect::<Vec<_>>(),
        //     false,
        // );
        // let func = self.module.add_function(&format!("func-{id}"), ty, None);
        // let entry = self.context.append_basic_block(func, "entry");
        // self.builder.position_at_end(entry);
        // self.visit_expr(function.expr);
    }

    fn build_nil_value(&mut self) {}
}

impl<'s, 'comp, 'a> Visitor<'comp, 'a, ()> for FunctionBuilder<'s, 'comp, 'a> {
    fn visit_expr_nil(&mut self, expr: &'comp Expression<'a>) -> () {
        self.build_nil_value();
    }
}

struct FunctionIdAllocator<'s, 'comp, 'a> {
    compiler: &'s mut Compiler<'comp, 'a>,
    id_gen: &'s mut IdGen,
}

impl<'s, 'comp, 'a> Visitor<'comp, 'a, ()> for FunctionIdAllocator<'s, 'comp, 'a> {
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

impl<'s, 'comp, 'a> Visitor<'comp, 'a, ()> for VariableAllocator<'s, 'comp, 'a> {
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

impl<'s, 'comp, 'a> Visitor<'comp, 'a, ()> for VariableIdCollector<'s, 'comp, 'a> {
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

trait Combine {
    fn empty() -> Self;
    fn combine(self, other: Self) -> Self;
}

impl Combine for () {
    fn empty() -> Self {
        ()
    }

    fn combine(self, other: Self) -> Self {
        self
    }
}

trait Visitor<'comp, 'a, Ret: Combine> {
    fn visit_where_clause(&mut self, where_clause: &'comp WhereClause<'a>) -> Ret {
        self.walk_where_clause(where_clause)
    }
    fn walk_where_clause(&mut self, where_clause: &'comp WhereClause<'a>) -> Ret {
        let mut ret = Ret::empty();
        for stmnt in &where_clause.stmnts {
            ret = ret.combine(self.visit_stmnt(stmnt))
        }
        ret
    }

    fn visit_stmnt(&mut self, stmnt: &'comp Statement<'a>) -> Ret {
        self.walk_stmnt(stmnt)
    }
    fn walk_stmnt(&mut self, stmnt: &'comp Statement<'a>) -> Ret {
        let a = self.visit_ident(stmnt.name);
        let b = self.visit_expr(&stmnt.expr);
        a.combine(b)
    }

    fn visit_ident(&mut self, _ident: T<&str>) -> Ret {
        Ret::empty()
    }

    fn visit_expr(&mut self, expr: &'comp Expression<'a>) -> Ret {
        self.walk_expr(expr)
    }
    fn walk_expr(&mut self, expr: &'comp Expression<'a>) -> Ret {
        let mut ret = Ret::empty();
        if let Some(w) = &expr.where_clause {
            ret = ret.combine(self.visit_where_clause(w));
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

    fn visit_expr_nil(&mut self, expr: &'comp Expression<'a>) -> Ret {
        Ret::empty()
    }

    fn visit_expr_literal(&mut self, expr: &'comp Expression<'a>, val: &T<BigInt>) -> Ret {
        Ret::empty()
    }

    fn visit_expr_variable(&mut self, expr: &'comp Expression<'a>, var: &T<&str>) -> Ret {
        Ret::empty()
    }

    fn visit_expr_lambda(
        &mut self,
        expr: &'comp Expression<'a>,
        bs: &T<()>,
        params: &Vec<T<&'a str>>,
        arrow: &T<()>,
        body: &'comp Box<Expression<'a>>,
    ) -> Ret {
        self.walk_expr_lambda(expr, bs, params, arrow, body)
    }
    fn walk_expr_lambda(
        &mut self,
        expr: &'comp Expression<'a>,
        bs: &T<()>,
        params: &Vec<T<&'a str>>,
        arrow: &T<()>,
        body: &'comp Box<Expression<'a>>,
    ) -> Ret {
        let mut ret = Ret::empty();
        for param in params {
            ret = ret.combine(self.visit_ident(*param));
        }
        ret.combine(self.visit_expr(body))
    }

    fn visit_expr_lambda_param(&mut self, param: T<&str>) -> Ret {
        self.walk_expr_lambda_param(param)
    }
    fn walk_expr_lambda_param(&mut self, param: T<&str>) -> Ret {
        self.visit_ident(param)
    }

    fn visit_expr_unary(
        &mut self,
        expr: &'comp Expression<'a>,
        op: &T<UnaryOp>,
        inner: &'comp Box<Expression<'a>>,
    ) -> Ret {
        self.walk_expr_unary(expr, op, inner)
    }
    fn walk_expr_unary(
        &mut self,
        expr: &'comp Expression<'a>,
        op: &T<UnaryOp>,
        inner: &'comp Box<Expression<'a>>,
    ) -> Ret {
        self.visit_expr(inner)
    }

    fn visit_expr_binary(
        &mut self,
        expr: &'comp Expression<'a>,
        left: &'comp Box<Expression<'a>>,
        op: &T<BinaryOp>,
        right: &'comp Box<Expression<'a>>,
    ) -> Ret {
        self.walk_expr_binary(expr, left, op, right)
    }
    fn walk_expr_binary(
        &mut self,
        expr: &'comp Expression<'a>,
        left: &'comp Box<Expression<'a>>,
        op: &T<BinaryOp>,
        right: &'comp Box<Expression<'a>>,
    ) -> Ret {
        let a = self.visit_expr(left);
        let b = self.visit_expr(right);
        a.combine(b)
    }

    fn visit_expr_funccall(
        &mut self,
        expr: &'comp Expression<'a>,
        func: &'comp Box<Expression<'a>>,
        args: &'comp Vec<Expression<'a>>,
        rbracket: &T<()>,
    ) -> Ret {
        self.walk_expr_funccall(expr, func, args, rbracket)
    }
    fn walk_expr_funccall(
        &mut self,
        expr: &'comp Expression<'a>,
        func: &'comp Box<Expression<'a>>,
        args: &'comp Vec<Expression<'a>>,
        rbracket: &T<()>,
    ) -> Ret {
        let mut ret = self.visit_expr(func);
        for arg in args {
            ret = ret.combine(self.visit_expr(arg));
        }
        ret
    }

    fn visit_expr_if(
        &mut self,
        expr: &'comp Expression<'a>,
        kw_if: &T<()>,
        cond: &'comp Box<Expression<'a>>,
        case_true: &'comp Box<Expression<'a>>,
        case_false: &'comp Box<Expression<'a>>,
        kw_end: &T<()>,
    ) -> Ret {
        self.walk_expr_if(expr, kw_if, cond, case_true, case_false, kw_end)
    }
    fn walk_expr_if(
        &mut self,
        expr: &'comp Expression<'a>,
        kw_if: &T<()>,
        cond: &'comp Box<Expression<'a>>,
        case_true: &'comp Box<Expression<'a>>,
        case_false: &'comp Box<Expression<'a>>,
        kw_end: &T<()>,
    ) -> Ret {
        let a = self.visit_expr(cond);
        let b = self.visit_expr(case_true);
        let c = self.visit_expr(case_false);
        a.combine(b.combine(c))
    }

    fn visit_expr_sequence(
        &mut self,
        expr: &'comp Expression<'a>,
        exprs: &'comp Vec<Expression<'a>>,
    ) -> Ret {
        self.walk_expr_sequence(expr, exprs)
    }
    fn walk_expr_sequence(
        &mut self,
        expr: &'comp Expression<'a>,
        exprs: &'comp Vec<Expression<'a>>,
    ) -> Ret {
        let mut ret = Ret::empty();
        for expr in exprs {
            ret = ret.combine(self.visit_expr(expr));
        }
        ret
    }
}
