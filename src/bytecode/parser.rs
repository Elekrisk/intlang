use std::mem::size_of;

use super::*;
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;

peg::parser! {
    pub grammar bytecode_parser() for [u8] {

        rule list<T>(x: rule<T>) -> Vec<T>
        = length:usize() vals:x ()*<{length}> { vals }

        rule bytelist() -> &'input [u8]
        = n:usize() a:$([_]*<{n}>) {a}

        rule usize() -> usize
        = a:$([_]*<{size_of::<usize>()}>) { usize::from_le_bytes(a.try_into().unwrap()) }
        rule isize() -> isize
        = a:$([_]*<{size_of::<isize>()}>) { isize::from_le_bytes(a.try_into().unwrap()) }

        rule sign() -> Sign
        = [0] { Sign::Minus } / [1] { Sign::NoSign } / [2] { Sign::Plus }

        rule number() -> BigInt
        = s:sign() bytes:bytelist() { BigInt::from_bytes_le(s, bytes) }

        rule instr() -> Instruction
        = [0] { Instruction::PushNil }
        / [1] val:number() { Instruction::PushInteger(val) }
        / [2] id:usize() captures:list(<v:usize() {LocalValueId(v)}>) {
            Instruction::PushFunction { function: FunctionId(id), captures }
        }
        / [3] id:usize() { Instruction::SetVariable(LocalValueId(id)) }
        / [4] id:usize() { Instruction::GetVariable(LocalValueId(id)) }
        / [5] id:usize() { Instruction::GetGlobal(GlobalValueId(id)) }
        / [6] { Instruction::Pop }
        / [7] { Instruction::Add }
        / [8] { Instruction::Sub }
        / [9] { Instruction::Mul }
        / [10] { Instruction::Div }
        / [11] { Instruction::Mod }
        / [12] offset:isize() { Instruction::If(offset) }
        / [13] offset:isize() { Instruction::Jump(offset) }
        / [14] arg_count:usize() { Instruction::Call(arg_count) }
        / [255] { Instruction::Ret }

        rule function() -> Function
        = id:usize() arg_count:usize() var_count:usize() capture_count:usize() body:list(<instr()>) {
            Function { id: FunctionId(id), arg_count, var_count, capture_count, body: FunctionBody::Lambda(body) }
        }

        pub rule program() -> Program
        = ff:function()* { Program { entry: ff.first().unwrap().id, functions: {
            let mut fs = FunctionStorage::new();
            for f in ff {
                fs.add(f);
            }
            fs
        }, values: ValueAllocator::new() } }
    }
}

peg::parser! {
    pub grammar text_parser() for str {

        rule _() -> ()
        = quiet!{(
            c:[_] {? if c.is_whitespace() { Ok(()) } else { Err("whitespace") }}
            / "#" [^ '\n']*
        )* {}}

        rule list<T>(x: rule<T>) -> Vec<T>
        = vals:x()**(_ "," _) { vals }

        rule usize() -> usize
        = n:number() {? n.to_usize().ok_or("usize") }
        rule isize() -> isize
        = n:number() {? n.to_isize().ok_or("isize") }

        rule number() -> BigInt
        = n:$("-"? ['0'..='9']+) { n.parse().unwrap() }

        rule ci(word: &'static str) -> ()
        = n:$((c:[_] {? if c.is_alphabetic() || c == '_' { Ok(()) } else { Err("") }})+) {?
            if n.to_lowercase() == word.to_lowercase() {
                Ok(())
            } else {
                Err(word)
            }
        }

        rule arg<T>(name: &'static str, value: rule<T>) -> T
        = _ ci(name) _ ":" _ v:value() {v}

        rule instr() -> Instruction
        = ci("PushNil") { Instruction::PushNil }
        / ci("PushInteger") val:arg("value", <number()>) { Instruction::PushInteger(val) }
        / ci("PushFunction") id:arg("id", <usize()>) captures:arg("captures", <list(<val:usize() {LocalValueId(val)}>)>) {
            Instruction::PushFunction { function: FunctionId(id), captures }
        }
        / ci("SetVariable") id:arg("id", <usize()>) { Instruction::SetVariable(LocalValueId(id)) }
        / ci("GetVariable") id:arg("id", <usize()>) { Instruction::GetVariable(LocalValueId(id)) }
        / ci("GetGlobal") id:arg("id", <usize()>) { Instruction::GetGlobal(GlobalValueId(id)) }
        / ci("Pop") { Instruction::Pop }
        / ci("Add") { Instruction::Add }
        / ci("Sub") { Instruction::Sub }
        / ci("Mul") { Instruction::Mul }
        / ci("Div") { Instruction::Div }
        / ci("Mod") { Instruction::Mod }
        / ci("If") offset:arg("offset", <isize()>) { Instruction::If(offset) }
        / ci("Jump") offset:arg("offset", <isize()>) { Instruction::Jump(offset) }
        / ci("Call") arg_count:arg("argumentcount", <usize()>) { Instruction::Call(arg_count) }
        / ci("Ret") { Instruction::Ret }

        rule function() -> Function
        = _ ci("Function") _ "[" _ id:usize() _ "]" _ arg_count:arg("argumentcount", <usize()>) var_count:arg("variablecount", <usize()>) capture_count:arg("capturecount", <usize()>) _ "{" _ body:(i:instr() _ {i})* _ "}" _ {
            Function { id: FunctionId(id), arg_count, var_count, capture_count, body: FunctionBody::Lambda(body) }
        }

        pub rule program() -> Program
        = ff:function()* { Program { entry: ff.first().map(|f| f.id).unwrap_or(FunctionId(0)), functions: {
            let mut fs = FunctionStorage::new();
            for f in ff {
                fs.add(f);
            }
            fs
        }, values: ValueAllocator::new() } }
    }
}
