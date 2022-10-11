#![feature(let_chains)]
#![feature(iterator_try_collect)]
#![feature(iter_intersperse)]
#![feature(try_blocks)]

use std::{f32::consts::E, io::Write};

use ast::VariableId;
// use bytecode::generator::generate;
// use executor::Executor;
use rustyline::{error::ReadlineError, Editor};

mod ast;
// mod executor;
mod parser;
// mod bytecode;
mod compiler;

fn main() {
    miette::set_panic_hook();

    // let mut editor = Editor::<()>::new().unwrap();
    // loop {
    //     match editor.readline(">> ") {
    //         Ok(line) => {
    //             editor.add_history_entry(&line);
    //             match parser::expr_parser::expr(&line) {
    //                 Ok(v) => {
    //                     let mut executor = Executor::new();
    //                     match executor.execute(&v) {
    //                         Ok(v) => if let executor::Value::Nil = v {} else { println!("{v}") },
    //                         Err(e) => println!("{:?}", e.with_source_code(line)),
    //                     }
    //                 },
    //                 Err(e) => println!("{}", e),
    //             }
    //         }
    //         Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => break,
    //         Err(e) => {
    //             println!("Error: {}", e)
    //         }
    //     }
    // }

    let s = std::fs::read_to_string("test.intlang").unwrap();

    match parser::expr_parser::expr(&s) {
        Ok(mut v) => {
            compiler::Compiler::compile(&mut v);
            // generate(&mut v);
            // let mut executor = Executor::new();
            // match executor.execute(&v) {
            //     Ok(v) => println!("{v}"),
            //     Err(e) => println!("{:?}", e.with_source_code(s.to_string())),
            // }
        },
        Err(e) => println!("{}", e),
    }

    // let s = std::fs::read_to_string("test.txt").unwrap();

    // match bytecode::parser::text_parser::program(&s) {
    //     Ok(v) => {
    //         println!("{v:?}");
    //         let mut e = bytecode::Executor::new(v);
    //         e.run();
    //     },
    //     Err(e) => println!("{e}"),
    // }
}
