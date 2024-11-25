use std::{io, process::exit};

use ast_transform_ops::{sfn_add_scopes, sfn_deinit, sfn_track_block_id, sfn_noop, efn_constant_fold, efn_noop, efn_unscoped_to_scoped, sfn_lift_global_into_fn, ffn_noop, ffn_create_main, sfn_ensure_fn_params_are_local_scoped, efn_ensure_fn_params_are_local_scoped, sfn_track_type_assignments, efn_update_variable_types, sfn_update_assignments};
use llvm_transform_ops::{sfn_stmt_to_llvm, ffn_add_runtime};
use test_fixtures::{get_all_features_program, get_factorial_program_text};

mod tree_walker;
mod print;
mod ast_transform_ops;
mod llvm_transform_ops;
mod test_fixtures;
mod parser;

extern crate pest;
extern crate pest_derive;
use pest_derive::Parser;

#[macro_use]
extern crate hexf;

#[derive(Parser)]
#[grammar = "wab.pest"]
pub struct WabParser;

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp { Add, Mul, Sub, Div, Lt, Gt, Lte, Gte, Eq }

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt { 
    Print(Expr),
    Assign(Expr, Expr),
    Alloc(Expr, VarType),
    AssignDeclare(Expr, Expr),
    MultiStmt(Vec<Stmt>),
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    Scope(u32),
    EndScope(u32),
    While(Expr, Vec<Stmt>),
    Fn(String, Vec<Expr>, Vec<Stmt>, VarType),
    Return(Expr),
    Call(String, Vec<Expr>),
    Noop(),
    Comment(String),
    LLVM(String, Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(BinOp, Vec<Expr>, VarType),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    UnscopedVar(String, VarType),
    ScopedVar(u32, String, VarType),
    GlobalVar(String, VarType),
    Call(String, Vec<Expr>, VarType),
    None()
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    I, //Integer
    F, // Float
    U
}

#[derive(Debug, PartialEq)]
pub struct Program {
    statements: Vec<Stmt>
}

pub struct Wab {}

type SFN = dyn Fn(&Stmt) -> Option<Stmt>;
type FFN = dyn Fn(&Vec<Stmt>) -> Vec<Stmt>;
type EFN = dyn Fn(&Expr) -> Option<Expr>;
pub struct Stage<'a> {
    name: &'a str,
    stmt_fn: &'a SFN,
    expr_fn: &'a EFN,
    final_fn: &'a FFN,
    converge: bool,
    debug: bool,
    reset: bool
}

fn compile(statements:&Vec<Stmt>) -> Vec<Stmt> {

    let stages:Vec<Stage> = vec![
        Stage{name: "✅ add scope annotations", stmt_fn: &sfn_add_scopes, expr_fn:&efn_noop, final_fn: &ffn_noop, converge:false, debug:false, reset:false},
        Stage{name: "✅ separate initialization from assignment", stmt_fn: &sfn_deinit, expr_fn:&efn_noop, final_fn: &ffn_noop,converge:false, debug:false, reset:false},
        Stage{name: "✅ resolve variable scopes", stmt_fn: &sfn_track_block_id, expr_fn:&efn_unscoped_to_scoped, final_fn: &ffn_noop,converge:false, debug:false, reset:true},
        Stage{name: "🔧 BUG FIX: (Statement) ensure fn parameters have local scopes", stmt_fn: &sfn_ensure_fn_params_are_local_scoped, expr_fn:&efn_noop, final_fn: &ffn_noop,converge:false, debug:false, reset:false},
        Stage{name: "🔧 BUG FIX: (Expressions) ensure fn parameters have local scopes", stmt_fn: &sfn_track_block_id, expr_fn:&efn_ensure_fn_params_are_local_scoped, final_fn: &ffn_noop,converge:false, debug:false, reset:false},
        Stage{name: "✅ constant folding", stmt_fn: &sfn_noop, expr_fn:&efn_constant_fold, final_fn: &ffn_noop,converge:true, debug:false, reset:false},
        Stage{name: "✅ create main from globals", stmt_fn: &sfn_lift_global_into_fn, expr_fn:&efn_noop, final_fn: &ffn_create_main, converge:false, debug:false, reset:true},
        Stage{name: "✅ Types Phase 1: infer types on assignments", stmt_fn: &sfn_track_type_assignments, expr_fn:&efn_noop, final_fn: &ffn_noop, converge:false, debug:true, reset:true},
        Stage{name: "✅ Types Phase 2: set types on variables", stmt_fn: &sfn_noop, expr_fn:&efn_update_variable_types, final_fn: &ffn_noop, converge:false, debug:true, reset:false},
        Stage{name: "✅ Types Phase 3: infer types on assignments", stmt_fn: &sfn_track_type_assignments, expr_fn:&efn_noop, final_fn: &ffn_noop, converge:false, debug:true, reset:true},
        Stage{name: "✅ Types Phase 4: set types on remaining statements", stmt_fn: &sfn_update_assignments, expr_fn:&efn_noop, final_fn: &ffn_noop, converge:false, debug:true, reset:false},
        Stage{name: "✅ Types Phase 5: set types on variables", stmt_fn: &sfn_noop, expr_fn:&efn_update_variable_types, final_fn: &ffn_noop, converge:false, debug:true, reset:false},


        Stage{name: "✅ create llvm ir", stmt_fn: &sfn_stmt_to_llvm, expr_fn:&efn_noop, final_fn: &ffn_add_runtime, converge:false, debug:true, reset:false}

        ];
    let mut current:Vec<Stmt> = statements.to_vec();
    Wab::pretty_print(Program{ statements: current.clone() }, format!("AST INPUT"));
    
    for stage in stages {
        if stage.debug {
            Wab::pretty_print(Program{ statements: current.clone() }, format!("AST BEFORE STAGE: {}", stage.name));
        }
        current = Wab::compile_stage(&current, stage);
    }

    return current;
}

fn main() {
    let stdin = io::read_to_string(io::stdin());

    match stdin {
        Ok(input) => {
            let ast = Wab::parse(input);
            if ast.len() == 0 {
                exit(1);
            }
            let program = Program{statements: ast};
            let result: Vec<Stmt> = compile(&program.statements.to_vec());
            Wab::pretty_print(Program{ statements: result }, "LLVM IR".to_string() );

        },
        Err(_) => {
            eprintln!("Pipe a file")
        },
    }
    //let program = get_all_features_program();
    //let input = get_factorial_program_text();
    //let ast = Wab::parse(input);
    //let program = Program{statements: ast};
    //Wab::pretty_print(Program{ statements: program.statements.to_vec() } );
    //let result: Vec<Stmt> = compile(&program.statements.to_vec());
    //Wab::pretty_print(Program{ statements: result }, "LLVM IR".to_string() );
}
