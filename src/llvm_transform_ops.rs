use std::{cell::Cell, vec, num::IntErrorKind};
use crate::{Expr, BinOp, print, VarType};
use super::Stmt;

use hexf::hexf64;



thread_local! {
    static LLVM_ID: Cell<u32> = Cell::new(0);
}

fn get_id() -> u32 {
    let mut result = 0;
    LLVM_ID.with(|it| {
        it.set(it.get().wrapping_add(1));
        result = it.get();
    });
    return result;
}

fn get_scoped_var(scope:&u32, name:&String) -> String {
    return format!("%.{:?}_{:}", scope, name)
}

fn get_global_var(name:&String) -> String {
    return format!("@.{:}", name)
}

fn get_binop_llvm(b:&BinOp, vt:&VarType) -> String {

    match ( b, vt ) {
        (BinOp::Add, VarType::I) => "add".to_string(),
        (BinOp::Mul, VarType::I) => "mul".to_string(),
        (BinOp::Sub, VarType::I) => "sub".to_string(),
        (BinOp::Div, VarType::I) => "div".to_string(),
        (BinOp::Lt, VarType::I) => "slt".to_string(),
        (BinOp::Gt, VarType::I) => "sgt".to_string(),
        (BinOp::Lte, VarType::I) => "lte".to_string(),
        (BinOp::Gte, VarType::I) => "gte".to_string(),
        (BinOp::Eq, VarType::I) => "eq".to_string(),
        (BinOp::Add, VarType::F) => "fadd".to_string(),
        (BinOp::Mul, VarType::F) => "fmul".to_string(),
        (BinOp::Sub, VarType::F) => "fsub".to_string(),
        (BinOp::Div, VarType::F) => "fdiv".to_string(),
        (BinOp::Lt, VarType::F) => "olt".to_string(),
        (BinOp::Gt, VarType::F) => "ogt".to_string(),
        (BinOp::Lte, VarType::F) => "ole".to_string(),
        (BinOp::Gte, VarType::F) => "oge".to_string(),
        (BinOp::Eq, VarType::F) => "oeq".to_string(),
        _ => panic!("No operation for {:?} {:?}", b, vt)
    }
}

fn getcmp_string(vt:&VarType) -> String {
    match vt {
        VarType::I => return "icmp".to_string(),
        VarType::F => return "fcmp".to_string(),
        VarType::U => todo!(),
    }
}

fn get_typestr(vt:&VarType) -> String {
    match vt {
        VarType::I => "i32".to_string(),
        VarType::F  => "double".to_string(),
        _ => panic!("I don't understand"),
    }
}

// Convert an expression to an llvm statement, with the result stored in
// the second part of the tuple
fn _expression_to_llvm_stmt(e:&Expr) -> (Stmt, String) {
    let result: String = format!("%.{}", get_id());
    let mut comment = format!("{:?}", e);
    comment.truncate(30);
    match e {
        Expr::Binary(op, args, vt) => {
            let (lhs_stmt, lhs_result) = _expression_to_llvm_stmt(&args[0]);
            let (rhs_stmt, rhs_result) = _expression_to_llvm_stmt(&args[1]);
            let binstr = get_binop_llvm(&op, vt);
            let cmpstr = getcmp_string(vt);
            let typestr = get_typestr(vt);
            let bin_stmt = match op {
                BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte | BinOp::Eq => Stmt::LLVM(format!("    {} = {} {} {} {}, {}", result, cmpstr, binstr, typestr, lhs_result, rhs_result), vec![Stmt::Comment(comment)]),
                BinOp::Add | BinOp::Mul | BinOp::Sub | BinOp::Div => Stmt::LLVM(format!("    {} = {} {} {}, {}", result, binstr, typestr, lhs_result, rhs_result), vec![Stmt::Comment(comment)]),

            };
            return (Stmt::MultiStmt(
                vec![lhs_stmt, rhs_stmt, bin_stmt]
            ), result)
        },
        Expr::IntegerLiteral(i) => {
            let alloc_stmt = Stmt::LLVM(format!("    {} = alloca i32", result), vec![]);
            let store_stmt = Stmt::LLVM(format!("    store i32 {}, i32* {}", i, result), vec![Stmt::Comment(comment)]);
            let local_name = format!("%.{}", get_id());
            let load_stmt = Stmt::LLVM(format!("    {} = load i32, i32* {}", local_name, result), vec![]);
            return ( Stmt::MultiStmt(vec![alloc_stmt, store_stmt, load_stmt]), local_name)
        },
        Expr::FloatLiteral(f) => { 
            {
                let alloc_stmt = Stmt::LLVM(format!("    {} = alloca double", result), vec![]);
                let store_stmt = Stmt::LLVM(format!("    store double {}, double* {}", f, result), vec![Stmt::Comment(comment)]);
                let local_name = format!("%.{}", get_id());
                let load_stmt = Stmt::LLVM(format!("    {} = load double, double* {}", local_name, result), vec![]);
                return ( Stmt::MultiStmt(vec![alloc_stmt, store_stmt, load_stmt]), local_name)
            }
        },
        Expr::UnscopedVar(_, vt) => panic!("We don't support transmuting {:?} into llvm", e),
        Expr::ScopedVar(s, name, vt) => {
            let typestr: String = get_typestr(vt);
            return ( Stmt::LLVM(format!("    {} = load {}, {}* {}", result, typestr, typestr, get_scoped_var(s, name)), vec![Stmt::Comment(comment)]), result);
        },
        Expr::GlobalVar(name, vt) => {
            let typestr = get_typestr(vt);
            return ( Stmt::LLVM(format!("    {} = load {}, {}* {}    ", result,  typestr, typestr, get_global_var(name)), vec![Stmt::Comment(comment)]), result)},
        Expr::Call(name, params, vt) => {
            if params.len() == 1 {
                let (param_stmt, param_name) = _expression_to_llvm_stmt(&params[0]);
                let call_stmt = Stmt::LLVM(format!("    {} = call i32 (i32) @{}(i32 {})", result, name, param_name), vec![Stmt::Comment(comment)]);
                return (Stmt::MultiStmt(vec![param_stmt, call_stmt]), result)
            } else {
                return (Stmt::LLVM(format!("    {} = call i32 (i32) @{}()", result, name), vec![Stmt::Comment(comment)]), result);

            }
        },
        Expr::None() => panic!("None shall pass")
    }
}


pub fn ffn_add_runtime(statements: &Vec<Stmt>) -> Vec<Stmt> {
    let mut results: Vec<Stmt> = statements.clone();
    results.push(Stmt::LLVM("declare i32 @_print_int(i32)".to_string(), vec![]));
    results.push(Stmt::LLVM("declare i32 @_print_double(double)".to_string(), vec![]));

    return results;
}

pub fn sfn_stmt_to_llvm(s: &Stmt) -> Option<Stmt> {
    let mut comment = format!("{:?}", s);
    comment.truncate(30);
    match s {
        Stmt::Print(e) => {
            let mut print_stmts = vec![];
            let mut vartype = VarType::I;
            let print_val = match e {
                crate::Expr::ScopedVar(scope, name, t) => {
                    vartype = t.clone();
                    get_scoped_var(scope, name)
                },
                crate::Expr::GlobalVar(name, t) => {
                    vartype = t.clone();
                    get_global_var(name)
                },
                crate::Expr::IntegerLiteral(val) => { vartype = VarType::I; format!("{}", val) },
                crate::Expr::Call(_fnname, _val, vt) => { 
                    vartype = vt.clone();
                    let (expr_stmt, expr_result) = _expression_to_llvm_stmt(e);
                    print_stmts.push(expr_stmt);
                    format!("{}", expr_result)
                },
                _ =>  {
                    let (expr_stmt, expr_result) = _expression_to_llvm_stmt(e);
                    print_stmts.push(expr_stmt);
                    format!("{}", expr_result)
                },
            };
            let typestr = get_typestr(&vartype);
            if print_stmts.len() == 0 {
                let load_local: String = format!("%.{}", get_id());
                let load_stmt = Stmt::LLVM(format!("    {} = load {}, {}* {}", load_local, typestr, typestr, print_val), vec![]);
                print_stmts.push(load_stmt);
            }
            let mut call_stmt;
            if vartype == VarType::I {
                call_stmt = Stmt::LLVM(format!("    call i32 (i32) @_print_int(i32 {})", print_val), vec![]);
                print_stmts.push(call_stmt);
            }
            if vartype == VarType::F {
                call_stmt = Stmt::LLVM(format!("    call double (double) @_print_double(double {})", print_val), vec![]);
                print_stmts.push(call_stmt);
            }

            if vartype == VarType::U {
                panic!("Can't print unknown");
            }


            return Some(Stmt::MultiStmt(print_stmts))
        },
        Stmt::Assign(variable, expr) => {
            let (expr_stmt, expr_result) = _expression_to_llvm_stmt(expr);
            match variable {
                Expr::ScopedVar(scope, name, vt) =>  {
                    let var_name = get_scoped_var(scope, name);
                    let assign_stmt = Stmt::LLVM(format!("    store i32 {}, i32* {}",  expr_result, var_name), vec![Stmt::Comment(comment)]);
                    return Some(Stmt::MultiStmt(vec![expr_stmt, assign_stmt]))
                },
                Expr::GlobalVar(name, vt) => {
                    let var_name = get_global_var(name); 
                    let assign_stmt = Stmt::LLVM(format!("    store i32 {}, i32* {}",  expr_result, var_name), vec![Stmt::Comment(comment)]);
                    return Some(Stmt::MultiStmt(vec![expr_stmt, assign_stmt]))
                },
                _ => panic!("Can't handle variables other than global or local in assign {:?}", s),
            };

        },
        Stmt::Alloc(e, vt) => {
            match e {
                crate::Expr::ScopedVar(scope, name, vt) => {
                    let typestr = get_typestr(vt);
                    return Some(Stmt::LLVM(format!("    {} = alloca {}", get_scoped_var(scope, name), typestr), vec![Stmt::Comment(comment)]))
                },
                crate::Expr::GlobalVar(name, vt) => {
                    {
                        let typestr = get_typestr(vt);
                        let mut initstr = "0";
                        if typestr == "double" {
                            initstr = "0.000000e+00"
                        }
                        return Some(Stmt::LLVM(format!("{} = global {} {}", get_global_var(name), typestr, initstr), vec![Stmt::Comment(comment)]))
                    }
                },
                _ => panic!("LLVM transformation can't handle {:?}", s)
            }
        },
        Stmt::MultiStmt(inner) => {
            let mut block = vec![];
            for stmt in inner {
                match sfn_stmt_to_llvm(stmt) {
                    Some(llvm_inner) => block.push(llvm_inner),
                    None => panic!("Unable to translate {:?}", stmt)
                }
            }
            return Some(Stmt::MultiStmt(block))
        },
        Stmt::If(e, t, f) => {
            let (cmp_stmt, cmp_result) = _expression_to_llvm_stmt(e);
            let true_label = format!("L{:}", get_id());
            let false_label = format!("L{:}", get_id());
            let end_label = format!("L{:}", get_id());

            let branch = format!("    br i1 {}, label %{}, label %{}", cmp_result, true_label, false_label);
            let br_stmt = Stmt::LLVM(branch, vec![Stmt::Comment(comment)]);
            let true_label_stmt = Stmt::LLVM(format!("{:}:", true_label), vec![]);
            let false_label_stmt = Stmt::LLVM(format!("{:}:", false_label), vec![]);
            let end_label_stmt = Stmt::LLVM(format!("{:}:", end_label), vec![]);
            let mut true_block = vec![];
            for true_stmt in t {
                match sfn_stmt_to_llvm(true_stmt) {
                    Some(inner) => true_block.push(inner),
                    None => panic!("Unable to translate {:?}", true_stmt)
                }
            }
            true_block.push(Stmt::LLVM(format!("    br label %{:}", end_label), vec![]));
            let mut false_block = vec![];
            for false_stmt in f {
                match sfn_stmt_to_llvm(false_stmt) {
                    Some(inner) => false_block.push(inner),
                    None => panic!("Unable to translate {:?}",false_stmt)
                }
            }
            false_block.push(Stmt::LLVM(format!("    br label %{:}", end_label), vec![]));
            let true_block_stmt = Stmt::MultiStmt(true_block);
            let false_block_stmt = Stmt::MultiStmt(false_block);
            return Some(Stmt::MultiStmt(vec![cmp_stmt, br_stmt, true_label_stmt, true_block_stmt, false_label_stmt, false_block_stmt, end_label_stmt])); 
        },
        Stmt::Scope(_) => Some(Stmt::LLVM("".to_string(), vec![Stmt::Comment(comment)])),
        Stmt::EndScope(_) => Some(Stmt::LLVM("".to_string(), vec![Stmt::Comment(comment)])),
        Stmt::While(e, statements) => {
            let begin_while_label = format!("L{:}", get_id());
            let begin_while_stmt = Stmt::LLVM(format!("{:}:", begin_while_label), vec![Stmt::Comment(comment)]);
            let goto_begin_while_stmt = Stmt::LLVM(format!("    br label %{:}", begin_while_label),  vec![]);
            let (cmp_stmt, cmp_result) = _expression_to_llvm_stmt(e);
            let true_label = format!("L{:}", get_id());
            let end_while_label = format!("L{:}", get_id());
            let true_label_stmt = Stmt::LLVM(format!("{:}:", true_label),  vec![]);
            let end_while_label_stmt = Stmt::LLVM(format!("{:}:", end_while_label),  vec![]);
            let branch = format!("    br i1 {}, label %{}, label %{}", cmp_result, true_label, end_while_label);
            let br_stmt = Stmt::LLVM(branch,  vec![]);

            let mut true_block = vec![];
            for true_stmt in statements {
                match sfn_stmt_to_llvm(true_stmt) {
                    Some(inner) => true_block.push(inner),
                    None => panic!("Unable to translate {:?}", true_stmt)
                }
            }
            true_block.push(goto_begin_while_stmt.clone());
            let true_block_stmt = Stmt::MultiStmt(true_block);
            return Some(Stmt::MultiStmt(vec![goto_begin_while_stmt, begin_while_stmt, cmp_stmt, br_stmt, true_label_stmt, true_block_stmt, end_while_label_stmt])); 


        },
        Stmt::Fn(name, params, statements, vt) => {
            //let arg:Vec<(Stmt, String)> = params.into_iter().map(|p| _expression_to_llvm_stmt(p)).collect();
            let mut fn_block = vec![];
            if params.len() > 1 {
                panic!("cannot translate fn with more than one arg")
            }
            if params.len() == 1 {
                let (arg, arg_name_raw) = match &params[0] {
                    crate::Expr::ScopedVar(scope, name, vt) => (get_scoped_var(&scope, &name), name),
                    _ => panic!("LLVM transformation can't handle {:?}", s),
                };
                let arg_scoped_var = get_scoped_var(&0, &format!("{:}{:}", arg_name_raw, get_id()));
                fn_block.push(Stmt::LLVM(format!("define i32 @{}(i32 {}) {{", name, arg_scoped_var),  vec![Stmt::Comment(comment)]));
                fn_block.push(Stmt::LLVM(format!("    {} = alloca i32", arg),  vec![]));
                fn_block.push(Stmt::LLVM(format!("    store i32 {}, i32* {}", arg_scoped_var, arg),  vec![]));
            


            } else {
                fn_block.push(Stmt::LLVM(format!("define i32 @{}() {{", name),  vec![Stmt::Comment(comment)]));
            }
            
            for fn_stmt in statements {
                match sfn_stmt_to_llvm(fn_stmt) {
                    Some(inner) => fn_block.push(inner),
                    None => panic!("Unable to translate {:?}",fn_stmt)
                }
            }
            fn_block.push(Stmt::LLVM(format!("}}"),  vec![]));
            let fn_block_stmt = Stmt::MultiStmt(fn_block);
            return Some(fn_block_stmt)
        },
        Stmt::Return(e) => {
            let (expr_stmt, expr_result) = _expression_to_llvm_stmt(e);
            let ret_stmt = Stmt::LLVM(format!("    ret i32 {}", expr_result),  vec![Stmt::Comment(comment)]);
            return Some(Stmt::MultiStmt(vec![expr_stmt, ret_stmt]))
        },
        Stmt::Call(name, params) => {
            if params.len() == 1 {
                let (param_stmt, param_name) = _expression_to_llvm_stmt(&params[0]);
                let call_stmt = Stmt::LLVM(format!("    call i32 (i32) @{}(i32 {})", name, param_name), vec![Stmt::Comment(comment)]);
                return Some(Stmt::MultiStmt(vec![param_stmt, call_stmt]))
            } else {
                return Some(Stmt::LLVM(format!("    call i32 (i32) @{}()", name), vec![Stmt::Comment(comment)]));

            }
        },
        Stmt::Noop() => Some(Stmt::Noop()),
        Stmt::LLVM(_, _) => panic!("LLVM transformation can't handle {:?}", s),
        Stmt::AssignDeclare(_, _) => panic!("LLVM transformation can't handle {:?}", s),
        Stmt::Comment(inner_comment) => Some(Stmt::LLVM(format!("; {:?}", inner_comment),  vec![Stmt::Comment(comment)])),

    }
}