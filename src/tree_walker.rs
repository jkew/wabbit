use crate::Stage;
use crate::ast_transform_ops::reset_compiler_state;

use super::Expr;
use super::Stmt;
use super::Wab;
use std::ops::Fn;

impl Wab {
    // Walks an expression hierarchy ( no statements )
    fn expression_walker<F>(exp: &Expr, expr_fn: &F) -> Expr
    where
        F: Fn(&Expr) -> Option<Expr>,
    {
        let e2 = expr_fn(exp);
        match e2 {
            Some(e2) => return e2, // If the expression fn made modifications return it
            None => {}             // otherwise continue down the tree evaluating expr
        }
        match exp {
            Expr::Binary(op, args, vt) => {
                let lhs = expr_fn(&args[0]).unwrap_or(args[0].clone());
                let rhs = expr_fn(&args[1]).unwrap_or(args[1].clone());
                return Expr::Binary(op.clone(), vec![lhs, rhs], vt.clone());
            }
            Expr::Call(name, params, vt) => {
                let p2 = params
                    .iter()
                    .map(|p| expr_fn(&p).unwrap_or(p.clone()))
                    .collect::<Vec<Expr>>();
                return Expr::Call(name.to_string(), p2, vt.clone());
            }
            _ => return expr_fn(exp).unwrap_or(exp.clone()),
        }
    }

    pub fn compile_stage(statements: &Vec<Stmt>, stage:Stage) -> Vec<Stmt> {
        eprintln!(">> {:?}", stage.name);
        let mut current:Vec<Stmt> = statements.to_vec();
        // Reset any thread locals used for tracking things in the tree walker
        if stage.reset {
            reset_compiler_state();
        }
        let mut last = current.to_vec();
        loop {
            current =  Wab::tree_walker(&current, &stage.stmt_fn, &stage.expr_fn).unwrap_or_else(|| panic!("Internal error"));
            if !stage.converge || last == current {
                break;
            }
            last = current.to_vec()
        }
        let finalizer = stage.final_fn;
        return finalizer(&current);
    } 

    // tree_walker performs one pass over the ast and applies the functions stmt_fn and expr_fn to
    // statements and expressions respectively
    pub fn tree_walker<F1, F2>(
        statements: &Vec<Stmt>,
        stmt_fn: &F1,
        expr_fn: &F2,
    ) -> Option<Vec<Stmt>>
    where
        F1: Fn(&Stmt) -> Option<Stmt>,
        F2: Fn(&Expr) -> Option<Expr>,
    {
        return Some(
            statements
                .iter()
                .map(|s| {
                    let s2 = stmt_fn(&s);
                    match s2 {
                        Some(s) => return s, // If the statment fn made modifications return it
                        None => {}           // otherwise continue down the tree evaluating expr
                    }
                    match s {
                        Stmt::If(e, t, f) => {
                            let e2 = Self::expression_walker(&e, expr_fn);
                            let t2 = Self::tree_walker(&t, stmt_fn, expr_fn).unwrap_or(t.clone());
                            let f2 = Self::tree_walker(&f, stmt_fn, expr_fn).unwrap_or(f.clone());
                            return Stmt::If(e2, t2, f2);
                        }
                        Stmt::While(e, t) => {
                            let e2 = Self::expression_walker(&e, expr_fn);
                            let t2 = Self::tree_walker(&t, stmt_fn, expr_fn).unwrap_or(t.clone());
                            return Stmt::While(e2, t2);
                        }
                        Stmt::Fn(n, params, b, vt) => {
                            let n2 = n.clone();
                            let p2 = params
                                .iter()
                                .map(|p| Self::expression_walker(&p, expr_fn))
                                .collect::<Vec<Expr>>();
                            let b2: Vec<Stmt> = Self::tree_walker(&b, stmt_fn, expr_fn).unwrap_or(b.clone());
                            return Stmt::Fn(n2, p2, b2, vt.clone());
                        }
                        Stmt::Assign(e1, e2) => {
                            let e21 = Self::expression_walker(&e1, expr_fn);
                            let e22 = Self::expression_walker(&e2, expr_fn);
                            return Stmt::Assign(e21, e22);
                        }
                        Stmt::AssignDeclare(e1, e2) => {
                            let e21 = Self::expression_walker(&e1, expr_fn);
                            let e22 = Self::expression_walker(&e2, expr_fn);
                            return Stmt::AssignDeclare(e21, e22);
                        }
                        Stmt::Print(e) => {
                            let e2 = Self::expression_walker(&e, expr_fn);
                            return Stmt::Print(e2);
                        }
                        Stmt::Alloc(e, vt) => {
                            let e2 = Self::expression_walker(&e, expr_fn);
                            return Stmt::Alloc(e2, vt.clone());
                        }
                        Stmt::Scope(id) => {
                            return Stmt::Scope(id.clone());
                        }
                        Stmt::EndScope(id) => {
                            return Stmt::EndScope(id.clone());
                        }
                        Stmt::MultiStmt(s) => {
                            let s2: Vec<Stmt> = Self::tree_walker(&s, stmt_fn, expr_fn).unwrap_or(s.clone());
                            return Stmt::MultiStmt(s2)
                        }
                        Stmt::Return(e) => {
                            let e2 = Self::expression_walker(&e, expr_fn);
                            return Stmt::Return(e2);
                        }
                        Stmt::Noop() => { return Stmt::Noop() }
                         _ => panic!("Internal Error: Uknown statement in tree walker {:?}", s),
                    }
                })
                .collect::<Vec<Stmt>>(),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::Expr;
    use super::Stmt;
    use super::Wab;
    use test_fixtures::get_all_features_program;
    use std::cell::Cell;

    thread_local! {
      static SFN_CNT: Cell<u32> = Cell::new(0);
      static EFN_CNT: Cell<u32> = Cell::new(0);
    }

    fn sfn_test_count(_s: &Stmt) -> Option<Stmt> {
        SFN_CNT.with(|it| it.set(it.get().wrapping_add(1)));
        return None;
    }

    fn efn_test_count(_e: &Expr) -> Option<Expr> {
        EFN_CNT.with(|it| it.set(it.get().wrapping_add(1)));
        return None;
    }

    #[test]
    pub fn test_tree_walker() {
        let program = get_all_features_program();
        let _result = Wab::tree_walker(&program.statements, &sfn_test_count, &efn_test_count)
            .unwrap_or(program.statements);
        SFN_CNT.with(|it| assert_eq!(it.get(), 12));
        EFN_CNT.with(|it| {
            assert_eq!(it.get(), 39);
        });
    }

}
