use crate::VarType;

use super::BinOp;
use super::Expr;
use super::Stmt;
use std::cell::Cell;
use std::cell::RefCell;
use std::env::var;


// An statement transform that does nothing
#[allow(dead_code)]
pub fn sfn_noop(_s: &Stmt) -> Option<Stmt> {
    return None;
}

// tree finalizer, takes the all the statements in and does something if it wants
#[allow(dead_code)]
pub fn ffn_noop(statements: &Vec<Stmt>) -> Vec<Stmt> {
    return statements.to_vec();
}

// An expression transform that does nothing
#[allow(dead_code)]
pub fn efn_noop(_e: &Expr) -> Option<Expr> {
    return None;
}

// An expression transform that does constant folding
pub fn efn_constant_fold(e: &Expr) -> Option<Expr> {
    match e {
        Expr::Binary(o, args, vt) => {
            let val = match (o, &args[0], &args[1]) {
                (BinOp::Add, Expr::IntegerLiteral(lhs_const), Expr::IntegerLiteral(rhs_const)) => {
                    lhs_const + rhs_const
                }
                (BinOp::Mul, Expr::IntegerLiteral(lhs_const), Expr::IntegerLiteral(rhs_const)) => {
                    lhs_const * rhs_const
                }
                (BinOp::Sub, Expr::IntegerLiteral(lhs_const), Expr::IntegerLiteral(rhs_const)) => {
                    lhs_const - rhs_const
                }
                (BinOp::Div, Expr::IntegerLiteral(lhs_const), Expr::IntegerLiteral(rhs_const)) => {
                    return Some(Expr::FloatLiteral((*lhs_const as f64)/(*rhs_const as f64 )))
                }
                _ => return None,
            };
            return Some(Expr::IntegerLiteral(val));
        }
        _ => return None,
    };
}

pub fn sfn_deinit(s: &Stmt) -> Option<Stmt> {
    match s {
        Stmt::AssignDeclare(v, e) => {
            return Some(Stmt::MultiStmt(vec![
                Stmt::Alloc(v.clone(), crate::VarType::U),
                Stmt::Assign(v.clone(), e.clone()),
            ]))
        }
        _ => None,
    }
}

thread_local! {
    static BLOCK_ID: Cell<u32> = Cell::new(0);
    static STACK: RefCell<Vec<u32>> = RefCell::new(vec![]);
    static VAR_SCOPE_REGISTRY: RefCell<Vec<(String, u32)>> = RefCell::new(vec![]);
    static VAR_TYPE_REGISTRY: RefCell<Vec<((String, u32, VarType))>> = RefCell::new(vec![]);
    static MAIN_STATEMENTS: RefCell<Vec<Stmt>> = RefCell::new(vec![]);
    static GLOBAL_ALLOCS: RefCell<Vec<Stmt>> = RefCell::new(vec![]);
}

pub fn reset_compiler_state() {
    BLOCK_ID.with(|it| it.set(0));
    STACK.with(|s| s.borrow_mut().clear());
    VAR_SCOPE_REGISTRY.with(|s| s.borrow_mut().clear());
    VAR_TYPE_REGISTRY.with(|s| s.borrow_mut().clear());
    MAIN_STATEMENTS.with(|s| s.borrow_mut().clear());
    GLOBAL_ALLOCS.with(|s| s.borrow_mut().clear());
}

fn _get_expression_type(e:&Expr) -> VarType {
    match e {
        Expr::Binary(_, exp, vt) => {
            // if this type has already been set, trust it
            if vt != &VarType::U {
                return vt.clone();
            }
            for inner_e in exp {
                let inner_type = _get_expression_type(inner_e);
                if inner_type != VarType::U {
                    return inner_type;
                }
            }
            return VarType::U;
        },
        Expr::IntegerLiteral(_) => return VarType::I,
        Expr::FloatLiteral(_) => return VarType::F,
        Expr::UnscopedVar(_, vt) => return vt.clone(),
        Expr::ScopedVar(_, _, vt) => return vt.clone(),
        Expr::GlobalVar(_, vt) => return vt.clone(),
        Expr::Call(_, exp, vt) => {
            // if this type has already been set, trust it
            if vt != &VarType::U {
                return vt.clone();
            }
            for inner_e in exp {
                let inner_type = _get_expression_type(inner_e);
                if inner_type != VarType::U {
                    return inner_type;
                }
            }
            return VarType::U;
        },
        Expr::None() => return VarType::U,
    }
}

fn _set_var_type(scope:&u32, name:&String, vt:VarType) {
    VAR_TYPE_REGISTRY.with(|it| {
        it.borrow_mut().push((name.clone(), scope.clone(), vt));
    })
}

fn _get_var_type(scope:u32, name:String) -> VarType {
    let mut var_type = VarType::U;
    VAR_TYPE_REGISTRY.with(|it| {
        for var_entry in it.borrow().clone().into_iter() {
            if name == var_entry.0 && scope == var_entry.1 {
                var_type = var_entry.2;
                break;
            }
        }
    });
    return var_type;
}

// Looks at assignment statements and tracks the type information in the VAR_TYPE_REGISTRY
pub fn sfn_track_type_assignments(s:&Stmt) -> Option<Stmt> {
    let mut exp_type = VarType::U;

    match s {
        Stmt::Assign(var, e) => {
            match var {
                Expr::UnscopedVar(_, _) => panic!("this compiler stage requires scoped variables"),
                Expr::ScopedVar(scope, name, val_type) => {
                    // infer types from expressions
                    exp_type = _get_expression_type(e);
                    _set_var_type(scope, name, exp_type.clone());
                return Some(Stmt::Assign(Expr::ScopedVar(scope.clone(), name.clone(), exp_type), e.clone()));
                },
                Expr::GlobalVar(name, val_type) => {
                    // infer types from expressions
                    exp_type = _get_expression_type(e);
                    _set_var_type(&0, name, exp_type.clone());
                    return Some(Stmt::Assign(Expr::GlobalVar(name.clone(), exp_type), e.clone()));
                },
                _ => return None,
            }
        },
        Stmt::Fn(name, params, statements, retval) => {
            for var in params {
                exp_type = _get_expression_type(var);
                match var {
                    Expr::UnscopedVar(_, _) => panic!("this compiler stage requires scoped variables"),
                    Expr::ScopedVar(scope, name, val_type) => {
                        _set_var_type(scope, name, exp_type.clone());
                        return None;
                    },
                    Expr::GlobalVar(name, val_type) => {
                        _set_var_type(&0, name, exp_type.clone());
                        return None;
                    },
                    _ => return None,
                }
            }
            return None
        },
        _ => return None
    }
}


pub fn efn_update_variable_types(e:&Expr) -> Option<Expr> {
    match e {
        Expr::UnscopedVar(_, _) => panic!("this compiler stage requires scoped variables"),
        Expr::ScopedVar(scope, name, val_type) => {
            if *val_type == VarType::U {
                let exp_type = _get_var_type(scope.clone(), name.clone());
                return Some(Expr::ScopedVar(scope.clone(), name.clone(), exp_type));
            }
            return None;
        },
        Expr::GlobalVar(name, val_type) => {
            if *val_type == VarType::U {
                let exp_type = _get_var_type(0, name.clone());
                return Some(Expr::GlobalVar(name.clone(), exp_type));
            }
            return None;
        },
        Expr::Binary(op, exprs, VarType::U) => {
            let mut exp_type = VarType::U;
            for expr in exprs.into_iter() {
                exp_type = _get_expression_type(expr);
                match exp_type {
                    VarType::I => return Some(Expr::Binary(op.clone(), exprs.clone(), VarType::I)),
                    VarType::F => return Some(Expr::Binary(op.clone(), exprs.clone(), VarType::F)),
                    VarType::U => {},
                }
            }
            return None;
 
        },
        _ => return None,
    }
}

pub fn sfn_update_assignments(s:&Stmt) -> Option<Stmt> {
    let mut exp_type = VarType::U;

    match s {
        Stmt::Assign(var, e) => {
            match var {
                Expr::UnscopedVar(_, _) => panic!("this compiler stage requires scoped variables"),
                Expr::ScopedVar(scope, name, val_type) => {
                    let exp_type = _get_var_type(scope.clone(), name.clone());
                    return Some(Stmt::Assign(Expr::ScopedVar(scope.clone(), name.clone(), exp_type), e.clone()));
                },
                Expr::GlobalVar(name, val_type) => {
                    let exp_type = _get_var_type(0, name.clone());
                    return Some(Stmt::Assign(Expr::GlobalVar(name.clone(), exp_type), e.clone()));
                },
                _ => return None,
            }
        },
        Stmt::Alloc(expr, vt) => {
            match expr {
                Expr::UnscopedVar(_, _) => panic!("this compiler stage requires scoped variables"),
                Expr::ScopedVar(scope, name, val_type) => {
                    return Some(Stmt::Alloc(Expr::ScopedVar(scope.clone(), name.clone(), exp_type), _get_var_type(scope.clone(), name.clone())));
                },
                Expr::GlobalVar(name, val_type) => {
                    return Some(Stmt::Alloc(Expr::GlobalVar(name.clone(), exp_type), _get_var_type(0, name.clone())));
                },
                _ => return None,
            }
        },
        Stmt::Fn(name, params, statements, vt) => {
            return None;
        },
        _ => return None
    }
}

fn _sfn_add_scope(v: &Vec<Stmt>) -> Vec<Stmt> {
    let mut block_id = 0;
    BLOCK_ID.with(|it| {
        it.set(it.get().wrapping_add(1));
        block_id = it.get();
    });

    return vec![
        vec![Stmt::Scope(block_id)],
        v.clone(),
        vec![Stmt::EndScope(block_id)],
    ]
    .concat();
}

pub fn sfn_add_scopes(s: &Stmt) -> Option<Stmt> {
    match s {
        Stmt::If(e, t, f) => {
            let t2 = _sfn_add_scope(t);
            let f2 = _sfn_add_scope(f);
            return Some(Stmt::If(e.clone(), t2, f2));
        }
        Stmt::While(e, t) => {
            let t2 = _sfn_add_scope(t);
            return Some(Stmt::While(e.clone(), t2));
        },
        Stmt::Fn(name, params, b, vt) => { 
            let b2 = _sfn_add_scope(b);
            return Some(Stmt::Fn(name.clone(), params.clone(), b2, vt.clone()));
        },
        _ => return None,
    }
}

pub fn sfn_track_block_id(s: &Stmt) -> Option<Stmt> {
    match s {
        Stmt::Scope(id) => {
            STACK.with(|it| {
                it.borrow_mut().push(*id)
            });
            return None;
        }
        Stmt::EndScope(_id) => {
            STACK.with(|it| {
                it.borrow_mut().pop();
            });
            return None;
        }
        Stmt::Alloc(e, vt) => {
            let mut block_id = Some(0 as u32);
            STACK.with(|it| {
                block_id =  it.borrow().clone().last().copied();
            });
            match (e, block_id) {
                (Expr::UnscopedVar(n, vt), Some(id) )=> {
                    VAR_SCOPE_REGISTRY.with(|it| {
                        it.borrow_mut().push((n.clone(), id))
                    });
                    return None
                }
                _ => return None
            }
            
        }
        // Update the variable scope map for parameters of local functions
        Stmt::Fn(_, params,_, vt) => {
            let mut block_id = Some(1 as u32);
            STACK.with(|it| {
                block_id =  it.borrow().clone().last().copied();
            });
            match block_id {
                Some(_) => {},
                None => block_id = Some(1),
            }
            for e in params {
                match (e, block_id) {
                    (Expr::GlobalVar(n, vt), Some(_id))=> {
                        VAR_SCOPE_REGISTRY.with(|it| {
                            it.borrow_mut().push((n.clone(), block_id.unwrap_or(1) ))
                        });
                        break;
                    }
                    _ => {},
                }
            }
            return None;
        }
        _ => None,
    }
}

fn _get_current_block_id() -> Option<u32> {
    let mut current_block_id:Option<u32> = None;
    STACK.with(|it| {
        current_block_id =  it.borrow().clone().last().copied();
    });
    return current_block_id;
}

pub fn sfn_ensure_fn_params_are_local_scoped(s: &Stmt) -> Option<Stmt> {
    sfn_track_block_id(s);
    let current_block_id = _get_current_block_id();
    match s {
        Stmt::Fn(name, params, statements, vt) => 
        {
            let new_params = params.into_iter().map(|p|
                match p {
                    Expr::UnscopedVar(_, _) => panic!("Unscoped parameter in function!"),
                    Expr::GlobalVar(name, vt) => return Expr::ScopedVar(current_block_id.unwrap_or(1), name.to_string(), vt.clone()),
                    _ => return p.clone(),
                }
            ).collect();
            return Some(Stmt::Fn(name.clone(), new_params, statements.clone(), vt.clone()));
        },
        _ => None
     }
}

pub fn efn_ensure_fn_params_are_local_scoped(e: &Expr) -> Option<Expr> {
    match e {
        Expr::GlobalVar(x, t) => {
            let current_block_id = _get_current_block_id();
            let mut possible_scopes = vec![];
            let mut scoped_block_id = None;
            VAR_SCOPE_REGISTRY.with(|it| {
                for v in it.borrow().iter().rev() {
                    if v.0 == *x {
                        if current_block_id.unwrap_or(0) == v.1 {
                            scoped_block_id = current_block_id;
                            return;
                        }
                        possible_scopes.push(v.1)
                    }
                }

            });

            match scoped_block_id {
                Some(id) => { 
                    return Some(Expr::ScopedVar(id, x.clone(), t.clone()))
                },
                None => return None,
            }
        }
        _ => return None,
    }
}

pub fn efn_unscoped_to_scoped(e: &Expr) -> Option<Expr> {
    match e {
        Expr::UnscopedVar(x, vt) => {
            let current_block_id = _get_current_block_id();
            let mut possible_scopes = vec![];
            let mut scoped_block_id = None;
            VAR_SCOPE_REGISTRY.with(|it| {
                for v in it.borrow().iter().rev() {
                    if v.0 == *x {
                        if current_block_id.unwrap_or(0) == v.1 {
                            scoped_block_id = current_block_id;
                            return;
                        }
                        possible_scopes.push(v.1)
                    }
                }

            });

            match scoped_block_id {
                Some(id) => return Some(Expr::ScopedVar(id, x.clone(), vt.clone())),
                None => { 
                    match possible_scopes.first() {
                        Some(id2) => return Some(Expr::ScopedVar(*id2, x.clone(), vt.clone())),
                        None => return Some(Expr::GlobalVar(x.clone(), vt.clone()))
                    }
                },
            }
        }
        _ => return None,
    }
}

pub fn ffn_create_main(statements: &Vec<Stmt>) -> Vec<Stmt> {
    let mut main_fn_statements = vec![];
    MAIN_STATEMENTS.with(|it| {
        main_fn_statements = it.borrow().clone()
        });
    main_fn_statements.push(Stmt::Return(Expr::IntegerLiteral(0)));
    let main = Stmt::Fn("main".to_string(), vec![], main_fn_statements, VarType::I);
    let mut global_allocs: Vec<Stmt> = vec![];
    GLOBAL_ALLOCS.with(|it| {
        global_allocs = it.borrow().clone()
        });
    let existing: Vec<Stmt> = statements.clone().into_iter().filter(|s| {
        // Remove the noop
        match s {
            Stmt::Noop() => false,
            _ => true
        }
    }).collect();
    let mut result:Vec<Stmt> = vec![];
    result.extend(global_allocs);
    result.extend(existing);
    result.push(main);
    return result;
}

pub fn sfn_lift_global_into_fn(s:&Stmt) -> Option<Stmt> {
    // update current state
    sfn_track_block_id(s);
    let current_block_id = _get_current_block_id();
    match (current_block_id, s) {
        (Some(_), Stmt::Fn(name, _params, _body, vt))  => {
            panic!("function names must be globally scoped {:?}", name)
        },
        (_, Stmt::Alloc(Expr::GlobalVar(_, vt), avt)) => {
            // all global variables moved outside of main
            GLOBAL_ALLOCS.with(|it| {
                it.borrow_mut().push(s.clone())
                });
            return Some(Stmt::Noop()); 
        }
        (None, Stmt::Fn(name, _params, _body, vt)) => {
            if name == "main" {
                panic!("already a main function, cannot lift global statements into new main")
            }
            return None
        },
        (None, Stmt::EndScope(_)) => return None,
        (None, Stmt::MultiStmt(_)) => return None,
        (None, s) => {
            // Record the global statement for later when the finalizer can create a main
            MAIN_STATEMENTS.with(|it| {
                it.borrow_mut().push(s.clone())
                });
            // Replace the current statement with a noop
            return Some(Stmt::Noop());
        },
        (_, _) => return None
    }
}


#[cfg(test)]
mod test {
    use crate::Stmt;
    use crate::ast_transform_ops::efn_noop;
    use crate::ast_transform_ops::efn_unscoped_to_scoped;
    use crate::ast_transform_ops::sfn_track_block_id;
    use crate::ast_transform_ops::sfn_add_scopes;
    use crate::ast_transform_ops::sfn_deinit;
    use super::BinOp;
    use super::Expr;
    use Program;
    use Wab;
    use ast_transform_ops::efn_constant_fold;
    use ast_transform_ops::sfn_noop;

    #[test]
    fn test_deinit() {
        let program = Program {
            statements: vec![Stmt::AssignDeclare(
                Expr::UnscopedVar("n".to_string(), crate::VarType::U),
                Expr::IntegerLiteral(10),
            )],
        };

        let result = Wab::tree_walker(&program.statements, &sfn_deinit, &efn_noop)
            .unwrap_or(program.statements);

        assert_eq!(
            result[0],
            Stmt::MultiStmt(vec![
                Stmt::Alloc(Expr::UnscopedVar("n".to_string(),  crate::VarType::U), crate::VarType::U),
                Stmt::Assign(Expr::UnscopedVar("n".to_string(),  crate::VarType::U), Expr::IntegerLiteral(10))
            ])
        );
    }

    #[test]
    fn test_add_scopes() {
        let program = Program {
            statements: vec![Stmt::If(
                Expr::Binary(
                    BinOp::Lt,
                    vec![Expr::IntegerLiteral(2), Expr::IntegerLiteral(4)], crate::VarType::U
                ),
                vec![Stmt::Print(Expr::IntegerLiteral(42))],
                vec![Stmt::Print(Expr::IntegerLiteral(-42))],
            )],
        };

        let result = Wab::tree_walker(&program.statements, &sfn_add_scopes, &efn_noop)
            .unwrap_or(program.statements);

        assert_eq!(
            result[0],
            Stmt::If(
                Expr::Binary(
                    BinOp::Lt,
                    vec![Expr::IntegerLiteral(2), Expr::IntegerLiteral(4)], crate::VarType::U
                ),
                vec![
                    Stmt::Scope(1),
                    Stmt::Print(Expr::IntegerLiteral(42)),
                    Stmt::EndScope(1)
                ],
                vec![
                    Stmt::Scope(2),
                    Stmt::Print(Expr::IntegerLiteral(-42)),
                    Stmt::EndScope(2)
                ]
            )
        );
    }

    #[test]
    fn test_add_scoped_variables() {
        let program = Program {
            statements: vec![
                Stmt::AssignDeclare(Expr::UnscopedVar("x".to_string(),  crate::VarType::U), Expr::IntegerLiteral(1)),
                Stmt::AssignDeclare(Expr::UnscopedVar("y".to_string(),  crate::VarType::U), Expr::IntegerLiteral(2)),
                Stmt::If(
                    Expr::Binary(
                        BinOp::Lt,
                        vec![Expr::UnscopedVar("x".to_string(),  crate::VarType::U), Expr::IntegerLiteral(4)], crate::VarType::U
                    ),
                    vec![Stmt::Print(Expr::UnscopedVar("x".to_string(),  crate::VarType::U))],
                    vec![
                        Stmt::AssignDeclare(
                            Expr::UnscopedVar("x".to_string(),  crate::VarType::U),
                            Expr::IntegerLiteral(3),
                        ),
                        Stmt::Print(Expr::UnscopedVar("x".to_string(),  crate::VarType::U)),
                        Stmt::Print(Expr::UnscopedVar("y".to_string(),  crate::VarType::U)),
                    ],
                ),
            ],
        };

        let pass1 = Wab::tree_walker(&program.statements, &sfn_add_scopes, &efn_noop)
            .unwrap_or(program.statements);

        assert_eq!(
            pass1,
            vec![
                Stmt::AssignDeclare(Expr::UnscopedVar("x".to_string(),  crate::VarType::U), Expr::IntegerLiteral(1)),
                Stmt::AssignDeclare(Expr::UnscopedVar("y".to_string(),  crate::VarType::U), Expr::IntegerLiteral(2)),
                Stmt::If(
                    Expr::Binary(
                        BinOp::Lt,
                        vec![Expr::UnscopedVar("x".to_string(),  crate::VarType::U), Expr::IntegerLiteral(4)], crate::VarType::U
                    ),
                    vec![
                        Stmt::Scope(1),
                        Stmt::Print(Expr::UnscopedVar("x".to_string(),  crate::VarType::U)),
                        Stmt::EndScope(1)
                    ],
                    vec![
                        Stmt::Scope(2),
                        Stmt::AssignDeclare(
                            Expr::UnscopedVar("x".to_string(),  crate::VarType::U),
                            Expr::IntegerLiteral(3)
                        ),
                        Stmt::Print(Expr::UnscopedVar("x".to_string(),  crate::VarType::U)),
                        Stmt::Print(Expr::UnscopedVar("y".to_string(),  crate::VarType::U)),
                        Stmt::EndScope(2)
                    ]
                )
            ]
        );

        let pass2 = Wab::tree_walker(&pass1, &sfn_deinit, &efn_noop).unwrap_or(pass1);

        assert_eq!(
            pass2,
            vec![
                Stmt::MultiStmt(
                    vec![
                        Stmt::Alloc(Expr::UnscopedVar("x".to_string(),  crate::VarType::U),crate::VarType::U),
                        Stmt::Assign(Expr::UnscopedVar("x".to_string(),  crate::VarType::U),Expr::IntegerLiteral(1))
                        ]
                ),
                Stmt::MultiStmt(
                    vec![
                        Stmt::Alloc(Expr::UnscopedVar("y".to_string(),  crate::VarType::U), crate::VarType::U),
                        Stmt::Assign(Expr::UnscopedVar("y".to_string(),  crate::VarType::U),Expr::IntegerLiteral(2))
                        ]
                ),
                Stmt::If(
                    Expr::Binary(
                        BinOp::Lt,
                        vec![Expr::UnscopedVar("x".to_string(),  crate::VarType::U), Expr::IntegerLiteral(4)], crate::VarType::U
                    ),
                    vec![
                        Stmt::Scope(1),
                        Stmt::Print(Expr::UnscopedVar("x".to_string(),  crate::VarType::U)),
                        Stmt::EndScope(1)
                    ],
                    vec![
                        Stmt::Scope(2),
                        Stmt::MultiStmt(
                            vec![
                                Stmt::Alloc(Expr::UnscopedVar("x".to_string(),  crate::VarType::U), crate::VarType::U),
                                Stmt::Assign(Expr::UnscopedVar("x".to_string(),  crate::VarType::U),Expr::IntegerLiteral(3))
                                ]
                        ),
                        Stmt::Print(Expr::UnscopedVar("x".to_string(),  crate::VarType::U)),
                        Stmt::Print(Expr::UnscopedVar("y".to_string(),  crate::VarType::U)),
                        Stmt::EndScope(2)
                    ]
                )
            ]
        );

        let pass3 = Wab::tree_walker(&pass2, &sfn_track_block_id, &efn_unscoped_to_scoped).unwrap_or(pass2);
        assert_eq!(
            pass3,
            vec![
                Stmt::MultiStmt(
                    vec![
                        Stmt::Alloc(Expr::GlobalVar("x".to_string(),  crate::VarType::U), crate::VarType::U),
                        Stmt::Assign(Expr::GlobalVar("x".to_string(),  crate::VarType::U),Expr::IntegerLiteral(1))
                        ]
                ),
                Stmt::MultiStmt(
                    vec![
                        Stmt::Alloc(Expr::GlobalVar("y".to_string(),  crate::VarType::U), crate::VarType::U),
                        Stmt::Assign(Expr::GlobalVar("y".to_string(),  crate::VarType::U),Expr::IntegerLiteral(2))
                        ]
                ),
                Stmt::If(
                    Expr::Binary(
                        BinOp::Lt,
                        vec![Expr::GlobalVar("x".to_string(),  crate::VarType::U), Expr::IntegerLiteral(4)], crate::VarType::U
                    ),
                    vec![
                        Stmt::Scope(1),
                        Stmt::Print(Expr::GlobalVar("x".to_string(),  crate::VarType::U)),
                        Stmt::EndScope(1)
                    ],
                    vec![
                        Stmt::Scope(2),
                        Stmt::MultiStmt(
                            vec![
                                Stmt::Alloc(Expr::ScopedVar(2, "x".to_string(),  crate::VarType::U), crate::VarType::U),
                                Stmt::Assign(Expr::ScopedVar(2, "x".to_string(), crate::VarType::U),Expr::IntegerLiteral(3))
                                ]
                        ),
                        Stmt::Print(Expr::ScopedVar(2, "x".to_string(), crate::VarType::U)),
                        Stmt::Print(Expr::GlobalVar("y".to_string(),  crate::VarType::U)),
                        Stmt::EndScope(2)
                    ]
                )
            ]
        );

    }

    #[test]
    fn test_const_folding() {
        let program2 = Program {
            statements: vec![
                Stmt::Print(Expr::IntegerLiteral(42)),
                Stmt::Print(Expr::Binary(
                    BinOp::Add,
                    vec![Expr::IntegerLiteral(2), Expr::IntegerLiteral(4)], crate::VarType::U
                )),
            ],
        };

        let result2 = Wab::tree_walker(&program2.statements, &sfn_noop, &efn_constant_fold)
            .unwrap_or(program2.statements);

        assert_eq!(result2[1], Stmt::Print(Expr::IntegerLiteral(6)));
    }

    #[test]
    fn test_const_folding_multipass() {
        let program = Program {
            statements: vec![Stmt::Print(Expr::Binary(
                BinOp::Add,
                vec![
                    Expr::Binary(
                        BinOp::Add,
                        vec![Expr::IntegerLiteral(2), Expr::IntegerLiteral(4)],crate::VarType::U
                    ),
                    Expr::Binary(
                        BinOp::Add,
                        vec![Expr::IntegerLiteral(2), Expr::IntegerLiteral(4)],crate::VarType::U
                    ),
                ],crate::VarType::U
            ))], 
        };

        let pass1 = Wab::tree_walker(&program.statements, &sfn_noop, &efn_constant_fold)
            .unwrap_or(program.statements);

        assert_eq!(
            pass1[0],
            Stmt::Print(Expr::Binary(
                BinOp::Add,
                vec![Expr::IntegerLiteral(6), Expr::IntegerLiteral(6)], crate::VarType::U
            ))
        );

        let pass2 = Wab::tree_walker(&pass1, &sfn_noop, &efn_constant_fold).unwrap_or(pass1);
        assert_eq!(pass2[0], Stmt::Print(Expr::IntegerLiteral(12)));
    }
}
