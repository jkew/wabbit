use crate::VarType;

use super::BinOp;
use super::Expr;
use super::Program;
use super::Stmt;


pub fn get_factorial_program_text() -> String {
    return "func fact(g) {
        var result = 1;
        var x = 1;
        while x < g {
            result = result * x;
        x = x + 1;
        }
        return result;
    }
    var a = 3;
    var b = 5;
    if a < b {
        print a;
     } else {
        print b;
     }
    var n = 0;
    while n < 10 {
       print fact(n);
       n = n + 1;
    };".to_string();
}

pub fn get_all_features_program() -> Program {
    return Program {
        statements: vec![
        Stmt::AssignDeclare(Expr::UnscopedVar("n".to_string(), crate::VarType::U), Expr::IntegerLiteral(10)),
        Stmt::While(
            Expr::Binary(
                BinOp::Lt,
                vec![Expr::UnscopedVar("n".to_string(), crate::VarType::U), Expr::IntegerLiteral(10)], crate::VarType::U
            ),
            vec![
                Stmt::If(
                    Expr::Binary(
                        BinOp::Eq,
                        vec![Expr::UnscopedVar("n".to_string(), crate::VarType::U), Expr::IntegerLiteral(5)], crate::VarType::U
                    ),
                    vec![
                        Stmt::AssignDeclare(
                            Expr::UnscopedVar("x".to_string(), crate::VarType::U),
                            Expr::Binary(
                                BinOp::Mul,
                                vec![
                                    Expr::UnscopedVar("n".to_string(), crate::VarType::U),
                                    Expr::IntegerLiteral(100),
                                ],crate::VarType::U
                            ),
                        ),
                        Stmt::Print(Expr::UnscopedVar("x".to_string(), crate::VarType::U)),
                    ],
                    vec![Stmt::Print(Expr::UnscopedVar("n".to_string(), crate::VarType::U))],
                ),
                Stmt::AssignDeclare(
                    Expr::UnscopedVar("n".to_string(), crate::VarType::U),
                    Expr::Binary(
                        BinOp::Add,
                        vec![Expr::UnscopedVar("n".to_string(), crate::VarType::U), Expr::IntegerLiteral(1)],crate::VarType::U
                    ),
                ),
            ],
        ),
        Stmt::Fn(
            "square".to_string(),
            vec![Expr::UnscopedVar("x".to_string(), crate::VarType::U)],
            vec![
                Stmt::AssignDeclare(
                    Expr::UnscopedVar("r".to_string(), crate::VarType::U),
                    Expr::Binary(
                        BinOp::Mul,
                        vec![
                            Expr::UnscopedVar("x".to_string(), crate::VarType::U),
                            Expr::UnscopedVar("x".to_string(), crate::VarType::U),
                        ],crate::VarType::U
                    ),
                ),
                Stmt::Return(Expr::UnscopedVar("r".to_string(), crate::VarType::U)),
            ], VarType::U
        ), 
        Stmt::AssignDeclare(
            Expr::UnscopedVar("result".to_string(), crate::VarType::U),
            Expr::Call("square".to_string(), vec![Expr::IntegerLiteral(4)], crate::VarType::U),
        ),
        Stmt::Print(Expr::UnscopedVar("result".to_string(), crate::VarType::U)),
    ],
    };
}
