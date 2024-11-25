use pest::{Parser, iterators::{Pairs, Pair}};

use crate::{Stmt, Wab, WabParser, Rule, Expr, BinOp, VarType};

impl Wab {

    fn parse_fn_ast(fn_pair:Pair<'_, Rule>) -> Stmt {
        let mut inner_statements = vec![];
        let mut params = vec![];
        let mut name = "<unknown>".to_string();
        let mut ret_type = VarType::I;
        let mut param_type = VarType::I;
        for pair in fn_pair.into_inner() {
            match pair.as_rule() {
                Rule::name => { name = pair.as_span().as_str().to_string() },
                Rule::param => { params.push(Expr::UnscopedVar(pair.as_span().as_str().to_string(), crate::VarType::U)) },
                Rule::statements => { inner_statements = Self::parsed_to_ast(pair.into_inner())},
                Rule::float_ret => { ret_type = VarType::F },
                Rule::float_param => { param_type = VarType::F }
                _ => {} //noop
            }
        }
        params = params.into_iter().map(|p| match p {
            Expr::UnscopedVar( n,_ ) => Expr::UnscopedVar(n, param_type.clone()), 
            _ => panic!("something has gone wrong")
        }).collect();
        return Stmt::Fn(name, params, inner_statements, ret_type);
    }

    fn parsed_decl_assign_stmt_ast(decl_pair:Pair<'_, Rule>) -> Stmt {
        let mut name = "<unknown>".to_string();
        let mut exp = Expr::None();
        for pair in decl_pair.into_inner() {
            match pair.as_rule() {
                Rule::name => { name = pair.as_span().as_str().to_string()},
                Rule::math_expression => exp = Self::parse_math_expression(pair),
                Rule::semi => {} // noop
                x => eprintln!(">>> 1 Error converting parsed to ast {:?}", x)
            }
        }
        return Stmt::AssignDeclare(Expr::UnscopedVar(name, crate::VarType::U), exp);
    }

    fn parse_operand(operand:Pair<'_, Rule>) -> Expr {
        for p in operand.into_inner() {
            match p.as_rule() {
                Rule::number => {
                    let str = p.as_span().as_str().to_string();
                    if str.contains(".") {
                        return Expr::FloatLiteral(p.as_span().as_str().to_string().parse::<f64>().unwrap())
                    } 
                    return Expr::IntegerLiteral(p.as_span().as_str().to_string().parse::<i64>().unwrap())
                },
                Rule::name => return Expr::UnscopedVar(p.as_span().as_str().to_string(), crate::VarType::U),
                Rule::func_call => {
                    let mut name = "<unknown>".to_string();
                    let mut param = Expr::None();
                    for f in p.into_inner() {
                        match f.as_rule() {
                            Rule::name => { name = f.as_span().as_str().to_string()},
                            Rule::operand => param = Self::parse_operand(f),
                            Rule::lparen => {},
                            Rule::rparen => {},
                            x => eprintln!(">>> 22 Error converting parsed to ast {:?}", x)
                        }
                    }
                    return Expr::Call(name, vec![param], crate::VarType::U)
                }
                x => eprintln!(">>> 2 Error converting parsed to ast {:?}", x)
            }
        }
        panic!("should be able to handle math exp {:?}", "operand")
    }
    fn parse_mathbin_exp0(math_exp:Pair<'_, Rule>) -> Expr {
        for p in math_exp.into_inner() {
            match p.as_rule() {
                Rule::mathbin_exp1 => return Self::parse_mathbin_exp1(p),
                Rule::mathbin_exp2 => return Self::parse_mathbin_exp2(p),
                x => eprintln!(">>> 3 Error converting parsed to ast {:?}", x),
            }
        }
        panic!("should be able to handle math exp 0")
    }

    fn parse_mathbin_exp1(math_exp:Pair<'_, Rule>) -> Expr {
        let mut lhs = Expr::None();
        let mut rhs = Expr::None();
        let mut binop = BinOp::Add;

        for p in math_exp.into_inner() {
            match p.as_rule() {
                Rule::operand_lhs => { 
                    let inner = p.into_inner().next().expect("Can't unwrap the rule for math exp");
                    lhs = Self::parse_operand(inner)
                },
                Rule::operand_rhs => { 
                    let inner = p.into_inner().next().expect("Can't unwrap the rule for math exp");
                    rhs = Self::parse_operand(inner)
                },
                Rule::mathbin1 => {
                    let inner: Pair<'_, Rule> = p.into_inner().next().expect("Can't unwrap the rule for math exp");
                    match inner.as_rule() {
                        Rule::add => binop = BinOp::Add,
                        Rule::sub => binop = BinOp::Sub,
                        Rule::mul => binop = BinOp::Mul,
                        Rule::div => binop = BinOp::Div,
                        x => eprintln!(">>> 4 Error converting parsed to ast {:?}", x),
                    }
                },
                x => eprintln!(">>> 5 Error converting parsed to ast {:?}", x),
            }
        }
        return Expr::Binary(binop, vec![lhs, rhs], crate::VarType::U);
    }

    fn parse_mathbin_exp2(math_exp:Pair<'_, Rule>) -> Expr {
        let mut lhs = Expr::None();
        let mut rhs = Expr::None();
        let mut binop = BinOp::Add;

        for p in math_exp.into_inner() {
            match p.as_rule() {
                Rule::operand_lhs => { 
                    let inner = p.into_inner().next().expect("Can't unwrap the rule for math exp");
                    lhs = Self::parse_operand(inner)
                },
                Rule::operand_rhs => { 
                    let inner = p.into_inner().next().expect("Can't unwrap the rule for math exp");
                    rhs = Self::parse_operand(inner)
                },
                Rule::mathbin2 => {
                    let inner = p.into_inner().next().expect("Can't unwrap the rule for math exp");
                    match inner.as_rule() {
                        Rule::add => binop = BinOp::Add,
                        Rule::sub => binop = BinOp::Sub,
                        Rule::mul => binop = BinOp::Mul,
                        Rule::div => binop = BinOp::Div,
                        x => eprintln!(">>> 6 Error converting parsed to ast {:?}", x),
                    }
                },
                x => eprintln!(">>> 7 Error converting parsed to ast {:?}", x),
            }
        }
        return Expr::Binary(binop, vec![lhs, rhs], crate::VarType::U);
    }


    fn parse_math_expression_all(math_exp_all:Pair<'_, Rule>) -> Expr {
        match math_exp_all.as_rule() {
            Rule::math_expression_lhs | Rule::math_expression_rhs => {
                // get inner expression
                let inner = math_exp_all.into_inner().next().expect("Can't unwrap the rule for math exp");
                return  Self::parse_math_expression_all(inner)
            },
            Rule::math_expression_all => { 
                let inner = math_exp_all.into_inner().next().expect("Can't unwrap the rule for math exp all");
                match inner.as_rule() {
                    Rule::mathbin_exp0 => { return Self::parse_mathbin_exp0(inner)},
                    Rule::mathbin_exp1 => { return Self::parse_mathbin_exp1(inner)},
                    Rule::mathbin_exp2 => { return Self::parse_mathbin_exp2(inner)},
                    Rule::operand=> { return Self::parse_operand(inner)},
                    x => eprintln!(">>> 8 Error converting parsed to ast {:?}", x),
                }
            }
            x => eprintln!(">>> 9 Error converting parsed to ast {:?}", x),
        }
        panic!("can't handle math expression");
    }

    fn parse_math_expression_bin(math_exp_bin:Pair<'_, Rule>) -> Expr {
        let mut lhs = Expr::None();
        let mut rhs = Expr::None();
        let mut binop = BinOp::Add;

        for exp in math_exp_bin.into_inner() {
            match exp.as_rule() {
                Rule::math_expression_lhs => lhs = Self::parse_math_expression_all(exp),
                Rule::math_expression_rhs => rhs = Self::parse_math_expression_all(exp),
                Rule::math_expression_bin => {
                    let inner: Pair<'_, Rule> = exp.into_inner().next().expect("Can't unwrap the rule for math exp");
                    match inner.as_rule() {
                        Rule::add => binop = BinOp::Add,
                        Rule::sub => binop = BinOp::Sub,
                        Rule::mul => binop = BinOp::Mul,
                        Rule::div => binop = BinOp::Div,
                        x => println!(">>> 10 Error converting parsed to ast {:?}", x), 
                    }
                },
                x => println!(">>> 11 Error converting parsed to ast {:?}", x),
            }
        }
        return Expr::Binary(binop, vec![lhs, rhs], crate::VarType::U);
    }


    fn parse_math_expression(math_exp:Pair<'_, Rule>) -> Expr {
        for exp in math_exp.into_inner() {
            match exp.as_rule() {
                Rule::math_expression_all => return Self::parse_math_expression_all(exp),
                Rule::math_expression_bin => return Self::parse_math_expression_bin(exp),
                x => eprintln!(">>> 19 Error converting parsed to ast {:?}", x),
            }
        }
        return Expr::None();
    }

    
    fn parse_cmp_expression_all(cmp_exp:Pair<'_, Rule>) -> Expr {
        let mut lhs = Expr::None();
        let mut rhs = Expr::None();
        let mut binop = BinOp::Add;
        for exp in cmp_exp.into_inner() {
            match exp.as_rule() {
                Rule::operand_lhs => { 
                    let inner = exp.into_inner().next().expect("Can't unwrap the rule for cmp exp");
                    lhs = Self::parse_operand(inner)
                },
                Rule::operand_rhs => { 
                    let inner = exp.into_inner().next().expect("Can't unwrap the rule for cmp exp");
                    rhs = Self::parse_operand(inner)
                },
                Rule::cmpbinop => {
                    let inner: Pair<'_, Rule> = exp.into_inner().next().expect("Can't unwrap the rule for cmp exp");
                    match inner.as_rule() {
                        Rule::lt => binop = BinOp::Lt,
                        Rule::lte => binop = BinOp::Lte,
                        Rule::gt => binop = BinOp::Gt,
                        Rule::gte => binop = BinOp::Gte,
                        Rule::eq => binop = BinOp::Eq,
                        x => eprintln!(">>> 18 Error converting parsed to ast {:?}", x),
                    }
                },
                x => eprintln!(">>> 5 Error converting parsed to ast {:?}", x),
            }
        }
        return Expr::Binary(binop, vec![lhs, rhs], crate::VarType::U)
    }

    fn parsed_math_assign_stmt_ast(math_assign:Pair<'_, Rule>) -> Stmt {
        let mut name = "<unknown>".to_string();
        let mut expression = Expr::None();
        for pair in math_assign.into_inner() {
            match pair.as_rule() {
                Rule::math_expression => expression = Self::parse_math_expression(pair),
                Rule::name => { name = pair.as_span().as_str().to_string() },
                Rule::semi => {} // noop
                x => println!(">>> 12 Error converting parsed to ast {:?}", x)
            }
        }
        return Stmt::Assign(Expr::UnscopedVar(name, crate::VarType::U), expression);
    }

    fn parsed_while(while_pair:Pair<'_, Rule>) -> Stmt {
        let mut inner_statements = vec![];
        let mut cmp = Expr::None();
        for pair in while_pair.into_inner() {
            match pair.as_rule() {
                Rule::cmp_expression => cmp = Self::parse_cmp_expression_all(pair),
                Rule::statements => { inner_statements = Self::parsed_to_ast(pair.into_inner()); }
                x => eprintln!(">>> 13 Error converting parsed to ast {:?}", x),
            }
        }
        return Stmt::While(cmp, inner_statements);
    }

    fn parsed_ifelse(if_pair:Pair<'_, Rule>) -> Stmt {
        let mut true_block = vec![];
        let mut false_block = vec![];
        let mut cmp = Expr::None();
        for pair in if_pair.into_inner() {
            match pair.as_rule() {
                Rule::cmp_expression => cmp = Self::parse_cmp_expression_all(pair),
                Rule::true_block => { 
                    let inner: Pair<'_, Rule> = pair.into_inner().next().expect("Can't unwrap the rule for if statements");
                    true_block = Self::parsed_to_ast(inner.into_inner())
                },
                Rule::false_block => { 
                    let inner: Pair<'_, Rule> = pair.into_inner().next().expect("Can't unwrap the rule for if statements");
                    false_block = Self::parsed_to_ast(inner.into_inner())
                }
                x => eprintln!(">>> 14 Error converting parsed to ast {:?}", x),
            }
        }
        return Stmt::If(cmp, true_block, false_block);
    }

    fn parsed_print(pairs:Pair<'_, Rule>) -> Stmt {
        let mut exp = Expr::None();
        for pair in pairs.into_inner() {
            match pair.as_rule() {
                Rule::math_expression => exp = Self::parse_math_expression(pair),
                Rule::semi => {},
                x => eprintln!(">>> 20 Error converting parsed to ast {:?}", x),
            }
        }
        return Stmt::Print(exp);
    }

    fn parsed_return(pairs:Pair<'_, Rule>) -> Stmt {
        let mut exp = Expr::None();
        for pair in pairs.into_inner() {
            match pair.as_rule() {
                Rule::math_expression => exp = Self::parse_math_expression(pair),
                Rule::semi => {},
                x => eprintln!(">>> 21 Error converting parsed to ast {:?}", x),
            }
        }
        return Stmt::Return(exp);
    }

    fn parsed_to_ast(parsed: Pairs<'_, Rule>) -> Vec<Stmt> {
        let mut statements = vec![];
        for pair in parsed {
            match pair.as_rule() {
                Rule::statements => return Self::parsed_to_ast(pair.into_inner()),
                Rule::func_stmt => statements.push(Self::parse_fn_ast(pair)),
                Rule::decl_assign_stmt => statements.push(Self::parsed_decl_assign_stmt_ast(pair)),
                Rule::mathassign_stmt => statements.push(Self::parsed_math_assign_stmt_ast(pair)),
                Rule::print_stmt => statements.push(Self::parsed_print(pair)),
                Rule::while_stmt => statements.push(Self::parsed_while(pair)),
                Rule::return_stmt => statements.push(Self::parsed_return(pair)),
                Rule::if_stmt =>  {
                    let inner: Pair<'_, Rule> = pair.into_inner().next().expect("Can't unwrap the rule for if statements");
                    match inner.as_rule() {
                        Rule::ifelse => statements.push(Self::parsed_ifelse(inner)),
                        x => eprintln!(">>> 16 Error converting parsed to ast {:?}", x),
                    }
                },
                Rule::semi => {}
                x => eprintln!(">>> 15 Error converting parsed to ast {:?}", x),
            }
        }
        return statements
    }

    pub fn parse(input:String) -> Vec<Stmt> {
        let result = WabParser::parse(Rule::statements, &input);

        let parsed = match result {
            Ok(pest_parsed) => pest_parsed,
            Err(error) => { 
                let position = error.clone().line_col;
                let (l, col) = match position {
                    pest::error::LineColLocation::Pos((linenum, colnum)) => (linenum, colnum),
                    pest::error::LineColLocation::Span((linenum, colnum), _) => (linenum, colnum),
                };
                let snip = error.line();
                eprintln!("{:#<1$}", "", 40);
                eprintln!("# Syntax error --> line {}:{}\n", l, col);
                let error_str = format!("\t| {}\n\t| {:pos$}^-- error\n",  snip, "", pos=col);
                eprintln!("{}", error_str);
                eprintln!("{:?}", error);
                return vec![];
            },
        };
        return  Self::parsed_to_ast(parsed);
    }
}

#[cfg(test)]
mod test {
    use pest::Parser;

    use crate::{Rule, WabParser, Wab};


    #[test]
    fn test_err() {
        let input = "crampus 9 if while < {{";
        let result = Wab::parse((&input).to_string());
        assert_eq!(result, vec![])
    }

    #[test]
    fn test_big() {
        let input = "func fact(n) {
            var result = 1;
            var x = 1;
            while x < n {
                result = result * x;
            x = x + 1;
            }
            return result;
        }
        if a < b {
            print a;
         } else {
            print b;
         }
        var n = 0;
        while n < 10 {
           print fact(n);
           n = n + 1;
        };";

        let result = WabParser::parse(Rule::statements, input);
        result.expect("unsuccessful parse") // unwrap the parse result
        .next().unwrap();
        let ast = Wab::parse((&input).to_string());
        println!("{:?}", ast);

    }

    #[test]
    fn test_fn() {
        let input = "
           print fact(n);
           n = n + 1;";

        let result = WabParser::parse(Rule::statements, input);
        result.expect("unsuccessful parse") // unwrap the parse result
        .next().unwrap();
        let ast = Wab::parse((&input).to_string());
        println!("{:?}", ast);

    }

    #[test]
    fn test_number() {
        let result = WabParser::parse(Rule::number, "-273.15");
        result.expect("unsuccessful parse") // unwrap the parse result
        .next().unwrap();
    }

    #[test]
    fn test_decl_assign() {
        let result = WabParser::parse(Rule::decl_assign_stmt, "var x = 42   ;");
        result.expect("unsuccessful parse") // unwrap the parse result
        .next().unwrap();
    }

    #[test]
    fn test_math_exp() {
        let tests = vec![
            "42 * 54",
            "3 + 4",
            "1 * 2 + 3 * 4",
            "1 + 2 + 3",
            "1 * 2 + 3",
            "(1 + 2) * 3",
            "(1 + 2) * (3 + 4)",
            "g * 4",
            "(1 + c) * b",
        ];
        for input in tests {
            let result = WabParser::parse(Rule::math_expression, input);
            result.expect("unsuccessful parse") // unwrap the parse result
            .next().unwrap();
        }
    }


    #[test]
    fn test_print() {
        let tests = vec![
            "print a;",
            "print 45;",
            "print func(45);",
            "print func(a);",
        ];
        for input in tests {
            let result = WabParser::parse(Rule::print_stmt, input);
            result.expect("unsuccessful parse") // unwrap the parse result
            .next().unwrap();
        }
    }

    #[test]
    fn test_cmp_exp() {
        let tests = vec![
            "42 > 54",
            "3 < 4",
            "a == b",
            "a >= 4"
        ];
        for input in tests {
            let result = WabParser::parse(Rule::cmp_expression, input);
            result.expect("unsuccessful parse") // unwrap the parse result
            .next().unwrap();
        }
    }

    #[test]
    fn test_math_assign() {
        let tests = vec![
            "x = 42 * 54;"
        ];
        for input in tests {
            let result = WabParser::parse(Rule::mathassign_stmt, input);
            result.expect("unsuccessful parse") // unwrap the parse result
            .next().unwrap();
        }
    }

    #[test]
    fn test_return() {
        let result = WabParser::parse(Rule::return_stmt, "return x;");
        result.expect("unsuccessful parse") // unwrap the parse result
        .next().unwrap();
    }

    #[test]
    fn test_stmt_block() {
        let input = "
            var result = 1;
            var x = 1;

                result = result * x;
            x = x + 1;
            return result;
        var n = 0;
           print fact(n);
           n = n + 1;
        ";
        let result = WabParser::parse(Rule::statements, input);
        result.expect("unsuccessful parse") // unwrap the parse result
        .next().unwrap();
    }




}