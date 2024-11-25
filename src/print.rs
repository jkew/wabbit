use super::Expr;
use super::Program;
use super::Stmt;
use super::Wab;

impl Wab {
    pub fn pretty_print(program: Program, header:String) {
        println!("; {:#<1$}", "", 40);
        println!("; # {}", header);
        println!("; {:#<1$}", "", 40);
        Self::_pretty_print(program.statements, 0 as usize)
    }
    fn _pretty_print_expr(expressions: Vec<Expr>, indent: usize) {
        for e in expressions {
            
            match e {
                Expr::Binary(op, p, vt) => {
                    Self::_pretty_print_expr(p[0..1].to_vec(), 0);
                    print!(" {:?} ", op);
                    Self::_pretty_print_expr(p[1..].to_vec(), 0);

                },
                Expr::ScopedVar(s, name, t) => {
                    print!("local_{:?} {} {:?}", s, name , t)
                },
                Expr::GlobalVar(name, t) => {
                    print!("global {} {:?}", name , t)
                },
                Expr::IntegerLiteral(i) => {
                    print!("{}i", i )
                },
                Expr::FloatLiteral(i) => {
                    print!("{}f", i )
                },
                _ => print!("{:indent$}{:?}", "", e, indent = indent)
            }
        }
    }
    fn _pretty_print(statements: Vec<Stmt>, indent: usize) {
        let index_default = 4;
        for s in statements {
            match s {
                Stmt::If(e, t, f) => {
                    print!(";{:indent$}if ", "", indent = indent);
                    Self::_pretty_print_expr(vec![e], 0);
                    println!("");
                    Self::_pretty_print(t, indent + index_default);
                    println!(";{:indent$}else", "", indent = indent);
                    Self::_pretty_print(f, indent + index_default);
                    println!(";{:indent$}end", "", indent = indent);
                }
                Stmt::While(e, t) => {
                    print!(";{:indent$}while ", "", indent = indent);
                    Self::_pretty_print_expr(vec![e], 0);
                    println!("");
                    Self::_pretty_print(t, indent + index_default);
                    println!(";{:indent$}end", "", indent = indent);
                }
                Stmt::Fn(n, p, s, vt) => {
                    print!(";{:indent$}func {} {:?} (", "", n, vt, indent = indent);
                    Self::_pretty_print_expr(p, 0);
                    println!("){{");
                    Self::_pretty_print(s, indent + index_default);
                    println!(";{:indent$}}}", "", indent = indent);
                }
                Stmt::Scope(x) => {
                    println!(";{:indent$}# scope {:?}", "", x, indent = indent);
                }
                Stmt::EndScope(x) => {
                    println!(";{:indent$}# end scope {:?}", "", x, indent = indent);
                }
                Stmt::MultiStmt(x) => {
                    Self::_pretty_print(x, indent);
                }
                Stmt::Assign(x1, x2) => {
                    print!(";{:indent$}", "", indent = indent);
                    Self::_pretty_print_expr(vec![x1], 0);
                    print!(" = ");
                    Self::_pretty_print_expr(vec![x2], 0);
                    println!("");
                },
                Stmt::Print(e) => {
                    print!(";{:indent$}print ", "", indent = indent);
                    Self::_pretty_print_expr(vec![e], 0);
                    println!("")
                }
                Stmt::Alloc(e, vt) => {
                    print!(";{:indent$}alloc {:?} ", "", vt, indent = indent);
                    Self::_pretty_print_expr(vec![e], 0);
                    println!("")
                }
                Stmt::Return(e) => {
                    print!(";{:indent$}return ", "", indent = indent);
                    Self::_pretty_print_expr(vec![e], 0);
                    println!("")
                }
                Stmt::LLVM(code, comment) => { 
                    let mut cmt_str = "".to_string();

                    if comment.len() > 0 {
                        match &comment[0] {
                            Stmt::Comment(s) => { cmt_str = format!("; {}", s) },
                            _ => panic!("can't handle non comment comments"),
                        }
                    }
                    println!("{:width$} {:width$}", code, cmt_str, width=40);

                    
                },
                Stmt::Noop() => {
                    
                }
                _ => println!(";{:indent$}{:?}", "", s, indent = indent),
            }
        }
    }
}
#[cfg(test)]
mod tests {
    use Wab;
    use test_fixtures::get_all_features_program;

    #[test]
    pub fn print_test() {
        println!("===== START PRINTING TESTS ====");
        Wab::pretty_print(get_all_features_program(), "test".to_string());
        println!("===== END PRINTING TESTS ====")
    }
}
