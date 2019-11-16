#[derive(Debug, Clone)]
enum Binop {
    Add,
    Sub,
    Mul,
    Eq,
}

#[derive(Debug, Clone)]
enum Value {
    IntVal(i32),
    BoolVal(bool),
    FunVal(String, Expr),
}

#[derive(Debug, Clone)]
enum Expr {
    IntLit(i32),
    BoolLit(bool),
    Var(String),
    Binop(Binop, Box<Expr>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Fun(String, Box<Expr>),
}

fn eval(expr: &Expr) -> Value {
    Value::IntVal(0)
}

fn main() {
    let var_name = "x".to_string();
    let body = Box::new(Expr::Binop(
        Binop::Add,
        Box::new(Expr::Var(var_name.clone())),
        Box::new(Expr::IntLit(10)),
    ));
    let add_10 = Expr::Fun(var_name, body);
    println!("add_10: [{:?}", add_10);
    let app = Expr::App(Box::new(add_10), Box::new(Expr::IntLit(7)));
    let result = eval(&app);
    println!("{:?}", result);
}
