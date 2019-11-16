use std::collections::BTreeMap;

type Map<T> = BTreeMap<String, T>;

#[derive(Debug, Clone, Copy)]
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
    Closure(String, Box<Expr>, Map<Box<Value>>),
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

fn eval(expr: &Expr, env: &mut Map<Box<Value>>) -> Option<Box<Value>> {
    let mut eval_binop = |op, e1, e2| {
        let apply_binop = move |v1, v2| match op {
            Binop::Add => Value::IntVal(v1 + v2),
            Binop::Sub => Value::IntVal(v1 + v2),
            Binop::Mul => Value::IntVal(v1 * v2),
            Binop::Eq => Value::BoolVal(v1 == v2),
        };
        let ev1 = eval(e1, env)?;
        let ev2 = eval(e2, env)?;
        match (op, *ev1, *ev2) {
            (_, Value::IntVal(v1), Value::IntVal(v2)) => Some(Box::new(apply_binop(v1, v2))),
            (Binop::Eq, Value::BoolVal(b1), Value::BoolVal(b2)) => {
                Some(Box::new(Value::BoolVal(b1 == b2)))
            }
            _ => unreachable!(),
        }
    };
    match expr {
        Expr::Var(x) => {
            let val = env.get(x)?;
            Some(val.clone())
        }
        Expr::IntLit(n) => Some(Box::new(Value::IntVal(*n))),
        Expr::BoolLit(b) => Some(Box::new(Value::BoolVal(*b))),
        Expr::Binop(op, exp1, exp2) => eval_binop(*op, exp1, exp2),
        Expr::Fun(x, exp) => Some(Box::new(Value::Closure(
            x.clone(),
            exp.clone(),
            env.clone(),
        ))),
        Expr::App(fun, arg) => {
            let arg = eval(arg, env)?;
            let fun = eval(fun, env)?;
            match *fun {
                Value::Closure(x, body, mut fun_env) => {
                    fun_env.insert(x, arg);
                    eval(&*body, &mut fun_env)
                }
                _ => unreachable!(),
            }
        }
    }
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
    let mut env = Map::new();
    match eval(&app, &mut env) {
        None => println!("eval failed.\n"),
        Some(val) => println!("{:?}", val),
    }
}
