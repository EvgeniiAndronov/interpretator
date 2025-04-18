use std::collections::HashMap;
use std::{env, fs};

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Keyword(String),
    Identifier(String),
    Literal(i32),
    LiteralFloat(f64),
    LiteralString(String),
    Operator(String),
    Semicolon,
    Colon,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    KeywordArray,
    KeywordAddB,
}

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Int,
    Float,
    Str,
    Array(Box<Type>, usize),
}

impl Type {
    fn default_value(&self) -> Value {
        match self {
            Type::Int => Value::Int(0),
            Type::Float => Value::Float(0.0),
            Type::Str => Value::String(String::new()),
            Type::Array(t, size) => {
                let default = t.default_value();
                Value::Array(vec![default; *size], t.clone(), *size)
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Literal(i32),
    LiteralFloat(f64),
    LiteralString(String),
    Identifier(String),
    BinaryOp(Box<Expr>, String, Box<Expr>),
    Grouped(Box<Expr>),
    ArrayIndex(Box<Expr>, Box<Expr>),
    ArrayLiteral(Vec<Expr>),
}

#[derive(Debug, Clone)]
enum Value {
    Int(i32),
    Float(f64),
    String(String),
    Bool(bool),
    Array(Vec<Value>, Box<Type>, usize),
}

#[derive(Debug)]
enum Statement {
    VarDecl(String, Type),
    Assignment(String, Expr),
    ConsoleOut(Expr),
    If(Expr, Vec<Statement>, Option<Vec<Statement>>),
    For(String, Expr, Expr, Expr, Vec<Statement>),
    Block(Vec<Statement>),
    ArrayDecl(String, Box<Type>, usize),
    ArrayAdd(String, Expr),
    ArrayAssignment(String, Expr, Expr),
}

struct Interpreter {
    variables: HashMap<String, Value>,
    var_types: HashMap<String, Type>,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
            var_types: HashMap::new(),
        }
    }

    fn check_types(value: &Value, expected_type: &Type) -> bool {
        match (value, expected_type) {
            (Value::Int(_), Type::Int) => true,
            (Value::Float(_), Type::Float) => true,
            (Value::String(_), Type::Str) => true,
            (Value::Array(_, t, _), Type::Array(expected_t, _)) => t == expected_t,
            _ => false,
        }
    }

    fn eval_expr(&self, expr: &Expr) -> Value {
        match expr {
            Expr::Literal(n) => Value::Int(*n),
            Expr::LiteralFloat(f) => Value::Float(*f),
            Expr::LiteralString(s) => Value::String(s.clone()),
            Expr::Identifier(name) => self.variables.get(name).unwrap().clone(),
            Expr::BinaryOp(lhs, op, rhs) => {
                let lhs_val = self.eval_expr(lhs);
                let rhs_val = self.eval_expr(rhs);
                match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => match op.as_str() {
                        "+" => Value::Int(a + b),
                        "-" => Value::Int(a - b),
                        "*" => Value::Int(a * b),
                        "/" => Value::Int(a / b),
                        ">" => Value::Bool(a > b),
                        "<" => Value::Bool(a < b),
                        "==" => Value::Bool(a == b),
                        "!=" => Value::Bool(a != b),
                        _ => panic!("Unknown operator for int: {}", op),
                    },
                    (Value::Float(a), Value::Float(b)) => match op.as_str() {
                        "+" => Value::Float(a + b),
                        "-" => Value::Float(a - b),
                        "*" => Value::Float(a * b),
                        "/" => Value::Float(a / b),
                        ">" => Value::Bool(a > b),
                        "<" => Value::Bool(a < b),
                        "==" => Value::Bool(a == b),
                        "!=" => Value::Bool(a != b),
                        _ => panic!("Unknown operator for float: {}", op),
                    },
                    (Value::String(a), Value::String(b)) if op == "+" => Value::String(a + &b),
                    (Value::Bool(a), Value::Bool(b)) => match op.as_str() {
                        "&&" => Value::Bool(a && b),
                        "||" => Value::Bool(a || b),
                        _ => panic!("Unknown operator for bool: {}", op),
                    },
                    _ => panic!("Type mismatch for operation"),
                }
            },
            Expr::Grouped(expr) => self.eval_expr(expr),
            Expr::ArrayIndex(arr_expr, index_expr) => {
                let arr = self.eval_expr(arr_expr);
                let index = self.eval_expr(index_expr);
                if let (Value::Array(elements, _, _), Value::Int(i)) = (arr, index) {
                    if i < 0 || i >= elements.len() as i32 {
                        panic!("Array index out of bounds");
                    }
                    elements[i as usize].clone()
                } else {
                    panic!("Invalid array access");
                }
            },
            Expr::ArrayLiteral(_) => todo!("Implement array literals"),
        }
    }

    fn exec(&mut self, stmt: &Statement) {
        match stmt {
            Statement::VarDecl(name, var_type) => {
                self.var_types.insert(name.clone(), var_type.clone());
                match var_type {
                    Type::Int => self.variables.insert(name.clone(), Value::Int(0)),
                    Type::Float => self.variables.insert(name.clone(), Value::Float(0.0)),
                    Type::Str => self.variables.insert(name.clone(), Value::String(String::new())),
                    Type::Array(..) => panic!("Array declaration should use ArrayDecl"),
                };
            },
            Statement::ArrayDecl(name, elem_type, size) => {
                let array_type = Type::Array(elem_type.clone(), *size);
                let default_value = array_type.default_value();
                self.var_types.insert(name.clone(), array_type);
                self.variables.insert(name.clone(), default_value);
            },
            Statement::ArrayAdd(name, value_expr) => {
                let value = self.eval_expr(value_expr);
                if let Some(Value::Array(elements, elem_type, size)) = self.variables.get_mut(name) {
                    if elements.len() >= *size {
                        panic!("Array overflow");
                    }
                    if !Self::check_types(&value, elem_type) {
                        panic!("Type mismatch in array add");
                    }
                    elements.push(value);
                } else {
                    panic!("Array {} not found", name);
                }
            },
            Statement::ArrayAssignment(name, index_expr, value_expr) => {
                let index = self.eval_expr(index_expr);
                let value = self.eval_expr(value_expr);
                let (elements, elem_type) = match self.variables.get_mut(name) {
                    Some(Value::Array(e, t, _)) => (e, t),
                    _ => panic!("Array {} not found", name),
                };

                if !Self::check_types(&value, elem_type) {
                    panic!("Type mismatch in array assignment");
                }

                if let Value::Int(i) = index {
                    if i < 0 || i >= elements.len() as i32 {
                        panic!("Array index out of bounds");
                    }
                    elements[i as usize] = value;
                } else {
                    panic!("Array index must be integer");
                }
            },
            Statement::Assignment(name, expr) => {
                let value = self.eval_expr(expr);
                if let Some(var_type) = self.var_types.get(name) {
                    match (var_type, &value) {
                        (Type::Int, Value::Int(_)) |
                        (Type::Float, Value::Float(_)) |
                        (Type::Str, Value::String(_)) => {
                            self.variables.insert(name.clone(), value);
                        },
                        _ => panic!("Type mismatch for variable {}", name),
                    }
                } else {
                    panic!("Undefined variable {}", name);
                }
            },
            Statement::ConsoleOut(expr) => {
                let value = self.eval_expr(expr);
                match value {
                    Value::Int(n) => println!("{}", n),
                    Value::Float(f) => println!("{}", f),
                    Value::String(s) => println!("{}", s),
                    Value::Bool(b) => println!("{}", b),
                    Value::Array(elements, _, _) => {
                        print!("[");
                        for (i, elem) in elements.iter().enumerate() {
                            if i > 0 { print!(", "); }
                            match elem {
                                Value::Int(n) => print!("{}", n),
                                Value::Float(f) => print!("{}", f),
                                Value::String(s) => print!("{}", s),
                                Value::Bool(b) => print!("{}", b),
                                _ => print!("?"),
                            }
                        }
                        println!("]");
                    }
                }
            },
            Statement::If(cond, then_block, else_block) => {
                let cond_val = match self.eval_expr(cond) {
                    Value::Bool(b) => b,
                    _ => panic!("Condition must be boolean"),
                };
                if cond_val {
                    self.execute_block(then_block);
                } else if let Some(block) = else_block {
                    self.execute_block(block);
                }
            },
            Statement::For(var, init, cond, update, body) => {
                self.exec(&Statement::VarDecl(var.clone(), Type::Int));
                self.exec(&Statement::Assignment(var.clone(), init.clone()));
                loop {
                    let cond_val = match self.eval_expr(cond) {
                        Value::Bool(b) => b,
                        _ => panic!("For loop condition must be boolean"),
                    };
                    if !cond_val { break; }
                    self.execute_block(body);
                    self.exec(&Statement::Assignment(var.clone(), update.clone()));
                }
            },
            Statement::Block(statements) => {
                self.execute_block(statements);
            },
        }
    }

    fn execute_block(&mut self, statements: &[Statement]) {
        for stmt in statements {
            self.exec(stmt);
        }
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            ' ' | '\n' | '\t' => continue,
            ':' => tokens.push(Token::Colon),
            ';' => tokens.push(Token::Semicolon),
            ',' => tokens.push(Token::Comma),
            '{' => tokens.push(Token::LBrace),
            '}' => tokens.push(Token::RBrace),
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '+' | '-' | '=' | '*' | '/' | '>' | '<' | '!' | '&' | '|' => {
                let mut op = c.to_string();
                if let Some(&next) = chars.peek() {
                    if (c == '=' && next == '=')
                        || (c == '!' && next == '=')
                        || (c == '&' && next == '&')
                        || (c == '|' && next == '|')
                    {
                        op.push(chars.next().unwrap());
                    }
                }
                tokens.push(Token::Operator(op));
            }
            '"' => {
                let mut s = String::new();
                while let Some(&next) = chars.peek() {
                    if next == '"' {
                        chars.next();
                        break;
                    }
                    s.push(chars.next().unwrap());
                }
                tokens.push(Token::LiteralString(s));
            }
            '0'..='9' => {
                let mut num = String::new();
                num.push(c);
                let mut is_float = false;
                while let Some(&next) = chars.peek() {
                    if next.is_digit(10) {
                        num.push(chars.next().unwrap());
                    } else if next == '.' && !is_float {
                        is_float = true;
                        num.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                if is_float {
                    tokens.push(Token::LiteralFloat(num.parse().unwrap()));
                } else {
                    tokens.push(Token::Literal(num.parse().unwrap()));
                }
            }
            '[' => tokens.push(Token::LBracket),
            ']' => tokens.push(Token::RBracket),
            _ if c.is_alphabetic() => {
                let mut ident = String::new();
                ident.push(c);
                while let Some(&next) = chars.peek() {
                    if next.is_alphanumeric() {
                        ident.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                match ident.as_str() {
                    "int" => tokens.push(Token::Keyword("int".to_string())),
                    "float" => tokens.push(Token::Keyword("float".to_string())),
                    "str" => tokens.push(Token::Keyword("str".to_string())),
                    "bool" => tokens.push(Token::Keyword("bool".to_string())),
                    "true" => tokens.push(Token::Literal(1)),
                    "false" => tokens.push(Token::Literal(0)),
                    "if" => tokens.push(Token::Keyword("if".to_string())),
                    "else" => tokens.push(Token::Keyword("else".to_string())),
                    "for" => tokens.push(Token::Keyword("for".to_string())),
                    "consoleout" => tokens.push(Token::Keyword("consoleout".to_string())),
                    "array" => tokens.push(Token::KeywordArray),
                    "addB" => tokens.push(Token::KeywordAddB),
                    _ => tokens.push(Token::Identifier(ident)),
                }
            }
            _ => panic!("Unknown character: {}", c),
        }
    }
    tokens
}

fn parse_expression(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    parse_logical_or(tokens, pos)
}

fn parse_logical_or(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_logical_and(tokens, pos)?;
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Operator(op) if op == "||" => {
                let op = op.clone();
                *pos += 1;
                let right = parse_logical_and(tokens, pos)?;
                left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
            }
            _ => break,
        }
    }
    Ok(left)
}

fn parse_logical_and(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_equality(tokens, pos)?;
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Operator(op) if op == "&&" => {
                let op = op.clone();
                *pos += 1;
                let right = parse_equality(tokens, pos)?;
                left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
            }
            _ => break,
        }
    }
    Ok(left)
}

fn parse_equality(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_relational(tokens, pos)?;
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Operator(op) if op == "==" || op == "!=" => {
                let op = op.clone();
                *pos += 1;
                let right = parse_relational(tokens, pos)?;
                left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
            }
            _ => break,
        }
    }
    Ok(left)
}

fn parse_relational(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_additive(tokens, pos)?;
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Operator(op) if op == "<" || op == ">" || op == "<=" || op == ">=" => {
                let op = op.clone();
                *pos += 1;
                let right = parse_additive(tokens, pos)?;
                left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
            }
            _ => break,
        }
    }
    Ok(left)
}

fn parse_additive(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_multiplicative(tokens, pos)?;
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Operator(op) if op == "+" || op == "-" => {
                let op = op.clone();
                *pos += 1;
                let right = parse_multiplicative(tokens, pos)?;
                left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
            }
            _ => break,
        }
    }
    Ok(left)
}

fn parse_multiplicative(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_primary(tokens, pos)?;
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Operator(op) if op == "*" || op == "/" => {
                let op = op.clone();
                *pos += 1;
                let right = parse_primary(tokens, pos)?;
                left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
            }
            _ => break,
        }
    }
    Ok(left)
}

fn parse_array_declaration(tokens: &[Token], pos: &mut usize) -> Result<Statement, String> {
    expect_token(tokens, pos, Token::KeywordArray)?;

    let name = match &tokens[*pos] {
        Token::Identifier(name) => name.clone(),
        _ => return Err("Expected array name".to_string()),
    };
    *pos += 1;

    expect_token(tokens, pos, Token::LBracket)?;

    let elem_type = match &tokens[*pos] {
        Token::Keyword(kw) => match kw.as_str() {
            "int" => Type::Int,
            "float" => Type::Float,
            "str" => Type::Str,
            _ => return Err("Invalid array element type".to_string()),
        },
        _ => return Err("Expected array element type".to_string()),
    };
    *pos += 1;

    expect_token(tokens, pos, Token::Comma)?;

    let size = match &tokens[*pos] {
        Token::Literal(n) => *n as usize,
        _ => return Err("Expected array size".to_string()),
    };
    *pos += 1;

    expect_token(tokens, pos, Token::RBracket)?;
    expect_token(tokens, pos, Token::Semicolon)?;

    Ok(Statement::ArrayDecl(name, Box::new(elem_type), size))
}

fn parse_array_add(tokens: &[Token], pos: &mut usize) -> Result<Statement, String> {
    let name = match &tokens[*pos] {
        Token::Identifier(name) => name.clone(),
        _ => return Err("Expected array name".to_string()),
    };
    *pos += 1;

    expect_token(tokens, pos, Token::KeywordAddB)?;
    expect_token(tokens, pos, Token::LParen)?;

    let value = parse_expression(tokens, pos)?;

    expect_token(tokens, pos, Token::RParen)?;
    expect_token(tokens, pos, Token::Semicolon)?;

    Ok(Statement::ArrayAdd(name, value))
}

fn parse_primary(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    match &tokens[*pos] {
        Token::LBracket => parse_array_literal(tokens, pos),
        Token::Literal(n) => {
            *pos += 1;
            Ok(Expr::Literal(*n))
        }
        Token::LiteralFloat(f) => {
            *pos += 1;
            Ok(Expr::LiteralFloat(*f))
        }
        Token::LiteralString(s) => {
            *pos += 1;
            Ok(Expr::LiteralString(s.clone()))
        }
        Token::Identifier(name) => {
            let name = name.clone();
            *pos += 1;

            // Добавляем обработку доступа по индексу
            if *pos < tokens.len() && tokens[*pos] == Token::LBracket {
                *pos += 1;
                let index = parse_expression(tokens, pos)?;
                expect_token(tokens, pos, Token::RBracket)?;
                Ok(Expr::ArrayIndex(Box::new(Expr::Identifier(name)), Box::new(index)))
            } else {
                Ok(Expr::Identifier(name))
            }
        }
        Token::LParen => {
            *pos += 1;
            let expr = parse_expression(tokens, pos)?;
            expect_token(tokens, pos, Token::RParen)?;
            Ok(Expr::Grouped(Box::new(expr)))
        }
        _ => Err("Expected number, identifier or parenthesis".to_string()),
    }
}

fn parse_for_loop(tokens: &[Token], pos: &mut usize) -> Result<Statement, String> {
    *pos += 1;
    expect_token(tokens, pos, Token::LParen)?;

    let var = match &tokens[*pos] {
        Token::Identifier(name) => name.clone(),
        _ => return Err("Expected identifier in for loop initialization".to_string()),
    };
    *pos += 1;
    expect_token(tokens, pos, Token::Operator("=".to_string()))?;
    let init = parse_expression(tokens, pos)?;
    expect_token(tokens, pos, Token::Semicolon)?;

    let cond = parse_expression(tokens, pos)?;
    expect_token(tokens, pos, Token::Semicolon)?;

    let update = parse_expression(tokens, pos)?;
    expect_token(tokens, pos, Token::RParen)?;

    let body = parse_block(tokens, pos)?;

    Ok(Statement::For(var, init, cond, update, body))
}

fn parse_array_literal(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    expect_token(tokens, pos, Token::LBracket)?;
    let mut elements = Vec::new();

    while *pos < tokens.len() && tokens[*pos] != Token::RBracket {
        elements.push(parse_expression(tokens, pos)?);
        if *pos < tokens.len() && tokens[*pos] == Token::Comma {
            *pos += 1;
        }
    }

    expect_token(tokens, pos, Token::RBracket)?;
    Ok(Expr::ArrayLiteral(elements))
}

fn parse_statement(tokens: &[Token], pos: &mut usize) -> Result<Statement, String> {
    if *pos >= tokens.len() {
        return Err("Unexpected end of input".to_string());
    }

    match &tokens[*pos] {
        Token::Keyword(kw) if kw == "consoleout" => {
            *pos += 1;
            expect_token(tokens, pos, Token::LParen)?;
            let expr = parse_expression(tokens, pos)?;
            expect_token(tokens, pos, Token::RParen)?;
            expect_token(tokens, pos, Token::Semicolon)?;
            Ok(Statement::ConsoleOut(expr))
        }
        Token::Keyword(kw) if kw == "if" => {
            *pos += 1;
            expect_token(tokens, pos, Token::LParen)?;
            let cond = parse_expression(tokens, pos)?;
            expect_token(tokens, pos, Token::RParen)?;
            let then_block = parse_block(tokens, pos)?;

            let else_block = if *pos < tokens.len() && tokens[*pos] == Token::Keyword("else".to_string()) {
                *pos += 1;
                Some(parse_block(tokens, pos)?)
            } else {
                None
            };

            Ok(Statement::If(cond, then_block, else_block))
        }
        Token::Keyword(kw) if kw == "for" => parse_for_loop(tokens, pos),
        Token::KeywordArray => parse_array_declaration(tokens, pos),
        Token::Identifier(_name) if peek(tokens, *pos + 1, Token::KeywordAddB) => {
            parse_array_add(tokens, pos)
        }
        Token::Identifier(name) => {
            let name = name.clone();
            *pos += 1;

            if *pos < tokens.len() {
                match &tokens[*pos] {
                    Token::Colon => {
                        // Обработка объявления переменной
                        *pos += 1;
                        match &tokens[*pos] {
                            Token::Keyword(kw) => {
                                let var_type = match kw.as_str() {
                                    "int" => Type::Int,
                                    "float" => Type::Float,
                                    "str" => Type::Str,
                                    _ => return Err(format!("Unknown type: {}", kw)),
                                };
                                *pos += 1;
                                expect_token(tokens, pos, Token::Semicolon)?;
                                Ok(Statement::VarDecl(name, var_type))
                            }
                            _ => Err("Expected variable type".to_string()),
                        }
                    }
                    Token::LBracket => {
                        // Обработка присваивания по индексу array[index] = value
                        *pos += 1;
                        let index = parse_expression(tokens, pos)?;
                        expect_token(tokens, pos, Token::RBracket)?;
                        expect_token(tokens, pos, Token::Operator("=".to_string()))?;
                        let value = parse_expression(tokens, pos)?;
                        expect_token(tokens, pos, Token::Semicolon)?;
                        Ok(Statement::ArrayAssignment(name, index, value))
                    }
                    Token::Operator(op) if op == "=" => {
                        // Обычное присваивание
                        *pos += 1;
                        let expr = parse_expression(tokens, pos)?;
                        expect_token(tokens, pos, Token::Semicolon)?;
                        Ok(Statement::Assignment(name, expr))
                    }
                    _ => Err("Expected ':', '=' or '[' after identifier".to_string()),
                }
            } else {
                Err("Unexpected end of input".to_string())
            }
        }
        Token::LBrace => {
            let block = parse_block(tokens, pos)?;
            Ok(Statement::Block(block))
        }
        _ => Err(format!("Unexpected token: {:?}", tokens[*pos])),
    }
}

fn parse_block(tokens: &[Token], pos: &mut usize) -> Result<Vec<Statement>, String> {
    expect_token(tokens, pos, Token::LBrace)?;
    let mut statements = Vec::new();

    while *pos < tokens.len() && tokens[*pos] != Token::RBrace {
        statements.push(parse_statement(tokens, pos)?);
    }

    expect_token(tokens, pos, Token::RBrace)?;
    Ok(statements)
}

fn expect_token(tokens: &[Token], pos: &mut usize, expected: Token) -> Result<(), String> {
    if *pos >= tokens.len() {
        return Err(format!("Expected {:?} but reached end of input", expected));
    }
    if tokens[*pos] != expected {
        return Err(format!(
            "Expected {:?} but found {:?}",
            expected, tokens[*pos]
        ));
    }
    *pos += 1;
    Ok(())
}

fn peek(tokens: &[Token], pos: usize, expected: Token) -> bool {
    tokens.get(pos) == Some(&expected)
}

fn parse_program(tokens: &[Token]) -> Vec<Statement> {
    let mut statements = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        match parse_statement(tokens, &mut pos) {
            Ok(stmt) => statements.push(stmt),
            Err(e) => {
                let context = if pos < tokens.len() {
                    format!(" at token {:?} (position {})", tokens[pos], pos)
                } else {
                    " at end of input".to_string()
                };
                panic!("Parse error: {}{}", e, context);
            }
        }
    }
    statements
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: lengevg <filename.evg>");
        std::process::exit(1);
    }

    let filename = &args[1];
    let code = match fs::read_to_string(filename) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error reading file {}: {}", filename, e);
            std::process::exit(1);
        }
    };

    let tokens = tokenize(&code);
    let program = parse_program(&tokens);
    let mut interpreter = Interpreter::new();
    interpreter.execute_block(&program);
}