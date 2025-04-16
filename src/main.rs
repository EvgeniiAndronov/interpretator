use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Keyword(String),
    Identifier(String),
    Literal(i32),
    Operator(String),
    Semicolon,
    Colon,
    LParen,
    RParen,
    LBrace,
    RBrace,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            ' ' | '\n' | '\t' => continue,
            ':' => tokens.push(Token::Colon),
            ';' => tokens.push(Token::Semicolon),
            '{' => tokens.push(Token::LBrace),
            '}' => tokens.push(Token::RBrace),
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '+' | '-' | '=' | '*' | '/' | '>' | '<' => tokens.push(Token::Operator(c.to_string())),
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
                    "int" | "if" | "else" | "consoleout" => tokens.push(Token::Keyword(ident)),
                    _ => tokens.push(Token::Identifier(ident)),
                }
            }
            _ if c.is_digit(10) => {
                let mut num = String::new();
                num.push(c);
                while let Some(&next) = chars.peek() {
                    if next.is_digit(10) {
                        num.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Literal(num.parse().unwrap()));
            }
            _ => panic!("Неизвестный символ: {}", c),
        }
    }
    tokens
}

#[derive(Debug)]
enum Statement {
    VarDecl(String),
    Assignment(String, Expr),
    ConsoleOut(Expr),
    If(Expr, Vec<Statement>, Option<Vec<Statement>>),
    Block(Vec<Statement>),
}

#[derive(Debug)]
enum Expr {
    Literal(i32),
    Identifier(String),
    BinaryOp(Box<Expr>, String, Box<Expr>),
    Grouped(Box<Expr>),
}

struct Interpreter {
    variables: HashMap<String, i32>,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
        }
    }

    fn eval_expr(&self, expr: &Expr) -> i32 {
        match expr {
            Expr::Literal(n) => *n,
            Expr::Identifier(name) => *self.variables.get(name).expect("Undefined variable"),
            Expr::BinaryOp(lhs, op, rhs) => {
                let lhs_val = self.eval_expr(lhs);
                let rhs_val = self.eval_expr(rhs);
                match op.as_str() {
                    "+" => lhs_val + rhs_val,
                    "-" => lhs_val - rhs_val,
                    "*" => lhs_val * rhs_val,
                    "/" => lhs_val / rhs_val,
                    ">" => (lhs_val > rhs_val) as i32,
                    "<" => (lhs_val < rhs_val) as i32,
                    _ => panic!("Unknown operator: {}", op),
                }
            }
            Expr::Grouped(expr) => self.eval_expr(expr),
        }
    }

    fn exec(&mut self, stmt: &Statement) {
        match stmt {
            Statement::VarDecl(name) => {
                self.variables.insert(name.clone(), 0);
            }
            Statement::Assignment(name, expr) => {
                let value = self.eval_expr(expr);
                self.variables.insert(name.clone(), value);
            }
            Statement::ConsoleOut(expr) => {
                let value = self.eval_expr(expr);
                println!("{}", value);
            }
            Statement::If(cond, then_block, else_block) => {
                let cond_val = self.eval_expr(cond);
                if cond_val != 0 {
                    self.execute_block(then_block);
                } else if let Some(block) = else_block {
                    self.execute_block(block);
                }
            }
            Statement::Block(statements) => {
                self.execute_block(statements);
            }
        }
    }

    fn execute_block(&mut self, statements: &[Statement]) {
        for stmt in statements {
            self.exec(stmt);
        }
    }
}

fn parse_expression(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    parse_additive(tokens, pos)
}

fn parse_additive(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_multiplicative(tokens, pos)?;

    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Operator(op) if op == "+" || op == "-" || op == ">" || op == "<" => {
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

fn parse_primary(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    match &tokens[*pos] {
        Token::Literal(n) => {
            *pos += 1;
            Ok(Expr::Literal(*n))
        }
        Token::Identifier(name) => {
            *pos += 1;
            Ok(Expr::Identifier(name.clone()))
        }
        Token::LParen => {
            *pos += 1;
            let expr = parse_expression(tokens, pos)?;
            if *pos >= tokens.len() || tokens[*pos] != Token::RParen {
                return Err("Expected closing parenthesis".to_string());
            }
            *pos += 1;
            Ok(Expr::Grouped(Box::new(expr)))
        }
        _ => Err("Expected number, identifier or parenthesis".to_string()),
    }
}

fn parse_statement(tokens: &[Token], pos: &mut usize) -> Result<Statement, String> {
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
        Token::Identifier(name) => {
            let name = name.clone();
            *pos += 1;

            if *pos < tokens.len() && tokens[*pos] == Token::Colon {
                *pos += 1;
                expect_token(tokens, pos, Token::Keyword("int".to_string()))?;
                expect_token(tokens, pos, Token::Semicolon)?;
                Ok(Statement::VarDecl(name))
            } else {
                expect_token(tokens, pos, Token::Operator("=".to_string()))?;
                let expr = parse_expression(tokens, pos)?;
                expect_token(tokens, pos, Token::Semicolon)?;
                Ok(Statement::Assignment(name, expr))
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
        return Err(format!("Expected {:?} but found {:?}", expected, tokens[*pos]));
    }
    *pos += 1;
    Ok(())
}

fn parse_program(tokens: &[Token]) -> Vec<Statement> {
    let mut statements = Vec::new();
    let mut pos = 0;

    while pos < tokens.len() {
        match parse_statement(tokens, &mut pos) {
            Ok(stmt) => statements.push(stmt),
            Err(e) => panic!("Parse error: {}", e),
        }
    }

    statements
}

use std::env;
use std::fs;

fn main() {
    // let mut code = r#"
    //     val1: int;
    //     val2: int;
    //     val1 = 5;
    //     val2 = 3;
    //     consoleout(val1 + val2);
    //
    //     if (val1 > val2) {
    //         consoleout(val1);
    //     } else {
    //         consoleout(val2);
    //     }
    //
    //     val3 = (val1 + val2) * 2;
    //     consoleout(val3);
    // "#;

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let code_from_file = fs::read_to_string(filename);
    let code:&str = &code_from_file.unwrap();

    let tokens = tokenize(code);
    let program = parse_program(&tokens);
    let mut interpreter = Interpreter::new();
    interpreter.execute_block(&program);
}