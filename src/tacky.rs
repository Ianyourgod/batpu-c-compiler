pub mod definition;

use crate::parser::nodes;

pub struct Tacky {
    program: nodes::Program,
    temp_count: i32,
}

impl Tacky {
    pub fn new(program: nodes::Program) -> Tacky {
        Tacky {
            program,
            temp_count: 0,
        }
    }

    pub fn emit(&mut self) -> definition::Program {
        let mut statements: Vec<definition::FuncDecl> = Vec::new();

        for func in self.program.statements.clone() {
            let mut body: Vec<definition::Instruction> = Vec::new();
            for instr in &func.body {
                self.emit_statement(instr, &mut body);
            }

            let func = definition::FuncDecl {
                name: func.name.clone(),
                body,
            };

            statements.push(func);
        }

        definition::Program {
            statements,
        }
    }

    fn emit_statement(&mut self, stmt: &nodes::Statement, body: &mut Vec<definition::Instruction>) {
        match stmt {
            nodes::Statement::Return(ref expr) => {
                let val = self.emit_expression(expr, body);
                body.push(definition::Instruction::Return(val));
            }
        }
    }

    fn make_temporary(&mut self) -> String {
        let name = format!("tmp.{}", self.temp_count);
        self.temp_count += 1;
        name
    }

    fn convert_binop(&self, op: &nodes::Binop) -> definition::Binop {
        match op {
            nodes::Binop::Add => definition::Binop::Add,
            nodes::Binop::Subtract => definition::Binop::Subtract,
        }
    }

    fn emit_expression(&mut self, expr: &nodes::Expression, body: &mut Vec<definition::Instruction>) -> definition::Val {
        match expr {
            nodes::Expression::IntegerLiteral(i) => {
                definition::Val::Const(*i)
            }
            nodes::Expression::Unop(op, ref expr) => {
                let src = self.emit_expression(expr, body);
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone());
                let tacky_op = match op {
                    nodes::Unop::Negate => definition::Unop::Negate,
                    nodes::Unop::BitwiseNot => definition::Unop::BitwiseNot,
                };
                body.push(definition::Instruction::Unary(tacky_op, src, dest.clone()));
                dest
            }
            nodes::Expression::Binop(op, ref lft, ref rht) => {
                let lhs = self.emit_expression(lft, body);
                let rhs = self.emit_expression(rht, body);
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone());
                let tacky_op = self.convert_binop(op);
                body.push(definition::Instruction::Binary(tacky_op, lhs, rhs, dest.clone()));
                dest
            }
        }
    }
}