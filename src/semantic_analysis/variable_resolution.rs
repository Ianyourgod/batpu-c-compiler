use core::panic;
use std::collections::HashMap;

use crate::parser::nodes::{self, Identifier};

pub struct VariableResolution {
    program: nodes::Program,
    unique_counter: u32,
}

impl VariableResolution {
    pub fn new(program: nodes::Program) -> VariableResolution {
        VariableResolution {
            program,
            unique_counter: 0,
        }
    }

    pub fn resolve(&mut self) -> nodes::Program {
        let mut statements: Vec<nodes::FuncDecl> = Vec::new();

        for func in self.program.statements.clone() {
            let mut body: Vec<nodes::BlockItem> = Vec::new();
            let mut variable_map: HashMap<nodes::Identifier, String> = HashMap::new();
            for instr in &func.body {
                self.resolve_block_item(instr, &mut body, &mut variable_map);
            }

            let func = nodes::FuncDecl {
                name: func.name.clone(),
                body,
            };

            statements.push(func);
        }

        nodes::Program {
            statements,
        }
    }

    fn generate_unique_name(&mut self, original_name: String) -> String {
        let name = format!("localvar.{}.{}", original_name, self.unique_counter);
        self.unique_counter += 1;
        name
    }

    fn resolve_block_item(&mut self, stmt: &nodes::BlockItem, body: &mut Vec<nodes::BlockItem>, variable_map: &mut HashMap<nodes::Identifier, String>) {
        match stmt {
            nodes::BlockItem::Statement(ref inner_stmt) => {
                match inner_stmt {
                    nodes::Statement::Return(ref expr) => {
                        let val = self.resolve_expression(expr, variable_map);
                        body.push(nodes::BlockItem::Statement(nodes::Statement::Return(val)));
                    },
                    nodes::Statement::Expression(ref expr) => {
                        let val = self.resolve_expression(expr, variable_map);
                        body.push(nodes::BlockItem::Statement(nodes::Statement::Expression(val)));
                    },
                    nodes::Statement::Empty => body.push(stmt.clone()),
                }
            },
            nodes::BlockItem::Declaration(ref decl) => {
                if variable_map.contains_key(&decl.name) {
                    panic!("Variable {:?} already declared", decl.name);
                }

                let orig_name = match decl.name { nodes::Identifier::Var(ref s) => s.clone() };

                let unique_name = self.generate_unique_name(orig_name);

                variable_map.insert(decl.name.clone(), unique_name.clone());

                if decl.expr.is_some() {
                    let expr = decl.expr.as_ref().unwrap();
                    let val = self.resolve_expression(expr, variable_map);
                    body.push(nodes::BlockItem::Declaration(nodes::Declaration {
                        name: nodes::Identifier::Var(unique_name),
                        expr: Some(val),
                    }));
                }
            },
        }
    }

    fn resolve_expression(&mut self, expr: &nodes::Expression, variable_map: &HashMap<Identifier, String>) -> nodes::Expression {
        match expr {
            nodes::Expression::Binop(ref op, ref src1, ref src2) => {
                let src1 = self.resolve_expression(src1, variable_map);
                let src2 = self.resolve_expression(src2, variable_map);

                nodes::Expression::Binop(op.clone(), Box::new(src1), Box::new(src2))
            },
            nodes::Expression::Unop(ref op, ref src) => {
                let src = self.resolve_expression(src, variable_map);

                nodes::Expression::Unop(op.clone(), Box::new(src))
            },
            nodes::Expression::IntegerLiteral(_) => expr.clone(),
            nodes::Expression::Var(ref ident) => {
                if variable_map.contains_key(ident) {
                    nodes::Expression::Var(nodes::Identifier::Var(variable_map.get(ident).unwrap().clone()))
                } else {
                    panic!("Variable {:?} not found", ident);
                }
            },
            nodes::Expression::Assign(ref lhs, ref rhs) => {
                match **lhs {
                    nodes::Expression::Var(_) => {}
                    _ => panic!("Invalid lvalue"),
                }

                let lhs = self.resolve_expression(lhs, variable_map);
                let rhs = self.resolve_expression(rhs, variable_map);

                nodes::Expression::Assign(Box::new(lhs), Box::new(rhs))
            },
        }
    }
}