use std::collections::HashMap;

use crate::parser::nodes;

pub struct VariableResolution {
    program: nodes::Program,
    unique_counter: u32,
}

#[derive(Clone)]
struct VarData {
    name: String,
    has_expr: bool,
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

        let mut global_var_map: HashMap<nodes::Identifier, VarData> = HashMap::new();

        for func in self.program.statements.clone() {
            global_var_map.insert(nodes::Identifier::Var(func.name.clone()), VarData { name: func.name.clone(), has_expr: func.body.len() > 0 });
            let mut body: Vec<nodes::BlockItem> = Vec::new();
            let mut variable_map = global_var_map.clone();
            let mut params: Vec<nodes::Identifier> = Vec::with_capacity(func.params.len());

            for param in func.params {
                let unique_name = self.generate_unique_name("arg", match param { nodes::Identifier::Var(ref s) => s.clone() });
                variable_map.insert(param, VarData { name: unique_name.clone(), has_expr: false });
                params.push(nodes::Identifier::Var(unique_name));
            }

            for instr in &func.body {
                body.push(self.resolve_block_item(instr, &mut variable_map));
            }

            let func = nodes::FuncDecl {
                name: func.name.clone(),
                params,
                body,
            };

            statements.push(func);
        }

        nodes::Program {
            statements,
        }
    }

    fn generate_unique_name(&mut self, function: &str, original_name: String) -> String {
        let name = format!("local{}.{}.{}", function, original_name, self.unique_counter);
        self.unique_counter += 1;
        name
    }

    fn resolve_block_item(&mut self, stmt: &nodes::BlockItem, variable_map: &mut HashMap<nodes::Identifier, VarData>) -> nodes::BlockItem {
        match stmt {
            nodes::BlockItem::Statement(ref inner_stmt) => nodes::BlockItem::Statement(self.resolve_statement(inner_stmt, variable_map)),
            nodes::BlockItem::Declaration(ref decl) => {
                match decl {
                    nodes::Declaration::VarDecl(decl) => {
                        if variable_map.contains_key(&decl.name) {
                            panic!("Variable {:?} already declared", decl.name);
                        }
        
                        let orig_name = match decl.name { nodes::Identifier::Var(ref s) => s.clone() };
        
                        let unique_name = self.generate_unique_name("var", orig_name);
        
                        variable_map.insert(decl.name.clone(), VarData { name: unique_name.clone(), has_expr: decl.expr.is_some() });
        
                        if decl.expr.is_some() {
                            let expr = decl.expr.as_ref().unwrap();
                            let val = self.resolve_expression(expr, variable_map);
                            nodes::BlockItem::Declaration(nodes::Declaration::VarDecl(nodes::VarDecl {
                                name: nodes::Identifier::Var(unique_name),
                                expr: Some(val),
                            }))
                        } else {
                            nodes::BlockItem::Statement(nodes::Statement::Empty)
                        }
                    }
                    nodes::Declaration::FuncDecl(_) => {
                        panic!("Function declarations not allowed in block items");
                    }
                }
                
            },
        }
    }

    fn resolve_statement(&mut self, stmt: &nodes::Statement, variable_map: &mut HashMap<nodes::Identifier, VarData>) -> nodes::Statement {
        match stmt {
            nodes::Statement::Return(ref expr) => {
                let val = self.resolve_expression(expr, variable_map);
                nodes::Statement::Return(val)
            },
            nodes::Statement::Expression(ref expr) => {
                let val = self.resolve_expression(expr, variable_map);
                nodes::Statement::Expression(val)
            },
            nodes::Statement::If(ref cond, ref then, ref else_) => {
                let cond = self.resolve_expression(cond, variable_map);
                let lft = Box::new(self.resolve_statement(&**then, variable_map));
                let rht = Box::new(match *else_.clone() {
                    Some(stmt) => Some(self.resolve_statement(&stmt, variable_map)),
                    None => None
                });

                nodes::Statement::If(cond, lft, rht)
            },
            nodes::Statement::Compound(ref stmts) => {
                let mut new_var_map = variable_map.clone();
                nodes::Statement::Compound(stmts.iter().map(|stmt| self.resolve_block_item(stmt, &mut new_var_map)).collect())
            }
            nodes::Statement::Break(_) => stmt.clone(),
            nodes::Statement::Continue(_) => stmt.clone(),
            nodes::Statement::While(ref cond, ref body, _) => {
                let cond = self.resolve_expression(cond, variable_map);
                let body = Box::new(self.resolve_statement(&**body, variable_map));

                nodes::Statement::While(cond, body, String::new())
            },
            nodes::Statement::DoWhile(ref body, ref cond, _) => {
                let body = Box::new(self.resolve_statement(&**body, variable_map));
                let cond = self.resolve_expression(cond, variable_map);

                nodes::Statement::DoWhile(body, cond, String::new())
            },
            nodes::Statement::For(ref init, ref cond, ref post, ref body, _) => {
                let init = match init {
                    nodes::ForInit::Declaration(ref decl) => {
                        let decl = match decl { nodes::Declaration::VarDecl(ref decl) => decl, _ => panic!("Invalid declaration") };

                        let decl = nodes::VarDecl {
                            name: decl.name.clone(),
                            expr: match decl.expr {
                                Some(ref expr) => Some(self.resolve_expression(expr, variable_map)),
                                None => None,
                            },
                        };
                        let var = match decl.name { nodes::Identifier::Var(ref s) => s.clone() };
                        variable_map.insert(decl.name.clone(), VarData { name: var, has_expr: decl.expr.is_some() });
                        nodes::ForInit::Declaration(nodes::Declaration::VarDecl(decl))
                    },
                    nodes::ForInit::Expression(ref expr) => {
                        let expr = self.resolve_expression(expr, variable_map);
                        nodes::ForInit::Expression(expr)
                    },
                    nodes::ForInit::Empty => nodes::ForInit::Empty,
                };

                let cond = match cond {
                    Some(ref expr) => Some(self.resolve_expression(expr, variable_map)),
                    None => None,
                };

                let post = match post {
                    Some(ref expr) => Some(self.resolve_expression(expr, variable_map)),
                    None => None,
                };

                let body = Box::new(self.resolve_statement(&**body, variable_map));

                nodes::Statement::For(init, cond, post, body, String::new())
            },
            nodes::Statement::Empty => stmt.clone(),
        }
    }

    fn is_valid_lvalue(&self, expr: &nodes::Expression) -> bool {
        match expr {
            nodes::Expression::Var(_) => true,
            _ => false,
        }
    }

    fn resolve_expression(&mut self, expr: &nodes::Expression, variable_map: &HashMap<nodes::Identifier, VarData>) -> nodes::Expression {
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
                    let ident = match ident { nodes::Identifier::Var(ref s) => s.clone() };
                    nodes::Expression::Var(nodes::Identifier::Var(variable_map.get(&nodes::Identifier::Var(ident)).unwrap().name.clone()))
                } else {
                    panic!("Variable {:?} not found", ident);
                }
            },
            nodes::Expression::Assign(ref lhs, ref rhs) => {
                if !self.is_valid_lvalue(&**lhs) {
                    panic!("Invalid lvalue");
                }

                let lhs = self.resolve_expression(lhs, variable_map);
                let rhs = self.resolve_expression(rhs, variable_map);

                nodes::Expression::Assign(Box::new(lhs), Box::new(rhs))
            },
            nodes::Expression::Conditional(ref cond, ref lft, ref rht) => {
                let cond = Box::new(self.resolve_expression(&**cond, variable_map));
                let lft = Box::new(self.resolve_expression(&**lft, variable_map));
                let rht = Box::new(self.resolve_expression(&**rht, variable_map));

                nodes::Expression::Conditional(cond, lft, rht)
            },
            nodes::Expression::Increment(ref expr) => {
                if !self.is_valid_lvalue(&**expr) {
                    panic!("Invalid lvalue");
                }

                let expr = self.resolve_expression(expr, variable_map);

                nodes::Expression::Increment(Box::new(expr))
            },
            nodes::Expression::Decrement(ref expr) => {
                if !self.is_valid_lvalue(&**expr) {
                    panic!("Invalid lvalue");
                }

                let expr = self.resolve_expression(expr, variable_map);

                nodes::Expression::Decrement(Box::new(expr))
            },
            nodes::Expression::FunctionCall(name, args) => {
                let ident = nodes::Identifier::Var(name.clone());
                if variable_map.contains_key(&ident) {
                    let name = variable_map.get(&ident).unwrap();
                    let args = args.iter().map(|arg| self.resolve_expression(arg, variable_map)).collect();
                    nodes::Expression::FunctionCall(name.name.clone(), args)
                } else {
                    panic!("Function {:?} not found", name);
                }
            }
        }
    }
}