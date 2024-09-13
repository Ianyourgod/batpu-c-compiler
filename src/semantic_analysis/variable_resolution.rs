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
    has_external_linkage: bool,
    from_current_scope: bool,
}

struct VariableMap {
    pub map: HashMap<String, VarData>
}

impl VariableMap {
    pub fn new() -> VariableMap {
        VariableMap {
            map: HashMap::new()
        }
    }

    pub fn clone(&mut self) -> VariableMap {
        let mut new_map: HashMap<String, VarData> = HashMap::new();
        
        for (key, value) in self.map.iter() {
            new_map.insert(key.clone(), VarData {
                name: value.name.clone(),
                has_expr: value.has_expr,
                has_external_linkage: value.has_external_linkage,
                from_current_scope: false,
            });
        }

        VariableMap { map: new_map }
    }

    pub fn insert(&mut self, k: String, v: VarData) {
        self.map.insert(k, v);
    }

    pub fn contains_key(&self, k: &String) -> bool {
        self.map.contains_key(k)
    }

    pub fn get(&self, k: &String) -> Option<&VarData> {
        self.map.get(k)
    }
}

impl VariableResolution {
    pub fn new(program: nodes::Program) -> VariableResolution {
        VariableResolution {
            program,
            unique_counter: 0,
        }
    }

    pub fn resolve(&mut self) -> nodes::Program {
        let mut statements: Vec<nodes::Declaration> = Vec::new();

        let mut global_var_map = VariableMap::new();

        for decl in self.program.statements.clone() {
            match decl {
                nodes::Declaration::FuncDecl(func) => {
                    let func = self.resolve_function_declaration(&func, &mut global_var_map);

                    statements.push(nodes::Declaration::FuncDecl(func));
                },
                nodes::Declaration::VarDecl(var_decl) => {
                    let decl = self.resolve_file_scope_declaration(&var_decl, &mut global_var_map);

                    statements.push(decl);
                }
            }
        }

        nodes::Program {
            statements,
        }
    }

    fn resolve_function_declaration(&mut self, decl: &nodes::FuncDecl, variable_map: &mut VariableMap) -> nodes::FuncDecl {
        let decl_name = &decl.name;
        if variable_map.contains_key(&decl_name) {
            let old_decl = variable_map.get(&decl_name).unwrap();
            if old_decl.from_current_scope && !old_decl.has_external_linkage {
                panic!("Function {:?} already declared", decl_name);
            }
        }

        variable_map.insert(decl_name.clone(), VarData {
            name: decl.name.clone(),
            has_expr: decl.body.len() > 0,
            has_external_linkage: true,
            from_current_scope: true,
        });

        let mut inner_map = variable_map.clone();
        let mut new_params: Vec<String> = Vec::with_capacity(decl.params.len());

        for param in &decl.params {
            if variable_map.contains_key(param) {
                panic!("Variable {:?} already declared", param);
            }
            
            let unique_name = self.generate_unique_name("arg", param);
            inner_map.insert(param.clone(), VarData {
                name: unique_name.clone(),
                has_expr: false,
                has_external_linkage: false,
                from_current_scope: true,
            });
            new_params.push(unique_name);
        }

        let mut new_body: Vec<nodes::BlockItem> = Vec::new();
        if decl.body.len() > 0 {
            for stmt in decl.body.clone() {
                new_body.push(self.resolve_block_item(&stmt, &mut inner_map));
            }
        }

        nodes::FuncDecl {
            name: decl.name.clone(),
            params: new_params,
            body: new_body,
            storage_class: decl.storage_class.clone(),
            ty: decl.ty.clone(),
        }
    }

    fn generate_unique_name(&mut self, function: &str, original_name: &String) -> String {
        let name = format!("local{}.{}.{}", function, original_name, self.unique_counter);
        self.unique_counter += 1;
        name
    }

    fn resolve_file_scope_declaration(&mut self, decl: &nodes::VarDecl, variable_map: &mut VariableMap) -> nodes::Declaration {
        variable_map.insert(decl.name.clone(), VarData {
            name: decl.name.clone(),
            has_expr: decl.expr.is_some(),
            from_current_scope: true,
            has_external_linkage: true,
        });

        nodes::Declaration::VarDecl(decl.clone())
    }

    fn resolve_block_item(&mut self, stmt: &nodes::BlockItem, variable_map: &mut VariableMap) -> nodes::BlockItem {
        match stmt {
            nodes::BlockItem::Statement(ref inner_stmt) => nodes::BlockItem::Statement(self.resolve_statement(inner_stmt, variable_map)),
            nodes::BlockItem::Declaration(ref decl) => {
                match decl {
                    nodes::Declaration::VarDecl(decl) => {
                        if variable_map.contains_key(&decl.name) {
                            let old_decl = variable_map.get(&decl.name).unwrap();
                            if old_decl.from_current_scope && !(old_decl.has_external_linkage && decl.storage_class == nodes::StorageClass::Extern) {
                                panic!("Conflicting local declarations of variable {:?}", decl.name);
                            }
                        }

                        if decl.storage_class == nodes::StorageClass::Extern {
                            variable_map.insert(decl.name.clone(), VarData {
                                name: decl.name.clone(),
                                has_expr: decl.expr.is_some(),
                                from_current_scope: true,
                                has_external_linkage: true,
                            });
                            return nodes::BlockItem::Declaration(nodes::Declaration::VarDecl(decl.clone()));
                        }
        
                        let orig_name = decl.name.clone();
        
                        let unique_name = self.generate_unique_name("var", &orig_name);

                        variable_map.insert(decl.name.clone(), VarData {
                            name: unique_name.clone(),
                            has_expr: decl.expr.is_some(),
                            from_current_scope: true,
                            has_external_linkage: false,
                        });
        
                        if decl.expr.is_some() {
                            let expr = decl.expr.as_ref().unwrap();
                            let val = self.resolve_init(expr, variable_map);
                            nodes::BlockItem::Declaration(nodes::Declaration::VarDecl(nodes::VarDecl {
                                name: unique_name,
                                expr: Some(val),
                                storage_class: decl.storage_class,
                                ty: decl.ty.clone(),
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

    fn resolve_init(&mut self, init: &nodes::Initializer, variable_map: &mut VariableMap) -> nodes::Initializer {
        match init {
            nodes::Initializer::Single(ref expr) => {
                let val = self.resolve_expression(expr, variable_map);
                nodes::Initializer::Single(val)
            },
            nodes::Initializer::Compound(ref inits) => {
                let mut new_inits: Vec<nodes::Initializer> = Vec::new();
                for init in inits {
                    new_inits.push(self.resolve_init(init, variable_map));
                }
                nodes::Initializer::Compound(new_inits)
            },
        }
    }

    fn resolve_statement(&mut self, stmt: &nodes::Statement, variable_map: &mut VariableMap) -> nodes::Statement {
        match stmt {
            nodes::Statement::Return(ref expr) => {
                match expr {
                    Some(expr) => {
                        let val = self.resolve_expression(expr, variable_map);
                        nodes::Statement::Return(Some(val))
                    }
                    None => nodes::Statement::Return(None)
                }
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

                        if variable_map.contains_key(&decl.name) {
                            let old_decl = variable_map.get(&decl.name).unwrap();
                            if old_decl.from_current_scope && !(old_decl.has_external_linkage && decl.storage_class == nodes::StorageClass::Extern) {
                                panic!("Conflicting local declarations of variable {:?}", decl.name);
                            }
                        }

                        if decl.storage_class == nodes::StorageClass::Extern {
                            panic!("Cannot declare extern variables in for loop");
                        }
        
                        let orig_name = decl.name.clone();
        
                        let unique_name = self.generate_unique_name("loopvar", &orig_name);

                        variable_map.insert(decl.name.clone(), VarData {
                            name: unique_name.clone(),
                            has_expr: decl.expr.is_some(),
                            from_current_scope: true,
                            has_external_linkage: false,
                        });
        
                        if decl.expr.is_some() {
                            let expr = decl.expr.as_ref().unwrap();
                            let val = self.resolve_init(expr, variable_map);
                            nodes::ForInit::Declaration(nodes::Declaration::VarDecl(nodes::VarDecl {
                                name: unique_name,
                                expr: Some(val),
                                storage_class: decl.storage_class,
                                ty: decl.ty.clone(),
                            }))
                        } else {
                            nodes::ForInit::Empty
                        }
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
        match expr.expr {
            nodes::ExpressionEnum::Var(_) => true,
            nodes::ExpressionEnum::Dereference(_) => true,
            _ => false,
        }
    }

    fn resolve_expression(&mut self, expr: &nodes::Expression, variable_map: &VariableMap) -> nodes::Expression {
        nodes::Expression::new(match expr.expr.clone() {
            nodes::ExpressionEnum::Binop(ref op, ref src1, ref src2) => {
                let src1 = self.resolve_expression(src1, variable_map);
                let src2 = self.resolve_expression(src2, variable_map);

                nodes::ExpressionEnum::Binop(op.clone(), Box::new(src1), Box::new(src2))
            },
            nodes::ExpressionEnum::Unop(ref op, ref src) => {
                let src = self.resolve_expression(src, variable_map);

                nodes::ExpressionEnum::Unop(op.clone(), Box::new(src))
            },
            nodes::ExpressionEnum::IntegerLiteral(_) | nodes::ExpressionEnum::CharLiteral(_) => expr.expr.clone(),
            nodes::ExpressionEnum::Var(ref ident) => {
                if variable_map.contains_key(ident) {
                    nodes::ExpressionEnum::Var(variable_map.get(ident).unwrap().name.clone())
                } else {
                    panic!("Variable {:?} not found", ident);
                }
            },
            nodes::ExpressionEnum::Assign(ref lhs, ref rhs) => {
                if !self.is_valid_lvalue(&**lhs) {
                    panic!("Invalid lvalue");
                }

                let lhs = self.resolve_expression(lhs, variable_map);
                let rhs = self.resolve_expression(rhs, variable_map);

                nodes::ExpressionEnum::Assign(Box::new(lhs), Box::new(rhs))
            },
            nodes::ExpressionEnum::Conditional(ref cond, ref lft, ref rht) => {
                let cond = Box::new(self.resolve_expression(&**cond, variable_map));
                let lft = Box::new(self.resolve_expression(&**lft, variable_map));
                let rht = Box::new(self.resolve_expression(&**rht, variable_map));

                nodes::ExpressionEnum::Conditional(cond, lft, rht)
            },
            nodes::ExpressionEnum::Increment(ref expr) => {
                if !self.is_valid_lvalue(&**expr) {
                    panic!("Invalid lvalue");
                }

                let expr = self.resolve_expression(expr, variable_map);

                nodes::ExpressionEnum::Increment(Box::new(expr))
            },
            nodes::ExpressionEnum::Decrement(ref expr) => {
                if !self.is_valid_lvalue(&**expr) {
                    panic!("Invalid lvalue");
                }

                let expr = self.resolve_expression(expr, variable_map);

                nodes::ExpressionEnum::Decrement(Box::new(expr))
            },
            nodes::ExpressionEnum::FunctionCall(name, args) => {
                let ident = &name;
                if variable_map.contains_key(&ident) {
                    let name = variable_map.get(&ident).unwrap();
                    let args = args.iter().map(|arg| self.resolve_expression(arg, variable_map)).collect();
                    nodes::ExpressionEnum::FunctionCall(name.name.clone(), args)
                } else {
                    panic!("Function {:?} not found", name);
                }
            },
            nodes::ExpressionEnum::Dereference(ref expr) => {
                let expr = self.resolve_expression(expr, variable_map);

                nodes::ExpressionEnum::Dereference(Box::new(expr))
            },
            nodes::ExpressionEnum::AddressOf(ref expr) => {
                let expr = self.resolve_expression(expr, variable_map);

                nodes::ExpressionEnum::AddressOf(Box::new(expr))
            },
            nodes::ExpressionEnum::Subscript(ref expr, ref index) => {
                let expr = self.resolve_expression(expr, variable_map);
                let index = self.resolve_expression(index, variable_map);

                nodes::ExpressionEnum::Subscript(Box::new(expr), Box::new(index))
            },
            nodes::ExpressionEnum::Cast(ty, ref expr) => {
                let expr = self.resolve_expression(expr, variable_map);

                nodes::ExpressionEnum::Cast(ty, Box::new(expr))
            },
            nodes::ExpressionEnum::SizeOf(ref expr) => {
                let expr = self.resolve_expression(expr, variable_map);

                nodes::ExpressionEnum::SizeOf(Box::new(expr))
            },
            nodes::ExpressionEnum::SizeOfType(_) => expr.expr.clone(),
        })
    }
}