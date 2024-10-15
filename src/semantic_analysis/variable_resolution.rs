use std::collections::HashMap;

use crate::{errors, parser::nodes};

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

fn clone_struct_map(map: &HashMap<String, (String, bool)>) -> HashMap<String, (String, bool)> {
    let mut new_map: HashMap<String, (String, bool)> = HashMap::new();

    for (key, value) in map.iter() {
        new_map.insert(key.clone(), (value.0.clone(), false));
    }

    new_map
}

struct Context {
    pub variable_map: VariableMap,
    pub struct_map: HashMap<String, (String, bool)>,
}

impl Context {
    pub fn clone(&mut self) -> Context {
        Context {
            variable_map: self.variable_map.clone(),
            struct_map: clone_struct_map(&self.struct_map),
        }
    }
}

impl VariableResolution {
    pub fn new(program: nodes::Program) -> VariableResolution {
        VariableResolution {
            program,
            unique_counter: 0,
        }
    }

    pub fn resolve(&mut self) -> Result<nodes::Program, errors::Error> {
        let mut statements: Vec<nodes::Declaration> = Vec::new();

        let mut global_context = Context {
            variable_map: VariableMap::new(),
            struct_map: HashMap::new(),
        };

        for decl in self.program.statements.clone() {
            match decl {
                nodes::Declaration::FuncDecl(func, line) => {
                    let func = self.resolve_function_declaration(&func, &mut global_context)?;

                    statements.push(nodes::Declaration::FuncDecl(func, line));
                },
                nodes::Declaration::VarDecl(var_decl, _) => {
                    let decl = self.resolve_file_scope_declaration(&var_decl, &mut global_context);

                    statements.push(decl);
                }
                nodes::Declaration::StructDecl(struct_decl, line) => {
                    let decl = self.resolve_struct_declaration(&struct_decl, &mut global_context)?;

                    statements.push(nodes::Declaration::StructDecl(decl, line));
                }
                nodes::Declaration::Empty(line) => statements.push(nodes::Declaration::Empty(line)),
            }
        }

        Ok(nodes::Program {
            statements,
        })
    }

    fn resolve_struct_declaration(&mut self, decl: &nodes::StructDecl, context: &mut Context) -> Result<nodes::StructDecl, errors::Error> {
        let prev_entry = context.struct_map.get(&decl.tag);

        let unique_tag = if prev_entry.is_none() || !prev_entry.unwrap().1 {
            let unique_tag = self.generate_unique_name("struct", &decl.tag);

            context.struct_map.insert(decl.tag.clone(), (unique_tag.clone(), true));

            unique_tag
        } else {
            prev_entry.unwrap().0.clone()
        };

        let mut new_members: Vec<nodes::MemberDecl> = Vec::new();

        for member in &decl.members {
            new_members.push(nodes::MemberDecl {
                name: member.name.clone(),
                ty: self.resolve_type(&member.ty, context)?,
                line: member.line,
            });
        }

        Ok(nodes::StructDecl {
            tag: unique_tag,
            members: new_members,
            line: decl.line,
        })
    }

    fn resolve_function_declaration(&mut self, decl: &nodes::FuncDecl, context: &mut Context) -> Result<nodes::FuncDecl, errors::Error> {
        let decl_name = &decl.name;
        if context.variable_map.contains_key(&decl_name) {
            let old_decl = context.variable_map.get(&decl_name).unwrap();
            if old_decl.from_current_scope && !old_decl.has_external_linkage {
                return Err(errors::Error::new(errors::ErrorType::Error, format!("Function {:?} already declared", decl_name), decl.line));
            }
        }

        context.variable_map.insert(decl_name.clone(), VarData {
            name: decl.name.clone(),
            has_expr: decl.body.len() > 0,
            has_external_linkage: true,
            from_current_scope: true,
        });

        let mut inner_map = context.clone();
        let mut new_params: Vec<String> = Vec::with_capacity(decl.params.len());

        for param in &decl.params {
            if context.variable_map.contains_key(param) {
                return Err(errors::Error::new(errors::ErrorType::Error, format!("Variable {:?} already declared", param), decl.line));
            }
            
            let unique_name = self.generate_unique_name("arg", param);
            inner_map.variable_map.insert(param.clone(), VarData {
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
                new_body.push(self.resolve_block_item(&stmt, &mut inner_map)?);
            }
        }

        let ty = self.resolve_type(&decl.ty, context)?;

        Ok(nodes::FuncDecl {
            name: decl.name.clone(),
            params: new_params,
            body: new_body,
            storage_class: decl.storage_class.clone(),
            ty,
            line: decl.line,
        })
    }

    fn generate_unique_name(&mut self, function: &str, original_name: &String) -> String {
        let name = format!("local{}.{}.{}", function, original_name, self.unique_counter);
        self.unique_counter += 1;
        name
    }

    fn resolve_file_scope_declaration(&mut self, decl: &nodes::VarDecl, context: &mut Context) -> nodes::Declaration {
        context.variable_map.insert(decl.name.clone(), VarData {
            name: decl.name.clone(),
            has_expr: decl.expr.is_some(),
            from_current_scope: true,
            has_external_linkage: true,
        });

        nodes::Declaration::VarDecl(decl.clone(), decl.line)
    }

    fn resolve_block_item(&mut self, stmt: &nodes::BlockItem, context: &mut Context) -> Result<nodes::BlockItem, errors::Error> {
        Ok(match stmt {
            nodes::BlockItem::Statement(ref inner_stmt, line) => nodes::BlockItem::Statement(self.resolve_statement(inner_stmt, context)?, *line),
            nodes::BlockItem::Declaration(ref decl, _) => {
                match decl {
                    nodes::Declaration::VarDecl(decl, line) => {
                        if context.variable_map.contains_key(&decl.name) {
                            let old_decl = context.variable_map.get(&decl.name).unwrap();
                            if old_decl.from_current_scope && !(old_decl.has_external_linkage && decl.storage_class == nodes::StorageClass::Extern) {
                                return Err(errors::Error::new(errors::ErrorType::Error, format!("Conflicting local declarations of variable {:?}", decl.name), *line));
                            }
                        }

                        if decl.storage_class == nodes::StorageClass::Extern {
                            context.variable_map.insert(decl.name.clone(), VarData {
                                name: decl.name.clone(),
                                has_expr: decl.expr.is_some(),
                                from_current_scope: true,
                                has_external_linkage: true,
                            });
                            return Ok(nodes::BlockItem::Declaration(nodes::Declaration::VarDecl(decl.clone(), *line), *line));
                        }
        
                        let orig_name = decl.name.clone();
        
                        let unique_name = self.generate_unique_name("var", &orig_name);

                        context.variable_map.insert(decl.name.clone(), VarData {
                            name: unique_name.clone(),
                            has_expr: decl.expr.is_some(),
                            from_current_scope: true,
                            has_external_linkage: false,
                        });
        
                        if decl.expr.is_some() {
                            let expr = decl.expr.as_ref().unwrap();
                            let val = self.resolve_init(expr, context)?;
                            let ty = self.resolve_type(&decl.ty, context)?;
                            nodes::BlockItem::Declaration(nodes::Declaration::VarDecl(nodes::VarDecl {
                                name: unique_name,
                                expr: Some(val),
                                storage_class: decl.storage_class,
                                ty,
                                line: *line
                            }, *line), *line)
                        } else {
                            let ty = self.resolve_type(&decl.ty, context)?;
                            nodes::BlockItem::Declaration(nodes::Declaration::VarDecl(nodes::VarDecl {
                                name: unique_name,
                                expr: None,
                                storage_class: decl.storage_class,
                                ty,
                                line: *line,
                            }, *line), *line)
                        }
                    }
                    nodes::Declaration::StructDecl(decl, line) => {
                        nodes::BlockItem::Declaration(nodes::Declaration::StructDecl(self.resolve_struct_declaration(decl, context)?, *line), *line)
                    }
                    nodes::Declaration::FuncDecl(_, _) => {
                        unimplemented!("Function declarations not allowed in block items");
                    }
                    nodes::Declaration::Empty(line) => nodes::BlockItem::Declaration(nodes::Declaration::Empty(*line), *line),
                }
                
            },
        })
    }

    fn resolve_init(&mut self, init: &nodes::Initializer, context: &mut Context) -> Result<nodes::Initializer, errors::Error> {
        Ok(match init {
            nodes::Initializer::Single(ref expr, line) => {
                let val = self.resolve_expression(expr, context)?;
                nodes::Initializer::Single(val, *line)
            },
            nodes::Initializer::Compound(ref inits, line) => {
                let mut new_inits: Vec<nodes::Initializer> = Vec::new();
                for init in inits {
                    new_inits.push(self.resolve_init(init, context)?);
                }
                nodes::Initializer::Compound(new_inits, *line)
            },
        })
    }

    fn resolve_statement(&mut self, stmt: &nodes::Statement, context: &mut Context) -> Result<nodes::Statement, errors::Error> {
        Ok(match stmt {
            nodes::Statement::Return(ref expr, line) => {
                match expr {
                    Some(expr) => {
                        let val = self.resolve_expression(expr, context)?;
                        nodes::Statement::Return(Some(val), *line)
                    }
                    None => nodes::Statement::Return(None, *line)
                }
            },
            nodes::Statement::Expression(ref expr, line) => {
                let val = self.resolve_expression(expr, context)?;
                nodes::Statement::Expression(val, *line)
            },
            nodes::Statement::If(ref cond, ref then, ref else_, line) => {
                let cond = self.resolve_expression(cond, context)?;
                let lft = Box::new(self.resolve_statement(&**then, context)?);
                let rht = Box::new(match *else_.clone() {
                    Some(stmt) => Some(self.resolve_statement(&stmt, context)?),
                    None => None
                });

                nodes::Statement::If(cond, lft, rht, *line)
            },
            nodes::Statement::Compound(ref stmts, line) => {
                let mut new_var_map = context.clone();
                let mut new_stmts: Vec<nodes::BlockItem> = Vec::new();
                for stmt in stmts {
                    new_stmts.push(self.resolve_block_item(stmt, &mut new_var_map)?);
                }
                nodes::Statement::Compound(new_stmts, *line)
            }
            nodes::Statement::Break(_, _) |
            nodes::Statement::Continue(_, _) => stmt.clone(),
            nodes::Statement::While(ref cond, ref body, _, line) => {
                let cond = self.resolve_expression(cond, context)?;
                let body = Box::new(self.resolve_statement(&**body, context)?);

                nodes::Statement::While(cond, body, String::new(), *line)
            },
            nodes::Statement::DoWhile(ref body, ref cond, _, line) => {
                let body = Box::new(self.resolve_statement(&**body, context)?);
                let cond = self.resolve_expression(cond, context)?;

                nodes::Statement::DoWhile(body, cond, String::new(), *line)
            },
            nodes::Statement::For(ref init, ref cond, ref post, ref body, _, line) => {
                let init = match init {
                    nodes::ForInit::Declaration(ref decl, line) => {
                        let decl = match decl { nodes::Declaration::VarDecl(ref decl, _) => decl, _ => return Err(errors::Error::new(errors::ErrorType::Error, String::from("Expected variable declaration in for"), *line)) };

                        if context.variable_map.contains_key(&decl.name) {
                            let old_decl = context.variable_map.get(&decl.name).unwrap();
                            if old_decl.from_current_scope && !(old_decl.has_external_linkage && decl.storage_class == nodes::StorageClass::Extern) {
                                return Err(errors::Error::new(errors::ErrorType::Error, format!("Conflicting local declarations of variable {:?}", decl.name), *line));
                            }
                        }

                        if decl.storage_class == nodes::StorageClass::Extern {
                            return Err(errors::Error::new(errors::ErrorType::Error, String::from("Extern storage class not allowed in for"), *line));
                        }
        
                        let orig_name = decl.name.clone();
        
                        let unique_name = self.generate_unique_name("loopvar", &orig_name);

                        context.variable_map.insert(decl.name.clone(), VarData {
                            name: unique_name.clone(),
                            has_expr: decl.expr.is_some(),
                            from_current_scope: true,
                            has_external_linkage: false,
                        });
        
                        if decl.expr.is_some() {
                            let expr = decl.expr.as_ref().unwrap();
                            let val = self.resolve_init(expr, context)?;
                            let ty = self.resolve_type(&decl.ty, context)?;
                            nodes::ForInit::Declaration(nodes::Declaration::VarDecl(nodes::VarDecl {
                                name: unique_name,
                                expr: Some(val),
                                storage_class: decl.storage_class,
                                ty,
                                line: *line
                            }, *line), *line)
                        } else {
                            nodes::ForInit::Empty(*line)
                        }
                    },
                    nodes::ForInit::Expression(ref expr, line) => {
                        let expr = self.resolve_expression(expr, context)?;
                        nodes::ForInit::Expression(expr, *line)
                    },
                    nodes::ForInit::Empty(line) => nodes::ForInit::Empty(*line),
                };

                let cond = match cond {
                    Some(ref expr) => Some(self.resolve_expression(expr, context)?),
                    None => None,
                };

                let post = match post {
                    Some(ref expr) => Some(self.resolve_expression(expr, context)?),
                    None => None,
                };

                let body = Box::new(self.resolve_statement(&**body, context)?);

                nodes::Statement::For(init, cond, post, body, String::new(), *line)
            },
            nodes::Statement::Empty(_) => stmt.clone(),
        })
    }

    fn is_valid_lvalue(&self, expr: &nodes::Expression) -> bool {
        match expr.expr {
            nodes::ExpressionEnum::Var(_, _) => true,
            nodes::ExpressionEnum::Dereference(_, _) => true,
            nodes::ExpressionEnum::Subscript(_, _, _) => true,
            nodes::ExpressionEnum::Dot(_, _, _) |
            nodes::ExpressionEnum::Arrow(_, _, _) => true,
            _ => false,
        }
    }

    fn resolve_expression(&mut self, expr: &nodes::Expression, context: &Context) -> Result<nodes::Expression, errors::Error> {
        Ok(nodes::Expression::new(match expr.expr.clone() {
            nodes::ExpressionEnum::Binop(ref op, ref src1, ref src2, line) => {
                let src1 = self.resolve_expression(src1, context)?;
                let src2 = self.resolve_expression(src2, context)?;

                nodes::ExpressionEnum::Binop(op.clone(), Box::new(src1), Box::new(src2), line)
            },
            nodes::ExpressionEnum::Unop(ref op, ref src, line) => {
                let src = self.resolve_expression(src, context)?;

                nodes::ExpressionEnum::Unop(op.clone(), Box::new(src), line)
            },
            nodes::ExpressionEnum::IntegerLiteral(_, _) | nodes::ExpressionEnum::CharLiteral(_, _) |
            nodes::ExpressionEnum::StringLiteral(_, _) => expr.expr.clone(),
            nodes::ExpressionEnum::Var(ref ident, line) => {
                if context.variable_map.contains_key(ident) {
                    nodes::ExpressionEnum::Var(context.variable_map.get(ident).unwrap().name.clone(), line)
                } else {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Variable {:?} not found", ident),
                        line
                    ));
                }
            },
            nodes::ExpressionEnum::Assign(ref lhs, ref rhs, line) => {
                if !self.is_valid_lvalue(&**lhs) {
                    return Err(errors::Error::new(errors::ErrorType::Error, String::from("Invalid lvalue"), line));
                }

                let lhs = self.resolve_expression(lhs, context)?;
                let rhs = self.resolve_expression(rhs, context)?;

                nodes::ExpressionEnum::Assign(Box::new(lhs), Box::new(rhs), line)
            },
            nodes::ExpressionEnum::OpAssign(op, ref lhs, ref rhs, line) => {
                if !self.is_valid_lvalue(&**lhs) {
                    return Err(errors::Error::new(errors::ErrorType::Error, String::from("Invalid lvalue"), line));
                }

                let lhs = self.resolve_expression(lhs, context)?;
                let rhs = self.resolve_expression(rhs, context)?;

                nodes::ExpressionEnum::OpAssign(op, Box::new(lhs), Box::new(rhs), line)
            },
            nodes::ExpressionEnum::Conditional(ref cond, ref lft, ref rht, line) => {
                let cond = Box::new(self.resolve_expression(&**cond, context)?);
                let lft = Box::new(self.resolve_expression(&**lft, context)?);
                let rht = Box::new(self.resolve_expression(&**rht, context)?);

                nodes::ExpressionEnum::Conditional(cond, lft, rht, line)
            },
            nodes::ExpressionEnum::Increment(ref expr, line) => {
                if !self.is_valid_lvalue(&**expr) {
                    return Err(errors::Error::new(errors::ErrorType::Error, String::from("Invalid lvalue"), line));
                }

                let expr = self.resolve_expression(expr, context)?;

                nodes::ExpressionEnum::Increment(Box::new(expr), line)
            },
            nodes::ExpressionEnum::Decrement(ref expr, line) => {
                if !self.is_valid_lvalue(&**expr) {
                    return Err(errors::Error::new(errors::ErrorType::Error, String::from("Invalid lvalue"), line));
                }

                let expr = self.resolve_expression(expr, context)?;

                nodes::ExpressionEnum::Decrement(Box::new(expr), line)
            },
            nodes::ExpressionEnum::FunctionCall(name, args, line) => {
                let ident = &name;
                if context.variable_map.contains_key(&ident) {
                    let name = context.variable_map.get(&ident).unwrap();
                    let mut new_args: Vec<nodes::Expression> = Vec::with_capacity(args.len());
                    for arg in args {
                        new_args.push(self.resolve_expression(&arg, context)?);
                    }
                    nodes::ExpressionEnum::FunctionCall(name.name.clone(), new_args, line)
                } else {
                    return Err(errors::Error::new(errors::ErrorType::Error, format!("Function {:?} not found", ident), line));
                }
            },
            nodes::ExpressionEnum::Dereference(ref expr, line) => {
                let expr = self.resolve_expression(expr, context)?;

                nodes::ExpressionEnum::Dereference(Box::new(expr), line)
            },
            nodes::ExpressionEnum::AddressOf(ref expr, line) => {
                let expr = self.resolve_expression(expr, context)?;

                nodes::ExpressionEnum::AddressOf(Box::new(expr), line)
            },
            nodes::ExpressionEnum::Subscript(ref expr, ref index, line) => {
                let expr = self.resolve_expression(expr, context)?;
                let index = self.resolve_expression(index, context)?;

                nodes::ExpressionEnum::Subscript(Box::new(expr), Box::new(index), line)
            },
            nodes::ExpressionEnum::Cast(ty, ref expr, line) => {
                let expr = self.resolve_expression(expr, context)?;
                let ty = self.resolve_type(&ty, context)?;

                nodes::ExpressionEnum::Cast(ty, Box::new(expr), line)
            },
            nodes::ExpressionEnum::SizeOf(ref expr, line) => {
                let expr = self.resolve_expression(expr, context)?;

                nodes::ExpressionEnum::SizeOf(Box::new(expr), line)
            },
            nodes::ExpressionEnum::SizeOfType(ty, line) => {
                let ty = self.resolve_type(&ty, context)?;
                
                nodes::ExpressionEnum::SizeOfType(ty, line)
            },
            nodes::ExpressionEnum::Arrow(ref expr, ref member, line) => {
                let expr = self.resolve_expression(expr, context)?;

                nodes::ExpressionEnum::Arrow(Box::new(expr), member.clone(), line)
            },
            nodes::ExpressionEnum::Dot(ref expr, ref member, line) => {
                let expr = self.resolve_expression(expr, context)?;

                nodes::ExpressionEnum::Dot(Box::new(expr), member.clone(), line)
            },
        }, expr.line))
    }

    fn resolve_type(&self, type_spec: &nodes::Type, context: &Context) -> Result<nodes::Type, errors::Error> {
        Ok(match type_spec {
            nodes::Type::Struct(ref name) => {
                if context.struct_map.contains_key(name) {
                    nodes::Type::Struct(context.struct_map.get(name).unwrap().0.clone())
                } else {
                    return Err(errors::Error::new(errors::ErrorType::Error, format!("Struct {:?} not found", name), 0));
                }
            },
            nodes::Type::Pointer(ref ty) => {
                let ty = self.resolve_type(ty, context)?;
                nodes::Type::Pointer(Box::new(ty))
            },
            nodes::Type::Array(ref ty, size) => {
                let ty = self.resolve_type(ty, context)?;
                nodes::Type::Array(Box::new(ty), *size)
            },
            nodes::Type::Fn(ref params, ref ty) => {
                let ty = self.resolve_type(ty, context)?;
                let mut new_params: Vec<nodes::Type> = Vec::with_capacity(params.len());
                for param in params {
                    new_params.push(self.resolve_type(param, context)?);
                }
                nodes::Type::Fn(new_params, Box::new(ty))
            },
            nodes::Type::Int | nodes::Type::Char | nodes::Type::Void => type_spec.clone(),
        })
    }
}