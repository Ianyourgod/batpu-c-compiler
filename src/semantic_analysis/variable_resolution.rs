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

    pub fn resolve(&mut self) -> nodes::Program {
        let mut statements: Vec<nodes::Declaration> = Vec::new();

        let mut global_context = Context {
            variable_map: VariableMap::new(),
            struct_map: HashMap::new(),
        };

        for decl in self.program.statements.clone() {
            match decl {
                nodes::Declaration::FuncDecl(func) => {
                    let func = self.resolve_function_declaration(&func, &mut global_context);

                    statements.push(nodes::Declaration::FuncDecl(func));
                },
                nodes::Declaration::VarDecl(var_decl) => {
                    let decl = self.resolve_file_scope_declaration(&var_decl, &mut global_context);

                    statements.push(decl);
                }
                nodes::Declaration::StructDecl(struct_decl) => {
                    let decl = self.resolve_struct_declaration(&struct_decl, &mut global_context);

                    statements.push(nodes::Declaration::StructDecl(decl));
                }
                nodes::Declaration::Empty => statements.push(nodes::Declaration::Empty),
            }
        }

        nodes::Program {
            statements,
        }
    }

    fn resolve_struct_declaration(&mut self, decl: &nodes::StructDecl, context: &mut Context) -> nodes::StructDecl {
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
                ty: self.resolve_type(&member.ty, context),
            });
        }

        nodes::StructDecl {
            tag: unique_tag,
            members: new_members,
        }
    }

    fn resolve_function_declaration(&mut self, decl: &nodes::FuncDecl, context: &mut Context) -> nodes::FuncDecl {
        let decl_name = &decl.name;
        if context.variable_map.contains_key(&decl_name) {
            let old_decl = context.variable_map.get(&decl_name).unwrap();
            if old_decl.from_current_scope && !old_decl.has_external_linkage {
                panic!("Function {:?} already declared", decl_name);
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
                panic!("Variable {:?} already declared", param);
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
                new_body.push(self.resolve_block_item(&stmt, &mut inner_map));
            }
        }

        let ty = self.resolve_type(&decl.ty, context);

        nodes::FuncDecl {
            name: decl.name.clone(),
            params: new_params,
            body: new_body,
            storage_class: decl.storage_class.clone(),
            ty,
        }
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

        nodes::Declaration::VarDecl(decl.clone())
    }

    fn resolve_block_item(&mut self, stmt: &nodes::BlockItem, context: &mut Context) -> nodes::BlockItem {
        match stmt {
            nodes::BlockItem::Statement(ref inner_stmt) => nodes::BlockItem::Statement(self.resolve_statement(inner_stmt, context)),
            nodes::BlockItem::Declaration(ref decl) => {
                match decl {
                    nodes::Declaration::VarDecl(decl) => {
                        if context.variable_map.contains_key(&decl.name) {
                            let old_decl = context.variable_map.get(&decl.name).unwrap();
                            if old_decl.from_current_scope && !(old_decl.has_external_linkage && decl.storage_class == nodes::StorageClass::Extern) {
                                panic!("Conflicting local declarations of variable {:?}", decl.name);
                            }
                        }

                        if decl.storage_class == nodes::StorageClass::Extern {
                            context.variable_map.insert(decl.name.clone(), VarData {
                                name: decl.name.clone(),
                                has_expr: decl.expr.is_some(),
                                from_current_scope: true,
                                has_external_linkage: true,
                            });
                            return nodes::BlockItem::Declaration(nodes::Declaration::VarDecl(decl.clone()));
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
                            let val = self.resolve_init(expr, context);
                            let ty = self.resolve_type(&decl.ty, context);
                            nodes::BlockItem::Declaration(nodes::Declaration::VarDecl(nodes::VarDecl {
                                name: unique_name,
                                expr: Some(val),
                                storage_class: decl.storage_class,
                                ty,
                            }))
                        } else {
                            let ty = self.resolve_type(&decl.ty, context);
                            nodes::BlockItem::Declaration(nodes::Declaration::VarDecl(nodes::VarDecl {
                                name: unique_name,
                                expr: None,
                                storage_class: decl.storage_class,
                                ty
                            }))
                        }
                    }
                    nodes::Declaration::StructDecl(decl) => {
                        nodes::BlockItem::Declaration(nodes::Declaration::StructDecl(self.resolve_struct_declaration(decl, context)))
                    }
                    nodes::Declaration::FuncDecl(_) => {
                        panic!("Function declarations not allowed in block items");
                    }
                    nodes::Declaration::Empty => nodes::BlockItem::Declaration(nodes::Declaration::Empty),
                }
                
            },
        }
    }

    fn resolve_init(&mut self, init: &nodes::Initializer, context: &mut Context) -> nodes::Initializer {
        match init {
            nodes::Initializer::Single(ref expr) => {
                let val = self.resolve_expression(expr, context);
                nodes::Initializer::Single(val)
            },
            nodes::Initializer::Compound(ref inits) => {
                let mut new_inits: Vec<nodes::Initializer> = Vec::new();
                for init in inits {
                    new_inits.push(self.resolve_init(init, context));
                }
                nodes::Initializer::Compound(new_inits)
            },
        }
    }

    fn resolve_statement(&mut self, stmt: &nodes::Statement, context: &mut Context) -> nodes::Statement {
        match stmt {
            nodes::Statement::Return(ref expr) => {
                match expr {
                    Some(expr) => {
                        let val = self.resolve_expression(expr, context);
                        nodes::Statement::Return(Some(val))
                    }
                    None => nodes::Statement::Return(None)
                }
            },
            nodes::Statement::Expression(ref expr) => {
                let val = self.resolve_expression(expr, context);
                nodes::Statement::Expression(val)
            },
            nodes::Statement::If(ref cond, ref then, ref else_) => {
                let cond = self.resolve_expression(cond, context);
                let lft = Box::new(self.resolve_statement(&**then, context));
                let rht = Box::new(match *else_.clone() {
                    Some(stmt) => Some(self.resolve_statement(&stmt, context)),
                    None => None
                });

                nodes::Statement::If(cond, lft, rht)
            },
            nodes::Statement::Compound(ref stmts) => {
                let mut new_var_map = context.clone();
                nodes::Statement::Compound(stmts.iter().map(|stmt| self.resolve_block_item(stmt, &mut new_var_map)).collect())
            }
            nodes::Statement::Break(_) => stmt.clone(),
            nodes::Statement::Continue(_) => stmt.clone(),
            nodes::Statement::While(ref cond, ref body, _) => {
                let cond = self.resolve_expression(cond, context);
                let body = Box::new(self.resolve_statement(&**body, context));

                nodes::Statement::While(cond, body, String::new())
            },
            nodes::Statement::DoWhile(ref body, ref cond, _) => {
                let body = Box::new(self.resolve_statement(&**body, context));
                let cond = self.resolve_expression(cond, context);

                nodes::Statement::DoWhile(body, cond, String::new())
            },
            nodes::Statement::For(ref init, ref cond, ref post, ref body, _) => {
                let init = match init {
                    nodes::ForInit::Declaration(ref decl) => {
                        let decl = match decl { nodes::Declaration::VarDecl(ref decl) => decl, _ => panic!("Invalid declaration") };

                        if context.variable_map.contains_key(&decl.name) {
                            let old_decl = context.variable_map.get(&decl.name).unwrap();
                            if old_decl.from_current_scope && !(old_decl.has_external_linkage && decl.storage_class == nodes::StorageClass::Extern) {
                                panic!("Conflicting local declarations of variable {:?}", decl.name);
                            }
                        }

                        if decl.storage_class == nodes::StorageClass::Extern {
                            panic!("Cannot declare extern variables in for loop");
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
                            let val = self.resolve_init(expr, context);
                            let ty = self.resolve_type(&decl.ty, context);
                            nodes::ForInit::Declaration(nodes::Declaration::VarDecl(nodes::VarDecl {
                                name: unique_name,
                                expr: Some(val),
                                storage_class: decl.storage_class,
                                ty,
                            }))
                        } else {
                            nodes::ForInit::Empty
                        }
                    },
                    nodes::ForInit::Expression(ref expr) => {
                        let expr = self.resolve_expression(expr, context);
                        nodes::ForInit::Expression(expr)
                    },
                    nodes::ForInit::Empty => nodes::ForInit::Empty,
                };

                let cond = match cond {
                    Some(ref expr) => Some(self.resolve_expression(expr, context)),
                    None => None,
                };

                let post = match post {
                    Some(ref expr) => Some(self.resolve_expression(expr, context)),
                    None => None,
                };

                let body = Box::new(self.resolve_statement(&**body, context));

                nodes::Statement::For(init, cond, post, body, String::new())
            },
            nodes::Statement::Empty => stmt.clone(),
        }
    }

    fn is_valid_lvalue(&self, expr: &nodes::Expression) -> bool {
        match expr.expr {
            nodes::ExpressionEnum::Var(_) => true,
            nodes::ExpressionEnum::Dereference(_) => true,
            nodes::ExpressionEnum::Subscript(_, _) => true,
            nodes::ExpressionEnum::Dot(_, _) |
            nodes::ExpressionEnum::Arrow(_, _) => true,
            _ => false,
        }
    }

    fn resolve_expression(&mut self, expr: &nodes::Expression, context: &Context) -> nodes::Expression {
        nodes::Expression::new(match expr.expr.clone() {
            nodes::ExpressionEnum::Binop(ref op, ref src1, ref src2) => {
                let src1 = self.resolve_expression(src1, context);
                let src2 = self.resolve_expression(src2, context);

                nodes::ExpressionEnum::Binop(op.clone(), Box::new(src1), Box::new(src2))
            },
            nodes::ExpressionEnum::Unop(ref op, ref src) => {
                let src = self.resolve_expression(src, context);

                nodes::ExpressionEnum::Unop(op.clone(), Box::new(src))
            },
            nodes::ExpressionEnum::IntegerLiteral(_) | nodes::ExpressionEnum::CharLiteral(_) |
            nodes::ExpressionEnum::StringLiteral(_) => expr.expr.clone(),
            nodes::ExpressionEnum::Var(ref ident) => {
                if context.variable_map.contains_key(ident) {
                    nodes::ExpressionEnum::Var(context.variable_map.get(ident).unwrap().name.clone())
                } else {
                    panic!("Variable {:?} not found", ident);
                }
            },
            nodes::ExpressionEnum::Assign(ref lhs, ref rhs) => {
                if !self.is_valid_lvalue(&**lhs) {
                    panic!("Invalid lvalue");
                }

                let lhs = self.resolve_expression(lhs, context);
                let rhs = self.resolve_expression(rhs, context);

                nodes::ExpressionEnum::Assign(Box::new(lhs), Box::new(rhs))
            },
            nodes::ExpressionEnum::Conditional(ref cond, ref lft, ref rht) => {
                let cond = Box::new(self.resolve_expression(&**cond, context));
                let lft = Box::new(self.resolve_expression(&**lft, context));
                let rht = Box::new(self.resolve_expression(&**rht, context));

                nodes::ExpressionEnum::Conditional(cond, lft, rht)
            },
            nodes::ExpressionEnum::Increment(ref expr) => {
                if !self.is_valid_lvalue(&**expr) {
                    panic!("Invalid lvalue");
                }

                let expr = self.resolve_expression(expr, context);

                nodes::ExpressionEnum::Increment(Box::new(expr))
            },
            nodes::ExpressionEnum::Decrement(ref expr) => {
                if !self.is_valid_lvalue(&**expr) {
                    panic!("Invalid lvalue");
                }

                let expr = self.resolve_expression(expr, context);

                nodes::ExpressionEnum::Decrement(Box::new(expr))
            },
            nodes::ExpressionEnum::FunctionCall(name, args) => {
                let ident = &name;
                if context.variable_map.contains_key(&ident) {
                    let name = context.variable_map.get(&ident).unwrap();
                    let args = args.iter().map(|arg| self.resolve_expression(arg, context)).collect();
                    nodes::ExpressionEnum::FunctionCall(name.name.clone(), args)
                } else {
                    panic!("Function {:?} not found", name);
                }
            },
            nodes::ExpressionEnum::Dereference(ref expr) => {
                let expr = self.resolve_expression(expr, context);

                nodes::ExpressionEnum::Dereference(Box::new(expr))
            },
            nodes::ExpressionEnum::AddressOf(ref expr) => {
                let expr = self.resolve_expression(expr, context);

                nodes::ExpressionEnum::AddressOf(Box::new(expr))
            },
            nodes::ExpressionEnum::Subscript(ref expr, ref index) => {
                let expr = self.resolve_expression(expr, context);
                let index = self.resolve_expression(index, context);

                nodes::ExpressionEnum::Subscript(Box::new(expr), Box::new(index))
            },
            nodes::ExpressionEnum::Cast(ty, ref expr) => {
                let expr = self.resolve_expression(expr, context);
                let ty = self.resolve_type(&ty, context);

                nodes::ExpressionEnum::Cast(ty, Box::new(expr))
            },
            nodes::ExpressionEnum::SizeOf(ref expr) => {
                let expr = self.resolve_expression(expr, context);

                nodes::ExpressionEnum::SizeOf(Box::new(expr))
            },
            nodes::ExpressionEnum::SizeOfType(ty) => {
                let ty = self.resolve_type(&ty, context);
                
                nodes::ExpressionEnum::SizeOfType(ty)
            },
            nodes::ExpressionEnum::Arrow(ref expr, ref member) => {
                let expr = self.resolve_expression(expr, context);

                nodes::ExpressionEnum::Arrow(Box::new(expr), member.clone())
            },
            nodes::ExpressionEnum::Dot(ref expr, ref member) => {
                let expr = self.resolve_expression(expr, context);

                nodes::ExpressionEnum::Dot(Box::new(expr), member.clone())
            },
        })
    }

    fn resolve_type(&self, type_spec: &nodes::Type, context: &Context) -> nodes::Type {
        match type_spec {
            nodes::Type::Struct(ref name) => {
                if context.struct_map.contains_key(name) {
                    nodes::Type::Struct(context.struct_map.get(name).unwrap().0.clone())
                } else {
                    panic!("Struct {:?} not found", name);
                }
            },
            nodes::Type::Pointer(ref ty) => {
                let ty = self.resolve_type(ty, context);
                nodes::Type::Pointer(Box::new(ty))
            },
            nodes::Type::Array(ref ty, size) => {
                let ty = self.resolve_type(ty, context);
                nodes::Type::Array(Box::new(ty), *size)
            },
            nodes::Type::Fn(ref params, ref ty) => {
                let ty = self.resolve_type(ty, context);
                let params = params.iter().map(|param| self.resolve_type(param, context)).collect();
                nodes::Type::Fn(params, Box::new(ty))
            },
            nodes::Type::Int | nodes::Type::Char | nodes::Type::Void => type_spec.clone(),
        }
    }
}