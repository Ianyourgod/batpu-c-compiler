use core::panic;

use crate::parser::nodes::{self, BlockItem};

pub struct TypeChecker {
    program: nodes::Program,
    symbol_table: nodes::SymbolTable,
}

impl TypeChecker {
    pub fn new(program: nodes::Program) -> TypeChecker {
        TypeChecker {
            program,
            symbol_table: nodes::SymbolTable::new(),
        }
    }

    pub fn resolve(&mut self) -> nodes::SymbolTable {
        for decl in self.program.statements.clone() {
            match decl {
                nodes::Declaration::VarDecl(var_decl) => nodes::Declaration::VarDecl(self.typecheck_file_scope_variable_declaration(&var_decl)),
                nodes::Declaration::FuncDecl(func) => self.typecheck_function_declaration(&func),
            };
        }

        self.symbol_table.clone()
    }

    fn typecheck_function_declaration(&mut self, decl: &nodes::FuncDecl) -> nodes::Declaration {
        let has_body = decl.body.len() > 0;
        let mut already_defined = false;

        let ident = nodes::Identifier { name: decl.name.clone() };

        let mut global = decl.storage_class != nodes::StorageClass::Static;

        if self.symbol_table.contains(&ident) {
            let old_decl = self.symbol_table.lookup(&ident).unwrap();

            let old_decl = (old_decl.0.clone(), match old_decl.1 {
                nodes::TableEntry::FnAttr(defined, global) => (defined, global),
                _ => panic!("Expected FnAttr, got {:?}", old_decl.1)
            });

            if old_decl.0 != decl.ty {
                panic!("Incompatible redeclaration of function {}", decl.name);
            }
            already_defined = old_decl.1.0;
            if already_defined && has_body {
                panic!("Function {} already defined", decl.name);
            }

            if old_decl.1.1 && !global {
                panic!("Static function declaration follows non-static declaration of {}", decl.name);
            }
            global = old_decl.1.1 || global;
        }

        let attrs = nodes::TableEntry::FnAttr(already_defined || has_body, global);

        self.symbol_table.insert(ident, (decl.ty.clone(), attrs));

        let body = if has_body {
            for param in &decl.params {
                self.symbol_table.insert(param.clone(), (nodes::Type::Int, nodes::TableEntry::LocalAttr));
            }

            let ret_type = match decl.ty.clone() {
                nodes::Type::Fn(_, ret) => *ret,
                _ => panic!("Expected Fn, got {:?}", decl.ty)
            };

            match self.typecheck_block(&decl.body, &ret_type) {
                nodes::Statement::Compound(body) => body,
                _ => unreachable!()
            }
        } else {
            Vec::new()
        };

        nodes::Declaration::FuncDecl(nodes::FuncDecl {
            name: decl.name.clone(),
            params: decl.params.clone(),
            body,
            storage_class: decl.storage_class.clone(),
            ty: decl.ty.clone(),
        })
    }

    fn typecheck_file_scope_variable_declaration(&mut self, decl: &nodes::VarDecl) -> nodes::VarDecl {
        let mut initial_value = if decl.expr.is_none() {
            if decl.storage_class == nodes::StorageClass::Extern {
                nodes::InitialValue::NoInit
            } else {
                nodes::InitialValue::Tentative
            }
        // is constant (check if expr is constant)
        } else if let nodes::ExpressionEnum::IntegerLiteral(i) = decl.expr.as_ref().unwrap().expr {
            nodes::InitialValue::Initial(i as i32)
        } else {
            nodes::InitialValue::Tentative
        };

        let mut global = decl.storage_class != nodes::StorageClass::Static;

        if self.symbol_table.contains(&decl.name) {
            let old_decl = self.symbol_table.lookup(&decl.name).unwrap();
            let old_decl = (old_decl.0.clone(), match old_decl.1.clone() {
                nodes::TableEntry::StaticAttr(init, global) => (init, global),
                _ => panic!("Expected StaticAttr, got {:?}", old_decl.1)
            });

            if old_decl.0 != nodes::Type::Int {
                panic!("Incompatible redeclaration of variable {:?}", decl.name);
            }
            if decl.storage_class == nodes::StorageClass::Extern {
                global = old_decl.1.1;
            } else if old_decl.1.1 != global {
                panic!("Variable {:?} redeclared with different storage class", decl.name);
            }

            if let nodes::InitialValue::Initial(_) = old_decl.1.0 {
                if let nodes::InitialValue::Initial(_) = initial_value {
                    panic!("Variable {:?} redeclared with different initial value", decl.name);
                } else {
                    initial_value = old_decl.1.0;
                }
            } else if let nodes::InitialValue::Initial(_) = initial_value {
                if old_decl.1.0 == nodes::InitialValue::Tentative {
                    initial_value = nodes::InitialValue::Tentative;
                }
            }
        }

        let attrs = nodes::TableEntry::StaticAttr(initial_value, global);
        self.symbol_table.insert(decl.name.clone(), (nodes::Type::Int, attrs));

        nodes::VarDecl {
            name: decl.name.clone(),
            expr: decl.expr.clone(),
            storage_class: decl.storage_class.clone(),
            ty: decl.ty.clone(),
        }
    }

    fn typecheck_statement(&mut self, stmt: &nodes::Statement, ret_type: &nodes::Type) -> nodes::Statement {
        match stmt {
            nodes::Statement::Break(_) | nodes::Statement::Continue(_) |
            nodes::Statement::Empty => stmt.clone(),
            nodes::Statement::Return(expr) | nodes::Statement::Expression(expr) => {
                let ret_expr = self.typecheck_expression(expr);

                if &ret_expr.ty != ret_type {
                    panic!("Incompatible return type expected {:?}, got {:?}", ret_type, ret_expr.ty);
                }

                nodes::Statement::Return(ret_expr)
            },
            nodes::Statement::Compound(block) => {
                self.typecheck_block(block, ret_type);
                nodes::Statement::Compound(block.clone())
            },
            nodes::Statement::If(expr, block, else_block) => {
                let expr = self.typecheck_expression(expr);
                let block = self.typecheck_statement(block, ret_type);
                let else_block = if else_block.is_some() {
                    Some(self.typecheck_statement(else_block.as_ref().as_ref().unwrap(), ret_type)) // im sorry
                } else {
                    None
                };

                nodes::Statement::If(expr.clone(), Box::new(block.clone()), Box::new(else_block))
            },
            nodes::Statement::While(expr, block, _) => {
                let expr = self.typecheck_expression(expr);
                let block = self.typecheck_statement(block, ret_type);

                nodes::Statement::While(expr.clone(), Box::new(block.clone()), String::new())
            },
            nodes::Statement::DoWhile(block, expr, label) => {
                let block = self.typecheck_statement(block, ret_type);
                let expr = self.typecheck_expression(expr);

                nodes::Statement::DoWhile(Box::new(block), expr.clone(), label.clone())
            },
            nodes::Statement::For(init, cond, post, block, label) => {
                let init = match init {
                    nodes::ForInit::Declaration(decl) => nodes::ForInit::Declaration(self.typecheck_variable_declaration(&self.decl_to_var(decl))),
                    nodes::ForInit::Expression(expr) => nodes::ForInit::Expression(self.typecheck_expression(expr)),
                    nodes::ForInit::Empty => nodes::ForInit::Empty,
                };
                let cond = if let Some(cond_expr) = cond.as_ref() {
                    Some(self.typecheck_expression(cond_expr))
                } else { None };
                let post = if let Some(expr) = post.as_ref() {
                    Some(self.typecheck_expression(expr))
                } else { None };
                let block = self.typecheck_statement(block, ret_type);

                nodes::Statement::For(init, cond, post, Box::new(block), label.clone())
            },
        }
    }

    fn typecheck_expression(&self, expr: &nodes::Expression) -> nodes::Expression {
        match expr.expr {
            nodes::ExpressionEnum::FunctionCall(ref ident, ref args) => {
                let ident_type = self.symbol_table.lookup(&nodes::Identifier { name: ident.clone() }).unwrap();
                if ident_type.0 == nodes::Type::Int {
                    panic!("Variable {} is not a function", ident);
                }
                
                let arg_types = &args.iter().map(|arg| arg.ty.clone()).collect::<Vec<_>>();

                let fn_type = match ident_type.0 { nodes::Type::Fn(ref args, _) => args, _ => panic!("Expected Fn, got {:?}", ident_type.0) };
                if fn_type != arg_types {
                    panic!("Function {} expects arguments of type {:?}, got {:?}", ident, fn_type, arg_types);
                }
                for arg in args {
                    self.typecheck_expression(&arg);
                }

                nodes::Expression {
                    expr: nodes::ExpressionEnum::FunctionCall(ident.clone(), args.clone()),
                    ty: ident_type.0.clone(),
                }
            },
            nodes::ExpressionEnum::Var(ref ident) => {
                if !self.symbol_table.contains(&ident) {
                    panic!("Variable {:?} not declared", ident);
                }
                let ident_type = self.symbol_table.lookup(&ident).unwrap();
                let is_fn = match ident_type.0 { nodes::Type::Fn(_, _) => true, _ => false };
                if is_fn {
                    panic!("Function name {:?} used as variable", ident);
                }

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Var(ident.clone()),
                    ty: ident_type.0.clone(),
                }
            },
            nodes::ExpressionEnum::Assign(ref lhs, ref rhs) => {
                let lft = self.typecheck_expression(&*lhs);
                let rht = self.typecheck_expression(&*rhs);

                if lft.ty != rht.ty {
                    panic!("Incompatible types in assignment");
                }

                let ty = lft.ty.clone();

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Assign(Box::new(lft), Box::new(rht)),
                    ty,
                }
            },
            nodes::ExpressionEnum::Conditional(ref cond, ref then, ref els) => {
                let cond = self.typecheck_expression(&*cond);
                let then = self.typecheck_expression(&*then);
                let els = self.typecheck_expression(&*els);

                if then.ty != els.ty {
                    panic!("Incompatible types in conditional expression");
                }

                let ty = then.ty.clone();

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Conditional(Box::new(cond), Box::new(then), Box::new(els)),
                    ty,
                }
            },
            nodes::ExpressionEnum::Increment(ref inner_expr) => {
                let inner_expr = self.typecheck_expression(&*inner_expr);

                let ty = inner_expr.ty.clone();

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Increment(Box::new(inner_expr)),
                    ty,
                }
            },
            nodes::ExpressionEnum::Decrement(ref inner_expr) => {
                let inner_expr = self.typecheck_expression(&*inner_expr);

                let ty = inner_expr.ty.clone();

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Decrement(Box::new(inner_expr)),
                    ty,
                }
            },
            nodes::ExpressionEnum::Unop(unop, ref expr) => {
                let expr = self.typecheck_expression(&*expr);

                match unop {
                    nodes::Unop::BitwiseNot => {
                        let ty = expr.ty.clone();

                        nodes::Expression {
                            expr: nodes::ExpressionEnum::Unop(unop, Box::new(expr)),
                            ty,
                        }
                    }
                    nodes::Unop::Negate => {
                        if expr.ty != nodes::Type::Int {
                            panic!("Negation of non-integer type");
                        }

                        let ty = expr.ty.clone();

                        nodes::Expression {
                            expr: nodes::ExpressionEnum::Unop(unop, Box::new(expr)),
                            ty,
                        }
                    }
                    nodes::Unop::LogicalNot => {
                        nodes::Expression {
                            expr: nodes::ExpressionEnum::Unop(unop, Box::new(expr)),
                            ty: nodes::Type::Int,
                        }
                    }
                }
            },
            nodes::ExpressionEnum::Binop(binop, ref lhs,  ref rhs) => {
                let left = self.typecheck_expression(&*lhs);
                let right = self.typecheck_expression(&*rhs);

                match binop {
                    nodes::Binop::And | nodes::Binop::Or => {
                        return nodes::Expression {
                            expr: nodes::ExpressionEnum::Binop(binop, Box::new(left), Box::new(right)),
                            ty: nodes::Type::Int,
                        };
                    }
                    _ => (),
                }

                let common_type = self.get_common_type(&left.ty, &right.ty);

                let converted_left = left;
                let converted_right = right;

                match binop {
                    nodes::Binop::Add | nodes::Binop::Subtract => 
                        nodes::Expression {
                            expr: nodes::ExpressionEnum::Binop(binop, Box::new(converted_left), Box::new(converted_right)),
                            ty: common_type,
                        },
                    _ =>
                        nodes::Expression {
                            expr: nodes::ExpressionEnum::Binop(binop, Box::new(converted_left), Box::new(converted_right)),
                            ty: nodes::Type::Int,
                        },
                }
            },
            nodes::ExpressionEnum::IntegerLiteral(_) => expr.clone(),
        }
    }

    fn get_common_type(&self, lhs: &nodes::Type, rhs: &nodes::Type) -> nodes::Type {
        if lhs == rhs {
            return lhs.clone();
        }

        return nodes::Type::Int;
    }

    fn typecheck_block(&mut self, body: &Vec<BlockItem>, ret_type: &nodes::Type) -> nodes::Statement {
        let block_items = body.iter().map(|item| self.resolve_block_item(item, ret_type)).collect();

        nodes::Statement::Compound(block_items)
    }

    fn resolve_block_item(&mut self, stmt: &BlockItem, ret_type: &nodes::Type) -> BlockItem {
        match stmt {
            BlockItem::Declaration(decl) => {
                BlockItem::Declaration(match decl {
                    nodes::Declaration::FuncDecl(fn_decl) => self.typecheck_function_declaration(fn_decl),
                    nodes::Declaration::VarDecl(var_decl) => self.typecheck_variable_declaration(var_decl)
                })
            },
            BlockItem::Statement(stmt) => BlockItem::Statement(self.typecheck_statement(stmt, ret_type)),
        }
    }

    fn decl_to_var(&self, decl: &nodes::Declaration) -> nodes::VarDecl {
        match decl {
            nodes::Declaration::VarDecl(var_decl) => var_decl.clone(),
            _ => panic!("Expected VarDecl, got {:?}", decl),
        }
    }

    fn typecheck_variable_declaration(&mut self, decl: &nodes::VarDecl) -> nodes::Declaration {
        self.symbol_table.insert(decl.name.clone(), (nodes::Type::Int, nodes::TableEntry::LocalAttr));
        let expr = if decl.expr.is_some() {
            let expr = self.typecheck_expression(decl.expr.as_ref().unwrap());

            if expr.ty != decl.ty {
                panic!("Incompatible type in variable declaration");
            }

            Some(expr)
        } else { None };

        nodes::Declaration::VarDecl(nodes::VarDecl {
            name: decl.name.clone(),
            expr,
            storage_class: decl.storage_class.clone(),
            ty: decl.ty.clone(),
        })
    }
}