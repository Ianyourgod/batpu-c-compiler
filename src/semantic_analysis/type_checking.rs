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

    pub fn resolve(&mut self) -> (nodes::Program, nodes::SymbolTable) {
        let mut statements: Vec<nodes::Declaration> = Vec::with_capacity(self.program.statements.len());
        for decl in self.program.statements.clone() {
            statements.push(match decl {
                nodes::Declaration::VarDecl(var_decl) => nodes::Declaration::VarDecl(self.typecheck_file_scope_variable_declaration(&var_decl)),
                nodes::Declaration::FuncDecl(func) => self.typecheck_function_declaration(&func),
            });
        }

        (nodes::Program {
            statements,
        }, self.symbol_table.clone())
    }

    fn typecheck_function_declaration(&mut self, decl: &nodes::FuncDecl) -> nodes::Declaration {
        let has_body = decl.body.len() > 0;
        let mut already_defined = false;

        let ident = &decl.name;

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

        self.symbol_table.insert(ident.clone(), (decl.ty.clone(), attrs));

        let body = if has_body {
            for param in &decl.params {
                self.symbol_table.insert(param.clone(), (nodes::Type::Int, nodes::TableEntry::LocalAttr));
            }

            let ret_type = match decl.ty.clone() {
                nodes::Type::Fn(_, ret) => *ret,
                _ => panic!("Expected Fn, got {:?}", decl.ty)
            };

            let body = match self.typecheck_block(&decl.body, &ret_type) {
                nodes::Statement::Compound(body) => body,
                _ => unreachable!()
            };

            body

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

    fn zero_init(&self, ty: &nodes::Type) -> nodes::Initializer {
        match ty {
            nodes::Type::Int => nodes::Initializer::Single(nodes::Expression {
                expr: nodes::ExpressionEnum::IntegerLiteral(0),
                ty: nodes::Type::Int,
            }),
            nodes::Type::Pointer(ty) => nodes::Initializer::Single(nodes::Expression {
                expr: nodes::ExpressionEnum::IntegerLiteral(0),
                ty: nodes::Type::Pointer(Box::new(*ty.clone())),
            }),
            nodes::Type::Array(ty, size) => {
                let mut exprs = Vec::with_capacity(*size as usize);
                for _ in 0..*size {
                    exprs.push(self.zero_init(ty));
                }

                nodes::Initializer::Compound(exprs)
            },
            _ => panic!("Cannot zero init type {:?}", ty),
        }
    }

    fn typecheck_initializer(&self, init: &nodes::Initializer, ty: &nodes::Type) -> nodes::Initializer {
        match init {
            nodes::Initializer::Single(expr) => {
                let expr = self.typecheck_and_convert(expr);
                let expr = self.convert_by_assignment(&expr, ty);

                nodes::Initializer::Single(expr)
            },
            nodes::Initializer::Compound(exprs) => {
                let mut new_exprs = Vec::with_capacity(exprs.len());

                let (ty, size) = match ty {
                    nodes::Type::Array(ty, size) => (*ty.clone(), *size),
                    _ => panic!("Expected Array, got {:?}", ty),
                };
                let mut count = 0;
                for expr in exprs {
                    new_exprs.push(self.typecheck_initializer(expr, &ty));
                    count += 1;
                }

                if count > size {
                    panic!("Too many initializers");
                } else if count < size {
                    for _ in count..size {
                        new_exprs.push(nodes::Initializer::Single(nodes::Expression {
                            expr: nodes::ExpressionEnum::IntegerLiteral(0),
                            ty: ty.clone(),
                        }));
                    }
                }

                nodes::Initializer::Compound(new_exprs)
            },
        }
    }

    fn typecheck_file_scope_variable_declaration(&mut self, decl: &nodes::VarDecl) -> nodes::VarDecl {
        let mut initial_value = if decl.expr.is_none() {
            if decl.storage_class == nodes::StorageClass::Extern {
                nodes::InitialValue::NoInit
            } else {
                nodes::InitialValue::Tentative
            }
        // is constant (check if expr is constant)
        } else if let nodes::Initializer::Compound(_) = decl.expr.as_ref().unwrap() {
            panic!("Compound initializers not supported in static");
        } else if let nodes::Initializer::Single(expr) = decl.expr.as_ref().unwrap() {
            let expr = self.typecheck_and_convert(expr);
            if let nodes::ExpressionEnum::IntegerLiteral(i) = expr.expr {
                nodes::InitialValue::Initial(i as i32)
            } else {
                panic!("Non-constant initializer in static declaration");
            }
        } else {
            panic!("Expected Initializer, got {:?}", decl.expr.as_ref().unwrap());
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
            nodes::Statement::Return(expr) => {
                let ret_expr = self.typecheck_and_convert(expr);

                let ret_expr = self.convert_by_assignment(&ret_expr, ret_type);

                nodes::Statement::Return(ret_expr)
            },
            nodes::Statement::Expression(expr) => {
                let expr = self.typecheck_and_convert(expr);

                nodes::Statement::Expression(expr)
            },
            nodes::Statement::Compound(block) => {
                let block = match self.typecheck_block(block, ret_type) {
                    nodes::Statement::Compound(items) => items,
                    e => panic!("Expected compound, found {:?}", e)
                };

                nodes::Statement::Compound(block)
            },
            nodes::Statement::If(expr, block, else_block) => {
                let expr = self.typecheck_and_convert(expr);
                let block = self.typecheck_statement(block, ret_type);
                let else_block = if else_block.is_some() {
                    Some(self.typecheck_statement(else_block.as_ref().as_ref().unwrap(), ret_type)) // im sorry
                } else {
                    None
                };

                nodes::Statement::If(expr.clone(), Box::new(block.clone()), Box::new(else_block))
            },
            nodes::Statement::While(expr, block, _) => {
                let expr = self.typecheck_and_convert(expr);
                let block = self.typecheck_statement(block, ret_type);

                nodes::Statement::While(expr.clone(), Box::new(block.clone()), String::new())
            },
            nodes::Statement::DoWhile(block, expr, label) => {
                let block = self.typecheck_statement(block, ret_type);
                let expr = self.typecheck_and_convert(expr);

                nodes::Statement::DoWhile(Box::new(block), expr.clone(), label.clone())
            },
            nodes::Statement::For(init, cond, post, block, label) => {
                let init = match init {
                    nodes::ForInit::Declaration(decl) => nodes::ForInit::Declaration(self.typecheck_variable_declaration(&self.decl_to_var(decl))),
                    nodes::ForInit::Expression(expr) => nodes::ForInit::Expression(self.typecheck_and_convert(expr)),
                    nodes::ForInit::Empty => nodes::ForInit::Empty,
                };
                let cond = if let Some(cond_expr) = cond.as_ref() {
                    Some(self.typecheck_and_convert(cond_expr))
                } else { None };
                let post = if let Some(expr) = post.as_ref() {
                    Some(self.typecheck_and_convert(expr))
                } else { None };
                let block = self.typecheck_statement(block, ret_type);

                nodes::Statement::For(init, cond, post, Box::new(block), label.clone())
            },
        }
    }

    fn is_null_pointer(&self, expr: &nodes::Expression) -> bool {
        match expr.expr {
            nodes::ExpressionEnum::IntegerLiteral(0) => true,
            _ => false,
        }
    }

    fn get_common_pointer_type(&self, left: &nodes::Expression, rht: &nodes::Expression) -> nodes::Type {
        let left_type = &left.ty;
        let right_type = &rht.ty;

        if left_type == right_type {
            return left_type.clone();
        } else if self.is_null_pointer(left) {
            return right_type.clone();
        } else if self.is_null_pointer(rht) {
            return left_type.clone();
        } else {
            panic!("Incompatible pointer types");
        }
    }

    fn is_pointer_type(&self, ty: &nodes::Type) -> bool {
        match ty {
            nodes::Type::Pointer(_) => true,
            _ => false,
        }
    }

    fn convert_by_assignment(&self, expr: &nodes::Expression, ty: &nodes::Type) -> nodes::Expression {
        if expr.ty == *ty {
            return expr.clone();
        }

        if self.is_null_pointer(expr) {
            return nodes::Expression {
                expr: nodes::ExpressionEnum::IntegerLiteral(0),
                ty: ty.clone(),
            };
        } else {
            panic!("Incompatible types in assignment, expected {:?}, got {:?}", ty, expr.ty);
        }
    }

    fn typecheck_expression(&self, expr: &nodes::Expression) -> nodes::Expression {
        match expr.expr {
            nodes::ExpressionEnum::FunctionCall(ref ident, ref args) => {
                let ident_type = self.symbol_table.lookup(ident).unwrap();
                
                match ident_type.1 {
                    nodes::TableEntry::FnAttr(_, _) => (),
                    _ => panic!("Expected FnAttr, got {:?}", ident_type.1),
                }

                let (fn_type, ret_type) = match ident_type.0 { nodes::Type::Fn(ref args, ref ret_type) => (args, ret_type), _ => panic!("Expected Fn, got {:?}", ident_type.0) };

                let mut new_args = Vec::with_capacity(args.len());
                for (idx, arg) in args.iter().enumerate() {
                    let arg = self.typecheck_and_convert(arg);
                    new_args.push(self.convert_by_assignment(&arg, &fn_type[idx]));
                }

                nodes::Expression {
                    expr: nodes::ExpressionEnum::FunctionCall(ident.clone(), new_args.clone()),
                    ty: *ret_type.clone(),
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
                let lft = self.typecheck_and_convert(&*lhs);
                let rht = self.typecheck_and_convert(&*rhs);

                if lft.ty != rht.ty {
                    panic!("Incompatible types in assignment");
                }

                if !self.is_lvalue(&lft) {
                    panic!("Assignment to non-lvalue");
                }

                let ty = lft.ty.clone();

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Assign(Box::new(lft), Box::new(rht)),
                    ty,
                }
            },
            nodes::ExpressionEnum::Conditional(ref cond, ref then, ref els) => {
                let cond = self.typecheck_and_convert(&*cond);
                let then = self.typecheck_and_convert(&*then);
                let els = self.typecheck_and_convert(&*els);

                let common_type = if self.is_pointer_type(&then.ty) || self.is_pointer_type(&els.ty) {
                    self.get_common_pointer_type(&then, &els)
                } else {
                    self.get_common_type(&then.ty, &els.ty)
                };

                let converted_left = then;
                let converted_right = els;

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Conditional(Box::new(cond), Box::new(converted_left), Box::new(converted_right)),
                    ty: common_type,
                }
            },
            nodes::ExpressionEnum::Increment(ref inner_expr) => {
                let inner_expr = self.typecheck_and_convert(&*inner_expr);

                let ty = inner_expr.ty.clone();

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Increment(Box::new(inner_expr)),
                    ty,
                }
            },
            nodes::ExpressionEnum::Decrement(ref inner_expr) => {
                let inner_expr = self.typecheck_and_convert(&*inner_expr);

                let ty = inner_expr.ty.clone();

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Decrement(Box::new(inner_expr)),
                    ty,
                }
            },
            nodes::ExpressionEnum::Unop(unop, ref expr) => {
                let expr = self.typecheck_and_convert(&*expr);

                if self.is_pointer_type(&expr.ty) {
                    match unop {
                        nodes::Unop::BitwiseNot => panic!("Bitwise not on pointer"),
                        nodes::Unop::Negate => panic!("Negation on pointer"),
                        nodes::Unop::LogicalNot => (),
                    }
                }

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
                let left = self.typecheck_and_convert(&*lhs);
                let right = self.typecheck_and_convert(&*rhs);

                match binop {
                    nodes::Binop::And | nodes::Binop::Or => {
                        return nodes::Expression {
                            expr: nodes::ExpressionEnum::Binop(binop, Box::new(left), Box::new(right)),
                            ty: nodes::Type::Int,
                        };
                    },
                    nodes::Binop::Equal | nodes::Binop::NotEqual => {
                        let common_type = if self.is_pointer_type(&left.ty) || self.is_pointer_type(&right.ty) {
                            self.get_common_pointer_type(&left, &right)
                        } else {
                            self.get_common_type(&left.ty, &right.ty)
                        };

                        let converted_left = left;
                        let converted_right = right;

                        return nodes::Expression {
                            expr: nodes::ExpressionEnum::Binop(binop, Box::new(converted_left), Box::new(converted_right)),
                            ty: common_type,
                        };
                    },
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
            nodes::ExpressionEnum::Dereference(ref expr) => {
                let expr = self.typecheck_and_convert(&*expr);
                let ty = match expr.ty { nodes::Type::Pointer(ref ty) => *ty.clone(), _ => panic!("Expected Ptr, got {:?}", expr.ty) };

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Dereference(Box::new(expr)),
                    ty,
                }
            },
            nodes::ExpressionEnum::AddressOf(ref expr) => {
                if !self.is_lvalue(&*expr) {
                    panic!("Address of non-lvalue");
                }

                let expr = self.typecheck_and_convert(&*expr);

                let ty = nodes::Type::Pointer(Box::new(expr.ty.clone()));
                nodes::Expression {
                    expr: nodes::ExpressionEnum::AddressOf(Box::new(expr)),
                    ty,
                }
            },
            nodes::ExpressionEnum::Subscript(ref left, ref right) => {
                let left = self.typecheck_and_convert(&*left);
                let right = self.typecheck_and_convert(&*right);

                // check if both are pointer, xor
                let lft_ptr = self.is_pointer_type(&left.ty);
                let rht_ptr = self.is_pointer_type(&right.ty);
                let one_pointer = lft_ptr ^ rht_ptr;
                let one_int = left.ty == nodes::Type::Int || right.ty == nodes::Type::Int;

                if !(one_pointer && one_int) {
                    panic!("Invalid types in subscript");
                }

                let ty = if lft_ptr {
                    match left.ty {
                        nodes::Type::Pointer(ref ty) => *ty.clone(),
                        _ => panic!("Expected Ptr, got {:?}", left.ty),
                    }
                } else {
                    match right.ty {
                        nodes::Type::Pointer(ref ty) => *ty.clone(),
                        _ => panic!("Expected Ptr, got {:?}", right.ty),
                    }
                };

                let out = nodes::Expression {
                    expr: nodes::ExpressionEnum::Subscript(Box::new(left), Box::new(right)),
                    ty,
                };

                out
            },
            nodes::ExpressionEnum::CharLiteral(ch) => nodes::Expression {
                expr: nodes::ExpressionEnum::CharLiteral(ch),
                ty: nodes::Type::Char,
            },
            nodes::ExpressionEnum::IntegerLiteral(_) => expr.clone(),
        }
    }

    fn typecheck_and_convert(&self, expr: &nodes::Expression) -> nodes::Expression {
        let expr = self.typecheck_expression(expr);

        match &expr.ty {
            nodes::Type::Array(ty, _) => {
                nodes::Expression {
                    expr: nodes::ExpressionEnum::AddressOf(Box::new(expr.clone())),
                    ty: nodes::Type::Pointer(Box::new(*ty.clone())),
                }
            },
            _ => expr,
        }
    }

    fn is_lvalue(&self, expr: &nodes::Expression) -> bool {
        match expr.expr {
            nodes::ExpressionEnum::Var(_) => true,
            nodes::ExpressionEnum::Dereference(_) => true,
            nodes::ExpressionEnum::Subscript(_, _) => true,
            _ => false,
        }
    }

    fn get_common_type(&self, lhs: &nodes::Type, rhs: &nodes::Type) -> nodes::Type {
        if lhs == rhs {
            return lhs.clone();
        }

        if self.is_pointer_type(lhs) || self.is_pointer_type(rhs) {
            let inner_type = if self.is_pointer_type(lhs) {
                match lhs {
                    nodes::Type::Pointer(ty) => *ty.clone(),
                    _ => panic!("Expected Ptr, got {:?}", lhs),
                }
            } else {
                match rhs {
                    nodes::Type::Pointer(ty) => *ty.clone(),
                    _ => panic!("Expected Ptr, got {:?}", rhs),
                }
            };
            return nodes::Type::Pointer(Box::new(inner_type));
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
        self.symbol_table.insert(decl.name.clone(), (decl.ty.clone(), nodes::TableEntry::LocalAttr));
        let expr = if decl.expr.is_some() {
            let expr = self.typecheck_initializer(decl.expr.as_ref().unwrap(), &decl.ty);

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