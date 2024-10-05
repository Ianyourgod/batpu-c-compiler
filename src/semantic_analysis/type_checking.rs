use core::panic;

use crate::parser::nodes::{self, BlockItem};

pub struct TypeChecker {
    program: nodes::Program,
    symbol_table: nodes::SymbolTable,
    type_table: nodes::TypeTable,
}

impl TypeChecker {
    pub fn new(program: nodes::Program) -> TypeChecker {
        TypeChecker {
            program,
            symbol_table: nodes::SymbolTable::new(),
            type_table: nodes::TypeTable::new(),
        }
    }

    pub fn resolve(&mut self) -> (nodes::Program, nodes::SymbolTable, nodes::TypeTable) {
        let mut statements: Vec<nodes::Declaration> = Vec::with_capacity(self.program.statements.len());
        for decl in self.program.statements.clone() {
            statements.push(match decl {
                nodes::Declaration::VarDecl(var_decl) => nodes::Declaration::VarDecl(self.typecheck_file_scope_variable_declaration(&var_decl)),
                nodes::Declaration::FuncDecl(func) => self.typecheck_function_declaration(&func),
                nodes::Declaration::StructDecl(struct_decl) => self.typecheck_struct_declaration(&struct_decl),
                nodes::Declaration::Empty => nodes::Declaration::Empty,
            });
        }

        (nodes::Program {
            statements,
        }, self.symbol_table.clone(), self.type_table.clone())
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

        self.validate_type_specifier(&decl.ty);

        let attrs = nodes::TableEntry::FnAttr(already_defined || has_body, global);

        self.symbol_table.insert(ident.clone(), (decl.ty.clone(), attrs));

        let (param_types, ret_type) = match &decl.ty {
            nodes::Type::Fn(args, ret) => (args, ret),
            _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected Fn, got {:?}", decl.ty),
        };

        if let nodes::Type::Struct(ref tag) = **ret_type {
            panic!("Cant send structs as ret type rn (tag = {})", tag);
        }

        for (idx, param) in decl.params.iter().enumerate() {
            let param_type = &param_types[idx];

            if let nodes::Type::Struct(_) = param_type {
                panic!("Cant send structs as params rn");
            }

            if !self.is_complete_type(param_type) {
                panic!("Incomplete type in function parameter");
            }
    
            self.validate_type_specifier(param_type);

            self.symbol_table.insert(param.clone(), (param_type.clone(), nodes::TableEntry::LocalAttr));
        }

        let body = if has_body {
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
            nodes::Type::Struct(tag) => {
                if !self.type_table.contains(tag) {
                    panic!("Unknown struct {:?}", tag);
                }
                let struct_def = self.type_table.lookup(tag).unwrap();

                let mut exprs = Vec::with_capacity(struct_def.1.len());
                for member in &struct_def.1 {
                    exprs.push(self.zero_init(&member.ty));
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
                match ty {
                    nodes::Type::Array(ty, size) => {
                        let mut new_exprs = Vec::with_capacity(exprs.len());

                        let (ty, size) = (*ty.clone(), *size);
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
                    nodes::Type::Struct(tag) => {
                        if !self.type_table.contains(tag) {
                            panic!("Unknown struct {:?}", tag);
                        }
                        let struct_def = self.type_table.lookup(tag).unwrap();

                        if exprs.len() > struct_def.1.len() {
                            panic!("Too many initializers");
                        }

                        let mut i = 0;
                        let mut typechecked_list = Vec::with_capacity(struct_def.1.len());
                        for init_elem in exprs {
                            let elem_ty = &struct_def.1[i].ty;
                            let init_elem = self.typecheck_initializer(init_elem, elem_ty);
                            typechecked_list.push(init_elem);
                            i += 1;
                        }
                        while i < struct_def.1.len() {
                            typechecked_list.push(self.zero_init(&struct_def.1[i].ty));
                            i += 1;
                        }

                        nodes::Initializer::Compound(typechecked_list)
                    },
                    _ => panic!("Cannot initialize type {:?} with compound initializer", ty),
                }
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
                if expr.is_none() && *ret_type != nodes::Type::Void {
                    panic!("Expected return value");
                } else if expr.is_some() && *ret_type == nodes::Type::Void {
                    panic!("Unexpected return value");
                }

                if expr.is_none() {
                    return nodes::Statement::Return(None);
                }

                let expr = expr.as_ref().unwrap();

                let ret_expr = self.typecheck_and_convert(expr);

                let ret_expr = self.convert_by_assignment(&ret_expr, ret_type);

                nodes::Statement::Return(Some(ret_expr))
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
            nodes::Statement::While(expr, block, label) => {
                let expr = self.typecheck_and_convert(expr);
                let block = self.typecheck_statement(block, ret_type);

                nodes::Statement::While(expr.clone(), Box::new(block.clone()), label.clone())
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

        let left_is_ptr = self.is_pointer_type(left_type);
        let right_is_ptr = self.is_pointer_type(right_type);

        if left_type == right_type {
            return left_type.clone();
        } else if self.is_null_pointer(left) {
            return right_type.clone();
        } else if self.is_null_pointer(rht) {
            return left_type.clone();
        } else if (self.is_void_ptr(left_type) || self.is_void_ptr(right_type)) && left_is_ptr && right_is_ptr {
            return nodes::Type::Pointer(Box::new(nodes::Type::Void));
        } else if left_is_ptr && right_is_ptr {
            let left_inner = match left_type {
                nodes::Type::Pointer(ty) => ty,
                _ => panic!("Expected Ptr, got {:?}", left_type),
            };
            let right_inner = match right_type {
                nodes::Type::Pointer(ty) => ty,
                _ => panic!("Expected Ptr, got {:?}", right_type),
            };

            if left_inner == right_inner {
                return left_type.clone();
            } else {
                panic!("Incompatible pointer types");
            }
        } else if left_is_ptr {
            return left_type.clone();
        } else {
            panic!("Incompatible pointer types");
        }
    }

    fn is_void_ptr(&self, ty: &nodes::Type) -> bool {
        match ty {
            nodes::Type::Pointer(ref ty) => **ty == nodes::Type::Void,
            _ => false,
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

        if (self.is_arithmetic_type(&expr.ty) && self.is_arithmetic_type(ty)) ||
           (self.is_null_pointer(expr) && self.is_pointer_type(ty)) ||
           (self.is_void_ptr(&expr.ty) && self.is_pointer_type(ty)) ||
           (self.is_void_ptr(ty) && self.is_pointer_type(&expr.ty)) {
            return self.convert_to(expr.clone(), ty);
        } else {
            panic!("Incompatible types in cba assignment, expected {:?}, got {:?}", ty, expr.ty);
        }
    }

    fn is_arithmetic_type(&self, ty: &nodes::Type) -> bool {
        match ty {
            nodes::Type::Int => true,
            _ => false,
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
                    panic!("Incompatible types in assignment, {:?} and {:?}", lft.ty, rht.ty);
                }

                if !self.is_lvalue(&lft) {
                    panic!("Assignment to non-lvalue");
                }

                let ty = lft.ty.clone();

                let right = self.convert_by_assignment(&rht, &ty);

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Assign(Box::new(lft), Box::new(right)),
                    ty,
                }
            },
            nodes::ExpressionEnum::Conditional(ref cond, ref then, ref els) => {
                let cond = self.typecheck_and_convert(&*cond);
                let then = self.typecheck_and_convert(&*then);
                let els = self.typecheck_and_convert(&*els);

                if !self.is_scalar_type(&cond.ty) {
                    panic!("Invalid type for conditional");
                }


                
                let common_type = if then.ty == nodes::Type::Void && els.ty == nodes::Type::Void {
                    nodes::Type::Void
                } else if self.is_pointer_type(&then.ty) || self.is_pointer_type(&els.ty) {
                    self.get_common_pointer_type(&then, &els)
                } else if self.is_arithmetic_type(&then.ty) && self.is_arithmetic_type(&els.ty) {
                    self.get_common_type(&then.ty, &els.ty)
                } else {
                    let l_tag = match then.ty {
                        nodes::Type::Struct(ref tag) => tag.clone(),
                        _ => panic!("Invalid types in conditional"),
                    };
                    let r_tag = match els.ty {
                        nodes::Type::Struct(ref tag) => tag.clone(),
                        _ => panic!("Invalid types in conditional"),
                    };

                    if l_tag != r_tag {
                        panic!("Incompatible types in conditional");
                    }

                    then.ty.clone()
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

                if !self.is_scalar_type(&expr.ty) {
                    panic!("Invalid type for unary operator");
                }

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
                        let expr = if expr.ty == nodes::Type::Char {
                            self.convert_to(expr, &nodes::Type::Int)
                        } else {
                            expr
                        };

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
                        #[allow(unused_variables)]
                        let common_type = if self.is_pointer_type(&left.ty) || self.is_pointer_type(&right.ty) {
                            self.get_common_pointer_type(&left, &right)
                        } else if self.is_arithmetic_type(&left.ty) && self.is_arithmetic_type(&right.ty) {
                            self.get_common_type(&left.ty, &right.ty)
                        } else if left.ty == right.ty {
                            left.ty.clone()
                        } else {
                            panic!("Invalid types in comparison ({:?} and {:?})", left.ty, right.ty);
                        };

                        let converted_left = left;
                        let converted_right = right;

                        return nodes::Expression {
                            expr: nodes::ExpressionEnum::Binop(binop, Box::new(converted_left), Box::new(converted_right)),
                            ty: nodes::Type::Int,
                        };
                    },
                    _ => (),
                }

                let common_type = self.get_common_type(&left.ty, &right.ty);

                let converted_left = self.convert_to(left, &common_type);
                let converted_right = self.convert_to(right, &common_type);

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

                if ty == nodes::Type::Void {
                    panic!("Dereference of void pointer");
                }

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
            nodes::ExpressionEnum::Cast(ref ty, ref expr) => {
                let expr = self.typecheck_and_convert(&*expr);

                if !self.is_scalar_type(ty) {
                    panic!("Invalid type in cast");
                } else if !self.is_scalar_type(&expr.ty) {
                    panic!("Invalid type in cast");
                }

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Cast(ty.clone(), Box::new(expr)),
                    ty: ty.clone(),
                }
            },
            nodes::ExpressionEnum::SizeOfType(ref ty) => {
                self.validate_type_specifier(ty);
                if !self.is_complete_type(ty) {
                    panic!("Incomplete type in sizeof, {:?}", ty);
                }
                nodes::Expression {
                    expr: nodes::ExpressionEnum::SizeOfType(ty.clone()),
                    ty: nodes::Type::Int,
                }
            },
            nodes::ExpressionEnum::SizeOf(ref expr) => {
                let expr = self.typecheck_and_convert(&*expr);
                if !self.is_complete_type(&expr.ty) {
                    panic!("Cant get size of incomplete type");
                }
                nodes::Expression {
                    expr: nodes::ExpressionEnum::SizeOf(Box::new(expr)),
                    ty: nodes::Type::Int,
                }
            },
            nodes::ExpressionEnum::Dot(ref expr, ref field) => {
                let expr = self.typecheck_and_convert(&*expr);

                let ty = match expr.ty {
                    nodes::Type::Struct(ref tag) => {
                        if !self.type_table.contains(&tag) {
                            panic!("Unknown struct {:?}", tag);
                        }
                        let struct_def = self.type_table.lookup(&tag).unwrap();
                        // its a vec of MemberEntrys (String, Type, Offset). we want to find it by the string, and return the type
                        let field_ty = struct_def.1.iter().find(|memb_entry| memb_entry.name == *field).unwrap().ty.clone();
                        field_ty.clone()
                    },
                    _ => panic!("Expected Struct, got {:?}", expr.ty),
                };

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Dot(Box::new(expr), field.clone()),
                    ty,
                }
            },
            nodes::ExpressionEnum::Arrow(ref expr, ref field) => {
                let expr = self.typecheck_and_convert(&*expr);

                let ty = match expr.ty {
                    nodes::Type::Pointer(ref ty) => {
                        match &**ty {
                            nodes::Type::Struct(tag) => {
                                if !self.type_table.contains(&tag) {
                                    panic!("Unknown struct {:?}", tag);
                                }
                                let struct_def = self.type_table.lookup(&tag).unwrap();
                                let field_ty = struct_def.1.iter().find(|memb_entry| memb_entry.name == *field).unwrap().ty.clone();
                                field_ty.clone()
                            },
                            _ => panic!("Expected Struct, got {:?}", ty),
                        }
                    },
                    _ => panic!("Expected Ptr, got {:?}", expr.ty),
                };

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Arrow(Box::new(expr), field.clone()),
                    ty,
                }
            },
            nodes::ExpressionEnum::CharLiteral(ch) => nodes::Expression {
                expr: nodes::ExpressionEnum::CharLiteral(ch),
                ty: nodes::Type::Char,
            },
            nodes::ExpressionEnum::StringLiteral(ref s) => nodes::Expression {
                expr: nodes::ExpressionEnum::StringLiteral(s.clone()),
                ty: nodes::Type::Pointer(Box::new(nodes::Type::Char)),
            },
            nodes::ExpressionEnum::IntegerLiteral(_) => expr.clone(),
        }
    }

    fn is_complete_type(&self, ty: &nodes::Type) -> bool {
        match ty {
            nodes::Type::Void => false,
            nodes::Type::Struct(tag) => {
                self.type_table.contains(tag)
            },
            _ => true,
        }
    }

    fn is_pointer_to_complete_type(&self, ty: &nodes::Type) -> bool {
        match ty {
            nodes::Type::Pointer(ty) => self.is_complete_type(ty),
            _ => false,
        }
    }

    fn validate_type_specifier(&self, ty: &nodes::Type) {
        match ty {
            nodes::Type::Array(ty, _) => {
                if !self.is_complete_type(ty) {
                    panic!("Incomplete type in array declaration");
                }
                self.validate_type_specifier(ty);
            },
            nodes::Type::Pointer(ty) => {
                self.validate_type_specifier(ty);
            },
            nodes::Type::Fn(args, ret) => {
                for arg in args {
                    self.validate_type_specifier(arg);
                }
                self.validate_type_specifier(ret);
            },
            _ => (),
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
            nodes::Type::Struct(tag) => {
                if !self.type_table.contains(tag) {
                    panic!("Unknown struct {:?}", tag);
                }
                expr
            },
            _ => expr,
        }
    }

    fn is_lvalue(&self, expr: &nodes::Expression) -> bool {
        match expr.expr {
            nodes::ExpressionEnum::Var(_) => true,
            nodes::ExpressionEnum::Dereference(_) => true,
            nodes::ExpressionEnum::Subscript(_, _) => true,
            nodes::ExpressionEnum::Dot(_, _) => true,
            nodes::ExpressionEnum::Arrow(_, _) => true,
            _ => false,
        }
    }

    fn convert_to(&self, expr: nodes::Expression, ty: &nodes::Type) -> nodes::Expression {
        if expr.ty == *ty {
            return expr.clone();
        }

        return nodes::Expression {
            expr: nodes::ExpressionEnum::Cast(ty.clone(), Box::new(expr)),
            ty: ty.clone(),
        };
    }

    fn is_scalar_type(&self, ty: &nodes::Type) -> bool {
        match ty {
            nodes::Type::Array(_, _) |
            nodes::Type::Fn(_, _) |
            nodes::Type::Void |
            nodes::Type::Struct(_) => false,

            nodes::Type::Pointer(_) |
            nodes::Type::Int |
            nodes::Type::Char => true,
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
                    nodes::Declaration::Empty => nodes::Declaration::Empty,
                    nodes::Declaration::FuncDecl(fn_decl) => self.typecheck_function_declaration(fn_decl),
                    nodes::Declaration::VarDecl(var_decl) => self.typecheck_variable_declaration(var_decl),
                    nodes::Declaration::StructDecl(struct_decl) => self.typecheck_struct_declaration(struct_decl),
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

        if decl.ty == nodes::Type::Void {
            panic!("Cannot declare variable of type void");
        }

        self.validate_type_specifier(&decl.ty);

        nodes::Declaration::VarDecl(nodes::VarDecl {
            name: decl.name.clone(),
            expr,
            storage_class: decl.storage_class.clone(),
            ty: decl.ty.clone(),
        })
    }

    fn typecheck_struct_declaration(&mut self, decl: &nodes::StructDecl) -> nodes::Declaration {
        if decl.members.len() == 0 {
            return nodes::Declaration::StructDecl(decl.clone());
        }
        self.validate_struct_declaration(decl);

        let mut member_entries = Vec::new();
        let mut struct_size = 0;

        for member in &decl.members {
            let member_type = member.ty.clone();
            let member_size = self.get_size(&member_type);

            let offset = struct_size;
            struct_size += member_size;

            member_entries.push(nodes::MemberEntry {
                name: member.name.clone(),
                ty: member_type,
                offset,
            });
        }

        let name = decl.tag.clone();
        self.type_table.insert(name, (struct_size, member_entries));

        nodes::Declaration::StructDecl(decl.clone())
    }

    fn get_size(&self, ty: &nodes::Type) -> i32 {
        match ty {
            nodes::Type::Int | nodes::Type::Pointer(_) |
            nodes::Type::Char => 1,
            nodes::Type::Array(ty, size) => self.get_size(ty) * *size as i32,
            nodes::Type::Struct(decl) => {
                self.type_table.lookup(decl).unwrap().0
            },
            nodes::Type::Fn(_, _) => panic!("Function types should not be used in this context"),
            nodes::Type::Void => panic!("Void is weird"),
        }
    }

    fn validate_struct_declaration(&self, decl: &nodes::StructDecl) {
        for member in &decl.members {
            self.validate_type_specifier(&member.ty);
        }
    }
}