use std::collections::HashSet;

use crate::{errors, parser::nodes::{self, BlockItem}};

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

    pub fn resolve(mut self) -> Result<(nodes::Program, nodes::SymbolTable, nodes::TypeTable, HashSet<String>), errors::Error> {
        let mut statements: Vec<nodes::Declaration> = Vec::with_capacity(self.program.statements.len());
        let mut used_functions = HashSet::new();
        for decl in self.program.statements.clone() {
            statements.push(match decl {
                nodes::Declaration::VarDecl(var_decl, line) => nodes::Declaration::VarDecl(self.typecheck_file_scope_variable_declaration(&var_decl, &mut used_functions)?, line),
                nodes::Declaration::FuncDecl(func, _) => self.typecheck_function_declaration(&func, &mut used_functions)?,
                nodes::Declaration::StructDecl(struct_decl, _) => self.typecheck_struct_declaration(&struct_decl)?,
                nodes::Declaration::Empty(line) => nodes::Declaration::Empty(line),
            });
        }

        Ok((nodes::Program {
            statements,
        }, self.symbol_table, self.type_table, used_functions))
    }

    fn typecheck_function_declaration(&mut self, decl: &nodes::FuncDecl, used_functions: &mut HashSet<String>) -> Result<nodes::Declaration, errors::Error> {
        let mut already_defined = false;

        let ident = &decl.name;

        let mut global = decl.storage_class != nodes::StorageClass::Static;

        if self.symbol_table.contains(&ident) {
            let old_decl = self.symbol_table.lookup(&ident).unwrap();

            let old_decl = (old_decl.0.clone(), match old_decl.1 {
                nodes::TableEntry::FnAttr(defined, global) => (defined, global),
                _ => return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Expected FnAttr, got {:?}", old_decl.1),
                    decl.line,
                )),
            });

            if old_decl.0 != decl.ty {
                return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Incompatible redeclaration of function {}", decl.name),
                    decl.line,
                ));
            }
            already_defined = old_decl.1.0;
            if already_defined && decl.has_body {
                return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Function {} already defined", decl.name),
                    decl.line,
                ));
            }

            if old_decl.1.1 && !global {
                return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Function {} redeclared with different storage class", decl.name),
                    decl.line,
                ));
            }
            global = old_decl.1.1 || global;
        }

        self.validate_type_specifier(&decl.ty)?;

        let attrs = nodes::TableEntry::FnAttr(already_defined || decl.has_body, global);

        self.symbol_table.insert(ident.clone(), (decl.ty.clone(), attrs));

        let (param_types, ret_type) = match &decl.ty {
            nodes::Type::Fn(args, ret) => (args, ret),
            _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected Fn, got {:?}", decl.ty),
        };

        if let nodes::Type::Struct(ref tag) = **ret_type {
            return Err(errors::Error::new(
                errors::ErrorType::Error,
                format!("Cant send structs as return type right now (tag = {})", tag),
                decl.line,
            ));
        }

        for (idx, param) in decl.params.iter().enumerate() {
            let param_type = &param_types[idx];

            if let nodes::Type::Struct(t) = param_type {
                return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Cant send structs as parameters right now (tag = {})", t),
                    decl.line,
                ));
            }

            if !self.is_complete_type(param_type) {
                return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Incomplete type in function parameter"),
                    decl.line,
                ));
            }
    
            self.validate_type_specifier(param_type)?;

            self.symbol_table.insert(param.clone(), (param_type.clone(), nodes::TableEntry::LocalAttr));
        }

        let body = if decl.has_body {
            let body = match self.typecheck_block(&decl.body, &ret_type, decl.line, used_functions)? {
                nodes::Statement::Compound(body, _) => body,
                _ => unreachable!()
            };

            body

        } else {
            Vec::new()
        };

        Ok(nodes::Declaration::FuncDecl(nodes::FuncDecl {
            name: decl.name.clone(),
            params: decl.params.clone(),
            body,
            storage_class: decl.storage_class.clone(),
            ty: decl.ty.clone(),
            line: decl.line,
            has_body: decl.has_body,
        }, decl.line))
    }

    fn zero_init(&self, ty: &nodes::Type, line: usize) -> Result<nodes::Initializer, errors::Error> {
        Ok(match ty {
            nodes::Type::Int => nodes::Initializer::Single(nodes::Expression {
                expr: nodes::ExpressionEnum::IntegerLiteral(0, line),
                ty: nodes::Type::Int,
                line,
            }, line),
            nodes::Type::Pointer(ty) => nodes::Initializer::Single(nodes::Expression {
                expr: nodes::ExpressionEnum::IntegerLiteral(0, line),
                ty: nodes::Type::Pointer(Box::new(*ty.clone())),
                line,
            }, line),
            nodes::Type::Array(ty, size) => {
                let mut exprs = Vec::with_capacity(*size as usize);
                for _ in 0..*size {
                    exprs.push(self.zero_init(ty, line)?);
                }

                nodes::Initializer::Compound(exprs, line)
            },
            nodes::Type::Struct(tag) => {
                if !self.type_table.contains(tag) {
                    return Err(
                        errors::Error::new(
                            errors::ErrorType::Error,
                            format!("Unknown struct {:?}", tag),
                            line,
                        )
                    );
                }
                let struct_def = self.type_table.lookup(tag).unwrap();

                let mut exprs = Vec::with_capacity(struct_def.1.len());
                for member in &struct_def.1 {
                    exprs.push(self.zero_init(&member.ty, line)?);
                }

                nodes::Initializer::Compound(exprs, line)
            },
            _ => return Err(errors::Error::new(
                errors::ErrorType::Error,
                format!("Cannot zero initialize type {:?}", ty),
                line,
            )),
        })
    }

    fn typecheck_initializer(&self, init: &nodes::Initializer, ty: &nodes::Type, used_functions: &mut HashSet<String>) -> Result<nodes::Initializer, errors::Error> {
        Ok(match init {
            nodes::Initializer::Single(expr, line) => {
                let expr = self.typecheck_and_convert(expr, used_functions)?;
                let expr = self.convert_by_assignment(&expr, ty)?;

                nodes::Initializer::Single(expr, *line)
            },
            nodes::Initializer::Compound(exprs, line) => {
                match ty {
                    nodes::Type::Array(ty, size) => {
                        let mut new_exprs = Vec::with_capacity(exprs.len());

                        let (ty, size) = (*ty.clone(), *size);
                        let mut count = 0;
                        for expr in exprs {
                            new_exprs.push(self.typecheck_initializer(expr, &ty, used_functions)?);
                            count += 1;
                        }

                        if count > size {
                            return Err(errors::Error::new(
                                errors::ErrorType::Error,
                                format!("Too many initializers, expected {}, got {}", size, count),
                                *line,
                            ));
                        } else if count < size {
                            for _ in count..size {
                                new_exprs.push(nodes::Initializer::Single(nodes::Expression {
                                    expr: nodes::ExpressionEnum::IntegerLiteral(0, *line),
                                    ty: ty.clone(),
                                    line: *line,
                                }, *line));
                            }
                        }

                        nodes::Initializer::Compound(new_exprs, *line)
                    },
                    nodes::Type::Struct(tag) => {
                        if !self.type_table.contains(tag) {
                            return Err(errors::Error::new(
                                errors::ErrorType::Error,
                                format!("Unknown struct {:?}", tag),
                                *line,
                            ));
                        }
                        let struct_def = self.type_table.lookup(tag).unwrap();

                        if exprs.len() > struct_def.1.len() {
                            return Err(errors::Error::new(
                                errors::ErrorType::Error,
                                format!("Too many initializers, expected {}, got {}", struct_def.1.len(), exprs.len()),
                                *line,
                            ));
                        }

                        let mut i = 0;
                        let mut typechecked_list = Vec::with_capacity(struct_def.1.len());
                        for init_elem in exprs {
                            let elem_ty = &struct_def.1[i].ty;
                            let init_elem = self.typecheck_initializer(init_elem, elem_ty, used_functions)?;
                            typechecked_list.push(init_elem);
                            i += 1;
                        }
                        while i < struct_def.1.len() {
                            typechecked_list.push(self.zero_init(&struct_def.1[i].ty, *line)?);
                            i += 1;
                        }

                        nodes::Initializer::Compound(typechecked_list, *line)
                    },
                    _ => return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Cannot initialize type {:?} with compound initializer", ty),
                        *line,
                    )),
                }
            },
        })
    }

    fn typecheck_file_scope_variable_declaration(&mut self, decl: &nodes::VarDecl, used_functions: &mut HashSet<String>) -> Result<nodes::VarDecl, errors::Error> {
        let mut initial_value = if decl.expr.is_none() {
            if decl.storage_class == nodes::StorageClass::Extern {
                nodes::InitialValue::NoInit
            } else {
                nodes::InitialValue::Tentative
            }
        // is constant (check if expr is constant)
        } else if let nodes::Initializer::Compound(_, _) = decl.expr.as_ref().unwrap() {
            return Err(errors::Error::new(
                errors::ErrorType::Error,
                format!("Compound initializer in file scope declaration"),
                decl.line,
            ));
        } else if let nodes::Initializer::Single(expr, _) = decl.expr.as_ref().unwrap() {
            let expr = self.typecheck_and_convert(expr, used_functions)?;
            if let nodes::ExpressionEnum::IntegerLiteral(i, _) = expr.expr {
                nodes::InitialValue::Initial(i as i32)
            } else {
                return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Expected integer literal in initializer, got {:?}", expr.expr),
                    decl.line,
                ));
            }
        } else {
            return Err(errors::Error::new(
                errors::ErrorType::Error,
                format!("Expected initializer, got {:?}", decl.expr),
                decl.line,
            ));
        };

        let mut global = decl.storage_class != nodes::StorageClass::Static;

        if self.symbol_table.contains(&decl.name) {
            let old_decl = self.symbol_table.lookup(&decl.name).unwrap();
            let old_decl = (old_decl.0.clone(), match old_decl.1.clone() {
                nodes::TableEntry::StaticAttr(init, global) => (init, global),
                _ => return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Expected StaticAttr, got {:?}", old_decl.1),
                    decl.line,
                )),
            });

            if old_decl.0 != decl.ty {
                return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Incompatible redeclaration of variable {:?}", decl.name),
                    decl.line,
                ));
            }
            if decl.storage_class == nodes::StorageClass::Extern {
                global = old_decl.1.1;
            } else if old_decl.1.1 != global {
                return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Variable {:?} redeclared with different storage class", decl.name),
                    decl.line,
                ));
            }

            if let nodes::InitialValue::Initial(_) = old_decl.1.0 {
                if let nodes::InitialValue::Initial(_) = initial_value {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Variable {:?} redeclared with different initial value", decl.name),
                        decl.line,
                    ));
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

        Ok(nodes::VarDecl {
            name: decl.name.clone(),
            expr: decl.expr.clone(),
            storage_class: decl.storage_class.clone(),
            ty: decl.ty.clone(),
            line: decl.line,
        })
    }

    fn typecheck_statement(&mut self, stmt: &nodes::Statement, ret_type: &nodes::Type, used_functions: &mut HashSet<String>) -> Result<nodes::Statement, errors::Error> {
        Ok(match stmt {
            nodes::Statement::InlineAsm(_, _) |
            nodes::Statement::Break(_, _) | nodes::Statement::Continue(_, _) |
            nodes::Statement::Empty(_) => stmt.clone(),
            nodes::Statement::Return(expr, line) => {
                if expr.is_none() && *ret_type != nodes::Type::Void {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Expected return value for function with return type {:?}, got void", ret_type),
                        *line,
                    ));
                } else if expr.is_some() && *ret_type == nodes::Type::Void {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Expected void return value for function with return type void, got {:?}", expr.as_ref().unwrap().ty),
                        *line,
                    ));
                }

                if expr.is_none() {
                    return Ok(nodes::Statement::Return(None, *line));
                }

                let expr = expr.as_ref().unwrap();

                let ret_expr = self.typecheck_and_convert(expr, used_functions)?;

                let ret_expr = self.convert_by_assignment(&ret_expr, ret_type)?;

                nodes::Statement::Return(Some(ret_expr), *line)
            },
            nodes::Statement::Expression(expr, line) => {
                let expr = self.typecheck_and_convert(expr, used_functions)?;

                nodes::Statement::Expression(expr, *line)
            },
            nodes::Statement::Compound(block, line) => {
                let block = match self.typecheck_block(block, ret_type, *line, used_functions)? {
                    nodes::Statement::Compound(items, _) => items,
                    e => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected Compound, got {:?}", e),
                };

                nodes::Statement::Compound(block, *line)
            },
            nodes::Statement::If(expr, block, else_block, line) => {
                let expr = self.typecheck_and_convert(expr, used_functions)?;
                let block = self.typecheck_statement(block, ret_type, used_functions)?;
                let else_block = if else_block.is_some() {
                    Some(self.typecheck_statement(else_block.as_ref().as_ref().unwrap(), ret_type, used_functions)?) // im sorry
                } else {
                    None
                };

                nodes::Statement::If(expr.clone(), Box::new(block.clone()), Box::new(else_block), *line)
            },
            nodes::Statement::While(expr, block, label, line) => {
                let expr = self.typecheck_and_convert(expr, used_functions)?;
                let block = self.typecheck_statement(block, ret_type, used_functions)?;

                nodes::Statement::While(expr.clone(), Box::new(block.clone()), label.clone(), *line)
            },
            nodes::Statement::DoWhile(block, expr, label, line) => {
                let block = self.typecheck_statement(block, ret_type, used_functions)?;
                let expr = self.typecheck_and_convert(expr, used_functions)?;

                nodes::Statement::DoWhile(Box::new(block), expr.clone(), label.clone(), *line)
            },
            nodes::Statement::For(init, cond, post, block, label, line) => {
                let init = match init {
                    nodes::ForInit::Declaration(decl, line) => nodes::ForInit::Declaration(self.typecheck_variable_declaration(&self.decl_to_var(decl), used_functions)?, *line),
                    nodes::ForInit::Expression(expr, line) => nodes::ForInit::Expression(self.typecheck_and_convert(expr, used_functions)?, *line),
                    nodes::ForInit::Empty(line) => nodes::ForInit::Empty(*line),
                };
                let cond = if let Some(cond_expr) = cond.as_ref() {
                    Some(self.typecheck_and_convert(cond_expr, used_functions)?)
                } else { None };
                let post = if let Some(expr) = post.as_ref() {
                    Some(self.typecheck_and_convert(expr, used_functions)?)
                } else { None };
                let block = self.typecheck_statement(block, ret_type, used_functions)?;

                nodes::Statement::For(init, cond, post, Box::new(block), label.clone(), *line)
            },
        })
    }

    fn is_null_pointer(&self, expr: &nodes::Expression) -> bool {
        match expr.expr {
            nodes::ExpressionEnum::IntegerLiteral(0, _) => true,
            _ => false,
        }
    }

    fn get_common_pointer_type(&self, left: &nodes::Expression, rht: &nodes::Expression) -> Result<nodes::Type, errors::Error> {
        let left_type = &left.ty;
        let right_type = &rht.ty;

        let left_is_ptr = self.is_pointer_type(left_type);
        let right_is_ptr = self.is_pointer_type(right_type);

        if left_type == right_type {
            return Ok(left_type.clone());
        } else if self.is_null_pointer(left) {
            return Ok(right_type.clone());
        } else if self.is_null_pointer(rht) {
            return Ok(left_type.clone());
        } else if (self.is_void_ptr(left_type) || self.is_void_ptr(right_type)) && left_is_ptr && right_is_ptr {
            return Ok(nodes::Type::Pointer(Box::new(nodes::Type::Void)));
        } else if left_is_ptr && right_is_ptr {
            let left_inner = match left_type {
                nodes::Type::Pointer(ty) => ty,
                _ => return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Expected Ptr, got {:?}", left_type),
                    left.line,
                )),
            };
            let right_inner = match right_type {
                nodes::Type::Pointer(ty) => ty,
                _ => return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Expected Ptr, got {:?}", right_type),
                    rht.line,
                )),
            };

            return if left_inner == right_inner {
                Ok(left_type.clone())
            } else {
                Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Incompatible pointer types"),
                    left.line,
                ))
            }
        } else if left_is_ptr {
            return Ok(left_type.clone());
        } else {
            return Err(errors::Error::new(
                errors::ErrorType::Error,
                format!("Incompatible pointer types, {:?} and {:?}", left_type, right_type),
                left.line,
            ));
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

    fn convert_by_assignment(&self, expr: &nodes::Expression, ty: &nodes::Type) -> Result<nodes::Expression, errors::Error> {
        if expr.ty == *ty {
            return Ok(expr.clone());
        }

        if (self.is_arithmetic_type(&expr.ty) && self.is_arithmetic_type(ty)) ||
           (self.is_null_pointer(expr) && self.is_pointer_type(ty)) ||
           (self.is_void_ptr(&expr.ty) && self.is_pointer_type(ty)) ||
           (self.is_void_ptr(ty) && self.is_pointer_type(&expr.ty)) {
            Ok(self.convert_to(expr.clone(), ty))
        } else {
            Err(errors::Error::new(
                errors::ErrorType::Error,
                format!("Incompatible types in cba assignment, expected {:?}, got {:?}", ty, expr.ty),
                expr.line,
            ))
        }
    }

    fn is_arithmetic_type(&self, ty: &nodes::Type) -> bool {
        match ty {
            nodes::Type::Int => true,
            _ => false,
        }
    }

    fn typecheck_expression(&self, expr: &nodes::Expression, used_functions: &mut HashSet<String>) -> Result<nodes::Expression, errors::Error> {
        Ok(match expr.expr {
            nodes::ExpressionEnum::FunctionCall(ref ident, ref args, line) => {
                if !used_functions.contains(ident) { // so we dont do unnecessary clones
                    // TODO! use arc<str> or smth so we don't have to do a shit ton of clones
                    used_functions.insert(ident.clone());
                }

                let ident_type = self.symbol_table.lookup(ident).unwrap();
                
                match ident_type.1 {
                    nodes::TableEntry::FnAttr(_, _) => (),
                    _ => return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Expected FnAttr, got {:?}", ident_type.1),
                        line,
                    )),
                }

                let (fn_type, ret_type) = match ident_type.0 { nodes::Type::Fn(ref args, ref ret_type) => (args, ret_type), _ => return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Expected Fn, got {:?}", ident_type.0),
                    line,
                )) };

                if args.len() != fn_type.len() {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Expected {} arguments, got {}", fn_type.len(), args.len()),
                        line,
                    ));
                }

                let mut new_args = Vec::with_capacity(args.len());
                for (idx, arg) in args.iter().enumerate() {
                    let arg = self.typecheck_and_convert(arg, used_functions)?;
                    new_args.push(self.convert_by_assignment(&arg, &fn_type[idx])?);
                }

                nodes::Expression {
                    expr: nodes::ExpressionEnum::FunctionCall(ident.clone(), new_args.clone(), line),
                    ty: *ret_type.clone(),
                    line,
                }
            },
            nodes::ExpressionEnum::Var(ref ident, line) => {
                if !self.symbol_table.contains(&ident) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Variable {:?} not declared before use", ident),
                        line,
                    ));
                }
                let ident_type = self.symbol_table.lookup(&ident).unwrap();

                let is_fn = match ident_type.0 { nodes::Type::Fn(_, _) => true, _ => false };
                if is_fn {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Function {:?} used as variable", ident),
                        line,
                    ));
                }

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Var(ident.clone(), line),
                    ty: ident_type.0.clone(),
                    line,
                }
            },
            nodes::ExpressionEnum::Assign(ref lhs, ref rhs, line) => {
                let lft = self.typecheck_and_convert(&*lhs, used_functions)?;
                let rht = self.typecheck_and_convert(&*rhs, used_functions)?;

                if lft.ty != rht.ty {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Incompatible types in assignment, {:?} and {:?}", lft.ty, rht.ty),
                        line,
                    ));
                }

                if !self.is_lvalue(&lft) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Assignment to non-lvalue"),
                        line,
                    ));
                }

                let ty = lft.ty.clone();

                let right = self.convert_by_assignment(&rht, &ty)?;

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Assign(Box::new(lft), Box::new(right), line),
                    ty,
                    line,
                }
            },
            nodes::ExpressionEnum::OpAssign(op, ref lhs, ref rhs, line) => {
                let lft = self.typecheck_and_convert(&*lhs, used_functions)?;
                let rht = self.typecheck_and_convert(&*rhs, used_functions)?;

                if lft.ty != rht.ty {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Incompatible types in assignment, {:?} and {:?}", lft.ty, rht.ty),
                        line,
                    ));
                }

                if !self.is_lvalue(&lft) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Assignment to non-lvalue"),
                        line,
                    ));
                }

                let ty = lft.ty.clone();

                let right = self.convert_by_assignment(&rht, &ty)?;

                nodes::Expression {
                    expr: nodes::ExpressionEnum::OpAssign(op, Box::new(lft), Box::new(right), line),
                    ty,
                    line,
                }
            },
            nodes::ExpressionEnum::Conditional(ref cond, ref then, ref els, line) => {
                let cond = self.typecheck_and_convert(&*cond, used_functions)?;
                let then = self.typecheck_and_convert(&*then, used_functions)?;
                let els = self.typecheck_and_convert(&*els, used_functions)?;

                if !self.is_scalar_type(&cond.ty) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Condition must be scalar, got {:?}", cond.ty),
                        line,
                    ));
                }

                
                let common_type = if then.ty == nodes::Type::Void && els.ty == nodes::Type::Void {
                    nodes::Type::Void
                } else if self.is_pointer_type(&then.ty) || self.is_pointer_type(&els.ty) {
                    self.get_common_pointer_type(&then, &els)?
                } else if self.is_arithmetic_type(&then.ty) && self.is_arithmetic_type(&els.ty) {
                    self.get_common_type(&then.ty, &els.ty)?
                } else {
                    let l_tag = match then.ty {
                        nodes::Type::Struct(ref tag) => tag.clone(),
                        _ => return Err(errors::Error::new(
                            errors::ErrorType::Error,
                            format!("Invalid types in conditional, {:?} and {:?}", then.ty, els.ty),
                            line,
                        )),
                    };
                    let r_tag = match els.ty {
                        nodes::Type::Struct(ref tag) => tag.clone(),
                        _ => return Err(errors::Error::new(
                            errors::ErrorType::Error,
                            format!("Invalid types in conditional, {:?} and {:?}", then.ty, els.ty),
                            line,
                        )),
                    };

                    if l_tag != r_tag {
                        return Err(errors::Error::new(
                            errors::ErrorType::Error,
                            format!("Incompatible types in conditional, {:?} and {:?}", l_tag, r_tag),
                            line,
                        ));
                    }

                    then.ty.clone()
                };

                let converted_left = then;
                let converted_right = els;

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Conditional(Box::new(cond), Box::new(converted_left), Box::new(converted_right), line),
                    ty: common_type,
                    line,
                }
            },
            nodes::ExpressionEnum::Increment(ref inner_expr, line) => {
                let inner_expr = self.typecheck_and_convert(&*inner_expr, used_functions)?;

                let ty = inner_expr.ty.clone();

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Increment(Box::new(inner_expr), line),
                    ty,
                    line,
                }
            },
            nodes::ExpressionEnum::Decrement(ref inner_expr, line) => {
                let inner_expr = self.typecheck_and_convert(&*inner_expr, used_functions)?;

                let ty = inner_expr.ty.clone();

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Decrement(Box::new(inner_expr), line),
                    ty,
                    line,
                }
            },
            nodes::ExpressionEnum::Unop(unop, ref expr, line) => {
                let expr = self.typecheck_and_convert(&*expr, used_functions)?;

                if !self.is_scalar_type(&expr.ty) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Expected scalar type in Unop, got {:?}", expr.ty),
                        line,
                    ));
                }

                if self.is_pointer_type(&expr.ty) {
                    match unop {
                        nodes::Unop::BitwiseNot => return Err(errors::Error::new(
                            errors::ErrorType::Error,
                            format!("Bitwise not on pointer"),
                            line,
                        )),
                        nodes::Unop::Negate => return Err(errors::Error::new(
                            errors::ErrorType::Error,
                            format!("Negation on pointer"),
                            line,
                        )),
                        nodes::Unop::LogicalNot => (),
                    }
                }

                match unop {
                    nodes::Unop::BitwiseNot => {
                        let ty = expr.ty.clone();

                        nodes::Expression {
                            expr: nodes::ExpressionEnum::Unop(unop, Box::new(expr), line),
                            ty,
                            line,
                        }
                    }
                    nodes::Unop::Negate => {
                        let expr = if expr.ty == nodes::Type::Char {
                            self.convert_to(expr, &nodes::Type::Int)
                        } else {
                            expr
                        };

                        if expr.ty != nodes::Type::Int {
                            return Err(errors::Error::new(
                                errors::ErrorType::Error,
                                format!("Negation of non-integer type"),
                                line,
                            ));
                        }

                        let ty = expr.ty.clone();

                        nodes::Expression {
                            expr: nodes::ExpressionEnum::Unop(unop, Box::new(expr), line),
                            ty,
                            line,
                        }
                    }
                    nodes::Unop::LogicalNot => {
                        nodes::Expression {
                            expr: nodes::ExpressionEnum::Unop(unop, Box::new(expr), line),
                            ty: nodes::Type::Int,
                            line,
                        }
                    }
                }
            },
            nodes::ExpressionEnum::Binop(binop, ref lhs,  ref rhs, line) => {
                let left = self.typecheck_and_convert(&*lhs, used_functions)?;
                let right = self.typecheck_and_convert(&*rhs, used_functions)?;

                match binop {
                    nodes::Binop::And | nodes::Binop::Or => {
                        return Ok(nodes::Expression {
                            expr: nodes::ExpressionEnum::Binop(binop, Box::new(left), Box::new(right), line),
                            ty: nodes::Type::Int,
                            line,
                        });
                    },
                    nodes::Binop::Equal | nodes::Binop::NotEqual => {
                        #[allow(unused_variables)]
                        let common_type = if self.is_pointer_type(&left.ty) || self.is_pointer_type(&right.ty) {
                            self.get_common_pointer_type(&left, &right)?
                        } else if self.is_arithmetic_type(&left.ty) && self.is_arithmetic_type(&right.ty) {
                            self.get_common_type(&left.ty, &right.ty)?
                        } else if left.ty == right.ty {
                            left.ty.clone()
                        } else {
                            return Err(errors::Error::new(
                                errors::ErrorType::Error,
                                format!("Invalid types in comparison, {:?} and {:?}", left.ty, right.ty),
                                line,
                            ));
                        };

                        let converted_left = left;
                        let converted_right = right;

                        return Ok(nodes::Expression {
                            expr: nodes::ExpressionEnum::Binop(binop, Box::new(converted_left), Box::new(converted_right), line),
                            ty: nodes::Type::Int,
                            line,
                        });
                    },
                    _ => (),
                }

                let common_type = self.get_common_type(&left.ty, &right.ty)?;

                let converted_left = self.convert_to(left, &common_type);
                let converted_right = self.convert_to(right, &common_type);

                match binop {
                    nodes::Binop::Add | nodes::Binop::Subtract => 
                        nodes::Expression {
                            expr: nodes::ExpressionEnum::Binop(binop, Box::new(converted_left), Box::new(converted_right), line),
                            ty: common_type,
                            line,
                        },
                    _ =>
                        nodes::Expression {
                            expr: nodes::ExpressionEnum::Binop(binop, Box::new(converted_left), Box::new(converted_right), line),
                            ty: nodes::Type::Int,
                            line,
                        },
                }
            },
            nodes::ExpressionEnum::Dereference(ref expr, line) => {
                let expr = self.typecheck_and_convert(&*expr, used_functions)?;
                let ty = match expr.ty { nodes::Type::Pointer(ref ty) => *ty.clone(), _ => return Err(errors::Error::new(
                    errors::ErrorType::Error,
                    format!("Expected Ptr, got {:?}", expr.ty),
                    line,
                )) };

                if ty == nodes::Type::Void {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Dereference of void pointer"),
                        line,
                    ));
                }

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Dereference(Box::new(expr), line),
                    ty,
                    line,
                }
            },
            nodes::ExpressionEnum::AddressOf(ref expr, line) => {
                if !self.is_lvalue(&*expr) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Cannot get address of non-lvalue"),
                        line,
                    ));
                }

                let expr = self.typecheck_and_convert(&*expr, used_functions)?;

                let ty = nodes::Type::Pointer(Box::new(expr.ty.clone()));
                nodes::Expression {
                    expr: nodes::ExpressionEnum::AddressOf(Box::new(expr), line),
                    ty,
                    line,
                }
            },
            nodes::ExpressionEnum::Subscript(ref left, ref right, line) => {
                let left = self.typecheck_and_convert(&*left, used_functions)?;
                let right = self.typecheck_and_convert(&*right, used_functions)?;

                // check if both are pointer, xor
                let lft_ptr = self.is_pointer_type(&left.ty);
                let rht_ptr = self.is_pointer_type(&right.ty);
                let one_pointer = lft_ptr ^ rht_ptr;
                let one_int = left.ty == nodes::Type::Int || right.ty == nodes::Type::Int;

                if !(one_pointer && one_int) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Invalid types in subscript, expected Pointer and Int, got {:?} and {:?}", left.ty, right.ty),
                        line,
                    ));
                }

                let ty = if lft_ptr {
                    match left.ty {
                        nodes::Type::Pointer(ref ty) => *ty.clone(),
                        _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected Ptr, got {:?}", left.ty),
                    }
                } else {
                    match right.ty {
                        nodes::Type::Pointer(ref ty) => *ty.clone(),
                        _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected Ptr, got {:?}", right.ty),
                    }
                };

                let out = nodes::Expression {
                    expr: nodes::ExpressionEnum::Subscript(Box::new(left), Box::new(right), line),
                    ty,
                    line,
                };

                out
            },
            nodes::ExpressionEnum::Cast(ref ty, ref expr, line) => {
                let expr = self.typecheck_and_convert(&*expr, used_functions)?;

                if !self.is_scalar_type(ty) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Cannot cast non-scalar type {:?}", ty),
                        line,
                    ));
                } else if !self.is_scalar_type(&expr.ty) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Cannot cast non-scalar type {:?}", expr.ty),
                        line,
                    ));
                }

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Cast(ty.clone(), Box::new(expr), line),
                    ty: ty.clone(),
                    line,
                }
            },
            nodes::ExpressionEnum::SizeOfType(ref ty, line) => {
                self.validate_type_specifier(ty)?;
                if !self.is_complete_type(ty) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Cannot get size of incomplete type {:?}", ty),
                        line,
                    ));
                }
                nodes::Expression {
                    expr: nodes::ExpressionEnum::SizeOfType(ty.clone(), line),
                    ty: nodes::Type::Int,
                    line,
                }
            },
            nodes::ExpressionEnum::SizeOf(ref expr, line) => {
                let expr = self.typecheck_and_convert(&*expr, used_functions)?;
                if !self.is_complete_type(&expr.ty) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Cannot get size of incomplete type {:?}", expr.ty),
                        line,
                    ));
                }
                nodes::Expression {
                    expr: nodes::ExpressionEnum::SizeOf(Box::new(expr), line),
                    ty: nodes::Type::Int,
                    line,
                }
            },
            nodes::ExpressionEnum::Dot(ref expr, ref field, line) => {
                let expr = self.typecheck_and_convert(&*expr, used_functions)?;

                let ty = match expr.ty {
                    nodes::Type::Struct(ref tag) => {
                        if !self.type_table.contains(&tag) {
                            return Err(errors::Error::new(
                                errors::ErrorType::Error,
                                format!("Unknown struct {:?}", tag),
                                line,
                            ));
                        }
                        let struct_def = self.type_table.lookup(&tag).unwrap();
                        // its a vec of MemberEntrys (String, Type, Offset). we want to find it by the string, and return the type
                        let field_ty = struct_def.1.iter().find(|memb_entry| memb_entry.name == *field).unwrap().ty.clone();
                        field_ty.clone()
                    },
                    _ => return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Expected Struct, got {:?}", expr.ty),
                        line,
                    )),
                };

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Dot(Box::new(expr), field.clone(), line),
                    ty,
                    line,
                }
            },
            nodes::ExpressionEnum::Arrow(ref expr, ref field, line) => {
                let expr = self.typecheck_and_convert(&*expr, used_functions)?;

                let ty = match expr.ty {
                    nodes::Type::Pointer(ref ty) => {
                        match &**ty {
                            nodes::Type::Struct(tag) => {
                                if !self.type_table.contains(&tag) {
                                    return Err(errors::Error::new(
                                        errors::ErrorType::Error,
                                        format!("Unknown struct {:?}", tag),
                                        line,
                                    ));
                                }
                                let struct_def = self.type_table.lookup(&tag).unwrap();
                                let field_ty = struct_def.1.iter().find(|memb_entry| memb_entry.name == *field).unwrap().ty.clone();
                                field_ty.clone()
                            },
                            _ => return Err(errors::Error::new(
                                errors::ErrorType::Error,
                                format!("Expected Struct, got {:?}", ty),
                                line,
                            )),
                        }
                    },
                    _ => return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Cannot use arrow on non-pointer type {:?}", expr.ty),
                        line,
                    )),
                };

                nodes::Expression {
                    expr: nodes::ExpressionEnum::Arrow(Box::new(expr), field.clone(), line),
                    ty,
                    line,
                }
            },
            nodes::ExpressionEnum::CharLiteral(ch, line) => nodes::Expression {
                expr: nodes::ExpressionEnum::CharLiteral(ch, line),
                ty: nodes::Type::Char,
                line,
            },
            nodes::ExpressionEnum::StringLiteral(ref s, line) => nodes::Expression {
                expr: nodes::ExpressionEnum::StringLiteral(s.clone(), line),
                ty: nodes::Type::Pointer(Box::new(nodes::Type::Char)),
                line,
            },
            nodes::ExpressionEnum::IntegerLiteral(_, _) => expr.clone(),
        })
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

    fn validate_type_specifier(&self, ty: &nodes::Type) -> Result<(), errors::Error> {
        Ok(match ty {
            nodes::Type::Array(ty, _) => {
                if !self.is_complete_type(ty) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Incomplete type in array declaration"),
                        0,
                    ));
                }
                self.validate_type_specifier(ty)?;
            },
            nodes::Type::Pointer(ty) => {
                self.validate_type_specifier(ty)?;
            },
            nodes::Type::Fn(args, ret) => {
                for arg in args {
                    self.validate_type_specifier(arg)?;
                }
                self.validate_type_specifier(ret)?;
            },
            _ => (),
        })
    }

    fn typecheck_and_convert(&self, expr: &nodes::Expression, used_functions: &mut HashSet<String>) -> Result<nodes::Expression, errors::Error> {
        let expr = self.typecheck_expression(expr, used_functions)?;

        Ok(match &expr.ty {
            nodes::Type::Array(ty, _) => {
                nodes::Expression {
                    expr: nodes::ExpressionEnum::AddressOf(Box::new(expr.clone()), expr.line),
                    ty: nodes::Type::Pointer(Box::new(*ty.clone())),
                    line: expr.line,
                }
            },
            nodes::Type::Struct(tag) => {
                if !self.type_table.contains(tag) {
                    return Err(errors::Error::new(
                        errors::ErrorType::Error,
                        format!("Unknown struct {:?}", tag),
                        expr.line,
                    ));
                }
                expr
            },
            _ => expr,
        })
    }

    fn is_lvalue(&self, expr: &nodes::Expression) -> bool {
        match expr.expr {
            nodes::ExpressionEnum::Var(_, _) => true,
            nodes::ExpressionEnum::Dereference(_, _) => true,
            nodes::ExpressionEnum::Subscript(_, _, _) => true,
            nodes::ExpressionEnum::Dot(_, _, _) => true,
            nodes::ExpressionEnum::Arrow(_, _, _) => true,
            _ => false,
        }
    }

    fn convert_to(&self, expr: nodes::Expression, ty: &nodes::Type) -> nodes::Expression {
        if expr.ty == *ty {
            return expr.clone();
        }

        let line = expr.line;

        return nodes::Expression {
            expr: nodes::ExpressionEnum::Cast(ty.clone(), Box::new(expr), line),
            ty: ty.clone(),
            line,
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

    fn get_common_type(&self, lhs: &nodes::Type, rhs: &nodes::Type) -> Result<nodes::Type, errors::Error> {
        if lhs == rhs {
            return Ok(lhs.clone());
        }

        if self.is_pointer_type(lhs) || self.is_pointer_type(rhs) {
            let inner_type = if self.is_pointer_type(lhs) {
                match lhs {
                    nodes::Type::Pointer(ty) => *ty.clone(),
                    _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected Ptr, got {:?}", lhs),
                }
            } else {
                match rhs {
                    nodes::Type::Pointer(ty) => *ty.clone(),
                    _ => unreachable!("Expected Ptr, got {:?}", rhs),
                }
            };
            return Ok(nodes::Type::Pointer(Box::new(inner_type)));
        }

        return Ok(nodes::Type::Int);
    }

    fn typecheck_block(&mut self, body: &Vec<BlockItem>, ret_type: &nodes::Type, line: usize, used_functions: &mut HashSet<String>) -> Result<nodes::Statement, errors::Error> {
        let mut block_items = Vec::with_capacity(body.len());
        for b in body {
            block_items.push(self.resolve_block_item(b, ret_type, used_functions)?);
        }

        Ok(nodes::Statement::Compound(block_items, line))
    }

    fn resolve_block_item(&mut self, stmt: &BlockItem, ret_type: &nodes::Type, used_functions: &mut HashSet<String>) -> Result<BlockItem, errors::Error> {
        Ok(match stmt {
            BlockItem::Declaration(decl, _) => {
                BlockItem::Declaration(match decl {
                    nodes::Declaration::Empty(line) => nodes::Declaration::Empty(*line),
                    nodes::Declaration::FuncDecl(fn_decl, line) => {
                        self.typecheck_function_declaration(fn_decl, used_functions)?;
                        nodes::Declaration::Empty(*line)
                    },
                    nodes::Declaration::VarDecl(var_decl, _) => self.typecheck_variable_declaration(var_decl, used_functions)?,
                    nodes::Declaration::StructDecl(struct_decl, _) => self.typecheck_struct_declaration(struct_decl)?,
                }, stmt.line())
            },
            BlockItem::Statement(stmt, line) => BlockItem::Statement(self.typecheck_statement(stmt, ret_type, used_functions)?, *line),
        })
    }

    fn decl_to_var(&self, decl: &nodes::Declaration) -> nodes::VarDecl {
        match decl {
            nodes::Declaration::VarDecl(var_decl, _) => var_decl.clone(),
            _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected VarDecl, got {:?}", decl),
        }
    }

    fn typecheck_variable_declaration(&mut self, decl: &nodes::VarDecl, used_functions: &mut HashSet<String>) -> Result<nodes::Declaration, errors::Error> {
        self.symbol_table.insert(decl.name.clone(), (decl.ty.clone(), nodes::TableEntry::LocalAttr));
        let expr = if decl.expr.is_some() {
            let expr = self.typecheck_initializer(decl.expr.as_ref().unwrap(), &decl.ty, used_functions)?;

            Some(expr)
        } else { None };

        if decl.ty == nodes::Type::Void {
            return Err(errors::Error::new(
                errors::ErrorType::Error,
                format!("Cannot declare variable of type void"),
                decl.line,
            ));
        }

        self.validate_type_specifier(&decl.ty)?;

        Ok(nodes::Declaration::VarDecl(nodes::VarDecl {
            name: decl.name.clone(),
            expr,
            storage_class: decl.storage_class.clone(),
            ty: decl.ty.clone(),
            line: decl.line,
        }, decl.line))
    }

    fn typecheck_struct_declaration(&mut self, decl: &nodes::StructDecl) -> Result<nodes::Declaration, errors::Error> {
        if decl.members.len() == 0 {
            return Ok(nodes::Declaration::StructDecl(decl.clone(), decl.line));
        }
        self.validate_struct_declaration(decl)?;

        let mut member_entries = Vec::new();
        let mut struct_size = 0;

        for member in &decl.members {
            let member_type = member.ty.clone();
            let member_size = self.get_size(&member_type, decl.line)?;

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

        Ok(nodes::Declaration::StructDecl(decl.clone(), decl.line))
    }

    fn get_size(&self, ty: &nodes::Type, line: usize) -> Result<i32, errors::Error> {
        Ok(match ty {
            nodes::Type::Int | nodes::Type::Pointer(_) |
            nodes::Type::Char => 1,
            nodes::Type::Array(ty, size) => self.get_size(ty, line)? * *size as i32,
            nodes::Type::Struct(decl) => {
                self.type_table.lookup(decl).unwrap().0
            },
            nodes::Type::Fn(_, _) => return Err(errors::Error::new(
                errors::ErrorType::Error,
                format!("Cannot get size of function type"),
                line,
            )),
            nodes::Type::Void => return Err(errors::Error::new(
                errors::ErrorType::Error,
                format!("Cannot get size of void type"),
                line,
            )),
        })
    }

    fn validate_struct_declaration(&self, decl: &nodes::StructDecl) -> Result<(), errors::Error> {
        for member in &decl.members {
            self.validate_type_specifier(&member.ty)?;
        }

        Ok(())
    }
}