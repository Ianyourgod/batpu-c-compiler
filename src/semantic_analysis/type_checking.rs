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
                nodes::Declaration::VarDecl(var_decl) => self.typecheck_file_scope_variable_declaration(&var_decl),
                nodes::Declaration::FuncDecl(func) => self.typecheck_function_declaration(&func)
            };
        }

        self.symbol_table.clone()
    }

    fn typecheck_function_declaration(&mut self, decl: &nodes::FuncDecl) {
        let fn_type = nodes::Type::Fn(decl.params.len() as i32);
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

            if old_decl.0 != fn_type {
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

        self.symbol_table.insert(ident, (fn_type, attrs));

        if has_body {
            for param in &decl.params {
                self.symbol_table.insert(param.clone(), (nodes::Type::Int, nodes::TableEntry::LocalAttr));
            }
            self.typecheck_block(&decl.body);
        }
    }

    fn typecheck_file_scope_variable_declaration(&mut self, decl: &nodes::VarDecl) {
        let mut initial_value = if decl.expr.is_none() {
            if decl.storage_class == nodes::StorageClass::Extern {
                nodes::InitialValue::NoInit
            } else {
                nodes::InitialValue::Tentative
            }
        // is constant (check if expr is constant)
        } else if let nodes::Expression::IntegerLiteral(i) = decl.expr.as_ref().unwrap() {
            nodes::InitialValue::Initial(*i as i32)
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
    }

    fn typecheck_statement(&mut self, stmt: &nodes::Statement) {
        match stmt {
            nodes::Statement::Break(_) | nodes::Statement::Continue(_) |
            nodes::Statement::Empty => (),
            nodes::Statement::Return(expr) | nodes::Statement::Expression(expr) => self.typecheck_expression(expr),
            nodes::Statement::Compound(block) => self.typecheck_block(block),
            nodes::Statement::If(expr, block, else_block) => {
                self.typecheck_expression(expr);
                self.typecheck_statement(block);
                if else_block.is_some() {
                    self.typecheck_statement(else_block.as_ref().as_ref().unwrap()); // im sorry
                }
            },
            nodes::Statement::While(expr, block, _) => {
                self.typecheck_expression(expr);
                self.typecheck_statement(block);
            },
            nodes::Statement::DoWhile(block, expr, _) => {
                self.typecheck_statement(block);
                self.typecheck_expression(expr);
            },
            nodes::Statement::For(init, cond, post, block, _) => {
                match init {
                    nodes::ForInit::Declaration(decl) => self.typecheck_variable_declaration(&self.decl_to_var(decl)),
                    nodes::ForInit::Expression(expr) => self.typecheck_expression(expr),
                    nodes::ForInit::Empty => (),
                }
                if let Some(cond_expr) = cond.as_ref() {
                    self.typecheck_expression(cond_expr);
                }
                if let Some(expr) = post.as_ref() {
                    self.typecheck_expression(expr);
                }
                self.typecheck_statement(block);
            },
        }
    }

    fn typecheck_expression(&mut self, expr: &nodes::Expression) {
        match expr {
            nodes::Expression::FunctionCall(ident, args) => {
                let ident_type = self.symbol_table.lookup(&nodes::Identifier { name: ident.clone() }).unwrap();
                if ident_type.0 == nodes::Type::Int {
                    panic!("Variable {} is not a function", ident);
                }
                let fn_type = match ident_type.0 { nodes::Type::Fn(t) => t, _ => panic!("Expected Fn, got {:?}", ident_type.0) };
                if fn_type != args.len() as i32 {
                    panic!("Function {} expects {} arguments, got {}", ident, fn_type, args.len());
                }
                for arg in args {
                    self.typecheck_expression(arg);
                }
            },
            nodes::Expression::Var(ident) => {
                if !self.symbol_table.contains(ident) {
                    panic!("Variable {:?} not declared", ident);
                }
                let ident_type = self.symbol_table.lookup(ident).unwrap();
                let is_fn = match ident_type.0 { nodes::Type::Fn(_) => true, _ => false };
                if is_fn {
                    panic!("Function name {:?} used as variable", ident);
                }
            },
            nodes::Expression::Assign(lhs, rhs) => {
                self.typecheck_expression(lhs);
                self.typecheck_expression(rhs);
            },
            nodes::Expression::Conditional(cond, then, els) => {
                self.typecheck_expression(cond);
                self.typecheck_expression(then);
                self.typecheck_expression(els);
            },
            nodes::Expression::Increment(expr) | nodes::Expression::Decrement(expr) => {
                self.typecheck_expression(expr);
            },
            nodes::Expression::Unop(_, expr) => {
                self.typecheck_expression(expr);
            },
            nodes::Expression::Binop(_, lhs, rhs) => {
                self.typecheck_expression(lhs);
                self.typecheck_expression(rhs);
            },
            nodes::Expression::IntegerLiteral(_) => (),
        }
    }

    fn typecheck_block(&mut self, body: &Vec<BlockItem>) {
        for block_item in body {
            self.resolve_block_item(block_item);
        }
    }

    fn resolve_block_item(&mut self, stmt: &BlockItem) {
        match stmt {
            BlockItem::Declaration(decl) => {
                match decl {
                    nodes::Declaration::FuncDecl(fn_decl) => self.typecheck_function_declaration(fn_decl),
                    nodes::Declaration::VarDecl(var_decl) => self.typecheck_variable_declaration(var_decl)
                }
            },
            BlockItem::Statement(stmt) => self.typecheck_statement(stmt),
        }
    }

    fn decl_to_var(&self, decl: &nodes::Declaration) -> nodes::VarDecl {
        match decl {
            nodes::Declaration::VarDecl(var_decl) => var_decl.clone(),
            _ => panic!("Expected VarDecl, got {:?}", decl),
        }
    }

    fn typecheck_variable_declaration(&mut self, decl: &nodes::VarDecl) {
        self.symbol_table.insert(decl.name.clone(), (nodes::Type::Int, nodes::TableEntry::LocalAttr));
        if decl.expr.is_some() {
            self.typecheck_expression(decl.expr.as_ref().unwrap());
        }
    }
}