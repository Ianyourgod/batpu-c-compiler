pub mod definition;

use crate::parser::nodes;

pub struct Tacky {
    program: nodes::Program,
    temp_count: i32,
    symbol_table: nodes::SymbolTable,
}

impl Tacky {
    pub fn new(program: nodes::Program, symbol_table: nodes::SymbolTable) -> Tacky {
        Tacky {
            program,
            temp_count: 0,
            symbol_table,
        }
    }

    pub fn emit(&mut self) -> definition::Program {
        let mut statements: Vec<definition::TopLevel> = Vec::new();

        for decl in self.program.statements.clone() {
            match decl {
                nodes::Declaration::VarDecl(var) => {
                    let name = &var.name;

                    let table_entry = self.symbol_table.lookup(name).unwrap();

                    let (global, init) = match table_entry.1 {
                        nodes::TableEntry::StaticAttr(ref init, global) => (global, init),
                        _ => panic!("Expected StaticAttr, got {:?}", table_entry.1),
                    };

                    let val = match init {
                        nodes::InitialValue::NoInit => continue,
                        nodes::InitialValue::Initial(val) => *val,
                        nodes::InitialValue::Tentative => 0,
                    };

                    statements.push(definition::TopLevel::StaticVariable(
                        var.name,
                        global,
                        var.ty.clone(),
                        val,
                    ));
                }
                nodes::Declaration::FuncDecl(func) => {
                    if func.body.len() == 0 {
                        continue;
                    }

                    let mut body: Vec<definition::Instruction> = Vec::new();
                    let mut params: Vec<(String, definition::Type)> = Vec::with_capacity(func.params.len());
                    
                    for param in &func.params {
                        params.push((param.clone(), self.symbol_table.lookup(&param).unwrap().0.clone()));
                    }
                    
                    for instr in &func.body {
                        self.emit_block_item(instr, &mut body);
                    }

                    let table_entry = self.symbol_table.lookup(&func.name).unwrap();

                    let global = match table_entry.1 {
                        nodes::TableEntry::FnAttr(_, global) => global,
                        _ => panic!("Expected FnAttr, got {:?}", table_entry.1),
                    };

                    let func = definition::FuncDef {
                        name: func.name.clone(),
                        params,
                        body,
                        global,
                    };

                    statements.push(definition::TopLevel::FuncDef(func));
                }
            }
        }

        definition::Program {
            statements,
        }
    }

    fn emit_block_item(&mut self, stmt: &nodes::BlockItem, body: &mut Vec<definition::Instruction>) {
        match stmt {
            nodes::BlockItem::Statement(ref inner_stmt) => {
                self.emit_statement(inner_stmt, body);
            }
            nodes::BlockItem::Declaration(ref decl) => {
                if let nodes::Declaration::VarDecl(ref decl) = decl {
                    // we only care about variable declarations

                    if decl.expr.is_some() {
                        // this is just assignment
                        let expr = decl.expr.as_ref().unwrap();
                        
                        let val = self.emit_tacky_and_convert(&expr.expr, body, &expr.ty);
                        let name = decl.name.clone();

                        body.push(definition::Instruction::Copy(definition::Val::Var(name, decl.ty.clone()), val));
                    }
                    // we don't need to do anything for declarations without an expression
                }
                // TODO: handle function declarations
            }
        }
    }

    fn emit_statement(&mut self, stmt: &nodes::Statement, body: &mut Vec<definition::Instruction>) {
        match stmt {
            nodes::Statement::Return(ref expr) => {
                let val = self.emit_tacky_and_convert(&expr.expr, body, &expr.ty);
                body.push(definition::Instruction::Return(val));
            }
            nodes::Statement::Expression(ref expr) => {
                self.emit_tacky_and_convert(&expr.expr, body, &expr.ty);
            }
            nodes::Statement::If(cond, then, else_) => {
                let end_label = self.make_temporary();
                let val = self.emit_tacky_and_convert(&cond.expr, body, &cond.ty);
                body.push(definition::Instruction::JumpIfZero(val, end_label.clone()));
                self.emit_statement(then, body);
                if else_.is_some() {
                    let actual_end_label = self.make_temporary();
                    body.push(definition::Instruction::Jump(actual_end_label.clone()));
                    body.push(definition::Instruction::Label(end_label));
                    self.emit_statement(&(else_.clone().unwrap()), body);
                    body.push(definition::Instruction::Label(actual_end_label));
                    return;
                }
                body.push(definition::Instruction::Label(end_label));
            }
            nodes::Statement::Compound(stmts) => {
                for stmt in stmts {
                    self.emit_block_item(stmt, body);
                }
            }
            nodes::Statement::Break(label) => {
                body.push(definition::Instruction::Label(format!("{}.break", label)))
            }
            nodes::Statement::Continue(label) => {
                body.push(definition::Instruction::Label(format!("{}.continue", label)))
            }
            nodes::Statement::While(cond, loop_body, label) => {
                let continue_label = format!("{}.continue", label);
                let break_label = format!("{}.break", label);
                body.push(definition::Instruction::Label(continue_label.clone()));
                let val = self.emit_tacky_and_convert(&cond.expr, body, &cond.ty);
                body.push(definition::Instruction::JumpIfZero(val, break_label.clone()));
                self.emit_statement(loop_body, body);
                body.push(definition::Instruction::Jump(continue_label.clone()));
                body.push(definition::Instruction::Label(break_label.clone()));
            }
            nodes::Statement::DoWhile(loop_body, cond, label) => {
                let continue_label = format!("{}.continue", label);
                let break_label = format!("{}.break", label);
                body.push(definition::Instruction::Label(continue_label.clone()));
                self.emit_statement(loop_body, body);
                let val = self.emit_tacky_and_convert(&cond.expr, body, &cond.ty);
                body.push(definition::Instruction::JumpIfNotZero(val, continue_label.clone()));
                body.push(definition::Instruction::Label(break_label.clone()));
            }
            nodes::Statement::For(init, cond, post, loop_body, label) => {
                let continue_label = format!("{}.continue", label);
                let break_label = format!("{}.break", label);
                match init {
                    nodes::ForInit::Declaration(ref decl) => {
                        let decl = match decl {
                            nodes::Declaration::VarDecl(ref decl) => decl,
                            _ => panic!("Invalid for loop initializer"),
                        };

                        if decl.expr.is_some() {
                            let expr = decl.expr.as_ref().unwrap();
                            let val = self.emit_tacky_and_convert(&expr.expr, body, &expr.ty);
                            let name = decl.name.clone();
                            body.push(definition::Instruction::Copy(definition::Val::Var(name, decl.ty.clone()), val));
                        }
                    }
                    nodes::ForInit::Expression(ref expr) => {
                        self.emit_tacky_and_convert(&expr.expr, body, &expr.ty);
                    }
                    nodes::ForInit::Empty => {}
                }
                body.push(definition::Instruction::Label(continue_label.clone()));
                if cond.is_some() {
                    let cond = cond.as_ref().unwrap();
                    let val = self.emit_tacky_and_convert(&cond.expr, body, &cond.ty);
                    body.push(definition::Instruction::JumpIfZero(val, break_label.clone()));
                }
                self.emit_statement(loop_body, body);
                if post.is_some() {
                    let post = post.as_ref().unwrap();
                    self.emit_tacky_and_convert(&post.expr, body, &post.ty);
                }
                body.push(definition::Instruction::Jump(continue_label.clone()));
                body.push(definition::Instruction::Label(break_label.clone()));
            }
            nodes::Statement::Empty => {}
        }
    }

    fn make_temporary(&mut self) -> String {
        let name = format!("tmp.{}", self.temp_count);
        self.temp_count += 1;
        name
    }

    fn convert_binop(&self, op: &nodes::Binop) -> definition::Binop {
        match op {
            nodes::Binop::Add => definition::Binop::Add,
            nodes::Binop::Subtract => definition::Binop::Subtract,
            nodes::Binop::And => definition::Binop::And,
            nodes::Binop::Or => definition::Binop::Or,
            nodes::Binop::Equal => definition::Binop::Equal,
            nodes::Binop::NotEqual => definition::Binop::NotEqual,
            nodes::Binop::LessThan => definition::Binop::LessThan,
            nodes::Binop::GreaterThan => definition::Binop::GreaterThan,
            nodes::Binop::LessThanEqual => definition::Binop::LessThanEqual,
            nodes::Binop::GreaterThanEqual => definition::Binop::GreaterThanEqual,
        }
    }

    fn is_short_circuiting(&self, op: &nodes::Binop) -> (bool, bool) {
        match op {
            nodes::Binop::And => (true, false),
            nodes::Binop::Or => (true, true),
            _ => (false, false),
        }
    }

    fn emit_expression(&mut self, expr: &nodes::ExpressionEnum, body: &mut Vec<definition::Instruction>, ty: &definition::Type) -> definition::Val {
        match expr {
            nodes::ExpressionEnum::IntegerLiteral(i) => {
                definition::Val::Const(*i)
            }
            nodes::ExpressionEnum::Unop(op, ref expr) => {
                let src = self.emit_tacky_and_convert(&expr.expr, body, ty);
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone(), expr.ty.clone());
                let tacky_op = match op {
                    nodes::Unop::Negate => definition::Unop::Negate,
                    nodes::Unop::BitwiseNot => definition::Unop::BitwiseNot,
                    nodes::Unop::LogicalNot => definition::Unop::LogicalNot,
                };
                body.push(definition::Instruction::Unary(tacky_op, src, dest.clone()));
                dest
            }
            nodes::ExpressionEnum::Binop(op, ref lft, ref rht) => {
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone(), lft.ty.clone());
                let tacky_op = self.convert_binop(op);

                let sh_circ = self.is_short_circuiting(op);
                if sh_circ.0 {
                    let short_circuit_label = self.make_temporary();
                    let short_circuit_end_label = self.make_temporary();

                    let lhs = self.emit_tacky_and_convert(&lft.expr, body, ty);

                    let sh_circ_val = if !sh_circ.1 {
                        body.push(definition::Instruction::JumpIfZero(lhs.clone(), short_circuit_label.clone()));
                        let rhs = self.emit_tacky_and_convert(&rht.expr, body, ty);
                        body.push(definition::Instruction::JumpIfZero(rhs.clone(), short_circuit_label.clone()));
                        true
                    } else {
                        body.push(definition::Instruction::JumpIfNotZero(lhs.clone(), short_circuit_label.clone()));
                        let rhs = self.emit_tacky_and_convert(&rht.expr, body, ty);
                        body.push(definition::Instruction::JumpIfNotZero(rhs.clone(), short_circuit_label.clone()));
                        false
                    };

                    body.push(definition::Instruction::Copy(dest.clone(), definition::Val::Const(sh_circ_val as i8)));
                    body.push(definition::Instruction::Jump(short_circuit_end_label.clone()));
                    body.push(definition::Instruction::Label(short_circuit_label.clone()));
                    body.push(definition::Instruction::Copy(dest.clone(), definition::Val::Const((!sh_circ_val) as i8)));
                    body.push(definition::Instruction::Label(short_circuit_end_label.clone()));

                    return dest;
                }

                let lhs = self.emit_tacky_and_convert(&lft.expr, body, ty);
                let rhs = self.emit_tacky_and_convert(&rht.expr, body, ty);

                body.push(definition::Instruction::Binary(tacky_op, lhs, rhs, dest.clone()));
                dest
            }
            nodes::ExpressionEnum::Var(s) => {
                definition::Val::Var(s.clone(), self.symbol_table.lookup(s).unwrap().0.clone())
            }
            nodes::ExpressionEnum::Assign(ref lhs, ref rhs) => {
                let lval = self.emit_expression(&lhs.expr, body, &lhs.ty);
                let rval = self.emit_tacky_and_convert(&rhs.expr, body, ty);

                match lval {
                    definition::Val::DereferencedPtr(ref ptr) => {
                        body.push(definition::Instruction::Store(rval.clone(), *ptr.clone()));
                        return rval;
                    }
                    _ => {
                        body.push(definition::Instruction::Copy(rval, lval.clone()));
                        return lval;
                    }
                }
            }
            nodes::ExpressionEnum::Conditional(ref cond, ref lft, ref rht) => {
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone(), lft.ty.clone());
                let e2_label = self.make_temporary();
                let end_label = self.make_temporary();
                let cond = self.emit_tacky_and_convert(&cond.expr, body, ty);
                body.push(definition::Instruction::JumpIfZero(cond, e2_label.clone()));
                let v1 = self.emit_tacky_and_convert(&lft.expr, body, ty);
                body.push(definition::Instruction::Copy(dest.clone(), v1));
                body.push(definition::Instruction::Jump(end_label.clone()));
                body.push(definition::Instruction::Label(e2_label));
                let v2 = self.emit_tacky_and_convert(&rht.expr, body, ty);
                body.push(definition::Instruction::Copy(dest.clone(), v2));
                body.push(definition::Instruction::Label(end_label));
                dest
            }
            nodes::ExpressionEnum::Increment(ref expr) => {
                let expr = match expr.expr {
                    nodes::ExpressionEnum::Var(ref s) => {
                        s.clone()
                    }
                    _ => panic!("Invalid increment target"),
                };

                let src = definition::Val::Var(expr.clone(), ty.clone());
                body.push(definition::Instruction::Unary(definition::Unop::AddImm, definition::Val::Const(1), src.clone()));
                src
            }
            nodes::ExpressionEnum::Decrement(ref expr) => {
                let expr = match expr.expr {
                    nodes::ExpressionEnum::Var(ref s) => {
                        s.clone()
                    }
                    _ => panic!("Invalid increment target"),
                };

                let src = definition::Val::Var(expr.clone(), ty.clone());
                body.push(definition::Instruction::Unary(definition::Unop::AddImm, definition::Val::Const(-1), src.clone()));
                src
            }
            nodes::ExpressionEnum::FunctionCall(ref ident, ref args) => {
                let mut arg_vals = Vec::new();
                for arg in args {
                    arg_vals.push(self.emit_tacky_and_convert(&arg.expr, body, ty));
                }

                let is_global = &self.symbol_table.lookup(ident).unwrap().1;

                let is_global = match is_global {
                    nodes::TableEntry::FnAttr(_, global) => *global,
                    _ => false,
                };

                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone(), ty.clone());
                body.push(definition::Instruction::FunCall(ident.clone(), arg_vals, dest.clone(), is_global));
                dest
            }
            nodes::ExpressionEnum::Dereference(ref expr) => {
                let res = self.emit_tacky_and_convert(&expr.expr, body, ty);
                definition::Val::DereferencedPtr(Box::new(res))
            }
            nodes::ExpressionEnum::AddressOf(ref expr) => {
                match expr.expr {
                    nodes::ExpressionEnum::Dereference(ref expr) => {
                        self.emit_tacky_and_convert(&expr.expr, body, ty)
                    }
                    _ => {
                        let dest_name = self.make_temporary();
                        let dest = definition::Val::Var(dest_name.clone(), ty.clone());
                        let src = self.emit_tacky_and_convert(&expr.expr, body, ty);
                        body.push(definition::Instruction::GetAddress(src, dest.clone()));
                        dest
                    }
                }
            }
        }
    }

    fn emit_tacky_and_convert(&mut self, expr: &nodes::ExpressionEnum, body: &mut Vec<definition::Instruction>, ty: &definition::Type) -> definition::Val {
        let val = self.emit_expression(expr, body, ty);
        match val {
            definition::Val::DereferencedPtr(ptr) => {
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone(), ty.clone());
                body.push(definition::Instruction::Load(*ptr, dest.clone()));
                dest
            }
            _ => val,
        }
    }
}