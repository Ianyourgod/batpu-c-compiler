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
                    let name = var.name.name.clone();

                    let table_entry = self.symbol_table.lookup(&nodes::Identifier { name: name.clone() }).unwrap();

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
                        name,
                        global,
                        val,
                    ));
                }
                nodes::Declaration::FuncDecl(func) => {
                    if func.body.len() == 0 {
                        continue;
                    }

                    let mut body: Vec<definition::Instruction> = Vec::new();
                    let mut params: Vec<String> = Vec::with_capacity(func.params.len());
                    
                    for param in &func.params {
                        params.push(param.name.clone());
                    }
                    
                    for instr in &func.body {
                        self.emit_block_item(instr, &mut body);
                    }

                    let table_entry = self.symbol_table.lookup(&nodes::Identifier { name: func.name.clone() }).unwrap();

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
                        
                        let val = self.emit_expression(expr, body);
                        let name = decl.name.name.clone();

                        body.push(definition::Instruction::Copy(definition::Val::Var(name), val));
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
                let val = self.emit_expression(expr, body);
                body.push(definition::Instruction::Return(val));
            }
            nodes::Statement::Expression(ref expr) => {
                self.emit_expression(expr, body);
            }
            nodes::Statement::If(cond, then, else_) => {
                let end_label = self.make_temporary();
                let val = self.emit_expression(cond, body);
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
                let val = self.emit_expression(cond, body);
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
                let val = self.emit_expression(cond, body);
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
                            let val = self.emit_expression(expr, body);
                            let name = decl.name.name.clone();
                            body.push(definition::Instruction::Copy(definition::Val::Var(name), val));
                        }
                    }
                    nodes::ForInit::Expression(ref expr) => {
                        self.emit_expression(expr, body);
                    }
                    nodes::ForInit::Empty => {}
                }
                body.push(definition::Instruction::Label(continue_label.clone()));
                if cond.is_some() {
                    let val = self.emit_expression(cond.as_ref().unwrap(), body);
                    body.push(definition::Instruction::JumpIfZero(val, break_label.clone()));
                }
                self.emit_statement(loop_body, body);
                if post.is_some() {
                    self.emit_expression(post.as_ref().unwrap(), body);
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

    fn emit_expression(&mut self, expr: &nodes::Expression, body: &mut Vec<definition::Instruction>) -> definition::Val {
        match expr {
            nodes::Expression::IntegerLiteral(i) => {
                definition::Val::Const(*i)
            }
            nodes::Expression::Unop(op, ref expr) => {
                let src = self.emit_expression(expr, body);
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone());
                let tacky_op = match op {
                    nodes::Unop::Negate => definition::Unop::Negate,
                    nodes::Unop::BitwiseNot => definition::Unop::BitwiseNot,
                    nodes::Unop::LogicalNot => definition::Unop::LogicalNot,
                };
                body.push(definition::Instruction::Unary(tacky_op, src, dest.clone()));
                dest
            }
            nodes::Expression::Binop(op, ref lft, ref rht) => {
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone());
                let tacky_op = self.convert_binop(op);

                let sh_circ = self.is_short_circuiting(op);
                if sh_circ.0 {
                    let short_circuit_label = self.make_temporary();
                    let short_circuit_end_label = self.make_temporary();

                    let lhs = self.emit_expression(lft, body);

                    let sh_circ_val = if !sh_circ.1 {
                        body.push(definition::Instruction::JumpIfZero(lhs.clone(), short_circuit_label.clone()));
                        let rhs = self.emit_expression(rht, body);
                        body.push(definition::Instruction::JumpIfZero(rhs.clone(), short_circuit_label.clone()));
                        true
                    } else {
                        body.push(definition::Instruction::JumpIfNotZero(lhs.clone(), short_circuit_label.clone()));
                        let rhs = self.emit_expression(rht, body);
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

                let lhs = self.emit_expression(lft, body);
                let rhs = self.emit_expression(rht, body);

                body.push(definition::Instruction::Binary(tacky_op, lhs, rhs, dest.clone()));
                dest
            }
            nodes::Expression::Var(ref s) => {
                definition::Val::Var(s.name.clone())
            }
            nodes::Expression::Assign(ref lhs, ref rhs) => {
                let lhs = match **lhs {
                    nodes::Expression::Var(ref s) => {
                        s.name.clone()
                    }
                    _ => panic!("Invalid assignment target"),
                };

                let rhs = self.emit_expression(rhs, body);
                body.push(definition::Instruction::Copy(definition::Val::Var(lhs.clone()), rhs.clone()));
                definition::Val::Var(lhs)
            }
            nodes::Expression::Conditional(ref cond, ref lft, ref rht) => {
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone());
                let e2_label = self.make_temporary();
                let end_label = self.make_temporary();
                let cond = self.emit_expression(cond, body);
                body.push(definition::Instruction::JumpIfZero(cond, e2_label.clone()));
                let v1 = self.emit_expression(lft, body);
                body.push(definition::Instruction::Copy(dest.clone(), v1));
                body.push(definition::Instruction::Jump(end_label.clone()));
                body.push(definition::Instruction::Label(e2_label));
                let v2 = self.emit_expression(rht, body);
                body.push(definition::Instruction::Copy(dest.clone(), v2));
                body.push(definition::Instruction::Label(end_label));
                dest
            }
            nodes::Expression::Increment(ref expr) => {
                let expr = match **expr {
                    nodes::Expression::Var(ref s) => {
                        s.name.clone()
                    }
                    _ => panic!("Invalid increment target"),
                };

                let src = definition::Val::Var(expr.clone());
                body.push(definition::Instruction::Unary(definition::Unop::AddImm, definition::Val::Const(1), src.clone()));
                src
            }
            nodes::Expression::Decrement(ref expr) => {
                let expr = match **expr {
                    nodes::Expression::Var(ref s) => {
                        s.name.clone()
                    }
                    _ => panic!("Invalid increment target"),
                };

                let src = definition::Val::Var(expr.clone());
                body.push(definition::Instruction::Unary(definition::Unop::AddImm, definition::Val::Const(-1), src.clone()));
                src
            }
            nodes::Expression::FunctionCall(ref ident, ref args) => {
                let mut arg_vals = Vec::new();
                for arg in args {
                    arg_vals.push(self.emit_expression(arg, body));
                }

                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone());
                body.push(definition::Instruction::FunCall(ident.clone(), arg_vals, dest.clone()));
                dest
            }
        }
    }
}