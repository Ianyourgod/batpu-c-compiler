pub mod definition;

use crate::parser::nodes;

pub struct Tacky {
    program: nodes::Program,
    temp_count: i32,
    symbol_table: nodes::SymbolTable,
    type_table: nodes::TypeTable,
}

impl Tacky {
    pub fn new(program: nodes::Program, symbol_table: nodes::SymbolTable, type_table: nodes::TypeTable) -> Tacky {
        Tacky {
            program,
            temp_count: 0,
            symbol_table,
            type_table,
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
                        _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected StaticAttr, got {:?}", table_entry.1),
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
                    let mut body: Vec<definition::Instruction> = Vec::new();
                    let mut params: Vec<(String, definition::Type)> = Vec::with_capacity(func.params.len());
                    
                    for param in &func.params {
                        params.push((param.clone(), self.symbol_table.lookup(&param).unwrap().0.clone()));
                    }

                    let table_entry = self.symbol_table.lookup(&func.name).unwrap();

                    let global = match table_entry.1 {
                        nodes::TableEntry::FnAttr(_, global) => global,
                        _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected FnAttr, got {:?}", table_entry.1),
                    };

                    if func.body.len() == 0 {
                        let func = definition::FuncDef {
                            name: func.name.clone(),
                            params,
                            body,
                            global,
                            defined: false,
                        };
    
                        statements.push(definition::TopLevel::FuncDef(func));
                        continue;
                    }
                    
                    for instr in &func.body {
                        self.emit_block_item(instr, &mut body);
                    }

                    body.push(definition::Instruction::Return(None));

                    let func = definition::FuncDef {
                        name: func.name.clone(),
                        params,
                        body,
                        global,
                        defined: true,
                    };

                    statements.push(definition::TopLevel::FuncDef(func));
                }
                nodes::Declaration::Empty |
                nodes::Declaration::StructDecl(_) => (),
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
                        
                        self.handle_initializer(expr, body, &decl.name, &decl.ty, 0, true);
                    }
                    // we don't need to do anything for declarations without an expression
                }
                // TODO: handle function declarations
            }
        }
    }

    fn handle_initializer(&mut self, init: &nodes::Initializer, body: &mut Vec<definition::Instruction>, name: &str, ty: &definition::Type, mut offset: i16, top_level: bool) {
        match init {
            nodes::Initializer::Single(ref expr) => {
                let val = self.emit_tacky_and_convert(&expr.expr, body, &expr.ty);
                if !top_level {
                    body.push(definition::Instruction::CopyToOffset(val, definition::Val::Var(name.to_string(), ty.clone()), offset as i16));
                } else {
                    body.push(definition::Instruction::Copy(definition::Val::Var(name.to_string(), ty.clone()), val));
                }
            }
            nodes::Initializer::Compound(ref inits) => {
                match ty {
                    definition::Type::Array(inner_ty, _) => {
                        if name == "localvar.input.40" {
                            println!("{:#?}", init);
                        }
                        for init in inits {
                            self.handle_initializer(init, body, name, ty, offset, false);
                            offset += inner_ty.size() as i16;
                        }
                    }
                    definition::Type::Struct(tag) => {
                        let members = self.type_table.lookup(tag).unwrap().1.clone();
                        for (member, init) in members.iter().zip(inits) {
                            let member_offset = member.offset as i16;
                            self.handle_initializer(init, body, name, ty, offset + member_offset, false);
                        }
                    }
                    _ => panic!("Invalid compound initializer"),
                }
            }
        }
    }

    fn emit_statement(&mut self, stmt: &nodes::Statement, body: &mut Vec<definition::Instruction>) {
        match stmt {
            nodes::Statement::Return(ref expr) => {
                let val = match expr {
                    Some(expr) => Some(self.emit_tacky_and_convert(&expr.expr, body, &expr.ty)),
                    None => None,
                };
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
                body.push(definition::Instruction::Jump(format!("{}.break", label)))
            }
            nodes::Statement::Continue(label) => {
                body.push(definition::Instruction::Jump(format!("{}.continue", label)))
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
                            self.handle_initializer(&expr, body, &decl.name, &decl.ty, 0, true);
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
            nodes::Binop::Multiply => definition::Binop::Multiply,
            nodes::Binop::Divide => definition::Binop::Divide,
            nodes::Binop::And => definition::Binop::And,
            nodes::Binop::Or => definition::Binop::Or,
            nodes::Binop::Equal => definition::Binop::Equal,
            nodes::Binop::NotEqual => definition::Binop::NotEqual,
            nodes::Binop::LessThan => definition::Binop::LessThan,
            nodes::Binop::GreaterThan => definition::Binop::GreaterThan,
            nodes::Binop::LessThanEqual => definition::Binop::LessThanEqual,
            nodes::Binop::GreaterThanEqual => definition::Binop::GreaterThanEqual,
            nodes::Binop::LeftShift => definition::Binop::LeftShift,
            nodes::Binop::RightShift => definition::Binop::RightShift,
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

                    body.push(definition::Instruction::Copy(dest.clone(), definition::Val::Const(sh_circ_val as i16)));
                    body.push(definition::Instruction::Jump(short_circuit_end_label.clone()));
                    body.push(definition::Instruction::Label(short_circuit_label.clone()));
                    body.push(definition::Instruction::Copy(dest.clone(), definition::Val::Const((!sh_circ_val) as i16)));
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
                    definition::Val::SubObject((var, ty), offset) => {
                        body.push(definition::Instruction::CopyToOffset(rval.clone(), definition::Val::Var(var, ty), offset));
                        return rval;
                    }
                    _ => {
                        body.push(definition::Instruction::Copy(lval.clone(), rval.clone()));
                        return lval;
                    }
                }
            }
            nodes::ExpressionEnum::Conditional(ref cond, ref lft, ref rht) => {
                let e2_label = self.make_temporary();
                let end_label = self.make_temporary();
                let cond = self.emit_tacky_and_convert(&cond.expr, body, ty);
                body.push(definition::Instruction::JumpIfZero(cond, e2_label.clone()));
                let v1 = self.emit_tacky_and_convert(&lft.expr, body, ty);
                if ty == &nodes::Type::Void {
                    body.push(definition::Instruction::Jump(end_label.clone()));
                    body.push(definition::Instruction::Label(e2_label));
                    self.emit_tacky_and_convert(&rht.expr, body, ty);
                    body.push(definition::Instruction::Label(end_label));
                    definition::Val::Const(0)
                } else {
                    let dest_name = self.make_temporary();
                    let dest = definition::Val::Var(dest_name.clone(), lft.ty.clone());

                    body.push(definition::Instruction::Copy(dest.clone(), v1));
                    body.push(definition::Instruction::Jump(end_label.clone()));
                    body.push(definition::Instruction::Label(e2_label));
                    let v2 = self.emit_tacky_and_convert(&rht.expr, body, ty);
                    body.push(definition::Instruction::Copy(dest.clone(), v2));
                    body.push(definition::Instruction::Label(end_label));
                    dest
                }
            }
            nodes::ExpressionEnum::Increment(ref expr) => {
                let lval = self.emit_expression(&expr.expr, body, &expr.ty);

                match lval {
                    definition::Val::DereferencedPtr(ref ptr) => {
                        let dest_name = self.make_temporary();
                        let dest = definition::Val::Var(dest_name.clone(), ty.clone());
                        body.push(definition::Instruction::Load(*ptr.clone(), dest.clone()));
                        body.push(definition::Instruction::Unary(definition::Unop::AddImm, definition::Val::Const(1), dest.clone()));
                        body.push(definition::Instruction::Store(dest.clone(), *ptr.clone()));
                        return dest;
                    }
                    definition::Val::SubObject((var, ty), offset) => {
                        let dest_name = self.make_temporary();
                        let dest = definition::Val::Var(dest_name.clone(), ty.clone());
                        body.push(definition::Instruction::CopyFromOffset(definition::Val::Var(var.clone(), ty.clone()), offset, dest.clone()));
                        body.push(definition::Instruction::Unary(definition::Unop::AddImm, definition::Val::Const(1), dest.clone()));
                        body.push(definition::Instruction::CopyToOffset(dest.clone(), definition::Val::Var(var.clone(), ty.clone()), offset));
                        return dest;
                    }
                    _ => {
                        body.push(definition::Instruction::Unary(definition::Unop::AddImm, definition::Val::Const(1), lval.clone()));
                        return lval;
                    }
                }
            }
            nodes::ExpressionEnum::Decrement(ref expr) => {
                let lval = self.emit_expression(&expr.expr, body, &expr.ty);

                match lval {
                    definition::Val::DereferencedPtr(ref ptr) => {
                        let dest_name = self.make_temporary();
                        let dest = definition::Val::Var(dest_name.clone(), ty.clone());
                        body.push(definition::Instruction::Load(*ptr.clone(), dest.clone()));
                        body.push(definition::Instruction::Unary(definition::Unop::AddImm, definition::Val::Const(-1), dest.clone()));
                        body.push(definition::Instruction::Store(dest.clone(), *ptr.clone()));
                        return dest;
                    }
                    definition::Val::SubObject((var, ty), offset) => {
                        let dest_name = self.make_temporary();
                        let dest = definition::Val::Var(dest_name.clone(), ty.clone());
                        body.push(definition::Instruction::CopyFromOffset(definition::Val::Var(var.clone(), ty.clone()), offset, dest.clone()));
                        body.push(definition::Instruction::Unary(definition::Unop::AddImm, definition::Val::Const(-1), dest.clone()));
                        body.push(definition::Instruction::CopyToOffset(dest.clone(), definition::Val::Var(var.clone(), ty.clone()), offset));
                        return dest;
                    }
                    _ => {
                        body.push(definition::Instruction::Unary(definition::Unop::AddImm, definition::Val::Const(-1), lval.clone()));
                        return lval;
                    }
                }
            }
            nodes::ExpressionEnum::FunctionCall(ref ident, ref args) => {
                let mut arg_vals = Vec::new();
                for arg in args {
                    arg_vals.push(self.emit_tacky_and_convert(&arg.expr, body, &arg.ty));
                }

                let is_global = &self.symbol_table.lookup(ident).unwrap().1;

                let is_global = match is_global {
                    nodes::TableEntry::FnAttr(_, global) => *global,
                    _ => false,
                };

                let dest = match ty {
                    definition::Type::Void => None,
                    _ => Some(definition::Val::Var(self.make_temporary(), ty.clone())),
                };

                body.push(definition::Instruction::FunCall(ident.clone(), arg_vals, dest.clone(), is_global));
                
                if dest.is_some() {
                    dest.unwrap()
                } else {
                    definition::Val::Const(0)
                }
            }
            nodes::ExpressionEnum::Dereference(ref expr) => {
                let res = self.emit_tacky_and_convert(&expr.expr, body, ty);
                definition::Val::DereferencedPtr(Box::new(res))
            }
            nodes::ExpressionEnum::AddressOf(ref expr) => {
                let src = self.emit_expression(&expr.expr, body, &expr.ty);
                match src {
                    definition::Val::DereferencedPtr(ptr) => *ptr,
                    definition::Val::SubObject(var, offset) => {
                        let dest_name = self.make_temporary();
                        let dest = definition::Val::Var(dest_name.clone(), ty.clone());

                        body.push(definition::Instruction::GetAddress(definition::Val::Var(var.0, var.1), dest.clone()));
                        body.push(definition::Instruction::AddPtr(dest.clone(), definition::Val::Const(offset as i16), 1, dest.clone()));
                        dest
                    },
                    _ => {
                        let dest_name = self.make_temporary();
                        let dest = definition::Val::Var(dest_name.clone(), ty.clone());
                        body.push(definition::Instruction::GetAddress(src, dest.clone()));
                        dest
                    }
                }
            }
            nodes::ExpressionEnum::Subscript(ref left, ref right) => {
                let left = self.emit_tacky_and_convert(&left.expr, body, ty);
                let right = self.emit_tacky_and_convert(&right.expr, body, ty);

                // we add, then dereference
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone(), ty.clone());
                
                body.push(definition::Instruction::AddPtr(left.clone(), right.clone(), ty.size() as i16, dest.clone()));
                definition::Val::DereferencedPtr(Box::new(dest))
            },
            nodes::ExpressionEnum::CharLiteral(ch) => {
                definition::Val::Const(Self::char_to_int(*ch))
            }
            nodes::ExpressionEnum::Cast(to, ref expr) => {
                if to == &nodes::Type::Void {
                    self.emit_tacky_and_convert(&expr.expr, body, to);
                    return definition::Val::Const(0);
                }

                self.emit_tacky_and_convert(&expr.expr, body, to)
            }
            nodes::ExpressionEnum::SizeOf(expr) => {
                definition::Val::Const(expr.ty.size() as i16)
            }
            nodes::ExpressionEnum::SizeOfType(ty) => {
                definition::Val::Const(match ty {
                    nodes::Type::Struct(tag) => self.type_table.lookup(tag).unwrap().0 as i16,
                    _ => ty.size() as i16,
                })
            }
            nodes::ExpressionEnum::Dot(ref expr, ref member) => {
                let struct_tag = match &expr.ty {
                    nodes::Type::Struct(tag) => tag,
                    _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected struct type, got {:?}", expr.ty),
                };
                let member_entry = self.type_table.lookup_member_entry(struct_tag, member).unwrap();
                let member_offset = member_entry.offset as i16;

                let expr = self.emit_expression(&expr.expr, body, &expr.ty);
                
                match expr {
                    definition::Val::SubObject(ident, offset) => definition::Val::SubObject(ident, offset + member_offset),
                    definition::Val::DereferencedPtr(ptr) => {
                        let dest_name = self.make_temporary();
                        let dest = definition::Val::Var(dest_name.clone(), ty.clone());
                        
                        body.push(definition::Instruction::AddPtr(*ptr, definition::Val::Const(member_offset as i16), 1, dest.clone()));

                        definition::Val::DereferencedPtr(Box::new(dest))
                    }
                    definition::Val::Var(ident, ty) => definition::Val::SubObject((ident, ty), member_offset as i16),
                    definition::Val::Const(_) => panic!("Cannot access member of constant"),
                }
            },
            nodes::ExpressionEnum::Arrow(ref expr, ref member) => {
                let struct_tag = match &expr.ty {
                    nodes::Type::Pointer(inner) => match inner.as_ref() {
                        nodes::Type::Struct(tag) => tag,
                        _ => panic!("Expected struct type, got {:?}", inner),
                    },
                    _ => panic!("Expected struct type, got {:?}", expr.ty),
                };
                let member_entry = self.type_table.lookup_member_entry(&struct_tag, member).unwrap();
                let member_offset = member_entry.offset as i16;

                let expr = self.emit_tacky_and_convert(&expr.expr, body, &expr.ty);
                
                // expr is a pointer. We need to add the offset to the pointer, then dereference it
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone(), ty.clone());

                body.push(definition::Instruction::AddPtr(expr.clone(), definition::Val::Const(member_offset as i16), 1, dest.clone()));
                definition::Val::DereferencedPtr(Box::new(dest))
            },
            nodes::ExpressionEnum::StringLiteral(s) => {
                // we turn this into a var = array of characters (so s+'\0')
                let mut chars = s.chars().collect::<Vec<char>>();

                chars.push('\0');

                let init = nodes::Initializer::Compound(chars.into_iter().map(|c| nodes::Initializer::Single(nodes::Expression {
                    expr: nodes::ExpressionEnum::CharLiteral(c),
                    ty: nodes::Type::Char,
                })).collect());

                let name = self.make_temporary();
                let ty = definition::Type::Array(Box::new(definition::Type::Char), s.len() as i16 + 1);
                self.handle_initializer(&init, body, &name, &ty, 0, true);

                let actual_ty = definition::Type::Pointer(Box::new(definition::Type::Char));

                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone(), actual_ty.clone());

                body.push(definition::Instruction::GetAddress(definition::Val::Var(name.clone(), ty.clone()), dest.clone()));

                dest
            }
        }
    }

    fn char_to_int(ch: char) -> i16 {
        /*
         0 - SPACE
         1 - 26 - A - Z
         27 - .
         28 - !
         29 - ?
         // others, just do ascii
         */

        let ch = ch.to_ascii_lowercase();

        match ch {
            ' ' => 0,
            'a'..='z' => ch as i16 - 96,
            '.' => 27,
            '!' => 28,
            '?' => 29,
            '\0' => 30,
            _ => ch as i16,
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
            },
            definition::Val::SubObject(var, offset) => {
                let dest_name = self.make_temporary();
                let dest = definition::Val::Var(dest_name.clone(), ty.clone());
                body.push(definition::Instruction::CopyFromOffset(definition::Val::Var(var.0, var.1), offset, dest.clone()));
                dest
            }
            _ => val,
        }
    }
}