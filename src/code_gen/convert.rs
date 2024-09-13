use crate::code_gen::assembly;
use crate::tacky::definition;

// we use as many registers as we can for parameters, as the cpu doesnt have a lot of memory
macro_rules! param_registers {
    () => {
        vec![
            assembly::Register::new("r1".to_string()),
            assembly::Register::new("r2".to_string()),
            assembly::Register::new("r3".to_string()),
            assembly::Register::new("r4".to_string()),
            assembly::Register::new("r5".to_string()),
            assembly::Register::new("r6".to_string()),
            assembly::Register::new("r7".to_string()),
            assembly::Register::new("r8".to_string()),
            assembly::Register::new("r9".to_string()),
            assembly::Register::new("r10".to_string()),
            assembly::Register::new("r11".to_string()),
            assembly::Register::new("r12".to_string()),
            assembly::Register::new("r13".to_string()),
        ]
    };
}

pub struct ConvertPass {
    program: definition::Program,
    tmp_counter: u32,
}

impl ConvertPass {
    pub fn new(program: definition::Program) -> ConvertPass {
        ConvertPass {
            program,
            tmp_counter: 0,
        }
    }

    pub fn generate(&mut self) -> assembly::Program {
        let mut assembly = assembly::Program {
            statements: Vec::new(),
        };

        for stmt in self.program.statements.clone() {
            match stmt {
                definition::TopLevel::FuncDef(ref func) => self.generate_function(func, &mut assembly),
                definition::TopLevel::StaticVariable(name, global, _ty, init) => self.generate_static_variable(name, global, init, &mut assembly),
            }
        }

        assembly
    }

    fn generate_function(&mut self, func: &definition::FuncDef, program: &mut assembly::Program) {
        let mut instrs: Vec<assembly::Instruction> = Vec::new();

        let param_regs = param_registers!();
        let mut current_reg = 0;
        for param in &func.params {
            if current_reg >= param_regs.len() {
                panic!("Too many parameters");
            }
            instrs.push(assembly::Instruction::Mov(
                assembly::Operand::Register(param_regs[current_reg].clone()),
                assembly::Operand::Pseudo(param.0.clone(), param.1.clone()),
            ));
            current_reg += 1;
        }

        for stmt in &func.body {
            self.generate_instruction(stmt, &mut instrs);
        }

        let func = assembly::FuncDecl {
            name: func.name.clone(),
            body: instrs,
            stack_size: 0,
            global: func.global,
        };

        program.statements.push(assembly::TopLevel::FuncDef(func));
    }

    fn generate_static_variable(&mut self, name: String, global: bool, init: i32, program: &mut assembly::Program) {
        program.statements.push(assembly::TopLevel::StaticVariable(name, global, init));
    }

    fn generate_label(&mut self, label: String) -> String {
        let label = format!("cL.{}.{}", label, self.tmp_counter); 
        self.tmp_counter += 1;
        label
    }

    fn generate_comparison(&mut self, op: &definition::Binop, dst: &definition::Val, instructions: &mut Vec<assembly::Instruction>) {
        let (is_simple, cond) = match op {
            definition::Binop::Equal => (true, assembly::CondCode::Equal),
            definition::Binop::NotEqual => (true, assembly::CondCode::NotEqual),
            definition::Binop::GreaterThanEqual => (true, assembly::CondCode::GreaterThanEqual),
            definition::Binop::LessThan => (true, assembly::CondCode::LessThan),
            _ => (false, assembly::CondCode::Equal),
        };

        if is_simple {
            let true_label = self.generate_label("true".to_string());
            let end_label = self.generate_label("end".to_string());
            
            instructions.push(assembly::Instruction::JmpCC(
                cond,
                true_label.clone()
            ));

            instructions.push(assembly::Instruction::Mov(
                assembly::Operand::Immediate(0),
                self.convert_val(dst)
            ));
            instructions.push(assembly::Instruction::Jmp(end_label.clone()));

            instructions.push(assembly::Instruction::Label(true_label));
            instructions.push(assembly::Instruction::Mov(
                assembly::Operand::Immediate(1),
                self.convert_val(dst)
            ));
            
            instructions.push(assembly::Instruction::Label(end_label));
        }
    }

    fn generate_instruction(&mut self, stmt: &definition::Instruction, instructions: &mut Vec<assembly::Instruction>) {
        match stmt {
            definition::Instruction::Return(ref val) => {
                // move the value to r1
                if val.is_none() {
                    instructions.push(assembly::Instruction::Return);
                    return;
                }

                let val = val.as_ref().unwrap();

                instructions.push(assembly::Instruction::Mov(
                    self.convert_val(val),
                    assembly::Operand::Register(assembly::Register::new("r1".to_string()))
                ));
                instructions.push(assembly::Instruction::Return);
            },
            definition::Instruction::Unary(ref op, ref src, ref dst) => {
                match op {
                    definition::Unop::LogicalNot => {
                        let src_converted = self.convert_val(src);
                        instructions.push(assembly::Instruction::Binary(
                            assembly::Binop::Nor,
                            src_converted.clone(),
                            src_converted,
                            self.convert_val(dst),
                        ));
                        return;
                    }
                    definition::Unop::AddImm => {
                        instructions.push(assembly::Instruction::Adi(
                            self.convert_val(dst),
                            match src {
                                definition::Val::Const(i) => *i,
                                _ => unreachable!(),
                            }
                        ));
                        return;
                    }
                    _ => (),
                }

                instructions.push(assembly::Instruction::Unary(
                    self.convert_unop(op),
                    self.convert_val(src),
                    self.convert_val(dst)
                ));
            },
            definition::Instruction::Binary(ref op, ref src1, ref src2, ref dst) => {
                match op {
                    definition::Binop::GreaterThan => {
                        instructions.push(assembly::Instruction::Cmp(
                            self.convert_val(src1),
                            self.convert_val(src2)
                        ));
                        let false_label = self.generate_label("false".to_string());
                        let end_label = self.generate_label("end".to_string());

                        let dst = self.convert_val(dst);

                        instructions.push(assembly::Instruction::JmpCC(assembly::CondCode::Equal, false_label.clone()));
                        instructions.push(assembly::Instruction::JmpCC(assembly::CondCode::LessThan, false_label.clone()));
                        instructions.push(assembly::Instruction::Mov(assembly::Operand::Immediate(1), dst.clone()));
                        instructions.push(assembly::Instruction::Jmp(end_label.clone()));
                        instructions.push(assembly::Instruction::Label(false_label));
                        instructions.push(assembly::Instruction::Mov(assembly::Operand::Immediate(0), dst.clone()));
                        instructions.push(assembly::Instruction::Label(end_label));
                        return;
                    },
                    definition::Binop::LessThanEqual => {
                        instructions.push(assembly::Instruction::Cmp(
                            self.convert_val(src1),
                            self.convert_val(src2)
                        ));
                        let true_label = self.generate_label("true".to_string());
                        let end_label = self.generate_label("end".to_string());

                        let dst = self.convert_val(dst);

                        instructions.push(assembly::Instruction::JmpCC(assembly::CondCode::Equal, true_label.clone()));
                        instructions.push(assembly::Instruction::JmpCC(assembly::CondCode::LessThan, true_label.clone()));
                        instructions.push(assembly::Instruction::Mov(assembly::Operand::Immediate(0), dst.clone()));
                        instructions.push(assembly::Instruction::Jmp(end_label.clone()));
                        instructions.push(assembly::Instruction::Label(true_label));
                        instructions.push(assembly::Instruction::Mov(assembly::Operand::Immediate(1), dst.clone()));
                        instructions.push(assembly::Instruction::Label(end_label));
                        return;
                    },
                    _ => ()
                }
                
                if self.is_comparison(op) {
                    instructions.push(assembly::Instruction::Cmp(
                        self.convert_val(src1),
                        self.convert_val(src2)
                    ));
                    self.generate_comparison(op, dst, instructions);
                    return;
                }

                instructions.push(assembly::Instruction::Binary(
                    self.convert_binop(op),
                    self.convert_val(src1),
                    self.convert_val(src2),
                    self.convert_val(dst)
                ));
            },
            definition::Instruction::Copy(ref dst, ref src) => {
                instructions.push(assembly::Instruction::Mov(
                    self.convert_val(src),
                    self.convert_val(dst)
                ));
            },
            definition::Instruction::JumpIfZero(ref src, ref label) => {
                instructions.push(assembly::Instruction::Cmp(
                    self.convert_val(src),
                    assembly::Operand::Immediate(0)
                ));
                instructions.push(assembly::Instruction::JmpCC(
                    assembly::CondCode::Equal,
                    label.clone()
                ));
            },
            definition::Instruction::JumpIfNotZero(ref src, ref label) => {
                instructions.push(assembly::Instruction::Cmp(
                    self.convert_val(src),
                    assembly::Operand::Immediate(0)
                ));
                instructions.push(assembly::Instruction::JmpCC(
                    assembly::CondCode::NotEqual,
                    label.clone()
                ));
            },
            definition::Instruction::Jump(ref label) => {
                instructions.push(assembly::Instruction::Jmp(label.clone()));
            },
            definition::Instruction::Label(ref label) => {
                instructions.push(assembly::Instruction::Label(label.clone()));
            },
            definition::Instruction::FunCall(ref name, ref params, ref dst, global) => {
                let param_regs = param_registers!();
                let mut current_reg = 0;
                for param in params {
                    if current_reg >= param_regs.len() {
                        panic!("Too many parameters");
                    }
                    instructions.push(assembly::Instruction::Mov(
                        self.convert_val(param),
                        assembly::Operand::Register(param_regs[current_reg].clone())
                    ));
                    current_reg += 1;
                }

                instructions.push(assembly::Instruction::Call(name.clone(), *global));

                if dst.is_none() {
                    return;
                }
                instructions.push(assembly::Instruction::Mov(
                    assembly::Operand::Register(assembly::Register::new("r1".to_string())),
                    self.convert_val(dst.as_ref().unwrap())
                ));
            },
            definition::Instruction::Load(ref src, ref dst) => {
                let src = self.convert_val(src);
                instructions.push(assembly::Instruction::Mov(
                    src,
                    assembly::Operand::Register(assembly::Register::new("r1".to_string())),
                ));
                instructions.push(assembly::Instruction::Lod(
                    assembly::Register::new("r1".to_string()),
                    0,
                    self.convert_val(dst),
                ));
            },
            definition::Instruction::Store(ref src, ref dst) => {
                let dst = self.convert_val(dst);
                instructions.push(assembly::Instruction::Mov(
                    dst,
                    assembly::Operand::Register(assembly::Register::new("r1".to_string())),
                ));
                instructions.push(assembly::Instruction::Str(
                    self.convert_val(src),
                    0,
                    assembly::Register::new("r1".to_string()),
                ));
            },
            definition::Instruction::GetAddress(ref src, ref dst) => {
                instructions.push(assembly::Instruction::Lea(
                    self.convert_val(src),
                    self.convert_val(dst)
                ));
            },
            definition::Instruction::CopyToOffset(ref val, ref var, ref offset) => {
                let (name, ty) = match var {
                    definition::Val::Var(name, ty) => (name.clone(), ty.clone()),
                    _ => unreachable!(),
                };
                instructions.push(assembly::Instruction::Mov(
                    self.convert_val(val),
                    assembly::Operand::PseudoMem(name, *offset, ty)
                ));
            },
            definition::Instruction::AddPtr(ref val1, ref val2, ref offset, ref dst) => {
                // mult val2 and offset by calling "..mult"
                let val1 = self.convert_val(val1);
                let val2 = self.convert_val(val2);
                let offset = self.convert_val(offset);
                let dst = self.convert_val(dst);
                instructions.push(assembly::Instruction::Mov(
                    val2,
                    assembly::Operand::Register(assembly::Register::new("r1".to_string())),
                ));
                instructions.push(assembly::Instruction::Mov(
                    offset,
                    assembly::Operand::Register(assembly::Register::new("r2".to_string())),
                ));
                instructions.push(assembly::Instruction::Call(".mult".to_string(), false));
                // if r1 is bigger than the address of val1, we need to swap them
                instructions.push(assembly::Instruction::Cmp(
                    assembly::Operand::Register(assembly::Register::new("r1".to_string())),
                    val1.clone(),
                ));
                let false_label = self.generate_label("false".to_string());
                let end_label = self.generate_label("end".to_string());
                instructions.push(assembly::Instruction::JmpCC(
                    assembly::CondCode::LessThan,
                    false_label.clone(),
                ));
                instructions.push(assembly::Instruction::Binary(
                    assembly::Binop::Subtract,
                    assembly::Operand::Register(assembly::Register::new("r1".to_string())),
                    val1.clone(),
                    dst.clone(),
                ));
                instructions.push(assembly::Instruction::Jmp(end_label.clone()));
                instructions.push(assembly::Instruction::Label(false_label));
                instructions.push(assembly::Instruction::Binary(
                    assembly::Binop::Subtract,
                    val1,
                    assembly::Operand::Register(assembly::Register::new("r1".to_string())),
                    dst,
                ));
                instructions.push(assembly::Instruction::Label(end_label));

            },
        }
    }

    fn convert_val(&mut self, val: &definition::Val) -> assembly::Operand {
        match val {
            definition::Val::Const(i) => assembly::Operand::Immediate(*i),
            definition::Val::Var(ref s, ref t) => {
                match t {
                    &definition::Type::Array(_, _) => {
                        assembly::Operand::PseudoMem(s.clone(), 0, t.clone())
                    },
                    _ => {
                        assembly::Operand::Pseudo(s.clone(), t.clone())
                    }
                }
            },
            definition::Val::DereferencedPtr(_) => panic!("Dereferenced pointers are not supported"),
        }
    }

    fn convert_unop(&mut self, op: &definition::Unop) -> assembly::Unop {
        match op {
            definition::Unop::Negate => assembly::Unop::Negate,
            definition::Unop::BitwiseNot => assembly::Unop::BitwiseNot,
            definition::Unop::LogicalNot | definition::Unop::AddImm => unreachable!(),
        }
    }

    fn is_comparison(&mut self, op: &definition::Binop) -> bool {
        match op {
            definition::Binop::Equal => true,
            definition::Binop::NotEqual => true,
            definition::Binop::GreaterThanEqual => true,
            definition::Binop::LessThan => true,
            _ => false,
        }
    }

    fn convert_binop(&mut self, op: &definition::Binop) -> assembly::Binop {
        match op {
            definition::Binop::Add => assembly::Binop::Add,
            definition::Binop::Subtract => assembly::Binop::Subtract,
            _ => unreachable!("{:?}", op)
        }
    }
}