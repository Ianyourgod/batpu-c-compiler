use crate::code_gen::assembly;
use crate::tacky::definition;

macro_rules! param_registers {
    () => {
        vec![
            assembly::Register::new(String::from("r1")),
            assembly::Register::new(String::from("r2")),
            assembly::Register::new(String::from("r3")),
            assembly::Register::new(String::from("r4")),
            assembly::Register::new(String::from("r5")),
            assembly::Register::new(String::from("r6")),
            assembly::Register::new(String::from("r7")),
            assembly::Register::new(String::from("r8")),
            assembly::Register::new(String::from("r9")),
        ]
    };
}

pub struct ConvertPass {
    program: definition::Program,
    tmp_counter: u32,
    function_table: assembly::FunctionTable,
}

impl ConvertPass {
    pub fn new(program: definition::Program) -> ConvertPass {
        ConvertPass {
            program,
            tmp_counter: 0,
            function_table: assembly::FunctionTable::new(),
        }
    }

    pub fn generate(&mut self) -> (assembly::Program, assembly::FunctionTable) {
        let mut assembly = assembly::Program {
            statements: Vec::new(),
        };

        for stmt in self.program.statements.clone() {
            match stmt {
                definition::TopLevel::FuncDef(ref func) => self.generate_function(func, &mut assembly),
                definition::TopLevel::StaticVariable(name, global, _ty, init) => self.generate_static_variable(name, global, init, &mut assembly),
            }
        }

        (assembly, self.function_table.clone())
    }

    fn generate_function(&mut self, func: &definition::FuncDef, program: &mut assembly::Program) {
        let mut instrs: Vec<assembly::Instruction> = Vec::new();

        let params = func.params.iter().map(|(n, t)| definition::Val::Var(n.clone(), t.clone())).collect();

        let offset = self.set_up_parameters(func.name.clone(), params, &mut instrs);

        for stmt in &func.body {
            self.generate_instruction(stmt, &mut instrs);
        }

        let func = assembly::FuncDecl {
            name: func.name.clone(),
            body: instrs,
            stack_size: offset,
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

    fn classify_parameters(&self, values: Vec<definition::Val>) -> (Vec<(assembly::Operand, assembly::Register)>, Vec<(assembly::Operand, i16)>) { // (registers, stack)
        let mut reg_args = Vec::new();
        let mut stack_args = Vec::new();

        let param_regs = param_registers!();

        for (i, val) in values.iter().enumerate() {
            let converted = self.convert_val(&val);
            if reg_args.len() < param_regs.len() {
                reg_args.push((converted, param_regs[i].clone()));
            } else {
                let size = self.size_of_operand(&converted);
                stack_args.push((converted, size));
            }
        }

        (reg_args, stack_args)
    } 

    fn set_up_parameters(&mut self, name: String, params: Vec<definition::Val>, instructions: &mut Vec<assembly::Instruction>) -> i16 {
        let (reg_args, stack_args) = self.classify_parameters(params);

        let mut used_regs = Vec::new();
        for (val, reg) in reg_args {
            // move the value to the register
            used_regs.push(reg.clone());
            instructions.push(assembly::Instruction::Mov(
                assembly::Operand::Register(reg),
                val
            ));
        }

        self.function_table.insert(name, used_regs);

        let mut offset = 0;
        for (val, size) in stack_args {
            // move the value to the stack
            instructions.push(assembly::Instruction::Mov(
                assembly::Operand::Memory(assembly::Register::new("r15".to_string()), -offset),
                val
            ));
            offset += size;
        }

        offset
    }

    fn type_of_operand(&self, val: &assembly::Operand) -> definition::Type {
        match val {
            assembly::Operand::Immediate(_) => definition::Type::Int,
            assembly::Operand::Pseudo(_, ty) => ty.clone(),
            assembly::Operand::PseudoMem(_, _, ty) => definition::Type::Pointer(Box::new(ty.clone())),
            _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: hey dont do that")
        }
    }

    fn size_of_operand(&self, val: &assembly::Operand) -> i16 {
        let ty = self.type_of_operand(val);

        match ty {
            definition::Type::Int |
            definition::Type::Pointer(_) |
            definition::Type::Char => 1,
            definition::Type::Void => 0,
            _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: hey dont do that")
        }
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
                let (reg_args, stack_args) = self.classify_parameters(params.clone());

                // move the parameters to the registers
                for (val, reg) in reg_args {
                    instructions.push(assembly::Instruction::Mov(
                        val,
                        assembly::Operand::Register(reg)
                    ));
                }

                // move the parameters to the stack
                for (val, offset) in stack_args.into_iter().rev() {
                    instructions.push(assembly::Instruction::Mov(
                        val,
                        assembly::Operand::Memory(assembly::Register::new("r14".to_string()), offset)
                    ));
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
            definition::Instruction::CopyFromOffset(ref var, ref offset, ref dst) => {
                let (name, ty) = match var {
                    definition::Val::Var(name, ty) => (name.clone(), ty.clone()),
                    _ => unreachable!(),
                };
                instructions.push(assembly::Instruction::Mov(
                    assembly::Operand::PseudoMem(name, *offset, ty),
                    self.convert_val(dst)
                ));
            },
            definition::Instruction::AddPtr(ref val1, ref val2, ref offset, ref dst) => {
                // mult val2 and offset by calling "__mult"
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
                instructions.push(assembly::Instruction::Call("__mult".to_string(), false));
                instructions.push(assembly::Instruction::Binary(
                    assembly::Binop::Add,
                    val1,
                    assembly::Operand::Register(assembly::Register::new("r1".to_string())),
                    dst
                ));
            },
        }
    }

    fn convert_val(&self, val: &definition::Val) -> assembly::Operand {
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
            definition::Val::DereferencedPtr(_) => unreachable!("INTERNAL ERROR. PLEASE REPORT: Dereferenced pointers should have been removed"),
            definition::Val::SubObject(_, _) => unreachable!("INTERNAL ERROR. PLEASE REPORT: Subobjects should have been removed"),
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