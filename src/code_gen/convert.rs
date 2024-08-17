use crate::code_gen::assembly;
use crate::tacky::definition;

pub struct ConvertPass {
    program: definition::Program,
    tmp_counter: u32,
}

impl ConvertPass {
    pub fn new(program: definition::Program) -> ConvertPass {
        //println!("{:#?}", program);
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
            self.generate_function(&stmt, &mut assembly);
        }

        assembly
    }

    fn generate_function(&mut self, func: &definition::FuncDecl, program: &mut assembly::Program) {
        let mut instrs: Vec<assembly::Instruction> = Vec::new();

        for stmt in &func.body {
            self.generate_instruction(stmt, &mut instrs);
        }

        let func = assembly::FuncDecl {
            name: func.name.clone(),
            body: instrs,
        };

        program.statements.push(func);
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
        }
    }

    fn convert_val(&mut self, val: &definition::Val) -> assembly::Operand {
        match val {
            definition::Val::Const(i) => assembly::Operand::Immediate(*i),
            definition::Val::Var(ref s) => assembly::Operand::Pseudo(s.clone()),
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