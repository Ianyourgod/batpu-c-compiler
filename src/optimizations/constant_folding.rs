use crate::tacky::definition;

pub struct ConstantFolding {
    body: Vec<definition::Instruction>,
}

impl ConstantFolding {
    pub fn new(body: Vec<definition::Instruction>) -> ConstantFolding {
        ConstantFolding {
            body,
        }
    }

    pub fn fold(&self) -> Vec<definition::Instruction> {
        let mut out = Vec::with_capacity(self.body.len());

        for instruction in &self.body {
            match instruction {
                definition::Instruction::Binary(ref op, ref src1, ref src2, ref dest) => {
                    let (is_const1, val1) = self.is_constant(src1);
                    let (is_const2, val2) = self.is_constant(src2);

                    if is_const1 && is_const2 {
                        let result = match op {
                            definition::Binop::Add => val1 + val2,
                            definition::Binop::Subtract => val1 - val2,
                            definition::Binop::And => val1 & val2,
                            definition::Binop::Or => val1 | val2,
                            definition::Binop::Equal => if val1 == val2 { 1 } else { 0 },
                            definition::Binop::NotEqual => if val1 != val2 { 1 } else { 0 },
                            definition::Binop::LessThan => if val1 < val2 { 1 } else { 0 },
                            definition::Binop::GreaterThan => if val1 > val2 { 1 } else { 0 },
                            definition::Binop::LessThanEqual => if val1 <= val2 { 1 } else { 0 },
                            definition::Binop::GreaterThanEqual => if val1 >= val2 { 1 } else { 0 },
                        };

                        out.push(definition::Instruction::Copy(dest.clone(), definition::Val::Const(result)));
                    } else {
                        out.push(instruction.clone());
                    }
                },
                _ => {
                    out.push(instruction.clone());
                }
            }
        }

        out
    }

    fn is_constant(&self, operand: &definition::Val) -> (bool, i8) {
        match operand {
            definition::Val::Const(val) => (true, *val),
            _ => (false, 0),
        }
    }
}