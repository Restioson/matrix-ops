use std::str::FromStr;
use fraction::{Fraction, Zero};
use ndarray::Array2;
use pest_derive::Parser;
use pest::Parser;
use pest::iterators::{Pairs};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct MyParser;

const OPERATIONS: &str = "
R2 - 3R1 => R2;
R3 - 2R1 => R3;
R4 - R1 => R4;

R4 - R2 => R4;
R2 / 4 => R2;
R3 - 3R2 => R3;
R3 / (7/4) => R3;
R4 - 6R3 => R4;
R3 + 6R4 => R3;
R4 / (-1/7) => R4;
R2 + (5/4)R3 + (6/4)R4 => R2;
R1 + R2 - 2R3 - 2R4 => R1;
";

const MATRIX: &str = "
1, -1, 2, 2, +0;
3, +1, 1, 0, +2;
2, +1, 2, 1, -4;
1, +3, 3, 1, +3;
";

fn main() {
    let mut matrix = match MyParser::parse(Rule::matrix_dims, "4x5") {
        Ok(mut parsed) => {
            let mut dims = parsed.next().unwrap().into_inner();
            let mut next = || dims.next().unwrap().as_str().parse().unwrap();
            let (rows, cols): (usize, usize) = (next(), next());
            Array2::<Fraction>::zeros((rows, cols))
        }
        Err(e) => panic!("{}", e),
    };

    match MyParser::parse(Rule::matrix, MATRIX) {
        Ok(mut parsed) => {
            let pairs = parsed.next().unwrap().into_inner();
            let mut num = 0;

            for (idx, item) in pairs.enumerate() {
                if item.as_rule() == Rule::EOI {
                    break;
                }

                num += 1;
                let val = evaluate_constant_operation(item.into_inner());

                let row = idx / matrix.dim().1;
                let col = idx % matrix.dim().1;

                matrix[(row, col)] = val;
            }

            if num != (matrix.dim().0 * matrix.dim().1) {
                panic!("Invalid matrix form");
            }
        }
        Err(e) => panic!("{}", e),
    };

    println!("Matrix:\n{}\n", matrix);

    match MyParser::parse(Rule::stmt, OPERATIONS) {
        Ok(parsed) => {
            for op in parsed {
                visit_row_ops(&mut matrix,op.into_inner())
            }
        }
        Err(e) => panic!("{}", e),
    }
}

#[derive(Copy, Clone, Debug)]
enum ConstantOperator {
    Add,
    Sub,
    Mul,
    Div,
}

impl FromStr for ConstantOperator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use ConstantOperator::*;

        Ok(match s {
            "+" => Add,
            "-" => Sub,
            "*" => Mul,
            "/" => Div,
            _ => return Err(()),
        })
    }
}

impl ConstantOperator {
    fn apply(self, a: Fraction, b: Fraction) -> Fraction {
        use ConstantOperator::*;

        match self {
            Add => a + b,
            Sub => a - b,
            Mul => a * b,
            Div => a / b,
        }
    }
}

#[derive(Copy, Clone)]
enum RowAndRowOperator {
    Add,
    Sub,
}

impl FromStr for RowAndRowOperator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use RowAndRowOperator::*;

        Ok(match s {
            "+" => Add,
            "-" => Sub,
            _ => return Err(()),
        })
    }
}

impl RowAndRowOperator {
    fn apply(self, a: Vec<Fraction>, b: Vec<Fraction>) -> Vec<Fraction> {
        use RowAndRowOperator::*;

        let func = match self {
            Add => |(a, b)| a + b,
            Sub => |(a, b)| a - b,
        };

        a.into_iter().zip(b).map(func).collect()
    }
}

fn evaluate_constant_operation(mut pairs: Pairs<'_, Rule>) -> Fraction {
    let first = pairs.next().unwrap();

    let mut acc = evaluate_constant_operand(first.into_inner());

    while pairs.peek().is_some() {
        let op = pairs.next();
        let second = pairs.next();

        match (op, second) {
            (Some(op), Some(second)) => {
                let op: ConstantOperator = op.as_str().parse().unwrap();
                let second = evaluate_constant_operand(second.into_inner());
                acc = op.apply(acc, second);
            },
            _ => return acc,
        }
    }

    acc
}

fn evaluate_constant_operand(pairs: Pairs<'_, Rule>) -> Fraction {
    let inner = pairs.into_iter().next().unwrap();

    if inner.as_rule() == Rule::constant {
        Fraction::from_decimal_str(inner.as_str()).unwrap()
    } else {
        evaluate_constant_operation(inner.into_inner())
    }
}

fn visit_row_ops(matrix: &mut Array2<Fraction>, pairs: Pairs<'_, Rule>) {
    for (step, op) in pairs.enumerate() {
        if op.as_rule() == Rule::row_operation {
            println!("Step {}: evaluating operation: {}", step + 1, op.as_str());
            evaluate_row_operation(matrix, op.into_inner());

            println!("{}\n", matrix);
        }
    }
}

fn evaluate_row_operation(matrix: &mut Array2<Fraction>, mut pairs: Pairs<'_, Rule>) {
    let row_expr = pairs.next().unwrap();
    let store_to_row = pairs.next().unwrap().as_str();
    let row_idx = store_to_row[1..].parse::<usize>().unwrap() - 1;

    let (mentions_row, vals) = evaluate_row_expression(&matrix, store_to_row, row_expr.into_inner());

    if !mentions_row {
        panic!("Does not mention row being stored into!");
    }

    for (col, val) in vals.into_iter().enumerate() {
        matrix[(row_idx, col)] = val;
    }
}

fn evaluate_row_expression(
    matrix: &Array2<Fraction>,
    test_mentions_row: &str,
    mut pairs: Pairs<'_, Rule>
) -> (bool, Vec<Fraction>) {
    let (name, mut acc) = evaluate_row_with_coefficient(matrix, pairs.next().unwrap().into_inner());

    let mut mentions_row = name == test_mentions_row;

    while pairs.peek().is_some() {
        let op = pairs.next();
        let second = pairs.next();

        match (op, second) {
            (Some(op), Some(second)) if second.as_rule() == Rule::row_with_coefficient => {
                let op: RowAndRowOperator = op.as_str().parse().unwrap();
                let (name, second) = evaluate_row_with_coefficient(matrix, second.into_inner());

                if name == test_mentions_row {
                    mentions_row = true;
                }

                acc = op.apply(acc, second);
            },
            _ => return (mentions_row, acc),
        }
    }

    (mentions_row, acc)
}

fn evaluate_row_with_coefficient<'a>(
    matrix: &Array2<Fraction>,
    pairs: Pairs<'a, Rule>
) -> (&'a str, Vec<Fraction>) {
    let mut coeff = Fraction::from(1);
    let mut coeff_is_inv = false;
    let mut row_name = None;

    for pair in pairs {
        if pair.as_rule() == Rule::row {
            row_name = Some(pair.as_str());
        } else if pair.as_rule() == Rule::row_and_constant_operator {
            if pair.as_str() == "/" {
                coeff_is_inv = true;
            }
        } else if pair.as_rule() == Rule::constant_operation {
            coeff = evaluate_constant_operation(pair.into_inner())
        } else if pair.as_rule() == Rule::constant_operand {
            coeff = evaluate_constant_operand(pair.into_inner())
        };
    }

    let row_name = row_name.unwrap();

    if coeff.is_zero() {
        panic!("Coefficient is zero");
    }

    if coeff_is_inv {
        coeff = coeff.recip()
    };

    let row_view = matrix.row(row_name[1..].parse::<usize>().unwrap() - 1);
    let row = row_view.into_iter().map(|it| it * &coeff).collect();

    (row_name, row)
}
