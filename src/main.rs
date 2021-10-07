use std::fmt::{Debug, Display, Formatter};
use std::io::{stdin, stdout, Write};
use fraction::{Fraction, Zero};
use ndarray::Array2;
use pest_derive::Parser;
use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};

type Result<T> = ::std::result::Result<T, MathError>;
type Number = Fraction;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct MatrixOpsParser;


fn main() -> Result<()> {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::mul, Assoc::Left) | Operator::new(Rule::div, Assoc::Left),
    ]);

    let mut input = String::new();

    let mut matrix = {
        print!("Enter matrix dimensions (e.g 3x5): ");
        stdout().flush().unwrap();
        stdin().read_line(&mut input).unwrap();
        let mut parsed = MatrixOpsParser::parse(Rule::matrix_dims, &input).map_err(MathError::InvalidDimensions)?;
        let mut dims = parsed.next().unwrap().into_inner();
        let mut next = || dims.next().unwrap().as_str().parse().unwrap();
        let (rows, cols): (usize, usize) = (next(), next());
        Array2::<Number>::zeros((rows, cols))
    };

    println!("Enter matrix lines (each line e.g `1, -1, 2, +2, +0`):");

    let mut row = 0;
    let mut num_elems = 0;

    while row < matrix.dim().0 {
        print!("> ");
        stdout().flush().unwrap();
        input.clear();
        stdin().read_line(&mut input).unwrap();

        let mut parsed = match MatrixOpsParser::parse(Rule::matrix_row, &input) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Error parsing line:\n{}", e);
                continue;
            }
        };

        let pairs = parsed.next().unwrap().into_inner();

        for (col, item) in pairs.enumerate() {
            if item.as_rule() == Rule::EOI {
                break;
            }

            num_elems += 1;

            matrix[(row, col)] = evaluate_constant_operation(&climber, item.into_inner())?;
        }

        row += 1;
    }

    if num_elems != (matrix.dim().0 * matrix.dim().1) {
        return Err(MathError::WrongNumberValues);
    }

    println!("Welcome to the evaluation console!");
    println!("Try an expression like: 4R1 + R2 / (2 * 3 / 4) => R1");

    loop {
        print!(">");
        stdout().flush().unwrap();
        input.clear();
        stdin().read_line(&mut input).unwrap();
        let parsed = match MatrixOpsParser::parse(Rule::stmt, &input) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Error parsing statement:\n{}", e);
                continue;
            }
        };

        for op in parsed {
            visit_row_ops(&climber, &mut matrix,op.into_inner());
        }
    }
}

enum MathError {
    MainRowCoefficientZero,
    MainRowNotMentioned,
    DivByZero,
    InvalidDecimal(fraction::error::ParseError),
    InvalidDimensions(pest::error::Error<Rule>),
    WrongNumberValues,
}

impl Debug for MathError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for MathError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use MathError::*;

        let mut p = |s| f.write_str(s);

        match self {
            MainRowCoefficientZero => p("The coefficient of the row of the operation cannot be 0"),
            MainRowNotMentioned => p("The row of the operation is not mentioned on the LHS"),
            DivByZero => p("Division by zero"),
            InvalidDecimal(e) => write!(f, "Invalid decimal format:{}", e),
            InvalidDimensions(e) => write!(f, "Invalid matrix dimensions:\n{}", e),
            WrongNumberValues => p("Invalid matrix form: values do not fit dimensions")
        }
    }
}

fn evaluate_constant_operation(climber: &PrecClimber<Rule>, pairs: Pairs<'_, Rule>) -> Result<Number> {
    let primary = |pair: Pair<'_, Rule>| {
        evaluate_constant_operand(climber, pair.into_inner())
    };

    let infix = |lhs: Result<Number>, op: Pair<Rule>, rhs: Result<Number>| {
        let (lhs, rhs) = (lhs?, rhs?);

        let res = match op.as_rule() {
            Rule::plus => lhs + rhs,
            Rule::minus => lhs - rhs,
            Rule::mul => lhs * rhs,
            Rule::div => if !rhs.is_zero() {
                lhs / rhs
            } else {
                return Err(MathError::DivByZero);
            },
            _ => unreachable!()
        };

        Ok(res)
    };

    climber.climb(pairs, primary, infix)
}

fn evaluate_constant_operand(climber: &PrecClimber<Rule>, pairs: Pairs<'_, Rule>) -> Result<Fraction> {
    let inner = pairs.into_iter().next().unwrap();

    if inner.as_rule() == Rule::constant {
        Number::from_decimal_str(inner.as_str()).map_err(MathError::InvalidDecimal)
    } else {
        evaluate_constant_operation(climber, inner.into_inner())
    }
}

fn visit_row_ops(climber: &PrecClimber<Rule>, matrix: &mut Array2<Number>, pairs: Pairs<'_, Rule>) {
    for (step, op) in pairs.enumerate() {
        if op.as_rule() == Rule::row_operation {
            println!("Step {}: evaluating operation: {}", step + 1, op.as_str());

            if let Err(e) = evaluate_row_operation(climber, matrix, op.into_inner()) {
                eprintln!("{}", e);
            }

            println!("{}\n", matrix);
        }
    }
}

fn evaluate_row_operation(
    climber: &PrecClimber<Rule>,
    matrix: &mut Array2<Number>,
    mut pairs: Pairs<'_, Rule>
) -> Result<()> {
    let row_expr = pairs.next().unwrap();
    let store_to_row = pairs.next().unwrap().as_str();
    let row_idx = store_to_row[1..].parse::<usize>().unwrap() - 1;

    let vals = evaluate_row_expression(climber, &matrix, store_to_row, row_expr.into_inner())?;

    for (col, val) in vals.into_iter().enumerate() {
        matrix[(row_idx, col)] = val;
    }

    Ok(())
}

fn evaluate_row_expression(
    climber: &PrecClimber<Rule>,
    matrix: &Array2<Number>,
    main_row: &str,
    mut pairs: Pairs<'_, Rule>
) -> Result<Vec<Number>> {
    let (name, mut acc) = evaluate_row_with_coefficient(climber, main_row, matrix, pairs.next().unwrap().into_inner())?;

    let mut mentions_main_row = name == main_row;

    while pairs.peek().is_some() {
        let op = pairs.next();
        let second = pairs.next();

        match (op, second) {
            (Some(op), Some(second)) if second.as_rule() == Rule::row_with_coefficient => {
                let (name, second) = evaluate_row_with_coefficient(climber, main_row, matrix, second.into_inner())?;

                let elementwise_fn = match op.as_rule() {
                    Rule::plus => |(a, b)| a + b,
                    Rule::minus => |(a, b)| a - b,
                    other => unreachable!("{:?}", other),
                };

                if name == main_row {
                    mentions_main_row = true;
                }

                acc = acc.into_iter().zip(second).map(elementwise_fn).collect();
            },
            (None, None) => break,
            other => unreachable!("{:?}", other),
        }
    }

    if !mentions_main_row {
        Err(MathError::MainRowNotMentioned)
    } else {
        Ok(acc)
    }
}

fn evaluate_row_with_coefficient<'a>(
    climber: &PrecClimber<Rule>,
    main_row: &str,
    matrix: &Array2<Number>,
    pairs: Pairs<'a, Rule>
) -> Result<(&'a str, Vec<Number>)> {
    let mut coeff = Number::from(1);
    let mut coeff_is_inv = false;
    let mut row_name = None;

    for pair in pairs {
        if pair.as_rule() == Rule::row {
            row_name = Some(pair.as_str());
        } else if pair.as_rule() == Rule::div {
            coeff_is_inv = true;
        } else if pair.as_rule() == Rule::constant_operation {
            coeff = evaluate_constant_operation(climber, pair.into_inner())?;
        } else if pair.as_rule() == Rule::constant_operand {
            coeff = evaluate_constant_operand(climber, pair.into_inner())?;
        };
    }

    let row_name = row_name.unwrap();

    if coeff.is_zero() && row_name == main_row {
        return Err(MathError::MainRowCoefficientZero);
    }

    if coeff_is_inv && coeff.is_zero() {
        return Err(MathError::DivByZero);
    }

    if coeff_is_inv {
        coeff = coeff.recip()
    }

    let row_view = matrix.row(row_name[1..].parse::<usize>().unwrap() - 1);
    Ok((row_name, row_view.into_iter().map(|it| it * &coeff).collect()))
}
