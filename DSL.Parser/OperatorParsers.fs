namespace DSL.Parser

open WhiteSpaceParsers
open FParsec
open AbstractSyntaxTree
open CharParsers
open IdentifierParsers
open DataTypeParsers

module OperatorParsers =
    let oppa = new OperatorPrecedenceParser<Expression, unit, unit>()

    let pOperator = oppa.ExpressionParser

    oppa.TermParser <-  
        choicews  [
            pIdentifierExpression;
            pDataTypeExpression;
            pParenthesisOf pOperator;
        ]   
    
    // Arithmetic operators
    oppa.AddOperator(InfixOperator("*", spaces, 6, Associativity.Left,  fun x y -> ArithmeticOperator(Multiply(x, y))))
    oppa.AddOperator(InfixOperator("/", spaces, 6, Associativity.Left,  fun x y -> ArithmeticOperator(Divide(x, y))))
    oppa.AddOperator(InfixOperator("%", spaces, 6, Associativity.Left,  fun x y -> ArithmeticOperator(Modulo(x, y))))
    oppa.AddOperator(InfixOperator("+", spaces, 5, Associativity.Left,  fun x y -> ArithmeticOperator(Add(x, y))))
    oppa.AddOperator(InfixOperator("-", spaces, 5, Associativity.Left,  fun x y -> ArithmeticOperator(Subtract(x, y))))

    // Comparision operators
    oppa.AddOperator(InfixOperator("=", spaces, 4, Associativity.Left,  fun x y -> ComparisionOperator(EqualTo(x, y))))
    oppa.AddOperator(InfixOperator(">", spaces, 4, Associativity.Left,  fun x y -> ComparisionOperator(GreaterThan(x, y))))
    oppa.AddOperator(InfixOperator("<", spaces, 4, Associativity.Left,  fun x y -> ComparisionOperator(LessThan(x, y))))
    oppa.AddOperator(InfixOperator(">=", spaces, 4, Associativity.Left,  fun x y -> ComparisionOperator(GreaterThanEqualTo(x, y))))
    oppa.AddOperator(InfixOperator("<=", spaces, 4, Associativity.Left,  fun x y -> ComparisionOperator(LessThanEqualTo(x, y))))
    oppa.AddOperator(InfixOperator("<>", spaces, 4, Associativity.Left,  fun x y -> ComparisionOperator(NotEqualTo(x, y))))

    // Logical operators
    oppa.AddOperator(PrefixOperator("NOT", spaces, 3, true, fun x -> LogicalOperator(Not(x))))
    oppa.AddOperator(InfixOperator("AND", spaces, 2, Associativity.Left,  fun x y -> LogicalOperator(And(x, y))))
    oppa.AddOperator(InfixOperator("OR", spaces, 1, Associativity.Left,  fun x y -> LogicalOperator(Or(x, y))))

    // Predicate operators
    oppa.AddOperator(PrefixOperator("EXISTS", spaces, 3, true, fun x -> PredicateOperator(Exists(x))))
