namespace DSL.Parser
open System
open ParserTypes.Monads
open ParserTypes.Types

module AbstractSyntaxTree =

    type NumericDataType =
        | Float of float

    type DataType =
        | Numeric of NumericDataType
        | String of string
        | Boolean of bool
    
    type TeamParameterValue =
        | Victories
        | PolePositions

    type TeamKey = TeamKey of string

    type IdentifierParameter =
        | TeamIdentifier of TeamKey
        | TeamParameter of TeamKey * TeamParameterValue

    type PredicateOperator =
        | Exists of Expression
        | In // to be specified
        | Between // to be specified
        | Any // to be specified
        | All // to be specified
        | Some // to be specified

    and LogicalOperator =
        | Not of Expression
        | And of Expression * Expression
        | Or of Expression * Expression

    and ArithmeticOperator =
        | Add of Expression * Expression
        | Subtract of Expression * Expression
        | Multiply of Expression * Expression
        | Divide of Expression * Expression
        | Modulo of Expression * Expression

    and ComparisionOperator =
        | EqualTo of Expression * Expression
        | GreaterThan of Expression * Expression
        | LessThan of Expression * Expression
        | GreaterThanEqualTo of Expression * Expression
        | LessThanEqualTo of Expression * Expression
        | NotEqualTo of Expression * Expression

    and Expression =
        | LogicalOperator of LogicalOperator
        | ComparisionOperator of ComparisionOperator
        | PredicateOperator of PredicateOperator
        | ArithmeticOperator of ArithmeticOperator
        | DataType of DataType
        | IdentifierParameter of IdentifierParameter

    type Operation =
        | Query of Expression

    module SemanticValidation =
        
        let validateNumericIdentifierParameter nip =
            match nip with
            | TeamParameter _ -> EmptySuccess
            | _ ->
                SemanticLog.create InvalidNumericIdentifier
                |> Semantic
                |> Failure

        let validateNumericDataType ndt =
            match ndt with
                | Numeric _ -> EmptySuccess
                | _ ->
                    SemanticLog.create InvalidNumericDataType
                    |> Semantic
                    |> Failure

        let validateNumericTypeExpression (exp: Expression) =
            match exp with 
            | ArithmeticOperator _ -> EmptySuccess
            | DataType dt -> validateNumericDataType dt
            | IdentifierParameter ip -> validateNumericIdentifierParameter ip
            | _ ->
                SemanticLog.create InvalidNumericExpression
                |> Semantic
                |> Failure

        let rec validateBooleanExpression (exp: Expression) : ParsingResult<unit, OperationLog> =
            match exp with
            | LogicalOperator lo -> validateLogicalOperator lo
            | ComparisionOperator co -> validateComparisionOperator co
            | PredicateOperator po -> validatePredicateOperator po
            | _ -> 
                SemanticLog.create InvalidBooleanExpression
                |> Semantic
                |> Failure

        and validateLogicalOperator (lo: LogicalOperator) =
            EmptySuccess
                            
        and validateComparisionOperator (lo: ComparisionOperator) =
            EmptySuccess

        and validatePredicateOperator (lo: PredicateOperator) =
            EmptySuccess

        let validate (operation: Operation) : ParsingResult<Operation, OperationLog> =
            match operation with 
                | Query q -> validateBooleanExpression q            
            <!> fun _ -> operation