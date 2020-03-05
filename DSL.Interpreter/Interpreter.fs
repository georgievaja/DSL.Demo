namespace DSL.Interpreter
open DSL.Parser.AbstractSyntaxTree
open DSL.Teams.TeamFunctions
open DSL.Teams.Team.Monads
open DSL.Interpreter.InterpreterTypes.Types
open DSL.Interpreter.InterpreterTypes.FunctionTypes
open DSL.Interpreter.InterpreterTypes.Monads

module Interpreter =

    let interpretExistsOperator (exp: Expression) =
        match exp with
            | IdentifierParameter ip -> 
                match ip with
                | TeamIdentifier (TeamKey t) ->
                    TeamExistsFunctions.workflow t
                    |> Success
                | _ -> 
                    Unexpected UnsupportedExistsExpression
                    |> Failure
            | _ -> 
                Unexpected UnsupportedExistsExpression
                |> Failure


    let interpretPredicateOperator (op: PredicateOperator) : InterpreterResult<OperationFunction, InterpreterFailure> =
        match op with 
            | Exists exp -> interpretExistsOperator exp
            | _ -> 
                Unexpected UnsupportedPredicateOperator
                |> Failure

    let interpretLogicalOperator (lo: LogicalOperator) : InterpreterResult<OperationFunction, InterpreterFailure> =
        Success (fun t -> OperationFunctionResult.Success (true, [])) // dummy

    let interpretComparisionOperator (lo: ComparisionOperator) : InterpreterResult<OperationFunction, InterpreterFailure> =
        Success (fun t -> OperationFunctionResult.Success (true, [])) // dummy

    /// Executes the language
    /// Accepts Abstract Syntax Tree and returns particular function in elevated type
    let interpret (op: Operation) : InterpreterResult<OperationFunction, InterpreterFailure> =        
        match op with
        | Query q -> match q with
                        | ComparisionOperator co -> interpretComparisionOperator co
                        | LogicalOperator lo -> interpretLogicalOperator lo
                        | PredicateOperator po -> interpretPredicateOperator po
                        | _ -> 
                            Unexpected UnsupportedQueryExpression
                            |> Failure

                                