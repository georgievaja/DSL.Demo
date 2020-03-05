namespace DSL.Interpreter

open DSL.Teams.Team.PublicTypes
open DSL.Teams.Team.Monads
open DSL.Teams.Team.Logs

module InterpreterTypes =
    module Types =
    
        type UnexpectedFailureType =
            | UnsupportedExistsExpression
            | UnsupportedPredicateOperator
            | UnsupportedQueryExpression
    
        type InterpreterFailure =
            | Unexpected of UnexpectedFailureType
    
    module FunctionTypes = 

        type OperationFunction = UnvalidatedTeams -> OperationFunctionResult<bool, FunctionFailure>

    module Monads =

        type InterpreterResult<'TSuccess, 'TFailure> = 
            | Success of 'TSuccess
            | Failure of 'TFailure
    
        let map f res =
            match res with      
                |Success (x, log) -> Success (f x, log)
                |Failure (x, log) -> Failure (x, log)
    
        let (<!>) x f = map f x
    

