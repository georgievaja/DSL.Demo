namespace DSL.Parser

open FParsec
open AbstractSyntaxTree
open ParserTypes.Monads
open ParserTypes.Types
open OperationParsers

module Parser =

    /// Parse rule and return ParsingResult with either Operation (if parsing succeeds)
    /// or OperationLog if parsing fails
    let parse (rule: string) : ParsingResult<Operation, OperationLog> = 
      match run pOperation rule with
       | ParserResult.Success (result, _, _) -> 
            SemanticValidation.validate result
       | ParserResult.Failure (_, error, _) -> 
            SyntacticLog.create rule error
            |> Syntactic
            |> Failure

                                                    

