namespace DSL.Parser

open WhiteSpaceParsers
open FParsec
open AbstractSyntaxTree
open CharParsers
open OperatorParsers

module OperationParsers =

    let pQuery : Parser<Operation, unit> = 
        pAfterString pOperator "query"
        |>> Query

    let pOperation : Parser<Operation, unit> = 
        choicews [
            pQuery;
        ]
