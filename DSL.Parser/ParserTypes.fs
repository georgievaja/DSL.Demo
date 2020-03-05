namespace DSL.Parser

open FParsec

module ParserTypes =
    module Types =
        
        /// Input string for parsing
        type InputString = private InputString of string
    
        /// Index of the unparsed string sequence
        type Index = Index of int64
    
        /// Line of the unparsed string sequence
        type Line = Line of int64
    
        /// Position of unparsed string sequence
        type ErrorPosition = private ErrorPosition of (Index * Line)
    
        /// Reason why the input string was not parsed
        type FailureText = private FailureText of string
    
        /// Failure message (when the input string was not parsed)
        type FailureMessage =
            | ExpectedValue of FailureText
            | ExpectedStringValue of FailureText
            | ExpectedCaseInsensitiveStringValue of FailureText
            | UnexpectedValue of FailureText
            | UnexpectedStringValue of FailureText
            | UnexpectedCaseInsensitiveStringValue of FailureText
            | MessageValue of FailureText
            | Other of FailureText
    
        type SemanticFailureType =
            | InvalidBooleanExpression
            | InvalidNumericIdentifier
            | InvalidNumericDataType
            | InvalidNumericExpression

        /// Operation log for failure occurrences
        type SyntacticLog =
            {
                InputString: InputString
                Position: ErrorPosition
                Messages: FailureMessage []
            }

        type SemanticLog = private SemanticLog of SemanticFailureType
    
        type OperationLog = 
            | Syntactic of SyntacticLog
            | Semantic of SemanticLog

        module FailureText =
            let create (msg: string) =
                FailureText msg
    
        module FailureMessage =
            let create (msg: ErrorMessage) =
                match msg with
                    | Expected e -> 
                        FailureText.create e
                        |> ExpectedValue
                    | ExpectedString e-> 
                        FailureText.create e
                        |> ExpectedStringValue
                    | ExpectedStringCI e ->
                        FailureText.create e
                        |> ExpectedCaseInsensitiveStringValue
                    | Unexpected e ->
                        FailureText.create e
                        |> UnexpectedValue
                    | UnexpectedString e ->
                        FailureText.create e
                        |> UnexpectedStringValue
                    | UnexpectedStringCI e ->
                        FailureText.create e
                        |> UnexpectedCaseInsensitiveStringValue
                    | Message e ->
                        FailureText.create e
                        |> MessageValue
                    | d -> 
                        FailureText.create (sprintf "Other parsing error happened: %s" (d.ToString()))
                        |> Other
    
        module InputString =
            let create (inputString: string) =
                InputString inputString
    
        module ErrorPosition =
            let create (position: Position) =
                ErrorPosition ((Index position.Index), (Line position.Line))
    
        module SyntacticLog =
            let create  (inputString: string) 
                        (error: ParserError) =
                {
                    InputString = InputString.create inputString
                    Position = ErrorPosition.create error.Position
                    Messages = error.Messages
                                |> ErrorMessageList.ToSortedArray
                                |> Array.map FailureMessage.create
                }

        module SemanticLog =
            let create (errorType: SemanticFailureType) =
                SemanticLog errorType
                
    module Monads =
        type ParsingResult<'TSuccess, 'TFailure> = 
            | Success of 'TSuccess
            | Failure of 'TFailure
    
        let EmptySuccess = Success ()

        let map f res =
            match res with      
                |Success (x) -> Success (f x)
                |Failure (x) -> Failure (x)
    
        let (<!>) x f = map f x

        let bind f x = 
            match x with
            | Success y -> f y
            | Failure f -> Failure f

        let (>>=) f x = bind f x

        let join (x: ParsingResult<ParsingResult<'a, 'b>, 'b>) =
            match x with 
            | Success s -> s
            | Failure f -> Failure f

        let combine 
            (f1: ParsingResult<'a,'g>) 
            (f2: ParsingResult<'b,'g>) : ParsingResult<('a*'b), 'g> = 
                match f1 with
                    | Success (x) -> match f2 with      
                                        |Success (x2) -> Success ((x, x2))
                                        |Failure (x2) -> Failure (x2)
                    | Failure (x) -> Failure (x)

        let (++) f1 f2 = combine f1 f2