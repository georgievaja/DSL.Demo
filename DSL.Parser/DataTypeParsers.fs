namespace DSL.Parser
open WhiteSpaceParsers
open FParsec
open AbstractSyntaxTree
open CharParsers

module NumericDataTypeParsers =
    let pDecimal : Parser<NumericDataType, unit> = 
        pfloatws
        |>> Float

module DataTypeParsers =   
    open NumericDataTypeParsers

    let pNumeric : Parser<DataType, unit> = 
        pDecimal
        |>> Numeric

    let pString : Parser<DataType, unit> =
        pStringLiteral
        |>> String

    let pBoolean : Parser<DataType, unit> =
        pBool
        |>> Boolean

    let pDataType : Parser<DataType, unit> =
        choicews [
            pNumeric;
            pBoolean;
            pString;
        ]

    let pDataTypeExpression : Parser<Expression, unit> =
        pDataType
        |>> DataType
        


        


