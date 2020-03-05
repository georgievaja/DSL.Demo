# DSL.Demo

Example code for designing the domain specific language, implementing a DSL parser using [FParsec](https://www.quanttec.com/fparsec/).

Includes the basic concepts of:
## DESIGNING THE LANGUAGE
    - Concrete syntax - wording, white spaces, symbols
    - Abstract syntax - concepts of language, relations to each other
    - Semantics - meaning of the sentence

## PARSING
    Two components in parsing: lexical analyzer and proper parsers
        ○ Lexers: transforms characters into atoms of meaning
        ○ Parsers: organizes tokens in the proper AST (transforming text written by humans in a more useful representation of the source code, an Abstract Syntax Tree)
    
## EXECUTION
    - Interpreter
        ○ An interpreter directly executes the language without transforming it in another form
