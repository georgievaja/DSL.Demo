# Operators Syntax

	- An operator manipulates individual data items and returns a result

## Arithmetic operators

*+ (Add)*
	- Addition
	- Operates on numeric values
*- (Subtract)*
	- Subtraction
	- Operates on numeric values
** (Multiply)*
	- Multiplication
	- Operates on numeric values
*/ (Divide)*
	- Division
	- Operates on numeric values
*% (Modulo)*
	- Returns the integer remainder of a division. For example, 17 % 5 = 2 because the remainder of 17 divided by 5 is 2
	- Operates on numeric values

## Comparison operators

*=*
	- Equal to
	- Operates on any compatible data types
*>*
	- Greater than
	- Operates on any compatible data types
*<*
	- Less than
	- Operates on any compatible data types
*>=*
	- Greater than equal to
	- Operates on any compatible data types
*<=*
	- Less than equal to
	- Operates on any compatible data types
*<>*
	- Not equal to
	- Operates on any compatible data types

## Logical operators
- The following operators perform logical operations with the bool operands:
- The Logical operators are those that are true or false
- They return a true or false values to combine one or more true or false values
	
*NOT*
	- The logical negation operator (!) reverses the meaning of its operand

*AND*
	- The & operator computes the logical AND of its operands. The result of x & y is true if both x and y evaluate to true. Otherwise, the result is false

*OR*
	- The | operator computes the logical OR of its operands. The result of x | y is true if either x or y evaluates to true. Otherwise, the result is false

## Predicate operators

*IN*
	- The IN operator checks a value within a set of values separated by commas and retrieve the rows from the table which are matching
	- Operates on any set of values of the same datatype

*BETWEEN*
	- The BETWEEN operator tests an expression against a range
	- The range consists of a beginning, followed by an AND keyword and an end expression
	- Operates on numeric, characters, or datetime values

*ANY*
	- ANY compares a value to each value in a list or results from a query and evaluates to true if the result of an inner query contains at least one row
	- Operates on a value to a list or a single - columns set of values

*ALL*
	- ALL is used to select all records of a QUERY STATEMENT
	- It compares a value to every value in a list or results from a query
	- The ALL must be preceded by the comparison operators and evaluates to TRUE if the query returns no rows
	- Operates on a value to a list or a single - columns set of values

*SOME*
	- SOME compare a value to each value in a list or results from a query and evaluate to true if the result of an inner query contains at least one row
	- Operates on a value to a list or a single - columns set of values

*EXISTS*
	- The EXISTS checks the existence of a result of a subquery
	- The EXISTS subquery tests whether a subquery fetches at least one row
	- When no data is returned then this operator returns false
	- Operates on data collection

## Operator precedence

- Precedence is the order in which database evaluates different operators in the same expression
- When evaluating an expression containing multiple operators (e.g. +, -, /), operator precedence evaluates operators with higher precedence before evaluating those with lower precedence
- Operator precedence evaluates operators with equal precedence from left to right within an expression
- If there are parentheses within the expression then it evaluated first and the rest part which are outside the parentheses are evaluated next
- The following table lists the levels of precedence among SQL operators from high to low

1. ( ) (parenthetical expressions)
3. *, /, % (mathematical operators)
4. +, - (arithmetic operators)
5. =, >, <, >=, <=, <> (comparison operators)
6. NOT
7. AND
8. ALL, ANY, BETWEEN, IN, LIKE, OR, SOME