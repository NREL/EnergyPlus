# Expressions

An expression is a sequence of variables and/or constants linked together by operators. An expression is always evaluated to a single value.

The rules for expressions are:

An expression is a sequence of variables and/or constants linked by operators.

Expressions always evaluate to a single value.

Comparison operators evaluate to 1.0 for "true" or 0.0 for "false."

Compound expressions are allowed and can be organized with parentheses.

The operators shown in Table 3 are available for use in Erl programs.

Table: Operators for Erl

Operator Symbol|Description|Evaluation Order|Example
---------------|-----------|----------------|-------
( )|Parentheses|left-to-right|SET z = 23/(3 + 2)
+|Addition|right-to-left|SET a = 4 + 5
-|Subtraction|right-to-left|SET b = a - 3
`*`|Multiplication|right-to-left|SET c = a \* b
/|Division|left-to-right|SET d = b/a
^|Raise to a power|left-to-right|SET e = c ^ 0.5
==|Equality comparison|left-to-right|IF a == b
\<\>|Inequality comparison|left-to-right|IF c \<\> d
>|Greater than comparison|left-to-right|IF a \> e
\>=|Greater than or equal to comparison|left-to-right|IF a \>= 6
\<|Less than comparison|left-to-right|IF b \< 2
\<=|Less than or equal to comparison|left-to-right|IF b \<= f
&&|Logical AND|right-to-left|IF c && d
\|\||Logical OR|right-to-left|IF c \|\| d

Because expressions can be evaluated to a single value, they can be used in SET and IF statements. That means both of the following instructions are allowed:

SET a = c \< d

IF a - 1

In the case of the SET example, the value of "a" is set to 1 if "c" is less than "d"; otherwise, it is set to 0. For the IF example, the IF block of instructions are executed if a – 1 is greater than zero.

Compound expressions allow multiple operators to be sequenced or nested. For example:

a + b \* 7 / 4.5

(a \* 3 + 4) ^ 2

(a \> b) && (c \< d)

For complicated expressions, it helps to make heavy use of parentheses in your equations.  By using parentheses with proper algebraic evaluation in mind to group terms, you can help the Erl parser.  The language processor is simplistic compared to a full-blown programming language and sometimes has problems applying the rules of algebra.  It is safer to err on the side of extra parentheses and to inspect the results of complex expressions in the EDD output.
