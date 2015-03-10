# Statement Keywords

Every programming language has instructions or commands that tell the processor what to do. Erl supports a few types of program statements. Each line of an Erl program begins with a statement keyword. The syntax for each line depends on the keyword that starts that line. Only those listed in Table 1 are allowed.

Table: Statement Keywords for Erl

----------------------------------------------------------------------------
 Keyword                Syntax              Statement Description
---------- -------------------------------- --------------------------------
   RUN           RUN \<program name\>,      Calls another Erl program or
                                            subroutine. Returns to the
                                            calling point when completed.
                                            Recursive calling is allowed.

  RETURN               RETURN,              Prematurely exits a subroutine
                                            or program and returns control
                                            to the caller.

   SET            SET \<variable\>          Assigns the right-hand side to
                  = \<expression\>,         the left-hand side. If \<
                                            variable\> has not been used
                                            before, it is dynamically
                                            declared (with local scope). **
                                            Note:\<variable\> should NOT
                                            start with numerics.**

    IF              IF\<expressio\>,        Begins an "IF block."
                                            Conditional decision. If\<
                                            expressio\> evaluates to
                                            anything other than zero, the
                                            block of statements after the
                                            IF is executed.

  ELSEIF          ELSEIF\<expressio\>,      Conditional decision that
                                            follows a regular IF block of
                                            instructions. If\<expressio\>
                                            evaluates to anything other
                                            than zero, the block of
                                            instructions after the ELSEIF
                                            is executed.

   ELSE                 ELSE,               Conditional decision.
                                            Associated with an IF
                                            statement, the block of
                                            statements after the ELSE is
                                            executed if\<expressio\>
                                            evaluates to zero for preceding
                                            IF and ELSEIF statements.

  ENDIF                 ENDIF,              Terminates IF block (required).

  WHILE           WHILE\<expressio\>,       Begins a "WHILE block."
                                            Conditional decision.  If\<
                                            expressio\> evaluates to
                                            anything other than zero, the
                                            block of statements after the
                                            WHILE is repeatedly executed.

 ENDWHILE             ENDWHILE,             Terminates WHILE block (
                                            required).
----------------------------------------------------------------------------

## Rules for IF blocks:

- IF blocks can be nested, but only up to five deep.
- ELSE is optional. If omitted, the IF block is terminated by ENDIF.

IF-ELSEIF-ELSE-ENDIF blocks are allowed. If there are many ELSEIF statements, the first in the list that evaluates to true (1.0) is applied and the execution jumps to the ENDIF for that IF block. If no IF or ELSEIF is true, the ELSE condition (if any) is applied. A single IF block currently has a limit of 199 ELSEIF statements plus one ELSE statement.

## Rules for WHILE blocks:

- WHILE blocks cannot be nested.
- A WHILE block must be terminated by an ENDWHILE
- The block is repeated until the expression following the WHILE statement evaluates to 0.0 (false).
- Once the WHILE loop is entered, there is a maximum limit on the number of times the loop can be repeated. The limit is set at one million repetitions to protect against the possibility of an Erl program entering an infinite loop if the WHILE loop's expression is malformed so as to never evaluate to 0.0 (false).

Erl programs are entered into the input data file (IDF) using the input objects called EnergyManagementSystem:Program and EnergyManagementSystem:Subroutine. These objects use individual fields to store the statements for an Erl program. As with most EnergyPlus objects, each field is separated by a comma and typically given a separate line of text for readability. In this case, each field can be considered a separate line of Erl program code. Every input field (line of code) must conform to the following rules:

Every input field contains only one statement.

Every field begins with a statement keyword that identifies what that particular line of code is doing.

The syntax for each statement depends on the keyword.

All field content (keywords, variable names, etc.) is case insensitive.

A comma (or semicolon if it is the last field) marks the end of every statement.

The maximum length for a field is 100 characters. If you enter a longer field, it will be truncated to the first 100 characters. This can have subtle effects if the remaining portion forms a viable expression.

The "!" character is for comments.

REMEMBER, every line needs to end in a comma or, if it is the last in the program, a semicolon.
