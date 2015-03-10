# Defining Blocks of Input

The  **##def** command allows a block of input text to be defined and given a name. The block of text can then be inserted anywhere in the EnergyPlus input stream by simply referencing the name of the block. (This process is called macro expansion.)  The block can have parameters (also called arguments) that can be given different values each time the block is referenced.

The syntax of the  **##def** command is as follows:

~~~~~~~~~~~~~~~~~~~~
            unique name
               |         zero or more arguments    __ macro text
               |            |                     |
               |            |                     |

        ##def  macro-name [ arg1 arg2,arg3 ...]   text line 1

      |_     |           | |    |_   |   _|     |
        |    |           | |      |  |  |       |__ one
        |    |           | |      |  |  |           or
    zero    one       zero_|     space(s)           more
    or      or        or         or comma           spaces
    more    more      more
    spaces  spaces    spaces
~~~~~~~~~~~~~~~~~~~~

Example:    Define a schedule macro with name "All_Const":

~~~~~~~~~~~~~~~~~~~~
    ##def All_Const[x]
            Fraction, WeekON, 1,1, 12,31;
        WEEKSCHEDULE, WeekON,
            DayON,DayON,DayON,
            DayON,DayON,DayON,
            DayON,DayON,DayON,
            DayON,DayON,DayON;
        DAYSCHEDULE, DayON, Fraction,
            x,x,x,x,x,x,x,x,x,x,x,x,
            x,x,x,x,x,x,x,x,x,x,x,x ;
    ##enddef
~~~~~~~~~~~~~~~~~~~~

Then, in the EnergyPlus input stream (file in.imf), when we write :

~~~~~~~~~~~~~~~~~~~~
    SCHEDULE, Constant At 0.8, All_Const[0.8]
~~~~~~~~~~~~~~~~~~~~

the result (file out.idf) is equivalent to:

~~~~~~~~~~~~~~~~~~~~
    SCHEDULE, Constant At 0.8,
            Fraction, WeekON, 1,1, 12,31;
        WEEKSCHEDULE, WeekON,
            DayON,DayON,DayON,
            DayON,DayON,DayON,
            DayON,DayON,DayON,
            DayON,DayON,DayON;
        DAYSCHEDULE, DayON, Fraction,
            0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,
            0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8 ;
~~~~~~~~~~~~~~~~~~~~

Macro definitions may have one or more arguments; the maximum number of arguments is 32. When a macro with arguments is referenced, its arguments must be given values. When a macro has no arguments, the brackets are still required both for macro definition and reference.

> Caution: Square brackets [  ] have been used in some versions of EnergyPlus inputs as comment/units fields. These will be expanded if left in the IDF and sent to EPMacro.

Macro names must be unique (except see **##set1** below); i.e., when a macro name is defined it cannot be defined again. Macro names are limited to 40 characters.

To summarize, commands you use to define macros are the following:

**##def**   macro-name  [**arg1**,..,**argn** ]  macro-text

Defines a macro with the name macro-name  and arguments "arg1" through "arg*n*". "Macro-text" is one or more lines of text. If there are no arguments, the syntax is **##def** macro-name macro-text.

**##enddef**

**Indicates the end of the macro definition initiated by ##def.**

**##def1**    macro-name  [**arg1**,..,**argn** ]  macro-text

This is the same as  **##def** but there is only one line of text so that the terminating command  **##enddef** is not required.

**##set1**    macro-name   macro-text

Like  **##def1**  but has no arguments and macro-text is evaluated before storing. "Macro-text is evaluated" means that if macro-text contains other macros, these macros will be expanded, and the expanded text becomes the macro-text defined by  **##set1**.

Example:

~~~~~~~~~~~~~~~~~~~~
                        ##def1   xx  123
                        ##set1   yy  xx[]

    is equivalent to:   ##set1   yy  123

    ##set1  can also be used to redefine macro-name.

    ##set1   x   0
      .
      .
      .
    ##set1   x    #eval[ x[]+1 ]

    (see Arithmetic Operations for description of the #eval macro.)
~~~~~~~~~~~~~~~~~~~~

## Arithmetic Operations

The built-in macro called  **#eval[ ]**  can be used to perform arithmetic, literal, and logical operations. It can be abbreviated to **# [ ].**

eval[ X OP Y  ]  or  #[ X OP Y ] 

gives the result  X OP Y. The allowed values for X, OP, and Y, and the corresponding result, are shown in the following table.

-----------------------------------------------------------------------
X *       OP **              Y                 Result
--------- ------------------ ----------------- ------------------------
number    + (plus)           number            number

number    - (minus)          number            number

number    * (times)          number            number

number    / (divided by)     number            number

number    min                number            number

number    max                number            number

number    mod                number            number

number    ** (power)         number            number

SIN       OF                 number (degrees)  number

COS       OF                 number (degrees)  number

TAN       OF                 number (degrees)  number

SQRT      OF                 number            number

ABS       OF                 number            number

ASIN      OF                 number            number (degrees)

ACOS      OF                 number            number (degrees)

ATAN      OF                 number            number

INT       OF                 number            number

LOG10     OF                 number            number

LOG       OF                 number            number

literal1  // (concatenate)   literal2          literal "
                                               literal1literal2"

literal1  /// (concatenate)  literal2          literal "literal1
                                               literal2"

literal   EQS (=)            literal           logical (true or false)
                                               case sensitive

literal   NES ()             literal           logical (true or false)
                                               case sensitive

literal   EQSU (=)           literal           logical (true or false)
                                               not case sensitive

literal   NESU ()            literal           logical (true or false)
                                               not case sensitive

logical   AND                logical           logical (true or false)

logical   OR                 logical           logical (true or false)

--        NOT                logical           logical (true or false)

number    EQ (=)             number            logical (true or false)

number    NE ()              number            logical (true or false)

number    GT (>)             number            logical (true or false)

number    GE ()              number            logical (true or false)

number    LT (<)             number            logical (true or false)

number    LE ()              number            logical (true or false)
-----------------------------------------------------------------------

| \* Upper or lower case is allowed for SIN, COS, etc.
| \*\*  Upper or lower case is allowed for OF, EQS, etc.

Example

**#eval[**  1 + 2  **]**     when expanded becomes 3.

**#eval[** 1 +  **#eval**[2 \* 3] **]**  when expanded becomes 7.

Example

**##set1** city[] Washington

  DesignDay, #[ city[ ] /// SUMMER ], ! Design Day Name

gives

     DesignDay, "Washington SUMMER", ! Design Day Name

The following example illustrates the use of **#eval**  inside  **#if**  commands:

**##if**  **#[** city[ ] EQS  Chicago **]**

**##if**  **#[#[** city[ ] EQS  Chicago ] and **#[** occup[ ] NES low **]** **]**

Notes:

1.For logical values:

False = 0 or BLANK,

True = any other character

2.A literal must be enclosed inside a pair of double quotes if it contains BLANKs or reserved characters like [ ]  ( )  ,

E.g., "abc \*def"

Otherwise, the quotes around the literals are optional.

3.Literal concatenation operators // and /// produce quoted literals.

E.g.,  #  [ large  ///  office ]   gives  "large office"

4.Literals are case sensitive. For example, "Chicago", "CHICAGO" and "chicago" are distinct.

5.EQS and NES are case sensitive string comparisons. EQSU and NESU are case insensitive string comparisons.

6.Literals are limited to 40 characters.