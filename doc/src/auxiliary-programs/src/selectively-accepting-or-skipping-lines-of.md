# Selectively Accepting or Skipping Lines of Input

The **##if** series of commands is used to selectively accept or skip lines of input according to the following sequence:

~~~~~~~~~~~~~~~~~~~~
    ##if  {condition1}
      line1a
      line1b
    ….

    ##elseif  {condition2}
      line2a
      line2b

    ##elseif  {condition3}
      line3a
      line3b
    ….

    ##else
      line N a
      line N b
    ….

    ##endif
~~~~~~~~~~~~~~~~~~~~

Then the lines that will be included into the EnergyPlus input stream are:

~~~~~~~~~~~~~~~~~~~~
    If {condition 1} is TRUE,
      line1a
      line1b
    ….
    otherwise

    If {condition 2} is TRUE,
      line2a
      line2b
    ….
    otherwise

    If {condition 3} is TRUE,
      line3a
      line3b
    ….

    otherwise

    If {condition 1}, {condition 2}, {condition 3} are all FALSE.
      line N a
      line N b
    ….
~~~~~~~~~~~~~~~~~~~~

There are six different **##if...** commands:

Command | -- | Result
--------|----|-------
**##ifdef**|{macro name}|:  if macro name defined, include following lines
**##ifndef**|{macro name}|:  if macro name NOT defined, include following lines
**##if**|{condition}|:  if condition is TRUE, include following lines
**##elseif**|{condition}|:  if condition is TRUE, and previous conditions are FALSE, include   |   following lines
**##else**|--|:  if all previous conditions are FALSE, include following lines
**##endif**|--|:  indicates the end of the if block

Notes:
{macro name} is explained in section *Defining Blocks of Input*, below.
{condition} is 0 or BLANK meaning FALSE, and any other character meaning TRUE.
**##ifdef**  and  **##ifndef**  do not have corresponding  **##elseif**  commands, but they do have
corresponding  **##else**  and  **##endif**  commands.