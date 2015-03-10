# Debugging Strategies

This section attempts to provide some debugging tips.

There is no debugging environment, so the main way to obtain information is to use verbose mode and trace each line.

Say, for example, we are trying to debug the following line:

~~~~~~~~~~~~~~~~~~~~
     ELSEIF (Hour >= 5) && (Hour < 19)  && (DayOfWeek >=2) && (DayOfWeek <=6) ,
~~~~~~~~~~~~~~~~~~~~

The line trace, shown next, shows only the result of the logical condition, i.e., 0.0 (highlighted) if overall it is false or 1.0 if overall it is true.

~~~~~~~~~~~~~~~~~~~~
    MYCOMPUTEDHEATINGSETPOINTPROG,Line 10,ELSEIF (HOUR >= 5) && (HOUR < 19)  && (DAYOFWEEK >=2) && (DAYOFWEEK <=6),0.0, Occurrence info=CHICAGO IL USA TMY2-94846 WMO#=725300, 09/23 10:20 - 10:30
~~~~~~~~~~~~~~~~~~~~

To debug what is going on with the individual terms in the logical expression, we can add some otherwise useless statements so line traces contain an echo of the current values of the HOUR and DAYOFWEEK built-in variables. So if we add the following lines before the start of the IF block,

~~~~~~~~~~~~~~~~~~~~
        Set locHour = Hour, ! echo out for debug
        Set locDay = DayOfWeek, ! echo out for debug
~~~~~~~~~~~~~~~~~~~~

We will see the values that Hour and DayOfWeek contain in the debug output. The local variables Erl variables locHour and locDay do not need to be used for anything, but by adding these Erl statements we can glean debugging insights.

The line of Erl code is switched to all uppercase on input, so the line trace differs from the input file in that all characters are capitalized. If the input file was developed using a CamelCase convention, it may be much more difficult to read in the line trace output. Thus, the underscore character "_" may be a more useful convention for inputting Erl code because it will be more readable in the debugging traces.
