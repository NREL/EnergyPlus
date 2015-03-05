# Metrics

> **Metric:**   A standard of measurement.1

Metric is another way of saying "standard".  Sections under this topic will illustrate various measures that might be applied to source code, documentation, user interface and other elements of a system.

Referring to *Code Complete*, "the term 'metrics' refers to any measurement related to software development.  Lines of code, number of defects, defects per..." and gives two solid reasons to measure the software development process:

#. Any way of measuring the process is superior to not measuring it at all.
#. To argue against metrics is to argue that it is better not to know what is really happening on your project.

Metrics are not an "absolute"; rather, they are methods for showing "abnormalities" that *may* need to be looked at to preserve quality code.  In many cases, we will rely on the code reviewers subjective opinion on the understandability of the code.  Listed below are some example metrics that developers might want to use as guides when writing EnergyPlus code.  It is suggested that developers take some of the ideas from each of these and apply them to their coding assignments.

## Complexity Metric

A complexity metric can be used rather than specify a number of lines that must be contained in a piece of source code.  McCabe's technique for measuring complexity can probably be automated and has been correlated to reliability and frequent errors.  The metric is simple, straightforward, and described in the following table.

Table: McCabe Complexity Measurement

Enumeration Technique
---------------------
Start with 1 for the straight path through the routine.
Add 1 for each of the following keywords (or equivalents): if while repeat for and or
Add 1 for each case in a case statement.  If the case statement does not have a default case, add 1 more.
Prescription
------------
The routine is probably fine.
Start to think about ways to simplify the routine.
Break part of the routine into a second routine and call it from the first routine.

The code developer will probably find that for most EnergyPlus processes the absolute numbers presented in the second half of the previous table should be taken with some reservation.  Most likely, for most processes such low limits on complexity would result in an inordinate number of unneeded subroutines.  Again, this particular metric should be applied with common sense.

### Lines of Code per routine

This has been put forth as a possible metric and is mentioned in *Code Complete*.  Using such a metric might limit the number of lines of code per routine to say 100 or 200.  While programmers might be allowed to make exceptions, these reasons would have to be defended during any code reviews.