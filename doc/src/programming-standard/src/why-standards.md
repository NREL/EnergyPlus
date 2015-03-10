# Why Standards?

> **Standard:** An acknowledged measure of comparison for quantitative or qualitative value; a criterion.

Without standards, software development is an uncontrolled activity or, often, an activity out of control.  With standards, the quality of software within the development group can continuously improve to the detriment of no individual contribution.  In addition, standards may be able to help us meet various goals for the development (such as cost, timeliness, and focus on the product).

At the July 1995, joint team meeting, several goals for the project were outlined:

#. Take best of existing DOE-2/BLAST/(IBLAST) capabilities, applications, and methods/structures.  Combine with existing best of others.
#. Reuse existing code and structures where possible.

Short Time Frames (< 24 months)

During April 1996, the "Champaign Best of" group met and determined priorities for development within, at least, their portion of the "Best of" development.  Using a list, the following weightings were determined, in descending order of importance (Level 1 more important than Level 2):

- Level 1:  Maintainability, Robustness, Reliability, Testability, Understandability(Readability)
- Level 2:  Portability, Reusability
- Level 3:  Speed, Size

Thus, we established a standard both for coders as well as for reviewers.  When we code or review, we will try to keep these elements in mind.  Should a trade-off be needed, the decision either at coding time or review time will fall to the priorities established.

These items are defined (for the most part) in Code Complete along with a table that shows how focus on one may hinder another.  The pertinent definitions are repeated here:

- Maintainability:The ease with which you can modify a software system to change or add capabilities, improve performance, or correct defects.
- Robustness:The degree to which a system continues to function in the presence of invalid inputs or stressful environmental conditions.
- Reliability:The ability of a system to perform its required function under stated conditions whenever required -- having a long mean time between failures.
- Testability:The degree to which you can unit-test and system-test a system; the degree to which you can verify that the system meets its requirements.
- Understandability:The ease with which you can comprehend a system at both the system-organizational and detailed-statement levels.  Understandability has to do with the coherence of the system at a more general level than readability does.
- Portability:The ease with which you can modify a system to operate in an environment different from that for which it was specifically designed.
- Reusability:The extent to which and the ease with which you can use parts of a system in other system.
- Speed:Related to Efficiency: execution time.
- Size:Related to Efficiency: memory requirements.

Not on our original list but included in the reference:

Efficiency:Minimal use of system resources, including memory and execution time.

Readability:The ease with which you can read and understand the source code of a system, especially at the detailed-statement level.

Accuracy:The degree to which a system, as built, is free from error, especially with respect to quantitative outputs.  Accuracy differs from correctness; it is a determination of how well a system does the job it is built for rather than whether it was built correctly.

At the same time, we should consider what our references put forth.  "Standards shouldn't be imposed at all, if you can avoid them.  Consider the alternatives to standards: flexible guidelines, a collection of suggestions rather than guidelines, or a set of examples that embody the best practices."