# Converting imf files

The transition/conversion programs can "automatically" transition imf (ep-macro) files. One note of caution: if your macro file contains #if statements in the form:

~~~~~~~~~~~~~~~~~~~~
    PEOPLE,
        ZONE ONE,                !- Zone Name
    ##if #[DoSizing[] EQSU Y]
        #[FAREA[] / OCCDENPEAK[]],        !- Number of People
      ##else
        #[FAREA[] / OCCDENAVG[]],         !- Number of People
    ##endif
        OCCSCHED[],       !- Number of People SCHEDULE Name (real--fraction)
        0.5000000,        !- Fraction Radiant
        Activity Sch;     !- Activity level SCHEDULE Name (units W/person, real)
~~~~~~~~~~~~~~~~~~~~

They should look like this:

~~~~~~~~~~~~~~~~~~~~
    ##if #[DoSizing[] EQSU Y]
    PEOPLE,
        ZONE ONE,                !- Zone Name
        #[FAREA[] / OCCDENPEAK[]],       !- Number of People
        OCCSCHED[],       !-   Number of People SCHEDULE Name (real--fraction)
        0.5000000,        !- Fraction Radiant
        Activity Sch;     !- Activity level SCHEDULE Name (units W/person, real)
      ##else
    PEOPLE,
        ZONE ONE,                !- Zone Name
        #[FAREA[] / OCCDENAVG[]],        !- Number of People
        OCCSCHED[],       !- Number of People SCHEDULE Name (real--fraction)
        0.5000000,        !- Fraction Radiant
        Activity Sch;     !- Activity level SCHEDULE Name (units W/person, real)
    ##endif
~~~~~~~~~~~~~~~~~~~~
