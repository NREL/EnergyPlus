Parametric Objects
==================

Group – Parametrics
-------------------

In order to assess a group of alternatives, the Parametric objects can be used in a single file as an alternative of maintaining a group of files with small differences. Updating portions of the multiple files that do not change between permutations becomes tedious and error prone and may lead to incorrect conclusions concerning the energy savings of specific measures. The ParametricPreprocessor.exe program (described in the AuxiliaryPrograms document) interprets the following objects in a single file and creates the group of files automatically. The objects in the group are:

- Parametric:SetValueForRun

- Parametric:Logic

- Parametric:RunControl

- Parametric:FileNameSuffix

A simple example of part of an IDF file is shown below:



```idf
  Parametric:SetValueForRun,
    $insDepth,                  !- Parameter Name
    0.0508,                     !- Value 1
    0.0762,                     !- Value 2
    0.1016,                     !- Value 3
    0.1270;                     !- Value 4


  Material,
    IN1,                     !- Name
    VeryRough,               !- Roughness
    =$insDepth,              !- Thickness {m}
    2.3000000E-02,           !- Conductivity {W/m-K}
    24.00000,                !- Density {kg/m3}
    1590.000,                !- Specific Heat {J/kg-K}
    0.9000000,               !- Thermal Absorptance
    0.5000000,               !- Solar Absorptance
    0.5000000;               !- Visible Absorptance
```




In this example, the parameter \$insDepth is used in the Material object to describe the thickness of the material, in this case insulation.  For the first simulation, $insDepth is set to 0.0508 (2 inches) shown as the first value in the Parametric:SetValueForRun object. This value is then substituted in where ever the $insDepth is shown in the file, in this example, in the Material object. The simulation would use this first value and produce a set of results based on this insulation thickness. The second simulation would use the second value of the Parametric:SetValueForRun object of 0.0762 (3 inches) for the thickness of the insulation described in the Material object. This would then be simulated with a second set of results. This would be repeated a third time with 0.1015 (4 inches) and 0.1270 (5 inches) of insulation.

A new IDF file is created for each of these simulations and is named the original name of the file plus either a number or a user defined suffix (if Parametric:FileNameSuffix is used). Looking at how these created IDF files show the substituted values can help understand problems that may occur.

The “=$insDepth” is a simple substation but could also be a more complex expression involving operators and predefined functions such as

  =$insDepth \* 1.1 + 0.0004 \* SQRT(12)

Many sets of simulations can be defined just using different Parametric:SetValueForRun objects but for more advanced sets of parametric runs, the Parametric:Logic object can be used to perform additional calculations, logical operations, and to exclude entire objects in the file. The Parametric:RunControl allows individual runs to be performed or not. The Parametric:FileNameSuffix object controls the naming of the resulting sets of output files.

### Expressions

Expressions can occur anywhere in the IDF file as a field value if the expression begins with an equals sign (=) and can also occur in assignment statements in the Parametric:Logic object. The operators include: +, -, \*, /, ^ (exponent), && (and), || (or), ==, &gt;, &lt;, &lt;=, &gt;=,  &lt;&gt;, and //. These are computed using standard operator precedence rules. The parenthesis are used to execute sub-expressions with higher precedence. Some built in functions, such as ABS, ACOS, ASIN, ATAN, COS, EXP, INT, LEN, LOG, MOD, NOT, SIN, SQRT, TAN are supported in expressions. Theses built in functions all take a single argument.

### Processing Order

The order that the parameters are processed is important to understand their values. Like other objects in EnergyPlus, the order that they occur in the file is not important.  The order of evaluation is:

1)    Parameter values are based on Parametric:SetValueForRun

2)    All lines of the Parametric:Logic object are evaluated

3)    All embedded expressions in the file are evaluated

### Parametric:SetValueForRun

The core parametric object is Parametric:SetValueForRun which sets the parameters value to different values depending on which run is being simulated.

For example

```idf
Parametric:SetValueForRun
  $ConstructionToUse,  !- Parameter Name
  TiltUpConcreteWall,  !- Value for run 1
  BlockWall,           !- Value for run 2
  SteelFrameWall,      !- Value for run 3
  WoodFrameWall;       !- Value for run 4


Parametric:SetValueForRun
  $WallRValue,           !- Parameter Name
  5,                     !- Value for run 1
  8,                     !- Value for run 2
  11,                    !- Value for run 3
  17;                    !- Value for run 4
```


Using Parametric:SetValueForRun, the value is set based on the which run number is currently being simulated. Multiple objects can be used and a set of parameter values would be defined for a given simulation. For example, the 3rd simulation would assign the "Value 3" value, 11, to the parameter \$WallRValue and the “Value 3” value, “SteelFrameWall” to $ConstructionToUse. Since many Parametric:SetValueForRun objects can be used at once, all of the  3rd values would be assigned to the appropriate parameters. In IDF Editor, this object will appear as a table of values with each row being all the parameter values for a specific simulation and each column belonging to a specific parameter.

#### Field: Parameter Name

The name of the parameter. It must begin with the dollar sign character, and the second character must be a letter. The parameter name, unlike a typical EnergyPlus name, cannot contain spaces or any characters other than letters and numerals.

#### Field: Value for run n

The value that should be substituted for the parameter on the n-th simulation. If multiple Parametric:SetValueForRun objects exist and they have different number of fields, the last field value will be used for the remaining field values. This means that to set all values the same, only the first value needed to be entered.

### Parametric:Logic

The approach of using parameters and expressions, by itself, is very flexible and allows for many possible parametric simulations but it does not allow for objects to be included for some parametric cases and not others. For example, you might want an overhang on a window in some parametric runs and not others. The Parametric:Logic object can help in that scenario.

A single Parametric:Logic object is allowed per file. It has a freeform syntax including IF and SELECT blocks with each field in the object representing a single "line" of code. Essentially, this logic object looks like simple programming code.

```idf
Parametric:Logic,
  Main,                            !- name
  IF $insDepth &lt; 0.1,              !- parametric logic line 1
     DISABLE “MaterialWallBoard”,  !- parametric logic line 2
     DISABLE “MaterialExtFinish”,  !- parametric logic line 3
  ENDIF;                           !- parametric logic line 4
```


In addition, lines could contain assignment statements in the form of

parameter  = expression such as the second control line in the following example.

```idf
Parametric:Logic,
  Main,                              !- name
  PARAMETER $THICKNESS,              !- parametric logic line 1
  $THICKNESS = 0.01 * $CMDEPTH,      !- parametric logic line 2
  IF $THICKNESS > 0.5,               !- parametric logic line 3
     DISABLE “MaterialWallBoard”,    !- parametric logic line 4
     DISABLE “MaterialExtFinish”,    !- parametric logic line 5
  ENDIF;                             !- parametric logic line 6
```

The statements include PARAMETER, IF, ELSE, ELSEIF, ENDIF, SELECT, CASE, DEFAULT, ENDSELECT, ENABLE, DISABLE, and REMARK. Nested IF and SELECT statements are allowed.  All objects are enabled until they are specifically disabled using the DISABLE statement and could be later re-enabled with ENABLE. An object that is named with the DISABLE statement would not appear in that simulation run.

The PARAMETER statement declares the name of the parameter. It is necessary that every parameter used in the Parametric:Logic object be initialized using the PARAMETER statement unless the parametes are created using the Parametric:SetValueForRun object. Requiring explicit parameter declaration reduces errors in software programming. Parameter names would not be case sensitive.

The DISABLE and ENABLE commands can have one or two arguments. If a second argument is present it is the kind of object.

```
DISABLE “MaterialWallBoard” “Material”,    !- parametric control line 4
```

The following sections describe each type of statement in more detail.

#### IF ENDIF Block

As previously shown, the simplest IF block is in the form of

```idf
IF <conditional-expression>
  <true-block-of-statements>
ENDIF
```

The &lt;conditional-expression&gt; usually contains a logical comparison operator such as:

*   == equal

*   &gt; greater than

*   &lt; less than

*   &lt;= less than or equal to

*   &gt;= greater than or equal to

*   &lt;&gt; not equal to


The &lt;true-block-of-statements&gt; can contain other statements including other lines of IF and ENDIF. These statement are only executed when the &lt;conditional-expression&gt; evaluates to true. If the &lt;conditional-expression&gt; is false, none of the statements before the ENDIF are executed and instead execution begins again with the statement after the ENDIF statement.

#### IF ELSE  ENDIF Block

Another form of the IF block can contain the ELSE statement:

```idf
IF <conditional-expression>
  <true-block-of-statements>
ELSE
  <false-block-of-statements>
ENDIF
```

Like the simpler IF ENDIF form the &lt;conditional-expression&gt; is computed and the &lt;true-block-of-statements&gt; are executed if it is true. The additional feature of this form is that if the &lt;conditional-expression&gt; is false, the &lt;false-block-of-statements&gt; is executed instead of the &lt;true-block-of-statements&gt;.

#### IF ELSEIF ELSE ENDIF Block

The most complex form of the IF block contains both ELSEIF statement (or statements)  and the ELSE statement:

```idf
IF <conditional-expression-1>
  <true-block-of-statements-1>
ELSEIF <conditional-expression-2>
  <true-block-of-statements-2>
ELSEIF <conditional-expression-2>
  <true-block-of-statements-2>
ELSE
  <false-block-of-statements>
ENDIF
```


This form allows more complex logic to be expressed. If the &lt;conditional-expression-1&gt; is true then &lt;true-block-of-statements-1&gt; executed and when that is complete, execution resumes after the ENDIF statement. If &lt;conditional-expression-1&gt; is false then the ELSEIF &lt;conditional-expression-2&gt; is evaluated and if that is true then &lt;true-block-of-statement-2&gt; is executed. On the other hand, if &lt;conditional-expression-2&gt; is  false, then the next ELSEIF conditional expression is evaluated following the same patter if another ELSEIF is present, otherwise, the &lt;false-block-of-statements&gt; after the ELSE are executed. Another way of thinking of this block is that the conditional expressions are evaluated from the top to the bottom until one is found to be true and then the associated block of statements are executed. If none of the conditional expressions is true, the &lt;false-block-of-statements&gt; are executed.

#### SELECT CASE DEFAULT ENDSELECT Block

The SELECT block is related to the IF block but is for the specific case of trying to match a variable with several specific values. The form of the SELECT block is:

```idf
SELECT <expression>
CASE <constant-1>
  <case-block-of-statements-1>
CASE <constant-2>
  <case-block-of-statements-2>
CASE <constant-3>
  <case-block-of-statements-3>
DEFAULT
  <default-block-of-statements>
ENDSELECT
```


With any number of CASE statements. The &lt;expression&gt; is evaluated and compared to the constants with each CASE statement. If a match is found the corresponding &lt;case-block-of-statements&gt; are executed otherwise if no match if found the &lt;default-block-of-statements&gt; is excecuted. One one matching CASE statement for each SELECT block is executed. An example of how this works is shown below:

```idf
Parametric:SetValueForRun,
  $EndMonth,
  2;


Parametric:Logic,
  Main2,
  PARAMETER $EndDay,
  SELECT $EndMonth,
   CASE 1,
    $EndDay = 31,
   CASE 2,
    $EndDay = 28,
   CASE 3,
    $EndDay = 31,
   CASE 4,
    $EndDay = 30,
   CASE 5,
    $EndDay = 31,
   CASE 6,
    $EndDay = 30,
   CASE 7,
    $EndDay = 31,
   CASE 8,
    $EndDay = 31,
   CASE 9,
    $EndDay = 30,
   CASE 10,
    $EndDay = 31,
   CASE 11,
    $EndDay = 30,
   CASE 12,
    $EndDay = 31,
   DEFAULT,
    $EndDay = 30,
   ENDSELECT;
```


With this example the $EndDay value would be set to 28.

#### ENABLE DISABLE

The ENABLE and DISABLE statements specifically are for removing (and not removing) other objects that appear in the EnergyPlus IDF file. It has two syntaxes:

```idf
DISABLE <objectname>
```

and

```idf
DISABLE <objectname>, <kind-of-object>
```

The DISABLE statement removes the object identified with either &lt;objectname&gt; or the combination of &lt;objectname&gt; and &lt;kind-of-object&gt; if the &lt;objectname&gt; is not unique in the EnergyPlus input file. By default, all objects are enabled in the input file. Objects need to be specifically disabled using the DISABLE statement. After an object is disabled in can later be re-enabled with the ENABLE statement which has similar syntax.

```idf
DISABLE <objectname>
```

and

```idf
DISABLE <objectname>, <kind-of-object>
```


```idf
  Parametric:Logic,
    Main,
    DISABLE  "SOUTH WALL OVERHANG";


  Shading:Zone:Detailed,
    SOUTH WALL OVERHANG,     !- Name
    ZONE SURFACE SOUTH,      !- Base Surface Name
    SCH 2,                   !- Transmittance Schedule Name
    4,                       !- Number of Vertices
    0.0,-1.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}
    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 2 {m}
    4.0,0.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}
    4.0,-1.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}
```


An object that is named with the DISABLE statement would not appear in that simulation run.

#### REMARK

The REMARK statement is simply a way to include a comment in the Parametric:Logic fields. The text that follows the REMARK statement is ignored.

#### OBJECT DEFINITION

The definition of the Parametric:Logic object is shown below:

#### Field: Name

The name of the Parametric:Logic object.

#### Field: Parametric Logic Line n

The line that contains one statement in one of the following forms:

* &lt;parameter&gt; = &lt;expression&gt;

* PARAMETER &lt;parameter&gt;

* IF &lt;expression&gt;

* ELSEIF &lt;expression&gt;

* ELSE

* ENDIF

* SELECT &lt;expression&gt;

* CASE &lt;constant&gt;

* DEFAULT

* ENDSELECT

* ENABLE &lt;constant&gt;

* ENABLE &lt;constant&gt; &lt;constant&gt;

* DISABLE &lt;constant&gt;

* DISABLE &lt;constant&gt; &lt;constant&gt;

* REMARK &lt;text to ignore&gt;

### Parametric:RunControl

The following Parametric:RunControl object controls which runs are simulated. The series of entries indicate if the run should be performed or not. This object is optional and if it is not included all runs are performed.

#### Field: Name

The name of the optional Parametric:RunControl object.

#### Field: Perform run n

The value of the field can be either a yes or no.  It cannot contain a parameter.

### Parametric:FileNameSuffix

The file name of the IDF files that are created for each run and the output files for each run are based on the name of the file plus a “suffix”. The suffixes can be defined using the Parametric:FileNameSuffix object or if the object is missing will default to the run number.

#### Field: Name

The name of the optional Parametric:FileNameSuffix object.

#### Field: Suffix for file name in run n

The value of the field is added to the IDF file name to create the resulting IDF file and output files.  It cannot contain a parameter.

