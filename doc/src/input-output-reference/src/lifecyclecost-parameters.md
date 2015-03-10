# LifeCycleCost:Parameters

Provides inputs related to the overall life-cycle analysis. It establishes many of the assumptions used in computing the present value. It is important that when comparing the results of multiple simulations that the fields in the LifeCycleCost:Parameters objects are the same for all the simulations. To aide in this comparison, the first table in the Life-Cycle Cost Report shows the inputs to this object. Only one LifeCycleCost:Parameters object is permitted for a file. When this object is present the tabular report file will contain the Life-Cycle Cost Report.

## Inputs

#### Field: Name

An identifying name used for the LifeCycleCost:Parameters object.

#### Field: Discounting Convention 

The field specifies if the discounting of future costs should be computed as occurring at the end of each year, the middle of each year, or the beginning of each year. The most common discounting convention uses the end of each year. Without a specific reason, the end of each year should be used. Some military projects may specifically require using the middle of each year. The year being used starts with the base year and month and repeats every full year. All costs assumed to occur during that duration are accumulated and shown as an expense either at the beginning, middle or end of the year.  The options are:

- BeginningOfYear
- MidYear
- EndOfYear

#### Field: Inflation Approach 

This field is used to determine if the analysis should use constant dollars or current dollars which is related to how inflation is treated. The two options are:

- ConstantDollar
- CurrentDollar

The default option is ConstantDollars

If ConstantDollar is selected, then the Real Discount Rate input is used and it excludes the rate of inflation. If CurrentDollar is selected, then the Nominal Discount Rate input is used and it includes the rate of inflation. From NIST Handbook 135:

"The constant dollar approach has the advantage of avoiding the need to project future rates of inflation or deflation. The price of a good or service stated in constant dollars is not affected by the rate of general inflation. For example, if the price of a piece of equipment is \$1,000 today and \$1,050 at the end of a year in which prices in general have risen at an annual rate of 5 percent, the price stated in constant dollars is still \$1,000; no inflation adjustment is necessary. In contrast, if cash flows are stated in current dollars, future amounts include general inflation, and an adjustment is necessary to convert the current-dollar estimate to its constant-dollar equivalent. This adjustment is important because constant- and current-dollar amounts must not be combined in an LCCA [life cycle cost analysis]."

For most analyses, using the ConstantDollar option will be easier since the effect of inflation may be ignored.

#### Field: Real Discount Rate

Enter the real discount rate as a decimal. For a 3% rate, enter the value 0.03. This input is used when the Inflation Approach is ConstantDollar. The real discount rate reflects the interest rates needed to make current and future expenditures have comparable equivalent values when general inflation is ignored. When Inflation Approach is set to CurrentDollar this input is ignored. If this field is blank and Inflation Approach is ConstantDollar, a warning is issued.

#### Field: Nominal Discount Rate

Enter the nominal discount rate as a decimal. For a 5% rate, enter the value 0.05. This input is used when the Inflation Approach is CurrentDollar. The real discount rate reflects the interest rates needed to make current and future expenditures have comparable equivalent values when general inflation is included. When Inflation Approach is set to ConstantDollar this input is ignored. If this field is blank and Inflation Approach is CurrentDollar, a warning is issued.

#### Field: Inflation Rate

Enter the rate of inflation for general goods and services as a decimal. For a 2% rate, enter the value 0.02. If this field is not blank and Inflation Approach is ConstantDollar, a warning is issued.

#### Field: Base Date Month 

Enter the month that is the beginning of study period, also known as the beginning of the base period. According to NIST 135 "the base date is the point in time to which all project-related costs are discounted in an LCCA [life cycle cost analysis]. The base date is usually the first day of the study period for the project, which in turn is usually the date that the LCCA is performed. In a constant dollar analysis, the base date usually defines the time reference for the constant dollars (e.g. 1995 constant dollars). It is essential that you use the same base data and constant-dollar year for all of the project alternatives to be compared. If you set the base date to the date that the LCCA is performed, then the constant dollar basis for the analysis will be the current date, and you can use actual costs as of that date without adjusting for general inflation."

The choices are:

- January
- February
- March
- April
- May
- June
- July
- August
- September
- October
- November
- December

The default value is January.

This field could also be referred to as part of "the date of study." It is used as the date for constant dollars.

#### Field: Base Date Year 

Enter the four digit year that is the beginning of study period, such as  a year expressed in four digits like "2013". The study period is also known as the base period. See more details in the previous field.

#### Field: Service Date Month 

Enter the month that is the beginning of building occupancy. Energy costs computed by EnergyPlus are assumed to occur during the year following the service date. The service date must be the same or later than the Base Date. The choices are:

- January
- February
- March
- April
- May
- June
- July
- August
- September
- October
- November
- December

The default value is January. According to NIST Handbook 135:

"The service date is the date on which the project is expected to be implemented; operating and maintenance costs (including energy- and water-related costs) are generally incurred after this date, not before."

This field could also be referred to as part of "beneficial occupancy date."

#### Field: Service Date Year 

Enter the four digit year that is the beginning of occupancy, such as  two years after the previously entered year expressed in four digits like "2013". See more details in the previous field.

#### Field: Length of Study Period in Years

Enter the number of years of the study period. It is the number of years that the study continues based on the start at the base date. The default value is 25 years. Only integers may be used indicating whole years.  According to NIST Handbook 135, "the study period for an LCCA is the time over which the costs and benefits related to a capital investment decision are of interest to the decision maker. Thus, the study period begins with the base date and includes both the [planning/construction] period (if any) and the relevant service period for the project. The service period begins with the service date and extends to the end of the study period."

#### Field: Tax rate

Enter the overall marginal tax rate for the project costs. This does not include energy or water taxes. The single tax rate entered here is not intended to be a replacement of the complex calculations necessary to compute personal or corporate taxes; instead it is an approximate that may be used for a simple analysis assuming a constant tax rate is applied on all costs. The tax rate entered should be based on the marginal tax rate for the entity and not the average tax rate. Enter the tax rate results in present value calculations after taxes. Most analyses do not factor in the impact of taxes and assume that all options under consideration have roughly the same tax impact. Due to this, many times the tax rate can be left to default to zero and the present value results before taxes are used to make decisions. The value should be entered as a decimal value. For 15% enter 0.15. For an analysis that does not include tax impacts, enter 0.0. The default is 0.

#### Field: Depreciation Method 

For an analysis that includes income tax impacts, this entry describes how capital costs are depreciated. According to IRS Publication 946 – How to Depreciate Property "Depreciation is an annual income tax deduction that allows you to recover the cost or other basis of certain property over the time you use the property. It is an allowance for Fair market value the wear and tear, deterioration, or obsolescence of the Intangible property." Details on which depreciation method to choose depends on the property being depreciated and IRS Publication 946 and your accountant are the best sources of information in determining which depreciation method to choose. Only one depreciation method may be used for an analysis and is applied to all capital expenditures. Only analyses that include tax impacts need to select a depreciation method.

The options are:

- ModifiedAcceleratedCostRecoverySystem-3year
- ModifiedAcceleratedCostRecoverySystem-5year
- ModifiedAcceleratedCostRecoverySystem-7year
- ModifiedAcceleratedCostRecoverySystem-10year
- ModifiedAcceleratedCostRecoverySystem-15year
- ModifiedAcceleratedCostRecoverySystem-20year
- StraightLine-27year
- StraightLine-31year
- StraightLine-39year
- StraightLine-40year
- None

Depreciation allowances reduce the actual/nominal tax dollars paid by the owner.  Thus, analyses using depreciation should be conducted in nominal dollars. For an analysis that does not include tax effects, None should be selected.

The default value is None.

An example of this object in an IDF:

~~~~~~~~~~~~~~~~~~~~

    LifeCycleCost:Parameters,
        TypicalLCC,              !- Name
        EndOfYear,               !- Discounting Convention
        ConstantDollar,          !- Inflation Approach
        0.034314,                !- Real Discount Rate
        ,                        !- Nominal Discount Rate
        ,                        !- Inflation
        January,                 !- Base Date Month
        2010,                    !- Base Date Year
        January,                 !- Service Date Month
        2010,                    !- Service Date Year
        25,                      !- Length of Study Period in Years
        0.39,                    !- Tax rate
        StraightLine-39year;     !- Depreciation Method
~~~~~~~~~~~~~~~~~~~~