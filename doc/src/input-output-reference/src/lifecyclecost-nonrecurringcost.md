# LifeCycleCost:NonrecurringCost

A non-recurring cost happens only once during the study period. For costs that occur more than once during the study period on a regular schedule, use the LifeCycleCost:RecurringCost object.

## Inputs

#### Field: Name

The identifier used for the object. The name is used in identifying the cash flow equivalent in the output results in the Life-Cycle Cost Report.

#### Field: Category 

Enter the category of the non-recurring costs. Choose the closest category. The options include:

- Construction
- Salvage
- OtherCapital

The default is [Construction](#construction).

For recommendations on estimating capital and salvage (residual) costs, see NIST 135 Section 4.5.

#### Field: Cost  

Enter the non-recurring cost value. For construction and other capital costs the value entered is typically a positive value. For salvage costs, the value entered is typically a negative value which represents the money paid to the investor for the equipment at the end of the study period.

#### Field: Start of Costs 

Enter when the costs start. The First Year of Cost is based on the number of years past the Start of Costs. For most non-recurring costs the Start of Costs should be Base Period which begins at the base month and year. The options are:

- ServicePeriod
- BasePeriod

The default value is BasePeriod

#### Field: Years From Start

This field and the Months From Start field together represent the time from either the start of the Service Period, on the service month and year, or start of the Base Period, on the base month and year (depending on the Start of Cost field) that the costs start to occur. Normally, for most capital costs the Start of Costs is the Base Period and the Years from Start will be 0. These would be first costs for the building equipment or system. Salvage costs are usually shown as happening in the last year of the analysis. Only integers should be entered representing whole years. The default value is 0.

#### Field: Months From Start

This field and the Years From Start field together represent the time from either the start of the Service Period, on the service month and year, or start of the Base Period, on the base month and year (depending on the Start of Cost field) that the costs start to occur. Normally, for most capital costs the Start of Costs is the Base Period and the Months from Start will be 0. Only integers should be entered representing whole months. The Years From Start (times 12) and Months From Start are added together. The default value is 0.

An example of this object in an IDF:

~~~~~~~~~~~~~~~~~~~~

    LifeCycleCost:NonrecurringCost,
        EstimatedSalvage,        !- Name
        Salvage,                 !- Category
        -2000,                   !- Cost
        ServicePeriod,           !- Start of Costs
        20,                      !- Years from Start
        0;                       !- Months from Start
~~~~~~~~~~~~~~~~~~~~