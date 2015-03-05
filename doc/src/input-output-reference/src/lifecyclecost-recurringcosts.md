# LifeCycleCost:RecurringCosts

Recurring costs are costs that repeat over time on a regular schedule during the study period. If costs associated with equipment do repeat but not on a regular schedule, use LifeCycleCost:NonrecurringCost objects instead.

## Inputs

#### Field: Name

The identifier used for the object. The name is used in identifying the cash flow equivalent in the output results in the Life-Cycle Cost Report.

#### Field: Category 

Enter the category of the recurring costs. Choose the closest category. The options include:

- EnergyCosts
- WaterCosts
- Maintenance
- Repair
- Operation
- Replacement
- MinorOverhaul
- MajorOverhaul
- OtherOperational

The default value is Maintenance.

For recommendations on estimating other operational costs, see NIST 135 Section 4.6.3.

#### Field: Cost  

Enter the cost in dollars (or the appropriate monetary unit) for the recurring costs. Enter the cost for each time it occurs. For example, if the annual maintenance cost is \$500, enter 500 here.

#### Field: Start of Costs 

Enter when the costs start. The First Year of Cost is based on the number of years past the Start of Costs. For most maintenance costs, the Start of Costs should be Service Period. The options are:

- ServicePeriod
- BasePeriod

The default value is ServicePeriod.

#### Field: Years From Start

This field and the Months From Start field together represent the time from either the start of the Service Period, on the service month and year, or start of the Base Period, on the base month and year (depending on the Start of Costs field) that the costs start to occur. Normally, for most maintenance costs that begin in the first year that the equipment is in service the Start of Costs is the Service Period and the Years from Start will be 0. Only integers should be entered representing whole years. The default value is 0.

#### Field: Months From Start

This field and the Years From Start field together represent the time from either the start of the Service Period, on the service month and year, or start of the Base Period, on the base month and year (depending on the Start of Costs field) that the costs start to occur. Normally, for most maintenance costs the Start of Costs is the Service Period and the Months from Start will be 0. Only integers should be entered representing whole months. The Years From Start (times 12) and Months From Start are added together. The default value is 0.

#### Field: Repeat Period Years

This field and the Repeat Period Months field indicate how much time elapses between reoccurrences of the cost. For costs that occur every year, such as annual maintenance costs, the Repeat Period Years should be 1 and Repeat Period Months should be 0. Only integers should be entered representing whole years. The default value is 1.

#### Field: Repeat Period Months

This field and the Repeat Period Years field indicate how much time elapses between reoccurrences of the cost. For costs that occur every year the Repeat Period Years should be 1 and Repeat Period Months should be 0. For, costs that occur every eighteen months, the Repeat Period Years should be 1 and the Repeat Period Months should be 6. Only integers should be entered representing whole years. The Repeat Period Years (times 12) and Repeat Period Months are added together. The default value is 0.

#### Field: Annual Escalation Rate

Enter the annual escalation rate as a decimal. For a 1% rate, enter the value 0.01. This input is used when the Inflation Approach is CurrentDollar. When Inflation Approach is set to ConstantDollar this input is ignored. The default value is 0.

An example of this object in an IDF:

~~~~~~~~~~~~~~~~~~~~

    LifeCycleCost:RecurringCosts,
        AnnualMaint,             !- Name
        Maintenance,             !- Category
        2000,                    !- Cost
        ServicePeriod,           !- Start of Costs
        0,                       !- Years from Start
        0,                       !- Months from Start
        1,                       !- Repeat Period Years
        0,                       !- Repeat Period Months
        0;                       !- Annual escalation rate
~~~~~~~~~~~~~~~~~~~~