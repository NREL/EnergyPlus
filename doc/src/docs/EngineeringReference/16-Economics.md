Economics Calculations
======================

Component Costs
---------------

EnergyPlus provides simple cost estimating capabilities as an aid to design analysis and for life cycle costs.  There are three broad steps involved.  The first involves determining *construction* costs by summing individual “line items.”  The second involves determining *project* costs by adjusting construction costs to account for things like profit and design fees.  The third involves comparing the current simulation to a reference case so that marginal increases can be calculated.  The reference documentation contained in this section pertains to the following input object names.

* **ComponentCost:LineItem**

* **ComponentCost:Adjustments**

* **ComponentCost:Reference**

### Line Item Costs

Line item cost calculations are generally trivial involving simple multiplication and summation.  This section documents details of how line item costs are calculated.  The program code consists mainly of a **Case** construct where the **Line Item Type** is used to control the details of how calculations are performed.

The overall philosophy is to provide methods of calculating items using either direct entry of needed data (**‘General’** using object type), or using component descriptive data entered elsewhere in the input file (e.g. **‘Lights’**), or by using quantities that are calculated by the program during the simulation (e.g. **‘Coil:DX’** and **‘Chiller:Electric’**). 

The rest of this section provides details by organized by the type of line item.

#### General

The line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P<sub>e</sub>*:

<div>$$L = N * {P_e}$$</div>

#### Construction

This line item type is called “Construction” but is used to estimate the costs of surfaces in the model.  The Construction attribute of Surface objects is useful for categorizing surfaces.  The number of units, *N*, is determined by summing the Area, *A*, of Surface objects that have the type of construction specified in the **Item Name** field.  Surfaces are screened to eliminate any duplicates that may exist for interior partitions creating a list of 1 to *m* unique surfaces.  If a surface is associated with a Zone, then zone multiplier, *M<sub>Z</sub>*, and list multipliers, *M<sub>G</sub>*, are applied (these are usually defaulted to 1).

<div>$$N = \sum\limits_1^m {(A*{M_Z} * {M_G}} )$$</div>

The line item subtotal, *L*, is calculated by multiplying the number of units (actually units of are here), *N* (m<sup>2</sup>) by the Cost-per-Area, *P<sub>a</sub>* , ($/m<sup>2</sup>):

<div>$$L = N * {P_a}$$</div>

#### Coil:DX and Coil:Cooling:DX:SingleSpeed

DX coil costs can be estimated in one of three ways: per-each, per-kW, per-kW-COP.  The program determines which method to use based on there being non-zero values in appropriate input fields.

If **cost per each** is greater than 0.0 then the number of units, *N*, is the number of cooling coils.  This will be 1 if the **Item Name** is the name of a specific coil described elsewhere.  If the name is set to the wildcard (\*) then this will equal the total number of DX:Coils in the model.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P<sub>e</sub>*:

<div>$$L = N * {P_e}$$</div>

If **cost per kilowatt** is greater than 0.0 then the number of units, *N*, is the number of kilowatts of total, rated, cooling capacity.  This will be based on all the DX:Coils in the model if **Item Name** is set to the wildcard (\*) and will be that of the named coil if set to a valid coil name.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt, *P<sub>kW</sub>*:

<div>$$L = N * {P_{kW}}$$</div>

If **Cost per kilowatt per coefficient-of-performance** is greater than 0.0 then the number of units, *N*, is the number of kilowatts of total, rated, cooling capacity multiplied by the nominal coefficient of performance (COP) of the DX:Coils.  This will be based on all the DX:Coils in the model if **Item Name** is set to the wildcard (\*) and will be that of the named coil if set to a valid coil name.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt-per-COP, *P<sub>kW-COP</sub>*:

<div>$$L = N * {P_{kW - COP}}$$</div>

#### Coil:Heating:Gas

Gas-fired heating coil costs can be estimated in one of three ways: per-each, per-kW, per-kW-COP.  The program determines which method to use based on there being non-zero values in appropriate input fields.

If **cost per each** is greater than 0.0 then the number of units, *N*, is the number of heaters.  This will be 1 if the **Item Name** is the name of a specific heater described elsewhere.  If the name is set to the wildcard (\*) then this will equal the total number of Coil:Heating:Gas objects in the model.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P<sub>e</sub>*:

<div>$$L = N * {P_e}$$</div>

If **Cost per kilowatt** is greater than 0.0 then the number of units, *N*, is the number of kilowatts of total, rated, heating capacity.  This will be based on all the Coil:Heating:Gas objects in the model if **Item Name** is set to the wildcard (\*) and will be that of the named coil if set to a valid coil name.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt, *P<sub>kW</sub>*:

<div>$$L = N * {P_{kW}}$$</div>

If **Cost per kilowatt per coefficient-of-performance** is greater than 0.0 then the number of units, *N*, is the number of kilowatts of total, rated, heating capacity multiplied by the theoretical efficiency of the heater(s).  (Here we are treating gas heating efficiency as a Coefficient of Performance (COP)).  This will be based on all the Coil:Heating:Gas objects in the model if **Item Name** is set to the wildcard (\*) and will be that of the named coil if set to a valid coil name.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt-per-COP, *P<sub>kW-COP</sub>*:

<div>$$L = N * {P_{kW - COP}}$$</div>

#### Chiller:Electric

Electric chiller costs can be estimated in one of three ways: per-each, per-kW, per-kW-COP.  The program determines which method to use based on there being non-zero values in appropriate input fields.

If **cost per each** is greater than 0.0 then the number of units, *N*, is the number of chillers.  This will be 1 if the **Item Name** is the name of a specific coil described elsewhere.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P<sub>e</sub>*:

<div>$$L = N * {P_e}$$</div>

If **Cost per kilowatt** is greater than 0.0 then the number of units, *N*, is the number of kilowatts of total, rated, cooling capacity for the specified chiller.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt, *P<sub>kW</sub>*:

<div>$$L = N * {P_{kW}}$$</div>

If **Cost per kilowatt per coefficient-of-performance** is greater than 0.0, then the number of units, *N*, is the number of kilowatts of total, rated, cooling capacity multiplied by the nominal coefficient of performance (COP) of the chiller.  This will be based on the named chiller (if set to a valid coil name).  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt-per-COP, *P<sub>kW-COP</sub>*:

<div>$$L = N * {P_{kW - COP}}$$</div>

#### Daylighting:Controls

The costs of controllers for harvesting daylight are determined by the number of reference points.  The cost for each controller, *P<sub>e</sub>* , are input.  The of units, *N*, is determined from the number of daylight reference points in all the zones if the **Item Name** is the wildcard (\*).  If **Item Name** is set to a valid Zone name then N is the number of daylight reference points in just that zone (zones can have more than one daylight controllers).  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P<sub>e</sub>*:

<div>$$L = N * {P_e}$$</div>

#### Shading:Zone:Detailed

Shading surfaces need to be handled separately because they do not have the **Construction** attribute.  The **Item Name** must be set to a valid name for a **Shading:Zone:Detailed** object defined elsewhere in the file.  The number of units, N, is determined from the area of the named surface multiplied by zone multiplier, *M<sub>Z</sub>*, and list multipliers, *M<sub>G</sub>*:

<div>$$N = A*{M_Z} * {M_g}$$</div>

The line item subtotal, *L*, is calculated by multiplying the number of units, *N* (m<sup>2</sup>) by the Cost-per-Area, *P<sub>a</sub>* , ($/m<sup>2</sup>):

<div>$$L = N * {P_a}$$</div>

#### Lights

The cost of electric lighting systems can be modeled in two ways: per-each and per-kW.  The program determines which method to use based on there being non-zero values in appropriate input fields.  The **Item Name** must be the name of a valid Zone defined elsewhere in the input.

If **cost per each** is greater than 0.0, then the number of units, *N*, is the number lighting systems in the zone and is assumed to be 1.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P<sub>e</sub>*:

<div>$$L = N * {P_e}$$</div>

If **Cost per kilowatt** is greater than 0.0 then the number of units, *N*, is the number of kilowatts in the design level for electric lighting systems defined in Lights objects associated with the zone.  The **Item Name** must be the name of a valid Zone defined elsewhere in the input.  *N* is then the sum of all the Lights associated with the named Zone. Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt, *P<sub>kW</sub>*:

<div>$$L = N * {P_{kW}}$$</div>

#### Generator:Photovoltaic

The costs of simple photovoltaic power systems can be modeled using cost per kilowatt.  The number of units, N, is the nominal rated peak power of the photovoltaic panels.  The photovoltaic generator must be modeled using the PhotovoltaicPerformance:Simple input object which is a very simplistic model and does not include input for the nominal rated peak power of the system.  Therefore a peak power is calculated using the usual 1000 W/m<sup>2</sup> of incident solar radiation, *G<sub>p</sub>*, multiplied by the active solar cell area, *A* and the efficiency, *E*, and converted to units of kilowatts.

<div>$$N = \frac{{{G_p} * A * E}}{{1000.0}}$$</div>

Where, the cell area A is calculated by multiplying the area of the surface associated with the simple photovoltaic system and the active area fraction defined in the PhotovoltaicPerformance:Simple. Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt, *P<sub>kW</sub>*:

<div>$$L = N * {P_{kW}}$$</div>

### Adjustments

Various adjustments are available to model total project costs from the component costs modeled using individual line items.  These adjustments are provided to allow propagating how changes in component costs are amplified by things like design fees, profit, bonding, and taxes.

The subtotal of individual line items, *S<sub>L</sub>*, is obtained by summing all line item subtotals, *L*:

<div>$${S_L} = \sum L $$</div>

For the reference building, *S<sub>L,</sub>*is user input (typically from a prior run of baseline building model).

The subtotal for miscellaneous construction costs (typically those costs not explicitly included as line items), *S<sub>m</sub>*, are calculated by multiplying the **Miscellaneous Cost Model** (per Square Meter), *C<sub>m</sub>*,  by the total conditioned floor area, *A<sub>c</sub>*:

<div>$${S_m} = {A_c} * {C_m}$$</div>

The subtotal for the amount that the construction costs should be altered because of regional differences in material and labor costs (e.g. when using national average data), *S<sub>r</sub>*, is determined by multiplying the Regional Adjustment Factor, *R<sub>f\\ ,</sub>* by the sum of *S<sub>L </sub>* and *S<sub>m:</sub>*

<div>$${S_r} = ({S_L} + {S_m}){R_f}$$</div>

Remaining adjustments are applied to the subtotal for construction costs, *S<sub>c</sub>*, which is the sum of *S<sub>L</sub>*, *S<sub>m,</sub>*, and *S<sub>r:</sub>*

<div>$${S_C} = {S_L} + {S_m} + {S_r}$$</div>

The **Design and Engineering Fee Fraction**,*F<sub>d</sub>*,is the fraction of construction costs, *S<sub>c</sub>*, attributable to costs associated with architectural and engineering services needed for the project.  The subtotal for costs associated with these fees, *S<sub>D</sub>*, are determined by multiplying *S<sub>C</sub>* by *F<sub>d</sub>*:

<div>$${S_D} = {S_c}*{F_D}$$</div>

The **Contractor Fee Fraction**,*F<sub>c</sub>*,is the fraction of construction costs, *S<sub>c</sub>*, attributable to costs associated with the contractor’s profit that should be included in the project.  The subtotal for costs associated with contracting, *S<sub>P</sub>*, are determined by multiplying *S<sub>C</sub>* by *F<sub>d</sub>*:

<div>$${S_P} = {S_c}*{F_C}$$</div>

The **Contingency Fraction**,*F<sub>S</sub>*,is the fraction of construction costs, *S<sub>c</sub>*, which should be included in a cost estimate to provide for contingencies (perhaps related to errors and uncertainty in the estimate and project).  The subtotal for contingency costs, *S<sub>S</sub>*, are determined by multiplying *S<sub>C</sub>* by *F<sub>S</sub>*:

<div>$${S_S} = {S_c}*{F_S}$$</div>

The **Permits, Bonding, Insurance Fraction,***F<sub>B</sub>*,is the fraction of construction costs, *S<sub>c</sub>*, which should be included in a cost estimate to provide for things like taxes, bonding, insurance, and permits.  The subtotal for these extra costs, *S<sub>B</sub>*, are determined by multiplying *S<sub>C</sub>* by *F<sub>B</sub>*:

<div>$${S_B} = {S_c}*{F_B}$$</div>

The **Commissioning Fee Fraction,***F<sub>CX</sub>*,is the fraction of construction costs, *S<sub>c</sub>*, which should be included in a cost estimate to provide for commissioning the building systems.  The subtotal for these extra costs, *S<sub>CX</sub>*, are determined by multiplying *S<sub>C</sub>* by *F<sub>CX</sub>*:

<div>$${S_{CX}} = {S_c}*{F_{CX}}$$</div>

Finally, the cost estimate total, *T*, is obtained by summing the various subtotals:

<div>$$T = {S_L} + {S_m} + {S_r} + {S_D} + {S_P} + {S_S} + {S_B} + {S_{CX}}$$</div>

The normalized total cost estimate, C, is calculated by dividing *T* by the total conditioned floor area, *A<sub>c</sub>*:

<div>$$C = \frac{T}{{{A_C}}}$$</div>

### Comparisons

The capability of comparing the current cost estimate to that of a reference building is provided because is common to consider the marginal increases in costs associated with applying different energy design measures.  EnergyPlus calculates and reports the difference between the current model and a reference case for all the subtotals and totals discussed above.  The reported differences are the reference values subtracted from the current value.

Tariff Computation
------------------

The EnergyPlus economic (Utility Costs) objects related to computing utility bills include:

* UtilityCost:Tariff

* UtilityCost:Qualify

* UtilityCost:Charge:Simple

* UtilityCost:Charge:Block

* UtilityCost:Ratchet

* UtilityCost:Variable

* UtilityCost:Computation

This section builds upon the discussion that appears in the Input Output Reference under the heading “EnergyPlus Economics.”  The actual computation of monthly utility bills is not difficult since it is mostly consists of multiplying energy consumptions or demands by the price on a per unit basis and adding different bill components.  The implementation in EnergyPlus becomes more complex since the objects were crafted to allow a great deal of  flexibility in specifying a utility tariff while, at the same time, being as simple as possible.

The following discussion on variables and hierarchies is based on the text that appears in the Input Output Reference.

### Conceptual Framework – Variables and Hierarchy

To understand how to use the utility bill calculation portion of EnergyPlus you first need to understand some important concepts of variables and hierarchy.  A variable, for the purposes of this section, is simply a named holder of a series of numbers. In most cases, the variable will be a named holder of 12 numbers, one number for each monthly utility bill. Here is a visualization of a variable called Electric Energy Use:

<table class="table table-striped">
<tr>
<th>Month</th>
<th>Electric Energy Use</th>
</tr>
<tr>
<td>January</td>
<td>12143</td>
</tr>
<tr>
<td>February</td>
<td>13454</td>
</tr>
<tr>
<td>March</td>
<td>14178</td>
</tr>
<tr>
<td>April</td>
<td>14876</td>
</tr>
<tr>
<td>May</td>
<td>15343</td>
</tr>
<tr>
<td>June</td>
<td>16172</td>
</tr>
<tr>
<td>July</td>
<td>16105</td>
</tr>
<tr>
<td>August</td>
<td>15762</td>
</tr>
<tr>
<td>September</td>
<td>14543</td>
</tr>
<tr>
<td>October</td>
<td>13987</td>
</tr>
<tr>
<td>November</td>
<td>13287</td>
</tr>
<tr>
<td>December</td>
<td>12403</td>
</tr>

</table>

If you have ever done any computer programming, you can think of a variable as an array.  Many of the names used in the utility bill calculation portion of EnergyPlus are names of variables.  In the case of the UtilityCost:Charge objects, the name of the object is also used as a name of a variable.

In many of today’s utility rates, the charges for energy or demand are broken into distribution and supply charges.  To allow for this, more than one charge may to be defined for a particular category.  The variables assigned to the same category are added together.

The categories are combined in following hierarchy:

#### ![](media/image7909.png)

Any charges included in the EnergyCharges category are added together. The EnergyCharges, DemandCharges and ServiceCharges are added together to form the Basis. The Basis, Adjustments and Surcharges are added together to form the Subtotal. The Subtotal and Taxes are added together to be the Total.  The total represents the total monthly charges on that tariff for the energy source used.  The combining of categories together is performed automatically unless the user specifies the UtilityCost:Computation.  In addition, each category, which is also a variable, may be used as a source. For example, a tax that is 5% of the subtotal would be shown as:

```idf
UtilityCost:Charge:Simple,
  TaxOfFivePercent,        ! Charge Variable Name
  TariffExample1,          ! Tariff Name
  Subtotal,                ! Source Variable
  Annual,                  ! Season
  Taxes,                   ! Category Variable Name
  0.05;                    ! Cost Per Unit Value (or Variable)
```

As you can see, the UtilityCost:Charge:Simple and UtilityCost:Charge:Block objects do most of the “work” of computing the annual energy cost.  The benefit of using this categorization is that totals of each category are shown in the output reports and it organizes the charges in the monthly calculations in a logical way that fits almost all tariffs.  If no categorization is desired, theoretically, all charges could be assigned to the Total category. The categories themselves are simply variable names. Charges may also be assigned to the “NotIncluded” category if the result of the charge is used as an intermediate calculation and should not be included in the Total.

The objects that create variables are:

* UtilityCost:Qualify

* UtilityCost:Charge:Simple

* UtilityCost:Charge:Block

* UtilityCost:Ratchet

* UtilityCost:Variable

### Default Order of Computation

The user has the option of two different ways to determine the order of computation. If an UtilityCost:Computation object is specified for the tariff, the sequence specified in that object is used for computing the various variables. If no UtilityCost:Computation object is specified, a sequence of computational steps is automatically derived and shown as part of the report. The routine that creates this automatic sequence of computation steps is called CreateDefaultComputation as part of the EconomicTariff module.

The order in which the computation should be made is complicated by the fact that the objects can each have variables that are inputs and others that are outputs.  Since any of the variables can be used as inputs, we must ensure that they are computed prior to being used. In other words, because the objects allow a great deal of flexibility, there is no simple default order that the computations should be made.

Luckily there are known algorithms for sorting though these types of interdependencies.  In fact, the method that spreadsheets use for sorting through the dependencies of cell formulas referencing other cells with formula is very similar.  In addition, linkers (used as part of the computer language compiling process) face similar issues of sorting through dependences. Figuring out the optimal path in a complex project represented by a PERT Chart also uses a similar algorithm.

Generically, dependency problems are usually characterized as Directed Acycle Graphs (DAGs). A DAG shows the individual formulas as circles and uses arrows between the circles to show which formula is dependent on which other formulas.  One of the simplest explanations can be seen at the following website:

 (Click on lecture 14 “Critical Path”).

The lecture and site was created by Alison Cawsey, Department of Computing and Electrical Engineering, Heriot-Watt University Edinburgh EH14 4AS, UK.  The specific algorithm that was used in EnergyPlus is described at this site and is quoted below:

Calculate, for each node, the in-degree of that node (ie, now many edges end up there). Store these in array D.

Repeat:

– Remove (output) node such that D[n]=0.

– Decrement D[x] for all nodes x that are neighbors of n (edge from n to x).

Of course in this case “node” has nothing to do with EnergyPlus nodes but is just describing one of the formulas in a DAG.  This is just one of several different methods to solve a DAG.  The general method for solving a DAG is called a topological sort. The algorithm used in EnergyPlus is one of the simplest methods available and is appropriate given the number of dependencies.  More efficient algorithms are known but are probably only appropriate for much larger number of dependencies.

One important note, if after the algorithm is exercised, and some of the formulas still have a count on the number of dependencies, it must be the result of a circular dependency and an error condition is flagged in the ERR file.

The objects have specific variables that are used as inputs and outputs, and thus the outputs are dependent on the inputs, are shown in the following table:



***Object Variables Inputs and Outputs***

<table class="table table-striped">
<tr>
<th>Object</th>
<th>Outputs</th>
<th>Inputs</th>
</tr>
<tr>
<td>Qualify</td>
<td>Name</td>
<td>Source</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td>Threshold</td>
</tr>
<tr>
<td>Charge:Simple</td>
<td>Name</td>
<td>Source</td>
</tr>
<tr>
<td> </td>
<td>Category</td>
<td>Cost Per Unit</td>
</tr>
<tr>
<td>Charge:Block</td>
<td>Name</td>
<td>Source</td>
</tr>
<tr>
<td> </td>
<td>Category</td>
<td>Block Size Multiplier</td>
</tr>
<tr>
<td> </td>
<td>Remaining</td>
<td>Block Size</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td>Block Cost</td>
</tr>
<tr>
<td>Ratchet</td>
<td>Name</td>
<td>Baseline</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td>Adjustment</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td>Multiplier</td>
</tr>
<tr>
<td> </td>
<td> </td>
<td>Offset</td>
</tr>
</table>



In addition, the hierarchy shown in the first diagram in this section also represents dependencies that are included when determining the order of computation.

The resulting order of computation is shown at the bottom of the economics report.

### Computation Steps

Once the order that the formulas should be computed is known, the actual evaluation of the formulas is based on a simple Last In First Out (LIFO) stack.  This is a common method to compute expressions where values are stored on the stack and operands work off of the top of the stack.

Life-Cycle Cost Computations
----------------------------

The objects used for performing a life-cycle cost analysis are:

* LifeCycleCost:Parameters

* LifeCycleCost:RecurringCosts

* LifeCycleCost:NonrecurringCost

* LifeCycleCost:UsePriceEscalation

* LifeCycleCost:UseAdjustment

The computation of life-cycle costs is broken into three main routines which are described below.

### ExpressAsCashFlows

Step 1. If the input file has ComponentCost:\* items, then create an additional instance of a nonrecurring cost to hold the total.

Step 2. Get the costs for each resource that has non-zero utility costs.

Step 3. Compute the inflation on a monthly basis. For cases where the inflation approach is constant dollars, the inflation is set to 1.0 for all months. For current dollar analyses, compute the inflation rate on a monthly basis. Just using 1/12 of the annual inflation is almost correct but introduces a small error so instead the inverse of the formula 4-32 from Newnan (Engineering Economic Analysis Ninth Edition by Donald Newnan, Ted Eschenbach, and Jerome Lavelle):

inflationPerMonth = ((inflation + 1.00) \*\* (1.0/12.0)) - 1

Then the inflation is applied for each month:

monthlyInflationFactor(jMonth) = (1 + inflationPerMonth) \*\* (jMonth - 1)

Step 4. Put the nonrecurring, nonrecurring, and utility costs into a monthly array for the entire length of the study period.

Step 5. Multiply the monthly costs by the monthly inflation which was set to 1.0 for constant dollar analyses.

Step 6. Multiply the monthly costs for resources with use adjustments.

Step 7. Sum the monthly costs by category.

Step 8. Based on the base date, accumulate the monthly costs into yearly costs.

### ComputePresentValue

Step 1. For each year of the study compute the discount factor (SPV) using the following formula:

SPV<sub>yr</sub> = 1 / ((1 + curDiscountRate) \*\* effectiveYear)

This formula is based on formula D.2.1.1 from NIST Handbook 135 Life Cycle Costing Manual for the Federal Energy Management Program by Sieglinde K. Fuller and Stephen R. Petersen.

Where the effectiveYear depends on the discount convention. If end of year discounting is used than the effectiveYear is the year. If middle of the year discounting is used than the effectiveYear is reduced by 0.5. If the beginning of year discounting is used, than the effectiveYear is reduced by 1.0.

For energy costs, the use price escalations are multiplied by the discount factors.

Step 2. Compute the present value for each month by multiply the monthly costs by the discount factor for each year.

Step 3. Sum the costs by category.

### ComputeTaxAndDepreciation

Step 1. Depending on the depreciation method selected the depreciation factors are set to various constants. Depreciation factors are based on IRS Publication 946 for 2009 "How to Depreciate Property." The MACRS values are based on Modified Accelerated Cost Recovery System GDS for 3, 5, 7, 10 year property are based on 200% depreciation method shown in Appendix A of IRS Publication 946 using half year. 15 and 20 are based on 150% (Chart 1 of IRS Publication 946). For Straight Line depreciation GDS is used for 27 years (actually 27.5) 31 years (actually 31.5 years) and 39 years using mid month. For 40 years ADS is used (chart 2) Table A-1 is used for 3, 4, 5, 10, 15 and 20 years. Table A-6 is for 27 years. Table A-7 for 31 years. Table A-7a for 39 years. Table A-13 for 40 years. These years are a classification of property and should not be confused with the length of the study. For 27 years, 31 years, 39 years and 40 years the June value was used. All references in this paragraph are to IRS Publication 946.

Step 2. Apply the annual depreciation factors to the capital costs.

Step 3. For each year the taxable income is the grand total of all costs minus the depreciated capital costs.

Step 4. Taxes are the taxable income times the tax rate.

Step 5. The after tax present value is

AfterTaxPresentValue<sub>yr</sub> = GrandTotal<sub>yr</sub> - Taxes<sub>yr</sub> \* SPV<sub>yr</sub>

All major results are presented on the tabular report.





