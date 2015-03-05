# Component Costs

EnergyPlus provides simple cost estimating capabilities as an aid to design analysis and for life cycle costs.  There are three broad steps involved.  The first involves determining *construction* costs by summing individual "line items."  The second involves determining *project* costs by adjusting construction costs to account for things like profit and design fees.  The third involves comparing the current simulation to a reference case so that marginal increases can be calculated.  The reference documentation contained in this section pertains to the following input object names.

- **ComponentCost:LineItem**
- **ComponentCost:Adjustments**
- **ComponentCost:Reference**

## Line Item Costs

Line item cost calculations are generally trivial involving simple multiplication and summation.  This section documents details of how line item costs are calculated.  The program code consists mainly of a **Case** construct where the **Line Item Type** is used to control the details of how calculations are performed.

The overall philosophy is to provide methods of calculating items using either direct entry of needed data (**‘General'** using object type), or using component descriptive data entered elsewhere in the input file (e.g. **‘Lights'**), or by using quantities that are calculated by the program during the simulation (e.g. **‘Coil:DX'** and **‘Chiller:Electric'**).

The rest of this section provides details by organized by the type of line item.

### General

The line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P~e~*:

![](media/image7367.png)\


### Construction

This line item type is called "Construction" but is used to estimate the costs of surfaces in the model.  The Construction attribute of Surface objects is useful for categorizing surfaces.  The number of units, *N*, is determined by summing the Area, *A*, of Surface objects that have the type of construction specified in the **Item Name** field.  Surfaces are screened to eliminate any duplicates that may exist for interior partitions creating a list of 1 to *m* unique surfaces.  If a surface is associated with a Zone, then zone multiplier, *M~Z~*, and list multipliers, *M~G~*, are applied (these are usually defaulted to 1).

![](media/image7368.png)\


The line item subtotal, *L*, is calculated by multiplying the number of units (actually units of are here), *N* (m^2^) by the Cost-per-Area, *P~a~~~*, (\$/m^2^):

![](media/image7369.png)\


### Coil:DX and Coil:Cooling:DX:SingleSpeed

DX coil costs can be estimated in one of three ways: per-each, per-kW, per-kW-COP.  The program determines which method to use based on there being non-zero values in appropriate input fields.

If **cost per each** is greater than 0.0 then the number of units, *N*, is the number of cooling coils.  This will be 1 if the **Item Name** is the name of a specific coil described elsewhere.  If the name is set to the wildcard (\*) then this will equal the total number of DX:Coils in the model.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P~e~*:

![](media/image7370.png)\


If **cost per kilowatt** is greater than 0.0 then the number of units, *N*, is the number of kilowatts of total, rated, cooling capacity.  This will be based on all the DX:Coils in the model if **Item Name** is set to the wildcard (\*) and will be that of the named coil if set to a valid coil name.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt, *P~kW~*:

![](media/image7371.png)\


If **Cost per kilowatt per coefficient-of-performance** is greater than 0.0 then the number of units, *N*, is the number of kilowatts of total, rated, cooling capacity multiplied by the nominal coefficient of performance (COP) of the DX:Coils.  This will be based on all the DX:Coils in the model if **Item Name** is set to the wildcard (\*) and will be that of the named coil if set to a valid coil name.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt-per-COP, *P~kW-COP~*:

![](media/image7372.png)\


### Coil:Heating:Gas

Gas-fired heating coil costs can be estimated in one of three ways: per-each, per-kW, per-kW-COP.  The program determines which method to use based on there being non-zero values in appropriate input fields.

If **cost per each** is greater than 0.0 then the number of units, *N*, is the number of heaters.  This will be 1 if the **Item Name** is the name of a specific heater described elsewhere.  If the name is set to the wildcard (\*) then this will equal the total number of Coil:Heating:Gas objects in the model.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P~e~*:

![](media/image7373.png)\


If **Cost per kilowatt** is greater than 0.0 then the number of units, *N*, is the number of kilowatts of total, rated, heating capacity.  This will be based on all the Coil:Heating:Gas objects in the model if **Item Name** is set to the wildcard (\*) and will be that of the named coil if set to a valid coil name.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt, *P~kW~*:

![](media/image7374.png)\


If **Cost per kilowatt per coefficient-of-performance** is greater than 0.0 then the number of units, *N*, is the number of kilowatts of total, rated, heating capacity multiplied by the theoretical efficiency of the heater(s).  (Here we are treating gas heating efficiency as a Coefficient of Performance (COP)).  This will be based on all the Coil:Heating:Gas objects in the model if **Item Name** is set to the wildcard (\*) and will be that of the named coil if set to a valid coil name.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt-per-COP, *P~kW-COP~*:

![](media/image7375.png)\


### Chiller:Electric

Electric chiller costs can be estimated in one of three ways: per-each, per-kW, per-kW-COP.  The program determines which method to use based on there being non-zero values in appropriate input fields.

If **cost per each** is greater than 0.0 then the number of units, *N*, is the number of chillers.  This will be 1 if the **Item Name** is the name of a specific coil described elsewhere.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P~e~*:

![](media/image7376.png)\


If **Cost per kilowatt** is greater than 0.0 then the number of units, *N*, is the number of kilowatts of total, rated, cooling capacity for the specified chiller.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt, *P~kW~*:

![](media/image7377.png)\


If **Cost per kilowatt per coefficient-of-performance** is greater than 0.0, then the number of units, *N*, is the number of kilowatts of total, rated, cooling capacity multiplied by the nominal coefficient of performance (COP) of the chiller.  This will be based on the named chiller (if set to a valid coil name).  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt-per-COP, *P~kW-COP~*:

![](media/image7378.png)\


### Daylighting:Controls

The costs of controllers for harvesting daylight are determined by the number of reference points.  The cost for each controller, *P~e~* , are input.  The of units, *N*, is determined from the number of daylight reference points in all the zones if the **Item Name** is the wildcard (\*).  If **Item Name** is set to a valid Zone name then N is the number of daylight reference points in just that zone (zones can have more than one daylight controllers).  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P~e~*:

![](media/image7379.png)\


### Shading:Zone:Detailed

Shading surfaces need to be handled separately because they do not have the **Construction** attribute.  The **Item Name** must be set to a valid name for a **Shading:Zone:Detailed** object defined elsewhere in the file.  The number of units, N, is determined from the area of the named surface multiplied by zone multiplier, *M~Z~*, and list multipliers, *M~G~*:

![](media/image7380.png)\


The line item subtotal, *L*, is calculated by multiplying the number of units, *N* (m^2^) by the Cost-per-Area, *P~a~~~*, (\$/m^2^):

![](media/image7381.png)\


### Lights

The cost of electric lighting systems can be modeled in two ways: per-each and per-kW.  The program determines which method to use based on there being non-zero values in appropriate input fields.  The **Item Name** must be the name of a valid Zone defined elsewhere in the input.

If **cost per each** is greater than 0.0, then the number of units, *N*, is the number lighting systems in the zone and is assumed to be 1.  Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-Each, *P~e~*:

![](media/image7382.png)\


If **Cost per kilowatt** is greater than 0.0 then the number of units, *N*, is the number of kilowatts in the design level for electric lighting systems defined in Lights objects associated with the zone.  The **Item Name** must be the name of a valid Zone defined elsewhere in the input.  *N* is then the sum of all the Lights associated with the named Zone. Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt, *P~kW~*:

![](media/image7383.png)\


### Generator:Photovoltaic

The costs of simple photovoltaic power systems can be modeled using cost per kilowatt.  The number of units, N, is the nominal rated peak power of the photovoltaic panels.  The photovoltaic generator must be modeled using the PhotovoltaicPerformance:Simple input object which is a very simplistic model and does not include input for the nominal rated peak power of the system.  Therefore a peak power is calculated using the usual 1000 W/m^2^ of incident solar radiation, *G~p~*, multiplied by the active solar cell area, *A* and the efficiency, *E*, and converted to units of kilowatts.

![](media/image7384.png)\


Where, the cell area A is calculated by multiplying the area of the surface associated with the simple photovoltaic system and the active area fraction defined in the PhotovoltaicPerformance:Simple. Then the line item subtotal, *L*, is calculated by multiplying the number of units, *N*, by the Cost-per-kilowatt, *P~kW~*:

![](media/image7385.png)\


## Adjustments

Various adjustments are available to model total project costs from the component costs modeled using individual line items.  These adjustments are provided to allow propagating how changes in component costs are amplified by things like design fees, profit, bonding, and taxes.

The subtotal of individual line items, *S~L~*, is obtained by summing all line item subtotals, *L*:

![](media/image7386.png)\


For the reference building, *S~L,~*is user input (typically from a prior run of baseline building model).

The subtotal for miscellaneous construction costs (typically those costs not explicitly included as line items), *S~m~*, are calculated by multiplying the **Miscellaneous Cost Model** (per Square Meter), *C~m~*,  by the total conditioned floor area, *A~c~*:

![](media/image7387.png)\


*The subtotal for the amount that the construction costs should be altered because of regional differences in material and labor costs (e.g. when using national average data), S~r~*, is determined by multiplying the Regional Adjustment Factor, *R~f ,~* by the sum of *S~L~* and *S~m:~*

![](media/image7388.png)\


*Remaining adjustments are applied to the subtotal for construction costs, S~c~*, which is the sum of *S~L~*, *S~m,~*, and *S~r:~*

![](media/image7389.png)\


The **Design and Engineering Fee Fraction**, *F~d~*, is the fraction of construction costs, *S~c~*, attributable to costs associated with architectural and engineering services needed for the project.  The subtotal for costs associated with these fees, *S~D~*, are determined by multiplying *S~C~* by *F~d~*:

![](media/image7390.png)\


The **Contractor Fee Fraction**, *F~c~*, is the fraction of construction costs, *S~c~*, attributable to costs associated with the contractor's profit that should be included in the project.  The subtotal for costs associated with contracting, *S~P~*, are determined by multiplying *S~C~* by *F~d~*:

![](media/image7391.png)\


The **Contingency Fraction**, *F~S~*, is the fraction of construction costs, *S~c~*, which should be included in a cost estimate to provide for contingencies (perhaps related to errors and uncertainty in the estimate and project).  The subtotal for contingency costs, *S~S~*, are determined by multiplying *S~C~* by *F~S~*:

![](media/image7392.png)\


The **Permits, Bonding, Insurance Fraction,** *F~B~*, is the fraction of construction costs, *S~c~*, which should be included in a cost estimate to provide for things like taxes, bonding, insurance, and permits.  The subtotal for these extra costs, *S~B~*, are determined by multiplying *S~C~* by *F~B~*:

![](media/image7393.png)\


The **Commissioning Fee Fraction,** *F~CX~*, is the fraction of construction costs, *S~c~*, which should be included in a cost estimate to provide for commissioning the building systems.  The subtotal for these extra costs, *S~CX~*, are determined by multiplying *S~C~* by *F~CX~*:

![](media/image7394.png)\


Finally, the cost estimate total, *T*, is obtained by summing the various subtotals:

![](media/image7395.png)\


The normalized total cost estimate, C, is calculated by dividing *T* by the total conditioned floor area, *A~c~*:

![](media/image7397.png)\


## Comparisons

The capability of comparing the current cost estimate to that of a reference building is provided because is common to consider the marginal increases in costs associated with applying different energy design measures.  EnergyPlus calculates and reports the difference between the current model and a reference case for all the subtotals and totals discussed above.  The reported differences are the reference values subtracted from the current value.
