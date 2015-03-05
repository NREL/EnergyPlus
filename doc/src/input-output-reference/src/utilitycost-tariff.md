# UtilityCost:Tariff

Defines the name of the tariff, the type of tariff, and other details about the overall tariff. Each other object that is part of the tariff model references the tariff name including the UtilityCost:Charge:Simple, UtilityCost:Charge:Block, UtilityCost:Ratchet, UtilityCost:Qualify, UtilityCost:Variable and UtilityCost:Computation objects.

Multiple UtilityCost:Tariff objects can appear in the same IDF file. It is common that electric and gas utilities have more than one rate that may be used. Only one UtilityCost:Tariff for each [Output:Meter](#outputmeter-and-outputmetermeterfileonly) object and Group Name is shown in the Economics Summary Report. The one that is shown in that report is chosen by first eliminating UtilityCost:Tariff objects that are not qualified due to their UtilityCost:Qualify objects. If more than one UtilityCost:Tariff object remains, the one that has the lowest cost for a combination of a Meter and Group Name will be selected. If the Group Name field is not used, then the lowest cost UtilityCost:Tariff is chosen for each Meter.

Part of the tariff description is used to transform the annual energy use of any fuel under any meter into a group of named variables containing the values for each of the periods that the bills are calculated (usually monthly). The schedules create a variable for energy use and for demand. If specified it also creates a variable for energy and a variable for demand for the "on-peak", "off-peak", and "shoulder" periods for use in time-of-use tariffs. It performs this allocation prior to other calculations for the tariff. The variables that are always created are:

- totalEnergy
- totalDemand

## Inputs

#### Field: Name

The name of the tariff. Tariffs are sometimes called rates. The name is used in identifying the output results and in associating all of the charges and other objects that make up a tariff.

#### Field: Output Meter Name 

The name of any meter defined but usually set to either Electricity:Facility or Gas:Facility.

#### Field: Conversion Factor Choice 

A choice that allows several different predefined conversion factors to be used; otherwise, user defined conversion factors are used as defined in the next two fields.

- UserDefined
- kWh
- Therm
- MMBtu
- MJ
- KBtu
- MCF
- CCF

The following table shows the conversion factors when specifying one of the predefined choices:

Table: Built in UtilityCost:Tariff Conversion Factors

Choice|Energy Conversion Factor|Demand Conversion Factor
------|------------------------|------------------------
kWh|0.0000002778|0.001
Therm|9.4781712E-09|0.00003412
MMBtu|9.4781712E-10|0.000003412
MJ|0.000001|0.0036
KBtu|9.4781712E-07|0.003412
MCF|9.4781712E-10|0.000003412
CCF|9.4781712E-09|0.00003412

#### Field: Energy Conversion Factor 

Is a multiplier used to convert energy into the units specified by the utility in their tariff. If left blank, it defaults to 1 (no conversion). This field will be used only if Conversion Factor Choice is set to UserDefined. Within EnergyPlus energy always has units of J (joules). For conversion from J to kWh use the value of 0.0000002778. This is also used for all objects that reference the UtilityCost:Tariff.

#### Field: Demand Conversion Factor

Is a multiplier used to convert demand into the units specified by the utility in their tariff. If left blank, it defaults to 1 (no conversion). This field will be used only if Conversion Factor Choice is set to UserDefined. Within EnergyPlus demand always has units of J/s (joules/sec) which equivalent to W (watts). For conversion from W to kW use the value of 0.001. This is also used for all objects that reference the UtilityCost:Tariff.

#### Field: Time of Use Period Schedule Name

The name of the schedule that defines the time-of-use periods that occur each day. The period schedule is used to determine which variables are defined. The values for the different variables are:

- 1 is Peak
- 2 is Shoulder
- 3 is OffPeak
- 4 is MidPeak

The following variables are created automatically if these different periods are used in the schedule and include:

- peakEnergy
- peakDemand
- shoulderEnergy
- shoulderDemand
- offPeakEnergy
- offPeakDemand
- midPeakEnergy
- midPeakDemand

Some special variables are created that include only demand from one period that exceeds the demand from another period. For other months that do not exceed, the values are zero. These variables are seldom used but are available for the occasional rate that includes a clause looking at demands during one period that exceed another period.

- PeakExceedsOffPeak
- OffPeakExceedsPeak
- PeakExceedsMidPeak
- MidPeakExceedsPeak
- PeakExceedsShoulder
- ShoulderExceedsPeak

If no Period Schedule is specified then no peak, offPeak, midPeak or shoulder variables are defined.

#### Field: Season Schedule Name

The name of a schedule that defines the seasons. The seasons are

- 1 is Winter
- 2 is Spring
- 3 is Summer
- 4 is Autumn

The change in the season schedule must occur at the same time as the change in the Monthly Schedule. In other words, a season must end at the same time as a billing month. Variables are automatically created if a season schedule is used. These variables are set to 1 within the season and 0 for the months that are not in the season. The variables are:

- isWinter
- isNotWinter
- isSpring
- isNotSpring
- isSummer
- isNotSummer
- isAutumn
- isNotAutumn

#### Field: Month Schedule Name

The name of the schedule that defines the billing periods of the year. Normally this entry is allowed to default and a schedule will be internally used that has the breaks between billing periods occurring at the same time as the breaks between months, i.e., at midnight prior to the first day of the month. If other billing periods are used such as two month cycles or a single bill for an entire season, such as some natural gas companies do in the summer, then the month schedule may be used to redefine it. Make sure that the month schedule and season schedule are consistent, otherwise an error will be issued.

- 1 is the first month, usually January
- 2 is the second month, usually February
- 3 is the third month, usually March
- 4 is the fourth month, usually April
- 5 is the fifth month, usually May
- 6 is the sixth month, usually June
- 7 is the seventh month, usually July
- 8 is the eighth month, usually August
- 9 is the ninth month, usually September
- 10 is the tenth month, usually October
- 11 is the eleventh month, usually November
- 12 is the twelfth month, usually December

Thus, the schedule that represents this would look like the following. If no schedule is specified the following Month Schedule is used which defines the months of the year as the normal calendar months.

~~~~~~~~~~~~~~~~~~~~

    Schedule:Compact, defaultMonths, number,
    Through: 1/31, For: AllDays, Until: 24:00, 1,
    Through: 2/28, For: AllDays, Until: 24:00, 2,
    Through: 3/31, For: AllDays, Until: 24:00, 3,
    Through: 4/30, For: AllDays, Until: 24:00, 4,
    Through: 5/31, For: AllDays, Until: 24:00, 5,
    Through: 6/30, For: AllDays, Until: 24:00, 6,
    Through: 7/31, For: AllDays, Until: 24:00, 7,
    Through: 8/31, For: AllDays, Until: 24:00, 8,
    Through: 9/30, For: AllDays, Until: 24:00, 9,
    Through: 10/31, For: AllDays, Until: 24:00, 10,
    Through: 11/30, For: AllDays, Until: 24:00, 11,
    Through: 12/31, For: AllDays, Until: 24:00, 12;
~~~~~~~~~~~~~~~~~~~~

#### Field: Demand Window Length

The determination of demand can vary by utility. Some utilities use the peak instantaneous demand measured but most use a fifteen minute average demand or a one hour average demand. Some gas utilities measure demand as the use during the peak day or peak week. The choices for demand window are:

- QuarterHour
- HalfHour
- FullHour
- Day
- Week

If no value is entered, QuarterHour is assumed. The choice may be overridden based on the value of [Timestep](#timestep) so that they are consistent. If the [Timestep](#timestep) is 6, i.e. 10 minute timesteps, then QuarterHour is not used and instead HalfHour is used instead. If [Timestep](#timestep) is 3, i.e. 20 minute timesteps, then QuarterHour and HalfHour are not used and instead FullHour is used instead. Day and Week are primarily used by fuel utilities.

#### Field: Monthly Charge or Variable Name

The fixed monthly service charge that many utilities have. The entry may be numeric and gets added to the ServiceCharges variable or if a variable name is entered here, its values for each month are used.

#### Field: Minimum Monthly Charge or Variable Name

The minimum total charge for the tariff or if a variable name is entered here, its values for each month are used.  The sum of the subtotal and taxes usually equals the total unless you have specified the minimum monthly charge. If the minimum monthly charge does not apply to the entire bill, a UtilityCost:Computation object can be specified using a Minimum function for the appropriate variables.

#### Field: Real Time Pricing Charge Schedule Name

Used with real time pricing rates. The name of a schedule that contains the cost of energy for that particular time period of the year. Real time rates can be modeled using a charge schedule with the actual real time prices entered in the schedule. The charges should be consistent with the conversion factor specified in the tariff.

#### Field: Customer Baseline Load Schedule Name

Used with real time pricing rates and often described as the CBL or customer baseline load. The name of a schedule that contains the baseline energy use for the customer. Many real time rates apply the charges as a credit or debit only to the difference between the baseline use and the actual use. The baseline use is established between the customer and the utility using a contract. If this field is used the baseline use schedule will be used for establishing the totalEnergy and totalDemand instead of the metered value and the real time rate charge is added or subtracted to the rate calculation using the real time charge schedule. If this field is not used, the real time charge schedule applies to all energy from the meter and totalEnergy and totalDemand is based on metered value as usual.

#### Field: Group Name

The group name of the tariff such as distribution, transmission, supplier, etc. If more than one tariff with the same group name is present and qualifies, only the lowest cost tariff is used. Usually the group name field is left blank which results in all tariffs using the same meter variable being compared and the lowest cost one being selected.

#### Field: Buy Or Sell

Sets whether the tariff is used for buying, selling or both to the utility. This should be allowed to default to BuyFromUtility unless a power generation system is included in the building that may generate more power than the building needs during the year. The choices are:

- BuyFromUtility – The values from the metered variable are used and are shown as being purchases from the utility.
- SellToUtility – The values from the metered variable are used for a "sell back" rate to the utility. The charges in the rate should be expressed as negative values.
- NetMetering – Negative values are used to reduce any positive values during the specific period on the tariff when negative values occur.

A warning will be issued if the selection of this field does not match the type of meter.