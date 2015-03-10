# Conceptual Framework – Variables and Hierarchy

To understand how to use the utility bill calculation portion of EnergyPlus you first need to understand some important concepts of variables and hierarchy. A variable, for the purposes of this section, is simply a named holder of a series of numbers. In most cases, the variable will be a named holder of 12 numbers, one number for each monthly utility bill. Here is a visualization of a variable called Electric Energy Use:

Table: Example Electric Energy Use (for Economics Calculation)

Month|Electric Energy Use
-----|-------------------
January|12143
February|13454
March|14178
April|14876
May|15343
June|16172
July|16105
August|15762
September|14543
October|13987
November|13287
December|12403

If you have ever done any computer programming, you can think of a variable as an array. Many of the names used in the utility bill calculation portion of EnergyPlus are names of variables. In the case of the UtilityCost:Charge objects, the name of the object is also used as a name of a variable.

In many of today's utility rates, the charges for energy or demand are broken into distribution and supply charges. To allow for this, more than one charge may to be defined for a particular category. The variables assigned to the same category are added together.

The categories are combined in following hierarchy:

![Hierarchy for Economics Charges](media/hierarchy-for-economics-charges.jpeg)


Any charges included in the EnergyCharges category are added together. The EnergyCharges, DemandCharges and ServiceCharges are added together to form the Basis. The Basis, Adjustments and Surcharges are added together to form the Subtotal. The Subtotal and Taxes are added together to be the Total. The total represents the total monthly charges on that tariff for the energy source used. The combining of categories together is performed automatically unless the user specifies the UtilityCost:Computation. In addition, each category, which is also a variable, may be used as a source. For example, a tax that is 5% of the subtotal would be shown as:

~~~~~~~~~~~~~~~~~~~~

    UtilityCost:Charge:Simple,
      TaxOfFivePercent,! Charge Variable Name
      TariffExample1,! Tariff Name
      Subtotal,! Source Variable
      Annual,! Season
      Taxes, ! Category Variable Name
      0.05;! Cost Per Unit Value or Variable Name
~~~~~~~~~~~~~~~~~~~~

As you can see, the UtilityCost:Charge:Simple and UtilityCost:Charge:Block objects do most of the "work" of computing the annual energy cost. The benefit of using this categorization is that totals of each category are shown in the output reports and it organizes the charges in the monthly calculations in a logical way that fits almost all tariffs. If no categorization is desired, theoretically, all charges could be assigned to the Total category. The categories themselves are simply variable names. Charges may also be assigned to the "NotIncluded" category if the result of the charge is used as an intermediate calculation and should not be included in the Total.

The rest of this section is divided into a reference of the objects and the fields, a set of examples, discussion of the reporting, and a description of Complex Tariff Modeling. For most tariffs, simply using a variety of UtilityCost:Charge:Simple's and UtilityCost:Charge:Block's will result in a good model but for some more complex tariffs, especially ones with complex block sizes, variables may be computed using a very simple programming language that may appear in the UtilityCost:Computation object. This will be described more in the Complex Tariff Modeling section.