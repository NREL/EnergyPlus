# UtilityCost:Computation

The UtilityCost:Computation object lists a series of computations that are used to perform the bill calculation. The object is only used for complex tariffs that cannot be modeled any other way. For 95% of the utility tariffs, using UtilityCost:Computation is unnecessary and should be avoided. If the UtilityCost:Computation object is used, it must contain references to all objects involved in the rate in the order that they should be computed.

## Inputs

#### Field: Name

The name of the UtilityCost:Computation.

#### Field: Tariff Name

The name of the tariff that this UtilityCost:Charge:Simple is associated with.

#### Field: Compute Step N

The Compute Step fields contain a simple language that describes the steps used in the computation process similar to a programming language. If no entry is made to any Compute Step fields (as usually the case) a default method of computing the rates shall be used. Compute Step is described later in this section under Complex Tariff Modeling.

Remember, this object should be omitted for almost all tariffs.