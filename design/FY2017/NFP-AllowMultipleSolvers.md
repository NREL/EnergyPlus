Allow multiple solvers to find a root
================

** Lixing Gu **

** Florida Solar Energy Center **

 - 4/6/17
 - Original NFP
 

## Justification for New Feature ##

The Regula Falsi method is used to find a root. The main application of root finding in EnergyPlus is to find a part load ratio or mass flow rate at given equipment/system load. The method assumes a general curve between two points of “a” and “b” as a straight line to find a root, and is the only method used in EnergyPlus internally. However, it may not reach convergence when the given equipment/system load is small. Ticket 6862 shows divergence with a small load. The main concern is that a straight line approach may fail to find a root within a small range. A good solution method is needed to avoid possible divergence.

![Regula Falsi method](RegulaFalsiMethod.jpg)
  
## E-mail and  Conference Call Conclusions ##

None

## Overview ##

The Regula Falsi method is a unique solver to find a root with given load in EnergyPlus. However, the method is unable to find a solution with small loads sometimes. A hybrid approach is proposed to combine Regula Falsi and bisection methods together. Regula Falsi method will be used first. If the given load is too small and the number of iterations is above a certain number, the bisection method will be used to find a solution. The proposed approach will provide a solution to reduce possibility of divergence. 

## Approach ##

The proposed approach is to use a hybrid approach by combining Regula Falsi and bisection methods together. Although bisection method is slow in general, it finds a solution eventually. From a point of view of convergence, the bisection method is better than Regula Falsi. The proposed approach is to use the Regula Falsi method first to narrow the working range. If the given load is too small and the number of iteration is above the value given in the Number of Iteration Before Bisection field in the proposed new object of HVACSystemRootFindingAlgorithm, the bisection method will be used.  The hybrid approach will take advantage of Regula Falsi to narrow the working range first and fast, and find a root eventually at given small loads using the bisection method when the Regula Falsi method may not find a solution. 

A new object is proposed:

	HVACSystemRootFindingAlgorithm,
  	A1 , \field Algorithm
       \type choice
       \key RegulaFalsi
       \key RegulaFalsiThenBisection
       \default RegulaFalsi
  	N1 ; \field Number of Iteration Before Bisection
       \note This field is used when RegulaFalsiThenBisection is entered.
       \type integer
       \default 0

### Note

It is possible to be hard-wired in the code without the new object. However, it may cause differences for every example file. In addition, the proposed object provides more flexibility for users to select which algorithm will be used. Furthermore, the new object may be expanded in the future, in case a new algorithm may be proposed and added.
  
## Testing/Validation/Data Sources ##

Compare simulation results using Regula Falsi only and multiple solvers.

It should be pointed out that differences are expected. The main check will be energy use during a simulation period.

## Input Output Reference Documentation ##

### HVACSystemRootFindingAlgorithm

The HVACSystemRootFindingAlgorithm object provides a way to select what type of solution
algorithm will be used to find a part load ratio or mass flow rate at given equipment/system load in HVAC system simulations. This object is an optional object. If the default algorithm is used, this object is not required in an input file.

#### Inputs

##### Field: Algorithm
Two choices are allowed to select which solution algorithm will be used. The RegulaFalsi
selection is the default selection. The RegulaFalsiThenBisection selection requires the program to apply the RegulaFalsi method first. After the number of iteration is above the value defined in the next field, the bisection algorithm will be applied.   

##### Field: Number of Iteration Before Bisection
This field define the number of iterations used by the RegulaFalsi method, when the selection of the above field is RegulaFalsiThenBisection. The bisection algorithm is applied, when the number of iteration is above the this number.

A default IDF example is shown below:

	HVACSystemRootFindingAlgorithm,
		RegulaFalsiThenBisection,
		20;

## Input Description ##

A new object is proposed.

	HVACSystemRootFindingAlgorithm
  	A1 , \field Algorithm
       \type choice
       \key RegulaFalsi
       \key RegulaFalsiThenBisection
       \default RegulaFalsi
  	N1 ; \field Number of Iteration Before Bisection
       \note This field is used when RegulaFalsiThenBisection is entered.
       \type integer
       \default 0


## Outputs Description ##

insert text

## Engineering Reference ##

insert text

## Example File and Transition Changes ##

An existing example file will be modified using the proposed approach.

No transition is needed.

## References ##

[Multiple solvers to find a root](https://github.com/NREL/EnergyPlusDevSupport/blob/master/DesignDocuments/EnhancementList/HVAC_General_2013_04.doc)



