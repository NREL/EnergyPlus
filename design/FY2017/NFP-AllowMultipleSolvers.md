Allow multiple solvers to find a root
================

** Lixing Gu **

** Florida Solar Energy Center **

 - 5/3/17
 - First revision based on conversation with Rich
 - 4/6/17
 - Original NFP
 

## Justification for New Feature ##

The Regula Falsi method is used to find a root. The main application of root finding in EnergyPlus is to find a part load ratio or mass flow rate at given equipment/system load. The method assumes a general curve between two points of “a” and “b” as a straight line to find a root, and is the only method used in EnergyPlus internally. However, it may not reach convergence when the given equipment/system load is small. Ticket 6862 shows divergence with a small load. The main concern is that a straight line approach may fail to find a root within a small range. A good solution method is needed to avoid possible divergence.

![Regula Falsi method](RegulaFalsiMethod.jpg)
  
## E-mail and  Conference Call Conclusions ##

No comments were received. Therefore, no conference call was arranged. However, Rich and I had a talk about this new feature.

### Rich's opinion 

Rich would like to start to use bisection method first. There is a section in the VRFTerminalUnitEquipment::ControlVRF_FluidTCtrl in the HVACVariableRefrigerantFlow.cc to narrow down the PLR range first before using the Regula Falsi method. He divides PLR into 10 sections with 0.1 increment. Then use a Do Loop to find a working range of PLR within 0.1 interval value that contains a solution. Finally, the Regula Falsi method is used to get a PLR solution.

In addition, he prefer do something internally without user involvement.

### My reply

I will add new choices to let user select which algorithm is used.  

	HVACSystemRootFindingAlgorithm,
  	A1 , \field Algorithm
       \type choice
       \key RegulaFalsi
       \key Bisection
       \key BisectionThenRegulaFalsi
       \key RegulaFalsiThenBisection
       \default RegulaFalsi
  	N1 ; \field Number of Iteration Before Algorithm Switch
       \note This field is used when RegulaFalsiThenBisection or BisectionThenRegulaFalsi is 
       \note entered. When iteration number is greater than the value, algorithm switches.
       \type integer
       \default 0

The proposed object will be used for advanced users only. Although I can make algorithm switch internally, my internal code will not cover everything due to limited thoughts. For example, I would like to use Regula Falsi method first in general, while Rich would like to use the bisection first in some special cases as a developer. In order to meet advanced users' requirements and developers' requirements, I also suggest to add 3 more optional arguments in General::SolveRegulaFalsi:

	void
	SolveRegulaFalsi(
		Real64 const Eps, // required absolute accuracy
		int const MaxIte, // maximum number of allowed iterations
		int & Flag, // integer storing exit status
		Real64 & XRes, // value of x that solves f(x) = 0
		std::function< Real64( Real64 const ) > f,
		Real64 const X_0, // 1st bound of interval that contains the solution
		Real64 const X_1, // 2nd bound of interval that contains the solution
Optional_int AlgorithmTypeNum, // ALgorithm selection
Optional_double & XX_0, // Low bound obtained with maximum number of allowed iterations
Optional_double & XX_1, // Hign bound obtained with maximum number of allowed iterations

	);

If the optional argument of AlgorithmTypeNum, the algorithm will not be switched.   

The XX_0 argument is an output value at low bound with the maximum number of allowed iterations. The XX_1 argument is an output value at low bound with the maximum number of allowed iterations.

The advantage by adding 3 optional arguments is to allow the program to get new narrowed bounds that contain the solution. Then the new bounds will be used again as X_0 and X-1 to get a solution using RegulaFalsi.

The above change will meet advanced users requirements in general and special requirements for developers.

Note: Optional arguments may be changed by using polymorphism during coding.
 
## Overview ##

The Regula Falsi method is a unique solver to find a root with given load in EnergyPlus. However, the method is unable to find a solution with small loads sometimes. A hybrid approach is proposed to combine Regula Falsi and bisection methods together. Regula Falsi method will be used first. If the given load is too small and the number of iterations is above a certain number, the bisection method will be used to find a solution. The proposed approach will provide a solution to reduce possibility of divergence. 

## Approach ##

The proposed approach is to use a hybrid approach by combining Regula Falsi and bisection methods together. Although bisection method is slow in general, it finds a solution eventually. From a point of view of convergence, the bisection method is better than Regula Falsi. The proposed approach is to use the Regula Falsi method first to narrow the working range. If the given load is too small and the number of iteration is above the value given in the Number of Iteration Before Bisection field in the proposed new object of HVACSystemRootFindingAlgorithm, the bisection method will be used.  The hybrid approach will take advantage of Regula Falsi to narrow the working range first and fast, and find a root eventually at given small loads using the bisection method when the Regula Falsi method may not find a solution. 

A new object is proposed:

	HVACSystemRootFindingAlgorithm,
  	A1 , \field Algorithm
       \type choice
       \key RegulaFalsi
       \key Bisection
       \key BisectionThenRegulaFalsi
       \key RegulaFalsiThenBisection
       \default RegulaFalsi
  	N1 ; \field Number of Iteration Before Algorithm Switch
       \note This field is used when RegulaFalsiThenBisection or BisectionThenRegulaFalsi is 
       \note entered. When iteration number is greater than the value, algorithm switches.
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
Four choices are allowed to select which solution algorithm will be used: RegulaFalsi, Bisection,  BisectionThenRegulaFalsi, and RegulaFalsiThenBisection. The RegulaFalsi
selection is the default selection. Bisection selction will allow the program to use the bisection method to get a solution. The BisectionThenRegulaFalsi selection requires the program to apply the bisection method first. After the number of iteration is above the value defined in the next field, the RegulaFalsi algorithm will be applied. The RegulaFalsiThenBisection selection requires the program to apply the RegulaFalsi method first. After the number of iteration is above the value defined in the next field, the bisection algorithm will be applied.  

##### Field: Number of Iteration Before Algorithm Switch
This field is used when RegulaFalsiThenBisection or BisectionThenRegulaFalsi is entered. When iteration number is greater than the value, algorithm switches either from RegulaFalsi to Bisection or from Bisection to RegulaFalsi.

A default IDF example is shown below:

	HVACSystemRootFindingAlgorithm,
		RegulaFalsiThenBisection,
		20;

## Input Description ##

A new object is proposed.

	HVACSystemRootFindingAlgorithm,
  	A1 , \field Algorithm
       \type choice
       \key RegulaFalsi
       \key Bisection
       \key BisectionThenRegulaFalsi
       \key RegulaFalsiThenBisection
       \default RegulaFalsi
  	N1 ; \field Number of Iteration Before Algorithm Switch
       \note This field is used when RegulaFalsiThenBisection or BisectionThenRegulaFalsi is 
       \note entered. When iteration number is greater than the value, algorithm switches.
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

## Design Document

Three modules are revised: DataHeatBalance, HeatBalanceManager, and General.

### DataHeatBalance

#### Create a struct to handle a new object of HVACSystemRootFindingAlgorithm 

	struct HVACSystemRootFindingAlgorithm
	{
		// Members
		std::string Algorithm;           // Choice of algorithm
		int TypeNum;                     // Type number: 1 RegulaFalsi; 2 RegulaFalsiThenBisection
		int NumOfIter;                   // Number of Iteration Before Bisection
		
		// Default Constructor
		HVACSystemRootFindingAlgorithm( ) :
			NumOfIter( 0 )
		{}

	};


#### Add 2 parameters for algorithm selection
 
	// Parameters for HVACSystemRootFindingAlgorithm
	int const RegulaFalsi( 1 );
	int const Bisection( 2 );
	int const BisectionThenRegulaFalsi( 3 );
	int const RegulaFalsiThenBisection( 4 );
 
### HeatBalanceManager

#### Add a new section to read the new object of HVACSystemRootFindingAlgorithm in the GetProjectControlData function

		// A new object is added by L. Gu, 4/17
		CurrentModuleObject = "HVACSystemRootFindingAlgorithm";
		NumObjects = GetNumObjectsFound( CurrentModuleObject );
		if ( NumObjects > 0 ) {
			GetObjectItem( CurrentModuleObject, 1, AlphaName, NumAlpha, BuildingNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( NumAlpha > 0 ) {
				HVACSystemRootFinding.Algorithm = AlphaName( 1 );
				{ auto const SELECT_CASE_var( AlphaName( 1 ) );
				if ( ( SELECT_CASE_var == "REGULAFALSI" ) ) {
					HVACSystemRootFinding.TypeNum = RegulaFalsi;
				}
				else if ( SELECT_CASE_var == "BiSECTION" ) {
					HVACSystemRootFinding.TypeNum = Bisection;
				else if ( SELECT_CASE_var == "BISECTIONTHENREGULAFALSI" ) {
					HVACSystemRootFinding.TypeNum = BisectionThenRegulaFalsi;
				else if ( SELECT_CASE_var == "REGULAFALSITHENBISECTION" ) {
					HVACSystemRootFinding.TypeNum = RegulaFalsiThenBisection;
				} else {
					HVACSystemRootFinding.TypeNum = RegulaFalsi;
					ShowWarningError( CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames( 1 ) + ". The default choice is assigned = " + AlphaName( 1 ) );
					ShowContinueError( "Valid choices are: RegulaFalsi, or RegulaFalsiThenBisection." );
				}}
			}
			if ( NumNumber > 0 ) {
				HVACSystemRootFinding.NumOfIter = BuildingNumbers( 1 );
			}
		}
		else {
			HVACSystemRootFinding.TypeNum = RegulaFalsi;
			HVACSystemRootFinding.Algorithm = "RegulaFalsi";
		}

		// Write Solution Algorithm to the initialization output file for User Verification
		gio::write( OutputFileInits, Format_726 );
		gio::write( OutputFileInits, Format_727 ) << AlphaName( 1 );

### General

#### Add a new section in the SolveRegulaFalsi function

			if ( HVACSystemRootFinding.TypeNum == RegulaFalsiThenBisection && NIte > HVACSystemRootFinding.NumOfIter ) {
				// Bisection
				XTemp = ( X1 + X0 ) / 2.0;
			} else {
				// Regula Falsi
				XTemp = ( Y0 * X1 - Y1 * X0 ) / DY;
			}

#### Add 3 new optional arguments in the SolveRegulaFalsi function

	void
	SolveRegulaFalsi(
		Real64 const Eps, // required absolute accuracy
		int const MaxIte, // maximum number of allowed iterations
		int & Flag, // integer storing exit status
		Real64 & XRes, // value of x that solves f(x) = 0
		std::function< Real64( Real64 const ) > f,
		Real64 const X_0, // 1st bound of interval that contains the solution
		Real64 const X_1, // 2nd bound of interval that contains the solution
		Optional_int AlgorithmTypeNum, // ALgorithm selection
		Optional_double & XX_0, // Low bound obtained with maximum number of allowed iterations
		Optional_double & XX_1, // Hign bound obtained with maximum number of allowed iterations

	);

