#ifndef PlantPressureSystem_hh_INCLUDED
#define PlantPressureSystem_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace PlantPressureSystem {

	// Data
	// MODULE PARAMETER/ENUMERATIONS DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	//TYPE, PUBLIC:: PlantPressureCurveData
	//  CHARACTER(len=MaxNameLength) :: Name                    = Blank
	//  REAL(r64)                    :: EquivDiameter           = 0.0d0   !- An effective diameter for calculation of Re & e/D [m]
	//  REAL(r64)                    :: MinorLossCoeff          = 0.0d0   !- K factor                                          [-]
	//  REAL(r64)                    :: EquivLength             = 0.0d0   !- An effective length to apply friction calculation [m]
	//  REAL(r64)                    :: EquivRoughness          = 0.0d0   !- An effective roughness (e) to calculate e/D       [m]
	//  LOGICAL                      :: ConstantFPresent        = .FALSE. !- Signal for if a constant value of f was entered
	//  REAL(r64)                    :: ConstantF               = 0.0d0   !- Constant value of f (if applicable)               [-]
	//END TYPE PlantPressureCurveData
	//          ! MODULE VARIABLE DECLARATIONS:
	//TYPE(PlantPressureCurveData), ALLOCATABLE, DIMENSION(:),PUBLIC :: PressureCurve
	//LOGICAL  :: GetInputFlag = .TRUE. !Module level, since GetInput could be called by SIMPRESSUREDROP or by BRANCHINPUTMANAGER

	// SUBROUTINE SPECIFICATIONS FOR MODULE:

	// Driver/Manager Routines

	// Initialization routines for module
	//PRIVATE GetPressureSystemInput

	// Algorithms/Calculation routines for the module
	//PRIVATE PressureCurveValue
	//PRIVATE CalculateMoodyFrictionFactor

	// Update routines to check convergence and update nodes

	// Utility routines for module

	// Public Utility routines
	//PUBLIC GetPressureCurveTypeAndIndex

	// Functions

	void
	SimPressureDropSystem(
		int const LoopNum, // Plant Loop to update pressure information
		bool const FirstHVACIteration, // System flag
		int const CallType, // Enumerated call type
		Optional_int_const LoopSideNum = _, // Loop side num for specific branch simulation
		Optional_int_const BranchNum = _ // Branch num for specific branch simulation
	);

	//=================================================================================================!

	//SUBROUTINE GetPressureSystemInput()
	// Getinput for PressureSystem moved to CurveManager module
	//=================================================================================================!

	void
	InitPressureDrop(
		int const LoopNum,
		bool const FirstHVACIteration
	);

	//=================================================================================================!

	void
	BranchPressureDrop(
		int const LoopNum, // Plant Loop Index
		int const LoopSideNum, // LoopSide Index (1=Demand, 2=Supply) on Plant Loop LoopNum
		int const BranchNum // Branch Index on LoopSide LoopSideNum
	);

	//=================================================================================================!

	//REAL(r64) FUNCTION PressureCurveValue(PressureCurveIndex, MassFlow, Density, Viscosity)

	//          ! FUNCTION INFORMATION:
	//          !       AUTHOR         Edwin Lee
	//          !       DATE WRITTEN   August 2009
	//          !       MODIFIED       na
	//          !       RE-ENGINEERED  na

	//          ! PURPOSE OF THIS FUNCTION:
	//          ! This will evaluate the pressure drop for components which use pressure information

	//          ! METHODOLOGY EMPLOYED:
	//          ! Friction factor pressure drop equation:
	//          ! DP = [f*(L/D) + K] * (rho * V^2) / 2

	//          ! REFERENCES:
	//          ! na

	//          ! USE STATEMENTS:
	//  USE DataPlant, ONLY : MassFlowTol
	//  Use DataGlobals, ONLY : Pi

	//  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

	//          ! FUNCTION ARGUMENT DEFINITIONS:
	//  INTEGER, INTENT(IN)      ::  PressureCurveIndex
	//  REAL(r64), INTENT(IN)    ::  MassFlow
	//  REAL(r64), INTENT(IN)    ::  Density
	//  REAL(r64), INTENT(IN)    ::  Viscosity

	//          ! FUNCTION PARAMETER DEFINITIONS:
	//          ! na

	//          ! INTERFACE BLOCK SPECIFICATIONS:
	//          ! na

	//          ! DERIVED TYPE DEFINITIONS:
	//          ! na

	//          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
	//  REAL(r64)                ::  Diameter
	//  REAL(r64)                ::  MinorLossCoeff
	//  REAL(r64)                ::  Length
	//  REAL(r64)                ::  Roughness
	//  LOGICAL                  ::  IsConstFPresent
	//  REAL(r64)                ::  ConstantF
	//  REAL(r64)                ::  FrictionFactor
	//  REAL(r64)                ::  CrossSectArea
	//  REAL(r64)                ::  Velocity
	//  REAL(r64)                ::  ReynoldsNumber
	//  REAL(r64)                ::  RoughnessRatio

	//  !Retrieve data from structure
	//  Diameter        = PressureCurve(PressureCurveIndex)%EquivDiameter
	//  MinorLossCoeff  = PressureCurve(PressureCurveIndex)%MinorLossCoeff
	//  Length          = PressureCurve(PressureCurveIndex)%EquivLength
	//  Roughness       = PressureCurve(PressureCurveIndex)%EquivRoughness
	//  IsConstFPresent = PressureCurve(PressureCurveIndex)%ConstantFPresent
	//  ConstantF       = PressureCurve(PressureCurveIndex)%ConstantF

	//  !Intermediate calculations
	//  CrossSectArea         =  (Pi / 4.0d0) * Diameter**2
	//  Velocity              =  MassFlow / (Density * CrossSectArea)
	//  ReynoldsNumber        =  Density * Diameter * Velocity / Viscosity !assuming mu here
	//  RoughnessRatio        =  Roughness / Diameter

	//  !If we don't have any flow then exit out
	//  IF (MassFlow .LT. MassFlowTol) THEN
	//    PressureCurveValue = 0.0d0
	//    RETURN
	//  END IF

	//  !Calculate the friction factor
	//  IF (IsConstFPresent) THEN   !use the constant value
	//    FrictionFactor    =  ConstantF
	//  ELSE ! must calculate f
	//    FrictionFactor    =  CalculateMoodyFrictionFactor(ReynoldsNumber,RoughnessRatio)
	//  END IF

	//  !Pressure drop calculation
	//  PressureCurveValue  =  (FrictionFactor * (Length / Diameter) + MinorLossCoeff) * (Density * Velocity**2) / 2.0d0

	//END FUNCTION PressureCurveValue

	//=================================================================================================!

	//REAL(r64) FUNCTION CalculateMoodyFrictionFactor(ReynoldsNumber, RoughnessRatio)

	//          ! FUNCTION INFORMATION:
	//          !       AUTHOR         Edwin Lee
	//          !       DATE WRITTEN   August 2009
	//          !       MODIFIED       na
	//          !       RE-ENGINEERED  na

	//          ! PURPOSE OF THIS FUNCTION:
	//          ! This will evaluate the moody friction factor based on Reynolds number and roughness ratio

	//          ! METHODOLOGY EMPLOYED:
	//          ! General empirical correlations for friction factor based on Moody Chart data

	//          ! REFERENCES:
	//          ! Haaland, SE (1983). "Simple and Explicit Formulas for the Friction Factor in Turbulent Flow".
	//          !   Trans. ASIVIE, J. of Fluids Engineering 103: 89-90.

	//          ! USE STATEMENTS:
	//  USE General, ONLY: RoundSigDigits

	//  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

	//          ! FUNCTION ARGUMENT DEFINITIONS:
	//  REAL(r64), INTENT(IN)    ::  ReynoldsNumber
	//  REAL(r64), INTENT(IN)    ::  RoughnessRatio

	//          ! FUNCTION PARAMETER DEFINITIONS:
	//          ! na

	//          ! INTERFACE BLOCK SPECIFICATIONS:
	//          ! na

	//          ! DERIVED TYPE DEFINITIONS:
	//          ! na

	//          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
	//  REAL(r64)                    ::  Term1, Term2, Term3
	//  CHARACTER(len=MaxNameLength) ::  RR, Re
	//  LOGICAL, SAVE                ::  FrictionFactorErrorHasOccurred = .FALSE.

	//  !Check for no flow before calculating values
	//  IF (ReynoldsNumber .EQ. 0.0d0) THEN
	//    CalculateMoodyFrictionFactor = 0.0d0
	//    RETURN
	//  END IF

	//  !Check for no roughness also here
	//  IF (RoughnessRatio .EQ. 0.0d0) THEN
	//    CalculateMoodyFrictionFactor = 0.0d0
	//    RETURN
	//  END IF

	//  !Calculate the friction factor
	//  Term1 = (RoughnessRatio/3.7d0)**(1.11d0)
	//  Term2 = 6.9d0/ReynoldsNumber
	//  Term3 = -1.8d0 * LOG10(Term1 + Term2)
	//  IF (Term3 .NE. 0.0d0) THEN
	//    CalculateMoodyFrictionFactor = Term3 ** (-2.0d0)
	//  ELSE
	//    IF (.NOT. FrictionFactorErrorHasOccurred) THEN
	//      RR=RoundSigDigits(RoughnessRatio,7)
	//      Re=RoundSigDigits(ReynoldsNumber,1)
	//      CALL ShowSevereError('Plant Pressure System: Error in moody friction factor calculation')
	//      CALL ShowContinueError('Current Conditions: Roughness Ratio='//TRIM(RR)//'; Reynolds Number='//TRIM(Re))
	//      CALL ShowContinueError('These conditions resulted in an unhandled numeric issue.')
	//      CALL ShowContinueError('Please contact EnergyPlus support/development team to raise an alert about this issue')
	//      CALL ShowContinueError('This issue will occur only one time.  The friction factor has been reset to 0.04 for calculations')
	//      FrictionFactorErrorHasOccurred = .TRUE.
	//    END IF
	//    CalculateMoodyFrictionFactor = 0.04d0
	//  END IF

	//  RETURN

	//END FUNCTION CalculateMoodyFrictionFactor

	//=================================================================================================!

	void
	UpdatePressureDrop( int const LoopNum );

	//=================================================================================================!

	void
	DistributePressureOnBranch(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		Real64 & BranchPressureDrop,
		bool & PumpFound
	);

	//=================================================================================================!

	void
	PassPressureAcrossMixer(
		int const LoopNum,
		int const LoopSideNum,
		Real64 & MixerPressure,
		int const NumBranchesOnLoopSide
	);

	//=================================================================================================!

	void
	PassPressureAcrossSplitter(
		int const LoopNum,
		int const LoopSideNum,
		Real64 & SplitterInletPressure
	);

	//=================================================================================================!

	void
	PassPressureAcrossInterface( int const LoopNum );

	//=================================================================================================!

	//SUBROUTINE GetPressureCurveTypeAndIndex(PressureCurveName, PressureCurveType, PressureCurveIndex)

	//          ! SUBROUTINE INFORMATION:
	//          !       AUTHOR         Edwin Lee
	//          !       DATE WRITTEN   August 2009
	//          !       MODIFIED       na
	//          !       RE-ENGINEERED  na

	//          ! PURPOSE OF THIS SUBROUTINE:
	//          ! Given a curve name, returns the curve type and index

	//          ! METHODOLOGY EMPLOYED:
	//          ! Curve types are:
	//          !  PressureCurve_Error       = pressure name was given, but curve is not available
	//          !  PressureCurve_None        = no pressure curve for this branch
	//          !  PressureCurve_Pressure    = pressure curve based on friction/minor loss
	//          !  PressureCurve_Generic     = curvemanager held curve which is function of flow rate

	//          ! REFERENCES:
	//          ! na

	//          ! USE STATEMENTS:
	//  USE InputProcessor, ONLY : FindItemInList
	//  USE CurveManager,   ONLY : GetCurveIndex, GetCurveType
	//  USE GlobalDataConstants,  ONLY : PressureCurve_None, PressureCurve_Pressure, PressureCurve_Generic, PressureCurve_Error

	//  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

	//          ! SUBROUTINE ARGUMENT DEFINITIONS:
	//  CHARACTER(len=*), INTENT (IN)  :: PressureCurveName            ! name of the curve
	//  INTEGER, INTENT(INOUT)         :: PressureCurveType
	//  INTEGER, INTENT(INOUT)         :: PressureCurveIndex

	//          ! SUBROUTINE PARAMETER DEFINITIONS:
	//          ! na

	//          ! INTERFACE BLOCK SPECIFICATIONS
	//          ! na

	//          ! DERIVED TYPE DEFINITIONS
	//          ! na

	//          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	//  INTEGER           :: TempCurveIndex
	//  LOGICAL           :: FoundCurve
	//  CHARACTER(len=32) :: GenericCurveType

	//  !If input is not gotten, go ahead and get it now
	//  IF (GetInputFlag) CALL GetPressureSystemInput

	//  !Initialize
	//  FoundCurve = .FALSE.
	//  PressureCurveType = PressureCurve_None
	//  PressureCurveIndex = 0

	//  !Try to retrieve a curve manager object
	//  TempCurveIndex = GetCurveIndex(PressureCurveName)

	//  !See if it is valid
	//  IF (TempCurveIndex > 0) THEN
	//    !We have to check the type of curve to make sure it is single independent variable type
	//    GenericCurveType = GetCurveType(TempCurveIndex)
	//    SELECT CASE (GenericCurveType)
	//      CASE ('LINEAR', 'QUADRATIC', 'CUBIC', 'QUARTIC', 'EXPONENT')
	//        PressureCurveType = PressureCurve_Generic
	//        PressureCurveIndex = TempCurveIndex
	//      CASE DEFAULT
	//        CALL ShowSevereError('Plant Pressure Simulation: Found error for curve: '//PressureCurveName)
	//        CALL ShowContinueError('Curve type detected: '//GenericCurveType)
	//        CALL ShowContinueError('Generic curves should be single independent variable such that DeltaP = f(mdot)')
	//        CALL ShowContinueError(' Therefore they should be of type: Linear, Quadratic, Cubic, Quartic, or Exponent')
	//        CALL ShowFatalError('Errors in pressure simulation input cause program termination')
	//    END SELECT
	//    RETURN
	//  END IF

	//  !Then try to retrieve a pressure curve object
	//  IF (ALLOCATED(PressureCurve)) THEN
	//    IF (SIZE(PressureCurve) > 0) THEN
	//      TempCurveIndex = FindItemInList(PressureCurveName,PressureCurve(1:SIZE(PressureCurve))%Name,SIZE(PressureCurve))
	//    ELSE
	//      TempCurveIndex = 0
	//    END IF
	//  END IF

	//  !See if it is valid
	//  IF (TempCurveIndex > 0) THEN
	//    PressureCurveType = PressureCurve_Pressure
	//    PressureCurveIndex = TempCurveIndex
	//    RETURN
	//  END IF

	//  !If we made it here, we didn't find either type of match

	//  !Last check, see if it is blank:
	//  IF (TRIM(PressureCurveName)=='') THEN
	//    PressureCurveType = PressureCurve_None
	//    RETURN
	//  END IF

	//  !At this point, we had a non-blank user entry with no match
	//  PressureCurveType = PressureCurve_Error
	//  RETURN

	//RETURN

	//END SUBROUTINE

	//=================================================================================================!

	Real64
	ResolveLoopFlowVsPressure(
		int const LoopNum, // - Index of which plant/condenser loop is being simulated
		Real64 const SystemMassFlow, // - Initial "guess" at system mass flow rate [kg/s]
		int const PumpCurveNum, // - Pump curve to use when calling the curve manager for psi = f(phi)
		Real64 const PumpSpeed, // - Pump rotational speed, [rps] (revs per second)
		Real64 const PumpImpellerDia, // - Nominal pump impeller diameter [m]
		Real64 const MinPhi, // - Minimum allowable value of phi, requested by the pump manager from curve mgr
		Real64 const MaxPhi // - Maximum allowable value of phi, requested by the pump manager from curve mgr
	);

	//=================================================================================================!

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // PlantPressureSystem

} // EnergyPlus

#endif
