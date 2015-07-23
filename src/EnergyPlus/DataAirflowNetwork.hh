#ifndef DataAirflowNetwork_hh_INCLUDED
#define DataAirflowNetwork_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataAirflowNetwork {

	// Using/Aliasing

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const CompTypeNum_DOP; // Detailed large opening component
	extern int const CompTypeNum_SOP; // Simple opening component
	extern int const CompTypeNum_SCR; // Surface crack component
	extern int const CompTypeNum_SEL; // Surface effective leakage ratio component
	extern int const CompTypeNum_PLR; // Distribution system crack component
	extern int const CompTypeNum_DWC; // Distribution system duct component
	extern int const CompTypeNum_CVF; // Distribution system constant volume fan component
	extern int const CompTypeNum_FAN; // Distribution system detailed fan component
	extern int const CompTypeNum_MRR; // Distribution system multiple curve fit power law resistant flow component
	extern int const CompTypeNum_DMP; // Distribution system damper component
	extern int const CompTypeNum_ELR; // Distribution system effective leakage ratio component
	extern int const CompTypeNum_CPD; // Distribution system constant pressure drop component
	extern int const CompTypeNum_COI; // Distribution system coil component
	extern int const CompTypeNum_TMU; // Distribution system terminal unit component
	extern int const CompTypeNum_EXF; // Zone exhaust fan
	extern int const CompTypeNum_HEX; // Distribution system heat exchanger
	extern int const CompTypeNum_HOP; // Horizontal opening component
	extern int const CompTypeNum_RVD; // Reheat VAV terminal damper

	// EPlus component Type
	extern int const EPlusTypeNum_SCN; // Supply connection
	extern int const EPlusTypeNum_RCN; // Return connection
	extern int const EPlusTypeNum_RHT; // Reheat terminal
	extern int const EPlusTypeNum_FAN; // Fan
	extern int const EPlusTypeNum_COI; // Heating or cooling coil
	extern int const EPlusTypeNum_HEX; // Heat ecxchanger
	extern int const EPlusTypeNum_RVD; // Reheat VAV terminal damper

	// EPlus node type
	extern int const EPlusTypeNum_ZIN; // Zone inlet node
	extern int const EPlusTypeNum_ZOU; // Zone outlet node
	extern int const EPlusTypeNum_SPL; // Splitter node
	extern int const EPlusTypeNum_MIX; // Mixer node
	extern int const EPlusTypeNum_OAN; // Outside air system node
	extern int const EPlusTypeNum_EXT; // OA system inlet node
	extern int const EPlusTypeNum_FIN; // Fan Inlet node
	extern int const EPlusTypeNum_FOU; // Fan Outlet Node
	extern int const EPlusTypeNum_COU; // Coil Outlet Node
	extern int const EPlusTypeNum_HXO; // Heat exchanger Outlet Node
	extern int const EPlusTypeNum_DIN; // Damper Inlet node
	extern int const EPlusTypeNum_DOU; // Damper Outlet Node
	extern int const EPlusTypeNum_SPI; // Splitter inlet Node
	extern int const EPlusTypeNum_SPO; // Splitter Outlet Node

	extern int const iWPCCntr_Input;
	extern int const iWPCCntr_SurfAvg;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	// Node simulation variable in air distribution system
	// Link simulation variable in air distribution system
	// Sensible and latent exchange variable in air distribution system

	extern int SimulateAirflowNetwork;
	// Vent Control  DistSys Control  Flag    Description
	//  NONE           NONE           0      No AirflowNetwork and SIMPLE
	//  SIMPLE         NONE           1      Simple calculations only
	//  MULTIZONE      NONE           2      Perform multizone calculations only
	//  NONE           DISTSYS        3      Perform distribution system durin system on time only
	//  SIMPLE         DISTSYS        4      Perform distribution system durin system on time and simple calculations during off time
	//  MULTIZONE      DISTSYS        5      Perform distribution system durin system on time and multizone calculations during off time

	extern int const AirflowNetworkControlSimple; // Simple calculations only
	extern int const AirflowNetworkControlMultizone; // Perform multizone calculations only
	extern int const AirflowNetworkControlSimpleADS; // Perform distribution system durin system
	// on time and simple calculations during off time
	extern int const AirflowNetworkControlMultiADS; // Perform distribution system durin system on time
	// and multizone calculations during off time

	extern Array1D_bool AirflowNetworkZoneFlag;

	extern int NumOfNodesMultiZone; // Number of nodes for multizone calculation
	extern int NumOfNodesDistribution; // Number of nodes for distribution system calculation
	extern int NumOfLinksMultiZone; // Number of links for multizone calculation
	extern int NumOfLinksDistribution; // Number of links for distribution system calculation

	extern int AirflowNetworkNumOfNodes; // Number of nodes for AirflowNetwork calculation
	// = NumOfNodesMultiZone+NumOfNodesDistribution
	extern int AirflowNetworkNumOfComps; // Number of components for AirflowNetwork calculation
	extern int AirflowNetworkNumOfLinks; // Number of links for AirflowNetwork calculation
	// = NumOfLinksMultiZone+NumOfLinksDistribution
	// RoomAirManager use
	extern int AirflowNetworkNumOfSurfaces; // The number of surfaces for multizone calculation
	extern int AirflowNetworkNumOfZones; // The number of zones for multizone calculation

	extern bool RollBackFlag; // Roll back flag when system time steo down shifting
	extern Array1D< Real64 > ANZT; // Local zone air temperature for roll back use
	extern Array1D< Real64 > ANZW; // Local zone air humidity ratio for roll back use
	extern Array1D< Real64 > ANCO; // Local zone air CO2 for roll back use
	extern Array1D< Real64 > ANGC; // Local zone air generic contaminant for roll back use
	extern int AirflowNetworkNumOfExhFan; // Number of zone exhaust fans
	extern Array1D_bool AirflowNetworkZoneExhaustFan; // Logical to use zone exhaust fans
	extern bool AirflowNetworkFanActivated; // Supply fan activation flag
	extern bool AirflowNetworkUnitarySystem; // set to TRUE for unitary systems (to make answers equal, will remove eventually)
	// Multispeed HP only
	extern int MultiSpeedHPIndicator; // Indicator for multispeed heat pump use
	// Addiitonal airflow needed for an VAV fan to compensate the leakage losses and supply pathway pressure losses [kg/s]
	extern Real64 VAVTerminalRatio; // The terminal flow ratio when a supply VAV fan reach its max flow rate
	extern bool VAVSystem; // This flag is used to represent a VAV system

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

	// Types

	struct AirflowNetworkSimuProp // Basic parameters for AirflowNetwork simulation
	{
		// Members
		std::string AirflowNetworkSimuName; // Provide a unique object name
		std::string Control; // AirflowNetwork control: MULTIZONE WITH DISTRIBUTION,
		// MULTIZONE WITHOUT DISTRIBUTION
		// MULTIZONE WITH DISTRIBUTION ONLY DURING FAN OPERATION,
		// and NO MULTIZONE OR DISTRIBUTION
		std::string WPCCntr; // Wind pressure coefficient input control: "SURFACE-AVERAGE CALCULATION", or "INPUT"
		int iWPCCntr; // Integer equivalent for WPCCntr field
		std::string CpArrayName; // CP Array name at WPCCntr = "INPUT"
		std::string BldgType; // Building type: "LOWRISE" or "HIGHRISE" at WPCCntr = "SURFACE-AVERAGE CALCULATIO"
		std::string HeightOption; // Height Selection: "ExternalNode" or "OpeningHeight" at WPCCntr = "INPUT"
		int MaxIteration; // Maximum number of iteration, defualt 500
		int InitFlag; // Initialization flag
		Real64 RelTol; // Relative airflow convergence
		Real64 AbsTol; // Absolute airflow convergence
		Real64 ConvLimit; // Convergence acceleration limit
		Real64 MaxPressure; // Maximum pressure change in an element [Pa]
		Real64 Azimuth; // Azimuth Angle of Long Axis of Building, not used at WPCCntr = "INPUT"
		Real64 AspectRatio; // Ratio of Building Width Along Short Axis to Width Along Long Axis
		int NWind; // Number of wind directions
		Real64 DiffP; // Minimum pressure difference
		int ExtLargeOpeningErrCount; // Exterior large opening error count during HVAC system operation
		int ExtLargeOpeningErrIndex; // Exterior large opening error index during HVAC system operation
		int OpenFactorErrCount; // Large opening error count at Open factor > 1.0
		int OpenFactorErrIndex; // Large opening error error index at Open factor > 1.0
		std::string InitType; // Initialization flag type:
		bool TExtHeightDep; // Choice of height dependence of external node temperature
		// "ZeroNodePressures", or "LinearInitializationMethod"

		// Default Constructor
		AirflowNetworkSimuProp() :
			Control( "NoMultizoneOrDistribution" ),
			WPCCntr( "Input" ),
			MaxIteration( 500 ),
			InitFlag( 0 ),
			RelTol( 1.0e-5 ),
			AbsTol( 1.0e-5 ),
			ConvLimit( -0.5 ),
			MaxPressure( 500.0 ),
			Azimuth( 0.0 ),
			AspectRatio( 1.0 ),
			NWind( 0 ),
			DiffP( 1.0e-4 ),
			ExtLargeOpeningErrCount( 0 ),
			ExtLargeOpeningErrIndex( 0 ),
			OpenFactorErrCount( 0 ),
			OpenFactorErrIndex( 0 ),
			InitType( "ZeroNodePressures" ),
			TExtHeightDep( false )
		{}

		// Member Constructor
		AirflowNetworkSimuProp(
			std::string const & AirflowNetworkSimuName, // Provide a unique object name
			std::string const & Control, // AirflowNetwork control: MULTIZONE WITH DISTRIBUTION,
			std::string const & WPCCntr, // Wind pressure coefficient input control: "SURFACE-AVERAGE CALCULATION", or "INPUT"
			int const iWPCCntr, // Integer equivalent for WPCCntr field
			std::string const & CpArrayName, // CP Array name at WPCCntr = "INPUT"
			std::string const & BldgType, // Building type: "LOWRISE" or "HIGHRISE" at WPCCntr = "SURFACE-AVERAGE CALCULATIO"
			std::string const & HeightOption, // Height Selection: "ExternalNode" or "OpeningHeight" at WPCCntr = "INPUT"
			int const MaxIteration, // Maximum number of iteration, defualt 500
			int const InitFlag, // Initialization flag
			Real64 const RelTol, // Relative airflow convergence
			Real64 const AbsTol, // Absolute airflow convergence
			Real64 const ConvLimit, // Convergence acceleration limit
			Real64 const MaxPressure, // Maximum pressure change in an element [Pa]
			Real64 const Azimuth, // Azimuth Angle of Long Axis of Building, not used at WPCCntr = "INPUT"
			Real64 const AspectRatio, // Ratio of Building Width Along Short Axis to Width Along Long Axis
			int const NWind, // Number of wind directions
			Real64 const DiffP, // Minimum pressure difference
			int const ExtLargeOpeningErrCount, // Exterior large opening error count during HVAC system operation
			int const ExtLargeOpeningErrIndex, // Exterior large opening error index during HVAC system operation
			int const OpenFactorErrCount, // Large opening error count at Open factor > 1.0
			int const OpenFactorErrIndex, // Large opening error error index at Open factor > 1.0
			std::string const & InitType, // Initialization flag type:
			bool const TExtHeightDep // Choice of height dependence of external node temperature
		) :
			AirflowNetworkSimuName( AirflowNetworkSimuName ),
			Control( Control ),
			WPCCntr( WPCCntr ),
			iWPCCntr( iWPCCntr ),
			CpArrayName( CpArrayName ),
			BldgType( BldgType ),
			HeightOption( HeightOption ),
			MaxIteration( MaxIteration ),
			InitFlag( InitFlag ),
			RelTol( RelTol ),
			AbsTol( AbsTol ),
			ConvLimit( ConvLimit ),
			MaxPressure( MaxPressure ),
			Azimuth( Azimuth ),
			AspectRatio( AspectRatio ),
			NWind( NWind ),
			DiffP( DiffP ),
			ExtLargeOpeningErrCount( ExtLargeOpeningErrCount ),
			ExtLargeOpeningErrIndex( ExtLargeOpeningErrIndex ),
			OpenFactorErrCount( OpenFactorErrCount ),
			OpenFactorErrIndex( OpenFactorErrIndex ),
			InitType( InitType ),
			TExtHeightDep( TExtHeightDep )
		{}

	};

	struct MultizoneZoneProp // Zone information
	{
		// Members
		std::string ZoneName; // Name of Associated EnergyPlus Thermal Zone
		std::string VentControl; // Ventilation Control Mode: "TEMPERATURE", "ENTHALPIC", "CONSTANT", or "NOVENT"
		std::string VentSchName; // Name of ventilation temperature control schedule
		Real64 Height; // Nodal height
		Real64 OpenFactor; // Limit Value on Multiplier for Modulating Venting Open Factor,
		// Not applicable if Vent Control Mode = CONSTANT or NOVENT
		Real64 LowValueTemp; // Lower Value on Inside/Outside Temperature Difference for
		// Modulating the Venting Open Factor with temp control
		Real64 UpValueTemp; // Upper Value on Inside/Outside Temperature Difference for
		// Modulating the Venting Open Factor with temp control
		Real64 LowValueEnth; // Lower Value on Inside/Outside Temperature Difference for
		// Modulating the Venting Open Factor with Enthalpic control
		Real64 UpValueEnth; // Upper Value on Inside/Outside Temperature Difference for
		// Modulating the Venting Open Factor with Enthalpic control
		int ZoneNum; // Zone number associated with ZoneName
		int VentSchNum; // Zone ventilation schedule number associated with ventilation schedule name
		int VentCtrNum; // Ventilation control mode number: 1 "Temperature", 2 "ENTHALPIC", 3 "CONSTANT", 4 "NOVENT"
		std::string VentingSchName; // Name of ventilation temperature control schedule
		int VentingSchNum; // Ventilation schedule number
		std::string SingleSidedCpType; // Type of calculation method for single sided wind pressure coefficients
		Real64 BuildWidth; // The width of the building along the facade that contains this zone.
		int ASH55PeopleInd; // Index of people object with ASH55 comfort calcs for ventilation control
		int CEN15251PeopleInd; // Index of people object with CEN15251 comfort calcs for ventilation control
		std::string OccupantVentilationControlName; // Occupant ventilation control name
		int OccupantVentilationControlNum; // Occupant ventilation control number

		// Default Constructor
		MultizoneZoneProp() :
			VentControl( "NoVent" ),
			Height( 0.0 ),
			OpenFactor( 1.0 ),
			LowValueTemp( 0.0 ),
			UpValueTemp( 100.0 ),
			LowValueEnth( 0.0 ),
			UpValueEnth( 300000.0 ),
			ZoneNum( 0 ),
			VentSchNum( 0 ),
			VentCtrNum( 0 ),
			VentingSchNum( 0 ),
			SingleSidedCpType( "STANDARD" ),
			BuildWidth( 10.0 ),
			ASH55PeopleInd( 0 ),
			CEN15251PeopleInd( 0 ),
			OccupantVentilationControlNum( 0 )
		{}

		// Member Constructor
		MultizoneZoneProp(
			std::string const & ZoneName, // Name of Associated EnergyPlus Thermal Zone
			std::string const & VentControl, // Ventilation Control Mode: "TEMPERATURE", "ENTHALPIC", "CONSTANT", or "NOVENT"
			std::string const & VentSchName, // Name of ventilation temperature control schedule
			Real64 const Height, // Nodal height
			Real64 const OpenFactor, // Limit Value on Multiplier for Modulating Venting Open Factor,
			Real64 const LowValueTemp, // Lower Value on Inside/Outside Temperature Difference for
			Real64 const UpValueTemp, // Upper Value on Inside/Outside Temperature Difference for
			Real64 const LowValueEnth, // Lower Value on Inside/Outside Temperature Difference for
			Real64 const UpValueEnth, // Upper Value on Inside/Outside Temperature Difference for
			int const ZoneNum, // Zone number associated with ZoneName
			int const VentSchNum, // Zone ventilation schedule number associated with ventilation schedule name
			int const VentCtrNum, // Ventilation control mode number: 1 "Temperature", 2 "ENTHALPIC", 3 "CONSTANT", 4 "NOVENT"
			std::string const & VentingSchName, // Name of ventilation temperature control schedule
			int const VentingSchNum, // Ventilation schedule number
			std::string const & SingleSidedCpType, // Type of calculation method for single sided wind pressure coefficients
			Real64 const BuildWidth, // The width of the building along the facade that contains this zone.
			int const ASH55PeopleInd, // Index of people object with ASH55 comfort calcs for ventilation control
			int const CEN15251PeopleInd, // Index of people object with CEN15251 comfort calcs for ventilation control
			std::string const & OccupantVentilationControlName, // Occupant ventilation control name
			int const OccupantVentilationControlNum // Occupant ventilation control number
		) :
			ZoneName( ZoneName ),
			VentControl( VentControl ),
			VentSchName( VentSchName ),
			Height( Height ),
			OpenFactor( OpenFactor ),
			LowValueTemp( LowValueTemp ),
			UpValueTemp( UpValueTemp ),
			LowValueEnth( LowValueEnth ),
			UpValueEnth( UpValueEnth ),
			ZoneNum( ZoneNum ),
			VentSchNum( VentSchNum ),
			VentCtrNum( VentCtrNum ),
			VentingSchName( VentingSchName ),
			VentingSchNum( VentingSchNum ),
			SingleSidedCpType( SingleSidedCpType ),
			BuildWidth( BuildWidth ),
			ASH55PeopleInd( ASH55PeopleInd ),
			CEN15251PeopleInd( CEN15251PeopleInd ),
			OccupantVentilationControlName( OccupantVentilationControlName ),
			OccupantVentilationControlNum( OccupantVentilationControlNum )
			{}

	};

	struct MultizoneSurfaceProp // Surface information
	{
		// Members
		std::string SurfName; // Name of Associated EnergyPlus surface
		std::string OpeningName; // Name of opening component, either simple or detailed large opening
		std::string ExternalNodeName; // Name of external node, but not used at WPC="INPUT"
		Real64 Factor; // Crack Actual Value or Window Open Factor for Ventilation
		int SurfNum; // Surface number
		Array1D_int NodeNums; // Positive: Zone numbers; 0: External
		Real64 OpenFactor; // Surface factor
		Real64 OpenFactorLast; // Surface factor at previous time step
		bool EMSOpenFactorActuated; // True if EMS actuation is on
		Real64 EMSOpenFactor; // Surface factor value from EMS for override
		Real64 Height; // Surface Height
		Real64 Width; // Surface width
		Real64 CHeight; // Surface central height in z direction
		std::string VentControl; // Ventilation Control Mode: TEMPERATURE, ENTHALPIC, CONSTANT, ZONELEVEL or NOVENT
		std::string VentSchName; // ! Name of ventilation temperature control schedule
		Real64 ModulateFactor; // Limit Value on Multiplier for Modulating Venting Open Factor
		Real64 LowValueTemp; // Lower Value on Inside/Outside Temperature Difference for
		// Modulating the Venting Open Factor with temp control
		Real64 UpValueTemp; // Upper Value on Inside/Outside Temperature Difference for
		// Modulating the Venting Open Factor with temp control
		Real64 LowValueEnth; // Lower Value on Inside/Outside Temperature Difference for
		// Modulating the Venting Open Factor with Enthalpic control
		Real64 UpValueEnth; // Upper Value on Inside/Outside Temperature Difference for
		// Modulating the Venting Open Factor with Enthalpic control
		std::string VentingSchName; // Name of ventilation temperature control schedule
		int VentSchNum; // Zone ventilation schedule number associated with ventilation schedule name
		int VentSurfCtrNum; // Ventilation control mode number: 1 "Temperature", 2 "ENTHALPIC", 3 "CONSTANT", 4 "NOVENT"
		int VentingSchNum; // Ventilation schedule number
		int ZonePtr; // Pointer to inside face zone
		bool IndVentControl; // Individual surface venting control
		int ExtLargeOpeningErrCount; // Exterior large opening error count during HVAC system operation
		int ExtLargeOpeningErrIndex; // Exterior large opening error index during HVAC system operation
		int OpenFactorErrCount; // Large opening error count at Open factor > 1.0
		int OpenFactorErrIndex; // Large opening error error index at Open factor > 1.0
		Real64 Multiplier; // Window multiplier
		bool HybridVentClose; // Hybrid ventilation window close control logical
		bool HybridCtrlGlobal; // Hybrid ventilation global control logical
		bool HybridCtrlMaster; // Hybrid ventilation global control master
		Real64 WindModifier; // Wind modifier from hybrid ventilation control
		std::string OccupantVentilationControlName; // Occupant ventilation control name
		int OccupantVentilationControlNum; // Occupant ventilation control number
		int OpeningStatus; // Open status at current time step
		int PrevOpeningstatus; // Open status at previous time step
		Real64 CloseElapsedTime; // Elapsed time during closing (min)
		Real64 OpenElapsedTime; // Elapsed time during closing (min)
		int ClosingProbStatus; // Closing probability status
		int OpeningProbStatus; // Opening probability status

		// Default Constructor
		MultizoneSurfaceProp() :
			Factor( 0.0 ),
			SurfNum( 0 ),
			NodeNums( 2, 0 ),
			OpenFactor( 0.0 ),
			OpenFactorLast( 0.0 ),
			EMSOpenFactorActuated( false ),
			EMSOpenFactor( 0.0 ),
			Height( 0.0 ),
			Width( 0.0 ),
			CHeight( 0.0 ),
			VentControl( "ZONELEVEL" ),
			ModulateFactor( 0.0 ),
			LowValueTemp( 0.0 ),
			UpValueTemp( 100.0 ),
			LowValueEnth( 0.0 ),
			UpValueEnth( 300000.0 ),
			VentSchNum( 0 ),
			VentSurfCtrNum( 0 ),
			VentingSchNum( 0 ),
			ZonePtr( 0 ),
			IndVentControl( false ),
			ExtLargeOpeningErrCount( 0 ),
			ExtLargeOpeningErrIndex( 0 ),
			OpenFactorErrCount( 0 ),
			OpenFactorErrIndex( 0 ),
			Multiplier( 1.0 ),
			HybridVentClose( false ),
			HybridCtrlGlobal( false ),
			HybridCtrlMaster( false ),
			WindModifier( 1.0 ),
			OccupantVentilationControlNum( 0 ),
			OpeningStatus( 0 ),
			PrevOpeningstatus( 0 ),
			CloseElapsedTime( 0.0 ),
			OpenElapsedTime( 0.0 ),
			ClosingProbStatus( 0 ),
			OpeningProbStatus( 0 )
		{}

		// Member Constructor
		MultizoneSurfaceProp(
			std::string const & SurfName, // Name of Associated EnergyPlus surface
			std::string const & OpeningName, // Name of opening component, either simple or detailed large opening
			std::string const & ExternalNodeName, // Name of external node, but not used at WPC="INPUT"
			Real64 const Factor, // Crack Actual Value or Window Open Factor for Ventilation
			int const SurfNum, // Surface number
			Array1_int const & NodeNums, // Positive: Zone numbers; 0: External
			Real64 const OpenFactor, // Surface factor
			Real64 const OpenFactorLast, // Surface factor at previous time step
			bool const EMSOpenFactorActuated, // True if EMS actuation is on
			Real64 const EMSOpenFactor, // Surface factor value from EMS for override
			Real64 const Height, // Surface Height
			Real64 const Width, // Surface width
			Real64 const CHeight, // Surface central height in z direction
			std::string const & VentControl, // Ventilation Control Mode: TEMPERATURE, ENTHALPIC, CONSTANT, ZONELEVEL or NOVENT
			std::string const & VentSchName, // ! Name of ventilation temperature control schedule
			Real64 const ModulateFactor, // Limit Value on Multiplier for Modulating Venting Open Factor
			Real64 const LowValueTemp, // Lower Value on Inside/Outside Temperature Difference for
			Real64 const UpValueTemp, // Upper Value on Inside/Outside Temperature Difference for
			Real64 const LowValueEnth, // Lower Value on Inside/Outside Temperature Difference for
			Real64 const UpValueEnth, // Upper Value on Inside/Outside Temperature Difference for
			std::string const & VentingSchName, // Name of ventilation temperature control schedule
			int const VentSchNum, // Zone ventilation schedule number associated with ventilation schedule name
			int const VentSurfCtrNum, // Ventilation control mode number: 1 "Temperature", 2 "ENTHALPIC", 3 "CONSTANT", 4 "NOVENT"
			int const VentingSchNum, // Ventilation schedule number
			int const ZonePtr, // Pointer to inside face zone
			bool const IndVentControl, // Individual surface venting control
			int const ExtLargeOpeningErrCount, // Exterior large opening error count during HVAC system operation
			int const ExtLargeOpeningErrIndex, // Exterior large opening error index during HVAC system operation
			int const OpenFactorErrCount, // Large opening error count at Open factor > 1.0
			int const OpenFactorErrIndex, // Large opening error error index at Open factor > 1.0
			Real64 const Multiplier, // Window multiplier
			bool const HybridVentClose, // Hybrid ventilation window close control logical
			bool const HybridCtrlGlobal, // Hybrid ventilation global control logical
			bool const HybridCtrlMaster, // Hybrid ventilation global control master
			Real64 const WindModifier, // Wind modifier from hybrid ventilation control
			std::string const & OccupantVentilationControlName, // Occupant ventilation control name
			int const OccupantVentilationControlNum, // Occupant ventilation control number
			int const OpeningStatus, // Open status at current time step
			int const PrevOpeningstatus, // Open status at previous time step
			Real64 const CloseElapsedTime, // Elapsed time during closing (min)
			Real64 const OpenElapsedTime, // Elapsed time during closing (min)
			int const ClosingProbStatus, // Closing probability status
			int const OpeningProbStatus // Opening probability status
		) :
			SurfName( SurfName ),
			OpeningName( OpeningName ),
			ExternalNodeName( ExternalNodeName ),
			Factor( Factor ),
			SurfNum( SurfNum ),
			NodeNums( 2, NodeNums ),
			OpenFactor( OpenFactor ),
			OpenFactorLast( OpenFactorLast ),
			EMSOpenFactorActuated( EMSOpenFactorActuated ),
			EMSOpenFactor( EMSOpenFactor ),
			Height( Height ),
			Width( Width ),
			CHeight( CHeight ),
			VentControl( VentControl ),
			VentSchName( VentSchName ),
			ModulateFactor( ModulateFactor ),
			LowValueTemp( LowValueTemp ),
			UpValueTemp( UpValueTemp ),
			LowValueEnth( LowValueEnth ),
			UpValueEnth( UpValueEnth ),
			VentingSchName( VentingSchName ),
			VentSchNum( VentSchNum ),
			VentSurfCtrNum( VentSurfCtrNum ),
			VentingSchNum( VentingSchNum ),
			ZonePtr( ZonePtr ),
			IndVentControl( IndVentControl ),
			ExtLargeOpeningErrCount( ExtLargeOpeningErrCount ),
			ExtLargeOpeningErrIndex( ExtLargeOpeningErrIndex ),
			OpenFactorErrCount( OpenFactorErrCount ),
			OpenFactorErrIndex( OpenFactorErrIndex ),
			Multiplier( Multiplier ),
			HybridVentClose( HybridVentClose ),
			HybridCtrlGlobal( HybridCtrlGlobal ),
			HybridCtrlMaster( HybridCtrlMaster ),
			WindModifier( WindModifier ),
			OccupantVentilationControlName( OccupantVentilationControlName ),
			OccupantVentilationControlNum( OccupantVentilationControlNum ),
			OpeningStatus( OpeningStatus ),
			PrevOpeningstatus( PrevOpeningstatus ),
			CloseElapsedTime( CloseElapsedTime ),
			OpenElapsedTime( OpenElapsedTime ),
			ClosingProbStatus( ClosingProbStatus ),
			OpeningProbStatus( OpeningProbStatus )
		{}

	};

	struct MultizoneCompDetOpeningProp // Large detailed opening component
	{
		// Members
		std::string Name; // Name of large detailed opening component
		Real64 FlowCoef; // Air Mass Flow Coefficient When Window or Door Is Closed
		Real64 FlowExpo; // Air Mass Flow exponent When Window or Door Is Closed
		std::string TypeName; // Name of Large vertical opening type
		int LVOType; // Large vertical opening type number
		Real64 LVOValue; // Extra crack length for LVO type 1 with multiple openable parts,
		// or Height of pivoting axis for LVO type 2
		int NumFac; // Number of Opening Factor Values
		Real64 OpenFac1; // Opening factor #1
		Real64 DischCoeff1; // Discharge coefficient for opening factor #1
		Real64 WidthFac1; // Width factor for for Opening factor #1
		Real64 HeightFac1; // Height factor for opening factor #1
		Real64 StartHFac1; // Start height factor for opening factor #1
		Real64 OpenFac2; // Opening factor #2
		Real64 DischCoeff2; // Discharge coefficient for opening factor #2
		Real64 WidthFac2; // Width factor for for Opening factor #2
		Real64 HeightFac2; // Height factor for opening factor #2
		Real64 StartHFac2; // Start height factor for opening factor #2
		Real64 OpenFac3; // Opening factor #3
		Real64 DischCoeff3; // Discharge coefficient for opening factor #3
		Real64 WidthFac3; // Width factor for for Opening factor #3
		Real64 HeightFac3; // Height factor for opening factor #3
		Real64 StartHFac3; // Start height factor for opening factor #3
		Real64 OpenFac4; // Opening factor #4
		Real64 DischCoeff4; // Discharge coefficient for opening factor #4
		Real64 WidthFac4; // Width factor for for Opening factor #4
		Real64 HeightFac4; // Height factor for opening factor #4
		Real64 StartHFac4; // Start height factor for opening factor #4
		Real64 OpenFactor; // Opening factor
		int WidthErrCount; // Width error count
		int WidthErrIndex; // Width error index
		int HeightErrCount; // Height error count
		int HeightErrIndex; // Height error index

		// Default Constructor
		MultizoneCompDetOpeningProp() :
			FlowCoef( 0.0 ),
			FlowExpo( 0.0 ),
			TypeName( "NONPIVOTED" ),
			LVOType( 0 ),
			LVOValue( 0.0 ),
			NumFac( 0 ),
			OpenFac1( 0.0 ),
			DischCoeff1( 0.0 ),
			WidthFac1( 0.0 ),
			HeightFac1( 0.0 ),
			StartHFac1( 0.0 ),
			OpenFac2( 0.0 ),
			DischCoeff2( 0.0 ),
			WidthFac2( 0.0 ),
			HeightFac2( 0.0 ),
			StartHFac2( 0.0 ),
			OpenFac3( 0.0 ),
			DischCoeff3( 0.0 ),
			WidthFac3( 0.0 ),
			HeightFac3( 0.0 ),
			StartHFac3( 0.0 ),
			OpenFac4( 0.0 ),
			DischCoeff4( 0.0 ),
			WidthFac4( 0.0 ),
			HeightFac4( 0.0 ),
			StartHFac4( 0.0 ),
			OpenFactor( 0.0 ),
			WidthErrCount( 0 ),
			WidthErrIndex( 0 ),
			HeightErrCount( 0 ),
			HeightErrIndex( 0 )
		{}

		// Member Constructor
		MultizoneCompDetOpeningProp(
			std::string const & Name, // Name of large detailed opening component
			Real64 const FlowCoef, // Air Mass Flow Coefficient When Window or Door Is Closed
			Real64 const FlowExpo, // Air Mass Flow exponent When Window or Door Is Closed
			std::string const & TypeName, // Name of Large vertical opening type
			int const LVOType, // Large vertical opening type number
			Real64 const LVOValue, // Extra crack length for LVO type 1 with multiple openable parts,
			int const NumFac, // Number of Opening Factor Values
			Real64 const OpenFac1, // Opening factor #1
			Real64 const DischCoeff1, // Discharge coefficient for opening factor #1
			Real64 const WidthFac1, // Width factor for for Opening factor #1
			Real64 const HeightFac1, // Height factor for opening factor #1
			Real64 const StartHFac1, // Start height factor for opening factor #1
			Real64 const OpenFac2, // Opening factor #2
			Real64 const DischCoeff2, // Discharge coefficient for opening factor #2
			Real64 const WidthFac2, // Width factor for for Opening factor #2
			Real64 const HeightFac2, // Height factor for opening factor #2
			Real64 const StartHFac2, // Start height factor for opening factor #2
			Real64 const OpenFac3, // Opening factor #3
			Real64 const DischCoeff3, // Discharge coefficient for opening factor #3
			Real64 const WidthFac3, // Width factor for for Opening factor #3
			Real64 const HeightFac3, // Height factor for opening factor #3
			Real64 const StartHFac3, // Start height factor for opening factor #3
			Real64 const OpenFac4, // Opening factor #4
			Real64 const DischCoeff4, // Discharge coefficient for opening factor #4
			Real64 const WidthFac4, // Width factor for for Opening factor #4
			Real64 const HeightFac4, // Height factor for opening factor #4
			Real64 const StartHFac4, // Start height factor for opening factor #4
			Real64 const OpenFactor, // Opening factor
			int const WidthErrCount, // Width error count
			int const WidthErrIndex, // Width error index
			int const HeightErrCount, // Height error count
			int const HeightErrIndex // Height error index
		) :
			Name( Name ),
			FlowCoef( FlowCoef ),
			FlowExpo( FlowExpo ),
			TypeName( TypeName ),
			LVOType( LVOType ),
			LVOValue( LVOValue ),
			NumFac( NumFac ),
			OpenFac1( OpenFac1 ),
			DischCoeff1( DischCoeff1 ),
			WidthFac1( WidthFac1 ),
			HeightFac1( HeightFac1 ),
			StartHFac1( StartHFac1 ),
			OpenFac2( OpenFac2 ),
			DischCoeff2( DischCoeff2 ),
			WidthFac2( WidthFac2 ),
			HeightFac2( HeightFac2 ),
			StartHFac2( StartHFac2 ),
			OpenFac3( OpenFac3 ),
			DischCoeff3( DischCoeff3 ),
			WidthFac3( WidthFac3 ),
			HeightFac3( HeightFac3 ),
			StartHFac3( StartHFac3 ),
			OpenFac4( OpenFac4 ),
			DischCoeff4( DischCoeff4 ),
			WidthFac4( WidthFac4 ),
			HeightFac4( HeightFac4 ),
			StartHFac4( StartHFac4 ),
			OpenFactor( OpenFactor ),
			WidthErrCount( WidthErrCount ),
			WidthErrIndex( WidthErrIndex ),
			HeightErrCount( HeightErrCount ),
			HeightErrIndex( HeightErrIndex )
		{}

	};

	struct MultizoneCompSimpleOpeningProp // Large simple opening component
	{
		// Members
		std::string Name; // Name of large simple opening component
		Real64 FlowCoef; // Air Mass Flow Coefficient When Window or Door Is Closed
		Real64 FlowExpo; // Air Mass Flow exponent When Window or Door Is Closed
		Real64 MinRhoDiff; // Minimum density difference for two-way flow
		Real64 DischCoeff; // Discharge coefficient at full opening
		Real64 OpenFactor; // Opening factor

		// Default Constructor
		MultizoneCompSimpleOpeningProp() :
			FlowCoef( 0.0 ),
			FlowExpo( 0.0 ),
			MinRhoDiff( 0.0 ),
			DischCoeff( 0.0 ),
			OpenFactor( 0.0 )
		{}

		// Member Constructor
		MultizoneCompSimpleOpeningProp(
			std::string const & Name, // Name of large simple opening component
			Real64 const FlowCoef, // Air Mass Flow Coefficient When Window or Door Is Closed
			Real64 const FlowExpo, // Air Mass Flow exponent When Window or Door Is Closed
			Real64 const MinRhoDiff, // Minimum density difference for two-way flow
			Real64 const DischCoeff, // Discharge coefficient at full opening
			Real64 const OpenFactor // Opening factor
		) :
			Name( Name ),
			FlowCoef( FlowCoef ),
			FlowExpo( FlowExpo ),
			MinRhoDiff( MinRhoDiff ),
			DischCoeff( DischCoeff ),
			OpenFactor( OpenFactor )
		{}

	};

	struct MultizoneCompHorOpeningProp // Large horizontal opening component
	{
		// Members
		std::string Name; // Name of large horizontal opening component
		Real64 FlowCoef; // Air Mass Flow Coefficient When Window or Door Is Closed
		Real64 FlowExpo; // Air Mass Flow exponent When Window or Door Is Closed
		Real64 Slope; // Sloping plane angle
		Real64 DischCoeff; // Discharge coefficient at full opening

		// Default Constructor
		MultizoneCompHorOpeningProp() :
			FlowCoef( 0.0 ),
			FlowExpo( 0.0 ),
			Slope( 0.0 ),
			DischCoeff( 0.0 )
		{}

		// Member Constructor
		MultizoneCompHorOpeningProp(
			std::string const & Name, // Name of large horizontal opening component
			Real64 const FlowCoef, // Air Mass Flow Coefficient When Window or Door Is Closed
			Real64 const FlowExpo, // Air Mass Flow exponent When Window or Door Is Closed
			Real64 const Slope, // Sloping plane angle
			Real64 const DischCoeff // Discharge coefficient at full opening
		) :
			Name( Name ),
			FlowCoef( FlowCoef ),
			FlowExpo( FlowExpo ),
			Slope( Slope ),
			DischCoeff( DischCoeff )
		{}

	};

	struct MultizoneSurfaceCrackStdCndns // Surface crack standard conditions
	{
		// Members
		std::string Name; // Name of standard conditions component
		Real64 StandardT; // Standard temperature for crack data
		Real64 StandardP; // Standard borometric pressure for crack data
		Real64 StandardW; // Standard humidity ratio for crack data

		// Default Constructor
		MultizoneSurfaceCrackStdCndns() :
			StandardT( 0.0 ),
			StandardP( 0.0 ),
			StandardW( 0.0 )
		{}

		// Member Constructor
		MultizoneSurfaceCrackStdCndns(
			std::string const & Name, // Name of standard conditions component
			Real64 const StandardT, // Standard temperature for crack data
			Real64 const StandardP, // Standard borometric pressure for crack data
			Real64 const StandardW // Standard humidity ratio for crack data
		) :
			Name( Name ),
			StandardT( StandardT ),
			StandardP( StandardP ),
			StandardW( StandardW )
		{}

	};

	struct MultizoneSurfaceCrackProp // Surface crack component
	{
		// Members
		std::string Name; // Name of crack component
		std::string ExternalNodeNames; // Name of external node.Not requird for internal surface
		Real64 FlowCoef; // Air Mass Flow Coefficient When Window or Door Is Closed
		Real64 FlowExpo; // Air Mass Flow exponent When Window or Door Is Closed
		Real64 StandardT; // Standard temperature for crack data
		Real64 StandardP; // Standard borometric pressure for crack data
		Real64 StandardW; // Standard humidity ratio for crack data

		// Default Constructor
		MultizoneSurfaceCrackProp() :
			FlowCoef( 0.0 ),
			FlowExpo( 0.0 ),
			StandardT( 0.0 ),
			StandardP( 0.0 ),
			StandardW( 0.0 )
		{}

		// Member Constructor
		MultizoneSurfaceCrackProp(
			std::string const & Name, // Name of crack component
			std::string const & ExternalNodeNames, // Name of external node.Not requird for internal surface
			Real64 const FlowCoef, // Air Mass Flow Coefficient When Window or Door Is Closed
			Real64 const FlowExpo, // Air Mass Flow exponent When Window or Door Is Closed
			Real64 const StandardT, // Standard temperature for crack data
			Real64 const StandardP, // Standard borometric pressure for crack data
			Real64 const StandardW // Standard humidity ratio for crack data
		) :
			Name( Name ),
			ExternalNodeNames( ExternalNodeNames ),
			FlowCoef( FlowCoef ),
			FlowExpo( FlowExpo ),
			StandardT( StandardT ),
			StandardP( StandardP ),
			StandardW( StandardW )
		{}

	};

	struct MultizoneSurfaceELAProp // Surface effective leakage area component
	{
		// Members
		std::string Name; // Name of effective leakage area component
		Real64 ELA; // Effective leakage area
		Real64 DischCoeff; // Discharge coefficient
		Real64 RefDeltaP; // Reference pressure difference
		Real64 FlowExpo; // Air Mass Flow exponent When Window or Door Is Closed
		Real64 TestDeltaP; // Testing pressure difference
		Real64 TestDisCoef; // Testing Discharge coefficient

		// Default Constructor
		MultizoneSurfaceELAProp() :
			ELA( 0.0 ),
			DischCoeff( 0.0 ),
			RefDeltaP( 0.0 ),
			FlowExpo( 0.0 ),
			TestDeltaP( 0.0 ),
			TestDisCoef( 0.0 )
		{}

		// Member Constructor
		MultizoneSurfaceELAProp(
			std::string const & Name, // Name of effective leakage area component
			Real64 const ELA, // Effective leakage area
			Real64 const DischCoeff, // Discharge coefficient
			Real64 const RefDeltaP, // Reference pressure difference
			Real64 const FlowExpo, // Air Mass Flow exponent When Window or Door Is Closed
			Real64 const TestDeltaP, // Testing pressure difference
			Real64 const TestDisCoef // Testing Discharge coefficient
		) :
			Name( Name ),
			ELA( ELA ),
			DischCoeff( DischCoeff ),
			RefDeltaP( RefDeltaP ),
			FlowExpo( FlowExpo ),
			TestDeltaP( TestDeltaP ),
			TestDisCoef( TestDisCoef )
		{}

	};

	struct MultizoneCompExhaustFanProp // Zone exhaust fan component
	{
		// Members
		std::string Name; // Name of exhaust fan component
		Real64 FlowRate; // mass flow rate
		int SchedPtr; // Schedule pointer
		Real64 FlowCoef; // Air Mass Flow Coefficient
		Real64 FlowExpo; // Air Mass Flow exponent
		Real64 StandardT; // Standard temperature for crack data
		Real64 StandardP; // Standard borometric pressure for crack data
		Real64 StandardW; // Standard humidity ratio for crack data
		int InletNode; // Inlet node number
		int OutletNode; // Outlet node number
		int EPlusZoneNum; // Zone number

		// Default Constructor
		MultizoneCompExhaustFanProp() :
			FlowRate( 0.0 ),
			SchedPtr( 0 ),
			FlowCoef( 0.0 ),
			FlowExpo( 0.0 ),
			StandardT( 0.0 ),
			StandardP( 0.0 ),
			StandardW( 0.0 ),
			InletNode( 0 ),
			OutletNode( 0 ),
			EPlusZoneNum( 0 )
		{}

		// Member Constructor
		MultizoneCompExhaustFanProp(
			std::string const & Name, // Name of exhaust fan component
			Real64 const FlowRate, // mass flow rate
			int const SchedPtr, // Schedule pointer
			Real64 const FlowCoef, // Air Mass Flow Coefficient
			Real64 const FlowExpo, // Air Mass Flow exponent
			Real64 const StandardT, // Standard temperature for crack data
			Real64 const StandardP, // Standard borometric pressure for crack data
			Real64 const StandardW, // Standard humidity ratio for crack data
			int const InletNode, // Inlet node number
			int const OutletNode, // Outlet node number
			int const EPlusZoneNum // Zone number
		) :
			Name( Name ),
			FlowRate( FlowRate ),
			SchedPtr( SchedPtr ),
			FlowCoef( FlowCoef ),
			FlowExpo( FlowExpo ),
			StandardT( StandardT ),
			StandardP( StandardP ),
			StandardW( StandardW ),
			InletNode( InletNode ),
			OutletNode( OutletNode ),
			EPlusZoneNum( EPlusZoneNum )
		{}

	};

	struct MultizoneExternalNodeProp // External node properties
	{
		// Members
		std::string Name; // Name of external node
		std::string WPCName; // Wind Pressure Coefficient Values Object Name
		Real64 Orien; // Orientation
		Real64 Height; // Nodal height
		int ExtNum; // External node number
		int CPVNum; // CP Value number
		int FacadeNum; // Facade number

		// Default Constructor
		MultizoneExternalNodeProp() :
			Orien( 0.0 ),
			Height( 0.0 ),
			ExtNum( 0 ),
			CPVNum( 0 ),
			FacadeNum( 0 )
		{}

		// Member Constructor
		MultizoneExternalNodeProp(
			std::string const & Name, // Name of external node
			std::string const & WPCName, // Wind Pressure Coefficient Values Object Name
			Real64 const Orien, // Orientation
			Real64 const Height, // Nodal height
			int const ExtNum, // External node number
			int const CPVNum, // CP Value number
			int const FacadeNum // Facade number
		) :
			Name( Name ),
			WPCName( WPCName ),
			Orien( Orien ),
			Height( Height ),
			ExtNum( ExtNum ),
			CPVNum( CPVNum ),
			FacadeNum( FacadeNum )
		{}

	};

	struct MultizoneCPArrayProp // CP Array
	{
		// Members
		std::string Name; // Name of CP array
		int NumWindDir; // Number of wind directions
		Array1D< Real64 > WindDir; // Wind direction

		// Default Constructor
		MultizoneCPArrayProp() :
			NumWindDir( 0 )
		{}

		// Member Constructor
		MultizoneCPArrayProp(
			std::string const & Name, // Name of CP array
			int const NumWindDir, // Number of wind directions
			Array1< Real64 > const & WindDir // Wind direction
		) :
			Name( Name ),
			NumWindDir( NumWindDir ),
			WindDir( WindDir )
		{}

	};

	struct MultizoneCPValueProp // CP Value
	{
		// Members
		std::string Name; // Name of CP Value
		std::string CPArrayName; // CP array Name
		Array1D< Real64 > CPValue; // CP Value

		// Default Constructor
		MultizoneCPValueProp()
		{}

		// Member Constructor
		MultizoneCPValueProp(
			std::string const & Name, // Name of CP Value
			std::string const & CPArrayName, // CP array Name
			Array1< Real64 > const & CPValue // CP Value
		) :
			Name( Name ),
			CPArrayName( CPArrayName ),
			CPValue( CPValue )
		{}

	};

	struct DeltaCpProp
	{
		// Members
		Array1D< Real64 > WindDir; // Wind direction

		// Default Constructor
		DeltaCpProp()
		{}

		// Member Constructor
		explicit
		DeltaCpProp( Array1< Real64 > const & WindDir ) :
			WindDir( WindDir )
		{}

	};

	struct DisSysNodeProp // CP Value
	{
		// Members
		std::string Name; // Name of node
		std::string EPlusName; // EnergyPlus node name
		std::string EPlusType; // EnergyPlus node type
		Real64 Height; // Nodal height
		int EPlusNodeNum; // EPlus node number

		// Default Constructor
		DisSysNodeProp() :
			Height( 0.0 ),
			EPlusNodeNum( 0 )
		{}

		// Member Constructor
		DisSysNodeProp(
			std::string const & Name, // Name of node
			std::string const & EPlusName, // EnergyPlus node name
			std::string const & EPlusType, // EnergyPlus node type
			Real64 const Height, // Nodal height
			int const EPlusNodeNum // EPlus node number
		) :
			Name( Name ),
			EPlusName( EPlusName ),
			EPlusType( EPlusType ),
			Height( Height ),
			EPlusNodeNum( EPlusNodeNum )
		{}

	};

	struct DisSysCompLeakProp // duct leak component
	{
		// Members
		std::string Name; // Name of component leak
		Real64 FlowCoef; // Air Mass Flow Coefficient
		Real64 FlowExpo; // Air Mass Flow exponent

		// Default Constructor
		DisSysCompLeakProp() :
			FlowCoef( 0.0 ),
			FlowExpo( 0.0 )
		{}

		// Member Constructor
		DisSysCompLeakProp(
			std::string const & Name, // Name of component leak
			Real64 const FlowCoef, // Air Mass Flow Coefficient
			Real64 const FlowExpo // Air Mass Flow exponent
		) :
			Name( Name ),
			FlowCoef( FlowCoef ),
			FlowExpo( FlowExpo )
		{}

	};

	struct DisSysCompELRProp // effective leakage ratio component
	{
		// Members
		std::string Name; // Name of component leak
		Real64 ELR; // Value of effective leakage ratio
		Real64 FlowRate; // Maximum airflow rate
		Real64 RefPres; // Reference pressure difference
		Real64 FlowExpo; // Air Mass Flow exponent

		// Default Constructor
		DisSysCompELRProp() :
			ELR( 0.0 ),
			FlowRate( 0.0 ),
			RefPres( 0.0 ),
			FlowExpo( 0.0 )
		{}

		// Member Constructor
		DisSysCompELRProp(
			std::string const & Name, // Name of component leak
			Real64 const ELR, // Value of effective leakage ratio
			Real64 const FlowRate, // Maximum airflow rate
			Real64 const RefPres, // Reference pressure difference
			Real64 const FlowExpo // Air Mass Flow exponent
		) :
			Name( Name ),
			ELR( ELR ),
			FlowRate( FlowRate ),
			RefPres( RefPres ),
			FlowExpo( FlowExpo )
		{}

	};

	struct DisSysCompDuctProp // Duct component
	{
		// Members
		std::string Name; // Name of duct component
		Real64 L; // Duct length [m]
		Real64 D; // Hydrolic diameter [m]
		Real64 A; // Cross section area [m2]
		Real64 Rough; // Surface roughness [m]
		Real64 TurDynCoef; // Turbulent dynamic loss coefficient
		Real64 UThermal; // Overall heat transmittance [W/m2.K]
		Real64 UMoisture; // Overall moisture transmittance [kg/m2]
		Real64 MThermal; // Thermal capacity [J/K]
		Real64 MMoisture; // Mositure capacity [kg]
		Real64 LamDynCoef; // Laminar dynamic loss coefficient
		Real64 LamFriCoef; // Laminar friction loss coefficient
		Real64 InitLamCoef; // Coefficient of linear initialization
		Real64 RelRough; // e/D: relative roughness,
		Real64 RelL; // L/D: relative length,
		Real64 g; // 1/sqrt(Darcy friction factor),
		Real64 A1; // 1.14 - 0.868589*ln(e/D),

		// Default Constructor
		DisSysCompDuctProp() :
			L( 0.0 ),
			D( 0.0 ),
			A( 0.0 ),
			Rough( 0.0 ),
			TurDynCoef( 0.0 ),
			UThermal( 0.0 ),
			UMoisture( 0.0 ),
			MThermal( 0.0 ),
			MMoisture( 0.0 ),
			LamDynCoef( 0.0 ),
			LamFriCoef( 0.0 ),
			InitLamCoef( 0.0 ),
			RelRough( 0.0 ),
			RelL( 0.0 ),
			g( 0.0 ),
			A1( 0.0 )
		{}

		// Member Constructor
		DisSysCompDuctProp(
			std::string const & Name, // Name of duct component
			Real64 const L, // Duct length [m]
			Real64 const D, // Hydrolic diameter [m]
			Real64 const A, // Cross section area [m2]
			Real64 const Rough, // Surface roughness [m]
			Real64 const TurDynCoef, // Turbulent dynamic loss coefficient
			Real64 const UThermal, // Overall heat transmittance [W/m2.K]
			Real64 const UMoisture, // Overall moisture transmittance [kg/m2]
			Real64 const MThermal, // Thermal capacity [J/K]
			Real64 const MMoisture, // Mositure capacity [kg]
			Real64 const LamDynCoef, // Laminar dynamic loss coefficient
			Real64 const LamFriCoef, // Laminar friction loss coefficient
			Real64 const InitLamCoef, // Coefficient of linear initialization
			Real64 const RelRough, // e/D: relative roughness,
			Real64 const RelL, // L/D: relative length,
			Real64 const g, // 1/sqrt(Darcy friction factor),
			Real64 const A1 // 1.14 - 0.868589*ln(e/D),
		) :
			Name( Name ),
			L( L ),
			D( D ),
			A( A ),
			Rough( Rough ),
			TurDynCoef( TurDynCoef ),
			UThermal( UThermal ),
			UMoisture( UMoisture ),
			MThermal( MThermal ),
			MMoisture( MMoisture ),
			LamDynCoef( LamDynCoef ),
			LamFriCoef( LamFriCoef ),
			InitLamCoef( InitLamCoef ),
			RelRough( RelRough ),
			RelL( RelL ),
			g( g ),
			A1( A1 )
		{}

	};

	struct DisSysCompDamperProp // Damper component
	{
		// Members
		std::string Name; // Name of damper component
		Real64 LTP; // Value for laminar turbulent transition
		Real64 LamFlow; // Laminar flow coefficient
		Real64 TurFlow; // Turbulent flow coefficient
		Real64 FlowExpo; // Air Mass Flow exponent
		Real64 FlowMin; // Minimum control air mass rate
		Real64 FlowMax; // Maximum control air mass rate
		Real64 A0; // First polynomial coefficient of the control variable (constant coefficient)
		Real64 A1; // Second polynomial coefficient of the control variable (linear coefficient)
		Real64 A2; // Third polynomial coefficient of the control variable (quadratic coefficient)
		Real64 A3; // Fourth polynomial coefficient of the control variable (cubic coefficient)

		// Default Constructor
		DisSysCompDamperProp() :
			LTP( 0.0 ),
			LamFlow( 0.0 ),
			TurFlow( 0.0 ),
			FlowExpo( 0.0 ),
			FlowMin( 0.0 ),
			FlowMax( 0.0 ),
			A0( 0.0 ),
			A1( 0.0 ),
			A2( 0.0 ),
			A3( 0.0 )
		{}

		// Member Constructor
		DisSysCompDamperProp(
			std::string const & Name, // Name of damper component
			Real64 const LTP, // Value for laminar turbulent transition
			Real64 const LamFlow, // Laminar flow coefficient
			Real64 const TurFlow, // Turbulent flow coefficient
			Real64 const FlowExpo, // Air Mass Flow exponent
			Real64 const FlowMin, // Minimum control air mass rate
			Real64 const FlowMax, // Maximum control air mass rate
			Real64 const A0, // First polynomial coefficient of the control variable (constant coefficient)
			Real64 const A1, // Second polynomial coefficient of the control variable (linear coefficient)
			Real64 const A2, // Third polynomial coefficient of the control variable (quadratic coefficient)
			Real64 const A3 // Fourth polynomial coefficient of the control variable (cubic coefficient)
		) :
			Name( Name ),
			LTP( LTP ),
			LamFlow( LamFlow ),
			TurFlow( TurFlow ),
			FlowExpo( FlowExpo ),
			FlowMin( FlowMin ),
			FlowMax( FlowMax ),
			A0( A0 ),
			A1( A1 ),
			A2( A2 ),
			A3( A3 )
		{}

	};

	struct DisSysCompCVFProp // Constant volume fan component
	{
		// Members
		std::string Name; // Name of detailed fan component
		Real64 FlowRate; // Air volume flow rate
		Real64 Ctrl; // Control ratio
		int FanTypeNum; // Fan type: Constant volume or ONOFF
		int FanIndex; // Fan index
		int InletNode; // Inlet node number
		int OutletNode; // Outlet node number
		Real64 MaxAirMassFlowRate; // Max Specified MAss Flow Rate of Damper [kg/s]

		// Default Constructor
		DisSysCompCVFProp() :
			FlowRate( 0.0 ),
			Ctrl( 0.0 ),
			FanTypeNum( 0 ),
			FanIndex( 0 ),
			InletNode( 0 ),
			OutletNode( 0 ),
			MaxAirMassFlowRate( 0.0 )
		{}

		// Member Constructor
		DisSysCompCVFProp(
			std::string const & Name, // Name of detailed fan component
			Real64 const FlowRate, // Air volume flow rate
			Real64 const Ctrl, // Control ratio
			int const FanTypeNum, // Fan type: Constant volume or ONOFF
			int const FanIndex, // Fan index
			int const InletNode, // Inlet node number
			int const OutletNode, // Outlet node number
			Real64 const MaxAirMassFlowRate // Max Specified MAss Flow Rate of Damper [kg/s]
		) :
			Name( Name ),
			FlowRate( FlowRate ),
			Ctrl( Ctrl ),
			FanTypeNum( FanTypeNum ),
			FanIndex( FanIndex ),
			InletNode( InletNode ),
			OutletNode( OutletNode ),
			MaxAirMassFlowRate( MaxAirMassFlowRate )
		{}

	};

	struct DisSysCompDetFanProp // Detailed fan component
	{
		// Members
		std::string Name; // Name of constant volume fan component
		Real64 FlowCoef; // Coefficient for linear initialization
		Real64 FlowExpo; // Turbulent flow coefficient
		Real64 RhoAir; // Reference air density
		Real64 Qfree; // Free delivery flow at P=0
		Real64 Pshut; // Shutoff pressure at Q=0
		Real64 TranRat; // Flow coefficient at laminar/turbulent transition
		int n; // Number of ranges for fan performance curve
		Array1D< Real64 > Coeff; // Coefficients of fan performance curve.
		//Each range has a min flow rate and 4 coeffieincts

		// Default Constructor
		DisSysCompDetFanProp() :
			FlowCoef( 0.0 ),
			FlowExpo( 0.0 ),
			RhoAir( 0.0 ),
			Qfree( 0.0 ),
			Pshut( 0.0 ),
			TranRat( 0.0 )
		{}

		// Member Constructor
		DisSysCompDetFanProp(
			std::string const & Name, // Name of constant volume fan component
			Real64 const FlowCoef, // Coefficient for linear initialization
			Real64 const FlowExpo, // Turbulent flow coefficient
			Real64 const RhoAir, // Reference air density
			Real64 const Qfree, // Free delivery flow at P=0
			Real64 const Pshut, // Shutoff pressure at Q=0
			Real64 const TranRat, // Flow coefficient at laminar/turbulent transition
			int const n, // Number of ranges for fan performance curve
			Array1< Real64 > const & Coeff // Coefficients of fan performance curve.
		) :
			Name( Name ),
			FlowCoef( FlowCoef ),
			FlowExpo( FlowExpo ),
			RhoAir( RhoAir ),
			Qfree( Qfree ),
			Pshut( Pshut ),
			TranRat( TranRat ),
			n( n ),
			Coeff( Coeff )
		{}

	};

	struct DisSysCompCoilProp // Coil component
	{
		// Members
		std::string Name; // Name of coil component
		std::string EPlusType; // EnergyPlus coil type
		Real64 L; // Air path length
		Real64 D; // Air path hydraulic diameter

		// Default Constructor
		DisSysCompCoilProp() :
			L( 0.0 ),
			D( 0.0 )
		{}

		// Member Constructor
		DisSysCompCoilProp(
			std::string const & Name, // Name of coil component
			std::string const & EPlusType, // EnergyPlus coil type
			Real64 const L, // Air path length
			Real64 const D // Air path hydraulic diameter
		) :
			Name( Name ),
			EPlusType( EPlusType ),
			L( L ),
			D( D )
		{}

	};

	struct DisSysCompHXProp // Coil component
	{
		// Members
		std::string Name; // Name of coil component
		std::string EPlusType; // EnergyPlus coil type
		Real64 L; // Air path length
		Real64 D; // Air path hydraulic diameter
		bool CoilParentExists; // Is a coil component

		// Default Constructor
		DisSysCompHXProp() :
			L( 0.0 ),
			D( 0.0 ),
			CoilParentExists( false )
		{}

		// Member Constructor
		DisSysCompHXProp(
			std::string const & Name, // Name of coil component
			std::string const & EPlusType, // EnergyPlus coil type
			Real64 const L, // Air path length
			Real64 const D, // Air path hydraulic diameter
			bool const CoilParentExists // Is a coil component
		) :
			Name( Name ),
			EPlusType( EPlusType ),
			L( L ),
			D( D ),
			CoilParentExists( CoilParentExists )
		{}

	};

	struct DisSysCompTermUnitProp // Turminal unit component
	{
		// Members
		std::string Name; // Name of coil component
		std::string EPlusType; // EnergyPlus coil type
		Real64 L; // Air path length
		Real64 D; // Air path hydraulic diameter
		int DamperInletNode; // Damper inlet node number
		int DamperOutletNode; // Damper outlet node number

		// Default Constructor
		DisSysCompTermUnitProp() :
			L( 0.0 ),
			D( 0.0 ),
			DamperInletNode( 0 ),
			DamperOutletNode( 0 )
		{}

		// Member Constructor
		DisSysCompTermUnitProp(
			std::string const & Name, // Name of coil component
			std::string const & EPlusType, // EnergyPlus coil type
			Real64 const L, // Air path length
			Real64 const D, // Air path hydraulic diameter
			int const DamperInletNode, // Damper inlet node number
			int const DamperOutletNode // Damper outlet node number
		) :
			Name( Name ),
			EPlusType( EPlusType ),
			L( L ),
			D( D ),
			DamperInletNode( DamperInletNode ),
			DamperOutletNode( DamperOutletNode )
		{}

	};

	struct DisSysCompCPDProp // Constant pressure drop component
	{
		// Members
		std::string Name; // Name of constant pressure drop component
		Real64 A; // cross section area
		Real64 DP; // Pressure difference across the component

		// Default Constructor
		DisSysCompCPDProp() :
			A( 0.0 ),
			DP( 0.0 )
		{}

		// Member Constructor
		DisSysCompCPDProp(
			std::string const & Name, // Name of constant pressure drop component
			Real64 const A, // cross section area
			Real64 const DP // Pressure difference across the component
		) :
			Name( Name ),
			A( A ),
			DP( DP )
		{}

	};

	struct DisSysLinkageProp // Distribution system linkage data
	{
		// Members
		std::string LinkName; // Name of distribution system linkage
		Array1D_string NodeNames; // Names of nodes (limited to 2)
		Array1D< Real64 > NodeHeights; // Node heights
		std::string CompName; // Name of element
		int CompNum; // Element Number
		std::string ZoneName; // Name of zone
		int ZoneNum; // Zone Number
		Array1D_int NodeNums; // Node numbers
		int LinkNum; // Linkage number

		// Default Constructor
		DisSysLinkageProp() :
			NodeNames( 2 ),
			NodeHeights( 2, 0.0 ),
			CompNum( 0 ),
			ZoneNum( 0 ),
			NodeNums( 2, 0 ),
			LinkNum( 0 )
		{}

		// Member Constructor
		DisSysLinkageProp(
			std::string const & LinkName, // Name of distribution system linkage
			Array1_string const & NodeNames, // Names of nodes (limited to 2)
			Array1< Real64 > const & NodeHeights, // Node heights
			std::string const & CompName, // Name of element
			int const CompNum, // Element Number
			std::string const & ZoneName, // Name of zone
			int const ZoneNum, // Zone Number
			Array1_int const & NodeNums, // Node numbers
			int const LinkNum // Linkage number
		) :
			LinkName( LinkName ),
			NodeNames( 2, NodeNames ),
			NodeHeights( 2, NodeHeights ),
			CompName( CompName ),
			CompNum( CompNum ),
			ZoneName( ZoneName ),
			ZoneNum( ZoneNum ),
			NodeNums( 2, NodeNums ),
			LinkNum( LinkNum )
		{}

	};

	struct AirflowNetworkNodeProp // AirflowNetwork nodal data
	{
		// Members
		std::string Name; // Provide a unique node name
		std::string NodeType; // Provide node type "External", "Thermal Zone" or "Other"
		std::string EPlusNode; // EnergyPlus node name
		Real64 NodeHeight; // Node height [m]
		int NodeNum; // Node number
		int NodeTypeNum; // Node type with integer number
		// 0: Calculated, 1: Given pressure;
		std::string EPlusZoneName; // EnergyPlus node name
		int EPlusZoneNum; // E+ zone number
		int EPlusNodeNum;
		int ExtNodeNum;
		int EPlusTypeNum;

		// Default Constructor
		AirflowNetworkNodeProp() :
			NodeHeight( 0.0 ),
			NodeNum( 0 ),
			NodeTypeNum( 0 ),
			EPlusZoneNum( 0 ),
			EPlusNodeNum( 0 ),
			ExtNodeNum( 0 ),
			EPlusTypeNum( 0 )
		{}

		// Member Constructor
		AirflowNetworkNodeProp(
			std::string const & Name, // Provide a unique node name
			std::string const & NodeType, // Provide node type "External", "Thermal Zone" or "Other"
			std::string const & EPlusNode, // EnergyPlus node name
			Real64 const NodeHeight, // Node height [m]
			int const NodeNum, // Node number
			int const NodeTypeNum, // Node type with integer number
			std::string const & EPlusZoneName, // EnergyPlus node name
			int const EPlusZoneNum, // E+ zone number
			int const EPlusNodeNum,
			int const ExtNodeNum,
			int const EPlusTypeNum
		) :
			Name( Name ),
			NodeType( NodeType ),
			EPlusNode( EPlusNode ),
			NodeHeight( NodeHeight ),
			NodeNum( NodeNum ),
			NodeTypeNum( NodeTypeNum ),
			EPlusZoneName( EPlusZoneName ),
			EPlusZoneNum( EPlusZoneNum ),
			EPlusNodeNum( EPlusNodeNum ),
			ExtNodeNum( ExtNodeNum ),
			EPlusTypeNum( EPlusTypeNum )
		{}

	};

	struct AirflowNetworkCompProp // AirflowNetwork element data
	{
		// Members
		std::string Name; // Provide a unique element name
		int CompTypeNum; // Provide numeric equivalent for AirflowNetworkCompType
		int TypeNum; // Component number under same component type
		int CompNum; // General component number
		std::string EPlusName; // Provide a unique element name
		std::string EPlusCompName; // Provide EPlus component name or Other
		std::string EPlusType; // Provide EPlus type, such as terminal reheat, coil, etc. 9/30/03 or Other
		int EPlusTypeNum; // Provide EPlus component type

		// Default Constructor
		AirflowNetworkCompProp() :
			CompTypeNum( 0 ),
			TypeNum( 0 ),
			CompNum( 0 ),
			EPlusTypeNum( 0 )
		{}

		// Member Constructor
		AirflowNetworkCompProp(
			std::string const & Name, // Provide a unique element name
			int const CompTypeNum, // Provide numeric equivalent for AirflowNetworkCompType
			int const TypeNum, // Component number under same component type
			int const CompNum, // General component number
			std::string const & EPlusName, // Provide a unique element name
			std::string const & EPlusCompName, // Provide EPlus component name or Other
			std::string const & EPlusType, // Provide EPlus type, such as terminal reheat, coil, etc. 9/30/03 or Other
			int const EPlusTypeNum // Provide EPlus component type
		) :
			Name( Name ),
			CompTypeNum( CompTypeNum ),
			TypeNum( TypeNum ),
			CompNum( CompNum ),
			EPlusName( EPlusName ),
			EPlusCompName( EPlusCompName ),
			EPlusType( EPlusType ),
			EPlusTypeNum( EPlusTypeNum )
		{}

	};

	struct AirflowNetworkLinkageProp // AirflowNetwork linkage data
	{
		// Members
		std::string Name; // Provide a unique linkage name
		Array1D_string NodeNames; // Names of nodes (limited to 2)
		Array1D< Real64 > NodeHeights; // Node heights
		std::string CompName; // Name of element
		int CompNum; // Element Number
		std::string ZoneName; // Name of zone
		int ZoneNum; // Zone Number
		Array1D_int NodeNums; // Node numbers
		int LinkNum; // Linkage number
		int DetOpenNum; // Large Opening number
		int ConnectionFlag; // Return and supply connection flag
		bool VAVTermDamper; // True if this component is a damper for a VAV terminal

		// Default Constructor
		AirflowNetworkLinkageProp() :
			NodeNames( 2 ),
			NodeHeights( 2, 0.0 ),
			CompNum( 0 ),
			ZoneNum( 0 ),
			NodeNums( 2, 0 ),
			LinkNum( 0 ),
			DetOpenNum( 0 ),
			ConnectionFlag( 0 ),
			VAVTermDamper( false )
		{}

		// Member Constructor
		AirflowNetworkLinkageProp(
			std::string const & Name, // Provide a unique linkage name
			Array1_string const & NodeNames, // Names of nodes (limited to 2)
			Array1< Real64 > const & NodeHeights, // Node heights
			std::string const & CompName, // Name of element
			int const CompNum, // Element Number
			std::string const & ZoneName, // Name of zone
			int const ZoneNum, // Zone Number
			Array1_int const & NodeNums, // Node numbers
			int const LinkNum, // Linkage number
			int const DetOpenNum, // Large Opening number
			int const ConnectionFlag, // Return and supply connection flag
			bool const VAVTermDamper // True if this component is a damper for a VAV terminal
		) :
			Name( Name ),
			NodeNames( 2, NodeNames ),
			NodeHeights( 2, NodeHeights ),
			CompName( CompName ),
			CompNum( CompNum ),
			ZoneName( ZoneName ),
			ZoneNum( ZoneNum ),
			NodeNums( 2, NodeNums ),
			LinkNum( LinkNum ),
			DetOpenNum( DetOpenNum ),
			ConnectionFlag( ConnectionFlag ),
			VAVTermDamper( VAVTermDamper )
		{}

	};

	struct AirflowNetworkNodeSimuData // Node variable for simulation
	{
		// Members
		Real64 TZ; // Temperature [C]
		Real64 WZ; // Humidity ratio [kg/kg]
		Real64 PZ; // Pressure [Pa]
		Real64 CO2Z; // CO2 [ppm]
		Real64 GCZ; // Generic contaminant [ppm]

		// Default Constructor
		AirflowNetworkNodeSimuData() :
			TZ( 0.0 ),
			WZ( 0.0 ),
			PZ( 0.0 ),
			CO2Z( 0.0 ),
			GCZ( 0.0 )
		{}

		// Member Constructor
		AirflowNetworkNodeSimuData(
			Real64 const TZ, // Temperature [C]
			Real64 const WZ, // Humidity ratio [kg/kg]
			Real64 const PZ, // Pressure [Pa]
			Real64 const CO2Z, // CO2 [ppm]
			Real64 const GCZ // Generic contaminant [ppm]
		) :
			TZ( TZ ),
			WZ( WZ ),
			PZ( PZ ),
			CO2Z( CO2Z ),
			GCZ( GCZ )
		{}

	};

	struct AirflowNetworkLinkSimuData
	{
		// Members
		Real64 FLOW; // Mass flow rate [kg/s]
		Real64 FLOW2; // Mass flow rate [kg/s] for two way flow
		Real64 DP; // Pressure difference across a component
		Real64 VolFLOW; // Mass flow rate [m3/s]
		Real64 VolFLOW2; // Mass flow rate [m3/s] for two way flow
		Real64 DP1;

		// Default Constructor
		AirflowNetworkLinkSimuData() :
			FLOW( 0.0 ),
			FLOW2( 0.0 ),
			DP( 0.0 ),
			VolFLOW( 0.0 ),
			VolFLOW2( 0.0 ),
			DP1( 0.0 )
		{}

		// Member Constructor
		AirflowNetworkLinkSimuData(
			Real64 const FLOW, // Mass flow rate [kg/s]
			Real64 const FLOW2, // Mass flow rate [kg/s] for two way flow
			Real64 const DP, // Pressure difference across a component
			Real64 const VolFLOW, // Mass flow rate [m3/s]
			Real64 const VolFLOW2, // Mass flow rate [m3/s] for two way flow
			Real64 const DP1
		) :
			FLOW( FLOW ),
			FLOW2( FLOW2 ),
			DP( DP ),
			VolFLOW( VolFLOW ),
			VolFLOW2( VolFLOW2 ),
			DP1( DP1 )
		{}

	};

	struct AirflowNetworkLinkReportData
	{
		// Members
		Real64 FLOW; // Mass flow rate [kg/s]
		Real64 FLOW2; // Mass flow rate [kg/s] for two way flow
		Real64 VolFLOW; // Mass flow rate [m^3/s]
		Real64 VolFLOW2; // Mass flow rate [m^3/s] for two way flow
		Real64 FLOWOFF; // Mass flow rate during OFF cycle [kg/s]
		Real64 FLOW2OFF; // Mass flow rate during OFF cycle [kg/s] for two way flow
		Real64 VolFLOWOFF; // Mass flow rate during OFF cycle [m^3/s]
		Real64 VolFLOW2OFF; // Mass flow rate during OFF cycle [m^3/s] for two way flow
		Real64 DP; // Average Pressure difference across a component
		Real64 DPON; // Pressure difference across a component with fan on
		Real64 DPOFF; // Pressure difference across a component with fan off

		// Default Constructor
		AirflowNetworkLinkReportData() :
			FLOW( 0.0 ),
			FLOW2( 0.0 ),
			VolFLOW( 0.0 ),
			VolFLOW2( 0.0 ),
			FLOWOFF( 0.0 ),
			FLOW2OFF( 0.0 ),
			VolFLOWOFF( 0.0 ),
			VolFLOW2OFF( 0.0 ),
			DP( 0.0 ),
			DPON( 0.0 ),
			DPOFF( 0.0 )
		{}

		// Member Constructor
		AirflowNetworkLinkReportData(
			Real64 const FLOW, // Mass flow rate [kg/s]
			Real64 const FLOW2, // Mass flow rate [kg/s] for two way flow
			Real64 const VolFLOW, // Mass flow rate [m^3/s]
			Real64 const VolFLOW2, // Mass flow rate [m^3/s] for two way flow
			Real64 const FLOWOFF, // Mass flow rate during OFF cycle [kg/s]
			Real64 const FLOW2OFF, // Mass flow rate during OFF cycle [kg/s] for two way flow
			Real64 const VolFLOWOFF, // Mass flow rate during OFF cycle [m^3/s]
			Real64 const VolFLOW2OFF, // Mass flow rate during OFF cycle [m^3/s] for two way flow
			Real64 const DP, // Average Pressure difference across a component
			Real64 const DPON, // Pressure difference across a component with fan on
			Real64 const DPOFF // Pressure difference across a component with fan off
		) :
			FLOW( FLOW ),
			FLOW2( FLOW2 ),
			VolFLOW( VolFLOW ),
			VolFLOW2( VolFLOW2 ),
			FLOWOFF( FLOWOFF ),
			FLOW2OFF( FLOW2OFF ),
			VolFLOWOFF( VolFLOWOFF ),
			VolFLOW2OFF( VolFLOW2OFF ),
			DP( DP ),
			DPON( DPON ),
			DPOFF( DPOFF )
		{}

	};

	struct AirflowNetworkNodeReportData // Node variable for simulation
	{
		// Members
		Real64 PZ; // Average Pressure [Pa]
		Real64 PZON; // Pressure with fan on [Pa]
		Real64 PZOFF; // Pressure with fan off [Pa]

		// Default Constructor
		AirflowNetworkNodeReportData() :
			PZ( 0.0 ),
			PZON( 0.0 ),
			PZOFF( 0.0 )
		{}

		// Member Constructor
		AirflowNetworkNodeReportData(
			Real64 const PZ, // Average Pressure [Pa]
			Real64 const PZON, // Pressure with fan on [Pa]
			Real64 const PZOFF // Pressure with fan off [Pa]
		) :
			PZ( PZ ),
			PZON( PZON ),
			PZOFF( PZOFF )
		{}

	};

	struct AirflowNetworkExchangeProp
	{
		// Members
		Real64 MultiZoneSen;
		Real64 MultiZoneLat;
		Real64 LeakSen;
		Real64 LeakLat;
		Real64 CondSen;
		Real64 DiffLat;
		Real64 TotalSen;
		Real64 TotalLat;
		Real64 SumMCp;
		Real64 SumMCpT;
		Real64 SumMHr;
		Real64 SumMHrW;
		Real64 SumMMCp;
		Real64 SumMMCpT;
		Real64 SumMMHr;
		Real64 SumMMHrW;
		Real64 SumMHrCO;
		Real64 SumMMHrCO;
		Real64 TotalCO2;
		Real64 SumMHrGC;
		Real64 SumMMHrGC;
		Real64 TotalGC;

		// Default Constructor
		AirflowNetworkExchangeProp() :
			MultiZoneSen( 0.0 ),
			MultiZoneLat( 0.0 ),
			LeakSen( 0.0 ),
			LeakLat( 0.0 ),
			CondSen( 0.0 ),
			DiffLat( 0.0 ),
			TotalSen( 0.0 ),
			TotalLat( 0.0 ),
			SumMCp( 0.0 ),
			SumMCpT( 0.0 ),
			SumMHr( 0.0 ),
			SumMHrW( 0.0 ),
			SumMMCp( 0.0 ),
			SumMMCpT( 0.0 ),
			SumMMHr( 0.0 ),
			SumMMHrW( 0.0 ),
			SumMHrCO( 0.0 ),
			SumMMHrCO( 0.0 ),
			TotalCO2( 0.0 ),
			SumMHrGC( 0.0 ),
			SumMMHrGC( 0.0 ),
			TotalGC( 0.0 )
		{}

		// Member Constructor
		AirflowNetworkExchangeProp(
			Real64 const MultiZoneSen,
			Real64 const MultiZoneLat,
			Real64 const LeakSen,
			Real64 const LeakLat,
			Real64 const CondSen,
			Real64 const DiffLat,
			Real64 const TotalSen,
			Real64 const TotalLat,
			Real64 const SumMCp,
			Real64 const SumMCpT,
			Real64 const SumMHr,
			Real64 const SumMHrW,
			Real64 const SumMMCp,
			Real64 const SumMMCpT,
			Real64 const SumMMHr,
			Real64 const SumMMHrW,
			Real64 const SumMHrCO,
			Real64 const SumMMHrCO,
			Real64 const TotalCO2,
			Real64 const SumMHrGC,
			Real64 const SumMMHrGC,
			Real64 const TotalGC
		) :
			MultiZoneSen( MultiZoneSen ),
			MultiZoneLat( MultiZoneLat ),
			LeakSen( LeakSen ),
			LeakLat( LeakLat ),
			CondSen( CondSen ),
			DiffLat( DiffLat ),
			TotalSen( TotalSen ),
			TotalLat( TotalLat ),
			SumMCp( SumMCp ),
			SumMCpT( SumMCpT ),
			SumMHr( SumMHr ),
			SumMHrW( SumMHrW ),
			SumMMCp( SumMMCp ),
			SumMMCpT( SumMMCpT ),
			SumMMHr( SumMMHr ),
			SumMMHrW( SumMMHrW ),
			SumMHrCO( SumMHrCO ),
			SumMMHrCO( SumMMHrCO ),
			TotalCO2( TotalCO2 ),
			SumMHrGC( SumMHrGC ),
			SumMMHrGC( SumMMHrGC ),
			TotalGC( TotalGC )
		{}

	};

	struct AiflowNetworkReportProp
	{
		// Members
		Real64 MultiZoneInfiSenGainW;
		Real64 MultiZoneInfiSenGainJ;
		Real64 MultiZoneInfiSenLossW;
		Real64 MultiZoneInfiSenLossJ;
		Real64 MultiZoneMixSenGainW;
		Real64 MultiZoneMixSenGainJ;
		Real64 MultiZoneMixSenLossW;
		Real64 MultiZoneMixSenLossJ;
		Real64 MultiZoneInfiLatGainW;
		Real64 MultiZoneInfiLatGainJ;
		Real64 MultiZoneInfiLatLossW;
		Real64 MultiZoneInfiLatLossJ;
		Real64 MultiZoneMixLatGainW;
		Real64 MultiZoneMixLatGainJ;
		Real64 MultiZoneMixLatLossW;
		Real64 MultiZoneMixLatLossJ;
		Real64 LeakSenGainW;
		Real64 LeakSenGainJ;
		Real64 LeakSenLossW;
		Real64 LeakSenLossJ;
		Real64 LeakLatGainW;
		Real64 LeakLatGainJ;
		Real64 LeakLatLossW;
		Real64 LeakLatLossJ;
		Real64 CondSenGainW;
		Real64 CondSenGainJ;
		Real64 CondSenLossW;
		Real64 CondSenLossJ;
		Real64 DiffLatGainW;
		Real64 DiffLatGainJ;
		Real64 DiffLatLossW;
		Real64 DiffLatLossJ;
		Real64 TotalSenGainW;
		Real64 TotalSenGainJ;
		Real64 TotalSenLossW;
		Real64 TotalSenLossJ;
		Real64 TotalLatGainW;
		Real64 TotalLatGainJ;
		Real64 TotalLatLossW;
		Real64 TotalLatLossJ;

		// Default Constructor
		AiflowNetworkReportProp() :
			MultiZoneInfiSenGainW( 0.0 ),
			MultiZoneInfiSenGainJ( 0.0 ),
			MultiZoneInfiSenLossW( 0.0 ),
			MultiZoneInfiSenLossJ( 0.0 ),
			MultiZoneMixSenGainW( 0.0 ),
			MultiZoneMixSenGainJ( 0.0 ),
			MultiZoneMixSenLossW( 0.0 ),
			MultiZoneMixSenLossJ( 0.0 ),
			MultiZoneInfiLatGainW( 0.0 ),
			MultiZoneInfiLatGainJ( 0.0 ),
			MultiZoneInfiLatLossW( 0.0 ),
			MultiZoneInfiLatLossJ( 0.0 ),
			MultiZoneMixLatGainW( 0.0 ),
			MultiZoneMixLatGainJ( 0.0 ),
			MultiZoneMixLatLossW( 0.0 ),
			MultiZoneMixLatLossJ( 0.0 ),
			LeakSenGainW( 0.0 ),
			LeakSenGainJ( 0.0 ),
			LeakSenLossW( 0.0 ),
			LeakSenLossJ( 0.0 ),
			LeakLatGainW( 0.0 ),
			LeakLatGainJ( 0.0 ),
			LeakLatLossW( 0.0 ),
			LeakLatLossJ( 0.0 ),
			CondSenGainW( 0.0 ),
			CondSenGainJ( 0.0 ),
			CondSenLossW( 0.0 ),
			CondSenLossJ( 0.0 ),
			DiffLatGainW( 0.0 ),
			DiffLatGainJ( 0.0 ),
			DiffLatLossW( 0.0 ),
			DiffLatLossJ( 0.0 ),
			TotalSenGainW( 0.0 ),
			TotalSenGainJ( 0.0 ),
			TotalSenLossW( 0.0 ),
			TotalSenLossJ( 0.0 ),
			TotalLatGainW( 0.0 ),
			TotalLatGainJ( 0.0 ),
			TotalLatLossW( 0.0 ),
			TotalLatLossJ( 0.0 )
		{}

		// Member Constructor
		AiflowNetworkReportProp(
			Real64 const MultiZoneInfiSenGainW,
			Real64 const MultiZoneInfiSenGainJ,
			Real64 const MultiZoneInfiSenLossW,
			Real64 const MultiZoneInfiSenLossJ,
			Real64 const MultiZoneMixSenGainW,
			Real64 const MultiZoneMixSenGainJ,
			Real64 const MultiZoneMixSenLossW,
			Real64 const MultiZoneMixSenLossJ,
			Real64 const MultiZoneInfiLatGainW,
			Real64 const MultiZoneInfiLatGainJ,
			Real64 const MultiZoneInfiLatLossW,
			Real64 const MultiZoneInfiLatLossJ,
			Real64 const MultiZoneMixLatGainW,
			Real64 const MultiZoneMixLatGainJ,
			Real64 const MultiZoneMixLatLossW,
			Real64 const MultiZoneMixLatLossJ,
			Real64 const LeakSenGainW,
			Real64 const LeakSenGainJ,
			Real64 const LeakSenLossW,
			Real64 const LeakSenLossJ,
			Real64 const LeakLatGainW,
			Real64 const LeakLatGainJ,
			Real64 const LeakLatLossW,
			Real64 const LeakLatLossJ,
			Real64 const CondSenGainW,
			Real64 const CondSenGainJ,
			Real64 const CondSenLossW,
			Real64 const CondSenLossJ,
			Real64 const DiffLatGainW,
			Real64 const DiffLatGainJ,
			Real64 const DiffLatLossW,
			Real64 const DiffLatLossJ,
			Real64 const TotalSenGainW,
			Real64 const TotalSenGainJ,
			Real64 const TotalSenLossW,
			Real64 const TotalSenLossJ,
			Real64 const TotalLatGainW,
			Real64 const TotalLatGainJ,
			Real64 const TotalLatLossW,
			Real64 const TotalLatLossJ
		) :
			MultiZoneInfiSenGainW( MultiZoneInfiSenGainW ),
			MultiZoneInfiSenGainJ( MultiZoneInfiSenGainJ ),
			MultiZoneInfiSenLossW( MultiZoneInfiSenLossW ),
			MultiZoneInfiSenLossJ( MultiZoneInfiSenLossJ ),
			MultiZoneMixSenGainW( MultiZoneMixSenGainW ),
			MultiZoneMixSenGainJ( MultiZoneMixSenGainJ ),
			MultiZoneMixSenLossW( MultiZoneMixSenLossW ),
			MultiZoneMixSenLossJ( MultiZoneMixSenLossJ ),
			MultiZoneInfiLatGainW( MultiZoneInfiLatGainW ),
			MultiZoneInfiLatGainJ( MultiZoneInfiLatGainJ ),
			MultiZoneInfiLatLossW( MultiZoneInfiLatLossW ),
			MultiZoneInfiLatLossJ( MultiZoneInfiLatLossJ ),
			MultiZoneMixLatGainW( MultiZoneMixLatGainW ),
			MultiZoneMixLatGainJ( MultiZoneMixLatGainJ ),
			MultiZoneMixLatLossW( MultiZoneMixLatLossW ),
			MultiZoneMixLatLossJ( MultiZoneMixLatLossJ ),
			LeakSenGainW( LeakSenGainW ),
			LeakSenGainJ( LeakSenGainJ ),
			LeakSenLossW( LeakSenLossW ),
			LeakSenLossJ( LeakSenLossJ ),
			LeakLatGainW( LeakLatGainW ),
			LeakLatGainJ( LeakLatGainJ ),
			LeakLatLossW( LeakLatLossW ),
			LeakLatLossJ( LeakLatLossJ ),
			CondSenGainW( CondSenGainW ),
			CondSenGainJ( CondSenGainJ ),
			CondSenLossW( CondSenLossW ),
			CondSenLossJ( CondSenLossJ ),
			DiffLatGainW( DiffLatGainW ),
			DiffLatGainJ( DiffLatGainJ ),
			DiffLatLossW( DiffLatLossW ),
			DiffLatLossJ( DiffLatLossJ ),
			TotalSenGainW( TotalSenGainW ),
			TotalSenGainJ( TotalSenGainJ ),
			TotalSenLossW( TotalSenLossW ),
			TotalSenLossJ( TotalSenLossJ ),
			TotalLatGainW( TotalLatGainW ),
			TotalLatGainJ( TotalLatGainJ ),
			TotalLatLossW( TotalLatLossW ),
			TotalLatLossJ( TotalLatLossJ )
		{}

	};

	// Object Data
	extern Array1D< AirflowNetworkNodeSimuData > AirflowNetworkNodeSimu;
	extern Array1D< AirflowNetworkLinkSimuData > AirflowNetworkLinkSimu;
	extern Array1D< AirflowNetworkExchangeProp > AirflowNetworkExchangeData;
	extern Array1D< AirflowNetworkExchangeProp > AirflowNetworkMultiExchangeData;
	extern Array1D< AirflowNetworkLinkReportData > AirflowNetworkLinkReport;
	extern Array1D< AirflowNetworkNodeReportData > AirflowNetworkNodeReport;
	extern Array1D< AirflowNetworkLinkReportData > AirflowNetworkLinkReport1;
	extern AirflowNetworkSimuProp AirflowNetworkSimu; // unique object name | AirflowNetwork control | Wind pressure coefficient input control | Integer equivalent for WPCCntr field | CP Array name at WPCCntr = "INPUT" | Building type | Height Selection | Maximum number of iteration | Initialization flag | Relative airflow convergence | Absolute airflow convergence | Convergence acceleration limit | Maximum pressure change in an element [Pa] | Azimuth Angle of Long Axis of Building | Ratio of Building Width Along Short Axis to Width Along Long Axis | Number of wind directions | Minimum pressure difference | Exterior large opening error count during HVAC system operation | Exterior large opening error index during HVAC system operation | Large opening error count at Open factor > 1.0 | Large opening error error index at Open factor > 1.0 | Initialization flag type
	extern Array1D< AirflowNetworkNodeProp > AirflowNetworkNodeData;
	extern Array1D< AirflowNetworkCompProp > AirflowNetworkCompData;
	extern Array1D< AirflowNetworkLinkageProp > AirflowNetworkLinkageData;
	extern Array1D< MultizoneZoneProp > MultizoneZoneData;
	extern Array1D< MultizoneSurfaceProp > MultizoneSurfaceData;
	extern Array1D< MultizoneCompDetOpeningProp > MultizoneCompDetOpeningData;
	extern Array1D< MultizoneCompSimpleOpeningProp > MultizoneCompSimpleOpeningData;
	extern Array1D< MultizoneCompHorOpeningProp > MultizoneCompHorOpeningData;
	extern Array1D< MultizoneSurfaceCrackStdCndns > MultizoneSurfaceStdConditionsCrackData;
	extern Array1D< MultizoneSurfaceCrackProp > MultizoneSurfaceCrackData;
	extern Array1D< MultizoneSurfaceELAProp > MultizoneSurfaceELAData;
	extern Array1D< MultizoneExternalNodeProp > MultizoneExternalNodeData;
	extern Array1D< MultizoneCPArrayProp > MultizoneCPArrayData;
	extern Array1D< MultizoneCPArrayProp > MultizoneCPArrayDataSingleSided;
	extern Array1D< MultizoneCPValueProp > MultizoneCPValueData;
	extern Array1D< MultizoneCPValueProp > MultizoneCPValueDataTemp; // temporary CP values
	extern Array1D< MultizoneCPValueProp > MultizoneCPValueDataTempUnMod; // temporary CPValues, without modifcation factor
	extern Array1D< DeltaCpProp > DeltaCp;
	extern Array1D< DeltaCpProp > EPDeltaCP;
	extern Array1D< MultizoneCompExhaustFanProp > MultizoneCompExhaustFanData;
	extern Array1D< DisSysNodeProp > DisSysNodeData;
	extern Array1D< DisSysCompLeakProp > DisSysCompLeakData;
	extern Array1D< DisSysCompELRProp > DisSysCompELRData;
	extern Array1D< DisSysCompDuctProp > DisSysCompDuctData;
	extern Array1D< DisSysCompDamperProp > DisSysCompDamperData;
	extern Array1D< DisSysCompCVFProp > DisSysCompCVFData;
	extern Array1D< DisSysCompDetFanProp > DisSysCompDetFanData;
	extern Array1D< DisSysCompCoilProp > DisSysCompCoilData;
	extern Array1D< DisSysCompHXProp > DisSysCompHXData;
	extern Array1D< DisSysCompTermUnitProp > DisSysCompTermUnitData;
	extern Array1D< DisSysCompCPDProp > DisSysCompCPDData;
	extern Array1D< AiflowNetworkReportProp > AirflowNetworkReportData;

} // DataAirflowNetwork

} // EnergyPlus

#endif
