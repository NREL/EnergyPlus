// EnergyPlus Headers
#include <DataAirflowNetwork.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataAirflowNetwork {

	// MODULE INFORMATION:
	//       AUTHOR         Lixing Gu, Don Shirey, and Muthusamy V. Swami
	//       DATE WRITTEN   Aug. 2003
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module should contain the information that is needed to simulate
	// performance of air distribution system, including pressure, temperature
	// and moisture levels at each node, and airflow and sensible and latent energy losses
	// at each element

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	int const CompTypeNum_DOP( 1 ); // Detailed large opening component
	int const CompTypeNum_SOP( 2 ); // Simple opening component
	int const CompTypeNum_SCR( 3 ); // Surface crack component
	int const CompTypeNum_SEL( 4 ); // Surface effective leakage ratio component
	int const CompTypeNum_PLR( 5 ); // Distribution system crack component
	int const CompTypeNum_DWC( 6 ); // Distribution system duct component
	int const CompTypeNum_CVF( 7 ); // Distribution system constant volume fan component
	int const CompTypeNum_FAN( 8 ); // Distribution system detailed fan component
	int const CompTypeNum_MRR( 9 ); // Distribution system multiple curve fit power law resistant flow component
	int const CompTypeNum_DMP( 10 ); // Distribution system damper component
	int const CompTypeNum_ELR( 11 ); // Distribution system effective leakage ratio component
	int const CompTypeNum_CPD( 12 ); // Distribution system constant pressure drop component
	int const CompTypeNum_COI( 13 ); // Distribution system coil component
	int const CompTypeNum_TMU( 14 ); // Distribution system terminal unit component
	int const CompTypeNum_EXF( 15 ); // Zone exhaust fan
	int const CompTypeNum_HEX( 16 ); // Distribution system heat exchanger
	int const CompTypeNum_HOP( 17 ); // Horizontal opening component
	int const CompTypeNum_RVD( 18 ); // Reheat VAV terminal damper

	// EPlus component Type
	int const EPlusTypeNum_SCN( 1 ); // Supply connection
	int const EPlusTypeNum_RCN( 2 ); // Return connection
	int const EPlusTypeNum_RHT( 3 ); // Reheat terminal
	int const EPlusTypeNum_FAN( 4 ); // Fan
	int const EPlusTypeNum_COI( 5 ); // Heating or cooling coil
	int const EPlusTypeNum_HEX( 6 ); // Heat ecxchanger
	int const EPlusTypeNum_RVD( 7 ); // Reheat VAV terminal damper

	// EPlus node type
	int const EPlusTypeNum_ZIN( 1 ); // Zone inlet node
	int const EPlusTypeNum_ZOU( 2 ); // Zone outlet node
	int const EPlusTypeNum_SPL( 3 ); // Splitter node
	int const EPlusTypeNum_MIX( 4 ); // Mixer node
	int const EPlusTypeNum_OAN( 5 ); // Outside air system node
	int const EPlusTypeNum_EXT( 6 ); // OA system inlet node
	int const EPlusTypeNum_FIN( 7 ); // Fan Inlet node
	int const EPlusTypeNum_FOU( 8 ); // Fan Outlet Node
	int const EPlusTypeNum_COU( 9 ); // Coil Outlet Node
	int const EPlusTypeNum_HXO( 10 ); // Heat exchanger Outlet Node
	int const EPlusTypeNum_DIN( 11 ); // Damper Inlet node
	int const EPlusTypeNum_DOU( 12 ); // Damper Outlet Node
	int const EPlusTypeNum_SPI( 13 ); // Splitter inlet Node
	int const EPlusTypeNum_SPO( 14 ); // Splitter Outlet Node

	int const iWPCCntr_Input( 1 );
	int const iWPCCntr_SurfAvg( 2 );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	// Node simulation variable in air distribution system
	// Link simulation variable in air distribution system
	// Sensible and latent exchange variable in air distribution system

	int SimulateAirflowNetwork( 1 );
	// Vent Control  DistSys Control  Flag    Description
	//  NONE           NONE           0      No AirflowNetwork and SIMPLE
	//  SIMPLE         NONE           1      Simple calculations only
	//  MULTIZONE      NONE           2      Perform multizone calculations only
	//  NONE           DISTSYS        3      Perform distribution system durin system on time only
	//  SIMPLE         DISTSYS        4      Perform distribution system durin system on time and simple calculations during off time
	//  MULTIZONE      DISTSYS        5      Perform distribution system durin system on time and multizone calculations during off time

	int const AirflowNetworkControlSimple( 1 ); // Simple calculations only
	int const AirflowNetworkControlMultizone( 2 ); // Perform multizone calculations only
	int const AirflowNetworkControlSimpleADS( 4 ); // Perform distribution system durin system
	// on time and simple calculations during off time
	int const AirflowNetworkControlMultiADS( 5 ); // Perform distribution system durin system on time
	// and multizone calculations during off time

	Array1D_bool AirflowNetworkZoneFlag;

	int NumOfNodesMultiZone( 0 ); // Number of nodes for multizone calculation
	int NumOfNodesDistribution( 0 ); // Number of nodes for distribution system calculation
	int NumOfLinksMultiZone( 0 ); // Number of links for multizone calculation
	int NumOfLinksDistribution( 0 ); // Number of links for distribution system calculation

	int AirflowNetworkNumOfNodes( 0 ); // Number of nodes for AirflowNetwork calculation
	// = NumOfNodesMultiZone+NumOfNodesDistribution
	int AirflowNetworkNumOfComps( 0 ); // Number of components for AirflowNetwork calculation
	int AirflowNetworkNumOfLinks( 0 ); // Number of links for AirflowNetwork calculation
	// = NumOfLinksMultiZone+NumOfLinksDistribution
	// RoomAirManager use
	int AirflowNetworkNumOfSurfaces( 0 ); // The number of surfaces for multizone calculation
	int AirflowNetworkNumOfZones( 0 ); // The number of zones for multizone calculation

	bool RollBackFlag( false ); // Roll back flag when system time steo down shifting
	Array1D< Real64 > ANZT; // Local zone air temperature for roll back use
	Array1D< Real64 > ANZW; // Local zone air humidity ratio for roll back use
	Array1D< Real64 > ANCO; // Local zone air CO2 for roll back use
	Array1D< Real64 > ANGC; // Local zone air generic contaminant for roll back use
	int AirflowNetworkNumOfExhFan( 0 ); // Number of zone exhaust fans
	Array1D_bool AirflowNetworkZoneExhaustFan; // Logical to use zone exhaust fans
	bool AirflowNetworkFanActivated( false ); // Supply fan activation flag
	bool AirflowNetworkUnitarySystem( false ); // set to TRUE for unitary systems (to make answers equal, will remove eventually)
	// Multispeed HP only
	int MultiSpeedHPIndicator( 0 ); // Indicator for multispeed heat pump use
	// Addiitonal airflow needed for an VAV fan to compensate the leakage losses and supply pathway pressure losses [kg/s]
	Real64 VAVTerminalRatio( 0.0 ); // The terminal flow ratio when a supply VAV fan reach its max flow rate
	bool VAVSystem( false ); // This flag is used to represent a VAV system

	//     NOTICE
	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

	// Object Data
	Array1D< AirflowNetworkNodeSimuData > AirflowNetworkNodeSimu;
	Array1D< AirflowNetworkLinkSimuData > AirflowNetworkLinkSimu;
	Array1D< AirflowNetworkExchangeProp > AirflowNetworkExchangeData;
	Array1D< AirflowNetworkExchangeProp > AirflowNetworkMultiExchangeData;
	Array1D< AirflowNetworkLinkReportData > AirflowNetworkLinkReport;
	Array1D< AirflowNetworkNodeReportData > AirflowNetworkNodeReport;
	Array1D< AirflowNetworkLinkReportData > AirflowNetworkLinkReport1;
	AirflowNetworkSimuProp AirflowNetworkSimu( "", "NoMultizoneOrDistribution", "Input", 0, "", "", "", 500, 0, 1.0e-5, 1.0e-5, -0.5, 500.0, 0.0, 1.0, 0, 1.0e-4, 0, 0, 0, 0, "ZeroNodePressures", false ); // unique object name | AirflowNetwork control | Wind pressure coefficient input control | Integer equivalent for WPCCntr field | CP Array name at WPCCntr = "INPUT" | Building type | Height Selection | Maximum number of iteration | Initialization flag | Relative airflow convergence | Absolute airflow convergence | Convergence acceleration limit | Maximum pressure change in an element [Pa] | Azimuth Angle of Long Axis of Building | Ratio of Building Width Along Short Axis to Width Along Long Axis | Number of wind directions | Minimum pressure difference | Exterior large opening error count during HVAC system operation | Exterior large opening error index during HVAC system operation | Large opening error count at Open factor > 1.0 | Large opening error error index at Open factor > 1.0 | Initialization flag type
	Array1D< AirflowNetworkNodeProp > AirflowNetworkNodeData;
	Array1D< AirflowNetworkCompProp > AirflowNetworkCompData;
	Array1D< AirflowNetworkLinkageProp > AirflowNetworkLinkageData;
	Array1D< MultizoneZoneProp > MultizoneZoneData;
	Array1D< MultizoneSurfaceProp > MultizoneSurfaceData;
	Array1D< MultizoneCompDetOpeningProp > MultizoneCompDetOpeningData;
	Array1D< MultizoneCompSimpleOpeningProp > MultizoneCompSimpleOpeningData;
	Array1D< MultizoneCompHorOpeningProp > MultizoneCompHorOpeningData;
	Array1D< MultizoneSurfaceCrackStdCndns > MultizoneSurfaceStdConditionsCrackData;
	Array1D< MultizoneSurfaceCrackProp > MultizoneSurfaceCrackData;
	Array1D< MultizoneSurfaceELAProp > MultizoneSurfaceELAData;
	Array1D< MultizoneExternalNodeProp > MultizoneExternalNodeData;
	Array1D< MultizoneCPArrayProp > MultizoneCPArrayData;
	Array1D< MultizoneCPArrayProp > MultizoneCPArrayDataSingleSided;
	Array1D< MultizoneCPValueProp > MultizoneCPValueData;
	Array1D< MultizoneCPValueProp > MultizoneCPValueDataTemp; // temporary CP values
	Array1D< MultizoneCPValueProp > MultizoneCPValueDataTempUnMod; // temporary CPValues, without modifcation factor
	Array1D< DeltaCpProp > DeltaCp;
	Array1D< DeltaCpProp > EPDeltaCP;
	Array1D< MultizoneCompExhaustFanProp > MultizoneCompExhaustFanData;
	Array1D< DisSysNodeProp > DisSysNodeData;
	Array1D< DisSysCompLeakProp > DisSysCompLeakData;
	Array1D< DisSysCompELRProp > DisSysCompELRData;
	Array1D< DisSysCompDuctProp > DisSysCompDuctData;
	Array1D< DisSysCompDamperProp > DisSysCompDamperData;
	Array1D< DisSysCompCVFProp > DisSysCompCVFData;
	Array1D< DisSysCompDetFanProp > DisSysCompDetFanData;
	Array1D< DisSysCompCoilProp > DisSysCompCoilData;
	Array1D< DisSysCompHXProp > DisSysCompHXData;
	Array1D< DisSysCompTermUnitProp > DisSysCompTermUnitData;
	Array1D< DisSysCompCPDProp > DisSysCompCPDData;
	Array1D< AiflowNetworkReportProp > AirflowNetworkReportData;

} // DataAirflowNetwork

} // EnergyPlus
