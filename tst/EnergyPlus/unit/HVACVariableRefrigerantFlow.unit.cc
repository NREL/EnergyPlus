// EnergyPlus::HVACVariableRefrigerantFlow unit tests

// Google test headers
#include <gtest/gtest.h>

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include "Fixtures/HVACFixture.hh"
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataSizing.hh>
#include <DataZoneEquipment.hh>
#include <DataZoneEnergyDemands.hh>
#include <DXCoils.hh>
#include <HeatBalanceManager.hh>
#include <HVACVariableRefrigerantFlow.hh>
#include <OutputReportPredefined.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>

using namespace EnergyPlus;
using namespace DXCoils;
using namespace EnergyPlus::CurveManager;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::HVACVariableRefrigerantFlow;
using namespace EnergyPlus::OutputReportPredefined;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;

namespace EnergyPlus {

TEST( HVACVariableRefrigerantFlow, VRF_FluidTCtrl_CompResidual )
{
    // PURPOSE OF THIS SUBROUTINE:
	//  Calculates residual function ((VRV terminal unit cooling output - Zone sensible cooling load)
    	
    using namespace CurveManager;
    
    int CurveNum = 1;
    int NumPar;
    double Te = -2.796; // Outdoor unit evaporating temperature
    double Tdis = 40.093; 
    double CondHeat = 1864.44;
    Array1D< Real64 > Par;
    
    // Allocate
    NumCurves = 1; //CurveManager::NumCurves
    PerfCurve.allocate( NumCurves );
    
    NumPar = 3;
    Par.allocate( NumPar );
    	
    // Inputs: curve parameters
    Par( 1 ) = Tdis;
    Par( 2 ) = CondHeat;
    Par( 3 ) = CurveNum;
    	
    // Inputs: parameters
    PerfCurve( CurveNum ).CurveType = CurveManager::BiQuadratic;
    PerfCurve( CurveNum ).ObjectType = CurveType_BiQuadratic;
    PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
    PerfCurve( CurveNum ).Coeff1 = 724.71125 ; // Coefficient1 Constant
    PerfCurve( CurveNum ).Coeff2 = -21.867868; // Coefficient2 x
    PerfCurve( CurveNum ).Coeff3 = 0.52480042; // Coefficient3 x**2
    PerfCurve( CurveNum ).Coeff4 = -17.043566; // Coefficient4 y
    PerfCurve( CurveNum ).Coeff5 = -.40346383; // Coefficient5 y**2
    PerfCurve( CurveNum ).Coeff6 = 0.29573589; // Coefficient6 x*y
    PerfCurve( CurveNum ).Var1Min= 15 ; // Minimum Value of x
    PerfCurve( CurveNum ).Var1Max= 65 ; // Maximum Value of x
    PerfCurve( CurveNum ).Var2Min= -30; // Minimum Value of y
    PerfCurve( CurveNum ).Var2Max= 15 ; // Maximum Value of y
    
    // Run and Check
    double CompResidual = HVACVariableRefrigerantFlow::CompResidual( Te, Par );
    EXPECT_NEAR( 1.652, CompResidual, 0.005 );
    
    // Clean up
    PerfCurve.deallocate( );
    Par.deallocate( ); 
    
}

TEST_F( EnergyPlusFixture, VRFFluidTCtrlGetCoilInput )
{
    // PURPOSE OF THE TEST:
    //   IDF Read in for the new coil type: Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl
    
    std::string const idf_objects = delimited_string({
       " Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl,  ",
       " 	 TU1 VRF DX Cooling Coil, !- Name							   ",
       " 	 VRFAvailSched,           !- Availability Schedule Name		   ",
       " 	 TU1 VRF DX CCoil Inlet Node,  !- Coil Air Inlet Node		   ",
       " 	 TU1 VRF DX CCoil Outlet Node, !- Coil Air Outlet Node		   ",
       " 	 2200,                    !- Rated Total Cooling Capacity {W}   ",
       " 	 0.865,                   !- Rated Sensible Heat Ratio		   ",
       " 	 3,                       !- Indoor Unit Reference Superheating ",
       " 	 IUEvapTempCurve,         !- Indoor Unit Evaporating Temperature",
       " 	 ;                        !- Name of Water Storage Tank for Cond",
       " Curve:Quadratic,												   ",
       "     IUEvapTempCurve,         !- Name							   ",
       "     0,                       !- Coefficient1 Const				   ",
       "     0.80404,                 !- Coefficient2 x					   ",
       "     0,                       !- Coefficient3 x**2				   ",
       "     0,                       !- Minimum Value of x				   ",
       "     15,                      !- Maximum Value of x				   ",
       "     ,                        !- Minimum Curve Outp				   ",
       "     ,                        !- Maximum Curve Outp				   ",
       "     Dimensionless,           !- Input Unit Type fo				   ",
       "     Dimensionless;           !- Output Unit Type				   "
    });
    
    ASSERT_FALSE( process_idf( idf_objects ) );
    
    // Run the method
    GetDXCoils();
    
    // Check the results
    ASSERT_EQ( 1, NumDXCoils);
    EXPECT_EQ( DXCoil( 1 ).DXCoilType_Num, 32 );
    EXPECT_EQ( DXCoil( 1 ).RatedTotCap( 1 ), 2200 );
    EXPECT_EQ( DXCoil( 1 ).RatedSHR( 1 ), 0.865 );
    EXPECT_EQ( DXCoil( 1 ).C1Te, 0 );
    EXPECT_EQ( DXCoil( 1 ).C2Te, 0.80404 );
    EXPECT_EQ( DXCoil( 1 ).C3Te, 0 );
    EXPECT_EQ( DXCoil( 1 ).SH, 3 );
    
}

TEST( HVACVariableRefrigerantFlow, VRF_FluidTCtrl_FanSpdResidualCool )
{
    // PURPOSE OF THIS TEST:
    //   Test the method FanSpdResidualCool.
    	
    using namespace DXCoils;
    
    int NumPar;
    double FanSpdRto;
    double ZnSenLoad;
    double Th2; 
    double TairInlet;
    double QfanRate;
    double Garate; 
    double BF; 
    Array1D< Real64 > Par;
    
    // Allocate
    NumPar = 6;
    Par.allocate( 6 );
    	
    // Inputs: 
    FanSpdRto = 0.5;
    ZnSenLoad = 2716.62;
    Th2 = 17.41212; 
    TairInlet = 25.55534; 
    QfanRate = 37.8;
    Garate = 0.20664; 
    BF = 0.0592; 
    Par( 1 ) = ZnSenLoad;
    Par( 2 ) = Th2; 
    Par( 3 ) = TairInlet;  
    Par( 4 ) = QfanRate;
    Par( 5 ) = Garate; 
    Par( 6 ) = BF; 
    
    // Run and Check
    double FanSpdResidual = FanSpdResidualCool( FanSpdRto, Par );
    EXPECT_NEAR( -0.7055, FanSpdResidual, 0.0005 );
    
    // Clean up
    Par.deallocate( ); 
	
}

TEST( HVACVariableRefrigerantFlow, VRF_FluidTCtrl_FanSpdResidualHeat )
{
    // PURPOSE OF THIS TEST:
    //   Test the method FanSpdResidualHeat.
    	
    using namespace DXCoils;
    
    int NumPar;
    double FanSpdRto;
    double ZnSenLoad;
    double Th2; 
    double TairInlet;
    double QfanRate;
    double Garate; 
    double BF; 
    Array1D< Real64 > Par;
    
    // Allocate
    NumPar = 6;
    Par.allocate( 6 );
    	
    // Inputs: 
    FanSpdRto = 0.5;
    ZnSenLoad = 4241.66;
    Th2 = 41.221; 
    TairInlet = 20.236; 
    QfanRate = 37.8;
    Garate = 0.21136; 
    BF = 0.1360; 
    Par( 1 ) = ZnSenLoad;
    Par( 2 ) = Th2; 
    Par( 3 ) = TairInlet;  
    Par( 4 ) = QfanRate;
    Par( 5 ) = Garate; 
    Par( 6 ) = BF; 
    
    // Run and Check
    double FanSpdResidual = FanSpdResidualHeat( FanSpdRto, Par );
    EXPECT_NEAR( -0.5459, FanSpdResidual, 0.0005 );
    
    // Clean up
    Par.deallocate( ); 
    
}

TEST( HVACVariableRefrigerantFlow, VRF_FluidTCtrl_CalcVRFIUAirFlow )
{
    // PURPOSE OF THIS TEST:
    //   Test the method CalcVRFIUAirFlow, which analyzes the VRF Indoor Unit operations given zonal loads.
    //   Calculated parameters includie: (1) Fan Speed Ratio, (2) SH/SC Degrees, and (3) Coil Inlet/Outlet conditions 
    	
    using namespace DXCoils;
    using namespace DataZoneEnergyDemands;
    using namespace EnergyPlus::Psychrometrics;
    using DataEnvironment::OutBaroPress;
    
    int ZoneIndex;  // index to zone where the VRF Terminal Unit resides 
    int CoolCoilIndex;  // index to VRFTU cooling coil 
    int HeatCoilIndex;  // index to VRFTU heating coil
    int Mode;       // mode 0 for cooling, 1 for heating, 2 for neither cooling nor heating
    bool SHSCModify = true; // indicate whether SH/SC is modified
    Real64 Temp;    // evaporating or condensing temperature
    Real64 FanSpdRatio; // fan speed ratio
    Real64 Wout;    // outlet air humidity ratio
    Real64 Toutlet; // outlet air temperature
    Real64 Houtlet; // outlet air enthalpy
    Real64 HcoilIn; // inlet air enthalpy
    Real64 TcIn;    // coil inlet temperature, after fan
    Real64 SHact;   // actual SH
    Real64 SCact;   // actual SC
    
    // Allocate
    int NumCoils = 2;
    DXCoil.allocate( NumCoils );
    int NumZones = 2;
    ZoneSysEnergyDemand.allocate( NumZones );
    
    // Common Inputs
    CoolCoilIndex = 1;  
    HeatCoilIndex = 2;  
    FanSpdRatio = 0;
    Wout = 1;    	
    OutBaroPress = 101570;
    InitializePsychRoutines();
    
    DXCoil( CoolCoilIndex ).C1Te = 0;
    DXCoil( CoolCoilIndex ).C2Te = 0.804;
    DXCoil( CoolCoilIndex ).C3Te = 0;
    DXCoil( CoolCoilIndex ).SH	 = 3.00;
    DXCoil( CoolCoilIndex ).SupplyFanIndex = 0;
    DXCoil( HeatCoilIndex ).C1Tc = -1.905;
    DXCoil( HeatCoilIndex ).C2Tc = 0.4333;
    DXCoil( HeatCoilIndex ).C3Tc = 0.0207;
    DXCoil( HeatCoilIndex ).SC	 = 5.00;
    DXCoil( HeatCoilIndex ).SupplyFanIndex = 0;
    
    // Run and Check for Cooling Mode
    Mode = 0;
    Temp = 15;   	
    ZoneIndex = 1; 
    
    ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToCoolingSP = -2716.6229;
    ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToHeatingSP = -45507.8487;
    
    DXCoil( CoolCoilIndex ).RatedAirMassFlowRate( 1 ) = 0.2066;
    DXCoil( CoolCoilIndex ).InletAirTemp = 25.5553;
    DXCoil( CoolCoilIndex ).InletAirHumRat = 8.4682e-3;
    DXCoil( CoolCoilIndex ).InletAirEnthalpy = 47259.78;
    
    CalcVRFIUAirFlow(ZoneIndex, Mode, Temp, CoolCoilIndex, HeatCoilIndex, SHSCModify, FanSpdRatio, Wout, Toutlet, Houtlet, HcoilIn, TcIn, SHact, SCact);
    EXPECT_NEAR( TcIn, 25.56, 0.01 );
    EXPECT_NEAR( Toutlet, 17.89, 0.01 );
    EXPECT_NEAR( Houtlet, 39440, 1 );
    EXPECT_NEAR( HcoilIn, 47259, 1 );
    EXPECT_NEAR( SHact, 3.00, 0.01 );
    
    
    // Run and Check for Heating Mode
    Mode = 1;
    Temp = 42;   	
    ZoneIndex = 2; 
    
    ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToCoolingSP = 43167.2628;
    ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToHeatingSP = 4241.66099;
    
    DXCoil( HeatCoilIndex ).RatedAirMassFlowRate( 1 ) = 0.21136;
    DXCoil( HeatCoilIndex ).InletAirTemp = 20.2362;
    DXCoil( HeatCoilIndex ).InletAirHumRat = 4.1053e-3;
    DXCoil( HeatCoilIndex ).InletAirEnthalpy = 30755.6253;
    
    CalcVRFIUAirFlow(ZoneIndex, Mode, Temp, CoolCoilIndex, HeatCoilIndex, SHSCModify, FanSpdRatio, Wout, Toutlet, Houtlet, HcoilIn, TcIn, SHact, SCact);
    EXPECT_NEAR( TcIn, 20.24, 0.01 );
    EXPECT_NEAR( Toutlet, 38.37, 0.01 );
    EXPECT_NEAR( Houtlet, 49113, 1 );
    EXPECT_NEAR( HcoilIn, 30756, 1 );
    EXPECT_NEAR( SCact, 5.00, 0.01 );
    
    // Clean up
    DXCoil.deallocate( ); 
    ZoneSysEnergyDemand.deallocate( ); 
}

TEST( HVACVariableRefrigerantFlow, VRF_FluidTCtrl_CalcVRFIUTeTc )
{
    // PURPOSE OF THIS TEST:
    //   Test the method CalcVRFIUTeTc_FluidTCtrl, which determines the VRF evaporating temperature at 
    //   cooling mode and the condensing temperature at heating mode.
    	
    using namespace HVACVariableRefrigerantFlow;
    
    // Allocate
    int NumVRFCondenser = 1;
    VRF.allocate( NumVRFCondenser );
    int NumTUList = 1;
    TerminalUnitList.allocate( NumTUList );
    
    // Common Inputs
    int IndexVRFCondenser = 1;
    int IndexTUList = 1;
    
    TerminalUnitList( IndexTUList ).NumTUInList = 2;
    
    VRF( IndexVRFCondenser ).ZoneTUListPtr = 1;
    VRF( IndexVRFCondenser ).AlgorithmIUCtrl = 0;
    VRF( IndexVRFCondenser ).EvapTempFixed = 3;
    VRF( IndexVRFCondenser ).CondTempFixed = 5;
    
    // Run and Check 
    CalcVRFIUTeTc_FluidTCtrl( IndexVRFCondenser );
    
    EXPECT_EQ( VRF( IndexVRFCondenser ).IUEvaporatingTemp, 3 );
    EXPECT_EQ( VRF( IndexVRFCondenser ).IUCondensingTemp, 5 );
    
    // Clean up
    VRF.deallocate( ); 
    TerminalUnitList.deallocate( ); 
}

TEST_F( HVACFixture, VRFTest ) {
		
		bool ErrorsFound( false );        // function returns true on error
		bool FirstHVACIteration( true );  // simulate the first pass through HVAC simulation, use false for next iteration
		int VRFCond( 1 );                 // index to VRF condenser
		int VRFTUNum( 1 );                // index to VRF terminal unit
		int EquipPtr( 1 );                // index to equipment list
		int CurZoneNum( 1 );              // index to zone
		int ZoneInletAirNode( 0 );        // zone inlet node number
		Real64 DefrostWatts( 0.0 );       // calculation of VRF defrost power [W]
		Real64 SysOutputProvided( 0.0 );  // function returns sensible capacity [W]
		Real64 LatOutputProvided( 0.0 );  // function returns latent capacity [W]
		
		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			" ",
			"AirConditioner:VariableRefrigerantFlow,",
			"  VRF Heat Pump,           !- Heat Pump Name",
			"  VRFCondAvailSched,       !- Availability Schedule Name",
			"  autosize,                !- Gross Rated Total Cooling Capacity {W}",
			"  3.2917,                  !- Gross Rated Cooling COP {W/W}",
			"  -5,                      !- Minimum Outdoor Temperature in Cooling Mode {C}",
			"  43,                      !- Maximum Outdoor Temperature in Cooling Mode {C}",
			"  VRFCoolCapFT,            !- Cooling Capacity Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFCoolCapFTBoundary,    !- Cooling Capacity Ratio Boundary Curve Name",
			"  VRFCoolCapFTHi,          !- Cooling Capacity Ratio Modifier Function of High Temperature Curve Name",
			"  VRFCoolEIRFT,            !- Cooling Energy Input Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFCoolEIRFTBoundary,    !- Cooling Energy Input Ratio Boundary Curve Name",
			"  VRFCoolEIRFTHi,          !- Cooling Energy Input Ratio Modifier Function of High Temperature Curve Name",
			"  CoolingEIRLowPLR,        !- Cooling Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
			"  CoolingEIRHiPLR,         !- Cooling Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
			"  CoolingCombRatio,        !- Cooling Combination Ratio Correction Factor Curve Name",
			"  VRFCPLFFPLR,             !- Cooling Part-Load Fraction Correlation Curve Name",
			"  autosize,                !- Gross Rated Heating Capacity {W}",
			"  ,                        !- Rated Heating Capacity Sizing Ratio {W/W}",
			"  3.5484,                  !- Gross Rated Heating COP {W/W}",
			"  -20,                     !- Minimum Outdoor Temperature in Heating Mode {C}",
			"  20,                      !- Maximum Outdoor Temperature in Heating Mode {C}",
			"  VRFHeatCapFT,            !- Heating Capacity Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFHeatCapFTBoundary,    !- Heating Capacity Ratio Boundary Curve Name",
			"  VRFHeatCapFTHi,          !- Heating Capacity Ratio Modifier Function of High Temperature Curve Name",
			"  VRFHeatEIRFT,            !- Heating Energy Input Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFHeatEIRFTBoundary,    !- Heating Energy Input Ratio Boundary Curve Name",
			"  VRFHeatEIRFTHi,          !- Heating Energy Input Ratio Modifier Function of High Temperature Curve Name",
			"  WetBulbTemperature,      !- Heating Performance Curve Outdoor Temperature Type",
			"  HeatingEIRLowPLR,        !- Heating Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
			"  HeatingEIRHiPLR,         !- Heating Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
			"  HeatingCombRatio,        !- Heating Combination Ratio Correction Factor Curve Name",
			"  VRFCPLFFPLR,             !- Heating Part-Load Fraction Correlation Curve Name",
			"  0.25,                    !- Minimum Heat Pump Part-Load Ratio {dimensionless}",
			"  SPACE1-1,                !- Zone Name for Master Thermostat Location",
			"  LoadPriority,            !- Master Thermostat Priority Control Type",
			"  ,                        !- Thermostat Priority Schedule Name",
			"  VRF Heat Pump TU List,   !- Zone Terminal Unit List Name",
			"  No,                      !- Heat Pump Waste Heat Recovery",
			"  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Cooling Mode {m}",
			"  10,                      !- Vertical Height used for Piping Correction Factor {m}",
			"  CoolingLengthCorrectionFactor,  !- Piping Correction Factor for Length in Cooling Mode Curve Name",
			"  -0.000386,               !- Piping Correction Factor for Height in Cooling Mode Coefficient {1/m}",
			"  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Heating Mode {m}",
			"  ,                        !- Piping Correction Factor for Length in Heating Mode Curve Name",
			"  ,                        !- Piping Correction Factor for Height in Heating Mode Coefficient {1/m}",
			"  15,                      !- Crankcase Heater Power per Compressor {W}",
			"  3,                       !- Number of Compressors {dimensionless}",
			"  0.33,                    !- Ratio of Compressor Size to Total Compressor Capacity {W/W}",
			"  7,                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater {C}",
			"  ReverseCycle,            !- Defrost Strategy",
			"  Timed,                   !- Defrost Control",
			"  DefrostEIRSched,         !- Defrost Energy Input Ratio Modifier Function of Temperature Curve Name",
			"  ,                        !- Defrost Time Period Fraction {dimensionless}",
			"  autosize,                !- Resistive Defrost Heater Capacity {W}",
			"  7,                       !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation {C}",
			"  AirCooled,               !- Condenser Type",
			"  MyVRFOANode,             !- Condenser Inlet Node Name",
			"  ,                        !- Condenser Outlet Node Name",
			"  ,                        !- Water Condenser Volume Flow Rate {m3/s}",
			"  ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
			"  ,                        !- Evaporative Condenser Air Flow Rate {m3/s}",
			"  0,                       !- Evaporative Condenser Pump Rated Power Consumption {W}",
			"  ,                        !- Supply Water Storage Tank Name",
			"  0,                       !- Basin Heater Capacity {W/K}",
			"  ,                        !- Basin Heater Setpoint Temperature {C}",
			"  ,                        !- Basin Heater Operating Schedule Name",
			"  Electricity;             !- Fuel Type",
			" ",
			"Zone,",
			"  SPACE1-1,                !- Name",
			"  0,                       !- Direction of Relative North {deg}",
			"  0,                       !- X Origin {m}",
			"  0,                       !- Y Origin {m}",
			"  0,                       !- Z Origin {m}",
			"  1,                       !- Type",
			"  1,                       !- Multiplier",
			"  2.438400269,             !- Ceiling Height {m}",
			"  239.247360229;           !- Volume {m3}",
			" ",
			"ZoneHVAC:EquipmentConnections,",
			"  SPACE1-1,                !- Zone Name",
			"  SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
			"  TU1 Outlet Node,         !- Zone Air Inlet Node or NodeList Name",
			"  TU1 Inlet Node,          !- Zone Air Exhaust Node or NodeList Name",
			"  SPACE1-1 Node,           !- Zone Air Node Name",
			"  SPACE1-1 Out Node;       !- Zone Return Air Node Name", // not used anywhere else in the example file
			" ",
			"ZoneHVAC:EquipmentList,",
			"  SPACE1-1 Eq,             !- Name",
			"  ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,  !- Zone Equipment 1 Object Type",
			"  TU1,                     !- Zone Equipment 1 Name",
			"  1,                       !- Zone Equipment 1 Cooling Sequence",
			"  1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
			" ",
			"ZoneTerminalUnitList,",
			"  VRF Heat Pump TU List,    !- Zone Terminal Unit List Name",
			"  TU1;                      !- Zone Terminal Unit Name 1",
			" ",
			"ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,",
			"  TU1,                      !- Zone Terminal Unit Name",
			"  VRFAvailSched,            !- Terminal Unit Availability Schedule",
			"  TU1 Inlet Node,           !- Terminal Unit Air Inlet Node Name",
			"  TU1 Outlet Node,          !- Terminal Unit Air Outlet Node Name",
			"  autosize,                 !- Supply Air Flow Rate During Cooling Operation {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate When No Cooling is Needed {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate During Heating Operation {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate When No Heating is Needed {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate During Cooling Operation {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate During Heating Operation {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
			"  VRFFanSchedule,           !- Supply Air Fan Operating Mode Schedule Name",
			"  drawthrough,              !- Supply Air Fan Placement",
			"  Fan:ConstantVolume,       !- Supply Air Fan Object Type",
			"  TU1 VRF Supply Fan,       !- Supply Air Fan Object Name",
			"  OutdoorAir:Mixer,         !- Outside Air Mixer Object Type",
			"  TU1 OA Mixer,             !- Outside Air Mixer Object Name",
			"  COIL:Cooling:DX:VariableRefrigerantFlow,  !- Cooling Coil Object Type",
			"  TU1 VRF DX Cooling Coil,  !- Cooling Coil Object Name",
			"  COIL:Heating:DX:VariableRefrigerantFlow,  !- Heating Coil Object Type",
			"  TU1 VRF DX Heating Coil,  !- Heating Coil Object Name",
			"  30,                       !- Zone Terminal Unit On Parasitic Electric Energy Use {W}",
			"  20;                       !- Zone Terminal Unit Off Parasitic Electric Energy Use{ W }",
			" ",
			"Fan:ConstantVolume,",
			"  TU1 VRF Supply Fan,       !- Name",
			"  VRFAvailSched,            !- Availability Schedule Name",
			"  0.7,                      !- Fan Total Efficiency",
			"  600.0,                    !- Pressure Rise{ Pa }",
			"  autosize,                 !- Maximum Flow Rate{ m3 / s }",
			"  0.9,                      !- Motor Efficiency",
			"  1.0,                      !- Motor In Airstream Fraction",
			"  TU1 VRF DX HCoil Outlet Node, !- Air Inlet Node Name",
			"  TU1 Outlet Node;          !- Air Outlet Node Name",
			" ",
			"OutdoorAir:Mixer,",
			"  TU1 OA Mixer,             !- Name",
			"  TU1 VRF DX CCoil Inlet Node, !- Mixed Air Node Name",
			"  Outside Air Inlet Node 1, !- Outdoor Air Stream Node Name",
			"  Relief Air Outlet Node 1, !- Relief Air Stream Node Name",
			"  TU1 Inlet Node;           !- Return Air Stream Node Name",
			" ",
			"OutdoorAir:NodeList,",
			"  OutsideAirInletNodes;     !- Node or NodeList Name 1",
			" ",
			"NodeList,",
			"  OutsideAirInletNodes, !- Name",
			"  Outside Air Inlet Node 1, !- Node 1 Name",
			"  MyVRFOANode;  !- Node 1 Name",
			" ",
			"COIL:Cooling:DX:VariableRefrigerantFlow,",
			"  TU1 VRF DX Cooling Coil, !- Name",
			"  VRFAvailSched,           !- Availability Schedule Name",
			"  autosize,                !- Gross Rated Total Cooling Capacity {W}",
			"  autosize,                !- Gross Rated Sensible Heat Ratio",
			"  autosize,                !- Rated Air Flow Rate {m3/s}",
			"  VRFTUCoolCapFT,          !- Cooling Capacity Ratio Modifier Function of Temperature Curve Name",
			"  VRFACCoolCapFFF,         !- Cooling Capacity Modifier Curve Function of Flow Fraction Name",
			"  TU1 VRF DX CCoil Inlet Node,  !- Coil Air Inlet Node",
			"  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Outlet Node",
			"  ;                        !- Name of Water Storage Tank for Condensate Collection",
			" ",
			"COIL:Heating:DX:VariableRefrigerantFlow,",
			"  TU1 VRF DX Heating Coil, !- Name",
			"  VRFAvailSched,           !- Availability Schedule",
			"  autosize,                !- Gross Rated Heating Capacity {W}",
			"  autosize,                !- Rated Air Flow Rate {m3/s}",
			"  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Inlet Node",
			"  TU1 VRF DX HCoil Outlet Node,  !- Coil Air Outlet Node",
			"  VRFTUHeatCapFT,          !- Heating Capacity Ratio Modifier Function of Temperature Curve Name",
			"  VRFACCoolCapFFF;         !- Heating Capacity Modifier Function of Flow Fraction Curve Name",
			" ",
			"ScheduleTypeLimits,",
			"  Any Number;              !- Name",
			" ",
			"Schedule:Compact,",
			"  VRFAvailSched,           !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 3/31,           !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,1.0,        !- Field 3",
			"  Through: 9/30,           !- Field 4",
			"  For: WeekDays,           !- Field 5",
			"  Until: 7:00,1.0,         !- Field 6",
			"  Until: 17:00,1.0,        !- Field 7",
			"  Until: 24:00,1.0,        !- Field 8",
			"  For: SummerDesignDay WinterDesignDay, !- Field 9",
			"  Until: 24:00,1.0,        !- Field 10",
			"  For: AllOtherDays,       !- Field 11",
			"  Until: 24:00,1.0,        !- Field 12",
			"  Through: 12/31,          !- Field 13",
			"  For: AllDays,            !- Field 14",
			"  Until: 24:00,1.0;        !- Field 15",
			" ",
			"Schedule:Compact,",
			"  VRFCondAvailSched,       !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 12:00,1.0,        !- Field 3",
			"  Until: 13:00,1.0,        !- Field 4",
			"  Until: 24:00,1.0;        !- Field 5",
			" ",
			"Schedule:Compact,",
			"  VRFFanSchedule,          !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 7:00,1.0,         !- Field 3",
			"  Until: 18:00,1.0,        !- Field 5",
			"  Until: 24:00,1.0;        !- Field 7",
			" ",
			"Curve:Biquadratic,",
			"  DefrostEIRSched,         !- Name",
			"  1.0,                     !- Coefficient1 Constant",
			"  0.0,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 y",
			"  0.0,                     !- Coefficient5 y**2",
			"  0.0,                     !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolCapFT,            !- Name",
			"  0.576882692,             !- Coefficient1 Constant",
			"  0.017447952,             !- Coefficient2 x",
			"  0.000583269,             !- Coefficient3 x**2",
			"  -1.76324E-06,            !- Coefficient4 y",
			"  -7.474E-09,              !- Coefficient5 y**2",
			"  -1.30413E-07,            !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFCoolCapFTBoundary,    !- Name",
			"  25.73473775,             !- Coefficient1 Constant",
			"  -0.03150043,             !- Coefficient2 x",
			"  -0.01416595,             !- Coefficient3 x**2",
			"  0,                       !- Coefficient4 x**3",
			"  11,                      !- Minimum Value of x",
			"  30,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolCapFTHi,          !- Name",
			"  0.6867358,               !- Coefficient1 Constant",
			"  0.0207631,               !- Coefficient2 x",
			"  0.0005447,               !- Coefficient3 x**2",
			"  -0.0016218,              !- Coefficient4 y",
			"  -4.259E-07,              !- Coefficient5 y**2",
			"  -0.0003392,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  16,                      !- Minimum Value of y",
			"  43,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolEIRFT,            !- Name",
			"  0.989010541,             !- Coefficient1 Constant",
			"  -0.02347967,             !- Coefficient2 x",
			"  0.000199711,             !- Coefficient3 x**2",
			"  0.005968336,             !- Coefficient4 y",
			"  -1.0289E-07,             !- Coefficient5 y**2",
			"  -0.00015686,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFCoolEIRFTBoundary,    !- Name",
			"  25.73473775,             !- Coefficient1 Constant",
			"  -0.03150043,             !- Coefficient2 x",
			"  -0.01416595,             !- Coefficient3 x**2",
			"  0,                       !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolEIRFTHi,          !- Name",
			"  0.14351470,              !- Coefficient1 Constant",
			"  0.01860035,              !- Coefficient2 x",
			"  -0.0003954,              !- Coefficient3 x**2",
			"  0.02485219,              !- Coefficient4 y",
			"  0.00016329,              !- Coefficient5 y**2",
			"  -0.0006244,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  16,                      !- Minimum Value of y",
			"  43,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  CoolingEIRLowPLR,        !- Name",
			"  0.4628123,               !- Coefficient1 Constant",
			"  -1.0402406,              !- Coefficient2 x",
			"  2.17490997,              !- Coefficient3 x**2",
			"  -0.5974817,              !- Coefficient4 x**3",
			"  0,                       !- Minimum Value of x",
			"  1,                       !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  CoolingEIRHiPLR,         !- Name",
			"  1.0,                     !- Coefficient1 Constant",
			"  0.0,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Linear,",
			"  CoolingCombRatio,        !- Name",
			"  0.618055,                !- Coefficient1 Constant",
			"  0.381945,                !- Coefficient2 x",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  1.0,                     !- Minimum Curve Output",
			"  1.2,                     !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"CURVE:QUADRATIC,",
			"  VRFCPLFFPLR,             !- Name",
			"  0.85,                    !- Coefficient1 Constant",
			"  0.15,                    !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.0,                     !- Minimum Value of x",
			"  1.0,                     !- Maximum Value of x",
			"  0.85,                    !- Minimum Curve Output",
			"  1.0,                     !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatCapFT,            !- Name",
			"  1.014599599,             !- Coefficient1 Constant",
			"  -0.002506703,            !- Coefficient2 x",
			"  -0.000141599,            !- Coefficient3 x**2",
			"  0.026931595,             !- Coefficient4 y",
			"  1.83538E-06,             !- Coefficient5 y**2",
			"  -0.000358147,            !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFHeatCapFTBoundary,    !- Name",
			"  -7.6000882,              !- Coefficient1 Constant",
			"  3.05090016,              !- Coefficient2 x",
			"  -0.1162844,              !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatCapFTHi,          !- Name",
			"  1.161134821,             !- Coefficient1 Constant",
			"  0.027478868,             !- Coefficient2 x",
			"  -0.00168795,             !- Coefficient3 x**2",
			"  0.001783378,             !- Coefficient4 y",
			"  2.03208E-06,             !- Coefficient5 y**2",
			"  -6.8969E-05,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -10,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatEIRFT,            !- Name",
			"  0.87465501,              !- Coefficient1 Constant",
			"  -0.01319754,             !- Coefficient2 x",
			"  0.00110307,              !- Coefficient3 x**2",
			"  -0.0133118,              !- Coefficient4 y",
			"  0.00089017,              !- Coefficient5 y**2",
			"  -0.00012766,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Value of y",
			"  12,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFHeatEIRFTBoundary,    !- Name",
			"  -7.6000882,              !- Coefficient1 Constant",
			"  3.05090016,              !- Coefficient2 x",
			"  -0.1162844,              !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Curve Output",
			"  15,                      !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatEIRFTHi,          !- Name",
			"  2.504005146,             !- Coefficient1 Constant",
			"  -0.05736767,             !- Coefficient2 x",
			"  4.07336E-05,             !- Coefficient3 x**2",
			"  -0.12959669,             !- Coefficient4 y",
			"  0.00135839,              !- Coefficient5 y**2",
			"  0.00317047,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -10,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  HeatingEIRLowPLR,        !- Name",
			"  0.1400093,               !- Coefficient1 Constant",
			"  0.6415002,               !- Coefficient2 x",
			"  0.1339047,               !- Coefficient3 x**2",
			"  0.0845859,               !- Coefficient4 x**3",
			"  0,                       !- Minimum Value of x",
			"  1,                       !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  HeatingEIRHiPLR,         !- Name",
			"  2.4294355,               !- Coefficient1 Constant",
			"  -2.235887,               !- Coefficient2 x",
			"  0.8064516,               !- Coefficient3 x**2",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Linear,",
			"  HeatingCombRatio,        !- Name",
			"  0.96034,                 !- Coefficient1 Constant",
			"  0.03966,                 !- Coefficient2 x",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  1.0,                     !- Minimum Curve Output",
			"  1.023,                   !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  CoolingLengthCorrectionFactor,  !- Name",
			"  1.0693794,               !- Coefficient1 Constant",
			"  -0.0014951,              !- Coefficient2 x",
			"  2.56E-06,                !- Coefficient3 x**2",
			"  -0.1151104,              !- Coefficient4 y",
			"  0.0511169,               !- Coefficient5 y**2",
			"  -0.0004369,              !- Coefficient6 x*y",
			"  8,                       !- Minimum Value of x",
			"  175,                     !- Maximum Value of x",
			"  0.5,                     !- Minimum Value of y",
			"  1.5,                     !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFTUCoolCapFT,          !- Name",
			"  0.504547273506488,       !- Coefficient1 Constant",
			"  0.0288891279198444,      !- Coefficient2 x",
			"  -0.000010819418650677,   !- Coefficient3 x**2",
			"  0.0000101359395177008,   !- Coefficient4 x**3",
			"  0.0,                     !- Minimum Value of x",
			"  50.0,                    !- Maximum Value of x",
			"  0.5,                     !- Minimum Curve Output",
			"  1.5,                     !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  VRFACCoolCapFFF,         !- Name",
			"  0.8,                     !- Coefficient1 Constant",
			"  0.2,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.5,                     !- Minimum Value of x",
			"  1.5;                     !- Maximum Value of x",
			" ",
			"Curve:Cubic,",
			"  VRFTUHeatCapFT,          !- Name",
			"  -0.390708928227928,      !- Coefficient1 Constant",
			"  0.261815023760162,       !- Coefficient2 x",
			"  -0.0130431603151873,     !- Coefficient3 x**2",
			"  0.000178131745997821,    !- Coefficient4 x**3",
			"  0.0,                     !- Minimum Value of x",
			"  50.0,                    !- Maximum Value of x",
			"  0.5,                     !- Minimum Curve Output",
			"  1.5,                     !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		DataGlobals::BeginEnvrnFlag = true;
		DataSizing::CurZoneEqNum = 1;
		DataEnvironment::OutBaroPress = 101325; // sea level
		DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in
		StdRhoAir = PsyRhoAirFnPbTdbW( DataEnvironment::OutBaroPress, 20.0, 0.0 );
		ZoneEqSizing.allocate( 1 );
		ZoneSizingRunDone = true;
		ZoneEqSizing( CurZoneEqNum ).DesignSizeFromParent = false;
		ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 25 );
		ZoneEqSizing( CurZoneEqNum ).SizingMethod( DataHVACGlobals::SystemAirflowSizing ) = DataSizing::SupplyAirFlowRate;

		FinalZoneSizing.allocate( 1 );
		FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.566337; // 400 cfm * 3 tons = 1200 cfm
		FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.566337;

		ZoneSysEnergyDemand.allocate( 1 );

		ProcessScheduleInput(); // read schedules
		GetCurveInput(); // read curves
		GetZoneData( ErrorsFound ); // read zone data
		EXPECT_FALSE( ErrorsFound ); 

		GetZoneEquipmentData(); // read equipment list and connections
		ZoneInletAirNode = GetVRFTUZoneInletAirNode( VRFTUNum ); // trigger GetVRFInput by calling a mining function

		Schedule( VRF( VRFCond ).SchedPtr ).CurrentValue = 1.0; // enable the VRF condenser
		Schedule( VRFTU( VRFTUNum ).SchedPtr ).CurrentValue = 1.0; // enable the terminal unit
		Schedule( VRFTU( VRFTUNum ).FanAvailSchedPtr ).CurrentValue = 1.0; // turn on fan
		Schedule( VRFTU( VRFTUNum ).FanOpModeSchedPtr ).CurrentValue = 0.0; // set cycling fan operating mode

		// Test coil sizing

		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired = 0.0; // set load = 0
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToCoolSP = 0.0;
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToHeatSP = 0.0;

		FinalZoneSizing( CurZoneEqNum ).ZoneRetTempAtCoolPeak = 26.66667;
		FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak = 0.01117049470250416; // AHRI condition at 80 F db / 67 F wb
		Node( VRF( VRFCond ).CondenserNodeNum ).Temp = 35.0; // AHRI condition at 95 F db
		Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).Temp = 35.0; // AHRI condition at 95 F db
		Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).HumRat = 0.01; // don't care
		FinalZoneSizing( CurZoneEqNum ).CoolDDNum = 1;
		FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax = 1;
		DesDayWeath.allocate( 1 );
		DesDayWeath( 1 ).Temp.allocate( 1 );
		DesDayWeath( FinalZoneSizing( CurZoneEqNum ).CoolDDNum ).Temp( FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax ) = 35.0;

		FinalZoneSizing( CurZoneEqNum ).CoolDesTemp = 13.1; // 55.58 F
		FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat = 0.009297628698818194; // humrat at 12.77777 C db / 12.6 C wb

		SimulateVRF( VRFTU( VRFTUNum ).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

		ASSERT_EQ( 1, NumVRFCond );
		ASSERT_EQ( ZoneInletAirNode, ZoneEquipConfig( VRFTU( VRFTUNum ).ZoneNum ).InletNode( 1 ) ); // only 1 inlet node specified above in ZoneHVAC:EquipmentConnections
		ASSERT_EQ( 1.0, VRF( VRFCond ).CoolingCombinationRatio );
		ASSERT_EQ( 10749.071979211991, VRF( VRFCond ).CoolingCapacity );
		ASSERT_EQ( 10749.071979211991, VRF( VRFCond ).HeatingCapacity );
		EXPECT_EQ( 0.0, VRF( VRFCond ).DefrostPower ); 

		// test defrost operation Issue #4950 - Reverse cycle with timed defrost = 0

		// set OA node temperatures for heating where defrost should be active
		Node( VRF( VRFCond ).CondenserNodeNum ).Temp = VRF( VRFCond ).MaxOATDefrost - 1.0;
		Node( VRF( VRFCond ).CondenserNodeNum ).HumRat = 0.005;
		Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).Temp = Node( VRF( VRFCond ).CondenserNodeNum ).Temp;

		// set zone load to heating
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired = VRF( VRFCond ).HeatingCapacity; // set load equal to the VRF heating capacity
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToCoolSP = VRF( VRFCond ).HeatingCapacity + 1000.0; // simulates a dual Tstat with load to cooling SP > load to heating SP
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToHeatSP = VRF( VRFCond ).HeatingCapacity;


		SimulateVRF( VRFTU( VRFTUNum ).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

		ASSERT_TRUE( VRF( VRFCond ).DefrostPower > 0.0 ); // defrost power should be greater than 0
		DefrostWatts = VRF( VRFCond ).VRFCondRTF * ( VRF( VRFCond ).HeatingCapacity / 1.01667 ) * VRF( VRFCond ).DefrostFraction;
		ASSERT_EQ( DefrostWatts, VRF( VRFCond ).DefrostPower ); // defrost power calculation check

		// clean up
		StdRhoAir = 0.0;
		ZoneEqSizing.deallocate();
		FinalZoneSizing.deallocate();
		ZoneSysEnergyDemand.deallocate();
		DesDayWeath.deallocate();

	}

}


