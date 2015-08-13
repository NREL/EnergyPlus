// EnergyPlus::HVACVariableRefrigerantFlow unit tests

// Google test headers
#include <gtest/gtest.h>

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>
#include "Fixtures/EnergyPlusFixture.hh"

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <CurveManager.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataEnvironment.hh>
#include <DXCoils.hh>
#include <HVACVariableRefrigerantFlow.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>

using namespace EnergyPlus;
using namespace HVACVariableRefrigerantFlow;
using namespace CurveManager;
using namespace ScheduleManager;
using namespace DXCoils;

namespace EnergyPlus {

TEST( HVACVariableRefrigerantFlow, VRF_FluidTCtrl_CalcVRF )
{
	// @@ PURPOSE OF THIS SUBROUTINE:
	// 		Subroutine CalcVRF_FluidTCtrl is part of the new VRF model based on physics, appliable for Fluid Temperature Control.
	// 		This subroutine simulates the components making up the VRF indoor terminal unit.

}

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
    DXCoil( HeatCoilIndex ).C1Tc = -1.905;
    DXCoil( HeatCoilIndex ).C2Tc = 0.4333;
    DXCoil( HeatCoilIndex ).C3Tc = 0.0207;
    DXCoil( HeatCoilIndex ).SC	 = 5.00;
    
    // Run and Check for Cooling Mode
    Mode = 0;
    Temp = 15;   	
    ZoneIndex = 1; 
    
    ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToCoolingSP = -2716.6229;
    ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToHeatingSP = -45507.8487;
    
    //DXCoil( CoolCoilIndex ).BF = 0.0592;
    //DXCoil( CoolCoilIndex ).Qfan = 37.80;
    DXCoil( CoolCoilIndex ).RatedAirMassFlowRate( 1 ) = 0.2066;
    DXCoil( CoolCoilIndex ).InletAirTemp = 25.5553;
    DXCoil( CoolCoilIndex ).InletAirHumRat = 8.4682e-3;
    DXCoil( CoolCoilIndex ).InletAirEnthalpy = 47259.78;
    
    CalcVRFIUAirFlow(ZoneIndex, Mode, Temp, CoolCoilIndex, HeatCoilIndex, SHSCModify, FanSpdRatio, Wout, Toutlet, Houtlet, HcoilIn, TcIn, SHact, SCact);
    EXPECT_NEAR( TcIn, 25.60, 0.01 );
    EXPECT_NEAR( Toutlet, 17.90, 0.01 );
    EXPECT_NEAR( Houtlet, 39443, 1 );
    EXPECT_NEAR( HcoilIn, 47306, 1 );
    EXPECT_NEAR( SHact, 3.00, 0.01 );
    
    
    // Run and Check for Heating Mode
    Mode = 1;
    Temp = 42;   	
    ZoneIndex = 2; 
    
    ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToCoolingSP = 43167.2628;
    ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToHeatingSP = 4241.66099;
    
    //DXCoil( HeatCoilIndex ).BF = 0.136;
    //DXCoil( HeatCoilIndex ).Qfan = 37.80;
    DXCoil( HeatCoilIndex ).RatedAirMassFlowRate( 1 ) = 0.21136;
    DXCoil( HeatCoilIndex ).InletAirTemp = 20.2362;
    DXCoil( HeatCoilIndex ).InletAirHumRat = 4.1053e-3;
    DXCoil( HeatCoilIndex ).InletAirEnthalpy = 30755.6253;
    
    CalcVRFIUAirFlow(ZoneIndex, Mode, Temp, CoolCoilIndex, HeatCoilIndex, SHSCModify, FanSpdRatio, Wout, Toutlet, Houtlet, HcoilIn, TcIn, SHact, SCact);
    EXPECT_NEAR( TcIn, 20.45, 0.01 );
    EXPECT_NEAR( Toutlet, 38.40, 0.01 );
    EXPECT_NEAR( Houtlet, 49142, 1 );
    EXPECT_NEAR( HcoilIn, 30973, 1 );
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
    VRF( IndexVRFCondenser ).Algorithm = 0;
    VRF( IndexVRFCondenser ).EvapTempFixed = 3;
    VRF( IndexVRFCondenser ).CondTempFixed = 5;
    
    // Run and Check 
    CalcVRFIUTeTc_FluidTCtrl( IndexVRFCondenser );
    
    EXPECT_EQ( VRF( IndexVRFCondenser ).MinEvaporatingTemp, 3 );
    EXPECT_EQ( VRF( IndexVRFCondenser ).MaxCondensingTemp, 5 );
    
    // Clean up
    VRF.deallocate( ); 
    TerminalUnitList.deallocate( ); 
}

}