// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

#ifndef DataGenerators_hh_INCLUDED
#define DataGenerators_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataGenerators {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const NormalizedCurveMode; // mode where efficiency curves are modifier curves
	extern int const DirectCurveMode; // mode where efficiency curves are direct

	extern int const ConstantRateSkinLoss; // fixed rate mode for skin losses
	extern int const UADTSkinLoss; // UAdelta T mode for skin losses
	extern int const QuadraticFuelNdotSkin; // Quadratic function of fuel flow for skin losses

	extern int const QuadraticFuncofNdot; // function of fuel rate mode for air flow
	extern int const ConstantStoicsAirRat; // Constant air ratio in stoics with fuel constituents
	extern int const QuadraticFuncofPel; // function of electric power mode

	extern int const NoRecoveryOnAirIntake; // mode for controlling intake air heat recovery
	extern int const RecoverBurnInvertBatt; // mode for controlling intake air heat recovery
	extern int const RecoverAuxiliaryBurner; // mode for controlling intake air heat recovery
	extern int const RecoverInverterBatt; // mode for controlling intake air heat recovery
	extern int const RecoverInverter; // mode for controlling intake air heat recovery
	extern int const RecoverBattery; // mode for controlling intake air heat recovery

	extern int const RegularAir;
	extern int const UserDefinedConstituents;

	extern int const FuelInTempFromNode;
	extern int const FuelInTempSchedule;

	extern int const WaterInReformMains;
	extern int const WaterInReformAirNode;
	extern int const WaterInReformWaterNode;
	extern int const WaterInReformSchedule;

	extern int const InverterEffConstant;
	extern int const InverterEffQuadratic;

	extern int const FixedEffectiveness; // exhaust gas HX modeling mode
	extern int const LMTDempiricalUAeff; // exhaust gas HX modeling mode
	extern int const LMTDfundementalUAeff; // exhaust gas HX modeling mode
	extern int const Condensing; // exhaust gas HX modeling mode

	extern int const SimpleEffConstraints; // electrical storage modeling mode
	extern int const LeadAcidBatterySaupe; // electrical storage modeling mode
	extern int const LeadAcidBatterManwellMcGowan; // electrical storage modeling mode

	extern int const SurroundingZone;
	extern int const AirInletForFC;

	extern int const OpModeOff; // CHP operating mode OFF
	extern int const OpModeStandby; // CHP operating mode Stand By
	extern int const OpModeWarmUp; // CHP operating mode Warm Up or start up
	extern int const OpModeNormal; // CHP operating mode Normal
	extern int const OpModeCoolDown; // CHP operating mode Cool down or shut down

	extern int const fuelModeGaseousConstituents;
	extern int const fuelModeGenericLiquid;

	extern Real64 const MinProductGasTemp; // Minimum bound on search for product gas temps
	extern Real64 const MaxProductGasTemp; // Maximum bound on search for product gas temps

	extern int const NISTShomate;
	extern int const NASAPolynomial;

	extern Real64 const RinKJperMolpK; // R is ideal gas constant (kJ/mol-K)
	extern Real64 const InitHRTemp; // Initialization temperature for heat recovery water

	extern Real64 const ImBalanceTol; // used as fraction of electrical power at power module

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	extern int NumFuelConstit;
	extern int NumGeneratorFuelSups;
	extern int NumFuelCellGenerators; // number of SOFC Generators specified in input
	extern int NumMicroCHPs;
	extern int NumMicroCHPParams; // number of parameter sets for micro chp
	extern int NumGensWDynamics; // number of dynamics controls for generators

	// Types

	struct FCPowerModuleStruct
	{
		// Members
		// user input data
		std::string Name; // name of this PowerModule data
		int EffMode; // mode for efficiency curves
		int EffCurveID; // pointer to curve for efficiency
		Real64 NomEff; // nominal efficiency
		Real64 NomPel; // nominal power rate at rating point
		int NumCycles; // number of start stop cycles
		Real64 CyclingDegradRat; // rate of degradation from cycles
		Real64 NumRunHours; // number of hours of operation
		Real64 OperateDegradRat; // rate of degradation from run time (per hour)
		Real64 ThreshRunHours; // number of hours before degradation starts
		Real64 UpTranLimit; // power up transient limit
		Real64 DownTranLimit; // power down tran limit
		Real64 StartUpTime; // time for start up [hours]
		Real64 StartUpFuel; // fuel use during start up
		Real64 StartUpElectConsum; // electricity used during start up
		Real64 StartUpElectProd; // electricity produced during start up
		Real64 ShutDownTime; // time to shut down [hours]
		Real64 ShutDownFuel; // fuel consumed during shut down
		Real64 ShutDownElectConsum; // Elect consumed during shut down
		Real64 ANC0; // Ancilliary Loads constant term
		Real64 ANC1; // Ancilliary Loads linear term
		int SkinLossMode; // how are skin losses determined
		std::string ZoneName;
		int ZoneID; // "pointer" to zone with component in it
		Real64 RadiativeFract;
		Real64 QdotSkin;
		Real64 UAskin;
		int SkinLossCurveID;
		int WaterSupplyCurveID; // pointer to curve for water use in reforming
		Real64 NdotDilutionAir; // user defined constant flow of dilution air (kmol/sec)
		Real64 StackHeatLossToDilution; // (watts)
		std::string DilutionInletNodeName; // dilution -> AirHR ?? added air heat recovery path
		int DilutionInletNode; // pointer to node for inlet
		std::string DilutionExhaustNodeName;
		int DilutionExhaustNode; // pointer to node getting exhaust
		Real64 PelMin; // minimum operating point for FCPM electrical power Pel
		Real64 PelMax; // maximum operating point for FCPM electrical power Pel
		//Calculated values and input from elsewhere
		Real64 Pel; // current DC electrical power produced
		Real64 PelLastTimeStep;
		Real64 Eel; // power module efficiency
		Real64 QdotStackCool; // Heat removed by stack cooler
		Real64 FractionalDayofLastStartUp; // fractional days into simulation
		Real64 FractionalDayofLastShutDown; // fractional Days into simulations
		bool HasBeenOn;
		bool DuringShutDown;
		bool DuringStartUp;
		Real64 NdotFuel; // molar fuel use rate.  (kmol/sec)
		Real64 TotFuelInEnthalphy; // Enthalpy of fuel coming into FCPM (watts)
		Real64 NdotProdGas; // (kmol/sec)
		Array1D< Real64 > ConstitMolalFract;
		Array1D_int GasLibID; // lookup ID in Gas Phase ThermoChemistry Structure Array
		Real64 TprodGasLeavingFCPM;
		Real64 NdotAir; // molar air use rate    (kmol/sec)
		Real64 TotAirInEnthalphy; // Enthalpy of air coming nto FCPM energy balance (watts)
		Real64 NdotLiqwater; // molar water use rate (kmol/sec)
		Real64 TwaterInlet;
		Real64 WaterInEnthalpy; // Enthalpy of liquid water used for reforming (watts)
		Real64 DilutionAirInEnthalpy; // Enthalpy of Dilution air coming into FCPM (watts)
		Real64 DilutionAirOutEnthalpy;
		Real64 PelancillariesAC; // ancillary power (watts)
		Real64 TotProdGasEnthalphy; // Enthalphy of product gases leaving FCPM   (watts)
		Real64 WaterOutEnthalpy; // enthalpy of vapor from water used for reforming
		int SeqSubstitIter;
		int RegulaFalsiIter;

		// Default Constructor
		FCPowerModuleStruct() :
			EffMode( 0 ),
			EffCurveID( 0 ),
			NomEff( 0.0 ),
			NomPel( 0.0 ),
			NumCycles( 0 ),
			CyclingDegradRat( 0.0 ),
			NumRunHours( 0.0 ),
			OperateDegradRat( 0.0 ),
			ThreshRunHours( 0.0 ),
			UpTranLimit( 0.0 ),
			DownTranLimit( 0.0 ),
			StartUpTime( 0.0 ),
			StartUpFuel( 0.0 ),
			StartUpElectConsum( 0.0 ),
			StartUpElectProd( 0.0 ),
			ShutDownTime( 0.0 ),
			ShutDownFuel( 0.0 ),
			ShutDownElectConsum( 0.0 ),
			ANC0( 0.0 ),
			ANC1( 0.0 ),
			SkinLossMode( 0 ),
			ZoneID( 0 ),
			RadiativeFract( 0.0 ),
			QdotSkin( 0.0 ),
			UAskin( 0.0 ),
			SkinLossCurveID( 0 ),
			WaterSupplyCurveID( 0 ),
			NdotDilutionAir( 0.0 ),
			StackHeatLossToDilution( 0.0 ),
			DilutionInletNode( 0 ),
			DilutionExhaustNode( 0 ),
			PelMin( 0.0 ),
			PelMax( 0.0 ),
			Pel( 0.0 ),
			PelLastTimeStep( 0.0 ),
			Eel( 0.0 ),
			QdotStackCool( 0.0 ),
			FractionalDayofLastStartUp( 0.0 ),
			FractionalDayofLastShutDown( 0.0 ),
			HasBeenOn( true ),
			DuringShutDown( false ),
			DuringStartUp( false ),
			NdotFuel( 0.0 ),
			TotFuelInEnthalphy( 0.0 ),
			NdotProdGas( 0.0 ),
			ConstitMolalFract( 14, 0.0 ),
			GasLibID( 14, 0 ),
			TprodGasLeavingFCPM( 0.0 ),
			NdotAir( 0.0 ),
			TotAirInEnthalphy( 0.0 ),
			NdotLiqwater( 0.0 ),
			TwaterInlet( 0.0 ),
			WaterInEnthalpy( 0.0 ),
			DilutionAirInEnthalpy( 0.0 ),
			DilutionAirOutEnthalpy( 0.0 ),
			PelancillariesAC( 0.0 ),
			TotProdGasEnthalphy( 0.0 ),
			WaterOutEnthalpy( 0.0 ),
			SeqSubstitIter( 0 ),
			RegulaFalsiIter( 0 )
		{}

	};

	struct FCAirSupplyDataStruct
	{
		// Members
		// user input data
		std::string Name; // name of this
		std::string NodeName; // Air supply node name
		int SupNodeNum; // Air supply node ID
		int BlowerPowerCurveID; // "pointer" to blower power quadratic
		Real64 BlowerHeatLossFactor; // alpha for blower heat loss fraction
		int AirSupRateMode; // control for modeling method used to deterime supply air flow rate
		Real64 Stoics; // excess air ratio
		int AirFuncPelCurveID; // "pointer" to curve for air as function of power
		Real64 AirTempCoeff; // coeff a3 in equ 16.
		int AirFuncNdotCurveID; // "pointer" to curve for air as function of fuel flow rate
		int IntakeRecoveryMode;
		int ConstituentMode; // how are air data input
		int NumConstituents;
		Array1D_string ConstitName;
		Array1D< Real64 > ConstitMolalFract;
		//Calculated values and input from elsewhere
		Array1D_int GasLibID; // lookup ID in Gas Phase ThermoChemistry Structure Array
		Real64 O2fraction;
		Real64 TairIntoBlower; // temperature entering blower
		Real64 TairIntoFCPM; // temperature leaving blower and entering FCPM
		Real64 PairCompEl; // power drawn by compressor
		Real64 QskinLoss; // pumping losses for zone
		Real64 QintakeRecovery; // heat recovered on intake air by accessories

		// Default Constructor
		FCAirSupplyDataStruct() :
			SupNodeNum( 0 ),
			BlowerPowerCurveID( 0 ),
			BlowerHeatLossFactor( 0.0 ),
			AirSupRateMode( 0 ),
			Stoics( 0.0 ),
			AirFuncPelCurveID( 0 ),
			AirTempCoeff( 0.0 ),
			AirFuncNdotCurveID( 0 ),
			IntakeRecoveryMode( 0 ),
			ConstituentMode( 0 ),
			NumConstituents( 0 ),
			ConstitName( 14 ),
			ConstitMolalFract( 14, 0.0 ),
			GasLibID( 14, 0 ),
			O2fraction( 0.0 ),
			TairIntoBlower( 0.0 ),
			TairIntoFCPM( 0.0 ),
			PairCompEl( 0.0 ),
			QskinLoss( 0.0 ),
			QintakeRecovery( 0.0 )
		{}

	};

	struct FCStackCoolerDataStruct
	{
		// Members
		// user input data
		std::string Name; // name of this stack cooler module
		std::string WaterInNodeName; // HR Water Inlet Node
		int WaterInNode; // HR Water Outlet Node ID
		std::string WaterOutNodeName; // HR water outlet Node name
		int WaterOutNode; // HR Water outlet Node ID
		Real64 TstackNom; // nominal fuel cell stack temperature
		Real64 TstackActual; // actual fuel cell stack temperature
		Real64 r0; // stack cooling power coefficient r0
		Real64 r1; // stack cooling power coefficient r1
		Real64 r2; // stack cooling power coefficient r2
		Real64 r3; // stack cooling power coefficient r3
		Real64 MdotStackCoolant; // stack coolant flow rate kg/s
		Real64 UAs_cool; // stack heat transfer coef
		Real64 Fs_cogen;
		Real64 As_cogen;
		Real64 MdotCogenNom;
		Real64 hCogenNom;
		Real64 ns;
		Real64 PstackPumpEl;
		Real64 PmpPowerLossFactor;
		Real64 f0;
		Real64 f1;
		Real64 f2;
		// calculated and from elsewhere
		bool StackCoolerPresent; // control modeling
		Real64 qs_cool;
		Real64 qs_air;

		// Default Constructor
		FCStackCoolerDataStruct() :
			WaterInNode( 0 ),
			WaterOutNode( 0 ),
			TstackNom( 0.0 ),
			TstackActual( 0.0 ),
			r0( 0.0 ),
			r1( 0.0 ),
			r2( 0.0 ),
			r3( 0.0 ),
			MdotStackCoolant( 0.0 ),
			UAs_cool( 0.0 ),
			Fs_cogen( 0.0 ),
			As_cogen( 0.0 ),
			MdotCogenNom( 0.0 ),
			hCogenNom( 0.0 ),
			ns( 0.0 ),
			PstackPumpEl( 0.0 ),
			PmpPowerLossFactor( 0.0 ),
			f0( 0.0 ),
			f1( 0.0 ),
			f2( 0.0 ),
			StackCoolerPresent( false ),
			qs_cool( 0.0 ),
			qs_air( 0.0 )
		{}

	};

	struct FCWaterSupplyDataStruct
	{
		// Members
		std::string Name; // name of this water supply module
		int WaterTempMode; // temperature of water inlet determination
		std::string NodeName; // node name for temperature at input
		int NodeNum; // node number for temperature at input
		int SchedNum; // water temperature at input
		int WaterSupRateCurveID; // "pointer" to water flow rate curve as a function of fuel rate
		int PmpPowerCurveID; // "pointer to Pump power curve as a function of water flow Rate
		Real64 PmpPowerLossFactor; // Pump heat loss factor
		//calculated data
		bool IsModeled;
		Real64 TwaterIntoCompress; // inlet Water Temperature
		Real64 TwaterIntoFCPM; // pumped water temp
		Real64 PwaterCompEl; // water pump power
		Real64 QskinLoss; // pumping losses for zone

		// Default Constructor
		FCWaterSupplyDataStruct() :
			WaterTempMode( 0 ),
			NodeNum( 0 ),
			SchedNum( 0 ),
			WaterSupRateCurveID( 0 ),
			PmpPowerCurveID( 0 ),
			PmpPowerLossFactor( 0.0 ),
			IsModeled( true ),
			TwaterIntoCompress( 0.0 ),
			TwaterIntoFCPM( 0.0 ),
			PwaterCompEl( 0.0 ),
			QskinLoss( 0.0 )
		{}

	};

	struct FCAuxilHeatDataStruct
	{
		// Members
		std::string Name; // name of this auxiliary heating module
		std::string ZoneName;
		int ZoneID;
		Real64 UASkin; // for skin losses to zone
		Real64 ExcessAirRAT;
		Real64 ANC0;
		Real64 ANC1;
		int SkinLossDestination; // control mode for where lost heat goes
		Real64 MaxPowerW;
		Real64 MinPowerW;
		Real64 MaxPowerkmolperSec;
		Real64 MinPowerkmolperSec;
		// calculated and from elsewhere
		int NumConstituents;
		Real64 TauxMix;
		Real64 NdotAuxMix;
		Array1D< Real64 > ConstitMolalFract;
		Array1D_int GasLibID; // lookup ID in Gas Phase ThermoChemistry Structure Array
		Real64 QskinLoss; // Heat lost to room
		Real64 QairIntake; // heat into intake air

		// Default Constructor
		FCAuxilHeatDataStruct() :
			ZoneID( 0 ),
			UASkin( 0.0 ),
			ExcessAirRAT( 0.0 ),
			ANC0( 0.0 ),
			ANC1( 0.0 ),
			SkinLossDestination( 0 ),
			MaxPowerW( 0.0 ),
			MinPowerW( 0.0 ),
			MaxPowerkmolperSec( 0.0 ),
			MinPowerkmolperSec( 0.0 ),
			NumConstituents( 0 ),
			TauxMix( 0.0 ),
			NdotAuxMix( 0.0 ),
			ConstitMolalFract( 14, 0.0 ),
			GasLibID( 14, 0 ),
			QskinLoss( 0.0 ),
			QairIntake( 0.0 )
		{}

	};

	struct FCExhaustHXDataStruct
	{
		// Members
		// user defined variables
		std::string Name; // name of this exhaust gas heat recovery
		std::string WaterInNodeName; // HR Water Inlet Node
		int WaterInNode; // HR Water Outlet Node ID
		std::string WaterOutNodeName; // HR water outlet Node name
		int WaterOutNode; // HR Water outlet Node ID
		Real64 WaterVolumeFlowMax; // HR water flow rate max avail
		std::string ExhaustOutNodeName; // air node for exhaust flow
		int ExhaustOutNode; // Exhaust Air node ID
		int HXmodelMode; // Heat Exchanger Calculation Method
		Real64 HXEffect; // Heat Exchanger Effectiveness (method 1)
		Real64 hxs0; // (method 2)
		Real64 hxs1; // (method 2)
		Real64 hxs2; // (method 2)
		Real64 hxs3; // (method 2)
		Real64 hxs4; // (method 2)
		Real64 h0gas; // (method 3)
		Real64 NdotGasRef; // (method 3)
		Real64 nCoeff; // (method 3)
		Real64 AreaGas; // (method 3)
		Real64 h0Water; // (method 3)
		Real64 NdotWaterRef; // (method 3)
		Real64 mCoeff; // (method 3)
		Real64 AreaWater; // (method 3)
		Real64 Fadjust; // (method 3)
		Real64 l1Coeff; // (method 4)
		Real64 l2Coeff; // (method 4)
		Real64 CondensationThresholdTemp; // (method 4) [degrees C]
		//calculated
		Real64 qHX; // heat flow from gas stream to water
		Real64 THXexh; // temperature of exhaust gases leaving heat exchanger.
		Real64 WaterMassFlowRateDesign; // Design level of water flow rate
		Real64 WaterMassFlowRate; // water flow rate in plant loop
		Real64 WaterInletTemp;
		Real64 WaterVaporFractExh; // water vapor fraction in exhaust gas stream.
		Real64 CondensateRate; // water condensation rate.
		Array1D< Real64 > ConstitMolalFract;
		Array1D_int GasLibID; // lookup ID in Gas Phase ThermoChemistry Structure Array
		Real64 NdotHXleaving;
		Real64 WaterOutletTemp;
		Real64 WaterOutletEnthalpy;

		// Default Constructor
		FCExhaustHXDataStruct() :
			WaterInNode( 0 ),
			WaterOutNode( 0 ),
			WaterVolumeFlowMax( 0.0 ),
			ExhaustOutNode( 0 ),
			HXmodelMode( 0 ),
			HXEffect( 0.0 ),
			hxs0( 0.0 ),
			hxs1( 0.0 ),
			hxs2( 0.0 ),
			hxs3( 0.0 ),
			hxs4( 0.0 ),
			h0gas( 0.0 ),
			NdotGasRef( 0.0 ),
			nCoeff( 0.0 ),
			AreaGas( 0.0 ),
			h0Water( 0.0 ),
			NdotWaterRef( 0.0 ),
			mCoeff( 0.0 ),
			AreaWater( 0.0 ),
			Fadjust( 0.0 ),
			l1Coeff( 0.0 ),
			l2Coeff( 0.0 ),
			CondensationThresholdTemp( 0.0 ),
			qHX( 0.0 ),
			THXexh( 0.0 ),
			WaterMassFlowRateDesign( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			WaterInletTemp( 0.0 ),
			WaterVaporFractExh( 0.0 ),
			CondensateRate( 0.0 ),
			ConstitMolalFract( 14, 0.0 ),
			GasLibID( 14, 0 ),
			NdotHXleaving( 0.0 ),
			WaterOutletTemp( 0.0 ),
			WaterOutletEnthalpy( 0.0 )
		{}

	};

	struct BatteryDichargeDataStruct
	{
		// Members
		// user defined variables
		std::string Name; // name of this battery data set
		Real64 NumInSeries;
		Real64 NumInParallel;
		Real64 NominalVoltage;
		Real64 LowVoltsDischarged; // not used
		int NumTablePairs;
		Array1D< Real64 > DischargeCurrent; // amps
		Array1D< Real64 > DischargeTime; // hours
		// calculated variables
		Real64 k; // parameter in Manwell McGowan model
		Real64 c; // parameter in Manwell McGowan model
		Real64 qmax; // parameter in Manwell McGowan model

		// Default Constructor
		BatteryDichargeDataStruct() :
			NumInSeries( 0.0 ),
			NumInParallel( 0.0 ),
			NominalVoltage( 0.0 ),
			LowVoltsDischarged( 0.0 ),
			NumTablePairs( 0 ),
			k( 0.0 ),
			c( 0.0 ),
			qmax( 0.0 )
		{}

	};

	struct FCElecStorageDataStruct
	{
		// Members
		//user defined variables
		std::string Name; // name of this electrical storage module
		int StorageModelMode;
		Real64 StartingEnergyStored; // joules inside
		Real64 EnergeticEfficCharge; // for
		Real64 EnergeticEfficDischarge;
		Real64 MaxPowerDraw; // for simple bucket method 0
		Real64 MaxPowerStore; // for simple bucket method 0
		Real64 NominalVoltage;
		Real64 NominalEnergyCapacity; // [J]
		//calculated and from elsewhere vars
		Real64 ThisTimeStepStateOfCharge; // [J]
		Real64 LastTimeStepStateOfCharge; // [J]
		Real64 PelNeedFromStorage;
		Real64 IdesiredDischargeCurrent;
		Real64 PelFromStorage; // power
		Real64 IfromStorage; // current this timestepm
		Real64 PelIntoStorage;
		Real64 QairIntake; // heat into intake air
		//nested structures
		BatteryDichargeDataStruct Battery;

		// Default Constructor
		FCElecStorageDataStruct() :
			StorageModelMode( 0 ),
			StartingEnergyStored( 0.0 ),
			EnergeticEfficCharge( 0.0 ),
			EnergeticEfficDischarge( 0.0 ),
			MaxPowerDraw( 0.0 ),
			MaxPowerStore( 0.0 ),
			NominalVoltage( 0.0 ),
			NominalEnergyCapacity( 0.0 ),
			ThisTimeStepStateOfCharge( 0.0 ),
			LastTimeStepStateOfCharge( 0.0 ),
			PelNeedFromStorage( 0.0 ),
			IdesiredDischargeCurrent( 0.0 ),
			PelFromStorage( 0.0 ),
			IfromStorage( 0.0 ),
			PelIntoStorage( 0.0 ),
			QairIntake( 0.0 )
		{}

	};

	struct FCInverterDataStruct
	{
		// Members
		// user-defined data
		std::string Name; // name of this inverter
		int EffMode; // efficiency calculation mode
		Real64 ConstEff;
		int EffQuadraticCurveID;
		// calculated and from elsewhere
		Real64 PCUlosses;
		Real64 QairIntake;

		// Default Constructor
		FCInverterDataStruct() :
			EffMode( 0 ),
			ConstEff( 0.0 ),
			EffQuadraticCurveID( 0 ),
			PCUlosses( 0.0 ),
			QairIntake( 0.0 )
		{}

	};

	struct FCReportDataStruct // these are all for reporting only!
	{
		// Members
		Real64 ACPowerGen; // reporting: power (W)
		Real64 ACEnergyGen; // reporting: energy (J)
		Real64 QdotExhaust; // reporting: exhaust gas heat recovered (W)
		Real64 TotalHeatEnergyRec; // reporting: total heat recovered (J)
		Real64 ExhaustEnergyRec; // reporting: exhaust gas heat recovered (J)
		Real64 FuelEnergyLHV; // reporting: Fuel Energy used in Lower Heating Value(J)
		Real64 FuelEnergyUseRateLHV; // reporting: Fuel Energy used in Lower Heating Value(W)
		Real64 FuelEnergyHHV; // reporting: Fuel Energy used in Lower Heating Value(J)
		Real64 FuelEnergyUseRateHHV; // reporting: Fuel Energy used in Lower Heating Value(W)
		Real64 FuelRateMdot; // (Kg/s)
		Real64 HeatRecInletTemp; // reporting: Heat Recovery Loop Inlet Temperature (C)
		Real64 HeatRecOutletTemp; // reporting: Heat Recovery Loop Outlet Temperature (C)
		Real64 HeatRecMdot; // reporting: Heat Recovery Loop Mass flow rate (kg/s)
		// air supply and blower
		Real64 TairInlet; // State point 1
		Real64 TairIntoFCPM; // Temperature at State point 4
		Real64 NdotAir; // air flow in kmol/sec
		Real64 TotAirInEnthalphy; // Enthalpy at State point 4
		Real64 BlowerPower; // electrical power used by air supply blower
		Real64 BlowerEnergy; // electrical energy used by air supply blower
		Real64 BlowerSkinLoss; // heat rate of losses by blower
		//fuel supply and compressor
		Real64 TfuelInlet; // State point 2 [C]
		Real64 TfuelIntoFCPM; // state point 5 [C]
		Real64 NdotFuel; // fuel flow in [kmol/sec]
		Real64 TotFuelInEnthalpy; // state point 5 [W]
		Real64 FuelCompressPower; // electrical power used by fuel supply compressor [W]
		Real64 FuelCompressEnergy; // electrical energy used by fuel supply compressor [J]
		Real64 FuelCompressSkinLoss; // heat rate of losses.by fuel supply compressor [W]
		//reformer water supply
		Real64 TwaterInlet; // State point 3
		Real64 TwaterIntoFCPM; // State point 6
		Real64 NdotWater; // water flow in kmol/sec (reformer water)
		Real64 WaterPumpPower; // electrical power used by water pump [W]
		Real64 WaterPumpEnergy; // electrical energy used by water pump [J]
		Real64 WaterIntoFCPMEnthalpy; // state point 6
		//product (exhaust) gas leaving power module
		Real64 TprodGas; // State point 7 Product Gas temperature
		Real64 EnthalProdGas; // state point 7 product gas enthalpy
		Real64 NdotProdGas; // point 7 flow rate [kmol/sec]
		Real64 NdotProdAr; // argon flow rate at point 7
		Real64 NdotProdCO2; // carbon dioxide flow rate at point 7
		Real64 NdotProdH2O; // water vapor flow rate at point 7
		Real64 NdotProdN2; // nitrogen flow rate at point 7
		Real64 NdotProdO2; // oxygen flow rate at point 7
		//heat exchanger for water to exhaust heat recovery
		Real64 qHX; // heat flow from gas stream to water [W]
		Real64 HXenergy; // energy from gas stream to water [J]
		Real64 THXexh; // temperature of exhaust gases leaving heat exchanger.
		Real64 WaterVaporFractExh; // water vapor fraction in exhaust gas stream
		// relative to water vapor entering HX  (NdotH20/Ndoaux-mix)
		Real64 CondensateRate; // water condensation rate [kmol/s]
		int SeqSubstIterations; // number of iterations in SOFC loop
		int RegulaFalsiIterations; // number of iterations in Tproduct gas solving
		Real64 ACancillariesPower;
		Real64 ACancillariesEnergy;
		Real64 PCUlosses; // power conditioning Unit losses
		Real64 DCPowerGen; // Pel, Power module power level [W]
		Real64 DCPowerEff; // Eel, power module efficiency []
		Real64 ElectEnergyinStorage; // State of charge in Electrical Storage [J]
		Real64 StoredPower; // Power added to Electrical Storage [W]
		Real64 StoredEnergy; // energy added to Electrical STorage [J]
		Real64 DrawnPower; // Power drawn from Electrical STorage [W]
		Real64 DrawnEnergy; // Energy drawn from Electrical STorage [J]
		Real64 SkinLossPower; // heat loss to surrounding zone [W]
		Real64 SkinLossEnergy; // heat loss to surround zone [J]
		Real64 SkinLossConvect; // convective heat loss to zone [W]
		Real64 SkinLossRadiat; // radiative heat loss to zone [W}
		Real64 ElectEfficiency;
		Real64 ThermalEfficiency;
		Real64 OverallEfficiency;
		Real64 ExergyEfficiency;

		// Default Constructor
		FCReportDataStruct() :
			ACPowerGen( 0.0 ),
			ACEnergyGen( 0.0 ),
			QdotExhaust( 0.0 ),
			TotalHeatEnergyRec( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			FuelEnergyLHV( 0.0 ),
			FuelEnergyUseRateLHV( 0.0 ),
			FuelEnergyHHV( 0.0 ),
			FuelEnergyUseRateHHV( 0.0 ),
			FuelRateMdot( 0.0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 ),
			TairInlet( 0.0 ),
			TairIntoFCPM( 0.0 ),
			NdotAir( 0.0 ),
			TotAirInEnthalphy( 0.0 ),
			BlowerPower( 0.0 ),
			BlowerEnergy( 0.0 ),
			BlowerSkinLoss( 0.0 ),
			TfuelInlet( 0.0 ),
			TfuelIntoFCPM( 0.0 ),
			NdotFuel( 0.0 ),
			TotFuelInEnthalpy( 0.0 ),
			FuelCompressPower( 0.0 ),
			FuelCompressEnergy( 0.0 ),
			FuelCompressSkinLoss( 0.0 ),
			TwaterInlet( 0.0 ),
			TwaterIntoFCPM( 0.0 ),
			NdotWater( 0.0 ),
			WaterPumpPower( 0.0 ),
			WaterPumpEnergy( 0.0 ),
			WaterIntoFCPMEnthalpy( 0.0 ),
			TprodGas( 0.0 ),
			EnthalProdGas( 0.0 ),
			NdotProdGas( 0.0 ),
			NdotProdAr( 0.0 ),
			NdotProdCO2( 0.0 ),
			NdotProdH2O( 0.0 ),
			NdotProdN2( 0.0 ),
			NdotProdO2( 0.0 ),
			qHX( 0.0 ),
			HXenergy( 0.0 ),
			THXexh( 0.0 ),
			WaterVaporFractExh( 0.0 ),
			CondensateRate( 0.0 ),
			SeqSubstIterations( 0 ),
			RegulaFalsiIterations( 0 ),
			ACancillariesPower( 0.0 ),
			ACancillariesEnergy( 0.0 ),
			PCUlosses( 0.0 ),
			DCPowerGen( 0.0 ),
			DCPowerEff( 0.0 ),
			ElectEnergyinStorage( 0.0 ),
			StoredPower( 0.0 ),
			StoredEnergy( 0.0 ),
			DrawnPower( 0.0 ),
			DrawnEnergy( 0.0 ),
			SkinLossPower( 0.0 ),
			SkinLossEnergy( 0.0 ),
			SkinLossConvect( 0.0 ),
			SkinLossRadiat( 0.0 ),
			ElectEfficiency( 0.0 ),
			ThermalEfficiency( 0.0 ),
			OverallEfficiency( 0.0 ),
			ExergyEfficiency( 0.0 )
		{}

	};

	struct FCDataStruct
	{
		// Members
		// from input data and nested types for subsystems
		std::string Name; // user identifier
		std::string NameFCPM; // name of FC Power Module
		FCPowerModuleStruct FCPM; // data for Power Module
		std::string NameFCAirSup; // name of air supply module for fuel cell
		FCAirSupplyDataStruct AirSup; // data for air supply module
		std::string NameFCFuelSup; // name of fuel supply module
		int FuelSupNum; // indes for fuel supply module structure
		std::string NameFCWaterSup; // name of water supply module
		FCWaterSupplyDataStruct WaterSup; // data for water supply module
		std::string NameFCAuxilHeat; // name of auxiliary heating module
		FCAuxilHeatDataStruct AuxilHeat; // data for auxiliary heating module
		std::string NameExhaustHX; // name of Exhaust HX module
		FCExhaustHXDataStruct ExhaustHX; // data for Exhaust heat exchanger module
		std::string NameElecStorage; // name of Battery module
		FCElecStorageDataStruct ElecStorage; // data for Battery module
		std::string NameInverter; // name of Inverter Module
		FCInverterDataStruct Inverter; // data for INverter module
		std::string NameStackCooler; // name of Inverter Module
		FCStackCoolerDataStruct StackCooler; // data for INverter module
		int CWLoopNum; // cooling water plant loop index number
		int CWLoopSideNum; // cooling water plant loop side index
		int CWBranchNum; // cooling water plant loop branch index
		int CWCompNum; // cooling water plant loop component index
		FCReportDataStruct Report; // data for reporting as E+ output variables
		// calculated whole-system level variables
		Real64 ACPowerGen; // Net output from SOFC unit
		Real64 QconvZone; // convective heat lost to surrounding zone
		Real64 QradZone; // radiative heat lost to surrounding zone
		int DynamicsControlID;
		Real64 TimeElapsed; // used to track when timestep has changed

		// Default Constructor
		FCDataStruct() :
			FuelSupNum( 0 ),
			CWLoopNum( 0 ),
			CWLoopSideNum( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			ACPowerGen( 0.0 ),
			QconvZone( 0.0 ),
			QradZone( 0.0 ),
			DynamicsControlID( 0 ),
			TimeElapsed( 0.0 )
		{}

	};

	struct GeneratorFuelSupplyDataStruct
	{
		// Members
		// user input data
		std::string Name; // name of this fuel supply module
		int FuelTempMode; // temperature of fuel node
		int FuelTypeMode; // type of fuel, gasous or liquid
		std::string NodeName; // node name for temperature at input
		int NodeNum; // node number for temperature at input
		int SchedNum; // fuel temperature at input
		int CompPowerCurveID; // "pointer" to compressor power cubic curve
		Real64 CompPowerLossFactor;
		int NumConstituents; // number of constituents in fue supply
		Array1D_string ConstitName;
		Array1D< Real64 > ConstitMolalFract;
		//calculated data (except some for generic liquid)
		Array1D_int GasLibID; // lookup ID in Gas Phase ThermoChemistry Structure Array
		Real64 LHV; // lower heating value of gaseous fuel (kJ/mol)
		Real64 LHVJperkg; // lower heating value of gaseous fuel (J/kg)
		Real64 LHVliquid; // userdefined lhv for generic liquid (J/kg)
		Real64 HHV; // higher heating value of fuel (J/kg)
		Real64 MW; // molecular weight g/mol
		Real64 eCO2; // mass flow based CO2 emmissions factor for complete combustion (-)
		Real64 KmolPerSecToKgPerSec; // conversion from moles to kilograms for this fuel. (
		Real64 StoicOxygenRate;
		Real64 TfuelIntoCompress; // inlet fuel temperature
		Real64 TfuelIntoFCPM; // compressed fuel temp
		Real64 PfuelCompEl; // fuel compressor power
		Real64 QskinLoss; // pumping losses for zone
		Real64 CO2ProductGasCoef; // molar multiplier for stoic products of this fuel
		Real64 H20ProductGasCoef; // molar multiplier for stoic products of this fuel

		// Default Constructor
		GeneratorFuelSupplyDataStruct() :
			FuelTempMode( 0 ),
			FuelTypeMode( 0 ),
			NodeNum( 0 ),
			SchedNum( 0 ),
			CompPowerCurveID( 0 ),
			CompPowerLossFactor( 0.0 ),
			ConstitName( 14 ),
			ConstitMolalFract( 14, 0.0 ),
			GasLibID( 14, 0 ),
			LHV( 0.0 ),
			LHVJperkg( 0.0 ),
			LHVliquid( 0.0 ),
			HHV( 0.0 ),
			MW( 0.0 ),
			eCO2( 0.0 ),
			KmolPerSecToKgPerSec( 0.0 ),
			StoicOxygenRate( 0.0 ),
			TfuelIntoCompress( 0.0 ),
			TfuelIntoFCPM( 0.0 ),
			PfuelCompEl( 0.0 ),
			QskinLoss( 0.0 ),
			CO2ProductGasCoef( 0.0 ),
			H20ProductGasCoef( 0.0 )
		{}

	};

	struct GasPropertyDataStruct
	{
		// Members
		std::string ConstituentName;
		std::string ConstituentFormula;
		Real64 StdRefMolarEnthOfForm;
		int ThermoMode; // method of calculation for thermodynamics
		Real64 ShomateA;
		Real64 ShomateB;
		Real64 ShomateC;
		Real64 ShomateD;
		Real64 ShomateE;
		Real64 ShomateF;
		Real64 ShomateG;
		Real64 ShomateH;
		Real64 NumCarbons;
		Real64 NumHydrogens;
		Real64 NumOxygens;
		Real64 MolecularWeight;
		Real64 NASA_A1;
		Real64 NASA_A2;
		Real64 NASA_A3;
		Real64 NASA_A4;
		Real64 NASA_A5;
		Real64 NASA_A6;
		Real64 NASA_A7;

		// Default Constructor
		GasPropertyDataStruct() :
			StdRefMolarEnthOfForm( 0.0 ),
			ThermoMode( 0 ),
			ShomateA( 0.0 ),
			ShomateB( 0.0 ),
			ShomateC( 0.0 ),
			ShomateD( 0.0 ),
			ShomateE( 0.0 ),
			ShomateF( 0.0 ),
			ShomateG( 0.0 ),
			ShomateH( 0.0 ),
			NumCarbons( 0.0 ),
			NumHydrogens( 0.0 ),
			NumOxygens( 0.0 ),
			MolecularWeight( 0.0 ),
			NASA_A1( 0.0 ),
			NASA_A2( 0.0 ),
			NASA_A3( 0.0 ),
			NASA_A4( 0.0 ),
			NASA_A5( 0.0 ),
			NASA_A6( 0.0 ),
			NASA_A7( 0.0 )
		{}

	};

	struct GeneratorDynamicsManagerStruct
	{
		// Members
		// user input data
		std::string Name;
		Real64 PelMin; // minimum operating point for electrical power Pel
		Real64 PelMax; // maximum operating point for electrical power Pel
		Real64 UpTranLimit; // power up transient limit W/s
		Real64 DownTranLimit; // power down tran limit  W/s
		Real64 UpTranLimitFuel; // fuel up transient limit kg/s
		Real64 DownTranLimitFuel; // fuel down transient limit kg/s
		bool WarmUpByTimeDelay; // Warm up mode control
		bool WarmUpByEngineTemp; // Warm up mode control
		Real64 StartUpTimeDelay; // time for start up [hours]
		Real64 WarmUpDelay; // time for warm up delay [s]
		Real64 StartUpFuel; // fuel use during start up
		Real64 StartUpElectConsum; // electricity used during start up
		Real64 StartUpElectProd; // electricity produced during start up
		Real64 ShutDownFuel; // fuel consumed during shut down
		Real64 ShutDownElectConsum; // Elect consumed during shut down
		Real64 PcoolDown; // power during cool down
		Real64 CoolDownDelay; // time for cool down delay [hours]
		int NumCyclesInit; // number of start stop cycles at beginning
		Real64 NumRunHoursInit; // number of hours of operation beginning
		Real64 Pstandby; // standby power [w]
		Real64 MCeng; // aggregated thermal mass of engine [  ]
		Real64 MCcw; // aggregated thermal mass of heat recovery [   ]
		Real64 kf; // coefficient k_f for warmup fuel flow rate
		Real64 TnomEngOp; // nominal engine operating temperature [C]
		Real64 kp; // coefficient k_p for warmup power
		bool MandatoryFullCoolDown;
		bool WarmRestartOkay;
		int AvailabilitySchedID;
		//Calculated values and input from elsewhere
		int CurrentOpMode; // current operating mode, uses params like OpModeNormal
		int LastOpMode;
		Real64 FractionalDayofLastShutDown;
		Real64 FractionalDayofLastStartUp;
		bool HasBeenOn;
		bool DuringStartUp;
		bool DuringShutDown;
		Real64 FuelMdotLastTimestep;
		Real64 PelLastTimeStep;
		int NumCycles;
		Real64 PLRforSubtimestepStartUp;
		Real64 PLRforSubtimestepShutDown; // part load for not in shut down, shut down part is (1 - PLR)
		Real64 ElectEffNom; // efficiency to use for control decisions
		Real64 ThermEffNom; // thermal efficiency to use fo control decisions
		Real64 QdotHXMax; // Thermal power max
		Real64 QdotHXMin; // thermal power min
		Real64 QdotHXOpt; // thermal power nominal/optimal

		// Default Constructor
		GeneratorDynamicsManagerStruct() :
			PelMin( 0.0 ),
			PelMax( 0.0 ),
			UpTranLimit( 0.0 ),
			DownTranLimit( 0.0 ),
			UpTranLimitFuel( 0.0 ),
			DownTranLimitFuel( 0.0 ),
			WarmUpByTimeDelay( false ),
			WarmUpByEngineTemp( true ),
			StartUpTimeDelay( 0.0 ),
			WarmUpDelay( 0.0 ),
			StartUpFuel( 0.0 ),
			StartUpElectConsum( 0.0 ),
			StartUpElectProd( 0.0 ),
			ShutDownFuel( 0.0 ),
			ShutDownElectConsum( 0.0 ),
			PcoolDown( 0.0 ),
			CoolDownDelay( 0.0 ),
			NumCyclesInit( 0 ),
			NumRunHoursInit( 0.0 ),
			Pstandby( 0.0 ),
			MCeng( 0.0 ),
			MCcw( 0.0 ),
			kf( 0.0 ),
			TnomEngOp( 0.0 ),
			kp( 0.0 ),
			MandatoryFullCoolDown( false ),
			WarmRestartOkay( true ),
			AvailabilitySchedID( 0 ),
			CurrentOpMode( OpModeOff ),
			LastOpMode( OpModeOff ),
			FractionalDayofLastShutDown( 0.0 ),
			FractionalDayofLastStartUp( 0.0 ),
			HasBeenOn( false ),
			DuringStartUp( false ),
			DuringShutDown( false ),
			FuelMdotLastTimestep( 0.0 ),
			PelLastTimeStep( 0.0 ),
			NumCycles( 0 ),
			PLRforSubtimestepStartUp( 0.0 ),
			PLRforSubtimestepShutDown( 0.0 ),
			ElectEffNom( 0.0 ),
			ThermEffNom( 0.0 ),
			QdotHXMax( 0.0 ),
			QdotHXMin( 0.0 ),
			QdotHXOpt( 0.0 )
		{}

	};

	struct MicroCHPParamsNonNormalized
	{
		// Members
		//user parameters
		std::string Name; // name of this PowerModule data
		Real64 MaxElecPower; // net electric power [W]
		Real64 MinElecPower; // net electric power [W]
		Real64 MinWaterMdot; // minimum cooling water flow [kg/s]
		Real64 MaxWaterTemp; // limit temp for inlet cooling water [C]
		int ElecEffCurveID; // index for TriQuadratic for electrical efficiency
		int ThermalEffCurveID; // index for TriQuadric for thermal efficiency
		bool InternalFlowControl; // Plant or Internal Flow rate control?
		bool PlantFlowControl; // default is plant control
		int WaterFlowCurveID; // index for BiQuadratic for water flow rate internal control
		int AirFlowCurveID; // index for Quadratic for generator air flow
		Real64 DeltaPelMax; // max rate of change in net electric power [W/s}
		Real64 DeltaFuelMdotMax; // Maximum Rate of change in fuel flow rate [kmol/s2]
		Real64 UAhx; // heat exchanger UA [W/K]
		Real64 UAskin; // skin loss UA [W/K]
		Real64 RadiativeFraction; // skin loss fraction to radiant energy []
		Real64 MCeng; // aggregated thermal mass of engine [J/K]
		Real64 MCcw; // aggregated thermal mass of heat recovery [J/k]
		Real64 Pstandby; // standby power [w]
		bool WarmUpByTimeDelay; // Warm up mode control
		bool WarmUpByEngineTemp; // Warm up mode control
		Real64 kf; // coefficient k_f for warmup fuel flow rate
		Real64 TnomEngOp; // nominal engine operating temperature [C]
		Real64 kp; // coefficient k_p for warmup power
		Real64 Rfuelwarmup; // Warm Up Fuel Flow Rate Limit Ratio
		Real64 WarmUpDelay; // time for warm up delay [s]
		Real64 PcoolDown; // power during cool down
		Real64 CoolDownDelay; // time for cool down delay [s]
		bool MandatoryFullCoolDown;
		bool WarmRestartOkay;
		// calculated and from elsewhere
		Real64 TimeElapsed; // Fraction of the current hour that has elapsed (h)
		// Saved in order to identify the beginning of a new system time
		int OpMode;
		Real64 OffModeTime; // amount of time generator spent in Off mode
		Real64 StandyByModeTime; // amount of time generator spent in standby mode
		Real64 WarmUpModeTime; // amount of time generator spent in warm up mode
		Real64 NormalModeTime; // amount of time generator spent in normal mode
		Real64 CoolDownModeTime; // amount of time generator spent in Cool down mode
		Real64 TengLast; // last timestep's value for engine temperature
		Real64 TempCWOutLast; // last timestep's value for cooling water outlet temperature
		Real64 Pnet;
		Real64 ElecEff;
		Real64 Qgross;
		Real64 ThermEff;
		Real64 Qgenss;
		Real64 NdotFuel;
		Real64 MdotFuel;
		Real64 Teng;
		Real64 TcwIn;
		Real64 TcwOut;
		Real64 MdotAir;
		Real64 QdotSkin; // rate of heat loss to zone
		Real64 QdotConvZone;
		Real64 QdotRadZone;

		// Default Constructor
		MicroCHPParamsNonNormalized() :
			MaxElecPower( 0.0 ),
			MinElecPower( 0.0 ),
			MinWaterMdot( 0.0 ),
			MaxWaterTemp( 0.0 ),
			ElecEffCurveID( 0 ),
			ThermalEffCurveID( 0 ),
			InternalFlowControl( false ),
			PlantFlowControl( true ),
			WaterFlowCurveID( 0 ),
			AirFlowCurveID( 0 ),
			DeltaPelMax( 0.0 ),
			DeltaFuelMdotMax( 0.0 ),
			UAhx( 0.0 ),
			UAskin( 0.0 ),
			RadiativeFraction( 0.0 ),
			MCeng( 0.0 ),
			MCcw( 0.0 ),
			Pstandby( 0.0 ),
			WarmUpByTimeDelay( false ),
			WarmUpByEngineTemp( true ),
			kf( 0.0 ),
			TnomEngOp( 0.0 ),
			kp( 0.0 ),
			Rfuelwarmup( 0.0 ),
			WarmUpDelay( 0.0 ),
			PcoolDown( 0.0 ),
			CoolDownDelay( 0.0 ),
			MandatoryFullCoolDown( false ),
			WarmRestartOkay( true ),
			TimeElapsed( 0.0 ),
			OpMode( 0 ),
			OffModeTime( 0.0 ),
			StandyByModeTime( 0.0 ),
			WarmUpModeTime( 0.0 ),
			NormalModeTime( 0.0 ),
			CoolDownModeTime( 0.0 ),
			TengLast( 20.0 ),
			TempCWOutLast( 20.0 ),
			Pnet( 0.0 ),
			ElecEff( 0.0 ),
			Qgross( 0.0 ),
			ThermEff( 0.0 ),
			Qgenss( 0.0 ),
			NdotFuel( 0.0 ),
			MdotFuel( 0.0 ),
			Teng( 20.0 ),
			TcwIn( 20.0 ),
			TcwOut( 20.0 ),
			MdotAir( 0.0 ),
			QdotSkin( 0.0 ),
			QdotConvZone( 0.0 ),
			QdotRadZone( 0.0 )
		{}

	};

	struct MicroCHPReportDataStruct // these are all for reporting only!
	{
		// Members
		int Mode; // report operating mode (dev only, remove at end)
		Real64 OffModeTime; // amount of time generator spent in Off mode
		Real64 StandyByModeTime; // amount of time generator spent in standby mode
		Real64 WarmUpModeTime; // amount of time generator spent in warm up mode
		Real64 NormalModeTime; // amount of time generator spent in normal mode
		Real64 CoolDownModeTime; // amount of time generator spent in Cool down mode
		Real64 ACPowerGen; // reporting: power (W)
		Real64 ACEnergyGen; // reporting: energy (J)
		Real64 QdotGross; // reporting: interim gross power (W)
		Real64 Qgenss; // reporting: net recovered heat rate steadystate(0)
		Real64 QdotHX; // reporting: rate of heat exchange from engine to coolant (W)
		Real64 QdotHR; // reporting: rate of heat recovered (W)
		Real64 Tengine; // reporting: engine mass temperature (C)
		Real64 TotalHeatEnergyRec; // reporting: total heat recovered (J)
		Real64 ExhaustEnergyRec; // reporting: exhaust gas heat recovered (J)
		Real64 FuelEnergyLHV; // reporting: Fuel Energy used in Lower Heating Value(J)
		Real64 FuelEnergyUseRateLHV; // reporting: Fuel Energy used in Lower Heating Value(W)
		Real64 FuelEnergyHHV; // reporting: Fuel Energy used in Higher Heating Value(J)
		Real64 FuelEnergyUseRateHHV; // reporting: Fuel Energy used in Higher Heating Value(W)
		Real64 HeatRecInletTemp; // reporting: Heat Recovery Loop Inlet Temperature (C)
		Real64 HeatRecOutletTemp; // reporting: Heat Recovery Loop Outlet Temperature (C)
		Real64 HeatRecMdot; // reporting: Heat Recovery Loop Mass flow rate (kg/s)
		// air supply and blower
		Real64 TairInlet; // State point 1
		Real64 MdotAir; // air flow in kmol/sec
		//fuel supply and compressor
		Real64 TfuelInlet; // State point 2 [C]
		Real64 NdotFuel; // fuel flow in [kmol/sec]
		Real64 MdotFuel; // fuel flow in [kg/s]
		Real64 FuelCompressPower; // electrical power used by fuel supply compressor [W]
		Real64 FuelCompressEnergy; // electrical energy used by fuel supply compressor [J]
		Real64 FuelCompressSkinLoss; // heat rate of losses.by fuel supply compressor [W]
		//heat exchanger for water to exhaust heat recovery
		//   REAL(r64)    :: qHX = 0.0d0 ! heat flow from gas stream to water [W]
		//   REAL(r64)    :: HXenergy = 0.0d0 !energy from gas stream to water [J]
		//   REAL(r64)    :: THXexh = 0.0d0 ! temperature of exhaust gases leaving heat exchanger.
		//   REAL(r64)    :: WaterVaporFractExh = 0.0d0 ! water vapor fraction in exhaust gas stream
		// relative to water vapor entering HX  (NdotH20/Ndoaux-mix)
		//    INTEGER :: SeqSubstIterations = 0 ! number of iterations in SOFC loop
		//    INTEGER :: RegulaFalsiIterations = 0 ! number of iterations in Tproduct gas solving
		Real64 SkinLossPower; // heat loss to surrounding zone [W]
		Real64 SkinLossEnergy; // heat loss to surround zone [J]
		Real64 SkinLossConvect; // convective heat loss to zone [W]
		Real64 SkinLossRadiat; // radiative heat loss to zone [W}
		Real64 ElectEfficiency;
		Real64 ThermalEfficiency;
		Real64 OverallEfficiency;

		// Default Constructor
		MicroCHPReportDataStruct() :
			Mode( 0 ),
			OffModeTime( 0.0 ),
			StandyByModeTime( 0.0 ),
			WarmUpModeTime( 0.0 ),
			NormalModeTime( 0.0 ),
			CoolDownModeTime( 0.0 ),
			ACPowerGen( 0.0 ),
			ACEnergyGen( 0.0 ),
			QdotGross( 0.0 ),
			Qgenss( 0.0 ),
			QdotHX( 0.0 ),
			QdotHR( 0.0 ),
			Tengine( 0.0 ),
			TotalHeatEnergyRec( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			FuelEnergyLHV( 0.0 ),
			FuelEnergyUseRateLHV( 0.0 ),
			FuelEnergyHHV( 0.0 ),
			FuelEnergyUseRateHHV( 0.0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 ),
			TairInlet( 0.0 ),
			MdotAir( 0.0 ),
			TfuelInlet( 0.0 ),
			NdotFuel( 0.0 ),
			MdotFuel( 0.0 ),
			FuelCompressPower( 0.0 ),
			FuelCompressEnergy( 0.0 ),
			FuelCompressSkinLoss( 0.0 ),
			SkinLossPower( 0.0 ),
			SkinLossEnergy( 0.0 ),
			SkinLossConvect( 0.0 ),
			SkinLossRadiat( 0.0 ),
			ElectEfficiency( 0.0 ),
			ThermalEfficiency( 0.0 ),
			OverallEfficiency( 0.0 )
		{}

	};

	struct MicroCHPDataStruct
	{
		// Members
		// user input data
		std::string Name; // name of this Micro CHP Generator
		std::string ParamObjName; // name of parameter object
		MicroCHPParamsNonNormalized A42Model; // Nested parameter data structure
		bool ModelTypeAnnex42; // normalized =  non-normalized?
		Real64 NomEff; // nominal efficiency
		std::string ZoneName;
		int ZoneID;
		std::string PlantInletNodeName;
		int PlantInletNodeID;
		std::string PlantOutletNodeName;
		int PlantOutletNodeID;
		Real64 PlantMassFlowRate; // only if internal control
		Real64 PlantMassFlowRateMax; // hardware limit for node%massflowrateMax
		bool PlantMassFlowRateMaxWasAutoSized; // true if mass flow rate was autosized on input
		std::string AirInletNodeName;
		int AirInletNodeID;
		std::string AirOutletNodeName;
		int AirOutletNodeID;
		MicroCHPReportDataStruct Report; // structure of report variables
		int FuelSupplyID; // index for fuel supply data structure
		int DynamicsControlID; // index in GeneratorDynamics data where control issues are handled
		int AvailabilitySchedID; // index for availability schedule
		int CWLoopNum; // cooling water plant loop index number
		int CWLoopSideNum; // cooling water plant loop side index
		int CWBranchNum; // cooling water plant loop branch index
		int CWCompNum; // cooling water plant loop component index

		// Default Constructor
		MicroCHPDataStruct() :
			ModelTypeAnnex42( true ),
			NomEff( 0.0 ),
			ZoneID( 0 ),
			PlantInletNodeID( 0 ),
			PlantOutletNodeID( 0 ),
			PlantMassFlowRate( 0.0 ),
			PlantMassFlowRateMax( 0.0 ),
			PlantMassFlowRateMaxWasAutoSized( false ),
			AirInletNodeID( 0 ),
			AirOutletNodeID( 0 ),
			FuelSupplyID( 0 ),
			DynamicsControlID( 0 ),
			AvailabilitySchedID( 0 ),
			CWLoopNum( 0 ),
			CWLoopSideNum( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 )
		{}
	};

	// Object Data
	extern Array1D< FCDataStruct > FuelCell; // dimension to number of machines
	extern Array1D< GasPropertyDataStruct > GasPhaseThermoChemistryData;
	extern Array1D< GeneratorFuelSupplyDataStruct > FuelSupply; // fuel supply (reused across various)
	extern Array1D< MicroCHPDataStruct > MicroCHP;
	extern Array1D< MicroCHPParamsNonNormalized > MicroCHPParamInput; // Used during get input then put into nested
	extern Array1D< GeneratorDynamicsManagerStruct > GeneratorDynamics;

	void
	clear_state();

} // DataGenerators

} // EnergyPlus

#endif
