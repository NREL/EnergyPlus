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

#ifndef HeatPumpWaterToWaterHEATING_hh_INCLUDED
#define HeatPumpWaterToWaterHEATING_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <PlantComponent.hh>

namespace EnergyPlus {

	struct PlantLocation;

namespace HeatPumpWaterToWaterHEATING {

	class GshpSpecs : public PlantComponent
	{
		public:

		GshpSpecs();

		void 
		simulate( const PlantLocation & calledFromLocation, bool const FirstHVACIteration, Real64 & CurLoad ) override;

		void
		InitGshp(); // GSHP number

		void
		CalcGshpModel(
			Real64 & MyLoad, // Operating Load
			bool const FirstHVACIteration
		);

		void
		UpdateGSHPRecords(); // GSHP number

		void 
		getDesignCapacities( const PlantLocation & calledFromLocation, Real64 & MaxLoad, Real64 & MinLoad, Real64 & OptLoad ) override;

		void
		onInitLoopEquip( const PlantLocation & calledFromLocation ) override;

		static PlantComponent * factory( int objectType, std::string objectName );

		static
		void
		GetGshpInput();

		std::string Name; // user identifier

		private:

		// Members
		int WWHPPlantTypeOfNum; // equipment type num
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // simulate the machine at it's operating part load ratio
		Real64 COP; // Coefficient of Performance of the machine
		Real64 NomCap; // Nominal Capcity of the HeatPump
		Real64 MinPartLoadRat; // Minimum operating Part Load Ratio
		Real64 MaxPartLoadRat; // Maximum operating Part Load Ratio
		Real64 OptPartLoadRat; // Optimal operating Part Load Ratio
		Real64 LoadSideVolFlowRate; // Design Flow Rate on the Load side m3/sec
		Real64 LoadSideDesignMassFlow; // Design flow rate (kg/s)
		Real64 SourceSideVolFlowRate; // Design Flow Rate on th Source Side m3/sec
		Real64 SourceSideDesignMassFlow; // Design flow rate (kg/s)
		int SourceSideInletNodeNum; // Node number on the inlet side of the plant
		int SourceSideOutletNodeNum; // Node number on the outlet side of the plant
		int LoadSideInletNodeNum; // Node number on the inlet side of the Load Side
		int LoadSideOutletNodeNum; // Node number on the outlet side of the Load Side
		Real64 SourceSideUACoeff; // Source Side heat transfer coeff W/K
		Real64 LoadSideUACoeff; // Load Side heat transfer coeff  W/K
		Real64 CompPistonDisp; // compressor piston displacement m3
		Real64 CompClearanceFactor; // compressor clearance factor
		Real64 CompSucPressDrop; // deltap ,  compressor suction and discharge pressure drop Pascals
		Real64 SuperheatTemp; // deltatsh , super heating  °C
		Real64 PowerLosses; // constant part of electro mechanical power losses  watts Joules/sec
		Real64 LossFactor; // loss factor used ot define the electro mechanical
		// loss that is supposed to be proportional to the theoretical power
		Real64 HighPressCutoff; // Maximum Design Pressure on the Load Side Pascals
		Real64 LowPressCutoff; // Minimum Design Pressure on the Source Side Pascals
		// Added by Arun 6-27-02
		// to implement cycletime - removed 9/10/2013 LKL
		bool IsOn;
		bool MustRun;
		//loop topology variables
		int SourceLoopNum; // source side plant loop index number
		int SourceLoopSideNum; // source side plant loop side index
		int SourceBranchNum; // source side plant loop branch index
		int SourceCompNum; // source side plant loop component index
		int LoadLoopNum; // load side plant loop index number
		int LoadLoopSideNum; // load side plant loop side index
		int LoadBranchNum; // load side plant loop branch index
		int LoadCompNum; // load side plant loop component index
		int CondMassFlowIndex; // index for criteria in PullCompInterconnectTrigger
		bool MyEnvrnFlag;
		bool MyPlanScanFlag;
		std::string RoutineName;
		std::string RoutineNameLoadSideTemp;
		std::string RoutineNameSourceSideTemp;
		std::string RoutineNameCompressInletTemp;
		std::string RoutineNameSuctionPr;
		std::string RoutineNameCompSuctionTemp;
		gio::Fmt fmtLD;
		Real64 LoadSideWaterMassFlowRate; // Load Side mass flow rate, water side Kg/s
		Real64 SourceSideWaterMassFlowRate; // Source Side mass flow rate, water side Kg/s
		Real64 Power; // power consumption Watts Joules/sec
		Real64 QLoad; // heat rejection from Load Side coil Joules
		Real64 QSource; // cooling capacity Joules
		Real64 SourceSideWaterOutletTemp; // Source Side outlet temperature °C
		Real64 SourceSideWaterInletTemp; // Source Side outlet temperature °C
		Real64 LoadSideWaterOutletTemp; // Source Side outlet temperature °C
		Real64 LoadSideWaterInletTemp; // Source Side outlet temperature °C
		// Report Variables
		Real64 RVPower; // Power Consumption Watts
		Real64 RVEnergy; // Energy Consumption Joules
		Real64 RVQLoad; // Load Side heat transfer rate Watts
		Real64 RVQLoadEnergy; // Load Side heat transfer Joules
		Real64 RVQSource; // Source Side heat transfer rate Watts
		Real64 RVQSourceEnergy; // Source Side heat transfer Joules
		Real64 RVLoadSideWaterInletTemp; // Load Side outlet temperature °C
		Real64 RVSourceSideWaterInletTemp; // Source Side outlet temperature °C
		Real64 RVLoadSideWaterOutletTemp; // Load Side outlet temperature °C
		Real64 RVSourceSideWaterOutletTemp; // Source Side outlet temperature °C
		Real64 RVLoadSidemdot; // Mass flow rate of the cooling water in Load Side Kg/s
		Real64 RVSourceSidemdot; // Mass flow rate of chilled water in Eavporator Kg/s
		int RVRunning; // On reporting Flag

		static bool GetInputFlag;
		static std::string const ModuleCompName;
		static std::string const ModuleCompNameUC;
		static std::string GSHPRefrigerant; // Refrigerent name and index
		static int GSHPRefrigIndex;
	};

	// Object Data
	extern Array1D< GshpSpecs > GSHP; // dimension to number of machines

} // HeatPumpWaterToWaterHEATING

} // EnergyPlus

#endif
