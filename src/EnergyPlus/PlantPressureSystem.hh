// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

#ifndef PlantPressureSystem_hh_INCLUDED
#define PlantPressureSystem_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace PlantPressureSystem {

	// Functions
	void
	clear_state();

	void
	SimPressureDropSystem(
		int const LoopNum, // Plant Loop to update pressure information
		bool const FirstHVACIteration, // System flag
		int const CallType, // Enumerated call type
		Optional_int_const LoopSideNum = _, // Loop side num for specific branch simulation
		Optional_int_const BranchNum = _ // Branch num for specific branch simulation
	);

	void
	InitPressureDrop(
		int const LoopNum,
		bool const FirstHVACIteration
	);

	void
	BranchPressureDrop(
		int const LoopNum, // Plant Loop Index
		int const LoopSideNum, // LoopSide Index (1=Demand, 2=Supply) on Plant Loop LoopNum
		int const BranchNum // Branch Index on LoopSide LoopSideNum
	);

	void
	UpdatePressureDrop( int const LoopNum );

	void
	DistributePressureOnBranch(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		Real64 & BranchPressureDrop,
		bool & PumpFound
	);

	void
	PassPressureAcrossMixer(
		int const LoopNum,
		int const LoopSideNum,
		Real64 & MixerPressure,
		int const NumBranchesOnLoopSide
	);

	void
	PassPressureAcrossSplitter(
		int const LoopNum,
		int const LoopSideNum,
		Real64 & SplitterInletPressure
	);

	void
	PassPressureAcrossInterface( int const LoopNum );

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

} // PlantPressureSystem

} // EnergyPlus

#endif
