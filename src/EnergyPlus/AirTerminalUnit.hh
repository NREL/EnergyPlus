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

#ifndef AirTerminalUnit_hh_INCLUDED
#define AirTerminalUnit_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

// types of air terminal units, refactored from old DataDefineEquip
enum AirTerminalUnitType {
	notYetDetermined,
	dualDuctConstVolume,
	dualDuctVAV,
	singleDuctVAVReheat,
	singleDuctConstVolReheat,
	singleDuctVAVNoReheat,
	singleDuct_SeriesPIU_Reheat,
	singleDuct_ParallelPIU_Reheat,
	singleDuct_ConstVol_4PipeInduc,
	singleDuctVAVReheatVSFan,
	singleDuctCBVAVReheat,
	singleDuctCBVAVNoReheat,
	singleDuctConstVolCooledBeam,
	dualDuctVAVOutdoorAir,
	singleDuctUserDefined,
	singleDuctInletATMixer,
	singleDuctSupplyATMixer,
	singleDuctConstVolFourPipeBeam
};

// base class for all air distribution units.  zone air terminals for connecting to central air handlers
class AirTerminalUnit
{
protected: // Creation

	// Default Constructor
	AirTerminalUnit() :
		terminalType( notYetDetermined ),
		aDUNum( 0 ),
		airAvailSchedNum( 0 ),
		airAvailable( false ),
		vDotDesignPrimAir( 0.0 ),
		vDotDesignPrimAirWasAutosized( false ),
		mDotDesignPrimAir( 0.0 ),
		airInNodeNum( 0 ),
		airOutNodeNum( 0 ),
		zoneIndex( 0 ),
		zoneNodeIndex( 0 )
	{}

	// Copy Constructor
	AirTerminalUnit( AirTerminalUnit const & ) = default;

	// Move Constructor
#if !defined(_MSC_VER) || defined(__INTEL_COMPILER) || (_MSC_VER>=1900)
	AirTerminalUnit( AirTerminalUnit && ) = default;
#endif

public: // Creation

	// Destructor
	virtual
	~AirTerminalUnit()
	{}

protected: // Assignment

	// Copy Assignment

	AirTerminalUnit &
	operator =( AirTerminalUnit const & ) = default;

	// Move Assignment
#if !defined(_MSC_VER) || defined(__INTEL_COMPILER) || (_MSC_VER>=1900)
	AirTerminalUnit &
	operator =( AirTerminalUnit && ) = default;
#endif

public: // Methods

	virtual
	void
	simulate(
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	) = 0;



protected: // Data

	AirTerminalUnitType terminalType; // Type of air distribution unit  //Legacy For use during transition to OO
	std::string name; // name of unit
	std::string unitType; // type of unit = e.g. AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam
	int aDUNum; // index of this unit in the corresponding air distribution unit structure
	int airAvailSchedNum; // index to schedule for pimary air availability
	bool airAvailable; // true if primary air is available
	Real64 vDotDesignPrimAir; // Design primary air volume flow rate m3/s (autosizable)
	bool vDotDesignPrimAirWasAutosized; // true if user input for design air flow was autsized on input
	Real64 mDotDesignPrimAir; // Design primary air mass flow rate kg/s
	int airInNodeNum; // unit air inlet system node number, air enters into air terminal unit
	int airOutNodeNum; // unit air outlet system node number, air enters into zone from air terminal
	int zoneIndex; // zone index for this air terminal unit
	int zoneNodeIndex; // index in node structure for the zone node for this air terminal
}; // AirTerminalUnit

} // EnergyPlus

#endif // AirTerminalUnit_hh_INCLUDED
