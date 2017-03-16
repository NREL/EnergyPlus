// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and The Regents of the University of California, through Lawrence Berkeley National Laboratory (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights reserved.
// 
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the U.S. Government consequently retains certain rights. As such, the U.S. Government has been granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable, worldwide license in the Software to reproduce, distribute copies to the public, prepare derivative works, and perform publicly and display publicly, and to permit others to do so.
// 
// Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
// 
// (1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
// 
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
// 
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory, the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
// 
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form without changes from the version obtained under this License, or (ii) Licensee makes a reference solely to the software portion of its product, Licensee must refer to the software as "EnergyPlus version X" software, where "X" is the version number Licensee obtained under this License and may not use a different name for the software. Except as specifically required in this Section (4), Licensee shall not use in a company name, a product name, in advertising, publicity, or other promotional activities any name, trade name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly similar designation, without the U.S. Department of Energy's prior written consent.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <ObjexxFCL/Array1D.hh>

#include <DataHeatBalance.hh>
#include <EnergyPlus.hh>
#include <PhaseChangeModeling/HysteresisModel.hh>
#include <DataIPShortCuts.hh>
#include <UtilityRoutines.hh>
#include <InputProcessor.hh>

namespace EnergyPlus {

namespace HysteresisPhaseChange {

	bool getHysteresisModels( true );
	int numHysteresisModels = 0;
	std::vector< HysteresisPhaseChange > hysteresisPhaseChangeModels;

	PhaseChangeModel * HysteresisPhaseChange::factory( const std::string objectName ) {
		if ( getHysteresisModels ) {
			readAllHysteresisModels();
			getHysteresisModels = false;
		}
		for ( auto & hm : hysteresisPhaseChangeModels ) {
			if ( hm.name == objectName ) {
				return &hm;
			}
		}
		ShowFatalError( "HysteresisPhaseChangeModel::factory: Error finding model named \"" + objectName + "\"" );
		return nullptr; // just for the compiler warning
	}

	Real64 HysteresisPhaseChange::getEnthalpy( Real64 T, Real64 EXTRATERMS ) {
		return 0;
	}

	Real64 HysteresisPhaseChange::getCurrentSpecificHeat() {
	//	int MatLay = 0; // this was previously passed in as an arg, now we shouldn't need it, just operate on ourself
	//	int I = 0; // need to get this somehow else
		Array1D< Real64 > phaseChangeDeltaT; // this was previouly a standalone array, this should now be stored on the class itself
		Array1D< Real64 > TDT; // ditto
		Array1D< Real64 > PhaseChangeState; // ditto

		Real64 TempLowPCM = 0; // need to init this from the class members
		Real64 TempHighPCM = 0; // ditto
		Real64 TempLowPCF = 0; // ditto
		Real64 TempHighPCF = 0; // ditto
		Real64 Tc = 0;
		Real64 TcF = 0;
		Real64 TcM = 0;
		Real64 Tau1 = 0;
		Real64 Tau2 = 0;
		Real64 Tau2M = 0;
		Real64 Tau1M = 0;
		Real64 Tau1F = 0;
		Real64 Tau2F = 0;
		Real64 DeltaHM = 0;
		Real64 DeltaH = 0;
		Real64 EXTRATERMS = 0;
		Real64 Cp;

		// this is pulled directly from a chunk of the Fortran PCM code changes
		if ( this->phaseChangeDeltaT < 0 ) {
			if ( this->TDT < TempLowPCM ) {
				this->phaseChangeState = 2;
				Tc = TcM;
				Tau1 = this->tau1;
				Tau2 = this->tau2;
				DeltaH = this->deltaHF;
			} else if ( this->TDT >= TempLowPCM && this->TDT <= TempHighPCM ) {
				this->phaseChangeState = -1;
				Tc = TcM;
				Tau1 = this->tau1;
				Tau2 = this->tau2;
				DeltaH = this->deltaHF;
				if ( ( this->phaseChangeState == 1 && this->phaseChangeState == -1 ) || ( this->phaseChangeState == 0 && this->phaseChangeState == -1 ) ) {
					this->phaseChangeState = 0;
				}
			} else if ( this->TDT > TempHighPCM ) {
				this->phaseChangeState = -2;
				Tc = TcM;
				Tau1 = this->tau1;
				Tau2 = this->tau2;
				DeltaH = this->deltaHF;
			}
		} else if ( this->phaseChangeDeltaT > 0 ) {
			if ( this->TDT < TempLowPCF ) { 
				this->phaseChangeState = 2;
				Tc = TcF;
				Tau1 = this->tau1Prime;
				Tau2 = this->tau2Prime;
				DeltaH = this->deltaHS;
			} else if ( this->TDT >= TempLowPCF && this->TDT <= TempHighPCF ) {
				this->phaseChangeState = 1;
				Tc = TcF;
				Tau1 = this->tau1Prime;
				DeltaH = this->deltaHS;
			}
			if ( ( this->phaseChangeState == -1 && this->phaseChangeState == 1 ) || ( this->phaseChangeState == 0 && this->phaseChangeState == 1 ) ) {
				this->phaseChangeState = 0;
			} else if ( this->TDT > TempHighPCF ) {
				this->phaseChangeState = -2;
				Tc = TcF;
				Tau1 = this->tau1Prime;
				Tau2 = this->tau2Prime;
				DeltaH = this->deltaHS;
			}
		}
		if ( this->phaseChangeState == 0 && this->phaseChangeState == 2 ) {
			this->phaseChangeTransition = 1;
		} else if ( this->phaseChangeState == 0 && this->phaseChangeState == 1 ) {
			this->phaseChangeTransition = 1;
			// this->phaseChangeState = 0; ?????
		} else if ( this->phaseChangeState == 1 && this->phaseChangeState == 0 ) {
			this->phaseChangeTransition = 1;
		} else if ( this->phaseChangeState == 2 && this->phaseChangeState == 0 ) {
			this->phaseChangeTransition = 1;
		} else {
			this->phaseChangeTransition = 0;
		}

		// if ( hysteresis flag == 1 )  -- implied by this derived class
		if ( this->phaseChangeTransition == 0 ) {
			this->enthOld = this->getEnthalpy(this->TD, EXTRATERMS );
			this->enthNew = this->getEnthalpy(this->TDT, EXTRATERMS );
              	} else if ( this->phaseChangeTransition == 1 ) {
			if ( this->phaseChangeState == 1 && this->phaseChangeState == 0 ) {
				this->enthRev = this->getEnthalpy(this->TR, EXTRATERMS );
				this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthOld - ( this->specHeatTransition * this->TD ) );
				this->enthalpyM = this->getEnthalpy(this->TDT, EXTRATERMS );
				this->enthalpyF = this->getEnthalpy(this->TDT, EXTRATERMS );
				if ( this->enthNew < this->enthRev && this->enthNew >= this->enthalpyF && this->TDT <= this->TD ) {
					this->phaseChangeState = 1;
					this->enthNew = this->getEnthalpy(this->TDT, EXTRATERMS );
				} else if ( this->enthNew < this->enthalpyF && this->enthNew > this->enthalpyM ) {
					this->phaseChangeState = 0;
					this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthOld - ( this->specHeatTransition * this->TD ) );
				} else if ( this->enthNew < this->enthalpyF && this->TDT > this->TR ) {
					this->phaseChangeState = 0;
					this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthRev - ( this->specHeatTransition * this->TR ) );
				} else if ( this->enthNew <= this->enthalpyM && this->TDT <= this->TR ) {
					this->phaseChangeState = 0;
					this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthRev - ( this->specHeatTransition * this->TR ) );
				}	
			} else if ( this->phaseChangeState == 0 && this->phaseChangeState == 0 ) {
				if ( this->TDT < this->TR ) {
					Tc = TcM;
					Tau1 = Tau1M;
					Tau2 = Tau2M;
					DeltaH = DeltaHM;                       
				} else if ( this->TDT > this->TR ) {
					Tc = TcF;
					Tau1 = Tau1F;
					Tau2 = Tau2F;
					DeltaH = deltaHF;
				}
				this->enthRev = this->getEnthalpy(this->TR, EXTRATERMS );
				this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthOld - ( this->specHeatTransition * this->TD ) );
				this->enthalpyM = this->getEnthalpy(this->TDT, EXTRATERMS );
				this->enthalpyF = this->getEnthalpy(this->TDT, EXTRATERMS );
				if ( this->TDT < this->TR && this->enthNew > this->enthalpyF ) {
					this->phaseChangeState = 1;
					this->enthNew = this->getEnthalpy(this->TDT, EXTRATERMS );
				} else if ( this->enthNew < this->enthalpyF && this->enthNew > this->enthalpyM && ( this->TDT < this->TD || this->TDT > this->TD ) ) {
					this->phaseChangeState = 0;
					this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthRev - ( this->specHeatTransition * this->TR ) );
				} else if ( this->enthNew <= this->enthalpyM && this->TDT >= this->TD && this->enthNew > this->enthOld ) {
					this->phaseChangeState = -1;
					this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthRev - ( this->specHeatTransition * this->TR ) );
				}
			} else if ( this->phaseChangeState == 0 && this->phaseChangeState == -1 ) {
				this->enthRev = this->getEnthalpy(this->TR, EXTRATERMS );
				this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthRev - ( this->specHeatTransition * this->TR ) );
				this->enthalpyM = this->getEnthalpy(this->TDT, EXTRATERMS );
				this->enthalpyF = this->getEnthalpy(this->TDT, EXTRATERMS );
				if ( this->enthNew < this->enthalpyF && this->enthNew > this->enthalpyM ) {
					this->phaseChangeState = 0;
					this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthRev - ( this->specHeatTransition * this->TR ) );
				} else if ( this->enthNew <= this->enthalpyM && this->TDT >= this->TD ) {
					this->phaseChangeState = -1;
					this->enthNew = this->getEnthalpy(this->TDT, EXTRATERMS );
				}
			} else if ( this->phaseChangeState == -1 && this->phaseChangeState == 0 ) {
				this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthOld - ( this->specHeatTransition * this->TD ) );
				this->enthalpyM = this->getEnthalpy(this->TDT, EXTRATERMS );
				this->enthalpyF = this->getEnthalpy(this->TDT, EXTRATERMS );
				if ( this->enthNew < this->enthOld && this->TDT < this->TD ) {
					this->phaseChangeState = 0;
					this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthOld - ( this->specHeatTransition * this->TD ) );
				} else if ( this->enthNew < this->enthalpyF && this->enthNew > this->enthalpyM && this->TDT < this->TD) {
					this->phaseChangeState = 0;
					this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthRev - ( this->specHeatTransition * this->TR ) );
				} else if ( this->enthNew >= this->enthalpyF && this->TDT <= this->TR ) {
					this->phaseChangeState = 0;
					this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthRev - ( this->specHeatTransition * this->TR ) );
				}
			} else if ( this->phaseChangeState == 0 && this->phaseChangeState == 1 ) {
				this->enthalpyM = this->getEnthalpy(this->TDT, EXTRATERMS );
				this->enthalpyF = this->getEnthalpy(this->TDT, EXTRATERMS );
				this->enthRev = this->getEnthalpy(this->TR, EXTRATERMS );
				this->enthNew = ( this->specHeatTransition * this->TDT ) + ( this->enthRev - ( this->specHeatTransition * this->TR ) );
			}
		}
		if ( this->phaseChangeTransition == 0 ) {
			if ( this->enthNew == this->enthOld ) {
				Cp = this->CpOld;
			} else {
				Cp = 0; // SpecHeat( I, this->TD, this->TDT, EXTRATERMS );
			}
		} else if ( this->phaseChangeTransition == 1 ) {
			Cp = this->specHeatTransition;
		}
		// Cp_node( 1 ) = Cp;
		return Cp;
	}

	void readAllHysteresisModels() {
		DataIPShortCuts::cCurrentModuleObject = "MaterialProperty:PhaseChangeHysteresis";
		numHysteresisModels = InputProcessor::GetNumObjectsFound( DataIPShortCuts::cCurrentModuleObject );

		if ( numHysteresisModels > 0 ) {
			for ( int hmNum = 1; hmNum <= numHysteresisModels; ++ hmNum ) {
				bool errorsFound = false;
				int ioStatus;
				int numAlphas;
				int numNumbers;
			    	InputProcessor::GetObjectItem(
					DataIPShortCuts::cCurrentModuleObject,
					hmNum,
					DataIPShortCuts::cAlphaArgs,
					numAlphas,
					DataIPShortCuts::rNumericArgs,
					numNumbers,
					ioStatus,
					DataIPShortCuts::lNumericFieldBlanks,
					DataIPShortCuts::lAlphaFieldBlanks,
					DataIPShortCuts::cAlphaFieldNames,
					DataIPShortCuts::cNumericFieldNames
				);
				bool isNotOK = false;
				bool isBlank = false;
				// InputProcessor::VerifyName( DataIPShortCuts::cAlphaArgs( 1 ), 
				if ( isNotOK ) {
					errorsFound = true;
					if ( isBlank ) {
						DataIPShortCuts::cAlphaArgs( 1 ) = "xxxxx";
					}
				}
				HysteresisPhaseChange thisHM;
				thisHM.name = DataIPShortCuts::cAlphaArgs( 1 );
				thisHM.tempCoeffForThermalConductivity = DataIPShortCuts::rNumericArgs( 1 );
				thisHM.totalLatentHeat = DataIPShortCuts::rNumericArgs( 2 );
				thisHM.specificHeatLiquid = DataIPShortCuts::rNumericArgs( 3 );
				thisHM.deltaTempMeltingHigh = DataIPShortCuts::rNumericArgs( 4 );
				thisHM.peakTempMelting = DataIPShortCuts::rNumericArgs( 5 );
				thisHM.deltaTempMeltingLow = DataIPShortCuts::rNumericArgs( 6 );
				thisHM.specificHeatSolid = DataIPShortCuts::rNumericArgs( 7 );
				thisHM.deltaTempFreezingHigh = DataIPShortCuts::rNumericArgs( 8 );
				thisHM.peakTempFreezing = DataIPShortCuts::rNumericArgs( 9 );
				thisHM.deltaTempFreezingLow = DataIPShortCuts::rNumericArgs( 10 );	
				if ( errorsFound ) {
					ShowFatalError( "Error processing " + DataIPShortCuts::cCurrentModuleObject + " named \"" +DataIPShortCuts::cAlphaArgs( 1 ) + "\"" );
				}
			}
		}
	}

	void clear_state() {
		numHysteresisModels = 0;
		getHysteresisModels = true;
		hysteresisPhaseChangeModels.clear();
	}

} // namespace HysteresisPhaseChange

} // namespace EnergyPlus

