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

	Real64 HysteresisPhaseChange::getCurrentSpecificHeat() {
		// this is pulled directly from a chunk of the Fortran PCM code changes
		if ( Material( Matlay ).phaseChange ) {
			auto & thisPCM( Materail( MatLay ).phaseChange );
			if ( phaseChangeDeltaT( I ) < 0 ) {
				if ( TDT( I ) < TempLowPCM ) {
					PhaseChangeState( I ) = 2;
					Tc = TcM;
					Tau1 = Material( MatLay ).tau1;
					Tau2 = Material( MatLay ).tau2;
					DeltaH = Material( MatLay ).deltaHF;
				} else if ( TDT( I ) >= TempLowPCM && TDT ( I ) <= TempHighPCM ) {
					PhaseChangeState( I ) = -1;
					Tc = TcM;
					Tau1 = Material( MatLay ).tau1;
					Tau2 = Material( MatLay ).tau2;
					DeltaH = Material( MatLay ).deltaHF;
					if ( ( PhaseChangeStateOld( I ) == 1 && PhaseChangeState( I ) == -1 ) || ( PhaseChangeStateOld( I ) == 0 && PhaseChangeState( I ) == -1 ) ) {
						PhaseChangeState( I ) = 0;
					}
				} else if ( TDT( I ) > TempHighPCM ) {
					PhaseChangeState( I ) = -2;
					Tc = TcM;
					Tau1 = Material( MatLay ).tau1;
					Tau2 = Material( MatLay ).tau2;
					DeltaH = Material( MatLay ).DeltaHF;
				}
			} else if ( PhaseChangeDeltaT( I ) > 0 ) {
				if ( TDT( I ) < TempLowPCF ) { 
					PhaseChangeState( I ) = 2;
					Tc = TcF;
					Tau1 = Material( MatLay ).tau1Prime;
					Tau2 = Material( MatLay ).tau2Prime;
					DeltaH = Material( MatLay ).DeltaHS;
				} else if ( TDT( I ) >= TempLowPCF && TDT( I ) <= TempHighPCF ) {
					PhaseChangeState( I ) = 1;
					Tc = TcF;
					Tau1 = Material( MatLay ).tau1Prime;
					DeltaH = Material( MatLay ).DeltaHS;
				if ( ( PhaseChangeStateOld( I ) == -1 && PhaseChangeState( I ) == 1 ) || ( PhaseChangeStateOld( I ) == 0 && PhaseChangeState( I ) == 1 ) ) {
					PhaseChangeState( I ) = 0;
				} else if ( TDT( I ) > TempHighPCF ) {
					PhaseChangeState( I ) = -2;
					Tc = TcF;
					Tau1 = Material( MatLay ).tau1Prime;
					Tau2 = Material( MatLay ).tau2Prime;
					DeltaH = Material( MatLay ).DeltaHS;
				}
			}
			if ( PhaseChangeStateOld( I ) == 0 && PhaseChangeState( I ) == 2 ) {
				PhaseChangeTransition( I ) = 1;
			} else if ( PhaseChangeStateOld( I ) == 0 && PhaseChangeState( I ) == 1 ) {
				PhaseChangeTransition( I ) = 1;
				// PhaseChangeState( I ) = 0; ?????
			} else if ( PhaseChangeStateOld( I ) == 1 && PhaseChangeState( I ) == 0 ) {
				PhaseChangeTransition( I ) = 1;
			} else if ( PhaseChangeStateOld( I ) == 2 && PhaseChangeState( I ) == 0 ) {
				PhaseChangeTransition( I ) = 1;
			} else {
				PhaseChangeTransition( I ) = 0;
			}

			// if ( hysteresis flag == 1 )  -- implied by this derived class
			if ( PhaseChangeTransition( I ) == 0 ) {
				EnthOld( I ) = SpecEnthalpy( I, TD( I ), ... );
				EnthNew( I ) = SpecEnthalpy( I, TDT( I ), ... );
	              	} else if ( PhaseChangeTransition( I ) == 1 ) {
				if ( PhaseChangeStateOld( I ) == 1 && PhaseChangeState( I ) == 0 ) {
					EnthRev( I ) = SpecEnthalpy( I, TR( I ), ... );
					EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthOld( I ) - ( SpecHeatTransition * TD( I ) ) );
					EnthalpyM( I ) = SpecEnthalpy( I, TDT( I ), ... );
					EnthalpyF( I ) = SpecEnthalpy( I, TDT( I ), ... );
					if ( EnthNew( I ) < EnthRev( I ) && EnthNew( I ) >= EnthalpyF( I ) && TDT( I ) <= TD( I ) ) {
						PhaseChangeState( I ) = 1;
						EnthNew( I ) = SpecEnthalpy( I, TDT( I ), ... );
					} else if ( EnthNew( I ) < EnthalpyF( I ) && EnthNew( I ) > EnthalpyM( I ) ) {
						PhaseChangeState( I ) = 0;
						EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthOld( I ) - ( SpecHeatTransition * TD( I ) ) );
					} else if ( EnthNew( I ) < EnthalpyF( I ) && TDT( I ) > TR( I ) ) {
						PhaseChangeState( I ) = 0;
						EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthRev( I ) - ( SpecHeatTransition * TR( I ) ) );
					} else if ( EnthNew( I ) <= EnthalpyM( I ) && TDT( I ) <= TR( I ) ) {
						PhaseChangeState( I ) = 0;
						EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthRev( I ) - ( SpecHeatTransition * TR( I ) ) );
					}	
				} else if ( PhaseChangeStateOld( I ) == 0 && PhaseChangeState( I ) == 0 ) {
					if ( TDT( I ) < TR( I ) ) {
						Tc = TcM;
						Tau1 = Tau1M;
						Tau2 = Tau2M;
 						DeltaH = DeltaHM;                       
 					} else if ( TDT( I ) > TR( I ) ) {
						Tc = TcF;
						Tau1 = Tau1F;
						Tau2 = Tau2F;
						DeltaH = DeltaHF;
					}
					EnthRev( I ) = SpecEnthalpy( I, TR( I ), ... );
					EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthOld( I ) - ( SpecHeatTransition * TD( I ) ) );
					EnthalpyM( I ) = SpecEnthalpy( I, TDT( I ), ... );
					EnthalpyF( I ) = SpecEnthalpy( I, TDT( I ), ... );
					if ( TDT( I ) < TR( I ) && EnthNew( I ) > EnthalpyF( I ) ) {
						PhaseChangeState( I ) = 1;
						EnthNew( I ) = SpecEnthalpy( I, TDT( I ) );
					} else if ( EnthNew( I ) < EnthalpyF( I ) && EnthNew( I ) > EnthalpyM( I ) && ( TDT( I ) < TD( I ) || TDT( I ) > TD( I ) ) ) {
						PhaseChangeState( I ) = 0;
						EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthRev( I ) - ( SpecHeatTransition * TR( I ) ) );
					} else if ( EnthNew( I ) <= EnthalpyM( I ) && TDT( I ) >= TD( I ) && EnthNew( I ) > EnthOld( I ) ) {
						PhaseChangeState( I ) = -1;
						EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthRev( I ) - ( SpecHeatTransition * TR( I ) ) );
					}
				} else if ( PhaseChangeStateOld( I ) == 0 && PhaseChangeState( I ) == -1 ) {
					EnthRev( I ) = SpecEnthalpy( I, TR( I ), ... );
					EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthRev( I ) - ( SpecHeatTransition * TR( I ) ) );
					EnthalpyM( I ) = SpecEnthalpy( I, TDT( I ), ... );
					EnthalpyF( I ) = SpecEnthalpy( I, TDT( I ), ... );
					if ( EnthNew( I ) < EnthalpyF( I ) && EnthNew( I ) > EnthalpyM( I ) ) {
						PhaseChangeState( I ) = 0;
						EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthRev( I ) - ( SpecHeatTransition * TR( I ) ) );
					} else if ( EnthNew( I ) <= EnthalpyM( I ) && TDT( I ) >= TD( I ) ) {
						PhaseChangeState( I ) = -1;
						EnthNew( I ) = SpecEnthalpy( I, TDT( I ), ... );
					}
				} else if ( PhaseChangeStateOld( I ) == -1 && PhaseChangeState( I ) == 0 ) {
					EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthOld( I ) - ( SpecHeatTransition * TD( I ) ) );
					EnthalpyM( I ) = SpecEnthalpy( I, TDT( I ), ... );
					EnthalpyF( I ) = SpecEnthalpy( I, TDT( I ), ... );
					if ( EnthNew( I ) < EnthOld( I ) && TDT( I ) < TD( I ) ) {
						PhaseChangeState( I ) = 0;
						EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthOld( I ) - ( SpecHeatTransition * TD( I ) ) );
					} else if ( EnthNew( I ) < EnthalpyF( I ) && EnthNew( I ) > EnthalpyM( I ) && TDT( I ) < TD( I )) {
						PhaseChangeState( I ) = 0;
						EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthRev( I ) - ( SpecHeatTransition * TR( I ) ) );
					} else if ( EnthNew( I ) >= EnthalpyF( I ) && TDT( I ) <= TR( I ) ) {
						PhaseChangeState( I ) = 0;
						EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthRev( I ) - ( SpecHeatTransition * TR( I ) ) );
					}
				} else if ( PhaseChangeStateOld( I ) == 0 && PhaseChangeState( I ) == 1 ) {
					EnthalpyM( I ) = SpecEnthalpy( I, TDT( I ), ... );
					EnthalpyF( I ) = SpecEnthalpy( I, TDT( I ), ... );
					EnthRev( I ) = SpecEnthalpy( I, TR( I ), ... );
					EnthNew( I ) = ( SpecHeatTransition * TDT( I ) ) + ( EnthRev( I ) - ( SpecHeatTransition * TR( I ) ) );
				}
			}
		}
		if ( PhaseChangeTransition( I ) = 0 ) {
			if ( EnthNew( I ) == EnthOld( I ) ) {
				Cp = CpOld( I );
			} else {
				Cp = SpecHeat( I, TD( I ), TDT( I ), ... );
			}
		} else if ( PhaseChangeTransition( I ) == 1 ) {
			Cp = SpecHeatTransition;
		}
		Cp_node( 1 ) = Cp;
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

