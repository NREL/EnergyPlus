#ifndef HeatBalanceIntRadExchange_hh_INCLUDED
#define HeatBalanceIntRadExchange_hh_INCLUDED

// C++
#include <forward_list>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1A.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/FArray2A.hh>
#include <ObjexxFCL/FArray2S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataViewFactorInformation.hh>

//Speedup Helpers
#include <perTArray.hh>

namespace EnergyPlus {

#define EP_HBIRE_SEQ

namespace HeatBalanceIntRadExchange {

  using namespace DataViewFactorInformation;

	// Data
	// MODULE PARAMETER DEFINITIONS

	// DERIVED TYPE DEFINITIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
  extern int count;
  //  extern int MaxNumOfZoneSurfaces; // Max saved to get large enough space for user input view factors
  extern std::vector< size_t > LoadBalanceVector;
  extern std::forward_list< EppPerformance::genPerTArray* > WriteVectors;
  extern std::vector<std::pair< std::vector< ReSurface >::iterator,
			       std::vector< ReSurface >::iterator >> threadSurfIterators;

	// SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceIntRadExchange

	// Functions

	struct badLU : std::exception{
		const char* what() const noexcept {return "LU Decomposition in CalcScriptF failed\n";}
	};
	 
	struct noMoreMemCalcSF : std::exception{
		const char* what() const noexcept {return "'new' failed in CalcScript\n";}
	};

        inline 
	std::vector< ReSurface >::iterator surfBegin(int tid, int ZoneToResimulate){
	  if( tid != -1 ){
	    return threadSurfIterators[tid].first;
	  }else{
	    return ZoneInfo[ ZoneToResimulate - 1 ].surfBegin;
	  }
	}

        inline 
	std::vector< ReSurface >::iterator surfEnd(int tid, int ZoneToResimulate){
	  if(tid != -1){
	    return threadSurfIterators[ tid ].second;
	  }else{
	    return ZoneInfo[ ZoneToResimulate - 1 ].surfEnd;
	  }
	}

        //a factory function for dependent write vectors (specialized for parallel performance,
        //written from this 'module'.  We may need to issue these vectors before we know the
        //load balancing semantics for them, so we will manage them from here and release from
        //our collection once InitInteriorRadExchange has been called
        template<typename T>
        EppPerformance::perTArray< T >&
	GetHBREWriteVector( size_t size ){
	  using namespace EppPerformance;
	  perTArray< T > *retVal;
	  if(LoadBalanceVector.size() == Perf_Thread_Count){
	    retVal =  new perTArray< T >( Perf_Thread_Count,   
						      LoadBalanceVector );
	    assert(size <= std::accumulate(LoadBalanceVector.begin(), LoadBalanceVector.end(), std::plus<size_t>()));
				    
	  }else{
	    size_t cellSize = size / Perf_Thread_Count;
	    if (cellSize * Perf_Thread_Count < size)
	      cellSize = ( cellSize + 1 ) * Perf_Thread_Count / Perf_Thread_Count;
	    retVal = new perTArray< T >(Perf_Thread_Count, cellSize);
	  }
	  WriteVectors.push_front(retVal);
	  return dynamic_cast<perTArray< T >&>(*WriteVectors.front());
	  //return WriteVectors.front();
	}

	

        void
	DoCalcInteriorRadExchange( const int SurfIterations, const int ZoneToResimulate,
		       const int tid = -1 );

	void
	CalcInteriorRadExchange(
		int const SurfIterations, // Number of iterations in calling subroutine
		int const ZoneToResimulate = -1 // if passed in, then only calculate for this zone
	);

        void
	CalcSurfaceTemp(ZoneViewFactorInformation& zone, const int SurfaceIterations);

        void
	CalcSurfaceEmiss(ZoneViewFactorInformation& zone);

	void
	InitInteriorRadExchange();

	void
	GetInputViewFactors(
		std::string const & ZoneName, // Needed to check for user input view factors.
		int const N, // NUMBER OF SURFACES
		FArray2A< Real64 > F, // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
		FArray1A_int const SPtr, // pointer to actual surface number
		bool & NoUserInputF, // Flag signifying no input F's for this
		bool & ErrorsFound // True when errors are found in number of fields vs max args
	);

	void
	GetInputViewFactorsbyName(
		std::string const & ZoneName, // Needed to check for user input view factors.
		int const N, // NUMBER OF SURFACES
		FArray2A< Real64 > F, // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
		FArray1A_int const SPtr, // pointer to actual surface number
		bool & NoUserInputF, // Flag signifying no input F's for this
		bool & ErrorsFound // True when errors are found in number of fields vs max args
	);

	void
	CalcApproximateViewFactors(
		int const N, // NUMBER OF SURFACES
		FArray1A< Real64 > const A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		FArray1A< Real64 > const Azimuth, // Facing angle of the surface (in degrees)
		FArray1A< Real64 > const Tilt, // Tilt angle of the surface (in degrees)
		FArray2A< Real64 > F, // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
		FArray1A_int const SPtr // pointer to REAL(r64) surface number (for error message)
	);

	void
	FixViewFactors(
		int const N, // NUMBER OF SURFACES
		FArray1A< Real64 > const A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		FArray2A< Real64 > F, // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
		int const ZoneNum, // Zone number being fixe
		Real64 & OriginalCheckValue, // check of SUM(F) - N
		Real64 & FixedCheckValue, // check after fixed of SUM(F) - N
		Real64 & FinalCheckValue, // the one to go with
		int & NumIterations, // number of iterations to fixed
		Real64 & RowSum // RowSum of Fixed
	);

	void
	CalcScriptF(
		    ZoneViewFactorInformation & zone
		// int const N, // Number of surfaces
		// FArray1A< Real64 > const A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		// FArray2A< Real64 > const F, // DIRECT VIEW FACTOR MATRIX (N X N)
		// FArray1A< Real64 > EMISS, // VECTOR OF SURFACE EMISSIVITIES
		// FArray2A< Real64 > ScriptF // MATRIX OF SCRIPT F FACTORS (N X N)
	);

	// void
	// CalcMatrixInverse(
	// 	FArray2S< Real64 > Matrix, // Input Matrix
	// 	FArray2S< Real64 > InvMatrix // Inverse of Matrix
	// );

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

} // HeatBalanceIntRadExchange

} // EnergyPlus

#endif
