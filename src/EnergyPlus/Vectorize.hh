#ifndef EnergyPlus_vectorize_hh_INCLUDED
#define EnergyPlus_vectorize_hh_INCLUDED

// EnergyPlus Vectorization Support

// ObjexxFCL Headers
#include <ObjexxFCL/vectorize.hh> // ASSUME_ALIGNED and RESTRICT macros

// C++ Headers
#include <cassert>
#include <cstdint>

// EnergyPlus headers
#include "EnergyPlus.hh"

namespace EnergyPlus {

	// This is the 
#define VEC_WIDTH OBJEXXFCL_ALIGN_AUTO
	
	int const VEC_LENGTH = VEC_WIDTH / sizeof(Real64);

#define ROUND_TO_VEC_LENGTH(I) (((I) + ((VEC_LENGTH) - 1)) & ~((VEC_LENGTH) - 1))


#define EXPLICIT_VECTORIZATION

	// EXPLICIT_VECTORIZATION assumes that vectors are aligned,
	// which is not a valid assumption on 32-bit builds
#if defined(_WIN32) && !defined(_WIN64)
#undef EXPLICIT_VECTORIZATION
#endif // defined(_WIN32)
	
	
} // EnergyPlus

#endif // EnergyPlus_vectorize_hh_INCLUDED
