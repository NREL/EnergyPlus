#ifndef TarcogShading_hh_INCLUDED
#define TarcogShading_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace TarcogShading {

	// Functions

	void
	shading(
		Array1< Real64 > const & theta,
		Array1< Real64 > const & gap,
		Array1< Real64 > & hgas,
		Array1< Real64 > & hcgas,
		Array1< Real64 > & hrgas,
		Array2< Real64 > const & frct,
		Array2_int const & iprop,
		Array1< Real64 > const & pressure,
		Array1_int const & nmix,
		Array1< Real64 > const & xwght,
		Array2< Real64 > const & xgcon,
		Array2< Real64 > const & xgvis,
		Array2< Real64 > const & xgcp,
		int const nlayer,
		Real64 const width,
		Real64 const height,
		Real64 const angle,
		Real64 const Tout,
		Real64 const Tin,
		Array1< Real64 > const & Atop,
		Array1< Real64 > const & Abot,
		Array1< Real64 > const & Al,
		Array1< Real64 > const & Ar,
		Array1< Real64 > const & Ah,
		Array1< Real64 > const & vvent,
		Array1< Real64 > const & tvent,
		Array1_int const & LayerType,
		Array1< Real64 > & Tgaps,
		Array1< Real64 > & qv,
		int & nperr,
		std::string & ErrorMessage,
		Array1< Real64 > & vfreevent
	);

	void
	forcedventilation(
		Array1A_int const iprop,
		Array1A< Real64 > const frct,
		Real64 const press,
		int const nmix,
		Array1A< Real64 > const xwght,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
		Real64 const s,
		Real64 const H,
		Real64 const hc,
		Real64 const forcedspeed,
		Real64 const Tinlet,
		Real64 & Toutlet,
		Real64 const Tav,
		Real64 & hcv,
		Real64 & qv,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	shadingin(
		Array1A_int const iprop1,
		Array1A< Real64 > const frct1,
		Real64 const press1,
		int const nmix1,
		Array1A_int const iprop2,
		Array1A< Real64 > const frct2,
		Real64 const press2,
		int const nmix2,
		Array1A< Real64 > const xwght,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
		Real64 & Atop,
		Real64 & Abot,
		Real64 const Al,
		Real64 const Ar,
		Real64 const Ah,
		Real64 const s1,
		Real64 const s2,
		Real64 const H,
		Real64 const L,
		Real64 const angle,
		Real64 const hc1,
		Real64 const hc2,
		Real64 & speed1,
		Real64 & speed2,
		Real64 & Tgap1,
		Real64 & Tgap2,
		Real64 const Tav1,
		Real64 const Tav2,
		Real64 & hcv1,
		Real64 & hcv2,
		Real64 & qv1,
		Real64 & qv2,
		int & nperr,
		std::string & ErrorMessage
	);

	void
	shadingedge(
		Array1A_int const iprop1,
		Array1A< Real64 > const frct1,
		Real64 const press1,
		int const nmix1,
		Array1A_int const iprop2,
		Array1A< Real64 > const frct2,
		Real64 const press2,
		int const nmix2,
		Array1A< Real64 > const xwght,
		Array2A< Real64 > const xgcon,
		Array2A< Real64 > const xgvis,
		Array2A< Real64 > const xgcp,
		Real64 & Atop,
		Real64 & Abot,
		Real64 const Al,
		Real64 const Ar,
		Real64 & Ah,
		Real64 const s,
		Real64 const H,
		Real64 const L,
		Real64 const angle,
		Real64 const forcedspeed,
		Real64 const hc,
		Real64 const Tenv,
		Real64 const Tav,
		Real64 & Tgap,
		Real64 & hcv,
		Real64 & qv,
		int & nperr,
		std::string & ErrorMessage,
		Real64 & speed
	);

} // TarcogShading

} // EnergyPlus

#endif
