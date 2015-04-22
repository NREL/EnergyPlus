#ifndef DataDaylightingDevices_hh_INCLUDED
#define DataDaylightingDevices_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataDaylightingDevices {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const MaxTZones; // Maximum number of transition zones
	extern int const NumOfAngles; // Number of data points on transmittance vs. angle curve

	extern int const VisibleBeam; // Constant for radiation type
	extern int const SolarBeam; // Constant for radiation type
	extern int const SolarAniso; // Constant for radiation type
	extern int const SolarIso; // Constant for radiation type

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// INTERFACE BLOCK SPECIFICATIONS: na

	// MODULE VARIABLE DECLARATIONS:
	extern int NumOfTDDPipes; // Number of TDD pipes in the input file
	extern int NumOfShelf; // Number of daylighting shelves in the input file

	// Types

	struct TDDPipeData
	{
		// Members
		// Input variables
		std::string Name; // Name of TDD pipe
		int Dome; // Pointer to the dome object
		int Diffuser; // Pointer to the diffuser object
		int Construction; // Pointer to the construction object
		Real64 Diameter; // Pipe diameter
		Real64 TotLength; // Total length of pipe, including exterior
		Real64 Reff; // Effective R value between TDD:DOME and TDD:DIFFUSER
		int NumOfTZones; // Number of transition zone
		Array1D_int TZone; // Pointers to transition zones
		Array1D< Real64 > TZoneLength; // Length of pipe in each transition zone
		// Calculated variables
		Real64 AspectRatio; // Aspect ratio, length / diameter
		Real64 ReflectVis; // Visible reflectance of surface
		Real64 ReflectSol; // Solar reflectance of surface
		Array1D< Real64 > PipeTransVisBeam; // Table of beam visible transmittance vs. cosine angle
		Array1D< Real64 > PipeTransSolBeam; // Table of beam solar transmittance vs. cosine angle
		Real64 TransSolIso; // Diffuse isotropic solar transmittance (constant)
		Real64 TransSolHorizon; // Diffuse horizon solar transmittance (constant)
		Real64 ExtLength; // Exterior exposed length of pipe
		Array1D< Real64 > TZoneHeatGain; // convection gain to transition zones
		// Report variables
		Real64 TransmittedSolar; // Solar transmitted by the TDD [W]
		Real64 PipeAbsorbedSolar; // Solar absorbed in the walls of the pipe [W]
		Real64 HeatGain; // Solar heat gain [W]
		Real64 HeatLoss; // Solar heat loss [W]
		Real64 TransVisBeam; // TDD visible transmittance
		Real64 TransSolBeam; // TDD beam solar transmittance
		Real64 TransVisDiff; // TDD diffuse visible transmittance
		Real64 TransSolDiff; // TDD diffuse solar transmittance

		// Default Constructor
		TDDPipeData() :
			Dome( 0 ),
			Diffuser( 0 ),
			Construction( 0 ),
			Diameter( 0.0 ),
			TotLength( 0.0 ),
			Reff( 0.0 ),
			NumOfTZones( 0 ),
			AspectRatio( 0.0 ),
			ReflectVis( 0.0 ),
			ReflectSol( 0.0 ),
			PipeTransVisBeam( NumOfAngles, 0.0 ),
			PipeTransSolBeam( NumOfAngles, 0.0 ),
			TransSolIso( 0.0 ),
			TransSolHorizon( 0.0 ),
			ExtLength( 0.0 ),
			TransmittedSolar( 0.0 ),
			PipeAbsorbedSolar( 0.0 ),
			HeatGain( 0.0 ),
			HeatLoss( 0.0 ),
			TransVisBeam( 0.0 ),
			TransSolBeam( 0.0 ),
			TransVisDiff( 0.0 ),
			TransSolDiff( 0.0 )
		{}

		// Member Constructor
		TDDPipeData(
			std::string const & Name, // Name of TDD pipe
			int const Dome, // Pointer to the dome object
			int const Diffuser, // Pointer to the diffuser object
			int const Construction, // Pointer to the construction object
			Real64 const Diameter, // Pipe diameter
			Real64 const TotLength, // Total length of pipe, including exterior
			Real64 const Reff, // Effective R value between TDD:DOME and TDD:DIFFUSER
			int const NumOfTZones, // Number of transition zone
			Array1_int const & TZone, // Pointers to transition zones
			Array1< Real64 > const & TZoneLength, // Length of pipe in each transition zone
			Real64 const AspectRatio, // Aspect ratio, length / diameter
			Real64 const ReflectVis, // Visible reflectance of surface
			Real64 const ReflectSol, // Solar reflectance of surface
			Array1< Real64 > const & PipeTransVisBeam, // Table of beam visible transmittance vs. cosine angle
			Array1< Real64 > const & PipeTransSolBeam, // Table of beam solar transmittance vs. cosine angle
			Real64 const TransSolIso, // Diffuse isotropic solar transmittance (constant)
			Real64 const TransSolHorizon, // Diffuse horizon solar transmittance (constant)
			Real64 const ExtLength, // Exterior exposed length of pipe
			Array1< Real64 > const & TZoneHeatGain, // convection gain to transition zones
			Real64 const TransmittedSolar, // Solar transmitted by the TDD [W]
			Real64 const PipeAbsorbedSolar, // Solar absorbed in the walls of the pipe [W]
			Real64 const HeatGain, // Solar heat gain [W]
			Real64 const HeatLoss, // Solar heat loss [W]
			Real64 const TransVisBeam, // TDD visible transmittance
			Real64 const TransSolBeam, // TDD beam solar transmittance
			Real64 const TransVisDiff, // TDD diffuse visible transmittance
			Real64 const TransSolDiff // TDD diffuse solar transmittance
		) :
			Name( Name ),
			Dome( Dome ),
			Diffuser( Diffuser ),
			Construction( Construction ),
			Diameter( Diameter ),
			TotLength( TotLength ),
			Reff( Reff ),
			NumOfTZones( NumOfTZones ),
			TZone( TZone ),
			TZoneLength( TZoneLength ),
			AspectRatio( AspectRatio ),
			ReflectVis( ReflectVis ),
			ReflectSol( ReflectSol ),
			PipeTransVisBeam( NumOfAngles, PipeTransVisBeam ),
			PipeTransSolBeam( NumOfAngles, PipeTransSolBeam ),
			TransSolIso( TransSolIso ),
			TransSolHorizon( TransSolHorizon ),
			ExtLength( ExtLength ),
			TZoneHeatGain( TZoneHeatGain ),
			TransmittedSolar( TransmittedSolar ),
			PipeAbsorbedSolar( PipeAbsorbedSolar ),
			HeatGain( HeatGain ),
			HeatLoss( HeatLoss ),
			TransVisBeam( TransVisBeam ),
			TransSolBeam( TransSolBeam ),
			TransVisDiff( TransVisDiff ),
			TransSolDiff( TransSolDiff )
		{}

	};

	struct ShelfData
	{
		// Members
		// Input variables
		std::string Name; // Name of daylighting shelf
		int Window; // Pointer to the window object
		int InSurf; // Pointer to the inside shelf heat transfer surface
		int OutSurf; // Pointer to the outside shelf attached shading surface
		int Construction; // Pointer to the outside shelf construction object
		// Calculated variables
		Real64 OutReflectVis; // Outside shelf visible reflectance
		Real64 OutReflectSol; // Outside shelf solar reflectance
		Real64 ViewFactor; // Outside shelf view factor to window
		// Report variables

		// Default Constructor
		ShelfData() :
			Window( 0 ),
			InSurf( 0 ),
			OutSurf( 0 ),
			Construction( 0 ),
			OutReflectVis( 0.0 ),
			OutReflectSol( 0.0 ),
			ViewFactor( 0.0 )
		{}

		// Member Constructor
		ShelfData(
			std::string const & Name, // Name of daylighting shelf
			int const Window, // Pointer to the window object
			int const InSurf, // Pointer to the inside shelf heat transfer surface
			int const OutSurf, // Pointer to the outside shelf attached shading surface
			int const Construction, // Pointer to the outside shelf construction object
			Real64 const OutReflectVis, // Outside shelf visible reflectance
			Real64 const OutReflectSol, // Outside shelf solar reflectance
			Real64 const ViewFactor // Outside shelf view factor to window
		) :
			Name( Name ),
			Window( Window ),
			InSurf( InSurf ),
			OutSurf( OutSurf ),
			Construction( Construction ),
			OutReflectVis( OutReflectVis ),
			OutReflectSol( OutReflectSol ),
			ViewFactor( ViewFactor )
		{}

	};

	// Object Data
	extern Array1D< TDDPipeData > TDDPipe;
	extern Array1D< ShelfData > Shelf;

} // DataDaylightingDevices

} // EnergyPlus

#endif
