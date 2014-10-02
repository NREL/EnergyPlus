#ifndef DataViewFactorInformation_hh_INCLUDED
#define DataViewFactorInformation_hh_INCLUDED

// C++ Headers
#include <atomic>
#include <iterator>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataViewFactorInformation {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	// Types

  struct ReSurface {
  public:
    ReSurface(): zone( 0 ), temperature( 23.0 ),
								 emissivity( 0 ), isWindow( false ), globalIndex( 0 ), 
								 zoneIndex( 0 ){}
    int zone; //compiles with &&, but chokes on it running at InitInteriorRadExchange assigning zone
    inline const int&
    operator ()( bool global = true ){ return global ? globalIndex : zoneIndex;}
    Real64 temperature;
    Real64 emissivity;
    bool isWindow;
    int globalIndex;
    int zoneIndex;
  };

	struct ZoneViewFactorInformation
	{
		// Members
	private:
	  int m_index;
		
	public:
	  const inline int&
	  operator ()(){
	    return m_index;
	  }
	  struct zoneSurfaces{
	    ZoneViewFactorInformation* zone;
	    zoneSurfaces( ZoneViewFactorInformation& z ):zone( &z ){}
	    inline
	    std::vector<ReSurface>::iterator begin(){
	      return zone->surfBegin;}
	    inline
	    std::vector<ReSurface>::iterator end(){
	      return zone->surfEnd;}
	    
	  };
	  inline void setIndex( int t_index ){m_index = t_index;}
	  void setShadeChanged( int t_globSurf );
		std::string Name; // Zone name
		int NumOfSurfaces; // Number of surfaces in the zone
		FArray2D< Real64 > F; // View Factors
		FArray2D< Real64 > ScriptF; // Hottel's Script F
		FArray1D< Real64 > Area; // Surface area
		FArray1D< Real64 > Emissivity; // Surface emissivity
		FArray1D< Real64 > Azimuth; // Azimuth angle of the surface (in degrees)
		FArray1D< Real64 > Tilt; // Tilt angle of the surface (in degrees)
		FArray1D_int SurfacePtr; // Surface ALLOCATABLE (to Surface derived type)
		FArray1D_string Class; // Class of surface (Wall, Roof, etc.)
		std::atomic<bool> ready;
		int owner; //thread that owns this zone
		bool shadeChanged;
		std::vector<ReSurface>::iterator surfBegin;
		std::vector<ReSurface>::iterator surfEnd;
		zoneSurfaces surfaces;
		        
		// Default Constructor
		ZoneViewFactorInformation() :
			Name(  ),
			NumOfSurfaces( 0 ),
			ready( false ),
			owner( -1 ),
			shadeChanged( false ),
			surfaces( *this )
		{}
	
	  //move constructor
	  ZoneViewFactorInformation( ZoneViewFactorInformation&& zone ):
	    Name( zone.Name ), // Zone name
	    NumOfSurfaces( zone.NumOfSurfaces ), // Number of surfaces in the zone
	    F( zone.F ), // View Factors
	    ScriptF( zone.ScriptF ), // Hottel's Script F
	    Area( zone.Area ), // Surface area
			Emissivity( zone.Emissivity ), // Surface emissivity
	    Azimuth( zone.Azimuth ), // Azimuth angle of the surface (in degrees )
	    Tilt( zone.Tilt ), // Tilt angle of the surface (in degrees )
	    SurfacePtr( zone.SurfacePtr ), // Surface ALLOCATABLE (to Surface derived type)
	    Class( zone.Class ), // Class of surface (Wall, Roof, etc.)
	    owner( zone.owner ),
	    shadeChanged( zone.shadeChanged ),
	    surfBegin( zone.surfBegin ),
	    surfEnd( zone.surfEnd ),
	    surfaces( *this )
	  {
	    ready = false;
	  }
	
	  ZoneViewFactorInformation( const ZoneViewFactorInformation& zone ):
	    Name( zone.Name ), // Zone name
	    NumOfSurfaces( zone.NumOfSurfaces ), // Number of surfaces in the zone
	    F( zone.F ), // View Factors
	    ScriptF( zone.ScriptF ), // Hottel's Script F
	    Area( zone.Area ), // Surface area
	    Emissivity( zone.Emissivity ), // Surface emissivity
	    Azimuth( zone.Azimuth ), // Azimuth angle of the surface ( in degrees)
	    Tilt( zone.Tilt ), // Tilt angle of the surface (in degrees)
	    SurfacePtr( zone.SurfacePtr ), // Surface ALLOCATABLE (to Surface derived type)
	    Class( zone.Class ), // Class of urface (Wall, Roof, etc.)
	    owner( zone.owner ),
	    shadeChanged( zone.shadeChanged ),
	    surfBegin( zone.surfBegin ),
	    surfEnd( zone.surfEnd ),
	    surfaces( *this )
	  {
	    ready = false;
	  }
														 
	  ZoneViewFactorInformation& operator=( const ZoneViewFactorInformation& zone )
		{
	    if( &zone == this ){ return *this; }
	    Name = zone.Name;
	    NumOfSurfaces = zone.NumOfSurfaces;
	    F = zone.F;
	    ScriptF = zone.ScriptF;
	    Area = zone.Area;
			Emissivity = zone.Emissivity;
	    Azimuth = zone.Azimuth;
	    Tilt = zone.Tilt;
	    SurfacePtr = zone.SurfacePtr;
	    Class = zone.Class;
			bool r = zone.ready;
	    ready = r;
	    owner = zone.owner;
	    shadeChanged = zone.shadeChanged;
	    surfBegin = zone.surfBegin;
	    surfEnd = zone.surfEnd;
	    surfaces.zone = this; 
	  }

	    

		// Member Constructor
		ZoneViewFactorInformation(
			std::string const & Name, // Zone name
			int const NumOfSurfaces, // Number of surfaces in the zone
			FArray2< Real64 > const & F, // View Factors
			FArray2< Real64 > const & ScriptF, // Hottel's Script F
			FArray1< Real64 > const & Area, // Surface area
			FArray1< Real64 > const & Emissivity, // Surface emissivity
			FArray1< Real64 > const & Azimuth, // Azimuth angle of the surface (in degrees)
			FArray1< Real64 > const & Tilt, // Tilt angle of the surface (in degrees)
			FArray1_int const & SurfacePtr, // Surface ALLOCATABLE (to Surface derived type)
			FArray1_string const & Class, // Class ofs urface (Wall, Roof, etc.)
			std::atomic<bool>  & ready,
			int const & owner,
			bool const & shadeChanged,
			std::vector<ReSurface>::iterator& surfBegin,
			std::vector<ReSurface>::iterator& surfEnd
		) :
		Name( Name ),
	    NumOfSurfaces( NumOfSurfaces ),
	    F( F ),
	    ScriptF( ScriptF ),
	    Area( Area ),
	    Emissivity( Emissivity ),
	    Azimuth( Azimuth ),
	    Tilt( Tilt ),
	    SurfacePtr( SurfacePtr ),
	    Class( Class ),
	    owner( owner ),
	    shadeChanged( shadeChanged ),
	    surfBegin( surfBegin ),
	    surfEnd( surfEnd ),
	    surfaces( *this )
	  {
	    bool r = ready;
	    this->ready = r;
	  }  
	};

	// Object Data
	extern std::vector< ZoneViewFactorInformation > ZoneInfo;
	extern std::vector< ReSurface > VfSurfaces;
	
	
} // DataViewFactorInformation

} // EnergyPlus

#endif
