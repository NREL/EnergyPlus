#ifndef BaseGroundTemperatureModel_hh_INCLUDED
#define BaseGroundTemperatureModel_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus{

	// Base class
	class BaseGroundTempsModel
	{
		public:
			// Public Members
			int objectType;
			std::string objectName;
			bool errorsFound;

			// Default Constructor
		BaseGroundTempsModel() :
			objectType( 0 ),
			errorsFound( false )

			{}
		
		// Virtual method for retrieving the ground temp
		virtual Real64
		getGroundTemp()=0;

		virtual Real64
		getGroundTempAtTimeInSeconds(
			Real64 const,
			Real64 const
		)=0;

		virtual Real64
		getGroundTempAtTimeInMonths(
			Real64 const,
			int const
		)=0;

	};

}

#endif
