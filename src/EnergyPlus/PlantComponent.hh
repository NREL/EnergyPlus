#ifndef PLANTCOMPONENT_HH_INCLUDED
#define PLANTCOMPONENT_HH_INCLUDED

#include <string>
#include <memory>
#include <PlantLocation.hh>

namespace EnergyPlus {

class PlantComponent
{
public:
	std::string name;
	int compType;
	virtual int performEveryTimeInit( const PlantLocation & calledFromLocation ) = 0;
	virtual int performOneTimeInit( const PlantLocation & calledFromLocation ) = 0;
	virtual int performBeginEnvrnInit( const PlantLocation & calledFromLocation ) = 0;
	virtual int performFirstHVACInit( const PlantLocation & calledFromLocation ) = 0;
	virtual int performInitLoopEquip( const PlantLocation & calledFromLocation ) = 0;
	virtual int simulate( const PlantLocation & calledFromLocation, bool const & FirstHVACIteration ) = 0;
};

}

#endif
