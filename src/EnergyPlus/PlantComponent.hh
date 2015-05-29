#ifndef PLANTCOMPONENT_HH_INCLUDED
#define PLANTCOMPONENT_HH_INCLUDED

#include <string>
#include <memory>

class PlantComponent
{
public:
	std::string name;
	int compType;
	bool myEnvrnFlag = true;
	bool oneTimeInit = true;
	virtual int performEveryTimeInit() = 0;
	virtual int performOneTimeInit() = 0;
	virtual int performBeginEnvrnInit() = 0;
	virtual int simulate() = 0;
};

#endif
