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

/* 
 * the process will be:
 *  the plant will loop over the entire plant topology once, and do construction, which is comprised of:
 *   - getting the type num based on object type string as defined in plant topology
 *   - using this to call the appropriate factory method which does the following:
 *     - checks to see if that object has already been instantiated before:
 *       - if so: it returns a shared pointer to that already instantiated version
 *       - if not: it uses the object name, calls the input processor, constructs the object, and returns are shared pointer to the new instance
 *   ! need to think about complex objects such as piping system manager and radiant systems
 *   ! I think the factory class will simply be smart enough to see if that has been done and if so return the appropriate PlantComponent instance shared pointer
 *  with construction complete, the plant can loop over the entire topology each time calling comp->performOneTimeInit();
 *   then set the component oneTimeInit flag to false so that it won't need to call it if it hits it on another spot in the topology
 *  with one time inits complete, the plant can then do begin envrn inits when needed
 *   then set the component beginEnvrn flag to false
 *   but then every time it ISNT new envrn, just set it back to true
 *  after that just call simulate
 * 
 * to implement:
 *  start by changing the pipes module
 *   - make the pip class an inherited class from PlantComponent
 *   - extract out the get input stuff for a single construction into the instantiate function
 *   - extract out any one time and begin envrn inits into the perform* functions
 *   - extract out the rest of the simulation logic into the simulate function
 *   - create a function to check the array of objects for the current name and just return the shared pointer as needed
 */ 

#endif
