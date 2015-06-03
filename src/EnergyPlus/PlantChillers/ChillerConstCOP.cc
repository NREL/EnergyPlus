#include <memory>

#include <ObjexxFCL/Array1D.hh>

#include <PlantChillers/ChillerConstCOP.hh>
#include <PlantComponent.hh>
#include <PlantLocation.hh>

namespace EnergyPlus {

namespace PlantChillers {

	Array1D< std::shared_ptr< ChillerConstCOP > > ConstCOPChiller; // dimension to number of machines

	std::shared_ptr< PlantComponent >
	ChillerConstCOP::constCOPChillerFactory(
		int EP_UNUSED(objectType),
		std::string EP_UNUSED(objectName)
	) {
		return nullptr;
	}

	int ChillerConstCOP::performEveryTimeInit(){return 0;}
	int ChillerConstCOP::performOneTimeInit()
	{
		//this->TempEvapOutDesign = 0.0;
		//this->TempCondInDesign = 0.0;
		//this->performEveryTimeInit;
		return 0;
	}
	int ChillerConstCOP::performBeginEnvrnInit(){return 0;}
	int ChillerConstCOP::performFirstHVACInit(){return 0;}
	int ChillerConstCOP::simulate( const PlantLocation & EP_UNUSED(calledFromLocation) ){return 0;}
	int ChillerConstCOP::sizeChiller(){return 0;}
	int ChillerConstCOP::calcChiller(){return 0;}
	int ChillerConstCOP::updateChiller(){return 0;}
}

}
