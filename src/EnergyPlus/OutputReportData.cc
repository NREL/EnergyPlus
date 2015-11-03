// C++ Headers
#include <string>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <OutputReportData.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>

namespace EnergyPlus {

	AnnualFieldSet::AnnualFieldSet( std::string varName, AnnualFieldSet::AggregationKind kindOfAggregation, int numDigitsShown )
	{
		m_variMeter = varName;
		m_aggregate = kindOfAggregation;
		m_showDigits = numDigitsShown;
	}

	int
	AnnualFieldSet::getVariableKeyCountandTypeFromFldSt( int &typeVar, int &avgSumVar, int &stepTypeVar, std::string &unitsVar )
	{
		int numkeys;
		GetVariableKeyCountandType( m_variMeter, numkeys, typeVar, avgSumVar, stepTypeVar, unitsVar );  //call outputprocessor routine with member variable
		return numkeys;
	}

	void
	AnnualFieldSet::getVariableKeysFromFldSt( int &typeVar, int keyCount, std::vector<std::string> &namesOfKeys, std::vector<int>  &indexesForKeyVar )
	{
		// this hides the Objexx arrays and returns regular vectors
		Array1D_string tempNamesOfKeys;
		Array1D_int tempIndexesForKeyVar;
		tempNamesOfKeys.allocate( keyCount );
		tempIndexesForKeyVar.allocate( keyCount );
		GetVariableKeys( m_variMeter, typeVar, tempNamesOfKeys, tempIndexesForKeyVar ); //call outputprocessor routine with member variable
		namesOfKeys.clear();
		indexesForKeyVar.clear();
		for ( int iKey = 1; iKey <= keyCount; ++iKey ) {
			namesOfKeys.push_back( tempNamesOfKeys( iKey ) );
			indexesForKeyVar.push_back( tempIndexesForKeyVar( iKey ) );
		}
	}



} // EnergyPlus


