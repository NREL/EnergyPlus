#include <iostream>
#include <fstream>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

int main( int argc, char const *argv[] )
{
	if ( argc != 2 ) {
		std::cout << "usage: ./generate_embeddable_jdd path/to/Energy+.jdd";
		return 1;
	}
	std::ifstream jdd_stream( argv[ 1 ], std::ifstream::in );
	if ( !jdd_stream.is_open() ) {
		std::cout << "JDD file path " << argv[ 1 ] << " not found" << std::endl;
		return 1;
	}
	auto const input_json = json::parse( jdd_stream );
	auto const v_cbor = json::to_cbor( input_json );
	if ( v_cbor.size() ) std::cout << v_cbor[ 0 ] << ",";
	for ( size_t i = 1; i < v_cbor.size(); ++i )
	{
		if ( i % 40 == 0 ) {
			std::cout << v_cbor[ i ] << "\n";
		} else {
			std::cout << v_cbor[ i ] << ",";
		}
	}
	std::cout << std::flush;
	return 0;
}
