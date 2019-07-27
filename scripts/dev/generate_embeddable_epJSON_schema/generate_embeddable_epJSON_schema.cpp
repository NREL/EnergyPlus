#include <iostream>
#include <fstream>
#include <cstdio>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

int main( int argc, char const *argv[] )
{
	if ( argc != 2 ) {
		std::cout << "usage: ./generate_embeddable_schema path/to/Energy+.schema.epJSON";
		return 1;
	}
	std::ifstream schema_stream( argv[ 1 ], std::ifstream::in );
	if ( !schema_stream.is_open() ) {
		std::cout << "schema file path " << argv[ 1 ] << " not found" << std::endl;
		return 1;
	}
	auto const input_json = json::parse( schema_stream );
	auto const v_cbor = json::to_cbor( input_json );

	printf("	const static std::array< std::uint8_t, %zu > embeddedSchema = {{\n", v_cbor.size() );

	for ( size_t i = 0; i < v_cbor.size(); ++i )
	{
		printf("0x%.2X,", v_cbor[ i ]);
		if ( i % 40 == 0 && i != 0 ) {
			printf("\n");
		}
	}
	printf("}};\n");
	return 0;
}
