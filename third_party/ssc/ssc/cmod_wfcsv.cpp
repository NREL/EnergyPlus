/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "core.h"
#include <iostream>
#include "lib_weatherfile.h"

static var_info _cm_vtab_wfcsvconv[] = 
{	
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP                     REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "input_file",               "Input weather file name",         "",       "tmy2,tmy3,intl,epw,smw",      "Weather File Converter", "*",                       "",                     "" },
	{ SSC_INOUT,        SSC_STRING,      "output_file",              "Output file name",                "",       "",                       "Weather File Converter", "?",                       "",                     "" },
	{ SSC_INPUT,        SSC_STRING,      "output_folder",            "Output folder",                   "",       "",                       "Weather File Converter", "?",                       "",                     "" },
	{ SSC_INPUT,        SSC_STRING,      "output_filename_format",   "Output file name format",         "",       "recognizes $city $state $country $type $loc",         "Weather File Converter", "?",                       "",                     "" },

var_info_invalid };

class cm_wfcsvconv : public compute_module
{
private:
public:
	cm_wfcsvconv()
	{
		add_var_info( _cm_vtab_wfcsvconv );
	}

	void exec( )
	{
		std::string input = as_string("input_file");


		if ( is_assigned("output_file") )
		{
			std::string output = as_string("output_file");
			if (!weatherfile::convert_to_wfcsv( input, output ))
				throw exec_error( "wfcsvconv", "could not convert " + input + " to " + output );
		}
		else
		{
			weatherfile wfile( input, true );
			if ( !wfile.ok() ) throw exec_error("wfcsvconv", "could not read input file: " + input );

			weather_header hdr;
			wfile.header( &hdr );

			std::string state = hdr.state;
			std::string city = weatherfile::normalize_city( hdr.city );
			
			std::string country = hdr.country;

			std::string loc = hdr.location;
			std::string type = "?";


			switch( wfile.type() )
			{
			case weatherfile::TMY2: type = "TMY2"; 
				if ( country.empty() ) country = "None";
				break;
			case weatherfile::TMY3: type = "TMY3"; 
				if ( country.empty() ) country = "None";
				break;
			case weatherfile::EPW: type = "EPW"; break;
			case weatherfile::SMW: type = "SMW"; break;
			}

			
			if ( !country.empty() ) country += " ";

			std::string ofmt = "$country $state $city ($type)";
			if ( is_assigned("output_file_format") )
				ofmt = as_string("output_filename_format");

			std::string folder = util::path_only(input);
			if ( is_assigned( "output_folder") )
				folder = as_string( "output_folder" );
			std::string output = folder + "/" + ofmt;

			util::replace( output, "$city", city );
			util::replace( output, "$state", state );
			util::replace( output, "$country ", country );
			util::replace( output, "$loc", loc );
			util::replace( output, "$type", type );
			
			// remove any fishy characters from output file name that
			// might have been read in from the header data in the csv file
			static const char illegal[] = { '?', '*', '#', '$', '%', '{' , '}', '<', '>', '!', ';', '@', '|', '`', '+', 0 };
			int i=0;
			char buf[2];
			buf[1] = 0;
			while( illegal[i] != 0 )
			{
				buf[0] = illegal[i++];
				util::replace( output, buf, "" );
			}

			if ( util::ext_only( output ) != "csv" )
				output += ".csv";
			
			if (!weatherfile::convert_to_wfcsv( input, output ))
				throw exec_error( "wfcsvconv", "could not convert " + input + " to " + output );

			assign( "output_file", var_data( output ) );
		}
	}
};

DEFINE_MODULE_ENTRY( wfcsvconv, "Converter for TMY2, TMY3, INTL, EPW, SMW weather files to standard CSV format", 1 )
