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

#ifndef _IOUTIL_
#define _IOUTIL_ 1

#include <string>
#include "interop.h"
#include "definitions.h"

namespace ioutil
{
	/*This namespace contains all of the required utility functions for IO operations.
	-> File access
	-> File read/write
	*/
	
	//--File directory and name functions--
	bool file_exists( const char *file );
	bool dir_exists( const char *path );
	bool remove_file( const char *path );
	bool mkdir( const char *path, bool make_full = false); 
	std::string path_only( const std::string &path );
	std::string name_only( const std::string &path );
	std::string ext_only( const std::string &path );
	char path_separator();
	std::string get_cwd();
	bool set_cwd( const std::string &path );
	//--

	//--File reading functions--
	void read_chars( FILE *fp, std::string &text, int nchars=256);
	bool read_line( FILE *fp, std::string &text, int prealloc = 256 );
	void read_file( const std::string &fname, std::string &file, std::string &eol_marker);
    void parseXMLInputFile(const std::string &fname,var_map &V, parametric &par_data, optimization &opt_data);
	bool saveXMLInputFile(const std::string &fname, var_map &V, parametric &par_data, optimization &opt_data, const std::string &version);
	std::string getDelimiter(std::string &text);	//Return the delimiter separating the text
	//--
};

#endif