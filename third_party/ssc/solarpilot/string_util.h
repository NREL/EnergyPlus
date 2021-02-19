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

#ifndef _STRING_UTIL_
#define _STRING_UTIL_ 1

#include <string>
#include <vector>

//string and data handling methods
extern std::vector< std::string > split( const std::string &str, const std::string &delim, bool ret_empty=false, bool ret_delim=false );
extern std::string join( const std::vector< std::string > &list, const std::string &delim );
		
extern bool to_integer(const std::string &str, int *x);
extern bool to_float(const std::string &str, float *x);
extern bool to_double(const std::string &str, double *x);
extern bool to_bool(const std::string &str, bool &x);
		
extern std::string to_string( int x, const char *fmt="%d" );
extern std::string to_string( double x, const char *fmt="%lg" );


extern std::string lower_case( const std::string &in );
extern std::string upper_case( const std::string &in );

extern std::string ReplaceString(std::string subject, const std::string &search, const std::string &replace);
extern void ReplaceStringInPlace(std::string &subject, const std::string &search, const std::string &replace);


//------



#endif