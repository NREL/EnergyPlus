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


#ifndef __dllinvoke_h
#define __dllinvoke_h

#include <string>
#include <exception>

/* note:
 local implementations of ssc_* functions
 forward dynamically to the .dll

 however, if the .dll is not loaded or if the 
 symbol cannot be addressed:
      EXCEPTIONS ARE THROWN of type sscdll_error
*/


class sscdll_error : public std::exception
{
public:
	sscdll_error(const std::string &s,
			const std::string &f) : text(s), func(f) { }
	virtual ~sscdll_error() throw() { }
	virtual const char *what() const noexcept override { return std::string( text + " " + func ).c_str(); }
	std::string text;
	std::string func;
};

// __SSCLINKAGECPP__ defined so that API functions are not declared extern "C".
// This allows C++ dynamic library re-implementation wrapper to throw exceptions
// for DLL not loaded and symbol address lookup errors
#define __SSCLINKAGECPP__ 1 
#include <sscapi.h>
#undef __SSCLINKAGECPP__


/* these functions do NOT throw exceptions */
bool sscdll_load( const char *path );
void sscdll_unload();
bool sscdll_isloaded();


/* include shared ssc code here */
#include <lib_util.h>
#include <vartab.h>

#endif
