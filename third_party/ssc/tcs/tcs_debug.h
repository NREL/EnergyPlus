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

#include <stdio.h>
#include <string>
#include <vector>
#include "tcstype.h"

class stdfile
{
public:
	stdfile() : p(0) {  }
	stdfile(const char *file, const char *mode) { p = fopen(file, mode); }
	stdfile(const std::string &file, const char *mode) { p = fopen(file.c_str(), mode); }
//	stdfile(const wxString &file, const char *mode) { p = fopen((const char*)file.c_str(), mode); }
	~stdfile() { close(); }
	bool open(const char *file, const char *mode) { close(); p = fopen(file, mode); return ok(); }
	bool open(const std::string &file, const char *mode) { return open(file.c_str(), mode); }
	bool ok() { return 0 != p; }
	operator FILE*() const { return p; }
	void close() { if (p) ::fclose(p); p = 0; }
private:
	FILE *p;
};


static void debug_log_init_call(const char* unit_name, tcstypeinfo *type, std::vector<tcsvalue> &values)
{
	std::string fn ("C:\\Projects\\SAM\\Documentation\\CSP\\Linear Fresnel\\Molten Salt\\tcsdbg.txt");
	stdfile dbgout(fn, "a");

	fprintf(dbgout, "\n\n%s\n", unit_name);

	for (int i = 0; i < values.size(); i++)
	{
		fprintf(dbgout, "%s = ", type->variables[i].name);
		switch (values[i].type)
		{
		case TCS_NUMBER:
			fprintf(dbgout, "%lg\n", values[i].data.value);
			break;
		case TCS_ARRAY:
			fprintf(dbgout, "[ ");
			for (int j = 0; j < values[i].data.array.length-1; j++)
				fprintf(dbgout, "%lg, ", values[i].data.array.values[j]);
			fprintf(dbgout, "%lg ]\n", values[i].data.array.values[values[i].data.array.length - 1]);
			break;
		case TCS_MATRIX:
			fprintf(dbgout, "[\n[");
			for (int nr = 0; nr < values[i].data.matrix.nrows - 1; nr++)
			{
				for (int nc = 0; nc < values[i].data.matrix.ncols - 1; nc++)
					fprintf(dbgout, "%lg, ", values[i].data.matrix.values[nc + nr*values[i].data.matrix.ncols]);
				fprintf(dbgout, "%lg ],\n[", values[i].data.matrix.values[values[i].data.matrix.ncols - 1 + nr*values[i].data.matrix.ncols]);
			}
			for (int nc = 0; nc < values[i].data.matrix.ncols - 1; nc++)
				fprintf(dbgout, "%lg, ", values[i].data.matrix.values[nc + (values[i].data.matrix.nrows - 1 )* values[i].data.matrix.ncols]);
			fprintf(dbgout, "%lg ]\n]\n", values[i].data.matrix.values[values[i].data.matrix.ncols - 1 + (values[i].data.matrix.nrows - 1)*values[i].data.matrix.ncols]);
			break;
		case TCS_STRING:
			fprintf(dbgout, "%s\n", values[i].data.cstr);
			break;
		}
	}

	dbgout.close();
}
