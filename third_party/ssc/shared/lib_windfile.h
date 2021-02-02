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

#ifndef __lib_windfile_h
#define __lib_windfile_h

#include <string>
#include <fstream>
#include "lib_util.h"

class winddata_provider
{
public:
	enum { INVAL, 
		TEMP,  /* degrees Celsius */
		PRES,  /* atmospheres */
		SPEED, /* m/s */
		DIR  /* degrees */
	};

	winddata_provider();
	virtual ~winddata_provider();
	
	std::string city;
	std::string state;
	std::string locid;
	std::string country;
	std::string desc;
	int year;
	double lat;
	double lon;
	double elev;
	double measurementHeight;

	std::vector<int> types() { return m_dataid; }
	std::vector<double> heights() { return m_heights; }
	std::vector<double> relativeHumidity() { return m_relativeHumidity; }

	bool read( double requested_height,
		double *speed,
		double *direction,
		double *temperature,
		double *pressure,
		double *speed_meas_height,
		double *dir_meas_height,
		bool bInterpolate = false);
	
	virtual bool read_line( std::vector<double> &values ) = 0;
	virtual size_t nrecords() = 0;

	
	std::string error() { return m_errorMsg; }

protected:
	/// index of resource type (temp=1,pres=2,speed=3,dir=4) for each measurement height
	std::vector<int> m_dataid;	
	/// measurement height corresponding to each column header; same size as m_dataid
	std::vector<double> m_heights;
	std::vector<double> m_relativeHumidity;
	std::string m_errorMsg;
	
	bool find_closest( int& closest_index, int id, int ncols, double requested_height, int index_to_exclude = -1 );
	bool can_interpolate( int index1, int index2, int ncols, double requested_height );


};

class windfile : public winddata_provider
{
private:
  	std::ifstream m_ifs;
	std::string m_buf;
	std::string m_file;
	size_t m_nrec;

public:
	windfile();
	explicit windfile( const std::string &file );
	~windfile() override;

	bool ok();
	std::string filename();
	void close();
	bool open( const std::string &file );
	
	bool read_line(std::vector<double> &values) override;
	size_t nrecords() override;
	
};

#endif

