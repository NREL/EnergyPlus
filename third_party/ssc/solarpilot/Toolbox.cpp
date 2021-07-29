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

#include "math.h"
#include <stdio.h>
#include <ctime>
#include <vector>
#include <algorithm>

#include "Toolbox.h"
#include "definitions.h"

using namespace std;

// ----------- points and vectors -----------------------
sp_point::sp_point(){};

sp_point::sp_point( const sp_point &P )
    : x(P.x), y(P.y), z(P.z)
{
}

sp_point::sp_point(double X, double Y, double Z)
    : x(X), y(Y), z(Z)
{
}

void sp_point::Set(double _x, double _y, double _z)
{
	this->x = _x; 
	this->y = _y; 
	this->z = _z;
}

void sp_point::Set( sp_point &P )
{
	this->x = P.x; 
	this->y = P.y;
	this->z = P.z;
}

void sp_point::Add( sp_point &P )
{
	this->x+=P.x; 
	this->y+=P.y; 
	this->z+=P.z;
}

void sp_point::Add(double _x, double _y, double _z)
{
	this->x += _x; 
	this->y += _y; 
	this->z += _z;
}

void sp_point::Subtract( sp_point &P )
{
	this->x += -P.x; 
	this->y += -P.y; 
	this->z += -P.z;
}

double& sp_point::operator [](const int &index){

    switch (index)
    {
    case 0:
        return x;
        break;
    case 1:
        return y;
        break;
    case 2:
        return z;
        break;
    default:
        throw spexception("Index out of range in sp_point()");
        break;
    }
};

bool sp_point::operator <(const sp_point &p) const {
	return this->x < p.x || (this->x == p.x && this->y < p.y);
};

Vect::Vect(){};

Vect::Vect( const Vect &V )
    : i(V.i), j(V.j), k(V.k)
{}

Vect::Vect(double i, double j, double k )
    : i(i), j(j), k(k)
{
}

void Vect::Set(double _i, double _j, double _k)
{
	i=_i; 
	j=_j; 
	k=_k;
}

void Vect::Set( Vect &V )
{
	i = V.i; 
	j = V.j; 
	k = V.k;
}

void Vect::Add( Vect &V )
{
	i+=V.i; 
	j+=V.j; 
	k+=V.k;
}

void Vect::Subtract( Vect &V )
{ 
	i+=-V.i; 
	j+=-V.j; 
	k+=-V.k;
}

void Vect::Add(double _i, double _j, double _k)
{
	i+=_i; 
	j+=_j; 
	k+=_k;
}

void Vect::Scale( double m )
{
    i *= m;
    j *= m;
    k *= m;
}

double &Vect::operator[](int index){
	if( index == 0) return i;
	if( index == 1) return j;
	if( index == 2) return k;
	throw spexception("Index out of range in Vect()"); //range error
};


	
PointVect::PointVect(const PointVect &v){
	x = v.x; y=v.y; z=v.z;
	i = v.i; j=v.j; k=v.k;
}
PointVect& PointVect::operator= (const PointVect &v) {
	x = v.x; y=v.y; z=v.z;
	i = v.i; j=v.j; k=v.k;
	return *this;
}
PointVect::PointVect(double px, double py, double pz, double vi, double vj, double vk) 
{
	x=px; y=py; z=pz; i=vi; j=vj; k=vk;
}

sp_point *PointVect::point()
{
	p.Set(x, y, z); 
	return &p;
};

Vect *PointVect::vect()
{
	v.Set(i, j, k); 
	return &v;
};
//-------------------------------------------------------

//------------------ DTObj class -------------------------------------
DTobj::DTobj()
{ 
	setZero();
}
	
void DTobj::setZero()
{
	_year=0;  
	_month=0;  
	_yday=0; 
	_mday=0; 
	_wday=0;  
	_hour=0;
	_min=0;  
	_sec=0;  
	_ms=0;
}
	
DTobj *DTobj::Now()
{
	struct tm now;
#ifdef _MSC_VER
	__time64_t long_time;
	// Get time as 64-bit integer.
	_time64( &long_time ); 
	// Convert to local time.
	_localtime64_s( &now, &long_time ); 
#else
	time_t long_time;
	// Get time as 64-bit integer.
	time( &long_time ); 
	// Convert to local time.
	now = *localtime(&long_time);
#endif
	_year=now.tm_year+1900;  
	_month=now.tm_mon;  
	_yday=now.tm_yday;  
	_mday=now.tm_mday;
	_wday=now.tm_wday; 
	_hour=now.tm_hour;
	_min=now.tm_min;  
	_sec=now.tm_sec;  
	_ms=0;

	return this;
}
//-------------------------------------------------------


//-----------------------DateTime class-------------------
//accessors
	//GETS
int DateTime::GetYear(){return _year;}
int DateTime::GetMonth(){return _month;}
int DateTime::GetYearDay(){return _yday;};
int DateTime::GetMonthDay(){return _mday;}
int DateTime::GetWeekDay(){return _wday;}
int DateTime::GetMinute(){return _min;};
int DateTime::GetSecond() {return _sec;};
int DateTime::GetMillisecond(){return _ms;};
// SETS
void DateTime::SetYear(const int val){_year=val; SetMonthLengths(_year);}
void DateTime::SetMonth(const int val){_month=val;}
void DateTime::SetYearDay(const int val){_yday=val;}
void DateTime::SetMonthDay(const int val){_mday=val;}
void DateTime::SetWeekDay(const int val){_wday=val;}
void DateTime::SetHour(const int val){_hour=val;}
void DateTime::SetMinute(const int val){_min=val;}
void DateTime::SetSecond(const int val){_sec=val;}
void DateTime::SetMillisecond(const int val){_ms=val;}

//Default constructor
DateTime::DateTime(){
	//Initialize all variables to the current date and time
	setDefaults();
}

DateTime::DateTime(double doy, double hour){
	//Convert from fractional hours to integer values for hr, min, sec
	int hr = int(floor(hour));
	double minutes = 60. * (hour - double(hr));
	int min = int(floor(minutes));
	int sec = int(60.*(minutes - double(min)));
	
	setZero();	//Zero values
	SetYearDay(int(doy+.001));	//day of the year
	SetHour(hr);
	SetMinute(min);
	SetSecond(sec);
}

//Fully specified constructor
DateTime::DateTime(DTobj &DT)
{
	_year=DT._year; _month=DT._month; _yday=DT._yday; _mday=DT._mday; _wday=DT._wday;
	_hour=DT._hour; _min=DT._min; _sec=DT._sec; _ms=DT._ms;
	//Initialize the month lengths array
	SetMonthLengths(_year);
}

void DateTime::setDefaults(){
	setZero();	//everything zeros but the day of year and year (hr=12 is noon)
	//Get the current year and set it as the default value
	DTobj N;	
	N.Now();
	//SetYear(N._year);
	SetYear(2011);	//Choose a non-leap year
	SetMonth(6);
	SetMonthDay(21);
	SetYearDay( GetDayOfYear() );	//Summer solstice
	SetHour(12);
	
}

void DateTime::SetDate(int year, int month, int day)
{
	//Given a year, month (1-12), and day of the month (1-N), set the values. Leave the time as it is
	SetYear(year); 
	SetMonth(month); 
	SetMonthDay(day); 
	SetMonthLengths(_year);
	SetYearDay( GetDayOfYear(year, month, day) );
}

void DateTime::SetMonthLengths(const int year){
	/* Calculate the number of days in each month and assign the result to the monthLength[] array.
	Provide special handling for 4-year, 100-year, and 400-year leap years.*/
	//Initialize the array of month lengths (accounting for leap year)
	for (int i=0; i<12; i+=2){ monthLength[i]=31; };
	for (int i=1; i<12; i+=2){ monthLength[i]=30; };
	//Given the year, determine how many days are in Feb.
	monthLength[1]=28; //Default
	if(year%4==0) {monthLength[1]=29;} //All years divisible by 4 are leap years
	if(year%100==0){ //All years divisible by 100 are not leap years, unless...
		if(year%400 == 0){monthLength[1]=29;} //.. the year is also divisible by 400.
		else {monthLength[1]=28;} 
	};
}

int DateTime::GetDayOfYear(){
	int val = GetDayOfYear(_year, _month, _mday);
	return val;
}

int DateTime::GetDayOfYear(int /*year*/, int month, int mday){
	//Return the day of the year specified by the class day/month/year class members
	//Day of the year runs from 1-365

	int doy=0;
 	if(month>1) { for (int i=0; i<month-1; i++) {doy+=monthLength[i];}; };
	doy+= mday; 
	return doy;
}

int DateTime::CalculateDayOfYear( int year, int month, int mday ){	//Static version
	
	int monthLength[12];
	//Initialize the array of month lengths (accounting for leap year)
	for (int i=0; i<12; i+=2){ monthLength[i]=31; };
	for (int i=1; i<12; i+=2){ monthLength[i]=30; };
	//Given the year, determine how many days are in Feb.
	monthLength[1]=28; //Default
	if(year%4==0) {monthLength[1]=29;} //All years divisible by 4 are leap years
	if(year%100==0){ //All years divisible by 100 are not leap years, unless...
		if(year%400 == 0){monthLength[1]=29;} //.. the year is also divisible by 400.
		else {monthLength[1]=28;} 
	};
	int doy=0;
 	if(month>1) { for (int i=0; i<month-1; i++) {doy+=monthLength[i];}; };
	doy+= mday; 
	return doy;

}

int DateTime::GetHourOfYear(){
//Return the hour of the year according to the values specified by the class members
	int doy = GetDayOfYear();
	int hr = (doy-1)*24 + _hour;
	return hr;
};

void DateTime::hours_to_date(double hours, int &month, int &day_of_month){
	/*
	Take hour of the year (0-8759) and convert it to month and day of the month. 
	If the year is not provided, the default is 2011 (no leap year)
	Month = 1-12
	Day = 1-365

	The monthLength[] array contains the number of days in each month. 
	This array is taken from the DateTime class and accounts for leap years.
	To modify the year to not include leap year days, change the year in the
	DateTime instance.

	*/
	double days = hours/24.;
	int dsum=0; 
	for(int i=0; i<12; i++){
		dsum += monthLength[i];
		if(days <= dsum){month = i+1; break;}
	}
	day_of_month = (int)floor(days - (dsum - monthLength[month-1]))+1;

}

std::string DateTime::GetMonthName(int month){
	switch(month)
	{
	case 1:
		return "January";
	case 2:
		return "February";
	case 3:
		return "March";
	case 4:
		return "April";
	case 5:
		return "May";
	case 6:
		return "June";
	case 7:
		return "July";
	case 8:
		return "August";
	case 9:
		return "September";
	case 10:
		return "October";
	case 11:
		return "November";
	case 12:
		return "December";
	default:
		return "";
	}
}


//---------Weather data object class --------------------
//Copy constructor
WeatherData::WeatherData( const WeatherData &wd) : 
	_N_items( wd._N_items ),
	Day( wd.Day ),
	Hour( wd.Hour ),
	Month( wd.Month ),
	DNI( wd.DNI ),
	T_db( wd.T_db ),
	Pres( wd.Pres ),
	V_wind( wd.V_wind ),
	Step_weight( wd.Step_weight )
{
	//Recreate the pointer array
	initPointers();
}

WeatherData::WeatherData(){	
	initPointers();
};

void WeatherData::initPointers(){
	//On initialization, create a vector of pointers to all of the data arrays
	v_ptrs.resize(8); 
	v_ptrs.at(0) = &Day;
	v_ptrs.at(1) = &Hour;
	v_ptrs.at(2) = &Month;
	v_ptrs.at(3) = &DNI;
	v_ptrs.at(4) = &T_db;
	v_ptrs.at(5) = &Pres;
	v_ptrs.at(6) = &V_wind;
	v_ptrs.at(7) = &Step_weight;
	//Initialize the number of items
	_N_items = (int)Day.size();
}

void WeatherData::resizeAll(int size, double val){ 
	for(unsigned int i=0; i<v_ptrs.size(); i++){
		v_ptrs.at(i)->resize(size, val); 
		_N_items = size;
	}
};

void WeatherData::clear()
{
	for(unsigned int i=0; i<v_ptrs.size(); i++){
		v_ptrs.at(i)->clear();
		_N_items = 0;
	}
}

void WeatherData::getStep(int step, double &day, double &hour, double &dni, double &step_weight){
	//retrieve weather data from the desired time step
	day = Day.at(step);
	hour = Hour.at(step);
	dni = DNI.at(step);
	step_weight = Step_weight.at(step);
}

void WeatherData::getStep(int step, double &day, double &hour, double &month, double &dni, 
	double &tdb, double &pres, double &vwind, double &step_weight){
	double *args[] = {&day, &hour, &month, &dni, &tdb, &pres, &vwind, &step_weight};
	
	//retrieve weather data from the desired time step
	for(unsigned int i=0; i<v_ptrs.size(); i++){
		*args[i] = v_ptrs.at(i)->at(step);
	}
}

void WeatherData::append(double day, double hour, double dni, double step_weight){
	Day.push_back(day);
	Hour.push_back( hour );
	DNI.push_back( dni );
	Step_weight.push_back( step_weight );
    _N_items ++;
}

void WeatherData::append(double day, double hour, double month, double dni, 
	double tdb, double pres, double vwind, double step_weight){
	Day.push_back( day );
	Hour.push_back( hour );
	Month.push_back( month );
	DNI.push_back( dni );
	T_db.push_back( tdb );
	Pres.push_back( pres );
	V_wind.push_back( vwind );
	Step_weight.push_back( step_weight );
    _N_items ++;
}

void WeatherData::setStep(int step, double day, double hour, double dni, double step_weight){
	Day.at(step) = day; 
	Hour.at(step) = hour;
	DNI.at(step) = dni;
	Step_weight.at(step) = step_weight;
}

void WeatherData::setStep(int step, double day, double hour, double month, double dni, 
	double tdb, double pres, double vwind, double step_weight){
	Day.at(step) = day;
	Hour.at(step) = hour;
	Month.at(step) = month;
	DNI.at(step) = dni;
	T_db.at(step) = tdb;
	Pres.at(step) = pres;
	V_wind.at(step) = vwind;
	Step_weight.at(step) = step_weight;
}

std::vector<std::vector<double>*> *WeatherData::getEntryPointers()
{
	return &v_ptrs;
}

//-----------------------Toolbox namespace--------------------

//misc
double Toolbox::round(double x){
	return fabs(x - ceil(x)) > 0.5 ? floor(x) : ceil(x);
}

void Toolbox::writeMatD(string dir, string name, matrix_t<double> &mat, bool clear){
	//write the data to a log file
	FILE *file;
	
	//ofstream file; 
	string path;
	path.append(dir);
	path.append("\\matrix_data_log.txt");


	if( clear ) {
		file = fopen(path.c_str(), "w");
	}
	else{
		file =	fopen(path.c_str(), "a");
	}

	int nr = (int)mat.nrows();
	int nc = (int)mat.ncols();
	
	fprintf(file, "%s\n", name.c_str());
	
	for (int i=0; i<nr; i++){
		for (int j=0; j<nc; j++){
			fprintf(file, "%e\t", mat.at(i,j));
		}
		fprintf(file, "%s", "\n");
	}
	fprintf(file, "%s", "\n");

	fclose(file);
}

void Toolbox::writeMatD(string dir, string name, block_t<double> &mat, bool clear){
	//write the data to a log file
	FILE *file;
	
	//ofstream file; 
	string path;
	path.append(dir);
	path.append("\\matrix_data_log.txt");


	if( clear ) {
		file =fopen(path.c_str(), "w");
	}
	else{
		file =fopen(path.c_str(), "a");
	}

	int nr = (int)mat.nrows();
	int nc = (int)mat.ncols();
	int nl = (int)mat.nlayers();

	fprintf(file, "%s\n", name.c_str());
	
	for (int k=0; k<nl; k++){
		fprintf(file, "%i%s", k, "--\n");
		for (int i=0; i<nr; i++){
			for (int j=0; j<nc; j++){
				fprintf(file, "%e\t", mat.at(i,j,k));
			}
			fprintf(file, "%s", "\n");
		}
	}
	fprintf(file, "%s", "\n");

	fclose(file);
}

void Toolbox::swap(double &a, double &b){
	//Swap the values in A and B
	double xt = a;
	a = b;
	b = xt;
}

void Toolbox::swap(double *a, double *b){
	double xt = *a;
	*a = *b;
	*b = xt;
}

double Toolbox::atan3(double &x, double &y){
	double v = atan2(x,y); 
	return v < 0. ? v += 2.*PI : v; 
}

void Toolbox::map_profiles(double *source, int nsource, double *dest, int ndest, double *weights){
	/* 
	Take a source array 'source(nsource)' and map the values to 'dest(ndest)'. 
	This method creates an integral-conserved map of values in 'dest' that may have a
	different number of elements than 'source'. The size of each node within 'source' 
	is optionally specified by the 'weights(nsource)' array. If all elements are of the
	same size, set weights=(double*)NULL or omit the optional argument.
	*/

	double *wsize;
	double wtot = 0.;
	if(weights != (double*)NULL){
		wsize = new double[nsource];
		for(int i=0; i<nsource; i++){
			wtot += weights[i]; //sum up weights		
			wsize[i] = weights[i];
		}
	}
	else{
		wsize = new double[nsource];
		for(int i=0; i<nsource; i++)
			wsize[i] = 1.;
		wtot = (double)nsource;
	}

	double delta_D = wtot/(double)ndest;

	int i=0;
	double ix = 0.;

	for(int j=0; j<ndest; j++){
		dest[j] = 0.;
		double 
			jx = (double)(j+1)*delta_D,
			jx0 = (double)j*delta_D;
		//From previous step
		if(ix-jx0>0)
			dest[j] += (ix-jx0)*source[i-1];
		//internal steps
		while(ix < jx ){
			ix += wsize[i];
			dest[j] += wsize[i] * source[i];
			i++;
		}
		//subtract overage
		if(ix > jx )
			dest[j] += -(ix-jx)*source[i-1];
		//Divide by length
		dest[j] *= 1./delta_D;
	}
	

	//Memory cleanup
	delete [] wsize;
	
}


/* Factorial of an integer x*/
int Toolbox::factorial(int x) {
	//if (floor(3.2)!=x) { cout << "Factorial must be an integer!" };
    
    int f = x;
    for (int i=1; i<x; i++) {
        int j=x-i;
        f=f*j;
	}
	if(f<1) f=1;	//Minimum of any factorial is 1
    return f;
}

double Toolbox::factorial_d(int x) {
	//returns the same as factorial, but with a doub value
	return double(factorial(x));
}

bool Toolbox::pointInPolygon( const vector< sp_point > &poly, const sp_point &pt) {
	/* This subroutine takes a polynomial array containing L_poly vertices (X,Y,Z) and a 
	single point (X,Y,Z) and determines whether the point lies within the polygon. If so, 
	the algorithm returns TRUE (otherwise FALSE) */
	
	//if polywind returns a number between -1 and 1, the point is in the polygon
	int wind = polywind(poly, pt);
	if (wind == -1 || wind == 1) { return true;}
	else {return false;}
}

vector<sp_point> Toolbox::projectPolygon(vector<sp_point> &poly, PointVect &plane) {
	/* Take a polygon with points in three dimensions (X,Y,Z) and project all points onto a plane defined 
	by the point-vector {x,y,z,i,j,k}. The subroutine returns a new polygon with the adjusted points all
	lying on the plane. The points are also assigned vector values corresponding to the normal vector
	of the plane that they lie in.*/

	//Declare variables
	double dist, A, B, C, D;
	sp_point pt; 

	//Declare a new polygon of type vector
	int Lpoly = (int)poly.size(); 
	vector< sp_point > FPoly(Lpoly);
	

	//Determine the coefficients for the equation of the plane {A,B,C,D}
	A=plane.i; B=plane.j; C=plane.k;
	Vect uplane; uplane.Set(A,B,C); vectmag(uplane);
	D = -A*plane.x - B*plane.y - C*plane.z;

	for (int i=0; i<Lpoly; i++) {
		//Determine the distance between the point and the plane
		pt = poly.at(i);
		dist = -(A*pt.x + B*pt.y + C*pt.z + D)/vectmag(*plane.vect());
		//knowing the distance, now shift the point to the plane
		FPoly.at(i).x = pt.x+dist*A;
		FPoly.at(i).y = pt.y+dist*B;
		FPoly.at(i).z = pt.z+dist*C;
		
	}
	return FPoly;
}

int Toolbox::polywind( const vector<sp_point> &vt, const sp_point &pt) {
	/*
	Determine the winding number of a polygon with respect to a point. This helps
	calculate the inclusion/exclusion of the point inside the polygon. If the point is
	inside the polygon, the winding number is equal to -1 or 1.
	*/

	//Declarations
	int i, np, wind = 0, which_ign;
	double 
        d0=0., 
        d1=0., 
        p0=0., 
        p1=0., 
        pt0=0., 
        pt1=0.; 
	
	/*The 2D polywind function can be mapped to 3D polygons by choosing a single dimension to 
	ignore. The best ignored dimension corresponds to the largest magnitude component of the
	normal vector to the plane containing the polygon.*/
	//Get the cross product of the first three points in the polygon to determine the planar normal vector
	Vect v1, v2;
	v1.Set((vt.at(0).x-vt.at(1).x),(vt.at(0).y - vt.at(1).y),(vt.at(0).z - vt.at(1).z));
	v2.Set((vt.at(2).x-vt.at(1).x),(vt.at(2).y - vt.at(1).y),(vt.at(2).z - vt.at(1).z));
	Vect pn = crossprod( v1, v2 );

	which_ign = 1;
	if(fabs(pn.j) > fabs(pn.i)) {which_ign=1;}
	if(fabs(pn.k) > fabs(pn.j)) {which_ign=2;}
	if(fabs(pn.i) > fabs(pn.k)) {which_ign=0;}

	/* Return the winding number of a polygon (specified by a vector of vertex points vt) 
	around an arbitrary point pt.*/
	np = (int)vt.size();
	switch (which_ign) {
	case 0:
		pt0 = pt.y; pt1 = pt.z;
		p0 = vt[np-1].y; p1 = vt[np-1].z;
		break;
	case 1:
		pt0 = pt.x; pt1 = pt.z;
		p0 = vt[np-1].x; p1 = vt[np-1].z;
		break;
	case 2:
		pt0 = pt.x; pt1 = pt.y;
		p0 = vt[np-1].x; p1 = vt[np-1].y;
	}

	for (i=0; i<np; i++) {
		switch (which_ign) {
		case 0:
			d0 = vt[i].y; d1 = vt[i].z;
			break;
		case 1:
			d0 = vt[i].x; d1 = vt[i].z;
			break;
		case 2:
			d0 = vt[i].x; d1 = vt[i].y;
		}

		if (p1 <= pt1) {
			if (d1 > pt1 && (p0-pt0)*(d1-pt1)-(p1-pt1)*(d0-pt0) > 0) wind++;
		}
		else {
			if (d1 <= pt1 && (p0-pt0)*(d1-pt1)-(p1-pt1)*(d0-pt0) < 0) wind--;
		}
		p0=d0;
		p1=d1;
	}
	return wind;
}

Vect Toolbox::crossprod(const Vect &A, const Vect &B) {
	/* Calculate the cross product of two vectors. The magnitude of the cross product
	represents the area contained in a parallelogram bounded by the multipled vectors.*/
	Vect res; 
	res.i = A.j*B.k - A.k*B.j;
	res.j = A.k*B.i - A.i*B.k;
	res.k = A.i*B.j - A.j*B.i;
	return res;
};

double Toolbox::crossprod(const sp_point &O, const sp_point &A, const sp_point &B)
{
	//2D cross-product of vectors OA and OB. 
	return (A.x - O.x) * (B.y - O.y) - (A.y - O.y) * (B.x - O.x);

}

void Toolbox::unitvect(Vect &A) {
	/*Take a vector that may or may not be in unit vector form and scale the magnitude to 
	make it a unit vector*/
	double M = vectmag(A);
	if(M==0.){A.i=0; A.j=0; A.k=0; return;}
	A.i /= M; A.j /= M; A.k /= M; return;
}

double Toolbox::dotprod(const Vect &A, const Vect &B)
{
	return (A.i * B.i + A.j * B.j + A.k * B.k); 
}

double Toolbox::dotprod(const Vect &A, const sp_point &B)
{
	return (A.i * B.x + A.j * B.y + A.k * B.z);
}

double Toolbox::vectmag(const Vect &A) 
{
	return sqrt(pow(A.i,2) + pow(A.j,2) + pow(A.k,2));
}

double Toolbox::vectmag(const sp_point &P){
	return sqrt( pow(P.x,2) + pow(P.y,2) + pow(P.z,3) );
}

double Toolbox::vectmag(double i, double j, double k){
	return sqrt( pow(i,2) + pow(j,2) + pow(k,2) );
}

double Toolbox::vectangle(const Vect &A, const Vect&B) 
{
	//Determine the angle between two vectors
	return acos(dotprod(A,B)/(vectmag(A)*vectmag(B)));
}

void Toolbox::rotation(double theta, int axis, Vect &V){
	sp_point p;
	p.Set(V.i, V.j, V.k);
	Toolbox::rotation(theta, axis, p);
	V.Set(p.x, p.y, p.z);
}

void Toolbox::rotation(double theta, int axis, sp_point &P){
	/*
	This method takes a point, a rotation angle, and the axis of rotation and 
	rotates the point about the origin in the specified direction. 

	The inputs are:
	theta	| [rad]	| Angle of rotation
	axis	| none	| X=0, Y=1, Z=2 : Axis to rotate about

	The method returns a modified point "P" that has been rotated according to the inputs.

	Rotation is clockwise about each axis (left hand rule). In other words, 
	positive rotation for each axis is defined by the apparent motion when positive end 
	of the axis points toward the viewer. 
	*/

	//the 3x3 rotation matrix
    double MR0i, MR0j, MR0k, MR1i, MR1j, MR1k, MR2i, MR2j, MR2k;

	double costheta = cos(theta);
	double sintheta = sin(theta);
       
    switch(axis)
	{
		/*
		The rotation vectors are entered in as the transverse for convenience of multiplication.
		The correct matrix form for each are:

		X axis
		[1,					0,				0,
		 0,				cos(theta),		-sin(theta),
		 0,				sin(theta),		cos(theta)]

		Y axis
		[cos(theta),		0,			sin(theta),
			0,				1,				0,
		 -sin(theta),		0,			cos(theta)]

		Z axis
		[cos(theta),	-sin(theta),		0,
		 sin(theta),	cos(theta),			0,
			0,				0,				1	]

		*/
	case 0:	//X axis
		//Fill in the x-rotation matrix for this angle theta
        MR0i = 1; MR0j = 0; MR0k = 0;
		MR1i = 0; MR1j = costheta; MR1k = sintheta; 
        MR2i = 0; MR2j = -sintheta; MR2k = costheta;
		break;
	case 1:	//Y axis
		//Fill in the y-rotation matrix for this angle theta
        MR0i = costheta; MR0j = 0; MR0k = -sintheta;
        MR1i = 0; MR1j = 1; MR1k = 0;
        MR2i = sintheta; MR2j = 0; MR2k = costheta;
		break;
	case 2:	//Z axis
		//Fill in the z-rotation matrix for this angle theta
        MR0i = costheta; MR0j = sintheta; MR0k = 0;
		MR1i = -sintheta; MR1j = costheta; MR1k = 0;
		MR2i = 0; MR2j = 0; MR2k = 1;
		break;
	default:
		throw spexception("Internal error: invalid axis number specified in rotation() method.");
	}

	//Create a copy of the point
	double Pcx = P.x;
    double Pcy = P.y; 
    double Pcz = P.z;

	//Do the rotation. The A matrix is the rotation vector and the B matrix is the point vector
	P.x = MR0i*Pcx + MR0j*Pcy + MR0k*Pcz; //dotprod(MR0, Pc);        //do the dotprod's here to avoid function call
	P.y = MR1i*Pcx + MR1j*Pcy + MR1k*Pcz; //dotprod(MR1, Pc);
	P.z = MR2i*Pcx + MR2j*Pcy + MR2k*Pcz; //dotprod(MR2, Pc);
	return;

}

bool Toolbox::plane_intersect(sp_point &P, Vect &N, sp_point &C, Vect &L, sp_point &Int){
	/*
	Determine the intersection point of a line and a plane. The plane is 
	defined by:
	P	| sp_point on the plane
	N	| Normal unit vector to the plane
	The line/vector is defined by:
	C	| (x,y,z) coordinate of the beginning of the line
	L	| Unit vector pointing along the line

	The distance 'd' along the unit vector is given by the equation:
	d = ((P - C) dot N)/(L dot N)

	The method fills in the values of a point "Int" that is the intersection. 

	In the case that the vector does not intersect with the plane, the method returns FALSE and
	the point Int is not modified. If an intersection is found, the method will return TRUE.
	*/

	double PC[3], LdN, PCdN, d;
	int i;
	//Create a vector between P and C
	for(i=0; i<3; i++){PC[i] = P[i]-C[i];}
	//Calculate the dot product of L and N
	LdN = 0.;
	for(i=0; i<3; i++){
		LdN += L[i]*N[i];
	}
	//Calculate the dot product of (P-C) and N
	PCdN = 0.;
	for(i=0; i<3; i++){
		PCdN += PC[i]*N[i];
	}

	if(LdN == 0.) return false; //Line is parallel to the plane
	d = PCdN / LdN;	//Multiplier on unit vector that intersects the plane
		
	//Calculate the coordinates of intersection
	Int.x = C.x + d*L.i;
	Int.y = C.y + d*L.j;
	Int.z = C.z + d*L.k;

	return true;

}

bool Toolbox::line_norm_intersect(sp_point &line_p0, sp_point &line_p1, sp_point &P, sp_point &I, double &rad){
	/* 
	Note: 2D implementation (no Z component)

	Given two points that form a line segment (line_p0 and line_p1) and an external point 
	NOT on the line (P), return the location of the intersection (I) between a second line
	that is normal to the first and passes through P. Also return the corresponding radius 'rad'
	||P I||. 

    (line_p0)       (I)                 (line_p1)
	O--------------X----------------------------O
                   |_|
                   |
                   |
                   O
                    (P)

	If the normal to the line segment lies outside of the segment, the method returns 
	FALSE, otherwise its TRUE. In the first case, 'I' is equal to the segment point closest 
	to 'P', otherwise it is the intersection point.

	Solve for 'I' using the derivative of the distance formula (r(x,y)) between I and P. I is the point
	where dr(x,y)/dx = 0.

	*/

	//Check to see if the 'x' components of p0 and p1 are the same, which is undefined slope.
	if(line_p0.x == line_p1.x){
		//check for containment
		double Iyr = (P.y - line_p0.y)/(line_p1.y - line_p0.y);
		if(Iyr < 0.){	//out of bounds on the p0 side
			I.Set(line_p0.x, line_p0.y, 0.);
			rad = vectmag(I.x - P.x, I.y - P.y, 0.);
			return false;
		}
		else if(Iyr > 1.){	//out of bounds on the p1 side
			I.Set(line_p1.x, line_p1.y, 0.);
			rad = vectmag(I.x - P.x, I.y - P.y, 0.);
			return false;
		}
		I.Set(line_p0.x, P.y, 0.);
	}

	double 
		drdx = (line_p1.y - line_p0.y)/(line_p1.x - line_p0.x),
		drdx_sq = pow(drdx,2);
	I.x = (P.x + P.y * drdx - line_p0.y*drdx + line_p0.x*drdx_sq)/(1. + drdx_sq);
	//check for containment
	double Ixr = (I.x - line_p0.x)/(line_p1.x - line_p0.x);
	if(Ixr < 0.){	//outside the bounds on the p0 side
		I.x = line_p0.x;
		I.y = line_p0.y;
		rad = vectmag(I.x - P.x, I.y - P.y, 0.);
		return false;
	}
	else if(Ixr > 1.){	//outside the bounds on the p1 side
		I.x = line_p1.x;
		I.y = line_p1.y;
		rad = vectmag(I.x - P.x, I.y - P.y, 0.);
		return false;
	}
	//in bounds
	I.y = line_p0.y + (I.x - line_p0.x)*drdx;
	rad = vectmag(I.x - P.x, I.y - P.y, 0.);
	return true;

}

void Toolbox::ellipse_bounding_box(double &A, double &B, double &phi, double sides[4], double cx, double cy){
	/* 
	This algorithm takes an ellipse in a plane at location {cx,cy} and with unrotated X axis 
	size A, unrotated Y axis size B, and with angle of rotation 'phi' [radians] and calculates 
	the bounding box edge locations in the coordinate system of the plane.

	This method fills the 'sides' array with values for:
	sides[0]	|	x-axis minimum
	sides[1]	|	x-axis maximum
	sides[2]	|	y-axis minimum
	sides[3]	|	y-axis maximum

	Reference:
	http://stackoverflow.com/questions/87734/how-do-you-calculate-the-axis-aligned-bounding-box-of-an-ellipse
	
	Governing equations are:
	x = cx + A*cos(t)*cos(phi) - b*sin(t)*sin(phi)
	y = cy + b*sin(t)*cos(phi) - a*cos(t)*sin(phi)
	
	where 't' is an eigenvalue with repeating solutions of dy/dt=0
	
	For X values:
	0 = dx/dt = -A*sin(t)*cos(phi) - B*cos(t)*sin(phi)
	--> tan(t) = -B*tan(phi)/A
	--> t = atan( -B*tan(phi)/A )

	for Y values:
	0 = dy/dt = B*cos(t)*cos(phi) - A*sin(t)*sin(phi)
	--> tan(t) = B*cot(phi)/A
	--> t = aan( B*cot(phi)/A )
	
	*/
	//double pi = PI;

	//X first
	//double tx = atan( -B*tan(phi)/A );
	double tx = atan2( -B*tan(phi), A);
	//plug back into the gov. equation
	double txx = A*cos(tx)*cos(phi) - B*sin(tx)*sin(phi);
	sides[0] = cx + txx/2.;
	sides[1] = cx - txx/2.;
	//enforce orderedness
	if(sides[1] < sides[0]) swap(&sides[0], &sides[1]);

	//Y next
	double ty = atan2( -B, tan(phi)*A );
	double tyy = B*sin(ty)*cos(phi) - A*cos(ty)*sin(phi);
	sides[2] = cy + tyy/2.;
	sides[3] = cy - tyy/2.;
	if(sides[3] < sides[2]) swap(&sides[3], &sides[2]);
	


}

void Toolbox::convex_hull(std::vector<sp_point> &points, std::vector<sp_point> &hull)
{
	/* 
	Returns a list of points on the convex hull in counter-clockwise order.
	Note: the last point in the returned list is the same as the first one.
	
	Source: http://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain

	*/
	int n = (int)points.size(), k = 0;
	vector<sp_point> H(2*n);

    //copy points
    vector<sp_point> pointscpy;
    pointscpy.reserve( points.size() );
    for(size_t i=0; i<points.size(); i++)
        pointscpy.push_back( points.at(i) );
 
	// Sort points lexicographically
	sort(pointscpy.begin(), pointscpy.end());
 
	// Build lower hull
	for (int i = 0; i < n; ++i) {
		while (k >= 2 && crossprod(H.at(k-2), H.at(k-1), pointscpy.at(i)) <= 0) k--;
		H.at(k++) = pointscpy[i];
	}
 
	// Build upper hull
	for (int i = n-2, t = k+1; i >= 0; i--) {
		while (k >= t && crossprod(H.at(k-2), H.at(k-1), pointscpy.at(i)) <= 0) k--;
		H.at(k++) = pointscpy[i];
	}
 
	

	H.resize(k);
	hull = H;
}

double Toolbox::area_polygon(std::vector<sp_point> &points){
	/* 

	INPUT: vector<sp_point> a list of 'sp_point' objects.
	OUTPUT: Return the total area

	ASSUME: we care about the area projected into the Z plane, therefore only x,y coordinates are considered.

	Calculate the area contained within a generic polygon. Use the projected segments method. 
	The polygon can be convex or non-convex and can be irregular. 

	The vector "points" is an ordered list of points composing the polygon in CLOCKWISE order. 
	The final point in "points" is assumed to be connected to the first point in "points".

	*/

	//add the first point to the end of the list
    if( points.size() == 0 )
        return 0.;
	points.push_back(points.front());

	int npt = (int)points.size();

	double area = 0.;

	for(int i=0; i<npt-1; i++){
		double w = points.at(i).x - points.at(i+1).x;
		double ybar = (points.at(i).y + points.at(i+1).y)*0.5;

		area += w * ybar;
	}
	
	//restore the array
	points.pop_back();

	return area;

}

typedef vector<sp_point> Poly;  //Local definitino of polygon, used only in polyclip
class polyclip  
{
    /*
    
    A class dedicated to clipping polygons
    
    This class must remain in Toolbox.cpp to compile!

    */
public:

    Poly clip(Poly &subjectPolygon, Poly &clipPolygon)
    {
        /* 
        
        http://en.wikipedia.org/wiki/Sutherland%E2%80%93Hodgman_algorithm
    
        */        

        outputList = subjectPolygon;
        cp1 = clipPolygon.back();
    
        for(int i=0; i<(int)clipPolygon.size(); i++)
        {
            cp2 = clipPolygon.at(i);
            inputList = outputList;
            outputList.clear();

            s = inputList.back();

            for(int j=0; j<(int)inputList.size(); j++)
            {
                e = inputList.at(j);

                if(inside(&e) ){
                    if(! inside(&s) )
                        outputList.push_back( computeIntersection() );
                    outputList.push_back(e);
                }
                else if( inside(&s) ){
                    outputList.push_back( computeIntersection() );
                }

                s = e;
            }

            cp1 = cp2;

        }

        return outputList;
    }

private:
    sp_point cp1, cp2;
    Poly outputList, inputList;
    sp_point s, e;

    bool inside(sp_point *P){
        return (cp2.x - cp1.x)*(P->y - cp1.y) > (cp2.y - cp1.y)*(P->x - cp1.x);
    };

    sp_point computeIntersection()
    {
        sp_point dc = cp1;
        dc.Subtract(cp2);

        sp_point dp = s;
        dp.Subtract(e);

        double n1 = cp1.x * cp2.y - cp1.y * cp2.x;
        double n2 = s.x * e.y - s.y * e.x;
        double n3 = 1./ (dc.x * dp.y - dc.y * dp.x);

        sp_point ret;
        ret.Set((n1*dp.x - n2*dc.x) * n3, (n1*dp.y - n2*dc.y)*n3, 0.);

        return ret;
    }
 };

vector<sp_point> Toolbox::clipPolygon(std::vector<sp_point> &A, std::vector<sp_point> &B)     
{
    /* 
    Compute the polygon that forms the intersection of two polygons subjectPolygon 
    and clipPolygon. (clipPolygon clips subjectPolygon).

    This only considers 2D polygons -- vertices X and Y in "sp_point" structure!
    */
    polyclip P;

    return P.clip(A,B);
}

void Toolbox::BezierQ(sp_point &start, sp_point &control, sp_point &end, double t, sp_point &result)
{
    /* 
    Locate a point 'result' along a quadratic Bezier curve.
    */
    double tc = 1. - t;

    result.x = tc * tc * start.x + 2 * (1 - t) * t * control.x + t * t * end.x;
    result.y = tc * tc * start.y + 2 * (1 - t) * t * control.y + t * t * end.y;
    result.z = tc * tc * start.z + 2 * (1 - t) * t * control.z + t * t * end.z;

}

void Toolbox::BezierC(sp_point &start, sp_point &control1, sp_point &control2, sp_point &end, double t, sp_point &result)
{
    /* 
    Locate a point 'result' along a cubic Bezier curve.
    */

    double tc = 1. - t;

    result.x = tc*tc*tc * start.x + 3. * tc *tc * t * control1.x + 3. * tc * t * t * control2.x + t * t * t * end.x;
    result.y = tc*tc*tc * start.y + 3. * tc *tc * t * control1.y + 3. * tc * t * t * control2.y + t * t * t * end.y;
    result.z = tc*tc*tc * start.z + 3. * tc *tc * t * control1.z + 3. * tc * t * t * control2.z + t * t * t * end.z;

}

void Toolbox::poly_from_svg(std::string &svg, std::vector< sp_point > &polygon, bool clear_poly)     //construct a polygon from an SVG path
{
    /* 
    The following commands are available for path data:

    M = moveto
    L = lineto
    H = horizontal lineto
    V = vertical lineto
    C = curveto
    Q = quadratic Bezier curve
    Z = closepath
    >> not yet supported
    S = smooth curveto
    T = smooth quadratic Bezier curveto
    A = elliptical Arc
    <<
    Note: All of the commands above can also be expressed with lower letters. Capital letters means absolutely positioned, lower cases means relatively positioned.

    */

    int move=-1; //initialize 
    std::string movedat;
    bool process_now = false;

    if( clear_poly )
        polygon.clear();
    polygon.reserve( svg.size()/5 );
    double x0,y0;
    x0=y0=0.; //last position for relative calcs

    for(size_t i=0; i<svg.size(); i++)
    {
        int c = svg.at(i);

        if( c > 64 && c < 123 ) //a new command
        {
            if( move > 0 )
                process_now = true;
            else
                move = c;

        }
        else
        {
            movedat += svg.at(i);
        }

        if( process_now )
        {
            std::vector< std::string > points_s = split(movedat, " ");
            std::vector< std::vector<double> > points;
            for(size_t j=0; j<points_s.size(); j++)
            {
                std::vector< std::string > xy_s = split( points_s.at(j), "," );

                points.push_back( std::vector<double>() );

                for( size_t k=0; k<xy_s.size(); k++)
                {
                    points.back().push_back( 0 );
                    to_double( xy_s.at(k), &points.back().at(k) );
                }
            }

            int npt = (int)points.size();

            switch (move)
            {
            case 'm':
                //pick up and move cursor (reset last position)
                x0 += points.front().at(0);
                y0 += points.front().at(1);
                polygon.push_back( sp_point(x0, -y0, 0.) );

                if( npt > 1 )  //any subsequent points are assumed to be 'l'
                {
                    for(int j=1; j<npt; j++)
                    {
                        x0 += points.at(j).at(0);
                        y0 += points.at(j).at(1);
                        polygon.push_back( sp_point(x0, -y0, 0.) );
                    }
                }

                break;

            case 'M':
                //pick up and move cursor (reset last position)
                x0 = points.front().at(0);
                y0 = points.front().at(1);
                polygon.push_back( sp_point(x0, -y0, 0.) );

                if( npt > 1 )  //any subsequent points are assumed to be 'l'
                {
                    for(int j=1; j<npt; j++)
                    {
                        x0 += points.at(j).at(0);
                        y0 += points.at(j).at(1);
                        polygon.push_back( sp_point(x0, -y0, 0.) );
                    }
                }

                break;
            case 'l':

                //trace all points
                for(int j=0; j<npt; j++)
                {
                    x0 += points.at(j).at(0);
                    y0 += points.at(j).at(1);
                    polygon.push_back( sp_point(x0, -y0, 0.) );
                }
                break;

            case 'L':

                //trace all points - absolute
                for(int j=0; j<npt; j++)
                {
                    x0 = points.at(j).at(0);
                    y0 = points.at(j).at(1);
                    polygon.push_back( sp_point(x0, -y0, 0.) );
                }
                break;

            case 'h':

                //horizontal line relative
                for(int j=0; j<npt; j++)
                {
                    x0 += points.at(j).front();
                    polygon.push_back( sp_point(x0, -y0, 0.) );
                }
                break;

            case 'H':
                
                //horizontal line absolute
                for(int j=0; j<npt; j++)
                {
                    x0 = points.at(j).front();
                    polygon.push_back( sp_point(x0, -y0, 0.) );
                }
                break;

            case 'v':

                //vertical line relative
                for(int j=0; j<npt; j++)
                {
                    y0 += points.at(j).front();
                    polygon.push_back( sp_point(x0, -y0, 0.) );
                }
                break;

            case 'V':

                //vertical line absolute
                for(int j=0; j<npt; j++)
                {
                    y0 = points.at(j).front();
                    polygon.push_back( sp_point(x0, -y0, 0.) );
                }
                break;

            case 'q':
            case 'Q':
            {
                //bezier curve
                double xcond=0.;
                double ycond=0.;
                
                //check to make sure there are an even number of points
                if( npt % 2 != 0 )
                    return;

                int nbz = 5;    //number of internal bezier points

                for(int j=0; j<npt; j+=2)  //jump through in pairs
                {
                    sp_point start(x0, y0, 0.);

                    if( move == 'q' ) //if relative, set the relative adder to the start point location
                    {
                        xcond = start.x;
                        ycond = start.y;
                    }

                    sp_point control( points.at(j).at(0) + xcond, points.at(j).at(1) + ycond, 0. );
                    sp_point end( points.at(j+1).at(0) + xcond, points.at(j+1).at(1) + ycond, 0. );

                    for(int k=0; k<nbz; k++)
                    {
                        double t = (k+1)/(double)(nbz+1);
                        sp_point result;
                        Toolbox::BezierQ(start, control, end, t, result);
                        result.y = -result.y;
                        polygon.push_back( result );
                    }
                    
                    //update cursor position
                    x0 = end.x;
                    y0 = end.y;

                    //add the end point
                    end.y = -end.y;
                    polygon.push_back( end );

                }
                break;
            }

            case 'c':
            case 'C':
            {
                //bezier curve
                double xcond=0.;
                double ycond=0.;
                
                //check to make sure there are a divisible number of points
                if( npt % 3 != 0 )
                    return;

                int nbz = 7;    //number of internal bezier points

                for(int j=0; j<npt; j+=3)  //jump through in pairs
                {
                    sp_point start(x0, y0, 0.);
                    //sp_point start = polygon.back();
                    if( move == 'c' ) //if relative, set the relative adder to the start point location
                    {
                        xcond = start.x;
                        ycond = start.y;
                    }

                    sp_point control1( points.at(j).at(0) + xcond, points.at(j).at(1) + ycond, 0. );
                    sp_point control2( points.at(j+1).at(0) + xcond, points.at(j+1).at(1) + ycond, 0. );
                    sp_point end( points.at(j+2).at(0) + xcond, points.at(j+2).at(1) + ycond, 0. );

                    for(int k=0; k<nbz; k++)
                    {
                        double t = (k+1)/(double)(nbz+2);
                        sp_point result;
                        Toolbox::BezierC(start, control1, control2, end, t, result);
                        result.y = -result.y;
                        polygon.push_back( result );
                    }

                    //update cursor position
                    x0 = end.x;
                    y0 = end.y;

                    //add the end point
                    end.y = -end.y;
                    polygon.push_back( end );
                }
                break;
            }

            case 'z':
            case 'Z':
                break;
            default: //c, t, s, a
                break;
            }

            movedat.clear();
            move = c; //next move
            process_now = false;
        }
    }

    return;
}


sp_point Toolbox::rotation_arbitrary(double theta, Vect &axis, sp_point &axloc, sp_point &pt){
	/* 
	Rotation of a point 'pt' about an arbitrary axis with direction 'axis' centered at point 'axloc'. 
	The point is rotated through 'theta' radians.
	http://inside.mines.edu/~gmurray/ArbitraryAxisRotation/
	
	Returns the rotated sp_point location
	*/

	double
		a = axloc.x,	//Point through which the axis passes
		b = axloc.y,
		c = axloc.z,
		x = pt.x,		//Point that we're rotating
		y = pt.y,
		z = pt.z,
		u = axis.i,		//Direction of the axis that we're rotating about
		v = axis.j,
		w = axis.k;
	sp_point fin;

	double 
		sinth = sin(theta),
		costh = cos(theta);

	fin.x = (a*(pow(v,2)+pow(w,2)) - u*(b*v + c*w - u*x - v*y - w*z))*(1.-costh) + x*costh + (-c*v + b*w - w*y + v*z)*sinth;
	fin.y = (b*(pow(u,2)+pow(w,2)) - v*(a*u + c*w - u*x - v*y - w*z))*(1.-costh) + y*costh + (c*u - a*w + w*x - u*z)*sinth;
	fin.z = (c*(pow(u,2)+pow(v,2)) - w*(a*u + b*v - u*x - v*y - w*z))*(1.-costh) + z*costh + (-b*u + a*v - v*x + u*y)*sinth;

	return fin;
}

double Toolbox::ZRotationTransform(Vect &normal_vect){
	/* 
	When a heliostat position is transformed using the SolTrace convention, the heliostat 
	ends up such that the horizontal (x) axis is not parallel with the ground XZ plane.

	This is caused by a mismatch between conventional rotation about (1) the y axis and (2) the 
	rotated y axis, and azimuth-elevation rotation about (1) the x axis and (2) the original z axis.

	This method calculates the angle of rotation about the modified z-axis in order to restore 
	the azimuth-elevation positioning.
	
	Rotations are assumed to be:
		(1) CCW about Y axis
		(2) CW about X' axis
		(3) CW about Z'' axis


	*/
	//double Pi = PI;
	double az = atan3(normal_vect.i,normal_vect.j);
	double el = asin(normal_vect.k);

	//Calculate Euler angles
	double alpha = atan2(normal_vect.i, normal_vect.k);		//Rotation about the Y axis
	double bsign = normal_vect.j > 0. ? 1. : -1.;
	double beta = -bsign*acos( ( pow(normal_vect.i,2) + pow(normal_vect.k,2) )/ max(sqrt(pow(normal_vect.i,2) + pow(normal_vect.k,2)), 1.e-8) );	//Rotation about the modified X axis

	//Calculate the modified axis vector
	Vect modax;
	modax.Set( cos(alpha), 0., -sin(alpha) );

	//Rotation references - axis point
	sp_point axpos;
	axpos.Set(0.,0.,0.);	//Set as origin
	//sp_point to rotate
	sp_point pbase;
	pbase.Set(0., -1., 0.);	//lower edge of heliostat
	//Rotated point
	sp_point prot = Toolbox::rotation_arbitrary(beta, modax, axpos, pbase);

	//Azimuth/elevation reference vector (vector normal to where the base of the heliostat should be)
	Vect azelref;
	azelref.Set( sin(az)*sin(el), cos(az)*sin(el), -cos(el) );

	//Calculate the angle between the rotated point and the azel ref vector
	//double gamma = acos( Toolbox::dotprod(azelref, prot) );
	/* 
	the sign of the rotation angle is determined by whether the 'k' component of the cross product
	vector is positive or negative. 
	*/
	Vect protv;
	protv.Set(prot.x, prot.y, prot.z);
	unitvect(protv);
	Vect cp = crossprod(protv, azelref);
	double gamma = asin( vectmag( cp ));
	double gsign = (cp.k > 0. ? 1. : -1.) * (normal_vect.j > 0. ? 1. : -1.);
	
	return gamma * gsign;


}

double Toolbox::ZRotationTransform(double Az, double Zen){
	/* 
	Overload for az-zen specification

	Notes:
	The Azimuth angle should be [-180,180], zero is north, +east, -west.
	Az and Zen are both in Radians.
	*/
	//Calculate the normal vector to the heliostat based on elevation and azimuth
	double Pi = PI;
	double el = Pi/2.-Zen;
	double az = Az+Pi;	//Transform to 0..360 (in radians)
	Vect aim;
	aim.Set( sin(az)*cos(el), cos(az)*cos(el), sin(el) );
	return ZRotationTransform(aim);
}

double Toolbox::intersect_fuv(double U, double V){
	/* 
	Helper function for intersect_ellipse_rect()
	*/
	double 
		u2 = sqrt(1.-pow(U,2)),
		v2 = sqrt(1.-pow(V,2));
	return asin(u2 * v2 - U*V) - U*u2 - V*v2 + 2.*U*V;
}

double Toolbox::intersect_ellipse_rect(double rect[4], double ellipse[2]){
	/* 
	Calculate the area of intersection of a rectangle and an ellipse where the sides of the 
	rectangle area parallel with the axes of the ellipse. 

	{rect[0], rect[1]} = Point location of center of rectangle
	{rect[2], rect[3]} = Rectangle width, Rectangle height
	
	{ellipse[0], ellipse[1]} = Ellipse width and height

	(A.D. Groves, 1963. Area of intersection of an ellipse and a rectangle. Ballistic Research Laboratories.)
	*/

	//Unpack
	double
		//a = rect[0] - rect[2]/2.,	//Lower left corner X location
		//b = rect[1] - rect[3]/2.,	//Lower left corner Y location
		c = rect[2],	//Rect width
		d = rect[3];	//Rect height
	double
		w = ellipse[0],	//Ellipse width
		h = ellipse[1];	//Ellipse height

	//Construct 4 separate possible rectangles
	double A[4], B[4], C[4], D[4];
	for(int i=1; i<5; i++){
		A[i-1] = max(0., pow(-1, (pow((double)i,2)-i)/2)*rect[0] - c/2.);
		B[i-1] = max(0., pow(-1, (pow((double)i,2)+i-2)/2)*rect[1] - d/2.);
		C[i-1] = max(0., pow(-1, (pow((double)i,2)-i)/2)*rect[0]+c/d-A[i-1]);
		D[i-1] = max(0., pow(-1, (pow((double)i,2)+i-2)/2)*rect[1]+d/2.-B[i-1]);
	}

	double atot=0.;
	for(int i=0; i<4; i++){
		if(C[i] == 0. || D[i] == 0.) continue;	//No area if width or height are 0

		//Calculate vertex radii
		double V[4];
		V[0] = pow(A[i]/w,2) + pow(B[i]/h,2);
		V[1] = pow(A[i]/w,2) + pow((B[i]+D[i])/h,2);
		V[2] = pow((A[i]+C[i])/w,2) + pow((B[i]+D[i])/h,2);
		V[3] = pow((A[i]+C[i])/w,2) + pow(B[i]/h,2);

		//Seven cases
		if(pow(A[i]/w,2) + pow(B[i]/h,2) >= 1.){
			continue;	//no area overlap
		}
		else if(V[0] < 1. && V[1] >= 1 && V[3] >= 1.){
			//Lower left vertex is the only one in the ellipse
			atot += w*h/2.*intersect_fuv(A[i]/w, B[i]/h);
		}
		else if(V[3] < 1. && V[1] >= 1.){
			//Lower edge inside ellipse, upper edge outside
			atot += w*h/2. * (intersect_fuv(A[i]/w, B[i]/h) - intersect_fuv((A[i]+C[i])/w, B[i]/h));
		}
		else if(V[1] < 1. && V[3] >= 1.){
			//Left edge inside, right edge outside
			atot += w*h/2. * (intersect_fuv(A[i]/w, B[i]/h) - intersect_fuv(A[i]/w, (B[i]+D[i])/h));
		}
		else if(V[1] < 1. && V[3] < 1. && V[2] > 1.){
			//All vertices inside ellipse except upper right corner
			atot += w*h/2.*(intersect_fuv(A[i]/w, B[i]/h) - intersect_fuv((A[i]+C[i])/w, B[i]/h) - intersect_fuv(A[i]/w, (B[i]+D[i])/h));
		}
		else if(V[2] < 1.){
			//All vertices inside the ellipse
			atot += w*h;
		}
		else{
			continue;	//Error
		}
	}
	return atot;

}

string Toolbox::getDelimiter(std::string &text){
	if (text.empty())
	  return ",";
	//Find the type of delimiter
	vector<string> delims;
	delims.push_back(",");
	delims.push_back(" ");
	delims.push_back("\t");
	delims.push_back(";");
	string delim = "\t";	//initialize
	int ns=0;
	for(int i=0; i<4; i++){
		vector<string> data = Toolbox::split(text, delims[i]);
		if((int)data.size()>ns){ delim = delims[i]; ns = (int)data.size(); }	//pick the delimiter that returns the most entries
	}
	return delim;
}

vector< string > Toolbox::split( const string &str, const string &delim, bool ret_empty, bool ret_delim )
{
	//Take a string with a delimiter and return a vector of separated values
	vector< string > list;

	char cur_delim[2] = {0,0};
	string::size_type m_pos = 0;
	string token;
	int dsize = (int)delim.size();
	
	while (m_pos < str.length())
	{
		//string::size_type pos = str.find_first_of(delim, m_pos);
		string::size_type pos = str.find(delim, m_pos);
		if (pos == string::npos)
		{
			cur_delim[0] = 0;
			token.assign(str, m_pos, string::npos);
			m_pos = str.length();
		}
		else
		{
			cur_delim[0] = str[pos];
			string::size_type len = pos - m_pos;			
			token.assign(str, m_pos, len);
			//m_pos = pos + 1;
			m_pos = pos + dsize;
		}
		
		if (token.empty() && !ret_empty)
			continue;

		list.push_back( token );
		
		if ( ret_delim && cur_delim[0] != 0 && m_pos < str.length() )
			list.push_back( string( cur_delim ) );
	}
	
	return list;
}
