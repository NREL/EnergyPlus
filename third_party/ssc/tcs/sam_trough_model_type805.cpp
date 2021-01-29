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

#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "htf_props.h"


#ifndef M_PI
#define M_PI 3.14159265358979323
#endif

#ifndef MAX
#define MAX(a,b) ( (a)>(b) ? (a) : (b) )
#endif

#ifndef MIN
#define MIN(a,b) ( (a)<(b) ? (a) : (b) )
#endif

#ifndef SIGN
#define SIGN(a,b) ( (b)>=0 ? fabs(a) : -fabs(a) )
#endif

enum {
	I_NumHCETypes,
	I_Solar_Field_Area,
	I_Solar_Field_Mult,
	I_HTFFluid,
	I_HCEtype,
	I_HCEFrac,
	I_HCEdust,
	I_HCEBelShad,
	I_HCEEnvTrans,
	I_HCEabs,
	I_HCEmisc,
	I_PerfFac,
	I_RefMirrAper,
	I_HCE_A0,
	I_HCE_A1,
	I_HCE_A2,
	I_HCE_A3,
	I_HCE_A4,
	I_HCE_A5,
	I_HCE_A6,
//	I_LU_Fl,
//	I_LuFlEr,

	I_SfTi,
	I_SolarAz,
	I_Insol_Beam_Normal,
	I_AmbientTemperature,
	I_WndSpd,
	I_Stow_Angle,
	I_DepAngle,
	I_IamF0,
	I_IamF1,
	I_IamF2,
	I_Ave_Focal_Length,
	I_Distance_SCA,
	I_Row_Distance,
	I_SCA_aper,
	I_SfAvail,
	I_ColTilt,
	I_ColAz,
	I_NumScas,
	I_ScaLen,
	I_MinHtfTemp,
	I_HtfGalArea,
	I_SfPar,
	I_SfParPF,
	I_ChtfPar,
	I_ChtfParPF,
	I_CHTFParF0,
	I_CHTFParF1,
	I_CHTFParF2,
	I_AntiFrPar,
	I_Site_Lat,
	I_Site_LongD,
	I_SHIFT,
	I_TurbOutG,
	I_TurbEffG,
	I_SfInTempD,
	I_SfOutTempD,
//	I_ColType,
	I_TrkTwstErr,
	I_GeoAcc,
	I_MirRef,
	I_MirCln,
	I_ConcFac,
	I_SfPipeHl300,
	I_SfPipeHl1,
	I_SfPipeHl2,
	I_SfPipeHl3,
	I_SFTempInit,

	O_SfTo,
	O_SfMassFlow,
	O_RecHl,
	O_AveSfTemp,
	O_SfPipeHlOut,
	O_IAM,
	O_Qabsout,
	O_Hour_Angle,
	O_Qsf,
	O_SFTotPar,
	O_QsfWarmUp,
	O_EndLoss,
	O_RowShadow,
	O_ColOptEff,
	O_SfOptEff,
	O_SfTi,
	O_Qdni,
	O_EparCHTF,
	O_EparSf,
	O_EparAnti,
	O_SolarTime,
	O_SolarAlt,
	O_Theta,
	O_CosTheta,
	O_TrackAngle,
	O_Ftrack,
	O_Qnip,
	O_QnipCosTh,
	O_Qabs,
	O_Qcol,
	O_QsfAbs,
	O_QsfHceHL,
	O_QsfPipeHL,
	O_QsfWarmup,
	O_QhtfFreezeProt,
	O_ColEff,
	O_Qsfnipcosth,
	O_Qdesign,
	O_Edesign,
//	O_SolarAz,
//	O_HCEFieldErr,
//	O_ColFieldErr,
//	O_HCEFactor0,
//	O_HCEFactor1,
//	O_HCEFactor2,
//	O_HCEFactor3,

	N_MAX };

tcsvarinfo sam_trough_model_type805_variables[] = {
	// vartype    datatype    index   name     label    units   meta   group   default_value

	// parameters
	{ TCS_INPUT,  TCS_NUMBER, I_NumHCETypes,            "NumHCETypes",              "Number of receiver types used", "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Solar_Field_Area,       "Solar_Field_Area",         "Solar Field Area", "m2",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Solar_Field_Mult,       "Solar_Field_Mult",         "Solar Field Multiple", "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_HTFFluid,               "HTFFluid",                 "Type of HTF fluid used", "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCEtype,                "HCEtype",                  "Number indicating the receiver type", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCEFrac,                "HCEFrac",                  "Fraction of field that is this type of HCE", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCEdust,                "HCEdust",                  "Performance degradation due to dust", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCEBelShad,             "HCEBelShad",               "Bellows shading (fraction of rad. lost due to bellows)", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCEEnvTrans,            "HCEEnvTrans",              "Envelope transmittance", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCEabs,                 "HCEabs",                   "Absorber absorptance", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCEmisc,                "HCEmisc",                  "Miscellaneous emittance", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_PerfFac,                "PerfFac",                  "Thermal Performance Factor", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_RefMirrAper,            "RefMirrAper",              "Mirror aperature width", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCE_A0,                 "HCE_A0",                   "Heat loss calculated coefficient", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCE_A1,                 "HCE_A1",                   "Heat loss calculated coefficient", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCE_A2,                 "HCE_A2",                   "Heat loss calculated coefficient", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCE_A3,                 "HCE_A3",                   "Heat loss calculated coefficient", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCE_A4,                 "HCE_A4",                   "Heat loss calculated coefficient", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCE_A5,                 "HCE_A5",                   "Heat loss calculated coefficient", "",     "",      "",     "" },
	{ TCS_INPUT,  TCS_ARRAY,  I_HCE_A6,                 "HCE_A6",                   "Heat loss calculated coefficient", "",     "",      "",     "" },

//	{ TCS_INPUT,  TCS_NUMBER, I_LU_Fl,                  "LU_Fl",                    "Fluid property file logical unit", "",      "",      "",     "" },
//	{ TCS_INPUT,  TCS_NUMBER, I_LuFlEr,                 "LuFlEr",                   "Fluid property error file logical unit", "",      "",      "",     "" },


	// inputs
	{ TCS_INPUT,  TCS_NUMBER, I_SfTi,                   "i_SfTi",                     "Solar Field HTF inlet Temperature (if -999, calculated)", "C",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SolarAz,                "SolarAz",                  "Solar Azimuthal Angle", "deg",      "",      "",     "" },
// testing with TRNSYS input in lk script and read in in init
//	{ TCS_INPUT,  TCS_ARRAY, I_SolarAz,                "SolarAz",                  "Solar Azimuthal Angle", "deg",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Insol_Beam_Normal,      "Insol_Beam_Normal",        "Normal Beam Insolation", "kJ/hr/m2",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_AmbientTemperature,     "AmbientTemperature",       "Ambient Temperature", "C",      "",      "",     "" },

	{ TCS_INPUT,  TCS_NUMBER, I_WndSpd,	                "WndSpd", 	                "Wind Speed", 						     "m/s",      "",      "",     "" },
// testing with TRNSYS input in lk script and read in in init
//	{ TCS_INPUT,  TCS_ARRAY, I_WndSpd,	                "WndSpd", 	                "Wind Speed", 						     "m/s",      "",      "",     "" },

	{ TCS_INPUT,  TCS_NUMBER, I_Stow_Angle,	            "Stow_Angle", 	            "Night-Time Trough Stow Angle", 				     "deg",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_DepAngle,	            "DepAngle", 	            "Deployment Angle", 						     "deg",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_IamF0,	                "IamF0", 	                "Incident Angle Modifier - 0 factor", 			     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_IamF1,	                "IamF1", 	                "Incident Angle Modifier - 1 factor", 			     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_IamF2,	                "IamF2", 	                "Incident Angle Modifier - 2 factor", 			     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Ave_Focal_Length,       "Ave_Focal_Length",         "Trough!s Average Focal Length", 				     "m",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Distance_SCA,           "Distance_SCA", 	        "Distance between SCAs in Row", 				     "m",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Row_Distance,           "Row_Distance", 	        "Distance between Rows of SCAs", 				     "m",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SCA_aper,	            "SCA_aper", 	            "SCA Aperature", 						     "m",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SfAvail,	            "SfAvail", 	                "Solar Field Availability", 					     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_ColTilt,	            "ColTilt", 	                "Collector Axis Tilt", 					     "deg",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_ColAz,	                "ColAz", 	                "Azimuthal Angle of Collector Axis", 			     "deg",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_NumScas,	            "NumScas", 	                "Number of SCAs per Row", 					     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_ScaLen,	                "ScaLen", 	                "Length of Single SCA", 					     "m",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_MinHtfTemp,	            "MinHtfTemp", 	            "Min. Heat Transfer Fluid Temperature", 			     "C",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_HtfGalArea,	            "HtfGalArea", 	            "HTF Fluids in Gallons per Field Area", 			     "gal/m2",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SfPar,	                "SfPar", 	                "SCA drives and electronics total parasitic", 		     "MWe",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SfParPF,	            "SfParPF", 	                "SCA drives and electronics multiplier", 			     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_ChtfPar,	            "ChtfPar", 	                "Solar field HTF pump parasitic - factor", 			     "MWe",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_ChtfParPF,	            "ChtfParPF", 	            "Solar field HTF pump parasitic - multiplier", 		     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_CHTFParF0,	            "CHTFParF0", 	            "Solar field HTF pump parasitic - equation constant", 	     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_CHTFParF1,	            "CHTFParF1", 	            "Solar field HTF pump parasitic - equation linear term", 	     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_CHTFParF2,	            "CHTFParF2", 	            "Solar field HTF pump parasitic - equation quadradic term", 	     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_AntiFrPar,	            "AntiFrPar", 	            "Fixed anti-freeze pumping parasitic", 			     "MWe",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Site_Lat,	            "Site_Lat", 	            "Latitude of Solar Plant Site", 				     "deg",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Site_LongD,	            "Site_LongD", 	            "Longitude of Site", 					     "deg",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SHIFT,	                "SHIFT", 	                "Longitude of Standard Meridian", 				     "deg",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TurbOutG,	            "TurbOutG", 	            "Gross Turbine Output (SETS THE DESIGN POINT)", 		     "MWe",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TurbEffG,	            "TurbEffG", 	            "Gross Turbine Eff (SETS THE DESIGN POINT)", 			     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SfInTempD,	            "SfInTempD", 	            "Solar Field DesignInlet  Temperature", 			     "C",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SfOutTempD,	            "SfOutTempD", 	            "Solar Field Design Outlet Temperature", 			     "C",      "",      "",     "" },
//	{ TCS_INPUT,  TCS_NUMBER, I_ColType,	            "ColType", 	                "Collector Type", 						     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TrkTwstErr,	            "TrkTwstErr", 	            "Tracking Error and Twist", 					     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_GeoAcc,	                "GeoAcc", 	                "Geometric Accuracy", 					     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_MirRef,	                "MirRef", 	                "Mirror Reflectivity", 					     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_MirCln,	                "MirCln", 	                "Mirror Cleanliness Factor", 				     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_ConcFac,	            "ConcFac", 	                "Concentrator Factor", 					     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SfPipeHl300,            "SfPipeHl300", 	            "Solar field piping heat loss at design", 			     "W/m2",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SfPipeHl1,	            "SfPipeHl1", 	            "Solar field piping heat loss at reduced temp. - linear term",    "C^(-1)",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SfPipeHl2,	            "SfPipeHl2", 	            "Solar field piping heat loss at reduced temp. - quadratic term", "C^(-2)",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SfPipeHl3,	            "SfPipeHl3", 	            "Solar field piping heat loss at reduced temp. - cubic term",     "C^(-3)",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SFTempInit,	            "SFTempInit", 	            "Solar Field Initial Temperature", 				                  "C",       "",      "",     "" },


	// outputs
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SfTo,		        "SfTo", 	                "Final Outlet temperature of the solar field", 			                "C",    "",      "DEBUG",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SfMassFlow,	        "SfMassFlow", 	            "Solar field mass flow rate", 					                        "kg/hr",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_RecHl,		        "RecHl", 	                "Total receiver heat loss", 						                    "kJ/hr-m2",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_AveSfTemp,	        "AveSfTemp", 	            "Average Solar Field Temperature during the timestep", 		            "C",    "",      "DEBUG",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SfPipeHlOut,	        "SfPipeHlOut",              "Solar Field Pipe Heat Loss (non-receiver loss)", 			            "kJ/hr-m2",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_IAM,		            "IAM", 	                    "Incidence Angle Modifier (average over the timestep)", 		        "",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qabsout,		        "Qabsout", 	                "Heat absorbed by the solar field before thermal loss", 		        "kJ/hr-m2",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Hour_Angle,	        "Hour_Angle", 	            "Hour angle", 							                                "rad",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qsf,		            "Qsf", 	                    "Total thermal power from the solar field after losses", 		        "MWt",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SFTotPar,	        "SFTotPar", 	            "Total Solar Field Parasitics", 					                    "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QsfWarmUp,	        "QsfWarmUp", 	            "Power required or that has contributed to warming the solar field",    "MWt",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EndLoss,		        "EndLoss", 	                "Fraction of energy lost due to spillage over the end of the row", 	    "",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_RowShadow,	        "RowShadow", 	            "Fraction of energy lost from row-to-row shadowing", 		            "",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_ColOptEff,	        "ColOptEff", 	            "Collector Optical Efficiency", 					                    "",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SfOptEff,	        "SfOptEff", 	            "Total Solar Field Optical Efficiency, with receiver optical loss", 	"",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SfTi,		        "o_SfTi", 	                "Solar field inlet temperature", 					                    "C",    "",      "DEBUG",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qdni,		        "Qdni", 	                "Total incident irradiation on the field before any losses", 	        "MWt",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EparCHTF,	        "EparCHTF", 	            "Cold HTF Pump Parasitics (HTF flow to Solar Field)", 		            "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EparSf,		        "EparSf", 	                "Parasitics associated with solar field tracking and drives", 	        "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EparAnti,	        "EparAnti", 	            "Antifreeze pumping parasitics", 					                    "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SolarTime,	        "SolarTime", 	            "The average solar time for the current timestep", 	                    "hour",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SolarAlt,	        "SolarAlt", 	            "The solar elevation angle above the horizon", 			                "deg",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Theta,		        "Theta", 	                "Angle between the aperture plane normal and incident radiation", 	    "deg",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_CosTheta,	        "CosTheta", 	            "Multiplying term that scales incident rad due to angular loss", 	    "",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TrackAngle,	        "TrackAngle", 	            "Collector tracking angle", 						                    "deg",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Ftrack,		        "Ftrack", 	                "The fraction of the time period that the field is tracking", 	        "",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qnip,		        "Qnip", 	                "Magnitude of the incident radiation (equal to beam normal)", 	        "W/m2",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QnipCosTh,	        "QnipCosTh", 	            "Incident radiation scaled by the cosine loss: effective radiation",    "W/m2",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qabs,		        "Qabs", 	                "Energy absorbed by the solar field before th. loss, per unit area",    "W/m2",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qcol,		        "Qcol", 	                "Energy delivered by the solar field, per unit area", 		            "W/m2",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QsfAbs,		        "QsfAbs", 	                "Total energy absorbed by the solar field before th. loss", 		    "MWt",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QsfHceHL,	        "QsfHceHL", 	            "Total energy lost by the receivers", 				                    "MWt",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QsfPipeHL,	        "QsfPipeHL", 	            "Total energy lost by the field piping", 				                "MWt",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QsfWarmup,	        "QsfWarmup", 	            "Total energy contributing to solar field warmup", 			            "MWt",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QhtfFreezeProt,	    "QhtfFreezeProt",           "Total energy contributing to solar field freeze protection", 	        "MWt",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_ColEff,		        "ColEff", 	                "Total collector + receiver efficiency, with optics and heat loss", 	"",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qsfnipcosth,	        "Qsfnipcosth",              "Total inc. radiation scaled by the cosine loss", 			            "MWt",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qdesign,	            "Qdesign",                  "Power block thermal design", 			                                "MWt",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Edesign,	            "Edesign",                  "Power block electric design", 			                                "MWe",    "",      "",     "" },
//	{ TCS_OUTPUT,  TCS_NUMBER, O_SolarAz,               "O_SolarAz",                "Solar Azimuthal Angle (TRNSYS)",                                       "deg",      "",      "",     "" },
//	{ TCS_OUTPUT,  TCS_NUMBER, O_HCEFieldErr,           "O_HCEFieldErr",            "HCE field Error",                                                      "",      "",      "",     "" },
//	{ TCS_OUTPUT,  TCS_NUMBER, O_ColFieldErr,           "O_ColFieldErr",            "Collector Field Error",                                                "",      "",      "",     "" },
//	{ TCS_OUTPUT,  TCS_NUMBER, O_HCEFactor0,            "O_HCEFactor0",             "HCE field factor 0",                                                   "",      "",      "",     "" },
//	{ TCS_OUTPUT,  TCS_NUMBER, O_HCEFactor1,            "O_HCEFactor1",             "HCE field factor 1",                                                   "",      "",      "",     "" },
//	{ TCS_OUTPUT,  TCS_NUMBER, O_HCEFactor2,            "O_HCEFactor2",             "HCE field factor 2",                                                   "",      "",      "",     "" },
//	{ TCS_OUTPUT,  TCS_NUMBER, O_HCEFactor3,            "O_HCEFactor3",             "HCE field factor 3",                                                   "",      "",      "",     "" },



	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};


class sam_trough_model_type805 : public tcstypeinterface
{
private:
	double m_time0; //= stored(1)
	double m_tFinal;// = stored(2)
	double m_delt; //= stored(3)
	double m_SfTiO; //= stored(4)
	double m_SfToO; //= stored(5)
	double m_AveSfTemp0;// = stored(6)
	double m_AveSfTemp0Next;// = stored(7)
	double m_SfTi_init;
	double m_sfti_calc;

	double *m_HCEfactor;
	double *m_HCEtype;
	double *m_HCEFrac;
	double *m_HCEdust;
	double *m_HCEBelShad;
	double *m_HCEEnvTrans;
	double *m_HCEabs;
	double *m_HCEmisc;
	double *m_PerfFac;
	double *m_RefMirrAper;
	//Removed 11/2009 MJW
	////////////////HCERe
	////////////////HCENu
	double *m_HCE_A0;
	double *m_HCE_A1;
	double *m_HCE_A2;
	double *m_HCE_A3;
	double *m_HCE_A4;
	double *m_HCE_A5;
	double *m_HCE_A6;


	// testing TRNSYS input
//	double *m_SolAz;
//	double *m_WndSpd;


public:
	sam_trough_model_type805( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
	}

	virtual ~sam_trough_model_type805()
	{
	}

	virtual int init()
	{
		m_SfTi_init = value(I_SFTempInit);

		// update these to inputs
		m_time0 = 1; //= stored(1)
		m_tFinal = 8760;// = stored(2)
		m_delt = 1; //= stored(3)
		m_SfTiO = m_SfTi_init; //= stored(4)
		m_SfToO = m_SfTi_init; //= stored(5)
		// sam_trough_model_type805 line 20
		//m_AveSfTemp0 = 100;// = stored(6)
		// updated 12/18/12
		m_AveSfTemp0 = m_SfTi_init;// = stored(6)
		m_AveSfTemp0Next = 100;// = stored(7)

		m_sfti_calc = value(I_SfTi);

		int len;
		m_HCEfactor = value(I_HCEtype, &len);
		m_HCEtype = value(I_HCEtype, &len); //this is a number based on receiver type - not sure it's useful in this new method
		m_HCEFrac = value(I_HCEFrac, &len); //Fraction of field that is this type of HCE
		m_HCEdust = value(I_HCEdust, &len); //perf degradation due to dust
		m_HCEBelShad = value(I_HCEBelShad, &len);//bellows shading (typically .971)
		m_HCEEnvTrans = value(I_HCEEnvTrans, &len); //envelope transmissivity (typically .96)
		m_HCEabs = value(I_HCEabs, &len); //Absorber absorption (typically .96)
		m_HCEmisc = value(I_HCEmisc, &len); //Miscellaneous emissivity (typically 1.0)
		//////////////////HCEflag(n)             = PAR(3+ NUMHCEType*20 - 12) //flag to indicate if actual or temp-over-ambient were used to measure properties 
		m_PerfFac = value(I_PerfFac, &len); //Thermal Performance Factor (typically 1)
		m_RefMirrAper = value(I_RefMirrAper, &len); //Mirror aperature (5m for LS2)
		//Removed 11/2009 MJW        HCEMinWind(n)          = PAR(4+ n*16 - 7)  //minimum windspeed 
		////////////////HCERecLength(n)         = PAR(3+ NUMHCEType*20 - 8)  //receiver length
		////////////////HCENumRecPerAperArea(n) = PAR(3+ NUMHCEType*20 - 7)  //number of receivers per aperture area
		m_HCE_A0 = value(I_HCE_A0, &len);  //heat loss calculated coefficient
		m_HCE_A1 = value(I_HCE_A1, &len);  //heat loss calculated coefficient
		m_HCE_A2 = value(I_HCE_A2, &len);  //heat loss calculated coefficient
		m_HCE_A3 = value(I_HCE_A3, &len);  //heat loss calculated coefficient
		m_HCE_A4 = value(I_HCE_A4, &len);  //heat loss calculated coefficient
		m_HCE_A5 = value(I_HCE_A5, &len);  //heat loss calculated coefficient
		m_HCE_A6 = value(I_HCE_A6, &len);     //heat loss calculated coefficient

	// testing TRNSYS input
//		m_SolAz = value(I_SolarAz, &len);
//		m_WndSpd = value(I_WndSpd, &len);

		return 0;
	}



	//Enthalpy of Caloria HT 43 [J/kg]
	double H_caloria(double T) //T[C]
	{
		return 1.94 * T*T + 1606.0 * T;
	}

	//Enthalpy of Salt (Metric Units), [J/kg]
	double H_salt(double T)  //T[C]
	{
		return 1443 * T + 0.086 * T*T;
	}

	//Enthalpy of HITEC XL Nitrate Salt, [J/kg]
	double H_salt_xl(double T) //T [C]
	{
		return 1536 * T - 0.1312 * T*T - 0.0000379667 * T*T*T;
	}

	//Enthalpy of HITEC Salt, [J/kg]
	double H_salt_hitec(double T) //T[C]
	{
		return 1560 * T;
	}

	//Enthalpy of Therminol Oil [J/kg]
	double H_therminol(double T) //T [C]
	{
		return 1000 * (-18.34 + 1.498 * T + 0.001377 * T*T);
	}

	//Enthalpy of Dowtherm Q [J/kg]
	double H_Dowtherm_Q(double T) //T [C]
	{
		return (0.00151461 * T*T + 1.59867 * T - 0.0250596) * 1000;    //Hank 10-2-03
	}

	//Enthalpy of Dowtherm RP [J/kg]
	double H_Dowtherm_RP(double T) //T [C]
	{
		return (0.0014879 * T*T + 1.5609 * T - 0.0024798) * 1000;      //Hank 10-2-03
	}

/************************* User Fluid ****************************
!| Note that the fluid properties are stored in the FPROP array  *
!| in the following order:                                       *
!*****************************************************************
!|  #    |   1   |   2   |   3   |   4   |   5   |   6   |   7   |
!|-------|-------|-------|-------|-------|-------|-------|-------|
!| Name  |   T   |   Cp  |  rho  |   Mu  |   Nu  |   k   |   h   |
!| Units |   C   |kJ/kg-K| kg/m3 |  Pa-s |  m2-s | W/m-K |  J/kg |
!|-------|-------|-------|-------|-------|-------|-------|-------|
!|   1   |   :   |   :   |   :   |   :   |   :   |   :   |   :   |
!|   2   |   :   |   :   |   :   |   :   |   :   |   :   |   :   |
!*****************************************************************/
	
	//Enthalpy of User Fluid, [J/kg]
	double H_user(double /*T*/,int /*fn*/) //T [C]
	{
		double enthalpy=0;
		return enthalpy;
	}
	/*
	   Gjsav=STORED(1)  !Gjsav is a module variable, shared between this and the subfunctions


		integer,intent(in)::fn
		double precision,dimension(size(fprop(1,fl_bounds(fn):(fl_bounds(fn+1)-1))))::dxx,dyy //Create dummy arrays
		integer::lb,ub,t_warn,dum
		//Call the user-defined property table
		lb=fl_bounds(fn)
		ub=fl_bounds(fn+1)-1
		if(ub.lt.lb) ub=size(fprop(1,:))
		dxx(:)=fprop(1,lb:ub)
		dyy(:)=fprop(7,lb:ub)
		call interp(T,size(dxx),dxx,dyy,Gjsav,H_user)
		if((Gjsav.eq.ub).or.(Gjsav.eq.lb)) dum=t_warn(T,dxx(lb),dxx(ub),"User-specified fluid")
	*/

	double H_fluid(double temp, int fluid) // Enthalpy from type 229
	{
		double enthalpy = 0.0;
		double T = temp - 273.15;
		if (( fluid >= 1 ) && ( fluid <= 17 ))	
			enthalpy = 1; //no props for this model
		else if ( fluid == 18 )
			enthalpy = H_salt(T); 
		else if ( fluid == 19 )
			enthalpy = H_caloria(T); 
		else if ( fluid == 20 )
			enthalpy = H_salt_xl(T); 
		else if ( fluid == 21 )
			enthalpy = H_therminol(T);
		else if ( fluid == 22 )
			enthalpy = H_salt_hitec(T);
		else if ( fluid == 23 )
			enthalpy = H_Dowtherm_Q(T);
		else if ( fluid == 24 )
			enthalpy = H_Dowtherm_RP(T);
		else if ( fluid == 25 )
			enthalpy = H_salt_xl(T);
		else if (( fluid >= 26 ) && ( fluid <= 35))
			enthalpy = 1; //no props for this model
		else if ( fluid >= 36 )
			enthalpy = H_user(T,fluid-35);

		return enthalpy;
	}

	double T_fluid(double H, int fluid) // need to port from type 229
	{
		double temp=0.0;
		double H_kJ;
		if ((fluid >= 1) && (fluid <= 17))
			temp = 1; //no props for this model
		else if (fluid == 18)
			temp = -0.0000000000262 * H*H + 0.0006923 * H + 0.03058;
		else if (fluid == 19)
			temp = 6.4394E-17 * H*H*H - 0.00000000023383 * H*H + 0.0005821 * H + 1.2744;
		else if (fluid == 20)
			temp = 0.00000000005111 * H*H + 0.0006466 * H + 0.2151;
		else if (fluid == 21)
			temp = 7.4333E-17 * H*H*H - 0.00000000024625 * H*H + 0.00063282 * H + 12.403;
		else if (fluid == 22)
			temp = -3.309E-24 * H*H + 0.000641 * H + 0.000000000001364;
		else if (fluid == 23)
			temp = 6.186E-17 * H*H*H - 0.00000000022211 * H*H + 0.00059998 * H + 0.77742;
		else if (fluid == 24)
			temp = 6.6607E-17 * H*H*H - 0.00000000023347 * H*H + 0.00061419 * H + 0.77419;
		else if (fluid == 25)
			temp = 0.00000000005111 * H*H + 0.0006466 * H + 0.2151;
		else if ((fluid >= 26) && (fluid <=28))
			temp = 1; //no props for this model
		else if (fluid == 29)    //Therminol 66
		{
			H_kJ = H / 1000.0;
			temp = -0.00018*H_kJ*H_kJ + 0.521*H_kJ + 7;
		}
		else if (fluid == 30)    //Therminol 59
		{
			H_kJ = H / 1000.0;
			temp = -0.000204*H_kJ*H_kJ + 0.539*H_kJ - 0.094;
		}
		else if ((fluid >= 31) && (fluid <= 35))
			temp = 1; //no props for this model
//		else if (fluid == 36)
//			T_fluid = T_user(H,Fluidnum-35) ;
		
		temp = temp + 273.15;  //MJW 8.26.2010
		return temp;
	}

	double density( int fluid, double T, double P) // need to port from type 229
	{
		double dens=0.0;
		double Td=T-273.15;             //Convert from K to C

		if (fluid==1)   //    1.) Air
			dens = P/(287.*T);
		else if (fluid==2)   //    2.) Stainless_AISI316
			dens=8349.38 - 0.341708*T - 0.0000865128*T*T;  //EES
		else if (fluid==3)   //    3.) Water (liquid)
			dens = 1000; 
		else if (fluid==6)   //    6.) Salt (68% KCl, 32% MgCl2)
			dens = 1E-10*T*T*T - 3E-07*T*T - 0.4739*T + 2384.2;
		else if (fluid==7)   //    7.) Salt (8% NaF, 92% NaBF4)
			dens = 8E-09*T*T*T - 2E-05*T*T - 0.6867*T + 2438.5;
		else if (fluid==8)   //    8.) Salt (25% KF, 75% KBF4)
			dens = 2E-08*T*T*T - 6E-05*T*T - 0.7701*T + 2466.1;
		else if (fluid==9)   //    9.) Salt (31% RbF, 69% RbBF4)
			dens = -1E-08*T*T*T + 4E-05*T*T - 1.0836*T + 3242.6;
		else if (fluid==10)   //    10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
			dens =  -2E-09*T*T*T + 1E-05*T*T - 0.7427*T + 2734.7;
		else if (fluid==11)   //    11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
			dens = -2E-11*T*T*T + 1E-07*T*T - 0.5172*T + 3674.3;
		else if (fluid==12)   //    12.) Salt (58% KF, 42% ZrF4)
			dens =  -6E-10*T*T*T + 4E-06*T*T - 0.8931*T + 3661.3;
		else if (fluid==13)   //    13.) Salt (58% LiCl, 42% RbCl)
			dens = -8E-10*T*T*T + 1E-06*T*T - 0.689*T + 2929.5;
		else if (fluid==14)   //    14.) Salt (58% NaCl, 42% MgCl2)
			dens = -5E-09*T*T*T + 2E-05*T*T - 0.5298*T + 2444.1;
		else if (fluid==15)   //    15.) Salt (59.5% LiCl, 40.5% KCl)
			dens = 1E-09*T*T*T - 5E-06*T*T - 0.864*T + 2112.6;
		else if (fluid==16)   //    16.) Salt (59.5% NaF, 40.5% ZrF4)
			dens =  -5E-09*T*T*T + 2E-05*T*T - 0.9144*T + 3837.;
		else if (fluid==17)   //    17.) Salt (60% NaNO3, 40% KNO3)
			dens = MAX(-1E-07*T*T*T + 0.0002*T*T - 0.7875*T + 2299.4,1000.e0);
		else if (fluid==18)
		//dens of Nitrate Salt, [kg/m3]
			dens = MAX(2090 - 0.636 * (T-273.15),1000.e0);
		else if (fluid==19)
		//dens of Caloria HT 43 [kg/m3]
			dens = MAX(885 - 0.6617 * Td - 0.0001265 * Td*Td,100.e0);
		else if (fluid==20)
		//dens of HITEC XL Nitrate Salt, [kg/m3]
			dens = MAX(2240 - 0.8266 * Td,800.e0);
		else if (fluid==21)
		//dens of Therminol Oil [kg/m3]
			dens = MAX(1074.0 - 0.6367 * Td - 0.0007762 * Td*Td,400.e0);
		else if (fluid==22)
		//dens of HITEC Salt, [kg/m3]
			dens = MAX(2080 - 0.733 * Td,1000.e0);
		else if (fluid==23)
		//dens of Dowtherm Q [kg/m3]
			dens = MAX(-0.757332 * Td + 980.787,100.e0);                               // Russ 10-2-03
		else if (fluid==24)
		//dens of Dowtherm RP [kg/m3]
			dens = MAX(-0.000186495 * Td*Td - 0.668337 * Td + 1042.11,200.e0);  //Russ 10-2-03
		else if (fluid==25)
		//dens of HITEC XL Nitrate Salt, [kg/m^3]
			dens = MAX(2240 - 0.8266 * Td,800.e0);
		else if (fluid==26) //Argon
			dens = MAX(P/(208.13*T),1.e-10);
		else if (fluid==27) //Hydrogen
			dens = MAX(P/(4124.*T),1.e-10);
		else if (fluid==28)    //T-91 Steel: "Thermo hydraulic optimisation of the EURISOL DS target" - Paul Scherrer Institut
			dens = -0.3289*Td + 7742.5;
		else if (fluid==29)    //Therminol 66
			dens = -0.7146*Td + 1024.8;
		else if (fluid==30)    //Therminol 59:
			dens = -0.0003*Td*Td - 0.6963*Td + 988.44;

		return dens;
	}

	double specheat( int fluid, double T, double /*P*/) // need to port from type 229
	{
		double spht=1.0;
		double Td = T - 273.15;

		if (fluid==1)   //    1.) Air
			spht = 1.03749 - 0.000305497*T + 7.49335E-07*T*T - 3.39363E-10*T*T*T;
		else if (fluid==2)   //    2.) Stainless_AISI316
			spht = 0.368455 + 0.000399548*T - 1.70558E-07*T*T; //EES
		else if (fluid==3)   //    3.) Water (liquid)
			spht = 4.181e0;  //mjw 8.1.11 
		else if (fluid==6)   //    6.) Salt (68% KCl, 32% MgCl2)
			spht = 1.156;
		else if (fluid==7)   //    7.) Salt (8% NaF, 92% NaBF4)
			spht = 1.507;
		else if (fluid==8)   //    8.) Salt (25% KF, 75% KBF4)
			spht = 1.306;
		else if (fluid==9)   //    9.) Salt (31% RbF, 69% RbBF4)
			spht = 9.127;
		else if (fluid==10)   //    10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
			spht = 2.010;
		else if (fluid==11)   //    11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
			spht = 1.239;
		else if (fluid==12)   //    12.) Salt (58% KF, 42% ZrF4)
			spht = 1.051;
		else if (fluid==13)   //    13.) Salt (58% LiCl, 42% RbCl)
			spht = 8.918;
		else if (fluid==14)   //    14.) Salt (58% NaCl, 42% MgCl2)
			spht = 1.080;
		else if (fluid==15)   //    15.) Salt (59.5% LiCl, 40.5% KCl)
			spht = 1.202;
		else if (fluid==16)   //    16.) Salt (59.5% NaF, 40.5% ZrF4)
			spht = 1.172;
		else if (fluid==17)   //    17.) Salt (60% NaNO3, 40% KNO3)
			spht = -1E-10*T*T*T + 2E-07*T*T + 5E-06*T + 1.4387;
		else if (fluid==18) //Heat Capacity of Nitrate Salt, [J/kg/K]
			spht = (1443. + 0.172 * (T-273.15))/1000.e0;
		else if (fluid==19)
		//Specific Heat of Caloria HT 43 [J/kgC]
			spht = (3.88 * (T-273.15) + 1606.0)/1000.;
		else if (fluid==20)
		//Heat Capacity of HITEC XL Nitrate Salt, [J/kg/K]
			spht = MAX(1536 - 0.2624 * Td - 0.0001139 * Td * Td,1000.e0)/1000.;
		else if (fluid==21)
		//Specific Heat of Therminol Oil, J/kg/K
			spht = (1.509 + 0.002496 * Td + 0.0000007888 * Td*Td);
		else if (fluid==22)
		//Heat Capacity of HITEC Salt, [J/kg/K]
			spht = (1560 - 0.0 * Td)/1000.;
		else if (fluid==23)
		//Specific Heat of Dowtherm Q, J/kg/K
			spht = (-0.00053943 * Td*Td + 3.2028 * Td + 1589.2)/1000.;               // Russ 10-2-03
		else if (fluid==24)
		//Specific Heat of Dowtherm RP, J/kg/K
			spht = (-0.0000031915 * Td*Td + 2.977 * Td + 1560.8)/1000.;       //Russ 10-2-03
		else if (fluid==25)
		//Heat Capacity of HITEC XL Nitrate Salt, [J/kg/K]
			spht = MAX(1536 - 0.2624 * Td - 0.0001139 * Td * Td,1000.e0)/1000.;
		else if (fluid==26)    // Argon
			spht = 0.5203; //Cp only, Cv is different
		else if (fluid==27)    // Hydrogen
			spht = MIN(MAX(-45.4022 + 0.690156*T - 0.00327354*T*T + 0.00000817326*T*T*T - 1.13234E-08*T*T*T*T + 8.24995E-12*T*T*T*T*T - 2.46804E-15*T*T*T*T*T*T,11.3e0),14.7e0);
		else if (fluid==28)    //T-91 Steel: "Thermo hydraulic optimisation of the EURISOL DS target" - Paul Scherrer Institut
			spht = 0.0004*Td*Td + 0.2473*Td + 450.08;
		else if (fluid==29)    //Therminol 66: 
			spht = 0.0036*Td + 1.4801;   
		else if (fluid==30)    //Therminol 59: 
			spht = 0.0033*Td + 1.6132;
		
		return spht;
	}

	virtual int call( double time, double step, int /*ncall*/ )
	{
		double AveSfTemp0Next = m_AveSfTemp0Next;


		// Always Read parameters
		int NumHCEType       = (int)value(I_NumHCETypes);
		double Solar_Field_Area = value(I_Solar_Field_Area);
		double Solar_Field_Mult = value(I_Solar_Field_Mult);	//NB Added on 9-11-06
		int HTFFluid         = (int)value(I_HTFFluid);
//		int LU_FL            = (int)value(I_LU_Fl);  //MJW 7/09
//		int LuFlEr           = (int)value(I_LuFlEr);  //MJW 7/09

		double HCEfieldErr = 0.0;
		
		double SfTi;

		// INPUTS
		if (m_sfti_calc == -999)
		{
			SfTi = m_SfTiO;  // use inlet temperature from last time as starting point for calculating inlet temp.
			if ((int)(time/step) == m_time0) 
			{
				SfTi = m_SfTi_init;
			}
		}
		else
			SfTi = m_sfti_calc; // Use xin(1) as inlet temperature if connected to inlet temperature 

//		double SolarAz = (value(I_SolarAz))*M_PI/180.0;  // TRNSYS convention
		double SolarAz = (value(I_SolarAz)-180.0)*M_PI/180.0;  

// testing with TRNSYS input
//		int ndx = (int)(time/step)-1;
//		if ((ndx<0) || (ndx>8759))
//		{
//			// should not be here - through exception
//			ndx=-1;
//		}
//		double SolarAz = m_SolAz[ndx]*M_PI/180.0;  // TRNSYS input testing

		double WndSpd = value(I_WndSpd);
// testing with TRNSYS input
//		double WndSpd = m_WndSpd[ndx];  // TRNSYS input testing


//		double Insol_Beam_Normal = value(I_Insol_Beam_Normal)/3.6; // Direct normal insolation in W/m2
		double Insol_Beam_Normal = value(I_Insol_Beam_Normal); // Direct normal insolation in W/m2
		double Tamb = value(I_AmbientTemperature);




		double Stow_Angle = value(I_Stow_Angle) * M_PI / 180.0; // stow angle input converted to radians
		double DepAngle = value(I_DepAngle) * M_PI / 180.0;  // deploy angle converted to radians (typically 10)

		double IamF0 = value(I_IamF0);
		double IamF1 = value(I_IamF1);
		double IamF2      = value(I_IamF2);
		double Ave_Focal_Length = value(I_Ave_Focal_Length);
		double Distance_SCA = value(I_Distance_SCA);
		double Row_Distance = value(I_Row_Distance);
		double SCA_aper   = value(I_SCA_aper);
		double SfAvail    = value(I_SfAvail);
		double ColTilt    = value(I_ColTilt)*M_PI/180.0;  //MJW 7/09
//		double ColAz	   = value(I_ColAz)*M_PI/180.0;  //MJW 7/09 -> added *pi/180.
		double ColAz	   = value(I_ColAz)*M_PI/180.0;  //MJW 7/09 -> added *pi/180. // TRNSYS convention
		double NumScas    = value(I_NumScas);
		double ScaLen	   = value(I_ScaLen);
		double MinHtfTemp = value(I_MinHtfTemp);
		double HtfGalArea = value(I_HtfGalArea);

		//Parasitic inputs
		double sfpar      = value(I_SfPar);
		//double SfParPF   = value(I_SfParPF);
		double ChtfPar   = value(I_ChtfPar);
		//double CHTFParPF = value(I_ChtfParPF);
		double CHTFParF0 = value(I_CHTFParF0);
		double ChtfParF1 = value(I_CHTFParF1);
		double ChtfParF2 = value(I_CHTFParF2);
		double AntiFrPar = value(I_AntiFrPar);
		double Site_Lat    = value(I_Site_Lat)*M_PI/180.0;
		//double Site_LongD  = value(I_Site_LongD)*M_PI/180.0; // trnsys has negative convention
//		double SHIFT       = value(I_SHIFT)*M_PI/180.0;
		//double SHIFT = (value(I_Site_LongD) - value(I_SHIFT)*15.)*M_PI/180.0; // from Mike
		
		double SHIFT = value(I_SHIFT)*M_PI / 180.0;
		
		double TurbOutG    = value(I_TurbOutG); //Gross Turbine Output (SETS THE DESIGN POINT) 
		double TurbEffG    = value(I_TurbEffG); //Gross Turbine Eff (SETS THE DESIGN POINT) 

		// Initialize important temperature values
		// SfOutTemp is the default design outlet loop temperature (def = 391 C)
		// SfInTemp is design inlet loop temperature  (def = 293 C)
		double SfInTempD = value(I_SfInTempD);
		double SfOutTempD = value(I_SfOutTempD);
		double AveSfTempD = (SfOutTempD + SfInTempD)/2.0;
//		double ColType = value(I_ColType);
		double TrkTwstErr= value(I_TrkTwstErr);
		double GeoAcc = value(I_GeoAcc);
		double MirRef = value(I_MirRef);
		double MirCln = value(I_MirCln);
		double ConcFac = value(I_ConcFac);
		double ColFactor = TrkTwstErr * GeoAcc * MirRef * MirCln * ConcFac;
		double ColFieldErr = ColFactor;
		double SfPipeHl300 = value(I_SfPipeHl300); // Solar Field pipe   heat losses at design temp, W/m2
		double SfPipeHl1 = value(I_SfPipeHl1);   // Solar Field piping heat losses at reduced temp, W/m2
		double SfPipeHl2 = value(I_SfPipeHl2);
		double SfPipeHl3 = value(I_SfPipeHl3);


		// Initialize important design values at design inlet and outlet loop temperatures
		double H_outD = H_fluid(SfOutTempD, HTFFluid);		//Enthalpy of HTF at Collector Outlet [J/kg]
		double H_inD =  H_fluid(SfInTempD, HTFFluid);		//Enthalpy of HTF at Collector Inlet [J/kg]

		// *************************************************************
		// QsfDesign = TurbOutG / TurbEffG - This is the power block design
		// *************************************
		//
		// Nate Add Solar Multiple to Replace 2.1 in the equation below.
		//
		// *************************************

		//QsfDesign = TurbOutG / TurbEffG * 2.1 //HP added solar multiple but should read in from SAM

		double QsfDesign = TurbOutG / TurbEffG * Solar_Field_Mult; //[MW] HP added solar multiple but should read in from SAM

		double SfMassFlowD = (QsfDesign * 1000000.0) / (H_outD - H_inD);		//Kg/sec

		//Delt is decimal fraction of hour to produce 1, 2 or 4 timesteps/hr
		//NB TimeSteps = 1.0 / Delt
		//double Delt = step/3600; // step in seconds
		//int TimeSteps = int(1.0 / Delt);
		int TimeSteps = int(1.0 / m_delt);

		//double time_hour = (time+3600)/3600;
		double time_hour = time / 3600.0;

		int Julian_Day = int(time_hour/24)+1;
		//int StartDay = int(m_time0/24) + 1;
		//int StopDay = int(m_tFinal/24) + 1;

		// Hour of day in standard time
		double  TimeDay = time_hour - ((Julian_Day-1)*24.0);

		double TSnow;
		if ((TimeDay - int(TimeDay)) == 0.00)
			TSnow = 1.0;
		else
			TSnow = 1.0/(TimeDay - int(TimeDay));

		// Duffie & Beckman 1.5.3b
		double B = (Julian_Day-1)*360.0/365.0*M_PI/180.0;

		// Eqn of time in minutes
		double EOT = 229.2 * (0.000075 + 0.001868 * cos(B) - 0.032077 * sin(B)	- 0.014615 * cos(B*2.0) - 0.04089 * sin(B*2.0));

		// Declination in radians (Duffie & Beckman 1.6.1)
		double Dec = 23.45 * sin(360.0*(284.0+Julian_Day)/365.0*M_PI/180.0) * M_PI/180.0;

		// Solar Noon in hours
		//SolarNoon = 12 - ((StdLongD - Site_LongD)*180.0/pi) / 15 - EOT / 60
		double SolarNoon = 12 - ((SHIFT)*180.0/M_PI) / 15 - EOT / 60;

		// Number of daylight hours
		//double DayLightHrs = 2.0 / 15.0 * acos(-tan(Site_Lat) * tan(Dec)) * 180.0 / M_PI;
	
		// Sunrise and set in hours
		//double SunRise = SolarNoon - (DayLightHrs / 2.0);
		//double SunSet = SolarNoon + (DayLightHrs / 2.0);

		// Deploy & stow times in hours
		// Calculations modified by MJW 11/13/2009 to correct bug
		DepAngle = MAX(DepAngle,1.e-6);
		double DepHr1 = cos(Site_Lat) / tan(DepAngle);
		double DepHr2 = -tan(Dec) * sin(Site_Lat) / tan(DepAngle);
		double DepHr3 = SIGN(1.e0,tan(M_PI-DepAngle))*acos((DepHr1*DepHr2 + sqrt(DepHr1*DepHr1-DepHr2*DepHr2+1.0)) / (DepHr1 * DepHr1 + 1.0)) * 180.0 / M_PI / 15.0;
		double DepTime = SolarNoon + DepHr3;

		Stow_Angle = MAX(Stow_Angle,1.e-6);
		double StwHr1 = cos(Site_Lat) / tan(Stow_Angle);
		double StwHr2 = -tan(Dec) * sin(Site_Lat) / tan(Stow_Angle);
		double StwHr3 = SIGN(1.e0,tan(M_PI-Stow_Angle))*acos((StwHr1*StwHr2 + sqrt(StwHr1*StwHr1-StwHr2*StwHr2+1.0)) / (StwHr1 * StwHr1 + 1.0)) * 180.0 / M_PI / 15.0;
		double StwTime = SolarNoon + StwHr3;

		// HrA and HrB are used for book keeping.  Accomodates 1, 2 and 4 timesteps/hr
		double HrA = int(TimeDay) - 1.0 + (TSnow-1.0)/TimeSteps;
		double HrB = int(TimeDay) - 1.0 + (TSnow/TimeSteps);

		// Ftrack is the fraction of the time period that the field is tracking. MidTrack is time at midpoint of operation
		double Ftrack,MidTrack,StdTime,SolarTime;

		// Solar field operates
		if ((HrB > DepTime) && (HrA < StwTime)) 
		{
			// solar field deploys during time period
			if (HrA < DepTime) 
			{
				Ftrack = (HrB - DepTime) / TimeSteps;
				MidTrack = HrB - Ftrack * 0.5 / TimeSteps;
			}
			// Solar field stows during time period
			else if (HrB > StwTime) 
			{
				Ftrack = (StwTime - HrA) / TimeSteps;
				MidTrack = HrA + Ftrack * 0.5 / TimeSteps;
			}
			// solar field operates during entire period
			else
			{
				Ftrack = 1.0;
				MidTrack = HrA + 0.5 / TimeSteps;
			}
		}
		// solar field doesn't operate
		else
		{
			Ftrack = 0.0;
			MidTrack = HrA + 0.5 / TimeSteps;
		}

		StdTime = MidTrack;
		SolarTime = StdTime+((SHIFT)*180.0/M_PI)/15.0+ EOT/60.0;

		// hour angle (arc of sun) in radians
		double Hour_Angle=(SolarTime-12.0)*15.0*M_PI/180.0;

		// B. Stine equation for Solar Altitude angle in radians
		// change to Type 16 output
		double SolarAlt = asin(sin(Dec)*sin(Site_Lat)+cos(Site_Lat)*cos(Dec)*cos(Hour_Angle));

		// AzNum and AzDen just checking to prevent error
		double AzNum = (sin(Dec)*cos(Site_Lat)-cos(Dec)*cos(Hour_Angle)*sin(Site_Lat));
		double AzDen = cos(SolarAlt);

		if (fabs(AzNum-AzDen)<= 0.0001) 
			AzDen = AzDen + 0.01;

		// cos theta
		// calculation of solar incidence angle for trough
		// Stine reference
		double CosTh = sqrt(1 - pow(cos(SolarAlt-ColTilt) - cos(ColTilt) * cos(SolarAlt) * (1 - cos(SolarAz -ColAz)) , 2) );

		// Theta in radians
		double Theta = acos(CosTh);

		// Calculation of Tracking Angle for Trough. Stine Reference
		double TrackAngle = atan ( cos(SolarAlt) * sin(SolarAz-ColAz) /     
					 (sin(SolarAlt-ColTilt)+sin(ColTilt)*cos(SolarAlt)*(1-cos(SolarAz-ColAz))) );

		// Incident Angle Modifier - IAM
		// based on LS-2 Testing at Sandia
		// based on Figure 10, p12 SAND94-1884
		// K=IAM/cos(IA)
		double IAM;
		if (CosTh == 0) 
			IAM = 0;
		else
			IAM = IamF0 + IamF1 * Theta / CosTh + IamF2 * Theta * Theta / CosTh;


		// EndLoss - light that reflects off end of collector plus the light that reflects from one SCA to the next
		// Hank's approach - not validated
		double EndGain = Ave_Focal_Length * tan(Theta) - Distance_SCA;
		if (EndGain < 0) 
			EndGain = 0;
		

		// NumScas is # of SCAs in a row; 4 at SEGS
		double EndLoss = 1 - (Ave_Focal_Length * tan(Theta) - (NumScas - 1) / NumScas * EndGain) / ScaLen;

		// Row to Row Shadowing Lossess
		double PH = M_PI / 2.0 - TrackAngle;

		// Row_Distance is distance between rows of SCA; 15m @ SEGS
		// SCA_Aper is SCA aperature; 5m LS-2
		double RowShadow = fabs(sin(PH)) * Row_Distance / SCA_aper;
   
		if ((RowShadow < 0.5) || (SolarAlt < 0)) 
			RowShadow = 0;
		else if (RowShadow > 1) 
			RowShadow = 1;





		//*****************************************************************************************************************
		//NB BEGIN REPEAT-CODE TO SEARCH FOR CONVERGENCE OF THE SOLAR FIELD OUTLET TEMPERATURE BASED ON INLET TEMPERATURE
		//NB THIS LOOP SEEMS TO BE NECESSARY DUE TO THE FACT THAT THE HEAT LOSSES ARE ALL CALCULATED BASED ON THE 
		//NB AVERAGE LOOP TEMPERATURE AND NOT THE INLET
		//NB THIS REPEAT-CODE SECTION ENDS JUST AFTER CALCULATION OF THE OUTPUT TEMPERATURE
		//NB IT REPEATS IF THE NEW OUTLET TEMPERATURE IS MORE THAN .1 DEGREES DIFFERENT THAN
		//NB THE PREVIOUSLY CALCULATED OUTLET TEMPERATURE ??? IS THIS ACCURATE ENOUGH????
		//NB IF IT HAS TO ITERATE MORE THAN 1000 TIMES, IT BAILS AND ISSUES AN ERROR MESSAGE
		int ITER = 0;
		bool CALCSFTi = false;
		double SfTo_hold = 1000;
		double SfTo=m_SfToO;

		double AveSfTemp;
		double RecHL;
		double ColOptEff, SfOptEff;
		double Qabs, dTemp;
		double SfPipeHl, Qhl, Qcol, Qnip;
		double QnipCosTh, Qdni, QsfNipCosTh;
		double Qsf, QsfHceHl, QsfPipeHl;
		int qmode;
		double QsfAbs,QHtfFreezeProt,SfMassFlow, H_thermMin;

		double QsfWarmUp, SfLoad;

		double Ttemp, HtfVolGal, HtfMassKg, dThtf;
		
		//DO WHILE ((fabs(SfTo - SfTo_hold) > 0.1).AND.(CALCSFTi.EQ..FALSE.)) // HP Changed 12-08-06
		//DO WHILE ((fabs(SfTo - SfTo_hold) > 0.1).OR.(CALCSFTi.EQ..FALSE.)) // HP Changed AND 12-08-06
		// HP Was not recalculating the thermal losses with the final temperatures

		do // HP Changed 12-08-06 ***************************************************************
		{
			ITER++;
//			SfTo_hold = m_SfToO;  // if repeating, set the old SFTo to the _hold for the next iteration.
			SfTo_hold = SfTo;  // if repeating, set the old SFTo to the _hold for the next iteration.

			if ((ITER==1) && (m_sfti_calc == -999))
			{
			//SFTi is calculated and not an input || MJW added the iteration constraint 6/21/2010
				CALCSFTi = true;  // Set this to true so this loop only is called once
				SfTi = m_SfTiO;       // Use the values from last time for the calculations
				SfTo = m_SfToO;       // Use the values from last time for the calculations
			}

			if (ITER >= 10000) 
			{
				// Fatal error. NOT SURE IF THIS WILL BE THE RIGHT ERROR MESSAGE BUT FOR NOW...
			//	call Messages(-1,'CSP Trough model exceeded internal iteration limit.','fatal',info(1),info(2))
				message( TCS_WARNING, "Warning - Empirical trough (805) model exceeded interal iteration limit" );		// 7.3.13 twn: Changed from 'fatal' to 'warning' in TRNSYS to stop simulation from crashing for some weather files
				break;
				//return 1;
			}

			//********* RECEIVER HEAT LOSS CALCUALATIONS ***************************
			// The following chunk of code calculates the receiver heat loss
			// according to Hank Price's new (4-28-06) heat loss model

			RecHL = 0.0;
			HCEfieldErr=0.0;

			// --- Calculations for each HCE Type --- 

			// HP Why do we have to do this every iteration???

			double HLWind, HLTerm1, HLTerm2, HLTerm3, HLTerm4, HL;


			for (int n=0;n< NumHCEType; n++)
			{


				if (SfTi == SfTo)
					SfTo = SfTi + 0.1;		//HP Keeps HL curve fits from blowing up

				m_HCEfactor[n] = m_HCEFrac[n] * m_HCEdust[n] * m_HCEBelShad[n] * m_HCEEnvTrans[n] * m_HCEabs[n] * m_HCEmisc[n];
				HCEfieldErr  = HCEfieldErr + m_HCEfactor[n];

				//HLWind = MAX(HCEMinWind(n),WndSpd)	//Keeps curve fits from blowing up.
													//MJW 11/09  Recommend removing this restriction
				HLWind = MAX(WndSpd, 0.0); //MJW 6/29/2010 Instead enforce a positive windspeed. Some datasets include negative windspeeds.

				// 7.7.2016 twn: these temperatures should be in C, per Burkholder & Kutscher 2008
				HLTerm1 = (m_HCE_A0[n]+m_HCE_A5[n]*pow(HLWind,0.5))*(SfTo-SfTi);
    
				HLTerm2 = (m_HCE_A1[n]+m_HCE_A6[n]*sqrt(HLWind))*((pow(SfTo,2)-pow(SfTi,2))/2.0-Tamb*(SfTo-SfTi));
		
				HLTerm3 = ((m_HCE_A2[n]+m_HCE_A4[n]*(Insol_Beam_Normal * CosTh * IAM))/3.0)*(pow(SfTo,3)-pow(SfTi,3));

				HLTerm4 = (m_HCE_A3[n]/4.0)*(pow(SfTo,4)-pow(SfTi,4));

				HL = (HLTerm1 + HLTerm2 + HLTerm3 + HLTerm4)/(SfTo-SfTi);		//[W/m]

				// Convert Receiver HL from W/m of receiver to W/m2 of collector aperture
				RecHL = RecHL + (m_PerfFac[n] * m_HCEFrac[n] * HL / m_RefMirrAper[n]);
			}
			// --- Receiver HEAT LOSS Calculations for each HCE Type --- end


			if (RecHL < 0)  // Check to make sure RecHL are not less than zero.
				RecHL = 0;
		

			//     ' SF Thermal Calc
			//		
			//		Qcol = Qabs - Qhl
			//	
			//		Where:
			//			Qabs = CosTh * Insol_Beam_Normal * SfOptEff * SfAvail
			//			SfOptEff = ColOptEff * RowShadow * EndLoss * IAM
			//			ColOptEff = HCEdust * HCEBelShad * HCEEnvTrans * HCEabs * HCEmisc
			//			Qhl = RecHL + SfPipeHl
			//
			// ColOptEff is the collector optical efficiency; It is a product of factors accounting for twist,
			// geometry, reflectivity, cleanliness, and concentration error. Below, ColOptEff being initialized.

			ColOptEff = ColFieldErr * HCEfieldErr * Ftrack;  //MJW Added ftrack -11/23/09
			SfOptEff = ColOptEff * RowShadow * EndLoss * IAM;
					
			// Heat absorbed by collectors in W/m2
			Qabs = CosTh * Insol_Beam_Normal * SfOptEff * SfAvail;
			
			// Difference between average solar field temperature from previous
			// timestep and ambient temperature
			AveSfTemp = (SfTi+SfTo)/2.0;
			dTemp = AveSfTemp - Tamb;

			// Solar Field pipe heat losses at design temp, W/m2
			SfPipeHl = (SfPipeHl3 * dTemp * dTemp * dTemp + SfPipeHl2 * dTemp * dTemp + SfPipeHl1 * dTemp) * SfPipeHl300;
			Qhl = RecHL + SfPipeHl;
			Qcol = Qabs - Qhl;

			if (Qcol < 0) 
				Qcol = 0;

			// Solar Field Thermal Delivery Calculation
			// Uses Data from Qcol to determine solar field output
			Qnip = Insol_Beam_Normal;
			QnipCosTh = Qnip*CosTh;

			// 
			Qdni = Qnip * Solar_Field_Area / 1000000.0	;			// incident direct normal radiation on solar field, MWt
			QsfNipCosTh = Qnip*CosTh* Solar_Field_Area / 1000000.0;	// radiation in the plane of the collectors, MWt
			QsfAbs = Qabs * Solar_Field_Area / 1000000.0;			// energy absorbed by receiver (before thermal losses), MWt
			QsfHceHl = RecHL * Solar_Field_Area / 1000000.0;			// energy lost by HCEs/receivers in field, MWt
			QsfPipeHl = SfPipeHl * Solar_Field_Area / 1000000.0;		// energy lost by header piping in field, MWt
			Qsf = Qcol * Solar_Field_Area / 1000000.0;				// net energy delivered by solar field, MWt

			QHtfFreezeProt = 0;
		//	QHtfFpTes = 0
		//	QHtfFPHtr = 0
			SfMassFlow = 0;

			//H_thermMin = 1000 * (-18.34 + 1.498 * MinHtfTemp + 0.001377 * MinHtfTemp**2)
			H_thermMin = H_fluid(MinHtfTemp+273.15, HTFFluid);

			Ttemp = 25.0;			//HP Commented 12-11-06

			// dens_fluid(T, fluidnum)
		//	HtfDfnT = 67.3091 - 0.020566 * (Ttemp) - 0.000014481 * (Ttemp) ** 2		//HP Commented 12-11-06
			HtfVolGal = HtfGalArea * Solar_Field_Area;
		//	HtfMassLb = HtfVolGal / 7.48 * (67.3091 - 0.020566 * (Ttemp) - 0.000014481 * (Ttemp) ** 2)		//HP Commented 12-11-06
			HtfMassKg = HtfVolGal / 264.2 * density(HTFFluid,Ttemp+273.15,0.0); //MJW 8.17.2010 dens_fluid(ttemp, HtfFluid)	//HP Added 12-11-06

			
			QsfWarmUp = 0.0;

			// this block of code calculates critical temperatures.
			// AveSfTemp - the ave solar field htf temperature,C, at end of timestep
			// SfTi - the loop inlet htf temperature,C, at end of timestep	
			// SfTo - the loop outlet htf temperature,C, at end of timestep
			// SfMassFlow - the htf mass flow rate in or out of entire field, kg/s	
	
			//-------MJW 6/2010
			//The original code didn't correctly allow temperature iteration. The corrected iteration method (see beginning of the 
			//do-loop, MJW comment) sometimes encounters a non-resolvable loop if heat loss + qsf ~= 0. If more than 10 iterations
			//occur, this is the case. Limit the allowed number to 10 and keep with the previously selected solar field mode.
			//**Note that this change has a significant impact on heat loss and annual energy production (~+4% for SAM default case)**
			if (ITER<=10)         //MJW|| for >10 iterations, the results don't change.
			{
				if (Qsf <= 0) 
					qmode = 0;
				else
					qmode = 1;
			}
			//-------end MJW 6/2010
    
			switch(qmode)
			{
				case 0:
		//		AveSfTemp = (T_fluid(H_fluid(AveSfTemp0, HtfFluid) - (Qhl*Solar_Field_Area*3600 / Timesteps / (HTFmasslb/2.2)),HTFFluid))
				AveSfTemp = (T_fluid(H_fluid(m_AveSfTemp0+273.15, HTFFluid) - (Qhl*Solar_Field_Area*3600 / TimeSteps / HtfMassKg),HTFFluid))-273.15;
				QsfWarmUp = 0.0;

				if (AveSfTemp <= MinHtfTemp) 
				{
					QHtfFreezeProt = (H_fluid(MinHtfTemp+273.15, HTFFluid) - H_fluid(AveSfTemp+273.15, HTFFluid)) * HtfMassKg / 3600 / 1000000.0;
					AveSfTemp = MinHtfTemp;
					// QHtfFreezeProt comes from heater or TES
				}
				else
				{
					QHtfFreezeProt = 0;
				}
				//IF (
		
		//		QHtfFPHtr = QHtfFreezeProt
		//		QHtfFpTES = QHtfFreezeProt

				AveSfTemp0Next = AveSfTemp;

				// Specific Heat of Therminol Oil, J/kg/K (T [C])
				dThtf = (QsfHceHl + QsfPipeHl) * 1000000 / SfMassFlowD / (specheat(HTFFluid, AveSfTemp+273.15, 0.e0)*1000.);
				if (m_sfti_calc == -999) 
				   SfTi = AveSfTemp + dThtf / 2.0 + 0.001;

				SfTo = AveSfTemp - dThtf / 2.0 - 0.001;
				SfLoad = 0;
				SfMassFlow = 0;
				break;

				case 1:
//	updated 12/18/12
//				if ((Qsf > 0) && (AveSfTemp < AveSfTempD))
				if ((Qsf > 0) && (m_AveSfTemp0 < AveSfTempD))
				{
						//NB SEEMS LIKE THIS SECTION HAPPENS IF QSF IS POSITIVE AND PREVIOUS PERIOD WAS BELOW DESIGN TEMPERATURE
					QsfWarmUp = (H_fluid(AveSfTempD+273.15, HTFFluid) - H_fluid(m_AveSfTemp0+273.15, HTFFluid)) * HtfMassKg / 3600.0 / 1000000.0;

					if (Qsf / TimeSteps > QsfWarmUp)
					{
						// NB IF THERE IS MORE THAN ENOUGH ENERGY TO REACH THE DESIGN POINT
						Qsf = Qsf - QsfWarmUp * TimeSteps;
						AveSfTemp = AveSfTempD;
						AveSfTemp0Next = AveSfTempD;
							// NB ADDED SFTO CALC
						if ( m_sfti_calc == -999) 
							SfTi = SfInTempD;
						SfTo = (2.0*AveSfTemp) - SfTi;
					}
					else
					{
						//NB NOT ENOUGH ENERGY TO REACH THE DESIGN POINT
						AveSfTemp = (T_fluid(H_fluid(m_AveSfTemp0+273.15, HTFFluid) + Qcol * Solar_Field_Area * 3600.0 / TimeSteps / HtfMassKg, HTFFluid))-273.15;
						QsfWarmUp = (H_fluid(AveSfTemp+273.15, HTFFluid) - H_fluid(m_AveSfTemp0+273.15, HTFFluid)) * HtfMassKg / 3600.0 / 1000000.0;
						AveSfTemp0Next = AveSfTemp;
						Qsf = 0;
							//NB ADDED SFTO CALC
						if (m_sfti_calc == -999) 
						{
						   // HP Changed 11-26-06
						   // SFTi = SfInTempD   //Unless have the inlet temperature, assume inlet is design point during startup
						   // SFTi = AveSfTemp - 10.  
							SfTi = AveSfTemp - QsfWarmUp/QsfDesign*(SfOutTempD-SfInTempD);
						}
						SfTo = (2.0*AveSfTemp) - SfTi;
					}
				}
				else
				{
					//NB IT ISN'T ALLOWED TO EXCEED THE DESIGN POINT IN AVERAGE TEMPERATURE
					AveSfTemp = AveSfTempD;
					AveSfTemp0Next = AveSfTempD;
					QsfWarmUp = 0.0;
					if (m_sfti_calc == -999) 
						SfTi = SfInTempD;
					//NB ADDED SFTO CALC
					SfTo = (2.0*AveSfTemp) - SfTi;  // this works either way if SFti is input or calculated (above)
				}

				SfLoad = Qsf / QsfDesign;
				SfMassFlow = Qsf * 1000000.0 / (H_fluid(SfTo+273.15, HTFFluid) - H_fluid(SfTi+273.15, HTFFluid)); //anytime Qsf>0
        
			// Here is where we add max and min on solar field???
         
			}

			//NB HERE IS WHERE THE LOOP ENDS TO ITERATE ON THE OUTLET TEMPERATURE BY RECALCULATING THE HEAT LOSSES, ETC.
		
			if((fabs(SfTo - SfTo_hold) < 0.1) && (CALCSFTi )) break; //HP Added 12-08-06
			}
		//if((fabs(SfTo - SfTo_hold) < 0.1) && (CALCSFTi )) break; //HP Added 12-08-06
		//while ((fabs(SfTo - SfTo_hold) >= 0.1) || (!CALCSFTi ));
		while (true);
		//END DO // **** HP Changed 12-08-06

		//MJW 8/09 Load the average solar field temp into storage for each iteration
// shj - private variable here
		//stored(7) = AveSfTemp0Next
		//call setStorageVars(stored,nS,INFO)

		double EparSf,EparChtf,EparAnti,SFTotPar;
		//CALCULATE PARASITICS FOR SOLAR FIELD (NOT IN ORIGINAL MJ VERSION)

		//   '  Solar field parasitics
        
		if (SfLoad > 0.01)                // Solar Field in Operation
		{
		//   EparSf = SfPar * SfParPF     //  Solar Field (Loc and motor parasitics)
		   EparSf = sfpar;																		// HP Changed 12-12-06
		//   EparChtf = ChtfPar * ChtfParPF * (ChtfParF0 + ChtfParF1 * SfLoad + ChtfParF2 * (SfLoad**2.))
		   EparChtf = ChtfPar  * (CHTFParF0 + ChtfParF1 * SfLoad + ChtfParF2 * pow(SfLoad,2));	//HP Changed 12-12-06
											//  Cold HTF Pumps (HTF flow to Solar Field)
		   EparAnti = 0;
		}
   
		else                             // Solar field is not in operation
		{
		   EparSf = 0;
		   EparChtf = 0;
		   EparAnti = AntiFrPar;         //  Antifreeze pumping when field is circulated at night
		}
   
		SFTotPar = EparSf + EparChtf + EparAnti;



		double SFmassflowout,RecHLout,SfPipeHlout,Qabsout,ColEff;
		// --- Set the outputs ---
		//Convert Mass flow from kg/sec to kg/hr (more standard TRNSYS units)
		//SfMassFlowOut = SfMassFlow*3600
		SFmassflowout = SfMassFlow;
		//Convert heat loss outputs from Watts/m2 to kJ/hr-m2 (more standard TRNSYS units)
		//RecHLout = RecHl*3.6
		RecHLout = RecHL;
		//SfPipeHlout = SfPipeHl*3.6
		SfPipeHlout = SfPipeHl;
		//Qabsout = Qabs*3.6
		Qabsout = Qabs;

		ColEff = Qcol/MAX(Qnip, 1.e-6);      //MJW 6/2010: Returns NaN when Qnip==0. Enforce limit






		// set stored values for this converged iteration
		m_SfTiO = SfTi;
		m_SfToO = SfTo;
		m_AveSfTemp0 = AveSfTemp0Next;
		m_AveSfTemp0Next = AveSfTemp0Next;



		if( Ftrack == 0 )	
			TrackAngle = 0.0;		// 7.3.13, twn: To match earlier change in TRNSYS



		value(O_SfTo, SfTo);          //C
		value(O_SfMassFlow, SFmassflowout); //kg/hr units {info only}
		value(O_RecHl, RecHLout);      //kj/hr-m^2  {info only}
		value(O_AveSfTemp , AveSfTemp);	   //temperature in C
		value(O_SfPipeHlOut,  SfPipeHlout);   //kj/hr-m^2 {info only}
		value(O_IAM, IAM);          //{info only}
		value(O_Qabsout, Qabsout);       //W/m^2  {info only}
		value(O_Hour_Angle, Hour_Angle);
		value(O_Qsf, Qsf );         // MW of output {used in type806}
		value(O_SFTotPar, SFTotPar);     // MW of parasitics (Should be subtracted from plant output)  {used in type807}
		value(O_QsfWarmup, QsfWarmUp);   //{info only}
		value(O_EndLoss, EndLoss );    //{info only}
		value(O_RowShadow, RowShadow );  //{info only}
		value(O_ColOptEff, ColOptEff );   // HANK - When you ask for the colleff - Is this it, the optical efficiency?? {info only}
		value(O_SfOptEff, SfOptEff );     //{info only}
		value(O_SfTi, SfTi );         //{info only}
		value(O_Qdni, Qdni );         //Used in waterfall outputs
		value(O_EparCHTF, EparChtf );     //{info only}

		value(O_EparSf, EparSf );    // Parasitics of the solar field MWhe
		value(O_EparAnti, EparAnti );  // Antifreeze parasitics MWhe

		value(O_SolarTime, SolarTime ); // 
		value(O_SolarAlt, SolarAlt*180.0/M_PI);   // solar altitude deg
		value(O_Theta, Theta*180.0/M_PI);    // deg
		value(O_CosTheta, CosTh);   // fraction
		value(O_TrackAngle, TrackAngle*180.0/M_PI); // deg 
		value(O_Ftrack, Ftrack);     // MJW 11/09 ::  Ftrack is the fraction of the time period that the field is tracking
		value(O_Qnip, Qnip);       // W/m2 {info only}
		value(O_QnipCosTh, QnipCosTh);  // W/m2 {info only}
		value(O_Qabs, Qabs );      // W/m2 {info only}
		value(O_Qcol, Qcol );       // W/m2 {info only}

		value(O_QsfAbs, QsfAbs );    // MWt {info only}
		value(O_QsfHceHL, QsfHceHl );  // MWt
		value(O_QsfPipeHL, QsfPipeHl ); // MWt
		value(O_QsfWarmUp, QsfWarmUp ); // MWt
		value(O_QhtfFreezeProt, QHtfFreezeProt); // MWt

		value(O_ColEff, ColEff);
		value(O_Qsfnipcosth, QsfNipCosTh); //MWt

		// no solar multiple for these!
		value(O_Qdesign, TurbOutG/TurbEffG);
		value(O_Edesign, TurbOutG);

		// testing TRNSYS conventions
//		value(O_SolarAz, SolarAz*180.0/M_PI);
//		value(O_HCEFieldErr, HCEfieldErr);
//		value(O_ColFieldErr, ColFieldErr);
//		value(O_HCEFactor0, m_HCEfactor[0]);
//		value(O_HCEFactor1, m_HCEfactor[1]);
//		value(O_HCEFactor2, m_HCEfactor[2]);
//		value(O_HCEFactor3, m_HCEfactor[3]);
		 

/* shj - port the code here
		return 1

		end subroutine Type805




		Double Precision Function HtfHfnT(T) 
		implicit none
		Double Precision T
		   //  enthalpy in btu/lb
		   //  temp in F
		   HtfHfnT = -19.477 + 0.35271 * T + 0.000178713 * T * T
		End Function

		Double Precision Function HtfDfnT(T)
		implicit none
		Double Precision T
		   //  density in lbs/ft3
		   //  temp in F
		   HtfDfnT = 67.3091 - 0.020566 * T - 0.000014481 * T * T
		End Function

		Double Precision Function HtfTfnH(H)
		implicit none
		Double Precision H
		   //  enthalpy in Btu/lbs
		   //  temp in F
		   HtfTfnH = 0.000002*(H**3)  - 0.0025*(H**2)  + 2.6377*H  + 54.147
		End Function

*/
		//Note: additional HTF properties used by this type can be found implemented in TYPE229

		//*******************************************************************************

		// NOT NEEDED AS ALREADY A BUIL-IN FUNCTION IN FORTRAN
		//Double Precision Function Log10(x)
		//    Log10 = Log(x) / Log(10)
		//End Function
		//
		//Density of Bed Material [kg/m3]
		//Double Precision Function dens_bed(BedNum) //BedNum: 1 = taconite
		////                                  2 = calcium carbonate
		////                                  3 = gravel
		////                                  4 = marble
		////                                  5 = limestone
		////                                  6 = other
		////                                  7 = carbon steel
		////                                  8 = sand
		////                                  9 = quartzite
		//implicit none
		//INTEGER BedNum
		//
		//If (BedNum == 1) Then
		//        dens_bed = 3800
		//    ElseIf (BedNum == 2) Then
		//        dens_bed = 2710
		//    ElseIf (BedNum == 3) Then
		//        dens_bed = 2643
		//    ElseIf (BedNum == 4) Then
		//        dens_bed = 2680
		//    ElseIf (BedNum == 5) Then
		//        dens_bed = 2320
		//    ElseIf (BedNum == 6) Then
		//        dens_bed = 5280
		//    ElseIf (BedNum == 7) Then
		//        dens_bed = 7854
		//    ElseIf (BedNum == 8) Then
		//        dens_bed = 1515
		//    ElseIf (BedNum == 9) Then
		//        dens_bed = 2640
		//    End If
		//End Function
		//
		////Bed material heat capacity
		//Double Precision Function Cp_bed(BedNum) //BedNum:  1 = taconite
		////                                 2 = calcium carbonate
		////                                 3 = gravel
		////                                 4 = marble
		////                                 5 = limestone
		////                                 6 = other
		////                                 7 = carbon steel
		////                                 8 = sand
		////                                 9 = quartzite
		//implicit none
		//Integer BedNum
		//
		//    If (BedNum == 1) Then
		//        Cp_bed = 0.651  //kJ/kg C
		//    ElseIf (BedNum == 2) Then
		//        Cp_bed = 0.835  //kJ/kg C
		//    ElseIf (BedNum == 3) Then
		//        Cp_bed = 1.065  //kJ/kg C at average temperature of 335 C
		//        //810# + 0.75 * Temp J/kg C orginal expression with temp correction
		//    ElseIf (BedNum == 4) Then
		//        Cp_bed = 0.83   //kJ/kg C
		//    ElseIf (BedNum == 5) Then
		//        Cp_bed = 0.81   //kJ/kg C
		//    ElseIf (BedNum == 6) Then
		//        Cp_bed = 0.651  //kJ/kg C
		//    ElseIf (BedNum == 7) Then
		//        Cp_bed = 0.567  //kJ/kg C
		//    ElseIf (BedNum == 8) Then
		//        Cp_bed = 0.8    //kJ/kg C
		//    ElseIf (BedNum == 9) Then
		//        Cp_bed = 1.105 //kJ/kg C
		//    End If
		//End Function
		//
		////Bed material cost, $/kg
		//Double Precision Function cost_bed(BedNum) //BedNum:  1 = taconite
		////                                   2 = calcium carbonate
		////                                   3 = gravel
		////                                   4 = marble
		////                                   5 = limestone
		////                                   6 = other
		////                                   7 = carbon steel
		////                                   8 = sand
		////                                   9 = quartzite
		//implicit none
		//Integer BedNum
		//
		//    If (BedNum == 1) Then
		//        cost_bed = 0.999
		//    ElseIf (BedNum == 2) Then
		//        cost_bed = 0.999
		//    ElseIf (BedNum == 3) Then
		//        cost_bed = 0.999
		//    ElseIf (BedNum == 4) Then
		//        cost_bed = 0.999
		//    ElseIf (BedNum == 5) Then
		//        cost_bed = 0.999
		//    ElseIf (BedNum == 6) Then
		//        cost_bed = 0.999
		//    ElseIf (BedNum == 7) Then
		//        cost_bed = 0.999
		//    ElseIf (BedNum == 8) Then
		//        cost_bed = 0.142
		//    ElseIf (BedNum == 9) Then
		//        cost_bed = 0.142
		//    End If
		//End Function
		//
		//
		//


		return 0;
	}
};

TCS_IMPLEMENT_TYPE( sam_trough_model_type805, "SAM Trough Model", "Steven Janzou", 1, sam_trough_model_type805_variables, NULL, 0 )

