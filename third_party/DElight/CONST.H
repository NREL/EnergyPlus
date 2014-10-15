/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: R.J. Hitchcock and W.L. Carroll
 *           Building Technologies Department
 *           Lawrence Berkeley National Laboratory
 */

// This work was supported by the Assistant Secretary for Energy Efficiency 
// and Renewable Energy, Office of Building Technologies, 
// Building Systems and Materials Division of the 
// U.S. Department of Energy under Contract No. DE-AC03-76SF00098.

/*
NOTICE: The Government is granted for itself and others acting on its behalf 
a paid-up, nonexclusive, irrevocable worldwide license in this data to reproduce, 
prepare derivative works, and perform publicly and display publicly. 
Beginning five (5) years after (date permission to assert copyright was obtained),
subject to two possible five year renewals, the Government is granted for itself 
and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
license in this data to reproduce, prepare derivative works, distribute copies to 
the public, perform publicly and display publicly, and to permit others to do so. 
NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL 
LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY 
INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE 
WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
*/
//****************************************************************************
//	
// Notice: The procedures described and provided in this software are
//	furnished under a license agreement or nondisclosure agreement.  This
//	software contains CONFIDENTIAL and proprietary information of National
//	Renewable Energy Laboratory and may not be disclosed to any party other
//	than those to whom it was originally distributed and in accordance with the
//	terms of the agreement.  It is against the law to copy the software on any
//	medium except as specifically allowed in the license or nondisclosure
//	agreement.  No part of this software may be reproduced or transmitted
//	in any form or by any means, electronic or mechanical, including
//	photocopying and recording, for any purpose without the express written
//	permission of National Renewable Energy Laboratory
//	
//	(C) Copyright 1993 National Renewable Energy Laboratory  All rights reserved.
//****************************************************************************
//
//	Module:		Const.h
//	Purpose:	Header file for constants

#ifndef _CONST_H_
#define _CONST_H_

#ifndef DONT_DEFINE_DEBUG_NEW
#ifdef _DEBUG		// use the debug version of new!  This will cause the program to run slower.
//#define new DEBUG_NEW
#endif
#endif	// #ifndef DONT_DEFINE_DEBUG_NEW


#define PROGRAM_NAME			"Energy-10"

enum BldgIndex {BLDG1=0, BLDG2, RANKBLDG};	// bldg ids

// OTHER file name
// use the String Table for these file names
// #define CALANDAR_FILE				"calandar.txt"				// list of days of the year for 1978
// #define WINDOW4_GLASS_TYPE_FILE	"w4glass.txt"

// String name name lengths
#define CITY_LEN					25
#define STATE_LEN					25
#define WINDOW4_TYPE_DESC_LEN	70		// The maximum # of chars from the Window4 Type 
												// Description that will be used

// the following constants are used in formating numbers for display in Dialog Boxes.
#define FLOAT_NO_DIGITS				8		// total number of digits in a float number
#define FLOAT_DEC_PRECISION		2		// digits after decimal place
#define FLOAT_XTRA_PRECISION		5		// digits after decimal place - Used for variables that require extra precision
#define FEET_DEC_PRECISION			2
#define INCHES_DEC_PRECISION		2
#define AREA_DEC_PRECISION			2
#define UVAL_DEC_PRECISION			2
#define RVAL_DEC_PRECISION			2
#define TEMP_DEC_PRECISION			1		// Temperature - num of digits after decimal
#define WHOLE_DOLLAR_NUM_DIGITS	3		// Number of digits left of the decimal pt.
#define FRACT_DOLLAR_NUM_DIGITS	2		// Number of digits right of the decimal pt.
#define FLOAT_NO_CHARS			FLOAT_NO_DIGITS+FLOAT_XTRA_PRECISION
										// the size of char arrays that will use above
										// values to output floating point numbers.
#define DOLLAR_FLD_NO_CHARS	WHOLE_DOLLAR_NUM_DIGITS+FRACT_DOLLAR_NUM_DIGITS+5
										// the size of char arrays that will be used 
										// hold dollar fields.  The +5 = 1 for decimal pt,
										// 1 for NULL, 1 for leading space, 2 extras just
										// in case.


#define MAX_NO_WALL_LAYERS					6
#define WALLS_PER_ZONE						20
#define ROOFS_PER_ZONE						8
#define FLOORS_PER_ZONE						8
#define PARTITIONS_PER_ZONE				8
#define WINDOW_TYPES_PER_WALL				10
#define SHADING_TYPES_PER_WALL			WINDOW_TYPES_PER_WALL
#define DOOR_TYPES_PER_WALL				3
#define MASS_WALL_PARTITION				7		// the 8th Partition is used as the Mass Thermal Wall
#define NO_AZIMUTH_ANGLES					13		// Horizon Elevation Shading Angle (Azimuth)
#define HOURS_PER_DAY						24		// 24 hours per day (in most places)
#define NO_SCHED_SHAPES						10		// Number of Shapes in a Schedule
#define DOS_FILE_NAME_LEN_LIMIT			13		// this is the DOS file name limit (8+1+3=12) (1 extra for NULL).
															// Excludes the path.
#define DOS_FILE_NAME_NO_EXT_LEN_LIMIT	9		// this is the DOS file name limit without the dot and the extension
															// (1 extra for NULL).
#define NO_ZONES_PER_BLDG					100		// Number of zones per building														
#define NO_ROOFS_WALLS_PER_BLDG			(NO_ZONES_PER_BLDG*(WALLS_PER_ZONE+ROOFS_PER_ZONE))
															// This is the number of roofs + walls per zone times the number
															// of zones per building
#define SHARED_PARTITIONS_PER_BLDG		8		// shared partitions between zones in a building
#define LTGZONES_PER_ZONE					3		// Number of Lighting Zones allowed in a Thermal Zone.
															// Ask ROB HITCHCOCK what this num should be.
#define WINDOWS_PER_SURFACE				20		// Number of Windows of one type allowed in a Surface.
															// Ask BALCOMB what this num should be.
#define REFERENCES_PER_LIGHTING_ZONE	100		// Number of Reference points per Lighting Zone allowed.
															// Ask ROB HITCHCOCK what this num should be.
#define NO_OF_CONSTR_2_SHOW	3		// this is used in the delete code.
												// It indicates how many Constructions (Wall, Roof, Floor)
												// should be displayed to be in use.
const int	MAX_KEEPS		= 32;				// number of keeps available in any project
const int	MAX_RANKS		= 17;				// number of possible EES's + 1 for the base case


#define NULL_CHAR								'\0'
#define EOL										'\n'
#define MAX_LONG								2147483647	// biggest (signed) long number allowed

/* function return values */
#define PROCEED								2
#define DONE									1
#define OK										0
#define GENERIC_ERROR						-1
#define FILE_NOT_FOUND						-1
#define CORRUPT_FILE							-2
#define FILE_NOT_OPENED						-3
#define WRONG_CITY_FILE						-4
#define RECORD_NOT_FOUND					-5
#define PEN_ERROR								-6
#define BITMAP_ERROR							-7
#define FILE_NOT_CREATED					-8
#define BRUSH_ERROR							-9
#define FONT_ERROR							-10
#define FORM_NOT_COMPLETE					-11
#define INVALID_FIELD_ENTRY				-12
#define INITIALIZATION_FAILED				-13
#define BAD_FILE_SPEC						-14
#define WILD_FILE_SPEC						-15
#define PATH_IN_FILE_SPEC					-16
#define UNINITIALIZED_STRUCT  			-17
#define MEMORY_ERROR							-18
#define NO_SUCH_LIB_ENTRY					-19
#define DATABASE_FILE_CREATION_ERROR	-20
#define DATABASE_FILE_UPDATE_ERROR		-21
#define NO_WINDOW_TYPE_SELECTED			-22
#define NO_MATERIAL_SPECIFIED				-23
#define CORRUPT_DATABASE					-24
#define NO_FRAME_SELECTED					-25
#define NO_GLAZING_SELECTED				-26
#define NO_DOOR_TYPE_SELECTED				-27
#define NO_SHADING_TYPE_SELECTED			-28
#define BAD_RECORD_NO						-29
#define LIB_NOT_FOUND						-30


// Open/Save file I/O function return values
typedef int FILE_ERR;
#define FILE_OK								0
#define BAD_OBJECT							-1		// The object being read or written is bad
#define FILE_BAD_DAY_MONTH					-2
#define FILE_ERROR_CREATING_PRJ_DIR		-3
#define FILE_ERROR_SAVING_PRJ				-4
#define FILE_EOF								-5
#define FILE_ERROR_VARIANT					-6
#define PROJECT_FILE_NOT_FOUND			-7		// See et2.cpp OverwriteVariant() for meaning
#define NO_PROJECT_FILE_NAME				-8		// See et2.cpp OverwriteVariant() for meaning
#define NO_PREVIOUS_SAVE					-9		// See et2.cpp OverwriteVariant() for meaning
#define FILE_ERROR_BACKUP_PRJ				-10	// Could not create a backup copy of the project file
#define FILE_CORRUPT							-11
#ifndef MEMORY_ERROR
#define MEMORY_ERROR							-12	// This could mean that we ran out of memory
#endif
#define COULD_NOT_LOAD_ET_VER_STR		-13	// Could not load the Energy10 Version String
															// from the String Table

const float EPSILON = 0.000001f;		// tollerance used by bEqual to determine float equality
#define CHECKING					2
#define BLDG_ASPECT_RATIO		1.5f		/* building is rectangular with the */
													/* longer side twice as long as the */
													/* shorter side.					*/
//#include <cmath>

//#define PI							3.141592f
const Double          PI = 3.14159265358979323846;
const Double          halfPI = PI / 2.0;
enum    ZeroOrOne   { vl_zero = 0, vl_0 = 0, vl_one = 1, vl_I = 1, vl_1 = 1 };
enum    Block       { vl_Z = 0, vl_B = 1, vl_block = 1 };
enum    Axis        { vl_x, vl_y, vl_z };

// constants used for cnewrite functions moved to cnewrite.h
// 3/6/95

// database/lib function return values are defined in dbConst.h
typedef int DBERR;

#endif // _CONST_H_


