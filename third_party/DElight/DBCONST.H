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
//	Module:		dbConst.h
//	Purpose:	Header file for Database constants

#ifndef _DBCONST_H_
#define _DBCONST_H_


// LIBRARY file names
#define BLDG_USES_LIB_NAME			"bldguses.lib"
#define ROOF_LIB_NAME				"roof.lib"
#define WNDW_LIB_NAME				"window.lib"
#define WALL_LIB_NAME				"wall.lib"
#define FLOOR_LIB_NAME				"floor.lib"
#define MATERIAL_LIB_NAME			"material.lib"
#define GLAZING_LIB_NAME			"glazing.lib"
#define FRAME_LIB_NAME				"frames.lib"
#define DOOR_LIB_NAME				"doors.lib"
#define SHADE_LIB_NAME				"shade.lib"
#define HVACSYS_LIB_NAME			"hvacsys.lib"
#define SHAPE_LIB_NAME				"shape.lib"
#define SCHED_LIB_NAME				"schedule.lib"
// LIBRARY file names for ASCII dump
#define BLDG_USES_LIB_ASCII		"bldguses.txt"
#define ROOF_LIB_ASCII				"roof.txt"
#define WNDW_LIB_ASCII				"window.txt"
#define WALL_LIB_ASCII				"wall.txt"
#define FLOOR_LIB_ASCII				"floor.txt"
#define MATERIAL_LIB_ASCII			"material.txt"
#define GLAZING_LIB_ASCII			"glazing.txt"
#define FRAME_LIB_ASCII				"frames.txt"
#define DOOR_LIB_ASCII				"doors.txt"
#define SHADE_LIB_ASCII				"shade.txt"
#define HVACSYS_LIB_ASCII			"hvacsys.txt"
#define SHAPE_LIB_ASCII				"shape.txt"
#define SCHED_LIB_ASCII				"schedule.txt"

//#define HTGSYS_LIB_NAME				"htgsys.lib"
//#define CLGSYS_LIB_NAME				"clgsys.lib"
//#define FANSYS_LIB_NAME				"fansys.lib"
//#define BOILER_LIB_NAME				"boiler.lib"
//#define  CHILR_LIB_NAME				"chilr.lib"

// LIBRARY item name lengths
#define REC_NAME_LEN				60
//#define REC_NAME_LEN				31			// this can be set to a higher number once the CNEwrite functions switch over to using CNEName();
#define CNE_NAME_LEN_LIMIT			15
#define BLDG_USE_NAME_LEN			REC_NAME_LEN
#define ROOF_NAME_LEN				REC_NAME_LEN
#define GLAZING_NAME_LEN			REC_NAME_LEN
#define FRAME_NAME_LEN				REC_NAME_LEN
#define WNDW_NAME_LEN				REC_NAME_LEN
#define WALL_CONSTRC_NAME_LEN		REC_NAME_LEN
#define FLOOR_CONSTRC_NAME_LEN	REC_NAME_LEN
#define MATERIAL_NAME_LEN			REC_NAME_LEN
#define WALL_NAME_LEN				REC_NAME_LEN
#define GLAZING_NAME_LEN			REC_NAME_LEN
#define DOOR_NAME_LEN				REC_NAME_LEN
#define SHADE_NAME_LEN				REC_NAME_LEN
#define HVAC_NAME_LEN				REC_NAME_LEN
#define SHAPE_NAME_LEN				REC_NAME_LEN
#define HEATING_SYS_NAME_LEN		REC_NAME_LEN		// HVAC Heating system
#define COOLING_SYS_NAME_LEN		REC_NAME_LEN		// HVAC Cooling system
#define FAN_SYS_NAME_LEN			REC_NAME_LEN		// HVAC Air Distribution system
#define SCHED_NAME_LEN				REC_NAME_LEN		// Schedule name length.
#define FLOOR_TYPE_NAME_LEN		21					// "Unconditioned Space" is the longest Floor Type
//#define WINDOW_TYPE_LEN				10					// max length of a Window Type (from Window4)
#define DB_HEADER_LEN				50					// length of header for library files

// database/lib function return values
#define DB_OK								 0
#define DB_FILE_ALREADY_OPEN			-1
#define DB_FILE_NOT_OPEN				-2
#define DB_COULD_NOT_OPEN				-3
#define DB_BAD_RECORD					-4
#define DB_DUPLICATE_RECORD			-5
#define DB_EOF_ENCOUNTERED				-6
#define DB_FILE_EMPTY					-7
#define DB_NO_MORE_RECORDS				-8
#define DB_BAD_RECORD_ITEM_NAME		-9
#define DB_RECORD_NOT_FOUND			-10
#define DB_OUT_OF_MEMORY				-11
#define DB_GENERIC_ERROR				-12	// Not sure what caused the Error
#define DB_BOF_ENCOUNTERED				-13
#define DB_COULD_NOT_ADD_RECORD		-14
#define DB_COULD_NOT_MODIFY_RECORD	-15
#define DB_LIB_NAME_VALIDATION_ERR	-16
#define DB_FILE_WITHOUT_HEADER		-17	// file is smaller than would be needed for the header
#define DB_NUM_RECS_NONINTEGRAL		-18	// file does not contain an integral number of records
#define DB_BAD_PARAMETER				-19
#define DB_BAD_GLB_DB_POINTER			-20	// The Global variable for the database is NULL or bad
#define DB_CAN_NOT_DUMP_LIB_VALUES	-21
#define DB_OLDER_VERSION				-22
#define DB_NEWER_VERSION				-23
#define DB_CONVERSION_FAILED			-24	// could not convert an older lib to the new version
#define DB_UNRECOGNIZED_VERSION		-25


#endif // _DBCONST_H_

