// Methods for parsing XML files.

/*
********************************************************************
Copyright Notice
----------------

Building Controls Virtual Test Bed (BCVTB) Copyright (c) 2008, The
Regents of the University of California, through Lawrence Berkeley
National Laboratory (subject to receipt of any required approvals from
the U.S. Dept. of Energy). All rights reserved.

If you have questions about your rights to use or distribute this
software, please contact Berkeley Lab's Technology Transfer Department
at TTD@lbl.gov

NOTICE.  This software was developed under partial funding from the U.S.
Department of Energy.  As such, the U.S. Government has been granted for
itself and others acting on its behalf a paid-up, nonexclusive,
irrevocable, worldwide license in the Software to reproduce, prepare
derivative works, and perform publicly and display publicly.  Beginning
five (5) years after the date permission to assert copyright is obtained
from the U.S. Department of Energy, and subject to any subsequent five
(5) year renewals, the U.S. Government is granted for itself and others
acting on its behalf a paid-up, nonexclusive, irrevocable, worldwide
license in the Software to reproduce, prepare derivative works,
distribute copies to the public, perform publicly and display publicly,
and to permit others to do so.


Modified BSD License agreement
------------------------------

Building Controls Virtual Test Bed (BCVTB) Copyright (c) 2008, The
Regents of the University of California, through Lawrence Berkeley
National Laboratory (subject to receipt of any required approvals from
the U.S. Dept. of Energy).  All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the
      distribution.
   3. Neither the name of the University of California, Lawrence
      Berkeley National Laboratory, U.S. Dept. of Energy nor the names
      of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

You are under no obligation whatsoever to provide any bug fixes,
patches, or upgrades to the features, functionality or performance of
the source code ("Enhancements") to anyone; however, if you choose to
make your Enhancements available either publicly, or directly to
Lawrence Berkeley National Laboratory, without imposing a separate
written license agreement for such Enhancements, then you hereby grant
the following license: a non-exclusive, royalty-free perpetual license
to install, use, modify, prepare derivative works, incorporate into
other computer software, distribute, and sublicense such enhancements or
derivative works thereof, in binary and source code form.

********************************************************************
*/

///////////////////////////////////////////////////////////
/// \file    utilXml.h
/// \brief   Methods for getting xml values 
///          using the expat libray
///
/// \author  Rui Zhang
///          Carnegie Mellon University
///          ruiz@cmu.edu
/// \date    2009-08-11
///
/// \version $Id: utilXml.c 55724 2009-09-16 17:51:58Z mwetter $
/// 
/// This file provides methods to get general xml values \c getxmlvalue
/// using simple xpath expressions
/// values will be in the same order as they are in the xml file
///
/// This file also provides methods to get the EnergyPlus \c getepvariables.
/// The variables returned will be in the same order as they are in the 
/// configuration file.
/// \sa getxmlvalue()
/// \sa getxmlvaluesf()
/// \sa getepvariables()
///
//////////////////////////////////////////////////////////

#include "utilXml.h"

////////////////////////////////////////////////////////////////
/// Call back functions that will be used by the expat xml parser.
///
///
/// This function is designed for the function \c getepvariables
/// to get input and output variables in the same order as they
/// appear in the configuration file
////////////////////////////////////////////////////////////////
static void XMLCALL
EPstart(void *data, const char *el, const char **attr)
{
  int i, j, k;
  if(ERROR_STATUS == 1) return;
  if( 0 == strcmp(el, "BCVTB-variables") ) 
    return;
  if( 0 == strcmp(el, "variable") ) {
    if( 0 == strcmp(attr[1], "EnergyPlus") )
      source = 0;
    else
      source = 1;
    return;
  } 
  if( 0 == strcmp(el, "EnergyPlus") ) {
    if( 0 == source){
      for(i=0; attr[i]; i++)i=i; 
      if (i != 4) {
        fprintf(stderr, "Error: Variable configuration file invalid.\n"
                        "       Expected two attribute values for source\n"
                        "       of EnergyPlus: 'name' and 'type'.\n");
        ERROR_STATUS = 1; return;
      }
      if ( 0 == strcmp(attr[0], "name") && 0 == strcmp(attr[2], "type") )
        i = 0;
      else if( 0 == strcmp(attr[2], "name") && 0 == strcmp(attr[0],"type"))
        i = 2;
      else {
        fprintf(stderr, "Error: Variable configuration file not valide.\n"
                        "       Expected two attribute values for source\n"
                        "       of EnergyPlus: 'name' and 'type'.\n");
        ERROR_STATUS = 1; return;
      }
        
      if( (strlen(outputVarsName)+strlen(attr[(i+1)%4])+2) > (*strLen) ){
        fprintf(stderr, "Error: Not enough memory allocated for EnergyPlus output.\n"
                        "       Allocated: %d.\n", *strLen);
        ERROR_STATUS = 1; return; 
      }
      strcat(outputVarsName,(char*) attr[(i+1)%4]);
      strcat(outputVarsName, (char*) ";");

      if( (strlen(outputVarsType)+strlen(attr[(i+3)%4])+2) > *strLen ){
        fprintf(stderr, "Error: Not enough memory allocated for EnergyPlus output.\n"
                        "       Allocated: %d.\n", *strLen);
        ERROR_STATUS = 1; return; 
      }
      strcat(outputVarsType, (char*) attr[(i+3)%4]);
      strcat(outputVarsType, (char*) ";");

      *numOutputVars = *numOutputVars + 1;
    }
    else if( 1 == source) {
      for( i=0; attr[i]; i++)i=i; 
      if( 2 != i ){
        fprintf(stderr, "Error: Expecting one input variable in one\n"
                        "       element in xml file.\n");
        ERROR_STATUS = 1; return;
      }
      for( j=0; j< numInputKeys; j++) {
        if( 0 == strcmp((char*)inputKeys[j], (char*)attr[0]) ){
          if( (strlen(inputVars)+strlen(attr[1])+2) > *strLen){
            fprintf(stderr, "Error: Memory allocated for parsed E+ input\n"
                            "       variables name is not enough,\n"
                            "       allocated: %d.\n", *strLen);
            ERROR_STATUS = 1; return; 
          }
          inputVarsType[*numInputVars] = j+1;
          strcat(inputVars,attr[1]);
          strcat(inputVars, ";");
          *numInputVars = *numInputVars + 1;
          break;
        }
      }
      if( numInputKeys == j) {
        fprintf(stderr, "Error: Unknown input variable type: %s.\n", attr[0]);
        ERROR_STATUS = 1; return;
      }
    }
  }
}

////////////////////////////////////////////////////////////////
/// Call back functions that will be used by the expat xml parser
///
/// This function is designed for the function \c getepvariables
/// to get input and output variables in the same order as they
/// appear in the configuration file
////////////////////////////////////////////////////////////////
static void XMLCALL
EPend(void *data, const char *el)
{
  source = -1; 
}


////////////////////////////////////////////////////////////////
///  This method frees the local memory allocated
///   
///\param strArr 1D string array to be freed
///\param n the size of the 1D string array
////////////////////////////////////////////////////////////////
void freeResource(char** strArr, int n){
  int i;
  for(i=0; i<n; i++)
    free(strArr[i]);
  free(strArr);
}


////////////////////////////////////////////////////////////////
///  This method will return the input and output variable for EnergyPlus
///  in sequence
///
///\param fileName the variable configuration file name.
///\param myOutputVarsName Array to store the output variable names found.
///\param myOutputvarsType Array to store the output variable types found.
///\param myNumOutputVars Integer holder to store number of output variables found.
///\param myInputKeys Array to store the input variable keys.
///\param myNumInputKeys Integer holder to store number of input variable keys.
///\param myInputVars Array to store the name of input variables found.
///\param myNumInputVars Integer holder to store number of input variables found.
///\param myInputVarsType Integer array to store the corresponding input variable types in myInputVars.
///\param myStrLen The length of the string that is passed to this function.
///
////////////////////////////////////////////////////////////////

int getepvariables( char*  const fileName, 
                    char*  const myOutputVarsName, 
                    char*  const myOutputVarsType, 
                    int*   const myNumOutputVars, 
                    char*  const myInputKeys, 
                    int*   const myNumInputKeys, 
                    char*  const myInputVars, 
                    int*   const myNumInputVars,
                    int*   const myInputVarsType,
                    int*   const myStrLen){

  FILE * fd;
  XML_Parser p;
  int i, j, count, ret;
  ret = check_variable_cfg_Validate(fileName);
  if(-1 == ret) 
    return -1;

  fd = fopen(fileName, "r");
  if(!fd){
    fprintf(stderr, "Error: Could not open file '%s' when getting EnergyPlus variables.\n", fileName);
    return -1;
  }
  p = XML_ParserCreate(NULL);
  if(!p){
    fprintf(stderr, "Error: Could not allocate memory for parser in function 'getepvariables'.\n");
    return -1;
  }
  
  outputVarsName = myOutputVarsName;
  outputVarsType = myOutputVarsType;
  numOutputVars = myNumOutputVars;
  inputVars = myInputVars;
  inputVarsType = myInputVarsType;
  numInputVars = myNumInputVars;
  numInputKeys = *myNumInputKeys;
  strLen = myStrLen;
  i=0; j=0; count=0;
  inputKeys = NULL;
  while(1){
    if(myInputKeys[count] == '\0') {
      if(inputKeys[i][j] != '\0')
        inputKeys[i][j] = '\0';
      break;
    }
    if(myInputKeys[count] == ','){
      inputKeys[i][j]='\0';
      i++;
      j=0;
      count++;
    }
    else {
      if(j == 0) {
        inputKeys = (char**) realloc(inputKeys, sizeof(char*) * (i+1) );
        if(inputKeys == NULL) {
          fprintf(stderr, "Error: Memory allocation failed in 'utilXml.c'\n");
          return -1;
        }
        inputKeys[i] = NULL;
      }
          
      inputKeys[i] = (char*)realloc(inputKeys[i], sizeof(char) * (j+2) );
      if(inputKeys[i] == NULL) {
        fprintf(stderr, "Error: Memory allocation failed in 'utilXml.c'.\n");
        return -1;
      }
      inputKeys[i][j] = myInputKeys[count];
      j++; count++;
    }
  }
  if((i+1) != *myNumInputKeys ){
    fprintf(stderr, 
	    "Error: Number of input variables keys found does not match:\nFound %d, expected %d\n", 
	    i+1, * myNumInputKeys);
    freeResource(inputKeys, i+1);
    return -1;
  }
  *numOutputVars = 0;
  *numInputVars = 0;
  outputVarsName[0] = '\0';
  outputVarsType[0] = '\0';
  inputVars[0] = '\0';
  source = -1;
  ERROR_STATUS = 0;
  XML_SetElementHandler(p, EPstart, EPend);

  for (;;) {
    int done;
    int len;

    len = (int)fread(Buff, 1, BUFFSIZE, fd);
    if (ferror(fd)) {
      fprintf(stderr, "Error when reading xml variables in '%s'.\n", fileName);
      freeResource(inputKeys, numInputKeys);
      return -1;
    }
    done = feof(fd);

    if (XML_Parse(p, Buff, len, done) == XML_STATUS_ERROR
	|| ERROR_STATUS == 1) {
      fprintf(stderr, "Error: Parser error in file '%s':\n%s\n",
	      fileName,
	      XML_ErrorString(XML_GetErrorCode(p)));
      freeResource(inputKeys, numInputKeys);
      return -1;
    }

    if (done)
      break;
  }
  XML_ParserFree(p);
  fclose(fd);
  freeResource(inputKeys, numInputKeys);
  return 0;
}

////////////////////////////////////////////////////////////////
///  This method will return the input and output variable for EnergyPlus
///  in sequence. The difference with getepvariables is that it does not
///  validate the configuration file
///
///\param fileName the variable configuration file name.
///\param myOutputVarsName Array to store the output variable names found.
///\param myOutputvarsType Array to store the output variable types found.
///\param myNumOutputVars Integer holder to store number of output variables found.
///\param myInputKeys Array to store the input variable keys.
///\param myNumInputKeys Integer holder to store number of input variable keys.
///\param myInputVars Array to store the name of input variables found.
///\param myNumInputVars Integer holder to store number of input variables found.
///\param myInputVarsType Integer array to store the corresponding input variable types in myInputVars.
///\param myStrLen The length of the string that is passed to this function.
///
////////////////////////////////////////////////////////////////

int getepvariablesFMU( char*  const fileName, 
					   char*  const myOutputVarsName, 
					   char*  const myOutputVarsType, 
					   int*   const myNumOutputVars, 
					   char*  const myInputKeys, 
					   int*   const myNumInputKeys, 
				       char*  const myInputVars, 
					   int*   const myNumInputVars,
					   int*   const myInputVarsType,
					   int*   const myStrLen){

  FILE * fd;
  XML_Parser p;
  int i, j, count, ret;
  //ret = check_variable_cfg_Validate(fileName);
  //if(-1 == ret) 
    //return -1;

  fd = fopen(fileName, "r");
  if(!fd){
    fprintf(stderr, "Error: Could not open file '%s' when getting EnergyPlus variables.\n", fileName);
    return -1;
  }
  p = XML_ParserCreate(NULL);
  if(!p){
    fprintf(stderr, "Error: Could not allocate memory for parser in function 'getepvariables'.\n");
    return -1;
  }
  
  outputVarsName = myOutputVarsName;
  outputVarsType = myOutputVarsType;
  numOutputVars = myNumOutputVars;
  inputVars = myInputVars;
  inputVarsType = myInputVarsType;
  numInputVars = myNumInputVars;
  numInputKeys = *myNumInputKeys;
  strLen = myStrLen;
  i=0; j=0; count=0;
  inputKeys = NULL;
  while(1){
    if(myInputKeys[count] == '\0') {
      if(inputKeys[i][j] != '\0')
        inputKeys[i][j] = '\0';
      break;
    }
    if(myInputKeys[count] == ','){
      inputKeys[i][j]='\0';
      i++;
      j=0;
      count++;
    }
    else {
      if(j == 0) {
        inputKeys = (char**) realloc(inputKeys, sizeof(char*) * (i+1) );
        if(inputKeys == NULL) {
          fprintf(stderr, "Error: Memory allocation failed in 'utilXml.c'\n");
          return -1;
        }
        inputKeys[i] = NULL;
      }
          
      inputKeys[i] = (char*)realloc(inputKeys[i], sizeof(char) * (j+2) );
      if(inputKeys[i] == NULL) {
        fprintf(stderr, "Error: Memory allocation failed in 'utilXml.c'.\n");
        return -1;
      }
      inputKeys[i][j] = myInputKeys[count];
      j++; count++;
    }
  }
  if((i+1) != *myNumInputKeys ){
    fprintf(stderr, 
	    "Error: Number of input variables keys found does not match:\nFound %d, expected %d\n", 
	    i+1, * myNumInputKeys);
    freeResource(inputKeys, i+1);
    return -1;
  }
  *numOutputVars = 0;
  *numInputVars = 0;
  outputVarsName[0] = '\0';
  outputVarsType[0] = '\0';
  inputVars[0] = '\0';
  source = -1;
  ERROR_STATUS = 0;
  XML_SetElementHandler(p, EPstart, EPend);

  for (;;) {
    int done;
    int len;

    len = (int)fread(Buff, 1, BUFFSIZE, fd);
    if (ferror(fd)) {
      fprintf(stderr, "Error when reading xml variables in '%s'.\n", fileName);
      freeResource(inputKeys, numInputKeys);
      return -1;
    }
    done = feof(fd);

    if (XML_Parse(p, Buff, len, done) == XML_STATUS_ERROR
	|| ERROR_STATUS == 1) {
      fprintf(stderr, "Error: Parser error in file '%s':\n%s\n",
	      fileName,
	      XML_ErrorString(XML_GetErrorCode(p)));
      freeResource(inputKeys, numInputKeys);
      return -1;
    }

    if (done)
      break;
  }
  XML_ParserFree(p);
  fclose(fd);
  freeResource(inputKeys, numInputKeys);
  return 0;
}


////////////////////////////////////////////////////////////////
/// Stack operation, this function will pop one element from stack
/// and will free the resource unused
////////////////////////////////////////////////////////////////
int stackPopBCVTB(){
  if(0==expStk.top) 
    return -1;
  free((expStk.head)[expStk.top]);
  expStk.head = (char**) realloc(expStk.head, sizeof(char*) * (expStk.top));
  if(expStk.head == NULL) {
    fprintf(stderr, "Error: Memory allocation failed in 'utilXml.c'.\n");
    return -1;
  }
  expStk.top--;
  return expStk.top;
}

////////////////////////////////////////////////////////////////
/// Stack operation, will push one element into the stack
/// and will allocate memory for the new element, hence is deep copy
////////////////////////////////////////////////////////////////
int stackPushBCVTB(char* str){
  if(!str) return -1;
  expStk.top++;
  expStk.head = (char**) realloc(expStk.head, sizeof(char*) * (expStk.top+1));
  if(expStk.head == NULL) {
    fprintf(stderr, "Error: Memory allocation failed in 'utilXml.c'");
    return -1;
  }
  expStk.head[expStk.top] = (char*)malloc(sizeof(char) * (strlen(str)+1) );
  if(expStk.head[expStk.top] == NULL) {
    fprintf(stderr, "Error: Memory allocation failed in 'utilXml.c'");
    return -1;
  }
  strcpy(expStk.head[expStk.top], str);
  return expStk.top;
}


////////////////////////////////////////////////////////////////
/// This is a general function that returns the value according to \c exp
///
/// \c exp mimics the xPath expression.
/// Its format is //el1/../eln[@attr]
/// which will return the \c attr value of \c eln, 
/// where \c eln is the n-th child of \c el1
///
/// Example: //variable/EnergyPlus[@name] will return the name attributes of EnergyPlus
/// which is equivalent to //EnergyPlus[@name]
///
///\param fileName the xml file name.  
///\param exp the xPath expression.
///\param myVals string to store the found values, semicolon separated.
///\param mynumVals number of values found.
///\param myStrLen length of the string that is passed.
////////////////////////////////////////////////////////////////
int 
getxmlvalues(char* const fileName, 
             char* const exp, 
             char* const myVals, 
             int*  const myNumVals,
             int   const myStrLen){

  char* temp;
  int i,j;
  FILE * fd;
  XML_Parser p;
  vals = myVals;
  numVals = myNumVals;
  *numVals = 0;
  strLen = &myStrLen;
  att = NULL;
  expStk.head = NULL;
  expStk.top = -1;
  expStk.cur = -1;
  fd = fopen(fileName, "r");
  if(!fd) {
    fprintf(stderr, "Error: Could not open file '%s'.\n", fileName);
    return -1;
  }
  p = XML_ParserCreate(NULL);
  if (!p) {
    fprintf(stderr, "Error: Could not allocate memory for parser in function 'getxmlvalue'.\n");
    return -1;
  }
  i=2; j=0;
  if(!exp || '\0' == exp[0]) 
    return -1;
  if( exp[0] != '/' || exp[1] != '/')
    return -1;

  temp = NULL;
  while(exp[i] != '\0'){
    if( exp[i] == '/' || exp[i] == '[' || exp[i] == ']') {
      if(0==j && 0==expStk.top) {
        fprintf(stderr, "Error when parsing expression in 'utilXml.c'.\n");
        return -1;
      }
      i++;
      if(strchr(temp, '@'))
        break;
      stackPushBCVTB(temp);
      free(temp);
      temp = NULL;
      j=0;
    }
    else {
      j++;  
      temp = (char*) realloc(temp, sizeof(char)*(j+1));
      if(temp == NULL) {
        fprintf(stderr, "Error: Memory allocation failed in 'utilXml.c'.\n");
        return -1;
      }
      temp[j-1]=exp[i];
      temp[j]='\0';
      i++;
    }
  }
  if(temp[0] == '@'){
    att = (char*) malloc(sizeof(char) * (strlen(temp) ) );
    if(att == NULL) {
      fprintf(stderr, "Error: Memory allocation failed in 'utilXml.c'.\n");
	  free(temp);
      return -1;
    }
    for(i=1; i<strlen(temp); i++) 
      att[i-1] = temp[i];
    att[i-1]='\0';
    free(temp);
  }
  else {
    fprintf(stderr, "Error when parsing expression in 'utilXml.c'.\n");
	free(temp);
	free(att);
	while(i!= -1) stackPopBCVTB();
    return -1;
  }
  expStk.cur = 0;
  if(1 == PARSEVALUE)
    vals[0]='\0';
  *numVals = 0;
  XML_SetElementHandler(p, start, end);

  for (;;) {
    int done;
    int len;

    len = (int)fread(Buff, 1, BUFFSIZE, fd);
    if (ferror(fd)) {
      fprintf(stderr, "Error when reading xml variables in '%s'.\n", fileName);
      return -1;
    }
    done = feof(fd);

    if (XML_Parse(p, Buff, len, done) == XML_STATUS_ERROR) {
      fprintf(stderr, "Error: Parse error in file '%s':\n%s\n",
	      fileName,
              XML_ErrorString(XML_GetErrorCode(p)));
      return -1;
    }

    if (done)
      break;
  }
  if( 0 == *numVals ){
	  fprintf(stderr, "Error: Did not find xml value\n       for expression '%s'.\n       in file '%s'\n", 
		  exp, fileName);
  }
  while( i != -1 ) 
    i = stackPopBCVTB();  
  att = NULL;
  XML_ParserFree(p);
  fclose(fd);
  return 0;

}

////////////////////////////////////////////////////////////////
/// Call back functions that will be used by the expat xml parser
//
/// This function is used for \c getxmlvalues
////////////////////////////////////////////////////////////////
static void XMLCALL
start(void *data, const char *el, const char **attr)
{
  int i;  
  if(0 == strcmp(el, expStk.head[expStk.cur]) && expStk.cur < expStk.top )
    expStk.cur++;
  if(expStk.cur == expStk.top){
    for(i=0; attr[i]; i += 2) {
      if( 0 == strcmp(attr[i], att) ){
        if(1 == PARSEVALUE){
          if( (strlen(vals)+strlen(attr[i+1])+2) > *strLen){
            fprintf(stderr, "Error: Memory allocated for parsed attribute\n"
                             "      values is not enough, allocated: %d.\n", 
                             *strLen);
            *numVals = strlen(vals) + strlen(attr[i+1])+2;
            return; 
          }
          if(vals[0] != '\0')
            strcat(vals, ";");
          strcat(vals, attr[i+1]);
        }
        *numVals = *numVals + 1;
      }
    }
  }
}

////////////////////////////////////////////////////////////////
/// Call back functions that will be used by the expat xml parser
//
/// This function is used for \c getxmlvalues
////////////////////////////////////////////////////////////////
static void XMLCALL
end(void *data, const char *el)
{
  if(!strcmp(el, expStk.head[expStk.cur])&& expStk.cur>0) 
    expStk.cur--; 
}


////////////////////////////////////////////////////////////////
/// This method returns the number of xmlvalues given xPath expressions.
/// This method will call the function \c getxmlvalues
///
/// \c exp mimics the xPath expression.
/// Its format is //el1/../eln[@attr]
/// which will return the \c attr value of \c eln, 
/// where \c eln is the n-th child of \c el1
///
/// Example: //variable/EnergyPlus[@name] will return the name attributes of EnergyPlus
/// which is equivalent to //EnergyPlus[@name]
///
///\param fileName the name of the xml file
///\param exp the xPath expression
////////////////////////////////////////////////////////////////
int getnumberofxmlvalues( char* const fileName,
		                      char* const exp){
  int n, ret;
  char* str;
  int strLen = 0;
  n=0;
  str = NULL;
  PARSEVALUE = 0;
  ret = getxmlvalues(fileName,
                     exp,
                     str,
                     &n,
                     strLen);
  if(-1 == ret){
    fprintf(stderr, "Error: In getnumberofxmlvalues.\n");
    return ret;
  }
  return n;
}

////////////////////////////////////////////////////////////////
/// This method returns the xmlvalues parsed given xPath expressions.
/// This method will first perform a validation check with DTDValidator
/// For compatibility with BCVTB 0.2 this function is mainly for E+ 
/// to get the input and output variables in variables.cfg. Thus the 
/// dtd file for the validity checking is the variables.dtd.
/// Then the function calls \c getxmlvalues to get the variables
/// and appends ";" at the end of the parsed string.
///
/// Return value: 0 normal; -1 error 
///
/// \c exp mimics the xPath expression.
/// Its format is //el1/../eln[@attr]
/// which will return the \c attr value of \c eln, 
/// where \c eln is the n-th child of \c el1
///
/// Example: //variable/EnergyPlus[@name] will return the name attributes of EnergyPlus
/// which is equivalent to //EnergyPlus[@name]
///
///\param fileName the xml file name;  
///\param exp the xPath expression.
///\param atrName the attribute name.
///\param nAtt number of attribute values found.
///\param str string to store the found values, semicolon separated.
///\param strLen the string length allocated
////////////////////////////////////////////////////////////////
int getxmlvaluesf(char* const fileName,
                  char* const exp,
                  char* const atrName,
                  int*  const nVal,
                  char* const str,
                  int* const strLen) {
  ///////////////////////////////////////////////
  /// This part of the code is for compatibility 
  /// with the BCVTB version 0.2 and earlier
  int ret = check_variable_cfg_Validate(fileName);
  if(-1 == ret) 
    return -1;
  //////////////////////////////////////////////
  PARSEVALUE = 1;
  ret = getxmlvalues(fileName,
	                   exp,
	                   str,
	                   nVal,
	                   *strLen);
  if( -1 == ret || *nVal > *strLen )
    return -1;
  if( strlen(str)+2 >= *strLen ) {
    fprintf(stderr, "Error: Not enough memory allocated for parsed xml\n"
                    "       values in getxmlvaluesf, allocated: %zx.\n", 
	    (size_t)*strlen);
    return -1;
  }
  strcat(str,";");
  return 0;

}

////////////////////////////////////////////////////////////////
/// This method returns one xmlvalue for a given xPath expressions.
/// The function will call the function \c getxmlvalues to get the variables
/// without ";" at the end of the parsed string
///
/// Return values: 0 normal; -1 error 
///
/// \c exp mimics the xPath expression.
/// Its format is //el1/../eln[@attr]
/// which will return the \c attr value of \c eln, 
/// where \c eln is the n-th child of \c el1
///
/// Example: //variable/EnergyPlus[@name] will return the name attributes of EnergyPlus
/// which is equivalent to //EnergyPlus[@name]
///
///\param fileName the xml file name.  
///\param exp the xPath expression.
///\param str string to store the found values, semicolon separated.
///\param nVals number of values found.
///\param strLen the string length allocated.
////////////////////////////////////////////////////////////////
int getxmlvalue(char* const fileName,
                char* const exp,
                char* const str,
                int*  const nVals,
                int   const strLen) {
  int ret; 
  PARSEVALUE = 1;
  ret = getxmlvalues(fileName,
	                   exp,
	                   str,
	                   nVals,
	                   strLen);

  if(ret != 0){
    fprintf(stderr,"Error: Error when attempting to parse file '%s'\n",fileName); 
    return -1;
  }
  if(*nVals == 0){
    fprintf(stderr,"Error: No xml value parsed in file '%s'\n",fileName); 
    return -1;
  }
  if(*nVals > 1){
    fprintf(stderr, "Error: More than one xml values parsed, \n"
                    "       while expecting one value. \n"
                    "       number of xml values parsed is: %d\n"
                    "       xPath: '%s'\n",
                    *nVals, exp);
    return -1;
  }
  return 0;
}
////////////////////////////////////////////////////////////////
/// This method checks the validity of the variables 
/// configuration xml file for a given dtd file that is
/// specified in the variables configuration file
/// 
/// Return values: -1 Error in the file
///                 0 File is validate
///
///
////////////////////////////////////////////////////////////////
int check_variable_cfg_Validate(char* const fileName){

  char *BCVTB_HOME, *dtdFileName;
  char *command;
  FILE *dtdF;
  int ret;
#ifdef _MSC_VER /************* Windows specific code ********/
  const char *const xmlPath = "\\lib\\xml\\";
  const char *const jarPath = "\\lib\\xml\\build\\jar\\DTDValidator.jar";
#else  /************* End of Windows specific code *******/
  const char *const xmlPath = "/lib/xml/";
  const char *const jarPath = "/lib/xml/build/jar/DTDValidator.jar";
#endif

  BCVTB_HOME = getenv("BCVTB_HOME");
  if(NULL == BCVTB_HOME){
    fprintf(stderr, "Error: Cannot get environment variable: BCVTB_HOME.\n");
    return -1;
  }
  command = (char*) malloc( sizeof(char) * 
                            ( strlen(BCVTB_HOME) + 
                              strlen(fileName) + 
                              strlen("variables.dtd") 
                              + 20
                            ) 
                            * 3
                          );
  if(NULL == command) {
    fprintf(stderr, "Error: Memory allocation failed in"
                    "       check_variable_cfg_Validate"
                    "       when parsing file '%s'.\n"
                    "       Program aborting.\n", fileName);
    return -1;
  }
  dtdFileName = (char*) malloc( sizeof (char) * ( strlen(BCVTB_HOME) + 30));
  if(NULL == command) {
    fprintf(stderr, "Error: Memory allocation failed in"
                    "       check_variable_cfg_Validate"
                    "       when parsing file '%s'. \n"
                    "       Program aborting.\n", fileName);
    return -1;
  }

  sprintf(dtdFileName, "%s%s%s", BCVTB_HOME, xmlPath, "variables.dtd");
  dtdF = fopen(dtdFileName, "r");
  if( NULL == dtdF ){
    fprintf(stderr, "Error: Cannot open '%s'.\n", dtdFileName);
    return -1;
  }
  else fclose(dtdF);
  sprintf(command, "java -jar \"%s%s\" \"%s\" \"%s%s\"", 
                    BCVTB_HOME, jarPath, fileName, BCVTB_HOME, xmlPath);
  ret = system(command);
  if( ret != 0) 
    return -1;
  else 
    return 0;
}

