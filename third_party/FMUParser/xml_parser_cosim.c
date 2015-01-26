///////////////////////////////////////////////////////
/// \file   xml_parser_cosim.c
///
/// \brief  xml parser for fmu.
///
/// \author Wangda Zuo,
///         Simulation Research Group,
///         LBNL,
///         WZuo@lbl.gov
///
/// \date   2011-10-10
///
/// \version $Id: xml_parser_cosim.c 55724 2011-10-10 17:51:58 wzuo $
///
/// This file is based on xml_parser.c that is copyrighted by
/// QTronic GmbH and that is distributed under the BSD license.
/// The original file, its copyright notice and its license can
/// be found in ../QTronic
///
/// The file has been modified for use with the FMU standard for
/// co-simulation. The original file was developed for model exchange.
/// The original file used 0 as indicator for failure and
/// 1 as indicator for success.
/// The new file uses 0 as indicator for success according to STL.
///
///
///////////////////////////////////////////////////////
/// Copyright notice of original file that served as the basis of this imlementation.
/* -------------------------------------------------------------------------
 * xml_Parser.c
 * A parser for file modelVariables.xml of an FMU.
 * The parser creates an AST (abstract syntax tree) for a given XML file.
 * The root node of the AST is of type ModelDescription.
 * Validation already performed by this parser
 * - check for match of open/close elements (performed by Expat)
 * - check element, attribute and enum value names, all case sensitive
 * - check for each element that is has the expected parent element
 * - check for correct sequence of elements
 * Validation to be performed by this parser
 * - check for each attribute value that it is of the expected type
 * - check that required attributes are present
 * - check that all decalaredType values reference an existing Type
 * - check that dependencies are only declared for outputs and
 *   refer only to inputs
 * Copyright 2010 QTronic GmbH. All rights reserved.
 * -------------------------------------------------------------------------*/

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
#include "xml_parser_cosim.h"
#include "util.h"

const char *elmNames[SIZEOF_ELM] = {
    "fmiModelDescription","UnitDefinitions","BaseUnit","DisplayUnitDefinition","TypeDefinitions",
    "Type","RealType","IntegerType","BooleanType","StringType","EnumerationType","Item",
     "DefaultExperiment","VendorAnnotations","Tool","Annotation", "ModelVariables","ScalarVariable",
    "DirectDependency", "Name", "Real", "Integer", "Boolean", "String", "Enumeration",
    "Implementation", "CoSimulation_StandAlone", "Capabilities", // Names for co-simulation
    "CoSimulation_Tool", "CoSimulationModel", // Names for co-simulation
    "File"
};

const char *attNames[SIZEOF_ATT] = {
    "fmiVersion","displayUnit","gain","offset","unit","name","description","quantity", "relativeQuantity",
    "min","max","nominal","declaredType","start","fixed","startTime","stopTime","tolerance","value",
    "valueReference","variability","causality","alias", "modelName","modelIdentifier","guid","author",
    "version","generationTool","generationDateAndTime","variableNamingConvention","numberOfContinuousStates",
    "numberOfEventIndicators","input",
    "canHandleVariableCommunicationStepSize", "canHandleEvents", "canRejectSteps", "canInterpolateInputs",
    "maxOutputDerivativeOrder", "canRunAsynchronuously", "canSignalEvents",
    "canBeInstantiatedOnlyOncePerProcess", "canNotUseMemoryManagementFunctions",
    "entryPoint", "manualStart", "type", "file"
};

const char *enuNames[SIZEOF_ENU] = {
    "flat","structured","constant","parameter","discrete","continuous",
    "input","output", "internal","none","noAlias","alias","negatedAlias"
};


#define ANY_TYPE -1
#define XMLBUFSIZE 1024
char text[XMLBUFSIZE];       // XML file is parsed in chunks of length XMLBUFZIZE
XML_Parser parser = NULL;    // non-NULL during parsing
Stack* stack = NULL;         // the parser stack
char* data = NULL;           // buffer that holds element content, see handleData
int skipData=0;              // 1 to ignore element content, 0 when recordig content

///////////////////////////////////////////////////////////////////////////////
/// Get string.
///
///\param element Element of xml data
///\param a Attribute
///\return The attribute of a.
///////////////////////////////////////////////////////////////////////////////
const char* getString(void* element, Att a){
    Element* e = (Element*)element;
    const char** attr = e->attributes;
    int i;
    for (i=0; i<e->n; i+=2)
        if (attr[i]==attNames[a])
        	return attr[i+1];
    return NULL;
}

///////////////////////////////////////////////////////////////////////////////
/// Get string.
///
///\param element Model description.
///\param a Attribute
///\return The attribute of a.
///////////////////////////////////////////////////////////////////////////////
const char* getMyString(ModelDescription* element, Att a){
    Element* e = (Element*)element;
    const char** attr = e->attributes;
    int i;
    for (i=0; i<e->n; i+=2)
        if (attr[i]==attNames[a])
        	return attr[i+1];
    return NULL;
}

///////////////////////////////////////////////////////////////////////////////
/// Get real value.
///
///\param element element
///\param a Attribute
///\param vs Value status.
///\return Value of a if there is no error occurred. Otherwise, 0.
///////////////////////////////////////////////////////////////////////////////
double getDouble(void* element, Att a, ValueStatus* vs){
    double d = 0;
    const char* value = getString(element, a);
    if (!value) { *vs=valueMissing; return d; }
    *vs = (1==sscanf(value, "%lf", &d)) ? valueDefined : valueIllegal;
    return d;
}

///////////////////////////////////////////////////////////////////////////////
/// Retrieve Enumeration values from XML,
/// e.g. the start value for a variable of user-defined enumeration type.
///
///\param element element
///\param a Attribute
///\param vs Value status.
///\return Value of a if there is no error occurred. Otherwise, 0.
///////////////////////////////////////////////////////////////////////////////
int getInt(void* element, Att a, ValueStatus* vs){
    int n = 0;
    const char* value = getString(element, a);
    if (!value) { *vs=valueMissing; return n; }
    *vs = (1==sscanf(value, "%d", &n)) ? valueDefined : valueIllegal;
    return n;
}

///////////////////////////////////////////////////////////////////////////////
/// Get unsigned integer
///
///\param element element
///\param a Attribute
///\param vs Value status.
///\return Value of a if there is no error occurred. Otherwise, -1.
///////////////////////////////////////////////////////////////////////////////
unsigned int getUInt(void* element, Att a, ValueStatus* vs){
    unsigned int u = -1;
    const char* value = getString(element, a);
    if (!value) { *vs=valueMissing; return u; }
    *vs = (1==sscanf(value, "%u", &u)) ? valueDefined : valueIllegal;
    return u;
}

///////////////////////////////////////////////////////////////////////////////
/// Get the boolean value.
///
///\param element element.
///\param a Attribute.
///\param vs Value status.
///\return 1 for true and 0 for false.
///////////////////////////////////////////////////////////////////////////////
char getBoolean(void* element, Att a, ValueStatus* vs){
    const char* value = getString(element, a);
    if (!value) { *vs=valueMissing; return 0; };
    *vs = valueDefined;
    if (!strcmp(value, "true")) return 1;
    if (!strcmp(value, "false")) return 0;
    *vs = valueIllegal;
    return 0;
}

static int checkEnumValue(const char* enu);

///////////////////////////////////////////////////////////////////////////////
/// Retrieve the value of the given built-in enum attribute.
/// If the value is missing, this is marked in the ValueStatus
/// and the corresponding default is returned.
///
///\param element Element.
///\return -1 or a globally unique id for the value such that
///        enuNames[id] is the string representation of the enum value.
///////////////////////////////////////////////////////////////////////////////
Enu getEnumValue(void* element, Att a, ValueStatus* vs) {
    const char* value = getString(element, a);
    Enu id = valueDefined;
    if (!value) {
        *vs = valueMissing;
        switch (a) {
            case att_variableNamingConvention: return enu_flat;
            case att_variability: return enu_continuous;
            case att_causality: return enu_internal;
            case att_alias: return enu_noAlias;
            default: return -1;
        }
    }
    id = checkEnumValue(value);
    if (id==-1) *vs = valueIllegal;
    return id;
}

///////////////////////////////////////////////////////////////////////////////
/// Get model identifier.
/// Convenience methods for accessing the model description.
/// Use is only safe after the ast has been successfuly validated.
///
///\param md Model description.
///\return Model ID.
///////////////////////////////////////////////////////////////////////////////
const char* getModelIdentifier(ModelDescription* md) {
    //const char* modelId = getString(md, att_modelIdentifier);
    const char* modelId = getMyString(md, att_modelIdentifier);
    assert(modelId); // this is a required attribute
    return modelId;
}

///////////////////////////////////////////////////////////////////////////////
/// Get number of states.
///
///\param md Model description.
///\return Number of states.
///////////////////////////////////////////////////////////////////////////////
int getNumberOfStates(ModelDescription* md) {
    ValueStatus vs;
    int n = getUInt(md, att_numberOfContinuousStates, &vs);
    assert(vs==valueDefined); // this is a required attribute
    return n;
}

///////////////////////////////////////////////////////////////////////////////
/// Get number of event indicators.
///
///\param md Model description.
///\return Number of event indicators.
///////////////////////////////////////////////////////////////////////////////
int getNumberOfEventIndicators(ModelDescription* md) {
    ValueStatus vs;
    int n = getInt(md, att_numberOfEventIndicators, &vs);
    assert(vs==valueDefined); // this is a required attribute
    return n;
}

///////////////////////////////////////////////////////////////////////////////
/// Get name.
/// Name is a required attribute of ScalarVariable, Type, Item, Annotation, and Tool
///
///\param element Element.
///\return Name.
///////////////////////////////////////////////////////////////////////////////
const char* getName(void* element) {
    const char* name = getString(element, att_name);
    assert(name); // this is a required attribute
    return name;
}

///////////////////////////////////////////////////////////////////////////////
/// Get causality.
///
///\param scalarVariable Scalar variable.
///\return One of: input, output, internal, none.
///        If value is missing, the default internal is returned.
///////////////////////////////////////////////////////////////////////////////
Enu getCausality(void* scalarVariable) {
    ValueStatus vs;
    return getEnumValue(scalarVariable, att_causality, &vs);
}

///////////////////////////////////////////////////////////////////////////////
/// Get variablity.
///
///\param scalarVariable Scalar variable.
///\return One of constant, parameter, discrete, continuous.
///        If value is missing, the default continuous is returned.
///////////////////////////////////////////////////////////////////////////////
Enu getVariability(void* scalarVariable) {
    ValueStatus vs;
    return getEnumValue(scalarVariable, att_variability, &vs);
}

///////////////////////////////////////////////////////////////////////////////
/// Get alias.
///
///\param scalarVariable Scalar variable.
///\return Value of noAlias, alias, negatedAlias.
///        If value is missing, the default noAlias is returned.
///////////////////////////////////////////////////////////////////////////////
Enu getAlias(void* scalarVariable) {
    ValueStatus vs;
    return getEnumValue(scalarVariable, att_alias, &vs);
}

///////////////////////////////////////////////////////////////////////////////
/// Get value reference.
/// The vr is unique only for one of the 4 base data types r,i,b,s and
/// may also be fmiUndefinedValueReference = 4294967295 = 0xFFFFFFFF
/// here, i means integer or enumeration
///
///\param scalarVariable Scalar variable.
///\return FMI value reference.
///////////////////////////////////////////////////////////////////////////////
fmiValueReference getValueReference(void* scalarVariable) {
    ValueStatus vs;
    fmiValueReference vr = getUInt(scalarVariable, att_valueReference, &vs);
    assert(((Element*)scalarVariable)->type == elm_ScalarVariable);
    assert(vs==valueDefined); // this is a reqired attribute
    return vr;
}

///////////////////////////////////////////////////////////////////////////////
/// Get the varible by its unique name.
///
///\param md Model description.
///\param name Variable name.
///\return Point to scalar variable.
///////////////////////////////////////////////////////////////////////////////
ScalarVariable* getVariableByName(ModelDescription* md, const char* name) {
    int i;
    if (md->modelVariables)
    for (i=0; md->modelVariables[i]; i++){
        ScalarVariable* sv = (ScalarVariable*)md->modelVariables[i];
        if (!strcmp(getName(sv), name)) return sv;
    }
    return NULL;
}

///////////////////////////////////////////////////////////////////////////////
/// Check if elements \c t1 and \c t2 have the same base type.
/// \c Enumeration and \c Integer have the same base type while
/// \c Real, \c String, \c Boolean define own base types.
///
///\param t1 Element.
///\param t2 Element.
///\return 1 if they have the same type. Otherwise, 0.
///////////////////////////////////////////////////////////////////////////////
int sameBaseType(Elm t1, Elm t2){
    return t1==t2 ||
           t1==elm_Enumeration && t2==elm_Integer ||
           t2==elm_Enumeration && t1==elm_Integer;
}

///////////////////////////////////////////////////////////////////////////////
/// Get scalar variable.
///
///\param md Model description.
///\param vr FMI value reference.
///\param type Element.
///\return NULL if variable not found or vr==fmiUndefinedValueReference
///////////////////////////////////////////////////////////////////////////////
ScalarVariable* getVariable(ModelDescription* md, fmiValueReference vr, Elm type){
    int i;
    if (md->modelVariables && vr!=fmiUndefinedValueReference)
    for (i=0; md->modelVariables[i]; i++){
        ScalarVariable* sv = (ScalarVariable*)md->modelVariables[i];
        if (sameBaseType(type, sv->typeSpec->type) && getValueReference(sv) == vr)
            return sv;
    }
    return NULL;
}

///////////////////////////////////////////////////////////////////////////////
/// Get declared type of element.
///
///\param md Model description.
///\param declaredType Declared type.
///\return Point of type definition if there is no error occurred.
///        Otherwise, NULL.
///////////////////////////////////////////////////////////////////////////////
Type* getDeclaredType(ModelDescription* md, const char* declaredType){
    int i;
    if (declaredType && md->typeDefinitions)
    for (i=0; md->typeDefinitions[i]; i++){
        Type* tp = (Type*)md->typeDefinitions[i];
        if (!strcmp(declaredType, getName(tp))) return tp;
    }
    return NULL;
}

//Not used
/*const char* getString2(ModelDescription* md, void* tp, Att a) {
    Type* type;
    const char* value = getString(tp, a);
    if (value) return value; // found
    // search declared type, if any
    type = getDeclaredType(md, getString(tp, att_declaredType));
    return type ? getString(type->typeSpec, a) : NULL;
}*/

///////////////////////////////////////////////////////////////////////////////
/// Get description from variable or from declared type, or NULL.
///
///\param md Model description.
///\param md sv Scalar variable.
///\return Attribute description or type or NULL.
///////////////////////////////////////////////////////////////////////////////
const char * getDescription(ModelDescription* md, ScalarVariable* sv) {
    const char* value = getString(sv, att_description);
    Type* type;
    if (value) return value; // found
    // search declared type, if any
    type = getDeclaredType(md, getString(sv->typeSpec, att_declaredType));
    return type ? getString(type, att_description) : NULL;
}

///////////////////////////////////////////////////////////////////////////////
/// Check the attribute defined by \c vr and \c type.
///
///\param md Model description.
///\param vr FMI value reference.
///\param type Element type.
///\param a Attribute.
///\return String of attribute's value if there is no error occurred.
///        Otherwise, return NULL.
///////////////////////////////////////////////////////////////////////////////
const char * getVariableAttributeString(ModelDescription* md,
        fmiValueReference vr, Elm type, Att a){
    const char* value;
    const char* declaredType;
    Type* tp;
    ScalarVariable* sv = getVariable(md, vr, type);
    if (!sv) return NULL;
    value = getString(sv->typeSpec, a);
    if (value) return value; // found
    // search declared type, if any
    tp = getDeclaredType(md, getString(sv->typeSpec, att_declaredType));
    return tp ? getString(tp->typeSpec, a) : NULL;
}

///////////////////////////////////////////////////////////////////////////////
/// Get attribute value from scalar variable given by \c vr and \c type,
/// incude default value provided by declared type, if any.
///
///\param md Model description.
///\param vr FMI value reference.
///\param type Element type.
///\param a Attribute.
///\param vs Value status.
///\return Attribute value if there is no error occurred.
///        Otherwise, return 0.0.
///////////////////////////////////////////////////////////////////////////////
double getVariableAttributeDouble(ModelDescription* md,
        fmiValueReference vr, Elm type, Att a, ValueStatus* vs){
    double d = 0;
    const char* value = getVariableAttributeString(md, vr, type, a);
    if (!value) { *vs = valueMissing; return d; }
    *vs = (1==sscanf(value, "%lf", &d)) ? valueDefined : valueIllegal;
    return d;
}

///////////////////////////////////////////////////////////////////////////////
/// Get nominal value from real variable or its declared type.
///
///\param md Model description.
///\param vr FMI value reference.
///\return Defined nonimal value. Otherwise, return 1.0 if no nominal value is defined.
///////////////////////////////////////////////////////////////////////////////
double getNominal(ModelDescription* md, fmiValueReference vr){
    ValueStatus vs;
    double nominal = getVariableAttributeDouble(md, vr, elm_Real, att_nominal, &vs);
    return vs==valueDefined ? nominal : 1.0;
}

static void printList(int indent, void** list);

///////////////////////////////////////////////////////////////////////////////
/// Various checks that log an error and stop the parser
///
///\param ptr point of data.
///\return 0 if no error occurred.
///         Otherwise, return 1 to indicate error.
///////////////////////////////////////////////////////////////////////////////
static int checkPointer(const void* ptr){
    if (! ptr) {
        printf("Out of memory\n");
        if (parser) XML_StopParser(parser, XML_FALSE);
        return 1; // error
    }
    return 0; // success
}

///////////////////////////////////////////////////////////////////////////////
/// Check if string \c name is in the string array \c array[].
///
///\param name The string to be checked.
///\param kind The type of string to be checked.
///\param array The list of strings for comparision.
///\param n The length of array.
///\return The index of matched string in the \c array[].
///        Otherwise, return -1 to indicate error.
///////////////////////////////////////////////////////////////////////////////
static int checkName(const char* name, const char* kind, const char* array[], int n){
    int i;
    for (i=0; i<n; i++) {
        if (!strcmp(name, array[i])) return i;
    }
    printf("Illegal %s %s\n", kind, name);
    XML_StopParser(parser, XML_FALSE);
    return -1;
}

///////////////////////////////////////////////////////////////////////////////
/// Check the enum value of \c elm.
///
///\param elm The name of element to be checked.
///\return The enum value of the element elm if it is found.
///        Otherwise, return -1 to indicate error.
///////////////////////////////////////////////////////////////////////////////
static int checkElement(const char* elm){
    return checkName(elm, "element", elmNames, SIZEOF_ELM);
}

///////////////////////////////////////////////////////////////////////////////
/// Check the enum value of an attribute.
///
///\param att The name of attribute to be checked.
///\return The enum value of the attribute if it found.
///        Otherwise, return -1 to indicate error.
///////////////////////////////////////////////////////////////////////////////
static int checkAttribute(const char* att){
    return checkName(att, "attribute", attNames, SIZEOF_ATT);
}

///////////////////////////////////////////////////////////////////////////////
/// Check enum value of input string \c enu.
///
///\param enu String to be checked.
///\return The enum value of string if it is found in the enum.
///        Otherwise, return -1 to indicate an error.
///////////////////////////////////////////////////////////////////////////////
static int checkEnumValue(const char* enu){
    return checkName(enu, "enum value", enuNames, SIZEOF_ENU);
}

///////////////////////////////////////////////////////////////////////////////
/// Print the error information for wrong type.
///
///\param expected The name of expected type.
///\param found The name of found type.
///////////////////////////////////////////////////////////////////////////////
static void logFatalTypeError(const char* expected, Elm found) {
    printf("Wrong element type, expected %s, found %s\n",
            expected, elmNames[found]);
    XML_StopParser(parser, XML_FALSE);
}

///////////////////////////////////////////////////////////////////////////////
/// Verify that \c element is of the given type.
///
///\param element The element to be checked.
///\param e The expected element.
///\return 0 if there is no error occurred. Otherwise, return 1 to indicate an error.
///////////////////////////////////////////////////////////////////////////////
static int checkElementType(void* element, Elm e) {
    Element* elm = (Element* )element;
    if (elm->type == e) return 0; // success
    logFatalTypeError(elmNames[e], elm->type);
    return 1; // error
}

///////////////////////////////////////////////////////////////////////////////
/// Verify that the next stack element exists and is of the given type.
/// If e==ANY_TYPE, the type check is ommited.
///
///\param e The element.
///\return 0 if there is no error occurred. Otherwise, return 1 to indicate an error.
///////////////////////////////////////////////////////////////////////////////
static int checkPeek(Elm e) {
    if (stackIsEmpty(stack)){
        printf("Illegal document structure, expected %s\n", elmNames[e]);
        XML_StopParser(parser, XML_FALSE);
        return 1; // error
    }
    return e==ANY_TYPE ? 0 : checkElementType(stackPeek(stack), e); //stackPeek() retrieves item at top of stack
}

///////////////////////////////////////////////////////////////////////////////
/// Get the next stack element of the given type.
/// If e==ANY_TYPE, the type check is ommited.
///
///\param e Element.
///\return The point to the element if there is no error. Otherwise, return NULL to indicate error.
///////////////////////////////////////////////////////////////////////////////
static void* checkPop(Elm e){
    return checkPeek(e) ?  NULL : stackPop(stack);     // stackPop() removes most recently added item
}

///////////////////////////////////////////////////////////////////////////////
/// Get AST node type.
///
///\param e The AST node
///\return Type of AST node.
///////////////////////////////////////////////////////////////////////////////
AstNodeType getAstNodeType(Elm e){
  //printf("  -> getting ASTNode Type for %s\n", elmNames[e]);
    switch (e) {
    case elm_fmiModelDescription:
        return astModelDescription;
    case elm_Type:
        return astType;
    case elm_ScalarVariable:
        return astScalarVariable;
    case elm_Implementation:  			// Add Implementation
    case elm_BaseUnit:
    case elm_EnumerationType:
    case elm_Tool:
    case elm_UnitDefinitions:
    case elm_TypeDefinitions:
    case elm_VendorAnnotations:
    case elm_ModelVariables:
    case elm_DirectDependency:
    case elm_CoSimulation_StandAlone:	// Add CoSimulation_StandAlone
    case elm_CoSimulation_Tool:			// Add CoSimulaiton_Tool
        return astListElement;
    default:
        return astElement;
    }
}

///////////////////////////////////////////////////////////////////////////////
/// Add Attributes to an given element.
/// It copies the \c attr array and all values,
/// replaces all attribute names by constant literal strings,
/// converts the null-terminated array into an array of known size n.
///
///\param el The element.
///\param attr Attributes.
///\returns 0 if there is no error occurred. Otherwise, return 1 to indicate an error.
///////////////////////////////////////////////////////////////////////////////
int addAttributes(Element* el, const char** attr) {
    int n, a;
    const char** att = NULL;
    for (n=0; attr[n]; n+=2);
    printDebug("\n");
    printfIntDebug("addAttributes(): n = %d\n", n);
    if (n>0) {
          att = calloc(n, sizeof(char*));
          if (checkPointer(att)) return 1;
      }
    printDebug("addAttributes(): allocated att");

    for (n=0; attr[n]; n+=2) {
        char* value = strdup(attr[n+1]); //duplicate string attr[n+1]
         printfDebug("addAttributes(): value = %s\n", value);

        if (checkPointer(value)) return 1;
        a = checkAttribute(attr[n]);
        printfIntDebug("addAttributes(): index a = %d\n", a);

        if (a == -1) {
          printf("Illegal attribute in");
          return 1;  // illegal attribute error
        }
        att[n  ] = attNames[a]; // no heap memory
        printfDebug("addAttributes(): attNames = %s\n", attNames[a]);

        att[n+1] = value;       // heap memory
        printfDebug("addAttributes(): att[n+1] = %s\n", att[n+1]	);

    }
    el->attributes = att; // NULL if n=0
    el->n = n;
    return 0; // success
}

///////////////////////////////////////////////////////////////////////////////
/// Add an new element.
///
///\param type Type of the element.
///\param size Size of the element.
///\param attr Attributes of the element.
///\return The point of the element if ther is no error occurred.
///        Otherwise, return NULL to indicate an error.
///////////////////////////////////////////////////////////////////////////////
Element* newElement(Elm type, int size, const char** attr) {
    Element* e = (Element*)calloc(1, size);
    printfIntDebug("Allocated 1 element with %d bytes of memory in newElement()\n", size);
    if (checkPointer(e)) return NULL;
    e->type = type;
    e->attributes = NULL;
    e->n=0;
    printDebug("Start to add new elmement to attr");
    if (addAttributes(e, attr)) return NULL;
    printDebug("Added new elmement to attr");
    return e;
}

///////////////////////////////////////////////////////////////////////////////////////
/// Callback functions called by the XML parser. It creates and push a new element node.
/// This is the start handler of Parser. XML_Parse() starts from here.
///
///\param context
///\param elm The element
///\param attr The attributes
///////////////////////////////////////////////////////////////////////////////////////
static void XMLCALL startElement(void *context, const char *elm, const char **attr) {
    Elm el;
    void* e;
    int size;
    el = checkElement(elm);  //Get the index of element
    printfDebug("startElement():%s\n", elm);
    if (el==-1) {
      printDebug("Error!");
      return; // error
    }
    printfDebug("*** starting element %s\n", elmNames[el]);
    skipData = (el != elm_Name); // skip element content for all elements but Name

    switch(getAstNodeType(el)){
        case astElement:          size = sizeof(Element); break;
        case astListElement:      size = sizeof(ListElement); break;
        case astType:             size = sizeof(Type); break;
        case astScalarVariable:   size = sizeof(ScalarVariable); break;
        case astModelDescription: size = sizeof(ModelDescription); break;
		default: assert(0);  // Error message if there is no matching element,
    }

    printfIntDebug("Start to created a new element with size: %d\n", size);
    e = newElement(el, size, attr);  // Create a new element
    printDebug("Created a new element");

    if(checkPointer(e)) assert(0); // Check the validity of the new element
    printDebug("startElement(): Start to stack push.");
    stackPush(stack, e); // Add the element to stack and grow the stack if necessary
}

///////////////////////////////////////////////////////////////////////////////
/// Pop all elements of the given type from stack and add it to the ListElement that follows.
/// The ListElement remains on the stack.
///
///\param e The elements to be popped out.
///////////////////////////////////////////////////////////////////////////////
static void popList(Elm e) {
    int n = 0;
    Element** array;
    Element* elm = stackPop(stack);
    while (elm->type == e) {
        elm = stackPop(stack);
        n++;
    }
    stackPush(stack, elm); // push ListElement into stack
    array = (Element**)stackLastPopedAsArray0(stack, n); // NULL terminated list
    if (getAstNodeType(elm->type)!=astListElement) return; // failure
    ((ListElement*)elm)->list = array;
    return; // success only if list!=NULL
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Pop the children from the stack and check for correct type and sequence of children
/// This is the end handler of parser. The parser ends here.
///
///\param context
///\param elm
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
static void XMLCALL endElement(void *context, const char *elm) {
    Elm el;
    el = checkElement(elm);
    printfDebug(" **** ending element %s.\n", elmNames[el]);
    switch(el) {
        case elm_fmiModelDescription:
            {
                 ModelDescription* md;
                 ListElement** im = NULL;  		//  NULL or list of CoSimulation_StandAlone under Implementation
                 ListElement** ud = NULL;     	//  NULL or list of BaseUnits under UnitDefinitions
                 Type**        td = NULL;     	//  NULL or list of Types under TypeDefinitions
                 Element*      de = NULL;     	// NULL or DefaultExperiment
                 ListElement** va = NULL;     	// NULL or list of Tools
                 ScalarVariable** mv = NULL;  	// NULL or list of ScalarVariables under ModelVariables
                 ListElement* child;
                 printDebug("$$$$$ entered endElement 'elm_fmiModelDescription'\n");
                 child = checkPop(ANY_TYPE); // get a child from stack

                 while (child && child->type != elm_fmiModelDescription)  // add while-loop in case the elements in the random order
                 {
					 if (child->type == elm_VendorAnnotations){
						 va = (ListElement**)child->list;    // VendorAnnotations (ListElement) contains Tool (ListElement))
						 free(child);
						 child = checkPop(ANY_TYPE);
						 if (!child) return;
					 }
					 if (child->type == elm_DefaultExperiment){
						 de = (Element*)child;				// DefaultExperiment (Element) with only attributes
						 child = checkPop(ANY_TYPE);
						 if (!child) return;
					 }
					 if (child->type == elm_TypeDefinitions){
						 td = (Type**)child->list;     		// TypeDefinitions (ListElement) contains Type (Type)
						 free(child);
						 child = checkPop(ANY_TYPE);
						 if (!child) return;
					 }
					 if (child->type == elm_UnitDefinitions){
						 ud = (ListElement**)child->list;   // UnitDefinitions (ListElement) contains BaseUnit (ListElement)
						 free(child);
						 child = checkPop(ANY_TYPE);
						 if (!child) return;
					 }
					 if (child->type == elm_Implementation){ 	// Implementation is listElement, it can be treated the same as UnitDefinitions
						 im = (ListElement**)child->list; 		// Implementation (Implementation) contains CoSimulation_StandAlone (ListElement)
						 free(child);							// Zuo:
						 child = checkPop(ANY_TYPE);			// Zuo:
						 if (!child) return;					// Zuo:
					 }											// Zuo:
					 if (child->type == elm_ModelVariables){
						  mv = (ScalarVariable**)child->list;  // ModelVariables (ListElement) contains ScalarVariable (ScalarVariable)
						  free(child);
						  child = checkPop(ANY_TYPE);
						  if (!child) return;
					 }
                 }
		         printDebug("$$$$$ checking element type \n");
                 if (checkElementType(child, elm_fmiModelDescription)) return;
                 printDebug("$$$$$ checked  element type \n");
                 md = (ModelDescription*)child;

                 // the following build the link of elements for ModelDescriptions
                 md->modelVariables = mv;
                 md->implementation = im; // added M. Wetter
                 md->vendorAnnotations = va;
                 md->defaultExperiment = de;
                 md->typeDefinitions = td;
                 md->unitDefinitions = ud;
                 printDebug("$$$$$ calling stackPush \n");
                 stackPush(stack, md);
                 printDebug("$$$$$ called stackPush \n");
                 break;
            }
        case elm_Type:
            {
                Type* tp;
                Element* ts = checkPop(ANY_TYPE);
                if (!ts) return;  // If there is no sub-element, return
                if (checkPeek(elm_Type)) return; // If the sub-element is not Type, return
                tp = (Type*)stackPeek(stack);  // Get the element
                switch (ts->type) {
                    case elm_RealType:
                    case elm_IntegerType:
                    case elm_BooleanType:
                    case elm_EnumerationType:
                    case elm_StringType:
                        break;
                    default: 		// Element type does not match
                         logFatalTypeError("RealType or similar", ts->type);
                         return;
                }
                tp->typeSpec = ts;  	// parent (tp) links to sub-element (ts)
                break;
            }
        case elm_ScalarVariable:
            {
                ScalarVariable* sv;
                Element** list = NULL;
                Element* child = checkPop(ANY_TYPE);
                if (!child) return;
                if (child->type==elm_DirectDependency){
                    list = ((ListElement*)child)->list; // DirectDependency is ListElement
                    free(child);
                    child = checkPop(ANY_TYPE);
                    if (!child) return;
                }
                if (checkPeek(elm_ScalarVariable)) return;  // If next element is not ScalarVariable, return
                sv = (ScalarVariable*)stackPeek(stack);
                switch (child->type) {
                    case elm_Real:
                    case elm_Integer:
                    case elm_Boolean:
                    case elm_Enumeration:
                    case elm_String:
                        break;
                    default:
                         logFatalTypeError("Real or similar", child->type);
                         return;
                }
                sv->directDependencies = list;
                sv->typeSpec = child;
                break;
            }
        case elm_Implementation:  	popList(elm_CoSimulation_StandAlone); break; // Needs to be modified if CoSimulation_Tool is added
	    case elm_CoSimulation_StandAlone:  popList(elm_Capabilities); break; // CoSimulation_StandAlone only has Capabilities
        case elm_ModelVariables:    popList(elm_ScalarVariable); break;
        case elm_VendorAnnotations: popList(elm_Tool);break;
        case elm_Tool:              popList(elm_Annotation); break;
        case elm_TypeDefinitions:   popList(elm_Type); break;
        case elm_EnumerationType:   popList(elm_Item); break;
        case elm_UnitDefinitions:   popList(elm_BaseUnit); break;
        case elm_BaseUnit:          popList(elm_DisplayUnitDefinition); break;
        case elm_DirectDependency:  popList(elm_Name); break;
        case elm_Name:
            {
                 // Exception: the name value is represented as element content.
                 // All other values of the XML file are represented using attributes.
                 Element* name = checkPop(elm_Name);
                 if (!name) return;
                 name->n = 2;
                 name->attributes = malloc(2*sizeof(char*));
                 name->attributes[0] = attNames[att_input];
                 name->attributes[1] = data;
                 data = NULL;
                 skipData = 1; // stop recording element content
                 stackPush(stack, name);
                 break;
            }
        case -1: return; // illegal element error
        default: // must be a leaf Element
				printDebug("+++++++ got a leaf element\n");
                assert(getAstNodeType(el)==astElement);
                break;
    }
    // All children of el removed from the stack.
    // The top element must be of type el now.
    checkPeek(el);
    printDebug("Checked Peek");
}

///////////////////////////////////////////////////////////////////////////////
/// Handle element data.
/// Feature in expat:
/// For some reason, if the element data is the empty string (Eg. <a></a>)
/// instead of an empty string with len == 0 we get "\n". The workaround is
/// to replace this with the empty string whenever we encounter "\n".
///
///\param context Not used in this function.
///\param XML_Char The XML characters.
///\param len The length of string to be copied.
///////////////////////////////////////////////////////////////////////////////
void XMLCALL handleData(void *context, const XML_Char *s, int len) {
    int n;
    if (skipData) return;
    if (!data) {
        // start a new data string
        if (len == 1 && s[0] == '\n') {
            data = strdup("");
        } else {
            data = malloc(len + 1);
            strncpy(data, s, len);
            data[len] = '\0';
        }
    }
    else {
        // continue existing string
        n = strlen(data) + len;
        char* tmpData;
        tmpData = realloc(data, n+1);
        if (!tmpData) {
			printf("Couldnt allocate memory in xml_parser_cosim::handleData\n");
			return;
		}
		data = tmpData;
        strncat(data, s, len);
        data[n] = '\0';
    }
    return;
}

///////////////////////////////////////////////////////////////////////////////
/// Print element information.
///
///\param indent Number of space for indent.
///\param element Element to be printed.
///////////////////////////////////////////////////////////////////////////////

void printElement(int indent, void* element){
    int i;
    Element* e = (Element*)element;
    ModelDescription* md;
    if (!e) return;
    // print attributes
    for (i=0; i<indent; i++) printf(" ");
    printf("%s", elmNames[e->type]);
    for (i=0; i<e->n; i+=2)
        printf(" %s=%s", e->attributes[i], e->attributes[i+1]);
    printf("\n");
    // print child nodes
    indent += 2;
    switch (getAstNodeType(e->type)) {
        case astListElement:
            printList(indent, (void **)((ListElement*)e)->list);
            break;
        case astScalarVariable:
            printElement(indent, ((Type*)e)->typeSpec);
            printList(indent, (void **)((ScalarVariable*)e)->directDependencies);
            break;
        case astType:
            printElement(indent, ((Type*)e)->typeSpec);
            break;
        case astModelDescription:
            md = (ModelDescription*)e;
            printList(indent, (void **)md->unitDefinitions);
            printList(indent, (void **)md->typeDefinitions);
            printElement(indent, md->defaultExperiment);
            printList(indent, (void **)md->vendorAnnotations);
            printList(indent, (void **)md->modelVariables);
            printList(indent, (void **)md->implementation); // added W. Zuo
            break;
    }
}

///////////////////////////////////////////////////////////////////////////////
/// Print information for list of elements.
///
///\param indent Number of space for indent.
///\param list List of elements to be printed.
///////////////////////////////////////////////////////////////////////////////
static void printList(int indent, void** list){
    int i;
    if (list) for (i=0; list[i]; i++)
       printElement(indent, list[i]);
}

////////////////////////////////////////////////////////////////////////////////////
/// Generate idf file for EnergyPlus
///
///\param md The model description
////////////////////////////////////////////////////////////////////////////////////
void printidf(const char* fmuFilNam, ModelDescription* md)
{
	FILE *fp;

  char type[12];
	int i, j, varname, vardes=-1;
	void **list;
	ScalarVariable* se;
	Element* ee;

	fp = fopen("tmp.idf", "w");

	if (fp == NULL) {
	  printf("Can't create temporary idf file!\n");
	  exit(42);  // STL error code: File not open.
	}

	/////////////////////////////////////////////////////////////////////////////
	// Define ExternalInterface
	fprintf(fp, "ExternalInterface,\n");
	fprintf(fp, "  FunctionalMockupUnitImport;\t\t!- Name of External Interface\n");

	////////////////////////////////////////////////////////////////////////////
  // Define ExternalInterface:FunctionalMockupUnitImport
	fprintf(fp, "\nExternalInterface:FunctionalMockupUnitImport,\n");

	fprintf(fp, "  %s,\t\t!- FMU Filename\n", fmuFilNam);
  //fprintf(fp, "   ,\t\t!- FMU Model Name\n");
  fprintf(fp, "   ,\t\t!- FMU Timeout in milli-seconds\n");
  //fprintf(fp, "   ,\t\t!- FMU Visible Value\n");
  //fprintf(fp, "   ,\t\t!- FMU Interactive Value\n");
  fprintf(fp, "   ;\t\t!- FMU LoggingOn Value\n");

	list = (void **)md->modelVariables;
	if (list)
		for(j=0; list[j]; j++)
		{
			Element* e = (Element*)list[j];
			Enu val = enu_none;

			for(i=0; i<e->n; i+=2)
			{
				if(!strcmp(e->attributes[i], "name"))
					varname = i+1;
				else if(!strcmp(e->attributes[i], "causality"))
					val = checkEnumValue(e->attributes[i+1]);
				else if(!strcmp(e->attributes[i], "description"))
					vardes = i+1;
			}

      /////////////////////////////////////////////////////////////////////////////////////
      // Define ExternalInterface:FunctionalMockupUnitImport:From:Variable
      // Define part of ExternalInterface:FunctionalMockupUnitImport:To
			if(val == enu_input || val == enu_output)
			{
				switch (val)
				{
					case enu_input:
						fprintf(fp, "\nExternalInterface:FunctionalMockupUnitImport:From:Variable,\n");
            fprintf(fp, "   ,\t\t!- EnergyPlus Key Value\n");
						break;
					case enu_output:
            // User should manually define the To type: Schedule, Actuator, Variable
						fprintf(fp, "\nExternalInterface:FunctionalMockupUnitImport:To:,\n");
						break;
					default:
						break;
				}


        fprintf(fp, "   ,\t\t!- EnergyPlus Variable Name\n");
	      fprintf(fp, "   %s,\t\t!- FMU Filename\n", fmuFilNam);
				fprintf(fp, "   ,\t\t!- FMU Instance Name\n");
				switch (val)
				{
          case enu_input:
            fprintf(fp, "   %s;\t\t!- FMU Variable Name\n", e->attributes[varname]);
            break;
					case enu_output:
            fprintf(fp, "   %s,\t\t!- FMU Variable Name\n", e->attributes[varname]);
            break;
        }

        // Get the type of FMU variable
				/*se = (ScalarVariable*)list[j];
				ee = se->typeSpec;
				switch(ee->type)
				{
					case elm_Integer:
						strcpy(type, "Integer");	break;
					case elm_Real:
						strcpy(type, "Real"); 		break;
					case elm_Boolean:
						strcpy(type, "Boolean"); 	break;
					case elm_String:
						strcpy(type, "String"); 	break;
					case elm_Enumeration:
						strcpy(type, "Enumeration"); break;
					default:
						break;
				}
        if(vardes != -1) fprintf(fp, ", %s", e->attributes[vardes]);
				fprintf(fp, ", %s\n", type); //Type of variable*/
			}

      if(val==enu_output) fprintf(fp, "   ;\t\t!- Initial Value\n");


    }
	fclose(fp);
}

static void freeList(void** list);

///////////////////////////////////////////////////////////////////////////////
/// Free element which may contain list of elements.
///
///\param element point of element.
//////////////////////////////////////////////////////////////////////////////
void freeElement(void* element){
    int i;
    Element* e = (Element*)element;
    ModelDescription* md;
    if (!e) return;
    // free attributes
    for (i=0; i<e->n; i+=2)
        free((void *)e->attributes[i+1]);
    free(e->attributes);
    // free child nodes
    switch (getAstNodeType(e->type)) {
        case astListElement:
            freeList((void **)((ListElement*)e)->list);
            break;
        case astScalarVariable:
            freeList((void **)((ScalarVariable*)e)->directDependencies);
        case astType:
            freeElement(((Type*)e)->typeSpec);
            break;
        case astModelDescription:
            md = (ModelDescription*)e;
            freeList((void **)md->unitDefinitions);
            freeList((void **)md->typeDefinitions);
            freeElement(md->defaultExperiment);
            freeList((void **)md->vendorAnnotations);
            freeList((void **)md->modelVariables);
            freeList((void **)md->implementation); // added M. Wetter
            break;
    }
    // free the struct
    free(e);
}

//////////////////////////////////////////////////////////////////////////////////
/// Free list
///
///\param list Point to lists
//////////////////////////////////////////////////////////////////////////////////
static void freeList(void** list){
    int i;
    if (!list) return;
    for (i=0; list[i]; i++)
        freeElement(list[i]);
    free(list);
}

//////////////////////////////////////////////////////////////////////////////////
/// Clean file and memory
///
///\param file The point of file
//////////////////////////////////////////////////////////////////////////////////
static void cleanup(FILE *file) {
    stackFree(stack);
    stack = NULL;
    XML_ParserFree(parser);
    parser = NULL;
    fclose(file);
}

//////////////////////////////////////////////////////////////////////////////////
/// Parse the fmu. The receiver must call freeElement(md) to release AST memory.
///
///\param xmlPath The xml file to be parsed
///\return the root node md of the AST if no error occurred. NULL to indicate failure
/////////////////////////////////////////////////////////////////////////////////
ModelDescription* parse(const char* xmlPath) {
  ModelDescription* md = NULL;
  FILE *file;
  int done = 0;

  stack = stackNew(100, 10); // Allocate stack memory
  if (checkPointer(stack)) return NULL;  // Check if the stack is creatted

  parser = XML_ParserCreate(NULL); // Create an parser
  if (checkPointer(parser)) return NULL;  // Check if the parser is created

  XML_SetElementHandler(parser, startElement, endElement); // Set handler for start and end tags
  XML_SetCharacterDataHandler(parser, handleData); // Set handler for text

	file = fopen(xmlPath, "rb");

	if (file == NULL) {
    printfError("Cannot open file '%s'\n", xmlPath);
    XML_ParserFree(parser); // Free the memory for parser
    return NULL; // Failure
  }

  while (!done) {
    int n = fread(text, sizeof(char), XMLBUFSIZE, file); // Read XMLBUFSIZE characters from file
    if (n != XMLBUFSIZE) done = 1; // Reach the end of file
    if (!XML_Parse(parser, text, n, done)){
      printf("Parse error in file %s at line %d:\n%s\n",
             xmlPath,
             (int)XML_GetCurrentLineNumber(parser),
              XML_ErrorString(XML_GetErrorCode(parser)));
      while (!stackIsEmpty(stack)) md = stackPop(stack);
      if (md) freeElement(md);
      cleanup(file);
      return NULL; // failure
    }
  }

  printDebug("******* Start to pop stack ");
  md = stackPop(stack);

  printDebug("******* Check if stack is empty ");
  assert(stackIsEmpty(stack));

  printDebug("******* Clean up file");
  cleanup(file);
  //printElement(1, md); // Print the element for debugging
  return md; // Success if all refs are valid
}


