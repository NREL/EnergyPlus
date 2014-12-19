// copyright 2014, Shannon Mackey <mackey@BUILDlab.net>
#include "JSONDataInterface.h"

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <random>


using namespace std;
using namespace idfx;

/////////////////////////////// begin  random and uuid gen
//these random functions from here:
// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3551.pdf
std::default_random_engine & global_urng( )
{
    static std::default_random_engine u {};
    return u;
}
void randomize( )
{
    static std::random_device rd {};
    global_urng().seed( rd() );
}
int randomInRange( int from, int thru )
{
    static std::uniform_int_distribution<> d {};
    using parm_t = decltype(d)::param_type;
    return d( global_urng(), parm_t {from, thru} );
}
double randomInRange( double from, double upto )
{
    static std::uniform_real_distribution<> d {};
    using parm_t = decltype(d)::param_type;
    return d( global_urng(), parm_t {from, upto} );
}

//return a version 4 (random) uuid
string getUuid()
{
    stringstream uuid_str;
    int four_low = 4096;
    int four_high = 65535;
    int three_low = 256;
    int three_high = 4095;
    uuid_str << std::hex << randomInRange(four_low,four_high);
    uuid_str << std::hex << randomInRange(four_low,four_high);
    uuid_str << "-" << std::hex << randomInRange(four_low,four_high);
    uuid_str << "-" << std::hex << randomInRange(four_low,four_high);
    uuid_str << "-4" << std::hex << randomInRange(three_low,three_high);
    uuid_str << "-8" << std::hex << randomInRange(three_low,three_high);
    uuid_str << std::hex << randomInRange(four_low,four_high);
    uuid_str << std::hex << randomInRange(four_low,four_high);
    return uuid_str.str();
}
/////////////////////// end random and uuid gen


JSONDataInterface::JSONDataInterface(const string &json_schema)
{
    randomize(); //init for uuidv4 generator
    schema_j = cJSON_Parse(json_schema.c_str());
    if (!schema_j)
        cout << "ERROR: schema load failure - "<< endl << cJSON_GetErrorPtr();
    model_j = cJSON_CreateObject();
    if (!model_j)
        cout << "ERROR: model create failure - "<< endl << cJSON_GetErrorPtr();
    //TBD: throw on ERROR
}

JSONDataInterface::~JSONDataInterface()
{
    cJSON_Delete(schema_j);
    cJSON_Delete(model_j);
}

cJSON *JSONDataInterface::getSchemaObject(string object_type)
{
    return cJSON_GetObjectItem(schema_j, object_type.c_str());
}

cJSON *JSONDataInterface::getModelRootObject()
{
    return model_j;
}

bool JSONDataInterface::importModel(string json_content)
{
    model_j = cJSON_Parse(json_content.c_str());
    if (!model_j->child) {
        cout << "ERROR: failure reading input file: " << endl << cJSON_GetErrorPtr();
        return false;
    }
    return integrateModel();
}

bool JSONDataInterface::integrateModel()
{
    //add uuid to all objects
    insertUUIDs();
    return validateModel();
    //TBD: remap references from Label to uuid
}

void JSONDataInterface::writeJSONdata(string filename)
{
    ofstream idfj(filename.c_str());
    if (idfj) {
        idfj << cJSON_Print(model_j);
    }
}

void JSONDataInterface::insertUUIDs()
{
    cJSON *child = model_j->child;
    if (child) {
        do {
            if (!cJSON_GetObjectItem(child, "uuid")) {
                cJSON_AddStringToObject(child, "uuid", getUuid().c_str());
            }
            child = child->next;
        } while (child);
    } else {
        cout << "ERROR: model is empty" << endl;
    }
}


void JSONDataInterface::checkRange(cJSON *attribute, string property_name, string child_name, bool &valid, double property_value)
{
    if (attribute) {
        string attribute_name = attribute->string;
        if (attribute_name.find("imum")  != std::string::npos) { //only if range checkable fields
            double attribute_value =  (attribute->valuestring) ? stod(attribute->valuestring) : attribute->valueint;
            if (attribute_name == "exclude_minimum") {
                if (!(property_value > attribute_value)) {
                    valid = false;
                    cout << "INVALID: " << child_name << " : " << property_name << " : " << property_value <<  " must be > " << attribute_value << endl;
                }
            } else if (attribute_name == "exclude_maximum") {
                if (!(property_value < attribute_value)) {
                    valid = false;
                    cout << "INVALID: " << child_name << " : " << property_name << " : " << property_value <<  " must be < " << attribute_value << endl;
                }
            } else if (attribute_name == "minimum") {
                if (!(property_value >= attribute_value)) {
                    valid = false;
                    cout << "INVALID: " << child_name << " : " << property_name << " : " << property_value <<  " must be >= " << attribute_value << endl;;
                }
            } else if (attribute_name == "maximum") {
                if (!(property_value <= attribute_value)) {
                    valid = false;
                    cout << "INVALID: " << child_name << " : " << property_name << " : " << property_value <<  " must be <= " << attribute_value << endl;
                }
            }
        }
    }
}

void JSONDataInterface::checkNumeric(double property_value, string property_name, cJSON *schema_object, bool &valid, string child_name)
{
    if (schema_object) {                               //get matching schema property
        cJSON *schema_property = cJSON_GetObjectItem(schema_object, property_name.c_str());
        if (schema_property) {
            for (int a = 0; a < cJSON_GetArraySize(schema_property); a++) {
                cJSON *attribute = cJSON_GetArrayItem(schema_property, a);
                checkRange(attribute, property_name, child_name, valid, property_value);
            }
        }
    }
}

bool JSONDataInterface::validateModel()
{
    bool valid = true;
    cJSON *child = model_j->child;
    if (child) {
        do {
            string child_name = child->string;
            for (int i = 0; i < cJSON_GetArraySize(child); i++) {
                cJSON *property = cJSON_GetArrayItem(child,i);
                if (property) {
                    string property_name = property->string;
                    switch (property->type) {
                    case cJSON_Number: { //check numeric types
                        double property_value = (property->valuedouble) ? property->valuedouble : property->valueint;
                        cJSON *schema_object = getSchemaObject(child_name);
                        checkNumeric(property_value, property_name, schema_object, valid, child_name);
                        break; }
                    case cJSON_String: { //check alpha types
                        //TBD - check choice and/or reference cases?
                        break; }
                    default: {
                        // nothing to do
                        break; }
                    }
                }
            }
            child = child->next;
        } while (child);
    } else {
        cout << "ERROR: model is empty" << endl;
    }

    return valid;
}
