// copyright 2014, Shannon Mackey <mackey@BUILDlab.net>
#include "JSONDataInterface.h"

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>

#include "idd-minimal-ordered.h"

//forward declarations
void randomize( );

using namespace std;
using namespace idfx;


JSONDataObject::JSONDataObject(const string &json_content) :
    data_j(cJSON_Parse(json_content.c_str()))
{

}

JSONDataObject::~JSONDataObject()
{
    cJSON_Delete(data_j);
}

std::map<std::string, std::string> JSONDataObject::getProperties()
{
    map<string, string> property_map;

    return property_map;
}

////

JSONDataInterface::JSONDataInterface(const string &json_schema)
{
    randomize( ); //prepare uuid generation function
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

std::list<JSONDataObject> JSONDataInterface::getModelObjects(string object_type_regex)
{

}

cJSON *JSONDataInterface::getSchemaObject(const string &object_type)
{
    return cJSON_GetObjectItem(schema_j, object_type.c_str());
}

cJSON *JSONDataInterface::getModelRootObject()
{
    return model_j;
}

bool JSONDataInterface::importIDFxModel(const string &json_content)
{
    model_j = cJSON_Parse(json_content.c_str());
    if (!model_j->child) {
        cout << "ERROR: failure reading input file: " << endl << cJSON_GetErrorPtr();
        return false;
    }
    return validateModel();
}

void JSONDataInterface::writeJSONdata(const string &filename)
{
    ofstream idfj(filename.c_str());
    if (idfj) {
        idfj << cJSON_Print(model_j);
    }
}

void JSONDataInterface::checkRange(cJSON *attribute, const string &property_name, const string &child_name, bool &valid, double property_value)
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

void JSONDataInterface::checkNumeric(double property_value, const string &property_name, cJSON *schema_object, bool &valid, const string &child_name)
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
///
///
// from here down...
//utility functions only used in old IDF text import

std::vector<std::string> insertStringSplit(const std::string &s, char delim)
{
    std::vector<std::string> elems;
    std::string item;
    std::stringstream ss(s);
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    //expedient measure to ensure vector has three values
    elems.push_back("");
    elems.push_back("");
    elems.push_back("");
    return elems;
}

std::vector<std::string> splitString(const std::string &s, char delim)
{
    std::vector<std::string> elems;
    std::string item;
    std::stringstream ss(s);
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

std::string& trimString(std::string& str)
{
    str.erase(str.begin(), find_if(str.begin(), str.end(),
                                   [](char& ch)->bool { return !isspace(ch); }));
    str.erase(find_if(str.rbegin(), str.rend(),
                      [](char& ch)->bool { return !isspace(ch); }).base(), str.end());
    return str;
}

void JSONDataInterface::importJsonModel(string filename)
{
    ifstream idfj(filename.c_str(), std::ifstream::in);
    if (idfj) {
        string json_data = string((std::istreambuf_iterator<char>(idfj)), std::istreambuf_iterator<char>());
        if (json_data != "") {
            if (!validateModel()) {
                cout << "FAILURE: Invalid values detected in imported IDF" << endl;
            }
        } else {
            cout << "ERROR: JSON data not read from file. " << endl;
        }
    } else {
        cout << "ERROR: file not open. " << endl;
    }
}

vector<string> getFileObjectStrings(string filename)
{
    string oneline = "";
    string nextline = "";
    ifstream idf(filename.c_str(), std::ifstream::in);
    while (getline(idf,nextline)) {
        //reduce file to list of actual object instances and their fields
        if (nextline != "") { //discard blank lines that make bits.at(0) fail
            vector<string> bits = splitString(nextline,'!');
            string split = bits.at(0);//drop all after remark
            string checkblank = trimString(split);
            if (checkblank != "")
                oneline.append(checkblank);
        }
    }
    return splitString(oneline,';');
}

void insertTypedData(vector<string> obj_props, cJSON *current_object, unsigned int field_counter, const char* object_type, const char* field_name, cJSON *attribute_data)
{
    if (attribute_data){
        cJSON *data_type_obj = cJSON_GetObjectItem(attribute_data, "data_type");
        string data_value = "";
        try {
            data_value = obj_props.at(field_counter).c_str();
        } catch (std::out_of_range) {
            ;// really do nothing except not crash, when the final field is blank;
        }
        if (data_type_obj) {
            string data_type = data_type_obj->valuestring;
            if (data_value != "") { //exclude empty fields
                if ((data_type == "real") || (data_type == "integer")) {
                    if (data_type == "integer") {
                        try {
                            cJSON_AddNumberToObject(current_object, field_name, stoi(data_value));
                        } catch (std::invalid_argument) {
                            cJSON_AddStringToObject(current_object, field_name, data_value.c_str());
                            //since practically any numeric value can get a non-numeric value(auto... or parametric),
                            //we can't call this a failure, we simply have to write it as a string
                        }
                    } if (data_type == "real") {
                        try {
                            cJSON_AddNumberToObject(current_object, field_name, stod(data_value));
                        } catch (std::invalid_argument) {
                            cJSON_AddStringToObject(current_object, field_name, data_value.c_str());
                            //since practically any numeric value can get a non-numeric value(auto... or parametric),
                            //we can't call this a failure, we simply have to write it as a string
                        }
                    }
                } else { //all others are string
                    cJSON_AddStringToObject(current_object, field_name, data_value.c_str());
                }
            } //empty, continue
        } else { //all others are string, even those without data_type
            cout << "FAIL : data_type not found. " <<  object_type << " : " << field_name << endl;
        }
    } else {cout << "FAIL : attribute_data = " <<  object_type << endl;}
}

void insertField(cJSON *full_schema_object, const char* object_type, unsigned int field_counter, vector<string> obj_props, cJSON *field_object, cJSON *current_object)
{
    if (field_object) {
        const char* field_name = field_object->valuestring;
        cJSON *attribute_data = cJSON_GetObjectItem(full_schema_object, field_name );
        insertTypedData(obj_props, current_object, field_counter, object_type, field_name, attribute_data);
    } else {cout << "FAIL : field_object = " <<  object_type << endl;}
}


void JSONDataInterface::importIDFModel(string filename)
{   //this is specific to importing IDF text files, therefore cJSON model sequestered here
    cJSON *idd_ordered = cJSON_Parse(getIDD().c_str());
    if (!idd_ordered)
        cout << cJSON_GetErrorPtr();

    vector<string> object_instance_lines = getFileObjectStrings(filename);
    for(string objstr : object_instance_lines) {
        auto obj_props = splitString(objstr, ',');
        string object_string = obj_props.at(0);
        //rudimentary capture of Version
        if ((object_string == "Version")||(object_string == "VERSION")) {
            string version = obj_props.at(1);
            if (version != ACCEPTED_VERSION) {
                cout << "idfxt TRANSLATOR ONLY WORKS ON VERSION 8.2 IDF INPUT FILES" << endl << "this file version is:  " << version << endl;
                break;
            }
        }
        const char* object_type = object_string.c_str();
        cJSON *schema_object = cJSON_GetObjectItem(idd_ordered, object_type);
        if (schema_object) {
            unsigned int schema_field_count = cJSON_GetArraySize(schema_object);
            unsigned int model_field_count = obj_props.size() - 1;
            unsigned int base_field_count = (model_field_count < schema_field_count) ? model_field_count : schema_field_count;
            cJSON *current_object = cJSON_CreateObject();
            if (current_object) {
                cJSON_AddItemToObject(getModelRootObject(), getUuid().c_str(), current_object);
                cJSON_AddStringToObject(current_object, "object_type", object_type);
                //set this loop count for base object
                cJSON *full_schema_object = getSchemaObject(object_type);
                if (full_schema_object){
                    unsigned int field_counter;
                    for(field_counter = 1; field_counter <= base_field_count; ++field_counter){
                        cJSON *field_object = cJSON_GetArrayItem(schema_object,field_counter-1);
                        insertField(full_schema_object, object_type, field_counter, obj_props, field_object, current_object);
                    }
                    // if obj_props remain, use 'em up in extension objects
                    string extension_test_string = object_type;
                    extension_test_string.append("_x");  //TODO: extract value from object's "extension_type", if name convention changes
                    cJSON *extension_type = cJSON_GetObjectItem(idd_ordered, extension_test_string.c_str());
                    if (extension_type) {
                        cJSON *extension_array = cJSON_CreateArray();
                        cJSON_AddItemToObject(current_object,"extensions",extension_array);
                        unsigned int field_total = obj_props.size();
                        while (field_counter < field_total) {
                            cJSON *extension_object = cJSON_CreateObject();
                            if (extension_object) {
                                int size = cJSON_GetArraySize(extension_type);
                                for (int f = 0; f < size; ++f) {
                                    cJSON *extension_field = cJSON_GetArrayItem(extension_type, f);
                                    if (extension_field) {
                                        const char* extension_field_name = extension_field->valuestring;
                                        cJSON *schema_extension_object = getSchemaObject(extension_test_string.c_str());
                                        if (schema_extension_object){
                                            cJSON *field_data = cJSON_GetObjectItem(schema_extension_object, extension_field_name );
                                            if (field_data){
                                                insertTypedData(obj_props, extension_object, field_counter, extension_test_string.c_str(), extension_field_name, field_data);
                                            }
                                        }
                                        field_counter++;
                                    }
                                }
                                cJSON_AddItemToArray(extension_array, extension_object);
                            }
                        }
                    }
                }
            } else {
                cout << "ERROR: type \"" << object_type <<"\" is not found in E+ " << ACCEPTED_VERSION << " schema." << endl;
            }
        }
    }
    cJSON_Delete(idd_ordered);
}
