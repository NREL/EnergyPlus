// copyright 2014, Shannon Mackey <mackey@BUILDlab.net>

#include <unistd.h>
#include <algorithm>
#include <cctype>
#include <cstring>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <chrono>

extern "C"
{
#include "../../third_party/cJSON/cJSON.h"
}

#include "JSONDataInterface.h"
#include "idd-full.h"
#include "idd-minimal-ordered.h"

using namespace std;
using namespace idfx;

const std::string ACCEPTED_VERSION = "8.2";
const std::string VERSION_TEXT = "0.0.1";
const std::string HELP_TEXT =
        "\n'idfxt' is a command-line tool for translating IDF files\nto a format suitable for EnergyPlus (version > 8.3).\n"
        "\nUSAGE:\n\tidfxt [option] [value]\n\nOPTIONS:\n"
        "  --idf \t -i <input_filename>\n\t\t\t Translate an IDF (.IDF) file to JSON (.IDFj),\n\t\t\t and validate.\n\n"
        "  --json\t -j <input_filename>\n\t\t\t Validate a JSON (.IDFj) file,\n\t\t\t and show results.\n\n"
        "  --version\t -v      Display idfxt version information.\n"
        "  --help\t -h      Display usage information.\n"
        "\nExamples:\n"
        "  idfxt -i example.idf  \n\t\tTranslate and import IDF file.\n\n"
        "  idfxt -j example.idfj \n\t\tValidate IDFj file.\n\n"
        "Note:\n  'idfxt' is primarily a tool for updating legacy IDF files.\n"
        "  Application developers should use the standard C++ library,\n  that idfxt is built upon, for building their own applications.\n"
        "  Information can be found at : <url here>\n\n" ;

enum Action {
    NONE = 0, TRANSLATE, JSON
};

JSONDataInterface *Data;

//forward declarations
void importJsonModel(string filename);
void importIDFModel(string filename);

//utility functions
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

std::string replaceString(std::string subject, const std::string& search,
                          const std::string& replace) {
    size_t pos = 0;
    while ((pos = subject.find(search, pos)) != std::string::npos) {
        subject.replace(pos, search.length(), replace);
        pos += replace.length();
    }
    return subject;
}

// translator functions
void initData()
{
    string json_schema_str = getSchema();
    Data = new JSONDataInterface(json_schema_str);
}

int actionJSON(string in_file)
{
    //TODO: check file type, or auto determine it and remove input type switch
    importJsonModel(in_file); //load Data
    if (!Data->integrateModel()) {
        cout << "FAILURE: Invalid values detected in - " << in_file << endl;
        return 1;
    }

    Data->writeJSONdata(replaceString(in_file, ".idfx", ".valid_idfx"));
    return 0;
}

int actionTranslate(string in_file)
{
    //TODO: check file type, or auto determine it and remove input type switch
    importIDFModel(in_file);  //load Data
    if (!Data->integrateModel()) {
        cout << "FAILURE: Invalid values detected in - " << in_file << endl;
        return 1;
    }
    Data->writeJSONdata(replaceString(in_file, ".idf", ".idfx"));
    return 0;
}

void importJsonModel(string filename)
{
    ifstream idfj(filename.c_str(), std::ifstream::in);
    if (idfj) {
        string json_data = string((std::istreambuf_iterator<char>(idfj)), std::istreambuf_iterator<char>());
        if (json_data != "") {
            if (!Data->importModel(json_data)) {
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


void importIDFModel(string filename)
{   //this is specific to importing IDF text files, therefore cJSON model created here, not in JSON-oriented JSONDataInterface
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
                cJSON_AddItemToObject(Data->getModelRootObject(), object_type, current_object);
                //set this loop count for base object
                cJSON *full_schema_object = Data->getSchemaObject(object_type);
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
                                        cJSON *schema_extension_object = Data->getSchemaObject(extension_test_string.c_str());
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



//// MAIN ////
int main(int argc, char **argv)
{
    string input_filename = "";
    // process args - set ActionMode and ActionValue
    // plain and simple - takes only single switches and/or arguments as shown in HELP_TEXT
    if ((argc == 2) || (argc == 3)) { //one or two args
        string arg1 = string(argv[1]);
        if (argc == 3) { //action and value
            input_filename = string(argv[2]);
            if ((arg1 == "-i") || (arg1 == "--idf")) {
                initData(); // setup JSONDataInterface
                actionTranslate(input_filename);
            } else if ((arg1 == "-j") || (arg1 == "--json")) {
                initData(); // setup JSONDataInterface
                actionJSON(input_filename);
            }
        } else { // action arg only
            if ((arg1 == "-v") || (arg1 == "--version")) {
                cout << "idfxt version: " << VERSION_TEXT << endl;
                return 0;
            } else if ((arg1 == "-h") || (arg1 == "--help")) {
                cout << HELP_TEXT;
                return 1;
            } else { // ?
                cout << HELP_TEXT;
                return 1;
            }
        }
    } else { //wrong number of args
        cout << HELP_TEXT;
        return 1;
    }

    delete Data;
    return 0;
}

