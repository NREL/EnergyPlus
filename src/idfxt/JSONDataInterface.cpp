// copyright 2014, Shannon Mackey <mackey@BUILDlab.net>
#include "JSONDataInterface.h"

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;
using namespace idfx;

JSONDataInterface::JSONDataInterface(const string &json_schema)
{
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

cJSON *JSONDataInterface::getSchemaObject(const string &object_type)
{
    return cJSON_GetObjectItem(schema_j, object_type.c_str());
}

cJSON *JSONDataInterface::getModelRootObject()
{
    return model_j;
}

bool JSONDataInterface::importModel(const string &json_content)
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
    return validateModel();
    //TBD: remap references from Label to uuid
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
