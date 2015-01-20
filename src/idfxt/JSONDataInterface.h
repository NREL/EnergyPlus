// copyright 2014, Shannon Mackey <mackey@BUILDlab.net>
#ifndef JSONDATAINTERFACE_H
#define JSONDATAINTERFACE_H

//for JSONDataObject and JSONDataInterface
#include <string>
#include <list>
#include <map>
#include <memory>

//for old IDF text import only
#include <algorithm>
#include <cctype>
#include <cstring>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <chrono>
#include <random>

const std::string ACCEPTED_VERSION = "8.2";
//end old IDF text import only

extern "C"
{
#include "../../third_party/cJSON/cJSON.h"
}

namespace idfx {


class JSONDataObject
{
public:
    JSONDataObject(const std::string &json_content);
    ~JSONDataObject();

    std::map<std::string, std::string> getProperties();
    std::string getPropertyValue(std::string);
//    void setPropertyValue(std::string, std::string);
//    void setPropertyValue(std::string, int);
//    void setPropertyValue(std::string, double);

    std::list<std::shared_ptr<JSONDataObject> > getExtensions();

    
    std::string print();
    //friend access private constructor to avoid conversion to/from strings internally
//    friend std::list<JSONDataObject> JSONDataInterface::getModelObjects(std::string);


private:
    cJSON *data_j;

    void c_getChildren(std::list< std::shared_ptr< idfx::JSONDataObject > > &return_object_list, cJSON *subitem);
//    JSONDataObject(cJSON &cjson); //to hide cJSON pointer
};


//////////////////////////////////////////////////////////

class JSONDataInterface
{
public:
    JSONDataInterface(const std::string &json_schema);
    ~JSONDataInterface();

    std::list<std::shared_ptr<JSONDataObject> > getModelObjects(std::string object_type = "");

    std::list<JSONDataObject> getModelObjectDataByIndex(
            std::string object_uuid,
            int index);

    bool importIDFxModel(const std::string &json_content);
    bool validateModel();
    void writeJSONdata(const std::string &filename);

    void importJsonModel(std::string filename);
    void importIDFModel(std::string filename);
private:
    cJSON *schema_j;
    cJSON *model_j;

    cJSON *getSchemaObject(const std::string &object_type);
    cJSON *getModelRootObject();

    void checkRange(cJSON *attribute,
                    const std::string &property_name,
                    const std::string &child_name,
                    bool &valid,
                    double property_value);
    void checkNumeric(double property_value,
                      const std::string &property_name,
                      cJSON *schema_object,
                      bool &valid,
                      const std::string &child_name);
};

} //idfxt namespace
#endif // JSONDATAINTERFACE_H
