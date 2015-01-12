// copyright 2014, Shannon Mackey <mackey@BUILDlab.net>


#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>


#include "JSONDataInterface.h"
#include "idd-full.h"


using namespace std;
using namespace idfx;

const std::string VERSION_TEXT = "0.0.1";
const std::string HELP_TEXT =
    "\n'idfxt' is a command-line tool for translating IDF files\nto a format suitable for EnergyPlus (version > 8.3).\n"
    "\nUSAGE:\n\tidfxt [option] [value]\n\nOPTIONS:\n"
    "  --idf \t -i <input_filename>\n\t\t\t Translate an IDF (.IDF) file to JSON (.IDFj),\n\t\t\t and validate.\n\n"
    "  --json\t -j <input_filename>\n\t\t\t Validate a JSON (.IDFj) file,\n\t\t\t and show results.\n\n"
    "  --json\t -t <input_filename>\n\t\t\t Test - dump to console all the objects' property maps\n\n"
    "  --version\t -v      Display idfxt version information.\n"
    "  --help\t -h      Display usage information.\n"
    "\nExamples:\n"
    "  idfxt -i example.idf  \n\t\tTranslate and import IDF file.\n\n"
    "  idfxt -j example.idfj \n\t\tValidate IDFj file.\n\n"
    "Note:\n  'idfxt' is primarily a tool for updating legacy IDF files.\n"
    "  Application developers should use the standard C++ library,\n  that idfxt is built upon, for building their own applications.\n"
    "  Information can be found at : <url here>\n\n" ;

JSONDataInterface *Data;

//utility function

std::string replaceString(std::string subject, const std::string &search,
                          const std::string &replace)
{
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
    Data->importJsonModel(in_file); //load Data
    if (!Data->validateModel()) {
        cout << "FAILURE: Invalid values detected in - " << in_file << endl;
        return 1;
    }

    Data->writeJSONdata(replaceString(in_file, ".idfx", ".valid_idfx"));
    return 0;
}

int actionTranslate(string in_file)
{
    //TODO: check file type, or auto determine it and remove input type switch
    Data->importIDFModel(in_file);  //load Data
    if (!Data->validateModel()) {
        cout << "FAILURE: Invalid values detected in - " << in_file << endl;
        return 1;
    }

    Data->writeJSONdata(replaceString(in_file, ".idf", ".idfx"));
    return 0;
}

void test_DumpPropertyMaps(string in_file)
{
    Data->importIDFModel(in_file);  //load Data
    auto allobjects = Data->getModelObjects();
    while (!allobjects.empty()) {
        auto one_object(allobjects.front());
        //cout << oneObject->print();
        for (const auto & one : one_object->getProperties()) {
            cout << one.first << " : " << one.second << endl;
        }
        for (const auto & child : one_object->getExtensions()) {
            if (child) {
                //	  cout << endl << " ---  " << child->print();
                cout << " ---------------- \n";
                for (const auto & key_val : child->getProperties()) {
                    cout << " ---  " << key_val.first << " : " << key_val.second << endl;
                }
            }
        }
        cout << endl << endl;
        allobjects.pop_front();
    }
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
            } else if ((arg1 == "-t") || (arg1 == "--test")) {
                initData();
                test_DumpPropertyMaps(input_filename);
            }
        } else { // action arg only
            if ((arg1 == "-v") || (arg1 == "--version")) {
                cout << "idfxt version: " << VERSION_TEXT << endl;
                return 0;
            } else { // ? catches --help, or anything else
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

