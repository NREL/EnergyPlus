/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <iostream>
#include <fstream>
#include <string>
#include <cstring>
#include <sstream>
#include <algorithm>
#include "rapidxml.hpp"
#include "definitions.h"
#include "sort_method.h"

#ifdef _WIN32
#include <direct.h>
#include <Windows.h>
#else
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include "IOUtil.h"

using namespace std;

//extern vardefs variable_definition_array;

bool ioutil::file_exists( const char *file )
{
#ifdef _WIN32
	// from wxWidgets: must use GetFileAttributes instead of ansi C 
	// b/c can cope with network (unc) paths
	DWORD ret = ::GetFileAttributesA( file );
	return (ret != (DWORD)-1) && !(ret & FILE_ATTRIBUTE_DIRECTORY);
#else
	struct stat st;
	return stat(file, &st) == 0 && S_ISREG(st.st_mode);
#endif
}

bool ioutil::dir_exists( const char *path )
{
#ifdef _WIN32
	// Windows fails to find directory named "c:\dir\" even if "c:\dir" exists,
	// so remove all trailing backslashes from the path - but don't do this for
	// the paths "d:\" (which are different from "d:") nor for just "\"
	char *wpath = _strdup( path );
	if (!wpath) return false;

	int pos = (int)strlen(wpath)-1;
	while (pos > 1 && (wpath[pos] == '/' || wpath[pos] == '\\'))
	{
		if (pos == 3 && wpath[pos-1] == ':') break;

		wpath[pos] = 0;
		pos--;
	}

	DWORD ret = ::GetFileAttributesA(wpath);
    bool exists =  (ret != (DWORD)-1) && (ret & FILE_ATTRIBUTE_DIRECTORY);

	free( wpath );

	return exists;
#else
	struct stat st;
	return ::stat(path, &st) == 0 && S_ISDIR(st.st_mode);
#endif
}

bool ioutil::remove_file( const char *path )
{
	return 0 == ::remove( path );
}

#ifdef SP_USE_MKDIR

#ifdef _WIN32
#define make_dir(x)  ::CreateDirectory(x, NULL)
#else
#define make_dir(x) ::_mkdir(x, 0777)
#endif

bool ioutil::mkdir( const char *path, bool make_full )
{
	if (make_full)
	{
		std::vector<std::string> parts = split( path, "/\\" );
	
		if (parts.size() < 1) return false;
		
		std::string cur_path = parts[0] + path_separator();
		
		for (size_t i=1;i<parts.size();i++)
		{
			cur_path += parts[i];

			if ( !dir_exists(cur_path.c_str()) )
				if (0 != make_dir( cur_path.c_str() ) ) return false;
						
			cur_path += path_separator();
		}

		return true;
	}
	else
		return 0 == make_dir( path );
}
#endif // SP_USE_MKDIR

std::string ioutil::path_only( const std::string &path )
{
	std::string::size_type pos = path.find_last_of("/\\");
	if (pos==std::string::npos) return path;
	else return path.substr(0, pos);
}

std::string ioutil::name_only( const std::string &path )
{
	std::string::size_type pos = path.find_last_of("/\\");
	if (pos==std::string::npos) return path;
	else return path.substr(pos+1);
}

std::string ioutil::ext_only( const std::string &path )
{
	std::string::size_type pos = path.find_last_of('.');
	if (pos==std::string::npos) return path;
	else return path.substr(pos+1);
}
	
char ioutil::path_separator()
{
#ifdef _WIN32
	return '\\';
#else
	return '/';
#endif
}

std::string ioutil::get_cwd()
{
	char buf[2048];
#ifdef _WIN32
	if (::GetCurrentDirectoryA(sizeof(buf), buf) == 0)
	  return std::string();
#else
	if (::getcwd(buf, sizeof(buf)) == NULL)
	  return std::string();
#endif
	return std::string(buf);
}

bool ioutil::set_cwd( const std::string &path )
{
#ifdef _WIN32
	return ::SetCurrentDirectoryA( path.c_str() ) != 0;
#else
	return ::chdir( path.c_str() ) == 0;
#endif
}


//--------------------

void ioutil::read_chars( FILE *fp, std::string &text, int nchars){
	int c;

	text = "";
	int nc=0;
	while( (c=fgetc(fp)) != EOF && nc < nchars){
		text += (char)c;
		nc++;
	}

}

bool ioutil::read_line( FILE *fp, std::string &buf, int prealloc )
{
	int c;

	buf = "";
	if (prealloc > 10)
		buf.reserve( (size_t)prealloc );

	// read the whole line, 1 character at a time, no concern about buffer length
	while ( (c=fgetc(fp)) != EOF && c != '\n' && c != '\r')
		buf += (char)c;

	// handle windows <CR><LF>
	if (c == '\r')
	{
		if ( (c=fgetc(fp)) != '\n')
			ungetc(c,fp);
	}

	// handle a stray <CR>
	if (c == '\n')
	{
		if ( (c=fgetc(fp)) != '\r')
			ungetc(c,fp);
	}

	return !(buf.length() == 0 && c == EOF);
}
void ioutil::read_file( const string &fname, string &file, string &eol_marker)
{
	
	file.clear();
	string line;
	ifstream fin(fname.c_str());
	
	eol_marker = "\n";

	if(fin.is_open())
	{
		while( getline(fin, line) )
		{
			file.append(line + "\n");
		}
		fin.close();
	}
	return;

}


void ioutil::parseXMLInputFile(const string &fname,var_map &V, parametric &par_data, optimization &opt_data){
	/*
	This algorithm takes file fname, reads in the contents and assigns the values to a map
	structure for later variable assignment. 

	The data structure for the file is XML format. Expected objects are variables and parametric simulation info.
	Structures are:


	--- 
	note that variables with multiline values (such as the solarfield.x.layout_data variable) would have each line separated by
	the newline "\n" character. 
	----
	<data>
		<version>2013.2.6</version>
		<header>Text for header line</header>

		<variable>
			<component>solarfield</component>
			<instance>0</instance>
			<varname>tht</varname>
			<units>m</units>
			<value>180.000</value>
		</variable>

		<parametric>
			<par_variable>
				<varname>solarfield.0.tht</varname>
				<display_text>Tower height</display_text>
				<units>m</units>
				<data_type>double</data_type>
				<selections>
					<selection>180.0</selection>
					<selection>190.0</selection>
				</selections>
				<choices>
					<choice>170.0</choice>
					<choice>180.0</choice>
					<choice>190.0</choice>
				</choices>
				<sim_values>
					<sim_value>180.0</sim_value>
					<sim_value>180.0</sim_value>
					<sim_value>190.0</sim_value>
					...
				</sim_values>
				<linked>true</linked>
				<layout_required>true</layout_required>
			</par_variable>
			<par_variable>
				...
			</par_variable>
		</parametric>
	</data>

	--The data structure returned is a set of maps 'V' where:
	V[<module type, str>][<instance, int>][<variable name, str>] = spvar{name, units, value}

	*/
	
	using namespace rapidxml;
	//Read in the file to a string
	string file;		//contents of the file
	string eol;
	ioutil::read_file(fname, file, eol);
	
	char *fstr = new char[file.size()+1];
	strncpy(fstr, (const char*)file.c_str(), file.size());
	fstr[file.size()] = 0;	//Null terminator

	xml_document<> doc;
	doc.parse<0>(fstr);
	xml_node<> *top_node = doc.first_node();	//<data>

	//get version
	string version = top_node->first_node("version")->value();

	//skip any header lines for now

    //Clean out the variable map
    V.reset();
    //remove existing instances of heliostat and receiver - they will be added later
    V.drop_heliostat(0);
    V.drop_receiver(0);
    
    vector<int> rec_insts;
    vector<int> hel_insts;

	//Read in all of the variables
	xml_node<> *var_node = top_node->first_node("variable");
    std::string component0 = "";
    int inst0 = -1;
	while(var_node != 0){
		//get the variable name composition
		std::string
			component = (char*)var_node->first_node("component")->value(),
			sinst = (char*)var_node->first_node("instance")->value(),
			varname = (char*)var_node->first_node("varname")->value(),
			units = (char*)var_node->first_node("units")->value();
		int inst;
		to_integer(sinst, &inst);

        //add new instances on the fly, if needed
        if(component == "receiver")
        {
            if(inst != inst0 || component != component0)
            {
                if( find(rec_insts.begin(), rec_insts.end(), inst) == rec_insts.end() )
                {
                    V.add_receiver( inst );
                    rec_insts.push_back( inst );
                }
            }
        }
        if(component == "heliostat")
        {
            if(inst != inst0 || component != component0)
            {
                if( find(hel_insts.begin(), hel_insts.end(), inst) == hel_insts.end() )
                {
                    V.add_heliostat( inst );
                    hel_insts.push_back( inst );
                }
            }
        }

        component0 = component;
        inst0 = inst;

		//Set up variable attributes according to the variable definitions. File data overwritten below...
		//V[component][inst][varname] = Defs[component][0][varname];
		//we need to store the info that we've been reading
        unordered_map<string, spbase*>::iterator v = V._varptrs.find( component + "." + sinst + "." + varname );
        if( v != V._varptrs.end() )
        {
            if( v->second->ctype == "combo" )
            {
                std::string selection = var_node->first_node("value")->value();
                std::vector< std::string > cbchoices = v->second->combo_get_choices();
                if(varname == "temp_which" || find( cbchoices.begin(), cbchoices.end(), selection ) != cbchoices.end() )
                    v->second->set_from_string( selection.c_str() );
            }
            else
            {
                v->second->set_from_string( var_node->first_node("value")->value() );
            }
            v->second->units = (char*)var_node->first_node("units")->value();
        }
        else
        {
            //variable isn't in the map.. what to do about that? Nothing for now
        }

		var_node = var_node->next_sibling("variable");
	}

	//Read in any parametric data
	par_data.clear();
	xml_node<> *par_node = top_node->first_node("parametric");
	if(par_node != 0){
		xml_node<> *par = par_node->first_node("par_variable");
		while(par != 0){
			//Add the variable by reference to the variable map object, then set relevant fields
			//par_data.addVar( getVarByString(V, (char*)par->first_node("varname")->value() ) );
            unordered_map<string, spbase*>::iterator v = V._varptrs.find( (string)par->first_node("varname")->value() );
            if( v != V._varptrs.end() )
                par_data.addVar( v->second );
            else
            {
			    par = par->next_sibling("par_variable");
                continue;
            }

			par_variable *pvar = &par_data.back();
			
			//units
			pvar->units = (char*)par->first_node("units")->value();
			//display text
			pvar->display_text = (char*)par->first_node("display_text")->value();
			//data type
			pvar->data_type = (char*)par->first_node("data_type")->value();
			//linked
			pvar->linked = lower_case( (string)(char*)par->first_node("linked")->value() ) == "true";
			//layout required
			pvar->layout_required = lower_case( (string)(char*)par->first_node("layout_required")->value() ) == "true";

			//Selections
			xml_node<> *sel_node = par->first_node("selections")->first_node("selection");
			pvar->selections.Clear();
			while(sel_node != 0){
				pvar->selections.push_back( sel_node->value() );
				sel_node = sel_node->next_sibling();
			}

			//Choices
			xml_node<> *choice_node = par->first_node("choices")->first_node("choice");
			pvar->choices.Clear();
			while(choice_node != 0){
				pvar->choices.push_back( choice_node->value() );
				choice_node = choice_node->next_sibling();
			}

			//Sim values
			xml_node<> *sim_node = par->first_node("sim_values")->first_node("sim_value");
			pvar->sim_values.Clear();
			while(sim_node != 0){
				pvar->sim_values.push_back( sim_node->value() );
				sim_node = sim_node->next_sibling();
			}

			par = par->next_sibling("par_variable");
		}

	}
    //Read in any optimization data
	opt_data.clear();
	xml_node<> *opt_node = top_node->first_node("optimization");
	if(opt_node != 0){
		xml_node<> *opt = opt_node->first_node("opt_variable");
		while(opt != 0){
			//Add the variable by reference to the variable map object, then set relevant fields
			//par_data.addVar( getVarByString(V, (char*)par->first_node("varname")->value() ) );
            unordered_map<string, spbase*>::iterator v = V._varptrs.find( (string)opt->first_node("varname")->value() );
            if( v != V._varptrs.end() )
                opt_data.addVar( v->second );
            else
            {
			    opt = opt->next_sibling("opt_variable");
                continue;
            }

			par_variable *ovar = &opt_data.back();
			
			//units
			ovar->units = (char*)opt->first_node("units")->value();
			//display text
			ovar->display_text = (char*)opt->first_node("display_text")->value();
			//data type
			ovar->data_type = (char*)opt->first_node("data_type")->value();
			//linked
			ovar->linked = lower_case( (string)(char*)opt->first_node("linked")->value() ) == "true";
			//layout required
			ovar->layout_required = lower_case( (string)(char*)opt->first_node("layout_required")->value() ) == "true";

			//Selections
			xml_node<> *sel_node = opt->first_node("selections")->first_node("selection");
			ovar->selections.Clear();
			while(sel_node != 0){
				ovar->selections.push_back( sel_node->value() );
				sel_node = sel_node->next_sibling();
			}

			//Choices
			xml_node<> *choice_node = opt->first_node("choices")->first_node("choice");
			ovar->choices.Clear();
			while(choice_node != 0){
				ovar->choices.push_back( choice_node->value() );
				choice_node = choice_node->next_sibling();
			}

			//Sim values
			xml_node<> *sim_node = opt->first_node("sim_values")->first_node("sim_value");
			ovar->sim_values.Clear();
			while(sim_node != 0){
				ovar->sim_values.push_back( sim_node->value() );
				sim_node = sim_node->next_sibling();
			}

			opt = opt->next_sibling("opt_variable");
		}

	}
	return;
}



bool ioutil::saveXMLInputFile(const string &fname, var_map &V, parametric &par_data, optimization &opt_data, const string &version){

	ofstream fobj(fname.c_str());
	if(fobj.is_open())
	{

		//main data structure
		fobj << "<data>\n";
		string t1 = "\t";
		string t2 = "\t\t";
		string t3 = "\t\t\t";
		string t4 = "\t\t\t\t";
	
		//version
		fobj << t1 << "<version>" << version << "</version>\n"; 
		//Add a header line with info on the last save time
		DTobj dt; dt.Now();
		fobj << t1 << "<header>Last saved " << dt._month << "-" << dt._mday << "-" << dt._year << " at " << dt._hour << ":" << dt._min << ":" << dt._sec << "</header>\n";
		
		//Write each variable
		string module, inst, varname, units;

        //get all of the keys, then sort alphabetically
        vector<string> keys;
        for( unordered_map<string, spbase*>::iterator it=V._varptrs.begin(); it != V._varptrs.end(); it++)
            keys.push_back(it->first);

        quicksort(keys);


        for(int i=0; i<(int)keys.size(); i++)
        {
            spbase *v = V._varptrs[keys.at(i)];

            vector<string> nm = split(v->name, ".");

            fobj << t1 << "<variable>\n";

			fobj << t2 << "<component>" << nm[0] << "</component>\n";
			fobj << t2 << "<instance>" << nm[1] << "</instance>\n";
			fobj << t2 << "<varname>" << nm[2] << "</varname>\n";
			fobj << t2 << "<units>" << v->units << "</units>\n";
            string val;
            v->as_string(val);
			fobj << t2 << "<value>" << val << "</value>\n";
				
			fobj << t1 << "</variable>\n";
        }

		//Write any parametric data
		if(par_data.size() > 0){
			fobj << t1 << "<parametric>\n";
			for(int i=0; i<par_data.size(); i++){
				fobj << t2 << "<par_variable>\n";
				par_variable *pv = &par_data[i];

				//varname
				fobj << t3 << "<varname>" << pv->varname << "</varname>\n";
				//display text
				fobj << t3 << "<display_text>" << pv->display_text << "</display_text>\n";
				//units
				fobj << t3 << "<units>" << pv->units << "</units>\n";
				//data type
				fobj << t3 << "<data_type>" << pv->data_type << "</data_type>\n";
				//linked
				fobj << t3 << "<linked>" << (pv->linked ? "true" : "false") << "</linked>\n";
				//layout required
				fobj << t3 << "<layout_required>" << (pv->layout_required ? "true" : "false") << "</layout_required>\n";

				//Selections
				fobj << t3 << "<selections>\n";
				for(int j=0; j<(int)pv->selections.size(); j++)
					fobj << t4 << "<selection>" << pv->selections[j] << "</selection>\n";
				fobj << t3 << "</selections>\n";

				//choices
				fobj << t3 << "<choices>\n";
				for(int j=0; j<(int)pv->choices.size(); j++)
					fobj << t4 << "<choice>" << pv->choices[j] << "</choice>\n";
				fobj << t3 << "</choices>\n";

				//sim_values
				fobj << t3 << "<sim_values>\n";
				for(int j=0; j<(int)pv->sim_values.size(); j++)
					fobj << t4 << "<sim_value>" << pv->sim_values[j] << "</sim_value>\n";
				fobj << t3 << "</sim_values>\n";

				fobj << t2 << "</par_variable>\n";
			}
			fobj << t1 << "</parametric>\n";
		}
        //Write any optimization data
		if(opt_data.size() > 0){
			fobj << t1 << "<optimization>\n";
			for(int i=0; i<opt_data.size(); i++){
				fobj << t2 << "<opt_variable>\n";
				par_variable *ov = &opt_data[i];

				//varname
				fobj << t3 << "<varname>" << ov->varname << "</varname>\n";
				//display text
				fobj << t3 << "<display_text>" << ov->display_text << "</display_text>\n";
				//units
				fobj << t3 << "<units>" << ov->units << "</units>\n";
				//data type
				fobj << t3 << "<data_type>" << ov->data_type << "</data_type>\n";
				//linked
				fobj << t3 << "<linked>" << (ov->linked ? "true" : "false") << "</linked>\n";
				//layout required
				fobj << t3 << "<layout_required>" << (ov->layout_required ? "true" : "false") << "</layout_required>\n";

				//Selections
				fobj << t3 << "<selections>\n";
				for(int j=0; j<(int)ov->selections.size(); j++)
					fobj << t4 << "<selection>" << ov->selections[j] << "</selection>\n";
				fobj << t3 << "</selections>\n";

				//choices
				fobj << t3 << "<choices>\n";
				for(int j=0; j<(int)ov->choices.size(); j++)
					fobj << t4 << "<choice>" << ov->choices[j] << "</choice>\n";
				fobj << t3 << "</choices>\n";

				//sim_values
				fobj << t3 << "<sim_values>\n";
				for(int j=0; j<(int)ov->sim_values.size(); j++)
					fobj << t4 << "<sim_value>" << ov->sim_values[j] << "</sim_value>\n";
				fobj << t3 << "</sim_values>\n";

				fobj << t2 << "</opt_variable>\n";
			}
			fobj << t1 << "</optimization>\n";
		}

		fobj << "</data>\n";

		fobj.close();
		//--------------
		return true;
	}
	else{
		return false;
	}


}

string ioutil::getDelimiter(std::string &text){
	if( text == "") return ",";
	//Find the type of delimiter
	vector<string> delims;
	delims.push_back(",");
	delims.push_back(" ");
	delims.push_back("\t");
	delims.push_back(";");
	string delim = "\t";	//initialize
	int ns=0;
	for(int i=0; i<4; i++){
		vector<string> data = split(text, delims[i]);
		if((int)data.size()>ns){ delim = delims[i]; ns = (int)data.size(); }	//pick the delimiter that returns the most entries
	}
	return delim;
}
