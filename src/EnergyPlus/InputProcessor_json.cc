// C++ Headers
#include <algorithm>
#include <istream>
#include <iostream>
#include <fstream>
#include <unordered_map>
#include <fstream>
#include <iostream>
#include <InputProcessor_json.hh>
#include <nlohmann/json.hpp>

// ObjexxFCL Headers
#include <ObjexxFCL/Backspace.hh>
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/stream.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <InputProcessor_json.hh>
#include <DataIPShortCuts.hh>
#include <DataOutputs.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataStringGlobals.hh>
#include <DataSystemVariables.hh>
#include <DisplayRoutines.hh>
#include <SortAndStringUtilities.hh>

using json = nlohmann::json;

json EnergyPlus::InputProcessor::jdf = json();
json EnergyPlus::InputProcessor::schema = json();
IdfParser EnergyPlus::InputProcessor::idf_parser = IdfParser();
State EnergyPlus::InputProcessor::state = State();
std::ostream *EnergyPlus::InputProcessor::echo_stream = nullptr;

json IdfParser::decode(std::string const &idf, json const &schema) {
    bool success = true;
    return decode(idf, schema, success);
}

json IdfParser::decode(std::string const &idf, json const &schema, bool &success) {
    json root;
    success = true;
    if (idf.empty()) return root;

    size_t index = 0;
    root = parse_idf(idf, index, success, schema);
    return root;
}

std::string IdfParser::encode(json const &root, json const &schema) {
    std::string encoded;

    for (auto obj = root.begin(); obj != root.end(); ++obj) {
        const auto &legacy_idd = schema["properties"][obj.key()]["legacy_idd"]["fields"];
        for (auto obj_in = obj.value().begin(); obj_in != obj.value().end(); ++obj_in) {
            encoded += obj.key();
            int skipped_fields = 0;
            for (int i = 0; i < legacy_idd.size(); i++) {
                std::string entry = legacy_idd[i];
                if (obj_in.value().find(entry) == obj_in.value().end()) {
                    if (entry == "name") encoded += ",\n  " + obj_in.key();
                    else skipped_fields++;
                    continue;
                }
                for (int j = 0; j < skipped_fields; j++) encoded += ",\n  ";
                skipped_fields = 0;
                encoded += ",\n  ";
                auto const &val = obj_in.value()[entry];
                if (val.is_string()) encoded += val.get<std::string>();
                else encoded += std::to_string(val.get<double>());
            }

            if (obj_in.value().find("extensions") == obj_in.value().end()) {
                encoded += ";\n\n";
                continue;
            }

            auto &extensions = obj_in.value()["extensions"];
            for (int extension_i = 0; extension_i < extensions.size(); extension_i++) {
                auto &cur_extension_obj = extensions[extension_i];
                auto &extensible = schema["properties"][obj.key()]["legacy_idd"]["extensibles"];
                for (int i = 0; i < extensible.size(); i++) {
                    std::string tmp = extensible[i];
                    if (cur_extension_obj.find(tmp) == cur_extension_obj.end()) {
                        skipped_fields++;
                        continue;
                    }
                    for (int j = 0; j < skipped_fields; j++) encoded += ",\n  ";
                    skipped_fields = 0;
                    encoded += ",\n  ";
                    if (cur_extension_obj[tmp].is_string()) encoded += cur_extension_obj[tmp];
                    else encoded += std::to_string(cur_extension_obj[tmp].get<double>());
                }
            }
            encoded += ";\n\n";
        }
    }
    return encoded;
}

json IdfParser::parse_idf(std::string const &idf, size_t &index, bool &success, json const &schema) {
    json root;
    Token token;

    while (true) {
        token = look_ahead(idf, index);
        if (token == Token::END) {
            break;
        } else if (token == Token::NONE) {
            success = false;
            return root;
        } else if (token == Token::EXCLAMATION) {
            eat_comment(idf, index);
        } else {
            std::string obj_name = parse_string(idf, index, success);
            for (char &c : obj_name) c = (char) toupper(c);
            auto tmp_umit = case_insensitive_keys.find(obj_name);
            if (tmp_umit != case_insensitive_keys.end()) {
                obj_name = tmp_umit->second;
            } else {
                print_out_line_error(idf, false);
                while (token != Token::SEMICOLON && token != Token::END) token = next_token(idf, index);
                continue;
            }
            json obj_loc = schema["properties"][obj_name];
            json loc = obj_loc["legacy_idd"];
            json obj = parse_object(idf, index, success, loc, obj_loc);
            if (!success) print_out_line_error(idf, true);
            std::string name = "";
            if (!obj.is_null()) {
                if (obj.find("name") != obj.end()) {
                    name = obj["name"].get<std::string>();
                    obj.erase("name");
                }
            }
            if (root[obj_name].find(name) != root[obj_name].end()) {
                name = obj_name + " " + std::to_string(root[obj_name].size() + 1);
            }
            root[obj_name][name] = obj;
        }
    }

    return root;
}

json IdfParser::parse_object(std::string const &idf, size_t &index, bool &success,
                             json const &loc, json const &obj_loc) {
    json root, extensible = json::object();
    json array_of_extensions = json::array();
    Token token;
    index += 1; // move index past the comma of the EnergyPlus Object Type
    int legacy_idd_index = 0, extensible_index = 0;
    success = true;
    bool was_value_parsed = false;

    while (true) {
        token = look_ahead(idf, index);
        if (token == Token::NONE) {
            success = false;
            return root;
        } else if (token == Token::END) {
            return root;
        } else if (token == Token::COMMA || token == Token::SEMICOLON) {
            if (!was_value_parsed) {
                int ext_size = 0;
                std::string field_name;
                if (legacy_idd_index < loc["fields"].size()) {
                    field_name = loc["fields"][legacy_idd_index];
                } else {
                    ext_size = static_cast<int>(loc["extensibles"].size());
                    field_name = loc["extensibles"][extensible_index % ext_size];
                    extensible_index++;
                }
                add_missing_field_value(field_name, root, extensible, obj_loc, loc, legacy_idd_index);
                if (ext_size && extensible_index % ext_size == 0) {
                    array_of_extensions.push_back(extensible);
                    extensible.clear();
                }
            }

            legacy_idd_index++;
            was_value_parsed = false;
            next_token(idf, index);
            if (token == Token::SEMICOLON) {
                for (int i = legacy_idd_index; i < loc["fields"].size(); i++) {
                    std::string name = loc["fields"][i];
                    add_missing_field_value(name, root, extensible, obj_loc, loc, legacy_idd_index);
                }
                if (loc.find("extensibles") == loc.end()) break;
                unsigned long ext_size = loc["extensibles"].size();
                if (extensible_index % ext_size == 0) break;
                while (extensible_index % ext_size) {
                    std::string name = loc["extensibles"][extensible_index % ext_size];
                    add_missing_field_value(name, root, extensible, obj_loc, loc, legacy_idd_index);
                    extensible_index++;
                }
                array_of_extensions.push_back(extensible);
                extensible.clear();
                break;
            }
        } else if (token == Token::EXCLAMATION) {
            eat_comment(idf, index);
        } else if (legacy_idd_index >= loc["fields"].size()) {
            if (loc.find("extensibles") == loc.end()) {
                success = false;
                return root;
            }
            auto const size = loc["extensibles"].size();
            std::string const field_name = loc["extensibles"][extensible_index % size];
            auto val = parse_value(idf, index, success,
                                   obj_loc["patternProperties"][".*"]["properties"]["extensions"]["items"]["properties"][field_name]);
            extensible[field_name] = val;
            was_value_parsed = true;
            extensible_index++;
            if (extensible_index && extensible_index % size == 0) {
                array_of_extensions.push_back(extensible);
                extensible.clear();
            }
        } else {
            was_value_parsed = true;
            std::string field = loc["fields"][legacy_idd_index];
            auto it = obj_loc.find("patternProperties");
            if (it == obj_loc.end()) {
                if (obj_loc.find("properties") != obj_loc.end()) {
                    root[field] = parse_value(idf, index, success, obj_loc["properties"][field]);
                } else {
                    std::cout << "Field " << field << " was not found at line " << cur_line_num << std::endl;
                }
                legacy_idd_index++;
                continue;
            }
            auto const &tmp = obj_loc["patternProperties"][".*"]["properties"];
            if (tmp.find(field) == tmp.end()) {
                if (field == "name") root[field] = parse_string(idf, index, success);
                else std::cout << "Field " << field << " was not found at line " << cur_line_num << std::endl;
            } else {
                root[field] = parse_value(idf, index, success, obj_loc["patternProperties"][".*"]["properties"][field]);
            }
            if (!success) return root;
        }
    }
    if (array_of_extensions.size()) {
        root["extensions"] = array_of_extensions;
    }
    return root;
}

void IdfParser::add_missing_field_value(std::string &field_name, json &root, json &extensible, json const &obj_loc,
                                        json const &loc, int legacy_idd_index) {
    json tmp;
    int ext_size = 0;
    if (obj_loc.find("patternProperties") != obj_loc.end()) {
        tmp = obj_loc["patternProperties"][".*"]["properties"];
    } else if (obj_loc.find("properties") != obj_loc.end()) {
        tmp = obj_loc["properties"][field_name];
    }
    if (legacy_idd_index >= loc["fields"].size()) {
        tmp = tmp["extensions"]["items"]["properties"];
        ext_size = static_cast<int>(loc["extensibles"].size());
    }
    if (tmp.find(field_name) != tmp.end()) {
        auto const obj_field = tmp[field_name];
        if (obj_field.find("default") != obj_field.end()) {
            auto const default_val = obj_field["default"];
            if (default_val.is_string()) {
                if (!ext_size) root[field_name] = default_val.get<std::string>();
                else extensible[field_name] = default_val.get<std::string>();
            } else {
                if (!ext_size) root[field_name] = default_val.get<double>();
                else extensible[field_name] = default_val.get<double>();
            }
        } else {
            if (!ext_size) root[field_name] = "";
            else extensible[field_name] = "";
        }
    }
}

json IdfParser::parse_number(std::string const &idf, size_t &index, bool &success) {
    size_t save_i = index;
    eat_whitespace(idf, save_i);
    json val;
    bool is_double = false, is_sign = false, is_scientific = false;
    std::string num_str, numeric = "-+.eE0123456789";

    while (numeric.find_first_of(idf[save_i]) != std::string::npos) {
        num_str += idf[save_i];
        if (idf[save_i] == '.') {
            if (is_double) {
                success = false;
                return val;
            }
            is_double = true;
        } else if (idf[save_i] == '-' || idf[save_i] == '+') {
            if (is_sign && !is_scientific) {
                success = false;
                return val;
            }
            is_sign = true;
        } else if (idf[save_i] == 'e' || idf[save_i] == 'E') {
            if (is_scientific) {
                success = false;
                return val;
            }
            is_scientific = true;
        }
        save_i++;
    }

    assert(!num_str.empty());
    Token token = look_ahead(idf, save_i);
    if (token != Token::SEMICOLON && token != Token::COMMA) {
        success = false;
        return val;
    }
    if (is_double) {
        val = stod(num_str, 0);
    } else {
        try {
            val = stoi(num_str, 0);
        } catch (std::exception e) {
            val = stoll(num_str, 0);
        }
    }
    index = save_i;
    return val;
}

json IdfParser::parse_value(std::string const &idf, size_t &index, bool &success, json const &field_loc) {
    json value;
    switch (look_ahead(idf, index)) {
        case Token::STRING: {
            value = parse_string(idf, index, success);
            if (field_loc.find("enum") != field_loc.end()) {
                for (auto &s : field_loc["enum"]) {
                    if (icompare(s, value.get<std::string>())) {
                        value = s;
                        break;
                    }
                }
            } else if (icompare(value.get<std::string>(), "Autosize") ||
                       icompare(value.get<std::string>(), "Autocalculate")) {
                value = field_loc["anyOf"][1]["enum"][0];
            }
            return value;
        }
        case Token::NUMBER: {
            size_t save_line_index = index_into_cur_line;
            size_t save_line_num = cur_line_num;
            value = parse_number(idf, index, success);
            if (!success) {
                cur_line_num = save_line_num;
                index_into_cur_line = save_line_index;
                success = true;
                value = parse_string(idf, index, success);
                return value;
            }
            return value;
        }
        case Token::NONE:
        case Token::END:
        case Token::EXCLAMATION:
        case Token::COMMA:
        case Token::SEMICOLON:
            break;
    }
    success = false;
    return value;
}

std::string IdfParser::parse_string(std::string const &idf, size_t &index, bool &success) {
    eat_whitespace(idf, index);

    std::string s;
    char c;

    bool complete = false;
    while (!complete) {
        if (index == idf.size()) {
            complete = true;
            break;
        }

        c = idf[index];
        increment_both_index(index, index_into_cur_line);
        if (c == ',') {
            complete = true;
            decrement_both_index(index, index_into_cur_line);
            break;
        } else if (c == ';') {
            complete = true;
            decrement_both_index(index, index_into_cur_line);
            break;
        } else if (c == '!') {
            complete = true;
            decrement_both_index(index, index_into_cur_line);
            break;
        } else if (c == '\\') {
            if (index == idf.size()) break;
            char next_c = idf[index];
            increment_both_index(index, index_into_cur_line);
            if (next_c == '"') {
                s += '"';
            } else if (next_c == '\\') {
                s += '\\';
            } else if (next_c == '/') {
                s += '/';
            } else if (next_c == 'b') {
                s += '\b';
            } else if (next_c == 't') {
                s += '\t';
            } else if (next_c == 'n') {
                complete = false;
                break;
            } else if (next_c == 'r') {
                complete = false;
                break;
            } else {
                s += c;
                s += next_c;
            }
        } else {
            s += c;
        }
    }

    if (!complete) {
        success = false;
        return std::string();
    }
	size_t str_size = s.size();
    return rtrim(s);
}

void IdfParser::increment_both_index(size_t &index, size_t &line_index) {
    index++;
    line_index++;
}

void IdfParser::decrement_both_index(size_t &index, size_t &line_index) {
    index--;
    line_index--;
}

void IdfParser::print_out_line_error(std::string const &idf, bool obj_found) {
    if (obj_found) std::cout << "error: \"extra field(s)\" ";
    else std::cout << "error: \"obj not found in schema\" ";
    std::cout << "at line number " << cur_line_num << " (index " << index_into_cur_line << ")\n";
    std::cout << "Line: ";
    while (idf[beginning_of_line_index++] != '\n') std::cout << idf[beginning_of_line_index];
    std::cout << std::endl;
}

void IdfParser::eat_whitespace(std::string const &idf, size_t &index) {
    while (index < idf.size()) {
        switch (idf[index]) {
            case ' ':
            case '\r':
            case '\t':
                increment_both_index(index, index_into_cur_line);
                continue;
            case '\n':
                increment_both_index(index, cur_line_num);
                beginning_of_line_index = index;
                index_into_cur_line = 0;
                continue;
            default:
                return;
        }
    }
}

void IdfParser::eat_comment(std::string const &idf, size_t &index) {
    while (true) {
        if (index == idf.size()) break;
        if (idf[index] == '\n') {
            increment_both_index(index, cur_line_num);
            index_into_cur_line = 0;
            beginning_of_line_index = index;
            break;
        }
        increment_both_index(index, index_into_cur_line);
    }
}

IdfParser::Token IdfParser::look_ahead(std::string const &idf, size_t index) {
    size_t save_index = index;
    size_t save_line_num = cur_line_num;
    size_t save_line_index = index_into_cur_line;
    Token token = next_token(idf, save_index);
    cur_line_num = save_line_num;
    index_into_cur_line = save_line_index;
    return token;
}

IdfParser::Token IdfParser::next_token(std::string const &idf, size_t &index) {
    eat_whitespace(idf, index);

    if (index == idf.size()) {
        return Token::END;
    }

    char const c = idf[index];
    increment_both_index(index, index_into_cur_line);
    switch (c) {
        case '!':
            return Token::EXCLAMATION;
        case ',':
            return Token::COMMA;
        case ';':
            return Token::SEMICOLON;
        default:
            static std::string const search_chars("-:.#/\\[]{}_@$%^&*()|+=<>?'\"~");
            static std::string const numeric(".-+0123456789");
            if (numeric.find_first_of(c) != std::string::npos) {
                return Token::NUMBER;
            } else if (isalnum(c) || (std::string::npos != search_chars.find_first_of(c))) {
                return Token::STRING;
            }
            break;
    }
    decrement_both_index(index, index_into_cur_line);
    return Token::NONE;
}

void State::initialize(json &parsed_schema) {
    stack.clear();
    schema = parsed_schema;
    stack.push_back(schema);
    json &loc = stack.back()["required"];
    for (auto &s : loc) root_required.emplace(s.get<std::string>(), false);
}

void State::traverse(json::parse_event_t &event, json &parsed, unsigned line_num, unsigned line_index) {
    switch (event) {
        case json::parse_event_t::object_start: {
            if (is_in_extensibles or stack.back().find("patternProperties") == stack.back().end()) {
                if (stack.back().find("properties") != stack.back().end())
                    stack.push_back(stack.back()["properties"]);
            } else {
                stack.push_back(stack.back()["patternProperties"][".*"]);
                if (stack.back().find("required") != stack.back().end()) {
                    auto &loc = stack.back()["required"];
                    obj_required.clear();
                    for (auto &s : loc) obj_required.emplace(s.get<std::string>(), false);
                }
            }
            last_seen_event = event;
            break;
        }

        case json::parse_event_t::value: {
            validate(parsed, line_num, line_index);
            if (does_key_exist) stack.pop_back();
            does_key_exist = true;
            last_seen_event = event;
            break;
        }

        case json::parse_event_t::key: {
            std::string key = parsed;
            prev_line_index = line_index;
            prev_key_len = (unsigned) key.size() + 3;
            if (need_new_object_name) {
                cur_obj_name = key;
                cur_obj_count = 0;
                need_new_object_name = false;
                if (cur_obj_name.find("Parametric:") != std::string::npos) {
                    errors.push_back("You must run Parametric Preprocesor for \"" + cur_obj_name + "\" at line " +
                                     std::to_string(line_num + 1));
                } else if (cur_obj_name.find("Template") != std::string::npos) {
                    errors.push_back("You must run the ExpandObjects program for \"" + cur_obj_name + "\" at line " +
                                     std::to_string(line_num + 1));
                }
            }

            if (stack.back().find("properties") == stack.back().end() and key != "") {
                if (stack.back().find(key) != stack.back().end()) {
                    stack.push_back(stack.back()[key]);
                } else {
                    errors.push_back("Key \"" + key + "\" in object \"" + cur_obj_name + "\" at line "
                                     + std::to_string(line_num) + " (index " + std::to_string(line_index) +
                                     ") not found in schema");
                    does_key_exist = false;
                }
            }


            if (!is_in_extensibles) {
                auto req = obj_required.find(key);
                if (req != obj_required.end())
                    req->second = true; // required field is now accounted for, for this specific object
                req = root_required.find(key);
                if (req != root_required.end()) req->second = true; // root_required field is now accounted for
            } else {
                auto req = extensible_required.find(key);
                if (req != extensible_required.end()) req->second = true;
            }

            last_seen_event = event;
            break;
        }

        case json::parse_event_t::array_start: {
            stack.push_back(stack.back()["items"]);
            if (stack.back().find("required") != stack.back().end()) {
                auto &loc = stack.back()["required"];
                extensible_required.clear();
                for (auto &s : loc) extensible_required.emplace(s.get<std::string>(), false);
            }
            is_in_extensibles = true;
            last_seen_event = event;
            break;
        }

        case json::parse_event_t::array_end: {
            stack.pop_back();
            stack.pop_back();
            is_in_extensibles = false;
            last_seen_event = event;
            break;
        }

        case json::parse_event_t::object_end: {
            if (is_in_extensibles) {
                for (auto &it : extensible_required) {
                    if (!it.second) {
                        errors.push_back(
                                "Required extensible field \"" + it.first + "\" in object \"" + cur_obj_name
                                + "\" ending at line " + std::to_string(line_num) + " (index "
                                + std::to_string(line_index) + ") was not provided");
                    }
                    it.second = false;
                }
            } else if (last_seen_event != json::parse_event_t::object_end) {
                cur_obj_count++;
                for (auto &it : obj_required) {
                    if (!it.second) {
                        errors.push_back(
                                "Required field \"" + it.first + "\" in object \"" + cur_obj_name
                                + "\" ending at line " + std::to_string(line_num) + " (index "
                                + std::to_string(line_index) + ") was not provided");
                    }
                    it.second = false;
                }
            } else { // must be at the very end of an object now
                if (cur_obj_name != "Version") stack.pop_back();
                const auto &loc = stack.back();
                if (loc.find("minProperties") != loc.end() && cur_obj_count < loc["minProperties"].get<unsigned>()) {
                    errors.push_back(
                            "minProperties for object \"" + cur_obj_name + "\" at line " + std::to_string(line_num) +
                            " was not met");
                }
                if (loc.find("maxProperties") != loc.end() && cur_obj_count > loc["maxProperties"].get<unsigned>()) {
                    errors.push_back(
                            "maxProperties for object \"" + cur_obj_name + "\" at line " + std::to_string(line_num) +
                            " was exceeded");
                }
                obj_required.clear();
                extensible_required.clear();
                need_new_object_name = true;
            }
            stack.pop_back();
            last_seen_event = event;
            break;
        }
    }
    if (!stack.size()) {
        for (auto &it: root_required) {
            if (!it.second) {
                errors.push_back("Required object \"" + it.first + "\" was not provided in input file");
            }
        }
    }
}

void State::validate(json &parsed, unsigned line_num, unsigned line_index) {
    auto &loc = stack.back();

    if (loc.find("enum") != loc.end()) {
        int i;
        auto const &enum_array = loc["enum"];
        if (parsed.is_string()) {
            for (i = 0; i < enum_array.size(); i++) {
                if (icompare(enum_array[i], parsed.get<std::string>())) {
                    break;
                }
            }
            if (i == enum_array.size()) {
                errors.push_back("In object \"" + cur_obj_name + "\" at line " + std::to_string(line_num)
                                 + ": \"" + parsed.get<std::string>() + "\" was not found in the enum");
            }
        } else {
            for (i = 0; i < enum_array.size(); i++) {
                if (enum_array[i].get<int>() == parsed.get<int>()) break;
            }
            if (i == enum_array.size()) {
                errors.push_back("In object \"" + cur_obj_name + "\" at line " + std::to_string(line_num)
                                 + ": \"" + std::to_string(parsed.get<int>()) + "\" was not found in the enum");
            }
        }
    }
    else if (parsed.is_number()) {
        double val = parsed;
        if (loc.find("anyOf") != loc.end()) {
            loc = loc["anyOf"][0];
        }
        if (loc.find("minimum") != loc.end()) {
            if (loc.find("exclusiveMinimum") != loc.end() && val <= loc["minimum"].get<double>()) {
                add_error("exmin", val, line_num, prev_line_index + prev_key_len);
            } else if (val < loc["minimum"].get<double>()) {
                add_error("min", val, line_num, prev_line_index + prev_key_len);
            }
        }
        if (loc.find("maximum") != loc.end()) {
            if (loc.find("exclusiveMaximum") != loc.end() && val >= loc["maximum"].get<double>()) {
                add_error("exmax", val, line_num, prev_line_index + prev_key_len);
            } else if (val > loc["maximum"].get<double>()) {
                add_error("max", val, line_num, prev_line_index + prev_key_len);
            }
        }
        if (loc.find("type") != loc.end() && loc["type"] != "number") {
            warnings.push_back("In object \"" + cur_obj_name + "\" at line " + std::to_string(line_num)
                               + ", type == " + loc["type"].get<std::string>()
                               + " but parsed value = " + std::to_string(val));
        }
    }
    else if (parsed.is_string()) {
        if (loc.find("anyOf") != loc.end()) {
            int i;
            for (i = 0; i < loc["anyOf"].size(); i++) {
                if (loc["anyOf"][i].find("type") != loc["anyOf"][i].end() && loc["anyOf"][i]["type"] == "string") break;
            }
            if (i == loc["anyOf"].size()) {
                warnings.push_back("type == string was not found in anyOf in object \"" + cur_obj_name
                                   + "\" at line " + std::to_string(line_num));
            }
        }
        else {
            if (loc.find("type") != loc.end() && loc["type"] != "string") {
                errors.push_back("In object \"" + cur_obj_name + "\", at line " + std::to_string(line_num) +
                                 ": type needs to be string");
            }
        }
    }
}


namespace EnergyPlus {
/* A BUNCH OF GLOBALS, some useless some used... */
// Module containing the input processor routines

// MODULE INFORMATION:
//       AUTHOR         Linda K. Lawrie
//       DATE WRITTEN   August 1997
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To provide the capabilities of reading the input data dictionary
// and input file and supplying the simulation routines with the data
// contained therein.

// METHODOLOGY EMPLOYED:

// REFERENCES:
// The input syntax is designed to allow for future flexibility without
// necessitating massive (or any) changes to this code.  Two files are
// used as key elements: (1) the input data dictionary will specify the
// sections and objects that will be allowed in the actual simulation
// input file and (2) the simulation input data file will be processed
// with the data therein being supplied to the actual simulation routines.

// OTHER NOTES:

// USE STATEMENTS:
// Use statements for data only modules
// Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataStringGlobals;
    using DataGlobals::MaxNameLength;
    using DataGlobals::AutoCalculate;
    using DataGlobals::rTinyValue;
    using DataGlobals::DisplayAllWarnings;
    using DataGlobals::DisplayUnusedObjects;
    using DataGlobals::CacheIPErrorFile;
    using DataGlobals::DoingInputProcessing;
    using DataSizing::AutoSize;
    using namespace DataIPShortCuts;
    using DataSystemVariables::SortedIDD;
    using DataSystemVariables::iASCII_CR;
    using DataSystemVariables::iUnicode_end;
    using DataGlobals::DisplayInputInAudit;

// Use statements for access to subroutines in other modules

// Data
//MODULE PARAMETER DEFINITIONS
//int const ObjectDefAllocInc( 100 ); // Starting number of Objects allowed in IDD as well as the increment
//// when max is reached
//int const ANArgsDefAllocInc( 500 ); // The increment when max total args is reached
//int const SectionDefAllocInc( 20 ); // Starting number of Sections allowed in IDD as well as the increment
//// when max is reached
//int const SectionsIDFAllocInc( 20 ); // Initial number of Sections allowed in IDF as well as the increment
//// when max is reached
//int const ObjectsIDFAllocInc( 500 ); // Initial number of Objects allowed in IDF as well as the increment
//// when max is reached
//std::string::size_type const MaxObjectNameLength( MaxNameLength ); // Maximum number of characters in an Object Name
//std::string::size_type const MaxSectionNameLength( MaxNameLength ); // Maximum number of characters in a Section Name
//std::string::size_type const MaxAlphaArgLength( MaxNameLength ); // Maximum number of characters in an Alpha Argument
//std::string::size_type const MaxInputLineLength( 500 ); // Maximum number of characters in an input line (in.idf, energy+.idd)
//std::string::size_type const MaxFieldNameLength( 140 ); // Maximum number of characters in a field name string // Not used with std::string
//std::string const Blank;
    static std::string const BlankString;
//static std::string const AlphaNum( "ANan" ); // Valid indicators for Alpha or Numeric fields (A or N)
//Real64 const DefAutoSizeValue( AutoSize );
//Real64 const DefAutoCalculateValue( AutoCalculate );
    static gio::Fmt fmtLD("*");
    static gio::Fmt fmtA("(A)");

// DERIVED TYPE DEFINITIONS

// INTERFACE BLOCK SPECIFICATIONS
// na

// MODULE VARIABLE DECLARATIONS:

/* Other stuff that I want out of my way for now */
//namespace {
//	// These were static variables within different functions. They were hoisted into the namespace
//	// to facilitate easier unit testing of those functions.
//	// These are purposefully not in the header file as an extern variable. No one outside of InputProcessor should
//	// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
//	// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
//	Array1D_string AlphaArgs;
//	Array1D< Real64 > NumberArgs;
//	Array1D_bool AlphaArgsBlank;
//	Array1D_bool NumberArgsBlank;
//}

//Integer Variables for the Module
//int NumObjectDefs( 0 ); // Count of number of object definitions found in the IDD
//int NumSectionDefs( 0 ); // Count of number of section defintions found in the IDD
//int MaxObjectDefs( 0 ); // Current "max" object defs (IDD), when reached will be reallocated and new Max set
//int MaxSectionDefs( 0 ); // Current "max" section defs (IDD), when reached will be reallocated and new Max set
//int NumLines( 0 ); // Count of number of lines in IDF
//int MaxIDFRecords( 0 ); // Current "max" IDF records (lines), when reached will be reallocated and new Max set
//int NumIDFRecords( 0 ); // Count of number of IDF records
//int MaxIDFSections( 0 ); // Current "max" IDF sections (lines), when reached will be reallocated and new Max set
//int NumIDFSections( 0 ); // Count of number of IDF records
    int EchoInputFile(0); // Unit number of the file echoing the IDD and input records (eplusout.audit)
//int InputLineLength( 0 ); // Actual input line length or position of comment character
//int MaxAlphaArgsFound( 0 ); // Count of max alpha args found in the IDD
//int MaxNumericArgsFound( 0 ); // Count of max numeric args found in the IDD
//int NumAlphaArgsFound( 0 ); // Count of max alpha args found in the IDD
//int NumNumericArgsFound( 0 ); // Count of max numeric args found in the IDD
//int MaxAlphaIDFArgsFound( 0 ); // Count of max alpha args found in the IDF
//int MaxNumericIDFArgsFound( 0 ); // Count of max numeric args found in the IDF
//int MaxAlphaIDFDefArgsFound( 0 ); // Count of max alpha args found in the IDF
//int MaxNumericIDFDefArgsFound( 0 ); // Count of max numeric args found in the IDF
//int NumOutOfRangeErrorsFound( 0 ); // Count of number of "out of range" errors found
//int NumBlankReqFieldFound( 0 ); // Count of number of blank required field errors found
//int NumMiscErrorsFound( 0 ); // Count of other errors found
//int MinimumNumberOfFields( 0 ); // When ReadLine discovers a "minimum" number of fields for an object, this variable is set
//int NumObsoleteObjects( 0 ); // Number of \obsolete objects
//int TotalAuditErrors( 0 ); // Counting some warnings that go onto only the audit file
//int NumSecretObjects( 0 ); // Number of objects in "Secret Mode"
//bool ProcessingIDD( false ); // True when processing IDD, false when processing IDF
//std::ostream * echo_stream( nullptr ); // Internal stream used for input file echoing (used for performance)

//Real Variables for Module
//na

//Character Variables for Module
//std::string InputLine; // Each line can be up to MaxInputLineLength characters long
//Array1D_string ListOfSections;
//Array1D_string ListOfObjects;
//Array1D_int iListOfObjects;
//Array1D_int ObjectGotCount;
//Array1D_int ObjectStartRecord;
//std::string CurrentFieldName; // Current Field Name (IDD)
//Array1D_string ObsoleteObjectsRepNames; // Array of Replacement names for Obsolete objects
//std::string ReplacementName;

//Logical Variables for Module
//bool OverallErrorFlag( false ); // If errors found during parse of IDF, will fatal at end
//bool EchoInputLine( true ); // Usually True, if the IDD is backspaced, then is set to false, then back to true
//bool ReportRangeCheckErrors( true ); // Module level reporting logical, can be turned off from outside the module (and then
//// must be turned back on.
//bool FieldSet( false ); // Set to true when ReadInputLine has just scanned a "field"
//bool RequiredField( false ); // Set to true when ReadInputLine has determined that this field is required
//bool RetainCaseFlag( false ); // Set to true when ReadInputLine has determined that this field should retain case
//bool ObsoleteObject( false ); // Set to true when ReadInputLine has an obsolete object
//bool RequiredObject( false ); // Set to true when ReadInputLine has a required object
//bool UniqueObject( false ); // Set to true when ReadInputLine has a unique object
//bool ExtensibleObject( false ); // Set to true when ReadInputLine has an extensible object
//int ExtensibleNumFields( 0 ); // set to number when ReadInputLine has an extensible object
//Array1D_bool IDFRecordsGotten; // Denotes that this record has been "gotten" from the IDF

//Derived Types Variables

// Object Data
// Array1D< InputProcessor::ObjectsDefinition > ObjectDef; // Contains all the Valid Objects on the IDD
// Array1D< SectionsDefinition > SectionDef; // Contains all the Valid Sections on the IDD
// Array1D< FileSectionsDefinition > SectionsOnFile; // lists the sections on file (IDF)
// LineDefinition LineItem; // Description of current record
// Array1D< LineDefinition > IDFRecords; // All the objects read from the IDF
// Array1D< SecretObjects > RepObjects; // Secret Objects that could replace old ones

// MODULE SUBROUTINES:

// Functions

// Clears the global data in InputProcessor.
// Needed for unit tests, should not be normally called.

void
InputProcessor::clear_state()
{
    state.errors.clear();
    state.warnings.clear();
    jdf.clear();
    EchoInputFile = 0;
//	ObjectDef.deallocate();
//	SectionDef.deallocate();
//	SectionsOnFile.deallocate();
//	ObjectStartRecord.deallocate();
//	ObjectGotCount.deallocate();
//	ObsoleteObjectsRepNames.deallocate();
//	ListOfSections.deallocate();
//	ListOfObjects.deallocate();
//	iListOfObjects.deallocate();
//	IDFRecordsGotten.deallocate();
//	IDFRecords.deallocate();
//	RepObjects.deallocate();
//	LineItem = LineDefinition();

//	NumObjectDefs = 0;
//	NumSectionDefs = 0;
//	MaxObjectDefs = 0;
//	MaxSectionDefs = 0;
//	NumLines = 0;
//	MaxIDFRecords = 0;
//	NumIDFRecords = 0;
//	MaxIDFSections = 0;
//	NumIDFSections = 0;
//	InputLineLength = 0;
//	MaxAlphaArgsFound = 0;
//	MaxNumericArgsFound = 0;
//	NumAlphaArgsFound = 0;
//	NumNumericArgsFound = 0;
//	MaxAlphaIDFArgsFound = 0;
//	MaxNumericIDFArgsFound = 0;
//	MaxAlphaIDFDefArgsFound = 0;
//	MaxNumericIDFDefArgsFound = 0;
//	NumOutOfRangeErrorsFound = 0;
//	NumBlankReqFieldFound = 0;
//	NumMiscErrorsFound = 0;
//	MinimumNumberOfFields = 0;
//	NumObsoleteObjects = 0;
//	TotalAuditErrors = 0;
//	NumSecretObjects = 0;
//	ProcessingIDD = false;

//	InputLine = std::string();
//	CurrentFieldName = std::string();
//	ReplacementName = std::string();
//
//	OverallErrorFlag = false;
//	EchoInputLine = true;
//	ReportRangeCheckErrors = true;
//	FieldSet = false;
//	RequiredField = false;
//	RetainCaseFlag = false;
//	ObsoleteObject = false;
//	RequiredObject = false;
//	UniqueObject = false;
//	ExtensibleObject = false;
//	ExtensibleNumFields = 0;

//	AlphaArgs.deallocate();
//	NumberArgs.deallocate();
//	AlphaArgsBlank.deallocate();
//	NumberArgsBlank.deallocate();
//
//	echo_stream = nullptr;
}

	void
	InputProcessor::ProcessInput()
	{
		int write_stat;
		int read_stat;
		bool FileExists;

		EchoInputFile = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "write" ); gio::open( EchoInputFile, outputAuditFileName, flags ); write_stat = flags.ios(); }
		if ( write_stat != 0 ) {
			DisplayString( "Could not open (write) "+ outputAuditFileName + " ." );
			ShowFatalError( "ProcessInput: Could not open file " + outputAuditFileName + " for output (write)." );
		}
		echo_stream = gio::out_stream( EchoInputFile );

		{ IOFlags flags; gio::inquire( outputIperrFileName, flags ); FileExists = flags.exists(); }
		if ( FileExists ) {
			CacheIPErrorFile = GetNewUnitNumber();
			{ IOFlags flags; flags.ACTION( "read" ); gio::open( CacheIPErrorFile, outputIperrFileName, flags ); read_stat = flags.ios(); }
			if ( read_stat != 0 ) {
				ShowFatalError( "EnergyPlus: Could not open file "+outputIperrFileName+" for input (read)." );
			}
			{ IOFlags flags; flags.DISPOSE( "delete" ); gio::close( CacheIPErrorFile, flags ); }
		}
		CacheIPErrorFile = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "write" ); gio::open( CacheIPErrorFile, outputIperrFileName, flags ); write_stat = flags.ios(); }
		if ( write_stat != 0 ) {
			DisplayString( "Could not open (write) "+outputIperrFileName );
			ShowFatalError( "ProcessInput: Could not open file " + outputIperrFileName + " for output (write)." );
		}
	}

/* Totally unused ProcessInput() and other input processing and validation stuff ? */
/*
void
ProcessInput()
{
	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine processes the input for EnergyPlus.  First, the
	// input data dictionary is read and interpreted.  Using the structure
	// from the data dictionary, the actual simulation input file is read.
	// This file is processed according to the "rules" in the data dictionary
	// and stored in a local data structure which will be used during the simulation.

	// Using/Aliasing
	using SortAndStringUtilities::SetupAndSort;
	using DataOutputs::iNumberOfRecords;
	using DataOutputs::iNumberOfDefaultedFields;
	using DataOutputs::iTotalFieldsWithDefaults;
	using DataOutputs::iNumberOfAutoSizedFields;
	using DataOutputs::iTotalAutoSizableFields;
	using DataOutputs::iNumberOfAutoCalcedFields;
	using DataOutputs::iTotalAutoCalculatableFields;

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	bool FileExists; // Check variable for .idd/.idf files
	static bool ErrorsInIDD( false ); // to check for any errors flagged during data dictionary processing
	int Loop;
	int CountErr;
	int Num1;
	int Which;
	int write_stat;
	int read_stat;

	InputProcessor::InitSecretObjects();

	EchoInputFile = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( EchoInputFile, outputAuditFileName, flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		DisplayString( "Could not open (write) "+ outputAuditFileName + " ." );
		ShowFatalError( "ProcessInput: Could not open file " + outputAuditFileName + " for output (write)." );
	}
	echo_stream = gio::out_stream( EchoInputFile );

	{ IOFlags flags; gio::inquire( outputIperrFileName, flags ); FileExists = flags.exists(); }
	if ( FileExists ) {
		CacheIPErrorFile = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "read" ); gio::open( CacheIPErrorFile, outputIperrFileName, flags ); read_stat = flags.ios(); }
		if ( read_stat != 0 ) {
			ShowFatalError( "EnergyPlus: Could not open file "+outputIperrFileName+" for input (read)." );
		}
		{ IOFlags flags; flags.DISPOSE( "delete" ); gio::close( CacheIPErrorFile, flags ); }
	}
	CacheIPErrorFile = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( CacheIPErrorFile, outputIperrFileName, flags ); write_stat = flags.ios(); }
	if ( write_stat != 0 ) {
		DisplayString( "Could not open (write) "+outputIperrFileName );
		ShowFatalError( "ProcessInput: Could not open file " + outputIperrFileName + " for output (write)." );
	}

	std::ifstream idd_stream( inputIddFileName, std::ios_base::in | std::ios_base::binary );
	if ( ! idd_stream ) {
		if ( idd_stream.is_open() ) idd_stream.close();
		if ( ! gio::file_exists( inputIddFileName ) ) { // No such file
			ShowFatalError( "ProcessInput: Energy+.idd missing. Program terminates. Fullname=" + inputIddFileName );
		} else {
			ShowFatalError( "ProcessInput: Could not open file \"" + inputIddFileName + "\" for input (read)." );
		}
	}
	NumLines = 0;

	DoingInputProcessing = true;
	gio::write( EchoInputFile, fmtLD ) << " Processing Data Dictionary -- Start";
	DisplayString( "Processing Data Dictionary" );
	ProcessingIDD = true;
	ProcessDataDicFile( idd_stream, ErrorsInIDD );
	idd_stream.close();

	ListOfObjects.allocate( NumObjectDefs );
	for ( int i = 1; i <= NumObjectDefs; ++i ) ListOfObjects( i ) = ObjectDef( i ).Name;
	if ( SortedIDD ) {
		iListOfObjects.allocate( NumObjectDefs );
		SetupAndSort( ListOfObjects, iListOfObjects );
	}
	ObjectStartRecord.dimension( NumObjectDefs, 0 );
	ObjectGotCount.dimension( NumObjectDefs, 0 );

	if ( NumObjectDefs == 0 ) {
		ShowFatalError( "ProcessInput: No objects found in IDD.  Program will terminate." );
		ErrorsInIDD = true;
	}
	//  If no fatal to here, rewind EchoInputFile -- only keep processing data...
	if ( ! ErrorsInIDD ) {
		gio::rewind_truncate( EchoInputFile );
	}

	ProcessingIDD = false;
	gio::write( EchoInputFile, fmtLD ) << " Processing Data Dictionary -- Complete";

	gio::write( EchoInputFile, fmtLD ) << " Maximum number of Alpha Args=" << MaxAlphaArgsFound;
	gio::write( EchoInputFile, fmtLD ) << " Maximum number of Numeric Args=" << MaxNumericArgsFound;
	gio::write( EchoInputFile, fmtLD ) << " Number of Object Definitions=" << NumObjectDefs;
	gio::write( EchoInputFile, fmtLD ) << " Number of Section Definitions=" << NumSectionDefs;
	gio::write( EchoInputFile, fmtLD ) << " Total Number of Alpha Fields=" << NumAlphaArgsFound;
	gio::write( EchoInputFile, fmtLD ) << " Total Number of Numeric Fields=" << NumNumericArgsFound;
	gio::write( EchoInputFile, fmtLD ) << " Total Number of Fields=" << NumAlphaArgsFound + NumNumericArgsFound;

	gio::write( EchoInputFile, fmtLD ) << " Processing Input Data File -- Start";
	if ( !DisplayInputInAudit ) {
		gio::write( EchoInputFile, fmtLD ) << " Echo of input lines is off. May be activated by setting the environmental variable DISPLAYINPUTINAUDIT=YES";
	}

	std::ifstream idf_stream( inputIdfFileName, std::ios_base::in | std::ios_base::binary );
	if ( ! idf_stream ) {
		if ( idf_stream.is_open() ) idf_stream.close();
		ShowFatalError( "ProcessInput: Could not open file \"" + inputIdfFileName + "\" for input (read)." );
	}
	NumLines = 0;
	EchoInputLine = true;
	DisplayString( "Processing Input File" );
	ProcessInputDataFile( idf_stream );
	idf_stream.close();

	ListOfSections.allocate( NumSectionDefs );
	for ( int i = 1; i <= NumSectionDefs; ++i ) ListOfSections( i ) = SectionDef( i ).Name;

	cAlphaFieldNames.allocate( MaxAlphaIDFDefArgsFound );
	cAlphaArgs.allocate( MaxAlphaIDFDefArgsFound );
	lAlphaFieldBlanks.dimension( MaxAlphaIDFDefArgsFound, false );
	cNumericFieldNames.allocate( MaxNumericIDFDefArgsFound );
	rNumericArgs.dimension( MaxNumericIDFDefArgsFound, 0.0 );
	lNumericFieldBlanks.dimension( MaxNumericIDFDefArgsFound, false );

	IDFRecordsGotten.dimension( NumIDFRecords, false );

	gio::write( EchoInputFile, fmtLD ) << " Processing Input Data File -- Complete";
	//   WRITE(EchoInputFile,*) ' Number of IDF "Lines"=',NumIDFRecords
	gio::write( EchoInputFile, fmtLD ) << " Maximum number of Alpha IDF Args=" << MaxAlphaIDFArgsFound;
	gio::write( EchoInputFile, fmtLD ) << " Maximum number of Numeric IDF Args=" << MaxNumericIDFArgsFound;
	GetIDFRecordsStats( iNumberOfRecords, iNumberOfDefaultedFields, iTotalFieldsWithDefaults, iNumberOfAutoSizedFields, iTotalAutoSizableFields, iNumberOfAutoCalcedFields, iTotalAutoCalculatableFields );
	gio::write( EchoInputFile, fmtLD ) << " Number of IDF \"Lines\"=" << iNumberOfRecords;
	gio::write( EchoInputFile, fmtLD ) << " Number of Defaulted Fields=" << iNumberOfDefaultedFields;
	gio::write( EchoInputFile, fmtLD ) << " Number of Fields with Defaults=" << iTotalFieldsWithDefaults;
	gio::write( EchoInputFile, fmtLD ) << " Number of Autosized Fields=" << iNumberOfAutoSizedFields;
	gio::write( EchoInputFile, fmtLD ) << " Number of Autosizable Fields =" << iTotalAutoSizableFields;
	gio::write( EchoInputFile, fmtLD ) << " Number of Autocalculated Fields=" << iNumberOfAutoCalcedFields;
	gio::write( EchoInputFile, fmtLD ) << " Number of Autocalculatable Fields =" << iTotalAutoCalculatableFields;

	CountErr = 0;
	for ( Loop = 1; Loop <= NumIDFSections; ++Loop ) {
		if ( SectionsOnFile( Loop ).LastRecord != 0 ) continue;
		if ( equali( SectionsOnFile( Loop ).Name, "REPORT VARIABLE DICTIONARY" ) ) continue;
		if ( CountErr == 0 ) {
			ShowSevereError( "IP: Potential errors in IDF processing -- see .audit file for details." );
			gio::write( EchoInputFile, fmtA ) << " Potential errors in IDF processing:";
		}
		++CountErr;
		Which = SectionsOnFile( Loop ).FirstRecord;
		if ( Which > 0 ) {
			if ( SortedIDD ) {
				Num1 = FindItemInSortedList( IDFRecords( Which ).Name, ListOfObjects, NumObjectDefs );
				if ( Num1 != 0 ) Num1 = iListOfObjects( Num1 );
			} else {
				Num1 = FindItemInList( IDFRecords( Which ).Name, ListOfObjects, NumObjectDefs );
			}
			if ( ObjectDef( Num1 ).NameAlpha1 && IDFRecords( Which ).NumAlphas > 0 ) {
				gio::write( EchoInputFile, fmtA ) << " Potential \"semi-colon\" misplacement=" + SectionsOnFile( Loop ).Name + ", at about line number=[" + IPTrimSigDigits( SectionsOnFile( Loop ).FirstLineNo ) + "], Object Type Preceding=" + IDFRecords( Which ).Name + ", Object Name=" + IDFRecords( Which ).Alphas( 1 );
			} else {
				gio::write( EchoInputFile, fmtA ) << " Potential \"semi-colon\" misplacement=" + SectionsOnFile( Loop ).Name + ", at about line number=[" + IPTrimSigDigits( SectionsOnFile( Loop ).FirstLineNo ) + "], Object Type Preceding=" + IDFRecords( Which ).Name + ", Name field not recorded for Object.";
			}
		} else {
			gio::write( EchoInputFile, fmtA ) << " Potential \"semi-colon\" misplacement=" + SectionsOnFile( Loop ).Name + ", at about line number=[" + IPTrimSigDigits( SectionsOnFile( Loop ).FirstLineNo ) + "], No prior Objects.";
		}
	}

	if ( NumIDFRecords == 0 ) {
		ShowSevereError( "IP: The IDF file has no records." );
		++NumMiscErrorsFound;
	}

	// Check for required objects
	for ( Loop = 1; Loop <= NumObjectDefs; ++Loop ) {
		if ( ! ObjectDef( Loop ).RequiredObject ) continue;
		if ( ObjectDef( Loop ).NumFound > 0 ) continue;
		ShowSevereError( "IP: Required Object=\"" + ObjectDef( Loop ).Name + "\" not found in IDF." );
		++NumMiscErrorsFound;
	}

	if ( TotalAuditErrors > 0 ) {
		ShowWarningError( "IP: Note -- Some missing fields have been filled with defaults. See the audit output file for details." );
	}

	if ( NumOutOfRangeErrorsFound > 0 ) {
		ShowSevereError( "IP: Out of \"range\" values found in input" );
	}

	if ( NumBlankReqFieldFound > 0 ) {
		ShowSevereError( "IP: Blank \"required\" fields found in input" );
	}

	if ( NumMiscErrorsFound > 0 ) {
		ShowSevereError( "IP: Other miscellaneous errors found in input" );
	}

	if ( OverallErrorFlag ) {
		if (IDDVerString.find(MatchVersion) == std::string::npos) {
			ShowSevereError("IP: Possible incorrect IDD File");
			ShowContinueError(IDDVerString + " not the same as expected =\"" + MatchVersion + "\"");
		}
		for ( Loop = 1; Loop <= NumIDFRecords; ++Loop ) {
			if ( InputProcessor::SameString( IDFRecords( Loop ).Name, "Version" ) ) {
				std::string::size_type const lenVer( len( MatchVersion ) );
				if ( ( lenVer > 0 ) && ( MatchVersion[ lenVer - 1 ] == '0' ) ) {
					Which = static_cast< int >( index( IDFRecords( Loop ).Alphas( 1 ).substr( 0, lenVer - 2 ), MatchVersion.substr( 0, lenVer - 2 ) ) );
				} else {
					Which = static_cast< int >( index( IDFRecords( Loop ).Alphas( 1 ), MatchVersion ) );
				}
				if ( Which != 0 ) {
					ShowContinueError( "Version in IDF=\"" + IDFRecords( Loop ).Alphas( 1 ) + "\" not the same as expected=\"" + MatchVersion + "\"" );
				}
				break;
			}
		}
		ShowContinueError( "Possible Invalid Numerics or other problems" );
		// Fatal error will now occur during post IP processing check in Simulation manager.
	}

}

void
ProcessDataDicFile(
		std::istream & idd_stream,
		bool & ErrorsFound // set to true if any errors flagged during IDD processing
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   August 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine processes data dictionary file for EnergyPlus.
	// The structure of the sections and objects are stored in derived
	// types (SectionDefs and ObjectDefs)

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	bool EndofFile( false ); // True when End of File has been reached (IDD or IDF)
	bool BlankLine( false );
	std::string::size_type Pos; // Test of scanning position on the current input line

	MaxSectionDefs = SectionDefAllocInc;
	MaxObjectDefs = ObjectDefAllocInc;

	SectionDef.allocate( MaxSectionDefs );
	ObjectDef.allocate( MaxObjectDefs );

	NumObjectDefs = 0;
	NumSectionDefs = 0;

	// Get version
	cross_platform_get_line( idd_stream, InputLine );
	std::string::size_type const len_line( InputLine.length() );
	if ( idd_stream && ( len_line > 0 ) ) {
		if ( int( InputLine[ len_line - 1 ] ) == iUnicode_end ) {
			ShowSevereError( "ProcessInput: \"Energy+.idd\" appears to be a Unicode or binary file." );
			ShowContinueError( "...This file cannot be read by this program. Please save as PC or Unix file and try again" );
			ShowFatalError( "Program terminates due to previous condition." );
		}
		if ( has( InputLine, "!IDD_Version" ) ) {
			IDDVerString = InputLine.substr( 1, len( InputLine ) - 1 );
		}
	}
	if ( idd_stream ) idd_stream.seekg( 0, std::ios::beg );

	while ( ! EndofFile ) {
		ReadInputLine( idd_stream, Pos, BlankLine, EndofFile );
		if ( BlankLine || EndofFile ) continue;
		Pos = scan( InputLine, ",;" );
		if ( Pos != std::string::npos ) {
			if ( InputLine[ Pos ] == ';' ) {
				AddSectionDef( InputLine.substr( 0, Pos ), ErrorsFound );
				if ( NumSectionDefs == MaxSectionDefs ) {
					SectionDef.redimension( MaxSectionDefs += SectionDefAllocInc );
				}
			} else {
				AddObjectDefandParse( idd_stream, InputLine.substr( 0, Pos ), Pos, EndofFile, ErrorsFound );
				if ( NumObjectDefs == MaxObjectDefs ) {
					ObjectDef.redimension( MaxObjectDefs += ObjectDefAllocInc );
				}
			}
		} else {
			ShowSevereError( "IP: IDD line~" + IPTrimSigDigits( NumLines ) + " , or ; expected on this line", EchoInputFile );
			ErrorsFound = true;
		}
	}

}

void
AddSectionDef(
		std::string const & ProposedSection, // Proposed Section to be added
		bool & ErrorsFound // set to true if errors found here
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   August 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine adds a new section to SectionDefs.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	bool errFlag; // Local error flag.  When True, Proposed Section is not added to global list

	std::string const SqueezedSection( MakeUPPERCase( stripped( ProposedSection ) ) ); // Input Argument, Left-Justified and Uppercase
	if ( len( stripped( ProposedSection ) ) > static_cast< std::string::size_type >( MaxSectionNameLength ) ) {
		ShowWarningError( "IP: Section length exceeds maximum, will be truncated=" + ProposedSection, EchoInputFile );
		ShowContinueError( "Will be processed as Section=" + SqueezedSection, EchoInputFile );
		ErrorsFound = true;
	}
	errFlag = false;

	if ( ! SqueezedSection.empty() ) {
		if ( FindItemInList( SqueezedSection, SectionDef ) > 0 ) {
			ShowSevereError( "IP: Already a Section called " + SqueezedSection + ". This definition ignored.", EchoInputFile );
			// Error Condition
			errFlag = true;
			ErrorsFound = true;
		}
	} else {
		ShowSevereError( "IP: Blank Sections not allowed.  Review " + outputAuditFileName + " file.", EchoInputFile );
		errFlag = true;
		ErrorsFound = true;
	}

	if ( ! errFlag ) {
		++NumSectionDefs;
		SectionDef( NumSectionDefs ).Name = SqueezedSection;
		SectionDef( NumSectionDefs ).NumFound = 0;
	}

}

void
AddObjectDefandParse(
		std::istream & idd_stream,
		std::string const & ProposedObject, // Proposed Object to Add
		std::string::size_type & CurPos, // Current position (initially at first ',') of InputLine
		bool & EndofFile, // End of File marker
		bool & ErrorsFound // set to true if errors found here
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   August 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine processes data dictionary file for EnergyPlus.
	// The structure of the sections and objects are stored in derived
	// types (SectionDefs and ObjectDefs)

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	std::string SqueezedObject; // Input Object, Left Justified, UpperCase
	int Count; // Count on arguments, loop
	std::string::size_type Pos; // Position scanning variable
	bool EndofObjectDef; // Set to true when ; has been found
	bool errFlag; // Local Error condition flag, when true, object not added to Global list
	char TargetChar; // Single character scanned to test for current field type (A or N)
	bool BlankLine; // True when this line is "blank" (may have comment characters as first character on line)
	static Array1D_bool AlphaOrNumeric; // Array of argument designations, True is Alpha, False is numeric, saved in ObjectDef when done
	static Array1D_bool RequiredFields; // Array of argument required fields
	static Array1D_bool AlphRetainCase; // Array of argument for retain case
	static Array1D_string AlphFieldChecks; // Array with alpha field names
	static Array1D_string AlphFieldDefaults; // Array with alpha field defaults
	bool MinMax; // Set to true when MinMax field has been found by ReadInputLine
	bool Default; // Set to true when Default field has been found by ReadInputLine
	bool AutoSize; // Set to true when Autosizable field has been found by ReadInputLine
	bool AutoCalculate; // Set to true when AutoCalculatable field has been found by ReadInputLine
	std::string MinMaxString; // Set from ReadInputLine
	std::string AlphDefaultString;
	int WhichMinMax; // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
	Real64 Value; // Value returned by ReadInputLine (either min, max, default, autosize or autocalculate)
	bool MinMaxError; // Used to see if min, max, defaults have been set appropriately (True if error)
	static int MaxANArgs( 7700 ); // Current count of Max args to object  (9/2010)
	bool ErrorsFoundFlag;
	static int PrevSizeNumNumeric( -1 );
	static int PrevCount( -1 );
	static int PrevSizeNumAlpha( -1 );

	// Object Data
	static Array1D< RangeCheckDef > NumRangeChecks; // Structure for Range Check, Defaults of numeric fields
	static Array1D< RangeCheckDef > TempChecks; // Structure (ref: NumRangeChecks) for re-allocation procedure

	if ( ! allocated( AlphaOrNumeric ) ) {
		AlphaOrNumeric.allocate( {0,MaxANArgs} );
		RequiredFields.allocate( {0,MaxANArgs} );
		AlphRetainCase.allocate( {0,MaxANArgs} );
		NumRangeChecks.allocate( MaxANArgs );
		AlphFieldChecks.allocate( MaxANArgs );
		AlphFieldDefaults.allocate( MaxANArgs );
		ObsoleteObjectsRepNames.allocate( 0 );
	}

	SqueezedObject = MakeUPPERCase( stripped( ProposedObject ) );
	if ( len( SqueezedObject ) > static_cast< std::string::size_type >( MaxObjectNameLength ) ) {
		ShowWarningError( "IP: Object length exceeds maximum, will be truncated=" + ProposedObject, EchoInputFile );
		ShowContinueError( "Will be processed as Object=" + SqueezedObject, EchoInputFile );
		ErrorsFound = true;
	}

	// Start of Object parse, set object level items
	errFlag = false;
	ErrorsFoundFlag = false;
	MinimumNumberOfFields = 0;
	ObsoleteObject = false;
	UniqueObject = false;
	RequiredObject = false;
	ExtensibleObject = false;
	ExtensibleNumFields = 0;
	MinMax = false;
	Default = false;
	AutoSize = false;
	AutoCalculate = false;
	WhichMinMax = 0;

	if ( ! SqueezedObject.empty() ) {
		if ( FindItemInList( SqueezedObject, ObjectDef ) > 0 ) {
			ShowSevereError( "IP: Already an Object called " + SqueezedObject + ". This definition ignored.", EchoInputFile );
			// Error Condition
			errFlag = true;
			// Rest of Object has to be processed. Error condition will be caught
			// at end
			ErrorsFound = true;
		}
	} else {
		errFlag = true;
		ErrorsFound = true;
	}

	++NumObjectDefs;
	ObjectDef( NumObjectDefs ).Name = SqueezedObject;
	ObjectDef( NumObjectDefs ).NumParams = 0;
	ObjectDef( NumObjectDefs ).NumAlpha = 0;
	ObjectDef( NumObjectDefs ).NumNumeric = 0;
	ObjectDef( NumObjectDefs ).NumFound = 0;
	ObjectDef( NumObjectDefs ).MinNumFields = 0;
	ObjectDef( NumObjectDefs ).NameAlpha1 = false;
	ObjectDef( NumObjectDefs ).ObsPtr = 0;
	ObjectDef( NumObjectDefs ).UniqueObject = false;
	ObjectDef( NumObjectDefs ).RequiredObject = false;
	ObjectDef( NumObjectDefs ).ExtensibleObject = false;
	ObjectDef( NumObjectDefs ).ExtensibleNum = 0;

	if ( PrevCount == -1 ) {
		PrevCount = MaxANArgs;
	}

	AlphaOrNumeric( {0,PrevCount} ) = true;
	RequiredFields( {0,PrevCount} ) = false;
	AlphRetainCase( {0,PrevCount} ) = false;

	if ( PrevSizeNumAlpha == -1 ) {
		PrevSizeNumAlpha = MaxANArgs;
	}

	AlphFieldChecks( {1,PrevSizeNumAlpha} ) = BlankString;
	AlphFieldDefaults( {1,PrevSizeNumAlpha} ) = BlankString;

	if ( PrevSizeNumNumeric == -1 ) {
		PrevSizeNumNumeric = MaxANArgs;
	}

	//clear only portion of NumRangeChecks array used in the previous
	//call to reduce computation time to clear this large array.
	for ( int i = 1; i <= PrevSizeNumNumeric; ++i ) {
		auto & numRangeCheck( NumRangeChecks( i ) );
		numRangeCheck.MinMaxChk = false;
		numRangeCheck.WhichMinMax( 1 ) = 0;
		numRangeCheck.WhichMinMax( 2 ) = 0;
		numRangeCheck.MinMaxString( 1 ).clear();
		numRangeCheck.MinMaxString( 2 ).clear();
		numRangeCheck.MinMaxValue( 1 ) = 0.0;
		numRangeCheck.MinMaxValue( 2 ) = 0.0;
		numRangeCheck.Default = 0.0;
		numRangeCheck.DefaultChk = false;
		numRangeCheck.DefAutoSize = false;
		numRangeCheck.DefAutoCalculate = false;
		numRangeCheck.FieldNumber = 0;
		numRangeCheck.FieldName = BlankString;
		numRangeCheck.AutoSizable = false;
		numRangeCheck.AutoSizeValue = DefAutoSizeValue;
		numRangeCheck.AutoCalculatable = false;
		numRangeCheck.AutoCalculateValue = DefAutoCalculateValue;
	}

	Count = 0;
	EndofObjectDef = false;
	// Parse rest of Object Definition

	while ( ! EndofFile && ! EndofObjectDef ) {

		if ( CurPos < static_cast< std::string::size_type >( InputLineLength ) ) {
			Pos = scan( InputLine.substr( CurPos, InputLineLength - CurPos ), AlphaNum );
			if ( Pos != std::string::npos ) {

				++Count;
				RequiredField = false;
				RetainCaseFlag = false;

				if ( Count > MaxANArgs ) { // Reallocation
					int const newANArgs( MaxANArgs + ANArgsDefAllocInc );
					AlphaOrNumeric.redimension( {0,newANArgs}, false );
					RequiredFields.redimension( {0,newANArgs}, false );
					AlphRetainCase.redimension( {0,newANArgs}, false );
					NumRangeChecks.redimension( newANArgs );
					AlphFieldChecks.redimension( newANArgs );
					AlphFieldDefaults.redimension( newANArgs );
					MaxANArgs = newANArgs;
				}

				TargetChar = InputLine[ CurPos + Pos ];

				if ( TargetChar == 'A' || TargetChar == 'a' ) {
					AlphaOrNumeric( Count ) = true;
					++ObjectDef( NumObjectDefs ).NumAlpha;
					if ( FieldSet ) AlphFieldChecks( ObjectDef( NumObjectDefs ).NumAlpha ) = CurrentFieldName;
					if ( ObjectDef( NumObjectDefs ).NumAlpha == 1 ) {
						if ( hasi( CurrentFieldName, "NAME" ) ) ObjectDef( NumObjectDefs ).NameAlpha1 = true;
					}
				} else {
					AlphaOrNumeric( Count ) = false;
					++ObjectDef( NumObjectDefs ).NumNumeric;
					if ( FieldSet ) NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).FieldName = CurrentFieldName;
				}

			} else {
				ReadInputLine( idd_stream, CurPos, BlankLine, EndofFile, MinMax, WhichMinMax, MinMaxString, Value, Default, AlphDefaultString, AutoSize, AutoCalculate, RetainCaseFlag, ErrorsFoundFlag );
				if ( ! AlphaOrNumeric( Count ) ) {
					// only record for numeric fields
					if ( MinMax ) {
						NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxChk = true;
						NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).FieldNumber = Count;
						if ( WhichMinMax <= 2 ) { //=0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
							NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).WhichMinMax( 1 ) = WhichMinMax;
							NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxString( 1 ) = MinMaxString;
							NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxValue( 1 ) = Value;
						} else {
							NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).WhichMinMax( 2 ) = WhichMinMax;
							NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxString( 2 ) = MinMaxString;
							NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxValue( 2 ) = Value;
						}
					} // End Min/Max
					if ( Default ) {
						NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).DefaultChk = true;
						NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).Default = Value;
						if ( AlphDefaultString == "AUTOSIZE" ) NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).DefAutoSize = true;
						if ( AlphDefaultString == "AUTOCALCULATE" ) NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).DefAutoCalculate = true;
					}
					if ( AutoSize ) {
						NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoSizable = true;
						NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoSizeValue = Value;
					}
					if ( AutoCalculate ) {
						NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoCalculatable = true;
						NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoCalculateValue = Value;
					}
				} else { // Alpha Field
					if ( Default ) {
						AlphFieldDefaults( ObjectDef( NumObjectDefs ).NumAlpha ) = AlphDefaultString;
					}
				}
				if ( ErrorsFoundFlag ) {
					errFlag = true;
					ErrorsFoundFlag = false;
				}
				if ( RequiredField ) {
					RequiredFields( Count ) = true;
					MinimumNumberOfFields = max( Count, MinimumNumberOfFields );
				}
				if ( RetainCaseFlag ) {
					AlphRetainCase( Count ) = true;
				}
				continue;
			}

			//  For the moment dont care about descriptions on each object
			if ( CurPos < static_cast< std::string::size_type >( InputLineLength ) ) {
				CurPos += Pos + 1;
				if ( CurPos < InputLine.length() ) {
					Pos = scan( InputLine.substr( CurPos, InputLineLength - CurPos ), ",;" );
				} else {
					Pos = std::string::npos;
				}
				if ( Pos == std::string::npos ) {
					ShowSevereError( "IP: IDD line~" + IPTrimSigDigits( NumLines ) + " , or ; expected on this line,position=\"" + InputLine.substr( CurPos ) + "\"", EchoInputFile );
					errFlag = true;
					ErrorsFound = true;
				}
				if ( InputLine[ InputLineLength - 1 ] != '\\' ) {
					ShowWarningError( "IP: IDD line~" + IPTrimSigDigits( NumLines ) + " \\ expected on this line", EchoInputFile );
				}
			} else {
				ReadInputLine( idd_stream, CurPos, BlankLine, EndofFile );
				if ( BlankLine || EndofFile ) continue;
				Pos = scan( InputLine.substr( CurPos, InputLineLength - CurPos ), ",;" );
			}
		} else {
			ReadInputLine( idd_stream, CurPos, BlankLine, EndofFile );
			continue;
		}

		if ( Pos == std::string::npos ) {
			// must be time to read another line
			ReadInputLine( idd_stream, CurPos, BlankLine, EndofFile );
			if ( BlankLine || EndofFile ) continue;
		} else {
			if ( InputLine[ CurPos + Pos ] == ';' ) {
				EndofObjectDef = true;
			}
			CurPos += Pos;
		}

	}

	// Reached end of object def but there may still be more \ lines to parse....
	// Goes until next object is encountered ("not blankline") or end of idd file
	// If last object is not numeric, then exit immediately....
	BlankLine = true;
	while ( BlankLine && ! EndofFile ) {
		// It's a numeric object as last one...
		ReadInputLine( idd_stream, CurPos, BlankLine, EndofFile, MinMax, WhichMinMax, MinMaxString, Value, Default, AlphDefaultString, AutoSize, AutoCalculate, RetainCaseFlag, ErrorsFoundFlag );
		if ( MinMax ) {
			NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxChk = true;
			NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).FieldNumber = Count;
			if ( WhichMinMax <= 2 ) { //=0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).WhichMinMax( 1 ) = WhichMinMax;
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxString( 1 ) = MinMaxString;
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxValue( 1 ) = Value;
			} else {
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).WhichMinMax( 2 ) = WhichMinMax;
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxString( 2 ) = MinMaxString;
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxValue( 2 ) = Value;
			}
		}
		if ( Default && ! AlphaOrNumeric( Count ) ) {
			NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).DefaultChk = true;
			NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).Default = Value;
			if ( AlphDefaultString == "AUTOSIZE" ) NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).DefAutoSize = true;
			if ( AlphDefaultString == "AUTOCALCULATE" ) NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).DefAutoCalculate = true;
		} else if ( Default && AlphaOrNumeric( Count ) ) {
			AlphFieldDefaults( ObjectDef( NumObjectDefs ).NumAlpha ) = AlphDefaultString;
		}
		if ( AutoSize ) {
			NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoSizable = true;
			NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoSizeValue = Value;
		}
		if ( AutoCalculate ) {
			NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoCalculatable = true;
			NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoCalculateValue = Value;
		}
		if ( ErrorsFoundFlag ) {
			errFlag = true;
			ErrorsFoundFlag = false;
		}
	}

	if ( ! BlankLine ) {
		Backspace( idd_stream );
		EchoInputLine = false;
	}

	if ( RequiredField ) {
		RequiredFields( Count ) = true;
		MinimumNumberOfFields = max( Count, MinimumNumberOfFields );
	}
	if ( RetainCaseFlag ) {
		AlphRetainCase( Count ) = true;
	}

	ObjectDef( NumObjectDefs ).NumParams = Count; // Also the total of ObjectDef(..)%NumAlpha+ObjectDef(..)%NumNumeric
	ObjectDef( NumObjectDefs ).MinNumFields = MinimumNumberOfFields;
	if ( ObsoleteObject ) {
		ObsoleteObjectsRepNames.redimension( ++NumObsoleteObjects );
		ObsoleteObjectsRepNames( NumObsoleteObjects ) = ReplacementName;
		ObjectDef( NumObjectDefs ).ObsPtr = NumObsoleteObjects;
	}
	if ( RequiredObject ) {
		ObjectDef( NumObjectDefs ).RequiredObject = true;
	}
	if ( UniqueObject ) {
		ObjectDef( NumObjectDefs ).UniqueObject = true;
	}
	if ( ExtensibleObject ) {
		ObjectDef( NumObjectDefs ).ExtensibleObject = true;
		ObjectDef( NumObjectDefs ).ExtensibleNum = ExtensibleNumFields;
	}

	NumAlphaArgsFound += ObjectDef( NumObjectDefs ).NumAlpha;
	MaxAlphaArgsFound = max( MaxAlphaArgsFound, ObjectDef( NumObjectDefs ).NumAlpha );
	NumNumericArgsFound += ObjectDef( NumObjectDefs ).NumNumeric;
	MaxNumericArgsFound = max( MaxNumericArgsFound, ObjectDef( NumObjectDefs ).NumNumeric );
	ObjectDef( NumObjectDefs ).AlphaOrNumeric.allocate( Count );
	ObjectDef( NumObjectDefs ).AlphaOrNumeric = AlphaOrNumeric( {1,Count} );
	ObjectDef( NumObjectDefs ).AlphRetainCase.allocate( Count );
	ObjectDef( NumObjectDefs ).AlphRetainCase = AlphRetainCase( {1,Count} );
	PrevCount = Count;
	ObjectDef( NumObjectDefs ).NumRangeChks.allocate( ObjectDef( NumObjectDefs ).NumNumeric );
	if ( ObjectDef( NumObjectDefs ).NumNumeric > 0 ) {
		ObjectDef( NumObjectDefs ).NumRangeChks = NumRangeChecks( {1,ObjectDef( NumObjectDefs ).NumNumeric} );
	}
	PrevSizeNumNumeric = ObjectDef( NumObjectDefs ).NumNumeric; //used to clear only portion of NumRangeChecks array
	ObjectDef( NumObjectDefs ).AlphFieldChks.allocate( ObjectDef( NumObjectDefs ).NumAlpha );
	if ( ObjectDef( NumObjectDefs ).NumAlpha > 0 ) {
		ObjectDef( NumObjectDefs ).AlphFieldChks = AlphFieldChecks( {1,ObjectDef( NumObjectDefs ).NumAlpha} );
	}
	ObjectDef( NumObjectDefs ).AlphFieldDefs.allocate( ObjectDef( NumObjectDefs ).NumAlpha );
	if ( ObjectDef( NumObjectDefs ).NumAlpha > 0 ) {
		ObjectDef( NumObjectDefs ).AlphFieldDefs = AlphFieldDefaults( {1,ObjectDef( NumObjectDefs ).NumAlpha} );
	}
	PrevSizeNumAlpha = ObjectDef( NumObjectDefs ).NumAlpha;
	ObjectDef( NumObjectDefs ).ReqField.allocate( Count );
	ObjectDef( NumObjectDefs ).ReqField = RequiredFields( {1,Count} );
	for ( Count = 1; Count <= ObjectDef( NumObjectDefs ).NumNumeric; ++Count ) {
		if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxChk ) {
			// Checking MinMax Range (min vs. max and vice versa)
			MinMaxError = false;
			// check min against max
			if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 1 ) {
				// min
				Value = ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 );
				if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 3 ) {
					if ( Value > ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
				} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 4 ) {
					if ( Value == ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
				}
			} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 2 ) {
				// min>
				Value = ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) + rTinyValue; // infintesimally bigger than min
				if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 3 ) {
					if ( Value > ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
				} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 4 ) {
					if ( Value == ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
				}
			}
			// check max against min
			if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 3 ) {
				// max
				Value = ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 );
				// Check max value against min
				if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 1 ) {
					if ( Value < ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
				} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 2 ) {
					if ( Value == ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
				}
			} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 4 ) {
				// max<
				Value = ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) - rTinyValue; // infintesimally bigger than min
				if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 1 ) {
					if ( Value < ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
				} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 2 ) {
					if ( Value == ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
				}
			}
			// check if error condition
			if ( MinMaxError ) {
				//  Error stated min is not in range with stated max
				MinMaxString = IPTrimSigDigits( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).FieldNumber );
				ShowSevereError( "IP: IDD: Field #" + MinMaxString + " conflict in Min/Max specifications/values, in class=" + ObjectDef( NumObjectDefs ).Name, EchoInputFile );
				errFlag = true;
			}
		}
		if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).DefaultChk ) {
			// Check Default against MinMaxRange
			//  Don't check when default is autosize...
			if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).AutoSizable && ObjectDef( NumObjectDefs ).NumRangeChks( Count ).DefAutoSize ) continue;
			if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).AutoCalculatable && ObjectDef( NumObjectDefs ).NumRangeChks( Count ).DefAutoCalculate ) continue;
			MinMaxError = false;
			Value = ObjectDef( NumObjectDefs ).NumRangeChks( Count ).Default;
			if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 1 ) {
				if ( Value < ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
			} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 2 ) {
				if ( Value <= ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
			}
			if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 3 ) {
				if ( Value > ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
			} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 4 ) {
				if ( Value >= ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
			}
			if ( MinMaxError ) {
				//  Error stated default is not in min/max range
				MinMaxString = IPTrimSigDigits( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).FieldNumber );
				ShowSevereError( "IP: IDD: Field #" + MinMaxString + " default is invalid for Min/Max values, in class=" + ObjectDef( NumObjectDefs ).Name, EchoInputFile );
				errFlag = true;
			}
		}
	}

	if ( errFlag ) {
		ShowContinueError( "IP: Errors occured in ObjectDefinition for Class=" + ObjectDef( NumObjectDefs ).Name + ", Object not available for IDF processing.", EchoInputFile );
		ObjectDef( NumObjectDefs ).AlphaOrNumeric.deallocate();
		ObjectDef( NumObjectDefs ).NumRangeChks.deallocate();
		ObjectDef( NumObjectDefs ).AlphFieldChks.deallocate();
		ObjectDef( NumObjectDefs ).AlphFieldDefs.deallocate();
		ObjectDef( NumObjectDefs ).ReqField.deallocate();
		ObjectDef( NumObjectDefs ).AlphRetainCase.deallocate();
		--NumObjectDefs;
		ErrorsFound = true;
	}

}

void
ProcessInputDataFile( std::istream & idf_stream )
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   August 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine processes input data file for EnergyPlus.  Each "record" is
	// parsed into the LineItem data structure and, if okay, put into the
	// IDFRecords data structure.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Locals
	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	bool EndofFile( false );
	bool BlankLine( false );
	std::string::size_type Pos;

	MaxIDFRecords = ObjectsIDFAllocInc;
	NumIDFRecords = 0;
	MaxIDFSections = SectionsIDFAllocInc;
	NumIDFSections = 0;

	SectionsOnFile.allocate( MaxIDFSections );
	IDFRecords.allocate( MaxIDFRecords );
	LineItem.Numbers.allocate( MaxNumericArgsFound );
	LineItem.NumBlank.allocate( MaxNumericArgsFound );
	LineItem.Alphas.allocate( MaxAlphaArgsFound );
	LineItem.AlphBlank.allocate( MaxAlphaArgsFound );

	// Check file
	cross_platform_get_line( idf_stream, InputLine );
	std::string::size_type const len_line( InputLine.length() );
	if ( idf_stream && ( len_line > 0 ) ) {
		if ( int( InputLine[ len_line - 1 ] ) == iUnicode_end ) {
			ShowSevereError( "ProcessInput: \"in.idf\" appears to be a Unicode or binary file." );
			ShowContinueError( "...This file cannot be read by this program. Please save as PC or Unix file and try again" );
			ShowFatalError( "Program terminates due to previous condition." );
		}
	}
	if ( idf_stream ) idf_stream.seekg( 0, std::ios::beg );

	while ( ! EndofFile ) {
		ReadInputLine( idf_stream, Pos, BlankLine, EndofFile );
		if ( BlankLine || EndofFile ) continue;
		Pos = scan( InputLine, ",;" );
		if ( Pos != std::string::npos ) {
			if ( InputLine[ Pos ] == ';' ) {
				ValidateSection( InputLine.substr( 0, Pos ), NumLines );
				if ( NumIDFSections == MaxIDFSections ) {
					SectionsOnFile.redimension( MaxIDFSections += SectionsIDFAllocInc );
				}
			} else {
				ValidateObjectandParse( idf_stream, InputLine.substr( 0, Pos ), Pos, EndofFile );
				if ( NumIDFRecords == MaxIDFRecords ) {
					IDFRecords.redimension( MaxIDFRecords += ObjectsIDFAllocInc );
				}
			}
		} else { // Error condition, no , or ; on first line
			ShowMessage( "IP: IDF Line~" + IPTrimSigDigits( NumLines ) + ' ' + InputLine );
			ShowSevereError( ", or ; expected on this line", EchoInputFile );
		}
	}

	//   IF (NumIDFSections > 0) THEN
	//     SectionsOnFile(NumIDFSections)%LastRecord=NumIDFRecords
	//   ENDIF

	if ( NumIDFRecords > 0 ) {
		for ( int i = 1; i <= NumObjectDefs; ++i ) {
			if ( ObjectDef( i ).RequiredObject && ObjectDef( i ).NumFound == 0 ) {
				ShowSevereError( "IP: No items found for Required Object=" + ObjectDef( i ).Name );
				++NumMiscErrorsFound;
			}
		}
	}

}

void
ValidateSection(
		std::string const & ProposedSection,
		int const LineNo
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine validates the section from the input data file
	// with the list of objects from the data dictionary file.

	// METHODOLOGY EMPLOYED:
	// A "squeezed" string is formed and checked against the list of
	// sections.

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	std::string SqueezedSection;
	int Found;
	int OFound;

	SqueezedSection = MakeUPPERCase( stripped( ProposedSection ) );
	if ( len( stripped( ProposedSection ) ) > static_cast< std::string::size_type >( MaxSectionNameLength ) ) {
		ShowWarningError( "IP: Section length exceeds maximum, will be truncated=" + ProposedSection, EchoInputFile );
		ShowContinueError( "Will be processed as Section=" + SqueezedSection, EchoInputFile );
	}
	if ( ! has_prefix( SqueezedSection, "END" ) ) {
		Found = FindItemInList( SqueezedSection, SectionDef );
		if ( Found == 0 ) {
			// Make sure this Section not an object name
			if ( SortedIDD ) {
				OFound = FindItemInSortedList( SqueezedSection, ListOfObjects, NumObjectDefs );
				if ( OFound != 0 ) OFound = iListOfObjects( OFound );
			} else {
				OFound = FindItemInList( SqueezedSection, ListOfObjects, NumObjectDefs );
			}
			if ( OFound != 0 ) {
				AddRecordFromSection( OFound );
			} else if ( NumSectionDefs == MaxSectionDefs ) {
				SectionDef.redimension( MaxSectionDefs += SectionDefAllocInc );
			}
			++NumSectionDefs;
			SectionDef( NumSectionDefs ).Name = SqueezedSection;
			SectionDef( NumSectionDefs ).NumFound = 1;
			// Add to "Sections on file" if appropriate
			if ( ! ProcessingIDD ) {
				++NumIDFSections;
				SectionsOnFile( NumIDFSections ).Name = SqueezedSection;
				SectionsOnFile( NumIDFSections ).FirstRecord = NumIDFRecords;
				SectionsOnFile( NumIDFSections ).FirstLineNo = LineNo;
			}
		} else {
			//      IF (NumIDFSections > 0) THEN
			//        SectionsOnFile(NumIDFSections)%LastRecord=NumIDFRecords
			//      ENDIF
			++SectionDef( Found ).NumFound;
			if ( ! ProcessingIDD ) {
				++NumIDFSections;
				SectionsOnFile( NumIDFSections ).Name = SqueezedSection;
				SectionsOnFile( NumIDFSections ).FirstRecord = NumIDFRecords;
				SectionsOnFile( NumIDFSections ).FirstLineNo = LineNo;
			}
		}
	} else { // End ...
		if ( ! ProcessingIDD ) {
			SqueezedSection.erase( 0, 3 );
			strip( SqueezedSection );
			for ( Found = NumIDFSections; Found >= 1; --Found ) {
				if ( ! InputProcessor::SameString( SectionsOnFile( Found ).Name, SqueezedSection ) ) continue;
				SectionsOnFile( Found ).LastRecord = NumIDFRecords;
			}
		}
	}

}

void
ValidateObjectandParse(
		std::istream & idf_stream,
		std::string const & ProposedObject,
		std::string::size_type & CurPos,
		bool & EndofFile
)
{
	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine validates the proposed object from the IDF and then
	// parses it, putting it into the internal InputProcessor Data structure.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	int const dimLineBuf( 10 );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	std::string SqueezedObject;
	std::string SqueezedArg;
	int Found;
	int NumArg;
	int NumArgExpected;
	int NumAlpha;
	int NumNumeric;
	std::string::size_type Pos;
	bool EndofObject;
	bool BlankLine;
	static bool errFlag( false );
	std::string::size_type LenLeft;
	int Count;
	std::string FieldString;
	std::string FieldNameString;
	std::string Message;
	std::string cStartLine;
	std::string cStartName;
	static Array1D_string LineBuf( dimLineBuf );
	static int StartLine;
	static int NumConxLines;
	static int CurLines;
	static int CurQPtr;

	std::string String;
	bool IDidntMeanIt;
	bool TestingObject;
	bool TransitionDefer;
	int TFound;
	std::string::size_type NextChr;
	std::string String1;

	SqueezedObject = MakeUPPERCase( stripped( ProposedObject ) );
	if ( len( SqueezedObject ) > static_cast< std::string::size_type >( MaxObjectNameLength ) ) {
		ShowWarningError( "IP: Object name length exceeds maximum, will be truncated=" + stripped( ProposedObject ), EchoInputFile );
		ShowContinueError( "Will be processed as Object=" + SqueezedObject, EchoInputFile );
	}
	IDidntMeanIt = false;

	TestingObject = true;
	TransitionDefer = false;
	while ( TestingObject ) {
		errFlag = false;
		IDidntMeanIt = false;
		if ( SortedIDD ) {
			Found = FindItemInSortedList( SqueezedObject, ListOfObjects, NumObjectDefs );
			if ( Found != 0 ) Found = iListOfObjects( Found );
		} else {
			Found = FindItemInList( SqueezedObject, ListOfObjects, NumObjectDefs );
		}
		if ( Found != 0 ) {
			if ( ObjectDef( Found ).ObsPtr > 0 ) {
				TFound = FindItemInList( SqueezedObject, RepObjects, &SecretObjects::OldName );
				if ( TFound != 0 ) {
					if ( RepObjects( TFound ).Transitioned ) {
						if ( ! RepObjects( TFound ).Used ) ShowWarningError( "IP: Objects=\"" + stripped( ProposedObject ) + "\" are being transitioned to this object=\"" + RepObjects( TFound ).NewName + "\"" );
						RepObjects( TFound ).Used = true;
						if ( SortedIDD ) {
							Found = FindItemInSortedList( SqueezedObject, ListOfObjects, NumObjectDefs );
							if ( Found != 0 ) Found = iListOfObjects( Found );
						} else {
							Found = FindItemInList( SqueezedObject, ListOfObjects, NumObjectDefs );
						}
					} else if ( RepObjects( TFound ).TransitionDefer ) {
						if ( ! RepObjects( TFound ).Used ) ShowWarningError( "IP: Objects=\"" + stripped( ProposedObject ) + "\" are being transitioned to this object=\"" + RepObjects( TFound ).NewName + "\"" );
						RepObjects( TFound ).Used = true;
						if ( SortedIDD ) {
							Found = FindItemInSortedList( SqueezedObject, ListOfObjects, NumObjectDefs );
							if ( Found != 0 ) Found = iListOfObjects( Found );
						} else {
							Found = FindItemInList( SqueezedObject, ListOfObjects, NumObjectDefs );
						}
						TransitionDefer = true;
					} else {
						Found = 0; // being handled differently for this obsolete object
					}
				}
			}
		}

		TestingObject = false;
		if ( Found == 0 ) {
			// Check to see if it's a "secret" object
			Found = FindItemInList( SqueezedObject, RepObjects, &SecretObjects::OldName );
			if ( Found == 0 ) {
				ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Did not find \"" + stripped( ProposedObject ) + "\" in list of Objects", EchoInputFile );
				// Will need to parse to next ;
				errFlag = true;
			} else if ( RepObjects( Found ).Deleted ) {
				if ( ! RepObjects( Found ).Used ) {
					ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Objects=\"" + stripped( ProposedObject ) + "\" have been deleted from the IDD.  Will be ignored." );
					RepObjects( Found ).Used = true;
				}
				IDidntMeanIt = true;
				errFlag = true;
				Found = 0;
			} else if ( RepObjects( Found ).TransitionDefer ) {

			} else { // This name is replaced with something else
				if ( ! RepObjects( Found ).Used ) {
					if ( ! RepObjects( Found ).Transitioned ) {
						ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Objects=\"" + stripped( ProposedObject ) + "\" are being replaced with this object=\"" + RepObjects( Found ).NewName + "\"" );
						RepObjects( Found ).Used = true;
						SqueezedObject = RepObjects( Found ).NewName;
						TestingObject = true;
					} else {
						ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Objects=\"" + stripped( ProposedObject ) + "\" are being transitioned to this object=\"" + RepObjects( Found ).NewName + "\"" );
						RepObjects( Found ).Used = true;
						if ( SortedIDD ) {
							Found = FindItemInSortedList( SqueezedObject, ListOfObjects, NumObjectDefs );
							if ( Found != 0 ) Found = iListOfObjects( Found );
						} else {
							Found = FindItemInList( SqueezedObject, ListOfObjects, NumObjectDefs );
						}
					}
				} else if ( ! RepObjects( Found ).Transitioned ) {
					SqueezedObject = RepObjects( Found ).NewName;
					TestingObject = true;
				} else {
					if ( SortedIDD ) {
						Found = FindItemInSortedList( SqueezedObject, ListOfObjects, NumObjectDefs );
						if ( Found != 0 ) Found = iListOfObjects( Found );
					} else {
						Found = FindItemInList( SqueezedObject, ListOfObjects, NumObjectDefs );
					}
				}
			}
		} else {

			// Start Parsing the Object according to definition

			errFlag = false;
			LineItem.Name = SqueezedObject;
			LineItem.Alphas = BlankString;
			LineItem.AlphBlank = false;
			LineItem.NumAlphas = 0;
			LineItem.Numbers = 0.0;
			LineItem.NumNumbers = 0;
			LineItem.NumBlank = false;
			LineItem.ObjectDefPtr = Found;
			NumArgExpected = ObjectDef( Found ).NumParams;
			++ObjectDef( Found ).NumFound;
			if ( ObjectDef( Found ).UniqueObject && ObjectDef( Found ).NumFound > 1 ) {
				ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Multiple occurrences of Unique Object=" + stripped( ProposedObject ) );
				++NumMiscErrorsFound;
			}
			if ( ObjectDef( Found ).ObsPtr > 0 ) {
				TFound = FindItemInList( SqueezedObject, RepObjects, &SecretObjects::OldName );
				if ( TFound == 0 ) {
					ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Obsolete object=" + stripped( ProposedObject ) + ", encountered.  Should be replaced with new object=" + ObsoleteObjectsRepNames( ObjectDef( Found ).ObsPtr ) );
				} else if ( ! RepObjects( TFound ).Used && RepObjects( TFound ).Transitioned ) {
					ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Objects=\"" + stripped( ProposedObject ) + "\" are being transitioned to this object=\"" + RepObjects( TFound ).NewName + "\"" );
					RepObjects( TFound ).Used = true;
				}
			}
		}
	}

	NumArg = 0;
	NumAlpha = 0;
	NumNumeric = 0;
	EndofObject = false;
	++CurPos;

	//  Keep context buffer in case of errors
	LineBuf = BlankString;
	NumConxLines = 0;
	StartLine = NumLines;
	cStartLine = InputLine.substr( 0, 300 );
	cStartName = BlankString;
	NumConxLines = 0;
	CurLines = NumLines;
	CurQPtr = 0;

	while ( ! EndofFile && ! EndofObject ) {
		if ( CurLines != NumLines ) {
			NumConxLines = min( NumConxLines + 1, dimLineBuf );
			++CurQPtr;
			if ( CurQPtr == 1 && is_blank( cStartName ) && not_blank( InputLine ) ) {
				if ( Found > 0 ) {
					if ( ObjectDef( Found ).NameAlpha1 ) {
						Pos = index( InputLine, ',' );
						if ( Pos != std::string::npos ) {
							cStartName = stripped( InputLine.substr( 0, Pos ) );
						}
					}
				}
			}
			if ( CurQPtr > dimLineBuf ) CurQPtr = 1;
			LineBuf( CurQPtr ) = InputLine;
			CurLines = NumLines;
		}
		if ( CurPos < static_cast< std::string::size_type >( InputLineLength ) ) {
			Pos = scan( InputLine.substr( CurPos, InputLineLength - CurPos ), ",;" );
			if ( Pos == std::string::npos ) {
				if ( InputLine[ InputLineLength - 1 ] == '!' ) {
					LenLeft = len_trim( InputLine.substr( CurPos, InputLineLength - CurPos - 1 ) );
				} else {
					LenLeft = len_trim( InputLine.substr( CurPos, InputLineLength - CurPos ) );
				}
				if ( LenLeft == 0 ) {
					CurPos = InputLineLength;
					continue;
				} else {
					if ( InputLine[ InputLineLength - 1 ] == '!' ) {
						Pos = InputLineLength - CurPos;
						DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
						ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Comma being inserted after:\"" + InputLine.substr( CurPos, InputLineLength - CurPos - 1 ) + "\" in Object=" + SqueezedObject, EchoInputFile );
					} else {
						Pos = InputLineLength - CurPos + 1;
						DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
						ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Comma being inserted after:\"" + InputLine.substr( CurPos, InputLineLength - CurPos ) + "\" in Object=" + SqueezedObject, EchoInputFile );
					}
				}
			}
		} else {
			ReadInputLine( idf_stream, CurPos, BlankLine, EndofFile );
			continue;
		}
		if ( Pos != std::string::npos ) {
			if ( ! errFlag ) {
				if ( Pos > 0 ) {
					SqueezedArg = MakeUPPERCase( stripped( InputLine.substr( CurPos, Pos ) ) );
					if ( len( stripped( InputLine.substr( CurPos, Pos ) ) ) > static_cast< std::string::size_type >( MaxAlphaArgLength ) ) {
						DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
						ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Alpha Argument length exceeds maximum, will be truncated=" + InputLine.substr( CurPos, Pos - 1 ), EchoInputFile );
						ShowContinueError( "Will be processed as Alpha=" + SqueezedArg, EchoInputFile );
					}
				} else {
					SqueezedArg = BlankString;
				}
				if ( NumArg == NumArgExpected && ! ObjectDef( Found ).ExtensibleObject ) {
					DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
					ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected for Object=" + ObjectDef( Found ).Name, EchoInputFile );
					ShowContinueError( " Maximum arguments reached for this object, trying to process ->" + SqueezedArg + "<-", EchoInputFile );
					errFlag = true;
				} else {
					if ( NumArg == NumArgExpected && ObjectDef( Found ).ExtensibleObject ) {
						ExtendObjectDefinition( Found, NumArgExpected );
					}
					++NumArg;
					if ( ObjectDef( Found ).AlphaOrNumeric( NumArg ) ) {
						if ( NumAlpha == ObjectDef( Found ).NumAlpha ) {
							DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
							ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected for Object=" + ObjectDef( Found ).Name, EchoInputFile );
							ShowContinueError( " Too many Alphas for this object, trying to process ->" + SqueezedArg + "<-", EchoInputFile );
							errFlag = true;
						} else {
							++NumAlpha;
							LineItem.NumAlphas = NumAlpha;
							if ( ObjectDef( Found ).AlphRetainCase( NumArg ) ) {
								SqueezedArg = InputLine.substr( CurPos, Pos );
								strip( SqueezedArg );
							}
							if ( ! SqueezedArg.empty() ) {
								LineItem.Alphas( NumAlpha ) = SqueezedArg;
							} else if ( ObjectDef( Found ).ReqField( NumArg ) ) { // Blank Argument
								if ( ! ObjectDef( Found ).AlphFieldDefs( NumAlpha ).empty() ) {
									LineItem.Alphas( NumAlpha ) = ObjectDef( Found ).AlphFieldDefs( NumAlpha );
								} else {
									if ( ObjectDef( Found ).NameAlpha1 && NumAlpha != 1 ) {
										DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
										ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected in Object=" + ObjectDef( Found ).Name + ", name=" + LineItem.Alphas( 1 ), EchoInputFile );
									} else {
										DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
										ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected in Object=" + ObjectDef( Found ).Name, EchoInputFile );
									}
									ShowContinueError( "Field [" + ObjectDef( Found ).AlphFieldChks( NumAlpha ) + "] is required but was blank", EchoInputFile );
									++NumBlankReqFieldFound;
								}
							} else {
								LineItem.AlphBlank( NumAlpha ) = true;
								if ( ! ObjectDef( Found ).AlphFieldDefs( NumAlpha ).empty() ) {
									LineItem.Alphas( NumAlpha ) = ObjectDef( Found ).AlphFieldDefs( NumAlpha );
								}
							}
						}
					} else {
						if ( NumNumeric == ObjectDef( Found ).NumNumeric ) {
							DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
							ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected for Object=" + ObjectDef( Found ).Name, EchoInputFile );
							ShowContinueError( " Too many Numbers for this object, trying to process ->" + SqueezedArg + "<-", EchoInputFile );
							errFlag = true;
						} else {
							++NumNumeric;
							LineItem.NumNumbers = NumNumeric;
							if ( ! SqueezedArg.empty() ) {
								if ( ! ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoSizable && ! ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoCalculatable ) {
									LineItem.Numbers( NumNumeric ) = ProcessNumber( SqueezedArg, errFlag );
								} else if ( SqueezedArg == "AUTOSIZE" ) {
									LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoSizeValue;
								} else if ( SqueezedArg == "AUTOCALCULATE" ) {
									LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoCalculateValue;
								} else {
									LineItem.Numbers( NumNumeric ) = ProcessNumber( SqueezedArg, errFlag );
								}
							} else { // numeric arg is blank.
								if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefaultChk ) { // blank arg has default
									if ( ! ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoSize && ! ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoCalculatable ) {
										LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).Default;
										LineItem.NumBlank( NumNumeric ) = true;
									} else if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoSize ) {
										LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoSizeValue;
										LineItem.NumBlank( NumNumeric ) = true;
									} else if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoCalculate ) {
										LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoCalculateValue;
										LineItem.NumBlank( NumNumeric ) = true;
									}
									errFlag = false;
								} else { // blank arg does not have default
									if ( ObjectDef( Found ).ReqField( NumArg ) ) { // arg is required
										if ( ObjectDef( Found ).NameAlpha1 ) { // object has name field - more context for error
											DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
											ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected in Object=" + ObjectDef( Found ).Name + ", name=" + LineItem.Alphas( 1 ), EchoInputFile );
											errFlag = true;
										} else { // object does not have name field
											DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
											ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected in Object=" + ObjectDef( Found ).Name, EchoInputFile );
											errFlag = true;
										}
										ShowContinueError( "Field [" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName + "] is required but was blank", EchoInputFile );
										++NumBlankReqFieldFound;
									}
									LineItem.Numbers( NumNumeric ) = 0.0;
									LineItem.NumBlank( NumNumeric ) = true;
								}
							}
							if ( errFlag ) {
								if ( SqueezedArg[ 0 ] != '=' ) { // argument does not start with "=" (parametric)
									FieldString = IPTrimSigDigits( NumNumeric );
									FieldNameString = ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName;
									if ( ! FieldNameString.empty() ) {
										Message = "Invalid Number in Numeric Field#" + FieldString + " (" + FieldNameString + "), value=" + SqueezedArg;
									} else { // Field Name not recorded
										Message = "Invalid Number in Numeric Field#" + FieldString + ", value=" + SqueezedArg;
									}
									Message += ", in " + ObjectDef( Found ).Name;
									if ( ObjectDef( Found ).NameAlpha1 ) {
										Message += "=" + LineItem.Alphas( 1 );
									}
									DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
									ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + ' ' + Message, EchoInputFile );
								} else { // parametric in Numeric field
									errFlag = false;
								}
							}
						}
					}
				}
			}

			if ( InputLine[ CurPos + Pos ] == ';' ) {
				EndofObject = true;
				// Check if more characters on line -- and if first is a comment character
				if ( not_blank( InputLine.substr( CurPos + Pos + 1 ) ) ) {
					NextChr = FindNonSpace( InputLine.substr( CurPos + Pos + 1 ) );
					if ( ( NextChr == std::string::npos ) || ( InputLine[ CurPos + Pos + 1 + NextChr ] != '!' ) ) {
						DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
						ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " End of Object=\"" + ObjectDef( Found ).Name + "\" reached, but next part of line not comment.", EchoInputFile );
						ShowContinueError( "Final line above shows line that contains error." );
					}
				}
			}
			CurPos += Pos + 1;
		}

	}

	// Store to IDFRecord Data Structure, errFlag is true if there was an error
	// Check out MinimumNumberOfFields
	if ( ! errFlag && ! IDidntMeanIt ) {
		if ( NumArg < ObjectDef( Found ).MinNumFields ) {
			if ( ObjectDef( Found ).NameAlpha1 ) {
				ShowAuditErrorMessage( " ** Warning ** ", "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", name=" + LineItem.Alphas( 1 ) + ", entered with less than minimum number of fields." );
			} else {
				ShowAuditErrorMessage( " ** Warning ** ", "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", entered with less than minimum number of fields." );
			}
			ShowAuditErrorMessage( " **   ~~~   ** ", "Attempting fill to minimum." );
			NumAlpha = 0;
			NumNumeric = 0;
			if ( ObjectDef( Found ).MinNumFields > ObjectDef( Found ).NumParams ) {
				String = IPTrimSigDigits( ObjectDef( Found ).MinNumFields );
				String1 = IPTrimSigDigits( ObjectDef( Found ).NumParams );
				ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object \\min-fields > number of fields specified, Object=" + ObjectDef( Found ).Name );
				ShowContinueError( "..\\min-fields=" + String + ", total number of fields in object definition=" + String1 );
				errFlag = true;
			} else {
				for ( Count = 1; Count <= ObjectDef( Found ).MinNumFields; ++Count ) {
					if ( ObjectDef( Found ).AlphaOrNumeric( Count ) ) {
						++NumAlpha;
						if ( NumAlpha <= LineItem.NumAlphas ) continue;
						++LineItem.NumAlphas;
						if ( ! ObjectDef( Found ).AlphFieldDefs( LineItem.NumAlphas ).empty() ) {
							LineItem.Alphas( LineItem.NumAlphas ) = ObjectDef( Found ).AlphFieldDefs( LineItem.NumAlphas );
							ShowAuditErrorMessage( " **   Add   ** ", ObjectDef( Found ).AlphFieldDefs( LineItem.NumAlphas ) + "   ! field=>" + ObjectDef( Found ).AlphFieldChks( NumAlpha ) );
						} else if ( ObjectDef( Found ).ReqField( Count ) ) {
							if ( ObjectDef( Found ).NameAlpha1 ) {
								ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", name=" + LineItem.Alphas( 1 ) + ", Required Field=[" + ObjectDef( Found ).AlphFieldChks( NumAlpha ) + "] was blank.", EchoInputFile );
							} else {
								ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", Required Field=[" + ObjectDef( Found ).AlphFieldChks( NumAlpha ) + "] was blank.", EchoInputFile );
							}
							errFlag = true;
						} else {
							LineItem.Alphas( LineItem.NumAlphas ).clear();
							LineItem.AlphBlank( LineItem.NumAlphas ) = true;
							ShowAuditErrorMessage( " **   Add   ** ", "<blank field>   ! field=>" + ObjectDef( Found ).AlphFieldChks( NumAlpha ) );
						}
					} else {
						++NumNumeric;
						if ( NumNumeric <= LineItem.NumNumbers ) continue;
						++LineItem.NumNumbers;
						LineItem.NumBlank( NumNumeric ) = true;
						if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefaultChk ) {
							if ( ! ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoSize && ! ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoCalculate ) {
								LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).Default;
								gio::write( String, fmtLD ) << ObjectDef( Found ).NumRangeChks( NumNumeric ).Default;
								strip( String );
								ShowAuditErrorMessage( " **   Add   ** ", String + "   ! field=>" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName );
							} else if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoSize ) {
								LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoSizeValue;
								ShowAuditErrorMessage( " **   Add   ** ", "autosize    ! field=>" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName );
							} else if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoCalculate ) {
								LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoCalculateValue;
								ShowAuditErrorMessage( " **   Add   ** ", "autocalculate    ! field=>" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName );
							}
						} else if ( ObjectDef( Found ).ReqField( Count ) ) {
							if ( ObjectDef( Found ).NameAlpha1 ) {
								ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", name=" + LineItem.Alphas( 1 ) + ", Required Field=[" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName + "] was blank.", EchoInputFile );
							} else {
								ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", Required Field=[" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName + "] was blank.", EchoInputFile );
							}
							errFlag = true;
						} else {
							LineItem.Numbers( NumNumeric ) = 0.0;
							LineItem.NumBlank( NumNumeric ) = true;
							ShowAuditErrorMessage( " **   Add   ** ", "<blank field>   ! field=>" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName );
						}
					}
				}
			}
		}
	}

	if ( ! errFlag && ! IDidntMeanIt ) {
		if ( TransitionDefer ) {
			MakeTransition( Found );
		}
		++NumIDFRecords;
		if ( ObjectStartRecord( Found ) == 0 ) ObjectStartRecord( Found ) = NumIDFRecords;
		MaxAlphaIDFArgsFound = max( MaxAlphaIDFArgsFound, LineItem.NumAlphas );
		MaxNumericIDFArgsFound = max( MaxNumericIDFArgsFound, LineItem.NumNumbers );
		MaxAlphaIDFDefArgsFound = max( MaxAlphaIDFDefArgsFound, ObjectDef( Found ).NumAlpha );
		MaxNumericIDFDefArgsFound = max( MaxNumericIDFDefArgsFound, ObjectDef( Found ).NumNumeric );
		IDFRecords( NumIDFRecords ).Name = LineItem.Name;
		IDFRecords( NumIDFRecords ).NumNumbers = LineItem.NumNumbers;
		IDFRecords( NumIDFRecords ).NumAlphas = LineItem.NumAlphas;
		IDFRecords( NumIDFRecords ).ObjectDefPtr = LineItem.ObjectDefPtr;
		IDFRecords( NumIDFRecords ).Alphas.allocate( LineItem.NumAlphas );
		IDFRecords( NumIDFRecords ).Alphas = LineItem.Alphas( {1,LineItem.NumAlphas} );
		IDFRecords( NumIDFRecords ).AlphBlank.allocate( LineItem.NumAlphas );
		IDFRecords( NumIDFRecords ).AlphBlank = LineItem.AlphBlank( {1,LineItem.NumAlphas} );
		IDFRecords( NumIDFRecords ).Numbers.allocate( LineItem.NumNumbers );
		IDFRecords( NumIDFRecords ).Numbers = LineItem.Numbers( {1,LineItem.NumNumbers} );
		IDFRecords( NumIDFRecords ).NumBlank.allocate( LineItem.NumNumbers );
		IDFRecords( NumIDFRecords ).NumBlank = LineItem.NumBlank( {1,LineItem.NumNumbers} );
		if ( LineItem.NumNumbers > 0 ) {
			for ( Count = 1; Count <= LineItem.NumNumbers; ++Count ) {
				if ( ObjectDef( Found ).NumRangeChks( Count ).MinMaxChk && ! LineItem.NumBlank( Count ) ) {
					InternalRangeCheck( LineItem.Numbers( Count ), Count, Found, LineItem.Alphas( 1 ), ObjectDef( Found ).NumRangeChks( Count ).AutoSizable, ObjectDef( Found ).NumRangeChks( Count ).AutoCalculatable );
				}
			}
		}
	} else if ( ! IDidntMeanIt ) {
		OverallErrorFlag = true;
	}

}

void
ValidateSectionsInput()
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine uses the data structure that is set up during
	// IDF processing and makes sure that record pointers are accurate.
	// They could be inaccurate if a 'section' is input without any
	// 'objects' following.  The invalidity will show itself in the
	// values of the FirstRecord and Last Record pointer.
	// If FirstRecord>LastRecord, then no records (Objects) have been
	// written to the SIDF file for that Section.

	// METHODOLOGY EMPLOYED:
	// Scan the SectionsOnFile data structure and look for invalid
	// FirstRecord,LastRecord items.  Reset those items to -1.

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:
	// na

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	for ( int Count = 1; Count <= NumIDFSections; ++Count ) {
		if ( SectionsOnFile( Count ).FirstRecord > SectionsOnFile( Count ).LastRecord ) {
			gio::write( EchoInputFile, fmtLD ) << " Section " << Count << ' ' << SectionsOnFile( Count ).Name << " had no object records";
			SectionsOnFile( Count ).FirstRecord = -1;
			SectionsOnFile( Count ).LastRecord = -1;
		}
	}

}
*/

    int
    EnergyPlus::InputProcessor::GetNumSectionsFound(std::string const &SectionWord) {
        // PURPOSE OF THIS SUBROUTINE:
        // This function returns the number of a particular section (in input data file)
        // found in the current run.  If it can't find the section in list
        // of sections, a -1 will be returned.

        // METHODOLOGY EMPLOYED:
        // Look up section in list of sections.  If there, return the
        // number of sections of that kind found in the current input.  If not, return -1.
        if (jdf.find("SectionWord") == jdf.end()) return -1;
        int num_sections_found = 0;
        json obj = jdf["SectionWord"];
        for (auto it = obj.begin(); it != obj.end(); ++it) num_sections_found++;
        return num_sections_found;
    }

/*

int
EnergyPlus::InputProcessor::GetNumSectionsinInput()
{

	// FUNCTION INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This function returns the number of sections in the entire input data file
	// of the current run.

	// METHODOLOGY EMPLOYED:
	// Return value of NumIDFSections.

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Return value

	// SUBROUTINE ARGUMENT DEFINITIONS:
	// na

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	// na

	return NumIDFSections;

}


void
EnergyPlus::InputProcessor::GetListofSectionsinInput(
	Array1S_string SectionList,
	int & NuminList
	)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine returns the list of sections as they occurred
	// in the Input Data File (IDF).

	// METHODOLOGY EMPLOYED:
	// Look up object in list of objects.  If there, return the
	// number of objects found in the current input.  If not, return
	// -1.

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Argument array dimensioning

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	int const MaxAllowedOut = min( NumIDFSections, isize( SectionList ) );
	if ( MaxAllowedOut != NumIDFSections ) {
		ShowWarningError( "More in list than allowed in passed array - (GetListofSectionsinInput)" );
	}
	NuminList = MaxAllowedOut;
	for ( int i = 1; i <= MaxAllowedOut; ++i ) SectionList( i ) = SectionsOnFile( i ).Name;

}

*/

    int
    EnergyPlus::InputProcessor::GetNumObjectsFound(std::string const &ObjectWord) {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This function returns the number of objects (in input data file)
        // found in the current run.  If it can't find the object in list
        // of objects, a 0 will be returned.

        // METHODOLOGY EMPLOYED:
        // Look up object in list of objects.  If there, return the
        // number of objects found in the current input.  If not, return 0.

//		json * jdf_object;
        if (jdf.find(ObjectWord) == jdf.end()) {
            auto tmp_umit = InputProcessor::idf_parser.case_insensitive_keys.find(MakeUPPERCase(ObjectWord));
            if (tmp_umit == InputProcessor::idf_parser.case_insensitive_keys.end()
                || jdf.find(tmp_umit->second) == jdf.end()) {
                return 0;
            }
            return static_cast<int>(jdf[tmp_umit->second].size());
        } else {
            return static_cast<int>(jdf[ObjectWord].size());
        }

//		auto const & jdf_object = jdf.find( ObjectWord );
//		if ( jdf_object != jdf->end() ) {
//			return static_cast<int>(jdf_object->size());
//		}

//		json * object_in_schema;
//		auto const & schema_properties = schema["properties"];
        if (schema["properties"].find(ObjectWord) == schema["properties"].end()) {
            auto tmp_umit = InputProcessor::idf_parser.case_insensitive_keys.find(MakeUPPERCase(ObjectWord));
            if (tmp_umit == InputProcessor::idf_parser.case_insensitive_keys.end()) {
                ShowWarningError("Requested Object not found in Definitions: " + ObjectWord);
            }
        }
        return 0;

//		auto const & jdd_properties = schema[ "properties" ];
//		auto const & jdd_object = jdd_properties.find( ObjectWord );
//		if ( jdd_object == jdd_properties.end() ) {
//			ShowWarningError( "Requested Object not found in Definitions: " + ObjectWord );
//		}
//		return 0;
    }

/*
void
EnergyPlus::InputProcessor::GetRecordLocations(
	int const Which,
	int & FirstRecord,
	int & LastRecord
	)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine returns the record location values (which will be
	// passed to 'GetObjectItem') for a section from the list of inputted
	// sections (sequential).

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	// na

	FirstRecord = SectionsOnFile( Which ).FirstRecord;
	LastRecord = SectionsOnFile( Which ).LastRecord;

}
*/

    void
    EnergyPlus::InputProcessor::GetObjectItem(
            std::string const &Object,
            int const Number,
            Array1S_string Alphas,
            int &NumAlphas,
            Array1S<Real64> Numbers,
            int &NumNumbers,
            int &Status,
            Optional<Array1_bool> NumBlank,
            Optional<Array1_bool> AlphaBlank,
            Optional<Array1_string> AlphaFieldNames,
            Optional<Array1_string> NumericFieldNames
    ) {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the 'number' 'object' from the IDFRecord data structure.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Count;
        int LoopIndex;
        std::string ObjectWord;
        std::string UCObject;
        //////////// hoisted into namespace ////////////
        // static Array1D_string AlphaArgs;
        // static Array1D< Real64 > NumberArgs;
        // static Array1D_bool AlphaArgsBlank;
        // static Array1D_bool NumberArgsBlank;
        ////////////////////////////////////////////////
        int MaxAlphas;
        int MaxNumbers;
        int Found;
        int StartRecord;
        std::string cfld1;
        std::string cfld2;
        bool GoodItem;

        json *object_in_jdf;
        if (jdf.find(Object) == jdf.end()) {
            auto tmp_umit = InputProcessor::idf_parser.case_insensitive_keys.find(MakeUPPERCase(Object));
            if (tmp_umit == InputProcessor::idf_parser.case_insensitive_keys.end()
                || jdf.find(tmp_umit->second) == jdf.end()) {
                return;
            }
            object_in_jdf = &jdf[tmp_umit->second];
        } else {
            object_in_jdf = &jdf[Object];
        }

        json *object_in_schema;
//		auto const & schema_properties = schema["properties"];
        if (schema["properties"].find(Object) == schema["properties"].end()) {
            auto tmp_umit = InputProcessor::idf_parser.case_insensitive_keys.find(MakeUPPERCase(Object));
            if (tmp_umit == InputProcessor::idf_parser.case_insensitive_keys.end()) {
                return;
            }
            object_in_schema = &schema["properties"][tmp_umit->second];
        } else {
            object_in_schema = &schema["properties"][Object];
        }

//		if (jdf.find(Object) == jdf.end() || schema["properties"].find(Object) == schema["properties"].end()) {
//			std::cout << "error: object " << Object << " not found in either the schema or jdf, uppercase?" << std::endl;
//			return;
//		}
//		json object_in_jdf = jdf[Object];
//		json object_in_schema = schema["properties"][Object];

        //Autodesk:Uninit Initialize variables used uninitialized
        NumAlphas = 0; //Autodesk:Uninit Force default initialization
        NumNumbers = 0; //Autodesk:Uninit Force default initialization
        // TODO: Make this safe, alphas and numerics might not exist
//		if (object_in_schema[ "legacy_idd" ].find( "alphas" ) != object_)
//	NumAlphas = object_in_schema["legacy_idd"]["alphas"].size();
//	NumNumbers = object_in_schema["legacy_idd"]["numerics"].size();

        MaxAlphas = isize(Alphas, 1);
        MaxNumbers = isize(Numbers, 1);
        GoodItem = false;

//	if ( ! allocated( AlphaArgs ) ) {
//		if ( NumObjectDefs == 0 ) {
//			// ProcessInput();
//			// jdf = IdfParser::parse_idf(idf_stream);
//			std::cout << "Alpha args == 0" << std::endl;
//		}
//		AlphaArgs.allocate( MaxAlphaArgsFound );
//		NumberArgs.allocate( MaxNumericArgsFound );
//		NumberArgsBlank.allocate( MaxNumericArgsFound );
//		AlphaArgsBlank.allocate( MaxAlphaArgsFound );
//	}

        Status = -1;
        UCObject = MakeUPPERCase(Object);
//	if ( SortedIDD ) {
//		Found = FindItemInSortedList( UCObject, ListOfObjects, NumObjectDefs );
//		if ( Found != 0 ) Found = iListOfObjects( Found );
//	} else {
//		Found = FindItemInList( UCObject, ListOfObjects, NumObjectDefs );
//	}
//	if ( Found == 0 ) { //  This is more of a developer problem
//		ShowFatalError( "IP: GetObjectItem: Requested object=" + UCObject + ", not found in Object Definitions -- incorrect IDD attached." );
//	}

        // TODO: Fix later
//		if (object_in_schema->at("legacy_idd")["alphas"].size() < MaxAlphas) {
//			ShowFatalError( "IP: GetObjectItem: " + Object + ", Number of Object Alpha Args [" + std::to_string(MaxAlphas)
//											+ "] > Size of alphas array [" + std::to_string(object_in_schema->at("legacy_idd")["alphas"].size()) + "]." );
//		}
//
//		if (object_in_schema->at("legacy_idd")["numerics"].size() < MaxNumbers) {
//			ShowFatalError( "IP: GetObjectItem: " + Object + ", Number of Numeric Args [" + std::to_string(MaxNumbers)
//											+ "] > Size of numerics array [" + std::to_string(object_in_schema->at("legacy_idd")["numerics"].size()) + "]." );
//		}

//	StartRecord = ObjectStartRecord( Found );
//	if ( StartRecord == 0 ) {
//		ShowWarningError( "IP: GetObjectItem: Requested object=" + UCObject + ", not found in IDF." );
//		Status = -1;
//		StartRecord = NumIDFRecords + 1;
//	}

//	if ( ObjectGotCount( Found ) == 0 ) {
//		gio::write( EchoInputFile, fmtLD ) << "Getting object=" << UCObject;
//	}
//	++ObjectGotCount( Found );

        Alphas = "";
        Numbers = 0;

        auto obj = object_in_jdf->begin() + Number - 1;
        auto const obj_val = obj.value();
        auto const &alphas_fields = object_in_schema->at("legacy_idd")["alphas"]["fields"];
        for (int i = 0; i < alphas_fields.size(); ++i) {
            std::string const field = alphas_fields[i];
            if (field == "name") {
                Alphas(i + 1) = MakeUPPERCase(obj.key());
                if (present(AlphaBlank)) AlphaBlank()(i + 1) = obj.key().empty();  // TODO is this what should be done? unit tests for the original input processor are needed
                if (present(AlphaFieldNames)) AlphaFieldNames()(i + 1) = field;
                NumAlphas++;
                continue;
            }
            auto it = obj.value().find(field);
            if (it != obj.value().end()) {
                std::string val;
                if (it.value().is_string()) {
                    val = it.value().get<std::string>();
                    Alphas(i + 1) = MakeUPPERCase(val);
                } else {
                    std::stringstream ss;
                    ss << it.value().get<double>();
                    Alphas(i + 1) = ss.str();
//					val = std::to_string(it.value().get<double>());
//					Alphas( i + 1 ) = val;
                }
                if (present(AlphaBlank)) AlphaBlank()(i + 1) = val.empty();
                NumAlphas++;
            } else {
                Alphas(i + 1) = "";  // this might be completely redundant bc this has already been taken care of during the input processing
                if (present(AlphaBlank)) AlphaBlank()(i + 1) = true;
            }
            if (present(AlphaFieldNames)) AlphaFieldNames()(i + 1) = field;
        }

        if (object_in_schema->at("legacy_idd")["alphas"].find("extensions") !=
            object_in_schema->at("legacy_idd")["alphas"].end()) {
            auto const &alphas_extensions = object_in_schema->at("legacy_idd")["alphas"]["extensions"];
            auto const extensions = obj.value()["extensions"];
            int alphas_index = alphas_fields.size();
            for (auto it = extensions.begin(); it != extensions.end(); ++it) {
                auto const extension_obj = it.value();
                for (auto i = 0; i < alphas_extensions.size(); i++) {
                    std::string const field = alphas_extensions[i];
                    if (extension_obj.find(field) != extension_obj.end()) {
                        if (extension_obj[field].is_string()) {
                            std::string const val = extension_obj[field];
                            Alphas(alphas_index + 1) = MakeUPPERCase(val);
							if (present(AlphaBlank)) AlphaBlank()(alphas_index + 1) = val.empty();
                        } else {
                            double val = extension_obj[field]; // TODO is this reachable?
                            std::stringstream ss;
                            ss << val;
                            Alphas(alphas_index + 1) = ss.str();
//							Alphas(alphas_index + 1) = std::to_string(val);
							if (present(AlphaBlank)) AlphaBlank()(alphas_index + 1) = false;
                        }
                        NumAlphas++;
                    } else {
                        Alphas(alphas_index + 1) = "";
                        if (present(AlphaBlank)) AlphaBlank()(alphas_index + 1) = true;
                    }
                    if (present(AlphaFieldNames)) AlphaFieldNames()(alphas_index + 1) = field;
                    alphas_index++;
                }
            }
        }

        auto const &numerics_fields = object_in_schema->at("legacy_idd")["numerics"]["fields"];
        for (int i = 0; i < numerics_fields.size(); ++i) {
            std::string const field = numerics_fields[i];
            auto it = obj.value().find(field);
            if (it != obj.value().end()) {
                if (!it.value().is_string()) {
					Numbers(i + 1) = it.value().get<double>();
					if (present(NumBlank)) NumBlank()(i + 1) = false;
				} else {
                    if (it.value().get<std::string>().empty()) {
                        Numbers(i + 1) = 0;
                    } else {
                        Numbers(i + 1) = -99999; // autosize and autocalculate
                    }
					if (present(NumBlank)) NumBlank()(i + 1) = it.value().get<std::string>().empty();
                }
                NumNumbers++;
            } else {
                // TODO What to do if a numeric field is left blank?
                auto const pattern_props = object_in_schema->find("patternProperties");
                if (pattern_props != object_in_schema->end()) {
                    auto const field_in_schema = pattern_props.value()[".*"]["properties"];
                    if (field_in_schema.find(field) != field_in_schema.end()) {
                        if (field_in_schema[field].find("default") != field_in_schema[field].end()) {
                            auto const default_val = field_in_schema[field]["default"];
                            if (!default_val.is_string()) Numbers(i + 1) = default_val.get<double>();
                            else Numbers(i + 1) = -99999;  // autosize and autocalculate
                        } else {
                            Numbers(i + 1) = 0; // TODO this might not be ok at all, unit tests needed.
                        }
                    } else {
                        std::cout << "field " << field << " not found in object " << Object << std::endl;
                    }
                } else if (object_in_schema->find("properties") != object_in_schema->end()) {
                    if (object_in_schema->at("properties")[field].find("default") !=
                        		object_in_schema->at("properties")[field].end()) {
                        Numbers(i + 1) = object_in_schema->at("properties")[field]["default"].get<double>();
                    } else {
                        Numbers(i + 1) = 0;
                    }
                }
                if (present(NumBlank)) NumBlank()(i + 1) = true; // TODO AHHH IS IT CONSIDERED BLANK STILL? unit tests needed
            }
            if (present(NumericFieldNames)) NumericFieldNames()(i + 1) = field;
        }


        if (object_in_schema->at("legacy_idd")["numerics"].find("extensions") !=
					object_in_schema->at("legacy_idd")["numerics"].end()) {
            auto const &numerics_extensions = object_in_schema->at("legacy_idd")["numerics"]["extensions"];
            auto const extensions = obj.value()["extensions"];
            int numerics_index = numerics_fields.size();
            for (auto it = extensions.begin(); it != extensions.end(); ++it) {
                auto const extension_obj = it.value();
                for (auto i = 0; i < numerics_extensions.size(); i++) {
                    std::string const field = numerics_extensions[i];
                    if (extension_obj.find(field) != extension_obj.end()) {
                        auto const val = extension_obj[field];
                        if (!val.is_string()) {
							Numbers(numerics_index + 1) = val.get<double>();
							if (present(NumBlank)) NumBlank()(numerics_index + 1) = false;
						} else {
							if (val.get<std::string>().empty()) {
								Numbers(numerics_index + 1) = 0; // TODO once again we don't even know if this is correct behavior
							} else { // autosize and autocalculate
								Numbers(numerics_index + 1) = -99999;
							}
							if (present(NumBlank)) NumBlank()(numerics_index + 1) = val.get<std::string>().empty();
						}
                        NumNumbers++;
                    } else {
                        auto const pattern_props = object_in_schema->find("patternProperties");
                        if (pattern_props != object_in_schema->end()) {
                            auto const field_in_schema = pattern_props.value()[".*"]["properties"];
                            if (field_in_schema.find(field) != field_in_schema.end()) {
                                if (field_in_schema[field].find("default") != field_in_schema[field].end()) {
                                    auto const default_val = field_in_schema[field]["default"];
                                    if (!default_val.is_string())
                                        Numbers(numerics_index + 1) = default_val.get<double>();
                                    else {
                                        if (it.value().get<std::string>().empty()) {
                                            Numbers(numerics_index + 1) = 0;
                                        } else {
                                            Numbers(numerics_index + 1) = -99999; // autosize and autocalculate
                                        }
                                    }
                                } else {
                                    Numbers(numerics_index + 1) = 0;
                                }
                            } else {
                                std::cout << "field " << field << " not found in object " << Object << std::endl;
                            }
                        } else if (object_in_schema->find("properties") != object_in_schema->end()) {
                            if (object_in_schema->at("properties")[field].find("default") !=
                                object_in_schema->at("properties")[field].end()) {
                                Numbers(numerics_index + 1) = object_in_schema->at("properties")[field]["default"].get<double>();
                            } else {
                                Numbers(numerics_index + 1) = 0;
                            }
                        }
                        if (present(NumBlank)) NumBlank()(numerics_index + 1) = true; // TODO same issue, we don't know if this is correct
                    }
                    if (present(NumericFieldNames)) NumericFieldNames()(numerics_index + 1) = field;
                    numerics_index++;
                }
            }
        }

//		auto const &numerics_extensions = object_in_schema[ "legacy_idd" ][ "numerics" ][ "extensions" ];
//		for (int i = numerics_fields.size(); i < numerics_fields.size() + numerics_extensions.size(); ++i) {
//			std::string const field = numerics_extensions[i];
//			auto it = obj.value().find(field);
//			if ( it != obj.value().end() ) {
//				if (!it.value().is_string()) Numbers( i + 1 ) = it.value().get<double>();
//				else Numbers( i + 1 ) = -99999;  // autosize and autocalculate
//				if ( present( NumBlank ) ) NumBlank()( i + 1 ) = false;
//				NumNumbers++;
//			} else {
//				// TODO What to do if a numeric field is left blank?
//				auto const pattern_props = object_in_schema.find( "patternProperties" );
//				if (pattern_props != object_in_schema.end()) {
//					auto const field_in_schema = pattern_props.value()[ ".*" ][ "properties" ];
//					if ( field_in_schema.find( field ) != field_in_schema.end() ) {
//						if ( field_in_schema[ field ].find( "default" ) != field_in_schema[ field ].end() ) {
//							auto const default_val = field_in_schema[ field ][ "default" ];
//							if (!default_val.is_string()) Numbers( i + 1 ) = default_val.get<double>();
//							else Numbers( i + 1 ) = -99999;  // autosize and autocalculate
//						} else {
//							Numbers ( i + 1 ) = -99999;
//						}
//					} else {
//						std::cout << "field " << field << " not found in object " << Object << std::endl;
//					}
//				} else if ( object_in_schema.find( "properties" ) != object_in_schema.end() ) {
//					if ( object_in_schema[ "properties" ][ field ].find("default") != object_in_schema["properties"][field].end()) {
//						Numbers( i + 1 ) = object_in_schema[ "properties" ][ field ][ "default" ].get< double >();
//					} else {
//						Numbers( i + 1 ) = -99999;
//					}
//				}
////			Numbers( i + 1 ) = -99999;
//				if ( present( NumBlank ) ) NumBlank()( i + 1 ) = true;
//			}
//			if ( present( NumericFieldNames ) ) NumericFieldNames()( i + 1 )= field;
//		}

        Status = 1;
    }

    int
    EnergyPlus::InputProcessor::GetObjectItemNum(
            std::string const &ObjType, // Object Type (ref: IDD Objects)
            std::string const &ObjName // Name of the object type
    ) {
        // PURPOSE OF THIS SUBROUTINE:
        // Get the occurrence number of an object of type ObjType and name ObjName

        json *obj;

        if (jdf.find(ObjType) == jdf.end() || jdf[ObjType].find(ObjName) == jdf[ObjType].end()) {
            auto tmp_umit = InputProcessor::idf_parser.case_insensitive_keys.find(MakeUPPERCase(ObjType));
            if (tmp_umit == InputProcessor::idf_parser.case_insensitive_keys.end()) {
                return -1;
            }
            obj = &jdf[tmp_umit->second];
        } else {
            obj = &jdf[ObjType];
        }

        int object_item_num = 1;
        bool found = false;
        for (auto it = obj->begin(); it != obj->end(); ++it) {
            if (MakeUPPERCase(it.key()) == MakeUPPERCase(ObjName)) {
                found = true;
                break;
            }
            object_item_num++;
        }

        if (!found) return -1;
        return object_item_num;
    }

// void
// EnergyPlus::InputProcessor::TellMeHowManyObjectItemArgs(
// 	std::string const & Object,
// 	int const Number,
// 	int & NumAlpha,
// 	int & NumNumbers,
// 	int & Status
// 	)
// {
// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda K. Lawrie
// 	//       DATE WRITTEN   September 1997

// 	// PURPOSE OF THIS SUBROUTINE:
// 	// This subroutine returns the number of arguments (alpha and numeric) for
// 	// the referenced 'number' Object.

// 	int Count;
// 	int LoopIndex;
// 	std::string ObjectWord;

// 	Count = 0;
// 	Status = -1;
// 	for ( LoopIndex = 1; LoopIndex <= NumIDFRecords; ++LoopIndex ) {
// 		if ( InputProcessor::SameString( IDFRecords( LoopIndex ).Name, Object ) ) {
// 			++Count;
// 			if ( Count == Number ) {
// 				// Read this one
// 				GetObjectItemfromFile( LoopIndex, ObjectWord, NumAlpha, NumNumbers );
// 				Status = 1;
// 				break;
// 			}
// 		}
// 	}
// }

// void
// EnergyPlus::InputProcessor::GetObjectItemfromFile(
// 	int const Which,
// 	std::string & ObjectWord,
// 	int & NumAlpha,
// 	int & NumNumeric,
// 	Optional< Array1S_string > AlphaArgs,
// 	Optional< Array1S< Real64 > > NumericArgs,
// 	Optional< Array1S_bool > AlphaBlanks,
// 	Optional< Array1S_bool > NumericBlanks
// 	)
// {

// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda K. Lawrie
// 	//       DATE WRITTEN   September 1997
// 	//       MODIFIED       na
// 	//       RE-ENGINEERED  na

// 	// PURPOSE OF THIS SUBROUTINE:
// 	// This subroutine "gets" the object instance from the data structure.

// 	// METHODOLOGY EMPLOYED:
// 	// na

// 	// REFERENCES:
// 	// na

// 	// Using
// 	using DataStringGlobals::NL;

// 	// Argument array dimensioning

// 	// Locals
// 	// SUBROUTINE ARGUMENT DEFINITIONS:

// 	// SUBROUTINE PARAMETER DEFINITIONS:
// 	// na

// 	// INTERFACE BLOCK SPECIFICATIONS
// 	// na

// 	// DERIVED TYPE DEFINITIONS
// 	// na

// 	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

// 	// Object Data
// 	LineDefinition xLineItem; // Description of current record

// 	if ( Which > 0 && Which <= NumIDFRecords ) {
// 		xLineItem = IDFRecords( Which );
// 		ObjectWord = xLineItem.Name;
// 		NumAlpha = xLineItem.NumAlphas;
// 		NumNumeric = xLineItem.NumNumbers;
// 		if ( present( AlphaArgs ) ) {
// 			if ( NumAlpha >= 1 ) {
// 				AlphaArgs()( {1,NumAlpha} ) = xLineItem.Alphas( {1,NumAlpha} );
// 			}
// 		}
// 		if ( present( AlphaBlanks ) ) {
// 			if ( NumAlpha >= 1 ) {
// 				AlphaBlanks()( {1,NumAlpha} ) = xLineItem.AlphBlank( {1,NumAlpha} );
// 			}
// 		}
// 		if ( present( NumericArgs ) ) {
// 			if ( NumNumeric >= 1 ) {
// 				NumericArgs()( {1,NumNumeric} ) = xLineItem.Numbers( {1,NumNumeric} );
// 			}
// 		}
// 		if ( present( NumericBlanks ) ) {
// 			if ( NumNumeric >= 1 ) {
// 				NumericBlanks()( {1,NumNumeric} ) = xLineItem.NumBlank( {1,NumNumeric} );
// 			}
// 		}
// 	} else {
// 		if ( echo_stream ) *echo_stream << " Requested Record " << Which << " not in range, 1 -- " << NumIDFRecords << NL;
// 	}

// }

// Utility Functions/Routines for Module

//void
//EnergyPlus::InputProcessor::ReadInputLine(
//	std::istream & in_stream,
//	std::string::size_type & CurPos,
//	bool & BlankLine,
//	bool & EndofFile
//	)
//{
//
//	// SUBROUTINE INFORMATION:
//	//       AUTHOR         Linda K. Lawrie
//	//       DATE WRITTEN   September 1997
//	//       MODIFIED       na
//	//       RE-ENGINEERED  na
//
//	// PURPOSE OF THIS SUBROUTINE:
//	// This subroutine reads a line in the specified file and checks for end of file
//
//	// METHODOLOGY EMPLOYED:
//	// na
//
//	// REFERENCES:
//	// na
//
//	// Using
//	using DataStringGlobals::NL;
//
//	// Locals
//	// SUBROUTINE ARGUMENT DEFINITIONS:
//
//	// SUBROUTINE PARAMETER DEFINITIONS:
//	char const SPC( ' ' );
//	char const TAB( '\t' );
//
//	// INTERFACE BLOCK SPECIFICATIONS
//	// na
//
//	// DERIVED TYPE DEFINITIONS
//	// na
//
//	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
//	int ReadStat;
//	std::string::size_type Pos;
//	std::string::size_type Slash;
//	std::string::size_type NSpace;
//	bool errFlag;
//
//	errFlag = false;
//	{ IOFlags flags; cross_platform_get_line( in_stream, InputLine ); flags.set_status( in_stream ); ReadStat = flags.ios(); }
//
//	if ( ReadStat != 0 ) InputLine.clear();
//	std::replace( InputLine.begin(), InputLine.end(), TAB, SPC ); // Replace tabs with spaces
//
//	BlankLine = false;
//	CurPos = 0;
//	if ( ReadStat < 0 ) {
//		EndofFile = true;
//	} else {
//		if ( EchoInputLine ) {
//			++NumLines;
//			if ( DisplayInputInAudit ) {
//				if ( echo_stream ) *echo_stream << std::setw( 7 ) << NumLines << ' ' << InputLine << NL;
//			}
//		}
//		EchoInputLine = true;
//		InputLineLength = static_cast< int >( len_trim( InputLine ) );
//		if ( InputLineLength == 0 ) {
//			BlankLine = true;
//		}
//		Pos = ( ProcessingIDD ? InputLine.find_first_of( "!\\" ) : InputLine.find( '!' ) ); // 4/30/09 remove ~
//		if ( Pos != std::string::npos ) {
//			Slash = ( ProcessingIDD ? InputLine.find( '\\' ) : std::string::npos );
//			InputLineLength = static_cast< int >( Pos + 1 );
//			if ( Pos > 0 ) {
//				if ( is_blank( InputLine.substr( 0, Pos ) ) ) {
//					BlankLine = true;
//				}
//			} else {
//				BlankLine = true;
//			}
//			if ( ( Slash != std::string::npos ) && ( Pos == Slash ) ) {
//				std::string UCInputLine( InputLine, Pos );
//				uppercase( UCInputLine ); // With this many comparisons probably faster to uppercase once vs many c-i comparisons
//				FieldSet = false;
//				switch ( UCInputLine.length() > 1 ? UCInputLine[ 1 ] : ' ' ) { //Performance Switch to reduce expensive string operations
//					case 'E':
//						if ( has_prefix( UCInputLine, "\\EXTENSIBLE" ) ) { // Extensible arg
//							ExtensibleObject = true;
//							if ( UCInputLine[ 11 ] != ':' ) {
//								ShowFatalError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal definition for extensible object, should be \"\\extensible:<num>\"", EchoInputFile );
//							} else { // process number
//								std::string const number_str( UCInputLine.substr( 12 ) );
//								NSpace = scan( number_str, " !" );
//								if ( NSpace != std::string::npos ) {
//									ExtensibleNumFields = int( ProcessNumber( number_str.substr( 0, NSpace ), errFlag ) );
//									if ( errFlag ) {
//										ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal Number for \\extensible:<num>", EchoInputFile );
//									}
//								} else {
//									ExtensibleNumFields = 0.0;
//									errFlag = false;
//								}
//							}
//						}
//						break;
//						case 'F':
//						if ( has_prefix( UCInputLine, "\\FIELD" ) ) {
//							// Capture Field Name
//							CurrentFieldName = InputLine.substr( Slash + 6 );
//							strip( CurrentFieldName );
//							FieldSet = true;
//						}
//						break;
//						case 'M':
//						if ( has_prefix( UCInputLine, "\\MIN-FIELDS" ) ) { // Min-Fields arg
//							//RequiredField = true;
//							NSpace = FindNonSpace( UCInputLine.substr( 11 ) );
//							if ( NSpace == std::string::npos ) {
//								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Need number for \\Min-Fields", EchoInputFile );
//								errFlag = true;
//								MinimumNumberOfFields = 0;
//							} else {
//								std::string const number_str( trimmed( UCInputLine.substr( 11 + NSpace ) ) );
//								NSpace = scan( number_str, " !" );
//								if ( NSpace == std::string::npos ) {
//									MinimumNumberOfFields = int( ProcessNumber( number_str, errFlag ) );
//								} else {
//									MinimumNumberOfFields = int( ProcessNumber( number_str.substr( 0, NSpace ), errFlag ) );
//								}
//								if ( errFlag ) {
//									ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal Number for \\Min-Fields", EchoInputFile );
//								}
//							}
//						}
//						break;
//						case 'O':
//						if ( has_prefix( UCInputLine, "\\OBSOLETE" ) ) { // Obsolete arg
//							NSpace = index( UCInputLine.substr( 9 ), "=>" );
//							if ( NSpace == std::string::npos ) {
//								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Need replacement object for \\Obsolete objects", EchoInputFile );
//								errFlag = true;
//							} else {
//								std::string const name_str( InputLine.substr( Pos + 9 + NSpace + 2 ) );
//								ReplacementName = trimmed( name_str );
//								ObsoleteObject = true;
//							}
//						}
//						break;
//						case 'R':
//						if ( has_prefix( UCInputLine, "\\REQUIRED-FIELD" ) ) { // Required-field arg
//							RequiredField = true;
//						} else if ( has_prefix( UCInputLine, "\\REQUIRED-OBJECT" ) ) { // Required-object arg
//							RequiredObject = true;
////						} else if ( has_prefix( UCInputLine, "\\RETAINCASE" ) ) {
////							//RetainCase = true; // Arg not present
//						}
//						break;
//						case 'U':
//						if ( has_prefix( UCInputLine, "\\UNIQUE-OBJECT" ) ) { // Unique-object arg
//							UniqueObject = true;
//						}
//						break;
//					}
//				}
//			}
//		}
//
//	}

//	void
//	EnergyPlus::InputProcessor::ReadInputLine(
//		std::istream & in_stream,
//		std::string::size_type & CurPos,
//		bool & BlankLine,
//		bool & EndofFile,
//		bool & MinMax,
//		int & WhichMinMax, // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
//		std::string & MinMaxString,
//		Real64 & Value,
//		bool & Default,
//		std::string & DefString,
//		bool & AutoSizable,
//		bool & AutoCalculatable,
//		bool & RetainCase,
//		bool & ErrorsFound
//		)
//	{
//
//	// SUBROUTINE INFORMATION:
//	//       AUTHOR         Linda K. Lawrie
//	//       DATE WRITTEN   September 1997
//	//       MODIFIED       na
//	//       RE-ENGINEERED  na
//
//	// PURPOSE OF THIS SUBROUTINE:
//	// This subroutine reads a line in the specified file and checks for end of file
//
//	// METHODOLOGY EMPLOYED:
//	// na
//
//	// REFERENCES:
//	// na
//
//	// Using
//		using DataStringGlobals::NL;
//
//	// Locals
//	// SUBROUTINE ARGUMENT DEFINITIONS:
//
//	// SUBROUTINE PARAMETER DEFINITIONS:
//		char const SPC( ' ' );
//		char const TAB( '\t' );
//
//	// INTERFACE BLOCK SPECIFICATIONS
//	// na
//
//	// DERIVED TYPE DEFINITIONS
//	// na
//
//	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
//		int ReadStat;
//		std::string::size_type Pos;
//		std::string::size_type Slash;
//		std::string::size_type P1;
//		std::string::size_type NSpace;
//		bool errFlag;
//		int ErrLevel;
//
//		errFlag = false;
//		{ IOFlags flags; cross_platform_get_line( in_stream, InputLine ); flags.set_status( in_stream ); ReadStat = flags.ios(); }
//
//		if ( ReadStat != 0 ) InputLine.clear();
//	std::replace( InputLine.begin(), InputLine.end(), TAB, SPC ); // Replace tabs with spaces
//
//	BlankLine = false;
//	CurPos = 0;
//	if ( ReadStat < 0 ) {
//		EndofFile = true;
//	} else {
//		if ( EchoInputLine ) {
//			++NumLines;
//			if ( DisplayInputInAudit ) {
//				if ( echo_stream ) *echo_stream << std::setw( 7 ) << NumLines << ' ' << InputLine << NL;
//			}
//		}
//		EchoInputLine = true;
//		InputLineLength = static_cast< int >( len_trim( InputLine ) );
//		if ( InputLineLength == 0 ) {
//			BlankLine = true;
//		}
//		Pos = ( ProcessingIDD ? InputLine.find_first_of( "!\\" ) : InputLine.find( '!' ) ); // 4/30/09 remove ~
//		if ( Pos != std::string::npos ) {
//			Slash = ( ProcessingIDD ? InputLine.find( '\\' ) : std::string::npos );
//			InputLineLength = static_cast< int >( Pos + 1 );
//			if ( Pos > 0 ) {
//				if ( is_blank( InputLine.substr( 0, Pos ) ) ) {
//					BlankLine = true;
//				}
//			} else {
//				BlankLine = true;
//			}
//			if ( ( Slash != std::string::npos ) && ( Pos == Slash ) ) {
//				std::string UCInputLine( InputLine, Pos );
//				uppercase( UCInputLine ); // With this many comparisons probably faster to uppercase once vs many c-i comparisons
//				FieldSet = false;
//				MinMax = false;
//				Default = false;
//				AutoSizable = false;
//				AutoCalculatable = false;
//				switch ( UCInputLine.length() > 1 ? UCInputLine[ 1 ] : ' ' ) { //Performance Switch to reduce expensive string operations
//					case 'A':
//						if ( has_prefix( UCInputLine, "\\AUTOS" ) ) { // AutoSizable arg
//							AutoSizable = true;
//							ProcessMinMaxDefLine( UCInputLine, WhichMinMax, MinMaxString, Value, DefString, ErrLevel );
//							if ( ErrLevel > 0 ) {
//								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Error in Autosize designation -- invalid number=" + UCInputLine, EchoInputFile );
//								errFlag = true;
//							}
//						} else if ( has_prefix( UCInputLine, "\\AUTOC" ) ) { // AutoCalculatable arg
//							AutoCalculatable = true;
//							ProcessMinMaxDefLine( UCInputLine, WhichMinMax, MinMaxString, Value, DefString, ErrLevel );
//							if ( ErrLevel > 0 ) {
//								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Error in Autocalculate designation -- invalid number=" + UCInputLine, EchoInputFile );
//								errFlag = true;
//							}
//						}
//						break;
//						case 'D':
//						if ( has_prefix( UCInputLine, "\\DEFAULT" ) ) { // Default arg
//							// WhichMinMax, MinMaxString not filled here
//							Default = true;
//							ProcessMinMaxDefLine( InputLine.substr( Pos ), WhichMinMax, MinMaxString, Value, DefString, ErrLevel );
//							if ( ( ! RetainCase ) && ( ! DefString.empty() ) ) uppercase( DefString );
//							if ( ErrLevel > 1 ) {
//								ShowContinueError( "Blank Default Field Encountered", EchoInputFile );
//								errFlag = true;
//							}
//						}
//						break;
//						case 'E':
//						if ( has_prefix( UCInputLine, "\\EXTENSIBLE" ) ) { // Extensible arg
//							ExtensibleObject = true;
//							if ( UCInputLine[ 11 ] != ':' ) {
//								ShowFatalError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal definition for extensible object, should be \"\\extensible:<num>\"", EchoInputFile );
//							} else { // process number
//								std::string const number_str( UCInputLine.substr( 12 ) );
//								NSpace = scan( number_str, " !" );
//								if ( NSpace != std::string::npos ) {
//									ExtensibleNumFields = int( ProcessNumber( number_str.substr( 0, NSpace ), errFlag ) );
//									if ( errFlag ) {
//										ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal Number for \\extensible:<num>", EchoInputFile );
//									}
//								} else {
//									ExtensibleNumFields = 0.0;
//									errFlag = false;
//								}
//							}
//						}
//						break;
//						case 'F':
//						if ( has_prefix( UCInputLine, "\\FIELD" ) ) {
//							// Capture Field Name
//							CurrentFieldName = InputLine.substr( Slash + 6 );
//							strip( CurrentFieldName );
//							P1 = scan( CurrentFieldName, '!' );
//							if ( P1 != std::string::npos ) CurrentFieldName.erase( P1 );
//							FieldSet = true;
//						}
//						break;
//						case 'M':
//						if ( has_prefix( UCInputLine, "\\MIN-FIELDS" ) ) { // Min-Fields arg
//							//RequiredField = true;
//							NSpace = FindNonSpace( UCInputLine.substr( 11 ) );
//							if ( NSpace == std::string::npos ) {
//								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Need number for \\Min-Fields", EchoInputFile );
//								errFlag = true;
//								MinimumNumberOfFields = 0;
//							} else {
//								std::string const number_str( UCInputLine.substr( 11 + NSpace ) );
//								NSpace = scan( number_str, " !" );
//								if ( NSpace == std::string::npos ) {
//									MinimumNumberOfFields = int( ProcessNumber( number_str, errFlag ) );
//								} else {
//									MinimumNumberOfFields = int( ProcessNumber( number_str.substr( 0, NSpace ), errFlag ) );
//								}
//								if ( errFlag ) {
//									ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal Number for \\Min-Fields", EchoInputFile );
//								}
//							}
//						} else if ( has_prefix( UCInputLine, "\\MINIMUM" ) || has_prefix( UCInputLine, "\\MAXIMUM" ) ) { // Min/Max args
//							MinMax = true;
//							ProcessMinMaxDefLine( UCInputLine, WhichMinMax, MinMaxString, Value, DefString, ErrLevel );
//							if ( ErrLevel > 0 ) {
//								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Error in Minimum/Maximum designation -- invalid number=" + UCInputLine, EchoInputFile );
//								errFlag = true;
//							}
//						}
//						break;
//						case 'O':
//						if ( has_prefix( UCInputLine, "\\OBSOLETE" ) ) { // Obsolete arg
//							NSpace = index( UCInputLine.substr( 9 ), "=>" );
//							if ( NSpace == std::string::npos ) {
//								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Need replacement object for \\Obsolete objects", EchoInputFile );
//								errFlag = true;
//							} else {
//								std::string const name_str( InputLine.substr( Pos + 9 + NSpace + 2 ) );
//								NSpace = scan( name_str, '!' );
//								if ( NSpace == std::string::npos ) {
//									ReplacementName = trimmed( name_str );
//								} else {
//									ReplacementName = trimmed( name_str.substr( 0, NSpace ) );
//								}
//								ObsoleteObject = true;
//							}
//						}
//						break;
//						case 'R':
//						if ( has_prefix( UCInputLine, "\\REQUIRED-FIELD" ) ) { // Required-field arg
//							RequiredField = true;
//						} else if ( has_prefix( UCInputLine, "\\REQUIRED-OBJECT" ) ) { // Required-object arg
//							RequiredObject = true;
//						} else if ( has_prefix( UCInputLine, "\\RETAINCASE" ) ) {
//							RetainCase = true;
//						}
//						break;
//						case 'U':
//						if ( has_prefix( UCInputLine, "\\UNIQUE-OBJECT" ) ) { // Unique-object arg
//							UniqueObject = true;
//						}
//						break;
//					}
//				}
//			}
//		}
//		if ( errFlag ) {
//			ErrorsFound = true;
//		}
//
//	}

// 	void
// 	EnergyPlus::InputProcessor::ExtendObjectDefinition(
// 		int const ObjectNum, // Number of the object definition to be extended.
// 		int & NumNewArgsLimit // Number of the parameters after extension
// 		)
// 	{

// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda Lawrie
// 	//       DATE WRITTEN   Sep 2008
// 	//       MODIFIED       na
// 	//       RE-ENGINEERED  na

// 	// PURPOSE OF THIS SUBROUTINE:
// 	// This routine expands the object definition according to the extensible "rules" entered
// 	// by the developer.  The developer should enter the number of fields to be duplicated.
// 	// See References section for examples.

// 	// METHODOLOGY EMPLOYED:
// 	// The routine determines the type of the fields to be added (A or N) and reallocates the
// 	// appropriate arrays in the object definition structure.

// 	// REFERENCES:
// 	// Extensible objects have a \extensible:<num> specification
// 	// \extensible:3 -- the last 3 fields are "extended"
// 	// Works on this part of the definition:
// 	//   INTEGER :: NumParams                       =0   ! Number of parameters to be processed for each object
// 	//   INTEGER :: NumAlpha                        =0   ! Number of Alpha elements in the object
// 	//   INTEGER :: NumNumeric                      =0   ! Number of Numeric elements in the object
// 	//   LOGICAL(1), ALLOCATABLE, DIMENSION(:) :: AlphaOrNumeric ! Positionally, whether the argument
// 	//                                                           ! is alpha (true) or numeric (false)
// 	//   LOGICAL(1), ALLOCATABLE, DIMENSION(:) :: ReqField ! True for required fields
// 	//   LOGICAL(1), ALLOCATABLE, DIMENSION(:) :: AlphRetainCase ! true if retaincase is set for this field (alpha fields only)
// 	//   CHARACTER(len=MaxNameLength+40),  &
// 	//               ALLOCATABLE, DIMENSION(:) :: AlphFieldChks ! Field names for alphas
// 	//   CHARACTER(len=MaxNameLength),  &
// 	//               ALLOCATABLE, DIMENSION(:) :: AlphFieldDefs ! Defaults for alphas
// 	//   TYPE(RangeCheckDef), ALLOCATABLE, DIMENSION(:) :: NumRangeChks  ! Used to range check and default numeric fields
// 	//   INTEGER :: LastExtendAlpha                 =0   ! Count for extended alpha fields
// 	//   INTEGER :: LastExtendNum                   =0   ! Count for extended numeric fields

// 	// USE STATEMENTS:
// 	// na

// 	// Locals
// 	// SUBROUTINE ARGUMENT DEFINITIONS:

// 	// SUBROUTINE PARAMETER DEFINITIONS:
// 	int const NewAlloc( 1000 ); // number of new items to allocate (* number of fields)

// 	// INTERFACE BLOCK SPECIFICATIONS:
// 	// na

// 	// DERIVED TYPE DEFINITIONS:
// 	// na

// 	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
// 	int NumAlphaField;
// 	int NumNumericField;
// 	int NumNewAlphas;
// 	int NumNewNumerics;
// 	int NumNewParams;
// 	int NumExtendFields;
// 	int NumParams;
// 	int Count;
// 	//  LOGICAL :: MaxArgsChanged
// 	std::string charout;
// 	static std::string CurObject;

// 	gio::write( EchoInputFile, fmtA ) << "Attempting to auto-extend object=" + ObjectDef( ObjectNum ).Name;
// 	if ( CurObject != ObjectDef( ObjectNum ).Name ) {
// 		DisplayString( "Auto-extending object=\"" + ObjectDef( ObjectNum ).Name + "\", input processing may be slow." );
// 		CurObject = ObjectDef( ObjectNum ).Name;
// 	}

// 	NumAlphaField = 0;
// 	NumNumericField = 0;
// 	NumParams = ObjectDef( ObjectNum ).NumParams;
// 	Count = NumParams - ObjectDef( ObjectNum ).ExtensibleNum + 1;
// 	//  MaxArgsChanged=.FALSE.

// 	Array1D_bool AorN( ObjectDef( ObjectNum ).ExtensibleNum, false );
// 	for ( int Loop = Count, Item = 1; Loop <= NumParams; ++Loop, ++Item ) {
// 		bool const AON_Loop( ObjectDef( ObjectNum ).AlphaOrNumeric( Loop ) );
// 		if ( AON_Loop ) {
// 			++NumAlphaField;
// 		} else {
// 			++NumNumericField;
// 		}
// 		AorN( Item ) = AON_Loop;
// 	}
// 	NumNewAlphas = NumAlphaField * NewAlloc;
// 	NumNewNumerics = NumNumericField * NewAlloc;
// 	NumNewParams = NumParams + NumNewAlphas + NumNewNumerics;
// 	NumExtendFields = NumAlphaField + NumNumericField;
// 	ObjectDef( ObjectNum ).AlphaOrNumeric.redimension( NumNewParams, false );
// 	for ( int Loop = NumParams + 1; Loop <= NumNewParams; Loop += NumExtendFields ) {
// 		ObjectDef( ObjectNum ).AlphaOrNumeric( {Loop,Loop+NumExtendFields-1} ) = AorN;
// 	}
// 	AorN.deallocate(); // done with this object AorN array.

// 	// required fields -- can't be extended and required.
// 	ObjectDef( ObjectNum ).ReqField.redimension( NumNewParams, false );

// 	ObjectDef( ObjectNum ).AlphRetainCase.redimension( NumNewParams, false );

// 	if ( NumAlphaField > 0 ) {
// 		ObjectDef( ObjectNum ).AlphFieldChks.redimension( ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas );
// 		for ( int Loop = ObjectDef( ObjectNum ).NumAlpha + 1, Loop_end = ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas; Loop <= Loop_end; ++Loop ) {
// 			++ObjectDef( ObjectNum ).LastExtendAlpha;
// 			charout = IPTrimSigDigits( ObjectDef( ObjectNum ).LastExtendAlpha );
// 			ObjectDef( ObjectNum ).AlphFieldChks( Loop ) = "Extended Alpha Field " + charout;
// 		}

// 		ObjectDef( ObjectNum ).AlphFieldDefs.redimension( ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas );

// 		if ( ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas > MaxAlphaArgsFound ) {
// 			// must redimension LineItem args
// 			LineItem.Alphas.redimension( ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas );

// 			LineItem.AlphBlank.redimension( ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas, true );

// 			MaxAlphaArgsFound = ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas;
// 			//      MaxArgsChanged=.TRUE.
// 		}

// 	}

// 	if ( NumNumericField > 0 ) {
// 		ObjectDef( ObjectNum ).NumRangeChks.redimension( ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics );
// 		for ( int Loop = ObjectDef( ObjectNum ).NumNumeric + 1, Loop_end = ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics; Loop <= Loop_end; ++Loop ) {
// 			ObjectDef( ObjectNum ).NumRangeChks( Loop ).FieldNumber = Loop;
// 			++ObjectDef( ObjectNum ).LastExtendNum;
// 			charout = IPTrimSigDigits( ObjectDef( ObjectNum ).LastExtendNum );
// 			ObjectDef( ObjectNum ).NumRangeChks( Loop ).FieldName = "Extended Numeric Field " + charout;
// 		}

// 		if ( ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics > MaxNumericArgsFound ) {
// 			// must redimension LineItem args
// 			LineItem.Numbers.redimension( ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics, 0.0 );

// 			LineItem.NumBlank.redimension( ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics, true );

// 			MaxNumericArgsFound = ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics;
// 			//      MaxArgsChanged=.TRUE.
// 		}

// 	}

// 	ObjectDef( ObjectNum ).NumParams = NumNewParams;
// 	NumNewArgsLimit = NumNewParams;
// 	ObjectDef( ObjectNum ).NumAlpha += NumNewAlphas;
// 	ObjectDef( ObjectNum ).NumNumeric += NumNewNumerics;
// }

    Real64
    EnergyPlus::InputProcessor::ProcessNumber(
            std::string const &String,
            bool &ErrorFlag
    ) {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function processes a string that should be numeric and
        // returns the real value of the string.

        // METHODOLOGY EMPLOYED:
        // FUNCTION ProcessNumber translates the argument (a string)
        // into a real number.  The string should consist of all
        // numeric characters (except a decimal point).  Numerics
        // with exponentiation (i.e. 1.2345E+03) are allowed but if
        // it is not a valid number an error message along with the
        // string causing the error is printed out and 0.0 is returned
        // as the value.

        // REFERENCES:
        // List directed Fortran input/output.

        // USE STATEMENTS:
        // na

        // Return value

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const ValidNumerics("0123456789.+-EeDd"); // This had a trailing tab character: Not sure why

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:


        Real64 rProcessNumber = 0.0;
        //  Make sure the string has all what we think numerics should have
        std::string const PString(stripped(String));
        std::string::size_type const StringLen(PString.length());
        ErrorFlag = false;
        if (StringLen == 0) return rProcessNumber;
        int IoStatus(0);
        if (PString.find_first_not_of(ValidNumerics) == std::string::npos) {
            {
                IOFlags flags;
                gio::read(PString, fmtLD, flags) >> rProcessNumber;
                IoStatus = flags.ios();
            }
            ErrorFlag = false;
        } else {
            rProcessNumber = 0.0;
            ErrorFlag = true;
        }
        if (IoStatus != 0) {
            rProcessNumber = 0.0;
            ErrorFlag = true;
        }

        return rProcessNumber;

    }

//void
//EnergyPlus::InputProcessor::ProcessMinMaxDefLine(
//		std::string const & partLine, // part of input line starting \min or \max  Not uppercase if \default
//		int & WhichMinMax, // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
//		std::string & MinMaxString,
//		Real64 & Value,
//		std::string & DefaultString,
//		int & ErrLevel
//		)
//{
//
//	// SUBROUTINE INFORMATION:
//	//       AUTHOR         Linda Lawrie
//	//       DATE WRITTEN   July 2000
//	//       MODIFIED       na
//	//       RE-ENGINEERED  na
//
//	// PURPOSE OF THIS SUBROUTINE:
//	// This subroutine processes the IDD lines that start with
//	// \minimum or \maximum and set up the parameters so that it can
//	// be automatically checked.
//
//	// METHODOLOGY EMPLOYED:
//	// na
//
//	// REFERENCES:
//	// IDD Statements.
//	//  \minimum         Minimum that includes the following value
//	//  i.e. min >=
//	//  \minimum>        Minimum that must be > than the following value
//	//  \maximum         Maximum that includes the following value
//	//  i.e. max <=
//	//  \maximum<        Maximum that must be < than the following value
//	//  \default         Default for field (when field is blank)
//
//	// USE STATEMENTS:
//	// na
//
//	// Locals
//	// SUBROUTINE ARGUMENT DEFINITIONS:
//
//	// SUBROUTINE PARAMETER DEFINITIONS:
//	// na
//
//	// INTERFACE BLOCK SPECIFICATIONS
//	// na
//
//	// DERIVED TYPE DEFINITIONS
//	// na
//
//	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
//
//	ErrLevel = 0;
//	std::string::size_type Pos = scan( partLine, ' ' );
//
//	{ auto const maxMinDefLine( uppercased( partLine.substr( 0, 4 ) ) );
//
//		if ( maxMinDefLine == "\\MIN" ) {
//			WhichMinMax = 1;
//			if ( has( partLine, '>' ) ) {
//				Pos = scan( partLine, '>' ) + 1;
//				WhichMinMax = 2;
//			}
//			if ( WhichMinMax == 1 ) {
//				MinMaxString = ">=";
//			} else {
//				MinMaxString = ">";
//			}
//
//		} else if ( maxMinDefLine == "\\MAX" ) {
//			WhichMinMax = 3;
//			if ( has( partLine, '<' ) ) {
//				Pos = scan( partLine, '<' ) + 1;
//				WhichMinMax = 4;
//			}
//			if ( WhichMinMax == 3 ) {
//				MinMaxString = "<=";
//			} else {
//				MinMaxString = "<";
//			}
//
//		} else if ( maxMinDefLine == "\\DEF" ) {
//			WhichMinMax = 5;
//			MinMaxString = BlankString;
//
//		} else if ( maxMinDefLine == "\\AUT" ) {
//			WhichMinMax = 6;
//			MinMaxString = BlankString;
//
//		} else {
//			WhichMinMax = 0; // invalid field
//			MinMaxString = BlankString;
//			Value = -999999.0;
//
//		}}
//
//		if ( WhichMinMax != 0 ) {
//		if ( Pos == std::string::npos ) Pos = partLine.length(); // So that NSpace=npos
//		std::string::size_type NSpace = FindNonSpace( partLine.substr( Pos ) );
//		if ( NSpace == std::string::npos ) {
//			if ( WhichMinMax != 6 ) { // Only autosize/autocalculate can't have argument
//				ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + "Min/Max/Default field cannot be blank -- must have value", EchoInputFile );
//			ErrLevel = 2;
//		} else if ( has_prefix( partLine, "\\AUTOS" ) ) {
//			Value = DefAutoSizeValue;
//		} else if ( has_prefix( partLine, "\\AUTOC" ) ) {
//			Value = DefAutoCalculateValue;
//		}
//	} else {
//		Pos += NSpace;
//		NSpace = scan( partLine.substr( Pos ), " !" );
//		MinMaxString += partLine.substr( Pos, NSpace );
//		bool errFlag;
//		Value = ProcessNumber( partLine.substr( Pos, NSpace ), errFlag );
//		if ( errFlag ) ErrLevel = 1;
//		NSpace = scan( partLine.substr( Pos ), '!' );
//		if ( NSpace != std::string::npos ) {
//			DefaultString = partLine.substr( Pos, NSpace - 1 );
//		} else {
//			DefaultString = partLine.substr( Pos );
//		}
//		strip( DefaultString );
//		if ( DefaultString == BlankString ) {
//			if ( WhichMinMax == 6 ) {
//				if ( has_prefix( partLine, "\\AUTOS" ) ) {
//					Value = DefAutoSizeValue;
//				} else {
//					Value = DefAutoCalculateValue;
//				}
//			} else {
//				ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + "Min/Max/Default field cannot be blank -- must have value", EchoInputFile );
//				ErrLevel = 2;
//			}
//		}
//	}
//}
//
//}

    int
    EnergyPlus::InputProcessor::FindItemInList(
            std::string const &String,
            Array1_string const &ListOfItems,
            int const NumItems
    ) {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up a string in a similar list of
        // items and returns the index of the item in the list, if
        // found.  This routine is not case insensitive and doesn't need
        // for most inputs -- they are automatically turned to UPPERCASE.
        // If you need case insensitivity use FindItem.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        for (int Count = 1; Count <= NumItems; ++Count) {
            if (String == ListOfItems(Count)) return Count;
        }
        return 0; // Not found
    }

    int
    EnergyPlus::InputProcessor::FindItemInList(
            std::string const &String,
            Array1S_string const ListOfItems,
            int const NumItems
    ) {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up a string in a similar list of
        // items and returns the index of the item in the list, if
        // found.  This routine is not case insensitive and doesn't need
        // for most inputs -- they are automatically turned to UPPERCASE.
        // If you need case insensitivity use FindItem.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        for (int Count = 1; Count <= NumItems; ++Count) {
            if (String == ListOfItems(Count)) return Count;
        }
        return 0; // Not found
    }

    int
    EnergyPlus::InputProcessor::FindItemInSortedList(
            std::string const &String,
            Array1S_string const ListOfItems,
            int const NumItems
    ) {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up a string in a similar list of
        // items and returns the index of the item in the list, if
        // found.  This routine is case insensitive.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int Probe(0);
        int LBnd(0);
        int UBnd(NumItems + 1);
        bool Found(false);
        while ((!Found) || (Probe != 0)) {
            Probe = (UBnd - LBnd) / 2;
            if (Probe == 0) break;
            Probe += LBnd;
            if (equali(String, ListOfItems(Probe))) {
                Found = true;
                break;
            } else if (lessthani(String, ListOfItems(Probe))) {
                UBnd = Probe;
            } else {
                LBnd = Probe;
            }
        }
        return Probe;
    }

    int
    EnergyPlus::InputProcessor::FindItem(
            std::string const &String,
            Array1D_string const &ListOfItems,
            int const NumItems
    ) {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   April 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up a string in a similar list of
        // items and returns the index of the item in the list, if
        // found.  This routine is case insensitive.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int FindItem = FindItemInList(String, ListOfItems, NumItems);
        if (FindItem != 0) return FindItem;

        for (int Count = 1; Count <= NumItems; ++Count) {
            if (equali(String, ListOfItems(Count))) return Count;
        }
        return 0; // Not found
    }

    int
    EnergyPlus::InputProcessor::FindItem(
            std::string const &String,
            Array1S_string const ListOfItems,
            int const NumItems
    ) {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   April 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up a string in a similar list of
        // items and returns the index of the item in the list, if
        // found.  This routine is case insensitive.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int FindItem = FindItemInList(String, ListOfItems, NumItems);
        if (FindItem != 0) return FindItem;

        for (int Count = 1; Count <= NumItems; ++Count) {
            if (equali(String, ListOfItems(Count))) return Count;
        }
        return 0; // Not found
    }

    std::string
    EnergyPlus::InputProcessor::MakeUPPERCase(std::string const &InputString) {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   September 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This function returns the Upper Case representation of the InputString.

        // METHODOLOGY EMPLOYED:
        // Uses the Intrinsic SCAN function to scan the lowercase representation of
        // characters (DataStringGlobals) for each character in the given string.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:
        // MaxInputLineLength because of PowerStation Compiler
        // otherwise could say (CHARACTER(len=LEN(InputString))

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        std::string ResultString(InputString);

        for (std::string::size_type i = 0, e = len(InputString); i < e; ++i) {
            int const curCharVal = int(InputString[i]);
            if ((97 <= curCharVal && curCharVal <= 122) ||
                (224 <= curCharVal && curCharVal <= 255)) { // lowercase ASCII and accented characters
                ResultString[i] = char(curCharVal - 32);
            }
        }

        return ResultString;

    }

    void
    EnergyPlus::InputProcessor::VerifyName(
            std::string const &NameToVerify,
            Array1D_string const &NamesList,
            int const NumOfNames,
            bool &ErrorFound,
            bool &IsBlank,
            std::string const &StringToDisplay
    ) {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine verifys that a new name can be added to the
        // list of names for this item (i.e., that there isn't one of that
        // name already and that this name is not blank).

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Found;

        ErrorFound = false;
        if (NumOfNames > 0) {
            Found = FindItem(NameToVerify, NamesList, NumOfNames);
            if (Found != 0) {
                ShowSevereError(StringToDisplay + ", duplicate name=" + NameToVerify);
                ErrorFound = true;
            }
        }

        if (NameToVerify.empty()) {
            ShowSevereError(StringToDisplay + ", cannot be blank");
            ErrorFound = true;
            IsBlank = true;
        } else {
            IsBlank = false;
        }

    }

    void
    EnergyPlus::InputProcessor::VerifyName(
            std::string const &NameToVerify,
            Array1S_string const NamesList,
            int const NumOfNames,
            bool &ErrorFound,
            bool &IsBlank,
            std::string const &StringToDisplay
    ) {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine verifys that a new name can be added to the
        // list of names for this item (i.e., that there isn't one of that
        // name already and that this name is not blank).

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Found;

        ErrorFound = false;
        if (NumOfNames > 0) {
            Found = FindItem(NameToVerify, NamesList, NumOfNames);
            if (Found != 0) {
                ShowSevereError(StringToDisplay + ", duplicate name=" + NameToVerify);
                ErrorFound = true;
            }
        }

        if (NameToVerify.empty()) {
            ShowSevereError(StringToDisplay + ", cannot be blank");
            ErrorFound = true;
            IsBlank = true;
        } else {
            IsBlank = false;
        }

    }

    void
    EnergyPlus::InputProcessor::RangeCheck(
            bool &ErrorsFound, // Set to true if error detected
            std::string const &WhatFieldString, // Descriptive field for string
            std::string const &WhatObjectString, // Descriptive field for object, Zone Name, etc.
            std::string const &ErrorLevel, // 'Warning','Severe','Fatal')
            Optional_string_const LowerBoundString, // String for error message, if applicable
            Optional_bool_const LowerBoundCondition, // Condition for error condition, if applicable
            Optional_string_const UpperBoundString, // String for error message, if applicable
            Optional_bool_const UpperBoundCondition, // Condition for error condition, if applicable
            Optional_string_const ValueString, // Value with digits if to be displayed with error
            Optional_string_const WhatObjectName // ObjectName -- used for error messages
    ) {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   July 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is a general purpose "range check" routine for GetInput routines.
        // Using the standard "ErrorsFound" logical, this routine can produce a reasonable
        // error message to describe the situation in addition to setting the ErrorsFound variable
        // to true.

        std::string ErrorString; // Uppercase representation of ErrorLevel
        std::string Message1;
        std::string Message2;

        bool Error = false;
        if (present(UpperBoundCondition)) {
            if (!UpperBoundCondition) Error = true;
        }
        if (present(LowerBoundCondition)) {
            if (!LowerBoundCondition) Error = true;
        }

        if (Error) {
            ConvertCaseToUpper(ErrorLevel, ErrorString);
            Message1 = WhatObjectString;
            if (present(WhatObjectName)) Message1 += "=\"" + WhatObjectName + "\", out of range data";
            Message2 = "Out of range value field=" + WhatFieldString;
            if (present(ValueString)) Message2 += ", Value=[" + ValueString + ']';
            Message2 += ", range={";
            if (present(LowerBoundString)) Message2 += LowerBoundString;
            if (present(LowerBoundString) && present(UpperBoundString)) {
                Message2 += " and " + UpperBoundString;
            } else if (present(UpperBoundString)) {
                Message2 += UpperBoundString;
            }
            Message2 += "}";

            {
                auto const errorCheck(ErrorString[0]);

                if ((errorCheck == 'W') || (errorCheck == 'w')) {
                    ShowWarningError(Message1);
                    ShowContinueError(Message2);

                } else if ((errorCheck == 'S') || (errorCheck == 's')) {
                    ShowSevereError(Message1);
                    ShowContinueError(Message2);
                    ErrorsFound = true;

                } else if ((errorCheck == 'F') || (errorCheck == 'f')) {
                    ShowSevereError(Message1);
                    ShowContinueError(Message2);
                    ShowFatalError("Program terminates due to preceding condition(s).");

                } else {
                    ShowSevereError(Message1);
                    ShowContinueError(Message2);
                    ErrorsFound = true;
                }
            }
        }
    }

// 	void
// 	EnergyPlus::InputProcessor::InternalRangeCheck(
// 		Real64 const Value,
// 		int const FieldNumber,
// 		int const WhichObject,
// 		std::string const & PossibleAlpha,
// 		bool const AutoSizable,
// 		bool const AutoCalculatable
// 		)
// 	{

// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda Lawrie
// 	//       DATE WRITTEN   July 2000
// 	//       MODIFIED       na
// 	//       RE-ENGINEERED  na

// 	// PURPOSE OF THIS SUBROUTINE:
// 	// This subroutine is an internal range check that checks fields which have
// 	// the \min and/or \max values set for appropriate values.

// 	// METHODOLOGY EMPLOYED:
// 	// na

// 	// REFERENCES:
// 	// na

// 	// USE STATEMENTS:
// 	// na

// 	// Locals
// 	// SUBROUTINE ARGUMENT DEFINITIONS:

// 	// SUBROUTINE PARAMETER DEFINITIONS:
// 	// na

// 	// INTERFACE BLOCK SPECIFICATIONS
// 	// na

// 	// DERIVED TYPE DEFINITIONS
// 	// na

// 	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
// 		std::string FieldString;
// 		std::string FieldNameString;
// 		std::string ValueString;
// 		std::string Message;

// 		bool Error = false;
// 		if ( ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 1 ) == 1 ) {
// 			if ( Value < ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).MinMaxValue( 1 ) ) Error = true;
// 		} else if ( ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 1 ) == 2 ) {
// 			if ( Value <= ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).MinMaxValue( 1 ) ) Error = true;
// 		}
// 		if ( ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 2 ) == 3 ) {
// 			if ( Value > ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).MinMaxValue( 2 ) ) Error = true;
// 		} else if ( ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 2 ) == 4 ) {
// 			if ( Value >= ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).MinMaxValue( 2 ) ) Error = true;
// 		}

// 		if ( Error ) {
// 			if ( ! ( AutoSizable && Value == ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).AutoSizeValue ) && ! ( AutoCalculatable && Value == ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).AutoCalculateValue ) ) {
// 				++NumOutOfRangeErrorsFound;
// 				if ( ReportRangeCheckErrors ) {
// 					FieldString = IPTrimSigDigits( FieldNumber );
// 					FieldNameString = ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).FieldName;
// 					gio::write( ValueString, "(F20.5)" ) << Value;
// 					strip( ValueString );
// 					if ( ! FieldNameString.empty() ) {
// 						Message = "Out of range value Numeric Field#" + FieldString + " (" + FieldNameString + "), value=" + ValueString + ", range={";
// 				} else { // Field Name not recorded
// 					Message = "Out of range value Numeric Field#" + FieldString + ", value=" + ValueString + ", range={";
// 				}
// 				if ( ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 1 ) != 0 ) Message += ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).MinMaxString( 1 );
// 				if ( ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 1 ) != 0 && ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 2 ) != 0 ) {
// 					Message += " and " + ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).MinMaxString( 2 );
// 				} else if ( ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 2 ) != 0 ) {
// 					Message += ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).MinMaxString( 2 );
// 				}
// 				Message += "}, in " + ObjectDef( WhichObject ).Name;
// 				if ( ObjectDef( WhichObject ).NameAlpha1 ) {
// 					Message += "=" + PossibleAlpha;
// 				}
// 				ShowSevereError( Message, EchoInputFile );
// 			}
// 		}
// 	}
// }

//void
//EnergyPlus::InputProcessor::TurnOnReportRangeCheckErrors()
//{
//
//	// SUBROUTINE INFORMATION:
//	//       AUTHOR         Linda Lawrie
//	//       DATE WRITTEN   July 2000
//	//       MODIFIED       na
//	//       RE-ENGINEERED  na
//
//	// PURPOSE OF THIS SUBROUTINE:
//	// This subroutine turns on the logical to report range check errors
//	// directly out of the InputProcessor.
//
//	// METHODOLOGY EMPLOYED:
//	// na
//
//	// REFERENCES:
//	// na
//
//	// USE STATEMENTS:
//	// na
//
//	// SUBROUTINE ARGUMENT DEFINITIONS:
//	// na
//
//	// SUBROUTINE PARAMETER DEFINITIONS:
//	// na
//
//	// INTERFACE BLOCK SPECIFICATIONS
//	// na
//
//	// DERIVED TYPE DEFINITIONS
//	// na
//
//	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
//	// na
//
//	ReportRangeCheckErrors = true;
//
//}

//void
//EnergyPlus::InputProcessor::TurnOffReportRangeCheckErrors()
//{
//
//	// SUBROUTINE INFORMATION:
//	//       AUTHOR         Linda Lawrie
//	//       DATE WRITTEN   July 20000
//	//       MODIFIED       na
//	//       RE-ENGINEERED  na
//
//	// PURPOSE OF THIS SUBROUTINE:
//	// This subroutine turns off the logical to report range check errors
//	// directly out of the InputProcessor.
//
//	// METHODOLOGY EMPLOYED:
//	// na
//
//	// REFERENCES:
//	// na
//
//	// USE STATEMENTS:
//	// na
//
//	// SUBROUTINE ARGUMENT DEFINITIONS:
//	// na
//
//	// SUBROUTINE PARAMETER DEFINITIONS:
//	// na
//
//	// INTERFACE BLOCK SPECIFICATIONS
//	// na
//
//	// DERIVED TYPE DEFINITIONS
//	// na
//
//	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
//	// na
//
//	ReportRangeCheckErrors = false;
//
//}

    int
    EnergyPlus::InputProcessor::GetNumRangeCheckErrorsFound() {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   July 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function returns the number of OutOfRange errors found during
        // input processing.
//	return NumOutOfRangeErrorsFound;
        // TODO: Fix this
        return 0;
    }

//==============================================================================
// The following routines allow access to the definition lines of the IDD and
// thus can be used to "report" on expected arguments for the Input Processor.

//int
//EnergyPlus::InputProcessor::GetNumObjectsInIDD()
//{
//
//	// FUNCTION INFORMATION:
//	//       AUTHOR         Linda K. Lawrie
//	//       DATE WRITTEN   May 1998
//	//       MODIFIED       na
//	//       RE-ENGINEERED  na
//
//	// PURPOSE OF THIS SUBROUTINE:
//	// This subroutine returns the number of objects found in the IDD and
//	// can be used to allocate the array for determining the definitions.
//
//	// METHODOLOGY EMPLOYED:
//	// Essentially allows outside access to an internal variable of the InputProcessor.
//	// Used primarily by utility programs that use the InputProcessor outside of the
//	// "true" EnergyPlus code.
//
//	// REFERENCES:
//	// na
//
//	// USE STATEMENTS:
//	// na
//
//	// Return value
//
//	// SUBROUTINE ARGUMENT DEFINITIONS:
//	// na
//
//	// SUBROUTINE PARAMETER DEFINITIONS:
//	// na
//
//	// INTERFACE BLOCK SPECIFICATIONS
//	// na
//
//	// DERIVED TYPE DEFINITIONS
//	// na
//
//	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
//	// na
//
//	return NumObjectDefs;
//
//}

// void
// EnergyPlus::InputProcessor::GetListOfObjectsInIDD(
// 		Array1S_string ObjectNames, // List of Object Names (from IDD)
// 		int & Number // Number in List
// 		)
// {

// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda K. Lawrie
// 	//       DATE WRITTEN   May 1998
// 	//       MODIFIED       na
// 	//       RE-ENGINEERED  na

// 	// PURPOSE OF THIS SUBROUTINE:
// 	// This subroutine returns the list of Object names that occur in the IDD.

// 	// METHODOLOGY EMPLOYED:
// 	// Essentially allows outside access to an internal variable of the InputProcessor.
// 	// Used primarily by utility programs that use the InputProcessor outside of the
// 	// "true" EnergyPlus code.

// 	for ( int i = 1; i <= NumObjectDefs; ++i ) ObjectNames( i ) = ObjectDef( i ).Name;
// 		Number = NumObjectDefs;
// }

// void
// EnergyPlus::InputProcessor::GetObjectDefInIDD(
// 		std::string const & ObjectWord, // Object for definition
// 		int & NumArgs, // How many arguments (max) this Object can have
// 		Array1S_bool AlphaOrNumeric, // Array designating Alpha (true) or Numeric (false) for each
// 		Array1S_bool RequiredFields, // Array designating RequiredFields (true) for each argument
// 		int & MinNumFields // Minimum Number of Fields to be returned to Get routines
// 		)
// {

// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda K. Lawrie
// 	//       DATE WRITTEN   May 1998
// 	//       MODIFIED       na
// 	//       RE-ENGINEERED  na

// 	// PURPOSE OF THIS SUBROUTINE:
// 	// This subroutine returns the "definition" of an Object from the IDD.  This is
// 	// the "maximum" definition with total number of arguments, and whether each argument
// 	// is "alpha" or "numeric".

// 	// METHODOLOGY EMPLOYED:
// 	// Essentially allows outside access to an internal variable of the InputProcessor.
// 	// Used primarily by utility programs that use the InputProcessor outside of the
// 	// "true" EnergyPlus code.

// 	int Which; // to determine which object definition to use

// 	if ( SortedIDD ) {
// 		Which = FindItemInSortedList( ObjectWord, ListOfObjects, NumObjectDefs );
// 		if ( Which != 0 ) Which = iListOfObjects( Which );
// 	} else {
// 		Which = FindItemInList( ObjectWord, ListOfObjects, NumObjectDefs );
// 	}
// 	NumArgs = ObjectDef( Which ).NumParams;
// 	AlphaOrNumeric( {1,NumArgs} ) = ObjectDef( Which ).AlphaOrNumeric( {1,NumArgs} );
// 	RequiredFields( {1,NumArgs} ) = ObjectDef( Which ).ReqField( {1,NumArgs} );
// 	MinNumFields = ObjectDef( Which ).MinNumFields;

// }

    void
    EnergyPlus::InputProcessor::GetObjectDefMaxArgs(
            std::string const &ObjectWord, // Object for definition
            int &NumArgs, // How many arguments (max) this Object can have
            int &NumAlpha, // How many Alpha arguments (max) this Object can have
            int &NumNumeric // How many Numeric arguments (max) this Object can have
    ) {
        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine returns maximum argument limits (total, alphas, numerics) of an Object from the IDD.
        // These dimensions (not sure what one can use the total for) can be used to dynamically dimension the
        // arrays in the GetInput routines.

        NumArgs = 0;
        NumAlpha = 0;
        NumNumeric = 0;
        json *object;
        if (schema["properties"].find(ObjectWord) == schema["properties"].end()) {
            auto tmp_umit = InputProcessor::idf_parser.case_insensitive_keys.find(MakeUPPERCase(ObjectWord));
            if (tmp_umit == InputProcessor::idf_parser.case_insensitive_keys.end()) {
                ShowSevereError("GetObjectDefMaxArgs: Did not find object=\"" + ObjectWord + "\" in list of objects.");
                return;
            }
            object = &schema["properties"][tmp_umit->second];
        } else {
            object = &schema["properties"][ObjectWord];
        }
//		if ( schema[ "properties" ].find( ObjectWord ) == schema[ "properties" ].end() ) {
//			ShowSevereError( "GetObjectDefMaxArgs: Did not find object=\"" + ObjectWord + "\" in list of objects." );
//			return;
//		}

//		const json & object = schema[ "properties" ][ ObjectWord ];
        const json &legacy_idd = object->at("legacy_idd");

        json *objects;
        if (jdf.find(ObjectWord) == jdf.end()) {
            auto tmp_umit = InputProcessor::idf_parser.case_insensitive_keys.find(MakeUPPERCase(ObjectWord));
            if (tmp_umit == InputProcessor::idf_parser.case_insensitive_keys.end()) {
                ShowSevereError("GetObjectDefMaxArgs: Did not find object=\"" + ObjectWord + "\" in list of objects.");
                return;
            }
            objects = &jdf[tmp_umit->second];
        } else {
            objects = &jdf[ObjectWord];
        }

//		json const objects = jdf[ ObjectWord ];
        size_t max_size = 0;
        for (auto const obj : *objects) {
//			json const ob = obj.value();
            if (obj.find("extensions") != obj.end()) {
                auto const size = obj["extensions"].size();
                if (size > max_size) max_size = size;
            }
        }

        if (legacy_idd.find("alphas") != legacy_idd.end()) {
            json const alphas = legacy_idd["alphas"];
            if (alphas.find("fields") != alphas.end()) {
                NumAlpha += alphas["fields"].size();
            }
            if (alphas.find("extensions") != alphas.end()) {
                NumAlpha += alphas["extensions"].size() * max_size;
            }
        }
        if (legacy_idd.find("numerics") != legacy_idd.end()) {
            json const numerics = legacy_idd["numerics"];
            if (numerics.find("fields") != numerics.end()) {
                NumNumeric += numerics["fields"].size();
            }
            if (numerics.find("extensions") != numerics.end()) {
                NumNumeric += numerics["extensions"].size() * max_size;
            }
        }
        NumArgs = NumAlpha + NumNumeric;
    }

// int
// EnergyPlus::InputProcessor::GetObjectDefMaxArgs(
// 		std::string const & ObjectWord // Object for definition
// )
// {
// 	if ( schema[ "properties" ].find( ObjectWord ) == schema[ "properties" ].end() ) return 0;
// 	int alpha_args = 0, numeric_args = 0;
// 	const json & legacy_idd = schema[ "properties" ][ ObjectWord ][ "legacy_idd" ];
// 	if ( legacy_idd.find( "alphas" ) != legacy_idd.end() ) alpha_args = legacy_idd[ "alphas" ].size();
// 	if ( legacy_idd.find( "numerics" ) != legacy_idd.end() ) numeric_args = legacy_idd[ "numerics" ].size();
// 	return alpha_args + numeric_args;
// }

// void
// EnergyPlus::InputProcessor::GetIDFRecordsStats(
// 		int & iNumberOfRecords, // Number of IDF Records
// 		int & iNumberOfDefaultedFields, // Number of defaulted fields in IDF
// 		int & iTotalFieldsWithDefaults, // Total number of fields that could be defaulted
// 		int & iNumberOfAutoSizedFields, // Number of autosized fields in IDF
// 		int & iTotalAutoSizableFields, // Total number of autosizeable fields
// 		int & iNumberOfAutoCalcedFields, // Total number of autocalculate fields
// 		int & iTotalAutoCalculatableFields // Total number of autocalculatable fields
// 		)
// {
// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda Lawrie
// 	//       DATE WRITTEN   February 2009
// 	// PURPOSE OF THIS SUBROUTINE:
// 	// This routine provides some statistics on the current IDF, such as number of records, total fields with defaults,
// 	// number of fields that overrode the default (even if it was default value), and similarly for AutoSize.

// 	// METHODOLOGY EMPLOYED:
// 	// Traverses the IDF Records looking at each field vs object definition for defaults and autosize.
// 	int iRecord;
// 	int iField;
// 	int iObjectDef;

// 	iNumberOfRecords = NumIDFRecords;
// 	iNumberOfDefaultedFields = 0;
// 	iTotalFieldsWithDefaults = 0;
// 	iNumberOfAutoSizedFields = 0;
// 	iTotalAutoSizableFields = 0;
// 	iNumberOfAutoCalcedFields = 0;
// 	iTotalAutoCalculatableFields = 0;

// 	for ( iRecord = 1; iRecord <= NumIDFRecords; ++iRecord ) {
// 		if ( IDFRecords( iRecord ).ObjectDefPtr <= 0 || IDFRecords( iRecord ).ObjectDefPtr > NumObjectDefs ) continue;
// 		iObjectDef = IDFRecords( iRecord ).ObjectDefPtr;
// 		for ( iField = 1; iField <= IDFRecords( iRecord ).NumAlphas; ++iField ) {
// 			if ( ! ObjectDef( iObjectDef ).AlphFieldDefs( iField ).empty() ) ++iTotalFieldsWithDefaults;
// 			if ( ! ObjectDef( iObjectDef ).AlphFieldDefs( iField ).empty() && IDFRecords( iRecord ).AlphBlank( iField ) ) ++iNumberOfDefaultedFields;
// 		}
// 		for ( iField = 1; iField <= IDFRecords( iRecord ).NumNumbers; ++iField ) {
// 			if ( ObjectDef( iObjectDef ).NumRangeChks( iField ).DefaultChk ) ++iTotalFieldsWithDefaults;
// 			if ( ObjectDef( iObjectDef ).NumRangeChks( iField ).DefaultChk && IDFRecords( iRecord ).NumBlank( iField ) ) ++iNumberOfDefaultedFields;
// 			if ( ObjectDef( iObjectDef ).NumRangeChks( iField ).AutoSizable ) ++iTotalAutoSizableFields;
// 			if ( ObjectDef( iObjectDef ).NumRangeChks( iField ).AutoSizable && IDFRecords( iRecord ).Numbers( iField ) == ObjectDef( iObjectDef ).NumRangeChks( iField ).AutoSizeValue ) ++iNumberOfAutoSizedFields;
// 			if ( ObjectDef( iObjectDef ).NumRangeChks( iField ).AutoCalculatable ) ++iTotalAutoCalculatableFields;
// 			if ( ObjectDef( iObjectDef ).NumRangeChks( iField ).AutoCalculatable && IDFRecords( iRecord ).Numbers( iField ) == ObjectDef( iObjectDef ).NumRangeChks( iField ).AutoCalculateValue ) ++iNumberOfAutoCalcedFields;
// 		}
// 	}
// }

// void
// EnergyPlus::InputProcessor::ReportOrphanRecordObjects()
// {

// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda Lawrie
// 	//       DATE WRITTEN   August 2002
// 	//       MODIFIED       na
// 	//       RE-ENGINEERED  na

// 	// PURPOSE OF THIS SUBROUTINE:
// 	// This subroutine reports "orphan" objects that are in the IDF but were
// 	// not "gotten" during the simulation.

// 	// METHODOLOGY EMPLOYED:
// 	// Uses internal (to InputProcessor) IDFRecordsGotten array, cross-matched with Object
// 	// names -- puts those into array to be printed (not adding dups).
// 	int Count;
// 	int Found;
// 	int ObjFound;
// 	int NumOrphObjNames;
// 	bool potentialOrphanedSpecialObjects( false );

// 	Array1D_string OrphanObjectNames( NumIDFRecords );
// 	Array1D_string OrphanNames( NumIDFRecords );
// 	NumOrphObjNames = 0;

// 	for ( Count = 1; Count <= NumIDFRecords; ++Count ) {
// 		if ( IDFRecordsGotten( Count ) ) continue;
// 		//  This one not gotten
// 		Found = FindItemInList( IDFRecords( Count ).Name, OrphanObjectNames, NumOrphObjNames );
// 		if ( Found == 0 ) {
// 			if ( SortedIDD ) {
// 				ObjFound = FindItemInSortedList( IDFRecords( Count ).Name, ListOfObjects, NumObjectDefs );
// 				if ( ObjFound != 0 ) ObjFound = iListOfObjects( ObjFound );
// 			} else {
// 				ObjFound = FindItemInList( IDFRecords( Count ).Name, ListOfObjects, NumObjectDefs );
// 			}
// 			if ( ObjFound > 0 ) {
// 				if ( ObjectDef( ObjFound ).ObsPtr > 0 ) continue; // Obsolete object, don't report "orphan"
// 				++NumOrphObjNames;
// 				OrphanObjectNames( NumOrphObjNames ) = IDFRecords( Count ).Name;
// 				// To avoid looking up potential things later when they *definitely* aren't there, we'll trap for specific flags here first
// 				//  and set the potential flag.  If the potential flag is false, nothing else is looked up later to save time
// 				if ( ( ! potentialOrphanedSpecialObjects ) && ( ! OrphanObjectNames( NumOrphObjNames ).empty() )  && ( OrphanObjectNames( NumOrphObjNames )[ 0 ] == 'Z' ) ) {
// 					potentialOrphanedSpecialObjects = true;
// 				}
// 				if ( ObjectDef( ObjFound ).NameAlpha1 ) {
// 					OrphanNames( NumOrphObjNames ) = IDFRecords( Count ).Alphas( 1 );
// 				}
// 			} else {
// 				ShowWarningError( "object not found=" + IDFRecords( Count ).Name );
// 			}
// 		} else if ( DisplayAllWarnings ) {
// 			if ( SortedIDD ) {
// 				ObjFound = FindItemInSortedList( IDFRecords( Count ).Name, ListOfObjects, NumObjectDefs );
// 				if ( ObjFound != 0 ) ObjFound = iListOfObjects( ObjFound );
// 			} else {
// 				ObjFound = FindItemInList( IDFRecords( Count ).Name, ListOfObjects, NumObjectDefs );
// 			}
// 			if ( ObjFound > 0 ) {
// 				if ( ObjectDef( ObjFound ).ObsPtr > 0 ) continue; // Obsolete object, don't report "orphan"
// 				++NumOrphObjNames;
// 				OrphanObjectNames( NumOrphObjNames ) = IDFRecords( Count ).Name;
// 				if ( ObjectDef( ObjFound ).NameAlpha1 ) {
// 					OrphanNames( NumOrphObjNames ) = IDFRecords( Count ).Alphas( 1 );
// 				}
// 			} else {
// 				ShowWarningError( "ReportOrphanRecordObjects: object not found=" + IDFRecords( Count ).Name );
// 			}
// 		}
// 	}

// 	// there are some orphans that we are deeming as special, in that they should be warned in detail even if !DisplayUnusedObjects and !DisplayAllWarnings
// 	// these are trapped by the potentialOrphanedSpecialObjects flag so that nothing is looked up if
// 	// for now, the list includes:
// 	//  - objects that start with "ZONEHVAC:"
// 	if ( potentialOrphanedSpecialObjects ) {
// 		for ( Count = 1; Count <= NumOrphObjNames; ++Count ) {
// 			if ( has_prefix( OrphanObjectNames( Count ), "ZONEHVAC:" ) ) {
// 				ShowSevereError( "Orphaned ZoneHVAC object found.  This was object never referenced in the idf, and was not used." );
// 				ShowContinueError( " -- Object type: " + OrphanObjectNames( Count ) );
// 				ShowContinueError( " -- Object name: " + OrphanNames( Count ) );
// 			}
// 		}
// 	}

// 	if ( NumOrphObjNames > 0 && DisplayUnusedObjects ) {
// 		gio::write( EchoInputFile, fmtLD ) << "Unused Objects -- Objects in IDF that were never \"gotten\"";
// 		for ( Count = 1; Count <= NumOrphObjNames; ++Count ) {
// 			if ( ! OrphanNames( Count ).empty() ) {
// 				gio::write( EchoInputFile, fmtA ) << ' ' + OrphanObjectNames( Count ) + '=' + OrphanNames( Count );
// 			} else {
// 				gio::write( EchoInputFile, fmtLD ) << OrphanObjectNames( Count );
// 			}
// 		}
// 		ShowWarningError( "The following lines are \"Unused Objects\".  These objects are in the idf" );
// 		ShowContinueError( " file but are never obtained by the simulation and therefore are NOT used." );
// 		if ( ! DisplayAllWarnings ) {
// 			ShowContinueError( " Only the first unused named object of an object class is shown.  Use Output:Diagnostics,DisplayAllWarnings to see all." );
// 		} else {
// 			ShowContinueError( " Each unused object is shown." );
// 		}
// 		ShowContinueError( " See InputOutputReference document for more details." );
// 		if ( ! OrphanNames( 1 ).empty() ) {
// 			ShowMessage( "Object=" + OrphanObjectNames( 1 ) + '=' + OrphanNames( 1 ) );
// 		} else {
// 			ShowMessage( "Object=" + OrphanObjectNames( 1 ) );
// 		}
// 		for ( Count = 2; Count <= NumOrphObjNames; ++Count ) {
// 			if ( ! OrphanNames( Count ).empty() ) {
// 				ShowContinueError( "Object=" + OrphanObjectNames( Count ) + '=' + OrphanNames( Count ) );
// 			} else {
// 				ShowContinueError( "Object=" + OrphanObjectNames( Count ) );
// 			}
// 		}
// 	} else if ( NumOrphObjNames > 0 ) {
// 		ShowMessage( "There are " + IPTrimSigDigits( NumOrphObjNames ) + " unused objects in input." );
// 		ShowMessage( "Use Output:Diagnostics,DisplayUnusedObjects; to see them." );
// 	}

// 	OrphanObjectNames.deallocate();
// 	OrphanNames.deallocate();

// }

// void
// EnergyPlus::InputProcessor::InitSecretObjects()
// {
// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda K. Lawrie
// 	//       DATE WRITTEN   March 2003
// 	// PURPOSE OF THIS SUBROUTINE:
// 	// This subroutine holds a set of objects that are either exact replacements for existing
// 	// objects or objects which are deleted.  If these are encountered in a user input file, they
// 	// will be flagged with a warning message but will not cause termination.  This routine allocates
// 	// and builds an internal structure used by the InputProcessor.
// 	NumSecretObjects = 5;
// 	RepObjects.allocate( NumSecretObjects );

// 	RepObjects( 1 ).OldName = "SKY RADIANCE DISTRIBUTION";
// 	RepObjects( 1 ).Deleted = true;

// 	RepObjects( 2 ).OldName = "SURFACE:SHADING:DETACHED";
// 	RepObjects( 2 ).NewName = "Shading:Site:Detailed";

// 	RepObjects( 3 ).OldName = "AIRFLOW MODEL";
// 	RepObjects( 3 ).Deleted = true;

// 	RepObjects( 4 ).OldName = "AIRFLOWNETWORK:MULTIZONE:SITEWINDCONDITIONS";
// 	RepObjects( 4 ).Deleted = true;

// 	RepObjects( 5 ).OldName = "OUTPUT:REPORTS";
// 	RepObjects( 5 ).NewName = "various - depends on fields";
// 	RepObjects( 5 ).Deleted = true;
// 	RepObjects( 5 ).TransitionDefer = true; // defer transition until ready to write IDF Record

// }

// void
// EnergyPlus::InputProcessor::MakeTransition( int & ObjPtr ) // Pointer to Object Definition
// {
// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda Lawrie
// 	//       DATE WRITTEN   March 2009

// 	// PURPOSE OF THIS SUBROUTINE:
// 	// For those who keep Output:Reports in their input files, this will make a
// 	// transition before storing in IDF Records.
// 	// Manipulates LineItem structure

// 	if ( ! equali( LineItem.Name, "OUTPUT:REPORTS" ) ) ShowFatalError( "Invalid object for deferred transition=" + LineItem.Name );
// 	if ( LineItem.NumAlphas < 1 ) ShowFatalError( "Invalid object for deferred transition=" + LineItem.Name );

// 	{ auto const makeTransition( uppercased( LineItem.Alphas( 1 ) ) );

// 		if ( makeTransition == "VARIABLEDICTIONARY" ) {
// 			LineItem.Name = "OUTPUT:VARIABLEDICTIONARY";
// 			if ( InputProcessor::SameString( LineItem.Alphas( 2 ), "IDF" ) ) {
// 				LineItem.Alphas( 1 ) = "IDF";
// 			} else {
// 				LineItem.Alphas( 1 ) = "REGULAR";
// 			}
// 			LineItem.NumAlphas = 1;
// 			if ( InputProcessor::SameString( LineItem.Alphas( 3 ), "Name" ) ) {
// 				LineItem.Alphas( 2 ) = "NAME";
// 				LineItem.NumAlphas = 2;
// 			} else {
// 				LineItem.Alphas( 2 ) = "NONE";
// 				LineItem.NumAlphas = 2;
// 			}

// 		} else if ( makeTransition == "SURFACES" ) {
// 			// Depends on first Alpha
// 			{ auto const surfacesTransition( LineItem.Alphas( 2 ) );

// 				if ( surfacesTransition == "DXF" || surfacesTransition == "DXF:WIREFRAME" || surfacesTransition == "VRML" ) {
// 					LineItem.Name = "OUTPUT:SURFACES:DRAWING";
// 					LineItem.Alphas( 1 ) = LineItem.Alphas( 2 );
// 					LineItem.NumAlphas = 1;
// 					if ( ! LineItem.Alphas( 3 ).empty() ) {
// 						++LineItem.NumAlphas;
// 						LineItem.Alphas( 2 ) = LineItem.Alphas( 3 );
// 					}
// 					if ( ! LineItem.Alphas( 4 ).empty() ) {
// 						++LineItem.NumAlphas;
// 						LineItem.Alphas( 3 ) = LineItem.Alphas( 4 );
// 					}

// 				} else if ( surfacesTransition == "LINES" || surfacesTransition == "DETAILS" || surfacesTransition == "VERTICES" || surfacesTransition == "DETAILSWITHVERTICES" || surfacesTransition == "VIEWFACTORINFO" || surfacesTransition == "COSTINFO" ) {
// 					LineItem.Name = "OUTPUT:SURFACES:LIST";
// 					LineItem.Alphas( 1 ) = LineItem.Alphas( 2 );
// 					LineItem.NumAlphas = 1;
// 					if ( ! LineItem.Alphas( 3 ).empty() ) {
// 						++LineItem.NumAlphas;
// 						LineItem.Alphas( 2 ) = LineItem.Alphas( 3 );
// 					}

// 				} else {
// 					ShowSevereError( "MakeTransition: Cannot transition=" + LineItem.Name + ", first field=" + LineItem.Alphas( 1 ) + ", second field=" + LineItem.Alphas( 2 ) );

// 				}}

// 			} else if ( makeTransition == "CONSTRUCTIONS" || makeTransition == "CONSTRUCTION" ) {
// 				LineItem.Name = "OUTPUT:CONSTRUCTIONS";
// 				LineItem.Alphas( 1 ) = "CONSTRUCTIONS";
// 				LineItem.NumAlphas = 1;

// 			} else if ( makeTransition == "MATERIALS" || makeTransition == "MATERIAL" ) {
// 				LineItem.Name = "OUTPUT:CONSTRUCTIONS";
// 				LineItem.Alphas( 1 ) = "MATERIALS";
// 				LineItem.NumAlphas = 1;

// 			} else if ( makeTransition == "SCHEDULES" ) {
// 				LineItem.Name = "OUTPUT:SCHEDULES";
// 				LineItem.Alphas( 1 ) = LineItem.Alphas( 2 );
// 				LineItem.NumAlphas = 1;

// 			} else {
// 				ShowSevereError( "MakeTransition: Cannot transition=" + LineItem.Name + ", first field=" + LineItem.Alphas( 1 ) );

// 			}}

// 			--ObjectDef( ObjPtr ).NumFound;
// 			ObjPtr = FindItemInList( LineItem.Name, ListOfObjects, NumObjectDefs );
// 			ObjPtr = iListOfObjects( ObjPtr );

// 			if ( ObjPtr == 0 ) ShowFatalError( "No Object Def for " + LineItem.Name );
// 			++ObjectDef( ObjPtr ).NumFound;

// 		}

// 		void
// EnergyPlus::InputProcessor::AddRecordFromSection( int const Which ) // Which object was matched
// {
// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda Lawrie
// 	//       DATE WRITTEN   March 2009

// 	// PURPOSE OF THIS SUBROUTINE:
// 	// When an object is entered like a section (i.e., <objectname>;), try to add a record
// 	// of the object using minfields, etc.
// 	int NumArg;
// 	int NumAlpha;
// 	int NumNumeric;
// 	int Count;
// 	std::string String;

// 	NumArg = 0;
// 	LineItem.Name = ObjectDef( Which ).Name;
// 	LineItem.Alphas = BlankString;
// 	LineItem.AlphBlank = false;
// 	LineItem.NumAlphas = 0;
// 	LineItem.Numbers = 0.0;
// 	LineItem.NumNumbers = 0;
// 	LineItem.NumBlank = false;
// 	LineItem.ObjectDefPtr = Which;

// 	++ObjectDef( Which ).NumFound;

// 	// Check out MinimumNumberOfFields
// 	if ( NumArg < ObjectDef( Which ).MinNumFields ) {
// 		if ( ObjectDef( Which ).NameAlpha1 ) {
// 			ShowAuditErrorMessage( " ** Warning ** ", "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Which ).Name + ", name=" + LineItem.Alphas( 1 ) + ", entered with less than minimum number of fields." );
// 		} else {
// 			ShowAuditErrorMessage( " ** Warning ** ", "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Which ).Name + ", entered with less than minimum number of fields." );
// 		}
// 		ShowAuditErrorMessage( " **   ~~~   ** ", "Attempting fill to minimum." );
// 		NumAlpha = 0;
// 		NumNumeric = 0;
// 		if ( ObjectDef( Which ).MinNumFields > ObjectDef( Which ).NumParams ) {
// 			ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object \\min-fields > number of fields specified, Object=" + ObjectDef( Which ).Name );
// 			ShowContinueError( "..\\min-fields=" + IPTrimSigDigits( ObjectDef( Which ).MinNumFields ) + ", total number of fields in object definition=" + IPTrimSigDigits( ObjectDef( Which ).NumParams ) );
// 			//      errFlag=.TRUE.
// 		} else {
// 			for ( Count = 1; Count <= ObjectDef( Which ).MinNumFields; ++Count ) {
// 				if ( ObjectDef( Which ).AlphaOrNumeric( Count ) ) {
// 					++NumAlpha;
// 					if ( NumAlpha <= LineItem.NumAlphas ) continue;
// 					++LineItem.NumAlphas;
// 					if ( ! ObjectDef( Which ).AlphFieldDefs( LineItem.NumAlphas ).empty() ) {
// 						LineItem.Alphas( LineItem.NumAlphas ) = ObjectDef( Which ).AlphFieldDefs( LineItem.NumAlphas );
// 						ShowAuditErrorMessage( " **   Add   ** ", ObjectDef( Which ).AlphFieldDefs( LineItem.NumAlphas ) + "   ! field=>" + ObjectDef( Which ).AlphFieldChks( NumAlpha ) );
// 					} else if ( ObjectDef( Which ).ReqField( Count ) ) {
// 						if ( ObjectDef( Which ).NameAlpha1 ) {
// 							ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Which ).Name + ", name=" + LineItem.Alphas( 1 ) + ", Required Field=[" + ObjectDef( Which ).AlphFieldChks( NumAlpha ) + "] was blank.", EchoInputFile );
// 						} else {
// 							ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Which ).Name + ", Required Field=[" + ObjectDef( Which ).AlphFieldChks( NumAlpha ) + "] was blank.", EchoInputFile );
// 						}
// 						//            errFlag=.TRUE.
// 					} else {
// 						LineItem.Alphas( LineItem.NumAlphas ).clear();
// 						LineItem.AlphBlank( LineItem.NumAlphas ) = true;
// 						ShowAuditErrorMessage( " **   Add   ** ", "<blank field>   ! field=>" + ObjectDef( Which ).AlphFieldChks( NumAlpha ) );
// 					}
// 				} else {
// 					++NumNumeric;
// 					if ( NumNumeric <= LineItem.NumNumbers ) continue;
// 					++LineItem.NumNumbers;
// 					LineItem.NumBlank( NumNumeric ) = true;
// 					if ( ObjectDef( Which ).NumRangeChks( NumNumeric ).DefaultChk ) {
// 						if ( ! ObjectDef( Which ).NumRangeChks( NumNumeric ).DefAutoSize && ! ObjectDef( Which ).NumRangeChks( NumNumeric ).DefAutoCalculate ) {
// 							LineItem.Numbers( NumNumeric ) = ObjectDef( Which ).NumRangeChks( NumNumeric ).Default;
// 							gio::write( String, fmtLD ) << ObjectDef( Which ).NumRangeChks( NumNumeric ).Default;
// 							strip( String );
// 							ShowAuditErrorMessage( " **   Add   ** ", String + "   ! field=>" + ObjectDef( Which ).NumRangeChks( NumNumeric ).FieldName );
// 						} else if ( ObjectDef( Which ).NumRangeChks( NumNumeric ).DefAutoSize ) {
// 							LineItem.Numbers( NumNumeric ) = ObjectDef( Which ).NumRangeChks( NumNumeric ).AutoSizeValue;
// 							ShowAuditErrorMessage( " **   Add   ** ", "autosize    ! field=>" + ObjectDef( Which ).NumRangeChks( NumNumeric ).FieldName );
// 						} else if ( ObjectDef( Which ).NumRangeChks( NumNumeric ).DefAutoCalculate ) {
// 							LineItem.Numbers( NumNumeric ) = ObjectDef( Which ).NumRangeChks( NumNumeric ).AutoCalculateValue;
// 							ShowAuditErrorMessage( " **   Add   ** ", "autocalculate    ! field=>" + ObjectDef( Which ).NumRangeChks( NumNumeric ).FieldName );
// 						}
// 					} else if ( ObjectDef( Which ).ReqField( Count ) ) {
// 						if ( ObjectDef( Which ).NameAlpha1 ) {
// 							ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Which ).Name + ", name=" + LineItem.Alphas( 1 ) + ", Required Field=[" + ObjectDef( Which ).NumRangeChks( NumNumeric ).FieldName + "] was blank.", EchoInputFile );
// 						} else {
// 							ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Which ).Name + ", Required Field=[" + ObjectDef( Which ).NumRangeChks( NumNumeric ).FieldName + "] was blank.", EchoInputFile );
// 						}
// 						//            errFlag=.TRUE.
// 					} else {
// 						LineItem.Numbers( NumNumeric ) = 0.0;
// 						LineItem.NumBlank( NumNumeric ) = true;
// 						ShowAuditErrorMessage( " **   Add   ** ", "<blank field>   ! field=>" + ObjectDef( Which ).NumRangeChks( NumNumeric ).FieldName );
// 					}
// 				}
// 			}
// 		}
// 	}

// 	//  IF (TransitionDefer) THEN
// 	//    CALL MakeTransition(Which)
// 	//  ENDIF
// 	++NumIDFRecords;
// 	if ( ObjectStartRecord( Which ) == 0 ) ObjectStartRecord( Which ) = NumIDFRecords;
// 	MaxAlphaIDFArgsFound = max( MaxAlphaIDFArgsFound, LineItem.NumAlphas );
// 	MaxNumericIDFArgsFound = max( MaxNumericIDFArgsFound, LineItem.NumNumbers );
// 	MaxAlphaIDFDefArgsFound = max( MaxAlphaIDFDefArgsFound, ObjectDef( Which ).NumAlpha );
// 	MaxNumericIDFDefArgsFound = max( MaxNumericIDFDefArgsFound, ObjectDef( Which ).NumNumeric );
// 	IDFRecords( NumIDFRecords ).Name = LineItem.Name;
// 	IDFRecords( NumIDFRecords ).NumNumbers = LineItem.NumNumbers;
// 	IDFRecords( NumIDFRecords ).NumAlphas = LineItem.NumAlphas;
// 	IDFRecords( NumIDFRecords ).ObjectDefPtr = LineItem.ObjectDefPtr;
// 	IDFRecords( NumIDFRecords ).Alphas.allocate( LineItem.NumAlphas );
// 	IDFRecords( NumIDFRecords ).Alphas = LineItem.Alphas( {1,LineItem.NumAlphas} );
// 	IDFRecords( NumIDFRecords ).AlphBlank.allocate( LineItem.NumAlphas );
// 	IDFRecords( NumIDFRecords ).AlphBlank = LineItem.AlphBlank( {1,LineItem.NumAlphas} );
// 	IDFRecords( NumIDFRecords ).Numbers.allocate( LineItem.NumNumbers );
// 	IDFRecords( NumIDFRecords ).Numbers = LineItem.Numbers( {1,LineItem.NumNumbers} );
// 	IDFRecords( NumIDFRecords ).NumBlank.allocate( LineItem.NumNumbers );
// 	IDFRecords( NumIDFRecords ).NumBlank = LineItem.NumBlank( {1,LineItem.NumNumbers} );
// 	if ( LineItem.NumNumbers > 0 ) {
// 		for ( Count = 1; Count <= LineItem.NumNumbers; ++Count ) {
// 			if ( ObjectDef( Which ).NumRangeChks( Count ).MinMaxChk && ! LineItem.NumBlank( Count ) ) {
// 				InternalRangeCheck( LineItem.Numbers( Count ), Count, Which, LineItem.Alphas( 1 ), ObjectDef( Which ).NumRangeChks( Count ).AutoSizable, ObjectDef( Which ).NumRangeChks( Count ).AutoCalculatable );
// 			}
// 		}
// 	}
// }

    void
    EnergyPlus::InputProcessor::PreProcessorCheck(bool &PreP_Fatal) // True if a preprocessor flags a fatal error
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   August 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine checks for existance of "Preprocessor Message" object and
        // performs appropriate action.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // Preprocessor Message,
        //    \memo This object does not come from a user input.  This is generated by a pre-processor
        //    \memo so that various conditions can be gracefully passed on by the InputProcessor.
        //    A1,        \field preprocessor name
        //    A2,        \field error severity
        //               \note Depending on type, InputProcessor may terminate the program.
        //               \type choice
        //               \key warning
        //               \key severe
        //               \key fatal
        //    A3,        \field message line 1
        //    A4,        \field message line 2
        //    A5,        \field message line 3
        //    A6,        \field message line 4
        //    A7,        \field message line 5
        //    A8,        \field message line 6
        //    A9,        \field message line 7
        //    A10,       \field message line 8
        //    A11,       \field message line 9
        //    A12;       \field message line 10

        // Using/Aliasing
        using namespace DataIPShortCuts;

        int NumAlphas; // Used to retrieve names from IDF
        int NumNumbers; // Used to retrieve rNumericArgs from IDF
        int IOStat; // Could be used in the Get Routines, not currently checked
        int NumParams; // Total Number of Parameters in 'Output:PreprocessorMessage' Object
        int NumPrePM; // Number of Preprocessor Message objects in IDF
        int CountP;
        int CountM;
        std::string Multiples;

        cCurrentModuleObject = "Output:PreprocessorMessage";
        NumPrePM = InputProcessor::GetNumObjectsFound(cCurrentModuleObject);
        if (NumPrePM > 0) {
            GetObjectDefMaxArgs(cCurrentModuleObject, NumParams, NumAlphas, NumNumbers);
            cAlphaArgs({1, NumAlphas}) = BlankString;
            for (CountP = 1; CountP <= NumPrePM; ++CountP) {
                InputProcessor::GetObjectItem(cCurrentModuleObject, CountP, cAlphaArgs, NumAlphas, rNumericArgs,
                                              NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks,
                                              cAlphaFieldNames, cNumericFieldNames);
                if (cAlphaArgs(1).empty()) cAlphaArgs(1) = "Unknown";
                if (NumAlphas > 3) {
                    Multiples = "s";
                } else {
                    Multiples = BlankString;
                }
                if (cAlphaArgs(2).empty()) cAlphaArgs(2) = "Unknown";
                {
                    auto const errorType(uppercased(cAlphaArgs(2)));
                    if (errorType == "INFORMATION") {
                        ShowMessage(cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                    "\" has the following Information message" + Multiples + ':');
                    } else if (errorType == "WARNING") {
                        ShowWarningError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                         "\" has the following Warning condition" + Multiples + ':');
                    } else if (errorType == "SEVERE") {
                        ShowSevereError(
                                cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" has the following Severe condition" +
                                Multiples + ':');
                    } else if (errorType == "FATAL") {
                        ShowSevereError(
                                cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" has the following Fatal condition" +
                                Multiples + ':');
                        PreP_Fatal = true;
                    } else {
                        ShowSevereError(
                                cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" has the following " + cAlphaArgs(2) +
                                " condition" + Multiples + ':');
                    }
                }
                CountM = 3;
                if (CountM > NumAlphas) {
                    ShowContinueError(cCurrentModuleObject + " was blank.  Check " + cAlphaArgs(1) +
                                      " audit trail or error file for possible reasons.");
                }
                while (CountM <= NumAlphas) {
                    if (len(cAlphaArgs(CountM)) == MaxNameLength) {
                        ShowContinueError(cAlphaArgs(CountM) + cAlphaArgs(CountM + 1));
                        CountM += 2;
                    } else {
                        ShowContinueError(cAlphaArgs(CountM));
                        ++CountM;
                    }
                }
            }
        }

    }

    // void
    // EnergyPlus::InputProcessor::CompactObjectsCheck()
    // {

    // // SUBROUTINE INFORMATION:
    // //       AUTHOR         Linda Lawrie
    // //       DATE WRITTEN   December 2005
    // //       MODIFIED       na
    // //       RE-ENGINEERED  na

    // // PURPOSE OF THIS SUBROUTINE:
    // // Check to see if Compact Objects (i.e. CompactHVAC and its ilk) exist in the
    // // input file.  If so, expandobjects was not run and there's a possible problem.
    // 	bool CompactObjectsFound;

    // 	CompactObjectsFound = false;

    // 	for ( int i = IDFRecords.l(), e = IDFRecords.u(); i <= e; ++i ) {
    // 		auto const & Name( IDFRecords( i ).Name );
    // 		if ( ( has_prefix( Name, "HVACTEMPLATE:" ) ) || ( has_prefix( Name, "HVACTemplate:" ) ) ) {
    // 			ShowSevereError( "HVACTemplate objects are found in the IDF File." );
    // 			CompactObjectsFound = true;
    // 			break;
    // 		}
    // 	}

    // 	if ( CompactObjectsFound ) {
    // 		ShowFatalError( "Program Terminates: The ExpandObjects program has not been run or is not in your EnergyPlus.exe folder." );
    // 	}

    // }

    // void
    // EnergyPlus::InputProcessor::ParametricObjectsCheck()
    // {

    // // SUBROUTINE INFORMATION:
    // //       AUTHOR         Jason Glazer (based on CompactObjectsCheck by Linda Lawrie)
    // //       DATE WRITTEN   September 2009
    // //       MODIFIED
    // //       RE-ENGINEERED  na

    // // PURPOSE OF THIS SUBROUTINE:
    // // Check to see if Parametric Objects exist in the input file.

    // 	for ( int i = IDFRecords.l(), e = IDFRecords.u(); i <= e; ++i ) {
    // 		auto const & Name( IDFRecords( i ).Name );
    // 		if ( has_prefixi( Name, "Parametric:" ) ) {
    // 			ShowSevereError( "Parametric objects are found in the IDF File." );
    // 			ShowFatalError( "Program Terminates: The ParametricPreprocessor program has not been run." );
    // 			break;
    // 		}
    // 	}
    // }

    void
    InputProcessor::PreScanReportingVariables() {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   July 2010

        // PURPOSE OF THIS SUBROUTINE:
        // This routine scans the input records and determines which output variables
        // are actually being requested for the run so that the OutputProcessor will only
        // consider those variables for output.  (At this time, all metered variables are
        // allowed to pass through).

        // METHODOLOGY EMPLOYED:
        // Uses internal records and structures.
        // Looks at:
        // Output:Variable
        // Meter:Custom
        // Meter:CustomDecrement
        // Meter:CustomDifference
        // Output:Table:Monthly
        // Output:Table:TimeBins
        // Output:Table:SummaryReports
        // EnergyManagementSystem:Sensor
        // EnergyManagementSystem:OutputVariable

        // Using/Aliasing
        using namespace DataOutputs;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const OutputVariable("Output:Variable");
        static std::string const MeterCustom("Meter:Custom");
        static std::string const MeterCustomDecrement("Meter:CustomDecrement");
//		static std::string const MeterCustomDifference( "METER:CUSTOMDIFFERENCE" );
        static std::string const OutputTableMonthly("Output:Table:Monthly");
        static std::string const OutputTableAnnual("Output:Table:Annual");
        static std::string const OutputTableTimeBins("Output:Table:TimeBins");
        static std::string const OutputTableSummaries("Output:Table:SummaryReports");
        static std::string const EMSSensor("EnergyManagementSystem:Sensor");
        static std::string const EMSOutputVariable("EnergyManagementSystem:OutputVariable");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CurrentRecord;
        int Loop;
        int Loop1;

        OutputVariablesForSimulation.allocate(10000);
        MaxConsideredOutputVariables = 10000;

        // Output Variable
        auto jdf_objects = jdf.find(OutputVariable);
        if (jdf_objects != jdf.end()) {
            auto const &jdf_object = jdf_objects.value();
            for (auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj) {
                json const &fields = obj.value();
                if (!fields.at("key_value").empty()) {
                    InputProcessor::AddRecordToOutputVariableStructure(fields.at("key_value"), fields.at("variable_name"));
                } else {
                    InputProcessor::AddRecordToOutputVariableStructure("*", fields.at("variable_name"));
                }
            }
        }

        jdf_objects = jdf.find(MeterCustom);
        if (jdf_objects != jdf.end()) {
            auto const &jdf_object = jdf_objects.value();
            for (auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj) {
                json const &fields = obj.value();

                //TODO: Might be incorrect
                for (auto const &extensions : fields["extensions"]) {
                    if (!obj.key().empty()) {
                        InputProcessor::AddRecordToOutputVariableStructure(extensions.at("key_name"),
                                                           extensions.at("output_variable_or_meter_name"));
                    } else {
                        InputProcessor::AddRecordToOutputVariableStructure("*", extensions.at("output_variable_or_meter_name"));
                    }
                }
            }
        }

        jdf_objects = jdf.find(MeterCustomDecrement);
        if (jdf_objects != jdf.end()) {
            auto const &jdf_object = jdf_objects.value();
            for (auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj) {
                json const &fields = obj.value();

                //TODO: Might be incorrect
                for (auto const &extensions : fields["extensions"]) {
                    if (!obj.key().empty()) {
                        InputProcessor::AddRecordToOutputVariableStructure(extensions.at("key_name"),
                                                           extensions.at("output_variable_or_meter_name"));
                    } else {
                        InputProcessor::AddRecordToOutputVariableStructure("*", extensions.at("output_variable_or_meter_name"));
                    }
                }
            }
        }

        jdf_objects = jdf.find(EMSSensor);
        if (jdf_objects != jdf.end()) {
            auto const &jdf_object = jdf_objects.value();
            for (auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj) {
                json const &fields = obj.value();
                if (!fields.at("output_variable_or_output_meter_index_key_name").empty()) {
                    InputProcessor::AddRecordToOutputVariableStructure(fields.at("output_variable_or_output_meter_index_key_name"),
                                                       fields.at("output_variable_or_output_meter_name"));
                } else {
                    InputProcessor::AddRecordToOutputVariableStructure("*", fields.at("output_variable_or_output_meter_name"));
                }
            }
        }

        jdf_objects = jdf.find(EMSOutputVariable);
        if (jdf_objects != jdf.end()) {
            auto const &jdf_object = jdf_objects.value();
            for (auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj) {
                // TODO: Might be wrong...
                InputProcessor::AddRecordToOutputVariableStructure("*", obj.key());
            }
        }

        jdf_objects = jdf.find(OutputTableTimeBins);
        if (jdf_objects != jdf.end()) {
            auto const &jdf_object = jdf_objects.value();
            for (auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj) {
                json const &fields = obj.value();
                if (!obj.key().empty()) {
                    InputProcessor::AddRecordToOutputVariableStructure(obj.key(), fields.at("key_value"));
                } else {
                    InputProcessor::AddRecordToOutputVariableStructure("*", fields.at("key_value"));
                }
            }
        }

        jdf_objects = jdf.find(OutputTableMonthly);
        if (jdf_objects != jdf.end()) {
            auto const &jdf_object = jdf_objects.value();
            for (auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj) {
                json const &fields = obj.value();
                InputProcessor::AddRecordToOutputVariableStructure("*", fields.at("variable_or_meter_name"));
            }
        }

        jdf_objects = jdf.find(OutputTableAnnual);
        if (jdf_objects != jdf.end()) {
            auto const &jdf_object = jdf_objects.value();
            for (auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj) {
                json const &fields = obj.value();
                InputProcessor::AddRecordToOutputVariableStructure("*", fields.at("variable_or_meter_or_ems_variable_or_field_name"));
            }
        }

        jdf_objects = jdf.find(OutputTableSummaries);
        if (jdf_objects != jdf.end()) {
            auto const &jdf_object = jdf_objects.value();
            for (auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj) {
                json const &fields = obj.value();
                auto const report_name = MakeUPPERCase(fields.at("report_name"));
                if (report_name == "ALLMONTHLY" || report_name == "ALLSUMMARYANDMONTHLY") {
                    for (Loop1 = 1; Loop1 <= NumMonthlyReports; ++Loop1) {
                        InputProcessor::AddVariablesForMonthlyReport(MonthlyNamedReports(Loop1));
                    }
                } else {
                   InputProcessor::AddVariablesForMonthlyReport(report_name);
                }
            }
        }

        if (NumConsideredOutputVariables > 0) {
            OutputVariablesForSimulation.redimension(NumConsideredOutputVariables);
            MaxConsideredOutputVariables = NumConsideredOutputVariables;
        }
    }

    void
    InputProcessor::AddVariablesForMonthlyReport(std::string const &reportName) {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   July 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine adds specific variables to the Output Variables for Simulation
        // Structure. Note that only non-metered variables need to be added here.  Metered
        // variables are automatically included in the minimized output variable structure.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        if (reportName == "ZONECOOLINGSUMMARYMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR WETBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "ZONE TOTAL INTERNAL LATENT GAIN ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE TOTAL INTERNAL LATENT GAIN RATE");

        } else if (reportName == "ZONEHEATINGSUMMARYMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE AIR SYSTEM SENSIBLE HEATING ENERGY"); // on meter
            AddRecordToOutputVariableStructure("*", "ZONE AIR SYSTEM SENSIBLE HEATING RATE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE");

        } else if (reportName == "ZONEELECTRICSUMMARYMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE LIGHTS ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE ELECTRIC EQUIPMENT ELECTRIC ENERGY");

        } else if (reportName == "SPACEGAINSMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE PEOPLE TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE LIGHTS TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY");

        } else if (reportName == "PEAKSPACEGAINSMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE PEOPLE TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE LIGHTS TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY");

        } else if (reportName == "SPACEGAINCOMPONENTSATCOOLINGPEAKMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE");
            AddRecordToOutputVariableStructure("*", "ZONE PEOPLE TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE LIGHTS TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY");

        } else if (reportName == "SETPOINTSNOTMETWITHTEMPERATURESMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE HEATING SETPOINT NOT MET TIME");
            AddRecordToOutputVariableStructure("*", "ZONE MEAN AIR TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "ZONE HEATING SETPOINT NOT MET WHILE OCCUPIED TIME");
            AddRecordToOutputVariableStructure("*", "ZONE COOLING SETPOINT NOT MET TIME");
            AddRecordToOutputVariableStructure("*", "ZONE COOLING SETPOINT NOT MET WHILE OCCUPIED TIME");

        } else if (reportName == "COMFORTREPORTSIMPLE55MONTHLY") {
            AddRecordToOutputVariableStructure("*",
                                               "ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL SUMMER CLOTHES NOT COMFORTABLE TIME");
            AddRecordToOutputVariableStructure("*", "ZONE MEAN AIR TEMPERATURE");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL WINTER CLOTHES NOT COMFORTABLE TIME");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL SUMMER OR WINTER CLOTHES NOT COMFORTABLE TIME");

        } else if (reportName == "UNGLAZEDTRANSPIREDSOLARCOLLECTORSUMMARYMONTHLY") {
            AddRecordToOutputVariableStructure("*", "SOLAR COLLECTOR SYSTEM EFFICIENCY");
            AddRecordToOutputVariableStructure("*", "SOLAR COLLECTOR OUTSIDE FACE SUCTION VELOCITY");
            AddRecordToOutputVariableStructure("*", "SOLAR COLLECTOR SENSIBLE HEATING RATE");

        } else if (reportName == "OCCUPANTCOMFORTDATASUMMARYMONTHLY") {
            AddRecordToOutputVariableStructure("*", "PEOPLE OCCUPANT COUNT");
            AddRecordToOutputVariableStructure("*", "PEOPLE AIR TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "PEOPLE AIR RELATIVE HUMIDITY");
            AddRecordToOutputVariableStructure("*", "ZONE THERMAL COMFORT FANGER MODEL PMV");
            AddRecordToOutputVariableStructure("*", "ZONE THERMAL COMFORT FANGER MODEL PPD");

        } else if (reportName == "CHILLERREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "CHILLER ELECTRIC ENERGY"); // on meter
            AddRecordToOutputVariableStructure("*", "CHILLER ELECTRIC POWER");
            AddRecordToOutputVariableStructure("*", "CHILLER EVAPORATOR COOLING ENERGY"); // on meter
            AddRecordToOutputVariableStructure("*", "CHILLER CONDENSER HEAT TRANSFER ENERGY"); // on meter
            AddRecordToOutputVariableStructure("*", "CHILLER COP");

        } else if (reportName == "TOWERREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "COOLING TOWER FAN ELECTRIC ENERGY"); // on meter
            AddRecordToOutputVariableStructure("*", "COOLING TOWER FAN ELECTRIC POWER");
            AddRecordToOutputVariableStructure("*", "COOLING TOWER HEAT TRANSFER RATE");
            AddRecordToOutputVariableStructure("*", "COOLING TOWER INLET TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "COOLING TOWER OUTLET TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "COOLING TOWER MASS FLOW RATE");

        } else if (reportName == "BOILERREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "BOILER HEATING ENERGY"); // on meter
            AddRecordToOutputVariableStructure("*", "BOILER GAS CONSUMPTION"); // on meter
            AddRecordToOutputVariableStructure("*", "BOILER HEATING ENERGY"); // on meter
            AddRecordToOutputVariableStructure("*", "BOILER HEATING RATE");
            AddRecordToOutputVariableStructure("*", "BOILER GAS CONSUMPTION RATE");
            AddRecordToOutputVariableStructure("*", "BOILER INLET TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "BOILER OUTLET TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "BOILER MASS FLOW RATE");
            AddRecordToOutputVariableStructure("*", "BOILER ANCILLARY ELECTRIC POWER");

        } else if (reportName == "DXREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "COOLING COIL TOTAL COOLING ENERGY"); // on meter
            AddRecordToOutputVariableStructure("*", "COOLING COIL ELECTRIC ENERGY"); // on meter
            AddRecordToOutputVariableStructure("*", "COOLING COIL SENSIBLE COOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "COOLING COIL LATENT COOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "COOLING COIL CRANKCASE HEATER ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "COOLING COIL RUNTIME FRACTION");
            AddRecordToOutputVariableStructure("*", "COOLING COIL TOTAL COOLING RATE");
            AddRecordToOutputVariableStructure("*", "COOLING COIL SENSIBLE COOLING RATE");
            AddRecordToOutputVariableStructure("*", "COOLING COIL LATENT COOLING RATE");
            AddRecordToOutputVariableStructure("*", "COOLING COIL ELECTRIC POWER");
            AddRecordToOutputVariableStructure("*", "COOLING COIL CRANKCASE HEATER ELECTRIC POWER");

        } else if (reportName == "WINDOWREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "SURFACE WINDOW TRANSMITTED SOLAR RADIATION RATE");
            AddRecordToOutputVariableStructure("*", "SURFACE WINDOW TRANSMITTED BEAM SOLAR RADIATION RATE");
            AddRecordToOutputVariableStructure("*", "SURFACE WINDOW TRANSMITTED DIFFUSE SOLAR RADIATION RATE");
            AddRecordToOutputVariableStructure("*", "SURFACE WINDOW HEAT GAIN RATE");
            AddRecordToOutputVariableStructure("*", "SURFACE WINDOW HEAT LOSS RATE");
            AddRecordToOutputVariableStructure("*", "SURFACE WINDOW INSIDE FACE GLAZING CONDENSATION STATUS");
            AddRecordToOutputVariableStructure("*", "SURFACE SHADING DEVICE IS ON TIME FRACTION");
            AddRecordToOutputVariableStructure("*", "SURFACE STORM WINDOW ON OFF STATUS");

        } else if (reportName == "WINDOWENERGYREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "SURFACE WINDOW TRANSMITTED SOLAR RADIATION ENERGY");
            AddRecordToOutputVariableStructure("*", "SURFACE WINDOW TRANSMITTED BEAM SOLAR RADIATION ENERGY");
            AddRecordToOutputVariableStructure("*", "SURFACE WINDOW TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY");
            AddRecordToOutputVariableStructure("*", "SURFACE WINDOW HEAT GAIN ENERGY");
            AddRecordToOutputVariableStructure("*", "SURFACE WINDOW HEAT LOSS ENERGY");

        } else if (reportName == "WINDOWZONESUMMARYMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE WINDOWS TOTAL HEAT GAIN RATE");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOWS TOTAL HEAT LOSS RATE");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOWS TOTAL TRANSMITTED SOLAR RADIATION RATE");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION RATE");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION RATE");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION RATE");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION RATE");

        } else if (reportName == "WINDOWENERGYZONESUMMARYMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE WINDOWS TOTAL HEAT GAIN ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOWS TOTAL HEAT LOSS ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOWS TOTAL TRANSMITTED SOLAR RADIATION ENERGY");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION ENERGY");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION ENERGY");

        } else if (reportName == "AVERAGEOUTDOORCONDITIONSMONTHLY") {
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR WETBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE WIND SPEED");
            AddRecordToOutputVariableStructure("*", "SITE SKY TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA");
            AddRecordToOutputVariableStructure("*", "SITE DIRECT SOLAR RADIATION RATE PER AREA");
            AddRecordToOutputVariableStructure("*", "SITE RAIN STATUS");

        } else if (reportName == "OUTDOORCONDITIONSMAXIMUMDRYBULBMONTHLY") {
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR WETBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE WIND SPEED");
            AddRecordToOutputVariableStructure("*", "SITE SKY TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA");
            AddRecordToOutputVariableStructure("*", "SITE DIRECT SOLAR RADIATION RATE PER AREA");

        } else if (reportName == "OUTDOORCONDITIONSMINIMUMDRYBULBMONTHLY") {
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR WETBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE WIND SPEED");
            AddRecordToOutputVariableStructure("*", "SITE SKY TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA");
            AddRecordToOutputVariableStructure("*", "SITE DIRECT SOLAR RADIATION RATE PER AREA");

        } else if (reportName == "OUTDOORCONDITIONSMAXIMUMWETBULBMONTHLY") {
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR WETBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE WIND SPEED");
            AddRecordToOutputVariableStructure("*", "SITE SKY TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA");
            AddRecordToOutputVariableStructure("*", "SITE DIRECT SOLAR RADIATION RATE PER AREA");

        } else if (reportName == "OUTDOORCONDITIONSMAXIMUMDEWPOINTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE OUTDOOR AIR WETBULB TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE WIND SPEED");
            AddRecordToOutputVariableStructure("*", "SITE SKY TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA");
            AddRecordToOutputVariableStructure("*", "SITE DIRECT SOLAR RADIATION RATE PER AREA");

        } else if (reportName == "OUTDOORGROUNDCONDITIONSMONTHLY") {
            AddRecordToOutputVariableStructure("*", "SITE GROUND TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE SURFACE GROUND TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE DEEP GROUND TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE MAINS WATER TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "SITE GROUND REFLECTED SOLAR RADIATION RATE PER AREA");
            AddRecordToOutputVariableStructure("*", "SITE SNOW ON GROUND STATUS");

        } else if (reportName == "WINDOWACREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOW AIR CONDITIONER ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOW AIR CONDITIONER SENSIBLE COOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOW AIR CONDITIONER LATENT COOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING RATE");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOW AIR CONDITIONER SENSIBLE COOLING RATE");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOW AIR CONDITIONER LATENT COOLING RATE");
            AddRecordToOutputVariableStructure("*", "ZONE WINDOW AIR CONDITIONER ELECTRIC POWER");

        } else if (reportName == "WATERHEATERREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "WATER HEATER TOTAL DEMAND HEAT TRANSFER ENERGY");
            AddRecordToOutputVariableStructure("*", "WATER HEATER USE SIDE HEAT TRANSFER ENERGY");
            AddRecordToOutputVariableStructure("*", "WATER HEATER BURNER HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "WATER HEATER GAS CONSUMPTION");
            AddRecordToOutputVariableStructure("*", "WATER HEATER TOTAL DEMAND HEAT TRANSFER ENERGY");
            AddRecordToOutputVariableStructure("*", "WATER HEATER LOSS DEMAND ENERGY");
            AddRecordToOutputVariableStructure("*", "WATER HEATER HEAT LOSS ENERGY");
            AddRecordToOutputVariableStructure("*", "WATER HEATER TANK TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "WATER HEATER HEAT RECOVERY SUPPLY ENERGY");
            AddRecordToOutputVariableStructure("*", "WATER HEATER SOURCE ENERGY");

        } else if (reportName == "GENERATORREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "GENERATOR PRODUCED ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "GENERATOR DIESEL CONSUMPTION");
            AddRecordToOutputVariableStructure("*", "GENERATOR GAS CONSUMPTION");
            AddRecordToOutputVariableStructure("*", "GENERATOR PRODUCED ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "GENERATOR TOTAL HEAT RECOVERY");
            AddRecordToOutputVariableStructure("*", "GENERATOR JACKET HEAT RECOVERY ENERGY");
            AddRecordToOutputVariableStructure("*", "GENERATOR LUBE HEAT RECOVERY");
            AddRecordToOutputVariableStructure("*", "GENERATOR EXHAUST HEAT RECOVERY ENERGY");
            AddRecordToOutputVariableStructure("*", "GENERATOR EXHAUST AIR TEMPERATURE");

        } else if (reportName == "DAYLIGHTINGREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "SITE EXTERIOR BEAM NORMAL ILLUMINANCE");
            AddRecordToOutputVariableStructure("*", "DAYLIGHTING LIGHTING POWER MULTIPLIER");
            AddRecordToOutputVariableStructure("*", "DAYLIGHTING LIGHTING POWER MULTIPLIER");
            AddRecordToOutputVariableStructure("*", "DAYLIGHTING REFERENCE POINT 1 ILLUMINANCE");
            AddRecordToOutputVariableStructure("*", "DAYLIGHTING REFERENCE POINT 1 GLARE INDEX");
            AddRecordToOutputVariableStructure("*", "DAYLIGHTING REFERENCE POINT 1 GLARE INDEX SETPOINT EXCEEDED TIME");
            AddRecordToOutputVariableStructure("*",
                                               "DAYLIGHTING REFERENCE POINT 1 DAYLIGHT ILLUMINANCE SETPOINT EXCEEDED TIME");
            AddRecordToOutputVariableStructure("*", "DAYLIGHTING REFERENCE POINT 2 ILLUMINANCE");
            AddRecordToOutputVariableStructure("*", "DAYLIGHTING REFERENCE POINT 2 GLARE INDEX");
            AddRecordToOutputVariableStructure("*", "DAYLIGHTING REFERENCE POINT 2 GLARE INDEX SETPOINT EXCEEDED TIME");
            AddRecordToOutputVariableStructure("*",
                                               "DAYLIGHTING REFERENCE POINT 2 DAYLIGHT ILLUMINANCE SETPOINT EXCEEDED TIME");

        } else if (reportName == "COILREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "HEATING COIL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "HEATING COIL HEATING RATE");
            AddRecordToOutputVariableStructure("*", "COOLING COIL SENSIBLE COOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "COOLING COIL TOTAL COOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "COOLING COIL TOTAL COOLING RATE");
            AddRecordToOutputVariableStructure("*", "COOLING COIL SENSIBLE COOLING RATE");
            AddRecordToOutputVariableStructure("*", "COOLING COIL WETTED AREA FRACTION");

        } else if (reportName == "PLANTLOOPDEMANDREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "PLANT SUPPLY SIDE COOLING DEMAND RATE");
            AddRecordToOutputVariableStructure("*", "PLANT SUPPLY SIDE HEATING DEMAND RATE");

        } else if (reportName == "FANREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "FAN ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "FAN RISE IN AIR TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "FAN ELECTRIC POWER");

        } else if (reportName == "PUMPREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "PUMP ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "PUMP FLUID HEAT GAIN ENERGY");
            AddRecordToOutputVariableStructure("*", "PUMP ELECTRIC POWER");
            AddRecordToOutputVariableStructure("*", "PUMP SHAFT POWER");
            AddRecordToOutputVariableStructure("*", "PUMP FLUID HEAT GAIN RATE");
            AddRecordToOutputVariableStructure("*", "PUMP OUTLET TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "PUMP MASS FLOW RATE");

        } else if (reportName == "CONDLOOPDEMANDREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "PLANT SUPPLY SIDE COOLING DEMAND RATE");
            AddRecordToOutputVariableStructure("*", "PLANT SUPPLY SIDE HEATING DEMAND RATE");
            AddRecordToOutputVariableStructure("*", "PLANT SUPPLY SIDE INLET TEMPERATURE");
            AddRecordToOutputVariableStructure("*", "PLANT SUPPLY SIDE OUTLET TEMPERATURE");

        } else if (reportName == "ZONETEMPERATUREOSCILLATIONREPORTMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE OSCILLATING TEMPERATURES TIME");
            AddRecordToOutputVariableStructure("*", "ZONE PEOPLE OCCUPANT COUNT");

        } else if (reportName == "AIRLOOPSYSTEMENERGYANDWATERUSEMONTHLY") {
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM HOT WATER ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM STEAM ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM CHILLED WATER ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM GAS ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM WATER VOLUME");

        } else if (reportName == "AIRLOOPSYSTEMCOMPONENTLOADSMONTHLY") {
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM FAN AIR HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM COOLING COIL TOTAL COOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM HEATING COIL TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM HEAT EXCHANGER TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM HEAT EXCHANGER TOTAL COOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM HUMIDIFIER TOTAL HEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM EVAPORATIVE COOLER TOTAL COOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM DESICCANT DEHUMIDIFIER TOTAL COOLING ENERGY");

        } else if (reportName == "AIRLOOPSYSTEMCOMPONENTENERGYUSEMONTHLY") {
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM FAN ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM HEATING COIL HOT WATER ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM COOLING COIL CHILLED WATER ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM DX HEATING COIL ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM DX COOLING COIL ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM HEATING COIL ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM HEATING COIL GAS ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM HEATING COIL STEAM ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM HUMIDIFIER ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM EVAPORATIVE COOLER ELECTRIC ENERGY");
            AddRecordToOutputVariableStructure("*", "AIR SYSTEM DESICCANT DEHUMIDIFIER ELECTRIC ENERGY");

        } else if (reportName == "MECHANICALVENTILATIONLOADSMONTHLY") {
            AddRecordToOutputVariableStructure("*", "ZONE MECHANICAL VENTILATION NO LOAD HEAT REMOVAL ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE MECHANICAL VENTILATION COOLING LOAD INCREASE ENERGY");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE MECHANICAL VENTILATION COOLING LOAD INCREASE DUE TO OVERHEATING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE MECHANICAL VENTILATION COOLING LOAD DECREASE ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE MECHANICAL VENTILATION NO LOAD HEAT ADDITION ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE MECHANICAL VENTILATION HEATING LOAD INCREASE ENERGY");
            AddRecordToOutputVariableStructure("*",
                                               "ZONE MECHANICAL VENTILATION HEATING LOAD INCREASE DUE TO OVERCOOLING ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE MECHANICAL VENTILATION HEATING LOAD DECREASE ENERGY");
            AddRecordToOutputVariableStructure("*", "ZONE MECHANICAL VENTILATION AIR CHANGES PER HOUR");

        } else {

        }

    }

// int
// FindFirstRecord( std::string const & UCObjType )
// {

// 	// FUNCTION INFORMATION:
// 	//       AUTHOR         Linda Lawrie
// 	//       DATE WRITTEN   July 2010
// 	//       MODIFIED       na
// 	//       RE-ENGINEERED  na

// 	// PURPOSE OF THIS FUNCTION:
// 	// Finds next record of Object Name

// 	int Found;
// 	if ( SortedIDD ) {
// 		Found = FindItemInSortedList( UCObjType, ListOfObjects, NumObjectDefs );
// 		if ( Found != 0 ) Found = iListOfObjects( Found );
// 	} else {
// 		Found = FindItemInList( UCObjType, ListOfObjects, NumObjectDefs );
// 	}

// 	int StartPointer;
// 	if ( Found != 0 ) {
// 		StartPointer = ObjectStartRecord( Found );
// 	} else {
// 		StartPointer = 0;
// 	}

// 	return StartPointer;

// }

// int
// FindNextRecord(
// 	std::string const & UCObjType,
// 	int const StartPointer
// 	)
// {

// 	// FUNCTION INFORMATION:
// 	//       AUTHOR         Linda Lawrie
// 	//       DATE WRITTEN   July 2010
// 	//       MODIFIED       na
// 	//       RE-ENGINEERED  na

// 	// PURPOSE OF THIS FUNCTION:
// 	// Finds next record of Object Name

// 	int NextPointer = 0;
// 	for ( int ObjNum = StartPointer + 1; ObjNum <= NumIDFRecords; ++ObjNum ) {
// 		if ( IDFRecords( ObjNum ).Name != UCObjType ) continue;
// 		NextPointer = ObjNum;
// 		break;
// 	}
// 	return NextPointer;
// }

    void
    InputProcessor::AddRecordToOutputVariableStructure(
            std::string const &KeyValue,
            std::string const &VariableName
    ) {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   July 2010

        // PURPOSE OF THIS SUBROUTINE:
        // This routine adds a new record (if necessary) to the Output Variable
        // reporting structure.  DataOutputs, OutputVariablesForSimulation

        // METHODOLOGY EMPLOYED:
        // OutputVariablesForSimulation is a linked list structure for later
        // semi-easy perusal.

        // Using/Aliasing
        using namespace DataOutputs;

        int CurNum;
        int NextNum;
        bool FoundOne;
        std::string::size_type vnameLen; // if < length, there were units on the line/name

        std::string::size_type const rbpos = index(VariableName, '[');
        if (rbpos == std::string::npos) {
            vnameLen = len_trim(VariableName);
        } else {
            vnameLen = len_trim(VariableName.substr(0, rbpos));
        }

        FoundOne = false;
        std::string const VarName(VariableName.substr(0, vnameLen));
        for (CurNum = 1; CurNum <= NumConsideredOutputVariables; ++CurNum) {
            if (VarName == OutputVariablesForSimulation(CurNum).VarName) {
                FoundOne = true;
                break;
            }
        }

        if (!FoundOne) {
            if (NumConsideredOutputVariables == MaxConsideredOutputVariables) {
                ReAllocateAndPreserveOutputVariablesForSimulation();
            }
            ++NumConsideredOutputVariables;
            OutputVariablesForSimulation(NumConsideredOutputVariables).Key = KeyValue;
            OutputVariablesForSimulation(NumConsideredOutputVariables).VarName = VarName;
            OutputVariablesForSimulation(NumConsideredOutputVariables).Previous = 0;
            OutputVariablesForSimulation(NumConsideredOutputVariables).Next = 0;
        } else {
            if (KeyValue != OutputVariablesForSimulation(CurNum).Key) {
                NextNum = CurNum;
                if (OutputVariablesForSimulation(NextNum).Next != 0) {
                    while (OutputVariablesForSimulation(NextNum).Next != 0) {
                        CurNum = NextNum;
                        NextNum = OutputVariablesForSimulation(NextNum).Next;
                    }
                    if (NumConsideredOutputVariables == MaxConsideredOutputVariables) {
                        ReAllocateAndPreserveOutputVariablesForSimulation();
                    }
                    ++NumConsideredOutputVariables;
                    OutputVariablesForSimulation(NumConsideredOutputVariables).Key = KeyValue;
                    OutputVariablesForSimulation(NumConsideredOutputVariables).VarName = VarName;
                    OutputVariablesForSimulation(NumConsideredOutputVariables).Previous = NextNum;
                    OutputVariablesForSimulation(NextNum).Next = NumConsideredOutputVariables;
                } else {
                    if (NumConsideredOutputVariables == MaxConsideredOutputVariables) {
                        ReAllocateAndPreserveOutputVariablesForSimulation();
                    }
                    ++NumConsideredOutputVariables;
                    OutputVariablesForSimulation(NumConsideredOutputVariables).Key = KeyValue;
                    OutputVariablesForSimulation(NumConsideredOutputVariables).VarName = VarName;
                    OutputVariablesForSimulation(NumConsideredOutputVariables).Previous = CurNum;
                    OutputVariablesForSimulation(CurNum).Next = NumConsideredOutputVariables;
                }
            }
        }
    }

    void
    InputProcessor::ReAllocateAndPreserveOutputVariablesForSimulation() {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   April 2011

        // PURPOSE OF THIS SUBROUTINE:
        // This routine does a simple reallocate for the OutputVariablesForSimulation structure, preserving
        // the data that is already in the structure.

        using namespace DataOutputs;

        int const OutputVarAllocInc(500);

        // up allocation by OutputVarAllocInc
        OutputVariablesForSimulation.redimension(MaxConsideredOutputVariables += OutputVarAllocInc);
    }

// void
// DumpCurrentLineBuffer(
// 	int const StartLine,
// 	std::string const & cStartLine,
// 	std::string const & cStartName,
// 	int const CurLine,
// 	int const NumConxLines,
// 	Array1S_string const LineBuf,
// 	int const CurQPtr
// 	)
// {

// 	// SUBROUTINE INFORMATION:
// 	//       AUTHOR         Linda Lawrie
// 	//       DATE WRITTEN   February 2003
// 	//       MODIFIED       March 2012 - Que lines instead of holding all.
// 	//       RE-ENGINEERED  na

// 	// PURPOSE OF THIS SUBROUTINE:
// 	// This subroutine dumps the "context" lines for error messages detected by
// 	// the input processor.

// 	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
// 	int Line;
// 	//  INTEGER PLine
// 	int SLine;
// 	int CurPos;
// 	std::string cLineNo;
// 	std::string TextLine;

// 	ShowMessage( "IDF Context for following error/warning message:" );
// 	ShowMessage( "Note -- lines truncated at 300 characters, if necessary..." );
// 	if ( StartLine <= 99999 ) {
// 		gio::write( TextLine, "(1X,I5,1X,A)" ) << StartLine << cStartLine;
// 	} else {
// 		gio::write( cLineNo, fmtLD ) << StartLine;
// 		strip( cLineNo );
// 		gio::write( TextLine, "(1X,A,1X,A)" ) << cLineNo << cStartLine;
// 	}
// 	ShowMessage( TextLine );
// 	if ( ! cStartName.empty() ) {
// 		ShowMessage( "indicated Name=" + cStartName );
// 	}
// 	ShowMessage( "Only last " + IPTrimSigDigits( NumConxLines ) + " lines before error line shown....." );
// 	SLine = CurLine - NumConxLines + 1;
// 	if ( NumConxLines == isize( LineBuf ) ) {
// 		CurPos = CurQPtr + 1;
// 		if ( CurQPtr + 1 > isize( LineBuf ) ) CurPos = 1;
// 	} else {
// 		CurPos = 1;
// 	}
// 	for ( Line = 1; Line <= NumConxLines; ++Line ) {
// 		if ( SLine <= 99999 ) {
// 			gio::write( TextLine, "(1X,I5,1X,A)" ) << SLine << LineBuf( CurPos );
// 		} else {
// 			gio::write( cLineNo, fmtLD ) << SLine;
// 			strip( cLineNo );
// 			gio::write( TextLine, "(1X,A,1X,A)" ) << cLineNo << LineBuf( CurPos );
// 		}
// 		ShowMessage( TextLine );
// 		++CurPos;
// 		if ( CurPos > isize( LineBuf ) ) CurPos = 1;
// 		++SLine;
// 	}

// }

//void
//ShowAuditErrorMessage(
//		std::string const & Severity, // if blank, does not add to sum
//		std::string const & ErrorMessage
//		)
//{
//
//	// SUBROUTINE INFORMATION:
//	//       AUTHOR         Linda K. Lawrie
//	//       DATE WRITTEN   March 2003
//	//       MODIFIED       na
//	//       RE-ENGINEERED  na
//
//	// PURPOSE OF THIS SUBROUTINE:
//	// This subroutine is just for messages that will be displayed on the audit trail
//	// (echo of the input file).  Errors are counted and a summary is displayed after
//	// finishing the scan of the input file.
//
//	// METHODOLOGY EMPLOYED:
//	// na
//
//	// REFERENCES:
//	// na
//
//	// USE STATEMENTS:
//	// na
//
//	// Locals
//	// SUBROUTINE ARGUMENT DEFINITIONS:
//
//	// SUBROUTINE PARAMETER DEFINITIONS:
//	static gio::Fmt ErrorFormat( "(2X,A)" );
//
//	// INTERFACE BLOCK SPECIFICATIONS
//	// na
//
//	// DERIVED TYPE DEFINITIONS
//	// na
//
//	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
//	// na
//
//	if ( ! Severity.empty() ) {
//		++TotalAuditErrors;
//		gio::write( EchoInputFile, ErrorFormat ) << Severity + ErrorMessage;
//	} else {
//		gio::write( EchoInputFile, ErrorFormat ) << " ************* " + ErrorMessage;
//	}
//
//}

    std::string
    InputProcessor::IPTrimSigDigits(int const IntegerValue) {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   March 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function accepts a number as parameter as well as the number of
        // significant digits after the decimal point to report and returns a string
        // that is appropriate.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        std::string String; // Working string

        gio::write(String, fmtLD) << IntegerValue;
        return stripped(String);

    }
}
