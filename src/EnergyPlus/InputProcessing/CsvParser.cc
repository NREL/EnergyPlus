// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#include <EnergyPlus/InputProcessing/CsvParser.hh>
#include <fast_float/fast_float.h>
#include <fmt/format.h>
#include <milo/dtoa.h>
#include <milo/itoa.h>

using json = nlohmann::json;

std::vector<std::string> const &CsvParser::errors()
{
    return errors_;
}

std::vector<std::string> const &CsvParser::warnings()
{
    return warnings_;
}

bool CsvParser::hasErrors()
{
    return !errors_.empty();
}

json CsvParser::decode(std::string_view csv, char user_delimiter, int user_rows_to_skip)
{
    bool success = true;
    return decode(csv, csv.size(), success, user_delimiter, user_rows_to_skip);
}

json CsvParser::decode(std::string_view csv, bool &success, char user_delimiter, int user_rows_to_skip)
{
    return decode(csv, csv.size(), success, user_delimiter, user_rows_to_skip);
}

json CsvParser::decode(std::string_view csv, size_t _csv_size, char user_delimiter, int user_rows_to_skip)
{
    bool success = true;
    return decode(csv, _csv_size, success, user_delimiter, user_rows_to_skip);
}

json CsvParser::decode(std::string_view csv, size_t _csv_size, bool &success, char _delimiter, int _rows_to_skip)
{
    if (csv.empty()) {
        success = false;
        return nullptr;
    }

    success = true;
    cur_line_num = 1;
    index_into_cur_line = 0;
    beginning_of_line_index = 0;
    delimiter = _delimiter;
    rows_to_skip = _rows_to_skip;
    csv_size = _csv_size;

    size_t index = 0;
    return parse_csv(csv, index, success);
}

std::string CsvParser::encode(json const &root)
{
    std::string encoded;
    if (csv_size > 0) {
        encoded.reserve(csv_size);
    } else {
        encoded.reserve(root["header"].size() * 8760 * 2 * 3);
    }

    return encoded;
}

void CsvParser::skip_rows(std::string_view csv, size_t &index)
{
    Token token;
    int rows_skipped = 0;
    while (true) {
        token = next_token(csv, index);
        if (token == Token::FILE_END) {
            break;
        } else if (token == Token::LINE_END) {
            ++rows_skipped;
            if (rows_skipped == rows_to_skip) {
                break;
            }
        }
    }
}

int CsvParser::find_number_columns(std::string_view csv, size_t &index)
{
    Token token;
    int num_columns = 0;

    size_t save_index = index;
    size_t save_line_num = cur_line_num;
    size_t save_line_index = index_into_cur_line;
    size_t save_beginning_of_line_index = beginning_of_line_index;

    while (true) {
        token = next_token(csv, save_index);
        if (token == Token::FILE_END) {
            break;
        } else if (token == Token::DELIMITER) {
            ++num_columns;
        } else if (token == Token::LINE_END) {
            ++num_columns;
            break;
        }
    }

    cur_line_num = save_line_num;
    index_into_cur_line = save_line_index;
    beginning_of_line_index = save_beginning_of_line_index;

    return num_columns;
}

json CsvParser::parse_csv(std::string_view csv, size_t &index, bool &success)
{
    json root = {{"header", json::array()}, {"values", json::array()}};
    bool check_first_row = true;
    bool has_header = (rows_to_skip == 1);

    if (csv_size > 3) {
        // UTF-8 Byte Order Mark
        if (csv[0] == '\xEF' && csv[1] == '\xBB' && csv[2] == '\xBF') {
            index += 3;
            index_into_cur_line += 3;
        }
    }

    if (rows_to_skip > 1) {
        skip_rows(csv, index);
    }

    json &header = root["header"];
    json &columns = root["values"];
    while (true) {
        if (index == csv_size) {
            break;
        } else {
            if (check_first_row) {
                int num_columns = find_number_columns(csv, index);
                check_first_row = !check_first_row;

                for (int i = 0; i < num_columns; ++i) {
                    auto arr = std::vector<json>();
                    arr.reserve(8764 * 4);
                    columns.push_back(std::move(arr));
                }

                if (has_header) {
                    parse_header(csv, index, success, header);
                }
                continue;
            }

            parse_line(csv, index, columns);
            if (!success) {
                auto found_index = csv.find_first_of('\n', beginning_of_line_index);
                std::string line;
                if (found_index != std::string::npos) {
                    line = csv.substr(beginning_of_line_index, found_index - beginning_of_line_index);
                }
                errors_.emplace_back(fmt::format("Line: {} Index: {} - Parsing Error. Error in following line.", cur_line_num, index_into_cur_line));
                errors_.emplace_back(fmt::format("~~~ {}", line));
                success = false;
                continue;
            }
        }
    }

    return root;
}

void CsvParser::parse_header(std::string_view csv, size_t &index, bool &success, json &header)
{
    Token token;

    while (true) {
        token = look_ahead(csv, index);
        if (token == Token::LINE_END || token == Token::FILE_END) {
            next_token(csv, index);
            return;
        } else if (token == Token::DELIMITER) {
            next_token(csv, index);
        } else {
            header.push_back(parse_value(csv, index));
            if (!success) return;
        }
    }
}

void CsvParser::parse_line(std::string_view csv, size_t &index, json &columns)
{
    Token token;
    int column_num = 0;

    while (true) {
        token = look_ahead(csv, index);
        if (token == Token::LINE_END || token == Token::FILE_END) {
            next_token(csv, index);
            return;
        } else if (token == Token::DELIMITER) {
            next_token(csv, index);
            ++column_num;
        } else {
            columns.at(column_num).push_back(parse_value(csv, index));
            //            if (!success) return;
        }
    }
}

// json CsvParser::parse_value(std::string_view csv, size_t &index, bool &success)
//{
//    Token token;
//    token = look_ahead(csv, index);
//
//    switch (token) {
//        case Token::STRING: {
//            return parse_string(csv, index);
//        }
//        case Token::NUMBER: {
//            return parse_number(csv, index);
//        }
//        case Token::FILE_END:
//        case Token::LINE_END:
//        case Token::DELIMITER:
//        default:
//            break;
//    }
//    success = false;
//    return nullptr;
//}

json CsvParser::parse_value(std::string_view csv, size_t &index)
{
    eat_whitespace(csv, index);

    size_t save_i = index;

    while (true) {
        if (save_i == csv_size) {
            break;
        }

        char const c = csv[save_i];
        if (c == delimiter || c == '\n' || c == '\r') {
            break;
        }
        ++save_i;
    }

    auto diff = save_i - index;
    auto value = csv.substr(index, diff);
    index_into_cur_line += diff;
    index = save_i;

    size_t plus_sign = 0;
    if (value.front() == '+') {
        plus_sign = 1;
    }

    auto const value_end = value.data() + value.size(); // have to do this for MSVC

    double val;
    auto result = fast_float::from_chars(value.data() + plus_sign, value.data() + value.size(), val);
    if (result.ec == std::errc::invalid_argument || result.ec == std::errc::result_out_of_range) {
        return rtrim(value);
    } else if (result.ptr != value_end) {
        auto const initial_ptr = result.ptr;
        while (delimiter != ' ' && result.ptr != value_end) {
            if (*result.ptr != ' ') {
                break;
            }
            ++result.ptr;
        }
        if (result.ptr == value_end) {
            index -= (value_end - initial_ptr);
            index_into_cur_line -= (value_end - initial_ptr);
            return val;
        }
        return rtrim(value);
    }
    //    double integral;
    //    double fractional = std::modf(val, &integral);
    //    if (fractional == 0) {
    //        return static_cast<int>(fractional);
    //    }
    return val;

    //    auto const convert_double = [](std::string_view str) -> json {
    //        double val;
    //        auto result = fast_float::from_chars(str.data(), str.data() + str.size(), val);
    //        if (result.ec == std::errc::invalid_argument || result.ec == std::errc::result_out_of_range || result.ptr != str.end()) {
    //            return rtrim(str);
    //        }
    //        return val;
    //    };
    //
    //    auto const convert_int = [&convert_double](std::string_view str) -> json {
    //        int val;
    //        auto result = std::from_chars(str.data(), str.data() + str.size(), val);
    //        if (result.ec == std::errc::result_out_of_range) {
    //            return convert_double(str);
    //        } else if (result.ec == std::errc::invalid_argument) {
    //            if (*result.ptr == '.') {
    //                return convert_double(str);
    //            } else {
    //                return rtrim(str);
    //            }
    //        } else if (result.ptr != str.end()) {
    //            if (*result.ptr == '.' || *result.ptr == 'e' || *result.ptr == 'E') {
    //                return convert_double(str);
    //            } else {
    //                return rtrim(str);
    //            }
    //        }
    //        return val;
    //    };
    //
    //    return convert_int(value);
}

// std::string CsvParser::parse_string(std::string_view csv, size_t &index)
//{
//    eat_whitespace(csv, index);
//
//    std::string str;
//    char c;
//
//    while (true) {
//        if (index == csv_size) {
//            break;
//        }
//
//        c = csv[index];
//        increment_both_index(index, index_into_cur_line);
//        if (c == delimiter || c == '\n') {
//            decrement_both_index(index, index_into_cur_line);
//            break;
//        } else if (c == '\r') {
//            continue;
//        } else {
//            str += c;
//        }
//    }
//
//    return rtrim(str);
//}

CsvParser::Token CsvParser::look_ahead(std::string_view csv, size_t index)
{
    size_t save_index = index;
    size_t save_line_num = cur_line_num;
    size_t save_line_index = index_into_cur_line;
    size_t save_beginning_of_line_index = beginning_of_line_index;
    Token token = next_token(csv, save_index);
    cur_line_num = save_line_num;
    index_into_cur_line = save_line_index;
    beginning_of_line_index = save_beginning_of_line_index;
    return token;
}

CsvParser::Token CsvParser::next_token(std::string_view csv, size_t &index)
{
    eat_whitespace(csv, index);

    if (index == csv_size) {
        return Token::FILE_END;
    }

    char const c = csv[index];
    if (c == delimiter) {
        increment_both_index(index, index_into_cur_line);
        return Token::DELIMITER;
    } else if (c == '\n') {
        increment_both_index(index, cur_line_num);
        beginning_of_line_index = index;
        index_into_cur_line = 0;
        return Token::LINE_END;
    }
    increment_both_index(index, index_into_cur_line);
    return Token::VALUE;
}

std::string_view CsvParser::rtrim(std::string_view str)
{
    static constexpr std::string_view whitespace(" \t", 2);
    if (str.empty()) {
        return str;
    }
    auto const index = str.find_last_not_of(whitespace);
    if (index == std::string::npos) {
        str.remove_suffix(str.size());
        return str;
    } else if (index + 1 < str.length()) {
        return str.substr(0, index + 1);
    }
    return str;
}

void CsvParser::increment_both_index(size_t &index, size_t &line_index)
{
    index++;
    line_index++;
}

void CsvParser::decrement_both_index(size_t &index, size_t &line_index)
{
    index--;
    line_index--;
}

void CsvParser::eat_whitespace(std::string_view csv, size_t &index)
{
    while (index < csv_size) {
        if ((delimiter != ' ' && csv[index] == ' ') || (delimiter != '\t' && csv[index] == '\t') || csv[index] == '\r') {
            increment_both_index(index, index_into_cur_line);
            continue;
        } else {
            return;
        }
    }
}
