#!/usr/bin/env python
# EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University
# of Illinois, The Regents of the University of California, through Lawrence
# Berkeley National Laboratory (subject to receipt of any required approvals
# from the U.S. Dept. of Energy), Oak Ridge National Laboratory, managed by UT-
# Battelle, Alliance for Sustainable Energy, LLC, and other contributors. All
# rights reserved.
#
# NOTICE: This Software was developed under funding from the U.S. Department of
# Energy and the U.S. Government consequently retains certain rights. As such,
# the U.S. Government has been granted for itself and others acting on its
# behalf a paid-up, nonexclusive, irrevocable, worldwide license in the
# Software to reproduce, distribute copies to the public, prepare derivative
# works, and perform publicly and display publicly, and to permit others to do
# so.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# (1) Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#
# (2) Redistributions in binary form must reproduce the above copyright notice,
#     this list of conditions and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#
# (3) Neither the name of the University of California, Lawrence Berkeley
#     National Laboratory, the University of Illinois, U.S. Dept. of Energy nor
#     the names of its contributors may be used to endorse or promote products
#     derived from this software without specific prior written permission.
#
# (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in
#     stand-alone form without changes from the version obtained under this
#     License, or (ii) Licensee makes a reference solely to the software
#     portion of its product, Licensee must refer to the software as
#     "EnergyPlus version X" software, where "X" is the version number Licensee
#     obtained under this License and may not use a different name for the
#     software. Except as specifically required in this Section (4), Licensee
#     shall not use in a company name, a product name, in advertising,
#     publicity, or other promotional activities any name, trade name,
#     trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or
#     confusingly similar designation, without the U.S. Department of Energy's
#     prior written consent.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

import os
import re
import sys
from pathlib import Path

sci_notation_str = r"[+-]?\d+\.?(\d+)?e[+-]?\d+"
sci_notation_pattern = re.compile(sci_notation_str)

int_real_str = r"[+-]?\d+\.?(\d+)?"
int_real_pattern = re.compile(int_real_str)

const_type_str = r"(const int|int const|const bool|bool const|const Real64|Real64 const|const double|double const)(?!(expr))"
const_type_pattern = re.compile(const_type_str)

var_name_str = r"[_a-zA-Z][_a-zA-Z0-9]*"
var_name_pattern = re.compile(var_name_str)

const_num_paren_str = r"CONSTTYPE VARNAME\((INTREAL|SCI)\);"
const_num_paren_str = const_num_paren_str.replace("CONSTTYPE", const_type_str)
const_num_paren_str = const_num_paren_str.replace("VARNAME", var_name_str)
const_num_paren_str = const_num_paren_str.replace("INTREAL", int_real_str)
const_num_paren_str = const_num_paren_str.replace("SCI", sci_notation_str)
const_num_paren_pattern = re.compile(const_num_paren_str)

const_num_equal_str = r"CONSTTYPE VARNAME = (INTREAL|SCI);"
const_num_equal_str = const_num_equal_str.replace("CONSTTYPE", const_type_str)
const_num_equal_str = const_num_equal_str.replace("VARNAME", var_name_str)
const_num_equal_str = const_num_equal_str.replace("INTREAL", int_real_str)
const_num_equal_str = const_num_equal_str.replace("SCI", sci_notation_str)
const_num_equal_pattern = re.compile(const_num_equal_str)

const_num_str = r"(PAREN|EQUAL)"
const_num_str = const_num_str.replace("PAREN", const_num_paren_str)
const_num_str = const_num_str.replace("EQUAL", const_num_equal_str)
const_num_pattern = re.compile(const_num_str)

array_const_str = r"(static )?Array[12345]D<(int|bool|double|Real64)> const VARNAME\("
array_const_str = array_const_str.replace("VARNAME", var_name_str)
array_const_pattern = re.compile(array_const_str)

array_us_const_str = r"(static )?Array[12345]D_(int|bool|double) const VARNAME\("
array_us_const_str = array_us_const_str.replace("VARNAME", var_name_str)
array_us_const_pattern = re.compile(array_us_const_str)


def test_array_underscore_const():
    # match these
    assert re.match(array_us_const_pattern, "Array1D_int const A(")
    assert re.match(array_us_const_pattern, "Array1D_int const Abc(")
    assert re.match(array_us_const_pattern, "Array1D_bool const A(")
    assert re.match(array_us_const_pattern, "Array1D_bool const Abc(")
    assert re.match(array_us_const_pattern, "Array1D_double const A(")
    assert re.match(array_us_const_pattern, "Array1D_double const Abc(")
    assert re.match(array_us_const_pattern, "Array2D_int const A(")
    assert re.match(array_us_const_pattern, "Array2D_int const Abc(")
    assert re.match(array_us_const_pattern, "Array2D_bool const A(")
    assert re.match(array_us_const_pattern, "Array2D_bool const Abc(")
    assert re.match(array_us_const_pattern, "Array2D_double const A(")
    assert re.match(array_us_const_pattern, "Array2D_double const Abc(")
    assert re.match(array_us_const_pattern, "static Array1D_int const A(")
    assert re.match(array_us_const_pattern, "static Array1D_int const Abc(")
    assert re.match(array_us_const_pattern, "static Array1D_bool const A(")
    assert re.match(array_us_const_pattern, "static Array1D_bool const Abc(")
    assert re.match(array_us_const_pattern, "static Array1D_double const A(")
    assert re.match(array_us_const_pattern, "static Array1D_double const Abc(")
    assert re.match(array_us_const_pattern, "static Array2D_int const A(")
    assert re.match(array_us_const_pattern, "static Array2D_int const Abc(")
    assert re.match(array_us_const_pattern, "static Array2D_bool const A(")
    assert re.match(array_us_const_pattern, "static Array2D_bool const Abc(")
    assert re.match(array_us_const_pattern, "static Array2D_double const A(")
    assert re.match(array_us_const_pattern, "static Array2D_double const Abc(")

    # don't match these
    assert not re.match(array_us_const_pattern, "Array1D_int A(")
    assert not re.match(array_us_const_pattern, "Array1D_int Abc(")
    assert not re.match(array_us_const_pattern, "Array1D_bool A(")
    assert not re.match(array_us_const_pattern, "Array1D_bool Abc(")
    assert not re.match(array_us_const_pattern, "Array1D_double A(")
    assert not re.match(array_us_const_pattern, "Array1D_double Abc(")
    assert not re.match(array_us_const_pattern, "static Array2D_int A(")
    assert not re.match(array_us_const_pattern, "static Array2D_int Abc(")
    assert not re.match(array_us_const_pattern, "static Array2D_bool A(")
    assert not re.match(array_us_const_pattern, "static Array2D_bool Abc(")
    assert not re.match(array_us_const_pattern, "static Array2D_double A(")
    assert not re.match(array_us_const_pattern, "static Array2D_double Abc(")


def test_array_const():
    # match these
    assert re.match(array_const_pattern, "Array1D<int> const A(")
    assert re.match(array_const_pattern, "Array1D<int> const Abc(")
    assert re.match(array_const_pattern, "Array1D<bool> const A(")
    assert re.match(array_const_pattern, "Array1D<bool> const Abc(")
    assert re.match(array_const_pattern, "Array1D<double> const A(")
    assert re.match(array_const_pattern, "Array1D<double> const Abc(")
    assert re.match(array_const_pattern, "Array2D<int> const A(")
    assert re.match(array_const_pattern, "Array2D<int> const Abc(")
    assert re.match(array_const_pattern, "Array2D<bool> const A(")
    assert re.match(array_const_pattern, "Array2D<bool> const Abc(")
    assert re.match(array_const_pattern, "Array2D<double> const A(")
    assert re.match(array_const_pattern, "Array2D<double> const Abc(")
    assert re.match(array_const_pattern, "static Array1D<int> const A(")
    assert re.match(array_const_pattern, "static Array1D<int> const Abc(")
    assert re.match(array_const_pattern, "static Array1D<bool> const A(")
    assert re.match(array_const_pattern, "static Array1D<bool> const Abc(")
    assert re.match(array_const_pattern, "static Array1D<double> const A(")
    assert re.match(array_const_pattern, "static Array1D<double> const Abc(")
    assert re.match(array_const_pattern, "static Array2D<int> const A(")
    assert re.match(array_const_pattern, "static Array2D<int> const Abc(")
    assert re.match(array_const_pattern, "static Array2D<bool> const A(")
    assert re.match(array_const_pattern, "static Array2D<bool> const Abc(")
    assert re.match(array_const_pattern, "static Array2D<double> const A(")
    assert re.match(array_const_pattern, "static Array2D<double> const Abc(")

    # don't match these
    assert not re.match(array_const_pattern, "Array1D<int> A(")
    assert not re.match(array_const_pattern, "Array1D<int> Abc(")
    assert not re.match(array_const_pattern, "Array1D<bool> A(")
    assert not re.match(array_const_pattern, "Array1D<bool> Abc(")
    assert not re.match(array_const_pattern, "Array1D<double> A(")
    assert not re.match(array_const_pattern, "Array1D<double> Abc(")
    assert not re.match(array_const_pattern, "static Array2D<int> A(")
    assert not re.match(array_const_pattern, "static Array2D<int> Abc(")
    assert not re.match(array_const_pattern, "static Array2D<bool> A(")
    assert not re.match(array_const_pattern, "static Array2D<bool> Abc(")
    assert not re.match(array_const_pattern, "static Array2D<double> A(")
    assert not re.match(array_const_pattern, "static Array2D<double> Abc(")


def test_const_num():
    # match these
    assert re.match(const_num_pattern, "const int VarName = 1;")
    assert re.match(const_num_pattern, "const int VarName(1);")
    assert re.match(const_num_pattern, "const int A(1);")

    # don't match these
    assert not re.match(const_num_pattern, "Real64 constexpr VarName = 1.e1;")
    assert not re.match(const_num_pattern, "constexpr int VarName = 1;")
    assert not re.match(const_num_pattern, "Real64 constexpr VarName(1.e1);")
    assert not re.match(const_num_pattern, "constexpr int VarName(1);")


def test_const_num_equal():
    # match these
    assert re.match(const_num_equal_pattern, "const int VarName = 1;")
    assert re.match(const_num_equal_pattern, "const int VarName = 1.0;")
    assert re.match(const_num_equal_pattern, "const int VarName = 1.0e1;")
    assert re.match(const_num_equal_pattern, "const int VarName = 1.e1;")
    assert re.match(const_num_equal_pattern, "int const VarName = 1;")
    assert re.match(const_num_equal_pattern, "int const VarName = 1.0;")
    assert re.match(const_num_equal_pattern, "int const VarName = 1.0e1;")
    assert re.match(const_num_equal_pattern, "int const VarName = 1.e1;")
    assert re.match(const_num_equal_pattern, "const bool VarName = 1;")
    assert re.match(const_num_equal_pattern, "const bool VarName = 1.0;")
    assert re.match(const_num_equal_pattern, "const bool VarName = 1.0e1;")
    assert re.match(const_num_equal_pattern, "const bool VarName = 1.e1;")
    assert re.match(const_num_equal_pattern, "bool const VarName = 1;")
    assert re.match(const_num_equal_pattern, "bool const VarName = 1.0;")
    assert re.match(const_num_equal_pattern, "bool const VarName = 1.0e1;")
    assert re.match(const_num_equal_pattern, "bool const VarName = 1.e1;")
    assert re.match(const_num_equal_pattern, "const double VarName = 1;")
    assert re.match(const_num_equal_pattern, "const double VarName = 1.0;")
    assert re.match(const_num_equal_pattern, "const double VarName = 1.0e1;")
    assert re.match(const_num_equal_pattern, "const double VarName = 1.e1;")
    assert re.match(const_num_equal_pattern, "double const VarName = 1;")
    assert re.match(const_num_equal_pattern, "double const VarName = 1.0;")
    assert re.match(const_num_equal_pattern, "double const VarName = 1.0e1;")
    assert re.match(const_num_equal_pattern, "double const VarName = 1.e1;")
    assert re.match(const_num_equal_pattern, "const Real64 VarName = 1;")
    assert re.match(const_num_equal_pattern, "const Real64 VarName = 1.0;")
    assert re.match(const_num_equal_pattern, "const Real64 VarName = 1.0e1;")
    assert re.match(const_num_equal_pattern, "const Real64 VarName = 1.e1;")
    assert re.match(const_num_equal_pattern, "Real64 const VarName = 1;")
    assert re.match(const_num_equal_pattern, "Real64 const VarName = 1.0;")
    assert re.match(const_num_equal_pattern, "Real64 const VarName = 1.0e1;")
    assert re.match(const_num_equal_pattern, "Real64 const VarName = 1.e1;")

    # don't match these
    assert not re.match(const_num_equal_pattern, "Real64 constexpr VarName = 1.e1;")
    assert not re.match(const_num_equal_pattern, "constexpr int VarName = 1;")


def test_const_num_paren():
    # match these
    assert re.match(const_num_paren_pattern, "const int VarName(1);")
    assert re.match(const_num_paren_pattern, "const int VarName(1.0);")
    assert re.match(const_num_paren_pattern, "const int VarName(1.0e1);")
    assert re.match(const_num_paren_pattern, "const int VarName(1.e1);")
    assert re.match(const_num_paren_pattern, "int const VarName(1);")
    assert re.match(const_num_paren_pattern, "int const VarName(1.0);")
    assert re.match(const_num_paren_pattern, "int const VarName(1.0e1);")
    assert re.match(const_num_paren_pattern, "int const VarName(1.e1);")
    assert re.match(const_num_paren_pattern, "const bool VarName(1);")
    assert re.match(const_num_paren_pattern, "const bool VarName(1.0);")
    assert re.match(const_num_paren_pattern, "const bool VarName(1.0e1);")
    assert re.match(const_num_paren_pattern, "const bool VarName(1.e1);")
    assert re.match(const_num_paren_pattern, "bool const VarName(1);")
    assert re.match(const_num_paren_pattern, "bool const VarName(1.0);")
    assert re.match(const_num_paren_pattern, "bool const VarName(1.0e1);")
    assert re.match(const_num_paren_pattern, "bool const VarName(1.e1);")
    assert re.match(const_num_paren_pattern, "const double VarName(1);")
    assert re.match(const_num_paren_pattern, "const double VarName(1.0);")
    assert re.match(const_num_paren_pattern, "const double VarName(1.0e1);")
    assert re.match(const_num_paren_pattern, "const double VarName(1.e1);")
    assert re.match(const_num_paren_pattern, "double const VarName(1);")
    assert re.match(const_num_paren_pattern, "double const VarName(1.0);")
    assert re.match(const_num_paren_pattern, "double const VarName(1.0e1);")
    assert re.match(const_num_paren_pattern, "double const VarName(1.e1);")
    assert re.match(const_num_paren_pattern, "const Real64 VarName(1);")
    assert re.match(const_num_paren_pattern, "const Real64 VarName(1.0);")
    assert re.match(const_num_paren_pattern, "const Real64 VarName(1.0e1);")
    assert re.match(const_num_paren_pattern, "const Real64 VarName(1.e1);")
    assert re.match(const_num_paren_pattern, "Real64 const VarName(1);")
    assert re.match(const_num_paren_pattern, "Real64 const VarName(1.0);")
    assert re.match(const_num_paren_pattern, "Real64 const VarName(1.0e1);")
    assert re.match(const_num_paren_pattern, "Real64 const VarName(1.e1);")

    # don't match these
    assert not re.match(const_num_paren_pattern, "Real64 constexpr VarName(1.e1);")
    assert not re.match(const_num_paren_pattern, "constexpr int VarName(1);")


def test_var_name():
    # match these
    assert re.match(var_name_pattern, "VarName")
    assert re.match(var_name_pattern, "VarName_")
    assert re.match(var_name_pattern, "_VarName_")
    assert re.match(var_name_pattern, "_VarName")
    assert re.match(var_name_pattern, "_VarName123")
    assert re.match(var_name_pattern, "VarName123")
    assert re.match(var_name_pattern, "VarName123_")
    assert re.match(var_name_pattern, "_VarName123_")

    # don't match these
    assert not re.match(var_name_pattern, "123")
    assert not re.match(var_name_pattern, "9.81")
    assert not re.match(var_name_pattern, "9.81e0")


def test_const_type():
    # match these
    assert re.match(const_type_pattern, "const int")
    assert re.match(const_type_pattern, "const bool")
    assert re.match(const_type_pattern, "const double")
    assert re.match(const_type_pattern, "const Real64")
    assert re.match(const_type_pattern, "int const")
    assert re.match(const_type_pattern, "bool const")
    assert re.match(const_type_pattern, "double const")
    assert re.match(const_type_pattern, "Real64 const")

    # don't match these
    assert not re.match(const_type_pattern, "constexpr int")
    assert not re.match(const_type_pattern, "constexpr bool")
    assert not re.match(const_type_pattern, "constexpr double")
    assert not re.match(const_type_pattern, "constexpr Real64")
    assert not re.match(const_type_pattern, "int constexpr")
    assert not re.match(const_type_pattern, "bool constexpr")
    assert not re.match(const_type_pattern, "double constexpr")
    assert not re.match(const_type_pattern, "Real64 constexpr")


def test_int_real():
    # match these
    assert re.match(int_real_pattern, "1")
    assert re.match(int_real_pattern, "+1")
    assert re.match(int_real_pattern, "-1")
    assert re.match(int_real_pattern, "1.0")
    assert re.match(int_real_pattern, "+1.0")
    assert re.match(int_real_pattern, "-1.0")

    # don't match these
    assert not re.match(int_real_pattern, "Var")
    assert not re.match(int_real_pattern, "Var123")


def test_sci_notation():
    # match these
    assert re.match(sci_notation_pattern, "9.81e0")
    assert re.match(sci_notation_pattern, "9.81e-0")
    assert re.match(sci_notation_pattern, "9.81e+0")
    assert re.match(sci_notation_pattern, "+9.81e0")
    assert re.match(sci_notation_pattern, "+9.81e-0")
    assert re.match(sci_notation_pattern, "+9.81e+0")
    assert re.match(sci_notation_pattern, "-9.81e0")
    assert re.match(sci_notation_pattern, "-9.81e-0")
    assert re.match(sci_notation_pattern, "-9.81e+0")
    assert re.match(sci_notation_pattern, "9.e0")
    assert re.match(sci_notation_pattern, "9.e-0")
    assert re.match(sci_notation_pattern, "9.e+0")
    assert re.match(sci_notation_pattern, "+9.e0")
    assert re.match(sci_notation_pattern, "+9.e-0")
    assert re.match(sci_notation_pattern, "+9.e+0")
    assert re.match(sci_notation_pattern, "-9.e0")
    assert re.match(sci_notation_pattern, "-9.e-0")
    assert re.match(sci_notation_pattern, "-9.e+0")

    # don't match
    assert not re.match(sci_notation_pattern, "VarName")


def constexpr_check(search_path: Path):
    errors_found = False

    files_to_search = []
    for p in [search_path]:
        for root, dirs, files in os.walk(p):
            for file in files:
                f_path = Path(root) / Path(file)
                f_extension = f_path.suffix
                if f_extension == ".hh":
                    files_to_search.append(f_path)
                elif f_extension == ".cc":
                    files_to_search.append(f_path)

    files_to_search.sort()

    for file in files_to_search:
        with open(file, "r") as f:
            lines = f.readlines()

        lines = [x.strip() for x in lines]

        bracket_count = 0

        for idx, line in enumerate(lines):

            # skip blank lines
            if line == "":
                continue

            # skip comment lines
            if line[0:2] == "//":
                continue

            # strip trailing comments
            if "//" in line:
                tokens = line.split("//")
                line = tokens[0].strip()

            if (re.match(const_num_pattern, line) or
                re.match(array_const_pattern, line) or re.match(array_us_const_pattern, line)) and bracket_count == 0:
                print("ERROR: 'const' found, convert to 'constexpr'")
                s = f"{file.name}: {idx + 1} - {line}"
                print(s)
                errors_found = True

            # count brackets in line
            bracket_count += line.count("(")
            bracket_count -= line.count(")")

    return errors_found


if __name__ == "__main__":

    # run tests
    if True:
        test_array_underscore_const()
        test_array_const()
        test_const_num()
        test_const_num_equal()
        test_const_num_paren()
        test_var_name()
        test_const_type()
        test_int_real()
        test_sci_notation()

    root_path = Path(__file__).parent.parent.parent
    src_path = root_path / "src" / "EnergyPlus"
    tst_path = root_path / "tst" / "EnergyPlus" / "unit"
    errors_found = constexpr_check(src_path)
    errors_found &= constexpr_check(tst_path)

    if errors_found:
        raise sys.exit(1)
