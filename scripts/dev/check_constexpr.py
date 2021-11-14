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
