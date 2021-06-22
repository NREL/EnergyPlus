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

#ifndef UtilityRoutines_hh_INCLUDED
#define UtilityRoutines_hh_INCLUDED

// C++ Headers
#include <functional>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.fwd.hh>
#include <ObjexxFCL/MArray1.fwd.hh>
#include <ObjexxFCL/Optional.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
class InputOutputFile;
struct EnergyPlusData;

int AbortEnergyPlus(EnergyPlusData &state);

void CloseMiscOpenFiles(EnergyPlusData &state);

void CloseOutOpenFiles();

int EndEnergyPlus(EnergyPlusData &state);

void ConvertCaseToUpper(std::string const &InputString, // Input string
                        std::string &OutputString       // Output string (in UpperCase)
);

void ConvertCaseToLower(std::string const &InputString, // Input string
                        std::string &OutputString       // Output string (in LowerCase)
);

std::string::size_type FindNonSpace(std::string const &String); // String to be scanned

template <typename T> inline T pow2(T const &x)
{
    return x * x;
}

template <typename T> inline T pow3(T const &x)
{
    return x * x * x;
}

template <typename T> inline T pow4(T const &x)
{
    T y(x * x);
    return y * y;
}

template <typename T> inline T pow5(T const &x)
{
    T y(x * x);
    y *= y;
    return y * x;
}

template <typename T> inline T pow6(T const &x)
{
    T y(x * x);
    y *= y;
    return y * y;
}

template <typename T> inline T pow7(T const &x)
{
    T y(x * x);
    y *= y;
    y *= y;
    return y * x;
}

bool env_var_on(std::string const &env_var_str);

class FatalError : public std::runtime_error
{
public:
    FatalError(std::string const &msg) : runtime_error(msg)
    {
    }
};

using OptionalOutputFileRef = Optional<std::reference_wrapper<EnergyPlus::InputOutputFile>>;

void ShowFatalError(EnergyPlusData &state, std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1 = _, OptionalOutputFileRef OutUnit2 = _);

void ShowSevereError(EnergyPlusData &state, std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1 = _, OptionalOutputFileRef OutUnit2 = _);

void ShowSevereMessage(EnergyPlusData &state,
                       std::string const &ErrorMessage,
                       OptionalOutputFileRef OutUnit1 = _,
                       OptionalOutputFileRef OutUnit2 = _);

void ShowContinueError(EnergyPlusData &state, std::string const &Message, OptionalOutputFileRef OutUnit1 = _, OptionalOutputFileRef OutUnit2 = _);

void ShowContinueErrorTimeStamp(EnergyPlusData &state,
                                std::string const &Message,
                                OptionalOutputFileRef OutUnit1 = _,
                                OptionalOutputFileRef OutUnit2 = _);

void ShowMessage(EnergyPlusData &state, std::string const &Message, OptionalOutputFileRef OutUnit1 = _, OptionalOutputFileRef OutUnit2 = _);

void ShowWarningError(EnergyPlusData &state, std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1 = _, OptionalOutputFileRef OutUnit2 = _);

void ShowWarningMessage(EnergyPlusData &state,
                        std::string const &ErrorMessage,
                        OptionalOutputFileRef OutUnit1 = _,
                        OptionalOutputFileRef OutUnit2 = _);

void ShowRecurringSevereErrorAtEnd(EnergyPlusData &state,
                                   std::string const &Message,             // Message automatically written to "error file" at end of simulation
                                   int &MsgIndex,                          // Recurring message index, if zero, next available index is assigned
                                   Optional<Real64 const> ReportMaxOf = _, // Track and report the max of the values passed to this argument
                                   Optional<Real64 const> ReportMinOf = _, // Track and report the min of the values passed to this argument
                                   Optional<Real64 const> ReportSumOf = _, // Track and report the sum of the values passed to this argument
                                   std::string const &ReportMaxUnits = "", // optional char string (<=15 length) of units for max value
                                   std::string const &ReportMinUnits = "", // optional char string (<=15 length) of units for min value
                                   std::string const &ReportSumUnits = ""  // optional char string (<=15 length) of units for sum value
);

void ShowRecurringWarningErrorAtEnd(EnergyPlusData &state,
                                    std::string const &Message,             // Message automatically written to "error file" at end of simulation
                                    int &MsgIndex,                          // Recurring message index, if zero, next available index is assigned
                                    Optional<Real64 const> ReportMaxOf = _, // Track and report the max of the values passed to this argument
                                    Optional<Real64 const> ReportMinOf = _, // Track and report the min of the values passed to this argument
                                    Optional<Real64 const> ReportSumOf = _, // Track and report the sum of the values passed to this argument
                                    std::string const &ReportMaxUnits = "", // optional char string (<=15 length) of units for max value
                                    std::string const &ReportMinUnits = "", // optional char string (<=15 length) of units for min value
                                    std::string const &ReportSumUnits = ""  // optional char string (<=15 length) of units for sum value
);

void ShowRecurringContinueErrorAtEnd(EnergyPlusData &state,
                                     std::string const &Message,             // Message automatically written to "error file" at end of simulation
                                     int &MsgIndex,                          // Recurring message index, if zero, next available index is assigned
                                     Optional<Real64 const> ReportMaxOf = _, // Track and report the max of the values passed to this argument
                                     Optional<Real64 const> ReportMinOf = _, // Track and report the min of the values passed to this argument
                                     Optional<Real64 const> ReportSumOf = _, // Track and report the sum of the values passed to this argument
                                     std::string const &ReportMaxUnits = "", // optional char string (<=15 length) of units for max value
                                     std::string const &ReportMinUnits = "", // optional char string (<=15 length) of units for min value
                                     std::string const &ReportSumUnits = ""  // optional char string (<=15 length) of units for sum value
);

void StoreRecurringErrorMessage(EnergyPlusData &state,
                                std::string const &ErrorMessage,             // Message automatically written to "error file" at end of simulation
                                int &ErrorMsgIndex,                          // Recurring message index, if zero, next available index is assigned
                                Optional<Real64 const> ErrorReportMaxOf = _, // Track and report the max of the values passed to this argument
                                Optional<Real64 const> ErrorReportMinOf = _, // Track and report the min of the values passed to this argument
                                Optional<Real64 const> ErrorReportSumOf = _, // Track and report the sum of the values passed to this argument
                                std::string const &ErrorReportMaxUnits = "", // Units for "max" reporting
                                std::string const &ErrorReportMinUnits = "", // Units for "min" reporting
                                std::string const &ErrorReportSumUnits = ""  // Units for "sum" reporting
);

void ShowErrorMessage(EnergyPlusData &state, std::string const &ErrorMessage, OptionalOutputFileRef OutUnit1 = _, OptionalOutputFileRef OutUnit2 = _);

void SummarizeErrors(EnergyPlusData &state);

void ShowRecurringErrors(EnergyPlusData &state);

namespace UtilityRoutines {

    template <class T> struct is_shared_ptr : std::false_type
    {
    };
    template <class T> struct is_shared_ptr<std::shared_ptr<T>> : std::true_type
    {
    };

    Real64 ProcessNumber(std::string const &String, bool &ErrorFlag);

    int FindItemInList(std::string const &String, Array1_string const &ListOfItems, int NumItems);

    inline int FindItemInList(std::string const &String, Array1_string const &ListOfItems)
    {
        return UtilityRoutines::FindItemInList(String, ListOfItems, ListOfItems.isize());
    }

    int FindItemInList(std::string const &String, Array1S_string const ListOfItems, int NumItems);

    inline int FindItemInList(std::string const &String, Array1S_string const ListOfItems)
    {
        return UtilityRoutines::FindItemInList(String, ListOfItems, ListOfItems.isize());
    }

    template <typename A> inline int FindItemInList(std::string const &String, MArray1<A, std::string> const &ListOfItems, int const NumItems)
    {
        for (int Count = 1; Count <= NumItems; ++Count) {
            if (String == ListOfItems(Count)) return Count;
        }
        return 0; // Not found
    }

    template <typename A> inline int FindItemInList(std::string const &String, MArray1<A, std::string> const &ListOfItems)
    {
        return UtilityRoutines::FindItemInList(String, ListOfItems, ListOfItems.isize());
    }

    template <typename Container, class = typename std::enable_if<!std::is_same<typename Container::value_type, std::string>::value>::type>
    // Container needs and operator[i] and elements need Name
    inline int FindItemInList(std::string const &String, Container const &ListOfItems, int const NumItems)
    {
        for (typename Container::size_type i = 0, e = NumItems; i < e; ++i) {
            if (String == ListOfItems[i].Name) return int(i + 1); // 1-based return index
        }
        return 0; // Not found
    }

    template <typename Container, class = typename std::enable_if<!std::is_same<typename Container::value_type, std::string>::value>::type>
    // Container needs isize() and operator[i] and elements need Name
    inline int FindItemInList(std::string const &String, Container const &ListOfItems)
    {
        return UtilityRoutines::FindItemInList(String, ListOfItems, ListOfItems.isize());
    }

    template <typename Container, class = typename std::enable_if<!std::is_same<typename Container::value_type, std::string>::value>::type>
    // Container needs operator[i] and value_type
    inline int FindItemInList(std::string const &String, Container const &ListOfItems, std::string Container::value_type::*name_p, int const NumItems)
    {
        for (typename Container::size_type i = 0, e = NumItems; i < e; ++i) {
            if (String == ListOfItems[i].*name_p) return int(i + 1); // 1-based return index
        }
        return 0; // Not found
    }

    template <typename Container, class = typename std::enable_if<!std::is_same<typename Container::value_type, std::string>::value>::type>
    // Container needs isize() and operator[i] and value_type
    inline int FindItemInList(std::string const &String, Container const &ListOfItems, std::string Container::value_type::*name_p)
    {
        return UtilityRoutines::FindItemInList(String, ListOfItems, name_p, ListOfItems.isize());
    }

    int FindItemInSortedList(std::string const &String, Array1S_string const ListOfItems, int NumItems);

    inline int FindItemInSortedList(std::string const &String, Array1S_string const ListOfItems)
    {
        return FindItemInSortedList(String, ListOfItems, ListOfItems.isize());
    }

    template <typename A> inline int FindItemInSortedList(std::string const &String, MArray1<A, std::string> const &ListOfItems, int const NumItems)
    {
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

    template <typename A> inline int FindItemInSortedList(std::string const &String, MArray1<A, std::string> const &ListOfItems)
    {
        return FindItemInSortedList(String, ListOfItems, ListOfItems.isize());
    }

    template <typename InputIterator> inline int FindItem(InputIterator first, InputIterator last, std::string const &str, std::false_type)
    {
        using valueType = typename std::iterator_traits<InputIterator>::value_type;
        // static_assert( std::is_convertible< decltype( std::declval< valueType >() ), Named >::value, "Iterator value must inherit from class Named"
        // );

        auto const it = std::find_if(first, last, [&str](const valueType &s) { return s.name == str; });
        if (it != last) return it - first + 1; // 1-based return index

        auto const it2 = std::find_if(first, last, [&str](const valueType &s) { return equali(s.name, str); });
        if (it2 != last) return it2 - first + 1; // 1-based return index

        return 0; // Not found
    }

    template <typename InputIterator> inline int FindItem(InputIterator first, InputIterator last, std::string const &str, std::true_type)
    {
        using valueType = typename std::iterator_traits<InputIterator>::value_type;
        // static_assert( std::is_convertible< decltype( *std::declval< valueType >() ), Named >::value, "Iterator value must inherit from class
        // Named" );

        auto const it = std::find_if(first, last, [&str](const valueType &s) { return s->name == str; });
        if (it != last) return it - first + 1; // 1-based return index

        auto const it2 = std::find_if(first, last, [&str](const valueType &s) { return equali(s->name, str); });
        if (it2 != last) return it2 - first + 1; // 1-based return index

        return 0; // Not found
    }

    template <typename InputIterator> inline int FindItem(InputIterator first, InputIterator last, std::string const &str)
    {
        return FindItem(first, last, str, is_shared_ptr<typename std::iterator_traits<InputIterator>::value_type>{});
    }

    int FindItem(std::string const &String, Array1D_string const &ListOfItems, int const NumItems);

    inline int FindItem(std::string const &String, Array1D_string const &ListOfItems)
    {
        return FindItem(String, ListOfItems, ListOfItems.isize());
    }

    int FindItem(std::string const &String, Array1S_string const ListOfItems, int const NumItems);

    inline int FindItem(std::string const &String, Array1S_string const ListOfItems)
    {
        return FindItem(String, ListOfItems, ListOfItems.isize());
    }

    template <typename A> inline int FindItem(std::string const &String, MArray1<A, std::string> const &ListOfItems, int const NumItems)
    {
        int const item_number(UtilityRoutines::FindItemInList(String, ListOfItems, NumItems));
        if (item_number != 0) return item_number;
        for (int Count = 1; Count <= NumItems; ++Count) {
            if (equali(String, ListOfItems(Count))) return Count;
        }
        return 0; // Not found
    }

    template <typename A> inline int FindItem(std::string const &String, MArray1<A, std::string> const &ListOfItems)
    {
        return FindItem(String, ListOfItems, ListOfItems.isize());
    }

    template <typename Container, class = typename std::enable_if<!std::is_same<typename Container::value_type, std::string>::value>::type>
    // Container needs size() and operator[i] and elements need Name
    inline int FindItem(std::string const &String, Container const &ListOfItems, int const NumItems)
    {
        int const item_number(UtilityRoutines::FindItemInList(String, ListOfItems, NumItems));
        if (item_number != 0) return item_number;
        for (typename Container::size_type i = 0, e = NumItems; i < e; ++i) {
            if (equali(String, ListOfItems[i].Name)) return i + 1; // 1-based return index
        }
        return 0; // Not found
    }

    template <typename Container, class = typename std::enable_if<!std::is_same<typename Container::value_type, std::string>::value>::type>
    // Container needs size() and operator[i] and elements need Name
    inline int FindItem(std::string const &String, Container const &ListOfItems)
    {
        return FindItem(String, ListOfItems, ListOfItems.isize());
    }

    template <typename Container, class = typename std::enable_if<!std::is_same<typename Container::value_type, std::string>::value>::type>
    // Container needs size() and operator[i] and value_type
    inline int FindItem(std::string const &String, Container const &ListOfItems, std::string Container::value_type::*name_p, int const NumItems)
    {
        int const item_number(UtilityRoutines::FindItemInList(String, ListOfItems, name_p, NumItems));
        if (item_number != 0) return item_number;
        for (typename Container::size_type i = 0, e = NumItems; i < e; ++i) {
            if (equali(String, ListOfItems[i].*name_p)) return i + 1; // 1-based return index
        }
        return 0; // Not found
    }

    template <typename Container, class = typename std::enable_if<!std::is_same<typename Container::value_type, std::string>::value>::type>
    // Container needs size() and operator[i] and value_type
    inline int FindItem(std::string const &String, Container const &ListOfItems, std::string Container::value_type::*name_p)
    {
        return FindItem(String, ListOfItems, name_p, ListOfItems.isize());
    }

    std::string MakeUPPERCase(std::string const &InputString); // Input String

    inline bool SameString(std::string const &s, std::string const &t)
    {
        // case insensitive comparison
        return equali(s, t);
    }

    typedef char const *c_cstring;

    inline bool SameString(std::string const &s, c_cstring const &t)
    {
        // case insensitive comparison
        return equali(s, t);
    }

    inline bool SameString(c_cstring const &s, std::string const &t)
    {
        // case insensitive comparison
        return equali(s, t);
    }

    inline bool SameString(c_cstring const &s, c_cstring const &t)
    {
        // case insensitive comparison
        return equali(s, t);
    }

    template <typename InputIterator>
    inline void VerifyName(EnergyPlusData &state,
                           InputIterator first,
                           InputIterator last,
                           std::string const &NameToVerify,
                           bool &ErrorFound,
                           bool &IsBlank,
                           std::string const &StringToDisplay)
    {
        IsBlank = false;
        ErrorFound = false;
        if (NameToVerify.empty()) {
            ShowSevereError(state, StringToDisplay + ", cannot be blank");
            ErrorFound = true;
            IsBlank = true;
            return;
        }
        int Found = FindItem(first, last, NameToVerify);
        if (Found != 0) {
            ShowSevereError(state, StringToDisplay + ", duplicate name=" + NameToVerify);
            ErrorFound = true;
        }
    }

    void VerifyName(EnergyPlusData &state,
                    std::string const &NameToVerify,
                    Array1D_string const &NamesList,
                    int const NumOfNames,
                    bool &ErrorFound,
                    bool &IsBlank,
                    std::string const &StringToDisplay);

    void VerifyName(EnergyPlusData &state,
                    std::string const &NameToVerify,
                    Array1S_string const NamesList,
                    int const NumOfNames,
                    bool &ErrorFound,
                    bool &IsBlank,
                    std::string const &StringToDisplay);

    template <typename A>
    inline void VerifyName(EnergyPlusData &state,
                           std::string const &NameToVerify,
                           MArray1<A, std::string> const &NamesList,
                           int const NumOfNames,
                           bool &ErrorFound,
                           bool &IsBlank,
                           std::string const &StringToDisplay)
    { // Overload for member arrays: Implemented here to avoid copy to Array_string to forward to other VerifyName
        ErrorFound = false;
        if (NumOfNames > 0) {
            int const Found = FindItem(NameToVerify, NamesList,
                                       NumOfNames); // Calls FindItem overload that accepts member arrays
            if (Found != 0) {
                ShowSevereError(state, StringToDisplay + ", duplicate name=" + NameToVerify);
                ErrorFound = true;
            }
        }

        if (NameToVerify.empty()) {
            ShowSevereError(state, StringToDisplay + ", cannot be blank");
            ErrorFound = true;
            IsBlank = true;
        } else {
            IsBlank = false;
        }
    }

    template <typename Container, class = typename std::enable_if<!std::is_same<typename Container::value_type, std::string>::value>::type>
    // Container needs size() and operator[i] and elements need Name
    inline void VerifyName(EnergyPlusData &state,
                           std::string const &NameToVerify,
                           Container const &NamesList,
                           int const NumOfNames,
                           bool &ErrorFound,
                           bool &IsBlank,
                           std::string const &StringToDisplay)
    {
        ErrorFound = false;
        if (NumOfNames > 0) {
            int const Found = FindItem(NameToVerify, NamesList,
                                       NumOfNames); // Calls FindItem overload that accepts member arrays
            if (Found != 0) {
                ShowSevereError(state, StringToDisplay + ", duplicate name=" + NameToVerify);
                ErrorFound = true;
            }
        }

        if (NameToVerify.empty()) {
            ShowSevereError(state, StringToDisplay + ", cannot be blank");
            ErrorFound = true;
            IsBlank = true;
        } else {
            IsBlank = false;
        }
    }

    template <typename Container, class = typename std::enable_if<!std::is_same<typename Container::value_type, std::string>::value>::type>
    // Container needs size() and operator[i] and value_type
    inline void VerifyName(EnergyPlusData &state,
                           std::string const &NameToVerify,
                           Container const &NamesList,
                           std::string Container::value_type::*name_p,
                           int const NumOfNames,
                           bool &ErrorFound,
                           bool &IsBlank,
                           std::string const &StringToDisplay)
    {
        ErrorFound = false;
        if (NumOfNames > 0) {
            int const Found = FindItem(NameToVerify, NamesList, name_p, NumOfNames);
            if (Found != 0) {
                ShowSevereError(state, StringToDisplay + ", duplicate name=" + NameToVerify);
                ErrorFound = true;
            }
        }

        if (NameToVerify.empty()) {
            ShowSevereError(state, StringToDisplay + ", cannot be blank");
            ErrorFound = true;
            IsBlank = true;
        } else {
            IsBlank = false;
        }
    }

    bool IsNameEmpty(EnergyPlusData &state, std::string &NameToVerify, std::string const &StringToDisplay, bool &ErrorFound);

    // Two structs for case insensitive containers.
    // Eg: for unordered_map, we need to have a case insenstive hasher and a case insensitive comparator
    // (The default allocator for unordered_map is fine)
    // For map, you'd only need the comparator
    struct case_insensitive_hasher
    {
        size_t operator()(const std::string &key) const noexcept;
    };

    struct case_insensitive_comparator
    {
        bool operator()(const std::string &a, const std::string &b) const noexcept;
    };

    void appendPerfLog(EnergyPlusData &state, std::string const &colHeader, std::string const &colValue, bool finalColumn = false);

    bool ValidateFuelType(EnergyPlusData &state,
                          std::string const &FuelTypeInput,
                          std::string &FuelTypeOutput,
                          bool &FuelTypeErrorsFound,
                          bool const &AllowSteamAndDistrict = false);

    bool ValidateFuelTypeWithAssignResourceTypeNum(std::string const &FuelTypeInput,
                                                   std::string &FuelTypeOutput,
                                                   DataGlobalConstants::ResourceType &FuelTypeNum,
                                                   bool &FuelTypeErrorsFound);

} // namespace UtilityRoutines

struct UtilityRoutinesData : BaseGlobalStruct
{

    bool outputErrorHeader = true;
    std::string appendPerfLog_headerRow;
    std::string appendPerfLog_valuesRow;
    bool GetMatrixInputFlag = true;

    void clear_state() override
    {
        outputErrorHeader = true;
        appendPerfLog_headerRow.clear();
        appendPerfLog_valuesRow.clear();
        GetMatrixInputFlag = true;
    }

    // Default Constructor
    UtilityRoutinesData() = default;
};
} // namespace EnergyPlus

#endif
