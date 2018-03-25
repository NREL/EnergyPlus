// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

#ifndef EnergyPlusFixture_hh_INCLUDED
#define EnergyPlusFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include <memory>
#include <ostream>

namespace EnergyPlus {

// This is a helper struct to redirect std::cout. This makes sure std::cout is redirected back and
// everything is cleaned up properly
struct RedirectCout
{
    RedirectCout(std::unique_ptr<std::ostringstream> const &m_buffer) : m_old_buffer(std::cout.rdbuf(m_buffer->rdbuf()))
    {
    }

    ~RedirectCout()
    {
        std::cout.rdbuf(m_old_buffer.release());
    }

private:
    std::unique_ptr<std::streambuf> m_old_buffer;
};

// This is a helper struct to redirect std::cerr. This makes sure std::cerr is redirected back and
// everything is cleaned up properly
struct RedirectCerr
{
    RedirectCerr(std::unique_ptr<std::ostringstream> const &m_buffer) : m_old_buffer(std::cerr.rdbuf(m_buffer->rdbuf()))
    {
    }

    ~RedirectCerr()
    {
        std::cerr.rdbuf(m_old_buffer.release());
    }

private:
    std::unique_ptr<std::streambuf> m_old_buffer;
};

class EnergyPlusFixture : public testing::Test
{
protected:
    static void SetUpTestCase();
    static void TearDownTestCase()
    {
    }

    // This is run every unit test for this fixture.
    // It sets up the various stream redirections.
    // It also calls show_message every unit test to output a begin message to the error file.
    virtual void SetUp();

    // This is run every unit test and makes sure to clear all state in global variables this fixture touches.
    virtual void TearDown();

    void clear_all_states();

    // This will output the "Begin Test" ShowMessage for every unit test that uses or inherits from this fixture.
    // Now this does not need to be manually entered for every unit test as well as it will automatically be updated as the
    // unit test names change.
    inline void show_message()
    {
        // Gets information about the currently running test.
        // Do NOT delete the returned object - it's managed by the UnitTest class.
        const ::testing::TestInfo *const test_info = ::testing::UnitTest::GetInstance()->current_test_info();
        ShowMessage("Begin Test: " + std::string(test_info->test_case_name()) + ", " + std::string(test_info->name()));
    }

    // This will compare either a STL container or ObjexxFCL container
    // Pass a container you want to compare against an expected container. You can pass in an existing
    // container or use an initializer list like below.
    // This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
    // if it makes sense for the unit test to continue after returning from function.
    // Will return true if containers are equal and false if they are not.
    // Example Usage:
    // 		EXPECT_TRUE( compare_containers( std::vector< bool >( { true } ) , ObjectDef( index ).AlphaOrNumeric ) );
    // 		EXPECT_TRUE( compare_containers( Array1D_bool( { true } ) , ObjectDef( index ).AlphaOrNumeric ) );
    template <class T, class T2> bool compare_containers(T const &expected_container, T2 const &actual_container)
    {
        bool is_valid = (expected_container.size() == actual_container.size());
        EXPECT_EQ(expected_container.size(), actual_container.size()) << "Containers are not equal size.";
        auto expected = expected_container.begin();
        auto actual = actual_container.begin();
        for (; expected != expected_container.end(); ++expected, ++actual) {
            // This may fail due to floating point issues for float and double...
            EXPECT_EQ(*expected, *actual) << "Incorrect 0-based index: " << (expected - expected_container.begin());
            is_valid = (*expected == *actual);
        }
        return is_valid;
    }

    // This function creates a string based on a vector of string inputs that is delimited by DataStringGlobals::NL by default, but any
    // delimiter can be passed in to this funciton. This allows for cross platform output string comparisons.
    std::string delimited_string(std::vector<std::string> const &strings, std::string const &delimiter = DataStringGlobals::NL);

    // This function reads all the lines in the supplied filePath. It puts each line into the vector.
    std::vector<std::string> read_lines_in_file(std::string const &filePath);

    // Compare an expected string against the ESO stream. The default is to reset the ESO stream after every call.
    // It is easier to test successive functions if the ESO stream is 'empty' before the next call.
    // This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
    // if it makes sense for the unit test to continue after returning from function.
    // Will return true if string matches the stream and false if it does not
    bool compare_eso_stream(std::string const &expected_string, bool reset_stream = true);

    // Compare an expected string against the EIO stream. The default is to reset the EIO stream after every call.
    // It is easier to test successive functions if the EIO stream is 'empty' before the next call.
    // This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
    // if it makes sense for the unit test to continue after returning from function.
    // Will return true if string matches the stream and false if it does not
    bool compare_eio_stream(std::string const &expected_string, bool reset_stream = true);

    // Compare an expected string against the MTR stream. The default is to reset the MTR stream after every call.
    // It is easier to test successive functions if the MTR stream is 'empty' before the next call.
    // This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
    // if it makes sense for the unit test to continue after returning from function.
    // Will return true if string matches the stream and false if it does not
    bool compare_mtr_stream(std::string const &expected_string, bool reset_stream = true);

    // Compare an expected string against the ERR stream. The default is to reset the ERR stream after every call.
    // It is easier to test successive functions if the ERR stream is 'empty' before the next call.
    // This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
    // if it makes sense for the unit test to continue after returning from function.
    // Will return true if string matches the stream and false if it does not
    bool compare_err_stream(std::string const &expected_string, bool reset_stream = true);

    // Compare an expected string against the COUT stream. The default is to reset the COUT stream after every call.
    // It is easier to test successive functions if the COUT stream is 'empty' before the next call.
    // This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
    // if it makes sense for the unit test to continue after returning from function.
    // Will return true if string matches the stream and false if it does not
    bool compare_cout_stream(std::string const &expected_string, bool reset_stream = true);

    // Compare an expected string against the CERR stream. The default is to reset the CERR stream after every call.
    // It is easier to test successive functions if the CERR stream is 'empty' before the next call.
    // This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
    // if it makes sense for the unit test to continue after returning from function.
    // Will return true if string matches the stream and false if it does not
    bool compare_cerr_stream(std::string const &expected_string, bool reset_stream = true);

    // Compare an expected string against the delightin stream. The default is to reset the delightin stream after every call.
    // It is easier to test successive functions if the delightin stream is 'empty' before the next call.
    // This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
    // if it makes sense for the unit test to continue after returning from function.
    // Will return true if string matches the stream and false if it does not
    bool compare_delightin_stream(std::string const &expected_string, bool reset_stream = true);

    // Check if ESO stream has any output. Useful to make sure there are or are not outputs to ESO.
    bool has_eso_output(bool reset_stream = true);

    // Check if EIO stream has any output. Useful to make sure there are or are not outputs to EIO.
    bool has_eio_output(bool reset_stream = true);

    // Check if MTR stream has any output. Useful to make sure there are or are not outputs to MTR.
    bool has_mtr_output(bool reset_stream = true);

    // Check if ERR stream has any output. Useful to make sure there are or are not outputs to ERR.
    bool has_err_output(bool reset_stream = true);

    // Check if COUT stream has any output. Useful to make sure there are or are not outputs to COUT.
    bool has_cout_output(bool reset_stream = true);

    // Check if CERR stream has any output. Useful to make sure there are or are not outputs to CERR.
    bool has_cerr_output(bool reset_stream = true);

    // Check if delightin stream has any output. Useful to make sure there are or are not outputs to delightin.
    bool has_delightin_output(bool reset_stream = true);

    // This function processes an idf snippet and defaults to using the idd cache for the fixture.
    // The cache should be used for nearly all calls to this function.
    // This more or less replicates inputProcessor->processInput() but in a more usable fashion for unit testing
    // This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
    // if it makes sense for the unit test to continue after returning from function.
    // Will return false if no errors found and true if errors found
    bool process_idf(std::string const &idf_snippet, bool use_assertions = true);

    // This is a helper function to easily compare an expected IDF data structure with the actual IDFRecords data structure
    // This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
    // if it makes sense for the unit test to continue after returning from function.
    // Will return true if data structures match and false if they do not.
    // Usage (assuming "Version,8.3;" was parsed as an idf snippet):
    // 		EXPECT_TRUE( compare_idf( "VERSION", 1, 0, 1, { "8.3" }, { false }, {}, {} ) );
    bool compare_idf(std::string const &name,
                     int const num_alphas,
                     int const num_numbers,
                     std::vector<std::string> const &alphas,
                     std::vector<bool> const &alphas_blank,
                     std::vector<Real64> const &numbers,
                     std::vector<bool> const &numbers_blank);

private:
    friend class InputProcessorFixture;

    // Function to process the Energy+.schema.epJSON, should not normally be called.
    // This will always grab the Energy+.schema.epJSON that is part of the Products folder
    // This function should be called by process_idf() so unit tests can take advantage of caching
    // To test this function use InputProcessorFixture
    // This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
    // if it makes sense for the unit test to continue after returning from function.
    // Will return false if no errors found and true if errors found

    static bool process_idd(std::string const &idd, bool &errors_found);

    std::unique_ptr<std::ostringstream> eso_stream;
    std::unique_ptr<std::ostringstream> eio_stream;
    std::unique_ptr<std::ostringstream> mtr_stream;
    std::unique_ptr<std::ostringstream> err_stream;
    std::unique_ptr<std::ostringstream> m_cout_buffer;
    std::unique_ptr<std::ostringstream> m_cerr_buffer;
    std::unique_ptr<std::ostringstream> m_delightin_stream;
    std::unique_ptr<RedirectCout> m_redirect_cout;
    std::unique_ptr<RedirectCerr> m_redirect_cerr;
};

} // namespace EnergyPlus

#endif
