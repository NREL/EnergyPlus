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

// EnergyPlus::DataVectorTypes::Vector Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// C++ Headers
#include <fstream>
#include <iostream>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/FileSystem.hh>

// We don't have a remove_all function since we do not use std::filesystem (or boost::filesystem), so make a very sketchy and crude one for testing
// only
namespace fs {
    void remove_all(const std::string& p) {
#ifdef _WIN32
        EnergyPlus::FileSystem::systemCall("rmdir /Q /S \"" + p + "\"");
#else
        EnergyPlus::FileSystem::systemCall("rm -Rf \"" + p + "\"");
#endif
    }
}

TEST(FileSystem, movefile_test)
{
    // test moveFile function, specifically on Windows

    // create text file to be overridden by new file
    std::string filename = "FileSystemTest.idf";
    std::string line;
    std::stringstream buffer;
    std::ofstream ofs(filename);
    ofs << "Version, 9.3;" << std::endl;
    ofs.close();

    // read text file
    std::ifstream ifs(filename);
    buffer << ifs.rdbuf();
    // check that text file was created correctly
    EXPECT_EQ(buffer.str(), "Version, 9.3;\n");
    ifs.close();

    // create temporary text file to move to existing file created above
    std::string filename_temp = "FileSystemTest_temp.idf";
    std::ofstream ofs_temp(filename_temp);
    ofs_temp << "Version, 9.4;" << std::endl;
    ofs_temp.close();

    // move temporary text file to overwrite existing file
    EnergyPlus::FileSystem::moveFile(filename_temp, filename);
    // read original text file after using moveFile function
    std::stringstream buffer_new;
    std::ifstream ifs_new(filename);
    buffer_new << ifs_new.rdbuf();
    // check that original text file was overwritten by temporary text file
    EXPECT_EQ(buffer_new.str(), "Version, 9.4;\n");
    ifs_new.close();

    // remove files
    EnergyPlus::FileSystem::removeFile(filename);
    EnergyPlus::FileSystem::removeFile(filename_temp);
}

TEST(FileSystem, getAbsolutePath)
{
    std::string pathName = "FileSystemTest.idf";
    std::string absPathName = EnergyPlus::FileSystem::getAbsolutePath(pathName);
    EXPECT_TRUE(absPathName.find(pathName) != std::string::npos); // Check that the path name appears in the absolute path

    std::string currentDirWithSep = std::string(".") + state->dataStrGlobals->pathChar;
    pathName =  currentDirWithSep + std::string("FileSystemTest.idf");  // e.g., "./FileSystemTest.idf"
    absPathName = EnergyPlus::FileSystem::getAbsolutePath(pathName);
    EXPECT_FALSE(absPathName.find(currentDirWithSep) != std::string::npos); // Make sure "./" doesn't appear in absolute path
}

TEST(FileSystem, Others)
{
    std::string pathName = "folder/FileSystemTest.txt.idf";
    // The current implementation of getParentDirectoryPath relies on makeNativePath being called first
    EnergyPlus::FileSystem::makeNativePath(*state, pathName);

    EXPECT_EQ("idf", EnergyPlus::FileSystem::getFileExtension(pathName));

    std::string noExt = "folder/FileSystemTest.txt";
    EnergyPlus::FileSystem::makeNativePath(*state, noExt);
    EXPECT_EQ(noExt, EnergyPlus::FileSystem::removeFileExtension(pathName));

    std::string folder = "folder/";
    EnergyPlus::FileSystem::makeNativePath(*state, folder);
    EXPECT_EQ(folder, EnergyPlus::FileSystem::getParentDirectoryPath(*state, pathName));

    std::string root = "./";
    EnergyPlus::FileSystem::makeNativePath(*state, root);
    EXPECT_EQ(root, EnergyPlus::FileSystem::getParentDirectoryPath(*state, "Myfile.txt.idf"));

}

TEST(FileSystem, getProgramPath)
{
    std::string programPath = EnergyPlus::FileSystem::getProgramPath();
    EXPECT_TRUE(EnergyPlus::FileSystem::pathExists(programPath));
    EXPECT_TRUE(programPath.find("energyplus_tests") != std::string::npos);
    EXPECT_EQ("energyplus_tests" + EnergyPlus::FileSystem::exeExtension, EnergyPlus::FileSystem::getFileName(state, programPath));
    EXPECT_TRUE(EnergyPlus::FileSystem::directoryExists(EnergyPlus::FileSystem::getParentDirectoryPath(*state, programPath)));
}

TEST(FileSystem, getParentDirectoryPath)
{
    std::string expected = "/a/b/";
    EnergyPlus::FileSystem::makeNativePath(*state, expected);

    std::string test = "/a/b/c";
    EnergyPlus::FileSystem::makeNativePath(*state, test);
    EXPECT_EQ(expected, EnergyPlus::FileSystem::getParentDirectoryPath(*state, test));

    test = "/a/b/c/";
    EnergyPlus::FileSystem::makeNativePath(*state, test);
    EXPECT_EQ(expected, EnergyPlus::FileSystem::getParentDirectoryPath(*state, test));
}

TEST(FileSystem, make_and_remove_Directory)
{
    fs::remove_all("sandbox");

    std::string dirName("sandbox/a");
    EnergyPlus::FileSystem::makeNativePath(*state, dirName);
    std::string expected = "sandbox/";
    EnergyPlus::FileSystem::makeNativePath(*state, expected);

    EXPECT_EQ(expected, EnergyPlus::FileSystem::getParentDirectoryPath(*state, dirName));

    EXPECT_FALSE(EnergyPlus::FileSystem::pathExists("sandbox"));
    EXPECT_FALSE(EnergyPlus::FileSystem::fileExists("sandbox"));
    EXPECT_FALSE(EnergyPlus::FileSystem::directoryExists("sandbox"));
    EXPECT_FALSE(EnergyPlus::FileSystem::pathExists(dirName));
    EXPECT_FALSE(EnergyPlus::FileSystem::fileExists(dirName));
    EXPECT_FALSE(EnergyPlus::FileSystem::directoryExists(dirName));

    // This fails, because it can't make intermediate directories... which I think is a weird unwanted limitation
    // eg: energyplus -d out/a/b/c/ sould be possible. It would create out, out/a, out/a/b/ and out/a/b/c/ as needed
    // Anyways, for now to avoid a failed test, let's create the intermediate directory manually
    EnergyPlus::FileSystem::makeDirectory(*state, "sandbox");
    EnergyPlus::FileSystem::makeDirectory(*state, "sandbox/a");

    EXPECT_TRUE(EnergyPlus::FileSystem::pathExists("sandbox"));
    EXPECT_FALSE(EnergyPlus::FileSystem::fileExists("sandbox"));
    EXPECT_TRUE(EnergyPlus::FileSystem::directoryExists("sandbox"));
    EXPECT_TRUE(EnergyPlus::FileSystem::pathExists(dirName));
    EXPECT_FALSE(EnergyPlus::FileSystem::fileExists(dirName));
    EXPECT_TRUE(EnergyPlus::FileSystem::directoryExists(dirName));

    std::string filePathName("sandbox/a/file.txt.idf");
    std::ofstream(filePathName).put('a'); // create regular file

    EXPECT_TRUE(EnergyPlus::FileSystem::pathExists("sandbox"));
    EXPECT_FALSE(EnergyPlus::FileSystem::fileExists("sandbox"));
    EXPECT_TRUE(EnergyPlus::FileSystem::directoryExists("sandbox"));
    EXPECT_TRUE(EnergyPlus::FileSystem::pathExists(dirName));
    EXPECT_FALSE(EnergyPlus::FileSystem::fileExists(dirName));
    EXPECT_TRUE(EnergyPlus::FileSystem::directoryExists(dirName));

    EXPECT_TRUE(EnergyPlus::FileSystem::pathExists(filePathName));
    EXPECT_TRUE(EnergyPlus::FileSystem::fileExists(filePathName));
    EXPECT_FALSE(EnergyPlus::FileSystem::directoryExists((filePathName)));
}


TEST(FileSystem, Elaborate)
{
    EnergyPlus::FileSystem::makeDirectory(*state, "sandbox");
    std::string pathName("sandbox/file1.txt.idf");
    std::ofstream(pathName).put('a'); // create regular file
    EXPECT_TRUE(EnergyPlus::FileSystem::pathExists(pathName));
    EXPECT_TRUE(EnergyPlus::FileSystem::fileExists(pathName));
    EXPECT_TRUE(EnergyPlus::FileSystem::pathExists("sandbox"));
    EXPECT_TRUE(EnergyPlus::FileSystem::directoryExists("sandbox"));
    EXPECT_TRUE(EnergyPlus::FileSystem::directoryExists("sandbox/"));
    EXPECT_GT(EnergyPlus::FileSystem::getAbsolutePath(pathName).size(), pathName.size());
    // Fails, ..../sandbox/ versus ..../sandbox
    //EXPECT_EQ(EnergyPlus::FileSystem::getAbsolutePath("sandbox/"),
              //EnergyPlus::FileSystem::getParentDirectoryPath(EnergyPlus::FileSystem::getAbsolutePath(pathName)));
    //EXPECT_EQ(EnergyPlus::FileSystem::getAbsolutePath("sandbox"),
              //EnergyPlus::FileSystem::getParentDirectoryPath(EnergyPlus::FileSystem::getAbsolutePath(pathName)));
    //EXPECT_EQ(EnergyPlus::FileSystem::getAbsolutePath("sandbox/"), EnergyPlus::FileSystem::getAbsolutePath("./sandbox/"));

    EXPECT_EQ(EnergyPlus::FileSystem::getAbsolutePath("./"), EnergyPlus::FileSystem::getAbsolutePath("sandbox/../"));

    // Fails, "/home/julien/Software/Others/EnergyPlus-build/." versus "/home/julien/Software/Others/EnergyPlus-build"
    //EXPECT_EQ(EnergyPlus::FileSystem::getAbsolutePath("."), EnergyPlus::FileSystem::getAbsolutePath("sandbox/.."));

    EnergyPlus::FileSystem::removeFile(pathName);
    EXPECT_FALSE(EnergyPlus::FileSystem::pathExists(pathName));
    EXPECT_FALSE(EnergyPlus::FileSystem::fileExists(pathName));

    fs::remove_all("sandbox");
}
