//Standard C++ library
#include <sys/types.h>
#include <sys/stat.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

// EnergyPlus Headers
#include <FileSystem.hh>
#include <DataStringGlobals.hh>
#include <DisplayRoutines.hh>


namespace EnergyPlus{

namespace FileSystem {

using namespace DataStringGlobals;

#ifdef _WIN32
std::string const exeExtension(".exe");
#else
std::string const exeExtension("");
#endif

void
makeNativePath(std::string &path)
{
	std::replace(path.begin(), path.end(), altpathChar, pathChar);
}

std::string
getFileName( std::string const& filePath )
{
	int pathCharPosition = filePath.find_last_of(pathChar);
	return filePath.substr(pathCharPosition + 1, filePath.size() - 1);
}

std::string 
getDirectoryPath( std::string const& filePath )
{
	int pathCharPosition = filePath.find_last_of(pathChar);
	return filePath.substr(0, pathCharPosition + 1);
}

std::string
getAbsolutePath( std::string const& filePath )
{
	char absolutePath[1024];
#ifdef _WIN32
	GetFullPathName(filePath.c_str(), sizeof(absolutePath), absolutePath, NULL);
#else
	char *result = realpath(filePath.c_str(), absolutePath);
#endif
	return std::string(absolutePath);
}

std::string
getProgramPath()
{
	char executableRelativePath[1024];

#ifdef __APPLE__
	uint32_t pathSize = sizeof(executableRelativePath);
	_NSGetExecutablePath(executableRelativePath, &pathSize);
#elif __linux__
	ssize_t len = readlink("/proc/self/exe", executableRelativePath, sizeof(executableRelativePath)-1);
#elif _WIN32
	GetModuleFileName(NULL, executableRelativePath, sizeof(executableRelativePath));
#endif

	return std::string(executableRelativePath);
}

std::string
getFileExtension(const std::string& fileName){
	int extensionPosition = fileName.find_last_of(".");
	return fileName.substr(extensionPosition + 1, fileName.size() - 1);
}

std::string
removeFileExtension(const std::string& fileName){
	int extensionPosition = fileName.find_last_of(".");
	return fileName.substr(0, extensionPosition);
}

void
makeDirectory(std::string directoryName)
{
	struct stat info;

	if ( stat(getAbsolutePath(directoryName).c_str(), &info) == 0){ // path already exists
		if ( !(info.st_mode & S_IFDIR) )
		{
			DisplayString("ERROR: " + getAbsolutePath(directoryName) + " is not a directory.");
			exit(EXIT_FAILURE);
		}
	}
	else { // directory does not already exist
#ifdef _WIN32
		CreateDirectory(directoryName.c_str(), NULL);
#else
		mkdir(directoryName.c_str(), 0755);
#endif
	}
}

void
moveFile(std::string filePath, std::string destination){
	rename(filePath.c_str(), destination.c_str());
}

int
systemCall(std::string command)
{
	return system(command.c_str());
}

void
removeFile(std::string fileName)
{
	remove(fileName.c_str());
}

void
linkFile(std::string fileName, std::string link)
{
#ifdef _WIN32
	CopyFile(fileName.c_str(), link.c_str(), false);
#else
	int status = symlink(fileName.c_str(), link.c_str());
#endif
}

}
}
