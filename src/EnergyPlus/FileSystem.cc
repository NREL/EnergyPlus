//Standard C++ library
#include <sys/types.h>
#include <sys/stat.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

// EnergyPlus Headers
#include <FileSystem.hh>
#include <DataStringGlobals.hh>
#include <DisplayRoutines.hh>


namespace EnergyPlus{

namespace FileSystem {

using DataStringGlobals::pathChar;


std::string
getFileName( std::string const& filePath )
{
	int pathCharPosition = filePath.find_last_of(pathChar);
	return filePath.substr(pathCharPosition + 1, filePath.size() - 1);
}

std::string getDirectoryPath( std::string const& filePath )
{
	int pathCharPosition = filePath.find_last_of(pathChar);
	return filePath.substr(0, pathCharPosition + 1);
}

std::string
getAbsolutePath( std::string const& filePath )
{
	char absolutePath[1024];
	realpath(filePath.c_str(), absolutePath);
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
	readlink("/proc/self/exe", executableRelativePath, sizeof(executableRelativePath)-1);
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
		mkdir(directoryName.c_str(), 0755);
	}
}

void
moveFile(std::string filePath, std::string destination){
	rename(filePath.c_str(), destination.c_str());
}

void
systemCall(std::string command)
{
	system(command.c_str());
}

void
removeFile(std::string fileName)
{
	remove(fileName.c_str());
}

void
linkFile(std::string fileName, std::string link)
{
	symlink(fileName.c_str(), link.c_str());
}

}
}
