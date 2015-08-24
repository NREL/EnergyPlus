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
#include <Shlwapi.h>
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
std::string const exeExtension;
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
getParentDirectoryPath( std::string const& path )
{
	std::string tempPath = path;
	if (path.at(path.size()-1) == pathChar)
		tempPath = path.substr(0, path.size()-1);

	int pathCharPosition = tempPath.find_last_of(pathChar);
	tempPath = tempPath.substr(0, pathCharPosition + 1);

	if (tempPath == "")
		tempPath = ".";

	return tempPath;
}

std::string
getAbsolutePath( std::string const& path )
{

#ifdef _WIN32
	char absolutePath[1024];
	GetFullPathName(path.c_str(), sizeof(absolutePath), absolutePath, NULL);
	return std::string(absolutePath);
#else
	// If the path doesn't exist, find which of it's parents' paths does exist
	std::string parentPath = path;
	while(!pathExists(parentPath)) {
		parentPath = getParentDirectoryPath(parentPath);
	}

	std::string pathTail;
	if ( parentPath == "." )
		pathTail = path;
	else
		pathTail = path.substr(parentPath.size(), path.size() - parentPath.size());

	char *absolutePathTemp = realpath(parentPath.c_str(), NULL);
	if (absolutePathTemp != NULL) {
		std::string absoluteParentPath(absolutePathTemp);
	    free(absolutePathTemp);
	    if (pathTail.size() == 0)
	    	return absoluteParentPath;
	    else
	    	return absoluteParentPath + pathChar + pathTail;

	} else {
		DisplayString("ERROR: Could not resolve path for " + path + ".");
		exit(EXIT_FAILURE);
	}
#endif


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
  if ( len == -1 ) {
    DisplayString("ERROR: Unable to locate executable.");
    exit(EXIT_FAILURE);
  } else {
    executableRelativePath[len] = '\0';
  }
#elif _WIN32
	GetModuleFileName(NULL, executableRelativePath, sizeof(executableRelativePath));
#endif

	return std::string(executableRelativePath);
}

std::string
getFileExtension(std::string const &fileName)
{
	int extensionPosition = fileName.find_last_of(".");
	return fileName.substr(extensionPosition + 1, fileName.size() - 1);
}

std::string
removeFileExtension(std::string const &fileName)
{
	int extensionPosition = fileName.find_last_of(".");
	return fileName.substr(0, extensionPosition);
}

void
makeDirectory(std::string const &directoryPath)
{
	// Create a directory if doesn't already exist
	if ( pathExists(directoryPath) ) { // path already exists
		if ( !directoryExists(directoryPath) ) {
			DisplayString("ERROR: " + getAbsolutePath(directoryPath) + " is not a directory.");
			exit(EXIT_FAILURE);
		}
	} else { // directory does not already exist
		std::string parentDirectoryPath = getParentDirectoryPath(directoryPath);
		if ( !pathExists(parentDirectoryPath) ) {
			DisplayString("ERROR: " + getAbsolutePath(parentDirectoryPath) + " is not a directory.");
			exit(EXIT_FAILURE);
		}
#ifdef _WIN32
		CreateDirectory(directoryPath.c_str(), NULL);
#else
		mkdir(directoryPath.c_str(), 0755);
#endif
	}
}

bool
pathExists(std::string const &path)
{
#ifdef _WIN32
	return PathFileExists(path.c_str());
#else
	struct stat info;
	return (stat(path.c_str(), &info) == 0);
#endif
}

bool
directoryExists(std::string const &directoryPath)
{
#ifdef _WIN32
	if (PathFileExists(directoryPath.c_str()))
		return PathIsDirectory(directoryPath.c_str());
	else
		return false;
#else
	struct stat info;
	if ( stat(directoryPath.c_str(), &info) == 0) {
		return (info.st_mode & S_IFDIR);
	} else
		return false;
#endif
}

bool
fileExists(std::string const &filePath)
{
#ifdef _WIN32
	if (PathFileExists(filePath.c_str()))
		return !PathIsDirectory(filePath.c_str());
	else
		return false;
#else
	struct stat info;
	if ( stat(filePath.c_str(), &info) == 0) {
		return !(info.st_mode & S_IFDIR);
	} else
		return false;
#endif
}

void
moveFile(std::string const &filePath, std::string const &destination)
{
	rename(filePath.c_str(), destination.c_str());
}

int
systemCall(std::string const &command)
{
	return system(command.c_str());
}

void
removeFile(std::string const &fileName)
{
	remove(fileName.c_str());
}

void
linkFile(std::string const &fileName, std::string const &link)
{
#ifdef _WIN32
	CopyFile(fileName.c_str(), link.c_str(), false);
#else
	int returnValue = symlink(fileName.c_str(), link.c_str()); // ignore the return value
	// we want to ignore the return value without muting all warnings, so...
	(void)( returnValue + 1 ); // outsmart the compiler :-/
#endif
}

}
}
