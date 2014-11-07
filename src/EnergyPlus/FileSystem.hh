#ifndef FileSystem_hh_INCLUDED
#define FileSystem_hh_INCLUDED

#include <algorithm>

namespace EnergyPlus{

namespace FileSystem {

extern std::string const exeExtension;

void
makeNativePath(std::string &path);

std::string
getFileName( std::string const& filePath );

std::string
getParentDirectoryPath( std::string const& filePath );

std::string
getAbsolutePath( std::string const& filePath );

std::string getProgramPath();

std::string
getFileExtension(const std::string& fileName);

std::string
removeFileExtension(const std::string& fileName);

void
makeDirectory(std::string directoryPath);

bool
pathExists(std::string path);

bool
directoryExists(std::string directoryPath);

bool
fileExists(std::string filePath);

void
moveFile(std::string filePath, std::string destination);

int
systemCall(std::string command);

void
removeFile(std::string fileName);

void
linkFile(std::string fileName, std::string link);


}
}
#endif
