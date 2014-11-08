#ifndef FileSystem_hh_INCLUDED
#define FileSystem_hh_INCLUDED

#include <algorithm>

namespace EnergyPlus{

namespace FileSystem {

extern std::string const exeExtension;

void
makeNativePath(std::string &path);

std::string
getFileName( std::string const &filePath );

std::string
getParentDirectoryPath( std::string const &filePath );

std::string
getAbsolutePath( std::string const &filePath );

std::string getProgramPath();

std::string
getFileExtension(std::string const &fileName);

std::string
removeFileExtension(std::string const &fileName);

void
makeDirectory(std::string const &directoryPath);

bool
pathExists(std::string const &path);

bool
directoryExists(std::string const &directoryPath);

bool
fileExists(std::string const &filePath);

void
moveFile(std::string const &filePath, std::string const &destination);

int
systemCall(std::string const &command);

void
removeFile(std::string const &fileName);

void
linkFile(std::string const &fileName, std::string const &link);


}
}
#endif
