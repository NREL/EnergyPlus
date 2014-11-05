#ifndef FileSystem_hh_INCLUDED
#define FileSystem_hh_INCLUDED

namespace EnergyPlus{

namespace FileSystem {

std::string
getFileName( std::string const& filePath );

std::string
getDirectoryPath( std::string const& filePath );

std::string
getAbsolutePath( std::string const& filePath );

std::string getProgramPath();

std::string
getFileExtension(const std::string& fileName);

std::string
removeFileExtension(const std::string& fileName);

void
makeDirectory(std::string directoryName);

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
