#include <iostream>
#include "EnergyPlusPgm.hh"

void message_callback_handler(std::string message)
{
	std::cout << "HiMessage: " << message << std::endl;
}

void progress_callback_handler(int progress)
{
	std::cout << "Progress: " << progress << std::endl;
}

int main() 
{
	std::cout << "Hello, world" << std::endl;
	StoreMessageCallback(message_callback_handler);
	StoreProgressCallback(progress_callback_handler);
	EnergyPlusPgm("C:\\tmp\\runfolder");
	return 0;
}
