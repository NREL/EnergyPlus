#include <EnergyPlusPgm.hh>

void message_callback_handler(std::string message)
{
}

void progress_callback_handler(int progress)
{
}

int
main()
{
	StoreMessageCallback( message_callback_handler );
	StoreProgressCallback( progress_callback_handler );
	EnergyPlusPgm();
}

