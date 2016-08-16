extern int NSApplicationMain(int argc, const char *argv[]);
extern void ASKInitialize();

int main(int argc, const char *argv[])
{
	ASKInitialize();

return NSApplicationMain(argc, argv);
}

