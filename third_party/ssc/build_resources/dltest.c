#include <stdio.h>
#include <dlfcn.h>

int main(int argc, char *argv[])
{
	void *pdll = 0;
	if ( argc < 2 )
	{
		printf("must provide name of dynamic library to load\n");
		return -1;
	}

	pdll = dlopen( argv[1], RTLD_NOW );
	if ( pdll == 0 )
	{
		printf("failed to load dynamic library: %s\n", argv[1]);
		printf("%s\n", dlerror());
		return -1;
	}

	dlclose( pdll );
	printf("loaded %s OK.  closing.\n", argv[1]);
	return 0;
}

