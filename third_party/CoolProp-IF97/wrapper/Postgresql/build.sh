g++ -c -fPIC -Wall -Werror -g3 -O0 -I`pg_config --includedir-server` -I`pg_config --includedir-server`/utils if97.c
g++ -shared -o if97.so if97.o
mkdir -p `pg_config --pkglibdir`/if97
cp ./if97.so `pg_config --pkglibdir`/if97
