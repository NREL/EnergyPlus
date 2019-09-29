#include <stdio.h>
#include <api/wallwrapper.h>

int main() {
    CWall *w = NULL;
    w = newCWall(5.0);
    setCWallThickness(w, 3.2);
    Real64 c = calculateCWall(w, 6.3);
    delCWall(w);
    printf("Doing wall calcs: %4.2f \n", c);
    // w = NULL;
    return 0;
}
