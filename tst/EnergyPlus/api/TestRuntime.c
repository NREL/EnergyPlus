#include <stdio.h>
#include <EnergyPlus/api/runtime.h>

void runEPlusAuto() {
   cRunEnergyPlus("/tmp/epdll");
}

void runEPlusAllEnvironments() {
    cInitializeEnergyPlus("/tmp/epdll");
    cInitializeSimulation();
    cRunAllEnvironments();
    cWrapUpSimulation();
    cWrapUpEnergyPlus();
}

void runEPlusEachEnvironment() {
    cInitializeEnergyPlus("/tmp/epdll");
    cInitializeSimulation();
    //cRunAllEnvironments();
    int env_count = 0;
    while (1) {
        int env = cGetNextEnvironment();
        if (env == 0) {
            env_count++;
        } else {
            break;
        }
        int skip = cSkipCurrentEnvironment();
        if (skip == 0) {
            continue;
        }
        cRunEnvironment();
    }
    cWrapUpSimulation();
    cWrapUpEnergyPlus();
}

int main() {
    runEPlusAuto();
    cClearAllStates();
    runEPlusAllEnvironments();
    cClearAllStates();
    runEPlusEachEnvironment();
    return 0;
}
