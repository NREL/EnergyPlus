#include <iostream>
#include "jit.h"

extern llvm::orc::KaleidoscopeJIT * j;

extern "C" {

#include <jmi.h>

// Vdp_base.c
char *C_GUID;

// Vdp_base.c
int fmi_runtime_options_map_length = 0;

// Vdp_base.c
char **fmi_runtime_options_map_names;

// Vdp_base.c
int *fmi_runtime_options_map_vrefs;

// Vdp_base.c
int jmi_destruct_external_objs(jmi_t* jmi) {
    std::cout << "jmi_destruct_external_objs" << std::endl;
    if( auto sym = j->findSymbol("jmi_destruct_external_objs") ) {
      auto result =  ((int(*)(jmi_t*))sym.getAddress())(jmi);
      std::cout << "jmi_destruct_external_objs result: " << result << std::endl;
      return result;
    } else {
      std::cout << "jmi_destruct_external_objs not found: " << std::endl;
      return 1;
    }
}

// Vdp_base.c
const char *jmi_get_model_identifier() {
    std::cout << "jmi_get_model_identifier" << std::endl;
    if( auto sym = j->findSymbol("jmi_get_model_identifier") ) {
      auto result = ((char*(*)(void))sym.getAddress())();
      std::cout << "jmi_get_model_identifier result: " << result << std::endl;
      return result;
    } else {
      std::cout << "jmi_get_model_identifier not found: " << std::endl;
      return "";
    }
}

// Vdp_base.c
int jmi_new(jmi_t** jmi, jmi_callbacks_t* jmi_callbacks) {
    std::cout << "jmi_new" << std::endl;
    if( auto sym = j->findSymbol("jmi_new") ) {
      auto result = ((int(*)(jmi_t**,jmi_callbacks_t*))sym.getAddress())(jmi,jmi_callbacks);
      std::cout << "jmi_new result: " << result << std::endl;
      return result;
    } else {
      std::cout << "jmi_new not found: " << std::endl;
      return 1;
    }
}

// Vdp_start.c
int jmi_set_start_values(jmi_t* jmi) {
    std::cout << "jmi_set_start_values" << std::endl;
    if( auto sym = j->findSymbol("jmi_set_start_values") ) {
      auto result = ((int(*)(jmi_t*))sym.getAddress())(jmi);
      std::cout << "jmi_set_start_values result: " << result << std::endl;
      return result;
    } else {
      std::cout << "jmi_set_start_values not found: " << std::endl;
      return 1;
    }
}

}

