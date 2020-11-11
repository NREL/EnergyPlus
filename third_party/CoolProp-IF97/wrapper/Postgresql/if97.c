/*
 * Filename: if97.c
 */

 extern "C" {
    #include <postgres.h>
    #include <fmgr.h>
    #include <utils/elog.h>
    PG_MODULE_MAGIC;
 }

 #include "../../IF97.h"

 extern "C" {
     // double hmass(double T, double p)
     PG_FUNCTION_INFO_V1(if97_hmass_Tp);
     Datum if97_hmass_Tp(PG_FUNCTION_ARGS) {
         try {
             PG_RETURN_FLOAT8(IF97::hmass_Tp(PG_GETARG_FLOAT8(0),PG_GETARG_FLOAT8(1)));
         } catch (const std::exception& e) {
            elog(WARNING, "if97: %s",e.what());
            PG_RETURN_NULL();
         }
     }

     // double Tsat97(double p)
     PG_FUNCTION_INFO_V1(if97_Tsat97);
     Datum if97_Tsat97(PG_FUNCTION_ARGS) {
        try {
            PG_RETURN_FLOAT8(IF97::Tsat97(PG_GETARG_FLOAT8(0)));
        } catch (const std::exception& e) {
            elog(WARNING, "if97: %s",e.what());
            PG_RETURN_NULL();
        }
     }
 }