/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdlib.h>
#include <string.h>
#include "vbapi.h"

#pragma warning (disable: 4302 4311 4312)

// test without marshalling

SSCEXPORT long VBCALL_CONVENTION sscvb_version()
{
	return (long)ssc_version();
}

SSCEXPORT long VBCALL_CONVENTION sscvb_build_info(char *build_info, long len)
{
	int info_len;
	const char* info = ssc_build_info();
	if (info == NULL)
		return 0;

	info_len = (int)strlen(info) + 1;

	if (build_info == NULL || len == 0)
		return info_len;

	strncpy(build_info, info, len);

	return len < (long)info_len ? len : (long)info_len;
}

SSCEXPORT void* VBCALL_CONVENTION sscvb_data_create()
{
	return ssc_data_create();
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_free(void *p_data)
{
	if (p_data)
	{
		ssc_data_free(p_data);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_clear(void *p_data)
{
	if (p_data)
	{
		ssc_data_clear(p_data);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_unassign(void *p_data, const char *name)
{
	if (p_data)
	{
		ssc_data_unassign(p_data, name);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_query(void *p_data, const char *name)
{
	if (p_data)
	{
		return (long)ssc_data_query(p_data, name);
	}
	else
		return 0;
	
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_first(void *p_data, const char *data_first)
{
	if (p_data)
	{
		data_first = ssc_data_first(p_data);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_next(void *p_data, const char *data_next)
{
	if (p_data)
	{
		data_next = ssc_data_next(p_data);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_string(void *p_data, const char *name, const char *value)
{
	if (p_data)
	{
		ssc_data_set_string(p_data, name, value);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_number(void *p_data, const char *name, double value)
{
	if (p_data)
	{
		ssc_number_t val = (ssc_number_t)value;
		ssc_data_set_number(p_data, name, val);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_array(void *p_data, const char *name, double *pvalues, long length)
{
	if (p_data)
	{
		int len = (int)length;
		if (len == 0)
			return 0;
		ssc_number_t *values = new ssc_number_t[len];
		for (int i = 0; i < len; i++)
			values[i] = (ssc_number_t)pvalues[i];
		ssc_data_set_array(p_data, name, values, len);
		delete[] values;
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_matrix(void *p_data, const char *name, double *pvalues, long nrows, long ncols)
{
	if (p_data)
	{
		int rows = (int)nrows;
		int cols = (int)ncols;
		int len = rows * cols;
		if (len == 0)
			return 0;
		ssc_number_t *values = new ssc_number_t[len];
		for (int i = 0; i < len; i++)
			values[i] = (ssc_number_t)pvalues[i];
		ssc_data_set_matrix(p_data, name, values, rows, cols);
		delete[] values;
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_table(void *p_data, const char *name, void *table)
{
	if (p_data && table)
	{
		ssc_data_set_table(p_data, name, table);
		return 1;
	}
	else
		return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_string(void *p_data, const char *name, char *value, long len)
{
	if (p_data)
	{
		int val_len;
		const char* val = ssc_data_get_string(p_data, name);
		if (val == NULL)
			return 0;

		val_len = (int)strlen(val) + 1;

		if (value == NULL || len == 0)
			return val_len;

		strncpy(value, val, len);

		return len < (long)val_len ? len : (long)val_len;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_number(void *p_data, const char *name, double *value)
{
	if (p_data)
	{
		ssc_number_t val;
		ssc_data_get_number(p_data, name, &val);
		*value = (double)val;
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_array(void *p_data, const char *name, double *pvalue, long length)
{
	if (p_data)
	{
		int len = (int)length;
		 ssc_number_t *values = ssc_data_get_array(p_data, name, &len);
		 if (!values)
			 return (long)0;
		 if (length == 0)
			return (long)len;
		for (int i = 0; i < len; i++)
			pvalue[i] = (double)values[i];
		return (long)len;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_matrix(void *p_data, const char *name, double *pvalue, long nrows, long ncols)
{
	if (p_data)
	{
		int rows = (int)nrows;
		int cols = (int)ncols;
		ssc_number_t *values = ssc_data_get_matrix(p_data, name, &rows, &cols);
		if (!values)
			return (long)0;
		if (nrows == 0)
			return (long)rows;
		if (ncols == 0)
			return (long)cols;
		size_t len = rows * cols;
		for (size_t i = 0; i < len; i++)
			pvalue[i] = (double)values[i];
		return (long)len;
	}
	else
		return 0;
	}

// TODO test this
SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_table(void *p_data, const char *name, void *table)
{
	if (p_data && table)
	{
		ssc_data_t tmp = ssc_data_get_table(p_data, name); 
		return (long)tmp;
	}
	else
		return 0;
}


SSCEXPORT void *VBCALL_CONVENTION sscvb_module_entry(long index)
{
	return ssc_module_entry((int)index);
}

SSCEXPORT long VBCALL_CONVENTION sscvb_entry_name(void *p_entry, const char *name)
{
	if (p_entry)
	{
		name = ssc_entry_name(p_entry);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_entry_description(void *p_entry, const char *description)
{
	if (p_entry)
	{
		description = ssc_entry_description(p_entry);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_entry_version(void *p_entry)
{
	if (p_entry)
	{
		return (long)ssc_entry_version(p_entry);
	}
	else
		return 0;
}


SSCEXPORT void *VBCALL_CONVENTION sscvb_module_create(const char *name)
{
	return ssc_module_create(name);
}

SSCEXPORT long VBCALL_CONVENTION sscvb_module_free(void *p_mod)
{
	if (p_mod)
	{
		ssc_module_free(p_mod);
		return 1;
	}
	else
		return 0;
}


SSCEXPORT void *VBCALL_CONVENTION sscvb_module_var_info(void *p_mod, long index)
{
	if (p_mod)
		return ssc_module_var_info(p_mod, (int)index);
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_var_type(void *p_inf)
{
	if (p_inf)
		return (long)ssc_info_var_type(p_inf);
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_data_type(void *p_inf)
{
	if (p_inf)
		return (long)ssc_info_data_type(p_inf);
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_name(void *p_inf, const char *name)
{
	if (p_inf)
	{
		name = ssc_info_name(p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_label(void *p_inf, const char *label)
{
	if (p_inf)
	{
		label = ssc_info_label(p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_units(void *p_inf, const char *units)
{
	if (p_inf)
	{
		units = ssc_info_units(p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_meta(void *p_inf, const char *meta)
{
	if (p_inf)
	{
		meta = ssc_info_meta(p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_group(void *p_inf, const char *group)
{
	if (p_inf)
	{
		group = ssc_info_group(p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_required(void *p_inf, const char *required)
{
	if (p_inf)
	{
		required = ssc_info_required(p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_constraints(void *p_inf, const char *constraints)
{
	if (p_inf)
	{
		constraints = ssc_info_constraints(p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_uihint(void *p_inf, const char *uihint)
{
	if (p_inf)
	{
		uihint = ssc_info_uihint(p_inf);
		return 1;
	}
	else
		return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_set_print(long print)
{
	ssc_module_exec_set_print((int)print);
	return 1;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_simple(const char *name, void *p_data)
{
	if (p_data)
		return (long)ssc_module_exec_simple(name, p_data);
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_simple_nothread(const char *name, void *p_data, const char *msg)
{
	if (p_data)
	{
		msg = ssc_module_exec_simple_nothread(name, p_data);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec(void *p_mod, void *p_data)
{
	if (p_mod && p_data)
		return (long)ssc_module_exec(p_mod, p_data);
	else
		return 0;
}


// TODO test this
SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_with_handler(void *p_mod, void *p_data, long pf_handler, void *pf_user_data)
{
	if (p_mod && p_data)
		return (long)ssc_module_exec_with_handler(p_mod, p_data, (ssc_bool_t(*)(ssc_module_t, ssc_handler_t, int action, float f0, float f1, const char *s0, const char *s1, void *user_data))pf_handler, pf_user_data);
	else
		return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_log(void *p_mod, long index, long *item_type, double *time, char *msg, long msg_len)
{
	int sscmsg_len;
	int ndx = (int)index;
	int it;
	float ts;
	if (p_mod)
	{
		const char* sscmsg = ssc_module_log(p_mod, ndx, &it, &ts);
		if (sscmsg == NULL)
			return 0;

		sscmsg_len = (int)strlen(sscmsg) + 1;

		if (msg == NULL || msg_len == 0)
			return sscmsg_len;

		strncpy(msg, sscmsg, msg_len);
		*time = (double)ts;
		*item_type = (long)it;
		return msg_len < (long)sscmsg_len ? msg_len : (long)sscmsg_len;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION __sscvb_segfault()
{
	__ssc_segfault();
	return 1;
}

