#include <stdio.h>
#include <string.h>

const char *files[]={"../include/mgl2/base_cf.h","../include/mgl2/data_cf.h", "../include/mgl2/datac_cf.h", "../include/mgl2/cont.h", "../include/mgl2/fit.h", "../include/mgl2/plot.h", "../include/mgl2/surf.h", "../include/mgl2/volume.h", "../include/mgl2/vect.h", "../include/mgl2/prim.h", "../include/mgl2/other.h", "../include/mgl2/canvas_cf.h", "../include/mgl2/addon.h", ""};

const char *head="\\ Mathgl library wrapper\n\
\\ Copyright (C) 2008-2013, Sergey Plis, Alexey Balakin\n\\\n\
\\ This program is free software; you can redistribute it and/or modify\n\
\\ it under the terms of the GNU General Public License as published by\n\
\\ the Free Software Foundation; either version 2 of the License, or\n\
\\ (at your option) any later version.\n\\\n\
\\ This program is distributed in the hope that it will be useful,\n\
\\ but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
\\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
\\ GNU General Public License for more details.\n\n\
Module mathgl\n\
library libmgl      libmgl.so\n\
library libmgl-glut libmgl-glut.so\n\
library libmgl-fltk libmgl-fltk.so\n\
library libmgl-qt   libmgl-qt.so\n\
library libmgl-wx   libmgl-wx.so\n\
legacy off\n\n\
libmgl mgl_create_graph_gl\t\t(ptr)\tmgl_create_graph_gl\n\
libmgl-glut mgl_create_graph_glut\tptr ptr ptr\t(ptr)\tmgl_create_graph_glut\n\
libmgl-fltk mgl_create_graph_fltk\tptr ptr ptr\t(ptr)\tmgl_create_graph_fltk\n\
libmgl-fltk mgl_fltk_run\t\t(void)\tmgl_fltk_run\n\
libmgl-qt   mgl_create_graph_qt\tptr ptr ptr\t(ptr)\tmgl_create_graph_qt\n\
libmgl-qt   mgl_qt_run\t\t\t(void)\tmgl_qt_run\n";

const char *parse_name(char *name, char *fnc)
{
	static char res[1024];	memset(res,0,1024);
	char *ptr,*arg[20],nul=0;	// TODO check if 20 arguments is enough
	unsigned i;
	for(i=0;name[i]!='(';i++)	res[i]=name[i];
	memcpy(fnc,res,i);	fnc[i]=0;	// copy name for later
	res[i]='\t';	i++;
	for(int j=1;j<=(25-i)/4;j++)	res[i+j-1]='\t';

/*	static char res[1024];
	char *ptr,*arg[20],nul=0;	// TODO check if 20 arguments is enough
	unsigned i;
	for(i=0;name[i]!='(';i++)	res[i]=name[i];
	memcpy(fnc,res,i);	fnc[i]=0;	// copy name for later
	res[i]='\t';	res[i+1]=0;	i++;*/
	
	while(name[i]<=' ')	i++;
	for(int j=0;j<20;j++)	arg[j]=&nul;
	for(int j=0;j<20;j++)
	{
		arg[j]= name[i]<=' ' ? name+i+1 : name+i;
		ptr=strchr(name+i,',');
		if(!ptr)	break;
		*ptr=0;	i=ptr-name+1;
	}
	ptr=strchr(name+i,')');	if(ptr) *ptr=0;
	for(int j=19;j>=0;j--)
	{
		if(arg[j][0]==0)	continue;
		if(!strncmp(arg[j],"HMGL ",5))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"HADT ",5))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"HCDT ",5))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"HMDT ",5))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"HMPR ",5))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"HMEX ",5))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"const float *",13))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"const double *",14))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"mreal *",7))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"double *",8))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"char *",6))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"int *",5)) 	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"long *",6))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"const char *",12))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"const wchar_t *",15))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"char ",5)) 	strcat(res, "char ");
		else if(!strncmp(arg[j],"long ",5)) 	strcat(res, "int ");
		else if(!strncmp(arg[j],"uint32_t ",9))	strcat(res, "int ");
		else if(!strncmp(arg[j],"int ",4))		strcat(res, "int ");
		else if(!strncmp(arg[j],"mreal ",6))	strcat(res, "sf ");
		else if(!strncmp(arg[j],"double ",7))	strcat(res, "double ");
		else if(!strncmp(arg[j],"gsl_vector *",12))	strcat(res, "ptr ");
		else if(!strncmp(arg[j],"gsl_matrix *",12))	strcat(res, "ptr ");
		else	sprintf(res, " !!! %s;", arg[j]);	// NOTE should be never here
	}
	return res;
}

bool parse_file(const char *fname, FILE *out)
{
	if(!fname || fname[0]==0)	return false;
	FILE *fp=fopen(fname,"rt");
	if(!fp)	return false;
	
	char buf[1024], *ptr, fnc[128]="";
	while(!feof(fp))
	{
		fgets(buf,1024,fp);
		// first filter unwanted strings
		if(buf[0]==0 || buf[0]=='\n' || buf[1]=='\n')	continue;
		if(buf[0]=='#' || buf[0]=='}')	continue;
		if(!strncmp(buf, "extern",6))	continue;
		if(!strncmp(buf, "class",5))	continue;
		if(!strncmp(buf, "struct",6))	continue;
		if(!strncmp(buf, "typedef",7))	continue;
		if(strstr(buf, "void *"))	continue;
		if(strstr(buf, "_("))	continue;
		if(strstr(buf, "FILE"))	continue;
		if(strstr(buf, "TODO"))	continue;
		if(strstr(buf, "//"))	continue;
		if(strstr(buf, "...)"))	continue;

		// TODO following 5 lines enable later
		if(strstr(buf, "* const *"))	continue;
		if(strstr(buf, "uint64_t"))	continue;
		if(strstr(buf, "dual"))	continue;
		if(strstr(buf, "HADT"))	continue;
		if(strstr(buf, "HAEX"))	continue;

		// now filter comments
		if(buf[0]=='/' && buf[1]=='*')
		{
			do fgets(buf,1024,fp);	while(!strstr(buf,"*/"));
			continue;
		}
		ptr = strchr(buf,';');	if(ptr)	*ptr=' ';
		for(unsigned i=strlen(buf)-1;buf[i]<=' ';i--)	buf[i]=0;
		if(buf[0]=='/' && buf[1]=='/')
			fprintf(out,"%s\n",buf);
		else if(!strncmp(buf,"void MGL_EXPORT",15))
			fprintf(out,"libmgl %s\t(void)\t%s\n",parse_name(buf+16,fnc),fnc);
		else if(!strncmp(buf,"int MGL_EXPORT",14))
			fprintf(out,"libmgl %s\t(int)\t%s\n",parse_name(buf+15,fnc),fnc);
		else if(!strncmp(buf,"double MGL_EXPORT",17))
			fprintf(out,"libmgl %s\t(double)\t%s\n",parse_name(buf+18,fnc),fnc);
		else if(!strncmp(buf,"mreal MGL_EXPORT",16))
			fprintf(out,"libmgl %s\t(sf)\t%s\n",parse_name(buf+17,fnc),fnc);
		else if(!strncmp(buf,"long MGL_EXPORT",15))
			fprintf(out,"libmgl %s\t(int)\t%s\n",parse_name(buf+16,fnc),fnc);
		else if(!strncmp(buf,"HMDT MGL_EXPORT",15))
			fprintf(out,"libmgl %s\t(ptr)\t%s\n",parse_name(buf+16,fnc),fnc);
		else if(!strncmp(buf,"HMGL MGL_EXPORT",15))
			fprintf(out,"libmgl %s\t(ptr)\t%s\n",parse_name(buf+16,fnc),fnc);
		else if(!strncmp(buf,"MGL_EXPORT const char *",23))
			fprintf(out,"libmgl %s\t(ptr)\t%s\n",parse_name(buf+24,fnc),fnc);
		else if(!strncmp(buf,"MGL_EXPORT mreal *",18))
			fprintf(out,"libmgl %s\t(ptr)\t%s\n",parse_name(buf+19,fnc),fnc);
		else if(!strncmp(buf,"MGL_EXPORT const unsigned char *",32))
			fprintf(out,"libmgl %s\t(ptr)\t%s\n",parse_name(buf+33,fnc),fnc);
		else if(!strncmp(buf,"HMPR MGL_EXPORT",15))
			fprintf(out,"libmgl %s\t(ptr)\t%s\n",parse_name(buf+16,fnc),fnc);
		else if(!strncmp(buf,"HMEX MGL_EXPORT",15))
			fprintf(out,"libmgl %s\t(ptr)\t%s\n",parse_name(buf+16,fnc),fnc);
		else if(!strncmp(buf,"HADT MGL_EXPORT",15))
			fprintf(out,"libmgl %s\t(ptr)\t%s\n",parse_name(buf+16,fnc),fnc);
		else	fprintf(out,"!!!!\t%s\n",buf);	// NOTE should be never here!
	}
	fclose(fp);
	return true;
}

int main()
{
	FILE *fout = fopen("../include/mgl2/mgl.fs","wt");
	fprintf(fout,"%s\n",head);
	for(int i=0;parse_file(files[i],fout);i++);
	fclose(fout);
	return 0;
}
