#include <stdio.h>
#include <string.h>
#include<mgl2/define.h>


const char *files[] =
{
	"../../include/mgl2/abstract.h",
	"../../include/mgl2/base_cf.h",
	"../../include/mgl2/data_cf.h",
	"../../include/mgl2/datac_cf.h",
	"../../include/mgl2/cont.h",
	"../../include/mgl2/fit.h",
	"../../include/mgl2/plot.h",
	"../../include/mgl2/surf.h",
	"../../include/mgl2/volume.h",
	"../../include/mgl2/vect.h",
	"../../include/mgl2/prim.h",
	"../../include/mgl2/other.h",
	"../../include/mgl2/canvas_cf.h",
	"../../include/mgl2/addon.h",
	"" };

const char *head =
		"//**************************************************************************\n\
// mgl_pas.pas is part of Math Graphic Library                             *\n\
// Copyright (C) 2008-2013 Mikhail Barg, Alexey Balakin                    *\n\
//                                                                         *\n\
//   This program is free software; you can redistribute it and/or modify  *\n\
//   it under the terms of the GNU Library General Public License as       *\n\
//   published by the Free Software Foundation; either version 2 of the    *\n\
//   License, or (at your option) any later version.                       *\n\
//                                                                         *\n\
//   This program is distributed in the hope that it will be useful,       *\n\
//   but WITHOUT ANY WARRANTY; without even the implied warranty of        *\n\
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *\n\
//   GNU General Public License for more details.                          *\n\
//                                                                         *\n\
//   You should have received a copy of the GNU Library General Public     *\n\
//   License along with this program; if not, write to the                 *\n\
//   Free Software Foundation, Inc.,                                       *\n\
//   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *\n\
//**************************************************************************\n\n\
unit mgl_pas;\n\n\
{$IFDEF FPC}\n\
{$MODE DELPHI }\n\
{$PACKENUM 4}    (* use 4-byte enums *)\n\
{$PACKRECORDS C} (* C/C++-compatible record packing *)\n\
{$ELSE}\n\
{$MINENUMSIZE 4} (* use 4-byte enums *)\n\
{$ENDIF}\n\n\
{$IFDEF DARWIN}\n\
{$linklib libmgl}\n\
{$ENDIF}\n\n\
interface\n\n\
uses\n\
{$IFDEF MSWINDOWS}\n\
Windows, Graphics,\n\
{$ENDIF}\n\
Math;\n\n\
const MGL_VER2 = 2.2;\n\
//* This define enables double precision in MathGL */\n\
MGL_USE_DOUBLE = 1;\n\n\
const\n\
{$IFDEF MSWINDOWS}\n\
//win - .dll\n\
libmgl = 'libmgl.dll';\n\
libmglglut = 'libmgl-glut.dll';\n\
libmglfltk = 'libmgl-fltk.dll';\n\
libmglqt   = 'libmgl-qt.dll';\n\
{$ELSE}\n\
{$IFDEF LINUX}\n\
//linux - .so\n\
libmgl = 'libmgl.so';\n\
libmglglut = 'libmgl-glut.so';\n\
libmglfltk = 'libmgl-fltk.so';\n\
libmglqt   = 'libmgl-qt.so';\n\
{$ELSE}\n\
{$IFDEF DARWIN}\n\
//darwin - .dylib\n\
libmgl = 'libmgl.dylib';\n\
libmglglut = 'libmgl-glut.dylib';\n\
libmglfltk = 'libmgl-fltk.dylib';\n\
libmglqt   = 'libmgl-qt.dylib';\n\
{$ELSE}\n\
// other platforms?\n\n\
{$ENDIF}\n\
{$ENDIF}\n\
{$ENDIF}\n\n\
{$IF (MGL_USE_DOUBLE = 0)}\n\
type mreal = double;\n\
{$ELSE}\n\
type mreal = real;\n\
{$IFEND}\n\
{$IFDEF FPC}\n\
{$ELSE}\n\
type QWord = Int64;\n\
{$ENDIF}\n\n\
Pmreal = ^mreal;\n\n\
type TNGLDraw = record\n\
end;\n\
type TMGLGraph = record\n\
end;\n\
type TMGLData = record\n\
end;\n\
type TMGLParse = record\n\
end;\n\
type TMGLFormula = record\n\
end;\n\
type TMGLFormulaC = record\n\
end;\n\
type TMGLDataC = record\n\
end;\n\
type HMDR = ^TNGLDraw;\n\
type HMGL = ^TMGLGraph;\n\
type HMDT = ^TMGLData;\n\
type HMPR = ^TMGLParse;\n\
type PPChar = ^PChar;\n\
type HMEX = ^TMGLFormula;\n\
type HAEX = ^TMGLFormulaC;\n\
type HADT = ^TMGLDataC;\n\n\
type Preal = ^single;\n\
type Pdouble = ^double;\n\
type Pint = ^integer;\n\
type dual = record\n\
re, im: mreal;\n\
end;\n\
type Pdual = ^dual;\n\
type TGSLVector = record\n\
end;\n\
type TGSLMatrix = record\n\
end;\n\
type PGSLVector = ^TGSLVector;\n\
type PGSLMatrix = ^TGSLMatrix;\n\n\
type TMglDrawFunction = function (gr: HMGL; p: pointer): integer; cdecl;\n\
function mgl_create_graph_gl(): HMGL; cdecl; external libmgl;\n\
function mgl_create_graph_glut(draw: TMglDrawFunction; const title: PChar; par: pointer): HMGL; cdecl; external libmglglut;\n\
function mgl_create_graph_fltk(draw: TMglDrawFunction; const title: PChar; par: pointer): HMGL; cdecl; external libmglfltk;\n\
procedure mgl_fltk_run(); cdecl; external libmglfltk;\n\
function mgl_create_graph_qt(draw: TMglDrawFunction; const title: PChar; par: pointer): HMGL; cdecl; external libmglqt;\n\
procedure mgl_qt_run(); cdecl; external libmglqt;\n";


const char *footer =
"\n\
\n\
{$IFDEF MSWINDOWS}\n\
//*****************************************************************************/\n\
// Delphi - specific\n\
//*****************************************************************************/\n\
procedure mgl_begin();\n\
procedure mgl_end();\n\
\n\
procedure mgl_draw_on_canvas(gr: HMGL; width, height: integer; canvas: TCanvas; switchXY: boolean = false);\n\
\n\
implementation\n\
\n\
var _FPUCW: word;\n\
\n\
procedure mgl_begin();\n\
 begin\n\
  _FPUCW := Get8087CW();     // backup current FPU CW\n\
  Set8087CW(_FPUCW or $3F); // masking all FPU exceptions\n\
 end;\n\
\n\
procedure mgl_end();\n\
 begin\n\
  Set8087CW(_FPUCW);         // restore old FPU CW\n\
 end;\n\
\n\
procedure mgl_draw_on_canvas(gr: HMGL; width, height: integer; canvas: TCanvas; switchXY: boolean = false);\n\
  var i, j: integer;\n\
      bytes: PByte;\n\
      col: TColor;\n\
 begin\n\
  bytes := mgl_get_rgb(gr);\n\
\n\
  if (not switchXY) then\n\
   for j := 0 to height - 1 do\n\
    for i := 0 to width - 1 do\n\
     begin\n\
      col := 0;\n\
      col := col or (bytes^);\n\
      inc(bytes);\n\
      col := col or (bytes^) shl 8;\n\
      inc(bytes);\n\
       col := col or (bytes^) shl 16;\n\
      inc(bytes);\n\
      canvas.Pixels[i, j] := col;\n\
    end\n\
  else\n\
   for j := height - 1 downto 0 do\n\
    for i := 0 to width - 1 do\n\
     begin\n\
      col := 0;\n\
      col := col or (bytes^);\n\
      inc(bytes);\n\
      col := col or (bytes^) shl 8;\n\
      inc(bytes);\n\
       col := col or (bytes^) shl 16;\n\
      inc(bytes);\n\
      canvas.Pixels[j, i] := col;\n\
     end;\n\
 end;\n\
\n\
{$ENDIF}\n\
end.\n";


bool processArgument(char *dest, const char *arg, const char *prefix, const char *format)
{
	const int prefixLen = strlen(prefix);
	if ( strncmp(arg, prefix, prefixLen) == 0 )
	{
		char argname[32];
		strcpy(argname, arg + prefixLen);
		if (strcmp(argname, "to") == 0)
		{
			int argNameLen = strlen(argname);
			argname[argNameLen] = '_';
			argname[argNameLen + 1] = 0;
		}
		sprintf(dest, format, argname);
		return true;
	}
	return false;
}

const char *parse_name(char *name, bool &needOverload)
{
	const int MAX_ARG = 20; // TODO check if 20 arguments is enough
	static char res[1024];
	char *ptr, *arg[MAX_ARG], nul = 0;	
	unsigned i, j;
	needOverload = false;
	for ( i = 0; name[i] != '('; ++i )
	{
		res[i] = name[i];
	}

//	res[i] = 0;
//	printf("'%s'\n", res);
	
	//TODO: special case, invent some general way to handle overloads, i.e. functions with same names. Still would require re-doing whole parser..
	if (strncmp(res, " mgl_expi", i - 1) == 0)
	{
		needOverload = true;
	}
	res[i] = '(';
	res[i + 1] = 0;
	++i;
	while ( name[i] <= ' ' )
	{
		++i;
	}
	for ( j = 0; j < MAX_ARG; ++j )
	{
		arg[j] = &nul;
	}
	for ( j = 0; j < MAX_ARG; ++j )
	{
		arg[j] = (name[i] <= ' ' ? name + i + 1 : name + i);
		ptr = strchr(name + i, ',');
		if ( !ptr )
		{
			break;
		}
		*ptr = 0;
		i = ptr - name + 1;
	}
	ptr = strchr(name + i, ')');
	if ( ptr )
	{
		*ptr = 0;
	}
	if ( arg[0][0] == 0 )
	{
		strcat(res, " ");
	}
	for ( j = 0; j < MAX_ARG; ++j )
	{
		if ( arg[j][0] == 0 )
		{
			break;
		}
		ptr = res + strlen(res);
		if ( 		processArgument(ptr, arg[j], "HMGL ", "%s: HMGL;")
				||  processArgument(ptr, arg[j], "HCDT ",  "const %s: HMDT;")
				||	processArgument(ptr, arg[j], "HMDT ",  "%s: HMDT;")
				||	processArgument(ptr, arg[j], "mglDataA *",  "%s: HMDT;")
				||	processArgument(ptr, arg[j], "HADT ",  "%s: HADT;")
				||	processArgument(ptr, arg[j], "HAEX ",  "%s: HAEX;")
				||	processArgument(ptr, arg[j], "HMPR ",  "%s: HMPR;")
				||	processArgument(ptr, arg[j], "HMEX ",  "%s: HMEX;")
				||	processArgument(ptr, arg[j], "const float *",  "const %s: Preal;")
				||	processArgument(ptr, arg[j], "const double *",  "const %s: Pdouble;")
				||	processArgument(ptr, arg[j], "mreal *",  "%s: Pmreal;")
				||	processArgument(ptr, arg[j], "double *",  "%s: Pdouble;")
				||	processArgument(ptr, arg[j], "char *",  "%s: PChar;")
				||	processArgument(ptr, arg[j], "unsigned char *",  "%s: PByte;")
				||	processArgument(ptr, arg[j], "int *", "%s: Pint;")
				||	processArgument(ptr, arg[j], "long *",  "%s: Pint;")
				||	processArgument(ptr, arg[j], "const char *",  "const %s: PChar;")
				||	processArgument(ptr, arg[j], "const unsigned char *",  "const %s: PByte;")
				||	processArgument(ptr, arg[j], "const wchar_t *",  "const %s: PWideChar;")
				||	processArgument(ptr, arg[j], "char ",  "%s: char;")
				||	processArgument(ptr, arg[j], "long ",  "%s: integer;")
				||	processArgument(ptr, arg[j], "uint32_t ",  "%s: LongWord;")
				||	processArgument(ptr, arg[j], "uint64_t ",  "%s: QWord;")
				||	processArgument(ptr, arg[j], "int ",  "%s: integer;")
				||	processArgument(ptr, arg[j], "mreal ",  "%s: mreal;")
				||	processArgument(ptr, arg[j], "const dual *",  "const %s: Pdual;")
				||	processArgument(ptr, arg[j], "dual *",  "%s: Pdual;")
				||	processArgument(ptr, arg[j], "dual ",  "%s: dual;")
				||	processArgument(ptr, arg[j], "double ",  "%s: double;")
				||	processArgument(ptr, arg[j], "gsl_vector *",  "%s: PGSLVector;")
				||	processArgument(ptr, arg[j], "gsl_matrix *",  "%s: PGSLMatrix;")
			)
		{
			//already procedded in processArgument
		}
		else
		{
			sprintf(ptr, " !!! %s;", arg[j]);
		}
	}
	
	i = strlen(res);
	res[i - 1] = ')';
	return res;
}

bool processDeclaration(FILE *out, char *declaration, const char *prefix, const char *format)
{
	const int prefixLen = strlen(prefix);
	if ( strncmp(declaration, prefix, prefixLen) == 0 )
	{
		bool needOverload = false;
		fprintf(out, format, parse_name(declaration + prefixLen, needOverload));
		if (needOverload)
		{
			fprintf(out, "overload;\n");
		}
		return true;
	}
	return false;
}

bool parse_file(const char *fname, FILE *out)
{
	if ( !fname || fname[0] == 0 )
	{
		return false;
	}
	FILE *fp = fopen(fname, "rt");
	if ( !fp )
	{
		return false;
	}
	
	fprintf(out, "{== %s ==}\n", fname);
	
	char buf[1024], *ptr;
	while ( !feof(fp) )
	{
		if(!fgets(buf, 1024, fp)) break;
		// first filter unwanted strings
		if ( buf[0] == 0 || buf[0] == '\n' || buf[1] == '\n' )
		{
			continue;
		}
		if ( buf[0] == '#' || buf[0] == '}' )
		{
			continue;
		}
		if ( !strncmp(buf, "extern", 6) )
		{
			continue;
		}
		if ( !strncmp(buf, "class", 5) )
		{
			continue;
		}
		if ( !strncmp(buf, "struct", 6) )
		{
			continue;
		}
		if ( !strncmp(buf, "typedef", 7) )
		{
			continue;
		}
		if ( strstr(buf, "void *") )
		{
			continue;
		}
		if ( strstr(buf, "_(") )
		{
			continue;
		}
		if ( strstr(buf, "FILE") )
		{
			continue;
		}
		if ( strstr(buf, "TODO") )
		{
			continue;
		}
		if ( strstr(buf, "...)") )
		{
			continue;
		}
		
		// TODO enable later
		if ( strstr(buf, "* const *") )
		{
			continue;
		}
		
		// now filter comments
		if ( buf[0] == '/' && buf[1] == '*' )
		{
			do
			{
				fgets(buf, 1024, fp);
			}
			while ( !strstr(buf, "*/") );
			continue;
		}
		ptr = strchr(buf, ';');
		if ( ptr )
		{
			*ptr = ' ';
		}
		for ( unsigned i = strlen(buf) - 1; buf[i] <= ' '; i-- )
		{
			buf[i] = 0;
		}
		if ( buf[0] == '/' && buf[1] == '/' )
		{
			fprintf(out, "%s\n", buf);
		}
		else if (	   processDeclaration(out, buf, "void MGL_EXPORT_PURE", 	"procedure %s; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "int MGL_EXPORT_PURE", 		"function %s: integer; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "double MGL_EXPORT_PURE", 	"function %s: double; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "mreal MGL_EXPORT_PURE", 	"function %s: mreal; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "long MGL_EXPORT_PURE", 	"function %s: integer; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "mdual MGL_EXPORT_PURE", 	"function %s: dual; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT_PURE dual *", 	"function %s: PDual; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HMDT MGL_EXPORT_PURE", 	"function %s: HMDT; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "HMGL MGL_EXPORT_PURE", 	"function %s: HMGL; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT_PURE const char *", "function %s: PChar; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT_PURE mreal *", 	"function %s: Pmreal; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT_PURE const unsigned char *", "function %s: PByte; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "HMPR MGL_EXPORT_PURE", 	"function %s: HMPR; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HMEX MGL_EXPORT_PURE", 	"function %s: HMEX; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HADT MGL_EXPORT_PURE", 	"function %s: HADT; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HAEX MGL_EXPORT_PURE", 	"function %s: HAEX; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "void MGL_EXPORT_CONST", 		"procedure %s; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "int MGL_EXPORT_CONST", 		"function %s: integer; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "double MGL_EXPORT_CONST", 	"function %s: double; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "mreal MGL_EXPORT_CONST", 	"function %s: mreal; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "long MGL_EXPORT_CONST", 		"function %s: integer; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "mdual MGL_EXPORT_CONST", 	"function %s: dual; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT_CONST dual *", 	"function %s: PDual; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HMDT MGL_EXPORT_CONST", 		"function %s: HMDT; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "HMGL MGL_EXPORT_CONST", 		"function %s: HMGL; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT_CONST const char *", "function %s: PChar; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT_CONST mreal *", 	"function %s: Pmreal; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT_CONST const unsigned char *", "function %s: PByte; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "HMPR MGL_EXPORT_CONST", 		"function %s: HMPR; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HMEX MGL_EXPORT_CONST", 		"function %s: HMEX; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HADT MGL_EXPORT_CONST", 		"function %s: HADT; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HAEX MGL_EXPORT_CONST", 		"function %s: HAEX; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "void MGL_EXPORT", 		"procedure %s; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "int MGL_EXPORT", 		"function %s: integer; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "double MGL_EXPORT", 	"function %s: double; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "mreal MGL_EXPORT", 	"function %s: mreal; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "long MGL_EXPORT", 		"function %s: integer; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "mdual MGL_EXPORT", 	"function %s: dual; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT dual *", 	"function %s: PDual; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HMDT MGL_EXPORT", 		"function %s: HMDT; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "HMGL MGL_EXPORT", 		"function %s: HMGL; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT const char *", "function %s: PChar; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT mreal *", 	"function %s: Pmreal; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "MGL_EXPORT const unsigned char *", "function %s: PByte; cdecl; external libmgl;\n") 
					|| processDeclaration(out, buf, "HMPR MGL_EXPORT", 		"function %s: HMPR; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HMEX MGL_EXPORT", 		"function %s: HMEX; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HADT MGL_EXPORT", 		"function %s: HADT; cdecl; external libmgl;\n")
					|| processDeclaration(out, buf, "HAEX MGL_EXPORT", 		"function %s: HAEX; cdecl; external libmgl;\n")
				)
		{
			//already processed by processDeclaration
		}
/*		else	// comment this -- it looks as it hangs on classes only, which should be omitted by anyway
		{
			fprintf(out, "{!!!!\t%s}\n", buf);	// NOTE should be never here!
		}*/
	}
	fclose(fp);
	return true;
}

int main()
{
	FILE *fout = fopen("../../include/mgl2/mgl_pas.pas", "wt");
	fprintf(fout, "%s\n", head);
	for ( int i = 0; parse_file(files[i], fout); i++ ) {}
	fprintf(fout, "%s\n", footer);
	fclose(fout);
	return 0;
}
