/***************************************************************************
 * make_bin.cpp is part of Math Graphic Library
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include <getopt.h>
#include "mgl2/font.h"
//-----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
	mglFont fnt;
	std::string path, base, fname;

	while(1)
	{
		int ch = getopt(argc, argv, "p:o:h");
		if(ch=='p')	path = optarg;
		else if(ch=='o')	fname = optarg;
		else if(ch=='h' || (ch==-1 && optind>=argc))
		{
			printf("make_bin convert mgl font to binary file.\nCurrent version is 2.%g\n",MGL_VER2);
			printf("Usage:\tmake_bin [parameter(s)] base\n");
			printf(
				"\t-p path      set specific path for base font files\n"
				"\t-o fname     set output filename (use ${base}.vfmb by default)\n"
				"\t-h           print this message\n" );
			return 0;
		}
		else if(ch==-1 && optind<argc)	{	base = argv[optind];	break;	}
	}
	if(fname.empty())	fname = base + ".vfmb";
	fnt.Load(base.c_str(),path.c_str());
	size_t size = fnt.SaveBin(fname.c_str());
	printf("Output size of %s should be %zu\n", fname.c_str(), size);
	return 0;
}
//-----------------------------------------------------------------------------
