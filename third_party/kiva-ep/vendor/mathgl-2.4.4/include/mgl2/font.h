/***************************************************************************
 * font.h is part of Math Graphic Library
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Library General Public License as       *
 *   published by the Free Software Foundation; either version 3 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU Library General Public     *
 *   License along with this program; if not, write to the                 *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
//-----------------------------------------------------------------------------
#ifndef _MGL_FONT_H_
#define _MGL_FONT_H_

#include "mgl2/define.h"
//-----------------------------------------------------------------------------
#define MGL_FONT_BOLD		0x01000000	// This value is used binary
#define MGL_FONT_ITAL		0x02000000	// This value is used binary
#define MGL_FONT_BOLD_ITAL	0x03000000
#define MGL_FONT_WIRE		0x04000000
#define MGL_FONT_OLINE		0x08000000	// This value is used binary
#define MGL_FONT_ULINE		0x10000000
#define MGL_FONT_ZEROW		0x20000000	// internal codes
#define MGL_FONT_UPPER		0x40000000
#define MGL_FONT_LOWER		0x80000000
#define MGL_FONT_ROMAN		0xfcffffff
#define MGL_FONT_MASK		0x00ffffff
#define MGL_COLOR_MASK		0xffffff00
#define MGL_FONT_STYLE		0x3f000000
//-----------------------------------------------------------------------------
struct mglGlyphDescr
{
	wchar_t id;		///< Unicode ID for glyph
	int tr[4];		///< Shift of glyph description by triangles (for solid font)
	int ln[4];		///< Shift of glyph description by lines (for wire font)
	short numt[4];	///< Number of triangles in glyph description (for solid font)
	short numl[4];	///< Number of lines in glyph description (for wire font)
	short width[4];	///< Width of glyph for wire font
	short y1[4], y2[4];	///< minimal and maximal y-coordinates
	mglGlyphDescr()	{	memset(this,0,sizeof(mglGlyphDescr));	}
};
inline bool operator<(const mglGlyphDescr &a,const mglGlyphDescr &b)	{	return a.id<b.id;	}
inline bool operator>(const mglGlyphDescr &a,const mglGlyphDescr &b)	{	return a.id>b.id;	}
#if defined(_MSC_VER)
MGL_EXTERN template class MGL_EXPORT std::vector<mglGlyphDescr>;
#endif
//-----------------------------------------------------------------------------
extern const float mgl_fact;
struct MGL_EXPORT mglTeXsymb	{	unsigned kod;	const wchar_t *tex;	};
const float mgl_fgen = 4*14;
/// Get font color, style and align for internal parser
bool MGL_EXPORT mglGetStyle(const char *how, int *font, int *align=0);
long MGL_EXPORT mgl_internal_code(unsigned s, const std::vector<mglGlyphDescr> &glyphs);
class mglBase;
//-----------------------------------------------------------------------------
/// Class for font typeface and text plotting procedures
class MGL_EXPORT mglFont
{
public:
	mglBase *gr;	///< mglBase class used for drawing characters
	mglFont(const char *name=0, const char *path=0);
	virtual ~mglFont();
	bool parse;		///< Parse LaTeX symbols

	/// Load font data to memory. Normally used by constructor.
	bool Load(const char *base, const char *path=0);
	/// Load binary font data to memory. Normally used by constructor.
	bool LoadBin(const char *base, const char *path=0);
	/// Save binary font data
	size_t SaveBin(const char *fname);
	/// Free memory
	void Clear();
	/// Copy data from other font
	void Copy(mglFont *);
	/// Restore default font
	void Restore();
	/// Return true if font is loaded
	inline bool Ready() const	{	return GetNumGlyph()!=0;	}

	/// Get height of text
	float Height(int font) const MGL_FUNC_PURE;
	/// Get height of text
	float Height(const char *how) const MGL_FUNC_PURE;
	/// Print text string for font specified by string
	float Puts(const char *str,const char *how,float c1,float c2) const;
	/// Get width of text string for font specified by string
	float Width(const char *str, const char *how, float *y1=0, float *y2=0) const;
	/// Print text string for font specified by string
	float Puts(const wchar_t *str,const char *how,float c1,float c2) const;
	/// Get width of text string for font specified by string
	float Width(const wchar_t *str,const char *how, float *y1=0, float *y2=0) const;

	/// Get internal code for symbol
	inline long Internal(unsigned s) const	{	return mgl_internal_code(s,glyphs);	}
	/// Return number of glyphs
	inline unsigned GetNumGlyph() const	{	return glyphs.size();	};
	/// Return some of pointers
	inline const short *GetTr(int s, long j) const	{	return Buf+glyphs[j].tr[s];	}
	inline const short *GetLn(int s, long j) const	{	return Buf+glyphs[j].ln[s];	}
	inline int GetNt(int s, long j) const		{	return glyphs[j].numt[s];	}
	inline int GetNl(int s, long j) const		{	return glyphs[j].numl[s];	}
	inline short GetWidth(int s, long j) const	{	return glyphs[j].width[s];	}
	inline float GetFact(int s) const			{	return fact[s];	}
	inline wchar_t GetUnicode(long j) const		{	return j>=0?glyphs[j].id:0;	}
protected:
	std::vector<mglGlyphDescr> glyphs;	///< information about know glyphs
	float fact[4];	///< Divider for width of glyph
	short *Buf;		///< Buffer for glyph descriptions
	size_t numb;		///< Buffer size

	/// Print text string for font specified by integer constant
	float Puts(const wchar_t *str,int font,int align, float c1,float c2) const;
	/// Get height of text string for font specified by integer constant
	float Height(const wchar_t *str,int font) const;
	/// Get width of text string for font specified by integer constant
	float Width(const wchar_t *str,int font,int align, float &y1, float &y2) const;
	/// Replace TeX symbols by its UTF code and add font styles
	void Convert(const wchar_t *str, unsigned *res) const;
	/// Fill minimal and maximal y-coordinates
	void FillY12();

	/// Draw string recursively
	/* x,y - position, f - factor, style: 0x1 - italic, 0x2 - bold, 0x4 - overline, 0x8 - underline, 0x10 - empty (not draw) */
	float Puts(const unsigned *str, float x,float y,float f,int style,float c1,float c2, float &y1, float &y2) const;
	/// Parse LaTeX command
	unsigned Parse(const wchar_t *s) const;
	/// Get symbol for character ch with given font style
	unsigned Symbol(char ch) const MGL_FUNC_PURE;
private:
	float get_ptr(long &i,unsigned *str, unsigned **b1, unsigned **b2,float &w1,float &w2, float f1, float f2, int st) const;
	bool read_data(const char *fname, int s, std::vector<short> &buf, std::vector<mglGlyphDescr> &extra);
	void main_copy();
	bool read_main(const char *fname, std::vector<short> &buf);
	inline void mem_alloc(long numg)	{	glyphs.resize(numg);	}
	bool read_def();
	void draw_ouline(int st, float x, float y, float f, float g, float ww, float ccol) const;
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
