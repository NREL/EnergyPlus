/***************************************************************************
 * base_cf.h is part of Math Graphic Library
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
#ifndef _MGL_BASE_CF_H_
#define _MGL_BASE_CF_H_
#include "mgl2/abstract.h"
//-----------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif

/// Check if MathGL version is valid (return 0) or not (return 1)
int MGL_EXPORT mgl_check_version(const char *ver);
int MGL_EXPORT mgl_check_version_(const char *ver, int);
/// Suppress printing warnings to stderr
void MGL_EXPORT mgl_suppress_warn(int on);
void MGL_EXPORT mgl_suppress_warn_(int *on);
/// Get last warning code
int MGL_EXPORT_PURE mgl_get_warn(HMGL gr);
int MGL_EXPORT_PURE mgl_get_warn_(uintptr_t *gr);
/// Set warning code ant fill message
void MGL_EXPORT mgl_set_warn(HMGL gr, int code, const char *text);
void MGL_EXPORT mgl_set_warn_(uintptr_t *gr, int *code, const char *text,int);
/// Get text of warning message(s)
MGL_EXPORT_PURE const char *mgl_get_mess(HMGL gr);
int MGL_EXPORT mgl_get_mess_(uintptr_t *gr, char *out, int len);

/// Set name of plot for saving filename
void MGL_EXPORT mgl_set_plotid(HMGL gr, const char *id);
void MGL_EXPORT mgl_set_plotid_(uintptr_t *gr, const char *id,int);
/// Get name of plot for saving filename
MGL_EXPORT_PURE const char *mgl_get_plotid(HMGL gr);
int MGL_EXPORT mgl_get_plotid_(uintptr_t *gr, char *out, int len);

/// Ask to stop drawing
void MGL_EXPORT mgl_ask_stop(HMGL gr, int stop);
void MGL_EXPORT mgl_ask_stop_(uintptr_t *gr, int *stop);
/// Check if plot termination is asked
int MGL_EXPORT mgl_need_stop(HMGL gr);
int MGL_EXPORT mgl_need_stop_(uintptr_t *gr);
/// Set callback function for event processing
void MGL_EXPORT mgl_set_event_func(HMGL gr, void (*func)(void *), void *par);

/// Get plot quality
int MGL_EXPORT_PURE mgl_get_quality(HMGL gr);
int MGL_EXPORT_PURE mgl_get_quality_(uintptr_t *gr);
/// Set plot quality
/** qual=0 -- no face drawing (fastest),
 *  qual=1 -- no color interpolation (fast),
 *  qual=2 -- high quality (normal),
 *  qual|4 -- direct bitmap drawing (low memory usage);
 *  qual|8 for dots drawing instead of primitives (extremely fast). */
void MGL_EXPORT mgl_set_quality(HMGL gr, int qual);
void MGL_EXPORT mgl_set_quality_(uintptr_t *gr, int *qual);
/// Set drawing region for Quality&4
void MGL_EXPORT mgl_set_draw_reg(HMGL gr, long nx, long ny, long m);
void MGL_EXPORT mgl_set_draw_reg_(uintptr_t *gr, int *nx, int *ny, int *m);

/// Check if support of frames is enabled (i.e. MGL_VECT_FRAME is set and Quality&MGL_DRAW_LMEM==0)
int MGL_EXPORT_PURE mgl_is_frames(HMGL gr);
/// Get bit-value flag of HMGL state (for advanced users only)
int MGL_EXPORT_PURE mgl_get_flag(HMGL gr, uint32_t flag);
int MGL_EXPORT_PURE mgl_get_flag_(uintptr_t *gr, unsigned long *flag);
/// Set bit-value flag of HMGL state (for advanced users only)
void MGL_EXPORT mgl_set_flag(HMGL gr, int val, uint32_t flag);
void MGL_EXPORT mgl_set_flag_(uintptr_t *gr, int *val, unsigned long *flag);
/// Change counter of HMGL uses (for advanced users only). Non-zero counter prevent automatic object removing.
long MGL_EXPORT mgl_use_graph(HMGL gr, int inc);
long MGL_EXPORT mgl_use_graph_(uintptr_t *gr, int *inc);
void MGL_EXPORT mgl_set_rdc_acc(HMGL gr, int reduce);
void MGL_EXPORT mgl_set_rdc_acc_(uintptr_t *gr, int *reduce);

/// Start group of objects
void MGL_EXPORT mgl_start_group(HMGL gr, const char *name);
void MGL_EXPORT mgl_start_group_(uintptr_t *gr, const char *name,int);
/// End group of objects
void MGL_EXPORT mgl_end_group(HMGL gr);
void MGL_EXPORT mgl_end_group_(uintptr_t *gr);
/// Highlight objects with given id
void MGL_EXPORT mgl_highlight(HMGL gr, int id);
void MGL_EXPORT mgl_highlight_(uintptr_t *gr, int *id);

/// Set default palette
void MGL_EXPORT mgl_set_palette(HMGL gr, const char *colors);
void MGL_EXPORT mgl_set_palette_(uintptr_t *gr, const char *colors, int);
void MGL_EXPORT mgl_set_pal_color_(uintptr_t *gr, int *n, mreal *r, mreal *g, mreal *b);
void MGL_EXPORT mgl_set_pal_num_(uintptr_t *gr, int *num);
/// Sets RGB values for color with given id
void MGL_EXPORT mgl_set_color(char id, double r, double g, double b);
void MGL_EXPORT mgl_set_color_(char *id, mreal *r, mreal *g, mreal *b, int);
/// Set default color scheme
void MGL_EXPORT mgl_set_def_sch(HMGL gr, const char *sch);
void MGL_EXPORT mgl_set_def_sch_(uintptr_t *gr, const char *sch,int);
/// Set mask for face coloring as array of type 'unsigned char[8]'
void MGL_EXPORT mgl_set_mask(char id, const char *mask);
void MGL_EXPORT mgl_set_mask_(const char *id, const char *mask,int,int);
/// Set mask for face coloring as unsigned long number
void MGL_EXPORT mgl_set_mask_val(char id, uint64_t mask);
void MGL_EXPORT mgl_set_mask_val_(const char *id, uint64_t *mask,int);
/// Set default mask rotation angle
void MGL_EXPORT mgl_set_mask_angle(HMGL gr, int angle);
void MGL_EXPORT mgl_set_mask_angle_(uintptr_t *gr, int *angle);

/// Set default value of alpha-channel
void MGL_EXPORT mgl_set_alpha_default(HMGL gr, double alpha);
void MGL_EXPORT mgl_set_alpha_default_(uintptr_t *gr, mreal *alpha);
/// Set relative width of rectangles in Bars, Barh, BoxPlot, Candle, OHLC (default is 0.7)
void MGL_EXPORT mgl_set_bar_width(HMGL gr, double width);
void MGL_EXPORT mgl_set_bar_width_(uintptr_t *gr, mreal *width);
/// Set number of mesh lines (use 0 to draw all of them)
void MGL_EXPORT mgl_set_meshnum(HMGL gr, int num);
void MGL_EXPORT mgl_set_meshnum_(uintptr_t *gr, int *num);
/// Set number of visible faces (use 0 to draw all of them)
void MGL_EXPORT mgl_set_facenum(HMGL gr, int num);
void MGL_EXPORT mgl_set_facenum_(uintptr_t *gr, int *num);
/// Clear unused points and primitives. Useful only in combination with mgl_set_facenum().
void MGL_EXPORT mgl_clear_unused(HMGL gr);
void MGL_EXPORT mgl_clear_unused_(uintptr_t *gr);

/// Set ambient light brightness
void MGL_EXPORT mgl_set_ambbr(HMGL gr, double i);
void MGL_EXPORT mgl_set_ambbr_(uintptr_t *gr, mreal *i);
/// Set diffusive light brightness
void MGL_EXPORT mgl_set_difbr(HMGL gr, double i);
void MGL_EXPORT mgl_set_difbr_(uintptr_t *gr, mreal *i);
/// Use diffusive light (only for local light sources) -- OBSOLETE
void MGL_EXPORT mgl_set_light_dif(HMGL gr, int enable);
void MGL_EXPORT mgl_set_light_dif_(uintptr_t *gr, int *enable);

/// Set cutting for points outside of bounding box
void MGL_EXPORT mgl_set_cut(HMGL gr, int cut);
void MGL_EXPORT mgl_set_cut_(uintptr_t *gr, int *cut);
/// Set additional cutting box
void MGL_EXPORT mgl_set_cut_box(HMGL gr, double x1,double y1,double z1,double x2,double y2,double z2);
void MGL_EXPORT mgl_set_cut_box_(uintptr_t *gr, mreal *x1, mreal *y1, mreal *z1, mreal *x2, mreal *y2, mreal *z2);
/// Set the cutting off condition (formula)
void MGL_EXPORT mgl_set_cutoff(HMGL gr, const char *EqC);
void MGL_EXPORT mgl_set_cutoff_(uintptr_t *gr, const char *EqC, int);

/// Set values of axis range
void MGL_EXPORT mgl_set_ranges(HMGL gr, double x1, double x2, double y1, double y2, double z1, double z2);
void MGL_EXPORT mgl_set_ranges_(uintptr_t *gr, mreal *x1, mreal *x2, mreal *y1, mreal *y2, mreal *z1, mreal *z2);
/// Set range in direction dir as [v1, v2]
void MGL_EXPORT mgl_set_range_val(HMGL gr, char dir, double v1,double v2);
void MGL_EXPORT mgl_set_range_val_(uintptr_t *gr, const char *dir, mreal *v1, mreal *v2,int);
/// Add [v1, v2] to the current range in direction dir
void MGL_EXPORT mgl_add_range_val(HMGL gr, char dir, double v1,double v2);
void MGL_EXPORT mgl_add_range_val_(uintptr_t *gr, const char *dir, mreal *v1, mreal *v2,int);
/// Set range in direction dir as minimal and maximal values of data a
void MGL_EXPORT mgl_set_range_dat(HMGL gr, char dir, HCDT a, int add);
void MGL_EXPORT mgl_set_range_dat_(uintptr_t *gr, const char *dir, uintptr_t *a, int *add,int);
/// Set ranges for automatic variables
void MGL_EXPORT mgl_set_auto_ranges(HMGL gr, double x1, double x2, double y1, double y2, double z1, double z2, double c1, double c2);
void MGL_EXPORT mgl_set_auto_ranges_(uintptr_t *gr, mreal *x1, mreal *x2, mreal *y1, mreal *y2, mreal *z1, mreal *z2, mreal *c1, mreal *c2);
/// Set axis range scaling -- simplified way to shift/zoom axis range -- need to redraw whole image!
void MGL_EXPORT mgl_zoom_axis(HMGL gr, double x1,double y1,double z1,double c1,double x2,double y2,double z2,double c2);
void MGL_EXPORT mgl_zoom_axis_(uintptr_t *gr, mreal *x1, mreal *y1, mreal *z1, mreal *c1, mreal *x2, mreal *y2, mreal *z2, mreal *c2);

/// Set axis origin
void MGL_EXPORT mgl_set_origin(HMGL gr, double x0, double y0, double z0);
void MGL_EXPORT mgl_set_origin_(uintptr_t *gr, mreal *x0, mreal *y0, mreal *z0);
/// Set the transformation formulas for coordinate. Use "" or NULL for built-in ones
void MGL_EXPORT mgl_set_func(HMGL gr, const char *EqX,const char *EqY,const char *EqZ,const char *EqA);
void MGL_EXPORT mgl_set_func_(uintptr_t *gr, const char *EqX, const char *EqY, const char *EqZ, const char *EqA, int, int, int, int);
/// Set one of predefined transformation rule
void MGL_EXPORT mgl_set_coor(HMGL gr, int how);
void MGL_EXPORT mgl_set_coor_(uintptr_t *gr, int *how);
/// Set to draw Ternary axis (triangle like axis, grid and so on)
/** val=1 for Ternary axis (a+b+c=1, z=z),
 *  val=2 for Quaternary axis (a+b+c+d=1),
 *  val|4 for projections. */
void MGL_EXPORT mgl_set_ternary(HMGL gr, int kind);
void MGL_EXPORT mgl_set_ternary_(uintptr_t *gr, int *kind);

/// Set to use or not tick labels rotation
void MGL_EXPORT mgl_set_tick_rotate(HMGL gr, int enable);
void MGL_EXPORT mgl_set_tick_rotate_(uintptr_t *gr, int *enable);
/// Set to use or not tick labels skipping
void MGL_EXPORT mgl_set_tick_skip(HMGL gr, int enable);
void MGL_EXPORT mgl_set_tick_skip_(uintptr_t *gr, int *enable);

/// Set default font for all new HMGL objects
void MGL_EXPORT mgl_def_font(const char *name, const char *path);
void MGL_EXPORT mgl_def_font_(const char *name, const char *path,int,int);
/// Set default size of marks (locally you can use "size" option)
void MGL_EXPORT mgl_set_mark_size(HMGL gr, double size);
void MGL_EXPORT mgl_set_mark_size_(uintptr_t *gr, mreal *size);
/// Set default size of arrows (locally you can use "size" option)
void MGL_EXPORT mgl_set_arrow_size(HMGL gr, double size);
void MGL_EXPORT mgl_set_arrow_size_(uintptr_t *gr, mreal *size);
/// Set default font size
void MGL_EXPORT mgl_set_font_size(HMGL gr, double size);
void MGL_EXPORT mgl_set_font_size_(uintptr_t *gr, mreal *size);
/// Set default font style and color
void MGL_EXPORT mgl_set_font_def(HMGL gr, const char *fnt);
void MGL_EXPORT mgl_set_font_def_(uintptr_t *gr, const char *fnt, int);
/// Set to use or not text rotation
void MGL_EXPORT mgl_set_rotated_text(HMGL gr, int enable);
void MGL_EXPORT mgl_set_rotated_text_(uintptr_t *gr, int *enable);
/// Set to scale text in relative subplots too
void MGL_EXPORT mgl_set_scale_text(HMGL gr, int enable);
void MGL_EXPORT mgl_set_scale_text_(uintptr_t *gr, int *enable);
/// Load font from file
void MGL_EXPORT mgl_load_font(HMGL gr, const char *name, const char *path);
void MGL_EXPORT mgl_load_font_(uintptr_t *gr, char *name, char *path, int l, int n);
/// Copy font from another mglGraph instance
void MGL_EXPORT mgl_copy_font(HMGL gr, HMGL gr_from);
void MGL_EXPORT mgl_copy_font_(uintptr_t *gr, uintptr_t *gr_from);
/// Restore font (load default font for new HMGL objects)
void MGL_EXPORT mgl_restore_font(HMGL gr);
void MGL_EXPORT mgl_restore_font_(uintptr_t *gr);
/// Add user-defined glyph for symbol and set its optional id
void MGL_EXPORT mgl_define_symbol(HMGL gr, char id, HCDT x, HCDT y);
void MGL_EXPORT mgl_define_symbol_(uintptr_t *gr, char *id, uintptr_t *x, uintptr_t *y,int);

#ifdef __cplusplus
}
#endif
//-----------------------------------------------------------------------------
#endif
