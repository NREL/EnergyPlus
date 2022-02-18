/***************************************************************************
 * wnd_cf.h is part of Math Graphic Library
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
#ifndef MGL_WND_CF_H
#define MGL_WND_CF_H
#include "mgl2/abstract.h"
//-----------------------------------------------------------------------------
#ifdef __cplusplus
extern "C" {
#endif

//-----------------------------------------------------------------------------
/// Callback function for mouse click
void MGL_EXPORT mgl_set_click_func(HMGL gr, void (*func)(void *p));
#if MGL_HAVE_PTHR_WIDGET
/// Mutex for lock/unlock by widget
void MGL_EXPORT mgl_wnd_set_mutex(HMGL gr, pthread_mutex_t *mutex);
#endif

/// Make custom dialog for parameters ids of element properties defined by args
void MGL_EXPORT mgl_wnd_make_dialog(HMGL gr, const char *ids, char const * const *args, const char *title);
/// Set callback functions for drawing and data reloading
void MGL_EXPORT mgl_wnd_set_func(HMGL gr, int (*draw)(HMGL gr, void *p), void *par, void (*reload)(void *p));
/// Set callback functions for setting properties
void MGL_EXPORT mgl_wnd_set_prop(HMGL gr, void (*prop)(char id, const char *val, void *p), void *par);
/// Set delay for animation in seconds
void MGL_EXPORT mgl_wnd_set_delay(HMGL gr, double dt);
void MGL_EXPORT mgl_wnd_set_delay_(uintptr_t *gr, mreal *dt);
/// Get delay for animation in seconds
double MGL_EXPORT_PURE mgl_wnd_get_delay(HMGL gr);
double MGL_EXPORT_PURE mgl_wnd_get_delay_(uintptr_t *gr);
/// Set window properties
void MGL_EXPORT mgl_setup_window(HMGL gr, int clf_upd, int showpos);
void MGL_EXPORT mgl_setup_window_(uintptr_t *gr, int *clf_upd, int *showpos);
/// Switch on/off transparency (do not overwrite user settings)
void MGL_EXPORT mgl_wnd_toggle_alpha(HMGL gr);
void MGL_EXPORT mgl_wnd_toggle_alpha_(uintptr_t *gr);
/// Switch on/off lighting (do not overwrite user settings)
void MGL_EXPORT mgl_wnd_toggle_light(HMGL gr);
void MGL_EXPORT mgl_wnd_toggle_light_(uintptr_t *gr);
/// Switch on/off zooming by mouse
void MGL_EXPORT mgl_wnd_toggle_zoom(HMGL gr);
void MGL_EXPORT mgl_wnd_toggle_zoom_(uintptr_t *gr);
/// Switch on/off rotation by mouse
void MGL_EXPORT mgl_wnd_toggle_rotate(HMGL gr);
void MGL_EXPORT mgl_wnd_toggle_rotate_(uintptr_t *gr);
/// Switch off all zooming and rotation
void MGL_EXPORT mgl_wnd_toggle_no(HMGL gr);
void MGL_EXPORT mgl_wnd_toggle_no_(uintptr_t *gr);
/// Update picture by calling user drawing function
void MGL_EXPORT mgl_wnd_update(HMGL gr);
void MGL_EXPORT mgl_wnd_update_(uintptr_t *gr);
/// Reload user data and update picture
void MGL_EXPORT mgl_wnd_reload(HMGL gr);
void MGL_EXPORT mgl_wnd_reload_(uintptr_t *gr);
/// Adjust size of bitmap to window size
void MGL_EXPORT mgl_wnd_adjust(HMGL gr);
void MGL_EXPORT mgl_wnd_adjust_(uintptr_t *gr);
/// Show next frame (if one)
void MGL_EXPORT mgl_wnd_next_frame(HMGL gr);
void MGL_EXPORT mgl_wnd_next_frame_(uintptr_t *gr);
/// Show previous frame (if one)
void MGL_EXPORT mgl_wnd_prev_frame(HMGL gr);
void MGL_EXPORT mgl_wnd_prev_frame_(uintptr_t *gr);
/// Run slideshow (animation) of frames
void MGL_EXPORT mgl_wnd_animation(HMGL gr);
void MGL_EXPORT mgl_wnd_animation_(uintptr_t *gr);
/// Get last mouse position
void MGL_EXPORT mgl_get_last_mouse_pos(HMGL gr, mreal *x, mreal *y, mreal *z);
void MGL_EXPORT mgl_get_last_mouse_pos_(uintptr_t *gr, mreal *x, mreal *y, mreal *z);
/// Return pointer to widget (Fl_Window* or QMainWindow*) used for plotting
MGL_EXPORT void *mgl_wnd_window(HMGL gr);
/// Return pointer to widget (Fl_MGLView* or QMathGL*) used for plotting
MGL_EXPORT void *mgl_wnd_widget(HMGL gr);
/// Move window to given position
void MGL_EXPORT mgl_wnd_move(HMGL gr, int x, int y);
void MGL_EXPORT mgl_wnd_move_(uintptr_t *gr, int *x, int *y);
/// Change window sizes
void MGL_EXPORT mgl_wnd_size(HMGL gr, int w, int h);
void MGL_EXPORT mgl_wnd_size_(uintptr_t *gr, int *w, int *h);

#ifdef __cplusplus
}
#endif
//-----------------------------------------------------------------------------
#endif
