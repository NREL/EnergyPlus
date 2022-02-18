\ Mathgl library wrapper
\ Copyright (C) 2008-2013, Sergey Plis, Alexey Balakin
\
\ This program is free software; you can redistribute it and/or modify
\ it under the terms of the GNU General Public License as published by
\ the Free Software Foundation; either version 2 of the License, or
\ (at your option) any later version.
\
\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.

Module mathgl
library libmgl      libmgl.so
library libmgl-glut libmgl-glut.so
library libmgl-fltk libmgl-fltk.so
library libmgl-qt   libmgl-qt.so
library libmgl-wx   libmgl-wx.so
legacy off

libmgl mgl_create_graph_gl		(ptr)	mgl_create_graph_gl
libmgl-glut mgl_create_graph_glut	ptr ptr ptr	(ptr)	mgl_create_graph_glut
libmgl-fltk mgl_create_graph_fltk	ptr ptr ptr	(ptr)	mgl_create_graph_fltk
libmgl-fltk mgl_fltk_run		(void)	mgl_fltk_run
libmgl-qt   mgl_create_graph_qt	ptr ptr ptr	(ptr)	mgl_create_graph_qt
libmgl-qt   mgl_qt_run			(void)	mgl_qt_run

libmgl mgl_get_warn				ptr 	(int)	mgl_get_warn
libmgl mgl_set_warn				ptr int ptr 	(void)	mgl_set_warn
libmgl gl_get_mess				ptr 	(ptr)	gl_get_mess
libmgl mgl_set_plotid			ptr ptr 	(void)	mgl_set_plotid
libmgl gl_get_plotid			ptr 	(ptr)	gl_get_plotid
libmgl mgl_get_quality			ptr 	(int)	mgl_get_quality
libmgl mgl_set_quality			int ptr 	(void)	mgl_set_quality
libmgl mgl_set_draw_reg			int int int ptr 	(void)	mgl_set_draw_reg
libmgl mgl_is_frames			ptr 	(int)	mgl_is_frames
libmgl mgl_get_flag				int ptr 	(int)	mgl_get_flag
libmgl mgl_set_flag				int int ptr 	(void)	mgl_set_flag
libmgl mgl_use_graph			int ptr 	(int)	mgl_use_graph
libmgl mgl_start_group			ptr ptr 	(void)	mgl_start_group
libmgl mgl_end_group			ptr 	(void)	mgl_end_group
libmgl mgl_highlight			int ptr 	(void)	mgl_highlight
libmgl mgl_set_palette			ptr ptr 	(void)	mgl_set_palette
libmgl mgl_set_color			double double double char 	(void)	mgl_set_color
libmgl mgl_set_def_sch			ptr ptr 	(void)	mgl_set_def_sch
libmgl mgl_set_mask				ptr char 	(void)	mgl_set_mask
libmgl mgl_set_mask_angle		int ptr 	(void)	mgl_set_mask_angle
libmgl mgl_set_alpha_default	double ptr 	(void)	mgl_set_alpha_default
libmgl mgl_set_bar_width		double ptr 	(void)	mgl_set_bar_width
libmgl mgl_set_meshnum			int ptr 	(void)	mgl_set_meshnum
libmgl mgl_set_facenum			int ptr 	(void)	mgl_set_facenum
libmgl mgl_clear_unused			ptr 	(void)	mgl_clear_unused
libmgl mgl_set_ambbr			double ptr 	(void)	mgl_set_ambbr
libmgl mgl_set_difbr			double ptr 	(void)	mgl_set_difbr
libmgl mgl_set_light_dif		int ptr 	(void)	mgl_set_light_dif
libmgl mgl_set_cut				int ptr 	(void)	mgl_set_cut
libmgl mgl_set_cut_box			double double double double double double ptr 	(void)	mgl_set_cut_box
libmgl mgl_set_cutoff			ptr ptr 	(void)	mgl_set_cutoff
libmgl mgl_set_ranges			double double double double double double ptr 	(void)	mgl_set_ranges
libmgl mgl_set_range_val		double double char ptr 	(void)	mgl_set_range_val
libmgl mgl_set_range_dat		int ptr char ptr 	(void)	mgl_set_range_dat
libmgl mgl_set_auto_ranges		double double double double double double double double ptr 	(void)	mgl_set_auto_ranges
libmgl mgl_zoom_axis			double double double double double double double double ptr 	(void)	mgl_zoom_axis
libmgl mgl_set_origin			double double double ptr 	(void)	mgl_set_origin
libmgl mgl_set_func				ptr ptr ptr ptr ptr 	(void)	mgl_set_func
libmgl mgl_set_coor				int ptr 	(void)	mgl_set_coor
libmgl mgl_set_ternary			int ptr 	(void)	mgl_set_ternary
libmgl mgl_set_tick_rotate		int ptr 	(void)	mgl_set_tick_rotate
libmgl mgl_set_tick_skip		int ptr 	(void)	mgl_set_tick_skip
libmgl mgl_def_font				ptr ptr 	(void)	mgl_def_font
libmgl mgl_set_mark_size		double ptr 	(void)	mgl_set_mark_size
libmgl mgl_set_arrow_size		double ptr 	(void)	mgl_set_arrow_size
libmgl mgl_set_font_size		double ptr 	(void)	mgl_set_font_size
libmgl mgl_set_font_def			ptr ptr 	(void)	mgl_set_font_def
libmgl mgl_set_rotated_text		int ptr 	(void)	mgl_set_rotated_text
libmgl mgl_load_font			ptr ptr ptr 	(void)	mgl_load_font
libmgl mgl_copy_font			ptr ptr 	(void)	mgl_copy_font
libmgl mgl_restore_font			ptr 	(void)	mgl_restore_font
libmgl mgl_srnd					int 	(void)	mgl_srnd
libmgl mgl_rnd						(double)	mgl_rnd
libmgl mgl_ipow					int double 	(double)	mgl_ipow
libmgl mgl_get_time				ptr ptr 	(double)	mgl_get_time
libmgl mgl_create_data				(ptr)	mgl_create_data
libmgl mgl_create_data_size		int int int 	(ptr)	mgl_create_data_size
libmgl mgl_create_data_file		ptr 	(ptr)	mgl_create_data_file
libmgl mgl_delete_data			ptr 	(void)	mgl_delete_data
libmgl gl_data_info				ptr 	(ptr)	gl_data_info
libmgl mgl_data_rearrange		int int int ptr 	(void)	mgl_data_rearrange
libmgl mgl_data_link			int int int ptr ptr 	(void)	mgl_data_link
libmgl mgl_data_set_float		int int int ptr ptr 	(void)	mgl_data_set_float
libmgl mgl_data_set_double		int int int ptr ptr 	(void)	mgl_data_set_double
libmgl mgl_data_set				ptr ptr 	(void)	mgl_data_set
libmgl mgl_data_set_vector		ptr ptr 	(void)	mgl_data_set_vector
libmgl mgl_data_set_matrix		ptr ptr 	(void)	mgl_data_set_matrix
libmgl mgl_data_set_value		int int int sf ptr 	(void)	mgl_data_set_value
libmgl mgl_data_get_value		int int int ptr 	(sf)	mgl_data_get_value
libmgl mgl_data_set_values		int int int ptr ptr 	(void)	mgl_data_set_values
libmgl mgl_data_read_hdf		ptr ptr ptr 	(int)	mgl_data_read_hdf
libmgl mgl_data_save_hdf		int ptr ptr ptr 	(void)	mgl_data_save_hdf
libmgl mgl_datas_hdf			int ptr ptr 	(int)	mgl_datas_hdf
libmgl mgl_data_read			ptr ptr 	(int)	mgl_data_read
libmgl mgl_data_read_mat		int ptr ptr 	(int)	mgl_data_read_mat
libmgl mgl_data_read_dim		int int int ptr ptr 	(int)	mgl_data_read_dim
libmgl mgl_data_read_range		int double double double ptr ptr 	(int)	mgl_data_read_range
libmgl mgl_data_read_all		int ptr ptr 	(int)	mgl_data_read_all
libmgl mgl_data_save			int ptr ptr 	(void)	mgl_data_save
libmgl mgl_data_export			int sf sf ptr ptr ptr 	(void)	mgl_data_export
libmgl mgl_data_import			sf sf ptr ptr ptr 	(void)	mgl_data_import
libmgl mgl_data_create			int int int ptr 	(void)	mgl_data_create
libmgl mgl_data_transpose		ptr ptr 	(void)	mgl_data_transpose
libmgl mgl_data_norm			int int sf sf ptr 	(void)	mgl_data_norm
libmgl mgl_data_norm_slice		int int char sf sf ptr 	(void)	mgl_data_norm_slice
libmgl mgl_data_subdata			int int int ptr 	(ptr)	mgl_data_subdata
libmgl mgl_data_subdata_ext		ptr ptr ptr ptr 	(ptr)	mgl_data_subdata_ext
libmgl mgl_data_column			ptr ptr 	(ptr)	mgl_data_column
libmgl mgl_data_set_id			ptr ptr 	(void)	mgl_data_set_id
libmgl mgl_data_fill			char sf sf ptr 	(void)	mgl_data_fill
libmgl mgl_data_fill_eq			ptr ptr ptr ptr ptr ptr 	(void)	mgl_data_fill_eq
libmgl mgl_data_refill_x		int sf sf ptr ptr ptr 	(void)	mgl_data_refill_x
libmgl mgl_data_refill_xy		int sf sf sf sf ptr ptr ptr ptr 	(void)	mgl_data_refill_xy
libmgl mgl_data_refill_xyz		sf sf sf sf sf sf ptr ptr ptr ptr ptr 	(void)	mgl_data_refill_xyz
libmgl mgl_data_refill_gr		ptr int ptr ptr ptr ptr ptr ptr 	(void)	mgl_data_refill_gr
libmgl mgl_data_grid			ptr ptr ptr ptr ptr ptr 	(void)	mgl_data_grid
libmgl mgl_data_grid_xy			sf sf sf sf ptr ptr ptr ptr 	(void)	mgl_data_grid_xy
libmgl mgl_data_put_val			int int int sf ptr 	(void)	mgl_data_put_val
libmgl mgl_data_put_dat			int int int ptr ptr 	(void)	mgl_data_put_dat
libmgl mgl_data_modify			int ptr ptr 	(void)	mgl_data_modify
libmgl mgl_data_modify_vw		ptr ptr ptr ptr 	(void)	mgl_data_modify_vw
libmgl mgl_data_squeeze			int int int int ptr 	(void)	mgl_data_squeeze
libmgl mgl_data_max				ptr 	(sf)	mgl_data_max
libmgl mgl_data_min				ptr 	(sf)	mgl_data_min
libmgl gl_data_value			int int int ptr 	(ptr)	gl_data_value
libmgl gl_data_data				ptr 	(ptr)	gl_data_data
libmgl mgl_data_get_nx			ptr 	(int)	mgl_data_get_nx
libmgl mgl_data_get_ny			ptr 	(int)	mgl_data_get_ny
libmgl mgl_data_get_nz			ptr 	(int)	mgl_data_get_nz
libmgl mgl_data_first			ptr ptr ptr ptr ptr 	(sf)	mgl_data_first
libmgl mgl_data_last			ptr ptr ptr ptr ptr 	(sf)	mgl_data_last
libmgl mgl_data_find			int int int char ptr ptr 	(int)	mgl_data_find
libmgl mgl_data_find_any		ptr ptr 	(int)	mgl_data_find_any
libmgl mgl_data_max_int			ptr ptr ptr ptr 	(sf)	mgl_data_max_int
libmgl mgl_data_max_real		ptr ptr ptr ptr 	(sf)	mgl_data_max_real
libmgl mgl_data_min_int			ptr ptr ptr ptr 	(sf)	mgl_data_min_int
libmgl mgl_data_min_real		ptr ptr ptr ptr 	(sf)	mgl_data_min_real
libmgl mgl_data_momentum_val	ptr ptr ptr ptr char ptr 	(sf)	mgl_data_momentum_val
libmgl mgl_data_combine			ptr ptr 	(ptr)	mgl_data_combine
libmgl mgl_data_extend			int int ptr 	(void)	mgl_data_extend
libmgl mgl_data_insert			int int char ptr 	(void)	mgl_data_insert
libmgl mgl_data_delete			int int char ptr 	(void)	mgl_data_delete
libmgl mgl_data_join			ptr ptr 	(void)	mgl_data_join
libmgl mgl_data_smooth			sf ptr ptr 	(void)	mgl_data_smooth
libmgl mgl_data_sum				ptr ptr 	(ptr)	mgl_data_sum
libmgl mgl_data_max_dir			ptr ptr 	(ptr)	mgl_data_max_dir
libmgl mgl_data_min_dir			ptr ptr 	(ptr)	mgl_data_min_dir
libmgl mgl_data_cumsum			ptr ptr 	(void)	mgl_data_cumsum
libmgl mgl_data_integral		ptr ptr 	(void)	mgl_data_integral
libmgl mgl_data_diff			ptr ptr 	(void)	mgl_data_diff
libmgl mgl_data_diff_par		ptr ptr ptr ptr 	(void)	mgl_data_diff_par
libmgl mgl_data_diff2			ptr ptr 	(void)	mgl_data_diff2
libmgl mgl_data_swap			ptr ptr 	(void)	mgl_data_swap
libmgl mgl_data_roll			int char ptr 	(void)	mgl_data_roll
libmgl mgl_data_mirror			ptr ptr 	(void)	mgl_data_mirror
libmgl mgl_data_sort			int int ptr 	(void)	mgl_data_sort
libmgl mgl_data_hankel			ptr ptr 	(void)	mgl_data_hankel
libmgl mgl_data_sinfft			ptr ptr 	(void)	mgl_data_sinfft
libmgl mgl_data_cosfft			ptr ptr 	(void)	mgl_data_cosfft
libmgl mgl_data_fill_sample		ptr ptr 	(void)	mgl_data_fill_sample
libmgl mgl_data_correl			ptr ptr ptr 	(ptr)	mgl_data_correl
libmgl mgl_clear_fft				(void)	mgl_clear_fft
libmgl mgl_data_spline			sf sf sf ptr 	(sf)	mgl_data_spline
libmgl mgl_data_linear			sf sf sf ptr 	(sf)	mgl_data_linear
libmgl mgl_data_spline_ext		ptr ptr ptr sf sf sf ptr 	(sf)	mgl_data_spline_ext
libmgl mgl_data_linear_ext		ptr ptr ptr sf sf sf ptr 	(sf)	mgl_data_linear_ext
libmgl mgl_data_solve_1d		int int sf ptr 	(sf)	mgl_data_solve_1d
libmgl mgl_data_solve			int ptr char sf ptr 	(ptr)	mgl_data_solve
libmgl mgl_data_trace			ptr 	(ptr)	mgl_data_trace
libmgl mgl_data_resize			int int int ptr 	(ptr)	mgl_data_resize
libmgl mgl_data_resize_box		sf sf sf sf sf sf int int int ptr 	(ptr)	mgl_data_resize_box
libmgl mgl_data_hist			int sf sf int ptr 	(ptr)	mgl_data_hist
libmgl mgl_data_hist_w			int sf sf int ptr ptr 	(ptr)	mgl_data_hist_w
libmgl mgl_data_momentum		ptr char ptr 	(ptr)	mgl_data_momentum
libmgl mgl_data_evaluate		int ptr ptr ptr ptr 	(ptr)	mgl_data_evaluate
libmgl mgl_data_envelop			char ptr 	(void)	mgl_data_envelop
libmgl mgl_data_sew				sf ptr ptr 	(void)	mgl_data_sew
libmgl mgl_data_crop			char int int ptr 	(void)	mgl_data_crop
libmgl mgl_data_clean			int ptr 	(void)	mgl_data_clean
libmgl mgl_data_mul_dat			ptr ptr 	(void)	mgl_data_mul_dat
libmgl mgl_data_div_dat			ptr ptr 	(void)	mgl_data_div_dat
libmgl mgl_data_add_dat			ptr ptr 	(void)	mgl_data_add_dat
libmgl mgl_data_sub_dat			ptr ptr 	(void)	mgl_data_sub_dat
libmgl mgl_data_mul_num			sf ptr 	(void)	mgl_data_mul_num
libmgl mgl_data_div_num			sf ptr 	(void)	mgl_data_div_num
libmgl mgl_data_add_num			sf ptr 	(void)	mgl_data_add_num
libmgl mgl_data_sub_num			sf ptr 	(void)	mgl_data_sub_num
libmgl mgl_transform_a			ptr ptr ptr 	(ptr)	mgl_transform_a
libmgl mgl_transform			ptr ptr ptr 	(ptr)	mgl_transform
libmgl mgl_data_fourier			ptr ptr ptr 	(void)	mgl_data_fourier
libmgl mgl_data_stfa			char int ptr ptr 	(ptr)	mgl_data_stfa
libmgl mgl_triangulation_3d		ptr ptr ptr 	(ptr)	mgl_triangulation_3d
libmgl mgl_triangulation_2d		ptr ptr 	(ptr)	mgl_triangulation_2d
libmgl mgl_find_root_txt		char sf ptr 	(sf)	mgl_find_root_txt
libmgl mgl_data_roots			char ptr ptr 	(ptr)	mgl_data_roots
libmgl mgl_datac_save			int ptr ptr 	(void)	mgl_datac_save
libmgl mgl_datac_save_hdf		int ptr ptr ptr 	(void)	mgl_datac_save_hdf
libmgl mgl_datac_real			ptr 	(ptr)	mgl_datac_real
libmgl mgl_datac_imag			ptr 	(ptr)	mgl_datac_imag
libmgl mgl_datac_abs			ptr 	(ptr)	mgl_datac_abs
libmgl mgl_datac_arg			ptr 	(ptr)	mgl_datac_arg
libmgl mgl_text_xyz				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_text_xyz
libmgl mgl_textw_xyz			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_textw_xyz
libmgl mgl_text_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_text_xy
libmgl mgl_textw_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_textw_xy
libmgl mgl_text_y				ptr ptr ptr ptr ptr 	(void)	mgl_text_y
libmgl mgl_textw_y				ptr ptr ptr ptr ptr 	(void)	mgl_textw_y
libmgl mgl_cont_gen				ptr ptr ptr ptr ptr double ptr 	(void)	mgl_cont_gen
libmgl mgl_contf_gen			ptr ptr ptr ptr ptr double double ptr 	(void)	mgl_contf_gen
libmgl mgl_cont_xy_val			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_cont_xy_val
libmgl mgl_cont_val				ptr ptr ptr ptr ptr 	(void)	mgl_cont_val
libmgl mgl_cont_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_cont_xy
libmgl mgl_cont					ptr ptr ptr ptr 	(void)	mgl_cont
libmgl mgl_contf_xy_val			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_contf_xy_val
libmgl mgl_contf_val			ptr ptr ptr ptr ptr 	(void)	mgl_contf_val
libmgl mgl_contf_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_contf_xy
libmgl mgl_contf				ptr ptr ptr ptr 	(void)	mgl_contf
libmgl mgl_contd_xy_val			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_contd_xy_val
libmgl mgl_contd_val			ptr ptr ptr ptr ptr 	(void)	mgl_contd_val
libmgl mgl_contd_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_contd_xy
libmgl mgl_contd				ptr ptr ptr ptr 	(void)	mgl_contd
libmgl mgl_contv_xy_val			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_contv_xy_val
libmgl mgl_contv_val			ptr ptr ptr ptr ptr 	(void)	mgl_contv_val
libmgl mgl_contv_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_contv_xy
libmgl mgl_contv				ptr ptr ptr ptr 	(void)	mgl_contv
libmgl mgl_axial_xy_val			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_axial_xy_val
libmgl mgl_axial_val			ptr ptr ptr ptr ptr 	(void)	mgl_axial_val
libmgl mgl_axial_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_axial_xy
libmgl mgl_axial				ptr ptr ptr ptr 	(void)	mgl_axial
libmgl mgl_torus				ptr ptr ptr ptr ptr 	(void)	mgl_torus
libmgl mgl_grid3_xyz			ptr double ptr ptr ptr ptr ptr ptr 	(void)	mgl_grid3_xyz
libmgl mgl_grid3				ptr double ptr ptr ptr 	(void)	mgl_grid3
libmgl mgl_dens3_xyz			ptr double ptr ptr ptr ptr ptr ptr 	(void)	mgl_dens3_xyz
libmgl mgl_dens3				ptr double ptr ptr ptr 	(void)	mgl_dens3
libmgl mgl_cont3_xyz_val		ptr double ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_cont3_xyz_val
libmgl mgl_cont3_val			ptr double ptr ptr ptr ptr 	(void)	mgl_cont3_val
libmgl mgl_cont3_xyz			ptr double ptr ptr ptr ptr ptr ptr 	(void)	mgl_cont3_xyz
libmgl mgl_cont3				ptr double ptr ptr ptr 	(void)	mgl_cont3
libmgl mgl_contf3_xyz_val		ptr double ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_contf3_xyz_val
libmgl mgl_contf3_val			ptr double ptr ptr ptr ptr 	(void)	mgl_contf3_val
libmgl mgl_contf3_xyz			ptr double ptr ptr ptr ptr ptr ptr 	(void)	mgl_contf3_xyz
libmgl mgl_contf3				ptr double ptr ptr ptr 	(void)	mgl_contf3
libmgl mgl_fit_1				ptr ptr ptr ptr ptr ptr 	(ptr)	mgl_fit_1
libmgl mgl_fit_2				ptr ptr ptr ptr ptr ptr 	(ptr)	mgl_fit_2
libmgl mgl_fit_3				ptr ptr ptr ptr ptr ptr 	(ptr)	mgl_fit_3
libmgl mgl_fit_xy				ptr ptr ptr ptr ptr ptr ptr 	(ptr)	mgl_fit_xy
libmgl mgl_fit_xyz				ptr ptr ptr ptr ptr ptr ptr ptr 	(ptr)	mgl_fit_xyz
libmgl mgl_fit_xyza				ptr ptr ptr ptr ptr ptr ptr ptr ptr 	(ptr)	mgl_fit_xyza
libmgl mgl_fit_ys				ptr ptr ptr ptr ptr ptr ptr 	(ptr)	mgl_fit_ys
libmgl mgl_fit_xys				ptr ptr ptr ptr ptr ptr ptr ptr 	(ptr)	mgl_fit_xys
libmgl mgl_fit_xyzs				ptr ptr ptr ptr ptr ptr ptr ptr ptr 	(ptr)	mgl_fit_xyzs
libmgl mgl_fit_xyzas			ptr ptr ptr ptr ptr ptr ptr ptr ptr ptr 	(ptr)	mgl_fit_xyzas
libmgl gl_get_fit				ptr 	(ptr)	gl_get_fit
libmgl mgl_hist_x				ptr ptr ptr ptr 	(ptr)	mgl_hist_x
libmgl mgl_hist_xy				ptr ptr ptr ptr ptr 	(ptr)	mgl_hist_xy
libmgl mgl_hist_xyz				ptr ptr ptr ptr ptr ptr 	(ptr)	mgl_hist_xyz
libmgl mgl_puts_fit				double ptr ptr double double double ptr 	(void)	mgl_puts_fit
libmgl mgl_fplot				ptr ptr ptr ptr 	(void)	mgl_fplot
libmgl mgl_fplot_xyz			ptr ptr ptr ptr ptr ptr 	(void)	mgl_fplot_xyz
libmgl mgl_radar				ptr ptr ptr ptr 	(void)	mgl_radar
libmgl mgl_plot_xyz				ptr ptr ptr ptr ptr ptr 	(void)	mgl_plot_xyz
libmgl mgl_plot_xy				ptr ptr ptr ptr ptr 	(void)	mgl_plot_xy
libmgl mgl_plot					ptr ptr ptr ptr 	(void)	mgl_plot
libmgl mgl_tens_xyz				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_tens_xyz
libmgl mgl_tens_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_tens_xy
libmgl mgl_tens					ptr ptr ptr ptr ptr 	(void)	mgl_tens
libmgl mgl_tape_xyz				ptr ptr ptr ptr ptr ptr 	(void)	mgl_tape_xyz
libmgl mgl_tape_xy				ptr ptr ptr ptr ptr 	(void)	mgl_tape_xy
libmgl mgl_tape					ptr ptr ptr ptr 	(void)	mgl_tape
libmgl mgl_boxplot_xy			ptr ptr ptr ptr ptr 	(void)	mgl_boxplot_xy
libmgl mgl_boxplot				ptr ptr ptr ptr 	(void)	mgl_boxplot
libmgl mgl_area_xyz				ptr ptr ptr ptr ptr ptr 	(void)	mgl_area_xyz
libmgl mgl_area_xy				ptr ptr ptr ptr ptr 	(void)	mgl_area_xy
libmgl mgl_area					ptr ptr ptr ptr 	(void)	mgl_area
libmgl mgl_region_xy			ptr ptr ptr ptr ptr ptr 	(void)	mgl_region_xy
libmgl mgl_region				ptr ptr ptr ptr ptr 	(void)	mgl_region
libmgl mgl_stem_xyz				ptr ptr ptr ptr ptr ptr 	(void)	mgl_stem_xyz
libmgl mgl_stem_xy				ptr ptr ptr ptr ptr 	(void)	mgl_stem_xy
libmgl mgl_stem					ptr ptr ptr ptr 	(void)	mgl_stem
libmgl mgl_step_xyz				ptr ptr ptr ptr ptr ptr 	(void)	mgl_step_xyz
libmgl mgl_step_xy				ptr ptr ptr ptr ptr 	(void)	mgl_step_xy
libmgl mgl_step					ptr ptr ptr ptr 	(void)	mgl_step
libmgl mgl_bars_xyz				ptr ptr ptr ptr ptr ptr 	(void)	mgl_bars_xyz
libmgl mgl_bars_xy				ptr ptr ptr ptr ptr 	(void)	mgl_bars_xy
libmgl mgl_bars					ptr ptr ptr ptr 	(void)	mgl_bars
libmgl mgl_barh_yx				ptr ptr ptr ptr ptr 	(void)	mgl_barh_yx
libmgl mgl_barh					ptr ptr ptr ptr 	(void)	mgl_barh
libmgl mgl_ohlc_x				ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_ohlc_x
libmgl mgl_ohlc					ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_ohlc
libmgl mgl_chart				ptr ptr ptr ptr 	(void)	mgl_chart
libmgl mgl_error_exy			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_error_exy
libmgl mgl_error_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_error_xy
libmgl mgl_error				ptr ptr ptr ptr ptr 	(void)	mgl_error
libmgl mgl_mark_xyz				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_mark_xyz
libmgl mgl_mark_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_mark_xy
libmgl mgl_mark_y				ptr ptr ptr ptr ptr 	(void)	mgl_mark_y
libmgl mgl_tube_xyzr			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_tube_xyzr
libmgl mgl_tube_xyr				ptr ptr ptr ptr ptr ptr 	(void)	mgl_tube_xyr
libmgl mgl_tube_r				ptr ptr ptr ptr ptr 	(void)	mgl_tube_r
libmgl mgl_tube_xyz				ptr ptr double ptr ptr ptr ptr 	(void)	mgl_tube_xyz
libmgl mgl_tube_xy				ptr ptr double ptr ptr ptr 	(void)	mgl_tube_xy
libmgl mgl_tube					ptr ptr double ptr ptr 	(void)	mgl_tube
libmgl mgl_candle_xyv			ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_candle_xyv
libmgl mgl_candle_yv			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_candle_yv
libmgl mgl_candle				ptr ptr ptr ptr ptr ptr 	(void)	mgl_candle
libmgl mgl_fsurf				ptr ptr ptr ptr 	(void)	mgl_fsurf
libmgl mgl_fsurf_xyz			ptr ptr ptr ptr ptr ptr 	(void)	mgl_fsurf_xyz
libmgl mgl_grid_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_grid_xy
libmgl mgl_grid					ptr ptr ptr ptr 	(void)	mgl_grid
libmgl mgl_mesh_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_mesh_xy
libmgl mgl_mesh					ptr ptr ptr ptr 	(void)	mgl_mesh
libmgl mgl_fall_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_fall_xy
libmgl mgl_fall					ptr ptr ptr ptr 	(void)	mgl_fall
libmgl mgl_belt_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_belt_xy
libmgl mgl_belt					ptr ptr ptr ptr 	(void)	mgl_belt
libmgl mgl_surf_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_surf_xy
libmgl mgl_surf					ptr ptr ptr ptr 	(void)	mgl_surf
libmgl mgl_dens_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_dens_xy
libmgl mgl_dens					ptr ptr ptr ptr 	(void)	mgl_dens
libmgl mgl_boxs_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_boxs_xy
libmgl mgl_boxs					ptr ptr ptr ptr 	(void)	mgl_boxs
libmgl mgl_tile_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_tile_xy
libmgl mgl_tile					ptr ptr ptr ptr 	(void)	mgl_tile
libmgl mgl_tiles_xy				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_tiles_xy
libmgl mgl_tiles				ptr ptr ptr ptr ptr 	(void)	mgl_tiles
libmgl mgl_surfc_xy				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_surfc_xy
libmgl mgl_surfc				ptr ptr ptr ptr ptr 	(void)	mgl_surfc
libmgl mgl_surfa_xy				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_surfa_xy
libmgl mgl_surfa				ptr ptr ptr ptr ptr 	(void)	mgl_surfa
libmgl mgl_stfa_xy				ptr ptr int ptr ptr ptr ptr ptr 	(void)	mgl_stfa_xy
libmgl mgl_stfa					ptr ptr int ptr ptr ptr 	(void)	mgl_stfa
libmgl mgl_map_xy				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_map_xy
libmgl mgl_map					ptr ptr ptr ptr ptr 	(void)	mgl_map
libmgl mgl_surf3_xyz_val		ptr ptr ptr ptr ptr ptr double ptr 	(void)	mgl_surf3_xyz_val
libmgl mgl_surf3_val			ptr ptr ptr double ptr 	(void)	mgl_surf3_val
libmgl mgl_surf3_xyz			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_surf3_xyz
libmgl mgl_surf3				ptr ptr ptr ptr 	(void)	mgl_surf3
libmgl mgl_surf3a_xyz_val		ptr ptr ptr ptr ptr ptr ptr double ptr 	(void)	mgl_surf3a_xyz_val
libmgl mgl_surf3a_val			ptr ptr ptr ptr double ptr 	(void)	mgl_surf3a_val
libmgl mgl_surf3a_xyz			ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_surf3a_xyz
libmgl mgl_surf3a				ptr ptr ptr ptr ptr 	(void)	mgl_surf3a
libmgl mgl_surf3c_xyz_val		ptr ptr ptr ptr ptr ptr ptr double ptr 	(void)	mgl_surf3c_xyz_val
libmgl mgl_surf3c_val			ptr ptr ptr ptr double ptr 	(void)	mgl_surf3c_val
libmgl mgl_surf3c_xyz			ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_surf3c_xyz
libmgl mgl_surf3c				ptr ptr ptr ptr ptr 	(void)	mgl_surf3c
libmgl mgl_cloud_xyz			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_cloud_xyz
libmgl mgl_cloud				ptr ptr ptr ptr 	(void)	mgl_cloud
libmgl mgl_beam_val				int ptr double ptr ptr ptr ptr double ptr 	(void)	mgl_beam_val
libmgl mgl_beam					int int ptr double ptr ptr ptr ptr ptr 	(void)	mgl_beam
libmgl mgl_traj_xy				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_traj_xy
libmgl mgl_traj_xyz				ptr ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_traj_xyz
libmgl mgl_vect_xy				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_vect_xy
libmgl mgl_vect_2d				ptr ptr ptr ptr ptr 	(void)	mgl_vect_2d
libmgl mgl_vect_xyz				ptr ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_vect_xyz
libmgl mgl_vect_3d				ptr ptr ptr ptr ptr ptr 	(void)	mgl_vect_3d
libmgl mgl_flow_xy				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_flow_xy
libmgl mgl_flow_2d				ptr ptr ptr ptr ptr 	(void)	mgl_flow_2d
libmgl mgl_flow_xyz				ptr ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_flow_xyz
libmgl mgl_flow_3d				ptr ptr ptr ptr ptr ptr 	(void)	mgl_flow_3d
libmgl mgl_flowp_xy				ptr ptr ptr ptr ptr ptr double double double ptr 	(void)	mgl_flowp_xy
libmgl mgl_flowp_2d				ptr ptr ptr ptr double double double ptr 	(void)	mgl_flowp_2d
libmgl mgl_flowp_xyz			ptr ptr ptr ptr ptr ptr ptr ptr double double double ptr 	(void)	mgl_flowp_xyz
libmgl mgl_flowp_3d				ptr ptr ptr ptr ptr double double double ptr 	(void)	mgl_flowp_3d
libmgl mgl_pipe_xy				ptr double ptr ptr ptr ptr ptr ptr 	(void)	mgl_pipe_xy
libmgl mgl_pipe_2d				ptr double ptr ptr ptr ptr 	(void)	mgl_pipe_2d
libmgl mgl_pipe_xyz				ptr double ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_pipe_xyz
libmgl mgl_pipe_3d				ptr double ptr ptr ptr ptr ptr 	(void)	mgl_pipe_3d
libmgl mgl_grad_xyz				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_grad_xyz
libmgl mgl_grad_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_grad_xy
libmgl mgl_grad					ptr ptr ptr ptr 	(void)	mgl_grad
libmgl mgl_vect3_xyz			ptr double ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_vect3_xyz
libmgl mgl_vect3				ptr double ptr ptr ptr ptr ptr 	(void)	mgl_vect3
libmgl mgl_mark					ptr double double double ptr 	(void)	mgl_mark
libmgl mgl_ball					double double double ptr 	(void)	mgl_ball
libmgl mgl_line					int ptr double double double double double double ptr 	(void)	mgl_line
libmgl mgl_curve				int ptr double double double double double double double double double double double double ptr 	(void)	mgl_curve
libmgl mgl_error_box			ptr double double double double double double ptr 	(void)	mgl_error_box
libmgl mgl_face					ptr double double double double double double double double double double double double ptr 	(void)	mgl_face
libmgl mgl_facex				double double ptr double double double double double ptr 	(void)	mgl_facex
libmgl mgl_facey				double double ptr double double double double double ptr 	(void)	mgl_facey
libmgl mgl_facez				double double ptr double double double double double ptr 	(void)	mgl_facez
libmgl mgl_sphere				ptr double double double double ptr 	(void)	mgl_sphere
libmgl mgl_drop					double double ptr double double double double double double double ptr 	(void)	mgl_drop
libmgl mgl_cone					ptr double double double double double double double double ptr 	(void)	mgl_cone
libmgl mgl_ellipse				ptr double double double double double double double ptr 	(void)	mgl_ellipse
libmgl mgl_rhomb				ptr double double double double double double double ptr 	(void)	mgl_rhomb
libmgl mgl_cones_xyz			ptr ptr ptr ptr ptr ptr 	(void)	mgl_cones_xyz
libmgl mgl_cones_xz				ptr ptr ptr ptr ptr 	(void)	mgl_cones_xz
libmgl mgl_cones				ptr ptr ptr ptr 	(void)	mgl_cones
libmgl mgl_dew_xy				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_dew_xy
libmgl mgl_dew_2d				ptr ptr ptr ptr ptr 	(void)	mgl_dew_2d
libmgl mgl_puts_dir				double ptr ptr double double double double double double ptr 	(void)	mgl_puts_dir
libmgl mgl_putsw_dir			double ptr ptr double double double double double double ptr 	(void)	mgl_putsw_dir
libmgl mgl_textmark_xyzr		ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_textmark_xyzr
libmgl mgl_textmarkw_xyzr		ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_textmarkw_xyzr
libmgl mgl_textmark_xyr			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_textmark_xyr
libmgl mgl_textmarkw_xyr		ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_textmarkw_xyr
libmgl mgl_textmark_yr			ptr ptr ptr ptr ptr ptr 	(void)	mgl_textmark_yr
libmgl mgl_textmarkw_yr			ptr ptr ptr ptr ptr ptr 	(void)	mgl_textmarkw_yr
libmgl mgl_textmark				ptr ptr ptr ptr ptr 	(void)	mgl_textmark
libmgl mgl_textmarkw			ptr ptr ptr ptr ptr 	(void)	mgl_textmarkw
libmgl mgl_label_xyz			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_label_xyz
libmgl mgl_labelw_xyz			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_labelw_xyz
libmgl mgl_label_xy				ptr ptr ptr ptr ptr ptr 	(void)	mgl_label_xy
libmgl mgl_labelw_xy			ptr ptr ptr ptr ptr ptr 	(void)	mgl_labelw_xy
libmgl mgl_label_y				ptr ptr ptr ptr ptr 	(void)	mgl_label_y
libmgl mgl_labelw_y				ptr ptr ptr ptr ptr 	(void)	mgl_labelw_y
libmgl mgl_table				ptr ptr ptr ptr double double ptr 	(void)	mgl_table
libmgl mgl_tablew				ptr ptr ptr ptr double double ptr 	(void)	mgl_tablew
libmgl mgl_triplot_xyzc			ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_triplot_xyzc
libmgl mgl_triplot_xyz			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_triplot_xyz
libmgl mgl_triplot_xy			ptr ptr ptr ptr ptr ptr 	(void)	mgl_triplot_xy
libmgl mgl_quadplot_xyzc		ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_quadplot_xyzc
libmgl mgl_quadplot_xyz			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_quadplot_xyz
libmgl mgl_quadplot_xy			ptr ptr ptr ptr ptr ptr 	(void)	mgl_quadplot_xy
libmgl mgl_tricont_xyzcv		ptr ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_tricont_xyzcv
libmgl mgl_tricont_xycv			ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_tricont_xycv
libmgl mgl_tricont_xyzc			ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_tricont_xyzc
libmgl mgl_tricont_xyc			ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_tricont_xyc
libmgl mgl_dots					ptr ptr ptr ptr ptr ptr 	(void)	mgl_dots
libmgl mgl_dots_a				ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_dots_a
libmgl mgl_dots_ca				ptr ptr ptr ptr ptr ptr ptr ptr 	(void)	mgl_dots_ca
libmgl mgl_crust				ptr ptr ptr ptr ptr ptr 	(void)	mgl_crust
libmgl mgl_dens_x				ptr double ptr ptr ptr 	(void)	mgl_dens_x
libmgl mgl_dens_y				ptr double ptr ptr ptr 	(void)	mgl_dens_y
libmgl mgl_dens_z				ptr double ptr ptr ptr 	(void)	mgl_dens_z
libmgl mgl_cont_x				ptr double ptr ptr ptr 	(void)	mgl_cont_x
libmgl mgl_cont_y				ptr double ptr ptr ptr 	(void)	mgl_cont_y
libmgl mgl_cont_z				ptr double ptr ptr ptr 	(void)	mgl_cont_z
libmgl mgl_cont_x_val			ptr double ptr ptr ptr ptr 	(void)	mgl_cont_x_val
libmgl mgl_cont_y_val			ptr double ptr ptr ptr ptr 	(void)	mgl_cont_y_val
libmgl mgl_cont_z_val			ptr double ptr ptr ptr ptr 	(void)	mgl_cont_z_val
libmgl mgl_contf_x				ptr double ptr ptr ptr 	(void)	mgl_contf_x
libmgl mgl_contf_y				ptr double ptr ptr ptr 	(void)	mgl_contf_y
libmgl mgl_contf_z				ptr double ptr ptr ptr 	(void)	mgl_contf_z
libmgl mgl_contf_x_val			ptr double ptr ptr ptr ptr 	(void)	mgl_contf_x_val
libmgl mgl_contf_y_val			ptr double ptr ptr ptr ptr 	(void)	mgl_contf_y_val
libmgl mgl_contf_z_val			ptr double ptr ptr ptr ptr 	(void)	mgl_contf_z_val
libmgl mgl_create_graph			int int 	(ptr)	mgl_create_graph
libmgl mgl_delete_graph			ptr 	(void)	mgl_delete_graph
libmgl mgl_set_size				int int ptr 	(void)	mgl_set_size
libmgl mgl_set_def_param		ptr 	(void)	mgl_set_def_param
libmgl mgl_combine_gr			ptr ptr 	(void)	mgl_combine_gr
libmgl mgl_finish				ptr 	(void)	mgl_finish
libmgl mgl_set_tick_len			double double ptr 	(void)	mgl_set_tick_len
libmgl mgl_set_axis_stl			ptr ptr ptr ptr 	(void)	mgl_set_axis_stl
libmgl mgl_adjust_ticks			ptr ptr 	(void)	mgl_adjust_ticks
libmgl mgl_set_ticks			double int double char ptr 	(void)	mgl_set_ticks
libmgl mgl_set_ticks_str		int ptr char ptr 	(void)	mgl_set_ticks_str
libmgl mgl_set_ticks_wcs		int ptr char ptr 	(void)	mgl_set_ticks_wcs
libmgl mgl_set_ticks_val		int ptr ptr char ptr 	(void)	mgl_set_ticks_val
libmgl mgl_set_ticks_valw		int ptr ptr char ptr 	(void)	mgl_set_ticks_valw
libmgl mgl_tune_ticks			double int ptr 	(void)	mgl_tune_ticks
libmgl mgl_set_tick_templ		ptr char ptr 	(void)	mgl_set_tick_templ
libmgl mgl_set_tick_templw		ptr char ptr 	(void)	mgl_set_tick_templw
libmgl mgl_set_ticks_time		ptr double char ptr 	(void)	mgl_set_ticks_time
libmgl mgl_set_tick_shift		double double double double ptr 	(void)	mgl_set_tick_shift
libmgl mgl_box					ptr 	(void)	mgl_box
libmgl mgl_box_str				int ptr ptr 	(void)	mgl_box_str
libmgl mgl_axis					ptr ptr ptr ptr 	(void)	mgl_axis
libmgl mgl_axis_grid			ptr ptr ptr ptr 	(void)	mgl_axis_grid
libmgl mgl_label				ptr double ptr char ptr 	(void)	mgl_label
libmgl mgl_labelw				ptr double ptr char ptr 	(void)	mgl_labelw
libmgl mgl_colorbar				ptr ptr 	(void)	mgl_colorbar
libmgl mgl_colorbar_ext			double double double double ptr ptr 	(void)	mgl_colorbar_ext
libmgl mgl_colorbar_val			ptr ptr ptr 	(void)	mgl_colorbar_val
libmgl mgl_colorbar_val_ext		double double double double ptr ptr ptr 	(void)	mgl_colorbar_val_ext
libmgl mgl_add_legend			ptr ptr ptr 	(void)	mgl_add_legend
libmgl mgl_add_legendw			ptr ptr ptr 	(void)	mgl_add_legendw
libmgl mgl_clear_legend			ptr 	(void)	mgl_clear_legend
libmgl mgl_legend_pos			ptr ptr double double ptr 	(void)	mgl_legend_pos
libmgl mgl_legend				ptr ptr int ptr 	(void)	mgl_legend
libmgl mgl_set_legend_marks		int ptr 	(void)	mgl_set_legend_marks
libmgl mgl_show_image			int ptr ptr 	(void)	mgl_show_image
libmgl mgl_write_frame			ptr ptr ptr 	(void)	mgl_write_frame
libmgl mgl_write_tga			ptr ptr ptr 	(void)	mgl_write_tga
libmgl mgl_write_bmp			ptr ptr ptr 	(void)	mgl_write_bmp
libmgl mgl_write_jpg			ptr ptr ptr 	(void)	mgl_write_jpg
libmgl mgl_write_png			ptr ptr ptr 	(void)	mgl_write_png
libmgl mgl_write_png_solid		ptr ptr ptr 	(void)	mgl_write_png_solid
libmgl mgl_write_bps			ptr ptr ptr 	(void)	mgl_write_bps
libmgl mgl_write_eps			ptr ptr ptr 	(void)	mgl_write_eps
libmgl mgl_write_svg			ptr ptr ptr 	(void)	mgl_write_svg
libmgl mgl_write_tex			ptr ptr ptr 	(void)	mgl_write_tex
libmgl mgl_write_obj			int ptr ptr ptr 	(void)	mgl_write_obj
libmgl mgl_write_obj_old		int ptr ptr ptr 	(void)	mgl_write_obj_old
libmgl mgl_write_stl			ptr ptr ptr 	(void)	mgl_write_stl
libmgl mgl_write_off			int ptr ptr ptr 	(void)	mgl_write_off
libmgl mgl_write_xyz			ptr ptr ptr 	(void)	mgl_write_xyz
libmgl mgl_write_prc			int ptr ptr ptr 	(void)	mgl_write_prc
libmgl mgl_write_gif			ptr ptr ptr 	(void)	mgl_write_gif
libmgl mgl_start_gif			int ptr ptr 	(void)	mgl_start_gif
libmgl mgl_close_gif			ptr 	(void)	mgl_close_gif
libmgl mgl_export_mgld			ptr ptr ptr 	(void)	mgl_export_mgld
libmgl mgl_import_mgld			int ptr ptr 	(void)	mgl_import_mgld
libmgl mgl_write_json			ptr ptr ptr 	(void)	mgl_write_json
libmgl mgl_write_json_z			ptr ptr ptr 	(void)	mgl_write_json_z
libmgl gl_get_json				ptr 	(ptr)	gl_get_json
libmgl gl_get_rgb				ptr 	(ptr)	gl_get_rgb
libmgl gl_get_rgba				ptr 	(ptr)	gl_get_rgba
libmgl mgl_set_obj_id			int ptr 	(void)	mgl_set_obj_id
libmgl mgl_get_obj_id			int int ptr 	(int)	mgl_get_obj_id
libmgl mgl_get_spl_id			int int ptr 	(int)	mgl_get_spl_id
libmgl mgl_get_width			ptr 	(int)	mgl_get_width
libmgl mgl_get_height			ptr 	(int)	mgl_get_height
libmgl mgl_calc_xyz				ptr ptr ptr int int ptr 	(void)	mgl_calc_xyz
libmgl mgl_calc_scr				ptr ptr double double double ptr 	(void)	mgl_calc_scr
libmgl mgl_is_active			int int int ptr 	(int)	mgl_is_active
libmgl mgl_new_frame			ptr 	(int)	mgl_new_frame
libmgl mgl_end_frame			ptr 	(void)	mgl_end_frame
libmgl mgl_get_num_frame		ptr 	(int)	mgl_get_num_frame
libmgl mgl_reset_frames			ptr 	(void)	mgl_reset_frames
libmgl mgl_get_frame			int ptr 	(void)	mgl_get_frame
libmgl mgl_set_frame			int ptr 	(void)	mgl_set_frame
libmgl mgl_show_frame			int ptr 	(void)	mgl_show_frame
libmgl mgl_del_frame			int ptr 	(void)	mgl_del_frame
libmgl mgl_set_transp_type		int ptr 	(void)	mgl_set_transp_type
libmgl mgl_set_alpha			int ptr 	(void)	mgl_set_alpha
libmgl mgl_set_fog				double double ptr 	(void)	mgl_set_fog
libmgl mgl_set_light			int ptr 	(void)	mgl_set_light
libmgl mgl_set_light_n			int int ptr 	(void)	mgl_set_light_n
libmgl mgl_add_light			double double double int ptr 	(void)	mgl_add_light
libmgl mgl_add_light_ext		double double char double double double int ptr 	(void)	mgl_add_light_ext
libmgl mgl_add_light_loc		double double char double double double double double double int ptr 	(void)	mgl_add_light_loc
libmgl mgl_mat_pop				ptr 	(void)	mgl_mat_pop
libmgl mgl_mat_push				ptr 	(void)	mgl_mat_push
libmgl mgl_clf					ptr 	(void)	mgl_clf
libmgl mgl_clf_rgb				double double double ptr 	(void)	mgl_clf_rgb
libmgl mgl_clf_chr				char ptr 	(void)	mgl_clf_chr
libmgl mgl_subplot				ptr int int int ptr 	(void)	mgl_subplot
libmgl mgl_subplot_d			double double ptr int int int ptr 	(void)	mgl_subplot_d
libmgl mgl_multiplot			ptr int int int int int ptr 	(void)	mgl_multiplot
libmgl mgl_inplot				double double double double ptr 	(void)	mgl_inplot
libmgl mgl_relplot				double double double double ptr 	(void)	mgl_relplot
libmgl mgl_columnplot			double int int ptr 	(void)	mgl_columnplot
libmgl mgl_gridplot				double int int int ptr 	(void)	mgl_gridplot
libmgl mgl_stickplot			double double int int ptr 	(void)	mgl_stickplot
libmgl mgl_title				double ptr ptr ptr 	(void)	mgl_title
libmgl mgl_titlew				double ptr ptr ptr 	(void)	mgl_titlew
libmgl mgl_set_plotfactor		double ptr 	(void)	mgl_set_plotfactor
libmgl mgl_aspect				double double double ptr 	(void)	mgl_aspect
libmgl mgl_rotate				double double double ptr 	(void)	mgl_rotate
libmgl mgl_rotate_vector		double double double double ptr 	(void)	mgl_rotate_vector
libmgl mgl_perspective			double ptr 	(void)	mgl_perspective
libmgl mgl_view					double double double ptr 	(void)	mgl_view
libmgl mgl_zoom					double double double double ptr 	(void)	mgl_zoom
libmgl mgl_wnd_set_delay		double ptr 	(void)	mgl_wnd_set_delay
libmgl mgl_wnd_get_delay		ptr 	(double)	mgl_wnd_get_delay
libmgl mgl_setup_window			int int ptr 	(void)	mgl_setup_window
libmgl mgl_wnd_toggle_alpha		ptr 	(void)	mgl_wnd_toggle_alpha
libmgl mgl_wnd_toggle_light		ptr 	(void)	mgl_wnd_toggle_light
libmgl mgl_wnd_toggle_zoom		ptr 	(void)	mgl_wnd_toggle_zoom
libmgl mgl_wnd_toggle_rotate	ptr 	(void)	mgl_wnd_toggle_rotate
libmgl mgl_wnd_toggle_no		ptr 	(void)	mgl_wnd_toggle_no
libmgl mgl_wnd_update			ptr 	(void)	mgl_wnd_update
libmgl mgl_wnd_reload			ptr 	(void)	mgl_wnd_reload
libmgl mgl_wnd_adjust			ptr 	(void)	mgl_wnd_adjust
libmgl mgl_wnd_next_frame		ptr 	(void)	mgl_wnd_next_frame
libmgl mgl_wnd_prev_frame		ptr 	(void)	mgl_wnd_prev_frame
libmgl mgl_wnd_animation		ptr 	(void)	mgl_wnd_animation
libmgl mgl_get_last_mouse_pos	ptr ptr ptr ptr 	(void)	mgl_get_last_mouse_pos
libmgl mgl_create_parser			(ptr)	mgl_create_parser
libmgl mgl_use_parser			int ptr 	(int)	mgl_use_parser
libmgl mgl_delete_parser		ptr 	(void)	mgl_delete_parser
libmgl mgl_parser_add_param		ptr int ptr 	(void)	mgl_parser_add_param
libmgl mgl_parser_add_paramw	ptr int ptr 	(void)	mgl_parser_add_paramw
libmgl mgl_parser_add_var		ptr ptr 	(ptr)	mgl_parser_add_var
libmgl mgl_parser_add_varw		ptr ptr 	(ptr)	mgl_parser_add_varw
libmgl mgl_parser_find_var		ptr ptr 	(ptr)	mgl_parser_find_var
libmgl mgl_parser_find_varw		ptr ptr 	(ptr)	mgl_parser_find_varw
libmgl mgl_parser_del_var		ptr ptr 	(void)	mgl_parser_del_var
libmgl mgl_parser_del_varw		ptr ptr 	(void)	mgl_parser_del_varw
libmgl mgl_parser_del_all		ptr 	(void)	mgl_parser_del_all
libmgl mgl_parse_line			int ptr ptr ptr 	(int)	mgl_parse_line
libmgl mgl_parse_linew			int ptr ptr ptr 	(int)	mgl_parse_linew
libmgl mgl_parse_text			ptr ptr ptr 	(void)	mgl_parse_text
libmgl mgl_parse_textw			ptr ptr ptr 	(void)	mgl_parse_textw
libmgl mgl_parser_restore_once	ptr 	(void)	mgl_parser_restore_once
libmgl mgl_parser_allow_setsize	int ptr 	(void)	mgl_parser_allow_setsize
libmgl mgl_parser_allow_file_io	int ptr 	(void)	mgl_parser_allow_file_io
libmgl mgl_parser_stop			ptr 	(void)	mgl_parser_stop
libmgl mgl_parser_cmd_type		ptr ptr 	(int)	mgl_parser_cmd_type
libmgl gl_parser_cmd_desc		ptr ptr 	(ptr)	gl_parser_cmd_desc
libmgl gl_parser_cmd_frmt		ptr ptr 	(ptr)	gl_parser_cmd_frmt
libmgl gl_parser_cmd_name		int ptr 	(ptr)	gl_parser_cmd_name
libmgl mgl_parser_cmd_num		ptr 	(int)	mgl_parser_cmd_num
libmgl mgl_parser_calc			ptr ptr 	(ptr)	mgl_parser_calc
libmgl mgl_parser_calcw			ptr ptr 	(ptr)	mgl_parser_calcw
libmgl mgl_create_expr			ptr 	(ptr)	mgl_create_expr
libmgl mgl_delete_expr			ptr 	(void)	mgl_delete_expr
libmgl mgl_expr_eval			double double double ptr 	(double)	mgl_expr_eval
libmgl mgl_expr_eval_v			ptr ptr 	(double)	mgl_expr_eval_v
libmgl mgl_expr_diff			double double double char ptr 	(double)	mgl_expr_diff
libmgl mgl_expr_diff_v			ptr char ptr 	(double)	mgl_expr_diff_v
libmgl mgl_gauss_rnd				(double)	mgl_gauss_rnd
libmgl mgl_fft_freq				int ptr 	(void)	mgl_fft_freq
libmgl mgl_strcls				ptr 	(void)	mgl_strcls
libmgl mgl_strpos				ptr ptr 	(int)	mgl_strpos
libmgl mgl_chrpos				char ptr 	(int)	mgl_chrpos
libmgl mgl_istrue				char 	(int)	mgl_istrue
