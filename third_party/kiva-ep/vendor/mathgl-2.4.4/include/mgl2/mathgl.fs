Module mathgl
also dos

library libmgl      libmgl.so
library libmgl-glut libmgl-glut.so
library libmgl-fltk libmgl-fltk.so
library libmgl-qt   libmgl-qt.so
library libmgl-wx   libmgl-wx.so

legacy off

libmgl      mgl_create_graph_gl                                                      (ptr) mgl_create_graph_gl
libmgl      mgl_create_graph_zb       int int                                        (ptr) mgl_create_graph_zb
libmgl      mgl_create_graph_ps       int int                                        (ptr) mgl_create_graph_ps
libmgl      mgl_fortran_func          ptr ptr                                        (int) mgl_fortran_func
libmgl-glut mgl_create_graph_glut                                        ptr ptr ptr (ptr) mgl_create_graph_glut
libmgl-fltk mgl_create_graph_fltk                                        ptr ptr ptr (ptr) mgl_create_graph_fltk
libmgl-qt   mgl_create_graph_qt                                          ptr ptr ptr (ptr) mgl_create_graph_qt
\ libmgl-glut mgl_create_graph_glut_dr  ptr ptr                                        (ptr) mgl_create_graph_glut_dr
\ libmgl-fltk mgl_create_graph_fltk_dr  ptr ptr                                        (ptr) mgl_create_graph_fltk_dr
\ libmgl-qt   mgl_create_graph_qt_dr    ptr ptr                                        (ptr) mgl_create_graph_qt_dr
libmgl      mgl_create_graph_idtf                                                    (ptr) mgl_create_graph_idtf
libmgl-fltk mgl_fltk_run                                                            (void) mgl_fltk_run
libmgl-qt   mgl_qt_run                                                              (void) mgl_qt_run
libmgl      mgl_set_show_mouse_pos    int ptr                                       (void) mgl_set_show_mouse_pos
libmgl      mgl_get_last_mouse_pos    sf sf sf ptr                                  (void) mgl_get_last_mouse_pos
libmgl      mgl_update                ptr                                           (void) mgl_update
libmgl      mgl_delete_graph          ptr                                           (void) mgl_delete_graph
libmgl      mgl_create_data                                                          (ptr) mgl_create_data
libmgl      mgl_create_data_size      int int int                                    (ptr) mgl_create_data_size
libmgl      mgl_create_data_file      ptr                                            (ptr) mgl_create_data_file
libmgl      mgl_delete_data           ptr                                           (void) mgl_delete_data
libmgl      mgl_create_parser                                                        (ptr) mgl_create_parser
libmgl      mgl_delete_parser         ptr                                           (void) mgl_delete_parser
libmgl      mgl_add_param             ptr int ptr                                   (void) mgl_add_param
libmgl      mgl_add_paramw            ptr int ptr                                   (void) mgl_add_paramw
libmgl      mgl_add_var               ptr ptr                                        (ptr) mgl_add_var
libmgl      mgl_find_var              ptr ptr                                        (ptr) mgl_find_var
libmgl      mgl_parse                 int ptr ptr ptr                                (int) mgl_parse
libmgl      mgl_parsew                int ptr ptr ptr                                (int) mgl_parsew
libmgl      mgl_parse_text            ptr ptr ptr                                   (void) mgl_parse_text
libmgl      mgl_parsew_text           ptr ptr ptr                                   (void) mgl_parsew_text
libmgl      mgl_restore_once          ptr                                           (void) mgl_restore_once
libmgl      mgl_parser_allow_setsize  int ptr                                       (void) mgl_parser_allow_setsize
libmgl      mgl_set_def_param         ptr                                           (void) mgl_set_def_param
libmgl      mgl_set_palette           ptr ptr                                       (void) mgl_set_palette
libmgl      mgl_set_pal_color         sf sf sf int ptr                              (void) mgl_set_pal_color
libmgl      mgl_set_pal_num           int ptr                                       (void) mgl_set_pal_num
libmgl      mgl_set_rotated_text      int ptr                                       (void) mgl_set_rotated_text
libmgl      mgl_set_cut               int ptr                                       (void) mgl_set_cut
libmgl      mgl_set_cut_box           sf sf sf sf sf sf ptr                         (void) mgl_set_cut_box
libmgl      mgl_set_tick_len          sf ptr                                        (void) mgl_set_tick_len
libmgl      mgl_set_bar_width         sf ptr                                        (void) mgl_set_bar_width
libmgl      mgl_set_base_line_width   sf ptr                                        (void) mgl_set_base_line_width
libmgl      mgl_set_mark_size         sf ptr                                        (void) mgl_set_mark_size
libmgl      mgl_set_arrow_size        sf ptr                                        (void) mgl_set_arrow_size
libmgl      mgl_set_font_size         sf ptr                                        (void) mgl_set_font_size
libmgl      mgl_set_font_def          ptr ptr                                       (void) mgl_set_font_def
libmgl      mgl_set_alpha_default     sf ptr                                        (void) mgl_set_alpha_default
libmgl      mgl_set_size              int int ptr                                   (void) mgl_set_size
libmgl      mgl_set_axial_dir         ptr ptr                                       (void) mgl_set_axial_dir
libmgl      mgl_set_meshnum           int ptr                                       (void) mgl_set_meshnum
libmgl      mgl_set_zoom              sf sf sf sf ptr                               (void) mgl_set_zoom
libmgl      mgl_set_plotfactor        sf ptr                                        (void) mgl_set_plotfactor
libmgl      mgl_set_draw_face         int ptr                                       (void) mgl_set_draw_face
libmgl      mgl_set_scheme            ptr ptr                                       (void) mgl_set_scheme
libmgl      mgl_load_font             ptr ptr ptr                                   (void) mgl_load_font
libmgl      mgl_copy_font             ptr ptr                                       (void) mgl_copy_font
libmgl      mgl_restore_font          ptr                                           (void) mgl_restore_font
libmgl      mgl_show_image            int ptr ptr                                   (void) mgl_show_image
libmgl      mgl_write_frame           ptr ptr ptr                                   (void) mgl_write_frame
libmgl      mgl_write_bmp             ptr ptr ptr                                   (void) mgl_write_bmp
libmgl      mgl_write_jpg             ptr ptr ptr                                   (void) mgl_write_jpg
libmgl      mgl_write_png             ptr ptr ptr                                   (void) mgl_write_png
libmgl      mgl_write_png_solid       ptr ptr ptr                                   (void) mgl_write_png_solid
libmgl      mgl_write_eps             ptr ptr ptr                                   (void) mgl_write_eps
libmgl      mgl_write_svg             ptr ptr ptr                                   (void) mgl_write_svg
libmgl      mgl_write_idtf            ptr ptr ptr                                   (void) mgl_write_idtf
libmgl      mgl_write_gif             ptr ptr ptr                                   (void) mgl_write_gif
libmgl      mgl_start_gif             int ptr ptr                                   (void) mgl_start_gif
libmgl      mgl_close_gif             ptr                                           (void) mgl_close_gif
libmgl      mgl_get_rgb               ptr                                            (ptr) mgl_get_rgb
libmgl      mgl_get_rgba              ptr                                            (ptr) mgl_get_rgba
libmgl      mgl_get_width             ptr                                            (int) mgl_get_width
libmgl      mgl_get_height            ptr                                            (int) mgl_get_height
libmgl      mgl_new_frame             ptr                                            (int) mgl_new_frame
libmgl      mgl_end_frame             ptr                                           (void) mgl_end_frame
libmgl      mgl_get_num_frame         ptr                                            (int) mgl_get_num_frame
libmgl      mgl_reset_frames          ptr                                           (void) mgl_reset_frames
libmgl      mgl_set_transp_type       int ptr                                       (void) mgl_set_transp_type
libmgl      mgl_set_transp            int ptr                                       (void) mgl_set_transp
libmgl      mgl_set_alpha             int ptr                                       (void) mgl_set_alpha
libmgl      mgl_set_fog               sf sf ptr                                     (void) mgl_set_fog
libmgl      mgl_set_light             int ptr                                       (void) mgl_set_light
libmgl      mgl_set_light_n           int int ptr                                   (void) mgl_set_light_n
libmgl      mgl_add_light             ptr sf sf sf int ptr                          (void) mgl_add_light
libmgl      mgl_add_light_rgb         sf sf sf sf int sf sf sf int ptr              (void) mgl_add_light_rgb
libmgl      mgl_set_ambbr             sf ptr                                        (void) mgl_set_ambbr
libmgl      mgl_identity              ptr                                           (void) mgl_identity
libmgl      mgl_clf                   ptr                                           (void) mgl_clf
libmgl      mgl_flush                 ptr                                           (void) mgl_flush
libmgl      mgl_clf_rgb               sf sf sf ptr                                  (void) mgl_clf_rgb
libmgl      mgl_subplot               int int int ptr                               (void) mgl_subplot
libmgl      mgl_subplot_d             sf sf int int int ptr                         (void) mgl_subplot_d
libmgl      mgl_inplot                sf sf sf sf ptr                               (void) mgl_inplot
libmgl      mgl_relplot               sf sf sf sf ptr                               (void) mgl_relplot
libmgl      mgl_columnplot            int int ptr                                   (void) mgl_columnplot
libmgl      mgl_aspect                sf sf sf ptr                                  (void) mgl_aspect
libmgl      mgl_rotate                sf sf sf ptr                                  (void) mgl_rotate
libmgl      mgl_rotate_vector         sf sf sf sf ptr                               (void) mgl_rotate_vector
libmgl      mgl_perspective           sf ptr                                        (void) mgl_perspective
libmgl      mgl_set_ticks             sf sf sf ptr                                  (void) mgl_set_ticks
libmgl      mgl_set_subticks          int int int ptr                               (void) mgl_set_subticks
libmgl      mgl_set_ticks_dir         sf int sf ptr ptr                             (void) mgl_set_ticks_dir
\ libmgl      mgl_set_ticks_val          ... ptr  double val int ptr ptr              (void) mgl_set_ticks_val
libmgl      mgl_set_ticks_vals        ptr sf int ptr ptr                            (void) mgl_set_ticks_vals
libmgl      mgl_set_caxis             sf sf ptr                                     (void) mgl_set_caxis
libmgl      mgl_set_axis              sf sf sf sf sf sf sf sf sf ptr                (void) mgl_set_axis
libmgl      mgl_set_axis_3d           sf sf sf sf sf sf ptr                         (void) mgl_set_axis_3d
libmgl      mgl_set_axis_2d           sf sf sf sf ptr                               (void) mgl_set_axis_2d
libmgl      mgl_set_origin            sf sf sf ptr                                  (void) mgl_set_origin
libmgl      mgl_set_tick_origin       sf sf sf ptr                                  (void) mgl_set_tick_origin
libmgl      mgl_set_crange            int ptr ptr                                   (void) mgl_set_crange
libmgl      mgl_set_xrange            int ptr ptr                                   (void) mgl_set_xrange
libmgl      mgl_set_yrange            int ptr ptr                                   (void) mgl_set_yrange
libmgl      mgl_set_zrange            int ptr ptr                                   (void) mgl_set_zrange
libmgl      mgl_set_auto              sf sf sf sf sf sf ptr                         (void) mgl_set_auto
libmgl      mgl_set_func              ptr ptr ptr ptr                               (void) mgl_set_func
libmgl      mgl_set_ternary           int ptr                                       (void) mgl_set_ternary
libmgl      mgl_set_cutoff            ptr ptr                                       (void) mgl_set_cutoff
libmgl      mgl_box                   int ptr                                       (void) mgl_box
libmgl      mgl_box_str               int ptr ptr                                   (void) mgl_box_str
libmgl      mgl_box_rgb               int sf sf sf ptr                              (void) mgl_box_rgb
libmgl      mgl_axis                  ptr ptr                                       (void) mgl_axis
libmgl      mgl_axis_grid             ptr ptr ptr                                   (void) mgl_axis_grid
libmgl      mgl_label                 ptr ptr ptr                                   (void) mgl_label
libmgl      mgl_label_ext             sf sf int ptr ptr ptr                         (void) mgl_label_ext
libmgl      mgl_tune_ticks            sf int ptr                                    (void) mgl_tune_ticks
libmgl      mgl_set_xttw              ptr ptr                                       (void) mgl_set_xttw
libmgl      mgl_set_yttw              ptr ptr                                       (void) mgl_set_yttw
libmgl      mgl_set_zttw              ptr ptr                                       (void) mgl_set_zttw
libmgl      mgl_set_cttw              ptr ptr                                       (void) mgl_set_cttw
libmgl      mgl_set_xtt               ptr ptr                                       (void) mgl_set_xtt
libmgl      mgl_set_ytt               ptr ptr                                       (void) mgl_set_ytt
libmgl      mgl_set_ztt               ptr ptr                                       (void) mgl_set_ztt
libmgl      mgl_set_ctt               ptr ptr                                       (void) mgl_set_ctt
libmgl      mgl_ball                  sf sf sf ptr                                  (void) mgl_ball
libmgl      mgl_ball_rgb              sf sf sf sf sf sf sf ptr                      (void) mgl_ball_rgb
libmgl      mgl_ball_str              ptr sf sf sf ptr                              (void) mgl_ball_str
libmgl      mgl_line                  int ptr sf sf sf sf sf sf ptr                 (void) mgl_line
libmgl      mgl_facex                 sf sf ptr sf sf sf sf sf ptr                  (void) mgl_facex
libmgl      mgl_facey                 sf sf ptr sf sf sf sf sf ptr                  (void) mgl_facey
libmgl      mgl_facez                 sf sf ptr sf sf sf sf sf ptr                  (void) mgl_facez
libmgl      mgl_curve                 int ptr sf sf sf sf sf sf sf sf sf sf sf sf ptr (void) mgl_curve
libmgl      mgl_puts                  ptr sf sf sf ptr                              (void) mgl_puts
libmgl      mgl_putsw                 ptr sf sf sf ptr                              (void) mgl_putsw
libmgl      mgl_puts_dir              sf ptr sf sf sf sf sf sf ptr                  (void) mgl_puts_dir
libmgl      mgl_putsw_dir             sf ptr sf sf sf sf sf sf ptr                  (void) mgl_putsw_dir
libmgl      mgl_text                  ptr sf sf sf ptr                              (void) mgl_text
libmgl      mgl_title                 sf ptr ptr ptr                                (void) mgl_title
libmgl      mgl_titlew                sf ptr ptr ptr                                (void) mgl_titlew
libmgl      mgl_putsw_ext             ptr sf ptr ptr sf sf sf ptr                   (void) mgl_putsw_ext
libmgl      mgl_puts_ext              ptr sf ptr ptr sf sf sf ptr                   (void) mgl_puts_ext
libmgl      mgl_text_ext              ptr sf ptr ptr sf sf sf ptr                   (void) mgl_text_ext
libmgl      mgl_colorbar              int ptr ptr                                   (void) mgl_colorbar
libmgl      mgl_colorbar_ext          sf sf sf sf int ptr ptr                       (void) mgl_colorbar_ext
libmgl      mgl_simple_plot           ptr int ptr ptr                               (void) mgl_simple_plot
libmgl      mgl_add_legend            ptr ptr ptr                                   (void) mgl_add_legend
libmgl      mgl_add_legendw           ptr ptr ptr                                   (void) mgl_add_legendw
libmgl      mgl_clear_legend          ptr                                           (void) mgl_clear_legend
libmgl      mgl_legend_xy             sf sf ptr sf sf ptr                           (void) mgl_legend_xy
libmgl      mgl_legend                sf sf ptr int ptr                             (void) mgl_legend
libmgl      mgl_set_legend_box        int ptr                                       (void) mgl_set_legend_box
libmgl      mgl_fplot                 int ptr ptr ptr                               (void) mgl_fplot
libmgl      mgl_fplot_xyz             int ptr ptr ptr ptr ptr                       (void) mgl_fplot_xyz
libmgl      mgl_plot_xyz              ptr ptr ptr ptr ptr                           (void) mgl_plot_xyz
libmgl      mgl_plot_xy               ptr ptr ptr ptr                               (void) mgl_plot_xy
libmgl      mgl_plot                  ptr ptr ptr                                   (void) mgl_plot
libmgl      mgl_plot_2                ptr ptr ptr                                   (void) mgl_plot_2
libmgl      mgl_plot_3                ptr ptr ptr                                   (void) mgl_plot_3
libmgl      mgl_tens_xyz              ptr ptr ptr ptr ptr ptr                       (void) mgl_tens_xyz
libmgl      mgl_tens_xy               ptr ptr ptr ptr ptr                           (void) mgl_tens_xy
libmgl      mgl_tens                  ptr ptr ptr ptr                               (void) mgl_tens
libmgl      mgl_area_xyz              ptr ptr ptr ptr ptr                           (void) mgl_area_xyz
libmgl      mgl_area_xy               ptr ptr ptr ptr                               (void) mgl_area_xy
libmgl      mgl_area_xys              ptr ptr ptr ptr                               (void) mgl_area_xys
libmgl      mgl_area                  ptr ptr ptr                                   (void) mgl_area
libmgl      mgl_area_2                ptr ptr ptr                                   (void) mgl_area_2
libmgl      mgl_area_3                ptr ptr ptr                                   (void) mgl_area_3
libmgl      mgl_region_xy             int ptr ptr ptr ptr ptr                       (void) mgl_region_xy
libmgl      mgl_region                int ptr ptr ptr ptr                           (void) mgl_region
libmgl      mgl_mark                  ptr sf sf sf ptr                              (void) mgl_mark
libmgl      mgl_stem_xyz              ptr ptr ptr ptr ptr                           (void) mgl_stem_xyz
libmgl      mgl_stem_xy               ptr ptr ptr ptr                               (void) mgl_stem_xy
libmgl      mgl_stem                  ptr ptr ptr                                   (void) mgl_stem
libmgl      mgl_stem_2                ptr ptr ptr                                   (void) mgl_stem_2
libmgl      mgl_stem_3                ptr ptr ptr                                   (void) mgl_stem_3
libmgl      mgl_step_xyz              ptr ptr ptr ptr ptr                           (void) mgl_step_xyz
libmgl      mgl_step_xy               ptr ptr ptr ptr                               (void) mgl_step_xy
libmgl      mgl_step                  ptr ptr ptr                                   (void) mgl_step
libmgl      mgl_step_2                ptr ptr ptr                                   (void) mgl_step_2
libmgl      mgl_step_3                ptr ptr ptr                                   (void) mgl_step_3
libmgl      mgl_bars_xyz              ptr ptr ptr ptr ptr                           (void) mgl_bars_xyz
libmgl      mgl_bars_xy               ptr ptr ptr ptr                               (void) mgl_bars_xy
libmgl      mgl_bars                  ptr ptr ptr                                   (void) mgl_bars
libmgl      mgl_bars_2                ptr ptr ptr                                   (void) mgl_bars_2
libmgl      mgl_bars_3                ptr ptr ptr                                   (void) mgl_bars_3
libmgl      mgl_barh_yx               ptr ptr ptr ptr                               (void) mgl_barh_yx
libmgl      mgl_barh                  ptr ptr ptr                                   (void) mgl_barh
libmgl      mgl_torus                 ptr ptr ptr ptr                               (void) mgl_torus
libmgl      mgl_torus_2               ptr ptr ptr                                   (void) mgl_torus_2
libmgl      mgl_text_xyz              sf ptr ptr ptr ptr ptr ptr                    (void) mgl_text_xyz
libmgl      mgl_text_xy               sf ptr ptr ptr ptr ptr                        (void) mgl_text_xy
libmgl      mgl_text_y                sf ptr ptr ptr ptr                            (void) mgl_text_y
libmgl      mgl_chart                 ptr ptr ptr                                   (void) mgl_chart
libmgl      mgl_error                 ptr ptr ptr ptr                               (void) mgl_error
libmgl      mgl_error_xy              ptr ptr ptr ptr ptr                           (void) mgl_error_xy
libmgl      mgl_error_exy             ptr ptr ptr ptr ptr ptr                       (void) mgl_error_exy
libmgl      mgl_mark_xyz              ptr ptr ptr ptr ptr ptr                       (void) mgl_mark_xyz
libmgl      mgl_mark_xy               ptr ptr ptr ptr ptr                           (void) mgl_mark_xy
libmgl      mgl_mark_y                ptr ptr ptr ptr                               (void) mgl_mark_y
libmgl      mgl_tube_xyzr             ptr ptr ptr ptr ptr ptr                       (void) mgl_tube_xyzr
libmgl      mgl_tube_xyr              ptr ptr ptr ptr ptr                           (void) mgl_tube_xyr
libmgl      mgl_tube_r                ptr ptr ptr ptr                               (void) mgl_tube_r
libmgl      mgl_tube_xyz              ptr sf ptr ptr ptr ptr                        (void) mgl_tube_xyz
libmgl      mgl_tube_xy               ptr sf ptr ptr ptr                            (void) mgl_tube_xy
libmgl      mgl_tube                  ptr sf ptr ptr                                (void) mgl_tube
libmgl      mgl_textmark_xyzr         ptr ptr ptr ptr ptr ptr ptr                   (void) mgl_textmark_xyzr
libmgl      mgl_textmark_xyr          ptr ptr ptr ptr ptr ptr                       (void) mgl_textmark_xyr
libmgl      mgl_textmark_yr           ptr ptr ptr ptr ptr                           (void) mgl_textmark_yr
libmgl      mgl_textmark              ptr ptr ptr ptr                               (void) mgl_textmark
libmgl      mgl_textmarkw_xyzr        ptr ptr ptr ptr ptr ptr ptr                   (void) mgl_textmarkw_xyzr
libmgl      mgl_textmarkw_xyr         ptr ptr ptr ptr ptr ptr                       (void) mgl_textmarkw_xyr
libmgl      mgl_textmarkw_yr          ptr ptr ptr ptr ptr                           (void) mgl_textmarkw_yr
libmgl      mgl_textmarkw             ptr ptr ptr ptr                               (void) mgl_textmarkw
libmgl      mgl_fsurf                 int ptr ptr ptr                               (void) mgl_fsurf
libmgl      mgl_fsurf_xyz             int ptr ptr ptr ptr ptr                       (void) mgl_fsurf_xyz
libmgl      mgl_grid_xy               sf ptr ptr ptr ptr ptr                        (void) mgl_grid_xy
libmgl      mgl_grid                  sf ptr ptr ptr                                (void) mgl_grid
libmgl      mgl_mesh_xy               ptr ptr ptr ptr ptr                           (void) mgl_mesh_xy
libmgl      mgl_mesh                  ptr ptr ptr                                   (void) mgl_mesh
libmgl      mgl_fall_xy               ptr ptr ptr ptr ptr                           (void) mgl_fall_xy
libmgl      mgl_fall                  ptr ptr ptr                                   (void) mgl_fall
libmgl      mgl_belt_xy               ptr ptr ptr ptr ptr                           (void) mgl_belt_xy
libmgl      mgl_belt                  ptr ptr ptr                                   (void) mgl_belt
libmgl      mgl_surf_xy               ptr ptr ptr ptr ptr                           (void) mgl_surf_xy
libmgl      mgl_surf                  ptr ptr ptr                                   (void) mgl_surf
libmgl      mgl_dens_xy               sf ptr ptr ptr ptr ptr                        (void) mgl_dens_xy
libmgl      mgl_dens                  sf ptr ptr ptr                                (void) mgl_dens
libmgl      mgl_boxs_xy               sf ptr ptr ptr ptr ptr                        (void) mgl_boxs_xy
libmgl      mgl_boxs                  sf ptr ptr ptr                                (void) mgl_boxs
libmgl      mgl_tile_xy               ptr ptr ptr ptr ptr                           (void) mgl_tile_xy
libmgl      mgl_tile                  ptr ptr ptr                                   (void) mgl_tile
libmgl      mgl_tiles_xy              ptr ptr ptr ptr ptr ptr                       (void) mgl_tiles_xy
libmgl      mgl_tiles                 ptr ptr ptr ptr                               (void) mgl_tiles
libmgl      mgl_cont_xy_val           sf ptr ptr ptr ptr ptr ptr                    (void) mgl_cont_xy_val
libmgl      mgl_cont_val              sf ptr ptr ptr ptr                            (void) mgl_cont_val
libmgl      mgl_cont_xy               sf int ptr ptr ptr ptr ptr                    (void) mgl_cont_xy
libmgl      mgl_cont                  sf int ptr ptr ptr                            (void) mgl_cont
libmgl      mgl_contf_xy_val          sf ptr ptr ptr ptr ptr ptr                    (void) mgl_contf_xy_val
libmgl      mgl_contf_val             sf ptr ptr ptr ptr                            (void) mgl_contf_val
libmgl      mgl_contf_xy              sf int ptr ptr ptr ptr ptr                    (void) mgl_contf_xy
libmgl      mgl_contf                 sf int ptr ptr ptr                            (void) mgl_contf
libmgl      mgl_axial_xy_val          ptr ptr ptr ptr ptr ptr                       (void) mgl_axial_xy_val
libmgl      mgl_axial_val             ptr ptr ptr ptr                               (void) mgl_axial_val
libmgl      mgl_axial_xy              int ptr ptr ptr ptr ptr                       (void) mgl_axial_xy
libmgl      mgl_axial                 int ptr ptr ptr                               (void) mgl_axial
libmgl      mgl_surfc_xy              ptr ptr ptr ptr ptr ptr                       (void) mgl_surfc_xy
libmgl      mgl_surfc                 ptr ptr ptr ptr                               (void) mgl_surfc
libmgl      mgl_surfa_xy              ptr ptr ptr ptr ptr ptr                       (void) mgl_surfa_xy
libmgl      mgl_surfa                 ptr ptr ptr ptr                               (void) mgl_surfa
libmgl      mgl_stfa_xy               sf ptr int ptr ptr ptr ptr ptr                (void) mgl_stfa_xy
libmgl      mgl_stfa                  sf ptr int ptr ptr ptr                        (void) mgl_stfa
libmgl      mgl_vect_xy               sf ptr ptr ptr ptr ptr ptr                    (void) mgl_vect_xy
libmgl      mgl_vect_2d               sf ptr ptr ptr ptr                            (void) mgl_vect_2d
libmgl      mgl_vectl_xy              sf ptr ptr ptr ptr ptr ptr                    (void) mgl_vectl_xy
libmgl      mgl_vectl_2d              sf ptr ptr ptr ptr                            (void) mgl_vectl_2d
libmgl      mgl_vectc_xy              sf ptr ptr ptr ptr ptr ptr                    (void) mgl_vectc_xy
libmgl      mgl_vectc_2d              sf ptr ptr ptr ptr                            (void) mgl_vectc_2d
libmgl      mgl_vect_xyz              ptr ptr ptr ptr ptr ptr ptr ptr               (void) mgl_vect_xyz
libmgl      mgl_vect_3d               ptr ptr ptr ptr ptr                           (void) mgl_vect_3d
libmgl      mgl_vectl_xyz             ptr ptr ptr ptr ptr ptr ptr ptr               (void) mgl_vectl_xyz
libmgl      mgl_vectl_3d              ptr ptr ptr ptr ptr                           (void) mgl_vectl_3d
libmgl      mgl_vectc_xyz             ptr ptr ptr ptr ptr ptr ptr ptr               (void) mgl_vectc_xyz
libmgl      mgl_vectc_3d              ptr ptr ptr ptr ptr                           (void) mgl_vectc_3d
libmgl      mgl_map_xy                int int ptr ptr ptr ptr ptr ptr               (void) mgl_map_xy
libmgl      mgl_map                   int int ptr ptr ptr ptr                       (void) mgl_map
libmgl      mgl_surf3a_xyz_val        ptr ptr ptr ptr ptr ptr sf ptr                (void) mgl_surf3a_xyz_val
libmgl      mgl_surf3a_val            ptr ptr ptr sf ptr                            (void) mgl_surf3a_val
libmgl      mgl_surf3a_xyz            int ptr ptr ptr ptr ptr ptr ptr               (void) mgl_surf3a_xyz
libmgl      mgl_surf3a                int ptr ptr ptr ptr                           (void) mgl_surf3a
libmgl      mgl_surf3c_xyz_val        ptr ptr ptr ptr ptr ptr sf ptr                (void) mgl_surf3c_xyz_val
libmgl      mgl_surf3c_val            ptr ptr ptr sf ptr                            (void) mgl_surf3c_val
libmgl      mgl_surf3c_xyz                                                          (void) mgl_surf3c_xyz
libmgl      mgl_surf3c                int ptr ptr ptr ptr                           (void) mgl_surf3c
libmgl      mgl_flow_xy               sf int int ptr ptr ptr ptr ptr ptr            (void) mgl_flow_xy
libmgl      mgl_flow_2d               sf int int ptr ptr ptr ptr                    (void) mgl_flow_2d
libmgl      mgl_flow_xyz              int int ptr ptr ptr ptr ptr ptr ptr ptr       (void) mgl_flow_xyz
libmgl      mgl_flow_3d               int int ptr ptr ptr ptr ptr                   (void) mgl_flow_3d
libmgl      mgl_pipe_xy               sf int int sf ptr ptr ptr ptr ptr ptr         (void) mgl_pipe_xy
libmgl      mgl_pipe_2d               sf int int sf ptr ptr ptr ptr                 (void) mgl_pipe_2d
libmgl      mgl_pipe_xyz              int int sf ptr ptr ptr ptr ptr ptr ptr ptr    (void) mgl_pipe_xyz
libmgl      mgl_pipe_3d               int int sf ptr ptr ptr ptr ptr                (void) mgl_pipe_3d
libmgl      mgl_dew_xy                sf ptr ptr ptr ptr ptr ptr                    (void) mgl_dew_xy
libmgl      mgl_dew_2d                sf ptr ptr ptr ptr                            (void) mgl_dew_2d
libmgl      mgl_grid3_xyz             ptr int ptr ptr ptr ptr ptr ptr               (void) mgl_grid3_xyz
libmgl      mgl_grid3                 ptr int ptr ptr ptr                           (void) mgl_grid3
libmgl      mgl_grid3_all_xyz         ptr ptr ptr ptr ptr ptr                       (void) mgl_grid3_all_xyz
libmgl      mgl_grid3_all             ptr ptr ptr                                   (void) mgl_grid3_all
libmgl      mgl_dens3_xyz             ptr int ptr ptr ptr ptr ptr ptr               (void) mgl_dens3_xyz
libmgl      mgl_dens3                 ptr int ptr ptr ptr                           (void) mgl_dens3
libmgl      mgl_dens3_all_xyz         ptr ptr ptr ptr ptr ptr                       (void) mgl_dens3_all_xyz
libmgl      mgl_dens3_all             ptr ptr ptr                                   (void) mgl_dens3_all
libmgl      mgl_surf3_xyz_val         ptr ptr ptr ptr ptr sf ptr                    (void) mgl_surf3_xyz_val
libmgl      mgl_surf3_val             ptr ptr sf ptr                                (void) mgl_surf3_val
libmgl      mgl_surf3_xyz             int ptr ptr ptr ptr ptr ptr                   (void) mgl_surf3_xyz
libmgl      mgl_surf3                 int ptr ptr ptr                               (void) mgl_surf3
libmgl      mgl_cont3_xyz_val         ptr int ptr ptr ptr ptr ptr ptr ptr           (void) mgl_cont3_xyz_val
libmgl      mgl_cont3_val             ptr int ptr ptr ptr ptr                       (void) mgl_cont3_val
libmgl      mgl_cont3_xyz             int ptr int ptr ptr ptr ptr ptr ptr           (void) mgl_cont3_xyz
libmgl      mgl_cont3                 int ptr int ptr ptr ptr                       (void) mgl_cont3
libmgl      mgl_cont_all_xyz          int ptr ptr ptr ptr ptr ptr                   (void) mgl_cont_all_xyz
libmgl      mgl_cont_all              int ptr ptr ptr                               (void) mgl_cont_all
libmgl      mgl_cloudp_xyz            sf ptr ptr ptr ptr ptr ptr                    (void) mgl_cloudp_xyz
libmgl      mgl_cloudp                sf ptr ptr ptr                                (void) mgl_cloudp
libmgl      mgl_cloud_xyz             sf ptr ptr ptr ptr ptr ptr                    (void) mgl_cloud_xyz
libmgl      mgl_cloud                 sf ptr ptr ptr                                (void) mgl_cloud
libmgl      mgl_contf3_xyz_val        ptr int ptr ptr ptr ptr ptr ptr ptr           (void) mgl_contf3_xyz_val
libmgl      mgl_contf3_val            ptr int ptr ptr ptr ptr                       (void) mgl_contf3_val
libmgl      mgl_contf3_xyz            int ptr int ptr ptr ptr ptr ptr ptr           (void) mgl_contf3_xyz
libmgl      mgl_contf3                int ptr int ptr ptr ptr                       (void) mgl_contf3
libmgl      mgl_contf_all_xyz         int ptr ptr ptr ptr ptr ptr                   (void) mgl_contf_all_xyz
libmgl      mgl_contf_all             int ptr ptr ptr                               (void) mgl_contf_all
libmgl      mgl_beam_val              int ptr sf ptr ptr ptr ptr sf ptr             (void) mgl_beam_val
libmgl      mgl_beam                  int int ptr sf ptr ptr ptr ptr ptr            (void) mgl_beam
libmgl      mgl_triplot_xyzc          ptr ptr ptr ptr ptr ptr ptr                   (void) mgl_triplot_xyzc
libmgl      mgl_triplot_xyz           ptr ptr ptr ptr ptr ptr                       (void) mgl_triplot_xyz
libmgl      mgl_triplot_xy            sf ptr ptr ptr ptr ptr                        (void) mgl_triplot_xy
libmgl      mgl_dots                  ptr ptr ptr ptr ptr                           (void) mgl_dots
libmgl      mgl_dots_tr               ptr ptr ptr                                   (void) mgl_dots_tr
libmgl      mgl_crust                 sf ptr ptr ptr ptr ptr                        (void) mgl_crust
libmgl      mgl_crust_tr              sf ptr ptr ptr                                (void) mgl_crust_tr
libmgl      mgl_dens_x                sf ptr ptr ptr                                (void) mgl_dens_x
libmgl      mgl_dens_y                sf ptr ptr ptr                                (void) mgl_dens_y
libmgl      mgl_dens_z                sf ptr ptr ptr                                (void) mgl_dens_z
libmgl      mgl_cont_x                int sf ptr ptr ptr                            (void) mgl_cont_x
libmgl      mgl_cont_y                int sf ptr ptr ptr                            (void) mgl_cont_y
libmgl      mgl_cont_z                int sf ptr ptr ptr                            (void) mgl_cont_z
libmgl      mgl_cont_x_val            sf ptr ptr ptr ptr                            (void) mgl_cont_x_val
libmgl      mgl_cont_y_val            sf ptr ptr ptr ptr                            (void) mgl_cont_y_val
libmgl      mgl_cont_z_val            sf ptr ptr ptr ptr                            (void) mgl_cont_z_val
libmgl      mgl_contf_x               int sf ptr ptr ptr                            (void) mgl_contf_x
libmgl      mgl_contf_y               int sf ptr ptr ptr                            (void) mgl_contf_y
libmgl      mgl_contf_z               int sf ptr ptr ptr                            (void) mgl_contf_z
libmgl      mgl_contf_x_val           sf ptr ptr ptr ptr                            (void) mgl_contf_x_val
libmgl      mgl_contf_y_val           sf ptr ptr ptr ptr                            (void) mgl_contf_y_val
libmgl      mgl_contf_z_val           sf ptr ptr ptr ptr                            (void) mgl_contf_z_val
libmgl      mgl_data_rearrange        int int int ptr                               (void) mgl_data_rearrange
libmgl      mgl_data_set_float        int int int sf ptr                            (void) mgl_data_set_float
libmgl      mgl_data_set_double       int int int df ptr                            (void) mgl_data_set_double
libmgl      mgl_data_set_float2       int int sf ptr                                (void) mgl_data_set_float2
libmgl      mgl_data_set_double2      int int df ptr                                (void) mgl_data_set_double2
libmgl      mgl_data_set_float3       int int int sf ptr                            (void) mgl_data_set_float3
libmgl      mgl_data_set_double3      int int int ptr ptr                           (void) mgl_data_set_double3
libmgl      mgl_data_set              ptr ptr                                       (void) mgl_data_set
libmgl      mgl_data_set_vector       ptr ptr                                       (void) mgl_data_set_vector
libmgl      mgl_data_set_matrix       ptr ptr                                       (void) mgl_data_set_matrix
libmgl      mgl_data_get_value        int int int ptr                                 (fp) mgl_data_get_value
libmgl      mgl_data_get_nx           ptr                                            (int) mgl_data_get_nx
libmgl      mgl_data_get_ny           ptr                                            (int) mgl_data_get_ny
libmgl      mgl_data_get_nz           ptr                                            (int) mgl_data_get_nz
libmgl      mgl_data_set_value        int int int sf ptr                            (void) mgl_data_set_value
libmgl      mgl_data_set_values       int int int ptr ptr                           (void) mgl_data_set_values
libmgl      mgl_data_read             ptr ptr                                        (int) mgl_data_read
libmgl      mgl_data_read_mat         int ptr ptr                                    (int) mgl_data_read_mat
libmgl      mgl_data_read_dim         int int int ptr ptr                            (int) mgl_data_read_dim
libmgl      mgl_data_save             int ptr ptr                                   (void) mgl_data_save
libmgl      mgl_data_export           int sf sf ptr ptr ptr                         (void) mgl_data_export
libmgl      mgl_data_import           sf sf ptr ptr ptr                             (void) mgl_data_import
libmgl      mgl_data_create           int int int ptr                               (void) mgl_data_create
libmgl      mgl_data_transpose        ptr ptr                                       (void) mgl_data_transpose
libmgl      mgl_data_norm             int int sf sf ptr                             (void) mgl_data_norm
libmgl      mgl_data_norm_slice       int int ptr sf sf ptr                         (void) mgl_data_norm_slice
libmgl      mgl_data_subdata          int int int ptr                                (ptr) mgl_data_subdata
libmgl      mgl_data_column           ptr ptr                                        (ptr) mgl_data_column
libmgl      mgl_data_set_id           ptr ptr                                       (void) mgl_data_set_id
libmgl      mgl_data_fill             ptr sf sf ptr                                 (void) mgl_data_fill
libmgl      mgl_data_fill_eq          ptr ptr ptr ptr ptr                           (void) mgl_data_fill_eq
libmgl      mgl_data_put_val          int int int sf ptr                            (void) mgl_data_put_val
libmgl      mgl_data_put_dat          int int int ptr ptr                           (void) mgl_data_put_dat
libmgl      mgl_data_modify           int ptr ptr                                   (void) mgl_data_modify
libmgl      mgl_data_modify_vw        ptr ptr ptr ptr                               (void) mgl_data_modify_vw
libmgl      mgl_data_squeeze          int int int int ptr                           (void) mgl_data_squeeze
libmgl      mgl_data_max              ptr                                             (fp) mgl_data_max
libmgl      mgl_data_min              ptr                                             (fp) mgl_data_min
libmgl      mgl_data_value            int int int ptr                                 (fp) mgl_data_value
libmgl      mgl_data_combine          ptr ptr                                        (ptr) mgl_data_combine
libmgl      mgl_data_extend           int int ptr                                   (void) mgl_data_extend
libmgl      mgl_data_insert           int int ptr ptr                               (void) mgl_data_insert
libmgl      mgl_data_delete           int int ptr ptr                               (void) mgl_data_delete
libmgl      mgl_data_smooth           ptr sf int ptr                                (void) mgl_data_smooth
libmgl      mgl_data_sum              ptr ptr                                        (ptr) mgl_data_sum
libmgl      mgl_data_max_dir          ptr ptr                                        (ptr) mgl_data_max_dir
libmgl      mgl_data_min_dir          ptr ptr                                        (ptr) mgl_data_min_dir
libmgl      mgl_data_cumsum           ptr ptr                                       (void) mgl_data_cumsum
libmgl      mgl_data_integral         ptr ptr                                       (void) mgl_data_integral
libmgl      mgl_data_diff             ptr ptr                                       (void) mgl_data_diff
libmgl      mgl_data_diff_par         ptr ptr ptr ptr                               (void) mgl_data_diff_par
libmgl      mgl_data_diff2            ptr ptr                                       (void) mgl_data_diff2
libmgl      mgl_data_swap             ptr ptr                                       (void) mgl_data_swap
libmgl      mgl_data_mirror           ptr ptr                                       (void) mgl_data_mirror
libmgl      mgl_data_spline           sf sf sf ptr                                    (fp) mgl_data_spline
libmgl      mgl_data_spline1          sf sf sf ptr                                    (fp) mgl_data_spline1
libmgl      mgl_data_linear           sf sf sf ptr                                    (fp) mgl_data_linear
libmgl      mgl_data_linear1          sf sf sf ptr                                    (fp) mgl_data_linear1
libmgl      mgl_data_resize           int int int ptr                                (ptr) mgl_data_resize
libmgl      mgl_data_resize_box                                                      (ptr) mgl_data_resize_box
libmgl      mgl_data_hist             int sf sf int ptr                              (ptr) mgl_data_hist
libmgl      mgl_data_hist_w           int sf sf int ptr ptr                          (ptr) mgl_data_hist_w
libmgl      mgl_data_momentum         ptr ptr ptr                                    (ptr) mgl_data_momentum
libmgl      mgl_data_evaluate_i       int ptr ptr                                    (ptr) mgl_data_evaluate_i
libmgl      mgl_data_evaluate_ij      int ptr ptr ptr                                (ptr) mgl_data_evaluate_ij
libmgl      mgl_data_evaluate_ijk     int ptr ptr ptr ptr                            (ptr) mgl_data_evaluate_ijk
libmgl      mgl_data_envelop          ptr ptr                                       (void) mgl_data_envelop
libmgl      mgl_data_sew              sf ptr ptr                                    (void) mgl_data_sew
libmgl      mgl_data_crop             ptr int int ptr                               (void) mgl_data_crop
libmgl      mgl_data_mul_dat          ptr ptr                                       (void) mgl_data_mul_dat
libmgl      mgl_data_div_dat          ptr ptr                                       (void) mgl_data_div_dat
libmgl      mgl_data_add_dat          ptr ptr                                       (void) mgl_data_add_dat
libmgl      mgl_data_sub_dat          ptr ptr                                       (void) mgl_data_sub_dat
libmgl      mgl_data_mul_num          sf ptr                                        (void) mgl_data_mul_num
libmgl      mgl_data_div_num          sf ptr                                        (void) mgl_data_div_num
libmgl      mgl_data_add_num          sf ptr                                        (void) mgl_data_add_num
libmgl      mgl_data_sub_num          sf ptr                                        (void) mgl_data_sub_num
libmgl      mgl_fit_1                 sf ptr ptr ptr ptr ptr                          (fp) mgl_fit_1
libmgl      mgl_fit_2                 sf ptr ptr ptr ptr ptr                          (fp) mgl_fit_2
libmgl      mgl_fit_3                 sf ptr ptr ptr ptr ptr                          (fp) mgl_fit_3
libmgl      mgl_fit_xy                sf ptr ptr ptr ptr ptr ptr                      (fp) mgl_fit_xy
libmgl      mgl_fit_xyz               sf ptr ptr ptr ptr ptr ptr ptr                  (fp) mgl_fit_xyz
libmgl      mgl_fit_xyza              sf ptr ptr ptr ptr ptr ptr ptr ptr              (fp) mgl_fit_xyza
libmgl      mgl_fit_ys                sf ptr ptr ptr ptr ptr ptr                      (fp) mgl_fit_ys
libmgl      mgl_fit_xys               sf ptr ptr ptr ptr ptr ptr ptr                  (fp) mgl_fit_xys
libmgl      mgl_fit_xyzs              sf ptr ptr ptr ptr ptr ptr ptr ptr              (fp) mgl_fit_xyzs
libmgl      mgl_fit_xyzas             sf ptr ptr ptr ptr ptr ptr ptr ptr ptr          (fp) mgl_fit_xyzas
libmgl      mgl_fit_1_d               ptr ptr ptr ptr ptr ptr                         (fp) mgl_fit_1_d
libmgl      mgl_fit_2_d               ptr ptr ptr ptr ptr ptr                         (fp) mgl_fit_2_d
libmgl      mgl_fit_3_d               ptr ptr ptr ptr ptr ptr                         (fp) mgl_fit_3_d
libmgl      mgl_fit_xy_d              ptr ptr ptr ptr ptr ptr ptr                     (fp) mgl_fit_xy_d
libmgl      mgl_fit_xyz_d             ptr ptr ptr ptr ptr ptr ptr ptr                 (fp) mgl_fit_xyz_d
libmgl      mgl_fit_xyza_d            ptr ptr ptr ptr ptr ptr ptr ptr ptr             (fp) mgl_fit_xyza_d
libmgl      mgl_fit_ys_d              ptr ptr ptr ptr ptr ptr ptr                     (fp) mgl_fit_ys_d
libmgl      mgl_fit_xys_d             ptr ptr ptr ptr ptr ptr ptr ptr                 (fp) mgl_fit_xys_d
libmgl      mgl_fit_xyzs_d            ptr ptr ptr ptr ptr ptr ptr ptr ptr             (fp) mgl_fit_xyzs_d
libmgl      mgl_fit_xyzas_d           ptr ptr ptr ptr ptr ptr ptr ptr ptr ptr         (fp) mgl_fit_xyzas_d
libmgl      mgl_puts_fit              sf ptr ptr sf sf sf ptr                       (void) mgl_puts_fit
libmgl      mgl_sphere                ptr sf sf sf sf ptr                           (void) mgl_sphere
libmgl      mgl_drop                  sf sf ptr sf sf sf sf sf sf sf ptr            (void) mgl_drop
libmgl      mgl_cone                  int ptr sf sf sf sf sf sf sf sf ptr           (void) mgl_cone
libmgl      mgl_pde_solve             sf sf ptr ptr ptr ptr                          (ptr) mgl_pde_solve
libmgl      mgl_qo2d_solve            ptr ptr sf sf ptr ptr ptr ptr                  (ptr) mgl_qo2d_solve
libmgl      mgl_af2d_solve            ptr ptr sf sf ptr ptr ptr ptr                  (ptr) mgl_af2d_solve
libmgl      mgl_ray_trace             sf sf sf sf sf sf sf sf ptr                    (ptr) mgl_ray_trace
libmgl      mgl_jacobian_2d           ptr ptr                                        (ptr) mgl_jacobian_2d
libmgl      mgl_jacobian_3d           ptr ptr ptr                                    (ptr) mgl_jacobian_3d
libmgl      mgl_transform_a           ptr ptr ptr                                    (ptr) mgl_transform_a
libmgl      mgl_transform             ptr ptr ptr                                    (ptr) mgl_transform
libmgl      mgl_data_stfa             ptr int ptr ptr                                (ptr) mgl_data_stfa

legacy on

previous
Module;
