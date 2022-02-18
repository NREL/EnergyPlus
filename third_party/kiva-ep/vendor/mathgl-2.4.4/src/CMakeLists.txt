set(mgl_src
	addon.cpp axis.cpp base_cf.cpp base.cpp canvas_cf.cpp canvas.cpp cont.cpp crust.cpp
	complex.cpp complex_ex.cpp complex_io.cpp fft.cpp data_gr.cpp
	data.cpp data_io.cpp data_ex.cpp data_png.cpp
	export_2d.cpp export_3d.cpp eval.cpp evalp.cpp export.cpp
	fit.cpp font.cpp obj.cpp other.cpp parser.cpp pde.cpp pixel.cpp pixel_gen.cpp
	plot.cpp prim.cpp surf.cpp vect.cpp volume.cpp evalc.cpp
	s_hull/s_hull_pro.cpp window.cpp fractal.cpp
	exec_dat.cpp exec_gr.cpp exec_set.cpp exec_prm.cpp c2mdual.c
)

set(mgl_hdr
	../include/mgl2/base_cf.h	../include/mgl2/fit.h		../include/mgl2/plot.h
	../include/mgl2/base.h		../include/mgl2/prim.h		../include/mgl2/canvas_cf.h
	../include/mgl2/font.h		../include/mgl2/canvas.h	../include/mgl2/surf.h
	../include/mgl2/mgl_cf.h	../include/mgl2/type.h		${MathGL2_BINARY_DIR}/include/mgl2/config.h
${MathGL2_BINARY_DIR}/include/mgl2/dllexport.h	cont.hpp
	../include/mgl2/cont.h		../include/mgl2/mgl.h		../include/mgl2/vect.h
	../include/mgl2/data.h		../include/mgl2/volume.h	../include/mgl2/data_cf.h
	../include/mgl2/define.h	../include/mgl2/other.h		../include/mgl2/eval.h
	../include/mgl2/parser.h	../include/mgl2/addon.h		../include/mgl2/evalc.h
	s_hull/s_hull_pro.h			../include/mgl2/wnd.h		../include/mgl2/canvas_wnd.h
	../include/mgl2/thread.h	../include/mgl2/abstract.h	../include/mgl2/pde.h
#	tex_table.cc	 def_font.cc
)

if(MSVC)
	add_definitions(-DMGL_LIB_MSVC)
endif(MSVC)

if(MGL_HAVE_GSL2)
	add_definitions(-DMGL_HAVE_GSL2)
endif(MGL_HAVE_GSL2)

if(MGL_HAVE_PNG)
	set(prc_src prc/PRCbitStream.cc prc/PRCdouble.cc prc/oPRCFile.cc prc/writePRC.cc prc.cpp )
	set(prc_hdr prc/PRC.h prc/PRCbitStream.h prc/PRCdouble.h prc/oPRCFile.h prc/writePRC.h )

	set(mgl_src ${mgl_src} ${prc_src} )
	set(mgl_hdr ${mgl_hdr} ${prc_hdr} )
	include_directories(prc)
endif(MGL_HAVE_PNG)

if(MGL_HAVE_OPENGL)
	set(mgl_src ${mgl_src} opengl.cpp )
	set(mgl_hdr ${mgl_hdr} ../include/mgl2/opengl.h )
endif(MGL_HAVE_OPENGL)

include(GenerateExportHeader)
mgl_add_lib(mgl ${mgl_src} ${mgl_hdr})
generate_export_header(mgl EXPORT_FILE_NAME ../include/mgl2/dllexport.h)

target_link_libraries(mgl ${MGL_DEP_LIBS})
target_link_libraries(mgl-static ${MGL_DEP_LIBS})

mgl_po_src(${mgl_src} ${mgl_hdr})

if(MGL_HAVE_MPI)
	mgl_add_lib(mpi mpi.cpp ../include/mgl2/mpi.h)
	target_link_libraries(mgl-mpi ${MPI_LIBRARIES} )
	target_link_libraries(mgl-mpi-static ${MPI_LIBRARIES} )
	target_include_directories(mgl-mpi SYSTEM PUBLIC ${MPI_CXX_INCLUDE_PATH})
endif(MGL_HAVE_MPI)

#install(FILES ${MathGL2_BINARY_DIR}/include/mgl2/dllexport.h DESTINATION ${MGL_INCLUDE_PATH})
