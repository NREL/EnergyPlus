if(QT_ENABLED)

configure_file(${MathGL2_SOURCE_DIR}/udav/mgl.xml.in ${MathGL2_BINARY_DIR}/udav/mgl.xml)

#set(mgl_wnd_src window.cpp)
set(udav_src anim_dlg.cpp find_dlg.cpp mem_pnl.cpp prop_dlg.cpp textedit.cpp args_dlg.cpp
	help_pnl.cpp newcmd_dlg.cpp text_pnl.cpp calc_dlg.cpp hint_dlg.cpp
	open_dlg.cpp qmglsyntax.cpp udav_wnd.cpp dat_pnl.cpp info_dlg.cpp opt_dlg.cpp setup_dlg.cpp
	files_dlg.cpp plot_pnl.cpp style_dlg.cpp data_dlg.cpp subplot_dlg.cpp )

set(udav_moc_hdr anim_dlg.h files_dlg.h info_dlg.h opt_dlg.h text_pnl.h args_dlg.h
	find_dlg.h mem_pnl.h plot_pnl.h setup_dlg.h udav_wnd.h calc_dlg.h help_pnl.h newcmd_dlg.h
	prop_dlg.h style_dlg.h dat_pnl.h open_dlg.h textedit.h hint_dlg.h data_dlg.h subplot_dlg.h )

set(udav_rc udav.qrc)

if(WIN32)
	set(udav_src ${udav_src} udav.rc)
endif(WIN32)

if(enable-qt5)
	include(../scripts/qt5.cmake)
	qt5_add_resources(udav_rc_src ${udav_rc} )
else(enable-qt5)
	include(../scripts/qt4.cmake)
	qt4_add_resources(udav_rc_src ${udav_rc} )
endif(enable-qt5)
add_executable(udav ${udav_src} ${udav_moc_hdr} ${udav_rc_src})
#set_target_properties(udav PROPERTIES COMPILE_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
if(enable-qt5)
	target_link_libraries(udav mgl-qt5 ${MGL_QT5_LIBS})
else(enable-qt5)
	target_link_libraries(udav mgl-qt4 ${MGL_QT4_LIBS})
endif(enable-qt5)

mgl_po_src(${udav_src} ${udav_hdr})

if(MGL_HAVE_PTHREAD)
	target_link_libraries(udav ${CMAKE_THREAD_LIBS_INIT})
endif(MGL_HAVE_PTHREAD)

install(
	TARGETS udav
	EXPORT MathGLTargets
	RUNTIME DESTINATION ${MathGL_INSTALL_BIN_DIR}
)
install(FILES udav.png DESTINATION ${CMAKE_INSTALL_PREFIX}/share/pixmaps/)
install(FILES udav_ru.qm DESTINATION ${CMAKE_INSTALL_PREFIX}/share/udav/)
find_program(findupddb update-desktop-database)
if(findupddb)
	install(FILES udav.desktop DESTINATION ${CMAKE_INSTALL_PREFIX}/share/applications/)
	install(CODE "execute_process(COMMAND \"${findupddb}\" \"${CMAKE_INSTALL_PREFIX}/share/applications\")")
endif(findupddb)
find_program(findupmdb update-mime-database)
if(findupmdb)
	install(FILES ${CMAKE_BINARY_DIR}/udav/mgl.xml DESTINATION ${CMAKE_INSTALL_PREFIX}/share/mime/packages/)
	install(CODE "execute_process(COMMAND \"${findupmdb}\" \"${CMAKE_INSTALL_PREFIX}/share/mime\")")
endif(findupmdb)
endif(QT_ENABLED)
