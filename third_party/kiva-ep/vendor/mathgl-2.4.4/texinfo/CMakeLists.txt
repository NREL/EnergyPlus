configure_file(${MathGL2_SOURCE_DIR}/texinfo/version.texi.in ${MathGL2_BINARY_DIR}/texinfo/version.texi)

#set(MGL_EXTRA light pendelta)
set(MGL_EXTRA )
set(MGL_PNG_ONLY quality0 quality1 quality2 quality4 quality5 quality6 quality8 pendelta fexport) 
set(MGL_PNG 3wave alpha apde area aspect axial axis barh bars belt bifurcation box boxplot boxs candle chart cloud colorbar combined cones cont cont_xyz cont3 contd contf contf_xyz contf3 conts contv correl curvcoor cut dat_diff dat_extra data1 data2 dens dens_xyz dens3 detect dew diffract dilate dots earth error error2 export fall fit flame2d flow flow3 fog fonts grad hist ifs2d ifs3d indirect inplot iris label lamerey legend light loglog map mark mask mesh minmax mirror molecule ode ohlc param1 param2 param3 paramv parser pde pipe plot pmap primitives projection projection5 pulse qo2d radar refill region scanfile schemes section several_light solve stem step stereo stfa style surf surf3 surf3a surf3c surf3ca surfa surfc surfca table tape tens ternary text text2 textmark ticks tile tiles torus traj triangulation triplot tube type0 type1 type2 vect vect3 venn )
set(MGL_PNG_N )
set(MGL_PNG_S )
set(MGL_PNG_J )
set(MGL_PNG_D )

set(MGL_OUT ${CMAKE_BINARY_DIR}/texinfo)
set(MGL_TEX ${CMAKE_SOURCE_DIR}/texinfo)
file(MAKE_DIRECTORY ${MGL_OUT}/udav)
file(MAKE_DIRECTORY ${MGL_OUT}/png)
file(MAKE_DIRECTORY ${MGL_OUT}/small)
file(MAKE_DIRECTORY ${MGL_OUT}/json)
file(MAKE_DIRECTORY ${MGL_OUT}/pdf)
file(COPY ${MGL_TEX}/qt.png ${MGL_TEX}/fltk.png ${MGL_TEX}/classes.png ${MGL_TEX}/emblem_sm.png ${MGL_TEX}/datadvance.png DESTINATION ${MGL_OUT})
file(COPY ${MGL_TEX}/index.html ${MGL_TEX}/json.html ${MGL_TEX}/mathgl.js DESTINATION ${MGL_OUT})
file(COPY ${CMAKE_SOURCE_DIR}/examples/iris.dat DESTINATION ${MGL_OUT}/png)
file(COPY ${CMAKE_SOURCE_DIR}/examples/iris.dat DESTINATION ${MGL_OUT}/small)
file(COPY ${CMAKE_SOURCE_DIR}/examples/iris.dat DESTINATION ${MGL_OUT}/json)
file(COPY ${CMAKE_SOURCE_DIR}/examples/iris.dat DESTINATION ${MGL_OUT}/pdf)
file(COPY ${CMAKE_SOURCE_DIR}/examples/Equirectangular-projection.jpg DESTINATION ${MGL_OUT}/png)
file(COPY ${CMAKE_SOURCE_DIR}/examples/Equirectangular-projection.jpg DESTINATION ${MGL_OUT}/small)
file(COPY ${CMAKE_SOURCE_DIR}/examples/Equirectangular-projection.jpg DESTINATION ${MGL_OUT}/json)
file(COPY ${CMAKE_SOURCE_DIR}/examples/Equirectangular-projection.jpg DESTINATION ${MGL_OUT}/pdf)
file(COPY ${CMAKE_SOURCE_DIR}/examples/samples.cpp DESTINATION ${MGL_OUT})
file(COPY ${CMAKE_SOURCE_DIR}/examples/wnd_samples.cpp DESTINATION ${MGL_OUT})

set(UDAV_IMG udav_arg.png udav_calc.png udav_cmd.png udav_data.png udav_mask.png
	udav_gen_set.png udav_help.png udav_light.png udav_main.png udav_opt.png udav_inplot.png
	udav_pen.png udav_prop.png udav_sch.png udav_txt.png udav_var.png)
foreach(SAMPLE ${UDAV_IMG})
	file(COPY ${MGL_TEX}/udav/${SAMPLE} DESTINATION ${MGL_OUT}/udav/)
endforeach(SAMPLE)

add_custom_command(OUTPUT ${MGL_OUT}/samples.texi
	COMMAND mgl_example -texi
	DEPENDS mgl_example
	WORKING_DIRECTORY ${MGL_OUT} )
#add_custom_command(OUTPUT ${MGL_OUT}/time.texi
#	COMMAND mgl_example -time
#	DEPENDS mgl_example
#	WORKING_DIRECTORY ${MGL_OUT} )
#add_custom_command(OUTPUT ${MGL_OUT}/time_big.texi
#	COMMAND mgl_example -time -big
#	DEPENDS mgl_example
#	WORKING_DIRECTORY ${MGL_OUT} )


foreach(SAMPLE ${MGL_PNG_ONLY})
	set(MGL_PNG_N ${MGL_PNG_N} ${MGL_OUT}/png/${SAMPLE}.png)
	add_custom_command(OUTPUT ${MGL_OUT}/png/${SAMPLE}.png
		COMMAND mgl_example -kind=${SAMPLE}
		DEPENDS mgl_example
		WORKING_DIRECTORY ${MGL_OUT}/png )
	set(MGL_PNG_S ${MGL_PNG_S} ${MGL_OUT}/small/${SAMPLE}-sm.png)
	add_custom_command(OUTPUT ${MGL_OUT}/small/${SAMPLE}-sm.png
		COMMAND mgl_example -kind=${SAMPLE} -mini
		DEPENDS mgl_example
		WORKING_DIRECTORY ${MGL_OUT}/small )
endforeach(SAMPLE)

foreach(SAMPLE ${MGL_PNG})
	set(MGL_PNG_N ${MGL_PNG_N} ${MGL_OUT}/png/${SAMPLE}.png)
	add_custom_command(OUTPUT ${MGL_OUT}/png/${SAMPLE}.png
		COMMAND mgl_example -kind=${SAMPLE}
		DEPENDS mgl_example
		WORKING_DIRECTORY ${MGL_OUT}/png )
	set(MGL_PNG_S ${MGL_PNG_S} ${MGL_OUT}/small/${SAMPLE}-sm.png)
	add_custom_command(OUTPUT ${MGL_OUT}/small/${SAMPLE}-sm.png
		COMMAND mgl_example -kind=${SAMPLE} -mini
		DEPENDS mgl_example
		WORKING_DIRECTORY ${MGL_OUT}/small )

	if(MGL_HAVE_DOC_JSON)
		set(MGL_PNG_J ${MGL_PNG_J} ${MGL_OUT}/json/${SAMPLE}.json)
		add_custom_command(OUTPUT ${MGL_OUT}/json/${SAMPLE}.json
			COMMAND mgl_example -json -kind=${SAMPLE}
			DEPENDS mgl_example
			WORKING_DIRECTORY ${MGL_OUT}/json )
	endif(MGL_HAVE_DOC_JSON)
	if(MGL_HAVE_DOC_PRC)
		set(MGL_PNG_D ${MGL_PNG_D} ${MGL_OUT}/pdf/${SAMPLE}.pdf)
		add_custom_command(OUTPUT ${MGL_OUT}/pdf/${SAMPLE}.pdf
			COMMAND mgl_example -pdf -kind=${SAMPLE}
			DEPENDS mgl_example
			WORKING_DIRECTORY ${MGL_OUT}/pdf )
	endif(MGL_HAVE_DOC_PRC)
endforeach(SAMPLE)

foreach(SAMPLE ${MGL_EXTRA})
	set(MGL_PNG_N ${MGL_PNG_N} ${MGL_OUT}/png/${SAMPLE}.png)
	add_custom_command(OUTPUT ${MGL_OUT}/png/${SAMPLE}.png
		COMMAND mgl_example -kind=${SAMPLE}
		DEPENDS mgl_example
		WORKING_DIRECTORY ${MGL_OUT}/png )
	set(MGL_PNG_S ${MGL_PNG_S} ${MGL_OUT}/small/${SAMPLE}-sm.png)
	add_custom_command(OUTPUT ${MGL_OUT}/small/${SAMPLE}-sm.png
		COMMAND mgl_example -kind=${SAMPLE} -mini
		DEPENDS mgl_example
		WORKING_DIRECTORY ${MGL_OUT}/small )
endforeach(SAMPLE)

# NOTE: direct generation of time*.texi is too slow :(
set(list_texi_files_en overview_en.texi example_en.texi ex_mgl_en.texi parse_en.texi formats_en.texi udav_en.texi symbols_en.texi core_en.texi concept_en.texi widget_en.texi data_en.texi other_en.texi appendix_en.texi fdl.texi ${MathGL2_BINARY_DIR}/texinfo/version.texi time.texi time_big.texi ${MGL_OUT}/samples.texi )
set(list_texi_files_ru overview_ru.texi example_ru.texi ex_mgl_ru.texi parse_ru.texi formats_ru.texi udav_ru.texi symbols_ru.texi core_ru.texi concept_ru.texi widget_ru.texi data_ru.texi other_ru.texi appendix_ru.texi fdl.texi ${MathGL2_BINARY_DIR}/texinfo/version.texi time.texi time_big.texi ${MGL_OUT}/samples.texi )

add_custom_command(OUTPUT ${MGL_OUT}/mathgl_en.info
	COMMAND ${findmi} ${MGL_TEX}/mathgl_en.texi
	DEPENDS ${list_texi_files_en} mathgl_en.texi #${MGL_PNG_N}
	WORKING_DIRECTORY ${MGL_OUT}
)
add_custom_command(OUTPUT ${MGL_OUT}/mathgl_ru.info
	COMMAND ${findmi} ${MGL_TEX}/mathgl_ru.texi
	DEPENDS ${list_texi_files_ru} mathgl_ru.texi #${MGL_PNG_N}
	WORKING_DIRECTORY ${MGL_OUT}
)

add_custom_command(OUTPUT ${MGL_OUT}/mathgl_en.html
	COMMAND ${findth} ${th_opt} -I ${MGL_OUT} --no-split ${MGL_TEX}/mathgl_en.texi
	DEPENDS ${list_texi_files_en} mathgl_en.texi ${MGL_PNG_N}
	WORKING_DIRECTORY ${MGL_OUT}
)
add_custom_command(OUTPUT ${MGL_OUT}/mgl_en.html
	COMMAND ${findth} ${th_opt} --set-customization-variable 'BODYTEXT ' -I ${MGL_OUT} --no-split ${MGL_TEX}/mgl_en.texi
	# following line bypass FLTK bugs
	COMMAND sed "'s/&ndash;/-/g'" -i ${MGL_OUT}/mgl_en.html
	COMMAND sed "'s/&lsquo;/`/g'" -i ${MGL_OUT}/mgl_en.html
	COMMAND sed "'s/&rsquo;/`/g'" -i ${MGL_OUT}/mgl_en.html
	DEPENDS ${list_texi_files_en} mgl_en.texi ${MGL_PNG_N}
	WORKING_DIRECTORY ${MGL_OUT}
)
add_custom_command(OUTPUT ${MGL_OUT}/mathgl_ru.html
	COMMAND ${findth} ${th_opt} -I=${MGL_OUT} --no-split ${MGL_TEX}/mathgl_ru.texi
	DEPENDS ${list_texi_files_ru} mathgl_ru.texi ${MGL_PNG_N}
	WORKING_DIRECTORY ${MGL_OUT}
)
add_custom_command(OUTPUT ${MGL_OUT}/mgl_ru.html
	COMMAND ${findth} ${th_opt} --set-customization-variable 'BODYTEXT ' -I=${MGL_OUT} --no-split ${MGL_TEX}/mgl_ru.texi
	# following line bypass FLTK bugs
	COMMAND sed "'s/&ndash;/-/g'" -i ${MGL_OUT}/mgl_ru.html
	COMMAND sed "'s/&lsquo;/`/g'" -i ${MGL_OUT}/mgl_ru.html
	COMMAND sed "'s/&rsquo;/`/g'" -i ${MGL_OUT}/mgl_ru.html
	DEPENDS ${list_texi_files_ru} mgl_ru.texi ${MGL_PNG_N}
	WORKING_DIRECTORY ${MGL_OUT}
)

add_custom_command(OUTPUT ${site_en}
	COMMAND ${findth} ${th_opt} -I=${MGL_OUT}/png --split=node -o doc_en ${MGL_TEX}/doc_en.texi
	DEPENDS ${list_texi_files_en} doc_en.texi ${MGL_PNG_N} web_en.texi ${MGL_PNG_S} ${MGL_PNG_J} ${MGL_PNG_D}
	WORKING_DIRECTORY ${MGL_OUT}
)
add_custom_command(OUTPUT ${site_ru}
	COMMAND ${findth} ${th_opt} -I=${MGL_OUT}/png --split=node -o doc_ru ${MGL_TEX}/doc_ru.texi
	DEPENDS ${list_texi_files_ru} doc_ru.texi ${MGL_PNG_N} web_ru.texi ${MGL_PNG_S} ${MGL_PNG_J} ${MGL_PNG_D}
	WORKING_DIRECTORY ${MGL_OUT}
)

add_custom_command(OUTPUT ${MGL_OUT}/mathgl_en.pdf
	COMMAND ${findtp} ${MGL_TEX}/mathgl_en.texi
	COMMAND ${findtp} ${MGL_TEX}/mathgl_en.texi
	COMMAND ${findtp} ${MGL_TEX}/mathgl_en.texi
	DEPENDS ${list_texi_files_en} mathgl_en.texi ${MGL_PNG_N}
	WORKING_DIRECTORY ${MGL_OUT}
)
add_custom_command(OUTPUT ${MGL_OUT}/mgl_en.pdf
	COMMAND ${findtp} ${MGL_TEX}/mgl_en.texi
	COMMAND ${findtp} ${MGL_TEX}/mgl_en.texi
	COMMAND ${findtp} ${MGL_TEX}/mgl_en.texi
	DEPENDS ${list_texi_files_en} mgl_en.texi ${MGL_PNG_N}
	WORKING_DIRECTORY ${MGL_OUT}
)

add_custom_command(OUTPUT ${MGL_OUT}/mathgl_ru.pdf
	COMMAND ${findtp} ${MGL_TEX}/mathgl_ru.texi
	COMMAND ${findtp} ${MGL_TEX}/mathgl_ru.texi
	COMMAND ${findtp} ${MGL_TEX}/mathgl_ru.texi
	DEPENDS ${list_texi_files_ru} mathgl_ru.texi ${MGL_PNG_N}
	WORKING_DIRECTORY ${MGL_OUT}
)
add_custom_command(OUTPUT ${MGL_OUT}/mgl_ru.pdf
	COMMAND ${findtp} ${MGL_TEX}/mgl_ru.texi
	COMMAND ${findtp} ${MGL_TEX}/mgl_ru.texi
	COMMAND ${findtp} ${MGL_TEX}/mgl_ru.texi
	DEPENDS ${list_texi_files_en} mgl_ru.texi ${MGL_PNG_N}
	WORKING_DIRECTORY ${MGL_OUT}
)

if(MGL_HAVE_DOC_INFO)
	add_custom_target(doc_info ALL
		DEPENDS ${MGL_OUT}/mathgl_en.info
		DEPENDS ${MGL_OUT}/mathgl_ru.info
	)
# TODO: try to install all mathgl*.info* in future!!!
	install(FILES ${MGL_OUT}/mathgl_en.info ${MGL_OUT}/mathgl_en.info-1 ${MGL_OUT}/mathgl_en.info-2 ${MGL_OUT}/mathgl_en.info-3 DESTINATION ${MGL_INFO_PATH})
endif(MGL_HAVE_DOC_INFO)

if(MGL_HAVE_DOC_HTML)
	add_custom_target(doc_html ALL
	DEPENDS ${MGL_OUT}/mgl_en.html
	DEPENDS ${MGL_OUT}/mathgl_en.html
	DEPENDS ${MGL_OUT}/mgl_ru.html
	DEPENDS ${MGL_OUT}/mathgl_ru.html
	)
	install(FILES ${MGL_TEX}/qt.png ${MGL_TEX}/fltk.png ${MGL_TEX}/classes.png ${MGL_TEX}/datadvance.png DESTINATION ${MGL_DOC_PATH})
	install(FILES ${MGL_OUT}/mathgl_en.html ${MGL_OUT}/mgl_en.html ${MGL_OUT}/mathgl_ru.html ${MGL_OUT}/mgl_ru.html DESTINATION ${MGL_DOC_PATH})
	install(DIRECTORY ${MGL_OUT}/png DESTINATION ${MGL_DOC_PATH})
	install(DIRECTORY ${MGL_OUT}/udav DESTINATION ${MGL_DOC_PATH})
endif(MGL_HAVE_DOC_HTML)

if(MGL_HAVE_DOC_SITE)
	add_custom_target(doc_site ALL
	DEPENDS ${site_en}
	DEPENDS ${site_ru}
	)
endif(MGL_HAVE_DOC_SITE)

if(MGL_HAVE_DOC_PDF_EN)
	add_custom_target(doc_en ALL
		DEPENDS ${MGL_OUT}/mgl_en.pdf
		DEPENDS ${MGL_OUT}/mathgl_en.pdf
	)
	install(FILES ${MGL_OUT}/mathgl_en.pdf ${MGL_OUT}/mgl_en.pdf DESTINATION ${MGL_DOC_PATH})
endif(MGL_HAVE_DOC_PDF_EN)

if(MGL_HAVE_DOC_PDF_RU)
	add_custom_target(doc_ru ALL
#			DEPENDS ${MGL_OUT}/mgl_ru.pdf
#			DEPENDS ${MGL_OUT}/mathgl_ru.pdf
	)
#		install(FILES ${MGL_OUT}/mathgl_ru.pdf DESTINATION ${MGL_DOC_PATH})
endif(MGL_HAVE_DOC_PDF_RU)

set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "doc_en;doc_ru;./mathgl*.*;./mgl*.*")

install(FILES ${MGL_TEX}/mglconv.1 ${MGL_TEX}/mglview.1 ${MGL_TEX}/udav.1 ${MGL_TEX}/mgl.cgi.1 DESTINATION ${MGL_MAN_PATH}/man1)
install(FILES ${MGL_TEX}/mgl.5 DESTINATION ${MGL_MAN_PATH}/man5)
