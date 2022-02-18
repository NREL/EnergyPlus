include(GetPrerequisites)

message("\nInstalling depended libraries to ${CMAKE_INSTALL_PREFIX}/bin:\n")

file(GLOB_RECURSE exe_list "${CMAKE_INSTALL_PREFIX}/*.exe")
file(GLOB_RECURSE dll_list "${CMAKE_INSTALL_PREFIX}/*.dll")

if(NOT (exe_list OR dll_list))
	message("Exe and dll files not found, cannot generate dependency list")
	return()
endif(NOT (exe_list OR dll_list))

set(mgl_all_dep)
foreach(exe_file ${exe_list} ${dll_list})
	get_prerequisites(${exe_file} DEPENDENCIES 1 1 "" "${CMAKE_INSTALL_PREFIX}/bin")
	foreach(DEPENDENCY_FILE ${DEPENDENCIES})
		gp_resolve_item("${exe_file}" "${DEPENDENCY_FILE}" "" "${CMAKE_INSTALL_PREFIX}/bin" resolved_file)
		if(NOT resolved_file MATCHES ".*libmgl.*")
			list(APPEND mgl_all_dep ${resolved_file})
		endif(NOT resolved_file MATCHES ".*libmgl.*")
	endforeach(DEPENDENCY_FILE ${DEPENDENCIES})
endforeach(exe_file ${exe_list})

list(REMOVE_DUPLICATES mgl_all_dep)
list(SORT mgl_all_dep)

foreach(dll_file ${mgl_all_dep})
	message("Installing: ${dll_file}")
endforeach(dll_file ${mgl_all_dep})

file(COPY ${mgl_all_dep} DESTINATION "${CMAKE_INSTALL_PREFIX}/bin")

if(mgl_qt_loc)
	message("Installing: ${mgl_qt_loc}")
	file(COPY ${mgl_qt_loc} DESTINATION "${CMAKE_INSTALL_PREFIX}/bin/plugins/platforms")
endif(mgl_qt_loc)
