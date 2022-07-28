macro(initialize_submodules)
    if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/.git")
        # Initialize submodules
        set(git_modules_file "${PROJECT_SOURCE_DIR}/.gitmodules")
        if (EXISTS ${git_modules_file})
            file(STRINGS ${git_modules_file} file_lines)
            foreach(line ${file_lines})
                if (${line} MATCHES "url =")
                    string(REGEX REPLACE "\\s*url = .*/(.*).git" "\\1" submodule "${line}")
                    string(STRIP "${submodule}" submodule)
                    if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${submodule}")
                        message(FATAL_ERROR "Submodule directory \"${CMAKE_CURRENT_SOURCE_DIR}/${submodule}\" does not exist")
                    endif()
                    # Initialize submodule if it hasn't already been cloned
                    if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${submodule}/.git")
                        message(STATUS "Initialize ${submodule} submodule")
                        execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive "${CMAKE_CURRENT_SOURCE_DIR}/${submodule}"
                                WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                                RESULT_VARIABLE GIT_SUBMOD_RESULT)
                        if(NOT GIT_SUBMOD_RESULT EQUAL "0")
                            message(FATAL_ERROR "git submodule update --init --recursive ${CMAKE_CURRENT_SOURCE_DIR}/${submodule} failed with ${GIT_SUBMOD_RESULT}, please checkout submodules")
                        endif()
                    endif()
                endif()
            endforeach()
        endif()

        # Create git hooks
        option(CREATE_GIT_HOOKS "Create git hooks to automatically update submodules." ON)
        if (CREATE_GIT_HOOKS)
            # post-checkout
            if (NOT EXISTS "${PROJECT_SOURCE_DIR}/.git/hooks/post-checkout")
                file(WRITE "${PROJECT_SOURCE_DIR}/.git/hooks/post-checkout"
                        "#!/bin/sh

echo \"Running .git/hooks/post-checkout\"
echo

echo \"git submodule sync --recursive\"
echo
git submodule sync --recursive

echo \"git submodule update --init --recursive\"
git submodule update --init --recursive

"
                        )
            endif()
            # post-merge
            if (NOT EXISTS "${PROJECT_SOURCE_DIR}/.git/hooks/post-merge")
                file(WRITE "${PROJECT_SOURCE_DIR}/.git/hooks/post-merge"
                        "#!/bin/sh

# Note: Merge also happens after pull command.
echo \"Running .git/hooks/post-merge\"
echo

echo \"git submodule sync --recursive\"
echo
git submodule sync --recursive

echo \"git submodule update --init --recursive\"
git submodule update --init --recursive

"
                        )
            endif()
            # pre-push
            if (NOT EXISTS "${PROJECT_SOURCE_DIR}/.git/hooks/pre-push")
                file(WRITE "${PROJECT_SOURCE_DIR}/.git/hooks/pre-push"
                        "#!/bin/sh

echo \"Running .git/hooks/pre-push\"

# Check if any submodules have unpushed commits.
if ! [[ -z $(git submodule --quiet foreach --recursive 'git log --branches --not --remotes') ]]; then
  echo
  echo \"Warning: You have unpushed commits in one or more submodules. Don't forget to\"
  echo \"push them if the parent repository is expecting those changes.\"
fi

"
                        )
            endif()
        endif()
    endif()
endmacro()