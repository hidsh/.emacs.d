# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.5)

if(EXISTS "/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitclone-lastrun.txt" AND EXISTS "/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitinfo.txt" AND
  "/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitclone-lastrun.txt" IS_NEWER_THAN "/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitinfo.txt")
  message(STATUS
    "Avoiding repeated git clone, stamp file is up to date: "
    "'/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitclone-lastrun.txt'"
  )
  return()
endif()

execute_process(
  COMMAND ${CMAKE_COMMAND} -E rm -rf "/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm"
  RESULT_VARIABLE error_code
)
if(error_code)
  message(FATAL_ERROR "Failed to remove directory: '/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm'")
endif()

# try the clone 3 times in case there is an odd git clone issue
set(error_code 1)
set(number_of_tries 0)
while(error_code AND number_of_tries LESS 3)
  execute_process(
    COMMAND "/opt/homebrew/bin/git" 
            clone --no-checkout --config "advice.detachedHead=false" "https://github.com/neovim/libvterm.git" "libvterm"
    WORKING_DIRECTORY "/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src"
    RESULT_VARIABLE error_code
  )
  math(EXPR number_of_tries "${number_of_tries} + 1")
endwhile()
if(number_of_tries GREATER 1)
  message(STATUS "Had to git clone more than once: ${number_of_tries} times.")
endif()
if(error_code)
  message(FATAL_ERROR "Failed to clone repository: 'https://github.com/neovim/libvterm.git'")
endif()

execute_process(
  COMMAND "/opt/homebrew/bin/git" 
          checkout "54c03b21f763fa775a4c0643a9d8326342873179" --
  WORKING_DIRECTORY "/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm"
  RESULT_VARIABLE error_code
)
if(error_code)
  message(FATAL_ERROR "Failed to checkout tag: '54c03b21f763fa775a4c0643a9d8326342873179'")
endif()

set(init_submodules TRUE)
if(init_submodules)
  execute_process(
    COMMAND "/opt/homebrew/bin/git" 
            submodule update --recursive --init 
    WORKING_DIRECTORY "/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm"
    RESULT_VARIABLE error_code
  )
endif()
if(error_code)
  message(FATAL_ERROR "Failed to update submodules in: '/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm'")
endif()

# Complete success, update the script-last-run stamp file:
#
execute_process(
  COMMAND ${CMAKE_COMMAND} -E copy "/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitinfo.txt" "/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitclone-lastrun.txt"
  RESULT_VARIABLE error_code
)
if(error_code)
  message(FATAL_ERROR "Failed to copy script-last-run stamp file: '/Users/g/.emacs.d/packages/vterm-20211226.817/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitclone-lastrun.txt'")
endif()
