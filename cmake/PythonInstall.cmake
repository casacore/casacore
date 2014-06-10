# - Install Python source files.
#  python_install(source1..sourceN DESTINATION install_dir)
# Install Python source files and byte-compile them in the directory
# ${PYTHON_INSTALL_DIR}/${install_dir}.

# Copyright (C) 2008-2009
# ASTRON (Netherlands Foundation for Research in Astronomy)
# P.O.Box 2, 7990 AA Dwingeloo, The Netherlands, seg@astron.nl
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# $Id: PythonInstall.cmake 23473 2013-01-10 08:02:35Z loose $

# Search for the Python interpreter.
find_package(PythonInterp)

# Derive the Python site-packages installation directory and build directory.
if(PYTHON_EXECUTABLE)
  set(_cmd
    "from distutils.sysconfig import get_python_lib"
    "print(get_python_lib(plat_specific=True, prefix=''))")
  execute_process(
    COMMAND "${PYTHON_EXECUTABLE}" "-c" "${_cmd}"
    OUTPUT_VARIABLE _pydir
    ERROR_VARIABLE _pyerr
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(_pyerr)
    message(FATAL_ERROR "Python command failed:\n${_pyerr}")
  endif(_pyerr)
  set(PYTHON_BUILD_DIR "${CMAKE_BINARY_DIR}/${_pydir}" CACHE PATH 
    "Build directory for Python extensions" FORCE)
  set(PYTHON_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${_pydir}" CACHE PATH 
    "Installation directory for Python extensions" FORCE)
endif(PYTHON_EXECUTABLE)


#
# macro python_install
#
macro(python_install)

  # Precondition check.
  if(NOT PYTHON_EXECUTABLE)
    message(FATAL_ERROR "python_install: Python interpreter not available")
  endif(NOT PYTHON_EXECUTABLE)

  # Parse arguments.
  string(REGEX REPLACE ";?DESTINATION.*" "" _py_files "${ARGN}")
  string(REGEX MATCH "DESTINATION;.*" _dest_dir "${ARGN}")
  string(REGEX REPLACE "^DESTINATION;" "" _dest_dir "${_dest_dir}")

  if(_py_files MATCHES "^$")
    message(FATAL_ERROR "python_install: no sources files specified")
  endif(_py_files MATCHES "^$")
  if(_dest_dir MATCHES "^$" OR _dest_dir MATCHES ";")
    message(FATAL_ERROR "python_install: destination directory invalid")
  endif(_dest_dir MATCHES "^$" OR _dest_dir MATCHES ";")

  # Set python package build/install directory.
  set(_inst_dir "${PYTHON_INSTALL_DIR}/${_dest_dir}")
  set(_build_dir "${PYTHON_BUILD_DIR}/${_dest_dir}")

  # Install and byte-compile each Python file.
  foreach(_py ${_py_files})
    get_filename_component(_py_path ${_py} PATH)
    get_filename_component(_py_abs ${_py} ABSOLUTE)
    # Create a symlink to each Python file; needed to mimic install tree.
    file(MAKE_DIRECTORY ${_build_dir}/${_py_path})
    execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink
      ${_py_abs} ${_build_dir}/${_py})
    install(FILES ${_py} DESTINATION ${_inst_dir}/${_py_path})
    set(_py_code
      "import py_compile"
      "print('-- Byte-compiling: ${_inst_dir}/${_py}')"
      "py_compile.compile('${_inst_dir}/${_py}', doraise=True)")
    install(CODE 
      "execute_process(COMMAND ${PYTHON_EXECUTABLE} -c \"${_py_code}\"
                       RESULT_VARIABLE _result)
       if(NOT _result EQUAL 0)
         message(FATAL_ERROR \"Byte-compilation FAILED: ${_inst_dir}/${_py}\")
       endif(NOT _result EQUAL 0)")
  endforeach(_py ${_py_files})

  # Make sure that there's a __init__.py file in each build/install directory.
  string(REGEX REPLACE "/" ";" _dir_list ${_dest_dir})
  set(_init_dir)
  foreach(_dir ${_dir_list})
    set(_init_dir "${_init_dir}/${_dir}")
    execute_process(COMMAND ${CMAKE_COMMAND} -E touch
      "${PYTHON_BUILD_DIR}${_init_dir}/__init__.py")
    install(CODE 
      "execute_process(COMMAND ${CMAKE_COMMAND} -E touch 
        \"${PYTHON_INSTALL_DIR}${_init_dir}/__init__.py\")")
  endforeach(_dir ${_dir_list})

endmacro(python_install)
