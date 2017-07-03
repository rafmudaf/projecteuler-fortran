cmake_minimum_required(VERSION 3.6)
project(problem1)
enable_language(Fortran)

set(FC /usr/local/bin/gfortran)
#set(CMAKE_CXX_STANDARD 11)

set(SOURCE_FILES main.F90)
add_executable(problem1 ${SOURCE_FILES})