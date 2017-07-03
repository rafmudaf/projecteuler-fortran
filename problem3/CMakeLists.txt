cmake_minimum_required(VERSION 3.6)
project(problem3)
enable_language(Fortran)

set(SOURCE_FILES main.F90)
add_executable(problem3 ${SOURCE_FILES})