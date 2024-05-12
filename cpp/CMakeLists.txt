cmake_minimum_required(VERSION 3.26)
project(skiena)

set(CMAKE_CXX_STANDARD 20)

find_package(fmt CONFIG REQUIRED)

add_executable(ex03-01 ex03-01.cpp)
add_executable(ex03-03 ex03-03.cpp)
add_executable(ex03-07 ex03-07.cpp)
add_executable(ex03-14 ex03-14.cpp)
target_link_libraries(ex03-14 PRIVATE fmt::fmt)
add_executable(ex03-26 ex03-26.cpp)
add_executable(ex03-28-partial-sums ex03-28-partial-sums.cpp)
add_executable(ex03-29-partial-sums-ii ex03-29-partial-sums-ii.cpp)
