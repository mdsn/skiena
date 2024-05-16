cmake_minimum_required(VERSION 3.26)
project(skiena)

set(CMAKE_CXX_STANDARD 20)

set(SGB ../../sgb)
include_directories(${SGB})
link_directories(/usr/local/lib)

find_library(gb REQUIRED NAMES libgb.a)
find_package(fmt CONFIG REQUIRED)

add_executable(ex03-01 ex03-01.cpp)
add_executable(ex03-03 ex03-03.cpp)
add_executable(ex03-07 ex03-07.cpp)
add_executable(ex03-14 ex03-14.cpp)
target_link_libraries(ex03-14 PRIVATE fmt::fmt)
add_executable(ex03-26 ex03-26.cpp)
add_executable(ex03-28-partial-sums ex03-28-partial-sums.cpp)
add_executable(ex03-29-partial-sums-ii ex03-29-partial-sums-ii.cpp)

add_executable(sgb-example0 sgb-example0.cpp)
target_link_libraries(sgb-example0 PRIVATE gb)