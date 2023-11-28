cmake_minimum_required (VERSION 3.10...3.22)
project(mini-c VERSION 0.1.0 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 17)

include(CTest)
enable_testing()

add_executable(mini-c 
    def.h 
    lex.yy.c 
    parser.tab.hpp 
    parser.tab.cpp
    ast.cpp
    semantics.cpp
    gen_ir.cpp
)

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
