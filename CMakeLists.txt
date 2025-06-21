cmake_minimum_required(VERSION 3.13)
project(sysy_compiler)
set(CMAKE_CXX_STANDARD 23)
add_compile_options(-Wall -Wextra -Werror)

set(SOURCE_FILES
  bin/main.cc
  parsing/parser.cc
  parsing/scanner.cc
)

add_executable(sysy ${SOURCE_FILES})
target_include_directories(sysy PRIVATE .)
