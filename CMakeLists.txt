cmake_minimum_required(VERSION 3.13)
project(sysy_compiler)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
add_compile_options(-Wall -Wextra -Werror)
add_compile_options(-fno-exceptions -fno-rtti)

include_directories(.)

# sysy library
set(SOURCE_FILES
  parsing/lexer.cc
  parsing/parser.cc
  parsing/token.cc)

add_library(sysy ${SOURCE_FILES})

# sysy binary
add_executable(
  sysy_bin
  bin/main.cc)
target_link_libraries(sysy_bin sysy)

# unit test
include(FetchContent)
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG v1.17.0
  DOWNLOAD_EXTRACT_TIMESTAMP FALSE)
FetchContent_MakeAvailable(googletest)

enable_testing()
add_executable(
  sysy_test
  tests/parsing/parser_test.cc
  tests/parsing/lexer_test.cc)
target_link_libraries(
  sysy_test
  sysy
  GTest::gtest_main)

include(GoogleTest)
gtest_discover_tests(sysy_test)
