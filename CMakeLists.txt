cmake_minimum_required(VERSION 3.13)
project(sysy_compiler)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
add_compile_options(-Wall -Wextra -Werror -Wno-unused-function -Wno-unused-parameter -Wno-unused-const-variable)
add_compile_options(-fno-exceptions -fno-rtti)

include_directories(.)

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/deps.cmake)
add_dependencies()

# sysy library
set(SOURCE_FILES
  base/zone.cc
  parsing/lexer.cc
  parsing/parser.cc
  parsing/token.cc)

add_library(sysy ${SOURCE_FILES})
target_link_libraries(sysy PUBLIC magic_enum)

# sysy binary
add_executable(
  sysy_bin
  bin/main.cc)
target_link_libraries(sysy_bin sysy)

# unit test
enable_testing()
add_executable(
  sysy_test
  tests/base/zone_test.cc
  tests/base/type_casts_test.cc
  tests/parsing/lexer_test.cc
  tests/parsing/parser_test.cc
  tests/parsing/token_test.cc)
target_link_libraries(
  sysy_test
  sysy
  GTest::gtest_main)

# fixtures test
add_executable(
  sysy_fixtures_test 
  tests/parsing/lexer_fixtures.cc)
target_compile_definitions(sysy_fixtures_test
  PUBLIC
  PROJECT_ROOT_PATH="${CMAKE_SOURCE_DIR}")
target_link_libraries(
  sysy_fixtures_test 
  sysy
  GTest::gtest)

include(GoogleTest)
gtest_discover_tests(sysy_test)
gtest_discover_tests(sysy_fixtures_test)
