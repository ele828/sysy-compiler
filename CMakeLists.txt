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
  ast/ast.cc
  ast/ast_context.cc
  base/zone.cc
  common/global_context.cc
  common/type.cc
  ir/argument.cc
  ir/basic_block.cc
  ir/constant.cc
  ir/constant_data.cc
  ir/function.cc
  ir/global_variable.cc
  ir/instruction.cc
  ir/ir_builder.cc
  ir/module.cc
  ir/use.cc
  ir/user.cc
  ir/value.cc
  parse/lexer.cc
  parse/parser.cc
  parse/token.cc
  sema/evaluator.cc
  sema/scope.cc
  sema/sema.cc)

add_library(sysy ${SOURCE_FILES})
target_link_libraries(sysy PUBLIC magic_enum)

# sysy runtime library
set(RUNTIME_FILES
  runtime/sysy_lib.cc)
add_library(sysy_runtime ${RUNTIME_FILES})

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
  tests/parse/lexer_test.cc
  tests/parse/parser_test.cc
  tests/parse/token_test.cc
  tests/sema/evaluator_test.cc
  tests/sema/sema_test.cc)
target_link_libraries(
  sysy_test
  sysy
  GTest::gtest_main)

# fixtures test
add_executable(
  sysy_fixtures_test
  tests/fixtures_test.cc)
target_compile_definitions(sysy_fixtures_test
  PUBLIC
  PROJECT_ROOT_PATH="${CMAKE_SOURCE_DIR}")
target_link_libraries(
  sysy_fixtures_test 
  sysy
  sysy_runtime
  GTest::gtest)

include(GoogleTest)
gtest_discover_tests(sysy_test)
gtest_discover_tests(sysy_fixtures_test)
