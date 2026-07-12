# SysY Compiler [![macOS C++ CI](https://github.com/ele828/sysy-compiler/actions/workflows/test.yml/badge.svg)](https://github.com/ele828/sysy-compiler/actions/workflows/test.yml)
A compiler for SysY language, which is a subset of C language.

## Build
```
cmake -B build -GNinja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
cmake --build build
```

## Run
```
./build/sysy
```

## Test
```
ctest --test-dir build
```
