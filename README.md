# SysY Compiler
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
