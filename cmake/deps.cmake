function (add_dependency name url tag)
  include(FetchContent OPTIONAL)
  message("add dependency: ${name}")
  FetchContent_Declare(
    ${name}
    GIT_REPOSITORY ${url}
    GIT_TAG ${tag}
    GIT_SHALLOW TRUE)
  FetchContent_MakeAvailable(${name})
endfunction()

function(add_dependencies)
  add_dependency(
    magic_enum
    https://github.com/Neargye/magic_enum.git
    tags/v0.9.7)

  add_dependency(
    fast_float
    https://github.com/fastfloat/fast_float.git
    tags/v8.0.2)

  add_dependency(
    googletest
    https://github.com/google/googletest.git
    v1.17.0)
endfunction()