function (add_dependency name url tag)
  include(FetchContent OPTIONAL)
  FetchContent_Declare(
    ${name}
    GIT_REPOSITORY ${url}
    GIT_TAG ${tag}
    DOWNLOAD_EXTRACT_TIMESTAMP FALSE
    GIT_PROGRESS TRUE
    GIT_SHALLOW TRUE)
  FetchContent_MakeAvailable(${name})
endfunction()

function(fetch_dependencies)
  add_dependency(
    fast_float
    https://github.com/fastfloat/fast_float.git
    tags/v8.0.2)

  add_dependency(
    googletest
    https://github.com/google/googletest.git
    v1.17.0)
endfunction()