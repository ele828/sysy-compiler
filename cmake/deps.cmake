function(fetch_dependencies)
  include(FetchContent OPTIONAL)
  FetchContent_Declare(
    fast_float
    GIT_REPOSITORY https://github.com/fastfloat/fast_float.git
    GIT_TAG tags/v8.0.2
    DOWNLOAD_EXTRACT_TIMESTAMP FALSE
    GIT_SHALLOW TRUE)

  FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG v1.17.0
    DOWNLOAD_EXTRACT_TIMESTAMP FALSE
    GIT_SHALLOW TRUE)
  FetchContent_MakeAvailable(
    fast_float
    googletest)
endfunction()