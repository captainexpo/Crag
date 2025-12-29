cmake_minimum_required(VERSION 3.10)

project(Comp LANGUAGES CXX C)

include(FetchContent)

set(CMAKE_BUILD_TYPE Debug)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O1 -g -std=c++20")

file(GLOB_RECURSE SRC_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")

# Add the executable
add_executable(comp ${SRC_FILES})

# Include the headers
target_include_directories(comp PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

# LLVM compiler flags
execute_process(
    COMMAND llvm-config --cxxflags
    OUTPUT_VARIABLE LLVM_CXXFLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(STRIP "${LLVM_CXXFLAGS}" LLVM_CXXFLAGS_STRIPPED)
string(REPLACE "-fno-exceptions" "" LLVM_CXXFLAGS_STRIPPED "${LLVM_CXXFLAGS_STRIPPED}")
separate_arguments(LLVM_CXXFLAGS_LIST NATIVE_COMMAND "${LLVM_CXXFLAGS_STRIPPED}")
target_compile_options(comp PRIVATE ${LLVM_CXXFLAGS_LIST})

# LLVM linker flags for components
execute_process(
    COMMAND llvm-config --ldflags --libs core support irreader
    OUTPUT_VARIABLE LLVM_LDFLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

string(STRIP "${LLVM_LDFLAGS}" LLVM_LDFLAGS_STRIPPED)
separate_arguments(LLVM_LDFLAGS_LIST NATIVE_COMMAND "${LLVM_LDFLAGS_STRIPPED}")
target_link_libraries(comp PRIVATE ${LLVM_LDFLAGS_LIST})


# find_package(argparse QUIET) 
#
# if (NOT argparse_FOUND)
# find argparse
  FetchContent_Declare(
    argparse
    GIT_REPOSITORY https://github.com/p-ranav/argparse.git
    GIT_TAG v3.2  
  )

  FetchContent_MakeAvailable(argparse)
# endif()
target_link_libraries(comp PRIVATE argparse)

# --- Runtime Library ---
file(GLOB_RECURSE RUNTIME_SRC_FILES "${CMAKE_CURRENT_SOURCE_DIR}/builtin/*.crag")

make_directory(lib)

add_custom_command(
    OUTPUT lib/runtime.o
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/build/comp ARGS ${RUNTIME_SRC_FILES} -o lib/runtime.o --unsafe -ORelease --no-runtime
    DEPENDS ${RUNTIME_SRC_FILES} ${CMAKE_CURRENT_SOURCE_DIR}/build/comp
    COMMENT "Compiling runtime.o"
)

# Create a static library for the runtime
set_source_files_properties(lib/runtime.o PROPERTIES GENERATED TRUE)
add_library(runtime STATIC lib/runtime.o)
set_property(TARGET runtime PROPERTY LINKER_LANGUAGE C)
set_target_properties(runtime PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)

