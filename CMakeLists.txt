cmake_minimum_required(VERSION 3.10)

project(Comp LANGUAGES CXX C)
set(CMAKE_C_COMPILER clang)
set(CMAKE_CXX_COMPILER clang++)

set(CMAKE_BUILD_TYPE Debug)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O1")

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

# --- Runtime Library ---

file(GLOB_RECURSE RUNTIME_SRC_FILES "${CMAKE_CURRENT_SOURCE_DIR}/builtin/*.c")

add_library(runtime STATIC ${RUNTIME_SRC_FILES})
set_target_properties(runtime PROPERTIES LINKER_LANGUAGE C)

target_include_directories(runtime PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/builtin)

set_target_properties(runtime PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib"
)
target_link_libraries(comp PRIVATE runtime)
