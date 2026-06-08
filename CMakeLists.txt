cmake_minimum_required(VERSION 3.10)

project(Comp LANGUAGES CXX C)

include(FetchContent)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0 -std=c++20")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -std=c++20")

file(GLOB_RECURSE SRC_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")

# Add the executable
add_executable(crag ${SRC_FILES})

# Include the headers
target_include_directories(crag PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

# LLVM compiler flags
execute_process(
    COMMAND llvm-config --cxxflags
    OUTPUT_VARIABLE LLVM_CXXFLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(STRIP "${LLVM_CXXFLAGS}" LLVM_CXXFLAGS_STRIPPED)
string(REPLACE "-fno-exceptions" "" LLVM_CXXFLAGS_STRIPPED "${LLVM_CXXFLAGS_STRIPPED}")
separate_arguments(LLVM_CXXFLAGS_LIST NATIVE_COMMAND "${LLVM_CXXFLAGS_STRIPPED}")
target_compile_options(crag PRIVATE ${LLVM_CXXFLAGS_LIST})

# LLVM linker flags for components
execute_process(
    COMMAND llvm-config --ldflags --libs core support irreader
    OUTPUT_VARIABLE LLVM_LDFLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

string(STRIP "${LLVM_LDFLAGS}" LLVM_LDFLAGS_STRIPPED)
separate_arguments(LLVM_LDFLAGS_LIST NATIVE_COMMAND "${LLVM_LDFLAGS_STRIPPED}")
target_link_libraries(crag PRIVATE ${LLVM_LDFLAGS_LIST})


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
target_link_libraries(crag PRIVATE argparse)

set(RUNTIME_SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/runtime")

# --- Runtime Library ---
file(GLOB RUNTIME_SRC_FILES
    "${RUNTIME_SRC_DIR}/*.crag"
)

make_directory(lib)

#for all runtime source files, add a custom command to compile them into object files

set(RUNTIME_OBJ_FILES)

foreach(RUNTIME_SRC_FILE ${RUNTIME_SRC_FILES})
    get_filename_component(RUNTIME_NAME "${RUNTIME_SRC_FILE}" NAME_WE)

    set(RUNTIME_OBJ_FILE
        "${CMAKE_CURRENT_BINARY_DIR}/lib/${RUNTIME_NAME}.o")

    add_custom_command(
        OUTPUT ${RUNTIME_OBJ_FILE}
        COMMAND $<TARGET_FILE:crag>
                ${RUNTIME_SRC_FILE}
                -o ${RUNTIME_OBJ_FILE}
                --unsafe
                -ORelease
                --no-runtime
        DEPENDS crag ${RUNTIME_SRC_FILE}
    )

    list(APPEND RUNTIME_OBJ_FILES ${RUNTIME_OBJ_FILE})
endforeach()

# Combine all runtime object files into a single object file using ld

message(STATUS "Runtime object files: ${RUNTIME_OBJ_FILES}")
add_custom_command(
    OUTPUT lib/runtime.o
    COMMAND ${CMAKE_LINKER} -r -o lib/runtime.o ${RUNTIME_OBJ_FILES}
    DEPENDS ${RUNTIME_OBJ_FILES}
    COMMENT "Combining runtime object files into lib/runtime.o"
)


# Create a static library for the runtime
set_source_files_properties(lib/runtime.o PROPERTIES GENERATED TRUE)
add_library(runtime STATIC lib/runtime.o)
set_property(TARGET runtime PROPERTY LINKER_LANGUAGE C)
set_target_properties(runtime PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)

message(STATUS "CMAKE_CXX_FLAGS = ${CMAKE_CXX_FLAGS}")
message(STATUS "CMAKE_CXX_FLAGS_DEBUG = ${CMAKE_CXX_FLAGS_DEBUG}")
message(STATUS "CMAKE_CXX_FLAGS_RELEASE = ${CMAKE_CXX_FLAGS_RELEASE}")
message(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")
