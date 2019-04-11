# CMake project file for forts
cmake_minimum_required (VERSION 2.8)

# project name and details
project (FortsRaw)
enable_language (Fortran)
set(PROJECT_DESCRIPTION "FortsRaw")
set(PROJECT_URL "https://git.ccfe.ac.uk/thomasms/fortsraw")

# set source directories
set (SOURCE_DIR_BASE ${CMAKE_CURRENT_LIST_DIR})
set (FORTSRAW_BUILD_DIR ${CMAKE_BINARY_DIR})
set (FORTSRAW_BENCH ${SOURCE_DIR_BASE}/bench)
set (FORTSRAW_EXAMPLES_DIR ${SOURCE_DIR_BASE}/examples)
set (FORTSRAW_EXTERNAL_DIR ${SOURCE_DIR_BASE}/external)
set (FORTSRAW_FORTRAN_LIB_DIR ${SOURCE_DIR_BASE}/fortsraw)
set (FORTSRAW_TEST_DIR ${SOURCE_DIR_BASE}/test)

# library names
set (FORTSRAW_LIB_NAME fortsraw)

# test names
set (FORTSRAW_TEST_NAME fortsrawtests)

# use fork bit notation
#add_definitions(-DFORK_USE_BIT_KIND=1)

# Set release build by default
set (CMAKE_BUILD_TYPE Release CACHE STRING "Select which configuration to build.")
set (FORTSRAW_BUILD_TYPE "Release")
if(CMAKE_BUILD_TYPE)
    set(FORTSRAW_BUILD_TYPE ${CMAKE_BUILD_TYPE})
else()
    set(CMAKE_BUILD_TYPE ${FORTSRAW_BUILD_TYPE})
endif()

# set the general fortran Debug/Release compiler flags
set(FORTSRAW_RELEASE_FLAGS "-O3")
set(FORTSRAW_DEBUG_FLAGS "-g -O0")

set(CMAKE_Fortran_FLAGS_RELEASE ${FORTSRAW_RELEASE_FLAGS})
set(CMAKE_Fortran_FLAGS_DEBUG ${FORTSRAW_DEBUG_FLAGS})

# Fortran OS specific compiler flags
if (CMAKE_Fortran_COMPILER_ID MATCHES MSVC)
    set ( F90_FLAGS_DEBUG "-fp-model")
    set ( F90_FLAGS_RELEASE  "-fp-model" )
elseif (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    set ( F90_FLAGS_DEBUG "-debug  -CB -CA -CU -std08 -fpp -fp-model source")
    set ( F90_FLAGS_RELEASE  "-std08 -unroll -fpp -fp-model source -diag-disable 8291,8577" )
elseif (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set ( F90_FLAGS_DEBUG "-cpp -ffree-form  -ffloat-store -fno-sign-zero -std=f2008 -ffree-line-length-none")
    set ( F90_FLAGS_RELEASE  "-cpp -ffree-form -ffloat-store -fno-sign-zero -std=f2008 -ffree-line-length-none" )
elseif (CMAKE_Fortran_COMPILER_ID MATCHES NAG)
    set ( F90_FLAGS_DEBUG "-fpp -free -float-store -f2008")
    set ( F90_FLAGS_RELEASE  "-fpp -free -float-store -f2008")
else ()
    message(ERROR "OS >>> Fortran Compiler not supported!!")
endif ()

# Release or Debug build
if(FORTSRAW_BUILD_TYPE MATCHES Release)
    message(STATUS "Release build")
    set(F90_FLAGS ${F90_FLAGS_RELEASE})
elseif(FORTSRAW_BUILD_TYPE MATCHES Debug)
    message(STATUS "Debug build")
    set(F90_FLAGS ${F90_FLAGS_DEBUG})
else()
    message(STATUS "Default build (Release)")
    set(F90_FLAGS ${F90_FLAGS_RELEASE})
endif()

# set the output directories
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY bin)

# Collect all the mod files into their own include directory
set(FORTSRAW_MODULE_DIR ${FORTSRAW_BUILD_DIR}/include)

# set include and link directories
include_directories (${FORTSRAW_BUILD_DIR}/include)
link_directories (${FORTSRAW_BUILD_DIR}/lib)

# sources
add_subdirectory(${FORTSRAW_FORTRAN_LIB_DIR})
add_subdirectory(${FORTSRAW_TEST_DIR})
#add_subdirectory(${FORTSRAW_EXAMPLES_DIR})
#add_subdirectory(${FORTSRAW_BENCH})

