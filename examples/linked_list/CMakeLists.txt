project (${FORTSRAW_LL_EXAMPLE})
enable_language (Fortran)

set (F90_SOURCES main.F90)

# set the flags
set_source_files_properties(${F90_SOURCES} PROPERTIES COMPILE_FLAGS "${F90_FLAGS}")

include_directories (${FORTSRAW_INCLUDE_DIR})
link_directories (${FORTSRAW_BUILD_DIR}/lib)

add_executable(${FORTSRAW_LL_EXAMPLE} ${F90_SOURCES})
target_link_libraries (${FORTSRAW_LL_EXAMPLE}
                       ${FORTSRAW_LIB_NAME})
set_target_properties(${FORTSRAW_LL_EXAMPLE}
                        PROPERTIES OUTPUT_NAME ${FORTSRAW_LL_EXAMPLE}
                        LIBRARY_OUTPUT_DIRECTORY ${FORTSRAW_BUILD_DIR}/lib
                        ARCHIVE_OUTPUT_DIRECTORY ${FORTSRAW_BUILD_DIR}/lib
                        Fortran_MODULE_DIRECTORY ${FORTSRAW_MODULE_DIR}
                        RUNTIME_OUTPUT_DIRECTORY ${FORTSRAW_EXAMPLE_BIN_DIR})