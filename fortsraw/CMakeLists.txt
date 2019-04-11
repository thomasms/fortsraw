project (${FORTSRAW_LIB_NAME})
enable_language (Fortran)

set (F90_SOURCES
    ${FORTSRAW_EXTERNAL_DIR}/fork/fork_m.F90
    ${FORTSRAW_FORTRAN_LIB_DIR}/linked_list/linked_list_m.F90
    ${FORTSRAW_FORTRAN_LIB_DIR}/fortsraw.F90)

# set the flags
set_source_files_properties(${F90_SOURCES} PROPERTIES COMPILE_FLAGS "${F90_FLAGS}")

add_library(${FORTSRAW_LIB_NAME} ${F90_SOURCES})
target_link_libraries (${FORTSRAW_LIB_NAME})
set_target_properties(${FORTSRAW_LIB_NAME}
                        PROPERTIES OUTPUT_NAME ${FORTSRAW_LIB_NAME}
                        LIBRARY_OUTPUT_DIRECTORY ${FORTSRAW_BUILD_DIR}/lib
                        ARCHIVE_OUTPUT_DIRECTORY ${FORTSRAW_BUILD_DIR}/lib
                        Fortran_MODULE_DIRECTORY ${FORTSRAW_MODULE_DIR})
