project (${FORTSRAW_TEST_NAME})
enable_language (Fortran)

set (EXTERNAL_SOURCES
    ${FORTSRAW_EXTERNAL_DIR}/toast/toast/toast.F90)

set (F90_SOURCES
    ${EXTERNAL_SOURCES}
    test_linked_list_m.F90
    test_driver_m.F90)

# set the flags
set_source_files_properties(${F90_SOURCES} PROPERTIES COMPILE_FLAGS "${F90_FLAGS}")

set (FORTSRAW_TEST_EXE_NAME fortranunittests)

include_directories (${FORTSRAW_BUILD_DIR}/toast/include)
link_directories (${FORTSRAW_BUILD_DIR}/lib 
                  ${FORTSRAW_BUILD_DIR}/toast/lib)

add_executable(${FORTSRAW_TEST_NAME} ${F90_SOURCES})
target_link_libraries (${FORTSRAW_TEST_NAME}
                       toast
                       ${FORTSRAW_LIB_NAME})
set_target_properties(${FORTSRAW_TEST_NAME}
                        PROPERTIES OUTPUT_NAME ${FORTSRAW_TEST_NAME}
                        LIBRARY_OUTPUT_DIRECTORY ${FORTSRAW_BUILD_DIR}/lib
                        ARCHIVE_OUTPUT_DIRECTORY ${FORTSRAW_BUILD_DIR}/lib
                        Fortran_MODULE_DIRECTORY ${FORTSRAW_MODULE_DIR}
                        RUNTIME_OUTPUT_DIRECTORY ${FORTSRAW_BUILD_DIR}/bin)