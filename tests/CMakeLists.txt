include(CTest)

add_executable(test_core test_core.F90)
target_link_libraries(test_core PRIVATE HCO GeosUtilHco JulDayHco HeadersHco NcdfUtilHco ${NETCDF_LIBRARIES} OpenMP::OpenMP_Fortran)
target_include_directories(test_core PRIVATE ${CMAKE_BINARY_DIR}/mod)

add_test(NAME test_core COMMAND test_core)
