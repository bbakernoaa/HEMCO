# Cmake lists file for HEMCO tests

# Add a test for NcdfUtil
add_executable(TestNcdfUtil TestNcdfUtil.F90)
target_link_libraries(TestNcdfUtil PRIVATE NcdfUtilHco)
add_test(NAME TestNcdfUtil COMMAND TestNcdfUtil)
