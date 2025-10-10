# HEMCO/src/Interfaces/NUOPC/CMakeLists.txt

# NUOPC Interface for HEMCO
# This interface provides NUOPC-compliant grid component for HEMCO

# Add the NUOPC interface module
set(NUOPC_INTERFACE_MODS
    ${CMAKE_CURRENT_SOURCE_DIR}/HEMCO_NUOPC_GridCompMod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/hcoi_esmf_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/hcoi_esmf_config_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/hcoi_esmf_integration_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/hcoi_esmf_io_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/hcoi_esmf_regrid_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/hcoi_esmf_main_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/hcoi_esmf_performance_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/hcoi_esmf_driver_mod.F90
 )

# Add test modules
set(NUOPC_TEST_MODS
    ${CMAKE_CURRENT_SOURCE_DIR}/test_hcoi_esmf_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/test_hcoi_esmf_config_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/test_hcoi_esmf_io_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/test_hcoi_esmf_regrid_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/test_hcoi_esmf_integration_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/test_hcoi_esmf_all_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/test_hcoi_esmf_main.F90
)

# Add all NUOPC modules
set(HEMCO_NUOPC_MODS
    ${NUOPC_INTERFACE_MODS}
    ${NUOPC_TEST_MODS}
)

# Add the NUOPC interface source files to the parent list
set(HEMCO_ALL_MODS ${HEMCO_ALL_MODS} ${HEMCO_NUOPC_MODS} PARENT_SCOPE)

# Install resource files
install(FILES 
    ${CMAKE_CURRENT_SOURCE_DIR}/HEMCO_NUOPC_GridComp.rc
    ${CMAKE_CURRENT_SOURCE_DIR}/HEMCO_NUOPC_Registry.rc
    DESTINATION ${CMAKE_INSTALL_PREFIX}/share/hemco/nuopc
)