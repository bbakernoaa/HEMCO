# HEMCO NUOPC Interface

This directory contains the NUOPC (National Unified Operational Prediction Capability) interface for HEMCO (Harmonized Emissions Component). This interface enables HEMCO to be integrated with NUOPC-compliant models using the ESMF (Earth System Modeling Framework) infrastructure.

## Overview

The HEMCO NUOPC interface provides:

1. **ESMF-based I/O capabilities**: Enhanced parallel I/O using ESMF infrastructure for improved performance
2. **ESMF-based regridding**: Direct regridding between ESMF grids without intermediate transformations
3. **Native ESMF grid support**: Operation directly on the host model's native ESMF grid
4. **Backward compatibility**: All existing HEMCO interfaces remain unmodified

## Files

- `HEMCO_NUOPC_GridCompMod.F90`: Main NUOPC grid component interface
- `hcoi_esmf_mod.F90`: Core ESMF interface module
- `hcoi_esmf_config_mod.F90`: Configuration handling for ESMF interface
- `hcoi_esmf_io_mod.F90`: ESMF-based I/O operations
- `hcoi_esmf_regrid_mod.F90`: ESMF-based regridding functionality
- `hcoi_esmf_integration_mod.F90`: Integration utilities
- `hcoi_esmf_main_mod.F90`: Main execution module
- `hcoi_esmf_driver_mod.F90`: Driver interface
- `hcoi_esmf_performance_mod.F90`: Performance utilities

## Build Configuration

To build HEMCO with NUOPC support, use the following CMake configuration:

```bash
mkdir build_nuopc
cd build_nuopc
cmake .. -DBUILD_NUOPC_INTERFACE=ON -DCMAKE_BUILD_TYPE=Release
make -j
```

Alternatively, use the provided deployment script:

```bash
./src/Interfaces/NUOPC/deploy_nuopc.sh
```

## Requirements

- ESMF library (version 8.0 or later)
- CMake (version 3.5 or later)
- Fortran compiler with ESMF support
- NetCDF libraries

## Integration with NUOPC Models

The HEMCO NUOPC interface can be integrated with any NUOPC-compliant model by using the `HEMCO_NUOPC_GridCompMod.F90` as the emissions grid component. This module follows the NUOPC cap pattern and can be connected to the main model through the standard NUOPC infrastructure.

## Configuration

The NUOPC interface uses the same configuration files as the standard HEMCO implementation. No changes are required to existing HEMCO configuration files to use the NUOPC interface.

## Testing

NUOPC-specific tests are available in the test modules:

- `test_hcoi_esmf_mod.F90`: Basic ESMF interface tests
- `test_hcoi_esmf_config_mod.F90`: Configuration tests
- `test_hcoi_esmf_io_mod.F90`: I/O functionality tests
- `test_hcoi_esmf_regrid_mod.F90`: Regridding tests
- `test_hcoi_esmf_integration_mod.F90`: Integration tests
- `test_hcoi_esmf_all_mod.F90`: Comprehensive tests
- `test_hcoi_esmf_main.F90`: Main test driver
- `test_nuopc_integration.F90`: NUOPC-specific integration tests

## Performance Considerations

The NUOPC interface provides several performance improvements:

1. **Parallel I/O**: ESMF-based I/O operations are optimized for parallel execution
2. **Direct regridding**: Eliminates intermediate grid transformations
3. **Memory efficiency**: Reduces memory footprint by using native ESMF grids

## Troubleshooting

1. **ESMF not found**: Ensure ESMF environment variables are set (ESMF_INSTALL_ROOT or ESMF_ROOT)
2. **Linking errors**: Verify that ESMF libraries are properly linked during build
3. **Runtime errors**: Check that ESMF libraries are available in the runtime environment

## Maintainers

For questions about the HEMCO NUOPC interface, contact the HEMCO development team.
