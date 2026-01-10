#!/bin/bash
set -e

#------------------------------------------------------------------------------
#                  GEOS-Chem Global Chemical Transport Model                  !
#
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: integration_test.sh
#
# !DESCRIPTION: Runs an integration test for the HEMCO standalone model.
#
# !CALLING SEQUENCE:
#  ./integration_test.sh /path/to/build_dir
#
# !REVISION HISTORY:
#  2024/07/25 A. Agent    - Initial version
#EOP
#------------------------------------------------------------------------------
#
#==============================================================================
# --- Main program ---
#==============================================================================

# Argument is the build directory
BUILD_DIR=$1

# Source code directory
SRC_DIR=$(pwd)

# Create a temporary run directory
RUN_DIR=$(mktemp -d)
cd ${RUN_DIR}

# Copy run files from the source `run` directory
cp ${SRC_DIR}/run/HEMCO_Config.rc.sample HEMCO_Config.rc
cp ${SRC_DIR}/run/HEMCO_sa_Config.template HEMCO_sa_Config.rc
cp ${SRC_DIR}/run/HEMCO_sa_Spec.rc .
cp ${SRC_DIR}/run/HEMCO_sa_Time.rc .
cp ${SRC_DIR}/run/download_data.py .
cp ${SRC_DIR}/run/download_data.yml .
cp ${SRC_DIR}/run/HEMCO_Diagn.rc.sample HEMCO_Diagn.rc

# Non-interactively configure the run for MERRA-2, 4x5 resolution
sed -i 's/{DATA_ROOT}/./g' HEMCO_sa_Config.rc
sed -i 's/{GRID_FILE}/HEMCO_sa_Grid.4x5.rc/g' HEMCO_sa_Config.rc
sed -i 's/{MET_NAME}/MERRA2/g' HEMCO_sa_Config.rc
sed -i 's/{GRID_RES}/4x5/g' HEMCO_sa_Config.rc
sed -i 's/{DATA_ROOT}/./g' HEMCO_Config.rc
sed -i 's/{GRID_DIR}/4x5/g' HEMCO_Config.rc
sed -i 's/{MET_DIR}/MERRA2/g' HEMCO_Config.rc
cp ${SRC_DIR}/run/HEMCO_sa_Grid.4x5.rc .

# Install Python dependencies and download data
python3 -m pip install --user pyyaml
./download_data.py

# Link the executable
ln -s ${BUILD_DIR}/hemco_standalone .

# Run HEMCO
./hemco_standalone > HEMCO.log

# Verify the run was successful
if grep -q "E n d i n g   H E M C O" HEMCO.log; then
  echo "Integration test successful!"
  exit 0
else
  echo "Integration test failed!"
  exit 1
fi
