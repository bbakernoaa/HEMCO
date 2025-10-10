!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: test_hcoi_esmf_config_mod
!
! !DESCRIPTION: Module TEST\_HCOI\_ESMF\_CONFIG\_MOD provides unit tests for the 
! simplified ESMF configuration module (HCOI\_ESMF\_CONFIG\_MOD) in the HEMCO-NUOPC integration.
!\\
!\\
! !INTERFACE:
!
MODULE TEST_HCOI_ESMF_Config_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
 USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE HCOI_ESMF_Config_Mod
  
  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: TEST_HCOI_ESMF_Config_RunAllTests
  PUBLIC :: TEST_HCOI_ESMF_Config_TestInit
  PUBLIC :: TEST_HCOI_ESMF_Config_TestRead
  PUBLIC :: TEST_HCOI_ESMF_Config_TestGetOpt
  PUBLIC :: TEST_HCOI_ESMF_Config_TestSetOpt
  PUBLIC :: TEST_HCOI_ESMF_Config_TestValidate
 PUBLIC :: TEST_HCOI_ESMF_Config_TestApply
  PUBLIC :: TEST_HCOI_ESMF_Config_TestCleanup
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial version for HEMCO-NUOPC configuration tests
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Test results structure
  TYPE :: ConfigTestResults
     INTEGER                :: TotalTests
     INTEGER                :: PassedTests
     INTEGER                :: FailedTests
     LOGICAL                :: AllTestsPassed
 END TYPE ConfigTestResults
  
  ! Global test results instance
 TYPE(ConfigTestResults), POINTER :: ConfigTestResultsInstance => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Config_RunAllTests
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_CONFIG\_RunAllTests executes
! all unit tests for the HCOI\_ESMF\_CONFIG\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Config_RunAllTests( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,         INTENT(  OUT)  :: TestPassed   ! Whether all tests passed
    INTEGER,         INTENT(INOUT) :: RC           ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    LOGICAL             :: individualTestPassed
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Config_RunAllTests (test_hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Allocate test results instance if not already allocated
    IF ( .NOT. ASSOCIATED(ConfigTestResultsInstance) ) THEN
       ALLOCATE(ConfigTestResultsInstance)
    ENDIF
    
    ! Initialize test results
    ConfigTestResultsInstance%TotalTests = 0
    ConfigTestResultsInstance%PassedTests = 0
    ConfigTestResultsInstance%FailedTests = 0
    ConfigTestResultsInstance%AllTestsPassed = .TRUE.
    
    ! Display test header
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '==============================================='
       WRITE(*,*) '  HEMCO ESMF Configuration Module Unit Tests  '
       WRITE(*,*) '==============================================='
    ENDIF
    
    ! Test 1: Initialization
    ConfigTestResultsInstance%TotalTests = ConfigTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Config_TestInit( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ConfigTestResultsInstance%PassedTests = ConfigTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Configuration Initialization Test'
    ELSE
       ConfigTestResultsInstance%FailedTests = ConfigTestResultsInstance%FailedTests + 1
       ConfigTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Configuration Initialization Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 2: Configuration read
    ConfigTestResultsInstance%TotalTests = ConfigTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Config_TestRead( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ConfigTestResultsInstance%PassedTests = ConfigTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Configuration Read Test'
    ELSE
       ConfigTestResultsInstance%FailedTests = ConfigTestResultsInstance%FailedTests + 1
       ConfigTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Configuration Read Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 3: Get option
    ConfigTestResultsInstance%TotalTests = ConfigTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Config_TestGetOpt( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ConfigTestResultsInstance%PassedTests = ConfigTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Get Option Test'
    ELSE
       ConfigTestResultsInstance%FailedTests = ConfigTestResultsInstance%FailedTests + 1
       ConfigTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Get Option Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 4: Set option
    ConfigTestResultsInstance%TotalTests = ConfigTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Config_TestSetOpt( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ConfigTestResultsInstance%PassedTests = ConfigTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Set Option Test'
    ELSE
       ConfigTestResultsInstance%FailedTests = ConfigTestResultsInstance%FailedTests + 1
       ConfigTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Set Option Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 5: Validation
    ConfigTestResultsInstance%TotalTests = ConfigTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Config_TestValidate( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ConfigTestResultsInstance%PassedTests = ConfigTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Validation Test'
    ELSE
       ConfigTestResultsInstance%FailedTests = ConfigTestResultsInstance%FailedTests + 1
       ConfigTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Validation Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 6: Apply configuration
    ConfigTestResultsInstance%TotalTests = ConfigTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Config_TestApply( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ConfigTestResultsInstance%PassedTests = ConfigTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Apply Configuration Test'
    ELSE
       ConfigTestResultsInstance%FailedTests = ConfigTestResultsInstance%FailedTests + 1
       ConfigTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Apply Configuration Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 7: Cleanup
    ConfigTestResultsInstance%TotalTests = ConfigTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Config_TestCleanup( individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ConfigTestResultsInstance%PassedTests = ConfigTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Configuration Cleanup Test'
    ELSE
       ConfigTestResultsInstance%FailedTests = ConfigTestResultsInstance%FailedTests + 1
       ConfigTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Configuration Cleanup Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Display test summary
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '==============================================='
       WRITE(*,*) 'Test Results: ', ConfigTestResultsInstance%PassedTests, '/', &
                  ConfigTestResultsInstance%TotalTests, ' tests passed'
       IF ( ConfigTestResultsInstance%FailedTests > 0 ) THEN
          WRITE(*,*) 'WARNING: ', ConfigTestResultsInstance%FailedTests, ' tests failed'
       ENDIF
       WRITE(*,*) '==============================================='
    ENDIF
    
    ! Set return code based on test results
    TestPassed = ConfigTestResultsInstance%AllTestsPassed
    IF ( TestPassed ) THEN
       RC = HCO_SUCCESS
    ELSE
       RC = HCO_FAIL
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Config_RunAllTests
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Config_TestInit
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_CONFIG\_TestInit tests the
! initialization functionality of the HCOI\_ESMF\_CONFIG\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Config_TestInit( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,         INTENT(  OUT)  :: TestPassed   ! Whether test passed
    INTEGER,         INTENT(INOUT)  :: RC           ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER             :: initialRC
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Config_TestInit (test_hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Test initialization with valid HEMCO state
    initialRC = HCO_SUCCESS
    CALL HCOI_ESMF_Config_Init( HcoState, initialRC )
    
    ! Check if initialization was successful
    IF ( initialRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Configuration initialization test PASSED'
       ELSE
          WRITE(*,*) 'Configuration initialization test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Config_TestInit
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Config_TestRead
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_CONFIG\_TestRead tests the
! configuration read functionality of the HCOI\_ESMF\_CONFIG\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Config_TestRead( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,         INTENT(  OUT)  :: TestPassed   ! Whether test passed
    INTEGER,         INTENT(INOUT)  :: RC           ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER             :: readRC
    CHARACTER(LEN=255)  :: testConfigFile
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Config_TestRead (test_hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Create a temporary configuration file for testing
    testConfigFile = 'temp_test_config.txt'
    
    ! Create a simple test configuration file
    OPEN(UNIT=99, FILE=TRIM(testConfigFile), STATUS='REPLACE', ACTION='WRITE')
    WRITE(99,*) '# Test configuration file for ESMF integration'
    WRITE(99,*) 'USE_NATIVE_GRID = .TRUE.'
    WRITE(99,*) 'ENABLE_CACHING = .TRUE.'
    WRITE(99,*) 'ENABLE_OPTIMIZATION = .TRUE.'
    WRITE(99,*) 'INTEGRATION_MODE = 1'
    WRITE(99,*) 'REGRID_METHOD = 1'
    WRITE(99,*) 'MAX_CACHE_ENTRIES = 25'
    WRITE(99,*) 'CACHE_THRESHOLD = 0.75'
    WRITE(99,*) 'VERBOSE_OUTPUT = .FALSE.'
    WRITE(99,*) 'DEBUG_MODE = .FALSE.'
    CLOSE(99)
    
    ! Initialize configuration first
    CALL HCOI_ESMF_Config_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       ! Clean up the temporary file
       OPEN(UNIT=99, FILE=TRIM(testConfigFile), STATUS='OLD')
       CLOSE(UNIT=99, STATUS='DELETE')
       RETURN
    ENDIF
    
    ! Test reading configuration from file
    readRC = HCO_SUCCESS
    CALL HCOI_ESMF_Config_Read( HcoState, TRIM(testConfigFile), readRC )
    
    ! Check if reading was successful
    IF ( readRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Clean up the temporary file
    OPEN(UNIT=99, FILE=TRIM(testConfigFile), STATUS='OLD')
    CLOSE(UNIT=99, STATUS='DELETE')
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Configuration read test PASSED'
       ELSE
          WRITE(*,*) 'Configuration read test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Config_TestRead
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Config_TestGetOpt
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_CONFIG\_TestGetOpt tests the
! get option functionality of the HCOI\_ESMF\_CONFIG\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Config_TestGetOpt( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,         INTENT(  OUT)  :: TestPassed   ! Whether test passed
    INTEGER,         INTENT(INOUT)  :: RC           ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    LOGICAL             :: logicalValue
    INTEGER             :: intValue
    REAL(hp)            :: realValue
    INTEGER             :: getRC
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Config_TestGetOpt (test_hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Initialize configuration first
    CALL HCOI_ESMF_Config_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       RETURN
    ENDIF
    
    ! Test getting a logical option
    getRC = HCO_SUCCESS
    CALL HCOI_ESMF_Config_GetOpt( 'USE_NATIVE_GRID', logicalValue, getRC )
    IF ( getRC /= HCO_SUCCESS ) THEN
       TestPassed = .FALSE.
    ELSE
       ! Test getting an integer option
       getRC = HCO_SUCCESS
       CALL HCOI_ESMF_Config_GetOpt( 'INTEGRATION_MODE', intValue, getRC )
       IF ( getRC /= HCO_SUCCESS ) THEN
          TestPassed = .FALSE.
       ELSE
          ! Test getting a real option
          getRC = HCO_SUCCESS
          CALL HCOI_ESMF_Config_GetOpt( 'CACHE_THRESHOLD', realValue, getRC )
          IF ( getRC /= HCO_SUCCESS ) THEN
             TestPassed = .FALSE.
          ELSE
             TestPassed = .TRUE.
          ENDIF
       ENDIF
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Get option test PASSED'
       ELSE
          WRITE(*,*) 'Get option test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Config_TestGetOpt
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Config_TestSetOpt
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_CONFIG\_TestSetOpt tests the
! set option functionality of the HCOI\_ESMF\_CONFIG\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Config_TestSetOpt( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,         INTENT(  OUT)  :: TestPassed   ! Whether test passed
    INTEGER,         INTENT(INOUT)  :: RC           ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    LOGICAL             :: logicalValue
    INTEGER             :: intValue
    REAL(hp)            :: realValue
    INTEGER             :: setRC, getRC
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Config_TestSetOpt (test_hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Initialize configuration first
    CALL HCOI_ESMF_Config_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       RETURN
    ENDIF
    
    ! Test setting a logical option
    setRC = HCO_SUCCESS
    logicalValue = .FALSE.
    CALL HCOI_ESMF_Config_SetOpt( 'USE_NATIVE_GRID', logicalValue, setRC )
    IF ( setRC /= HCO_SUCCESS ) THEN
       TestPassed = .FALSE.
    ELSE
       ! Verify the value was set by getting it back
       getRC = HCO_SUCCESS
       CALL HCOI_ESMF_Config_GetOpt( 'USE_NATIVE_GRID', logicalValue, getRC )
       IF ( getRC /= HCO_SUCCESS .OR. logicalValue /= .FALSE. ) THEN
          TestPassed = .FALSE.
       ELSE
          ! Test setting an integer option
          setRC = HCO_SUCCESS
          intValue = 2  ! HCOI_CONFIG_MODE_OPTIMIZED
          CALL HCOI_ESMF_Config_SetOpt( 'INTEGRATION_MODE', intValue, setRC )
          IF ( setRC /= HCO_SUCCESS ) THEN
             TestPassed = .FALSE.
          ELSE
             ! Verify the value was set
             getRC = HCO_SUCCESS
             CALL HCOI_ESMF_Config_GetOpt( 'INTEGRATION_MODE', intValue, getRC )
             IF ( getRC /= HCO_SUCCESS .OR. intValue /= 2 ) THEN
                TestPassed = .FALSE.
             ELSE
                ! Test setting a real option
                setRC = HCO_SUCCESS
                realValue = 0.9_hp
                CALL HCOI_ESMF_Config_SetOpt( 'CACHE_THRESHOLD', realValue, setRC )
                IF ( setRC /= HCO_SUCCESS ) THEN
                   TestPassed = .FALSE.
                ELSE
                   ! Verify the value was set
                   getRC = HCO_SUCCESS
                   CALL HCOI_ESMF_Config_GetOpt( 'CACHE_THRESHOLD', realValue, getRC )
                   IF ( getRC /= HCO_SUCCESS .OR. ABS(realValue - 0.9_hp) > 1.0e-6_hp ) THEN
                      TestPassed = .FALSE.
                   ELSE
                      TestPassed = .TRUE.
                   ENDIF
                ENDIF
             ENDIF
          ENDIF
       ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Set option test PASSED'
       ELSE
          WRITE(*,*) 'Set option test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Config_TestSetOpt
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Config_TestValidate
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_CONFIG\_TestValidate tests the
! validation functionality of the HCOI\_ESMF\_CONFIG\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Config_TestValidate( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,         INTENT(  OUT)  :: TestPassed   ! Whether test passed
    INTEGER,         INTENT(INOUT)  :: RC           ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    LOGICAL             :: valid
    INTEGER             :: validateRC
    CHARACTER(LEN=255) :: LOC = 'TEST_HCOI_ESMF_Config_TestValidate (test_hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Initialize configuration first
    CALL HCOI_ESMF_Config_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       RETURN
    ENDIF
    
    ! Test validation with valid configuration
    validateRC = HCO_SUCCESS
    CALL HCOI_ESMF_Config_Validate( HcoState, valid, validateRC )
    
    ! Check if validation was successful and configuration is valid
    IF ( validateRC == HCO_SUCCESS .AND. valid ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Configuration validation test PASSED'
       ELSE
          WRITE(*,*) 'Configuration validation test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Config_TestValidate
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Config_TestApply
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_CONFIG\_TestApply tests the
! apply configuration functionality of the HCOI\_ESMF\_CONFIG\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Config_TestApply( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,         INTENT(  OUT)  :: TestPassed   ! Whether test passed
    INTEGER,         INTENT(INOUT)  :: RC           ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER             :: applyRC
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Config_TestApply (test_hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Initialize configuration first
    CALL HCOI_ESMF_Config_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       RETURN
    ENDIF
    
    ! Test applying configuration
    applyRC = HCO_SUCCESS
    CALL HCOI_ESMF_Config_Apply( HcoState, applyRC )
    
    ! Check if applying was successful
    IF ( applyRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Apply configuration test PASSED'
       ELSE
          WRITE(*,*) 'Apply configuration test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Config_TestApply
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Config_TestCleanup
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_CONFIG\_TestCleanup tests the
! cleanup functionality of the HCOI\_ESMF\_CONFIG\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Config_TestCleanup( TestPassed, RC )
!
! !ARGUMENTS:
!
    LOGICAL,         INTENT(  OUT)  :: TestPassed   ! Whether test passed
    INTEGER,         INTENT(INOUT)  :: RC           ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER             :: cleanupRC
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Config_TestCleanup (test_hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Test cleanup function
    cleanupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Config_Cleanup( cleanupRC )
    
    ! Check if cleanup was successful
    IF ( cleanupRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Verbose output
    WRITE(*,*) 'Configuration cleanup test ', MERGE('PASSED', 'FAILED', TestPassed)
    
  END SUBROUTINE TEST_HCOI_ESMF_Config_TestCleanup
!EOC
END MODULE TEST_HCOI_ESMF_Config_Mod