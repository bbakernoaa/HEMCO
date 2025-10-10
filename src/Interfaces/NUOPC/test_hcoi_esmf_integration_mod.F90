!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: test_hcoi_esmf_integration_mod
!
! !DESCRIPTION: Module TEST\_HCOI\_ESMF\_INTEGRATION\_MOD provides unit tests for the 
! simplified ESMF integration module (HCOI\_ESMF\_INTEGRATION\_MOD) in the HEMCO-NUOPC integration.
!\\
!\\
! !INTERFACE:
!
MODULE TEST_HCOI_ESMF_Integration_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
 USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE HCOI_ESMF_Integration_Mod
  
#if defined (ESMF_)
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: TEST_HCOI_ESMF_Integration_RunAllTests
  PUBLIC :: TEST_HCOI_ESMF_Integration_TestInit
  PUBLIC :: TEST_HCOI_ESMF_Integration_TestSetup
  PUBLIC :: TEST_HCOI_ESMF_Integration_TestExecute
  PUBLIC :: TEST_HCOI_ESMF_Integration_TestMapData
  PUBLIC :: TEST_HCOI_ESMF_Integration_TestGetHostGrid
  PUBLIC :: TEST_HCOI_ESMF_Integration_TestCleanup
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial version for HEMCO-NUOPC integration tests
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Test results structure
  TYPE :: IntegrationTestResults
     INTEGER                :: TotalTests
     INTEGER                :: PassedTests
     INTEGER                :: FailedTests
     LOGICAL                :: AllTestsPassed
 END TYPE IntegrationTestResults
  
  ! Global test results instance
 TYPE(IntegrationTestResults), POINTER :: IntegrationTestResultsInstance => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Integration_RunAllTests
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_INTEGRATION\_RunAllTests executes
! all unit tests for the HCOI\_ESMF\_INTEGRATION\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Integration_RunAllTests( HcoState, TestPassed, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Integration_RunAllTests (test_hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Allocate test results instance if not already allocated
    IF ( .NOT. ASSOCIATED(IntegrationTestResultsInstance) ) THEN
       ALLOCATE(IntegrationTestResultsInstance)
    ENDIF
    
    ! Initialize test results
    IntegrationTestResultsInstance%TotalTests = 0
    IntegrationTestResultsInstance%PassedTests = 0
    IntegrationTestResultsInstance%FailedTests = 0
    IntegrationTestResultsInstance%AllTestsPassed = .TRUE.
    
    ! Display test header
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '================================================'
       WRITE(*,*) '  HEMCO ESMF Integration Module Unit Tests  '
       WRITE(*,*) '================================================'
    ENDIF
    
    ! Test 1: Initialization
    IntegrationTestResultsInstance%TotalTests = IntegrationTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Integration_TestInit( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IntegrationTestResultsInstance%PassedTests = IntegrationTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Integration Initialization Test'
    ELSE
       IntegrationTestResultsInstance%FailedTests = IntegrationTestResultsInstance%FailedTests + 1
       IntegrationTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Integration Initialization Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 2: Setup
    IntegrationTestResultsInstance%TotalTests = IntegrationTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Integration_TestSetup( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IntegrationTestResultsInstance%PassedTests = IntegrationTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Integration Setup Test'
    ELSE
       IntegrationTestResultsInstance%FailedTests = IntegrationTestResultsInstance%FailedTests + 1
       IntegrationTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Integration Setup Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 3: Execute
    IntegrationTestResultsInstance%TotalTests = IntegrationTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Integration_TestExecute( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IntegrationTestResultsInstance%PassedTests = IntegrationTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Integration Execute Test'
    ELSE
       IntegrationTestResultsInstance%FailedTests = IntegrationTestResultsInstance%FailedTests + 1
       IntegrationTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Integration Execute Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 4: Map data
    IntegrationTestResultsInstance%TotalTests = IntegrationTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Integration_TestMapData( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IntegrationTestResultsInstance%PassedTests = IntegrationTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Map Data Test'
    ELSE
       IntegrationTestResultsInstance%FailedTests = IntegrationTestResultsInstance%FailedTests + 1
       IntegrationTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Map Data Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 5: Get host grid
    IntegrationTestResultsInstance%TotalTests = IntegrationTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Integration_TestGetHostGrid( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IntegrationTestResultsInstance%PassedTests = IntegrationTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Get Host Grid Test'
    ELSE
       IntegrationTestResultsInstance%FailedTests = IntegrationTestResultsInstance%FailedTests + 1
       IntegrationTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Get Host Grid Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 6: Cleanup
    IntegrationTestResultsInstance%TotalTests = IntegrationTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Integration_TestCleanup( individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IntegrationTestResultsInstance%PassedTests = IntegrationTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Integration Cleanup Test'
    ELSE
       IntegrationTestResultsInstance%FailedTests = IntegrationTestResultsInstance%FailedTests + 1
       IntegrationTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Integration Cleanup Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Display test summary
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '================================================'
       WRITE(*,*) 'Test Results: ', IntegrationTestResultsInstance%PassedTests, '/', &
                  IntegrationTestResultsInstance%TotalTests, ' tests passed'
       IF ( IntegrationTestResultsInstance%FailedTests > 0 ) THEN
          WRITE(*,*) 'WARNING: ', IntegrationTestResultsInstance%FailedTests, ' tests failed'
       ENDIF
       WRITE(*,*) '================================================'
    ENDIF
    
    ! Set return code based on test results
    TestPassed = IntegrationTestResultsInstance%AllTestsPassed
    IF ( TestPassed ) THEN
       RC = HCO_SUCCESS
    ELSE
       RC = HCO_FAIL
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Integration_RunAllTests
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Integration_TestInit
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_INTEGRATION\_TestInit tests the
! initialization functionality of the HCOI\_ESMF\_INTEGRATION\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Integration_TestInit( HcoState, TestPassed, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Integration_TestInit (test_hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Test initialization with valid HEMCO state
    initialRC = HCO_SUCCESS
    CALL HCOI_ESMF_Integration_Init( HcoState, initialRC )
    
    ! Check if initialization was successful
    IF ( initialRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Integration initialization test PASSED'
       ELSE
          WRITE(*,*) 'Integration initialization test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Integration_TestInit
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Integration_TestSetup
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_INTEGRATION\_TestSetup tests the
! setup functionality of the HCOI\_ESMF\_INTEGRATION\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Integration_TestSetup( HcoState, TestPassed, RC )
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
#if defined (ESMF_)
    TYPE(ESMF_Grid)     :: testGrid
    INTEGER             :: setupRC
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Integration_TestSetup (test_hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Create a test ESMF grid
    testGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/HcoState%NX, HcoState%NY/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Initialize integration system first
    CALL HCOI_ESMF_Integration_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       RETURN
    ENDIF
    
    ! Test setup with the created grid
    setupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Integration_Setup( HcoState, testGrid, .TRUE., HCOI_MODE_NATIVE, setupRC )
    
    ! Check if setup was successful
    IF ( setupRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Cleanup test grid
    CALL ESMF_GridDestroy(testGrid, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
#else
    ! In non-ESMF builds, this should fail
    TestPassed = .FALSE.
#endif
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Integration setup test PASSED'
       ELSE
          WRITE(*,*) 'Integration setup test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Integration_TestSetup
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Integration_TestExecute
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_INTEGRATION\_TestExecute tests the
! execute functionality of the HCOI\_ESMF\_INTEGRATION\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Integration_TestExecute( HcoState, TestPassed, RC )
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
#if defined (ESMF_)
    TYPE(ESMF_Grid)     :: testGrid
    REAL(hp), POINTER   :: srcData(:,:), dstData(:,:)
    INTEGER             :: i, j, nx, ny
    INTEGER             :: setupRC, executeRC
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Integration_TestExecute (test_hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Get dimensions
    nx = HcoState%NX
    ny = HcoState%NY
    
    ! Allocate test data arrays
    ALLOCATE(srcData(nx, ny))
    ALLOCATE(dstData(nx, ny))
    
    ! Initialize test data with a simple pattern
    DO j = 1, ny
       DO i = 1, nx
          srcData(i, j) = REAL(i + j, hp) * 0.01_hp
          dstData(i, j) = 0.0_hp
       ENDDO
    ENDDO
    
    ! Create a test ESMF grid
    testGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/nx, ny/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    ! Initialize integration system and setup
    CALL HCOI_ESMF_Integration_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    setupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Integration_Setup( HcoState, testGrid, .TRUE., HCOI_MODE_NATIVE, setupRC )
    IF ( setupRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Test execution
    executeRC = HCO_SUCCESS
    CALL HCOI_ESMF_Integration_Execute( HcoState, srcData, dstData, executeRC )
    
    ! Check if execution was successful
    IF ( executeRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Cleanup
    CALL ESMF_GridDestroy(testGrid, rc=STATUS)
    DEALLOCATE(srcData, dstData)
    
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
#else
    ! In non-ESMF builds, this should fail
    TestPassed = .FALSE.
#endif
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Integration execute test PASSED'
       ELSE
          WRITE(*,*) 'Integration execute test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Integration_TestExecute
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Integration_TestMapData
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_INTEGRATION\_TestMapData tests the
! map data functionality of the HCOI\_ESMF\_INTEGRATION\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Integration_TestMapData( HcoState, TestPassed, RC )
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
#if defined (ESMF_)
    TYPE(ESMF_Grid)     :: testGrid
    REAL(hp), POINTER   :: srcData(:,:), dstData(:,:)
    INTEGER             :: i, j, nx, ny
    INTEGER             :: setupRC, mapRC
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Integration_TestMapData (test_hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Get dimensions
    nx = HcoState%NX
    ny = HcoState%NY
    
    ! Allocate test data arrays
    ALLOCATE(srcData(nx, ny))
    ALLOCATE(dstData(nx, ny))
    
    ! Initialize test data with a simple pattern
    DO j = 1, ny
       DO i = 1, nx
          srcData(i, j) = REAL(i + j, hp) * 0.01_hp
          dstData(i, j) = 0.0_hp
       ENDDO
    ENDDO
    
    ! Create a test ESMF grid
    testGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/nx, ny/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    ! Initialize integration system and setup
    CALL HCOI_ESMF_Integration_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    setupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Integration_Setup( HcoState, testGrid, .TRUE., HCOI_MODE_NATIVE, setupRC )
    IF ( setupRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Test mapping data
    mapRC = HCO_SUCCESS
    CALL HCOI_ESMF_Integration_MapData( HcoState, srcData, dstData, mapRC )
    
    ! Check if mapping was successful
    IF ( mapRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Cleanup
    CALL ESMF_GridDestroy(testGrid, rc=STATUS)
    DEALLOCATE(srcData, dstData)
    
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
#else
    ! In non-ESMF builds, this should fail
    TestPassed = .FALSE.
#endif
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Map data test PASSED'
       ELSE
          WRITE(*,*) 'Map data test FAILED'
       ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Integration_TestMapData
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Integration_TestGetHostGrid
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_INTEGRATION\_TestGetHostGrid tests the
! get host grid functionality of the HCOI\_ESMF\_INTEGRATION\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Integration_TestGetHostGrid( HcoState, TestPassed, RC )
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
#if defined (ESMF_)
    TYPE(ESMF_Grid)     :: testGrid, hostGrid
    INTEGER             :: setupRC, getRC
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Integration_TestGetHostGrid (test_hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Create a test ESMF grid
    testGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/HcoState%NX, HcoState%NY/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Initialize integration system and setup
    CALL HCOI_ESMF_Integration_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       RETURN
    ENDIF
    
    setupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Integration_Setup( HcoState, testGrid, .TRUE., HCOI_MODE_NATIVE, setupRC )
    IF ( setupRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Test getting host grid
    getRC = HCO_SUCCESS
    hostGrid = ESMF_GridEmptyCreate(rc=getRC)
    IF (ESMF_LogFoundError(getRC, __RC__, ESMF_CONTEXT, LOC)) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Call the function - this will actually return a dummy grid in our test
    CALL HCOI_ESMF_Integration_GetHostGrid( HcoState, hostGrid, getRC )
    
    ! Check if getting host grid was successful
    IF ( getRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Cleanup
    CALL ESMF_GridDestroy(testGrid, rc=STATUS)
    CALL ESMF_GridDestroy(hostGrid, rc=STATUS)
    
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
#else
    ! In non-ESMF builds, this should fail
    TestPassed = .FALSE.
#endif
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Get host grid test PASSED'
       ELSE
          WRITE(*,*) 'Get host grid test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Integration_TestGetHostGrid
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Integration_TestCleanup
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_INTEGRATION\_TestCleanup tests the
! cleanup functionality of the HCOI\_ESMF\_INTEGRATION\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Integration_TestCleanup( TestPassed, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Integration_TestCleanup (test_hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Test cleanup function
    cleanupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Integration_Cleanup( cleanupRC )
    
    ! Check if cleanup was successful
    IF ( cleanupRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Verbose output
    WRITE(*,*) 'Integration cleanup test ', MERGE('PASSED', 'FAILED', TestPassed)
    
  END SUBROUTINE TEST_HCOI_ESMF_Integration_TestCleanup
!EOC
END MODULE TEST_HCOI_ESMF_Integration_Mod