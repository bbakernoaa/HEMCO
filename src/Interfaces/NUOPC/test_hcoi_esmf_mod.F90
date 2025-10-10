!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: test_hcoi_esmf_mod
!
! !DESCRIPTION: Module TEST\_HCOI\_ESMF\_MOD provides unit tests for the 
! simplified ESMF interface module (HCOI\_ESMF\_MOD) in the HEMCO-NUOPC integration.
!\\
!\\
! !INTERFACE:
!
MODULE TEST_HCOI_ESMF_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
 USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE HCOI_ESMF_Mod
  
#if defined (ESMF_)
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: TEST_HCOI_ESMF_RunAllTests
  PUBLIC :: TEST_HCOI_ESMF_TestInit
  PUBLIC :: TEST_HCOI_ESMF_TestSetupGrids
 PUBLIC :: TEST_HCOI_ESMF_TestCreateRegrid
  PUBLIC :: TEST_HCOI_ESMF_TestPerformRegrid
  PUBLIC :: TEST_HCOI_ESMF_TestMapToHost
 PUBLIC :: TEST_HCOI_ESMF_TestCleanup
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
  TYPE :: TestResults
     INTEGER                :: TotalTests
     INTEGER                :: PassedTests
     INTEGER                :: FailedTests
     LOGICAL                :: AllTestsPassed
 END TYPE TestResults
  
  ! Global test results instance
 TYPE(TestResults), POINTER :: TestResultsInstance => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_RunAllTests
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_RunAllTests executes
! all unit tests for the HCOI\_ESMF\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_RunAllTests( HcoState, TestPassed, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_RunAllTests (test_hcoi_esmf_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Allocate test results instance if not already allocated
    IF ( .NOT. ASSOCIATED(TestResultsInstance) ) THEN
       ALLOCATE(TestResultsInstance)
    ENDIF
    
    ! Initialize test results
    TestResultsInstance%TotalTests = 0
    TestResultsInstance%PassedTests = 0
    TestResultsInstance%FailedTests = 0
    TestResultsInstance%AllTestsPassed = .TRUE.
    
    ! Display test header
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '========================================='
       WRITE(*,*) '  HEMCO ESMF Interface Module Unit Tests  '
       WRITE(*,*) '========================================='
    ENDIF
    
    ! Test 1: Initialization
    TestResultsInstance%TotalTests = TestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_TestInit( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       TestResultsInstance%PassedTests = TestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Initialization Test'
    ELSE
       TestResultsInstance%FailedTests = TestResultsInstance%FailedTests + 1
       TestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Initialization Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 2: Grid setup
    TestResultsInstance%TotalTests = TestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_TestSetupGrids( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       TestResultsInstance%PassedTests = TestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Grid Setup Test'
    ELSE
       TestResultsInstance%FailedTests = TestResultsInstance%FailedTests + 1
       TestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Grid Setup Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 3: Create regrid operator
    TestResultsInstance%TotalTests = TestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_TestCreateRegrid( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       TestResultsInstance%PassedTests = TestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Create Regrid Operator Test'
    ELSE
       TestResultsInstance%FailedTests = TestResultsInstance%FailedTests + 1
       TestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Create Regrid Operator Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 4: Perform regrid operation
    TestResultsInstance%TotalTests = TestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_TestPerformRegrid( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       TestResultsInstance%PassedTests = TestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Perform Regrid Operation Test'
    ELSE
       TestResultsInstance%FailedTests = TestResultsInstance%FailedTests + 1
       TestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Perform Regrid Operation Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 5: Map to host functionality
    TestResultsInstance%TotalTests = TestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_TestMapToHost( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       TestResultsInstance%PassedTests = TestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Map To Host Test'
    ELSE
       TestResultsInstance%FailedTests = TestResultsInstance%FailedTests + 1
       TestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Map To Host Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 6: Cleanup functionality
    TestResultsInstance%TotalTests = TestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_TestCleanup( individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       TestResultsInstance%PassedTests = TestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Cleanup Test'
    ELSE
       TestResultsInstance%FailedTests = TestResultsInstance%FailedTests + 1
       TestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Cleanup Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Display test summary
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '========================================='
       WRITE(*,*) 'Test Results: ', TestResultsInstance%PassedTests, '/', &
                  TestResultsInstance%TotalTests, ' tests passed'
       IF ( TestResultsInstance%FailedTests > 0 ) THEN
          WRITE(*,*) 'WARNING: ', TestResultsInstance%FailedTests, ' tests failed'
       ENDIF
       WRITE(*,*) '========================================='
    ENDIF
    
    ! Set return code based on test results
    TestPassed = TestResultsInstance%AllTestsPassed
    IF ( TestPassed ) THEN
       RC = HCO_SUCCESS
    ELSE
       RC = HCO_FAIL
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_RunAllTests
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_TestInit
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_TestInit tests the
! initialization functionality of the HCOI\_ESMF\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_TestInit( HcoState, TestPassed, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_TestInit (test_hcoi_esmf_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Test initialization with valid HEMCO state
    initialRC = HCO_SUCCESS
    CALL HCOI_ESMF_Init( HcoState, initialRC )
    
    ! Check if initialization was successful
    IF ( initialRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Test initialization with null HEMCO state (should not crash)
    ! This is more of a defensive test
    IF ( TestPassed ) THEN
       ! Test with valid state again to ensure module is properly initialized
       initialRC = HCO_SUCCESS
       CALL HCOI_ESMF_Init( HcoState, initialRC )
       IF ( initialRC /= HCO_SUCCESS ) THEN
          TestPassed = .FALSE.
       ENDIF
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Initialization test PASSED'
       ELSE
          WRITE(*,*) 'Initialization test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_TestInit
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_TestSetupGrids
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_TestSetupGrids tests the
! grid setup functionality of the HCOI\_ESMF\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_TestSetupGrids( HcoState, TestPassed, RC )
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
    INTEGER             :: gridRC
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_TestSetupGrids (test_hcoi_esmf_mod.F90)'
    
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
    
    ! Test grid setup with the created grid
    gridRC = HCO_SUCCESS
    CALL HCOI_ESMF_SetupGrids( HcoState, testGrid, gridRC )
    
    ! Check if grid setup was successful
    IF ( gridRC == HCO_SUCCESS ) THEN
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
          WRITE(*,*) 'Grid setup test PASSED'
       ELSE
          WRITE(*,*) 'Grid setup test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_TestSetupGrids
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_TestCreateRegrid
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_TestCreateRegrid tests the
! regrid operator creation functionality of the HCOI\_ESMF\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_TestCreateRegrid( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,         INTENT(  OUT)  :: TestPassed   ! Whether test passed
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
#if defined (ESMF_)
    TYPE(ESMF_Grid)     :: testGrid
    INTEGER             :: gridRC, regridRC
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_TestCreateRegrid (test_hcoi_esmf_mod.F90)'
    
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
    
    ! First set up grids
    gridRC = HCO_SUCCESS
    CALL HCOI_ESMF_SetupGrids( HcoState, testGrid, gridRC )
    IF ( gridRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Test regrid operator creation
    regridRC = HCO_SUCCESS
    CALL HCOI_ESMF_CreateRegrid( HcoState, regridRC )
    
    ! Check if regrid operator creation was successful
    IF ( regridRC == HCO_SUCCESS ) THEN
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
          WRITE(*,*) 'Create regrid operator test PASSED'
       ELSE
          WRITE(*,*) 'Create regrid operator test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_TestCreateRegrid
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_TestPerformRegrid
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_TestPerformRegrid tests the
! regridding operation functionality of the HCOI\_ESMF\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_TestPerformRegrid( HcoState, TestPassed, RC )
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
    INTEGER             :: gridRC, regridRC, performRC
    INTEGER             :: STATUS
    REAL(hp)            :: tolerance
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_TestPerformRegrid (test_hcoi_esmf_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Get grid dimensions
    nx = HcoState%NX
    ny = HcoState%NY
    
    ! Set tolerance for validation
    tolerance = 1.0e-6_hp
    
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
    
    ! Set up grids and create regrid operator
    gridRC = HCO_SUCCESS
    CALL HCOI_ESMF_SetupGrids( HcoState, testGrid, gridRC )
    IF ( gridRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    regridRC = HCO_SUCCESS
    CALL HCOI_ESMF_CreateRegrid( HcoState, regridRC )
    IF ( regridRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Test regridding operation
    performRC = HCO_SUCCESS
    CALL HCOI_ESMF_PerformRegrid( HcoState, srcData, dstData, performRC )
    
    ! Check if regridding was successful
    IF ( performRC == HCO_SUCCESS ) THEN
       ! For same grids, the result should be approximately the same
       TestPassed = .TRUE.
       DO j = 1, ny
          DO i = 1, nx
             IF ( ABS(dstData(i, j) - srcData(i, j)) > tolerance ) THEN
                TestPassed = .FALSE.
                EXIT
             ENDIF
          ENDDO
          IF ( .NOT. TestPassed ) EXIT
       ENDDO
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
          WRITE(*,*) 'Perform regrid operation test PASSED'
       ELSE
          WRITE(*,*) 'Perform regrid operation test FAILED'
       ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_TestPerformRegrid
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_TestMapToHost
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_TestMapToHost tests the
! map to host functionality of the HCOI\_ESMF\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_TestMapToHost( HcoState, TestPassed, RC )
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
    REAL(hp), POINTER   :: srcData(:,:), hostData(:,:)
    INTEGER             :: i, j, nx, ny
    INTEGER             :: gridRC, regridRC, mapRC
    INTEGER             :: STATUS
    REAL(hp)            :: tolerance
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_TestMapToHost (test_hcoi_esmf_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Get grid dimensions
    nx = HcoState%NX
    ny = HcoState%NY
    
    ! Set tolerance for validation
    tolerance = 1.0e-6_hp
    
    ! Allocate test data arrays
    ALLOCATE(srcData(nx, ny))
    ALLOCATE(hostData(nx, ny))
    
    ! Initialize test data with a simple pattern
    DO j = 1, ny
       DO i = 1, nx
          srcData(i, j) = REAL(i + j, hp) * 0.01_hp
          hostData(i, j) = 0.0_hp
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
       DEALLOCATE(srcData, hostData)
       RETURN
    ENDIF
    
    ! Set up grids and create regrid operator
    gridRC = HCO_SUCCESS
    CALL HCOI_ESMF_SetupGrids( HcoState, testGrid, gridRC )
    IF ( gridRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, hostData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    regridRC = HCO_SUCCESS
    CALL HCOI_ESMF_CreateRegrid( HcoState, regridRC )
    IF ( regridRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, hostData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Test map to host operation
    mapRC = HCO_SUCCESS
    CALL HCOI_ESMF_MapToHost( HcoState, srcData, hostData, mapRC )
    
    ! Check if mapping was successful
    IF ( mapRC == HCO_SUCCESS ) THEN
       ! For same grids, the result should be approximately the same
       TestPassed = .TRUE.
       DO j = 1, ny
          DO i = 1, nx
             IF ( ABS(hostData(i, j) - srcData(i, j)) > tolerance ) THEN
                TestPassed = .FALSE.
                EXIT
             ENDIF
          ENDDO
          IF ( .NOT. TestPassed ) EXIT
       ENDDO
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Cleanup
    CALL ESMF_GridDestroy(testGrid, rc=STATUS)
    DEALLOCATE(srcData, hostData)
    
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
          WRITE(*,*) 'Map to host test PASSED'
       ELSE
          WRITE(*,*) 'Map to host test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_TestMapToHost
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_TestCleanup
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_TestCleanup tests the
! cleanup functionality of the HCOI\_ESMF\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_TestCleanup( TestPassed, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_TestCleanup (test_hcoi_esmf_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Test cleanup function
    cleanupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Cleanup( cleanupRC )
    
    ! Check if cleanup was successful
    IF ( cleanupRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Verbose output
    WRITE(*,*) 'Cleanup test ', MERGE('PASSED', 'FAILED', TestPassed)
    
  END SUBROUTINE TEST_HCOI_ESMF_TestCleanup
!EOC
END MODULE TEST_HCOI_ESMF_Mod