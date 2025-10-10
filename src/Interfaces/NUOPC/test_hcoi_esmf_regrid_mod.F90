!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: test_hcoi_esmf_regrid_mod
!
! !DESCRIPTION: Module TEST\_HCOI\_ESMF\_REGRID\_MOD provides unit tests for the 
! simplified ESMF regridding module (HCOI\_ESMF\_REGRID\_MOD) in the HEMCO-NUOPC integration.
!\\
!\\
! !INTERFACE:
!
MODULE TEST_HCOI_ESMF_Regrid_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
 USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE HCOI_ESMF_Regrid_Mod
  
#if defined (ESMF_)
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: TEST_HCOI_ESMF_Regrid_RunAllTests
  PUBLIC :: TEST_HCOI_ESMF_Regrid_TestInit
  PUBLIC :: TEST_HCOI_ESMF_Regrid_TestSetup
  PUBLIC :: TEST_HCOI_ESMF_Regrid_TestExecute
  PUBLIC :: TEST_HCOI_ESMF_Regrid_TestFinalize
  PUBLIC :: TEST_HCOI_ESMF_Regrid_TestCreateOperator
  PUBLIC :: TEST_HCOI_ESMF_Regrid_TestApplyOperator
  PUBLIC :: TEST_HCOI_ESMF_Regrid_TestStoreWeights
 PUBLIC :: TEST_HCOI_ESMF_Regrid_TestLoadWeights
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial version for HEMCO-NUOPC regridding tests
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Test results structure
 TYPE :: RegridTestResults
     INTEGER                :: TotalTests
     INTEGER                :: PassedTests
     INTEGER                :: FailedTests
     LOGICAL                :: AllTestsPassed
 END TYPE RegridTestResults
  
  ! Global test results instance
 TYPE(RegridTestResults), POINTER :: RegridTestResultsInstance => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Regrid_RunAllTests
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_REGRID\_RunAllTests executes
! all unit tests for the HCOI\_ESMF\_REGRID\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Regrid_RunAllTests( HcoState, TestPassed, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Regrid_RunAllTests (test_hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Allocate test results instance if not already allocated
    IF ( .NOT. ASSOCIATED(RegridTestResultsInstance) ) THEN
       ALLOCATE(RegridTestResultsInstance)
    ENDIF
    
    ! Initialize test results
    RegridTestResultsInstance%TotalTests = 0
    RegridTestResultsInstance%PassedTests = 0
    RegridTestResultsInstance%FailedTests = 0
    RegridTestResultsInstance%AllTestsPassed = .TRUE.
    
    ! Display test header
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '==========================================='
       WRITE(*,*) '  HEMCO ESMF Regridding Module Unit Tests  '
       WRITE(*,*) '==========================================='
    ENDIF
    
    ! Test 1: Initialization
    RegridTestResultsInstance%TotalTests = RegridTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Regrid_TestInit( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       RegridTestResultsInstance%PassedTests = RegridTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Regridding Initialization Test'
    ELSE
       RegridTestResultsInstance%FailedTests = RegridTestResultsInstance%FailedTests + 1
       RegridTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Regridding Initialization Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 2: Setup
    RegridTestResultsInstance%TotalTests = RegridTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Regrid_TestSetup( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       RegridTestResultsInstance%PassedTests = RegridTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Regridding Setup Test'
    ELSE
       RegridTestResultsInstance%FailedTests = RegridTestResultsInstance%FailedTests + 1
       RegridTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Regridding Setup Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 3: Create operator
    RegridTestResultsInstance%TotalTests = RegridTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Regrid_TestCreateOperator( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       RegridTestResultsInstance%PassedTests = RegridTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Create Regrid Operator Test'
    ELSE
       RegridTestResultsInstance%FailedTests = RegridTestResultsInstance%FailedTests + 1
       RegridTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Create Regrid Operator Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 4: Apply operator
    RegridTestResultsInstance%TotalTests = RegridTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Regrid_TestApplyOperator( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       RegridTestResultsInstance%PassedTests = RegridTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Apply Regrid Operator Test'
    ELSE
       RegridTestResultsInstance%FailedTests = RegridTestResultsInstance%FailedTests + 1
       RegridTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Apply Regrid Operator Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 5: Execute regridding
    RegridTestResultsInstance%TotalTests = RegridTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Regrid_TestExecute( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       RegridTestResultsInstance%PassedTests = RegridTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Execute Regridding Test'
    ELSE
       RegridTestResultsInstance%FailedTests = RegridTestResultsInstance%FailedTests + 1
       RegridTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Execute Regridding Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 6: Store weights
    RegridTestResultsInstance%TotalTests = RegridTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Regrid_TestStoreWeights( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       RegridTestResultsInstance%PassedTests = RegridTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Store Weights Test'
    ELSE
       RegridTestResultsInstance%FailedTests = RegridTestResultsInstance%FailedTests + 1
       RegridTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Store Weights Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 7: Load weights
    RegridTestResultsInstance%TotalTests = RegridTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Regrid_TestLoadWeights( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       RegridTestResultsInstance%PassedTests = RegridTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Load Weights Test'
    ELSE
       RegridTestResultsInstance%FailedTests = RegridTestResultsInstance%FailedTests + 1
       RegridTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Load Weights Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 8: Finalize
    RegridTestResultsInstance%TotalTests = RegridTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Regrid_TestFinalize( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       RegridTestResultsInstance%PassedTests = RegridTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Regridding Finalize Test'
    ELSE
       RegridTestResultsInstance%FailedTests = RegridTestResultsInstance%FailedTests + 1
       RegridTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Regridding Finalize Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Display test summary
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '==========================================='
       WRITE(*,*) 'Test Results: ', RegridTestResultsInstance%PassedTests, '/', &
                  RegridTestResultsInstance%TotalTests, ' tests passed'
       IF ( RegridTestResultsInstance%FailedTests > 0 ) THEN
          WRITE(*,*) 'WARNING: ', RegridTestResultsInstance%FailedTests, ' tests failed'
       ENDIF
       WRITE(*,*) '==========================================='
    ENDIF
    
    ! Set return code based on test results
    TestPassed = RegridTestResultsInstance%AllTestsPassed
    IF ( TestPassed ) THEN
       RC = HCO_SUCCESS
    ELSE
       RC = HCO_FAIL
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Regrid_RunAllTests
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Regrid_TestInit
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_REGRID\_TestInit tests the
! initialization functionality of the HCOI\_ESMF\_REGRID\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Regrid_TestInit( HcoState, TestPassed, RC )
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
    INTEGER             :: initialRC
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Regrid_TestInit (test_hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Test initialization with valid HEMCO state
    initialRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_Init( HcoState, initialRC )
    
    ! Check if initialization was successful
    IF ( initialRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Regridding initialization test PASSED'
       ELSE
          WRITE(*,*) 'Regridding initialization test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Regrid_TestInit
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Regrid_TestSetup
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_REGRID\_TestSetup tests the
! setup functionality of the HCOI\_ESMF\_REGRID\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Regrid_TestSetup( HcoState, TestPassed, RC )
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
    TYPE(ESMF_Grid)     :: srcGrid, dstGrid
    INTEGER             :: setupRC
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Regrid_TestSetup (test_hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Create source and destination grids for testing
    srcGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/HcoState%NX, HcoState%NY/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    dstGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/HcoState%NX, HcoState%NY/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Initialize regridding system first
    CALL HCOI_ESMF_Regrid_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       RETURN
    ENDIF
    
    ! Test setup with the created grids
    setupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_Setup( HcoState, srcGrid, dstGrid, HCOI_REGRID_METHOD_BILINEAR, &
                                 .TRUE., .TRUE., setupRC )
    
    ! Check if setup was successful
    IF ( setupRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Cleanup grids
    CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
    CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
    
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
          WRITE(*,*) 'Regridding setup test PASSED'
       ELSE
          WRITE(*,*) 'Regridding setup test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Regrid_TestSetup
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Regrid_TestCreateOperator
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_REGRID\_TestCreateOperator tests the
! create operator functionality of the HCOI\_ESMF\_REGRID\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Regrid_TestCreateOperator( HcoState, TestPassed, RC )
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
    TYPE(ESMF_Grid)     :: srcGrid, dstGrid
    INTEGER             :: setupRC, createRC
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Regrid_TestCreateOperator (test_hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Create source and destination grids for testing
    srcGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/HcoState%NX, HcoState%NY/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    dstGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/HcoState%NX, HcoState%NY/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Initialize and setup regridding system first
    CALL HCOI_ESMF_Regrid_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       RETURN
    ENDIF
    
    setupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_Setup( HcoState, srcGrid, dstGrid, HCOI_REGRID_METHOD_BILINEAR, &
                                 .TRUE., .TRUE., setupRC )
    IF ( setupRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Test creating regridding operator
    createRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_CreateOperator( HcoState, srcGrid, dstGrid, createRC )
    
    ! Check if operator creation was successful
    IF ( createRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Cleanup grids
    CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
    CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
    
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
    
  END SUBROUTINE TEST_HCOI_ESMF_Regrid_TestCreateOperator
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Regrid_TestApplyOperator
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_REGRID\_TestApplyOperator tests the
! apply operator functionality of the HCOI\_ESMF\_REGRID\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Regrid_TestApplyOperator( HcoState, TestPassed, RC )
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
    TYPE(ESMF_Grid)     :: srcGrid, dstGrid
    TYPE(ESMF_Field)    :: srcField, dstField
    REAL(hp), POINTER   :: srcData(:,:), dstData(:,:)
    INTEGER             :: i, j, nx, ny
    INTEGER             :: setupRC, createRC, applyRC
    INTEGER             :: STATUS
    REAL(hp)            :: tolerance
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Regrid_TestApplyOperator (test_hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Get dimensions
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
    
    ! Create source and destination grids for testing
    srcGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/nx, ny/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    dstGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/nx, ny/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Create source and destination fields
    srcField = ESMF_FieldCreate( &
         grid=srcGrid, &
         name="TestSrcField", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    dstField = ESMF_FieldCreate( &
         grid=dstGrid, &
         name="TestDstField", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Initialize regridding system and create operator
    CALL HCOI_ESMF_Regrid_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    setupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_Setup( HcoState, srcGrid, dstGrid, HCOI_REGRID_METHOD_BILINEAR, &
                                 .TRUE., .TRUE., setupRC )
    IF ( setupRC /= HCO_SUCCESS ) THEN
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    createRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_CreateOperator( HcoState, srcGrid, dstGrid, createRC )
    IF ( createRC /= HCO_SUCCESS ) THEN
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Test applying regridding operator
    applyRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_ApplyOperator( HcoState, srcField, dstField, applyRC )
    
    ! Check if operator application was successful
    IF ( applyRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Cleanup
    CALL ESMF_FieldDestroy(dstField, rc=STATUS)
    CALL ESMF_FieldDestroy(srcField, rc=STATUS)
    CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
    CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
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
          WRITE(*,*) 'Apply regrid operator test PASSED'
       ELSE
          WRITE(*,*) 'Apply regrid operator test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Regrid_TestApplyOperator
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Regrid_TestExecute
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_REGRID\_TestExecute tests the
! execute functionality of the HCOI\_ESMF\_REGRID\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Regrid_TestExecute( HcoState, TestPassed, RC )
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
    TYPE(ESMF_Grid)     :: srcGrid, dstGrid
    REAL(hp), POINTER   :: srcData(:,:), dstData(:,:)
    INTEGER             :: i, j, nx, ny
    INTEGER             :: setupRC, executeRC
    INTEGER             :: STATUS
    REAL(hp)            :: tolerance
    LOGICAL             :: dataValid
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Regrid_TestExecute (test_hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Get dimensions
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
    
    ! Create source and destination grids for testing
    srcGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/nx, ny/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    dstGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/nx, ny/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Initialize regridding system
    CALL HCOI_ESMF_Regrid_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    setupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_Setup( HcoState, srcGrid, dstGrid, HCOI_REGRID_METHOD_BILINEAR, &
                                 .TRUE., .TRUE., setupRC )
    IF ( setupRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Test executing regridding
    executeRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_Execute( HcoState, srcGrid, dstGrid, srcData, dstData, executeRC )
    
    ! Check if execution was successful
    IF ( executeRC == HCO_SUCCESS ) THEN
       ! Verify that the destination data is not all zeros (meaning regridding happened)
       dataValid = .FALSE.
       DO j = 1, ny
          DO i = 1, nx
             IF ( ABS(dstData(i, j)) > tolerance ) THEN
                dataValid = .TRUE.
                EXIT
             ENDIF
          ENDDO
          IF ( dataValid ) EXIT
       ENDDO
       
       IF ( dataValid ) THEN
          TestPassed = .TRUE.
       ELSE
          TestPassed = .FALSE.
       ENDIF
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Cleanup
    CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
    CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
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
          WRITE(*,*) 'Execute regridding test PASSED'
       ELSE
          WRITE(*,*) 'Execute regridding test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Regrid_TestExecute
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Regrid_TestStoreWeights
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_REGRID\_TestStoreWeights tests the
! store weights functionality of the HCOI\_ESMF\_REGRID\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Regrid_TestStoreWeights( HcoState, TestPassed, RC )
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
    TYPE(ESMF_Grid)     :: srcGrid, dstGrid
    INTEGER             :: setupRC, createRC, storeRC
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Regrid_TestStoreWeights (test_hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Create source and destination grids for testing
    srcGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/HcoState%NX, HcoState%NY/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    dstGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/HcoState%NX, HcoState%NY/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Initialize regridding system and create operator
    CALL HCOI_ESMF_Regrid_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       RETURN
    ENDIF
    
    setupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_Setup( HcoState, srcGrid, dstGrid, HCOI_REGRID_METHOD_BILINEAR, &
                                 .TRUE., .TRUE., setupRC )
    IF ( setupRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    createRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_CreateOperator( HcoState, srcGrid, dstGrid, createRC )
    IF ( createRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Test storing weights
    storeRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_StoreWeights( HcoState, storeRC )
    
    ! Check if storing weights was successful
    IF ( storeRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Cleanup
    CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
    CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
    
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
          WRITE(*,*) 'Store weights test PASSED'
       ELSE
          WRITE(*,*) 'Store weights test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Regrid_TestStoreWeights
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Regrid_TestLoadWeights
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_REGRID\_TestLoadWeights tests the
! load weights functionality of the HCOI\_ESMF\_REGRID\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Regrid_TestLoadWeights( HcoState, TestPassed, RC )
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
    TYPE(ESMF_Grid)     :: srcGrid, dstGrid
    TYPE(ESMF_FieldRegridWeight) :: weights
    INTEGER             :: setupRC, createRC, storeRC
    INTEGER             :: loadRC
    LOGICAL             :: found
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Regrid_TestLoadWeights (test_hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Create source and destination grids for testing
    srcGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/HcoState%NX, HcoState%NY/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    dstGrid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/HcoState%NX, HcoState%NY/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Initialize regridding system and create operator
    CALL HCOI_ESMF_Regrid_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       RETURN
    ENDIF
    
    setupRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_Setup( HcoState, srcGrid, dstGrid, HCOI_REGRID_METHOD_BILINEAR, &
                                 .TRUE., .TRUE., setupRC )
    IF ( setupRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    createRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_CreateOperator( HcoState, srcGrid, dstGrid, createRC )
    IF ( createRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    storeRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_StoreWeights( HcoState, storeRC )
    IF ( storeRC /= HCO_SUCCESS ) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Test loading weights
    weights = ESMF_FieldRegridWeightEmptyCreate(rc=RC)
    IF (ESMF_LogFoundError(RC, __RC__, ESMF_CONTEXT, LOC)) THEN
       CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
       CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
       RETURN
    ENDIF
    
    found = .FALSE.
    loadRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_LoadWeights( HcoState, 'SRC_GRID', 'DST_GRID', weights, found, loadRC )
    
    ! Check if loading weights was successful and weights were found
    IF ( loadRC == HCO_SUCCESS .AND. found ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Cleanup
    CALL ESMF_FieldRegridWeightDestroy(weights, rc=STATUS)
    CALL ESMF_GridDestroy(srcGrid, rc=STATUS)
    CALL ESMF_GridDestroy(dstGrid, rc=STATUS)
    
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
          WRITE(*,*) 'Load weights test PASSED'
       ELSE
          WRITE(*,*) 'Load weights test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Regrid_TestLoadWeights
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_Regrid_TestFinalize
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_REGRID\_TestFinalize tests the
! finalize functionality of the HCOI\_ESMF\_REGRID\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_Regrid_TestFinalize( HcoState, TestPassed, RC )
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
    INTEGER             :: finalizeRC
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_Regrid_TestFinalize (test_hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Test finalization
    finalizeRC = HCO_SUCCESS
    CALL HCOI_ESMF_Regrid_Finalize( HcoState, finalizeRC )
    
    ! Check if finalization was successful
    IF ( finalizeRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Regridding finalize test PASSED'
       ELSE
          WRITE(*,*) 'Regridding finalize test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_Regrid_TestFinalize
!EOC
END MODULE TEST_HCOI_ESMF_Regrid_Mod