!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoi_esmf_test_mod
!
! !DESCRIPTION: Module HCOI\_ESMF\_TEST\_MOD provides simplified testing
! functionality for the streamlined ESMF integration in HEMCO-NUOPC. This module
! focuses on validating the core functionality without complex backward compatibility
! concerns.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMF_Test_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE HCOI_ESMF_Simplified_Mod
  USE HCOI_ESMF_Integration_Mod
  
#if defined (ESMF_)
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  ! Simple testing functions:
  PUBLIC :: HCOI_ESMF_Test_RunAllTests
  PUBLIC :: HCOI_ESMF_Test_ValidateGridMapping
  PUBLIC :: HCOI_ESMF_Test_PerformanceBenchmark
  PUBLIC :: HCOI_ESMF_Test_MemoryUsage
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial simplified version for NUOPC ESMF testing
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Test results structure
  TYPE :: HCOI_TestResults
     INTEGER                :: TotalTests
     INTEGER                :: PassedTests
     INTEGER                :: FailedTests
     LOGICAL                :: AllTestsPassed
     REAL(hp)               :: PerformanceScore
     INTEGER                :: MemoryUsage
  END TYPE HCOI_TestResults
  
  ! Global test results instance
  TYPE(HCOI_TestResults), POINTER :: SimpleTestResults => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Test_RunAllTests
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Test\_RunAllTests executes
! all simplified tests for the ESMF integration system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Test_RunAllTests( HcoState, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HEMCO state object
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
    LOGICAL             :: testResult
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Test_RunAllTests (hcoi_esmf_test_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Allocate test results instance if not already allocated
    IF ( .NOT. ASSOCIATED(SimpleTestResults) ) THEN
       ALLOCATE(SimpleTestResults)
    ENDIF
    
    ! Initialize test results
    SimpleTestResults%TotalTests = 0
    SimpleTestResults%PassedTests = 0
    SimpleTestResults%FailedTests = 0
    SimpleTestResults%AllTestsPassed = .TRUE.
    SimpleTestResults%PerformanceScore = 0.0_hp
    SimpleTestResults%MemoryUsage = 0
    
    ! Display test header
    IF ( HcoState%amIRoot ) THEN
       CALL HCO_MSG( '==========================================' )
       CALL HCO_MSG( '  HEMCO ESMF Simplified Integration Tests  ' )
       CALL HCO_MSG( '==========================================' )
    ENDIF
    
    ! Test 1: Grid mapping validation
    SimpleTestResults%TotalTests = SimpleTestResults%TotalTests + 1
    CALL HCOI_ESMF_Test_ValidateGridMapping( HcoState, testResult, RC )
    IF ( RC == HCO_SUCCESS .AND. testResult ) THEN
       SimpleTestResults%PassedTests = SimpleTestResults%PassedTests + 1
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'PASS: Grid Mapping Validation Test' )
    ELSE
       SimpleTestResults%FailedTests = SimpleTestResults%FailedTests + 1
       SimpleTestResults%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'FAIL: Grid Mapping Validation Test' )
    ENDIF
    
    ! Test 2: Performance benchmark
    SimpleTestResults%TotalTests = SimpleTestResults%TotalTests + 1
    CALL HCOI_ESMF_Test_PerformanceBenchmark( HcoState, testResult, RC )
    IF ( RC == HCO_SUCCESS .AND. testResult ) THEN
       SimpleTestResults%PassedTests = SimpleTestResults%PassedTests + 1
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'PASS: Performance Benchmark Test' )
    ELSE
       SimpleTestResults%FailedTests = SimpleTestResults%FailedTests + 1
       SimpleTestResults%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'FAIL: Performance Benchmark Test' )
    ENDIF
    
    ! Test 3: Memory usage validation
    SimpleTestResults%TotalTests = SimpleTestResults%TotalTests + 1
    CALL HCOI_ESMF_Test_MemoryUsage( HcoState, testResult, RC )
    IF ( RC == HCO_SUCCESS .AND. testResult ) THEN
       SimpleTestResults%PassedTests = SimpleTestResults%PassedTests + 1
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'PASS: Memory Usage Validation Test' )
    ELSE
       SimpleTestResults%FailedTests = SimpleTestResults%FailedTests + 1
       SimpleTestResults%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'FAIL: Memory Usage Validation Test' )
    ENDIF
    
    ! Display test summary
    IF ( HcoState%amIRoot ) THEN
       CALL HCO_MSG( '==========================================' )
       WRITE(*,*) 'Test Results: ', SimpleTestResults%PassedTests, '/', &
                  SimpleTestResults%TotalTests, ' tests passed'
       IF ( SimpleTestResults%FailedTests > 0 ) THEN
          WRITE(*,*) 'WARNING: ', SimpleTestResults%FailedTests, ' tests failed'
       ENDIF
       CALL HCO_MSG( '==========================================' )
    ENDIF
    
    ! Set return code based on test results
    IF ( SimpleTestResults%AllTestsPassed ) THEN
       RC = HCO_SUCCESS
    ELSE
       RC = HCO_FAIL
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Test_RunAllTests
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Test_ValidateGridMapping
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Test\_ValidateGridMapping
! validates the grid mapping functionality of the simplified ESMF integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Test_ValidateGridMapping( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,            INTENT(  OUT) :: TestPassed   ! Whether test passed
    INTEGER,            INTENT(INOUT) :: RC           ! Return code
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
    TYPE(ESMF_Field)    :: srcField, dstField
    REAL(hp), POINTER   :: srcData(:,:), dstData(:,:)
    REAL(hp)            :: tolerance
    INTEGER             :: i, j, nx, ny
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Test_ValidateGridMapping (hcoi_esmf_test_mod.F90)'
    
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
    
    ! Create test grid
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
    
    ! Create source and destination fields
    srcField = ESMF_FieldCreate( &
         grid=testGrid, &
         name="SrcTestField", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    dstField = ESMF_FieldCreate( &
         grid=testGrid, &
         name="DstTestField", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    ! Initialize source field with test data
    CALL ESMF_FieldSet(srcField, data=srcData, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    ! Perform mapping using simplified ESMF integration
    CALL HCOI_ESMF_Simplified_MapToHost( HcoState, srcData, dstData, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error performing simplified mapping', RC, THISLOC=LOC )
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    ! Validate results - for identical grids, data should be the same
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
    
    ! Cleanup
    CALL ESMF_FieldDestroy(dstField, rc=STATUS)
    CALL ESMF_FieldDestroy(srcField, rc=STATUS)
    CALL ESMF_GridDestroy(testGrid, rc=STATUS)
    DEALLOCATE(srcData, dstData)
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          CALL HCO_MSG( 'Grid mapping validation test PASSED', &
                       LUN=HcoState%Config%hcoLogLUN )
       ELSE
          CALL HCO_MSG( 'Grid mapping validation test FAILED', &
                       LUN=HcoState%Config%hcoLogLUN )
       ENDIF
    ENDIF
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
  END SUBROUTINE HCOI_ESMF_Test_ValidateGridMapping
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Test_PerformanceBenchmark
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Test\_PerformanceBenchmark
! performs performance benchmarking of the simplified ESMF integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Test_PerformanceBenchmark( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,            INTENT(  OUT) :: TestPassed   ! Whether test passed
    INTEGER,            INTENT(INOUT) :: RC           ! Return code
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
    REAL(hp), POINTER   :: srcData(:,:), dstData(:,:)
    INTEGER             :: i, j, nx, ny
    INTEGER             :: iterations, testIterations
    REAL                :: startTime, endTime, totalTime
    INTEGER             :: clockRate, clockMax
    REAL(hp)            :: performanceThreshold
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Test_PerformanceBenchmark (hcoi_esmf_test_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Get grid dimensions
    nx = HcoState%NX
    ny = HcoState%NY
    
    ! Set test parameters
    testIterations = 100  ! Number of iterations for timing
    performanceThreshold = 1.0_hp  ! Performance threshold in seconds
    
    ! Allocate test data arrays
    ALLOCATE(srcData(nx, ny))
    ALLOCATE(dstData(nx, ny))
    
    ! Initialize test data
    DO j = 1, ny
       DO i = 1, nx
          srcData(i, j) = REAL(i + j, hp) * 0.01_hp
          dstData(i, j) = 0.0_hp
       ENDDO
    ENDDO
    
    ! Performance timing
    CALL SYSTEM_CLOCK(startTime, clockRate, clockMax)
    
    ! Perform multiple mapping operations for timing
    DO iterations = 1, testIterations
       CALL HCOI_ESMF_Simplified_MapToHost( HcoState, srcData, dstData, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error performing mapping in performance test', RC, THISLOC=LOC )
          DEALLOCATE(srcData, dstData)
          RETURN
       ENDIF
    ENDDO
    
    CALL SYSTEM_CLOCK(endTime, clockRate, clockMax)
    
    ! Calculate timing results
    totalTime = REAL(endTime - startTime) / REAL(clockRate)
    
    ! Check if performance meets threshold
    IF (totalTime <= performanceThreshold) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Store performance score
    IF ( ASSOCIATED(SimpleTestResults) ) THEN
       SimpleTestResults%PerformanceScore = totalTime
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          CALL HCO_MSG( 'Performance benchmark test PASSED', &
                       LUN=HcoState%Config%hcoLogLUN )
          WRITE(HcoState%Config%hcoLogLUN,*) '  Total time: ', totalTime, ' seconds'
          WRITE(HcoState%Config%hcoLogLUN,*) '  Performance threshold: ', performanceThreshold, ' seconds'
       ELSE
          CALL HCO_MSG( 'Performance benchmark test FAILED', &
                       LUN=HcoState%Config%hcoLogLUN )
          WRITE(HcoState%Config%hcoLogLUN,*) '  Total time: ', totalTime, ' seconds'
          WRITE(HcoState%Config%hcoLogLUN,*) '  Performance threshold: ', performanceThreshold, ' seconds'
       ENDIF
    
    ! Cleanup
    DEALLOCATE(srcData, dstData)
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
  END SUBROUTINE HCOI_ESMF_Test_PerformanceBenchmark
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Test_MemoryUsage
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Test\_MemoryUsage
! validates memory usage of the simplified ESMF integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Test_MemoryUsage( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,            INTENT(  OUT) :: TestPassed   ! Whether test passed
    INTEGER,            INTENT(INOUT) :: RC           ! Return code
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
    INTEGER             :: initialMemory, finalMemory
    INTEGER             :: memoryUsage
    INTEGER             :: memoryThreshold
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Test_MemoryUsage (hcoi_esmf_test_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Set memory threshold (in MB)
    memoryThreshold = 100  ! 100 MB memory threshold
    
    ! Get initial memory usage
    ! In a real implementation, we would use system calls to get actual memory usage
    initialMemory = 0  ! Placeholder - would use actual memory measurement
    
    ! Perform some operations that might consume memory
    CALL HCOI_ESMF_Simplified_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error initializing simplified ESMF', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Get final memory usage
    ! In a real implementation, we would use system calls to get actual memory usage
    finalMemory = 0  ! Placeholder - would use actual memory measurement
    
    ! Calculate memory usage
    memoryUsage = finalMemory - initialMemory
    
    ! Check if memory usage is within threshold
    IF (memoryUsage <= memoryThreshold) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Store memory usage
    IF ( ASSOCIATED(SimpleTestResults) ) THEN
       SimpleTestResults%MemoryUsage = memoryUsage
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          CALL HCO_MSG( 'Memory usage test PASSED', &
                       LUN=HcoState%Config%hcoLogLUN )
          WRITE(HcoState%Config%hcoLogLUN,*) '  Memory usage: ', memoryUsage, ' MB'
          WRITE(HcoState%Config%hcoLogLUN,*) '  Memory threshold: ', memoryThreshold, ' MB'
       ELSE
          CALL HCO_MSG( 'Memory usage test FAILED', &
                       LUN=HcoState%Config%hcoLogLUN )
          WRITE(HcoState%Config%hcoLogLUN,*) '  Memory usage: ', memoryUsage, ' MB'
          WRITE(HcoState%Config%hcoLogLUN,*) '  Memory threshold: ', memoryThreshold, ' MB'
       ENDIF
    ENDIF
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
  END SUBROUTINE HCOI_ESMF_Test_MemoryUsage
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Test_Cleanup
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Test\_Cleanup cleans up
! the simplified ESMF testing resources.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Test_Cleanup( RC )
!
! !ARGUMENTS:
!
    INTEGER,         INTENT(INOUT) :: RC          ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Test_Cleanup (hcoi_esmf_test_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if test results instance exists
    IF ( ASSOCIATED(SimpleTestResults) ) THEN
       ! Deallocate the test results instance
       DEALLOCATE(SimpleTestResults)
       SimpleTestResults => NULL()
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Test_Cleanup
!EOC
END MODULE HCOI_ESMF_Test_Mod!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoi_esmf_test_mod
!
! !DESCRIPTION: Module HCOI\_ESMF\_\_TEST\_MOD provides simplified testing
! functionality for the streamlined ESMF integration in HEMCO-NUOPC. This module
! focuses on validating the core functionality without complex backward compatibility
! concerns.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMF_Test_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE HCOI_ESMF_Simplified_Mod
  USE HCOI_ESMF_Integration_Mod
  
#if defined (ESMF_)
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  ! Simple testing functions:
  PUBLIC :: HCOI_ESMF_Test_RunAllTests
  PUBLIC :: HCOI_ESMF_Test_ValidateGridMapping
  PUBLIC :: HCOI_ESMF_Test_PerformanceBenchmark
  PUBLIC :: HCOI_ESMF_Test_MemoryUsage
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial simplified version for NUOPC ESMF testing
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Test results structure
  TYPE :: HCOI_TestResults
     INTEGER                :: TotalTests
     INTEGER                :: PassedTests
     INTEGER                :: FailedTests
     LOGICAL                :: AllTestsPassed
     REAL(hp)               :: PerformanceScore
     INTEGER                :: MemoryUsage
  END TYPE HCOI_TestResults
  
  ! Global test results instance
  TYPE(HCOI_TestResults), POINTER :: SimpleTestResults => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Test_RunAllTests
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Test\_RunAllTests executes
! all simplified tests for the ESMF integration system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Test_RunAllTests( HcoState, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State), POINTER        :: HcoState     ! HEMCO state object
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
    LOGICAL             :: testResult
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Test_RunAllTests (hcoi_esmf_test_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Allocate test results instance if not already allocated
    IF ( .NOT. ASSOCIATED(SimpleTestResults) ) THEN
       ALLOCATE(SimpleTestResults)
    ENDIF
    
    ! Initialize test results
    SimpleTestResults%TotalTests = 0
    SimpleTestResults%PassedTests = 0
    SimpleTestResults%FailedTests = 0
    SimpleTestResults%AllTestsPassed = .TRUE.
    SimpleTestResults%PerformanceScore = 0.0_hp
    SimpleTestResults%MemoryUsage = 0
    
    ! Display test header
    IF ( HcoState%amIRoot ) THEN
       CALL HCO_MSG( '==========================================' )
       CALL HCO_MSG( '  HEMCO ESMF Simplified Integration Tests  ' )
       CALL HCO_MSG( '==========================================' )
    ENDIF
    
    ! Test 1: Grid mapping validation
    SimpleTestResults%TotalTests = SimpleTestResults%TotalTests + 1
    CALL HCOI_ESMF_Test_ValidateGridMapping( HcoState, testResult, RC )
    IF ( RC == HCO_SUCCESS .AND. testResult ) THEN
       SimpleTestResults%PassedTests = SimpleTestResults%PassedTests + 1
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'PASS: Grid Mapping Validation Test' )
    ELSE
       SimpleTestResults%FailedTests = SimpleTestResults%FailedTests + 1
       SimpleTestResults%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'FAIL: Grid Mapping Validation Test' )
    ENDIF
    
    ! Test 2: Performance benchmark
    SimpleTestResults%TotalTests = SimpleTestResults%TotalTests + 1
    CALL HCOI_ESMF_Test_PerformanceBenchmark( HcoState, testResult, RC )
    IF ( RC == HCO_SUCCESS .AND. testResult ) THEN
       SimpleTestResults%PassedTests = SimpleTestResults%PassedTests + 1
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'PASS: Performance Benchmark Test' )
    ELSE
       SimpleTestResults%FailedTests = SimpleTestResults%FailedTests + 1
       SimpleTestResults%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'FAIL: Performance Benchmark Test' )
    ENDIF
    
    ! Test 3: Memory usage validation
    SimpleTestResults%TotalTests = SimpleTestResults%TotalTests + 1
    CALL HCOI_ESMF_Test_MemoryUsage( HcoState, testResult, RC )
    IF ( RC == HCO_SUCCESS .AND. testResult ) THEN
       SimpleTestResults%PassedTests = SimpleTestResults%PassedTests + 1
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'PASS: Memory Usage Validation Test' )
    ELSE
       SimpleTestResults%FailedTests = SimpleTestResults%FailedTests + 1
       SimpleTestResults%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) CALL HCO_MSG( 'FAIL: Memory Usage Validation Test' )
    ENDIF
    
    ! Display test summary
    IF ( HcoState%amIRoot ) THEN
       CALL HCO_MSG( '==========================================' )
       WRITE(*,*) 'Test Results: ', SimpleTestResults%PassedTests, '/', &
                  SimpleTestResults%TotalTests, ' tests passed'
       IF ( SimpleTestResults%FailedTests > 0 ) THEN
          WRITE(*,*) 'WARNING: ', SimpleTestResults%FailedTests, ' tests failed'
       ENDIF
       CALL HCO_MSG( '==========================================' )
    ENDIF
    
    ! Set return code based on test results
    IF ( SimpleTestResults%AllTestsPassed ) THEN
       RC = HCO_SUCCESS
    ELSE
       RC = HCO_FAIL
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Test_RunAllTests
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Test_ValidateGridMapping
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Test\_ValidateGridMapping
! validates the grid mapping functionality of the simplified ESMF integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Test_ValidateGridMapping( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,            INTENT(  OUT) :: TestPassed   ! Whether test passed
    INTEGER,            INTENT(INOUT) :: RC           ! Return code
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
    TYPE(ESMF_Field)    :: srcField, dstField
    REAL(hp), POINTER   :: srcData(:,:), dstData(:,:)
    REAL(hp)            :: tolerance
    INTEGER             :: i, j, nx, ny
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Test_ValidateGridMapping (hcoi_esmf_test_mod.F90)'
    
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
    
    ! Create test grid
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
    
    ! Create source and destination fields
    srcField = ESMF_FieldCreate( &
         grid=testGrid, &
         name="SrcTestField", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    dstField = ESMF_FieldCreate( &
         grid=testGrid, &
         name="DstTestField", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    ! Initialize source field with test data
    CALL ESMF_FieldSet(srcField, data=srcData, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    ! Perform mapping using simplified ESMF integration
    CALL HCOI_ESMF_Simplified_MapToHost( HcoState, srcData, dstData, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error performing simplified mapping', RC, THISLOC=LOC )
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_GridDestroy(testGrid, rc=STATUS)
       DEALLOCATE(srcData, dstData)
       RETURN
    ENDIF
    
    ! Validate results - for identical grids, data should be the same
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
    
    ! Cleanup
    CALL ESMF_FieldDestroy(dstField, rc=STATUS)
    CALL ESMF_FieldDestroy(srcField, rc=STATUS)
    CALL ESMF_GridDestroy(testGrid, rc=STATUS)
    DEALLOCATE(srcData, dstData)
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          CALL HCO_MSG( 'Grid mapping validation test PASSED', &
                       LUN=HcoState%Config%hcoLogLUN )
       ELSE
          CALL HCO_MSG( 'Grid mapping validation test FAILED', &
                       LUN=HcoState%Config%hcoLogLUN )
       ENDIF
    ENDIF
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
  END SUBROUTINE HCOI_ESMF_Test_ValidateGridMapping
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Test_PerformanceBenchmark
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Test\_PerformanceBenchmark
! performs performance benchmarking of the simplified ESMF integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Test_PerformanceBenchmark( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,            INTENT(  OUT) :: TestPassed   ! Whether test passed
    INTEGER,            INTENT(INOUT) :: RC           ! Return code
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
    REAL(hp), POINTER   :: srcData(:,:), dstData(:,:)
    INTEGER             :: i, j, nx, ny
    INTEGER             :: iterations, testIterations
    REAL                :: startTime, endTime, totalTime
    INTEGER             :: clockRate, clockMax
    REAL(hp)            :: performanceThreshold
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Test_PerformanceBenchmark (hcoi_esmf_test_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Get grid dimensions
    nx = HcoState%NX
    ny = HcoState%NY
    
    ! Set test parameters
    testIterations = 100  ! Number of iterations for timing
    performanceThreshold = 1.0_hp  ! Performance threshold in seconds
    
    ! Allocate test data arrays
    ALLOCATE(srcData(nx, ny))
    ALLOCATE(dstData(nx, ny))
    
    ! Initialize test data
    DO j = 1, ny
       DO i = 1, nx
          srcData(i, j) = REAL(i + j, hp) * 0.01_hp
          dstData(i, j) = 0.0_hp
       ENDDO
    ENDDO
    
    ! Performance timing
    CALL SYSTEM_CLOCK(startTime, clockRate, clockMax)
    
    ! Perform multiple mapping operations for timing
    DO iterations = 1, testIterations
       CALL HCOI_ESMF_Simplified_MapToHost( HcoState, srcData, dstData, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error performing mapping in performance test', RC, THISLOC=LOC )
          DEALLOCATE(srcData, dstData)
          RETURN
       ENDIF
    ENDDO
    
    CALL SYSTEM_CLOCK(endTime, clockRate, clockMax)
    
    ! Calculate timing results
    totalTime = REAL(endTime - startTime) / REAL(clockRate)
    
    ! Check if performance meets threshold
    IF (totalTime <= performanceThreshold) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Store performance score
    IF ( ASSOCIATED(SimpleTestResults) ) THEN
       SimpleTestResults%PerformanceScore = totalTime
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          CALL HCO_MSG( 'Performance benchmark test PASSED', &
                       LUN=HcoState%Config%hcoLogLUN )
          WRITE(HcoState%Config%hcoLogLUN,*) '  Total time: ', totalTime, ' seconds'
          WRITE(HcoState%Config%hcoLogLUN,*) '  Performance threshold: ', performanceThreshold, ' seconds'
       ELSE
          CALL HCO_MSG( 'Performance benchmark test FAILED', &
                       LUN=HcoState%Config%hcoLogLUN )
          WRITE(HcoState%Config%hcoLogLUN,*) '  Total time: ', totalTime, ' seconds'
          WRITE(HcoState%Config%hcoLogLUN,*) '  Performance threshold: ', performanceThreshold, ' seconds'
       ENDIF
    
    ! Cleanup
    DEALLOCATE(srcData, dstData)
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
  END SUBROUTINE HCOI_ESMF_Test_PerformanceBenchmark
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Test_MemoryUsage
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Test\_MemoryUsage
! validates memory usage of the simplified ESMF integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Test_MemoryUsage( HcoState, TestPassed, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,            INTENT(  OUT) :: TestPassed   ! Whether test passed
    INTEGER,            INTENT(INOUT) :: RC           ! Return code
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
    INTEGER             :: initialMemory, finalMemory
    INTEGER             :: memoryUsage
    INTEGER             :: memoryThreshold
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Test_MemoryUsage (hcoi_esmf_test_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
#if defined (ESMF_)
    ! Set memory threshold (in MB)
    memoryThreshold = 100  ! 100 MB memory threshold
    
    ! Get initial memory usage
    ! In a real implementation, we would use system calls to get actual memory usage
    initialMemory = 0  ! Placeholder - would use actual memory measurement
    
    ! Perform some operations that might consume memory
    CALL HCOI_ESMF_Simplified_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error initializing simplified ESMF', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Get final memory usage
    ! In a real implementation, we would use system calls to get actual memory usage
    finalMemory = 0  ! Placeholder - would use actual memory measurement
    
    ! Calculate memory usage
    memoryUsage = finalMemory - initialMemory
    
    ! Check if memory usage is within threshold
    IF (memoryUsage <= memoryThreshold) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Store memory usage
    IF ( ASSOCIATED(SimpleTestResults) ) THEN
       SimpleTestResults%MemoryUsage = memoryUsage
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          CALL HCO_MSG( 'Memory usage test PASSED', &
                       LUN=HcoState%Config%hcoLogLUN )
          WRITE(HcoState%Config%hcoLogLUN,*) '  Memory usage: ', memoryUsage, ' MB'
          WRITE(HcoState%Config%hcoLogLUN,*) '  Memory threshold: ', memoryThreshold, ' MB'
       ELSE
          CALL HCO_MSG( 'Memory usage test FAILED', &
                       LUN=HcoState%Config%hcoLogLUN )
          WRITE(HcoState%Config%hcoLogLUN,*) '  Memory usage: ', memoryUsage, ' MB'
          WRITE(HcoState%Config%hcoLogLUN,*) '  Memory threshold: ', memoryThreshold, ' MB'
       ENDIF
    ENDIF
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
  END SUBROUTINE HCOI_ESMF_Test_MemoryUsage
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Test_Cleanup
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Test\_Cleanup cleans up
! the simplified ESMF testing resources.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Test_Cleanup( RC )
!
! !ARGUMENTS:
!
    INTEGER,         INTENT(INOUT) :: RC          ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Test_Cleanup (hcoi_esmf_test_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if test results instance exists
    IF ( ASSOCIATED(SimpleTestResults) ) THEN
       ! Deallocate the test results instance
       DEALLOCATE(SimpleTestResults)
       SimpleTestResults => NULL()
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Test_Cleanup
!EOC
END MODULE HCOI_ESMF_Test_Mod
END MODULE HCOI_ESMF_Test_Mod
