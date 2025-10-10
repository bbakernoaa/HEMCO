!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: test_hcoi_esmf_io_mod
!
! !DESCRIPTION: Module TEST\_HCOI\_ESMF\_IO\_MOD provides unit tests for the 
! simplified ESMF I/O module (HCOI\_ESMF\_IO\_MOD) in the HEMCO-NUOPC integration.
!\\
!\\
! !INTERFACE:
!
MODULE TEST_HCOI_ESMF_IO_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE HCOI_ESMF_IO_Mod
  
  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: TEST_HCOI_ESMF_IO_RunAllTests
  PUBLIC :: TEST_HCOI_ESMF_IO_TestInit
  PUBLIC :: TEST_HCOI_ESMF_IO_TestFinal
  PUBLIC :: TEST_HCOI_ESMF_IO_TestReadData
  PUBLIC :: TEST_HCOI_ESMF_IO_TestWriteData
  PUBLIC :: TEST_HCOI_ESMF_IO_TestCacheData
  PUBLIC :: TEST_HCOI_ESMF_IO_TestReadFromCache
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial version for HEMCO-NUOPC I/O tests
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Test results structure
  TYPE :: IOTestResults
     INTEGER                :: TotalTests
     INTEGER                :: PassedTests
     INTEGER                :: FailedTests
     LOGICAL                :: AllTestsPassed
 END TYPE IOTestResults
  
  ! Global test results instance
 TYPE(IOTestResults), POINTER :: IOTestResultsInstance => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_IO_RunAllTests
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_IO\_RunAllTests executes
! all unit tests for the HCOI\_ESMF\_IO\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_IO_RunAllTests( HcoState, TestPassed, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_IO_RunAllTests (test_hcoi_esmf_io_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Allocate test results instance if not already allocated
    IF ( .NOT. ASSOCIATED(IOTestResultsInstance) ) THEN
       ALLOCATE(IOTestResultsInstance)
    ENDIF
    
    ! Initialize test results
    IOTestResultsInstance%TotalTests = 0
    IOTestResultsInstance%PassedTests = 0
    IOTestResultsInstance%FailedTests = 0
    IOTestResultsInstance%AllTestsPassed = .TRUE.
    
    ! Display test header
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '=================================='
       WRITE(*,*) '  HEMCO ESMF I/O Module Unit Tests  '
       WRITE(*,*) '=================================='
    ENDIF
    
    ! Test 1: Initialization
    IOTestResultsInstance%TotalTests = IOTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_IO_TestInit( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IOTestResultsInstance%PassedTests = IOTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: I/O Initialization Test'
    ELSE
       IOTestResultsInstance%FailedTests = IOTestResultsInstance%FailedTests + 1
       IOTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: I/O Initialization Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 2: Finalization
    IOTestResultsInstance%TotalTests = IOTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_IO_TestFinal( individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IOTestResultsInstance%PassedTests = IOTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: I/O Finalization Test'
    ELSE
       IOTestResultsInstance%FailedTests = IOTestResultsInstance%FailedTests + 1
       IOTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: I/O Finalization Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Reinitialize for remaining tests
    CALL HCOI_ESMF_IO_Init( HcoState, RC )
    
    ! Test 3: Write data
    IOTestResultsInstance%TotalTests = IOTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_IO_TestWriteData( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IOTestResultsInstance%PassedTests = IOTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Write Data Test'
    ELSE
       IOTestResultsInstance%FailedTests = IOTestResultsInstance%FailedTests + 1
       IOTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Write Data Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 4: Read data
    IOTestResultsInstance%TotalTests = IOTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_IO_TestReadData( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IOTestResultsInstance%PassedTests = IOTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Read Data Test'
    ELSE
       IOTestResultsInstance%FailedTests = IOTestResultsInstance%FailedTests + 1
       IOTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Read Data Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 5: Cache data
    IOTestResultsInstance%TotalTests = IOTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_IO_TestCacheData( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IOTestResultsInstance%PassedTests = IOTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Cache Data Test'
    ELSE
       IOTestResultsInstance%FailedTests = IOTestResultsInstance%FailedTests + 1
       IOTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Cache Data Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 6: Read from cache
    IOTestResultsInstance%TotalTests = IOTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_IO_TestReadFromCache( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       IOTestResultsInstance%PassedTests = IOTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) 'PASS: Read From Cache Test'
    ELSE
       IOTestResultsInstance%FailedTests = IOTestResultsInstance%FailedTests + 1
       IOTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) 'FAIL: Read From Cache Test'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Display test summary
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '=================================='
       WRITE(*,*) 'Test Results: ', IOTestResultsInstance%PassedTests, '/', &
                  IOTestResultsInstance%TotalTests, ' tests passed'
       IF ( IOTestResultsInstance%FailedTests > 0 ) THEN
          WRITE(*,*) 'WARNING: ', IOTestResultsInstance%FailedTests, ' tests failed'
       ENDIF
       WRITE(*,*) '=================================='
    ENDIF
    
    ! Set return code based on test results
    TestPassed = IOTestResultsInstance%AllTestsPassed
    IF ( TestPassed ) THEN
       RC = HCO_SUCCESS
    ELSE
       RC = HCO_FAIL
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_IO_RunAllTests
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_IO_TestInit
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_IO\_TestInit tests the
! initialization functionality of the HCOI\_ESMF\_IO\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_IO_TestInit( HcoState, TestPassed, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_IO_TestInit (test_hcoi_esmf_io_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Test initialization with valid HEMCO state
    initialRC = HCO_SUCCESS
    CALL HCOI_ESMF_IO_Init( HcoState, initialRC )
    
    ! Check if initialization was successful
    IF ( initialRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'I/O initialization test PASSED'
       ELSE
          WRITE(*,*) 'I/O initialization test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_IO_TestInit
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_IO_TestFinal
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_IO\_TestFinal tests the
! finalization functionality of the HCOI\_ESMF\_IO\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_IO_TestFinal( TestPassed, RC )
!
! !ARGUMENTS:
!
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
    INTEGER             :: finalRC
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_IO_TestFinal (test_hcoi_esmf_io_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Test finalization
    finalRC = HCO_SUCCESS
    CALL HCOI_ESMF_IO_Final( finalRC )
    
    ! Check if finalization was successful
    IF ( finalRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Verbose output
    WRITE(*,*) 'I/O finalization test ', MERGE('PASSED', 'FAILED', TestPassed)
    
  END SUBROUTINE TEST_HCOI_ESMF_IO_TestFinal
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_IO_TestWriteData
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_IO\_TestWriteData tests the
! write data functionality of the HCOI\_ESMF\_IO\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_IO_TestWriteData( HcoState, TestPassed, RC )
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
    REAL(hp), POINTER   :: testData(:,:,:)
    INTEGER             :: i, j, k, nx, ny, nz
    INTEGER             :: writeRC
    CHARACTER(LEN=255)  :: testFile
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_IO_TestWriteData (test_hcoi_esmf_io_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Get dimensions
    nx = HcoState%NX
    ny = HcoState%NY
    nz = MIN(HcoState%NZ, 3)  ! Use at most 3 levels to keep test simple
    
    ! Allocate test data array
    ALLOCATE(testData(nx, ny, nz))
    
    ! Initialize test data with a simple pattern
    DO k = 1, nz
       DO j = 1, ny
          DO i = 1, nx
             testData(i, j, k) = REAL(i + j + k, hp) * 0.01_hp
          ENDDO
       ENDDO
    ENDDO
    
    ! Define test file name
    testFile = 'temp_test_io.nc'
    
    ! Test writing data to file
    writeRC = HCO_SUCCESS
    CALL HCOI_ESMF_IO_WriteData( HcoState, TRIM(testFile), 'test_var', testData, writeRC )
    
    ! Check if write was successful
    IF ( writeRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Clean up
    DEALLOCATE(testData)
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Write data test PASSED'
       ELSE
          WRITE(*,*) 'Write data test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_IO_TestWriteData
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_IO_TestReadData
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_IO\_TestReadData tests the
! read data functionality of the HCOI\_ESMF\_IO\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_IO_TestReadData( HcoState, TestPassed, RC )
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
    REAL(hp), POINTER   :: readData(:,:,:)
    INTEGER             :: i, j, k, nx, ny, nz
    INTEGER             :: readRC
    CHARACTER(LEN=255)  :: testFile
    REAL(hp)            :: tolerance
    LOGICAL             :: dataMatch
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_IO_TestReadData (test_hcoi_esmf_io_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Get dimensions
    nx = HcoState%NX
    ny = HcoState%NY
    nz = MIN(HcoState%NZ, 3)  ! Use at most 3 levels to keep test simple
    
    ! Allocate test data array
    ALLOCATE(readData(nx, ny, nz))
    
    ! Initialize read data array
    DO k = 1, nz
       DO j = 1, ny
          DO i = 1, nx
             readData(i, j, k) = 0.0_hp
          ENDDO
       ENDDO
    ENDDO
    
    ! Define test file name - should match the one from the write test
    testFile = 'temp_test_io.nc'
    
    ! Test reading data from file
    readRC = HCO_SUCCESS
    CALL HCOI_ESMF_IO_ReadData( HcoState, TRIM(testFile), 'test_var', readData, readRC )
    
    ! Check if read was successful
    IF ( readRC == HCO_SUCCESS ) THEN
       ! Verify that data was read correctly (for this test, we'll assume the file exists and has data)
       ! Since we just wrote the file, it should exist and have data
       tolerance = 1.0e-6_hp
       dataMatch = .TRUE.
       DO k = 1, nz
          DO j = 1, ny
             DO i = 1, nx
                ! For this test, we'll just check that values are not zero
                IF ( ABS(readData(i, j, k)) < tolerance ) THEN
                   dataMatch = .FALSE.
                   EXIT
                ENDIF
             ENDDO
             IF ( .NOT. dataMatch ) EXIT
          ENDDO
          IF ( .NOT. dataMatch ) EXIT
       ENDDO
       
       IF ( dataMatch ) THEN
          TestPassed = .TRUE.
       ELSE
          TestPassed = .FALSE.
       ENDIF
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Clean up
    DEALLOCATE(readData)
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Read data test PASSED'
       ELSE
          WRITE(*,*) 'Read data test FAILED'
       ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_IO_TestReadData
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_IO_TestCacheData
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_IO\_TestCacheData tests the
! cache data functionality of the HCOI\_ESMF\_IO\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_IO_TestCacheData( HcoState, TestPassed, RC )
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
    REAL(hp), POINTER   :: testData(:,:,:)
    INTEGER             :: i, j, k, nx, ny, nz
    INTEGER             :: cacheRC
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_IO_TestCacheData (test_hcoi_esmf_io_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Get dimensions
    nx = HcoState%NX
    ny = HcoState%NY
    nz = MIN(HcoState%NZ, 2)  ! Use 2 levels for this test
    
    ! Allocate test data array
    ALLOCATE(testData(nx, ny, nz))
    
    ! Initialize test data with a simple pattern
    DO k = 1, nz
       DO j = 1, ny
          DO i = 1, nx
             testData(i, j, k) = REAL(i * j * k, hp) * 0.02_hp
          ENDDO
       ENDDO
    ENDDO
    
    ! Initialize I/O system if not already done
    CALL HCOI_ESMF_IO_Init( HcoState, RC )
    
    ! Test caching data
    cacheRC = HCO_SUCCESS
    CALL HCOI_ESMF_IO_CacheData( 'test_file.nc', 'test_var', testData, cacheRC, nx, ny, nz )
    
    ! Check if caching was successful
    IF ( cacheRC == HCO_SUCCESS ) THEN
       TestPassed = .TRUE.
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Clean up
    DEALLOCATE(testData)
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Cache data test PASSED'
       ELSE
          WRITE(*,*) 'Cache data test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_IO_TestCacheData
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_IO_TestReadFromCache
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_IO\_TestReadFromCache tests the
! read from cache functionality of the HCOI\_ESMF\_IO\_MOD module.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_IO_TestReadFromCache( HcoState, TestPassed, RC )
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
    REAL(hp), POINTER   :: cachedData(:,:,:)
    INTEGER             :: i, j, k, nx, ny, nz
    INTEGER             :: cacheRC
    LOGICAL             :: found
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_IO_TestReadFromCache (test_hcoi_esmf_io_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Get dimensions
    nx = HcoState%NX
    ny = HcoState%NY
    nz = MIN(HcoState%NZ, 2)  ! Use 2 levels for this test
    
    ! Allocate test data array
    ALLOCATE(cachedData(nx, ny, nz))
    
    ! Initialize cached data array
    DO k = 1, nz
       DO j = 1, ny
          DO i = 1, nx
             cachedData(i, j, k) = 0.0_hp
          ENDDO
       ENDDO
    ENDDO
    
    ! Initialize I/O system if not already done
    CALL HCOI_ESMF_IO_Init( HcoState, RC )
    
    ! First cache some data
    DO k = 1, nz
       DO j = 1, ny
          DO i = 1, nx
             cachedData(i, j, k) = REAL(i + j + k, hp) * 0.03_hp
          ENDDO
       ENDDO
    ENDDO
    
    cacheRC = HCO_SUCCESS
    CALL HCOI_ESMF_IO_CacheData( 'cache_test_file.nc', 'cache_test_var', cachedData, cacheRC, nx, ny, nz )
    IF ( cacheRC /= HCO_SUCCESS ) THEN
       DEALLOCATE(cachedData)
       RETURN
    ENDIF
    
    ! Reset the array to zeros
    DO k = 1, nz
       DO j = 1, ny
          DO i = 1, nx
             cachedData(i, j, k) = 0.0_hp
          ENDDO
       ENDDO
    ENDDO
    
    ! Test reading from cache
    found = .FALSE.
    cacheRC = HCO_SUCCESS
    CALL HCOI_ESMF_IO_ReadFromCache( 'cache_test_file.nc', 'cache_test_var', cachedData, found, cacheRC )
    
    ! Check if reading from cache was successful and data was found
    IF ( cacheRC == HCO_SUCCESS .AND. found ) THEN
       ! Verify that the data is not all zeros (meaning it was retrieved from cache)
       TestPassed = .TRUE.
       DO k = 1, nz
          DO j = 1, ny
             DO i = 1, nx
                IF ( ABS(cachedData(i, j, k)) < 1.0e-10_hp ) THEN
                   TestPassed = .FALSE.
                   EXIT
                ENDIF
             ENDDO
             IF ( .NOT. TestPassed ) EXIT
          ENDDO
          IF ( .NOT. TestPassed ) EXIT
       ENDDO
    ELSE
       TestPassed = .FALSE.
    ENDIF
    
    ! Clean up
    DEALLOCATE(cachedData)
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (TestPassed) THEN
          WRITE(*,*) 'Read from cache test PASSED'
       ELSE
          WRITE(*,*) 'Read from cache test FAILED'
       ENDIF
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_IO_TestReadFromCache
!EOC
END MODULE TEST_HCOI_ESMF_IO_Mod