!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: test_hcoi_esmf_all_mod
!
! !DESCRIPTION: Module TEST\_HCOI\_ESMF\_ALL\_MOD provides a comprehensive test
! driver for the complete HEMCO-NUOPC integration system. This module orchestrates
! all unit tests for the various ESMF components.
!\\
!\\
! !INTERFACE:
!
MODULE TEST_HCOI_ESMF_All_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE TEST_HCOI_ESMF_Mod
  USE TEST_HCOI_ESMF_Config_Mod
  USE TEST_HCOI_ESMF_IO_Mod
  USE TEST_HCOI_ESMF_Regrid_Mod
  USE TEST_HCOI_ESMF_Integration_Mod
  
#if defined (ESMF_)
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: TEST_HCOI_ESMF_ComprehensiveTest
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial version for comprehensive HEMCO-NUOPC tests
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Test results structure
  TYPE :: ComprehensiveTestResults
     INTEGER                :: TotalTests
     INTEGER                :: PassedTests
     INTEGER                :: FailedTests
     LOGICAL                :: AllTestsPassed
     REAL(hp)               :: TotalExecutionTime
  END TYPE ComprehensiveTestResults
  
  ! Global test results instance
  TYPE(ComprehensiveTestResults), POINTER :: ComprehensiveTestResultsInstance => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: TEST_HCOI_ESMF_ComprehensiveTest
!
! !DESCRIPTION: Subroutine TEST\_HCOI\_ESMF\_ComprehensiveTest executes
! a comprehensive test suite for the entire HEMCO-NUOPC ESMF integration system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE TEST_HCOI_ESMF_ComprehensiveTest( HcoState, TestPassed, RC )
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
    REAL                :: startTime, endTime
    INTEGER             :: clockRate, clockMax
    CHARACTER(LEN=255)  :: LOC = 'TEST_HCOI_ESMF_ComprehensiveTest (test_hcoi_esmf_all_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    TestPassed = .FALSE.
    
    ! Allocate comprehensive test results instance if not already allocated
    IF ( .NOT. ASSOCIATED(ComprehensiveTestResultsInstance) ) THEN
       ALLOCATE(ComprehensiveTestResultsInstance)
    ENDIF
    
    ! Initialize test results
    ComprehensiveTestResultsInstance%TotalTests = 0
    ComprehensiveTestResultsInstance%PassedTests = 0
    ComprehensiveTestResultsInstance%FailedTests = 0
    ComprehensiveTestResultsInstance%AllTestsPassed = .TRUE.
    ComprehensiveTestResultsInstance%TotalExecutionTime = 0.0_hp
    
    ! Display comprehensive test header
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '============================================================'
       WRITE(*,*) '  COMPREHENSIVE HEMCO-NUOPC ESMF INTEGRATION TEST SUITE  '
       WRITE(*,*) '============================================================'
       WRITE(*,*) 'Testing all ESMF components for HEMCO-NUOPC integration...'
       WRITE(*,*) ''
    ENDIF
    
    ! Record start time
    CALL SYSTEM_CLOCK(startTime, clockRate, clockMax)
    
    ! Test 1: ESMF Interface Module
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) 'Testing ESMF Interface Module...'
    ENDIF
    
    ComprehensiveTestResultsInstance%TotalTests = ComprehensiveTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_RunAllTests( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ComprehensiveTestResultsInstance%PassedTests = ComprehensiveTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) '  PASS: ESMF Interface Module Tests'
    ELSE
       ComprehensiveTestResultsInstance%FailedTests = ComprehensiveTestResultsInstance%FailedTests + 1
       ComprehensiveTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) '  FAIL: ESMF Interface Module Tests'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 2: ESMF Configuration Module
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) 'Testing ESMF Configuration Module...'
    ENDIF
    
    ComprehensiveTestResultsInstance%TotalTests = ComprehensiveTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Config_RunAllTests( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ComprehensiveTestResultsInstance%PassedTests = ComprehensiveTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) '  PASS: ESMF Configuration Module Tests'
    ELSE
       ComprehensiveTestResultsInstance%FailedTests = ComprehensiveTestResultsInstance%FailedTests + 1
       ComprehensiveTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) '  FAIL: ESMF Configuration Module Tests'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 3: ESMF I/O Module
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) 'Testing ESMF I/O Module...'
    ENDIF
    
    ComprehensiveTestResultsInstance%TotalTests = ComprehensiveTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_IO_RunAllTests( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ComprehensiveTestResultsInstance%PassedTests = ComprehensiveTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) '  PASS: ESMF I/O Module Tests'
    ELSE
       ComprehensiveTestResultsInstance%FailedTests = ComprehensiveTestResultsInstance%FailedTests + 1
       ComprehensiveTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) '  FAIL: ESMF I/O Module Tests'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 4: ESMF Regridding Module
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) 'Testing ESMF Regridding Module...'
    ENDIF
    
    ComprehensiveTestResultsInstance%TotalTests = ComprehensiveTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Regrid_RunAllTests( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ComprehensiveTestResultsInstance%PassedTests = ComprehensiveTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) '  PASS: ESMF Regridding Module Tests'
    ELSE
       ComprehensiveTestResultsInstance%FailedTests = ComprehensiveTestResultsInstance%FailedTests + 1
       ComprehensiveTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) '  FAIL: ESMF Regridding Module Tests'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Test 5: ESMF Integration Module
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) 'Testing ESMF Integration Module...'
    ENDIF
    
    ComprehensiveTestResultsInstance%TotalTests = ComprehensiveTestResultsInstance%TotalTests + 1
    CALL TEST_HCOI_ESMF_Integration_RunAllTests( HcoState, individualTestPassed, RC )
    IF ( RC == HCO_SUCCESS .AND. individualTestPassed ) THEN
       ComprehensiveTestResultsInstance%PassedTests = ComprehensiveTestResultsInstance%PassedTests + 1
       IF ( HcoState%amIRoot ) WRITE(*,*) '  PASS: ESMF Integration Module Tests'
    ELSE
       ComprehensiveTestResultsInstance%FailedTests = ComprehensiveTestResultsInstance%FailedTests + 1
       ComprehensiveTestResultsInstance%AllTestsPassed = .FALSE.
       IF ( HcoState%amIRoot ) WRITE(*,*) '  FAIL: ESMF Integration Module Tests'
       RC = HCO_SUCCESS  ! Continue with other tests
    ENDIF
    
    ! Record end time
    CALL SYSTEM_CLOCK(endTime, clockRate, clockMax)
    ComprehensiveTestResultsInstance%TotalExecutionTime = REAL(endTime - startTime) / REAL(clockRate)
    
    ! Display comprehensive test summary
    IF ( HcoState%amIRoot ) THEN
       WRITE(*,*) '============================================================'
       WRITE(*,*) 'COMPREHENSIVE TEST RESULTS SUMMARY'
       WRITE(*,*) '============================================================'
       WRITE(*,*) 'Total Tests: ', ComprehensiveTestResultsInstance%TotalTests
       WRITE(*,*) 'Passed Tests: ', ComprehensiveTestResultsInstance%PassedTests
       WRITE(*,*) 'Failed Tests: ', ComprehensiveTestResultsInstance%FailedTests
       WRITE(*,*) 'Total Execution Time: ', ComprehensiveTestResultsInstance%TotalExecutionTime, ' seconds'
       WRITE(*,*) ''
       IF ( ComprehensiveTestResultsInstance%AllTestsPassed ) THEN
          WRITE(*,*) 'OVERALL RESULT: ALL TESTS PASSED'
       ELSE
          WRITE(*,*) 'OVERALL RESULT: SOME TESTS FAILED'
       ENDIF
       WRITE(*,*) '============================================================'
    ENDIF
    
    ! Set return code based on test results
    TestPassed = ComprehensiveTestResultsInstance%AllTestsPassed
    IF ( TestPassed ) THEN
       RC = HCO_SUCCESS
    ELSE
       RC = HCO_FAIL
    ENDIF
    
  END SUBROUTINE TEST_HCOI_ESMF_ComprehensiveTest
!EOC
END MODULE TEST_HCOI_ESMF_All_Mod