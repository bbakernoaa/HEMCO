!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !PROGRAM: test_hcoi_esmf_main
!
! !DESCRIPTION: Program TEST\_HCOI\_ESMF\_MAIN demonstrates how to run
! comprehensive tests for the HEMCO-NUOPC ESMF integration system.
!\\
!\\
! !INTERFACE:
!
PROGRAM TEST_HCOI_ESMF_Main
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE TEST_HCOI_ESMF_All_Mod
  
  IMPLICIT NONE
  
  ! Local variables
  TYPE(HCO_State), POINTER :: HcoState => NULL()
  INTEGER                  :: RC
  LOGICAL                  :: AllTestsPassed
  
  ! Initialize error handling
  CALL HCO_InitErrorHandling()
  
  ! Initialize HEMCO state (this would normally be done by the HEMCO driver)
  ! For testing purposes, we'll create a minimal state
  CALL HCO_InitState(HcoState, RC)
  IF ( RC /= HCO_SUCCESS ) THEN
     WRITE(*,*) 'Error initializing HEMCO state'
     STOP 1
  ENDIF
  
  ! Set some basic parameters for testing
  HcoState%NX = 10
  HcoState%NY = 10
  HcoState%NZ = 5
  HcoState%amIRoot = .TRUE.
  HcoState%Config%doVerbose = .FALSE.
  
  ! Run comprehensive tests
  WRITE(*,*) 'Starting comprehensive HEMCO-NUOPC ESMF integration tests...'
  CALL TEST_HCOI_ESMF_ComprehensiveTest( HcoState, AllTestsPassed, RC )
  
  IF ( RC == HCO_SUCCESS ) THEN
     IF ( AllTestsPassed ) THEN
        WRITE(*,*) 'SUCCESS: All tests passed!'
        WRITE(*,*) 'HEMCO-NUOPC ESMF integration is working correctly.'
     ELSE
        WRITE(*,*) 'FAILURE: Some tests failed!'
        WRITE(*,*) 'Please review the test output for details.'
     ENDIF
  ELSE
     WRITE(*,*) 'ERROR: Test execution failed with return code ', RC
  ENDIF
  
  ! Cleanup
  CALL HCO_FinalizeState(HcoState, RC)
  
  ! Stop the program
  STOP
END PROGRAM TEST_HCOI_ESMF_Main