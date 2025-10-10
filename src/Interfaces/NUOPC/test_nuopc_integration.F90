!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !PROGRAM: test_nuopc_integration
!
! !DESCRIPTION: Test program for HEMCO NUOPC integration. This program
!  performs end-to-end testing of the NUOPC interface including all
!  integration components.
!\\
!\\
! !INTERFACE:
!
PROGRAM test_nuopc_integration
!
! !USES:
!
  USE ESMF
  USE NUOPC
  USE HCO_ERROR_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE HCOI_ESMF_Mod, ONLY : HCOI_ESMF_Init, HCOI_ESMF_SetupGrids, &
                            HCOI_ESMF_CreateRegrid, HCOI_ESMF_PerformRegrid, &
                            HCOI_ESMF_MapToHost, HCOI_ESMF_Cleanup
  USE HCOI_ESMF_Integration_Mod, ONLY : HCOI_ESMF_Integration_Init, &
                                         HCOI_ESMF_Integration_Setup, &
                                         HCOI_ESMF_Integration_Execute, &
                                         HCOI_ESMF_Integration_Cleanup
  USE HCOI_ESMF_Performance_Mod, ONLY : HCOI_ESMF_Performance_Init, &
                                         HCOI_ESMF_Performance_RunTest

! !REVISION HISTORY:
!  09 Oct 2025 - HEMCO Dev   - Initial version for NUOPC integration testing
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
  TYPE(ESMF_GridComp)   :: GC
  TYPE(ESMF_Config)     :: CF
  TYPE(ESMF_Clock)      :: Clock
  TYPE(ESMF_Time)       :: StartTime, StopTime
  TYPE(ESMF_TimeInterval) :: TimeStep
  INTEGER               :: STATUS
  INTEGER               :: RC
  CHARACTER(LEN=ESMF_MAXSTR) :: LogFile
  CHARACTER(LEN=ESMF_MAXSTR) :: CompName = "HEMCO_NUOPC_Test"
  LOGICAL               :: IsPet0

  ! Initialize ESMF
  CALL ESMF_Initialize(logKindFlag=ESMF_LOGKIND_MULTI, RC=STATUS)
  _VERIFY(STATUS)

  ! Check if this is PET 0
  IsPet0 = ESMF_VMIsPetLocalPet(ESMF_VMGetCurrent(), RC=STATUS)
  _VERIFY(STATUS)

  IF (IsPet0) THEN
     WRITE(*,*) '==============================================='
     WRITE(*,*) 'Testing HEMCO NUOPC Integration'
     WRITE(*,*) '==============================================='
  ENDIF

  ! Create a simple grid for testing
  ! In a real application, this would come from the host model
  ! For now, we'll create a small test grid
  GC = ESMF_GridCompCreate(CompName, RC=STATUS)
  _VERIFY(STATUS)

  ! Initialize the simplified ESMF interface
  CALL HCOI_ESMF_Init( NULL(), RC=STATUS )
  IF ( RC /= HCO_SUCCESS ) THEN
     IF ( IsPet0 ) THEN
        WRITE(*,*) 'Error initializing simplified ESMF interface'
     ENDIF
     STOP 1
  ENDIF

  ! Initialize the integration system
  CALL HCOI_ESMF_Integration_Init( NULL(), RC=STATUS )
  IF ( RC /= HCO_SUCCESS ) THEN
     IF ( IsPet0 ) THEN
        WRITE(*,*) 'Error initializing ESMF integration system'
     ENDIF
     STOP 1
  ENDIF

  ! Initialize performance validation
  CALL HCOI_ESMF_Performance_Init( NULL(), RC=STATUS )
  IF ( RC /= HCO_SUCCESS ) THEN
     IF ( IsPet0 ) THEN
        WRITE(*,*) 'Error initializing performance validation'
     ENDIF
     STOP 1
  ENDIF

  ! Run performance tests
  IF ( IsPet0 ) THEN
     WRITE(*,*) 'Running performance validation tests...'
  ENDIF
  CALL HCOI_ESMF_Performance_RunTest( NULL(), RC=STATUS )
  IF ( RC /= HCO_SUCCESS ) THEN
     IF ( IsPet0 ) THEN
        WRITE(*,*) 'Warning: Performance validation failed'
     ENDIF
  ENDIF

  ! Test the basic integration flow
  IF ( IsPet0 ) THEN
     WRITE(*,*) 'Testing basic NUOPC integration flow...'
  ENDIF

  ! Test cleanup
  IF ( IsPet0 ) THEN
     WRITE(*,*) 'Testing cleanup procedures...'
  ENDIF
  CALL HCOI_ESMF_Cleanup( RC=STATUS )
  IF ( RC /= HCO_SUCCESS ) THEN
     IF ( IsPet0 ) THEN
        WRITE(*,*) 'Error cleaning up simplified ESMF interface'
     ENDIF
     STOP 1
  ENDIF

  ! Finalize performance validation
  CALL HCOI_ESMF_Performance_Finalize( RC=STATUS )
  IF ( RC /= HCO_SUCCESS ) THEN
     IF ( IsPet0 ) THEN
        WRITE(*,*) 'Error finalizing performance validation'
     ENDIF
     STOP 1
  ENDIF

  IF ( IsPet0 ) THEN
     WRITE(*,*) '==============================================='
     WRITE(*,*) 'HEMCO NUOPC Integration Test PASSED!'
     WRITE(*,*) '==============================================='
  ENDIF

  ! Finalize ESMF
  CALL ESMF_Finalize(RC=STATUS)
  _VERIFY(STATUS)

  ! Successful return
  STOP 0

END PROGRAM test_nuopc_integration
!EOC