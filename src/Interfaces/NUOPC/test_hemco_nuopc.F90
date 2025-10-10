!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !PROGRAM: test_hemco_nuopc
!
! !DESCRIPTION: Test driver for the HEMCO NUOPC Grid Component. This program
!  tests the basic functionality of the NUOPC interface including initialization,
!  running, and finalization of the HEMCO component.
!\\
!\\
! !INTERFACE:
!
PROGRAM test_hemco_nuopc
!
! !USES:
!
  USE ESMF
  USE HEMCO_NUOPC_GridCompMod
!
! !REVISION HISTORY:
!  09 Oct 2025 - HEMCO Dev   - Initial version
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
  TYPE(ESMF_GridComp)              :: GC
  TYPE(ESMF_Config)                :: CF
  TYPE(ESMF_Clock)                 :: Clock
  TYPE(ESMF_Time)                  :: StartTime, StopTime
  TYPE(ESMF_TimeInterval)          :: TimeStep
  INTEGER                          :: STATUS
  INTEGER                          :: RC
  CHARACTER(LEN=ESMF_MAXSTR)       :: LogFile
  CHARACTER(LEN=ESMF_MAXSTR)       :: CompName = "HEMCO_NUOPC_Test"
  LOGICAL                          :: IsPet0

  ! Set up traceback
  __Iam__('test_hemco_nuopc')

  ! Initialize ESMF
 CALL ESMF_Initialize(logKindFlag=ESMF_LOGKIND_MULTI, RC=STATUS)
  _VERIFY(STATUS)

  ! Check if this is PET 0
  IsPet0 = ESMF_VMIsPetLocalPet(ESMF_VMGetCurrent(), RC=STATUS)
  _VERIFY(STATUS)

  IF (IsPet0) THEN
     WRITE(*,*) '==============================================='
     WRITE(*,*) 'Testing HEMCO NUOPC Grid Component'
     WRITE(*,*) '==============================================='
  ENDIF

  ! Create a simple grid for testing
  ! In a real application, this would come from the host model
  ! For now, we'll create a small test grid
  ! This will be handled internally by the component

  ! Create the grid component
 GC = ESMF_GridCompCreate(CompName, RC=STATUS)
  _VERIFY(STATUS)

  ! Set the services for the component
  CALL SetServices(GC, RC=STATUS)
  _VERIFY(STATUS)

  ! Create a simple clock for testing
  StartTime = ESMF_TimeSet(ESMF_TimeGetCurrent(), yy=2025, mm=1, dd=1, h=0, RC=STATUS)
  _VERIFY(STATUS)
  StopTime = ESMF_TimeSet(ESMF_TimeGetCurrent(), yy=2025, mm=1, dd=1, h=1, RC=STATUS)
  _VERIFY(STATUS)
  TimeStep = ESMF_TimeIntervalSet(ESMF_TimeIntervalCreate(), s=3600, RC=STATUS)
  _VERIFY(STATUS)

  Clock = ESMF_ClockCreate(StartTime, StopTime, TimeStep, name="HEMCO_Test_Clock", RC=STATUS)
  _VERIFY(STATUS)

  ! Set the clock for the grid component
 CALL ESMF_GridCompSet(GC, CLOCK=Clock, RC=STATUS)
  _VERIFY(STATUS)

  IF (IsPet0) WRITE(*,*) 'Successfully created and configured HEMCO NUOPC component'

  ! Initialize the component (Phase 1)
  CALL ESMF_GridCompInitialize(GC, phase=1, RC=STATUS)
  _VERIFY(STATUS)
  IF (IsPet0) WRITE(*,*) 'Successfully completed Initialize Phase 1'

  ! Initialize the component (Phase 2)
  CALL ESMF_GridCompInitialize(GC, phase=2, RC=STATUS)
  _VERIFY(STATUS)
  IF (IsPet0) WRITE(*,*) 'Successfully completed Initialize Phase 2'

  ! Run the component
  CALL ESMF_GridCompRun(GC, RC=STATUS)
  _VERIFY(STATUS)
  IF (IsPet0) WRITE(*,*) 'Successfully completed Run phase'

  ! Finalize the component
  CALL ESMF_GridCompFinalize(GC, RC=STATUS)
  _VERIFY(STATUS)
  IF (IsPet0) WRITE(*,*) 'Successfully completed Finalize phase'

  ! Destroy the clock
  CALL ESMF_ClockDestroy(Clock, RC=STATUS)
  _VERIFY(STATUS)

  ! Destroy the grid component
  CALL ESMF_GridCompDestroy(GC, RC=STATUS)
  _VERIFY(STATUS)

  IF (IsPet0) THEN
     WRITE(*,*) '==============================================='
     WRITE(*,*) 'Hemco NUOPC Grid Component Test PASSED!'
     WRITE(*,*) '==============================================='
  ENDIF

  ! Finalize ESMF
  CALL ESMF_Finalize(RC=STATUS)
  _VERIFY(STATUS)

  ! Successful return
  STOP 0

END PROGRAM test_hemco_nuopc
!EOC
!------------------------------------------------------------------------------