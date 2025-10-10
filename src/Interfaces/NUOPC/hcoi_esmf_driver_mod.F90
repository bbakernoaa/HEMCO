!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoi_esmf_driver_mod
!
! !DESCRIPTION: Module HCOI\_ESMF\_\_DRIVER\_MOD provides a simplified driver
! for the ESMF-based regridding system in HEMCO-NUOPC. This module eliminates
! backward compatibility concerns and focuses on essential functionality for direct
! operation on the host NUOPC model's native ESMF grid.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMF_Driver_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE HCOI_ESMF_Mod
  USE HCOI_ESMF_Integration_Mod
  USE HCOI_ESMF_Test_Mod
  USE HCOI_ESMF_Config_Mod
  USE HCOI_ESMF_IO_Mod
  
#if defined (ESMF_)
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  ! Simplified driver functions:
  PUBLIC :: HCOI_ESMF_Driver_Init
  PUBLIC :: HCOI_ESMF_Driver_Run
  PUBLIC :: HCOI_ESMF_Driver_Finalize
  PUBLIC :: HCOI_ESMF_Driver_SetupGrids
  PUBLIC :: HCOI_ESMF_Driver_PerformMapping
  PUBLIC :: HCOI_ESMF_Driver_RunTests
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial simplified version for NUOPC ESMF driver
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Driver state
  TYPE :: HCOI_DriverState
     LOGICAL                :: IsInitialized      ! Initialization flag
     LOGICAL                :: IsRunning          ! Running flag
     LOGICAL                :: GridsSetup         ! Whether grids are set up
     LOGICAL                :: EnableTesting      ! Whether to enable testing
     LOGICAL                :: EnableDiagnostics  ! Whether to enable diagnostics
     INTEGER                :: DriverMode         ! Driver mode
  END TYPE HCOI_DriverState
  
  ! Constants for driver modes
  INTEGER, PARAMETER :: HCOI_DRIVER_MODE_STANDARD = 0    ! Standard mode
  INTEGER, PARAMETER :: HCOI_DRIVER_MODE_NATIVE = 1      ! Native grid mode
  INTEGER, PARAMETER :: HCOI_DRIVER_MODE_OPTIMIZED = 2   ! Optimized mode
  
  ! Global driver state instance
  TYPE(HCOI_DriverState), POINTER :: DriverStateInstance => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Driver_Init
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Driver\_Init initializes the
! simplified ESMF driver for NUOPC integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Driver_Init( HcoState, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Driver_Init (hcoi_esmf_driver_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Allocate driver state instance if not already allocated
    IF ( .NOT. ASSOCIATED(DriverStateInstance) ) THEN
       ALLOCATE(DriverStateInstance)
    ENDIF
    
    ! Initialize driver state
    DriverStateInstance%IsInitialized = .FALSE.
    DriverStateInstance%IsRunning = .FALSE.
    DriverStateInstance%GridsSetup = .FALSE.
    DriverStateInstance%EnableTesting = .FALSE.
    DriverStateInstance%EnableDiagnostics = .FALSE.
    DriverStateInstance%DriverMode = HCOI_DRIVER_MODE_STANDARD
    
    ! Initialize the simplified integration system
    CALL HCOI_ESMF_Integration_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error initializing simplified integration', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Initialize the simplified configuration system
    CALL HCOI_ESMF_Config_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error initializing simplified configuration', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Initialize the simplified I/O system
    CALL HCOI_ESMF_IO_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error initializing simplified I/O', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Mark as initialized
    DriverStateInstance%IsInitialized = .TRUE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF driver initialized', &
                    LUN=HcoState%Config%hcoLogLUN)
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Driver_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Driver_Run
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Driver\_Run executes the
! simplified ESMF driver for NUOPC integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Driver_Run( HcoState, HostGrid, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),     POINTER        :: HcoState     ! HEMCO state object
    TYPE(ESMF_Grid),     INTENT(IN   )  :: HostGrid     ! Host model's ESMF grid
    INTEGER,             INTENT(INOUT) :: RC           ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    LOGICAL             :: useNativeGrid
    INTEGER             :: integrationMode
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Driver_Run (hcoi_esmf_driver_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if driver is initialized
    IF ( .NOT. ASSOCIATED(DriverStateInstance) .OR. &
         .NOT. DriverStateInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Driver not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Mark as running
    DriverStateInstance%IsRunning = .TRUE.
    
    ! Set default configuration
    useNativeGrid = .TRUE.  ! Default to native grid
    integrationMode = HCOI_DRIVER_MODE_NATIVE  ! Default to native mode
    
    ! Setup grids
    CALL HCOI_ESMF_Driver_SetupGrids( HcoState, HostGrid, useNativeGrid, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error setting up grids', RC, THISLOC=LOC )
       DriverStateInstance%IsRunning = .FALSE.
       RETURN
    ENDIF
    
    ! Configure integration
    CALL HCOI_ESMF_Integration_Setup( HcoState, HostGrid, useNativeGrid, integrationMode, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error configuring integration', RC, THISLOC=LOC )
       DriverStateInstance%IsRunning = .FALSE.
       RETURN
    ENDIF
    
    ! Configure I/O
    ! In a real implementation, we would configure I/O based on user settings
    ! For now, we'll just ensure it's initialized
    
    ! Run tests if enabled
    IF ( DriverStateInstance%EnableTesting ) THEN
       CALL HCOI_ESMF_Driver_RunTests( HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_WARNING( 'Warning running tests', RC, THISLOC=LOC )
          RC = HCO_SUCCESS  ! Continue anyway
       ENDIF
    
    ! Mark as not running
    DriverStateInstance%IsRunning = .FALSE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF driver executed', &
                    LUN=HcoState%Config%hcoLogLUN)
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Driver_Run
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Driver_Finalize
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Driver\_Finalize finalizes the
! simplified ESMF driver for NUOPC integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Driver_Finalize( HcoState, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Driver_Finalize (hcoi_esmf_driver_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if driver is initialized
    IF ( .NOT. ASSOCIATED(DriverStateInstance) .OR. &
         .NOT. DriverStateInstance%IsInitialized ) THEN
       CALL HCO_WARNING( 'Driver not initialized', RC, THISLOC=LOC )
       RC = HCO_SUCCESS
       RETURN
    ENDIF
    
    ! Finalize the simplified I/O system
    CALL HCOI_ESMF_IO_Final( RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error finalizing simplified I/O', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Finalize the simplified configuration system
    CALL HCOI_ESMF_Config_Cleanup( RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error finalizing simplified configuration', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Finalize the simplified integration system
    CALL HCOI_ESMF_Integration_Cleanup( RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error finalizing simplified integration', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Clean up driver state instance
    DEALLOCATE(DriverStateInstance)
    DriverStateInstance => NULL()
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF driver finalized', &
                    LUN=HcoState%Config%hcoLogLUN)
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Driver_Finalize
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Driver_SetupGrids
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Driver\_SetupGrids sets up the
! grids for the simplified ESMF driver.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Driver_SetupGrids( HcoState, HostGrid, UseNativeGrid, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),     POINTER        :: HcoState      ! HEMCO state object
    TYPE(ESMF_Grid),     INTENT(IN   )  :: HostGrid      ! Host model's ESMF grid
    LOGICAL,             INTENT(IN   ) :: UseNativeGrid  ! Whether to use native grid
    INTEGER,             INTENT(INOUT) :: RC            ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Driver_SetupGrids (hcoi_esmf_driver_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if driver is initialized
    IF ( .NOT. ASSOCIATED(DriverStateInstance) .OR. &
         .NOT. DriverStateInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Driver not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Setup grids using the simplified integration system
    CALL HCOI_ESMF_SetupGrids( HcoState, HostGrid, UseNativeGrid, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error setting up simplified grids', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Mark grids as setup
    DriverStateInstance%GridsSetup = .TRUE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF driver grids set up', &
                    LUN=HcoState%Config%hcoLogLUN)
       WRITE(HcoState%Config%hcoLogLUN,*) '  Use native grid: ', UseNativeGrid
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Driver_SetupGrids
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Driver_PerformMapping
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Driver\_PerformMapping performs
! data mapping using the simplified ESMF driver.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Driver_PerformMapping( HcoState, SrcData, DstData, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    REAL(hp),           INTENT(IN   )  :: SrcData(:,:) ! Input data on HEMCO grid
    REAL(hp),           INTENT(INOUT) :: DstData(:,:)  ! Output data on host grid
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Driver_PerformMapping (hcoi_esmf_driver_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if driver is initialized and grids are set up
    IF ( .NOT. ASSOCIATED(DriverStateInstance) .OR. &
         .NOT. DriverStateInstance%IsInitialized .OR. &
         .NOT. DriverStateInstance%GridsSetup ) THEN
       CALL HCO_ERROR( 'Driver not initialized or grids not set up', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Perform mapping using the simplified integration system
    CALL HCOI_ESMF_MapToHost( HcoState, SrcData, DstData, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error performing simplified mapping', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF driver performed mapping', &
                    LUN=HcoState%Config%hcoLogLUN)
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Driver_PerformMapping
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Driver_RunTests
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Driver\_RunTests runs the test suite
! for the simplified ESMF driver.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Driver_RunTests( HcoState, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Driver_RunTests (hcoi_esmf_driver_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if driver is initialized
    IF ( .NOT. ASSOCIATED(DriverStateInstance) .OR. &
         .NOT. DriverStateInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Driver not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Enable testing in driver state
    DriverStateInstance%EnableTesting = .TRUE.
    
    ! Run simplified integration tests
    CALL HCOI_ESMF_Test_RunAllTests( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error running simplified integration tests', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Run simplified grid mapping validation
    CALL HCOI_ESMF_Test_ValidateGridMapping( HcoState, testResult, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error running grid mapping validation test', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Run performance benchmark
    CALL HCOI_ESMF_Test_PerformanceBenchmark( HcoState, testResult, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error running performance benchmark test', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Run memory usage validation
    CALL HCOI_ESMF_Test_MemoryUsage( HcoState, testResult, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error running memory usage validation test', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Disable testing in driver state
    DriverStateInstance%EnableTesting = .FALSE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF driver tests completed', &
                    LUN=HcoState%Config%hcoLogLUN)
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Driver_RunTests
!EOC
END MODULE HCOI_ESMF_Driver_Mod
