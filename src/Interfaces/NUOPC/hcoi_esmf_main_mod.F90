!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoi_esmf_main_mod
!
! !DESCRIPTION: Module HCOI\_ESMF\_\_MAIN\_MOD provides the main simplified
! ESMF integration interface for HEMCO-NUOPC. This module eliminates backward
! compatibility concerns and focuses on essential functionality for direct operation
! on the host NUOPC model's native ESMF grid.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMF_Main_Mod
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
  USE HCOI_ESMF_Driver_Mod
  
#if defined (ESMF_)
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  ! Main simplified ESMF functions:
  PUBLIC :: HCOI_ESMF_Main_Init
  PUBLIC :: HCOI_ESMF_Main_Setup
  PUBLIC :: HCOI_ESMF_Main_Execute
  PUBLIC :: HCOI_ESMF_Main_Finalize
  PUBLIC :: HCOI_ESMF_Main_RunTests
  PUBLIC :: HCOI_ESMF_Main_GetHostGrid
  PUBLIC :: HCOI_ESMF_Main_MapData
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial simplified version for NUOPC ESMF main interface
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Main integration state
  TYPE :: HCOI_MainState
     LOGICAL                :: IsInitialized      ! Initialization flag
     LOGICAL                :: IsSetup            ! Setup flag
     LOGICAL                :: IsExecuting        ! Execution flag
     INTEGER                :: IntegrationMode    ! Integration mode
     LOGICAL                :: UseNativeGrid      ! Whether to use native grid
     LOGICAL                :: EnableTesting      ! Whether to enable testing
     LOGICAL                :: EnableDiagnostics  ! Whether to enable diagnostics
  END TYPE HCOI_MainState
  
  ! Constants for integration modes
  INTEGER, PARAMETER :: HCOI_MAIN_MODE_STANDARD = 0    ! Standard mode
  INTEGER, PARAMETER :: HCOI_MAIN_MODE_NATIVE = 1      ! Native grid mode
  INTEGER, PARAMETER :: HCOI_MAIN_MODE_OPTIMIZED = 2   ! Optimized mode
  
  ! Global main state instance
  TYPE(HCOI_MainState), POINTER :: MainStateInstance => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Main_Init
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Main\_Init initializes the main
! simplified ESMF integration system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Main_Init( HcoState, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Main_Init (hcoi_esmf_main_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Allocate main state instance if not already allocated
    IF ( .NOT. ASSOCIATED(MainStateInstance) ) THEN
       ALLOCATE(MainStateInstance)
    ENDIF
    
    ! Initialize main state
    MainStateInstance%IsInitialized = .FALSE.
    MainStateInstance%IsSetup = .FALSE.
    MainStateInstance%IsExecuting = .FALSE.
    MainStateInstance%IntegrationMode = HCOI_MAIN_MODE_STANDARD
    MainStateInstance%UseNativeGrid = .FALSE.
    MainStateInstance%EnableTesting = .FALSE.
    MainStateInstance%EnableDiagnostics = .FALSE.
    
    ! Initialize the simplified ESMF components
    CALL HCOI_ESMF_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error initializing simplified ESMF', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
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
    
    ! Initialize the simplified driver system
    CALL HCOI_ESMF_Driver_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error initializing simplified driver', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Mark as initialized
    MainStateInstance%IsInitialized = .TRUE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF main integration system initialized', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Main_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Main_Setup
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Main\_Setup configures the main
! simplified ESMF integration system with host model grid information.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Main_Setup( HcoState, HostGrid, UseNativeGrid, IntegrationMode, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),     POINTER        :: HcoState        ! HEMCO state object
    TYPE(ESMF_Grid),     INTENT(IN   )  :: HostGrid        ! Host model's ESMF grid
    LOGICAL,             INTENT(IN   )  :: UseNativeGrid    ! Whether to use native grid
    INTEGER,             INTENT(IN   )  :: IntegrationMode  ! Integration mode
    INTEGER,             INTENT(INOUT) :: RC              ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Main_Setup (hcoi_esmf_main_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if main system is initialized
    IF ( .NOT. ASSOCIATED(MainStateInstance) .OR. .NOT. MainStateInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Main system not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Store configuration
    MainStateInstance%UseNativeGrid = UseNativeGrid
    MainStateInstance%IntegrationMode = IntegrationMode
    
    ! Setup the simplified integration system with host grid information
    CALL HCOI_ESMF_Integration_Setup( HcoState, HostGrid, UseNativeGrid, IntegrationMode, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error setting up simplified integration', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Setup the simplified ESMF interface with host grid information
    CALL HCOI_ESMF_SetupGrids( HcoState, HostGrid, UseNativeGrid, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error setting up simplified ESMF interface grids', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Setup the simplified configuration with host grid information
    CALL HCOI_ESMF_Config_Setup( HcoState, HostGrid, UseNativeGrid, IntegrationMode, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error setting up simplified configuration', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Setup the simplified I/O with host grid information
    CALL HCOI_ESMF_IO_Setup( HcoState, HostGrid, UseNativeGrid, IntegrationMode, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error setting up simplified I/O', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Setup the simplified driver with host grid information
    CALL HCOI_ESMF_Driver_Setup( HcoState, HostGrid, UseNativeGrid, IntegrationMode, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error setting up simplified driver', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Mark as setup
    MainStateInstance%IsSetup = .TRUE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF main integration system configured', &
                    LUN=HcoState%Config%hcoLogLUN )
       WRITE(HcoState%Config%hcoLogLUN,*) '  Integration mode: ', IntegrationMode
       WRITE(HcoState%Config%hcoLogLUN,*) '  Use native grid: ', UseNativeGrid
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Main_Setup
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Main_Execute
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Main\_Execute performs the main
! execution of the simplified ESMF integration system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Main_Execute( HcoState, SrcData, DstData, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Main_Execute (hcoi_esmf_main_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if main system is initialized and setup
    IF ( .NOT. ASSOCIATED(MainStateInstance) .OR. &
         .NOT. MainStateInstance%IsInitialized .OR. &
         .NOT. MainStateInstance%IsSetup ) THEN
       CALL HCO_ERROR( 'Main system not initialized or setup', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Mark as executing
    MainStateInstance%IsExecuting = .TRUE.
    
    ! Execute based on integration mode
    SELECT CASE (MainStateInstance%IntegrationMode)
    CASE (HCOI_MAIN_MODE_NATIVE)
       ! For native mode, use direct mapping
       CALL HCOI_ESMF_MapToHost( HcoState, SrcData, DstData, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error performing native mapping', RC, THISLOC=LOC )
          MainStateInstance%IsExecuting = .FALSE.
          RETURN
       ENDIF
       
    CASE (HCOI_MAIN_MODE_OPTIMIZED)
       ! For optimized mode, use optimized mapping
       CALL HCOI_ESMF_MapToHost( HcoState, SrcData, DstData, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error performing optimized mapping', RC, THISLOC=LOC )
          MainStateInstance%IsExecuting = .FALSE.
          RETURN
       ENDIF
       
    CASE DEFAULT
       ! For standard mode, use standard mapping
       CALL HCOI_ESMF_MapToHost( HcoState, SrcData, DstData, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error performing standard mapping', RC, THISLOC=LOC )
          MainStateInstance%IsExecuting = .FALSE.
          RETURN
       ENDIF
    END SELECT
    
    ! Perform I/O operations if needed
    CALL HCOI_ESMF_IO_Execute( HcoState, SrcData, DstData, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_WARNING( 'Warning performing I/O operations', RC, THISLOC=LOC )
       RC = HCO_SUCCESS  ! Continue anyway
    ENDIF
    
    ! Mark as not executing
    MainStateInstance%IsExecuting = .FALSE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF main integration system executed', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Main_Execute
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Main_Finalize
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Main\_Finalize finalizes the main
! simplified ESMF integration system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Main_Finalize( HcoState, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Main_Finalize (hcoi_esmf_main_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if main system is initialized
    IF ( .NOT. ASSOCIATED(MainStateInstance) .OR. .NOT. MainStateInstance%IsInitialized ) THEN
       CALL HCO_WARNING( 'Main system not initialized', RC, THISLOC=LOC )
       RC = HCO_SUCCESS
       RETURN
    ENDIF
    
    ! Finalize the simplified driver system
    CALL HCOI_ESMF_Driver_Finalize( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error finalizing simplified driver', RC, THISLOC=LOC )
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
    
    ! Finalize the simplified ESMF interface
    CALL HCOI_ESMF_Cleanup( RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error finalizing simplified ESMF interface', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Clean up main state instance
    DEALLOCATE(MainStateInstance)
    MainStateInstance => NULL()
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF main integration system finalized', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Main_Finalize
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Main_RunTests
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Main\_RunTests runs the test suite
! for the simplified ESMF integration system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Main_RunTests( HcoState, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Main_RunTests (hcoi_esmf_main_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if main system is initialized
    IF ( .NOT. ASSOCIATED(MainStateInstance) .OR. .NOT. MainStateInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Main system not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Enable testing in main state
    MainStateInstance%EnableTesting = .TRUE.
    
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
    
    ! Run simplified driver tests
    CALL HCOI_ESMF_Driver_RunTests( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error running simplified driver tests', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Run simplified I/O tests
    CALL HCOI_ESMF_IO_RunTests( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error running simplified I/O tests', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Disable testing in main state
    MainStateInstance%EnableTesting = .FALSE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF main integration tests completed', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Main_RunTests
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Main_GetHostGrid
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Main\_GetHostGrid retrieves the
! host model's native ESMF grid.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Main_GetHostGrid( HcoState, HostGrid, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    TYPE(ESMF_Grid),    INTENT(  OUT)  :: HostGrid     ! Output: Host model's ESMF grid
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Main_GetHostGrid (hcoi_esmf_main_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    HostGrid = ESMF_GridEmptyCreate(rc=RC)
    
    ! Check if main system is initialized and setup
    IF ( .NOT. ASSOCIATED(MainStateInstance) .OR. &
         .NOT. MainStateInstance%IsInitialized .OR. &
         .NOT. MainStateInstance%IsSetup ) THEN
       CALL HCO_ERROR( 'Main system not initialized or setup', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Get the host grid from the simplified integration system
    CALL HCOI_ESMF_Integration_GetHostGrid( HostGrid, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error getting host grid from simplified integration', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Host grid retrieved from simplified ESMF main integration', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Main_GetHostGrid
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Main_MapData
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Main\_MapData maps data between
! HEMCO grid and host grid using the simplified ESMF integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Main_MapData( HcoState, SrcData, DstData, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Main_MapData (hcoi_esmf_main_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if main system is initialized and setup
    IF ( .NOT. ASSOCIATED(MainStateInstance) .OR. &
         .NOT. MainStateInstance%IsInitialized .OR. &
         .NOT. MainStateInstance%IsSetup ) THEN
       CALL HCO_ERROR( 'Main system not initialized or setup', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Map data using the simplified integration system
    CALL HCOI_ESMF_Integration_MapData( HcoState, SrcData, DstData, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error mapping data using simplified integration', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Data mapped using simplified ESMF main integration', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Main_MapData
!EOC
END MODULE HCOI_ESMF_Main_Mod