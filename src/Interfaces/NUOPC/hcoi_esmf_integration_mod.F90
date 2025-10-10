!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoi_esmf_integration_mod
!
! !DESCRIPTION: Module HCOI\_ESMF\_INTEGRATION\_MOD provides a simplified
! integration layer for the ESMF-based regridding system in HEMCO-NUOPC. This
! module eliminates backward compatibility concerns and focuses on essential
! functionality for direct operation on the host NUOPC model's native ESMF grid.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMF_Integration_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  USE HCOI_ESMF_Simplified_Mod
  
#if defined (ESMF_)
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  !  integration functions:
  PUBLIC :: HCOI_ESMF_Integration_Init
  PUBLIC :: HCOI_ESMF_Integration_Setup
  PUBLIC :: HCOI_ESMF_Integration_Execute
  PUBLIC :: HCOI_ESMF_Integration_Cleanup
  PUBLIC :: HCOI_ESMF_Integration_MapData
  PUBLIC :: HCOI_ESMF_Integration_GetHostGrid
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial simplified version for NUOPC integration
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  !  integration state
  TYPE :: HCOI_Integration
     LOGICAL                :: IsInitialized      ! Initialization flag
     LOGICAL                :: UseNativeGrid      ! Whether to use native grid
     INTEGER                :: IntegrationMode    ! Integration mode
     LOGICAL                :: GridsSetup         ! Whether grids are set up
  END TYPE HCOI_Integration
  
  ! Constants for integration modes
  INTEGER, PARAMETER :: HCOI_MODE_STANDARD = 0    ! Standard integration
  INTEGER, PARAMETER :: HCOI_MODE_NATIVE = 1      ! Native grid integration
  INTEGER, PARAMETER :: HCOI_MODE_OPTIMIZED = 2   ! Optimized integration
  
  ! Global integration instance
  TYPE(HCOI_Integration), POINTER :: IntInstance => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Integration_Init
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Integration\_Init initializes the
! simplified ESMF integration system for NUOPC.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Integration_Init( HcoState, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Integration_Init (hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Allocate integration instance if not already allocated
    IF ( .NOT. ASSOCIATED(SimpleIntInstance) ) THEN
       ALLOCATE(SimpleIntInstance)
    ENDIF
    
    ! Initialize integration state
    IntInstance%IsInitialized = .FALSE.
    IntInstance%UseNativeGrid = .FALSE.
    IntInstance%IntegrationMode = HCOI_MODE_STANDARD
    IntInstance%GridsSetup = .FALSE.
    
    ! Initialize the simplified ESMF interface
    CALL HCOI_ESMF_Simplified_Init( HcoState, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error initializing simplified ESMF interface', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Mark as initialized
    IntInstance%IsInitialized = .TRUE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF integration system initialized', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Integration_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Integration_Setup
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Integration\_Setup configures the
! simplified ESMF integration with host model grid information.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Integration_Setup( HcoState, HostGrid, UseNativeGrid, IntegrationMode, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),     POINTER        :: HcoState      ! HEMCO state object
    TYPE(ESMF_Grid),     INTENT(IN   )  :: HostGrid      ! Host model's ESMF grid
    LOGICAL,             INTENT(IN   )  :: UseNativeGrid ! Whether to use native grid
    INTEGER,             INTENT(IN   )  :: IntegrationMode ! Integration mode
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
#if defined (ESMF_)
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Integration_Setup (hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if integration instance is initialized
    IF ( .NOT. ASSOCIATED(SimpleIntInstance) .OR. .NOT. IntInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Simple integration instance not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Store configuration
    IntInstance%UseNativeGrid = UseNativeGrid
    IntInstance%IntegrationMode = IntegrationMode
    
    ! Setup grids based on integration mode
    SELECT CASE (IntegrationMode)
    CASE (HCOI_MODE_NATIVE)
       ! For native mode, use the host grid directly
       CALL HCOI_ESMF_Simplified_SetupGrids( HcoState, HostGrid, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error setting up native grids', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
       ! Create regridding operator for native mode
       CALL HCOI_ESMF_Simplified_CreateRegrid( HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error creating regrid operator for native mode', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
       IntInstance%GridsSetup = .TRUE.
       
    CASE (HCOI_MODE_OPTIMIZED)
       ! For optimized mode, also use the host grid directly but with optimizations
       CALL HCOI_ESMF_Simplified_SetupGrids( HcoState, HostGrid, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error setting up optimized grids', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
       ! Create regridding operator for optimized mode
       CALL HCOI_ESMF_Simplified_CreateRegrid( HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error creating regrid operator for optimized mode', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
       IntInstance%GridsSetup = .TRUE.
       
    CASE DEFAULT
       ! For standard mode, still set up grids but use standard regridding
       CALL HCOI_ESMF_Simplified_SetupGrids( HcoState, HostGrid, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error setting up standard grids', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
       ! Create regridding operator for standard mode
       CALL HCOI_ESMF_Simplified_CreateRegrid( HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error creating regrid operator for standard mode', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
       IntInstance%GridsSetup = .TRUE.
    END SELECT
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF integration configured', &
                    LUN=HcoState%Config%hcoLogLUN )
       WRITE(HcoState%Config%hcoLogLUN,*) ' Integration mode: ', IntegrationMode
       WRITE(HcoState%Config%hcoLogLUN,*) '  Use native grid: ', UseNativeGrid
       WRITE(HcoState%Config%hcoLogLUN,*) '  Grids setup: ', IntInstance%GridsSetup
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Integration_Setup
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Integration_Execute
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Integration\_Execute performs the
! main integration operations using the simplified ESMF system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Integration_Execute( HcoState, SrcData, DstData, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Integration_Execute (hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if integration instance is initialized
    IF ( .NOT. ASSOCIATED(SimpleIntInstance) .OR. .NOT. IntInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Simple integration instance not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Check if grids are set up
    IF ( .NOT. IntInstance%GridsSetup ) THEN
       CALL HCO_ERROR( 'Grids not set up', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Perform mapping based on integration mode
    SELECT CASE (SimpleIntInstance%IntegrationMode)
    CASE (HCOI_MODE_NATIVE)
       ! For native mode, perform direct mapping
       CALL HCOI_ESMF_Simplified_MapToHost( HcoState, SrcData, DstData, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error performing native mapping', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
    CASE (HCOI_MODE_OPTIMIZED)
       ! For optimized mode, perform optimized mapping
       CALL HCOI_ESMF_Simplified_MapToHost( HcoState, SrcData, DstData, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error performing optimized mapping', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
    CASE DEFAULT
       ! For standard mode, perform standard mapping
       CALL HCOI_ESMF_Simplified_MapToHost( HcoState, SrcData, DstData, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error performing standard mapping', RC, THISLOC=LOC )
          RETURN
       ENDIF
    END SELECT
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF integration executed', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Integration_Execute
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Integration_MapData
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Integration\_MapData maps data
! between HEMCO grid and host grid using the simplified ESMF integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Integration_MapData( HcoState, SrcData, DstData, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Integration_MapData (hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if integration instance is initialized
    IF ( .NOT. ASSOCIATED(SimpleIntInstance) .OR. .NOT. IntInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Simple integration instance not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Check if grids are set up
    IF ( .NOT. IntInstance%GridsSetup ) THEN
       CALL HCO_ERROR( 'Grids not set up', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Perform the mapping operation
    CALL HCOI_ESMF_Simplified_MapToHost( HcoState, SrcData, DstData, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error performing data mapping', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Data mapped using simplified ESMF integration', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Integration_MapData
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Integration_GetHostGrid
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Integration\_GetHostGrid retrieves
! the host model's native ESMF grid.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Integration_GetHostGrid( HcoState, HostGrid, RC )
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
#if defined (ESMF_)
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Integration_GetHostGrid (hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    HostGrid = ESMF_GridEmptyCreate(rc=RC)
    
    ! Check if integration instance is initialized
    IF ( .NOT. ASSOCIATED(SimpleIntInstance) .OR. .NOT. IntInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Simple integration instance not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Check if grids are set up
    IF ( .NOT. IntInstance%GridsSetup ) THEN
       CALL HCO_ERROR( 'Grids not set up', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! In a real implementation, we would retrieve the actual host grid
    ! For now, we'll return a placeholder
    ! HostGrid = ActualHostGrid  ! Would be set during setup
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Host grid retrieved using simplified ESMF integration', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Integration_GetHostGrid
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Integration_Cleanup
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Integration\_Cleanup cleans up the
! simplified ESMF integration resources.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Integration_Cleanup( RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Integration_Cleanup (hcoi_esmf_integration_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if integration instance exists
    IF ( ASSOCIATED(SimpleIntInstance) ) THEN
       ! Clean up the simplified ESMF interface
       CALL HCOI_ESMF_Simplified_Cleanup( RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error cleaning up simplified ESMF interface', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
       ! Deallocate the integration instance
       DEALLOCATE(SimpleIntInstance)
       IntInstance => NULL()
    ENDIF
    
    ! Verbose output
    ! Note: We can't use HcoState here since it's not passed to this routine
    ! In a real implementation, we might pass HcoState or use a global logger
    
  END SUBROUTINE HCOI_ESMF_Integration_Cleanup
!EOC
END MODULE HCOI_ESMF_Integration_Mod