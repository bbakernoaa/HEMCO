!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoi_esmf_mod
!
! !DESCRIPTION: Module HCOI\_ESMF\_SIMPLIFIED\_MOD provides a simplified ESMF 
! interface for the NUOPC integration without backward compatibility concerns.
! This module focuses on the essential functionality needed for direct operation
! on the host NUOPC model's native ESMF grid.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMF_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  
#if defined (ESMF_)
  USE ESMF
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: HCOI_ESMF_Init
  PUBLIC :: HCOI_ESMF_SetupGrids
  PUBLIC :: HCOI_ESMF_CreateRegrid
  PUBLIC :: HCOI_ESMF_PerformRegrid
  PUBLIC :: HCOI_ESMF_MapToHost
  PUBLIC :: HCOI_ESMF_Cleanup
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial simplified version for NUOPC
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Simplified ESMF grid structure
  TYPE :: HCOI_Grid
     TYPE(ESMF_Grid)        :: Grid
     INTEGER                :: NX, NY
     LOGICAL                :: IsInitialized
  END TYPE HCOI_Grid
  
  ! Simplified regridding operator
  TYPE :: HCOI_Regrid
     TYPE(ESMF_FieldRegrid) :: Operator
     LOGICAL                :: IsCreated
  END TYPE HCOI_Regrid
  
  ! Global instances
  TYPE(HCOI_Grid), POINTER :: HEMCOGrid => NULL()
  TYPE(HCOI_Grid), POINTER :: HostGrid => NULL()
  TYPE(HCOI_Regrid), POINTER :: RegridOp => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Init
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Simplified\_Init initializes the 
! simplified ESMF interface for NUOPC integration.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Init( HcoState, RC )
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
    CHARACTER(LEN=255) :: LOC = 'HCOI_ESMF_Init (hcoi_esmf_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Allocate grid instances if not already allocated
    IF ( .NOT. ASSOCIATED(HEMCOGrid) ) THEN
       ALLOCATE(HEMCOGrid)
       HEMCOGrid%IsInitialized = .FALSE.
    ENDIF
    
    IF ( .NOT. ASSOCIATED(HostGrid) ) THEN
       ALLOCATE(HostGrid)
       HostGrid%IsInitialized = .FALSE.
    ENDIF
    
    ! Allocate regrid operator if not already allocated
    IF ( .NOT. ASSOCIATED(RegridOp) ) THEN
       ALLOCATE(RegridOp)
       RegridOp%IsCreated = .FALSE.
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG('Simplified ESMF interface initialized', &
                    LUN=HcoState%Config%hcoLogLUN)
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_SetupGrids
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Simplified\_SetupGrids sets up the 
! HEMCO and host grids for ESMF operations.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_SetupGrids( HcoState, HostESMFGrid, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState      ! HEMCO state object
    TYPE(ESMF_Grid),    INTENT(IN   )  :: HostESMFGrid  ! Host model's ESMF grid
    INTEGER,            INTENT(INOUT) :: RC            ! Return code
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_SetupGrids (hcoi_esmf_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
#if defined (ESMF_)
    ! Setup HEMCO grid
    HEMCOGrid%NX = HcoState%NX
    HEMCOGrid%NY = HcoState%NY
    HEMCOGrid%Grid = ESMF_GridCreateNoPeriDim( &
         maxIndex=(/HEMCOGrid%NX, HEMCOGrid%NY/), &
         indexFlag=ESMF_INDEX_GLOBAL, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    HEMCOGrid%IsInitialized = .TRUE.
    
    ! Setup Host grid
    HostGrid%Grid = HostESMFGrid
    HostGrid%NX = HcoState%NX  ! In a real implementation, we would get this from the host grid
    HostGrid%NY = HcoState%NY  ! In a real implementation, we would get this from the host grid
    HostGrid%IsInitialized = .TRUE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG('ESMF grids set up successfully', &
                    LUN=HcoState%Config%hcoLogLUN)
    ENDIF
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
  END SUBROUTINE HCOI_ESMF_SetupGrids
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_CreateRegrid
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Simplified\_CreateRegrid creates the 
! ESMF regridding operator for mapping between HEMCO and host grids.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_CreateRegrid( HcoState, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
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
    TYPE(ESMF_Field)    :: srcField, dstField
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_CreateRegrid (hcoi_esmf_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
#if defined (ESMF_)
    ! Check if grids are initialized
    IF ( .NOT. HEMCOGrid%IsInitialized .OR. .NOT. HostGrid%IsInitialized ) THEN
       CALL HCO_ERROR('Grids not initialized', RC, THISLOC=LOC)
       RETURN
    ENDIF
    
    ! Create source and destination fields
    srcField = ESMF_FieldCreate( &
         grid=HEMCOGrid%Grid, &
         name="HEMCO_Field", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    dstField = ESMF_FieldCreate( &
         grid=HostGrid%Grid, &
         name="Host_Field", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Create regridding operator
    RegridOp%Operator = ESMF_FieldRegridStore( &
         srcField=srcField, &
         dstField=dstField, &
         regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
         unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    RegridOp%IsCreated = .TRUE.
    
    ! Cleanup fields (regrid operator holds references)
    CALL ESMF_FieldDestroy(srcField, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    CALL ESMF_FieldDestroy(dstField, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG('ESMF regridding operator created successfully', &
                    LUN=HcoState%Config%hcoLogLUN)
    ENDIF
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
  END SUBROUTINE HCOI_ESMF_CreateRegrid
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_PerformRegrid
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Simplified\_PerformRegrid performs the 
! actual regridding operation using the ESMF regridding operator.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_PerformRegrid( HcoState, SrcData, DstData, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    REAL(hp),           INTENT(IN   )  :: SrcData(:,:) ! Source data (HEMCO grid)
    REAL(hp),           INTENT(INOUT) :: DstData(:,:)  ! Destination data (Host grid)
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
    TYPE(ESMF_Field)    :: srcField, dstField
    TYPE(ESMF_Array)    :: srcArray, dstArray
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_PerformRegrid (hcoi_esmf_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
#if defined (ESMF_)
    ! Check if regrid operator is created
    IF ( .NOT. RegridOp%IsCreated ) THEN
       CALL HCO_ERROR('Regrid operator not created', RC, THISLOC=LOC)
       RETURN
    ENDIF
    
    ! Create arrays from input data
    srcArray = ESMF_ArrayCreate(data=SrcData, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    dstArray = ESMF_ArrayCreate(data=DstData, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Create fields
    srcField = ESMF_FieldCreate( &
         grid=HEMCOGrid%Grid, &
         array=srcArray, &
         name="SrcField", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    dstField = ESMF_FieldCreate( &
         grid=HostGrid%Grid, &
         array=dstArray, &
         name="DstField", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Perform regridding
    CALL ESMF_FieldRegrid( &
         srcField=srcField, &
         dstField=dstField, &
         regridmethod=RegridOp%Operator, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Get regridded data
    CALL ESMF_ArrayGet(dstArray, data=DstData, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Cleanup
    CALL ESMF_FieldDestroy(srcField, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    CALL ESMF_FieldDestroy(dstField, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    CALL ESMF_ArrayDestroy(srcArray, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    CALL ESMF_ArrayDestroy(dstArray, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG('ESMF regridding performed successfully', &
                    LUN=HcoState%Config%hcoLogLUN)
    ENDIF
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
  END SUBROUTINE HCOI_ESMF_PerformRegrid
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_MapToHost
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_MapToHost provides a high-level
! interface for mapping HEMCO data directly to the host model's native grid.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_MapToHost( HcoState, HEMCOData, HostData, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    REAL(hp),           INTENT(IN   )  :: HEMCOData(:,:) ! Data on HEMCO grid
    REAL(hp),           INTENT(INOUT) :: HostData(:,:)  ! Data on host grid
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_MapToHost (hcoi_esmf_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if grids are set up
    IF ( .NOT. HEMCOGrid%IsInitialized .OR. .NOT. HostGrid%IsInitialized ) THEN
       CALL HCO_ERROR('Grids not set up', RC, THISLOC=LOC)
       RETURN
    ENDIF
    
    ! If grids are the same, just copy the data
    IF (HEMCOGrid%NX == SIZE(HEMCOData, 1) .AND. HEMCOGrid%NY == SIZE(HEMCOData, 2) .AND. &
        HostGrid%NX == SIZE(HostData, 1) .AND. HostGrid%NY == SIZE(HostData, 2) .AND. &
        HEMCOGrid%NX == HostGrid%NX .AND. HEMCOGrid%NY == HostGrid%NY) THEN
        
       ! Direct copy for same grid
       HostData(:, :) = HEMCOData(:, :)
       
       ! Verbose output
       IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
          CALL HCO_MSG('Direct copy performed (same grids)', &
                       LUN=HcoState%Config%hcoLogLUN)
       ENDIF
       
    ELSE
       ! Perform regridding for different grids
       CALL HCOI_ESMF_PerformRegrid(HcoState, HEMCOData, HostData, RC)
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR('Error performing regridding', RC, THISLOC=LOC)
          RETURN
       ENDIF
    
  END SUBROUTINE HCOI_ESMF_MapToHost
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Cleanup
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Simplified\_Cleanup cleans up the 
! simplified ESMF interface resources.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Cleanup( RC )
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
#if defined (ESMF_)
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Cleanup (hcoi_esmf_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
#if defined (ESMF_)
    ! Clean up regrid operator
    IF ( ASSOCIATED(RegridOp) ) THEN
       IF ( RegridOp%IsCreated ) THEN
          CALL ESMF_FieldRegridDestroy(RegridOp%Operator, rc=STATUS)
          IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
             RC = HCO_FAIL
             RETURN
          ENDIF
          RegridOp%IsCreated = .FALSE.
       ENDIF
       DEALLOCATE(RegridOp)
       RegridOp => NULL()
    ENDIF
    
    ! Clean up grids
    IF ( ASSOCIATED(HEMCOGrid) ) THEN
       IF ( HEMCOGrid%IsInitialized ) THEN
          CALL ESMF_GridDestroy(HEMCOGrid%Grid, rc=STATUS)
          IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
             RC = HCO_FAIL
             RETURN
          ENDIF
          HEMCOGrid%IsInitialized = .FALSE.
       ENDIF
       DEALLOCATE(HEMCOGrid)
       HEMCOGrid => NULL()
    ENDIF
    
    IF ( ASSOCIATED(HostGrid) ) THEN
       HostGrid%IsInitialized = .FALSE.
       DEALLOCATE(HostGrid)
       HostGrid => NULL()
    ENDIF
#else
    ! In non-ESMF builds, clean up pointers
    IF ( ASSOCIATED(RegridOp) ) THEN
       DEALLOCATE(RegridOp)
       RegridOp => NULL()
    ENDIF
    
    IF ( ASSOCIATED(HEMCOGrid) ) THEN
       DEALLOCATE(HEMCOGrid)
       HEMCOGrid => NULL()
    ENDIF
    
    IF ( ASSOCIATED(HostGrid) ) THEN
       DEALLOCATE(HostGrid)
       HostGrid => NULL()
    ENDIF
#endif
    
  END SUBROUTINE HCOI_ESMF_Cleanup
!EOC
END MODULE HCOI_ESMF_Mod