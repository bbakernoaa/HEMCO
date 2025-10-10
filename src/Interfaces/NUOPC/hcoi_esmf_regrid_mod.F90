!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoi_esmf_regrid_mod
!
! !DESCRIPTION: Module HCOI\_ESMF\_REGRID\_MOD provides simplified ESMF
! regridding functionality for HEMCO-NUOPC integration. This module focuses on
! the essential regridding operations without backward compatibility concerns,
! enabling direct operation on the host NUOPC model's native ESMF grid.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMF_Regrid_Mod
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
  ! Simplified ESMF regridding functions:
  PUBLIC :: HCOI_ESMF_Regrid_Init
  PUBLIC :: HCOI_ESMF_Regrid_Setup
  PUBLIC :: HCOI_ESMF_Regrid_Execute
  PUBLIC :: HCOI_ESMF_Regrid_Finalize
  PUBLIC :: HCOI_ESMF_Regrid_CreateOperator
  PUBLIC :: HCOI_ESMF_Regrid_ApplyOperator
  PUBLIC :: HCOI_ESMF_Regrid_StoreWeights
  PUBLIC :: HCOI_ESMF_Regrid_LoadWeights
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial simplified version for NUOPC ESMF regridding
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Simplified regridding state
  TYPE :: HCOI_RegridState
     LOGICAL                :: IsInitialized      ! Initialization flag
     LOGICAL                :: IsSetup            ! Setup flag
     LOGICAL                :: IsExecuting        ! Execution flag
     INTEGER                :: RegridMethod       ! Regridding method
     LOGICAL                :: UseWeightCaching   ! Whether to use weight caching
     LOGICAL                :: EnableOptimization ! Whether to enable optimization
     INTEGER                :: MaxCacheEntries    ! Maximum cache entries
     INTEGER                :: CurrentCacheEntries ! Current cache entries
  END TYPE HCOI_RegridState
  
  ! Simplified regridding operator
  TYPE :: HCOI_RegridOperator
     TYPE(ESMF_FieldRegrid) :: Operator          ! ESMF regridding operator
     LOGICAL                :: IsCreated         ! Whether operator is created
     CHARACTER(LEN=255)     :: SrcGridID         ! Source grid identifier
     CHARACTER(LEN=255)     :: DstGridID         ! Destination grid identifier
     INTEGER                :: LastAccessTime    ! Last access time for LRU
  END TYPE HCOI_RegridOperator
  
  ! Simplified weight cache entry
  TYPE :: HCOI_WeightCacheEntry
     CHARACTER(LEN=255)     :: SrcGridID         ! Source grid identifier
     CHARACTER(LEN=255)     :: DstGridID         ! Destination grid identifier
     TYPE(ESMF_FieldRegridWeight) :: Weights     ! Cached regridding weights
     LOGICAL                :: IsValid           ! Whether entry is valid
     INTEGER                :: AccessCount       ! Access count for LRU
  END TYPE HCOI_WeightCacheEntry
  
  ! Constants for regridding methods
  INTEGER, PARAMETER :: HCOI_REGRID_METHOD_BILINEAR = ESMF_REGRIDMETHOD_BILINEAR
  INTEGER, PARAMETER :: HCOI_REGRID_METHOD_CONSERVE = ESMF_REGRIDMETHOD_CONSERVE
  INTEGER, PARAMETER :: HCOI_REGRID_METHOD_NEAREST_STOD = ESMF_REGRIDMETHOD_NEAREST_STOD
  INTEGER, PARAMETER :: HCOI_REGRID_METHOD_NEAREST_DTOS = ESMF_REGRIDMETHOD_NEAREST_DTOS
  
  ! Constants for optimization modes
  INTEGER, PARAMETER :: HCOI_REGRID_OPT_NONE = 0      ! No optimization
  INTEGER, PARAMETER :: HCOI_REGRID_OPT_WEIGHTS = 1   ! Weight caching
  INTEGER, PARAMETER :: HCOI_REGRID_OPT_MEMORY = 2    ! Memory optimization
  INTEGER, PARAMETER :: HCOI_REGRID_OPT_FULL = 3      ! Full optimization
  
  ! Global regridding state instance
  TYPE(HCOI_RegridState), POINTER :: SimpleRegridStateInstance => NULL()
  
  ! Global regridding operator instance
  TYPE(HCOI_RegridOperator), POINTER :: SimpleRegridOperatorInstance => NULL()
  
  ! Global weight cache
  TYPE(HCOI_WeightCacheEntry), POINTER :: SimpleWeightCache(:) => NULL()
  INTEGER, PARAMETER :: MAX_WEIGHT_CACHE_SIZE = 50
  INTEGER :: CurrentWeightCacheSize = 0
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Regrid_Init
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Regrid\_Init initializes the
! simplified ESMF regridding system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Regrid_Init( HcoState, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Regrid_Init (hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Allocate regridding state instance if not already allocated
    IF ( .NOT. ASSOCIATED(SimpleRegridStateInstance) ) THEN
       ALLOCATE(SimpleRegridStateInstance)
    ENDIF
    
    ! Initialize regridding state
    SimpleRegridStateInstance%IsInitialized = .FALSE.
    SimpleRegridStateInstance%IsSetup = .FALSE.
    SimpleRegridStateInstance%IsExecuting = .FALSE.
    SimpleRegridStateInstance%RegridMethod = HCOI_REGRID_METHOD_BILINEAR
    SimpleRegridStateInstance%UseWeightCaching = .TRUE.
    SimpleRegridStateInstance%EnableOptimization = .TRUE.
    SimpleRegridStateInstance%MaxCacheEntries = 10
    SimpleRegridStateInstance%CurrentCacheEntries = 0
    
    ! Allocate regridding operator instance if not already allocated
    IF ( .NOT. ASSOCIATED(SimpleRegridOperatorInstance) ) THEN
       ALLOCATE(SimpleRegridOperatorInstance)
    ENDIF
    
    ! Initialize regridding operator
    SimpleRegridOperatorInstance%Operator = ESMF_FieldRegridEmptyCreate(rc=RC)
    IF (ESMF_LogFoundError(RC, __RC__, ESMF_CONTEXT, LOC)) RETURN
    
    SimpleRegridOperatorInstance%IsCreated = .FALSE.
    SimpleRegridOperatorInstance%SrcGridID = ''
    SimpleRegridOperatorInstance%DstGridID = ''
    SimpleRegridOperatorInstance%LastAccessTime = 0
    
    ! Allocate weight cache if not already allocated
    IF ( .NOT. ASSOCIATED(SimpleWeightCache) ) THEN
       ALLOCATE(SimpleWeightCache(MAX_WEIGHT_CACHE_SIZE))
    ENDIF
    
    ! Initialize weight cache
    CurrentWeightCacheSize = 0
    DO I = 1, MAX_WEIGHT_CACHE_SIZE
       SimpleWeightCache(I)%SrcGridID = ''
       SimpleWeightCache(I)%DstGridID = ''
       SimpleWeightCache(I)%Weights = ESMF_FieldRegridWeightEmptyCreate(rc=RC)
       IF (ESMF_LogFoundError(RC, __RC__, ESMF_CONTEXT, LOC)) RETURN
       SimpleWeightCache(I)%IsValid = .FALSE.
       SimpleWeightCache(I)%AccessCount = 0
    ENDDO
    
    ! Mark as initialized
    SimpleRegridStateInstance%IsInitialized = .TRUE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF regridding system initialized', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Regrid_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Regrid_Setup
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Regrid\_Setup configures the
! simplified ESMF regridding system with source and destination grid information.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Regrid_Setup( HcoState, SrcGrid, DstGrid, RegridMethod, &
                                          UseWeightCaching, EnableOptimization, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState         ! HEMCO state object
    TYPE(ESMF_Grid),    INTENT(IN   )  :: SrcGrid          ! Source ESMF grid
    TYPE(ESMF_Grid),    INTENT(IN   )  :: DstGrid          ! Destination ESMF grid
    INTEGER,            INTENT(IN   )  :: RegridMethod     ! Regridding method
    LOGICAL,            INTENT(IN   )  :: UseWeightCaching ! Whether to use weight caching
    LOGICAL,            INTENT(IN   )  :: EnableOptimization ! Whether to enable optimization
    INTEGER,            INTENT(INOUT) :: RC               ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Regrid_Setup (hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if regridding state is initialized
    IF ( .NOT. ASSOCIATED(SimpleRegridStateInstance) .OR. &
         .NOT. SimpleRegridStateInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Regridding state not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Store configuration
    SimpleRegridStateInstance%RegridMethod = RegridMethod
    SimpleRegridStateInstance%UseWeightCaching = UseWeightCaching
    SimpleRegridStateInstance%EnableOptimization = EnableOptimization
    
    ! Validate regridding method
    SELECT CASE (RegridMethod)
    CASE (HCOI_REGRID_METHOD_BILINEAR, &
          HCOI_REGRID_METHOD_CONSERVE, &
          HCOI_REGRID_METHOD_NEAREST_STOD, &
          HCOI_REGRID_METHOD_NEAREST_DTOS)
       ! Valid method, no action needed
    CASE DEFAULT
       CALL HCO_WARNING( 'Invalid regridding method, using bilinear', RC, THISLOC=LOC )
       SimpleRegridStateInstance%RegridMethod = HCOI_REGRID_METHOD_BILINEAR
       RC = HCO_SUCCESS  ! Continue anyway
    END SELECT
    
    ! Mark as setup
    SimpleRegridStateInstance%IsSetup = .TRUE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF regridding system configured', &
                    LUN=HcoState%Config%hcoLogLUN )
       WRITE(HcoState%Config%hcoLogLUN,*) '  Regridding method: ', RegridMethod
       WRITE(HcoState%Config%hcoLogLUN,*) '  Use weight caching: ', UseWeightCaching
       WRITE(HcoState%Config%hcoLogLUN,*) '  Enable optimization: ', EnableOptimization
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Regrid_Setup
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Regrid_Execute
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Regrid\_Execute performs the
! simplified ESMF regridding operation.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Regrid_Execute( HcoState, SrcGrid, DstGrid, &
                                            SrcData, DstData, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    TYPE(ESMF_Grid),    INTENT(IN   )  :: SrcGrid      ! Source ESMF grid
    TYPE(ESMF_Grid),    INTENT(IN   )  :: DstGrid      ! Destination ESMF grid
    REAL(hp),           INTENT(IN   )  :: SrcData(:,:) ! Input data on source grid
    REAL(hp),           INTENT(INOUT) :: DstData(:,:)  ! Output data on destination grid
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Regrid_Execute (hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if regridding state is initialized and setup
    IF ( .NOT. ASSOCIATED(SimpleRegridStateInstance) .OR. &
         .NOT. SimpleRegridStateInstance%IsInitialized .OR. &
         .NOT. SimpleRegridStateInstance%IsSetup ) THEN
       CALL HCO_ERROR( 'Regridding state not initialized or setup', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Mark as executing
    SimpleRegridStateInstance%IsExecuting = .TRUE.
    
#if defined (ESMF_)
    ! Create ESMF arrays from the input data
    srcArray = ESMF_ArrayCreate(data=SrcData, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       SimpleRegridStateInstance%IsExecuting = .FALSE.
       RETURN
    ENDIF
    
    dstArray = ESMF_ArrayCreate(data=DstData, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_ArrayDestroy(srcArray, rc=STATUS)
       SimpleRegridStateInstance%IsExecuting = .FALSE.
       RETURN
    ENDIF
    
    ! Create ESMF fields
    srcField = ESMF_FieldCreate(grid=SrcGrid, &
                               array=srcArray, &
                               name="SrcField", &
                               rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_ArrayDestroy(srcArray, rc=STATUS)
       CALL ESMF_ArrayDestroy(dstArray, rc=STATUS)
       SimpleRegridStateInstance%IsExecuting = .FALSE.
       RETURN
    ENDIF
    
    dstField = ESMF_FieldCreate(grid=DstGrid, &
                               array=dstArray, &
                               name="DstField", &
                               rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_ArrayDestroy(srcArray, rc=STATUS)
       CALL ESMF_ArrayDestroy(dstArray, rc=STATUS)
       SimpleRegridStateInstance%IsExecuting = .FALSE.
       RETURN
    ENDIF
    
    ! Create or retrieve regridding operator
    CALL HCOI_ESMF_Regrid_CreateOperator( HcoState, SrcGrid, DstGrid, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error creating regridding operator', RC, THISLOC=LOC )
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       CALL ESMF_ArrayDestroy(srcArray, rc=STATUS)
       CALL ESMF_ArrayDestroy(dstArray, rc=STATUS)
       SimpleRegridStateInstance%IsExecuting = .FALSE.
       RETURN
    ENDIF
    
    ! Apply regridding operator
    CALL HCOI_ESMF_Regrid_ApplyOperator( HcoState, srcField, dstField, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error applying regridding operator', RC, THISLOC=LOC )
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       CALL ESMF_ArrayDestroy(srcArray, rc=STATUS)
       CALL ESMF_ArrayDestroy(dstArray, rc=STATUS)
       SimpleRegridStateInstance%IsExecuting = .FALSE.
       RETURN
    ENDIF
    
    ! Get the regridded data back to the output array
    CALL ESMF_ArrayGet(dstArray, data=DstData, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       CALL ESMF_ArrayDestroy(srcArray, rc=STATUS)
       CALL ESMF_ArrayDestroy(dstArray, rc=STATUS)
       SimpleRegridStateInstance%IsExecuting = .FALSE.
       RETURN
    ENDIF
    
    ! Cleanup
    CALL ESMF_FieldDestroy(srcField, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       CALL ESMF_ArrayDestroy(srcArray, rc=STATUS)
       CALL ESMF_ArrayDestroy(dstArray, rc=STATUS)
       SimpleRegridStateInstance%IsExecuting = .FALSE.
       RETURN
    ENDIF
    
    CALL ESMF_FieldDestroy(dstField, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_ArrayDestroy(srcArray, rc=STATUS)
       CALL ESMF_ArrayDestroy(dstArray, rc=STATUS)
       SimpleRegridStateInstance%IsExecuting = .FALSE.
       RETURN
    ENDIF
    
    CALL ESMF_ArrayDestroy(srcArray, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_ArrayDestroy(dstArray, rc=STATUS)
       SimpleRegridStateInstance%IsExecuting = .FALSE.
       RETURN
    ENDIF
    
    CALL ESMF_ArrayDestroy(dstArray, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       SimpleRegridStateInstance%IsExecuting = .FALSE.
       RETURN
    ENDIF
#else
    ! In non-ESMF builds, just copy data
    DstData(:, :) = SrcData(:, :)
#endif
    
    ! Mark as not executing
    SimpleRegridStateInstance%IsExecuting = .FALSE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF regridding executed', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Regrid_Execute
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Regrid_Finalize
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Regrid\_Finalize finalizes the
! simplified ESMF regridding system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Regrid_Finalize( HcoState, RC )
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
#if defined (ESMF_)
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Regrid_Finalize (hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if regridding state is initialized
    IF ( .NOT. ASSOCIATED(SimpleRegridStateInstance) .OR. &
         .NOT. SimpleRegridStateInstance%IsInitialized ) THEN
       CALL HCO_WARNING( 'Regridding state not initialized', RC, THISLOC=LOC )
       RC = HCO_SUCCESS
       RETURN
    ENDIF
    
#if defined (ESMF_)
    ! Clean up regridding operator if created
    IF ( ASSOCIATED(SimpleRegridOperatorInstance) ) THEN
       IF ( SimpleRegridOperatorInstance%IsCreated ) THEN
          CALL ESMF_FieldRegridDestroy(SimpleRegridOperatorInstance%Operator, rc=STATUS)
          IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
             RC = HCO_FAIL
             RETURN
          ENDIF
          SimpleRegridOperatorInstance%IsCreated = .FALSE.
       ENDIF
    ENDIF
    
    ! Clean up weight cache
    IF ( ASSOCIATED(SimpleWeightCache) ) THEN
       DO I = 1, MAX_WEIGHT_CACHE_SIZE
          IF ( SimpleWeightCache(I)%IsValid ) THEN
             CALL ESMF_FieldRegridWeightDestroy(SimpleWeightCache(I)%Weights, rc=STATUS)
             IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
                RC = HCO_FAIL
                RETURN
             ENDIF
             SimpleWeightCache(I)%IsValid = .FALSE.
          ENDIF
       ENDDO
    ENDIF
#endif
    
    ! Clean up instances
    IF ( ASSOCIATED(SimpleRegridOperatorInstance) ) THEN
       DEALLOCATE(SimpleRegridOperatorInstance)
       SimpleRegridOperatorInstance => NULL()
    ENDIF
    
    IF ( ASSOCIATED(SimpleWeightCache) ) THEN
       DEALLOCATE(SimpleWeightCache)
       SimpleWeightCache => NULL()
    ENDIF
    
    IF ( ASSOCIATED(SimpleRegridStateInstance) ) THEN
       DEALLOCATE(SimpleRegridStateInstance)
       SimpleRegridStateInstance => NULL()
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF regridding system finalized', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Regrid_Finalize
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Regrid_CreateOperator
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Regrid\_CreateOperator creates an
! ESMF regridding operator for the simplified system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Regrid_CreateOperator( HcoState, SrcGrid, DstGrid, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    TYPE(ESMF_Grid),    INTENT(IN   )  :: SrcGrid      ! Source ESMF grid
    TYPE(ESMF_Grid),    INTENT(IN   )  :: DstGrid      ! Destination ESMF grid
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Regrid_CreateOperator (hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if regridding state is initialized and setup
    IF ( .NOT. ASSOCIATED(SimpleRegridStateInstance) .OR. &
         .NOT. SimpleRegridStateInstance%IsInitialized .OR. &
         .NOT. SimpleRegridStateInstance%IsSetup ) THEN
       CALL HCO_ERROR( 'Regridding state not initialized or setup', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
#if defined (ESMF_)
    ! Check if operator is already created
    IF ( ASSOCIATED(SimpleRegridOperatorInstance) .AND. &
         SimpleRegridOperatorInstance%IsCreated ) THEN
       ! Operator already exists, no need to recreate
       RC = HCO_SUCCESS
       RETURN
    ENDIF
    
    ! Create source and destination fields for operator creation
    srcField = ESMF_FieldCreate( &
         grid=SrcGrid, &
         name="TempSrcField", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    dstField = ESMF_FieldCreate( &
         grid=DstGrid, &
         name="TempDstField", &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       RETURN
    ENDIF
    
    ! Create regridding operator
    SimpleRegridOperatorInstance%Operator = ESMF_FieldRegridStore( &
         srcField=srcField, &
         dstField=dstField, &
         regridmethod=SimpleRegridStateInstance%RegridMethod, &
         unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_FieldDestroy(srcField, rc=STATUS)
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       RETURN
    ENDIF
    
    ! Mark operator as created
    SimpleRegridOperatorInstance%IsCreated = .TRUE.
    SimpleRegridOperatorInstance%SrcGridID = 'SRC_GRID'
    SimpleRegridOperatorInstance%DstGridID = 'DST_GRID'
    SimpleRegridOperatorInstance%LastAccessTime = 0
    
    ! Store weights in cache if enabled
    IF ( SimpleRegridStateInstance%UseWeightCaching ) THEN
       CALL HCOI_ESMF_Regrid_StoreWeights( HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_WARNING( 'Warning storing regridding weights in cache', RC, THISLOC=LOC )
          RC = HCO_SUCCESS  ! Continue anyway
       ENDIF
    ENDIF
    
    ! Cleanup temporary fields
    CALL ESMF_FieldDestroy(srcField, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       CALL ESMF_FieldDestroy(dstField, rc=STATUS)
       RETURN
    ENDIF
    
    CALL ESMF_FieldDestroy(dstField, rc=STATUS)
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF regridding operator created', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Regrid_CreateOperator
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Regrid_ApplyOperator
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Regrid\_ApplyOperator applies an
! ESMF regridding operator to perform the actual regridding operation.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Regrid_ApplyOperator( HcoState, SrcField, DstField, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    TYPE(ESMF_Field),   INTENT(IN   )  :: SrcField     ! Source ESMF field
    TYPE(ESMF_Field),   INTENT(INOUT) :: DstField     ! Destination ESMF field
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Regrid_ApplyOperator (hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if regridding state is initialized and setup
    IF ( .NOT. ASSOCIATED(SimpleRegridStateInstance) .OR. &
         .NOT. SimpleRegridStateInstance%IsInitialized .OR. &
         .NOT. SimpleRegridStateInstance%IsSetup ) THEN
       CALL HCO_ERROR( 'Regridding state not initialized or setup', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
#if defined (ESMF_)
    ! Check if operator is created
    IF ( .NOT. ASSOCIATED(SimpleRegridOperatorInstance) .OR. &
         .NOT. SimpleRegridOperatorInstance%IsCreated ) THEN
       CALL HCO_ERROR( 'Regridding operator not created', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Apply regridding operator
    CALL ESMF_FieldRegrid( &
         srcField=SrcField, &
         dstField=DstField, &
         regridmethod=SimpleRegridOperatorInstance%Operator, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Update access time
    SimpleRegridOperatorInstance%LastAccessTime = SimpleRegridOperatorInstance%LastAccessTime + 1
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF regridding operator applied', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Regrid_ApplyOperator
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Regrid_StoreWeights
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Regrid\_StoreWeights stores
! regridding weights in the cache for future use.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Regrid_StoreWeights( HcoState, RC )
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
    TYPE(ESMF_FieldRegridWeight) :: weights
    INTEGER             :: cacheIndex, minAccessCount
    INTEGER             :: i, minIndex
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Regrid_StoreWeights (hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if regridding state is initialized and setup
    IF ( .NOT. ASSOCIATED(SimpleRegridStateInstance) .OR. &
         .NOT. SimpleRegridStateInstance%IsInitialized .OR. &
         .NOT. SimpleRegridStateInstance%IsSetup ) THEN
       CALL HCO_ERROR( 'Regridding state not initialized or setup', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Check if weight caching is enabled
    IF ( .NOT. SimpleRegridStateInstance%UseWeightCaching ) THEN
       RC = HCO_SUCCESS
       RETURN
    ENDIF
    
#if defined (ESMF_)
    ! Check if cache is full
    IF ( CurrentWeightCacheSize >= MAX_WEIGHT_CACHE_SIZE ) THEN
       ! Find least recently used entry to replace
       minAccessCount = HUGE(1)
       minIndex = 1
       DO i = 1, MAX_WEIGHT_CACHE_SIZE
          IF ( SimpleWeightCache(i)%AccessCount < minAccessCount ) THEN
             minAccessCount = SimpleWeightCache(i)%AccessCount
             minIndex = i
          ENDIF
       ENDDO
       
       ! Remove the least recently used entry
       cacheIndex = minIndex
       IF ( SimpleWeightCache(cacheIndex)%IsValid ) THEN
          CALL ESMF_FieldRegridWeightDestroy(SimpleWeightCache(cacheIndex)%Weights, rc=STATUS)
          IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
             RC = HCO_FAIL
             RETURN
          ENDIF
          SimpleWeightCache(cacheIndex)%IsValid = .FALSE.
          CurrentWeightCacheSize = CurrentWeightCacheSize - 1
       ENDIF
    ELSE
       ! Find an empty slot
       cacheIndex = -1
       DO i = 1, MAX_WEIGHT_CACHE_SIZE
          IF ( .NOT. SimpleWeightCache(i)%IsValid ) THEN
             cacheIndex = i
             EXIT
          ENDIF
       ENDDO
       
       ! If no empty slot found, use next available slot
       IF ( cacheIndex == -1 ) THEN
          cacheIndex = MOD(CurrentWeightCacheSize, MAX_WEIGHT_CACHE_SIZE) + 1
       ENDIF
    ENDIF
    
    ! Get weights from regridding operator
    weights = ESMF_FieldRegridGetWeight( &
         regridmethod=SimpleRegridOperatorInstance%Operator, &
         RC=STATUS )
    IF (ESMF_LogFoundError(STATUS, __RC__, ESMF_CONTEXT, LOC)) THEN
       RC = HCO_FAIL
       RETURN
    ENDIF
    
    ! Store weights in cache
    SimpleWeightCache(cacheIndex)%SrcGridID = TRIM(SimpleRegridOperatorInstance%SrcGridID)
    SimpleWeightCache(cacheIndex)%DstGridID = TRIM(SimpleRegridOperatorInstance%DstGridID)
    SimpleWeightCache(cacheIndex)%Weights = weights
    SimpleWeightCache(cacheIndex)%IsValid = .TRUE.
    SimpleWeightCache(cacheIndex)%AccessCount = 1
    CurrentWeightCacheSize = CurrentWeightCacheSize + 1
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Regridding weights stored in cache', &
                    LUN=HcoState%Config%hcoLogLUN )
       WRITE(HcoState%Config%hcoLogLUN,*) '  Cache entries: ', CurrentWeightCacheSize
    ENDIF
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
  END SUBROUTINE HCOI_ESMF_Regrid_StoreWeights
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Regrid_LoadWeights
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Regrid\_LoadWeights loads
! regridding weights from the cache if available.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Regrid_LoadWeights( HcoState, SrcGridID, DstGridID, &
                                                Weights, Found, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    CHARACTER(LEN=*),   INTENT(IN   )  :: SrcGridID    ! Source grid identifier
    CHARACTER(LEN=*),   INTENT(IN   )  :: DstGridID    ! Destination grid identifier
    TYPE(ESMF_FieldRegridWeight), INTENT(  OUT)  :: Weights    ! Output: cached weights
    LOGICAL,            INTENT(  OUT) :: Found       ! Whether weights were found
    INTEGER,            INTENT(INOUT) :: RC          ! Return code
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
    INTEGER             :: i
    INTEGER             :: STATUS
#endif
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Regrid_LoadWeights (hcoi_esmf_regrid_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    Found = .FALSE.
    Weights = ESMF_FieldRegridWeightEmptyCreate(rc=RC)
    IF (ESMF_LogFoundError(RC, __RC__, ESMF_CONTEXT, LOC)) RETURN
    
    ! Check if regridding state is initialized and setup
    IF ( .NOT. ASSOCIATED(SimpleRegridStateInstance) .OR. &
         .NOT. SimpleRegridStateInstance%IsInitialized .OR. &
         .NOT. SimpleRegridStateInstance%IsSetup ) THEN
       CALL HCO_ERROR( 'Regridding state not initialized or setup', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Check if weight caching is enabled
    IF ( .NOT. SimpleRegridStateInstance%UseWeightCaching ) THEN
       RC = HCO_SUCCESS
       RETURN
    ENDIF
    
#if defined (ESMF_)
    ! Search for matching weights in cache
    DO i = 1, MAX_WEIGHT_CACHE_SIZE
       IF ( SimpleWeightCache(i)%IsValid .AND. &
            TRIM(SimpleWeightCache(i)%SrcGridID) == TRIM(SrcGridID) .AND. &
            TRIM(SimpleWeightCache(i)%DstGridID) == TRIM(DstGridID) ) THEN
          
          ! Found matching weights
          Weights = SimpleWeightCache(i)%Weights
          Found = .TRUE.
          SimpleWeightCache(i)%AccessCount = SimpleWeightCache(i)%AccessCount + 1
          
          ! Verbose output
          IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
             CALL HCO_MSG( 'Regridding weights loaded from cache', &
                          LUN=HcoState%Config%hcoLogLUN )
          ENDIF
          
          EXIT
       ENDIF
    ENDDO
#else
    ! In non-ESMF builds, return error
    RC = HCO_FAIL
#endif
    
  END SUBROUTINE HCOI_ESMF_Regrid_LoadWeights
!EOC
END MODULE HCOI_ESMF_Regrid_Mod