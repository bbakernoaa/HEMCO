!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoi_esmf_io_mod
!
! !DESCRIPTION: Module HCOI\_ESMF\_IO\_MOD provides simplified ESMF-based
! I/O operations for HEMCO-NUOPC integration. This module eliminates backward
! compatibility concerns and focuses on essential I/O functionality for direct
! operation on the host NUOPC model's native ESMF grid.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMF_IO_Mod
!
! !USES:
!
  USE HCO_ERROR_MOD
  USE HCO_TYPES_MOD
  USE HCO_STATE_MOD, ONLY : HCO_State
  USE HCOX_STATE_MOD, ONLY : Ext_State
  
#if defined (ESMF_)
  USE ESMF
  USE HCO_m_netcdf_io_read
  USE HCO_m_netcdf_io_write
#endif

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  ! Simplified ESMF I/O functions:
  PUBLIC :: HCOI_ESMF_IO_Init
  PUBLIC :: HCOI_ESMF_IO_Final
  PUBLIC :: HCOI_ESMF_IO_ReadData
  PUBLIC :: HCOI_ESMF_IO_WriteData
  PUBLIC :: HCOI_ESMF_IO_CacheData
  PUBLIC :: HCOI_ESMF_IO_ReadFromCache
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial simplified version for NUOPC ESMF I/O
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Simplified I/O configuration parameters
  INTEGER, PARAMETER ::  MAX_CACHE_ENTRIES = 50
  
  ! Simplified cache structure for frequently accessed data
  TYPE :: HCOI_CacheEntry
     CHARACTER(LEN=255)           :: FileName
     CHARACTER(LEN=255)           :: VarName
     INTEGER                      :: NX, NY, NZ
     REAL(hp), POINTER            :: DataPtr(:,:,:) => NULL()
     LOGICAL                      :: IsValid
     INTEGER                      :: AccessCount
  END TYPE HCOI_CacheEntry
  
  ! Simplified cache array
  TYPE(HCOI_CacheEntry) :: CacheEntries( MAX_CACHE_ENTRIES)
  INTEGER                   :: NumCacheEntries = 0
  
  ! Simplified I/O parameters
  LOGICAL :: UseCollectiveIO = .TRUE.
  LOGICAL :: EnableCaching = .TRUE.
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_IO_Init
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_IO\_Init initializes the simplified
! ESMF I/O system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_IO_Init( HcoState, RC )
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
    INTEGER :: I
    CHARACTER(LEN=255) :: LOC = 'HCOI_ESMF_IO_Init (hcoi_esmf__io_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Initialize cache
    DO I = 1,  MAX_CACHE_ENTRIES
       CacheEntries(I)%FileName = ''
       CacheEntries(I)%VarName = ''
       CacheEntries(I)%NX = 0
       CacheEntries(I)%NY = 0
       CacheEntries(I)%NZ = 0
       CacheEntries(I)%DataPtr => NULL()
       CacheEntries(I)%IsValid = .FALSE.
       CacheEntries(I)%AccessCount = 0
    ENDDO
    NumCacheEntries = 0
    
    ! Set default I/O parameters
    UseCollectiveIO = .TRUE.
    EnableCaching = .TRUE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF I/O system initialized', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_IO_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_IO_Final
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_IO\_Final finalizes the simplified
! ESMF I/O system, cleaning up cache resources.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_IO_Final( RC )
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
    INTEGER :: I
    CHARACTER(LEN=255) :: LOC = 'HCOI_ESMF_IO_Final (hcoi_esmf_io_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Clean up cache
    DO I = 1,  MAX_CACHE_ENTRIES
       IF ( ASSOCIATED(CacheEntries(I)%DataPtr) ) THEN
          DEALLOCATE( CacheEntries(I)%DataPtr )
          CacheEntries(I)%DataPtr => NULL()
       ENDIF
       CacheEntries(I)%IsValid = .FALSE.
    ENDDO
    NumCacheEntries = 0
    
    ! Verbose output
    ! Note: We can't use HcoState here since it's not passed to this routine
    ! In a real implementation, we might pass HcoState or use a global logger
    
  END SUBROUTINE HCOI_ESMF_IO_Final
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_IO_ReadData
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_IO\_ReadData reads data from file
! using the simplified ESMF I/O system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_IO_ReadData( HcoState, FileName, VarName, Data, RC, &
                                         StartIdx, CountIdx )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER         :: HcoState     ! HEMCO state object
    CHARACTER(LEN=*),   INTENT(IN)      :: FileName     ! File name
    CHARACTER(LEN=*),   INTENT(IN)      :: VarName     ! Variable name
    REAL(hp),           POINTER         :: Data(:,:,:)  ! Output: data array
    INTEGER,            INTENT(INOUT)   :: RC           ! Return code
    INTEGER, OPTIONAL,  INTENT(IN)      :: StartIdx(3)  ! Start indices
    INTEGER, OPTIONAL,  INTENT(IN)      :: CountIdx(3)  ! Count indices
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                        :: STAT
    INTEGER                        :: StartIdxLocal(3), CountIdxLocal(3)
    INTEGER                        :: Ncid, VarId
    LOGICAL                        :: FoundInCache
    CHARACTER(LEN=255)             :: LOC = 'HCOI_ESMF_IO_ReadData (hcoi_esmf_io_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if data is in cache
    FoundInCache = .FALSE.
    IF ( EnableCaching ) THEN
       CALL HCOI_ESMF_IO_ReadFromCache( FileName, VarName, Data, FoundInCache, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error reading from cache', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
       IF ( FoundInCache .AND. RC == HCO_SUCCESS ) THEN
          RETURN
       ENDIF
    ENDIF
    
    ! Set default indices if not provided
    IF ( PRESENT(StartIdx) ) THEN
       StartIdxLocal = StartIdx
    ELSE
       StartIdxLocal = (/ 1, 1, 1 /)
    ENDIF
    
    IF ( PRESENT(CountIdx) ) THEN
       CountIdxLocal = CountIdx
    ELSE
       ! Use HcoState dimensions if available
       IF ( ASSOCIATED(HcoState) ) THEN
          CountIdxLocal = (/ HcoState%NX, HcoState%NY, HcoState%NZ /)
       ELSE
          CountIdxLocal = (/ 1, 1, 1 /)
       ENDIF
    ENDIF
    
    ! Use standard NetCDF read
    CALL HCO_NcFile_Open( TRIM(FileName), Ncid, RC, .TRUE. )
    IF ( RC /= HCO_SUCCESS ) RETURN
    
    CALL HCO_NcInq_VarID( Ncid, TRIM(VarName), VarId, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_NcFile_Close( Ncid, RC )
       RETURN
    ENDIF
    
    ! Ensure data array is properly allocated
    IF ( .NOT. ASSOCIATED(Data) ) THEN
       ALLOCATE( Data(CountIdxLocal(1), CountIdxLocal(2), CountIdxLocal(3)) )
    ELSE
       ! Check if existing array has correct dimensions
       IF ( SIZE(Data,1) /= CountIdxLocal(1) .OR. SIZE(Data,2) /= CountIdxLocal(2) .OR. &
            SIZE(Data,3) /= CountIdxLocal(3) ) THEN
          DEALLOCATE(Data)
          ALLOCATE( Data(CountIdxLocal(1), CountIdxLocal(2), CountIdxLocal(3)) )
       ENDIF
    END IF
    
    CALL NcRd_3d_R4( Data, Ncid, TRIM(VarName), StartIdxLocal, CountIdxLocal )
    
    CALL HCO_NcFile_Close( Ncid, RC )
    
    ! Cache the data if successful and caching enabled
    IF ( RC == HCO_SUCCESS .AND. EnableCaching .AND. .NOT. FoundInCache ) THEN
       CALL HCOI_ESMF_IO_CacheData( FileName, VarName, Data, RC, &
                                        CountIdxLocal(1), CountIdxLocal(2), CountIdxLocal(3) )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_IO_ReadData
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_IO_WriteData
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_IO\_WriteData writes data to file
! using the simplified ESMF I/O system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_IO_WriteData( HcoState, FileName, VarName, Data, RC, &
                                          StartIdx, CountIdx )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER         :: HcoState     ! HEMCO state object
    CHARACTER(LEN=*),   INTENT(IN)      :: FileName     ! File name
    CHARACTER(LEN=*),   INTENT(IN)      :: VarName     ! Variable name
    REAL(hp),           INTENT(IN)      :: Data(:,:,:)  ! Input: data array
    INTEGER,            INTENT(INOUT)   :: RC           ! Return code
    INTEGER, OPTIONAL,  INTENT(IN)      :: StartIdx(3)  ! Start indices
    INTEGER, OPTIONAL,  INTENT(IN)      :: CountIdx(3)  ! Count indices
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                        :: STAT
    INTEGER                        :: StartIdxLocal(3), CountIdxLocal(3)
    INTEGER                        :: Ncid, VarId
    CHARACTER(LEN=255)             :: LOC = 'HCOI_ESMF_IO_WriteData (hcoi_esmf__io_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Set default indices if not provided
    IF ( PRESENT(StartIdx) ) THEN
       StartIdxLocal = StartIdx
    ELSE
       StartIdxLocal = (/ 1, 1, 1 /)
    ENDIF
    
    IF ( PRESENT(CountIdx) ) THEN
       CountIdxLocal = CountIdx
    ELSE
       ! Use size of data array if available
       CountIdxLocal = (/ SIZE(Data,1), SIZE(Data,2), SIZE(Data,3) /)
    ENDIF
    
    ! Use standard NetCDF write
    CALL HCO_NcFile_Open( TRIM(FileName), Ncid, RC, .FALSE. )
    IF ( RC /= HCO_SUCCESS ) RETURN
    
    CALL HCO_NcInq_VarID( Ncid, TRIM(VarName), VarId, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_NcFile_Close( Ncid, RC )
       RETURN
    ENDIF
    
    CALL NcWr_3d_R4( Data, Ncid, TRIM(VarName), StartIdxLocal, CountIdxLocal )
    
    CALL HCO_NcFile_Close( Ncid, RC )
    
  END SUBROUTINE HCOI_ESMF_IO_WriteData
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_IO_CacheData
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_IO\_CacheData caches data in memory
! for faster access.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_IO_CacheData( FileName, VarName, Data, RC, NX, NY, NZ )
!
! !ARGUMENTS:
!
    CHARACTER(LEN=*),   INTENT(IN)      :: FileName     ! File name
    CHARACTER(LEN=*),   INTENT(IN)      :: VarName     ! Variable name
    REAL(hp),           INTENT(IN)      :: Data(:,:,:)  ! Data to cache
    INTEGER,            INTENT(INOUT)   :: RC           ! Return code
    INTEGER,            INTENT(IN)      :: NX, NY, NZ  ! Data dimensions
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                        :: I, CacheIdx
    LOGICAL                        :: FoundEmptySlot
    CHARACTER(LEN=255)             :: LOC = 'HCOI_ESMF_IO_CacheData (hcoi_esmf__io_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if caching is enabled
    IF ( .NOT. EnableCaching ) THEN
       RC = HCO_SUCCESS
       RETURN
    ENDIF
    
    ! Check if already cached
    DO I = 1, NumCacheEntries
       IF ( TRIM(CacheEntries(I)%FileName) == TRIM(FileName) .AND. &
            TRIM(CacheEntries(I)%VarName) == TRIM(VarName) ) THEN
          ! Update access count
          CacheEntries(I)%AccessCount = CacheEntries(I)%AccessCount + 1
          RC = HCO_SUCCESS
          RETURN
       ENDIF
    ENDDO
    
    ! Find an empty slot or replace least used
    FoundEmptySlot = .FALSE.
    CacheIdx = -1
    
    ! Look for empty slot
    DO I = 1,  MAX_CACHE_ENTRIES
       IF ( .NOT. CacheEntries(I)%IsValid ) THEN
          CacheIdx = I
          FoundEmptySlot = .TRUE.
          EXIT
       ENDIF
    ENDDO
    
    ! If no empty slot, find least accessed entry to replace
    IF ( .NOT. FoundEmptySlot ) THEN
       CacheIdx = 1
       DO I = 2,  MAX_CACHE_ENTRIES
          IF ( CacheEntries(I)%AccessCount < CacheEntries(CacheIdx)%AccessCount ) THEN
             CacheIdx = I
          ENDIF
       ENDDO
    ENDIF
    
    ! Clean up old data if needed
    IF ( ASSOCIATED(CacheEntries(CacheIdx)%DataPtr) ) THEN
       DEALLOCATE( CacheEntries(CacheIdx)%DataPtr )
    ENDIF
    
    ! Allocate and copy data
    ALLOCATE( CacheEntries(CacheIdx)%DataPtr(NX, NY, NZ) )
    CacheEntries(CacheIdx)%DataPtr = Data(1:NX, 1:NY, 1:NZ)
    
    ! Set cache entry properties
    CacheEntries(CacheIdx)%FileName = TRIM(FileName)
    CacheEntries(CacheIdx)%VarName = TRIM(VarName)
    CacheEntries(CacheIdx)%NX = NX
    CacheEntries(CacheIdx)%NY = NY
    CacheEntries(CacheIdx)%NZ = NZ
    CacheEntries(CacheIdx)%IsValid = .TRUE.
    CacheEntries(CacheIdx)%AccessCount = 1
    
    ! Update cache entry count
    IF ( .NOT. FoundEmptySlot .AND. NumCacheEntries <  MAX_CACHE_ENTRIES ) THEN
        NumCacheEntries =  NumCacheEntries + 1
    ELSEIF ( FoundEmptySlot .AND. CacheIdx >  NumCacheEntries ) THEN
        NumCacheEntries = CacheIdx
    ENDIF
    
    RC = HCO_SUCCESS
    
  END SUBROUTINE HCOI_ESMF_IO_CacheData
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_IO_ReadFromCache
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_IO\_ReadFromCache reads data from
! cache if available.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_IO_ReadFromCache( FileName, VarName, Data, Found, RC )
!
! !ARGUMENTS:
!
    CHARACTER(LEN=*),   INTENT(IN)      :: FileName     ! File name
    CHARACTER(LEN=*),   INTENT(IN)      :: VarName     ! Variable name
    REAL(hp),           POINTER         :: Data(:,:,:)  ! Output: cached data (if found)
    LOGICAL,            INTENT(  OUT)  :: Found        ! Whether data was found
    INTEGER,            INTENT(INOUT)   :: RC           ! Return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                        :: I
    INTEGER                        :: NX, NY, NZ
    CHARACTER(LEN=255)             :: LOC = 'HCOI_ESMF_IO_ReadFromCache (hcoi_esmf__io_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    Found = .FALSE.
    
    ! Check if caching is enabled
    IF ( .NOT.  EnableCaching ) THEN
       RC = HCO_SUCCESS
       RETURN
    ENDIF
    
    ! Look for cached data
    DO I = 1,  NumCacheEntries
       IF (  CacheEntries(I)%IsValid .AND. &
            TRIM( CacheEntries(I)%FileName) == TRIM(FileName) .AND. &
            TRIM( CacheEntries(I)%VarName) == TRIM(VarName) ) THEN
           
          ! Get dimensions
          NX =  CacheEntries(I)%NX
          NY =  CacheEntries(I)%NY
          NZ =  CacheEntries(I)%NZ
          
          ! Ensure data array is properly allocated
          IF ( .NOT. ASSOCIATED(Data) ) THEN
             ALLOCATE( Data(NX, NY, NZ) )
          ELSE
             ! Check if existing array has correct dimensions
             IF ( SIZE(Data,1) /= NX .OR. SIZE(Data,2) /= NY .OR. SIZE(Data,3) /= NZ ) THEN
                DEALLOCATE(Data)
                ALLOCATE( Data(NX, NY, NZ) )
             ENDIF
          END IF
          
          ! Copy cached data
          Data(1:NX, 1:NY, 1:NZ) =  CacheEntries(I)%DataPtr(1:NX, 1:NY, 1:NZ)
          
          ! Update access count
           CacheEntries(I)%AccessCount =  CacheEntries(I)%AccessCount + 1
          
          Found = .TRUE.
          EXIT
       ENDIF
    ENDDO
    
  END SUBROUTINE HCOI_ESMF_IO_ReadFromCache
!EOC
END MODULE HCOI_ESMF__IO_Mod