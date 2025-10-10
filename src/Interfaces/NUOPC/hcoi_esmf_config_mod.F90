!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoi_esmf_config_mod
!
! !DESCRIPTION: Module HCOI\_ESMF\_\_CONFIG\_MOD provides simplified configuration
! management for the ESMF-based regridding system in HEMCO-NUOPC. This module
! eliminates backward compatibility concerns and focuses on essential configuration
! parameters for direct operation on the host NUOPC model's native ESMF grid.
!\\
!\\
! !INTERFACE:
!
MODULE HCOI_ESMF_Config_Mod
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
  ! Simplified configuration functions:
  PUBLIC :: HCOI_ESMF_Config_Init
  PUBLIC :: HCOI_ESMF_Config_Read
  PUBLIC :: HCOI_ESMF_Config_GetOpt
  PUBLIC :: HCOI_ESMF_Config_SetOpt
  PUBLIC :: HCOI_ESMF_Config_Validate
  PUBLIC :: HCOI_ESMF_Config_Apply
  PUBLIC :: HCOI_ESMF_Config_Cleanup
!
! !REVISION HISTORY:
!  09 Oct 2025 - N. Kumar   - Initial simplified version for NUOPC ESMF configuration
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !MODULE VARIABLES:
!
  ! Simplified configuration structure
  TYPE :: HCOI_Config
     LOGICAL                :: IsInitialized      ! Initialization flag
     LOGICAL                :: UseNativeGrid      ! Whether to use native grid
     LOGICAL                :: EnableCaching      ! Whether to enable caching
     LOGICAL                :: EnableOptimization ! Whether to enable optimization
     INTEGER                :: IntegrationMode    ! Integration mode
     INTEGER                :: RegridMethod       ! Regridding method
     INTEGER                :: MaxCacheEntries    ! Maximum cache entries
     REAL(hp)               :: CacheThreshold     ! Cache threshold
     LOGICAL                :: VerboseOutput      ! Verbose output flag
     LOGICAL                :: DebugMode          ! Debug mode flag
  END TYPE HCOI_Config
  
  ! Constants for configuration options
  INTEGER, PARAMETER :: HCOI_CONFIG_MODE_STANDARD = 0    ! Standard mode
  INTEGER, PARAMETER :: HCOI_CONFIG_MODE_NATIVE = 1      ! Native grid mode
  INTEGER, PARAMETER :: HCOI_CONFIG_MODE_OPTIMIZED = 2   ! Optimized mode
  
  INTEGER, PARAMETER :: HCOI_REGRID_METHOD_BILINEAR = 1    ! Bilinear interpolation
  INTEGER, PARAMETER :: HCOI_REGRID_METHOD_CONSERV = 2     ! Conservative remapping
  INTEGER, PARAMETER :: HCOI_REGRID_METHOD_NEAREST = 3     ! Nearest neighbor
  
  ! Global configuration instance
  TYPE(HCOI_Config), POINTER :: SimpleConfigInstance => NULL()
  
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Config_Init
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Config\_Init initializes the
! simplified ESMF configuration system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Config_Init( HcoState, RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Config_Init (hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Allocate configuration instance if not already allocated
    IF ( .NOT. ASSOCIATED(SimpleConfigInstance) ) THEN
       ALLOCATE(SimpleConfigInstance)
    ENDIF
    
    ! Initialize configuration with default values
    SimpleConfigInstance%IsInitialized = .FALSE.
    SimpleConfigInstance%UseNativeGrid = .TRUE.        ! Default to native grid
    SimpleConfigInstance%EnableCaching = .TRUE.        ! Default to enable caching
    SimpleConfigInstance%EnableOptimization = .TRUE.   ! Default to enable optimization
    SimpleConfigInstance%IntegrationMode = HCOI_CONFIG_MODE_NATIVE  ! Default to native mode
    SimpleConfigInstance%RegridMethod = HCOI_REGRID_METHOD_BILINEAR  ! Default to bilinear
    SimpleConfigInstance%MaxCacheEntries = 50          ! Default max cache entries
    SimpleConfigInstance%CacheThreshold = 0.8_hp       ! Default cache threshold (80%)
    SimpleConfigInstance%VerboseOutput = .FALSE.       ! Default to quiet output
    SimpleConfigInstance%DebugMode = .FALSE.           ! Default to no debug mode
    
    ! Mark as initialized
    SimpleConfigInstance%IsInitialized = .TRUE.
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF configuration system initialized', &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Config_Init
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Config_Read
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Config\_Read reads configuration
! parameters from file for the simplified ESMF system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Config_Read( HcoState, ConfigFile, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    CHARACTER(LEN=*),   INTENT(IN)      :: ConfigFile    ! Configuration file name
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
    INTEGER             :: LUN, IOS
    CHARACTER(LEN=255)  :: line, keyword, value
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Config_Read (hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if configuration instance is initialized
    IF ( .NOT. ASSOCIATED(SimpleConfigInstance) .OR. .NOT. SimpleConfigInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Simple configuration instance not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Open configuration file
    OPEN(NEWUNIT=LUN, FILE=TRIM(ConfigFile), STATUS='OLD', ACTION='READ', IOSTAT=IOS)
    IF ( IOS /= 0 ) THEN
       CALL HCO_WARNING( 'Cannot open configuration file: ' // TRIM(ConfigFile), RC, THISLOC=LOC )
       RC = HCO_SUCCESS  ! Not a fatal error
       RETURN
    ENDIF
    
    ! Read configuration file line by line
    DO
       READ(LUN, '(A)', IOSTAT=IOS) line
       IF ( IOS /= 0 ) EXIT  ! End of file or error
       
       ! Skip comment lines and blank lines
       IF ( LEN_TRIM(line) == 0 .OR. line(1:1) == '#' .OR. line(1:1) == '!' ) CYCLE
       
       ! Parse line into keyword and value
       CALL HCO_ParseConfigLine( line, keyword, value, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_WARNING( 'Error parsing configuration line: ' // TRIM(line), RC, THISLOC=LOC )
          RC = HCO_SUCCESS  ! Continue reading
          CYCLE
       ENDIF
       
       ! Process configuration options
       SELECT CASE (TRIM(keyword))
       CASE ('USE_NATIVE_GRID')
          READ(value, *, IOSTAT=IOS) SimpleConfigInstance%UseNativeGrid
          IF ( IOS /= 0 ) THEN
             CALL HCO_WARNING( 'Error reading USE_NATIVE_GRID value: ' // TRIM(value), RC, THISLOC=LOC )
             RC = HCO_SUCCESS  ! Continue reading
          ENDIF
          
       CASE ('ENABLE_CACHING')
          READ(value, *, IOSTAT=IOS) SimpleConfigInstance%EnableCaching
          IF ( IOS /= 0 ) THEN
             CALL HCO_WARNING( 'Error reading ENABLE_CACHING value: ' // TRIM(value), RC, THISLOC=LOC )
             RC = HCO_SUCCESS  ! Continue reading
          ENDIF
          
       CASE ('ENABLE_OPTIMIZATION')
          READ(value, *, IOSTAT=IOS) SimpleConfigInstance%EnableOptimization
          IF ( IOS /= 0 ) THEN
             CALL HCO_WARNING( 'Error reading ENABLE_OPTIMIZATION value: ' // TRIM(value), RC, THISLOC=LOC )
             RC = HCO_SUCCESS  ! Continue reading
          ENDIF
          
       CASE ('INTEGRATION_MODE')
          READ(value, *, IOSTAT=IOS) SimpleConfigInstance%IntegrationMode
          IF ( IOS /= 0 ) THEN
             CALL HCO_WARNING( 'Error reading INTEGRATION_MODE value: ' // TRIM(value), RC, THISLOC=LOC )
             RC = HCO_SUCCESS  ! Continue reading
          ELSE
             ! Validate integration mode
             SimpleConfigInstance%IntegrationMode = MAX(MIN(SimpleConfigInstance%IntegrationMode, &
                                                         HCOI_CONFIG_MODE_OPTIMIZED), &
                                                     HCOI_CONFIG_MODE_STANDARD)
          ENDIF
          
       CASE ('REGRID_METHOD')
          READ(value, *, IOSTAT=IOS) SimpleConfigInstance%RegridMethod
          IF ( IOS /= 0 ) THEN
             CALL HCO_WARNING( 'Error reading REGRID_METHOD value: ' // TRIM(value), RC, THISLOC=LOC )
             RC = HCO_SUCCESS  ! Continue reading
          ELSE
             ! Validate regrid method
             SELECT CASE (SimpleConfigInstance%RegridMethod)
             CASE (HCOI_REGRID_METHOD_BILINEAR, &
                   HCOI_REGRID_METHOD_CONSERV, &
                   HCOI_REGRID_METHOD_NEAREST)
                ! Valid method, no action needed
             CASE DEFAULT
                SimpleConfigInstance%RegridMethod = HCOI_REGRID_METHOD_BILINEAR
                CALL HCO_WARNING( 'Invalid REGRID_METHOD value: ' // TRIM(value) // &
                                 ', using default BILINEAR', RC, THISLOC=LOC )
                RC = HCO_SUCCESS  ! Continue reading
             END SELECT
          ENDIF
          
       CASE ('MAX_CACHE_ENTRIES')
          READ(value, *, IOSTAT=IOS) SimpleConfigInstance%MaxCacheEntries
          IF ( IOS /= 0 ) THEN
             CALL HCO_WARNING( 'Error reading MAX_CACHE_ENTRIES value: ' // TRIM(value), RC, THISLOC=LOC )
             RC = HCO_SUCCESS  ! Continue reading
          ELSE
             ! Validate max cache entries
             SimpleConfigInstance%MaxCacheEntries = MAX(1, SimpleConfigInstance%MaxCacheEntries)
          ENDIF
          
       CASE ('CACHE_THRESHOLD')
          READ(value, *, IOSTAT=IOS) SimpleConfigInstance%CacheThreshold
          IF ( IOS /= 0 ) THEN
             CALL HCO_WARNING( 'Error reading CACHE_THRESHOLD value: ' // TRIM(value), RC, THISLOC=LOC )
             RC = HCO_SUCCESS  ! Continue reading
          ELSE
             ! Validate cache threshold
             SimpleConfigInstance%CacheThreshold = MAX(MIN(SimpleConfigInstance%CacheThreshold, 1.0_hp), 0.0_hp)
          ENDIF
          
       CASE ('VERBOSE_OUTPUT')
          READ(value, *, IOSTAT=IOS) SimpleConfigInstance%VerboseOutput
          IF ( IOS /= 0 ) THEN
             CALL HCO_WARNING( 'Error reading VERBOSE_OUTPUT value: ' // TRIM(value), RC, THISLOC=LOC )
             RC = HCO_SUCCESS  ! Continue reading
          ENDIF
          
       CASE ('DEBUG_MODE')
          READ(value, *, IOSTAT=IOS) SimpleConfigInstance%DebugMode
          IF ( IOS /= 0 ) THEN
             CALL HCO_WARNING( 'Error reading DEBUG_MODE value: ' // TRIM(value), RC, THISLOC=LOC )
             RC = HCO_SUCCESS  ! Continue reading
          ENDIF
          
       CASE DEFAULT
          ! Unknown keyword, issue warning but continue
          CALL HCO_WARNING( 'Unknown configuration keyword: ' // TRIM(keyword), RC, THISLOC=LOC )
          RC = HCO_SUCCESS  ! Continue reading
       END SELECT
    ENDDO
    
    ! Close configuration file
    CLOSE(LUN, IOSTAT=IOS)
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF configuration read from file: ' // TRIM(ConfigFile), &
                    LUN=HcoState%Config%hcoLogLUN )
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Config_Read
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Config_GetOpt
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Config\_GetOpt retrieves a configuration
! option from the simplified ESMF configuration system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Config_GetOpt( OptionName, OptionValue, RC )
!
! !ARGUMENTS:
!
    CHARACTER(LEN=*),   INTENT(IN)      :: OptionName    ! Name of configuration option
    CLASS(*),           INTENT(  OUT) :: OptionValue   ! Value of configuration option
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Config_GetOpt (hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if configuration instance is initialized
    IF ( .NOT. ASSOCIATED(SimpleConfigInstance) .OR. .NOT. SimpleConfigInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Simple configuration instance not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Get configuration option based on name
    SELECT TYPE (OptionValue)
    TYPE IS (LOGICAL)
       SELECT CASE (TRIM(OptionName))
       CASE ('USE_NATIVE_GRID')
          OptionValue = SimpleConfigInstance%UseNativeGrid
       CASE ('ENABLE_CACHING')
          OptionValue = SimpleConfigInstance%EnableCaching
       CASE ('ENABLE_OPTIMIZATION')
          OptionValue = SimpleConfigInstance%EnableOptimization
       CASE ('VERBOSE_OUTPUT')
          OptionValue = SimpleConfigInstance%VerboseOutput
       CASE ('DEBUG_MODE')
          OptionValue = SimpleConfigInstance%DebugMode
       CASE DEFAULT
          CALL HCO_ERROR( 'Unknown logical configuration option: ' // TRIM(OptionName), RC, THISLOC=LOC )
          RETURN
       END SELECT
       
    TYPE IS (INTEGER)
       SELECT CASE (TRIM(OptionName))
       CASE ('INTEGRATION_MODE')
          OptionValue = SimpleConfigInstance%IntegrationMode
       CASE ('REGRID_METHOD')
          OptionValue = SimpleConfigInstance%RegridMethod
       CASE ('MAX_CACHE_ENTRIES')
          OptionValue = SimpleConfigInstance%MaxCacheEntries
       CASE DEFAULT
          CALL HCO_ERROR( 'Unknown integer configuration option: ' // TRIM(OptionName), RC, THISLOC=LOC )
          RETURN
       END SELECT
       
    TYPE IS (REAL(hp))
       SELECT CASE (TRIM(OptionName))
       CASE ('CACHE_THRESHOLD')
          OptionValue = SimpleConfigInstance%CacheThreshold
       CASE DEFAULT
          CALL HCO_ERROR( 'Unknown real configuration option: ' // TRIM(OptionName), RC, THISLOC=LOC )
          RETURN
       END SELECT
       
    CLASS DEFAULT
       CALL HCO_ERROR( 'Unsupported configuration option type for: ' // TRIM(OptionName), RC, THISLOC=LOC )
       RETURN
    END SELECT
    
  END SUBROUTINE HCOI_ESMF_Config_GetOpt
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Config_SetOpt
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Config\_SetOpt sets a configuration
! option in the simplified ESMF configuration system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Config_SetOpt( OptionName, OptionValue, RC )
!
! !ARGUMENTS:
!
    CHARACTER(LEN=*),   INTENT(IN)      :: OptionName    ! Name of configuration option
    CLASS(*),           INTENT(IN)      :: OptionValue   ! Value of configuration option
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Config_SetOpt (hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if configuration instance is initialized
    IF ( .NOT. ASSOCIATED(SimpleConfigInstance) .OR. .NOT. SimpleConfigInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Simple configuration instance not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Set configuration option based on name
    SELECT TYPE (OptionValue)
    TYPE IS (LOGICAL)
       SELECT CASE (TRIM(OptionName))
       CASE ('USE_NATIVE_GRID')
          SimpleConfigInstance%UseNativeGrid = OptionValue
       CASE ('ENABLE_CACHING')
          SimpleConfigInstance%EnableCaching = OptionValue
       CASE ('ENABLE_OPTIMIZATION')
          SimpleConfigInstance%EnableOptimization = OptionValue
       CASE ('VERBOSE_OUTPUT')
          SimpleConfigInstance%VerboseOutput = OptionValue
       CASE ('DEBUG_MODE')
          SimpleConfigInstance%DebugMode = OptionValue
       CASE DEFAULT
          CALL HCO_ERROR( 'Unknown logical configuration option: ' // TRIM(OptionName), RC, THISLOC=LOC )
          RETURN
       END SELECT
       
    TYPE IS (INTEGER)
       SELECT CASE (TRIM(OptionName))
       CASE ('INTEGRATION_MODE')
          SimpleConfigInstance%IntegrationMode = MAX(MIN(OptionValue, &
                                                       HCOI_CONFIG_MODE_OPTIMIZED), &
                                                   HCOI_CONFIG_MODE_STANDARD)
       CASE ('REGRID_METHOD')
          SELECT CASE (OptionValue)
          CASE (HCOI_REGRID_METHOD_BILINEAR, &
                HCOI_REGRID_METHOD_CONSERV, &
                HCOI_REGRID_METHOD_NEAREST)
             SimpleConfigInstance%RegridMethod = OptionValue
          CASE DEFAULT
             CALL HCO_WARNING( 'Invalid REGRID_METHOD value: ' // TRIM(Int2Str(OptionValue)) // &
                              ', using BILINEAR', RC, THISLOC=LOC )
             SimpleConfigInstance%RegridMethod = HCOI_REGRID_METHOD_BILINEAR
             RC = HCO_SUCCESS  ! Continue anyway
          END SELECT
       CASE ('MAX_CACHE_ENTRIES')
          SimpleConfigInstance%MaxCacheEntries = MAX(1, OptionValue)
       CASE DEFAULT
          CALL HCO_ERROR( 'Unknown integer configuration option: ' // TRIM(OptionName), RC, THISLOC=LOC )
          RETURN
       END SELECT
       
    TYPE IS (REAL(hp))
       SELECT CASE (TRIM(OptionName))
       CASE ('CACHE_THRESHOLD')
          SimpleConfigInstance%CacheThreshold = MAX(MIN(OptionValue, 1.0_hp), 0.0_hp)
       CASE DEFAULT
          CALL HCO_ERROR( 'Unknown real configuration option: ' // TRIM(OptionName), RC, THISLOC=LOC )
          RETURN
       END SELECT
       
    CLASS DEFAULT
       CALL HCO_ERROR( 'Unsupported configuration option type for: ' // TRIM(OptionName), RC, THISLOC=LOC )
       RETURN
    END SELECT
    
  END SUBROUTINE HCOI_ESMF_Config_SetOpt
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Config_Validate
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Config\_Validate validates the
! simplified ESMF configuration parameters.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Config_Validate( HcoState, Valid, RC )
!
! !ARGUMENTS:
!
    TYPE(HCO_State),    POINTER        :: HcoState     ! HEMCO state object
    LOGICAL,            INTENT(  OUT) :: Valid        ! Whether configuration is valid
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Config_Validate (hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code and output
    RC = HCO_SUCCESS
    Valid = .TRUE.
    
    ! Check if configuration instance is initialized
    IF ( .NOT. ASSOCIATED(SimpleConfigInstance) .OR. .NOT. SimpleConfigInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Simple configuration instance not initialized', RC, THISLOC=LOC )
       Valid = .FALSE.
       RETURN
    ENDIF
    
    ! Validate configuration parameters
    ! Check integration mode
    IF ( SimpleConfigInstance%IntegrationMode < HCOI_CONFIG_MODE_STANDARD .OR. &
         SimpleConfigInstance%IntegrationMode > HCOI_CONFIG_MODE_OPTIMIZED ) THEN
       Valid = .FALSE.
       CALL HCO_ERROR( 'Invalid integration mode: ' // TRIM(Int2Str(SimpleConfigInstance%IntegrationMode)), &
                      RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Check regrid method
    SELECT CASE (SimpleConfigInstance%RegridMethod)
    CASE (HCOI_REGRID_METHOD_BILINEAR, &
          HCOI_REGRID_METHOD_CONSERV, &
          HCOI_REGRID_METHOD_NEAREST)
       ! Valid method, no action needed
    CASE DEFAULT
       Valid = .FALSE.
       CALL HCO_ERROR( 'Invalid regrid method: ' // TRIM(Int2Str(SimpleConfigInstance%RegridMethod)), &
                      RC, THISLOC=LOC )
       RETURN
    END SELECT
    
    ! Check max cache entries
    IF ( SimpleConfigInstance%MaxCacheEntries < 1 ) THEN
       Valid = .FALSE.
       CALL HCO_ERROR( 'Invalid max cache entries: ' // TRIM(Int2Str(SimpleConfigInstance%MaxCacheEntries)), &
                      RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Check cache threshold
    IF ( SimpleConfigInstance%CacheThreshold < 0.0_hp .OR. &
         SimpleConfigInstance%CacheThreshold > 1.0_hp ) THEN
       Valid = .FALSE.
       CALL HCO_ERROR( 'Invalid cache threshold: ' // TRIM(Real2Str(SimpleConfigInstance%CacheThreshold)), &
                      RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       IF (Valid) THEN
          CALL HCO_MSG( 'Simplified ESMF configuration validated successfully', &
                       LUN=HcoState%Config%hcoLogLUN )
       ELSE
          CALL HCO_MSG( 'Simplified ESMF configuration validation FAILED', &
                       LUN=HcoState%Config%hcoLogLUN )
       ENDIF
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Config_Validate
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Config_Apply
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Config\_Apply applies the
! simplified ESMF configuration to the system.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Config_Apply( HcoState, RC )
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
    LOGICAL             :: validConfig
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Config_Apply (hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if configuration instance is initialized
    IF ( .NOT. ASSOCIATED(SimpleConfigInstance) .OR. .NOT. SimpleConfigInstance%IsInitialized ) THEN
       CALL HCO_ERROR( 'Simple configuration instance not initialized', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Validate configuration
    CALL HCOI_ESMF_Config_Validate( HcoState, validConfig, RC )
    IF ( RC /= HCO_SUCCESS ) THEN
       CALL HCO_ERROR( 'Error validating configuration', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    IF ( .NOT. validConfig ) THEN
       CALL HCO_ERROR( 'Invalid configuration parameters', RC, THISLOC=LOC )
       RETURN
    ENDIF
    
    ! Apply configuration to the system
    ! This would involve setting up the ESMF integration with the configured parameters
    
    ! Set integration mode
    SELECT CASE (SimpleConfigInstance%IntegrationMode)
    CASE (HCOI_CONFIG_MODE_NATIVE)
       ! Configure for native grid integration
       CALL HCOI_ESMF_Simplified_Setup( HcoState, &
            ESMF_GridEmptyCreate(), &  ! Placeholder for actual host grid
            SimpleConfigInstance%UseNativeGrid, &
            SimpleConfigInstance%IntegrationMode, &
            RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error setting up native grid integration', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
    CASE (HCOI_CONFIG_MODE_OPTIMIZED)
       ! Configure for optimized integration
       CALL HCOI_ESMF_Simplified_Setup( HcoState, &
            ESMF_GridEmptyCreate(), &  ! Placeholder for actual host grid
            SimpleConfigInstance%UseNativeGrid, &
            SimpleConfigInstance%IntegrationMode, &
            RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error setting up optimized integration', RC, THISLOC=LOC )
          RETURN
       ENDIF
       
    CASE DEFAULT
       ! Configure for standard integration
       CALL HCOI_ESMF_Simplified_Setup( HcoState, &
            ESMF_GridEmptyCreate(), &  ! Placeholder for actual host grid
            SimpleConfigInstance%UseNativeGrid, &
            SimpleConfigInstance%IntegrationMode, &
            RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_ERROR( 'Error setting up standard integration', RC, THISLOC=LOC )
          RETURN
       ENDIF
    END SELECT
    
    ! Configure regrid method
    ! This would be implemented in the integration modules
    
    ! Configure caching
    IF ( SimpleConfigInstance%EnableCaching ) THEN
       CALL HCOI_ESMF_InitWeightCaching( SimpleConfigInstance%MaxCacheEntries, RC )
       IF ( RC /= HCO_SUCCESS ) THEN
          CALL HCO_WARNING( 'Warning initializing weight caching', RC, THISLOC=LOC )
          RC = HCO_SUCCESS  ! Continue anyway
       ENDIF
    
    ! Configure optimization
    IF ( SimpleConfigInstance%EnableOptimization ) THEN
       ! Enable optimization features
       ! This would be implemented in the integration modules
    ENDIF
    
    ! Configure verbosity
    ! This is handled through HcoState%Config%doVerbose
    
    ! Configure debug mode
    IF ( SimpleConfigInstance%DebugMode ) THEN
       ! Enable debug features
       ! This would be implemented in the integration modules
    ENDIF
    
    ! Verbose output
    IF ( HcoState%amIRoot .AND. HcoState%Config%doVerbose ) THEN
       CALL HCO_MSG( 'Simplified ESMF configuration applied successfully', &
                    LUN=HcoState%Config%hcoLogLUN )
       WRITE(HcoState%Config%hcoLogLUN,*) '  Integration mode: ', SimpleConfigInstance%IntegrationMode
       WRITE(HcoState%Config%hcoLogLUN,*) '  Regrid method: ', SimpleConfigInstance%RegridMethod
       WRITE(HcoState%Config%hcoLogLUN,*) '  Use native grid: ', SimpleConfigInstance%UseNativeGrid
       WRITE(HcoState%Config%hcoLogLUN,*) '  Enable caching: ', SimpleConfigInstance%EnableCaching
       WRITE(HcoState%Config%hcoLogLUN,*) '  Max cache entries: ', SimpleConfigInstance%MaxCacheEntries
       WRITE(HcoState%Config%hcoLogLUN,*) '  Cache threshold: ', SimpleConfigInstance%CacheThreshold
       WRITE(HcoState%Config%hcoLogLUN,*) '  Enable optimization: ', SimpleConfigInstance%EnableOptimization
       WRITE(HcoState%Config%hcoLogLUN,*) '  Verbose output: ', SimpleConfigInstance%VerboseOutput
       WRITE(HcoState%Config%hcoLogLUN,*) '  Debug mode: ', SimpleConfigInstance%DebugMode
    ENDIF
    
  END SUBROUTINE HCOI_ESMF_Config_Apply
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HCOI_ESMF_Config_Cleanup
!
! !DESCRIPTION: Subroutine HCOI\_ESMF\_Config\_Cleanup cleans up the
! simplified ESMF configuration resources.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOI_ESMF_Config_Cleanup( RC )
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
    CHARACTER(LEN=255)  :: LOC = 'HCOI_ESMF_Config_Cleanup (hcoi_esmf_config_mod.F90)'
    
    ! Initialize return code
    RC = HCO_SUCCESS
    
    ! Check if configuration instance exists
    IF ( ASSOCIATED(SimpleConfigInstance) ) THEN
       ! Deallocate the configuration instance
       DEALLOCATE(SimpleConfigInstance)
       SimpleConfigInstance => NULL()
    ENDIF
    
    ! Verbose output
    ! Note: We can't use HcoState here since it's not passed to this routine
    ! In a real implementation, we might pass HcoState or use a global logger
    
  END SUBROUTINE HCOI_ESMF_Config_Cleanup
!EOC
END MODULE HCOI_ESMF_Config_Mod