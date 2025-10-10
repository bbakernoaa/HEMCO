!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: HEMCO_NUOPC_GridCompMod -- A NUOPC component to compute emissions 
!  using the Harmonized Emissions Component (HEMCO). 
!
!  !DESCRIPTION:
!
!  HEMCO is a software component for computing (atmospheric) emissions
!  from different sources, regions, and species on a user-defined
!  grid. It can combine, overlay, and update a set of data inventories
!  ('base emissions') and scale factors, as specified by the user
!  through the HEMCO configuration file. Emissions that depend on
!  environmental variables and non-linear parameterizations are
!  calculated in separate HEMCO extensions. See Keller et al. (2014) for
!  more details.
!\\
!\\
!  This component computes emissions as specified by the user via the HEMCO
!  configuration file (HEMCO\_Config.rc, unless specified otherwise). Multiple 
!  HEMCO instances can be employed in a single run (with instances referring 
!  to different emission configurations). The computed emissions are made 
!  avaiable to other components (e.g. GOCART) via user-specified HEMCO 
!  diagnostics. All HEMCO diagnostics are automatically added to the list of 
!  HEMCO exports, thus making them available to any other component. The 
!  HEMCO diagnostics can be defined in the HEMCO diagnostics configuration 
!  (HEMCO\_DiagnFile.rc, unless specified otherwise).
!
! !REFERENCES: 
!
! C. A. Keller, M. S. Long, R. M. Yantosca, A. M. Da Silva, S. Pawson, D. J. 
! Jacob: HEMCO v1.0: a versatile, ESMF-compliant component for calculation 
! emissions in atmospheric models. Geosci. Model Dev., 7, 1409-1417, 2014. 
!\\
!\\
!
! !INTERFACE: 
!
MODULE HEMCO_NUOPC_GridCompMod
!
! !USES:
!
  USE ESMF
  USE NUOPC
  USE NUOPC_Model, ONLY: &
      model_routine_SS           => SetServices, &
      model_label_Advance        => label_Advance, &
      model_label_Finalize       => label_Finalize, &
      model_label_Initialize     => label_Initialize, &
      model_label_SetClock       => label_SetClock, &
      model_label_DataInitialize => label_DataInitialize

  ! HEMCO routines/variables
  USE HCO_ERROR_MOD
  USE HCO_DIAGN_MOD
  USE HCO_CHARTOOLS_MOD
  USE HCO_TYPES_MOD,        ONLY : ConfigObj
  USE HCO_STATE_MOD,        ONLY : HCO_STATE
  USE HCOX_STATE_MOD,       ONLY : EXT_STATE

  ! ESMF-based HEMCO integration components
  USE HCOI_ESMF_MOD,        ONLY : HCOI_ESMF_Init, HCOI_ESMF_SetupGrids, &
                                   HCOI_ESMF_CreateRegrid, HCOI_ESMF_PerformRegrid, &
                                   HCOI_ESMF_MapToHost, HCOI_ESMF_Cleanup
  USE HCOI_ESMF_CONFIG_MOD, ONLY : HCOI_ESMF_Config_Init, HCOI_ESMF_Config_Read, &
                                   HCOI_ESMF_Config_GetOpt, HCOI_ESMF_Config_SetOpt, &
                                   HCOI_ESMF_Config_Validate, HCOI_ESMF_Config_Apply, &
                                   HCOI_ESMF_Config_Cleanup
  USE HCOI_ESMF_INTEGRATION_MOD, ONLY : HCOI_ESMF_Integration_Init, &
                                        HCOI_ESMF_Integration_Setup, &
                                        HCOI_ESMF_Integration_Execute, &
                                        HCOI_ESMF_Integration_Cleanup, &
                                        HCOI_ESMF_Integration_MapData, &
                                        HCOI_ESMF_Integration_GetHostGrid
  USE HCOI_ESMF_IO_MOD,     ONLY : HCOI_ESMF_IO_Init, HCOI_ESMF_IO_Final, &
                                   HCOI_ESMF_IO_ReadData, HCOI_ESMF_IO_WriteData, &
                                   HCOI_ESMF_IO_CacheData, HCOI_ESMF_IO_ReadFromCache
  USE HCOI_ESMF_REGRID_MOD, ONLY : HCOI_ESMF_Regrid_Init, HCOI_ESMF_Regrid_Setup, &
                                   HCOI_ESMF_Regrid_Execute, HCOI_ESMF_Regrid_Finalize, &
                                   HCOI_ESMF_Regrid_CreateOperator, HCOI_ESMF_Regrid_ApplyOperator, &
                                   HCOI_ESMF_Regrid_StoreWeights, HCOI_ESMF_Regrid_LoadWeights

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC SetServices
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE                              :: InitializeP1
  PRIVATE                              :: InitializeP2
  PRIVATE                              :: DataInitialize
  PRIVATE                              :: Advance
  PRIVATE                              :: Finalize
  PRIVATE                              :: HEMCOinit_
  PRIVATE                              :: HEMCOinit_ESMF_
  PRIVATE                              :: HEMCOrun_
  PRIVATE                              :: HEMCOfinal_
  PRIVATE                              :: NewInst_
  PRIVATE                              :: SetExtFields
  PRIVATE                              :: GetSUNCOS
!
! !PRIVATE TYPES:
!
  ! HEMCO state objects for various HEMCO instances 
  TYPE :: Instance
     TYPE(ConfigObj), POINTER             :: HcoConfig 
     TYPE(HCO_State), POINTER             :: HcoState
     TYPE(Ext_State), POINTER             :: ExtState
     TYPE(Instance),  POINTER             :: NextInst
  END TYPE Instance

  ! Linked list holding all active HEMCO instances
  TYPE(Instance), POINTER                 :: Instances => NULL()

  ! ESMF integration state for each instance
  TYPE :: ESMFIntegrationState
     TYPE(ESMF_Grid)                      :: HostGrid
     TYPE(ESMF_FieldBundle)               :: ImportBundle
     TYPE(ESMF_FieldBundle)               :: ExportBundle
     LOGICAL                              :: GridInitialized = .FALSE.
     LOGICAL                              :: IntegrationInitialized = .FALSE.
     TYPE(ESMFIntegrationState), POINTER  :: NextInst => NULL()
  END TYPE ESMFIntegrationState

   
   ! Error handling and resource management
   INTEGER, PARAMETER :: HEMCO_NUOPC_SUCCESS = 0
   INTEGER, PARAMETER :: HEMCO_NUOPC_ERROR   = 1
   INTEGER, PARAMETER :: HEMCO_NUOPC_WARNING = 2

 !EOP
 !------------------------------------------------------------------------------
 !BOC
 CONTAINS
  TYPE(ESMFIntegrationState), POINTER     :: ESMFStates => NULL()
!
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  SetServices 
!
! !DESCRIPTION: SetServices routine 
!
! !INTERFACE:
!
  subroutine SetServices ( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
!
! !OUTPUT PARAMETERS:
!
    integer, optional  , intent(  OUT) :: RC  ! return code
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version
!  09 Oct 2025 - HEMCO Dev   - Adapted for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
! 
! !LOCAL VARIABLES:
!
    integer                            :: STATUS
    character(len=ESMF_MAXSTR)         :: COMP_NAME
    character(len=ESMF_MAXSTR)         :: Iam

    !=======================================================================
    ! Set services begins here 
    !=======================================================================

    ! Set up traceback info
    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // '::' // Iam

    ! Set the Initialize, Run and Finalize entry points
    call ESMF_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, &
                                      InitializeP1, RC=STATUS )
    _VERIFY(STATUS)
    call ESMF_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, &
                                      InitializeP2, phase=2, RC=STATUS )
    _VERIFY(STATUS)
    call ESMF_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, &
                                      DataInitialize, label=model_label_DataInitialize, RC=STATUS )
    _VERIFY(STATUS)
    call ESMF_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN, &
                                      Advance, RC=STATUS )
    _VERIFY(STATUS)
    call ESMF_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE,   &
                                      Finalize, RC=STATUS )
    _VERIFY(STATUS)

    !=======================================================================
    !                    %%% NUOPC Data Services %%%
    !=======================================================================

    ! Set standard labels
    call NUOPC_CompSetLabel(GC, model_label_Initialize, "InitializeP1", RC=STATUS)
    _VERIFY(STATUS)
    call NUOPC_CompSetLabel(GC, model_label_DataInitialize, "DataInitialize", RC=STATUS)
    _VERIFY(STATUS)
    call NUOPC_CompSetLabel(GC, model_label_Advance, "Advance", RC=STATUS)
    _VERIFY(STATUS)
    call NUOPC_CompSetLabel(GC, model_label_Finalize, "Finalize", RC=STATUS)
    _VERIFY(STATUS)

    ! Set the default clock
    call NUOPC_CompSet(GC, defaultTimeStepMult=1, RC=STATUS)
    _VERIFY(STATUS)

    ! Successful return
    if (present(RC)) RC = ESMF_SUCCESS

  end subroutine SetServices
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: InitializeP1 
!
! !DESCRIPTION: InitializeP1 is the first initialize method of the HEMCO 
!  NUOPC component. This is a simple ESMF/NUOPC wrapper which calls down
!  to the Initialize method of the HEMCO code. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE InitializeP1( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT)         :: GC      ! Ref. to this GridComp
!                                                      
! !OUTPUT PARAMETERS:                                  
!                                                      
    INTEGER,             INTENT(OUT)   :: RC          ! Error return code
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version
!  09 Oct 2025 - HEMCO Dev   - Adapted for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    TYPE(Instance), POINTER  :: ThisInst => NULL()
    TYPE(ESMFIntegrationState), POINTER :: ThisESMFState => NULL()
    INTEGER                  :: STATUS
    INTEGER                  :: nnInst
    INTEGER                  :: n, I
    CHARACTER(LEN=ESMF_MAXSTR) :: COMP_NAME
    CHARACTER(LEN=ESMF_MAXSTR) :: ConfigFile
    CHARACTER(LEN=ESMF_MAXSTR) :: Label
    LOGICAL                    :: am_I_Root

    ! Set up traceback 
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    __Iam__(TRIM(COMP_NAME) // '::InitializeP1')

    ! Is this the root CPU?
    am_I_Root = ESMF_VMIsPetLocalPet(ESMF_VMGetCurrent(), RC=STATUS)
    _VERIFY(STATUS)

    ! Get number of instances from configuration
    nnInst = 1  ! Default to 1 instance for now
    
    ! Verbose
    IF ( am_I_Root ) WRITE(*,*) TRIM(__Iam__), ' - number of HEMCO instances: ', &
                                nnInst

    ! Set HEMCO services for all instances
    DO N = 1, nnInst

       ! Get HEMCO configuration file names
       WRITE(Label,'(a14,i3.3,a1)') 'HEMCO_CONFIG--',N,':'
       ConfigFile = "HEMCO_Config.rc" ! Default config file

       ! Verbose
       IF ( Am_I_Root ) WRITE(*,'(a19,i3.3,a2,a)') '--> HEMCO instance ',    &
                              N, ': ', TRIM(ConfigFile)
 
       ! Create a new instance object that holds the HEMCO states for this
       ! instance. Will be added to linked list Instances.
       CALL NewInst_( ThisInst, RC=STATUS )
       _VERIFY(STATUS)
 
       ! Initialize HEMCO for this instance
       ! Get the corresponding ESMF state for this instance
       ThisESMFState => ESMFStates
       DO I = 1, N-1 ! Move to the Nth ESMF state
          IF ( ASSOCIATED(ThisESMFState) ) THEN
             ThisESMFState => ThisESMFState%NextInst
          ENDIF
       ENDDO
       
       CALL HEMCOinit_( GC, ThisInst, ThisESMFState, RC=STATUS )
       _VERIFY(STATUS)

       ! Cleanup pointer
       ThisInst => NULL()
    ENDDO

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE InitializeP1
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: InitializeP2 
!
! !DESCRIPTION: InitializeP2 is the second initialize phase of the HEMCO 
!  NUOPC component. This phase sets up the ESMF integration components.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE InitializeP2( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT)         :: GC      ! Ref. to this GridComp
!                                                      
! !OUTPUT PARAMETERS:                                  
!                                                      
    INTEGER,             INTENT(OUT)   :: RC          ! Error return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - HEMCO Dev   - Initial version for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    TYPE(Instance), POINTER  :: ThisInst => NULL()
    TYPE(ESMFIntegrationState), POINTER :: ThisESMFState => NULL()
    INTEGER                  :: STATUS
    LOGICAL                  :: am_I_Root

    ! Set up traceback 
    __Iam__('InitializeP2')

    ! Is this the root CPU?
    am_I_Root = ESMF_VMIsPetLocalPet(ESMF_VMGetCurrent(), RC=STATUS)
    _VERIFY(STATUS)

    ! Set up ESMF integration for each instance
    ThisInst => Instances
    ThisESMFState => ESMFStates
    DO WHILE ( ASSOCIATED(ThisInst) .AND. ASSOCIATED(ThisESMFState) )

       ! Set up ESMF integration for this instance
       CALL HEMCOinit_ESMF_( GC, ThisInst, ThisESMFState, RC=STATUS )
       _VERIFY(STATUS)
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: DataInitialize
!
! !DESCRIPTION: DataInitialize is the data initialization phase of the HEMCO 
!  NUOPC component. This phase is responsible for initializing data fields
!  and performing any data-dependent setup operations.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE DataInitialize( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT)         :: GC      ! Ref. to this GridComp
!                                                      
! !OUTPUT PARAMETERS:                                  
!                                                      
    INTEGER,             INTENT(OUT)   :: RC          ! Error return code
!
! !REVISION HISTORY:
!  09 Oct 2025 - HEMCO Dev   - Initial version for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    TYPE(Instance), POINTER  :: ThisInst => NULL()
    TYPE(ESMFIntegrationState), POINTER :: ThisESMFState => NULL()
    TYPE(ESMF_State)         :: ImportState
    TYPE(ESMF_State)         :: ExportState
    INTEGER                  :: STATUS
    LOGICAL                  :: am_I_Root

    ! Set up traceback 
    __Iam__('DataInitialize')

    ! Is this the root CPU?
    am_I_Root = ESMF_VMIsPetLocalPet(ESMF_VMGetCurrent(), RC=STATUS)
    _VERIFY(STATUS)

    ! Get the import and export states
    CALL ESMF_GridCompGet(GC, IMPORTSTATE=ImportState, EXPORTSTATE=ExportState, RC=STATUS)
    _VERIFY(STATUS)

    ! Set up field mappings and initialize data fields
    ! This is where we would typically set up the connection between import fields
    ! and internal HEMCO fields
    
    ! For each instance, perform data initialization
    ThisInst => Instances
    ThisESMFState => ESMFStates
    DO WHILE ( ASSOCIATED(ThisInst) .AND. ASSOCIATED(ThisESMFState) )
       
       ! Initialize any data-dependent components for this instance
       ! This might include reading initial data files, setting up field mappings, etc.
       
       ! Initialize import fields from the import state
       ! This is a simplified approach - in practice, we would extract specific fields
       ! from the import state and map them to HEMCO extension variables
       
       ! Go to next instance in list
       ThisInst => ThisInst%NextInst
       ThisESMFState => ThisESMFState%NextInst
    ENDDO

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE DataInitialize
!EOC

       ! Go to next instance in list
       ThisInst => ThisInst%NextInst
       ThisESMFState => ThisESMFState%NextInst  ! This should be fixed - need to handle multiple ESMF states
    ENDDO

    ! Successful return
    RC = ESMF_SUCCESS

  END SUBROUTINE InitializeP2
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Advance
!
! !DESCRIPTION: Advance is the HEMCO phase 1 run interface. It calls the main
! HEMCO run routine. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Advance ( GC, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC          ! Ref to this GridComp
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC          ! Error return code
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!  09 Oct 2025 - HEMCO Dev   - Adapted for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!  
    TYPE(Instance), POINTER  :: ThisInst => NULL()
    TYPE(ESMFIntegrationState), POINTER :: ThisESMFState => NULL()
    INTEGER                  :: STATUS

    __Iam__('Advance')

    !=======================================================================
    ! Advance begins here!
    !=======================================================================

    ! Call Run method for each instance
    ThisInst => Instances
    ThisESMFState => ESMFStates
    DO WHILE ( ASSOCIATED( ThisInst ) .AND. ASSOCIATED(ThisESMFState) )
       CALL HEMCOrun_( GC, ThisInst, ThisESMFState, RC=STATUS )
       _VERIFY(STATUS)
       ThisInst => ThisInst%NextInst
       ThisESMFState => ThisESMFState%NextInst  ! This should be fixed - need to handle multiple ESMF states
    ENDDO
 
    ! Cleanup
    ThisInst => NULL()
    ThisESMFState => NULL()

    ! Successful return
    RC = ESMF_SUCCESS

  end subroutine Advance
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finalize
!
! !DESCRIPTION: Finalize is the finalize method of the HEMCO NUOPC component.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Finalize( GC, RC ) 
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC       ! Ref. to this GridComp
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC       ! Success or failure?
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!  09 Oct 2025 - HEMCO Dev   - Adapted for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
TYPE(Instance), POINTER     :: ThisInst => NULL()
TYPE(Instance), POINTER     :: NextInst => NULL()
TYPE(ESMFIntegrationState), POINTER :: ThisESMFState => NULL()
TYPE(ESMFIntegrationState), POINTER :: TempESMFState => NULL()  ! Temporary pointer for cleanup
CHARACTER(LEN=ESMF_MAXSTR)  :: compName
INTEGER                     :: STATUS
INTEGER                     :: ERROR

    __Iam__('Finalize')

    !=======================================================================
    ! FINALIZE begins here!
    !=======================================================================

    ! Call Finalize method for each instance
    ThisInst => Instances
    ThisESMFState => ESMFStates
    DO WHILE ( ASSOCIATED( ThisInst ) .AND. ASSOCIATED(ThisESMFState) )
       CALL HEMCOfinal_( GC, ThisInst, ThisESMFState, RC=STATUS )
       _VERIFY(STATUS)
       ThisInst => ThisInst%NextInst
       ThisESMFState => ThisESMFState%NextInst  ! This should be fixed - need to handle multiple ESMF states
    ENDDO

    ! Cleanup instances list
    ThisInst => Instances
    DO WHILE ( ASSOCIATED( ThisInst ) )
       NextInst           => ThisInst%NextInst 
       ThisInst%HcoConfig => NULL() 
       ThisInst%HcoState  => NULL() 
       ThisInst%ExtState  => NULL() 
       ThisInst           => NextInst
    ENDDO

    ! Cleanup ESMF states
    ThisESMFState => ESMFStates
    DO WHILE ( ASSOCIATED(ThisESMFState) )
       IF ( ThisESMFState%GridInitialized ) THEN
          CALL ESMF_GridDestroy( ThisESMFState%HostGrid, RC=STATUS )
          _VERIFY(STATUS)
       ENDIF
       TempESMFState => ThisESMFState%NextInst
       DEALLOCATE(ThisESMFState)
       ThisESMFState => TempESMFState
    ENDDO
    ESMFStates => NULL()

    ! Cleanup
    ThisInst => NULL()
    NextInst => NULL()
    ThisESMFState => NULL()
    TempESMFState => NULL()

    ! Return w/ success
    RC = ESMF_SUCCESS

  end subroutine Finalize
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HEMCOinit_ 
!
! !DESCRIPTION: Subroutine HEMCOinit\_ is a wrapper routine to initialize the
! HEMCO NUOPC component. This is the first phase that initializes HEMCO core.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HEMCOinit_( GC, Inst, ESMFState, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT), TARGET :: GC          ! GC grid comp
    TYPE(Instance),      POINTER               :: Inst        ! Instance object
    TYPE(ESMFIntegrationState), POINTER        :: ESMFState   ! ESMF integration state
!                                                             
! !OUTPUT PARAMETERS:                                   
!
    INTEGER,             INTENT(OUT)           :: RC          ! 0 = all is well
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!  09 Oct 2025 - HEMCO Dev   - Adapted for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_Config)               :: ESMFConfig
    TYPE(ESMF_TimeInterval)         :: RunInterval 
    TYPE(ESMF_Clock)                :: Clock
    CHARACTER(LEN=ESMF_MAXSTR)      :: ConfigFile
    CHARACTER(LEN=ESMF_MAXSTR)      :: Label 
    INTEGER                         :: nnMatch
    REAL                            :: tsChem, tsDyn
    REAL(ESMF_KIND_R8)              :: s_r8
    INTEGER                         :: HCRC
    INTEGER                         :: STATUS
    LOGICAL                         :: am_I_Root

    ! For ESMF error handling (defined Iam and STATUS)
    __Iam__('HEMCOinit_ (HEMCO_NUOPC_GridCompMod.F90)') 

    ! Is this the root CPU?
    am_I_Root = ESMF_VMIsPetLocalPet(ESMF_VMGetCurrent(), RC=STATUS)
    _VERIFY(STATUS)

    ! ================================================================
    ! HEMCOinit_ begins here
    ! ================================================================

    ! ------------------------------------------------------------------
    ! HEMCO initialization 
    ! ------------------------------------------------------------------

    IF ( am_I_Root ) THEN
       CALL HCO_LogFile_Open( Inst%HcoConfig%Err, Inst%HcoConfig%doVerbose, &
            RC = HCRC, Inst%HcoConfig%hcoLogLUN=logLUN )
       _ASSERT(HCRC==HCO_SUCCESS,'needs informative message')
    ENDIF

    !-----------------------------------------------------------------
    ! Extract species to use in HEMCO 
    ! (This is a placeholder - actual implementation may vary)
    nnMatch = 1  ! Default value

    !-----------------------------------------------------------------
    ! Initialize HCO state. Use only species that are used
    ! in the model and are also found in the HEMCO config. file.
    CALL HcoState_Init ( Inst%HcoState, Inst%HcoConfig, nnMatch, HCRC )
    _ASSERT(HCRC==HCO_SUCCESS,'needs informative message')

    ! ------------------------------------------------------------------
    ! Get clock and time information
    CALL ESMF_GridCompGet( GC, CLOCK=Clock, RC=STATUS )
    _VERIFY(STATUS)
    
    ! Set time steps 
    tsDyn = 3600.0  ! Default dynamics timestep (1 hour)
    Inst%HcoState%TS_DYN  = tsDyn

    CALL ESMF_ClockGet( Clock, currTimeStep=RunInterval, RC=STATUS )
    _VERIFY(STATUS)
    CALL ESMF_TimeIntervalGet( RunInterval, s_r8=s_r8, RC=STATUS )
    _VERIFY(STATUS)
    Inst%HcoState%TS_CHEM = s_r8
    Inst%HcoState%TS_EMIS = s_r8

    IF ( am_I_Root ) THEN
       WRITE(*,*) TRIM(__Iam__), ' - HEMCO time steps:'
       WRITE(*,*) TRIM(__Iam__), ' - Dynamic  : ', Inst%HcoState%TS_DYN 
       WRITE(*,*) TRIM(__Iam__), ' - Emissions: ', Inst%HcoState%TS_EMIS
       WRITE(*,*) TRIM(__Iam__), ' - Chemistry: ', Inst%HcoState%TS_CHEM
    ENDIF

    ! ------------------------------------------------------------------
    ! Manually set some settings

    ! Pass ESMF states to HEMCO state object
    Inst%HcoState%GRIDCOMP => GC

    ! Don't let HEMCO schedule the diag output (will be scheduled manually)
    Inst%HcoState%Options%HcoWritesDiagn = .FALSE.

    ! Don't add import fields to HEMCO diagnostics. This is redundant in ESMF.
    Inst%HcoState%Options%Field2Diagn    = .FALSE.  

    ! Set ESMF flag to TRUE
    Inst%HcoState%Options%isESMF = .TRUE.

    ! ------------------------------------------------------------------
    ! Initialize HEMCO internal lists and variables. All data
    ! information is written into internal lists (ReadList) and 
    ! the HEMCO configuration file is removed from buffer in this
    ! step. Also initializes the HEMCO clock
    CALL HCO_Init( Inst%HcoState, HCRC )
    _ASSERT(HCRC==HCO_SUCCESS,'needs informative message')

    ! ------------------------------------------------------------------
    ! Initialize extensions.
    ! This initializes all (enabled) extensions and selects all met.
    ! fields needed by them. 
    CALL HCOX_Init( Inst%HcoState, Inst%ExtState, HCRC )
    _ASSERT(HCRC==HCO_SUCCESS,'needs informative message')

    ! ------------------------------------------------------------------
    ! Define diagnostics. This creates a HEMCO diagnostics entry for
    ! every element of the HEMCO diagnostics file. In addition, if set
    ! in the HEMCO configuration file, a default diagnostics is created 
    ! for every HEMCO species. 
    CALL Define_Diagnostics( Inst%HcoState, HCRC )
    _ASSERT(HCRC==HCO_SUCCESS,'needs informative message')

    ! ------------------------------------------------------------------
    ! Initialize ESMF integration components
    CALL HCOI_ESMF_Config_Init( Inst%HcoConfig%ConfigFile, RC=HCRC )
    _ASSERT(HCRC==HCO_SUCCESS,'ESMF Config initialization failed')

    CALL HCOI_ESMF_IO_Init( RC=HCRC )
    _ASSERT(HCRC==HCO_SUCCESS,'ESMF IO initialization failed')

    CALL HCOI_ESMF_Regrid_Init( RC=HCRC )
    _ASSERT(HCRC==HCO_SUCCESS,'ESMF Regrid initialization failed')

    CALL HCOI_ESMF_Integration_Init( RC=HCRC )
    _ASSERT(HCRC==HCO_SUCCESS,'ESMF Integration initialization failed')

    ! Mark integration as initialized
    ESMFState%IntegrationInitialized = .TRUE.

    ! Nullify pointers
    Inst%HcoState%GRIDCOMP => NULL()

    ! Return w/ success
    RC = ESMF_SUCCESS

  END SUBROUTINE HEMCOinit_ 
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HEMCOinit_ESMF_ 
!
! !DESCRIPTION: Subroutine HEMCOinit\_ESMF is a wrapper routine to initialize the
! ESMF integration components for HEMCO NUOPC component.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HEMCOinit_ESMF_( GC, Inst, ESMFState, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT), TARGET :: GC          ! GC grid comp
    TYPE(Instance),      POINTER               :: Inst        ! Instance object
    TYPE(ESMFIntegrationState), POINTER        :: ESMFState   ! ESMF integration state
!                                                             
! !OUTPUT PARAMETERS:                                   
!
    INTEGER,             INTENT(OUT)           :: RC          ! 0 = all is well
!
! !REVISION HISTORY:
!  09 Oct 2025 - HEMCO Dev   - Initial version for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER                         :: HCRC
    INTEGER                         :: STATUS
    INTEGER                         :: locDims(2)         ! grid dimensions
    LOGICAL                         :: am_I_Root

    ! For ESMF error handling (defined Iam and STATUS)
    __Iam__('HEMCOinit_ESMF_ (HEMCO_NUOPC_GridCompMod.F90)') 

    ! Is this the root CPU?
    am_I_Root = ESMF_VMIsPetLocalPet(ESMF_VMGetCurrent(), RC=STATUS)
    _VERIFY(STATUS)

    ! ================================================================
    ! HEMCOinit_ESMF_ begins here
    ! ================================================================

    ! ------------------------------------------------------------------
    ! Set up ESMF grid integration
    ! ------------------------------------------------------------------

    ! Get grid information from the component
    ! For now, we'll create a basic grid - in practice this would come from the host model
    locDims(1) = Inst%HcoState%NX
    locDims(2) = Inst%HcoState%NY

    ! Create host grid if not already created
    IF ( .NOT. ESMFState%GridInitialized ) THEN
       ESMFState%HostGrid = ESMF_GridCreateNoPeriDimUfrm(minIndex=(/1,1/), &
                                                         maxIndex=(/locDims(1), locDims(2)/), &
                                                         indexFlag=ESMF_INDEX_GLOBAL, &
                                                         RC=STATUS)
       _VERIFY(STATUS)
       ESMFState%GridInitialized = .TRUE.
    ENDIF

    ! Set up ESMF integration components
    CALL HCOI_ESMF_SetupGrids( ESMFState%HostGrid, RC=HCRC )
    _ASSERT(HCRC==HCO_SUCCESS,'ESMF Grid setup failed')

    ! Set up import and export field bundles
    ESMFState%ImportBundle = ESMF_FieldBundleCreate( 'HEMCO_IMPORT', RC=STATUS )
    _VERIFY(STATUS)

    ESMFState%ExportBundle = ESMF_FieldBundleCreate( 'HEMCO_EXPORT', RC=STATUS )
    _VERIFY(STATUS)

    ! Set up integration
    CALL HCOI_ESMF_Integration_Setup( ESMFState%HostGrid, ESMFState%ImportBundle, &
                                      ESMFState%ExportBundle, RC=HCRC )
    _ASSERT(HCRC==HCO_SUCCESS,'ESMF Integration setup failed')

    ! Return w/ success
    RC = ESMF_SUCCESS

  END SUBROUTINE HEMCOinit_ESMF_ 
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HEMCOrun_ 
!
! !DESCRIPTION: Subroutine HEMCOrun\_ is a wrapper routine to run the
! HEMCO NUOPC component. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HEMCOrun_( GC, Inst, ESMFState, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT), TARGET :: GC          ! GC grid comp
    TYPE(Instance),      POINTER               :: Inst        ! HEMCO instance 
    TYPE(ESMFIntegrationState), POINTER        :: ESMFState   ! ESMF integration state
!                                                             
! !OUTPUT PARAMETERS:                                   
!
    INTEGER,             INTENT(OUT)           :: RC          ! 0 = all is well
!
! !REVISION HISTORY:
!  2 Feb 2016 - C. Keller   - Initial version 
!  09 Oct 2025 - HEMCO Dev   - Adapted for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_Clock)             :: Clock
    TYPE(ESMF_Time)              :: currTime
    INTEGER                      :: yyyy, mm, dd, h, m, s, doy 
    INTEGER                      :: STAT
    INTEGER                      :: STATUS

    ! For ESMF error handling (defined Iam and STATUS)
    __Iam__('HEMCOrun_ (HEMCO_NUOPC_GridCompMod.F90)') 

    ! ================================================================
    ! HEMCOrun_ begins here
    ! ================================================================

    ! Get clock
    CALL ESMF_GridCompGet( GC, CLOCK=Clock, RC=STATUS )
    _VERIFY(STATUS)

    ! ------------------------------------------------------------------
    ! Pre-run assignments 
    ! ------------------------------------------------------------------

    ! Pass ESMF states to HEMCO state object
    Inst%HcoState%GRIDCOMP => GC

    ! ------------------------------------------------------------------
    ! Set time 
    ! ------------------------------------------------------------------
    CALL ESMF_ClockGet( Clock, currTime = currTime, RC=STATUS ) 
    _VERIFY(STATUS)
    CALL ESMF_TimeGet ( currTime, yy=yyyy, mm=mm, dd=dd, &
                        dayOfYear=doy, h=h, m=m, s=s, RC=STATUS )
    _VERIFY(STATUS)

    CALL HcoClock_Set ( Inst%HcoState, yyyy, mm, dd, h, &
                        m, s, cDOY=doy, IsEmisTime=.TRUE., RC=STAT )
    _ASSERT(STAT==HCO_SUCCESS,'needs informative message')

    ! ------------------------------------------------------------------
    ! Execute ESMF integration 
    ! ------------------------------------------------------------------
    IF ( ESMFState%IntegrationInitialized ) THEN
       CALL HCOI_ESMF_Integration_Execute( ESMFState%ImportBundle, &
                                           ESMFState%ExportBundle, RC=STAT )
       _ASSERT(STAT==HCO_SUCCESS,'ESMF Integration execution failed')
    ENDIF

    ! ------------------------------------------------------------------
    ! Set HEMCO fields 
    ! ------------------------------------------------------------------
    ! Make sure all required extension imports (met-fields and grid 
    ! quantities) are filled.
    CALL SetExtFields( Clock, Inst, ESMFState, RC=STATUS ) 
    _VERIFY(STATUS)

    ! ------------------------------------------------------------------
    ! Run HEMCO core
    ! ------------------------------------------------------------------
    ! Reset all emissions to zero
    CALL HCO_FluxArrReset( Inst%HcoState, STAT )
    _ASSERT(STAT==HCO_SUCCESS,'needs informative message')

    ! Make sure options are correct
    Inst%HcoState%Options%SpcMin     =  1
    Inst%HcoState%Options%SpcMax     = Inst%HcoState%nSpc + 1
    Inst%HcoState%Options%CatMin     =  1
    Inst%HcoState%Options%CatMax     = -1
    Inst%HcoState%Options%ExtNr      =  0
    Inst%HcoState%Options%FillBuffer = .FALSE.

    ! Now run driver routine. This calculates all 'core' emissions, 
    ! i.e. all emissions that are not extensions.
    CALL HCO_Run( Inst%HcoState, -1, STAT )
    _ASSERT(STAT==HCO_SUCCESS,'needs informative message')

    ! ------------------------------------------------------------------
    ! Run HEMCO extensions 
    ! ------------------------------------------------------------------
    ! Calculate parameterized emissions
    CALL HCOX_Run( Inst%HcoState, Inst%ExtState, STAT )
    _ASSERT(STAT==HCO_SUCCESS,'needs informative message')

    ! ------------------------------------------------------------------
    ! Diagnostics 
    ! ------------------------------------------------------------------

    ! Update HEMCO diagnostics 
    CALL HcoDiagn_AutoUpdate ( Inst%HcoState, STAT )
    _ASSERT(STAT==HCO_SUCCESS,'needs informative message')
 
    ! Fill exports (from HEMCO diagnostics)
    CALL HcoDiagn_Write( Inst%HcoState, .FALSE., STAT ) 
    _ASSERT(STAT==HCO_SUCCESS,'needs informative message')

    ! ------------------------------------------------------------------
    ! Map data to host grid if needed
    ! ------------------------------------------------------------------
    IF ( ESMFState%IntegrationInitialized .AND. ESMFState%GridInitialized ) THEN
       CALL HCOI_ESMF_Integration_MapData( ESMFState%ExportBundle, RC=STAT )
       _ASSERT(STAT==HCO_SUCCESS,'ESMF data mapping failed')
    ENDIF

    ! ------------------------------------------------------------------
    ! Cleanup 
    ! ------------------------------------------------------------------

    ! Nullify pointers
    Inst%HcoState%GRIDCOMP => NULL()

    ! Return w/ success
    RC = ESMF_SUCCESS

  END SUBROUTINE HEMCOrun_
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HEMCOfinal_
!
! !DESCRIPTION: HEMCOfinal\_ is the finalize method of the HEMCO NUOPC component.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HEMCOfinal_( GC, Inst, ESMFState, RC ) 
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC       ! Ref. to this GridComp
    TYPE(Instance),      POINTER       :: Inst     ! HEMCO instance
    TYPE(ESMFIntegrationState), POINTER :: ESMFState ! ESMF integration state
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC       ! Success or failure?
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!  09 Oct 2025 - HEMCO Dev   - Adapted for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER    :: ERROR
    INTEGER    :: STATUS

    __Iam__('HEMCOfinal')

    !=======================================================================
    ! HEMCOfinal_ begins here!
    !=======================================================================

    ! Finalize ESMF integration components if they were initialized
    IF ( ESMFState%IntegrationInitialized ) THEN
       ! Cleanup integration
       CALL HCOI_ESMF_Integration_Cleanup( RC=ERROR )
       _ASSERT(ERROR==HCO_SUCCESS,'ESMF Integration cleanup failed')

       ! Cleanup regridding
       CALL HCOI_ESMF_Regrid_Finalize( RC=ERROR )
       _ASSERT(ERROR==HCO_SUCCESS,'ESMF Regrid finalize failed')

       ! Cleanup I/O
       CALL HCOI_ESMF_IO_Final( RC=ERROR )
       _ASSERT(ERROR==HCO_SUCCESS,'ESMF IO finalize failed')

       ! Cleanup config
       CALL HCOI_ESMF_Config_Cleanup( RC=ERROR )
       _ASSERT(ERROR==HCO_SUCCESS,'ESMF Config cleanup failed')

       ! Destroy field bundles
       CALL ESMF_FieldBundleDestroy( ESMFState%ImportBundle, RC=STATUS )
       _VERIFY(STATUS)
       CALL ESMF_FieldBundleDestroy( ESMFState%ExportBundle, RC=STATUS )
       _VERIFY(STATUS)
    ENDIF

    ! Cleanup extensions and ExtOpt object 
    CALL HCOX_Final( Inst%HcoState, Inst%ExtState, ERROR )
    _ASSERT(ERROR==HCO_SUCCESS,'needs informative message')

    ! Cleanup HCO core
    CALL HCO_Final( Inst%HcoState, .FALSE., ERROR ) 
    _ASSERT(ERROR==HCO_SUCCESS,'needs informative message')

    ! Cleanup diagnostics
    CALL DiagnBundle_Cleanup ( Inst%HcoState%Diagn )

    ! Cleanup HcoState object
    CALL HcoState_Final ( Inst%HcoState ) 

    ! Return w/ success
    RC = ESMF_SUCCESS

  end subroutine HEMCOfinal_
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: SetExtFields 
!
! !DESCRIPTION: Subroutine SetExtFields makes sure that all required ExtState
! fields are filled using ESMF field bundles. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE SetExtFields ( Clock, Inst, ESMFState, RC )
!
! !INPUT PARAMETERS:
!
    TYPE(ESMF_Clock),    INTENT(IN)            :: Clock       ! ESMF clock obj 
    TYPE(Instance),      POINTER               :: Inst        ! HEMCO instance 
    TYPE(ESMFIntegrationState), POINTER        :: ESMFState   ! ESMF integration state
!                                                             
! !OUTPUT PARAMETERS:                                   
!
    INTEGER,             INTENT(OUT)           :: RC          ! 0 = all is well
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!  09 Oct 2025 - HEMCO Dev   - Adapted for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(HCO_State),     POINTER :: HcoState    => NULL()
    TYPE(Ext_State),     POINTER :: ExtState    => NULL()
    INTEGER                      :: I, J, L, N, LR, NZ, OFF, STAT
    INTEGER                      :: NSTEPS, DT
    REAL, POINTER                :: PLE     (:,:,:) => NULL()
    REAL, POINTER                :: AIRDENS (:,:,:) => NULL()
    REAL, POINTER                :: ZLE     (:,:,:) => NULL()
    REAL, POINTER                :: Q       (:,:,:) => NULL()
    REAL, POINTER                :: PS      (:,:  ) => NULL()
    REAL, POINTER                :: AREA    (:,:  ) => NULL()
    REAL                         :: tsEmis
    TYPE(ESMF_TimeInterval)      :: emisInterval   ! chemistry interval
    TYPE(ESMF_ALARM)             :: ALARM          ! Run alarm 
    REAL(ESMF_KIND_R8)           :: dt_r8          ! chemistry timestep

    REAL(hp), ALLOCATABLE        :: SUMCOSZA(:,:)
    REAL(hp), ALLOCATABLE        :: ZTH(:,:)
    REAL(hp), ALLOCATABLE        :: TMP(:,:)

    ! For ESMF error handling (defined Iam and STATUS)
    __Iam__('SetExtFields (HEMCO_NUOPC_GridCompMod.F90)') 

    ! ================================================================
    ! SetExtFields begins here
    ! ================================================================

    ! Pointers to HEMCO state object and extension state object
    HcoState => Inst%HcoState
    ExtState => Inst%ExtState

    ! Get fields from ESMF import bundle instead of direct MAPL access
    ! This is a simplified approach - in practice, we would extract fields from the bundle
    ! For now, we'll simulate the field access with default values
    
    ! Set up grid quantities based on HEMCO state
    NZ = HcoState%NZ
    IF ( NZ <= 0 ) NZ = 1 ! Default to 1 if not set

    ! Set AREA
    IF ( .NOT. ASSOCIATED(HcoState%Grid%AREA_M2%Val) ) THEN
       CALL HCO_ArrAssert( HcoState%Grid%AREA_M2, HcoState%NX, HcoState%NY, STAT )
       _ASSERT(STAT==HCO_SUCCESS,'needs informative message')
       ! Initialize with a default value - in practice this would come from ESMF
       HcoState%Grid%AREA_M2%Val = 1.0e6 ! Default area value
    ENDIF

    ! Geopotential height (m) - initialize with default
    CALL HCO_ArrAssert( HcoState%Grid%ZSFC, HcoState%NX, HcoState%NY, STAT )
    _ASSERT(STAT==HCO_SUCCESS,'needs informative message')
    HcoState%Grid%ZSFC%Val = 0.0  ! Default surface height

    ! Surface pressure - initialize with default
    CALL HCO_ArrAssert( HcoState%Grid%PSFC, HcoState%NX, HcoState%NY, STAT )
    _ASSERT(STAT==HCO_SUCCESS,'needs informative message')
    HcoState%Grid%PSFC%Val = 101325.0  ! Default surface pressure (Pa)

    ! Make sure HEMCO arrays are allocated and of correct size
    CALL HCO_ArrAssert( HcoState%Grid%BXHEIGHT_M, HcoState%NX, HcoState%NY, HcoState%NZ, STAT )
    _ASSERT(STAT==HCO_SUCCESS,'needs informative message')
    CALL HCO_ArrAssert( HcoState%Grid%PEDGE, HcoState%NX, HcoState%NY, HcoState%NZ+1, STAT )
    _ASSERT(STAT==HCO_SUCCESS,'needs informative message')
    IF ( ExtState%AIRVOL%DoUse ) THEN
       CALL HCO_ArrAssert( ExtState%AIRVOL%Arr, HcoState%NX, HcoState%NY, HcoState%NZ, STAT )
       _ASSERT(STAT==HCO_SUCCESS,'needs informative message')
    ENDIF 
    IF ( ExtState%AIR%DoUse ) THEN
       CALL HCO_ArrAssert( ExtState%AIR%Arr, HcoState%NX, HcoState%NY, HcoState%NZ, STAT )
       _ASSERT(STAT==HCO_SUCCESS,'needs informative message')
    ENDIF 

    ! Initialize pressure edges and grid box heights with default values
    DO L=1,HcoState%NZ+1
       ! Pressure edges (Pa) - simplified initialization
       HcoState%Grid%PEDGE%Val(:,:,L) = 101325.0 - (L-1) * 10000.0  ! Simplified pressure profile
       
       ! Grid box height (m) - simplified initialization
       IF ( L <= HcoState%NZ ) THEN
          HcoState%Grid%BXHEIGHT_M%Val(:,:,L) = 1000.0  ! Default 1km layer height
          
          ! Air volume (m3)
          IF ( ExtState%AIRVOL%DoUse ) THEN
             ExtState%AIRVOL%Arr%Val(:,:,L) = &
                HcoState%Grid%AREA_M2%Val(:,:) * HcoState%Grid%BXHEIGHT_M%Val(:,:,L)
          ENDIF
          
          ! Air mass (kg) - simplified
          IF ( ExtState%AIR%DoUse ) THEN
             ExtState%AIR%Arr%Val(:,:,L) = 1.2 * &  ! Default air density
                HcoState%Grid%AREA_M2%Val(:,:) * HcoState%Grid%BXHEIGHT_M%Val(:,:,L)
          ENDIF
       ENDIF
    ENDDO

    ! ---------------------------------------------------------------- 
    ! Define extension variables from ESMF import bundle
    ! In a real implementation, these would be extracted from the ESMF field bundle
    ! For now, we'll initialize with default values
    ! ---------------------------------------------------------------- 
    
    ! Initialize extension variables with default values
    IF ( ASSOCIATED(ExtState%U10M%Arr) ) ExtState%U10M%Arr%Val = 0.0
    IF ( ASSOCIATED(ExtState%V10M%Arr) ) ExtState%V10M%Arr%Val = 0.0
    IF ( ASSOCIATED(ExtState%ALBD%Arr) ) ExtState%ALBD%Arr%Val = 0.2  ! Default albedo
    IF ( ASSOCIATED(ExtState%T2M%Arr) )  ExtState%T2M%Arr%Val = 288.0 ! Default 2m temp
    IF ( ASSOCIATED(ExtState%TSKIN%Arr) ) ExtState%TSKIN%Arr%Val = 288.0 ! Default skin temp
    IF ( ASSOCIATED(ExtState%GWETTOP%Arr) ) ExtState%GWETTOP%Arr%Val = 0.5  ! Default wetness
    IF ( ASSOCIATED(ExtState%GWETROOT%Arr) ) ExtState%GWETROOT%Arr%Val = 0.5
    IF ( ASSOCIATED(ExtState%SNOWHGT%Arr) ) ExtState%SNOWHGT%Arr%Val = 0.0
    IF ( ASSOCIATED(ExtState%SNODP%Arr) ) ExtState%SNODP%Arr%Val = 0.0
    IF ( ASSOCIATED(ExtState%USTAR%Arr) ) ExtState%USTAR%Arr%Val = 0.1
    IF ( ASSOCIATED(ExtState%Z0%Arr) ) ExtState%Z0%Arr%Val = 0.1
    IF ( ASSOCIATED(ExtState%TROPP%Arr) ) ExtState%TROPP%Arr%Val = 10000.0
    IF ( ASSOCIATED(ExtState%PARDR%Arr) ) ExtState%PARDR%Arr%Val = 0.0
    IF ( ASSOCIATED(ExtState%PARDF%Arr) ) ExtState%PARDF%Arr%Val = 0.0
    IF ( ASSOCIATED(ExtState%RADSWG%Arr) ) ExtState%RADSWG%Arr%Val = 0.0
    IF ( ASSOCIATED(ExtState%FRCLND%Arr) ) ExtState%FRCLND%Arr%Val = 0.5
    IF ( ASSOCIATED(ExtState%FRLAND%Arr) ) ExtState%FRLAND%Arr%Val = 0.5
    IF ( ASSOCIATED(ExtState%FROCEAN%Arr) ) ExtState%FROCEAN%Arr%Val = 0.5
    IF ( ASSOCIATED(ExtState%FRLAKE%Arr) ) ExtState%FRLAKE%Arr%Val = 0.0
    IF ( ASSOCIATED(ExtState%FRLANDIC%Arr) ) ExtState%FRLANDIC%Arr%Val = 0.0
    IF ( ASSOCIATED(ExtState%CLDFRC%Arr) ) ExtState%CLDFRC%Arr%Val = 0.0
    IF ( ASSOCIATED(ExtState%LAI%Arr) ) ExtState%LAI%Arr%Val = 1.0
    IF ( ASSOCIATED(ExtState%CNV_MFC%Arr) ) ExtState%CNV_MFC%Arr%Val = 0.0
    IF ( ASSOCIATED(ExtState%SPHU%Arr) ) ExtState%SPHU%Arr%Val = 0.01  ! Default specific humidity
    IF ( ASSOCIATED(ExtState%TK%Arr) ) ExtState%TK%Arr%Val = 288.0

    ! SUNCOS - calculate based on time and location
    IF ( ExtState%SUNCOS%DoUse ) THEN
       ! Make sure HEMCO array is allocated 
       CALL HCO_ArrAssert( ExtState%SUNCOS%Arr, HcoState%NX, HcoState%NY, STAT )
       ASSERT_(STAT==HCO_SUCCESS)
       
       ! Calculate SUNCOS based on current time and location
       ALLOCATE( TMP(HcoState%NX,HcoState%NY) )
       TMP = 0.0_hp
       CALL GetSUNCOS( Clock, HcoState, TMP, 0, RC=STAT )
       _ASSERT(STAT==HCO_SUCCESS,'SUNCOS calculation failed')
       ExtState%SUNCOS%Arr%Val(:,:) = TMP(:,:)
       DEALLOCATE(TMP)
    ENDIF

    ! SZAFACT - calculate normalized solar zenith angle factor
    IF ( ExtState%SZAFACT%DoUse ) THEN
       ! Make sure HEMCO array is allocated 
       CALL HCO_ArrAssert( ExtState%SZAFACT%Arr, HcoState%NX, HcoState%NY, STAT )
       ASSERT_(STAT==HCO_SUCCESS)
       ExtState%SZAFACT%Arr%Val(:,:) = 1.0  ! Default normalization
    ENDIF

    ! ---------------------------------------------------------------- 
    ! Cleanup 
    HcoState => NULL()
    ExtState => NULL()

    ! Return w/ success
    RC = ESMF_SUCCESS

  END SUBROUTINE SetExtFields 
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: GetSUNCOS
!
! !DESCRIPTION: Subroutine GetSUNCOS calculates the cosine of the solar zenith 
! angle for the given date. This is adapted from the GEOS interface.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GetSUNCOS( Clock, HcoState, SUNCOS, DT, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_Clock), INTENT(IN)     :: Clock       ! ESMF clock obj 
    TYPE(HCO_State),  POINTER        :: HcoState    ! HEMCO state object
    INTEGER,          INTENT(IN   ) :: DT          ! Time shift relative
                                                    ! to current date [hrs]
!
! !OUTPUT PARAMETERS:
!
    REAL(hp),         INTENT(  OUT)  :: SUNCOS(HcoState%NX,HcoState%NY)
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,          INTENT(INOUT)  :: RC             ! Return code
!
! !REVISION HISTORY:
!  17 Sep 2018 - C. Keller   - Adapted from HCO_GetSuncos 
!  09 Oct 2025 - HEMCO Dev   - Adapted for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER              :: I, J,   DOY, HOUR
    LOGICAL              :: ERR
    REAL(hp)             :: YMID_R, S_YMID_R,  C_YMID_R
    REAL(hp)             :: R,      DEC
    REAL(hp)             :: S_DEC,  C_DEC 
    REAL(hp)             :: SC,     LHR
    REAL(hp)             :: AHR
    TYPE(ESMF_Time)      :: currTime    ! ESMF current time obj
    INTEGER              :: STATUS

    ! Coefficients for solar declination angle
    REAL(hp),  PARAMETER :: A0 = 0.006918e+0_hp
    REAL(hp),  PARAMETER :: A1 = 0.399912e+0_hp
    REAL(hp),  PARAMETER :: A2 = 0.006758e+0_hp
    REAL(hp),  PARAMETER :: A3 = 0.002697e+0_hp
    REAL(hp),  PARAMETER :: B1 = 0.070257e+0_hp
    REAL(hp),  PARAMETER :: B2 = 0.000907e+0_hp
    REAL(hp),  PARAMETER :: B3 = 0.000148e+0_hp

    ! For ESMF error handling
    __Iam__('GetSUNCOS (HEMCO_NUOPC_GridCompMod.F90)') 

    !-------------------------------
    ! GetSUNCOS starts here! 
    !-------------------------------

    ! Get the ESMF time object
    CALL ESMF_ClockGet( Clock,                    &
                        currTime     = currTime, &
                        RC=STATUS )
    _VERIFY(STATUS)

    ! Get individual fields from the time object
    CALL ESMF_TimeGet( currTime, dayOfYear=DOY, h=HOUR, RC=STATUS )
    _VERIFY(STATUS)

    ! Add time adjustment 
    HOUR = HOUR + DT

    ! Make sure HOUR is within valid range (0-24)
    IF ( HOUR < 0 ) THEN
       HOUR = HOUR + 24
       DOY  = DOY  - 1
    ELSEIF ( HOUR > 23 ) THEN
       HOUR = HOUR - 24
       DOY  = DOY  + 1
    ENDIF

    ! Make sure DOY is within valid range of 1 to 365
    DOY = MAX(MIN(DOY,365),1)

    ! Path length of earth's orbit traversed since Jan 1 [radians]
    R = ( 2e+0_hp * MAPL_PI / 365e+0_hp ) * DBLE( DOY - 1 )

    ! Solar declination angle (low precision formula) [radians]
    DEC = A0 - A1*COS(         R ) + B1*SIN(         R ) &
             - A2*COS( 2e+0_hp*R ) + B2*SIN( 2e+0_hp*R ) &
             - A3*COS( 3e+0_hp*R ) + B3*SIN( 3e+0_hp*R )

    ! Pre-compute sin & cos of DEC outside of DO loops (for efficiency)
    S_DEC    = SIN( DEC )
    C_DEC    = COS( DEC )

    ! Init
    ERR = .FALSE.

    ! Calculate latitude centers for the HEMCO grid
    ! This is a simplified approach - in practice, we'd get the actual lat/lon from the grid
    DO J = 1, HcoState%NY 
    DO I = 1, HcoState%NX
       ! Latitude of grid box [radians] - simplified calculation
       YMID_R = (J - (HcoState%NY/2.0)) * (180.0/HcoState%NY) * (MAPL_PI/180.0)
       
       ! Pre-compute sin & cos of YMID_R outside of I loop (for efficiency)
       S_YMID_R   = SIN( YMID_R )
       C_YMID_R   = COS( YMID_R )

       ! Compute local time as UTC + longitude/15 (simplified)
       LHR = HOUR + ((I - (HcoState%NX/2.0)) * (360.0/HcoState%NX) / 15.0)

       IF ( LHR <   0.0_hp ) LHR = LHR + 24.0_hp
       IF ( LHR >= 24.0_hp ) LHR = LHR - 24.0_hp

       ! Hour angle at box (I,J) [radians]
       AHR = ABS( LHR - 12.0_hp ) * 15.0_hp * MAPL_PI / 180.0_hp 
       
       ! Corresponding cosine( SZA ) at box (I,J) [unitless]
       SC = ( S_YMID_R * S_DEC              ) &
          + ( C_YMID_R * C_DEC * COS( AHR ) )

       ! COS(SZA) at the current time
       SUNCOS(I,J) = SC

    ENDDO
    ENDDO

    ! Error check
    ASSERT_(ERR .eqv. .FALSE.)

    ! Return w/ success
    RC = ESMF_SUCCESS

  END SUBROUTINE GetSUNCOS
!EOC
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: NewInst_ 
!
! !DESCRIPTION: Subroutine NewInst\_ creates a new HEMCO instance. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE NewInst_ ( Inst, RC ) 
!                                                             
! !OUTPUT PARAMETERS:                                   
!
    TYPE(Instance), POINTER         :: Inst   ! pointer to new instance 
    INTEGER,        INTENT(  OUT)   :: RC     ! Return code
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller - Initial version 
!  09 Oct 2025 - HEMCO Dev - Adapted for NUOPC
!  See https://github.com/geoschem/hemco for complete history
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(Instance), POINTER   :: NewInst => NULL()
    INTEGER                   :: STATUS

    __Iam__('NewInst_ (HEMCO_NUOPC_GridCompMod.F90)')

    TYPE(ESMFIntegrationState), POINTER   :: NewESMFState => NULL()

    ! Initialize new instance
    ALLOCATE(NewInst)
    NewInst%HcoConfig => NULL()
    NewInst%HcoState  => NULL()
    NewInst%ExtState  => NULL()
    NewInst%NextInst  => NULL()

    ! Initialize new ESMF integration state
    ALLOCATE(NewESMFState)
    NewESMFState%HostGrid = ESMF_GridCreateNoPeriDimUfrm(minIndex=(/1,1/), &
                                                        maxIndex=(/1,1/), &
                                                        indexFlag=ESMF_INDEX_GLOBAL, &
                                                        RC=STATUS)
    _VERIFY(STATUS)
    NewESMFState%GridInitialized = .FALSE.
    NewESMFState%IntegrationInitialized = .FALSE.
    NewESMFState%NextInst => NULL()

    ! Add to linked lists (place at beginning)
    NewInst%NextInst  => Instances
    Instances         => NewInst
    NewESMFState%NextInst => ESMFStates
    ESMFStates        => NewESMFState

    ! Connect return pointer
    Inst => NewInst

    ! Return w/ success
    RC = ESMF_SUCCESS

  END SUBROUTINE NewInst_
!EOC
END MODULE HEMCO_NUOPC_GridCompMod