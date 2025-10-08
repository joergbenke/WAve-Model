MODULE WAM_INITIAL_MODULE

  ! ---------------------------------------------------------------------------- !
  !                                                                              !
  !   THIS MODULE CONTAINS ALL DATA WHICH ARE USED BY THE PRESET PROGRAM.        !
  !   ALL PROCEDURES ARE INCLUDED TO COMPUTE THE WAM MODEL COLDSTART FILE,       !
  !   TO COMPUTE SAVE AND CONNECT RESTART FILES TO THE WAM MODEL.                !
  !                                                                              !
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     A.  EXTERNALS.                                                           !
  !                                                                              !
  ! ---------------------------------------------------------------------------- !

  USE WAM_GENERAL_MODULE,    ONLY:  &
       &       ABORT1,                   &  !! TERMINATES PROCESSING.
       &       INCDATE                      !! UPDATES DATE/TIME GROUP.

  USE WAM_RESTART_MODULE,    ONLY:  &
       &       CONNECT_RESTART              !! CONNECT RESTART FILE TO THE WAM MODEL.

  USE WAM_COLDSTART_MODULE,  ONLY:  &
       &       PREPARE_COLDSTART            !! PREPARES COLDSTART FIELDS FOR WAMODEL.

  USE WAM_ICE_MODULE,        ONLY:  &
       &       PUT_ICE,                  &  !! PUTS ICE INDICATOR INTO DATA FILED.
       &       GET_ICE                      !! GETS A NEW ICE FIELD.

  USE WAM_TOPO_MODULE,       ONLY:  &
       &       WAM_TOPO,                 &  !! READ AND PREPARE DEPTHS.
       &       PUT_DRY,                  &  !! PUTS DRY INDICATOR INTO DATA FILED.
       &       FIND_DRY_POINTS              !! FINDS DRY POINTS.

  USE WAM_CURRENT_MODULE,    ONLY:  &
       &       WAM_CURRENT                  !! READ AND PREPARE CURRENTS.

  USE WAM_SOURCE_MODULE,     ONLY:  &
       &       MAKE_SHALLOW_SNL             !! COMPUTE THE NONLINEAR TRANSFER FUNCTION
  !! COEFFICIENTS FOR SHALLOW WATER.
  use wam_special_module,    only:  &
       &       chready                      !! wait for wind/ice files 

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     B. VARIABLES FROM OTHER MODULES.                                         !
  !                                                                              !
  ! ---------------------------------------------------------------------------- !

  USE WAM_GENERAL_MODULE, ONLY: DEG, ROAIR

  USE WAM_FRE_DIR_MODULE, ONLY: KL, ML, FR, CO, TH, DELTH, DELTR, COSTH, SINTH,  &
       &                             GOM, C, INV_LOG_CO,                              &
       &                             DF, DF_FR2, DF_FR,                               &
       &                             DFIM, DFIMOFR, DFIM_FR2, DFIM_FR, FR5, FRM5,     &
       &                             RHOWG_DFIM,                                      &
       &                             FMIN, MO_TAIL, MM1_TAIL, MP1_TAIL, MP2_TAIL,     &
       &                             MPM, KPM, JXO, JYO

  USE WAM_GRID_MODULE,    ONLY: HEADER, NX, NY, NSEA, NLON_RG,                   &
       &                             XDELLA, DELLAM, XDELLO, ZDELLO, DELPHI,          &
       &                             AMOWEP, AMOSOP, AMOEAP, AMONOP, IPER,            &
       &                             SINPH, COSPH, DEPTH_B, KLAT, KLON, WLAT,         &
       &                             IXLG, KXLT, KFROMIJ, L_S_MASK, ONE_POINT,        &
       &                             REDUCED_GRID, OBSLAT, OBSLON

  USE WAM_NEST_MODULE,    ONLY: N_NEST, MAX_NEST, N_NAME, n_code,                &
       NBOUNC, IJARC, BLATC, BLNGC, DLAMAC, DPHIAC,     &
       &                             N_SOUTH, N_NORTH, N_EAST, N_WEST, N_ZDEL,        &
       &                             NBINP, NBOUNF, C_NAME, BLNGF, BLATF, IJARF,      &
       &                             IBFL, IBFR, BFW

  USE WAM_MODEL_MODULE,   ONLY: FL3, U10, UDIR, TAUW, USTAR, Z0, ROAIRN, WSTAR,  &
       &                             DEPTH, INDEP, U, V

  USE WAM_TIMOPT_MODULE,  ONLY: CDATEA, CDATEE, CDTPRO, IDELPRO, IDELT, IDEL_WAM,&
       &                             COLDSTART, SPHERICAL_RUN, SHALLOW_RUN,           &
       &                             REFRACTION_C_RUN, L_OBSTRUCTION,                 &
       &                             CDATEWO, ifcst,                                  &
       &                             CDTA, TOPO_RUN, CD_TOPO_NEW,                     &
       &                             CDCA, CURRENT_RUN, CD_CURR_NEW 

  USE WAM_TABLES_MODULE,  ONLY: NDEPTH, DEPTHA, DEPTHD, DEPTHE,                  &
       &                             FLMINFR, TCGOND, TFAK, TSIHKD, TFAC_ST, T_TAIL,  &
       &                             JUMAX, DELU

  USE WAM_FILE_MODULE,    ONLY: FILE03, IU06, ITEST, IU07, FILE07, FILE08, FILE09

  USE WAM_WIND_MODULE,    ONLY: IDELWI, IDELWO

  USE WAM_TOPO_MODULE,    ONLY: IDELTI, IDELTO

  USE WAM_CURRENT_MODULE, ONLY: IDELCI, IDELCO

  USE WAM_PROPAGATION_MODULE, ONLY: NADV, DCO, DPSN

  USE WAM_ICE_MODULE,         ONLY: ICE_RUN

  use wam_mpi_module,     only: nijs, nijl, ninf, nsup
  use wam_special_module, only: readyf

  use netcdf


  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     C. MODULE VARIABLES.                                                     !
  !                                                                              !
  ! ---------------------------------------------------------------------------- !

  IMPLICIT NONE
  PRIVATE

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     D.  PUBLIC INTERFACES.                                                   !
  !                                                                              !
  ! ---------------------------------------------------------------------------- !

  INTERFACE PREPARE_START               !! PREPARES START FIELDS FOR WAMODEL.
     MODULE PROCEDURE PREPARE_START
  END INTERFACE PREPARE_START
  PUBLIC PREPARE_START

  INTERFACE READ_PREPROC_FILE           !! READS PREPROC OUTPUT FILE.
     MODULE PROCEDURE READ_PREPROC_FILE
  END INTERFACE READ_PREPROC_FILE
  PUBLIC READ_PREPROC_FILE

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     E.  PRIVATE INTERFACES.                                                  !
  !                                                                              !
  ! ---------------------------------------------------------------------------- !

  INTERFACE PREPARE_FIRST_DEPTH             !! PREPARES START FIELDS FOR WAMODEL.
     MODULE PROCEDURE PREPARE_FIRST_DEPTH
  END INTERFACE PREPARE_FIRST_DEPTH
  PRIVATE PREPARE_FIRST_DEPTH

  INTERFACE PREPARE_FIRST_CURRENT             !! PREPARES START FIELDS FOR WAMODEL.
     MODULE PROCEDURE PREPARE_FIRST_CURRENT
  END INTERFACE PREPARE_FIRST_CURRENT
  PRIVATE PREPARE_FIRST_CURRENT

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     F. PUBLIC MODULE PROCEDURES.                                             !
  !                                                                              !
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE PREPARE_START
    USE WAM_OASIS_MODULE,     ONLY: USE_OASIS_ELEV_IN,USE_OASIS_CURR_IN,	&  !! ModR04: Include OASIS
         use_oasis_ice_in,WAM_OASIS_REC_ICE,	&
         USE_OASIS_WIND_IN,WAM_OASIS_REC_ATMO,	&
         WAM_OASIS_REC_TOPO,WAM_OASIS_REC_CURRENT

    INTEGER :: LEN
    LOGICAL :: ERROR,gotfield  !! ModR04: new gotfield

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. ALLOCATE ARRAYS.                                                      !
    !        ----------------                                                      !

    IF (ALLOCATED(U10   )) DEALLOCATE(U10  )
    IF (ALLOCATED(UDIR  )) DEALLOCATE(UDIR )
    IF (ALLOCATED(USTAR )) DEALLOCATE(USTAR)
    IF (ALLOCATED(Z0    )) DEALLOCATE(Z0   )
    IF (ALLOCATED(TAUW  )) DEALLOCATE(TAUW )
    IF (ALLOCATED(ROAIRN)) DEALLOCATE(ROAIRN)
    IF (ALLOCATED(WSTAR )) DEALLOCATE(WSTAR)
    IF (ALLOCATED(DEPTH )) DEALLOCATE(DEPTH)
    IF (ALLOCATED(INDEP )) DEALLOCATE(INDEP)
    IF (ALLOCATED(FL3   )) DEALLOCATE(FL3  )
    IF (ALLOCATED(U     )) DEALLOCATE(U    )
    IF (ALLOCATED(V     )) DEALLOCATE(V    )

    ALLOCATE (FL3(nijs:nijl, 1:KL,1:ML))

    ALLOCATE (U10(nijs:nijl))
    allocate (udir(nijs:nijl))
    allocate (ustar(nijs:nijl))
    allocate (z0(nijs:nijl))
    allocate (tauw(nijs:nijl))
    ALLOCATE (ROAIRN(NIJS:NIJL))
    ALLOCATE (WSTAR(NIJS:NIJL))

    ALLOCATE(DEPTH(NINF:NSUP))
    ALLOCATE(INDEP(NINF:NSUP))
    ALLOCATE(U    (NINF:NSUP))
    ALLOCATE(V    (NINF:NSUP))

    USTAR = 0.
    TAUW  = 0.
    Z0    = 0.
    ROAIRN=ROAIR
    WSTAR = 0.
    U     = 0.
    V     = 0.
    DEPTH = 999.0

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. COSINE OF LATITUDE FACTORS (IF SPHERICAL GRID).                       !
    !        -----------------------------------------------                       !

    IF (SPHERICAL_RUN .AND. .NOT.ONE_POINT) THEN
       IF (.NOT.ALLOCATED(DCO))  ALLOCATE(DCO(NINF:NSUP))
       IF (.NOT.ALLOCATED(DPSN)) ALLOCATE(DPSN(nijs:nijl,2))

       DCO(ninf:nsup) = 1./COSPH(KFROMIJ(ninf:nsup))           !! COSINE OF LATITUDE.
       DPSN = 1.
       WHERE (KLAT(nijs:nijl,1,1).NE.ninf-1) DPSN(nijs:nijl,1) =                   &
            &         DCO(nijs:nijl)/DCO(KLAT(nijs:nijl,1,1))         !! COS PHI FACTOR SOUTH.
       WHERE (KLAT(nijs:nijl,2,1).NE.ninf-1) DPSN(nijs:nijl,2) =                   &
            &         DCO(nijs:nijl)/DCO(KLAT(nijs:nijl,2,1))         !! COS PHI FACTOR NORTH.
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. GENERATE START FIELDS OR READ RESTART FILE.                           !
    !        -------------------------------------------                           !

    !IF(USE_OASIS_WIND_IN)CALL WAM_OASIS_REC_ATMO(cdatea)              !! ModR04: Include OASIS !! ModR07: Removed
    !IF(USE_OASIS_ELEV_IN)CALL WAM_OASIS_REC_TOPO(cdatea,gotfield)     !! ModR04, ModR07
    !IF(USE_OASIS_CURR_IN)CALL WAM_OASIS_REC_CURRENT(cdatea,gotfield)  !! ModR04, ModR07

    IF (COLDSTART) THEN
       CALL PREPARE_COLDSTART
       IF (ITEST.GE.2) WRITE(IU06,*) '    SUB. PREPARE_START: PREPARE_COLDSTART DONE'
    ELSE
       CALL CONNECT_RESTART
       IF (ITEST.GE.2) WRITE(IU06,*) '    SUB. PREPARE_START: CONNECT_RESTART DONE'
    END IF

    ! ---------------------------------------------------------------------------- !
    !
    !     4. ICE INFORMATION.
    !        ----------------

    ICE_RUN = .FALSE.
    LEN = LEN_TRIM(FILE03)
    IF (LEN.GT.0) THEN
       INQUIRE (FILE=FILE03(1:LEN), EXIST=ICE_RUN)
       IF (.NOT.ICE_RUN) THEN
          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +        WARNING ERROR SUB. PREPARE_START.         +'
          WRITE (IU06,*) ' +        =================================         +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' + THE ICE INPUT FILE DEFINED IN THE USER INPUT     +'
          WRITE (IU06,*) ' + DOES NOT EXIST.                                  +'
          WRITE (IU06,*) ' + FILENAME IS FIL03 = ', TRIM(FILE03)
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +               MODEL CONTINUES                    +'
          WRITE (IU06,*) ' +                 WITHOUT ICE                      +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
       END IF
    END IF

    if (readyf) then
       call chready (ifcst)  !! wait for first ice/wind-file
       write (iu06,*) ' +++ WAM waits for wind ready files'
    else
       write (iu06,*) ' +++ WAM runs without wind ready files'
    endif

    !IF (ICE_RUN.or.use_oasis_ice_in) THEN !! ModR04: Include OASIS
    IF (ICE_RUN .and. .not.use_oasis_ice_in) THEN !! ModR07: Include OASIS
       IF (ICE_RUN) THEN
          CALL GET_ICE
          IF (ITEST.GE.2) WRITE(IU06,*) '    SUB. PREPARE_START: GET_ICE DONE'
       END IF
       !IF (use_oasis_ice_in) call Wam_oasis_rec_ice(cdatea) !! ModR04: Include OASIS !! ModR07: Removed

       CALL PUT_ICE (FL3, 0.)
       IF (ITEST.GE.2) WRITE(IU06,*) '    SUB. PREPARE_START: ICE INSERTED'
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     5. PREPARE FIRST DEPTH FIELD AND DATE FOR NEXT DEPTH FIELD.              !
    !        --------------------------------------------------------              !

    CALL PREPARE_FIRST_DEPTH
    IF (ITEST.GE.2) THEN
       WRITE (IU06,*) '    SUB. PREPARE_START: PREPARE_FIRST_DEPTH DONE '
    END IF

    IF (SHALLOW_RUN) THEN
       CALL MAKE_SHALLOW_SNL (DEPTH(nijs:nijl))
       IF (ITEST.GE.2) THEN
          WRITE (IU06,*) '    SUB. PREPARE_START: MAKE_SHALLOW_SNL DONE '
       END IF
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     6. PREPARE FIRST CURRENT FIELD AND DATE FOR NEXT CURRENT FIELD.          !
    !        ------------------------------------------------------------          !

    CALL PREPARE_FIRST_CURRENT
    IF (ITEST.GE.2) THEN
       WRITE (IU06,*) '    SUB. PREPARE_START: PREPARE_FIRST_CURRENT DONE '
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     7. NUMBER OF PROPAGATION TIME STEPS PER WAM MODEL CALL.                  !
    !        ----------------------------------------------------                  !

    IF (ONE_POINT) THEN
       IDELPRO = IDELT
    END IF

    NADV = MAX(IDELWI, IDELCI, IDELTI, IDELPRO, IDELT)

    ERROR = (MOD(NADV,IDELPRO).NE.0) .OR. (MOD(NADV,IDELT ).NE.0)
    IF (IDELWI.GT.0)   ERROR = ERROR .OR. (MOD(NADV,IDELWI).NE.0)
    IF (IDELTI.GT.0)   ERROR = ERROR .OR. (MOD(NADV,IDELTI).NE.0)
    IF (IDELCI.GT.0)   ERROR = ERROR .OR. (MOD(NADV,IDELCI).NE.0)


    IF (ERROR) THEN
       WRITE(IU06,*) '*************************************************'
       WRITE(IU06,*) '*                                               *'
       WRITE(IU06,*) '*     FATAL ERROR IN SUB. PREPARE_START         *'
       WRITE(IU06,*) '*     =================================         *'
       WRITE(IU06,*) '*                                               *'
       WRITE(IU06,*) '* TIMESTEPS DO NOT SYNCRONIZE AT THERE MAXIMUM. *'
       WRITE(IU06,*) '* WIND INPUT TIMESTEP      : ', IDELWI
       WRITE(IU06,*) '* DEPTH INPUT TIMESTEP     : ', IDELTI
       WRITE(IU06,*) '* CURRENT INPUT TIMESTEP   : ', IDELCI
       WRITE(IU06,*) '* SOURCE FUNCTION TIMESTEP : ', IDELT
       WRITE(IU06,*) '* PROPAGATION TIMESTEP     : ', IDELPRO
       WRITE(IU06,*) '*                                               *'
       WRITE(IU06,*) '*        PROGRAM ABORTS  PROGRAM ABORTS         *'
       WRITE(IU06,*) '*                                               *'
       WRITE(IU06,*) '*************************************************'
       CALL ABORT1
    END IF

    NADV = MAX(NADV/IDELPRO,1)
    IDEL_WAM = NADV*IDELPRO

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     8. INITIALIZE DATE FOR NEXT WIND FIELD.                                  !
    !        ------------------------------------                                  !

    CDATEWO = CDTPRO
    IF (IDELT.GT.IDELWO) THEN
       WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
       WRITE (IU06,*) ' +                                                  +'
       WRITE (IU06,*) ' +        WARNING ERROR SUB. PREPARE_START.         +'
       WRITE (IU06,*) ' +        =================================         +'
       WRITE (IU06,*) ' +                                                  +'
       WRITE (IU06,*) ' + WIND OUTPUT TIMESTEP     : ', IDELWO
       WRITE (IU06,*) ' +    IS LESS THAN                                  +'
       WRITE (IU06,*) ' + SOURCE FUNCTION TIMESTEP : ', IDELT
       WRITE (IU06,*) ' +                                                  +'
       WRITE (IU06,*) ' +            MODEL CONTINUES WITH                  +'
       WRITE (IU06,*) ' + WIND OUTPUT TIMESTEP = SOURECE FUNCTION TIMESTEP.+'
       WRITE (IU06,*) ' +                                                  +'
       WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
       IDELWO = IDELT
    END IF
    IF (IDELT.LT.IDELWO) CALL INCDATE(CDATEWO,IDELWO/2)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     9. INITIALIZE DATE FOR NEXT DEPTH FIELD.                                 !
    !        -------------------------------------                                 !

    IF (USE_OASIS_ELEV_IN .AND. .NOT.COLDSTART) THEN !! ModR07: Update OASIS
       CD_TOPO_NEW=CDATEA
    ELSE IF (IDELTI.LE.0 .OR. .NOT.TOPO_RUN) THEN
       CD_TOPO_NEW = '99991231235900'
       TOPO_RUN = .FALSE.
       IDELTI = 0
       IDELTO = 0
    ELSE
       IF (.NOT.USE_OASIS_ELEV_IN) CD_TOPO_NEW = CDTPRO       !! ModR04: Only without OASIS
       IF (IDELPRO.GT.IDELTO) THEN
          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +        WARNING ERROR SUB. PREPARE_START.         +'
          WRITE (IU06,*) ' +        =================================         +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' + TOPO OUTPUT TIMESTEP  : ', IDELTO
          WRITE (IU06,*) ' +    IS LESS THAN                                  +'
          WRITE (IU06,*) ' + PROPAGATION TIMESTEP  : ', IDELPRO
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +            MODEL CONTINUES WITH                  +'
          WRITE (IU06,*) ' +   TOPO OUTPUT TIMESTEP = PROPAGATION TIMESTEP.   +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          IDELTO = IDELPRO
       END IF
       IF (.NOT.USE_OASIS_ELEV_IN .AND. IDELPRO.LT.IDELTO) &  !! ModR04: Only without OASIS
            CALL INCDATE(CD_TOPO_NEW,IDELTO/2)
    END IF
    IF (ITEST.GE.3) THEN
       WRITE (IU06,*) '        NEXT DEPTH DATE IS  CD_TOPO_NEW = ', CD_TOPO_NEW
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !    10. INITIALIZE DATE FOR NEXT CURRENT FIELD.                               !
    !        ---------------------------------------                               !

    IF (IDELCI.LE.0 .OR. .NOT.CURRENT_RUN) THEN
       CD_CURR_NEW = '99991231235900'
       CURRENT_RUN = .FALSE.
       IDELCI = 0
       IDELCO = 0
    ELSE
       IF (.NOT.USE_OASIS_CURR_IN) CD_CURR_NEW = CDTPRO       !! ModR04: Only without OASIS
       IF (IDELPRO.GT.IDELCO) THEN
          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +        WARNING ERROR SUB. PREPARE_START.         +'
          WRITE (IU06,*) ' +        =================================         +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' + CURRENT OUTPUT TIMESTEP  : ', IDELCO
          WRITE (IU06,*) ' +    IS LESS THAN                                  +'
          WRITE (IU06,*) ' + PROPAGATION TIMESTEP     : ', IDELPRO
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +            MODEL CONTINUES WITH                  +'
          WRITE (IU06,*) ' + CURRENT OUTPUT TIMESTEP = PROPAGATION TIMESTEP.  +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          IDELCO = IDELPRO
       END IF
       IF (.NOT.USE_OASIS_CURR_IN .AND. IDELPRO.LT.IDELCO) &  !! ModR04: Only without OASIS
            CALL INCDATE(CD_CURR_NEW ,IDELCO/2)
    END IF
    IF (ITEST.GE.3) THEN
       WRITE (IU06,*) '        NEXT CURRENT DATE IS CD_CURR_NEW = ', CD_CURR_NEW
    END IF

  END SUBROUTINE PREPARE_START



  ! **************************************************************************** !                                                                      

  SUBROUTINE check(status, var_name)
    use iso_fortran_env, only: stderr => error_unit, &
         stdout => output_unit

    implicit none

    INTEGER, intent (in) :: status
    character(len = *) :: var_name

    IF(status /= NF90_NOERR) THEN
       write(stderr, *) "Warning (",trim(var_name), "): ", TRIM(NF90_STRERROR(status))
       !       STOP "Error while netCDF operation ... Aborting!"                                                                                        
    END IF
  END SUBROUTINE check


  subroutine error_msg_allocation( error_msg )
    character(len = *) :: error_msg
    integer            :: status = 0
    if (status /= 0) then
       write(*, *) "Error while alllocating the array ", error_msg, ". STATUS =", status
    end if
  end subroutine error_msg_allocation


  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE READ_PREPROC_FILE

    USE WAM_OASIS_MODULE,     ONLY: USE_OASIS,WAM_OASIS_WRITE_GRID !! ModR04: Include OASIS
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !   READ_PREPROC_FILE -  READ OUTPUT FILE FROM PREPROC.                        !
    !                                                                              !
    !     H. GUNTHER      GKSS/ECMWF      MAY 1990                                 !
    !     H. GUNTHER      GKSS        OCTOBER 2000  FT90                           !
    !     J. BENKE        JSC       SEPTEMBER 2025                                 !
    !     PURPOSE.                                                                 !
    !     --------                                                                 !
    !                                                                              !
    !       NETCDF INPUT OF NETCDF PREPROC (GRIDINFO) OUTPUT.                      !
    !                                                                              ! 
    !     METHOD.                                                                  !
    !     -------                                                                  !
    !                                                                              !
    !       AFTER 2025:  NETCDF READ FROM FILE07                                   !
    !       BEFORE 2025: UNFORMATTED BINARY READ FROM FILE07.                      !
    !                                                                              !  
    !     REFERENCE.                                                               !
    !     ----------                                                               !
    !                                                                              !
    !       NONE.                                                                  !
    !                                                                              !

    use iso_fortran_env, only: stdout => output_unit, stderr => error_unit

    implicit none


    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     LOCAL VARIABLES.                                                         !
    !     ----------------                                                         !

    LOGICAL  :: L_OBSTRUCTION_T
    logical  :: DEBUG = .true.
    
    character, dimension(200) :: FILE07_NC
    character(len = 50), allocatable, dimension(:) :: name_of_dim
    character(len = 50), allocatable, dimension(:) :: name_of_var

    INTEGER :: IOS, LEN, I = 1
    integer :: n_dims, n_vars, n_vars_fixed, n_attrs, k_un
    integer :: ncid, varid, status 
    integer :: dimid_n_nest, dimid_ml, dimid_kl, dimid_max_nbounc, dimid_nbounf
    integer :: dimid_nx, dimid_ny, dimid_nsea, dimid_jumax, dimid_ndepth
    integer :: iper_tmp, one_point_tmp, reduced_grid_tmp, l_obstruction_t_tmp, l_s_mask_tmp

    integer, allocatable, dimension(:) :: len_of_dim, id_of_dim
    integer, allocatable, dimension(:) :: id_of_var, ndim_of_var, xtype_of_var, dimids


    write(*, *) "***** wam_initial_module/read_preproc_file *****"

    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     0. OPEN GRID_INFO FILE FROM PREPROC OUTPUT.                              !
    !        ----------------------------------------                              !

    IOS = 0
    !LEN = LEN_TRIM(FILE07)
    !OPEN (UNIT=IU07, FILE=FILE07(1:LEN), FORM='UNFORMATTED', STATUS='OLD',         &
    !     &                                                                 IOSTAT=IOS)

    IOS = nf90_open("./grid/grind_info.nc", NF90_NOWRITE, ncid)
    IF (IOS.NE.0) THEN
       WRITE (IU06,*) ' ****************************************************'
       WRITE (IU06,*) ' *                                                  *'
       WRITE (IU06,*) ' *     FATAL ERROR IN SUB. READ_PREPROC_FILE        *'
       WRITE (IU06,*) ' *     =====================================        *'
       WRITE (IU06,*) ' *                                                  *'
       WRITE (IU06,*) ' * PREPROC OUTPUT FILE COULD NOT BE OPENED          *'
       WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
       WRITE (IU06,*) ' *    FILE NAME IS  FILE07 = ', FILE07(1:LEN)
       WRITE (IU06,*) ' *    UNIT IS         IU07 = ', IU07
       WRITE (IU06,*) ' *                                                  *'
       WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
       WRITE (IU06,*) ' *                                                  *'
       WRITE (IU06,*) ' ****************************************************'
       CALL ABORT1
    END IF

    ! ---------------------------------------------------------------------------- !
    !    0.  Create Dimensions
    !        ------------------

    call check( nf90_inquire( ncid, n_dims, n_vars, n_attrs, k_un ), "nf90_inquire" )
    if(DEBUG .eqv. .true.) then
       write(stdout, *)
       write(stdout, *) "n_dims = ", n_dims
       write(stdout, *) "n_vars = ", n_vars
       write(stdout, *) "n_attrs = ", n_attrs
       write(stdout, *) "k_un = ", k_un
       write(stdout, *)
    endif
    
    ! Create list of type dimension_attr and dimids                                                                                                     
    if(DEBUG .eqv. .true.) then
       write(*, *) "-------------------------------------------------------"
       write(*, *) "-- Allocate dimension arrays (name, id, len, dimid)  --"
       write(*, *) "-------------------------------------------------------"
    endif
    
    if(.not. allocated(name_of_dim)) then
       allocate( name_of_dim(n_dims), stat = status)
       call error_msg_allocation( "len_of_dim" )
    end if

    if(.not. allocated(id_of_dim)) then
       allocate( id_of_dim(n_dims), stat = status)
       call error_msg_allocation( "id_of_dim" )
    end if

    if(.not. allocated(len_of_dim)) then
       allocate( len_of_dim(n_dims), stat = status)
       call error_msg_allocation( "len_of_dim" )
    end if

    if(.not. allocated(dimids)) then
       allocate( dimids(n_dims), stat = status)
       call error_msg_allocation( "dimids_of_dim" )
    end if

    
    if(DEBUG .eqv. .true.) then
       write(*, *) "-------------------------------------------------"
       write(*, *) "-- Dimension part                              --" 
       write(*, *) "-- nf90_inq_dimid and nf90_inquire_dimension   --"
       write(*, *) "-------------------------------------------------"
    endif
    
    name_of_dim(1) = "n_nest"
    name_of_dim(2) = "ml"
    name_of_dim(3) = "kl"
    name_of_dim(4) = "nbounf"
    name_of_dim(5) = "nx"
    name_of_dim(6) = "ny"
    name_of_dim(7) = "dim_nsea"
    name_of_dim(8) = "jumax"
    name_of_dim(9) = "dim_ndepth"
    name_of_dim(10) = "result_max_val"
    name_of_dim(11) = "dim_three"
    name_of_dim(12) = "dim_two"
    name_of_dim(13) = "stringlen"
    name_of_dim(14) = "stringlen_c_name"

    ! Define dimensions                                                                                                                                 
    call check( nf90_inq_dimid(ncid, name_of_dim(1), id_of_dim(1)), "nf90_inq_dim N_NEST" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(1), name_of_dim(1), len_of_dim(1)), "nf90_inq_dim N_NEST" )

    call check( nf90_inq_dimid(ncid, name_of_dim(2), id_of_dim(2)), "nf90_inq_dim ML" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(2), name_of_dim(2), len_of_dim(2)), "nf90_inq_dim ML" )

    call check( nf90_inq_dimid(ncid, name_of_dim(3), id_of_dim(3)), "nf90_inq_dim KL" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(3), name_of_dim(3), len_of_dim(3)), "nf90_inq_dim KL" )

    call check( nf90_inq_dimid(ncid, name_of_dim(4), id_of_dim(4)), "nf90_inq_dim NBOUNF" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(4), name_of_dim(4), len_of_dim(4)), "nf90_inq_dim NBOUNF" )

    call check( nf90_inq_dimid(ncid, name_of_dim(5), id_of_dim(5)), "nf90_inq_dim NX" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(5), name_of_dim(5), len_of_dim(5)), "nf90_inq_dim NX" )

    call check( nf90_inq_dimid(ncid, name_of_dim(6), id_of_dim(6)), "nf90_inq_dim NY" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(6), name_of_dim(6), len_of_dim(6)), "nf90_inq_dim NY" )

    call check( nf90_inq_dimid(ncid, name_of_dim(7), id_of_dim(7)), "nf90_inq_dim DIM_NSEA" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(7), name_of_dim(7), len_of_dim(7)), "nf90_inq_dim DIM_NSEA" )

    call check( nf90_inq_dimid(ncid, name_of_dim(8), id_of_dim(8)), "nf90_inq_dim JUMAX" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(8), name_of_dim(8), len_of_dim(8)), "nf90_inq_dim JUMAX" )

    call check( nf90_inq_dimid(ncid, name_of_dim(9), id_of_dim(9)), "nf90_inq_dim DIM_NDEPTH" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(9), name_of_dim(9), len_of_dim(9)), "nf90_inq_dim DIM_NDEPTH" )

    call check( nf90_inq_dimid(ncid, name_of_dim(10), id_of_dim(10)), "nf90_inq_dim result_max_val" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(10), name_of_dim(10), len_of_dim(10)), "nf90_inq_dim result_max_val" )

    call check( nf90_inq_dimid(ncid, name_of_dim(11), id_of_dim(11)), "nf90_inq_dim DIM_THREE" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(11), name_of_dim(11), len_of_dim(11)), "nf90_inq_dim DIM_THREE" )

    call check( nf90_inq_dimid(ncid, name_of_dim(12), id_of_dim(12)), "nf90_inq_dim DIM_TWO" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(12), name_of_dim(12), len_of_dim(12)), "nf90_inq_dim DIM_TWO" )

    call check( nf90_inq_dimid(ncid, name_of_dim(13), id_of_dim(13)), "nf90_inq_dim STRINGLEN" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(13), name_of_dim(13), len_of_dim(13)), "nf90_inq_dim STRINGLEN" )

    call check( nf90_inq_dimid(ncid, name_of_dim(14), id_of_dim(14)), "nf90_inq_dim STRINGLEN_C_NAME" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(14), name_of_dim(14), len_of_dim(14)), "nf90_inq_dim STRINGLEN_C_NAME" )

    ! DEfine all variabless                                                                                                                           
    if(DEBUG .eqv. .true.) then
       write(*, *) "-------------------------------------------------"
       write(*, *) "-- Variable part                               --" 
       write(*, *) "-- nf90_inq_varid and nf90_inquire_variable    --"
       write(*, *) "-------------------------------------------------"
    endif

    if(DEBUG .eqv. .true.) then
       write(*, *) "------------------------------------------------------------------------"
       write(*, *) "----- Allocation of name_var, if_of_var, ndim_of_var, xtype_of_var -----"
       write(*, *) "------------------------------------------------------------------------"
    endif

    if(.not. allocated(name_of_var)) then
       allocate( name_of_var( n_vars_fixed ), stat = status )
       call error_msg_allocation( "name_of_var" )
    end if

    if(.not. allocated(id_of_var)) then
       allocate( id_of_var( n_vars_fixed ), stat = status )
       call error_msg_allocation( "id_of_var" )
    end if

    if(.not. allocated(xtype_of_var)) then
       allocate( xtype_of_var( n_vars_fixed ), stat = status )
       call error_msg_allocation( "xtype_of_var" )
    end if

    if(.not. allocated(ndim_of_var)) then
       allocate( ndim_of_var( n_vars_fixed ), stat = status )
       call error_msg_allocation( "ndim_of_var" )
    end if

    
    if(DEBUG .eqv. .true.) then
       write(*, *) "--------------------------------------"
       write(*, *) "----- Initialization of name_var -----"
       write(*, *) "--------------------------------------"
    endif

    n_vars_fixed = 92  ! If we take n_vars it could be that some variables are omited because they were not defined in file                             

    name_of_var(1) = "header"
    name_of_var(2) = "n_nest"
    name_of_var(3) = "max_nest"
    name_of_var(4) = "nbounc"
    name_of_var(5) = "n_name"
    name_of_var(6) = "n_code"
    name_of_var(7) = "xdello"
    name_of_var(8) = "xdella"
    name_of_var(9) = "n_south"
    name_of_var(10) = "n_north"

    name_of_var(11) = "n_east"
    name_of_var(12) = "n_west"
    name_of_var(13) = "ijarc"
    name_of_var(14) = "blngc"
    name_of_var(15) = "blatc"
    name_of_var(16) = "n_zdel"
    name_of_var(17) = "ml"
    name_of_var(18) = "kl"
    name_of_var(19) = "fr"
    name_of_var(20) = "dfim"

    name_of_var(21) = "gom"
    name_of_var(22) =  "c"
    name_of_var(23) =  "th"
    name_of_var(24) = "costh"
    name_of_var(25) = "sinth"
    name_of_var(26) = "delth"
    name_of_var(27) = "deltr"
    name_of_var(28) = "inv_log_co"
    name_of_var(29) = "df"
    name_of_var(30) = "df_fr"

    name_of_var(31) = "df_fr2"
    name_of_var(32) = "dfim_ofr"
    name_of_var(33) = "dfim_fr"
    name_of_var(34) = "dfim_fr2"
    name_of_var(35) = "fr5"
    name_of_var(36) = "frm5"
    name_of_var(37) = "rhowg_dfim"
    name_of_var(38) = "fmin"
    name_of_var(39) = "mo_tail"
    name_of_var(40) = "mm1_tail"

    name_of_var(41) = "mp1_tail"
    name_of_var(42) = "mp2_tail"
    name_of_var(43) = "mpm"
    name_of_var(44) = "kpm"
    name_of_var(45) = "jxo"
    name_of_var(46) = "jyo"
    name_of_var(47) = "nbounf"
    name_of_var(48) = "nbinp"
    name_of_var(49) = "c_name"
    name_of_var(50) = "blngf"

    name_of_var(51) = "blatf"
    name_of_var(52) = "ijarf"
    name_of_var(53) = "ibfl"
    name_of_var(54) = "ibfr"
    name_of_var(55) = "bfw"
    name_of_var(56) = "nx"
    name_of_var(57) = "ny"
    name_of_var(58) = "nsea"
    name_of_var(59) = "iper"
    name_of_var(60) = "one_point"

    name_of_var(61) = "reduced_grid"
    name_of_var(62) = "l_obstruction_t"
    name_of_var(63) = "obslat"
    name_of_var(64) = "obslon"
    name_of_var(65) = "nlon_rg"
    name_of_var(66) = "delphi"
    name_of_var(67) = "dellam"
    name_of_var(68) = "sinph"
    name_of_var(69) = "cosph"
    name_of_var(70) = "amowep"

    name_of_var(71) = "amosop"
    name_of_var(72) = "amoeap"
    name_of_var(73) = "amonop"
    name_of_var(74) = "zdello"
    name_of_var(75) = "ixlg"
    name_of_var(76) = "kxlt"
    name_of_var(77) = "l_s_mask"
    name_of_var(78) = "klat"
    name_of_var(79) = "klon"
    name_of_var(80) = "wlat"

    name_of_var(81) = "depth_b"
    name_of_var(82) = "ndepth"
    name_of_var(83) = "deptha"
    name_of_var(84) = "depthd"
    name_of_var(85) = "depthe"
    name_of_var(86) = "flminfr"
    name_of_var(87) = "tcgond"
    name_of_var(88) = "tfak"
    name_of_var(89) = "tsihkd"
    name_of_var(90) = "tfac_st"

    name_of_var(91) = "t_tail"
    name_of_var(92) = "delu"

    !    name_of_var = [ "header", "n_nest  ", "max_nest", "nbounc", "n_name", "n_code", "xdello", "xdella", "n_south", "n_north", "n_east", &
    !         "n_west", "ijarc", "blngc", "blatc", "n_zdel", "ml", "kl", "fr", "dfim", "gom", &
    !         "c", "th", "costh", "sinth", "delth", "deltr", "inv_log_co", "df", "df_fr", "df_fr2", &
    !         "dfim_ofr", "dfim_fr", "dfim_fr2", "fr5", "frm5", "rhowg_dfim", "fmin", "mo_tail", "mm1_tail", "mp1_tail", &
    !         "mp2_tail", "mpm", "kpm", "jxo", "jyo", "nbounf", "nbinp", "c_name", "blngf", "blatf", &
    !         "ijarf", "ibfl", "ibfr", "bfw", "nx", "ny", "nsea", "iper", "one_point", "reduced_grid", &
    !         "l_obstruction_t", "obslat", "obslon", "nlon_rg", "delphi", "dellam", "sinph", "cosph", "amowep", "amosop", &
    !         "amoeap", "amonop", "zdello", "ixlg", "kxlt", "l_s_mask", "klat", "klon", "wlat", "depth_b", &
    !         "ndepth", "deptha", "depthb", "depthe", "flminfr", "tcgond", "tfak", "tsihkd", "tfac_st", "t_tail", &
    !         "delu"] 

    if(DEBUG .eqv. .true.) then
       write(*, *) "-----------------------------"
       write(*, *) "----- nf90_inq_varid    -----"
       write(*, *) "-----------------------------"
    endif

    i = 1
    do while (i <= n_vars_fixed)
       write(*, *) "Loop 1, Index: ", i
       call check( nf90_inq_varid(ncid, trim(name_of_var(i)), id_of_var(i) ), "nf90_inq_varid " // trim(name_of_var(i)) )
!       write( stdout, * ) "Id of var: ", id_of_var(i), " with index ", i
!       write( stdout, * ) "name of var: ", name_of_var(i), " with index ", i
    !   write(stdout, *) "maxval(NBOUNC) = ", maxval(NBOUNC)
       write(stdout, *) "NBOUNF = ", NBOUNF
       write(stdout, *) "l_obstr = ", l_obstruction_t
       
       if( (i == 6) .and. (maxval(NBOUNC) <= 0) ) then
          i = i + 11
       else if( (i == 49) .and. (NBOUNF <= 0)) then
          i = i + 7
       else if( (i == 62) .and. (l_obstruction_t .eqv. .FALSE.)) then
          i = i + 3
       else
          i = i + 1
          write(stdout, *) "i (in if) = ", i
       end if
       write(stdout, *) "(after) i = ", i

!       if( i > n_vars_fixed ) then
!          exit
!       endif
    end do

    
    if(DEBUG .eqv. .true.) then
       write(*, *) "-----------------------------"
       write(*, *) "----- nf90_inq_varid    -----"
       write(*, *) "-----------------------------"
    endif

    i = 1
    do while (i <= n_vars_fixed)
       write(*, *) "Loop 2, Index: ", i
       call check( nf90_inquire_variable( ncid = ncid, varid = id_of_var(i), xtype = xtype_of_var(i), &
            ndims = ndim_of_var(i), dimids = dimids ), "nf90_inquire_variable " // trim(name_of_var(i)) )

       if( (i == 6) .and. (maxval(NBOUNC) <= 0) ) then
          i = i + 11
       elseif( (i == 49) .and. (NBOUNF <= 0)) then
          i = i + 7
       else if( (i == 62) .and. (l_obstruction_t .eqv. .FALSE.)) then
          i = i + 3
       else
          i = i + 1
       end if

       dimids = -999
!       if( i > n_vars_fixed ) then
!          exit
!       endif
    end do

    if(DEBUG .eqv. .true.) then
       write(*, *) "-------------------------------------------------"
       write(*, *) "-- Output of inquire variables                 --" 
       write(*, *) "-- nf90_inq_varid and nf90_inquire_variable    --"
       write(*, *) "-------------------------------------------------"

       write( stdout, * ) "n_vars = ", n_vars
       do i = 0, 10
          write( stdout, * ) "varid = ", id_of_var(i)
          write( stdout, * ) "name of var = ", name_of_var(i)
          write( stdout, * ) "xtype of var = ", xtype_of_var(i)
          write( stdout, * ) "ndims of var = ", ndim_of_var(i)
          write( stdout, * ) "dimids = ", dimids
          write( stdout, * )
       end do
    endif


    ! Set values to "zero" to see later, if the correct values were read                                                                                
    n_nest = -999
    max_nest = -999
    nbounc = -999
    n_code = -999
    xdello = -999
    xdella = -999

    ml = -999
    kl = -999
    fr = -999

    
    if(DEBUG .eqv. .true.) then
       write(*, *) "-------------------------------------------------"
       write(*, *) "--  Reading of variables                       --"
       write(*, *) "--  nf90_get_var and test for allocation       --"
       write(*, *) "-------------------------------------------------"
    endif
    
    call check( nf90_get_var(ncid, id_of_var(1), header), "nf90_get_var header" )


    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. READ COARSE GRID BOUNDARY OUTPUT INFORMATION.                         !
    !        ---------------------------------------------                         !

    call check( nf90_get_var(ncid, id_of_var(2), n_nest), "nf90_get_var n_nest" )
    call check( nf90_get_var(ncid, id_of_var(3), max_nest), "nf90_get_var max_nest" )
    !READ(IU07) N_NEST, MAX_NEST

    IF (.NOT.ALLOCATED(NBOUNC )) ALLOCATE (NBOUNC(N_NEST))
    IF (.NOT.ALLOCATED(N_NAME )) ALLOCATE (N_NAME(N_NEST))
    if (.not.allocated(n_code )) allocate (n_code(n_nest))
    IF (.NOT.ALLOCATED(IJARC  )) ALLOCATE (IJARC(MAX_NEST,N_NEST))
    IF (.NOT.ALLOCATED(N_SOUTH)) ALLOCATE (N_SOUTH(N_NEST))
    IF (.NOT.ALLOCATED(N_NORTH)) ALLOCATE (N_NORTH(N_NEST))
    IF (.NOT.ALLOCATED(N_EAST )) ALLOCATE (N_EAST(N_NEST))
    IF (.NOT.ALLOCATED(N_WEST )) ALLOCATE (N_WEST(N_NEST))
    IF (.NOT.ALLOCATED(BLNGC  )) ALLOCATE (BLNGC(MAX_NEST,N_NEST))
    IF (.NOT.ALLOCATED(BLATC  )) ALLOCATE (BLATC(MAX_NEST,N_NEST))
    IF (.NOT.ALLOCATED(N_ZDEL )) ALLOCATE (N_ZDEL(MAX_NEST,N_NEST))

    call check( nf90_get_var(ncid, id_of_var(4), nbounc), "nf90_get_var nbounc" )
    call check( nf90_get_var(ncid, id_of_var(5), n_name), "nf90_get_var n_name" )
    call check( nf90_get_var(ncid, id_of_var(6), n_code), "nf90_get_var n_code" )

    if(DEBUG .eqv. .true.) then
       write( stdout, *) "------------------- Output of Variables ------------------"
       write( stdout, *) "After reading n_nest = ", n_nest
       write( stdout, *) "After reading max_nest = ", max_nest
       write( stdout, *) "After reading nbounc = ", nbounc
       write( stdout, *) "After reading n_name = ", n_name
       write( stdout, *) "After reading n_code = ", n_code
    endif

    DO I=1,N_NEST
       IF (NBOUNC(I).GT.0) THEN
          call check( nf90_get_var(ncid, id_of_var(7), xdello), "nf90_get_var xdello" )
          call check( nf90_get_var(ncid, id_of_var(8), xdella), "nf90_get_var xdella" )
          call check( nf90_get_var(ncid, id_of_var(9), n_south), "nf90_get_var n_south" )
          call check( nf90_get_var(ncid, id_of_var(10), n_north), "nf90_get_var n_north" )
          call check( nf90_get_var(ncid, id_of_var(11), n_east), "nf90_get_var n_east" )
          call check( nf90_get_var(ncid, id_of_var(12), n_west), "nf90_get_var n_wnest" )
          call check( nf90_get_var(ncid, id_of_var(13), ijarc), "nf90_get_varijarc" )
          call check( nf90_get_var(ncid, id_of_var(14), blngc), "nf90_get_var blngc" )
          call check( nf90_get_var(ncid, id_of_var(15), blatc), "nf90_get_var blatc" )
          call check( nf90_get_var(ncid, id_of_var(16), n_zdel), "nf90_get_var n_zdel" )

          if(DEBUG .eqv. .true.) then

             write( stdout, *) "After reading xdello = ", xdello
             write( stdout, *) "After reading xdella = ", xdella
             write( stdout, *) "After reading n_south = ", n_south
             write( stdout, *) "After reading n_north = ", n_north
             write( stdout, *) "After reading n_south = ", n_east
             write( stdout, *) "After reading n_north = ", n_west
             write( stdout, *) "After reading IJARC = ", ijarc
             write( stdout, *) "After reading blngc = ", blngc
             write( stdout, *) "After reading blatc = ", blatc
             write( stdout, *) "After reading n_zdel = ", n_zdel
          end if
       endif
    end do

    ! Original part
    !
    !   DO I=1,N_NEST
    !   READ(IU07) NBOUNC(I), N_NAME(I), n_code(i)
    !   IF (NBOUNC(I).GT.0) THEN
    !      READ(IU07) IJARC(1:NBOUNC(I),I)
    !      READ(IU07) XDELLO, XDELLA, N_SOUTH(I), N_NORTH(I), N_EAST(I), N_WEST(I), &
    !&                BLNGC(1:NBOUNC(I),I), BLATC(1:NBOUNC(I),I), N_ZDEL(1:NBOUNC(I),I)
    !   END IF
    !   END DO

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. READ FINE GRID BOUNDARY INPUT INFORMATION.                            !
    !        ------------------------------------------                            !

    call check( nf90_get_var(ncid, id_of_var(47), nbounf), "nf90_get_var nbounf" )
    call check( nf90_get_var(ncid, id_of_var(48), nbinp), "nf90_get_var nbinp" )
    call check( nf90_get_var(ncid, id_of_var(49), c_name), "nf90_get_var c_name" )

    if(DEBUG .eqv. .true.) then
       write( stdout, *) "After reading nbouf = ", nbounf
       write( stdout, *) "After reading nbinp = ", nbinp
       write( stdout, *) "After reading c_name = ", c_name
       write( stdout, *)
    endif

    if( NBOUNF > 0 ) then
       IF (.NOT.ALLOCATED(BLNGF)) ALLOCATE (BLNGF(NBOUNF))
       IF (.NOT.ALLOCATED(BLATF)) ALLOCATE (BLATF(NBOUNF))
       IF (.NOT.ALLOCATED(IJARF)) ALLOCATE (IJARF(NBOUNF))
       IF (.NOT.ALLOCATED(IBFL )) ALLOCATE (IBFL(NBOUNF))
       IF (.NOT.ALLOCATED(IBFR )) ALLOCATE (IBFR(NBOUNF))
       IF (.NOT.ALLOCATED(BFW  )) ALLOCATE (BFW(NBOUNF))

       call check( nf90_get_var(ncid, id_of_var(50), blngf), "nf90_get_var blngf" )
       call check( nf90_get_var(ncid, id_of_var(51), blatf), "nf90_get_var blatf" )
       call check( nf90_get_var(ncid, id_of_var(52), ijarf), "nf90_get_var ijarf" )
       call check( nf90_get_var(ncid, id_of_var(53), ibfl), "nf90_get_var ibfl" )
       call check( nf90_get_var(ncid, id_of_var(54), ibfr), "nf90_get_var ibfr" )
       call check( nf90_get_var(ncid, id_of_var(55), bfw), "nf90_get_var bfw" )

       if(DEBUG .eqv. .true.) then
          write( stdout, *) "After reading blngf = ", blngf
          write( stdout, *) "After reading blatf = ", blatf
          write( stdout, *) "After reading ijarf = ", ijarf
          write( stdout, *) "After reading ibfl = ", ibfl
          write( stdout, *) "After reading ibfr = ", ibfr
          write( stdout, *) "After reading bfw = ", bfw
       end if
    endif


    ! READ (UNIT=IU07) NBOUNF, NBINP, C_NAME

    ! IF (NBOUNF.GT.0) THEN
    !   IF (.NOT.ALLOCATED(BLNGF)) ALLOCATE (BLNGF(NBOUNF))
    !   IF (.NOT.ALLOCATED(BLATF)) ALLOCATE (BLATF(NBOUNF))
    !   IF (.NOT.ALLOCATED(IJARF)) ALLOCATE (IJARF(NBOUNF))
    !   IF (.NOT.ALLOCATED(IBFL )) ALLOCATE (IBFL(NBOUNF))
    !   IF (.NOT.ALLOCATED(IBFR )) ALLOCATE (IBFR(NBOUNF))
    !   IF (.NOT.ALLOCATED(BFW  )) ALLOCATE (BFW(NBOUNF))
    !   READ (IU07) BLNGF(1:NBOUNF), BLATF(1:NBOUNF), IJARF(1:NBOUNF),              &
    !&             IBFL(1:NBOUNF), IBFR(1:NBOUNF), BFW(1:NBOUNF)
    ! END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. READ FREQUENCY DIRECTION GRID.                                        !
    !        ------------------------------                                        !

    !READ (IU07) ML, KL
    call check( nf90_get_var(ncid, id_of_var(17), ml), "nf90_get_var ml" )
    call check( nf90_get_var(ncid, id_of_var(18), kl), "nf90_get_var kl" )

    IF ( .NOT.ALLOCATED(FR)      ) ALLOCATE( FR(ML)      )
    IF ( .NOT.ALLOCATED(DFIM)    ) ALLOCATE( DFIM(ML)    )
    IF ( .NOT.ALLOCATED(GOM)     ) ALLOCATE( GOM(ML)     )
    IF ( .NOT.ALLOCATED(C)       ) ALLOCATE( C(ML)       )
    IF ( .NOT.ALLOCATED(TH)      ) ALLOCATE( TH(KL)      )
    IF ( .NOT.ALLOCATED(COSTH)   ) ALLOCATE( COSTH(KL)   )
    IF ( .NOT.ALLOCATED(SINTH)   ) ALLOCATE( SINTH(KL)   )
    IF ( .NOT.ALLOCATED(DF)      ) ALLOCATE( DF(ML)      )
    IF ( .NOT.ALLOCATED(DF_FR)   ) ALLOCATE( DF_FR(ML)   )
    IF ( .NOT.ALLOCATED(DF_FR2)  ) ALLOCATE( DF_FR2(ML)  )
    IF ( .NOT.ALLOCATED(DFIM)    ) ALLOCATE( DFIM(ML)    )
    IF ( .NOT.ALLOCATED(DFIMOFR) ) ALLOCATE( DFIMOFR(ML) )
    IF ( .NOT.ALLOCATED(DFIM_FR) ) ALLOCATE( DFIM_FR(ML) )
    IF ( .NOT.ALLOCATED(DFIM_FR2)) ALLOCATE( DFIM_FR2(ML))
    IF ( .NOT.ALLOCATED(FR5)     ) ALLOCATE( FR5(ML)     )
    IF ( .NOT.ALLOCATED(FRM5)    ) ALLOCATE( FRM5(ML)    )
    IF ( .NOT.ALLOCATED(RHOWG_DFIM)) ALLOCATE( RHOWG_DFIM(ML))
    IF (.NOT. ALLOCATED(MPM)     ) ALLOCATE(MPM(ML,-1:1))  !! FREQUENCY NEIGHTBOURS.
    IF (.NOT. ALLOCATED(KPM)     ) ALLOCATE(KPM(KL,-1:1))  !! DIRECTION NEIGHTBOURS.
    IF (.NOT. ALLOCATED(JXO)     ) ALLOCATE(JXO(KL,2))
    IF (.NOT. ALLOCATED(JYO)     ) ALLOCATE(JYO(KL,2))

    call check( nf90_get_var(ncid, id_of_var(19), fr), "nf90_get_var fr" )
    call check( nf90_get_var(ncid, id_of_var(20), dfim), "nf90_get_var dfim" )
    call check( nf90_get_var(ncid, id_of_var(21), gom), "nf90_get_var gom" )

    call check( nf90_get_var(ncid, id_of_var(22), c), "nf90_get_var c" )
    call check( nf90_get_var(ncid, id_of_var(23), th), "nf90_get_var th" )
    call check( nf90_get_var(ncid, id_of_var(24), costh), "nf90_get_var costh" )
    call check( nf90_get_var(ncid, id_of_var(25), sinth), "nf90_get_var sinth" )
    call check( nf90_get_var(ncid, id_of_var(26), delth), "nf90_get_var delth" )

    call check( nf90_get_var(ncid, id_of_var(27), deltr), "nf90_get_var deltr" )
    call check( nf90_get_var(ncid, id_of_var(28), inv_log_co), "nf90_get_var inv_log_co" )
    call check( nf90_get_var(ncid, id_of_var(29), df), "nf90_get_var df" )
    call check( nf90_get_var(ncid, id_of_var(30), df_fr), "nf90_get_var df_fr" )
    call check( nf90_get_var(ncid, id_of_var(31), df_fr2), "nf90_get_var df_fr2" )

    call check( nf90_get_var(ncid, id_of_var(32), dfimofr), "nf90_get_var dfimofr" )
    call check( nf90_get_var(ncid, id_of_var(33), dfim_fr), "nf90_get_var dfim_fr" )
    call check( nf90_get_var(ncid, id_of_var(34), dfim_fr2), "nf90_get_var dfim_fr2" )
    call check( nf90_get_var(ncid, id_of_var(35), fr5), "nf90_get_var fr5" )
    call check( nf90_get_var(ncid, id_of_var(36), frm5), "nf90_get_var frm5" )

    call check( nf90_get_var(ncid, id_of_var(37), rhowg_dfim), "nf90_get_var rhowg_dfim" )
    call check( nf90_get_var(ncid, id_of_var(38), fmin), "nf90_get_var fmin" )
    call check( nf90_get_var(ncid, id_of_var(39), mo_tail), "nf90_get_var mo_tail" )
    call check( nf90_get_var(ncid, id_of_var(40), mm1_tail), "nf90_get_var mm1_tail" )
    call check( nf90_get_var(ncid, id_of_var(41), mp1_tail), "nf90_get_var mp1_tail" )

    call check( nf90_get_var(ncid, id_of_var(42), mp2_tail), "nf90_get_var mp2_tail" )
    call check( nf90_get_var(ncid, id_of_var(43), mpm), "nf90_get_var mpm" )
    call check( nf90_get_var(ncid, id_of_var(44), kpm), "nf90_get_var kpm" )
    call check( nf90_get_var(ncid, id_of_var(45), jxo), "nf90_get_var jxo" )
    call check( nf90_get_var(ncid, id_of_var(46), jyo), "nf90_get_var jyo" )


    if(DEBUG .eqv. .true.) then
       write( stdout, *) "After reading ml = ", ml
       write( stdout, *) "After reading kl = ", kl
       write( stdout, *) "After reading fr = ", fr
       write( stdout, *) "After reading dfim = ", dfim
       write( stdout, *) "After reading gom = ", gom

       write( stdout, *) "After reading c = ", c
       write( stdout, *) "After reading th = ", th
       write( stdout, *) "After reading costh = ", costh
       write( stdout, *) "After reading sinth = ", sinth
       write( stdout, *) "After reading delth = ", delth

       write( stdout, *) "After reading deltr = ", deltr
       write( stdout, *) "After reading inv_log_co = ", inv_log_co
       write( stdout, *) "After reading df = ", df
       write( stdout, *) "After reading df_fr = ", df_fr
       write( stdout, *) "After reading df_fr2 = ", df_fr2

       write( stdout, *) "After reading dfimofr = ", dfimofr
       write( stdout, *) "After reading dfim_fr = ", dfim_fr
       write( stdout, *) "After reading dfim_fr2 = ", dfim_fr2
       write( stdout, *) "After reading fr5 = ", fr5
       write( stdout, *) "After reading frm5 = ", frm5

       write( stdout, *) "After reading rhowg_dfim = ", rhowg_dfim
       write( stdout, *) "After reading fmin = ", fmin
       write( stdout, *) "After reading mo_tail = ", mo_tail
       write( stdout, *) "After reading mm1_tail = ", mm1_tail
       write( stdout, *) "After reading mp1_tail = ", mp1_tail

       write( stdout, *) "After reading mp2_tail = ", mp2_tail
       write( stdout, *) "After reading mpm = ", mpm
       write( stdout, *) "After reading kpm = ", kpm
       write( stdout, *) "After reading jxo = ", jxo
       write( stdout, *) "After reading jyo = ", jyo

       write( stdout, *)
    endif

    !    READ (IU07)  FR, DFIM, GOM, C, DELTH, DELTR, TH, COSTH, SINTH, INV_LOG_CO,     &
    !&            DF, DF_FR, DF_FR2, DFIM, DFIMOFR, DFIM_FR, DFIM_FR2, FR5, FRM5,   &
    !&            RHOWG_DFIM,                                                       &
    !&            FMIN, MO_TAIL, MM1_TAIL, MP1_TAIL, MP2_TAIL

    !READ (IU07)  MPM, KPM, JXO, JYO

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     4. READ GRID INFORMATION.                                                !
    !        ----------------------                                                !


    call check( nf90_get_var(ncid, id_of_var(56), nx), "nf90_get_var nx" )
    call check( nf90_get_var(ncid, id_of_var(57), ny), "nf90_get_var ny" )
    call check( nf90_get_var(ncid, id_of_var(58), nsea), "nf90_get_var nsea" )
    call check( nf90_get_var(ncid, id_of_var(59), iper_tmp), "nf90_get_var iper" )
    iper = merge(.TRUE., .FALSE., iper_tmp /= 0)
    call check( nf90_get_var(ncid, id_of_var(60), one_point_tmp), "nf90_get_var one_point" )
    one_point = merge(.TRUE., .FALSE., one_point_tmp /= 0)
    call check( nf90_get_var(ncid, id_of_var(61), reduced_grid_tmp), "nf90_get_var reduced_grid" )
    reduced_grid = merge(.TRUE., .FALSE., reduced_grid_tmp /= 0)
    call check( nf90_get_var(ncid, id_of_var(62), l_obstruction_t_tmp), "nf90_get_var l_obstruction_t" )
    l_obstruction_t = merge(.TRUE., .FALSE., l_obstruction_t_tmp /= 0)

    if(DEBUG .eqv. .true.) then
       write( stdout, *) "After reading nx = ", nx
       write( stdout, *) "After reading ny = ", ny
       write( stdout, *) "After reading nx = ", nsea
       write( stdout, *) "After reading IPER = ", iper
       write( stdout, *) "After reading one_point = ", one_point
       write( stdout, *) "After reading reduced_grid = ", reduced_grid
       write( stdout, *) "After reading l_obstruction_t = ", l_obstruction_t
       write( stdout, *)
    endif

    !    READ (IU07) NX, NY, NSEA, IPER, ONE_POINT, REDUCED_GRID, L_OBSTRUCTION_T
    IF ( .NOT.ALLOCATED(L_S_MASK)) ALLOCATE( L_S_MASK(1:NX,1:NY) )
    IF ( .NOT.ALLOCATED(NLON_RG) ) ALLOCATE( NLON_RG(1:NY) )
    IF ( .NOT.ALLOCATED(DELLAM)  ) ALLOCATE( DELLAM(1:NY)  )
    IF ( .NOT.ALLOCATED(ZDELLO)  ) ALLOCATE( ZDELLO(1:NY) )
    IF ( .NOT.ALLOCATED(IXLG)    ) ALLOCATE( IXLG(1:NSEA) )
    IF ( .NOT.ALLOCATED(KXLT)    ) ALLOCATE( KXLT(1:NSEA) )
    IF ( .NOT.ALLOCATED(SINPH)   ) ALLOCATE( SINPH(1:NY)  )
    IF ( .NOT.ALLOCATED(COSPH)   ) ALLOCATE( COSPH(1:NY)  )
    IF ( .NOT.ALLOCATED(KLAT)    ) ALLOCATE( KLAT(1:NSEA,1:2,1:2) )
    IF ( .NOT.ALLOCATED(KLON)    ) ALLOCATE( KLON(1:NSEA,1:2) )
    IF ( .NOT.ALLOCATED(WLAT)    ) ALLOCATE( WLAT(1:NSEA,1:2) )
    IF ( .NOT.ALLOCATED(DEPTH_B) ) ALLOCATE( DEPTH_B(1:NSEA) )

    IF ( .NOT.ALLOCATED(OBSLAT ) ) ALLOCATE (OBSLAT (NSEA,2,ML))
    IF ( .NOT.ALLOCATED(OBSLON ) ) ALLOCATE (OBSLON (NSEA,2,ML))

    call check( nf90_get_var(ncid, id_of_var(65), nlon_rg), "nf90_get_var nlon_rg" )
    call check( nf90_get_var(ncid, id_of_var(66), delphi), "nf90_get_var delphi" )
    call check( nf90_get_var(ncid, id_of_var(67), dellam), "nf90_get_var dellam" )
    call check( nf90_get_var(ncid, id_of_var(68), sinph), "nf90_get_var sinph" )
    call check( nf90_get_var(ncid, id_of_var(69), cosph), "nf90_get_var cosph" )
    call check( nf90_get_var(ncid, id_of_var(70), amowep), "nf90_get_var amowep" )

    call check( nf90_get_var(ncid, id_of_var(71), amosop), "nf90_get_var amosop" )
    call check( nf90_get_var(ncid, id_of_var(72), amoeap), "nf90_get_var amoeap" )
    call check( nf90_get_var(ncid, id_of_var(73), amonop), "nf90_get_var amonop" )
    call check( nf90_get_var(ncid, id_of_var(74), zdello), "nf90_get_var zdello" )
    call check( nf90_get_var(ncid, id_of_var(75), ixlg), "nf90_get_var ixlg" )

    call check( nf90_get_var(ncid, id_of_var(76), kxlt), "nf90_get_var kxlt" )
    call check( nf90_get_var(ncid, id_of_var(77), l_s_mask_tmp), "nf90_get_var l_s_mask" )
    l_s_mask = merge(.TRUE., .FALSE., l_s_mask_tmp /= 0)
    call check( nf90_get_var(ncid, id_of_var(78), klat), "nf90_get_var klat" )
    call check( nf90_get_var(ncid, id_of_var(79), klon), "nf90_get_var klon" )
    call check( nf90_get_var(ncid, id_of_var(80), wlat), "nf90_get_var wlat" )
    call check( nf90_get_var(ncid, id_of_var(81), depth_b), "nf90_get_var depth_b" )

    if( L_OBSTRUCTION_T .eqv. .TRUE.) then
       call check( nf90_get_var(ncid, id_of_var(63), obslat), "nf90_get_var obslat" )
       call check( nf90_get_var(ncid, id_of_var(64), obslon), "nf90_get_var obslon" )

       write( stdout, *) "After reading obslat = ", obslat
       write( stdout, *) "After reading obslon = ", obslon
       write( stdout, *)
       IF (.NOT.L_OBSTRUCTION) THEN
          OBSLAT = 1.
          OBSLON = 1.
       END IF
    ELSE
       OBSLAT = 1.
       OBSLON = 1.
       IF (L_OBSTRUCTION) THEN
          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +     WARNING ERROR SUB. READ_PREPROC_FILE.        +'
          WRITE (IU06,*) ' +     =======================================      +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' + REDUCTION DUE TO SUB-GRID FEATURES REQUESTED,    +'
          WRITE (IU06,*) ' + BUT OBSTRUCTION FACTORS ARE NOT IN               +'
          WRITE (IU06,*) ' + PREPROC OUTPUT FILE.                             +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +               MODEL CONTINUES                    +'
          WRITE (IU06,*) ' +              WITHOUT REDUCTION                   +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          L_OBSTRUCTION = .FALSE.
       END IF

    end if



    if(DEBUG .eqv. .true.) then
       write( stdout, *) "After reading nlon_rg = ", nlon_rg
       write( stdout, *) "After reading delphi = ", delphi
       write( stdout, *) "After reading dellam = ", dellam
       write( stdout, *) "After reading sinph = ", sinph
       write( stdout, *) "After reading cosph = ", cosph
       write( stdout, *) "After reading amowep = ", amowep

       write( stdout, *) "After reading amosop = ", amosop
       write( stdout, *) "After reading amoeap = ", amoeap
       write( stdout, *) "After reading amonop = ", amonop
       write( stdout, *) "After reading zdello = ", zdello
       write( stdout, *) "After reading ixlg = ", ixlg

       write( stdout, *) "After reading kxlt = ", kxlt
       write( stdout, *) "After reading l_s_mask = ", l_s_mask
       write( stdout, *) "After reading klat = ", klat
       write( stdout, *) "After reading klon = ", klon
       write( stdout, *) "After reading wlat = ", wlat
       write( stdout, *) "After reading depth_b = ", depth_b
       write( stdout, *)
    endif


    !    READ (IU07) NLON_RG
    !    READ (IU07) DELPHI, DELLAM, SINPH, COSPH, AMOWEP, AMOSOP, AMOEAP, AMONOP,      &
    !&           XDELLA, XDELLO, ZDELLO
    !READ (IU07) IXLG, KXLT, L_S_MASK
    ! READ (IU07) KLAT, KLON, WLAT, DEPTH_B
    !IF (L_OBSTRUCTION_T) THEN
    !   READ (IU07) OBSLAT, OBSLON
    !   IF (.NOT.L_OBSTRUCTION) THEN
    !      OBSLAT = 1.
    !      OBSLON = 1.
    !   END IF
    !ELSE
    !   OBSLAT = 1.
    !   OBSLON = 1.
    !   IF (L_OBSTRUCTION) THEN
    !      WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
    !      WRITE (IU06,*) ' +                                                  +'
    !      WRITE (IU06,*) ' +     WARNING ERROR SUB. READ_PREPROC_FILE.        +'
    !      WRITE (IU06,*) ' +     =======================================      +'
    !      WRITE (IU06,*) ' +                                                  +'
    !      WRITE (IU06,*) ' + REDUCTION DUE TO SUB-GRID FEATURES REQUESTED,    +'
    !      WRITE (IU06,*) ' + BUT OBSTRUCTION FACTORS ARE NOT IN               +'
    !      WRITE (IU06,*) ' + PREPROC OUTPUT FILE.                             +'
    !      WRITE (IU06,*) ' +                                                  +'
    !      WRITE (IU06,*) ' +               MODEL CONTINUES                    +'
    !      WRITE (IU06,*) ' +              WITHOUT REDUCTION                   +'
    !      WRITE (IU06,*) ' +                                                  +'
    !      WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
    !      L_OBSTRUCTION = .FALSE.
    !   END IF
    !END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     7. READ TABLES.                                                          !
    !        ------------                                                          !

    call check( nf90_get_var(ncid, id_of_var(82), ndepth), "nf90_get_var ndepth" )
    call check( nf90_get_var(ncid, id_of_var(83), deptha), "nf90_get_var deptha" )
    call check( nf90_get_var(ncid, id_of_var(84), depthd), "nf90_get_var depthd" )
    call check( nf90_get_var(ncid, id_of_var(85), depthe), "nf90_get_var depthe" )

    !READ (IU07) NDEPTH, DEPTHA, DEPTHD, DEPTHE


    IF (ALLOCATED(FLMINFR)) DEALLOCATE (FLMINFR)
    ALLOCATE (FLMINFR(1:JUMAX,1:ML))
    IF (ALLOCATED(TCGOND) ) DEALLOCATE (TCGOND)
    ALLOCATE (TCGOND(NDEPTH,ML) )
    IF (ALLOCATED(TFAK)   ) DEALLOCATE (TFAK)
    ALLOCATE (TFAK(NDEPTH,ML)   )
    IF (ALLOCATED(TSIHKD) ) DEALLOCATE (TSIHKD)
    ALLOCATE (TSIHKD(NDEPTH,ML) )
    IF (ALLOCATED(TFAC_ST)) DEALLOCATE (TFAC_ST)
    ALLOCATE( TFAC_ST(NDEPTH,ML))
    IF (ALLOCATED(T_TAIL)) DEALLOCATE (T_TAIL)
    ALLOCATE( T_TAIL(NDEPTH,ML))

    call check( nf90_get_var(ncid, id_of_var(86), flminfr), "nf90_get_var flminfr" )
    call check( nf90_get_var(ncid, id_of_var(87), tcgond), "nf90_get_var tcgond" )
    call check( nf90_get_var(ncid, id_of_var(88), tfak), "nf90_get_var tfak" )
    call check( nf90_get_var(ncid, id_of_var(89), tsihkd), "nf90_get_var tsihkd" )
    call check( nf90_get_var(ncid, id_of_var(90), tfac_st), "nf90_get_var tfac_st" )

    call check( nf90_get_var(ncid, id_of_var(91), t_tail), "nf90_get_var t_tail" )
    call check( nf90_get_var(ncid, id_of_var(92), delu), "nf90_get_var delu" )
    ! READ (IU07) FLMINFR, TCGOND, TFAK, TSIHKD, TFAC_ST, T_TAIL
    ! READ (IU07) DELU

    if(DEBUG .eqv. .true.) then
       write( stdout, *) "After reading ndepth = ", ndepth
       write( stdout, *) "After reading deptha = ", deptha
       write( stdout, *) "After reading depthd = ", depthd
       write( stdout, *) "After reading depthe = ", depthe
       write( stdout, *) "After reading flminfr = ", flminfr
       write( stdout, *) "After reading tcgond = ", tcgond
       write( stdout, *) "After reading tfak = ", tfak
       write( stdout, *) "After reading tsihkd = ", tsihkd
       write( stdout, *) "After reading tfac_st = ", tfac_st
       write( stdout, *) "After reading t_tail = ", t_tail
       write( stdout, *) "After reading delu = ", delu
       write( stdout, *)
    endif
    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     8. CLOSE FILE AND RETURN.                                                !
    !        ----------------------                                                !

    ! CLOSE (UNIT=IU07, STATUS='KEEP')

    !
    ! End of reading to netCDF file
    !                                                                                                                                                   
    call check( nf90_close(ncid), "NF90_CLOSE" )

    IF(USE_OASIS) CALL WAM_OASIS_WRITE_GRID  !! ModR04: Include OASIS

  END SUBROUTINE READ_PREPROC_FILE

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     G. PRIVATE MODULE PROCEDURES.                                            !
  !                                                                              !
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE PREPARE_FIRST_DEPTH

    USE WAM_OASIS_MODULE,     ONLY: USE_OASIS_ELEV_IN  !! ModR04: Include OASIS
    INTEGER  :: LEN

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. DEEP WATER RUN: RETURN.                                               !
    !        -----------------------                                               !

    IF (.NOT.SHALLOW_RUN) THEN
       TOPO_RUN = .FALSE.
       CDTA = ' '
       IF (USE_OASIS_ELEV_IN) THEN  !! ModR04: Include OASIS
          WRITE (IU06,*) ' ****************************************************'
          WRITE (IU06,*) ' *						       *'
          WRITE (IU06,*) ' *	     FATAL ERROR SUB. PREPARE_FIRST_DEPTH.     *'
          WRITE (IU06,*) ' *	     =====================================     *'
          WRITE (IU06,*) ' *						       *'
          WRITE (IU06,*) ' * OASIS TOPO IS REQUESTED FOR A DEEPWATER RUN      *'
          WRITE (IU06,*) ' *						       *'
          WRITE (IU06,*) ' * ADAPT THE OASIS namcouple FILE                   *'
          WRITE (IU06,*) ' *						       *'
          WRITE (IU06,*) ' *	    PROGRAM ABORTS  PROGRAM ABORTS	       *'
          WRITE (IU06,*) ' *						       *'
          WRITE (IU06,*) ' ****************************************************'
          CALL ABORT1
       END IF
       RETURN 
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. SHALLOW WATER RUN: CHECK TOPO FILE.                                   !
    !        -----------------------------------                                   !

    LEN = LEN_TRIM(FILE08)
    TOPO_RUN = .FALSE.
    IF (LEN.GT.0) INQUIRE (FILE=FILE08(1:LEN), EXIST=TOPO_RUN)
    IF (LEN.GT.0 .AND. .NOT.TOPO_RUN .AND. .NOT.USE_OASIS_ELEV_IN) THEN  !! ModR04: Only without OASIS
       WRITE (IU06,*) ' ****************************************************'
       WRITE (IU06,*) ' *                                                  *'
       WRITE (IU06,*) ' *        FATAL ERROR SUB. PREPARE_FIRST_DEPTH.     *'
       WRITE (IU06,*) ' *        =====================================     *'
       WRITE (IU06,*) ' *                                                  *'
       WRITE (IU06,*) ' * THE TOPO INPUT FILE DEFINED IN THE USER INPUT    *'
       WRITE (IU06,*) ' * DOES NOT EXIST.                                  *'
       WRITE (IU06,*) ' *    FILE NAME IS     FILE08 = ', TRIM(FILE08)
       WRITE (IU06,*) ' *                                                  *'
       WRITE (IU06,*) ' * CHANGE FILE NAME TO BLANK FOR BASIC DEPTH OR     *'
       WRITE (IU06,*) ' * CORRECT FILE NAME IN THE USER INPUT.             *'
       WRITE (IU06,*) ' *                                                  *'
       WRITE (IU06,*) ' *       PROGRAM ABORTS  PROGRAM ABORTS             *'
       WRITE (IU06,*) ' *                                                  *'
       WRITE (IU06,*) ' ****************************************************'
       CALL ABORT1
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. SHALLOW WATER RUN: A TOPO FILE DOES NOT EXIST.                        !
    !        ----------------------------------------------                        !

    IF (.NOT.TOPO_RUN) THEN
       IF (CDTA.EQ.' ') THEN

          !     3.1 DEPTH IS NOT DEFINED: USED BASIC DEPTH FROM PREPROC.                        !

          DEPTH(ninf:nsup) = DEPTH_B(ninf:nsup)
          IF (ITEST.GE.3) THEN
             WRITE (IU06,*) '    SUB.PREPARE_FIRST_DEPTH:'
             WRITE (IU06,*) '        BASIC DEPTH FROM PREPROC'
             IF (USE_OASIS_ELEV_IN) THEN  !! ModR04: Include OASIS
                WRITE (IU06,*) '	 DEPTH FROM OASIS COUPLER'
             ELSE
                WRITE (IU06,*) '        DEPTH IS STATIONARY'
             END IF
          END IF

       ELSE IF (USE_OASIS_ELEV_IN) THEN  !! ModR04: Include OASIS

          IF (ITEST.GE.3) THEN
             WRITE (IU06,*) '    SUB.PREPARE_FIRST_DEPTH:'
             WRITE (IU06,*) '	 BASIC DEPTH FROM RESTART FILE'
             WRITE (IU06,*) '	 DEPTH FROM OASIS COUPLER'
          END IF

       ELSE

          !     3.2 DEPTH IS DEFINED FROM RESTART: USED DEPTH FROM RESTART.                        !

          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +      WARNING ERROR SUB. PREPARE_FIRST_DEPTH.     +'
          WRITE (IU06,*) ' +      =======================================     +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' + A RUN WITH STATIONARY DEPTH WAS REQUESTED.       +'
          WRITE (IU06,*) ' + (A TOPO FILE IS NOT DEFINED IN THE USER INPUT)   +'
          WRITE (IU06,*) ' + A DEPTH FIELD WAS FOUND IN THE RESTART FILE.     +'
          WRITE (IU06,*) ' + THIS DEPTH FIELD IS NOT THE BASIC DEPTH FIELD.   +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +               MODEL CONTINUES                    +'
          WRITE (IU06,*) ' +   USING DEPTH FROM RESTART FOR THE FULL RUN.     +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'

          IF (ITEST.GE.3) THEN
             WRITE (IU06,*) '    SUB.PREPARE_FIRST_DEPTH:'
             WRITE (IU06,*) '        DEPTH FROM RESTART FILE'
             WRITE (IU06,*) '        DEPTH IS STATIONARY'
          END IF
       END IF

       !CALL FIND_DRY_POINTS  !! ModR04: Redundant Part? Done later anyways.
       !IF (ITEST.GE.3) THEN
       !   WRITE (IU06,*) '    SUB. PREPARE_FIRST_DEPTH: FIND_DRY_POINTS DONE'
       !END IF
       !CALL PUT_DRY (FL3, 0.)
       !RETURN                !! ModR04: End redundant part

    ELSE  !! ModR04: i.e. when TOPO_RUN = .TRUE.

       ! ---------------------------------------------------------------------------- !
       !                                                                              !
       !     4. SHALLOW WATER RUN: A TOPO FILE EXISTS.                                !
       !        --------------------------------------                                !

       IF (COLDSTART) THEN

          !     4.1 COLD START: PREPARE FIRST DEPTH FIELD.                               !

          CDTA = CDATEA
          CALL WAM_TOPO (DEPTH, CDTA)
          IF (ITEST.GE.3) THEN
             WRITE (IU06,*) '    SUB.PREPARE_FIRST_DEPTH: WAM_TOPO DONE'
             WRITE (IU06,*) '        FIRST DEPTH FIELD PROCESSED'
             WRITE (IU06,*) '        DATE IS ................... CDTA = ', CDTA
          END IF
       ELSE

          !     4.2 HOT START.                                                            !

          IF (CDTA.EQ.' ') THEN

             !     4.2.1 FIRST DEPTH FIELD IS NOT IN RESTART: PREPARE FIRST DEPTH FIELD.     !

             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +      WARNING ERROR SUB. PREPARE_FIRST_DEPTH.     +'
             WRITE (IU06,*) ' +      =======================================     +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' + A HOT START FOR A MODEL RUN WITH DEPTH DIFFERENT +'
             WRITE (IU06,*) ' + FROM THE BASIC DEPTH IN PREPROC IS REQUESTED.    +'
             WRITE (IU06,*) ' + BUT THE FIRST DEPTH FIELD DOES NOT EXIST IN      +'
             WRITE (IU06,*) ' + THE RESTART FILE.                                +'
             WRITE (IU06,*) ' + (PREVIOUS RUN WAS WITH BASIC DEPTH)              +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +               MODEL CONTINUES                    +'
             WRITE (IU06,*) ' +     USING DEPTH FIELD FROM TOPO INPUT FILE       +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'

             CDTA = CDATEA
             CALL WAM_TOPO (DEPTH, CDTA)
             IF (ITEST.GE.3) THEN
                WRITE (IU06,*) '    SUB.PREPARE_FIRST_DEPTH: WAM_TOPO DONE'
                WRITE (IU06,*) '        FIRST DEPTH FIELD PROCESSED'
                WRITE (IU06,*) '        DATE IS ................... CDTA = ', CDTA
             END IF

          ELSE IF (CDTA.NE.CDATEA) THEN

             !     4.2.2 FIRST DEPTH FIELD IS IN RESTART DATES DO NOT MATCH.                 !

             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +      WARNING ERROR SUB. PREPARE_FIRST_DEPTH.     +'
             WRITE (IU06,*) ' +      =======================================     +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' + A HOT START FOR A MODEL RUN WITH DEPTH DIFFERENT +'
             WRITE (IU06,*) ' + FROM THE BASIC DEPTH IN PREPROC IS REQUESTED.    +'
             WRITE (IU06,*) ' + BUT THE DATE OF THE FIRST DEPTH FIELD IN THE     +'
             WRITE (IU06,*) ' + RESTART FILE IS NOT THE START DATE.              +'
             WRITE (IU06,*) ' + (PREVIOUS RUN WAS WITH STATIONARY DEPTH)         +'
             WRITE (IU06,*) ' + DEPTH DATE FROM RESTART IS CDTA = ', CDTA
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +               MODEL CONTINUES                    +'
             WRITE (IU06,*) ' +          CHANGING THE DEPTH DATE                 +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
             CDTA = CDATEA
             IF (ITEST.GE.3) THEN
                WRITE (IU06,*) '    SUB.PREPARE_FIRST_DEPTH:'
                WRITE (IU06,*) '        DEPTH FROM RESTART FILE'
                WRITE (IU06,*) '        DATE IS ................... CDTA = ', CDTA
             END IF
          END IF
       END IF

    END IF

    IF (USE_OASIS_ELEV_IN) THEN  !! ModR04: Include OASIS
       TOPO_RUN=.TRUE.
       !CDTA = CDATEA !! ModR07: Removed
    END IF

    CALL FIND_DRY_POINTS
    IF (ITEST.GE.3) THEN
       WRITE (IU06,*) '    SUB. PREPARE_FIRST_DEPTH: FIND_DRY_POINTS DONE'
    END IF
    CALL PUT_DRY (FL3, 0.)

  END SUBROUTINE PREPARE_FIRST_DEPTH

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE PREPARE_FIRST_CURRENT

    USE WAM_OASIS_MODULE,     ONLY: USE_OASIS_CURR_IN  !! ModR04: Include OASIS
    INTEGER  :: LEN

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. RUN WITHOUT CURRENT REFRACTION: RETURN.                               !
    !        ---------------------------------------                               !

    IF (.NOT. REFRACTION_C_RUN) THEN
       IF (USE_OASIS_CURR_IN) THEN  !! ModR04: Include OASIS

          !     1. RUN WITHOUT CURRENT REFRACTION BUT WITH COUPLING: ABORT.

          WRITE (IU06,*) ' ****************************************************'
          WRITE (IU06,*) ' *						  *'
          WRITE (IU06,*) ' *      FATAL ERROR SUB. PREPARE_FIRST_CURRENT.	  *'
          WRITE (IU06,*) ' *      =======================================	  *'
          WRITE (IU06,*) ' *						  *'
          WRITE (IU06,*) ' * CURRENT ONLINE COUPLING IS REQUESTED BUT         *'
          WRITE (IU06,*) ' * CURRENT REFRACTION IS NOT REQUESTED.		  *'
          WRITE (IU06,*) ' *						  *'
          WRITE (IU06,*) ' * SWITCH CURRENT REFRACTION ON IN THE USER INPUT   *'
          WRITE (IU06,*) ' * OR DISABLE CURRENT COUPLING IN THE               *'
          WRITE (IU06,*) ' * namcouple FILE					  *'
          WRITE (IU06,*) ' *						  *'
          WRITE (IU06,*) ' *       PROGRAM ABORTS  PROGRAM ABORTS		  *'
          WRITE (IU06,*) ' *						  *'
          WRITE (IU06,*) ' ****************************************************'
          CALL ABORT1
       END IF
       CURRENT_RUN = .FALSE.
       CDCA = ' '
       RETURN 
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. RUN WITH CURRENT REFRACTION:CHECK CURRENT FILE.                       !
    !        -----------------------------------------------                       !

    LEN = LEN_TRIM(FILE09)
    CURRENT_RUN = .FALSE.
    IF (LEN.GT.0) INQUIRE (FILE=FILE09(1:LEN), EXIST=CURRENT_RUN)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. COLD START.                                                           !
    !        -----------                                                           !

    IF (COLDSTART) THEN

       IF (.NOT. CURRENT_RUN .AND. .NOT. USE_OASIS_CURR_IN) THEN  !! ModR04: Only without OASIS

          !     3.1 COLD START AND FILE DOES NOT EXIST: ABORT.                           !

          WRITE (IU06,*) ' ****************************************************'
          WRITE (IU06,*) ' *                                                  *'
          WRITE (IU06,*) ' *      FATAL ERROR SUB. PREPARE_FIRST_CURRENT.     *'
          WRITE (IU06,*) ' *      =======================================     *'
          WRITE (IU06,*) ' *                                                  *'
          WRITE (IU06,*) ' * A CURRENT INPUT FILE IS NOT DEFINED IN THE USER  *'
          WRITE (IU06,*) ' * INPUT OR DOES NOT EXIST. BUT A RUN WITH CURRENT  *'
          WRITE (IU06,*) ' * REFRACTION IS REQUESTED.                         *'
          WRITE (IU06,*) ' *    FILE NAME IS     FILE09 = ', TRIM(FILE09)
          WRITE (IU06,*) ' *                                                  *'
          WRITE (IU06,*) ' * SWITCH OFF CURRENT REFRACTION OR		  *'
          WRITE (IU06,*) ' * CORRECT FILE NAME IN THE USER INPUT.             *'
          WRITE (IU06,*) ' *                                                  *'
          WRITE (IU06,*) ' *       PROGRAM ABORTS  PROGRAM ABORTS             *'
          WRITE (IU06,*) ' *                                                  *'
          WRITE (IU06,*) ' ****************************************************'
          CALL ABORT1
       ELSE IF (CURRENT_RUN) THEN  !! ModR04: Include OASIS

          !     3.2 COLD START AND FILE EXISTS: DO FIRST CURRENT FIELD.                  !

          CDCA = CDATEA
          CALL WAM_CURRENT (U, V, CDCA)
          IF (ITEST.GE.3) THEN
             WRITE (IU06,*) '  '
             WRITE (IU06,*) '    SUB. PREPARE_FIRST_CURRENT: WAM_CURRENT DONE'
             WRITE (IU06,*) '        FIRST CURRENT FIELD PROCESSED'
             WRITE (IU06,*) '        DATE IS ................... CDCA = ', CDCA
          END IF
          !RETURN !! ModR04: No return here when with OASIS?? 
       ELSE       !! ModR04: Include OASIS

          !     3.3 OASIS: ZERO CURRENT FOR PREPARATION.                                 !

          CURRENT_RUN = .TRUE.
          U = 0.
          V = 0.
          CDCA = CDATEA
          IF (ITEST.GE.3) THEN
             WRITE (IU06,*) '    SUB.PREPARE_FIRST_CURRENT:'
             WRITE (IU06,*) '	 SET FIRST CURRENT FIELD TO ZERO'
          END IF
       END IF
       !END IF
    ELSE !! ModR04: i.e., when HOTSTART

       ! ---------------------------------------------------------------------------- !
       !                                                                              !
       !     4. HOT START.                                                            !
       !        -----------                                                           !

       IF (.NOT. CURRENT_RUN) THEN

          !     4.1 HOT START AND FILE DOES NOT EXIST.                                   !

          IF (CDCA.EQ.' ' .AND. USE_OASIS_CURR_IN) THEN  !! ModR04: Include OASIS

             !     4.1.0 HOT START AND CURRENTS NOT IN RESTART BUT ONLINE COUPLNG: CONTINUE.!

             CURRENT_RUN = .TRUE.
             U = 0.
             V = 0.
             CDCA = CDATEA
             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +     WARNING ERROR SUB. PREPARE_FIRST_CURRENT.    +'
             WRITE (IU06,*) ' +     =========================================    +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' + A HOT START WITH CURRENT REFRACTION IS REQUESTED.+'
             WRITE (IU06,*) ' + NO CURRENT FIELD WAS FOUND IN THE RESTART FILE.  +'
             WRITE (IU06,*) ' + A CURRENT INPUT FILE IS NOT DEFINED IN THE USER  +'
             WRITE (IU06,*) ' + INPUT OR DOES NOT EXIST.                         +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +               MODEL CONTINUES ONLINE COUPLING    +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
             IF (ITEST.GE.3) THEN
                WRITE (IU06,*) '    SUB.PREPARE_FIRST_CURRENT:'
                WRITE (IU06,*) '	 SET FIRST CURRENT FIELD TO ZERO'
             END IF
          ELSE IF (CDCA.EQ.' ') THEN

             !     4.1.1 HOT START AND CURRENTS NOT IN RESTART: ABORT.                      !

             WRITE (IU06,*) ' ****************************************************'
             WRITE (IU06,*) ' *                                                  *'
             WRITE (IU06,*) ' *      FATAL ERROR SUB. PREPARE_FIRST_CURRENT.     *'
             WRITE (IU06,*) ' *      =======================================     *'
             WRITE (IU06,*) ' *                                                  *'
             WRITE (IU06,*) ' * A HOT START WITH CURRENT REFRACTION IS REQUESTED.*'
             WRITE (IU06,*) ' * A CURRENT INPUT FILE IS NOT DEFINED IN THE USER  *'
             WRITE (IU06,*) ' * INPUT OR DOES NOT EXIST AND A CURRENT FIELD DOES *'
             WRITE (IU06,*) ' * NOT EXIST IN THE RESTART FILE.                   *'
             WRITE (IU06,*) ' *    FILE NAME IS     FILE09 = ', TRIM(FILE09)
             WRITE (IU06,*) ' *                                                  *'
             WRITE (IU06,*) ' *       PROGRAM ABORTS  PROGRAM ABORTS             *'
             WRITE (IU06,*) ' *                                                  *'
             WRITE (IU06,*) ' ****************************************************'
             CALL ABORT1   
          ELSE 

             !     4.1.2 HOT START AND CURRENTS ARE IN RESTART: CONTINUE.                     !

             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +     WARNING ERROR SUB. PREPARE_FIRST_CURRENT.    +'
             WRITE (IU06,*) ' +     =========================================    +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' + A HOT START WITH CURRENT REFRACTION IS REQUESTED.+'
             WRITE (IU06,*) ' + A CURRENT INPUT FILE IS NOT DEFINED IN THE USER  +'
             WRITE (IU06,*) ' + INPUT OR DOES NOT EXIST.                         +'
             WRITE (IU06,*) ' + A CURRENT FIELD WAS FOUND IN THE RESTART FILE.   +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +               MODEL CONTINUES                    +'
             WRITE (IU06,*) ' +     USING CURRENT FIELD FROM RESTART FILE.       +'
             WRITE (IU06,*) ' +           CURRENTS ARE STATIONARY                +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
             IDELCI = 0
             IF (ITEST.GE.3) THEN
                WRITE (IU06,*) '  '
                WRITE (IU06,*) '    SUB. PREPARE_FIRST_CURRENT:'
                WRITE (IU06,*) '        FIRST CURRENT FIELD FROM RESTART'
                WRITE (IU06,*) '        DATE IS ................... CDCA = ', CDCA
             END IF
             RETURN
          END IF
          !END IF
       ELSE !! ModR04: i.e., when CURRENT_RUN

          !     4.2 HOT START AND FILE EXISTS.                                       !

          IF (CDCA.EQ.' ') THEN

             !     4.1.1 HOT START, CURRENTS ARE NOT IN RESTART: FIRST FIELD FROM FILE. !

             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +     WARNING ERROR SUB. PREPARE_FIRST_CURRENT.    +'
             WRITE (IU06,*) ' +     =========================================    +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' + A HOT START WITH CURRENT REFRACTION IS REQUESTED.+'
             WRITE (IU06,*) ' + THE FIRST CURRENT FIELD DOES NOT EXIST IN THE    +'
             WRITE (IU06,*) ' + RESTART FILE.                                    +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +               MODEL CONTINUES                    +'
             WRITE (IU06,*) ' + USING FIRST CURRENT FIELD FROM CURRENT INPUT FILE+'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          END IF

          !     4.1.1 HOT START, CURRENTS ARE IN RESTART: CHECK FILE DATE. !

          IF (CDCA.NE.' ' .AND. CDCA.NE.CDATEA) THEN
             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +     WARNING ERROR SUB. PREPARE_FIRST_CURRENT.    +'
             WRITE (IU06,*) ' +     =========================================    +'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' + A HOT START WITH CURRENT REFRACTION IS REQUESTED.+'
             WRITE (IU06,*) ' + THE DATE OF CURRENT FIELD IN THE RESTART FILE    +'
             WRITE (IU06,*) ' + IS NOT THE START DATE, BECAUSE THE PREVIOUS RUN  +'
             WRITE (IU06,*) ' + WAS WITH STATIONARY CURRENTS.                    +'
             WRITE (IU06,*) ' + CURRENT DATE FROM RESTART IS CDCA = ', CDCA
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' +               MODEL CONTINUES                    +'
             WRITE (IU06,*) ' + USING FIRST CURRENT FIELD FROM CURRENT INPUT FILE+'
             WRITE (IU06,*) ' +                                                  +'
             WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          END IF

          CDCA = CDATEA
          CALL WAM_CURRENT (U, V, CDCA)
          IF (ITEST.GE.3) THEN
             WRITE (IU06,*) '  '
             WRITE (IU06,*) '    SUB. PREPARE_FIRST_CURRENT: WAM_CURRENT DONE'
             WRITE (IU06,*) '        FIRST CURRENT FIELD PROCESSED'
             WRITE (IU06,*) '        DATE IS ................... CDCA = ', CDCA
          END IF

       END IF !! Hotstart: No Current_run VS Current_run 

    END IF !! Coldstart VS Hotstart

  END SUBROUTINE PREPARE_FIRST_CURRENT

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_INITIAL_MODULE
