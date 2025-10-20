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
END INTERFACE
PUBLIC PREPARE_START

INTERFACE READ_PREPROC_FILE           !! READS PREPROC OUTPUT FILE.
   MODULE PROCEDURE READ_PREPROC_FILE
END INTERFACE
PUBLIC READ_PREPROC_FILE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     E.  PRIVATE INTERFACES.                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE PREPARE_FIRST_DEPTH             !! PREPARES START FIELDS FOR WAMODEL.
   MODULE PROCEDURE PREPARE_FIRST_DEPTH
END INTERFACE
PRIVATE PREPARE_FIRST_DEPTH

INTERFACE PREPARE_FIRST_CURRENT             !! PREPARES START FIELDS FOR WAMODEL.
   MODULE PROCEDURE PREPARE_FIRST_CURRENT
END INTERFACE
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

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

 SUBROUTINE check(status, var_name)
    !----------------------------------------------------------------------------!
    !                                                                            !
    !   CHECK - Error handling for netCDF API                                    !
    !                                                                            !
    !     J. BENKE              FZJ       10/2025                                !
    !                                                                            !
    !----------------------------------------------------------------------------!
    
    use iso_fortran_env, only: stderr => error_unit

    implicit none

    integer, intent(in) :: status
    character(len = *), intent(in) :: var_name

    write(stderr, *) "Warning (",trim(var_name), "): ", TRIM(NF90_STRERROR(status))
    IF(status /= NF90_NOERR) THEN
       STOP "Error while netCDF operation ... Aborting!"
    END IF
  END SUBROUTINE check


   subroutine error_msg_allocation( error_msg )
    use iso_fortran_env, only: stderr => error_unit

    character(len = *), intent(in) :: error_msg
    integer                        :: status = 0

    if (status /= 0) then
       write(stderr, *) "Error while alllocating the array ", error_msg, ". STATUS =", status
       STOP "NETCDF ERROR"
    end if
  end subroutine error_msg_allocation


SUBROUTINE READ_PREPROC_FILE

USE WAM_OASIS_MODULE,     ONLY: USE_OASIS,WAM_OASIS_WRITE_GRID !! ModR04: Include OASIS
! ---------------------------------------------------------------------------- !
!                                                                              !
!   READ_PREPROC_FILE -  READ OUTPUT FILE FROM PREPROC.                        !
!                                                                              !
!     H. GUNTHER      GKSS/ECMWF      MAY 1990                                 !
!     H. GUNTHER      GKSS        OCTOBER 2000  FT90                           !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       INPUT OF PREPROC OUTPUT.                                               !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       UNFORMATTED READ FROM FILE07.                                          !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

use iso_fortran_env, only: stdout => output_unit, stderr => error_unit
!use netcdf

LOGICAL  :: L_OBSTRUCTION_T, debug = .true.

character(len = 80) :: FILE07_NC
character(len = 50), allocatable, dimension(:) :: name_of_dim

INTEGER  :: IOS = 0, LEN, I
integer  :: ncid, status
integer  :: n_dims, n_vars, n_attrs, k_un
integer, allocatable, dimension(:) :: id_of_dim

! Section 1 variable id definition
integer :: varid_header
    
! Section 2 variable id definition
integer :: varid_n_nest, varid_max_nest
integer :: varid_nbounc, varid_n_name, varid_n_code
integer :: varid_ijarc, varid_xdello, varid_xdella
integer :: varid_n_south, varid_n_north, varid_n_east, varid_n_west
integer :: varid_blngc, varid_blatc, varid_n_zdel

! Section 3 variable id definition
integer :: varid_nbounf, varid_nbinp, varid_c_name
integer :: varid_blngf, varid_blatf, varid_ijarf, varid_ibfl, varid_ibfr, varid_bfw

! Section 4 variable id definition
integer :: varid_ml, varid_kl
integer :: varid_fr, varid_dfim, varid_gom, varid_c, varid_delth, varid_deltr, varid_th
integer :: varid_costh, varid_sinth, varid_inv_log_co, varid_df, varid_df_fr, varid_df_fr2
integer :: varid_dfimofr

integer :: varid_dfim_fr, varid_dfim_fr2, varid_fr5, varid_frm5, varid_rhowg_dfim
integer :: varid_fmin, varid_mo_tail, varid_mm1_tail, varid_mp1_tail, varid_mp2_tail 
integer :: varid_mpm, varid_kpm, varid_jxo, varid_jyo

! Section 5 variable id definition
integer :: varid_nx, varid_ny, varid_nsea, varid_iper, varid_one_point
integer :: varid_reduced_grid, varid_l_obstruction_t, varid_nlon_rg, varid_delphi, varid_dellam 
integer :: varid_sinph, varid_cosph, varid_amowep, varid_amosop, varid_amoeap, varid_amonop

integer :: varid_zdello, varid_ixlg, varid_kxlt, varid_l_s_mask, varid_klat, varid_klon, varid_wlat
integer :: varid_depth_b, varid_obslat, varid_obslon

! section 6 variable id definition 
integer :: varid_ndepth, varid_deptha, varid_depthd, varid_depthe
integer :: varid_flminfr, varid_tcgond, varid_tfak, varid_tsihkd, varid_tfac_st, varid_t_tail
integer :: varid_delu

integer :: iper_tmp, one_point_tmp, reduced_grid_tmp, l_obstruction_t_tmp, l_s_mask_tmp


! ---------------------------------------------------------------------------- !
!                                                                              !
!     0. OPEN GRID_INFO FILE FROM PREPROC OUTPUT.                              !
!        ----------------------------------------                              !

LEN = LEN_TRIM(FILE07)
FILE07_NC = trim(FILE07) // ".nc"

! Open File
IOS = nf90_open(FILE07_NC, NF90_NOWRITE, ncid)
write(stdout, *) "FILE07_NC ",trim(FILE07_NC), " IOS = ", IOS     

IF (IOS .NE. 0) THEN
   WRITE (IU06,*) ' ****************************************************'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *     FATAL ERROR IN SUB. READ_PREPROC_FILE        *'
   WRITE (IU06,*) ' *     =====================================        *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' * PREPROC OUTPUT FILE COULD NOT BE OPENED          *'
   WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS                 
   WRITE (IU06,*) ' *    FILE NAME IS  FILE07 = ', FILE07_NC           
   WRITE (IU06,*) ' *    UNIT IS         IU07 = ', IU07                
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' ****************************************************'
   CALL ABORT1
END IF

call check( nf90_inquire( ncid, n_dims, n_vars, n_attrs, k_un ), "nf90_inquire" )
if(DEBUG .eqv. .true.) then
   write(stdout, *) "-- nf90_inquire start --"
   write(stdout, *) "n_dims = ", n_dims
   write(stdout, *) "n_vars = ", n_vars
   write(stdout, *) "n_attrs = ", n_attrs
   write(stdout, *) "k_un = ", k_un
   write(stdout, *) "-- nf90_inquire end --"
   write(stdout, *)
endif

! Create list of type dimension_attr and dimids                                                                                                                                     
if(.not. allocated(name_of_dim)) then
   allocate( name_of_dim(n_dims), stat = status)
   call error_msg_allocation( "name_of_dim" )
end if

if(.not. allocated(id_of_dim)) then
   allocate( id_of_dim(n_dims), stat = status)
   call error_msg_allocation( "id_of_dim" )
end if

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

call check( nf90_inq_dimid(ncid, name_of_dim(1), id_of_dim(1)), "nf90_inq_dim N_NEST" )
!    call check( nf90_inquire_dimension(ncid, id_of_dim(1), name_of_dim(1), len_of_dim(1)), "nf90_inq_dim N_NEST" )                                                                     
call check( nf90_inq_dimid(ncid, name_of_dim(2), id_of_dim(2)), "nf90_inq_dim ML" )
call check( nf90_inq_dimid(ncid, name_of_dim(3), id_of_dim(3)), "nf90_inq_dim KL" )
call check( nf90_inq_dimid(ncid, name_of_dim(4), id_of_dim(4)), "nf90_inq_dim NBOUNF" )
call check( nf90_inq_dimid(ncid, name_of_dim(5), id_of_dim(5)), "nf90_inq_dim NX" )
call check( nf90_inq_dimid(ncid, name_of_dim(6), id_of_dim(6)), "nf90_inq_dim NY" )
call check( nf90_inq_dimid(ncid, name_of_dim(7), id_of_dim(7)), "nf90_inq_dim DIM_NSEA" )
call check( nf90_inq_dimid(ncid, name_of_dim(8), id_of_dim(8)), "nf90_inq_dim JUMAX" )
call check( nf90_inq_dimid(ncid, name_of_dim(9), id_of_dim(9)), "nf90_inq_dim DIM_NDEPTH" )
call check( nf90_inq_dimid(ncid, name_of_dim(10), id_of_dim(10)), "nf90_inq_dim result_max_val" )
call check( nf90_inq_dimid(ncid, name_of_dim(11), id_of_dim(11)), "nf90_inq_dim DIM_THREE" )
call check( nf90_inq_dimid(ncid, name_of_dim(12), id_of_dim(12)), "nf90_inq_dim DIM_TWO" )
call check( nf90_inq_dimid(ncid, name_of_dim(13), id_of_dim(13)), "nf90_inq_dim STRINGLEN" )
call check( nf90_inq_dimid(ncid, name_of_dim(14), id_of_dim(14)), "nf90_inq_dim STRINGLEN_C_NAME" )



! section 1 nf90_inq_varid
call check( nf90_inq_varid(ncid, "header", varid_header ), "nf90_inq_varid " // "header")
call check( nf90_get_var(ncid, varid_header, header), "nf90_get_var header" )

!READ (IU07) HEADER

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. READ COARSE GRID BOUNDARY OUTPUT INFORMATION.                         !
!        ---------------------------------------------                         !

    ! section 2 nf90_inq_varid
call check( nf90_inq_varid(ncid, "n_nest", varid_n_nest ), "nf90_inq_varid " // "n_nest")
call check( nf90_inq_varid(ncid, "max_nest", varid_max_nest ), "nf90_inq_varid " // "max_nest")

call check( nf90_get_var(ncid, varid_n_nest, n_nest), "nf90_get_var n_nest" )
call check( nf90_get_var(ncid, varid_max_nest, max_nest), "nf90_get_var max_nest" )


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

call check( nf90_inq_varid(ncid, "nbounc", varid_nbounc ), "nf90_inq_varid " // "nbounc")
call check( nf90_inq_varid(ncid, "n_name", varid_n_name ), "nf90_inq_varid " // "n_name")
call check( nf90_inq_varid(ncid, "n_code", varid_n_code ), "nf90_inq_varid " // "n_code")

call check( nf90_get_var(ncid, varid_nbounc, nbounc), "nf90_get_var nbounc" )
call check( nf90_get_var(ncid, varid_n_name, n_name), "nf90_get_var n_name" )
call check( nf90_get_var(ncid, varid_n_code, n_code), "nf90_get_var n_code" )

if(DEBUG .eqv. .true.) then
   write( stdout, *) "------------------- Output of Variables ------------------"
   write( stdout, *) "After reading header = ", trim(header)
   write( stdout, *) "After reading n_nest = ", n_nest
   write( stdout, *) "After reading max_nest = ", max_nest
   write( stdout, *) "After reading nbounc = ", nbounc
   write( stdout, *) "After reading n_name = ", trim(n_name(1)), trim(n_name(2))
   write( stdout, *) "After reading n_code = ", n_code
endif


if( maxval(NBOUNC) > 0 ) then
   call check( nf90_inq_varid(ncid, "ijarc", varid_ijarc ), "nf90_inq_varid " // "ijarc")
   call check( nf90_inq_varid(ncid, "xdello", varid_xdello ), "nf90_inq_varid " // "xdello")
   call check( nf90_inq_varid(ncid, "xdella", varid_xdella ), "nf90_inq_varid " // "xdella")
   call check( nf90_inq_varid(ncid, "n_south", varid_n_south ), "nf90_inq_varid " // "n_south")

   call check( nf90_inq_varid(ncid, "n_north", varid_n_north ), "nf90_inq_varid " // "n_north")
   call check( nf90_inq_varid(ncid, "n_east", varid_n_east ), "nf90_inq_varid " // "n_east")
   call check( nf90_inq_varid(ncid, "n_west", varid_n_west ), "nf90_inq_varid " // "n_west")
   call check( nf90_inq_varid(ncid, "blngc", varid_blngc ), "nf90_inq_varid " // "blngc")
   call check( nf90_inq_varid(ncid, "blatc", varid_blatc ), "nf90_inq_varid " // "blatc")

   call check( nf90_inq_varid(ncid, "n_zdel", varid_n_zdel ), "nf90_inq_varid " // "n_zdel")

   call check( nf90_get_var(ncid, varid_ijarc, ijarc), "nf90_get_var ijarc" )
   call check( nf90_get_var(ncid, varid_xdello, xdello), "nf90_get_var xdello" )
   call check( nf90_get_var(ncid, varid_xdella, xdella), "nf90_get_var xdella" )
   call check( nf90_get_var(ncid, varid_n_south, n_south), "nf90_get_var n_south" )
   call check( nf90_get_var(ncid, varid_n_north, n_north), "nf90_get_var n_north" )
   call check( nf90_get_var(ncid, varid_n_east, n_east), "nf90_get_var n_east" )
   call check( nf90_get_var(ncid, varid_n_west, n_west), "nf90_get_var n_wnest" )
   call check( nf90_get_var(ncid, varid_blngc, blngc), "nf90_get_var blngc" )
   call check( nf90_get_var(ncid, varid_blatc, blatc), "nf90_get_var blatc" )
   call check( nf90_get_var(ncid, varid_n_zdel, n_zdel), "nf90_get_var n_zdel" )

   if(DEBUG .eqv. .true.) then
      write( stdout, *) "After reading IJARC = ", ijarc
      write( stdout, *) "After reading xdello = ", xdello
      write( stdout, *) "After reading xdella = ", xdella
      write( stdout, *) "After reading n_south = ", n_south
      write( stdout, *) "After reading n_north = ", n_north
      write( stdout, *) "After reading n_south = ", n_east
      write( stdout, *) "After reading n_north = ", n_west
      write( stdout, *) "After reading blngc = ", blngc
      write( stdout, *) "After reading blatc = ", blatc
      write( stdout, *) "After reading n_zdel = ", n_zdel
   endif
end if


    ! DO I=1,N_NEST
!   READ(IU07) NBOUNC(I), N_NAME(I), n_code(i)
!   IF (NBOUNC(I).GT.0) THEN
!      READ(IU07) IJARC(1:NBOUNC(I),I)
!      READ(IU07) XDELLO, XDELLA, N_SOUTH(I), N_NORTH(I), N_EAST(I), N_WEST(I), &
!&                BLNGC(1:NBOUNC(I),I), BLATC(1:NBOUNC(I),I), N_ZDEL(1:NBOUNC(I),I)
!   END IF
! END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. READ FINE GRID BOUNDARY INPUT INFORMATION.                            !
!        ------------------------------------------                            !

!READ (UNIT=IU07) NBOUNF, NBINP, C_NAME

call check( nf90_inq_varid(ncid, "nbounf", varid_nbounf ), "nf90_inq_varid " // "nbounf")
call check( nf90_inq_varid(ncid, "nbinp", varid_nbinp ), "nf90_inq_varid " // "nbinp")
call check( nf90_inq_varid(ncid, "c_name", varid_c_name ), "nf90_inq_varid " // "c_name")

call check( nf90_get_var(ncid, varid_nbounf, nbounf), "nf90_get_var nbounf" )
call check( nf90_get_var(ncid, varid_nbinp, nbinp), "nf90_get_var nbinp" )
call check( nf90_get_var(ncid, varid_c_name, c_name), "nf90_get_var c_name" )

if(DEBUG .eqv. .true.) then
   write( stdout, *) "After reading nbouf = ", nbounf
   write( stdout, *) "After reading nbinp = ", nbinp
   write( stdout, *) "After reading c_name = ", c_name
   write( stdout, *)     
endif


IF (NBOUNF.GT.0) THEN
   IF (.NOT.ALLOCATED(BLNGF)) ALLOCATE (BLNGF(NBOUNF))
   IF (.NOT.ALLOCATED(BLATF)) ALLOCATE (BLATF(NBOUNF))
   IF (.NOT.ALLOCATED(IJARF)) ALLOCATE (IJARF(NBOUNF))
   IF (.NOT.ALLOCATED(IBFL )) ALLOCATE (IBFL(NBOUNF))
   IF (.NOT.ALLOCATED(IBFR )) ALLOCATE (IBFR(NBOUNF))
   IF (.NOT.ALLOCATED(BFW  )) ALLOCATE (BFW(NBOUNF))

   call check( nf90_inq_varid(ncid, "blngf", varid_blngf ), "nf90_inq_varid " // "blngf")
   call check( nf90_inq_varid(ncid, "blatf", varid_blatf ), "nf90_inq_varid " // "blatf")
   call check( nf90_inq_varid(ncid, "ijarf", varid_ijarf ), "nf90_inq_varid " // "ijarf")
   call check( nf90_inq_varid(ncid, "ibfl", varid_ibfl ), "nf90_inq_varid " // "ibfl")
   call check( nf90_inq_varid(ncid, "ibfr", varid_ibfr ), "nf90_inq_varid " // "ibfr")
   call check( nf90_inq_varid(ncid, "bfw", varid_bfw ), "nf90_inq_varid " // "bfw")

   call check( nf90_get_var(ncid, varid_blngf, blngf), "nf90_get_var blngf" )
   call check( nf90_get_var(ncid, varid_blatf, blatf), "nf90_get_var blatf" )
   call check( nf90_get_var(ncid, varid_ijarf, ijarf), "nf90_get_var ijarf" )
   call check( nf90_get_var(ncid, varid_ibfl, ibfl), "nf90_get_var ibfl" )
   call check( nf90_get_var(ncid, varid_ibfr, ibfr), "nf90_get_var ibfr" )
   call check( nf90_get_var(ncid, varid_bfw, bfw), "nf90_get_var bfw" )

   if(DEBUG .eqv. .true.) then
      write( stdout, *) "After reading blngf = ", blngf
      write( stdout, *) "After reading blatf = ", blatf
      write( stdout, *) "After reading ijarf = ", ijarf
      write( stdout, *) "After reading ibfl = ", ibfl
      write( stdout, *) "After reading ibfr = ", ibfr
      write( stdout, *) "After reading bfw = ", bfw
   end if

   !   READ (IU07) BLNGF(1:NBOUNF), BLATF(1:NBOUNF), IJARF(1:NBOUNF),              &
   !&             IBFL(1:NBOUNF), IBFR(1:NBOUNF), BFW(1:NBOUNF)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. READ FREQUENCY DIRECTION GRID.                                        !
!        ------------------------------                                        !

!READ (IU07) ML, KL

call check( nf90_inq_varid(ncid, "ml", varid_ml ), "nf90_inq_varid " // "ml")
call check( nf90_inq_varid(ncid, "kl", varid_kl ), "nf90_inq_varid " // "kl")

call check( nf90_get_var(ncid, varid_ml, ml), "nf90_get_var ml" )
call check( nf90_get_var(ncid, varid_kl, kl), "nf90_get_var kl" )


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

!READ (IU07)  FR, DFIM, GOM, C, DELTH, DELTR, TH, COSTH, SINTH, INV_LOG_CO,     &
!&            DF, DF_FR, DF_FR2, DFIM, DFIMOFR, DFIM_FR, DFIM_FR2, FR5, FRM5,   &
!&            RHOWG_DFIM,                                                       &
!&            FMIN, MO_TAIL, MM1_TAIL, MP1_TAIL, MP2_TAIL

  call check( nf90_inq_varid(ncid, "ml", varid_ml ), "nf90_inq_varid " // "ml")
    call check( nf90_inq_varid(ncid, "kl", varid_kl ), "nf90_inq_varid " // "kl")
    call check( nf90_inq_varid(ncid, "fr", varid_fr ), "nf90_inq_varid " // "fr")
    call check( nf90_inq_varid(ncid, "dfim", varid_dfim ), "nf90_inq_varid " // "dfim")
    call check( nf90_inq_varid(ncid, "gom", varid_gom ), "nf90_inq_varid " // "gom")

    call check( nf90_inq_varid(ncid, "c", varid_c ), "nf90_inq_varid " // "c")
    call check( nf90_inq_varid(ncid, "delth", varid_delth ), "nf90_inq_varid " // "delth")
    call check( nf90_inq_varid(ncid, "deltr", varid_deltr ), "nf90_inq_varid " // "deltr")
    call check( nf90_inq_varid(ncid, "th", varid_th ), "nf90_inq_varid " // "th")
    call check( nf90_inq_varid(ncid, "costh", varid_costh ), "nf90_inq_varid " // "costh")

    call check( nf90_inq_varid(ncid, "sinth", varid_sinth ), "nf90_inq_varid " // "sinth")
    call check( nf90_inq_varid(ncid, "inv_log_co", varid_inv_log_co ), "nf90_inq_varid " // "inv_log_co")
    call check( nf90_inq_varid(ncid, "df", varid_df ), "nf90_inq_varid " // "df")
    call check( nf90_inq_varid(ncid, "df_fr", varid_df_fr ), "nf90_inq_varid " // "df_fr")
    call check( nf90_inq_varid(ncid, "df_fr2", varid_df_fr2 ), "nf90_inq_varid " // "df_fr2")

    call check( nf90_inq_varid(ncid, "dfimofr", varid_dfimofr ), "nf90_inq_varid " // "dfimofr" )
    call check( nf90_inq_varid(ncid, "dfim_fr", varid_dfim_fr ), "nf90_inq_varid " // "dfim_fr")
    call check( nf90_inq_varid(ncid, "dfim_fr2", varid_dfim_fr2 ), "nf90_inq_varid " // "dfim_fr2")
    call check( nf90_inq_varid(ncid, "fr5", varid_fr5 ), "nf90_inq_varid " // "fr5")
    call check( nf90_inq_varid(ncid, "frm5", varid_frm5 ), "nf90_inq_varid " // "frm5")
    call check( nf90_inq_varid(ncid, "rhowg_dfim", varid_rhowg_dfim ), "nf90_inq_varid " // "rhowg_dfim")

    call check( nf90_inq_varid(ncid, "fmin", varid_fmin ), "nf90_inq_varid " // "fmin")
    call check( nf90_inq_varid(ncid, "mo_tail", varid_mo_tail ), "nf90_inq_varid " // "mo_tail")
    call check( nf90_inq_varid(ncid, "mm1_tail", varid_mm1_tail ), "nf90_inq_varid " // "mm1_tail")
    call check( nf90_inq_varid(ncid, "mp1_tail", varid_mp1_tail ), "nf90_inq_varid " // "mp1_tail")
    call check( nf90_inq_varid(ncid, "mp2_tail", varid_mp2_tail ), "nf90_inq_varid " // "mp2_tail")

       call check( nf90_get_var(ncid, varid_ml, ml), "nf90_get_var ml" )
    call check( nf90_get_var(ncid, varid_kl, kl), "nf90_get_var kl" )
    call check( nf90_get_var(ncid, varid_fr, fr), "nf90_get_var fr" )
    call check( nf90_get_var(ncid, varid_dfim, dfim), "nf90_get_var dfim" )
    call check( nf90_get_var(ncid, varid_gom, gom), "nf90_get_var gom" )
 
    call check( nf90_get_var(ncid, varid_c, c), "nf90_get_var c" )
    call check( nf90_get_var(ncid, varid_delth, delth), "nf90_get_var delth" )
    call check( nf90_get_var(ncid, varid_deltr, deltr), "nf90_get_var deltr" )
    call check( nf90_get_var(ncid, varid_th, th), "nf90_get_var th" )
    call check( nf90_get_var(ncid, varid_costh, costh), "nf90_get_var costh" )

    call check( nf90_get_var(ncid, varid_sinth, sinth), "nf90_get_var sinth" )
    call check( nf90_get_var(ncid, varid_inv_log_co, inv_log_co), "nf90_get_var inv_log_co" )
    call check( nf90_get_var(ncid, varid_df, df), "nf90_get_var df" )
    call check( nf90_get_var(ncid, varid_df_fr, df_fr), "nf90_get_var df_fr" )
    call check( nf90_get_var(ncid, varid_df_fr2, df_fr2), "nf90_get_var df_fr2" )
    
    call check( nf90_get_var(ncid, varid_dfimofr, dfimofr), "nf90_get_var dfimofr" )
    call check( nf90_get_var(ncid, varid_dfim_fr, dfim_fr), "nf90_get_var dfim_fr" )
    call check( nf90_get_var(ncid, varid_dfim_fr2, dfim_fr2), "nf90_get_var dfim_fr2" )
    call check( nf90_get_var(ncid, varid_fr5, fr5), "nf90_get_var fr5" )
    call check( nf90_get_var(ncid, varid_frm5, frm5), "nf90_get_var frm5" )

    call check( nf90_get_var(ncid, varid_rhowg_dfim, rhowg_dfim), "nf90_get_var rhowg_dfim" )
    call check( nf90_get_var(ncid, varid_fmin, fmin), "nf90_get_var fmin" )
    call check( nf90_get_var(ncid, varid_mo_tail, mo_tail), "nf90_get_var mo_tail" )
    call check( nf90_get_var(ncid, varid_mm1_tail, mm1_tail), "nf90_get_var mm1_tail" )
    call check( nf90_get_var(ncid, varid_mp1_tail, mp1_tail), "nf90_get_var mp1_tail" )

    call check( nf90_get_var(ncid, varid_mp2_tail, mp2_tail), "nf90_get_var mp2_tail" )

!READ (IU07)  MPM, KPM, JXO, JYO

    call check( nf90_inq_varid(ncid, "mpm", varid_mpm ), "nf90_inq_varid " // "mpm")
    call check( nf90_inq_varid(ncid, "kpm", varid_kpm ), "nf90_inq_varid " // "kpm")
    call check( nf90_inq_varid(ncid, "jxo", varid_jxo ), "nf90_inq_varid " // "jxo")
    call check( nf90_inq_varid(ncid, "jyo", varid_jyo ), "nf90_inq_varid " // "jyo")

    call check( nf90_get_var(ncid, varid_mpm, mpm), "nf90_get_var mpm" )
    call check( nf90_get_var(ncid, varid_kpm, kpm), "nf90_get_var kpm" )
    call check( nf90_get_var(ncid, varid_jxo, jxo), "nf90_get_var jxo" )
    call check( nf90_get_var(ncid, varid_jyo, jyo), "nf90_get_var jyo" )

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

    
    
! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. READ GRID INFORMATION.                                                !
!        ----------------------                                                !

! READ (IU07) NX, NY, NSEA, IPER, ONE_POINT, REDUCED_GRID, L_OBSTRUCTION_T
    
    call check( nf90_inq_varid(ncid, "nx", varid_nx ), "nf90_inq_varid " // "nx")
    call check( nf90_inq_varid(ncid, "ny", varid_ny ), "nf90_inq_varid " // "ny")
    call check( nf90_inq_varid(ncid, "nsea", varid_nsea ), "nf90_inq_varid " // "nsea")
    call check( nf90_inq_varid(ncid, "iper", varid_iper ), "nf90_inq_varid " // "iper")
    call check( nf90_inq_varid(ncid, "one_point", varid_one_point ), "nf90_inq_varid " // "one_point")

    call check( nf90_inq_varid(ncid, "reduced_grid", varid_reduced_grid ), "nf90_inq_varid " // "reduced_grid")
    call check( nf90_inq_varid(ncid, "l_obstruction_t", varid_l_obstruction_t ), "nf90_inq_varid " // "l_obstruction_t")
    if( l_obstruction_t .eqv. .true. ) then
       call check( nf90_inq_varid(ncid, "obslat", varid_obslat ), "nf90_inq_varid " // "obslat")
       call check( nf90_inq_varid(ncid, "obslon", varid_obslon ), "nf90_inq_varid " // "obslon")
    endif


    call check( nf90_get_var(ncid, varid_nx, nx), "nf90_get_var nx" )
    call check( nf90_get_var(ncid, varid_ny, ny), "nf90_get_var ny" )
    call check( nf90_get_var(ncid, varid_nsea, nsea), "nf90_get_var nsea" )
    call check( nf90_get_var(ncid, varid_iper, iper_tmp), "nf90_get_var iper" )
    iper = merge(.TRUE., .FALSE., iper_tmp /= 0)
    call check( nf90_get_var(ncid, varid_one_point, one_point_tmp), "nf90_get_var one_point" )
    one_point = merge(.TRUE., .FALSE., one_point_tmp /= 0)
    call check( nf90_get_var(ncid, varid_reduced_grid, reduced_grid_tmp), "nf90_get_var reduced_grid" )
    reduced_grid = merge(.TRUE., .FALSE., reduced_grid_tmp /= 0)
    call check( nf90_get_var(ncid, varid_l_obstruction_t, l_obstruction_t_tmp), "nf90_get_var l_obstruction_t" )
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

if( l_obstruction_t .eqv. .true. ) then
   call check( nf90_inq_varid(ncid, "obslat", varid_obslat ), "nf90_inq_varid " // "obslat")
   call check( nf90_inq_varid(ncid, "obslon", varid_obslon ), "nf90_inq_varid " // "obslon")

   call check( nf90_get_var(ncid, varid_obslat, obslat), "nf90_get_var obslat" )
   call check( nf90_get_var(ncid, varid_obslon, obslon), "nf90_get_var obslon" )

   if(DEBUG .eqv. .true.) then
      write( stdout, *) "After reading obslat = ", obslat
      write( stdout, *) "After reading obslon = ", obslon
      write( stdout, *)
   endif
end if

! -----------

call check( nf90_inq_varid(ncid, "nlon_rg", varid_nlon_rg ), "nf90_inq_varid " // "nlon_rg")
call check( nf90_inq_varid(ncid, "delphi", varid_delphi ), "nf90_inq_varid " // "delphi")
call check( nf90_inq_varid(ncid, "dellam", varid_dellam ), "nf90_inq_varid " // "dellam")

call check( nf90_inq_varid(ncid, "sinph", varid_sinph ), "nf90_inq_varid " // "sinph")
call check( nf90_inq_varid(ncid, "cosph", varid_cosph ), "nf90_inq_varid " // "cosph")
call check( nf90_inq_varid(ncid, "amowep", varid_amowep ), "nf90_inq_varid " // "amowep")
call check( nf90_inq_varid(ncid, "amosop", varid_amosop ), "nf90_inq_varid " // "amosop")
call check( nf90_inq_varid(ncid, "amoeap", varid_amoeap ), "nf90_inq_varid " // "amoeap")
call check( nf90_inq_varid(ncid, "amonop", varid_amonop ), "nf90_inq_varid " // "amonop")

call check( nf90_inq_varid(ncid, "zdello", varid_zdello ), "nf90_inq_varid " // "zdello")
call check( nf90_inq_varid(ncid, "ixlg", varid_ixlg ), "nf90_inq_varid " // "ixlg")
call check( nf90_inq_varid(ncid, "kxlt", varid_kxlt ), "nf90_inq_varid " // "kxlt")
call check( nf90_inq_varid(ncid, "l_s_mask", varid_l_s_mask ), "nf90_inq_varid " // "l_s_mask")
call check( nf90_inq_varid(ncid, "klat", varid_klat ), "nf90_inq_varid " // "klat")
call check( nf90_inq_varid(ncid, "klon", varid_klon ), "nf90_inq_varid " // "klon")
call check( nf90_inq_varid(ncid, "wlat", varid_wlat ), "nf90_inq_varid " // "wlat")
call check( nf90_inq_varid(ncid, "depth_b", varid_depth_b ), "nf90_inq_varid " // "depth_b")


call check( nf90_get_var(ncid, varid_nlon_rg, nlon_rg), "nf90_get_var nlon_rg" )
call check( nf90_get_var(ncid, varid_delphi, delphi), "nf90_get_var delphi" )
call check( nf90_get_var(ncid, varid_dellam, dellam), "nf90_get_var dellam" )
call check( nf90_get_var(ncid, varid_sinph, sinph), "nf90_get_var sinph" )
call check( nf90_get_var(ncid, varid_cosph, cosph), "nf90_get_var cosph" )
call check( nf90_get_var(ncid, varid_amowep, amowep), "nf90_get_var amowep" )

call check( nf90_get_var(ncid, varid_amosop, amosop), "nf90_get_var amosop" )
call check( nf90_get_var(ncid, varid_amoeap, amoeap), "nf90_get_var amoeap" )
call check( nf90_get_var(ncid, varid_amonop, amonop), "nf90_get_var amonop" )
call check( nf90_get_var(ncid, varid_xdello, xdello), "nf90_get_var xdello" )
call check( nf90_get_var(ncid, varid_xdella, xdella), "nf90_get_var xdella" )
call check( nf90_get_var(ncid, varid_zdello, zdello), "nf90_get_var zdello" )
call check( nf90_get_var(ncid, varid_ixlg, ixlg), "nf90_get_var ixlg" )

call check( nf90_get_var(ncid, varid_kxlt, kxlt), "nf90_get_var kxlt" )
call check( nf90_get_var(ncid, varid_l_s_mask, l_s_mask_tmp), "nf90_get_var l_s_mask" )
l_s_mask = merge(.TRUE., .FALSE., l_s_mask_tmp /= 0)
call check( nf90_get_var(ncid, varid_klat, klat), "nf90_get_var klat" )
call check( nf90_get_var(ncid, varid_klon, klon), "nf90_get_var klon" )
call check( nf90_get_var(ncid, varid_wlat, wlat), "nf90_get_var wlat" )
call check( nf90_get_var(ncid, varid_depth_b, depth_b), "nf90_get_var depth_b" )

if( L_OBSTRUCTION_T .eqv. .TRUE.) then
   call check( nf90_inq_varid(ncid, "obslat", varid_obslat ), "nf90_inq_varid " // "obslat")
   call check( nf90_inq_varid(ncid, "obslon", varid_obslon ), "nf90_inq_varid " // "obslon")
   
   call check( nf90_get_var(ncid, varid_obslat, obslat), "nf90_get_var obslat" )
   call check( nf90_get_var(ncid, varid_obslon, obslon), "nf90_get_var obslon" )

   IF (.NOT.L_OBSTRUCTION) THEN
      OBSLAT = 1.
      OBSLON = 1.
   END IF

   if(DEBUG .eqv. .true.) then
      write( stdout, *) "After reading obslat = ", obslat
      write( stdout, *) "After reading obslon = ", obslon
      write( stdout, *)
   endif
else
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


! READ (IU07) NLON_RG
!READ (IU07) DELPHI, DELLAM, SINPH, COSPH, AMOWEP, AMOSOP, AMOEAP, AMONOP,      &
!&           XDELLA, XDELLO, ZDELLO
!READ (IU07) IXLG, KXLT, L_S_MASK
!READ (IU07) KLAT, KLON, WLAT, DEPTH_B
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
! END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     7. READ TABLES.                                                          !
!        ------------                                                          !

! READ (IU07) NDEPTH, DEPTHA, DEPTHD, DEPTHE
call check( nf90_inq_varid(ncid, "ndepth", varid_ndepth ), "nf90_inq_varid " // "ndepth")
call check( nf90_inq_varid(ncid, "deptha", varid_deptha ), "nf90_inq_varid " // "deptha")
call check( nf90_inq_varid(ncid, "depthd", varid_depthd ), "nf90_inq_varid " // "depthd")
call check( nf90_inq_varid(ncid, "depthe", varid_depthe ), "nf90_inq_varid " // "depthe")

call check( nf90_get_var(ncid, varid_ndepth, ndepth), "nf90_get_var ndepth" )
call check( nf90_get_var(ncid, varid_deptha, deptha), "nf90_get_var deptha" )
call check( nf90_get_var(ncid, varid_depthd, depthd), "nf90_get_var depthd" )
call check( nf90_get_var(ncid, varid_depthe, depthe), "nf90_get_var depthe" )


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

call check( nf90_inq_varid(ncid, "flminfr", varid_flminfr ), "nf90_inq_varid " // "flminfr")
call check( nf90_inq_varid(ncid, "tcgond", varid_tcgond ), "nf90_inq_varid " // "tcgond")
call check( nf90_inq_varid(ncid, "tfak", varid_tfak ), "nf90_inq_varid " // "tfak")
call check( nf90_inq_varid(ncid, "tsihkd", varid_tsihkd ), "nf90_inq_varid " // "tsihkd")
call check( nf90_inq_varid(ncid, "tfac_st", varid_tfac_st ), "nf90_inq_varid " // "tfac_st")
call check( nf90_inq_varid(ncid, "t_tail", varid_t_tail ), "nf90_inq_varid " // "t_tail")
call check( nf90_inq_varid(ncid, "delu", varid_delu ), "nf90_inq_varid " // "delu")

call check( nf90_get_var(ncid, varid_flminfr, flminfr), "nf90_get_var flminfr" )
call check( nf90_get_var(ncid, varid_tcgond, tcgond), "nf90_get_var tcgond" )
call check( nf90_get_var(ncid, varid_tfak, tfak), "nf90_get_var tfak" )
call check( nf90_get_var(ncid, varid_tsihkd, tsihkd), "nf90_get_var tsihkd" )
call check( nf90_get_var(ncid, varid_tfac_st, tfac_st), "nf90_get_var tfac_st" )
call check( nf90_get_var(ncid, varid_t_tail, t_tail), "nf90_get_var t_tail" )
call check( nf90_get_var(ncid, varid_delu, delu), "nf90_get_var delu" )

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



! READ (IU07) FLMINFR, TCGOND, TFAK, TSIHKD, TFAC_ST, T_TAIL
! READ (IU07) DELU

! ---------------------------------------------------------------------------- !
!                                                                              !
!     8. CLOSE FILE AND RETURN.                                                !
!        ----------------------                                                !

call check( nf90_close(ncid), "NF90_CLOSE" )

!CLOSE (UNIT=IU07, STATUS='KEEP')

IF(USE_OASIS)CALL WAM_OASIS_WRITE_GRID  !! ModR04: Include OASIS

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
