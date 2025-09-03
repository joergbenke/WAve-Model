MODULE PREPROC_MODULE

  ! ---------------------------------------------------------------------------- !
  !                                                                              !
  !   THIS MODULE CONTAINS ALL VARIABLES AND CONSTANTS NECESSARY FOR THE         !
  !   PREPROC PROGRAM. ALL PROCEDURES ARE INCLUDED TO COMPUTE THE INFORMATION    !
  !   STORED IN WAM_CONST_MODULE, WAM_TABLE_MODULE WAM_NEST_MODULE AND           !
  !   WAM_GRID_MODULE.                                                           !
  !                                                                              !
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     A.  EXTERNALS.                                                           !
  !                                                                              !
  ! ---------------------------------------------------------------------------- !

  USE netcdf

  USE WAM_COORDINATE_MODULE          !! COORDINATE TYPE AND PROCEDURES

  USE WAM_GENERAL_MODULE,   ONLY:  &
       &       AKI,                     &
       &       ABORT1,                  & !! TERMINATES PROCESSING.
       &       PRINT_ARRAY,             & !! PRINTS AN ARRAY.
       &       PRINT_GENERAL_MODULE

  USE WAM_GRID_MODULE,      ONLY:  &
       &       EQUAL_TO_M_GRID,         & !! COMPARES TWO GRIDS.
       &       PRINT_GRID_STATUS          !! PRINTS THE MODULE INFORMATION.

  USE WAM_FRE_DIR_MODULE,   ONLY:  &
       &       PRINT_FRE_DIR_STATUS       !! PRINTS THE MODULE INFORMATION.

  USE WAM_NEST_MODULE,  ONLY:      &
       &       PREPARE_BOUNDARY_nest,   & !! MAKES NEST BOUNDARIES.
       &       PRINT_NEST_STATUS          !! PRINTS THE MODULE INFORMATION.

  USE WAM_TABLES_MODULE,    ONLY:  &
       &       PRINT_TABLES_STATUS        !! PRINTS THE MODULE INFORMATION.

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     B. VARIABLES FROM OTHER MODULES.                                         !
  !                                                                              !
  ! ---------------------------------------------------------------------------- !

  USE WAM_FILE_MODULE,    ONLY: IU06, ITEST, IU07, FILE07

  USE WAM_GENERAL_MODULE, ONLY: ZPI, DEG, RAD, CIRC

  USE WAM_FRE_DIR_MODULE, ONLY: KL, ML, FR, CO, TH, DELTH, DELTR, COSTH, SINTH,  &
       &                             GOM, C, INV_LOG_CO,                              &
       &                             DF, DF_FR, DF_FR2,                               &
       &                             DFIM, DFIMOFR, DFIM_FR, DFIM_FR2, FR5, FRM5,     &
       &                             RHOWG_DFIM,                                      &
       &                             FMIN, MO_TAIL, MM1_TAIL, MP1_TAIL, MP2_TAIL,     &
       &                             MPM, KPM, JXO, JYO

  USE WAM_GRID_MODULE,    ONLY: HEADER, NX, NY, NSEA, NLON_RG, IPER,             &
       &                             AMOWEP, AMOSOP, AMOEAP, AMONOP,                  &
       &                             XDELLA, XDELLO, DELLAM, ZDELLO, DELPHI,          &
       &                             SINPH, COSPH, DEPTH_B, KLAT, KLON, WLAT,         &
       &                             IXLG, KXLT, L_S_MASK, ONE_POINT, REDUCED_GRID,   &
       &                             OBSLAT, OBSLON

  USE WAM_NEST_MODULE,    ONLY: N_NEST, MAX_NEST, N_NAME, n_code,                &
       &                             NBOUNC, IJARC, BLATC, BLNGC, DLAMAC, DPHIAC,     &
       &                             N_SOUTH, N_NORTH, N_EAST, N_WEST, N_ZDEL,        &
       &                             NBINP, NBOUNF, C_NAME, BLNGF, BLATF,             &
       &                             IJARF, IBFL, IBFR, BFW

  USE WAM_TABLES_MODULE,  ONLY: NDEPTH, DEPTHA, DEPTHD, DEPTHE,                  &
       &                             FLMINFR, TCGOND, TFAK, TSIHKD, TFAC_ST, T_TAIL,  &
       &                             DELU, JUMAX

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     C. MODULE VARIABLES.                                                     !
  !                                                                              !
  ! ---------------------------------------------------------------------------- !

  IMPLICIT NONE
  PRIVATE

  LOGICAL :: SET_STATUS    = .FALSE. !! .TRUE. IF USER INPUT IS DEFINED.

  ! ---------------------------------------------------------------------------- !
  !                                                                              !
  !     2. TOPOGRAPHY INPUT DATA AS PROVIDED BY USER.                            !
  !        ------------------------------------------                            !

  INTEGER              :: NX_T = -1     !! NUMBER OF LONGITUDES.
  INTEGER              :: NY_T = -1     !! NUMBER OF LATITUDES.
  LOGICAL              :: PER_T         !! .TRUE. IF GRID IS PERIODICAL.
  INTEGER              :: DY_T          !! LATITUDE INCREMENT [M_SEC].
  INTEGER              :: DX_T          !! LONGITUDE INCREMENT [M_SEC].
  INTEGER              :: SOUTH_T       !! SOUTH LATITUDE OF GRID [M_SEC].
  INTEGER              :: NORTH_T       !! NORTH LATITUDE OF GRID [M_SEC].
  INTEGER              :: WEST_T        !! WEST LONGITUDE OF GRID [M_SEC].
  INTEGER              :: EAST_T        !! EAST LONGITUDE OF GRID [M_SEC].
  LOGICAL :: EQUAL_GRID =.FALSE.        !! .TRUE. IF GRID IS EQUAL TO MODEL GRID.
  LOGICAL              :: L_INTERPOL_T  !! INTERPOLATION OPTION FOR MODEL DEPTH.
  LOGICAL, PUBLIC      :: L_OBSTRUCTION_T   !! OBSTRUCTION FACTORS DUE TO SUB-GRID FEATURES.
  REAL,    ALLOCATABLE :: GRID_IN(:,:)  !! WATER DEPTH [M].
  REAL                 :: LAND_LIMIT    !! DEPTH <= LAND_LIMIT ARE NOT ACTIVE

  INTEGER, ALLOCATABLE, DIMENSION(:) :: ALON        !! longitudes in INPUT GRID
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ALAT        !! latitudes  in INPUT GRID

  ! ---------------------------------------------------------------------------- !
  !                                                                              !
  !     3. GRID CORRECTION AREAS.                                                !
  !        ----------------------                                                !

  INTEGER              :: N_CA = 0      !! NO. OF CORRECTION AREAS
  INTEGER, ALLOCATABLE :: SOUTH_CA(:)   !! S - LATITUDE OF AREA
  INTEGER, ALLOCATABLE :: NORTH_CA(:)   !! N - LATITUDE OF AREA
  INTEGER, ALLOCATABLE :: WEST_CA (:)   !! W - LONGITUIDE OF AREA
  INTEGER, ALLOCATABLE :: EAST_CA (:)   !! E - LONGITUIDE OF AREA
  REAL,    ALLOCATABLE :: DEPTH_CA(:)   !! DEPTH OF AREA

  ! ---------------------------------------------------------------------------- !
  !                                                                              !
  !     4. GRIDDED MODEL DEPTH DATA.                                             !
  !        -------------------------                                             !

  REAL,    ALLOCATABLE, DIMENSION(:,:) :: GRD             !! GRIDDED TOPOGRAPHY
  REAL,    ALLOCATABLE, DIMENSION(:,:) :: PERCENTLAND     !! percent of land points in WAM grid cell
  REAL,    ALLOCATABLE, DIMENSION(:,:) :: PERCENTSHALLOW  !! percent of shallow points in WAM grid cell
  INTEGER, ALLOCATABLE, DIMENSION(:)   :: XLAT            !! LATITUDES OF GRID.

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     D.  PUBLIC INTERFACES.                                                   !
  !                                                                              !
  ! ---------------------------------------------------------------------------- !


  INTERFACE SET_GRID_CORRECTIONS       !! TRANSFERS GRID CORRECTIONS TO MODULE.
     MODULE PROCEDURE SET_GRID_CORRECTIONS_C  !! TEXT COORDINATES
     MODULE PROCEDURE SET_GRID_CORRECTIONS_D  !! DEGREE COORDINATES
     MODULE PROCEDURE SET_GRID_CORRECTIONS_M  !! M_SEC
  END INTERFACE SET_GRID_CORRECTIONS
  PUBLIC SET_GRID_CORRECTIONS

  INTERFACE SET_GRID_DEF               !! TRANSFERS MODEL GRID DEFINITIONS 
     MODULE PROCEDURE SET_GRID_DEF_C     !! TEXT COORDINATES
     MODULE PROCEDURE SET_GRID_DEF_D     !! DEGREE COORDINATES
     MODULE PROCEDURE SET_GRID_DEF_M     !! M_SEC
  END INTERFACE SET_GRID_DEF
  PUBLIC SET_GRID_DEF

  INTERFACE SET_HEADER                 !! TRANSFERS MODEL HEADER 
     MODULE PROCEDURE SET_HEADER
  END INTERFACE SET_HEADER
  PUBLIC SET_HEADER

  INTERFACE SET_TOPOGRAPHY             !! TRANSFERS DEPTH DATA TO MODULE.
     MODULE PROCEDURE SET_TOPOGRAPHY_C   !! TEXT COORDINATES
     MODULE PROCEDURE SET_TOPOGRAPHY_D   !! DEGREE COORDINATES
     MODULE PROCEDURE SET_TOPOGRAPHY_M   !! M_SEC
  END INTERFACE SET_TOPOGRAPHY
  PUBLIC SET_TOPOGRAPHY

  INTERFACE PREPARE_CONST              !! COMPUTES WAM_CONST_MODULE DATA.
     MODULE PROCEDURE PREPARE_CONST
  END INTERFACE PREPARE_CONST
  PUBLIC PREPARE_CONST

  INTERFACE PRINT_PREPROC_STATUS        !! PRINTS PREPROC STATUS.
     MODULE PROCEDURE PRINT_PREPROC_STATUS
  END INTERFACE PRINT_PREPROC_STATUS
  PUBLIC PRINT_PREPROC_STATUS

  INTERFACE WRITE_PREPROC_FILE          !! WRITES PREPROC OUTPUT FILE.
     MODULE PROCEDURE WRITE_PREPROC_FILE
  END INTERFACE WRITE_PREPROC_FILE
  PUBLIC WRITE_PREPROC_FILE

  INTERFACE
     SUBROUTINE READ_PREPROC_USER        !! READ USER INPUT FOR PREPROC.
     END SUBROUTINE READ_PREPROC_USER

     SUBROUTINE READ_TOPOGRAPHY          !! READ TOPOGRAPHY INPUT FILE
     END SUBROUTINE READ_TOPOGRAPHY
  END INTERFACE
  PUBLIC READ_PREPROC_USER, READ_TOPOGRAPHY

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     E.  PRIVATE INTERFACES.                                                  !
  !                                                                              !
  ! ---------------------------------------------------------------------------- !

  INTERFACE ADJUST_DEPTH                  !! ADJUST DEPTH.
     MODULE PROCEDURE ADJUST_DEPTH
  END INTERFACE ADJUST_DEPTH
  PRIVATE ADJUST_DEPTH

  INTERFACE CLOSEST_GP_LAT                !! FINDS CLOSEST AND SECOND CLOSEST GRID
     MODULE PROCEDURE CLOSEST_GP_LAT      !! POINT AT N OR S LATITUDE NEIGHTBOUR.
  END INTERFACE CLOSEST_GP_LAT
  PRIVATE CLOSEST_GP_LAT

  INTERFACE COUNT_POINTS_E_W             !! ROUTINE TO COUNT POINTS.
     MODULE PROCEDURE COUNT_POINTS_E_W
  END INTERFACE COUNT_POINTS_E_W
  PRIVATE COUNT_POINTS_E_W

  INTERFACE COUNT_POINTS_N_S             !! ROUTINE  TO COUNT POINTS..
     MODULE PROCEDURE COUNT_POINTS_N_S
  END INTERFACE COUNT_POINTS_N_S
  PRIVATE COUNT_POINTS_N_S

  INTERFACE CREATE_OBSTRUCTIONS          !! CREATES OBSTRUCTION DUE TO SUB-GRID FEATRURES.
     MODULE PROCEDURE CREATE_OBSTRUCTIONS
  END INTERFACE CREATE_OBSTRUCTIONS
  PRIVATE CREATE_OBSTRUCTIONS

  INTERFACE MEAN_DEPTH                   !! ROUTINE TO ARRANGE WAMODEL GRID.
     MODULE PROCEDURE MEAN_DEPTH
  END INTERFACE MEAN_DEPTH
  PRIVATE MEAN_DEPTH

  INTERFACE PREPARE_GRID                 !! ROUTINE TO ARRANGE WAMODEL GRID.
     MODULE PROCEDURE PREPARE_GRID
  END INTERFACE PREPARE_GRID
  PRIVATE PREPARE_GRID

  INTERFACE SUBGRID_TOPOGRAPHY           !! ARRANGE SUBGRID TOPOGRAPHY.
     MODULE PROCEDURE SUBGRID_TOPOGRAPHY
  END INTERFACE SUBGRID_TOPOGRAPHY
  PRIVATE SUBGRID_TOPOGRAPHY

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     F. PUBLIC MODULE PROCEDURES.                                             !
  !                                                                              !
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE SET_GRID_CORRECTIONS_C (SOUTH, NORTH, WEST, EAST, D_COR)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     INTERFACE VARIABLES.                                                     !
    !     --------------------                                                     !

    CHARACTER (LEN=LEN_COOR), INTENT(IN)  :: SOUTH(:)  !! SOUTH LATITUDES.
    CHARACTER (LEN=LEN_COOR), INTENT(IN)  :: NORTH(:)  !! NORTH LATITUDES.
    CHARACTER (LEN=LEN_COOR), INTENT(IN)  :: WEST(:)   !! WEST LONGITUDES.
    CHARACTER (LEN=LEN_COOR), INTENT(IN)  :: EAST(:)   !! EAST LONGITUDES.
    REAL,                     INTENT(IN)  :: D_COR(:)  !! DEPTH IN CORR. AREAS [M].

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. CONVERT TO M_SEC AND CALL SET_GRID_CORRECTIONS_M.                     !
    !        ------------------------------------------------                      !

    CALL SET_GRID_CORRECTIONS_M (SOUTH=READ_COOR_TEXT(SOUTH),                      &
         &                            NORTH=READ_COOR_TEXT(NORTH),                      &
         &                            WEST=READ_COOR_TEXT(WEST),                        &
         &                            EAST=READ_COOR_TEXT(EAST),                        &
         &                            D_COR=D_COR)

  END SUBROUTINE SET_GRID_CORRECTIONS_C

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE SET_GRID_CORRECTIONS_D (SOUTH, NORTH, WEST, EAST, D_COR)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     INTERFACE VARIABLES.                                                     !
    !     --------------------                                                     !

    REAL (KIND=KIND_D), INTENT(IN)  :: SOUTH(:) !! SOUTH LAT. OF CORR. AREAS [DEG].
    REAL (KIND=KIND_D), INTENT(IN)  :: NORTH(:) !! NORTH LAT. OF CORR. AREAS [DEG].
    REAL (KIND=KIND_D), INTENT(IN)  :: WEST(:)  !! WEST LONG. OF CORR. AREAS [DEG].
    REAL (KIND=KIND_D), INTENT(IN)  :: EAST(:)  !! EAST LONG. OF CORR. AREAS [DEG].
    REAL,               INTENT(IN)  :: D_COR(:) !! DEPTH IN CORR. AREAS [M].

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. CONVERT TO M_SEC AND CALL SET_GRID_CORRECTIONS_M.                     !
    !        ------------------------------------------------                      !

    CALL SET_GRID_CORRECTIONS_M (SOUTH=DEG_TO_M_SEC(SOUTH),                        &
         &                            NORTH=DEG_TO_M_SEC(NORTH),                        &
         &                            WEST=DEG_TO_M_SEC(WEST),                          &
         &                            EAST=DEG_TO_M_SEC(EAST),                          &
         &                            D_COR=D_COR)

  END SUBROUTINE SET_GRID_CORRECTIONS_D

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE SET_GRID_CORRECTIONS_M (SOUTH, NORTH, WEST, EAST, D_COR)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     INTERFACE VARIABLES.                                                     !
    !     --------------------                                                     !

    INTEGER, INTENT(IN)  :: SOUTH(:)   !! SOUTH LATITUDES OF CORR. AREAS [M_SEC].
    INTEGER, INTENT(IN)  :: NORTH(:)   !! NORTH LATITUDES OF CORR. AREAS [M_SEC].
    INTEGER, INTENT(IN)  :: WEST(:)    !! WEST LONGITUDES OF CORR. AREAS [M_SEC].
    INTEGER, INTENT(IN)  :: EAST(:)    !! EAST LONGITUDES OF CORR. AREAS [M_SEC].
    REAL,    INTENT(IN)  :: D_COR(:)   !! DEPTH IN CORR. AREAS [M].

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. CLEAR CORRECTION AREAS.                                               !
    !        -----------------------                                               !

    N_CA = 0
    IF (ALLOCATED(SOUTH_CA)) DEALLOCATE(SOUTH_CA)
    IF (ALLOCATED(NORTH_CA)) DEALLOCATE(NORTH_CA)
    IF (ALLOCATED(WEST_CA )) DEALLOCATE(WEST_CA)
    IF (ALLOCATED(EAST_CA )) DEALLOCATE(EAST_CA)
    IF (ALLOCATED(DEPTH_CA)) DEALLOCATE(DEPTH_CA)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. COPY CORRECTION AREAS DEFINITIONS.                                    !
    !        ----------------------------------                                    !

    N_CA = COUNT (SOUTH.NE.COOR_UNDEF)
    IF (N_CA.GT.0) THEN
       ALLOCATE(SOUTH_CA(1:N_CA))
       ALLOCATE(NORTH_CA(1:N_CA))
       ALLOCATE(WEST_CA(1:N_CA))
       ALLOCATE(EAST_CA(1:N_CA))
       ALLOCATE(DEPTH_CA(1:N_CA))

       SOUTH_CA(1:N_CA) = SOUTH(1:N_CA)
       NORTH_CA(1:N_CA) = NORTH(1:N_CA)
       WEST_CA(1:N_CA)  = WEST (1:N_CA)
       EAST_CA(1:N_CA)  = EAST (1:N_CA)
       DEPTH_CA(1:N_CA) = D_COR(1:N_CA)

       CALL ADJUST (WEST_CA, EAST_CA)
    END IF

  END SUBROUTINE SET_GRID_CORRECTIONS_M

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE SET_GRID_DEF_C (N_LON, N_LAT, D_LON, D_LAT,                         &
       &                          SOUTH, NORTH, WEST, EAST,                           &
       &                          LAND, R_GRID, L_INTERPOL, L_OBSTRUCTION)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     INTERFACE VARIABLES.                                                     !
    !     --------------------                                                     !

    INTEGER,                  INTENT(IN) :: N_LON   !! NUMBER OF LONGITUDES IN GRID.
    INTEGER,                  INTENT(IN) :: N_LAT   !! NUMBER OF LATITUDES IN GRID.
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: D_LON   !! LONGITUDE INCREMENT OF GRID.
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: D_LAT   !! LATITUDE INCREMENT OF GRID.
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: SOUTH   !! SOUTH LATITUDE OF GRID.
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: NORTH   !! NORTH LATITUDE OF GRID.
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: WEST    !! WEST LONGITUDE OF GRID.
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: EAST    !! EAST LONGITUDE OF GRID.
    REAL,      INTENT(IN) :: LAND     !! DEPTH >= LAND ARE SEAPOINTS [M].
    LOGICAL,   INTENT(IN) :: R_GRID   !! REDUCED GRID OPTION.
    LOGICAL,   INTENT(IN) :: L_INTERPOL     !! INTERPOLATION OPTION FOR MODEL DEPTH.
    LOGICAL,   INTENT(IN) :: L_OBSTRUCTION !! REDUCTION FACTORS DUE TO SUB-GRID FEATURES.

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. CONVERT TO M_SEC AND CALL SET_GRID_CORRECTIONS_M.                     !
    !        -------------------------------------------------                     !

    CALL SET_GRID_DEF_M (N_LON, N_LAT,                                             &
         &                    READ_COOR_TEXT(D_LON), READ_COOR_TEXT(D_LAT),             &
         &                    READ_COOR_TEXT(SOUTH), READ_COOR_TEXT(NORTH),             &
         &                    READ_COOR_TEXT(WEST),  READ_COOR_TEXT(EAST),              &
         &                    LAND, R_GRID, L_INTERPOL, L_OBSTRUCTION)

  END SUBROUTINE SET_GRID_DEF_C

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE SET_GRID_DEF_D (N_LON, N_LAT, D_LON, D_LAT,                         &
       &                          SOUTH, NORTH, WEST, EAST,                           &
       &                          LAND, R_GRID, L_INTERPOL, L_OBSTRUCTION)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     INTERFACE VARIABLES.                                                     !
    !     --------------------                                                     !

    INTEGER,            INTENT(IN) :: N_LON   !! NUMBER OF LONGITUDES IN GRID.
    INTEGER,            INTENT(IN) :: N_LAT   !! NUMBER OF LATITUDES IN GRID.
    REAL (KIND=KIND_D), INTENT(IN) :: D_LON   !! LONGITUDE INCREMENT OF GRID.
    REAL (KIND=KIND_D), INTENT(IN) :: D_LAT   !! LATITUDE INCREMENT OF GRID.
    REAL (KIND=KIND_D), INTENT(IN) :: SOUTH   !! SOUTH LATITUDE OF GRID.
    REAL (KIND=KIND_D), INTENT(IN) :: NORTH   !! NORTH LATITUDE OF GRID.
    REAL (KIND=KIND_D), INTENT(IN) :: WEST    !! WEST LONGITUDE OF GRID.
    REAL (KIND=KIND_D), INTENT(IN) :: EAST    !! EAST LONGITUDE OF GRID.
    REAL,      INTENT(IN) :: LAND     !! DEPTH >= LAND ARE SEAPOINTS [M].
    LOGICAL,   INTENT(IN) :: R_GRID   !! REDUCED GRID OPTION.
    LOGICAL,   INTENT(IN) :: L_INTERPOL     !! INTERPOLATION OPTION FOR MODEL DEPTH.
    LOGICAL,   INTENT(IN) :: L_OBSTRUCTION !! REDUCTION FACTORS DUE TO SUB-GRID FEATURES.

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. CONVERT TO M_SEC AND CALL SET_GRID_CORRECTIONS_M.                     !
    !        -------------------------------------------------                     !

    CALL SET_GRID_DEF_M (N_LON, N_LAT,                                             &
         &                    DEG_TO_M_SEC(D_LON), DEG_TO_M_SEC(D_LAT),                 &
         &                    DEG_TO_M_SEC(SOUTH), DEG_TO_M_SEC(NORTH),                 &
         &                    DEG_TO_M_SEC(WEST),  DEG_TO_M_SEC(EAST),                  &
         &                    LAND, R_GRID, L_INTERPOL, L_OBSTRUCTION)

  END SUBROUTINE SET_GRID_DEF_D

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE SET_GRID_DEF_M (N_LON, N_LAT, D_LON, D_LAT,                         &
       &                          SOUTH, NORTH, WEST, EAST,                           &
       &                          LAND, R_GRID, L_INTERPOL, L_OBSTRUCTION)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     INTERFACE VARIABLES.                                                     !
    !     --------------------                                                     !

    INTEGER,           INTENT(IN) :: N_LON   !! NUMBER OF LONGITUDES IN GRID.
    INTEGER,           INTENT(IN) :: N_LAT   !! NUMBER OF LATITUDES IN GRID.
    INTEGER,           INTENT(IN) :: D_LON   !! LONGITUDE INCREMENT OF GRID.
    INTEGER,           INTENT(IN) :: D_LAT   !! LATITUDE INCREMENT OF GRID.
    INTEGER,           INTENT(IN) :: SOUTH   !! SOUTH LATITUDE OF GRID.
    INTEGER,           INTENT(IN) :: NORTH   !! NORTH LATITUDE OF GRID.
    INTEGER,           INTENT(IN) :: WEST    !! WEST LONGITUDE OF GRID.
    INTEGER,           INTENT(IN) :: EAST    !! EAST LONGITUDE OF GRID.
    REAL,      INTENT(IN) :: LAND          !! DEPTH >= LAND ARE SEAPOINTS [M].
    LOGICAL,   INTENT(IN) :: R_GRID        !! REDUCED GRID OPTION.
    LOGICAL,   INTENT(IN) :: L_INTERPOL     !! INTERPOLATION OPTION FOR MODEL DEPTH.
    LOGICAL,   INTENT(IN) :: L_OBSTRUCTION !! REDUCTION FACTORS DUE TO SUB-GRID FEATURES.

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     LOCAL VARIABLES.                                                         !
    !     ----------------                                                         !

    LOGICAL  :: ERROR = .FALSE.              !! ERROR FLAG
    character (len=len_coor) :: formtext

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. CLEAR GRID DEFINITIONS.                                               !
    !        -----------------------                                               !

    IPER         = .FALSE. !! PERIODIC GRID

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. COPY GRID DEFINITIONS AND CONVERT TO M_SEC.                           !
    !        -------------------------------------------                           !

    L_INTERPOL_T = L_INTERPOL
    L_OBSTRUCTION_T =  L_OBSTRUCTION
    AMOWEP = WEST
    AMOSOP = SOUTH
    AMOEAP = EAST
    AMONOP = NORTH
    XDELLO = D_LON
    XDELLA = D_LAT
    NX = N_LON
    NY = N_LAT

    REDUCED_GRID = R_GRID
    LAND_LIMIT = LAND

    IF (NX .EQ.-1 .AND. NY .EQ.-1 .AND.                                      &
         &   XDELLO.EQ.COOR_UNDEF .AND. XDELLA.EQ.COOR_UNDEF .AND.                      &
         &   AMOSOP.EQ.COOR_UNDEF .AND. AMONOP.EQ.COOR_UNDEF .AND.                      &
         &   AMOWEP.EQ.COOR_UNDEF .AND. AMOEAP.EQ.COOR_UNDEF ) THEN
       WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
       WRITE (IU06,*) ' +                                                        +'
       WRITE (IU06,*) ' +         INFORMATION FROM SUB. SET_GRID_DEF             +'
       WRITE (IU06,*) ' +         ==================================             +'
       WRITE (IU06,*) ' +                                                        +'
       WRITE (IU06,*) ' +     ALL MODEL GRID SPECIFICATIONS ARE UNDEFINED.       +'
       WRITE (IU06,*) ' +                                                        +'
       WRITE (IU06,*) ' +            PROGRAM WILL CONTINUE                       +'
       WRITE (IU06,*) ' +  USING THE DEFINITIONS FROM THE TOPOGRAPHY INPUT FILE  +'
       WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
       SET_STATUS = (NX.GT.0 .AND. NY.GT.0 .AND. NX_T.GT.0 .AND. NY_T.GT.0)
       RETURN
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. COMPUTE MISSING VALUES.                                               !
    !        ----------------------                                                !

    ERROR = .FALSE.
    IF (AMOWEP.EQ.COOR_UNDEF) ERROR = .TRUE.
    IF (AMOSOP.EQ.COOR_UNDEF) ERROR = .TRUE.

    IF (.NOT.ERROR) THEN
       IF (NX.EQ.1 .OR. XDELLO.EQ.0 .OR. AMOEAP.EQ.AMOWEP) THEN
          NX = 1
          XDELLO = M_S_PER
          AMOEAP = AMOWEP
       END IF
       IF (NY.EQ.1 .OR. XDELLA.EQ.0 .OR. AMOSOP.EQ.AMONOP) THEN
          NY = 1
          XDELLA = 0
          AMONOP = AMOSOP
       END IF
    END IF

    CALL CHECK_GRID_DEFINITION (AMOWEP, AMOSOP, AMOEAP, AMONOP,                   &
         &                           XDELLO, XDELLA, NX, NY, ERROR)

    IF (ERROR) THEN
       WRITE (IU06,*) ' **********************************************************'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' *           FATAL ERROR IN SUB. SET_GRID_DEF             *'
       WRITE (IU06,*) ' *           ================================             *'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' *         A MODEL GRID COULD NOT BE DEFINED.             *'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' **********************************************************'
       CALL ABORT1
    END IF

    IPER = PERIODIC (AMOWEP, AMOEAP, XDELLO, NX)   !! PERIODIC ?
    ONE_POINT = NX.EQ.1 .AND. NY.EQ.1

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. CHECK CONSISTENCY.                                                    !
    !        ------------------                                                    !

    IF ((NX.LE.1 .OR. NY.LE.1) .AND. (NX.NE.1 .OR. NY.LT.1)) THEN
       WRITE (IU06,*) ' **********************************************************'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' *           FATAL ERROR IN SUB. SET_GRID_DEF             *'
       WRITE (IU06,*) ' *           ================================             *'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' *  A MODEL GRID MUST HAVE                                *'
       WRITE (IU06,*) ' *  MORE THAN ONE POINT ON BOTH GRID AXES                 *'
       WRITE (IU06,*) ' *                OR                                      *'
       WRITE (IU06,*) ' *  ONE POINT ON BOTH GRID AXES (PROPAGATION IS NOT DONE) *'
       WRITE (IU06,*) ' *                OR                                      *'
       WRITE (IU06,*) ' *  ONE POINT ON LONGITUDE AXIS AND MORE THAN ONE POINT   *'
       WRITE (IU06,*) ' *  ON LATITUDE AXIS AND THE LONGITUDE INCREMENT MUST     *'
       WRITE (IU06,*) ' *  BE 360 DEG.  (PERIODIC GRID IN LONGITUDE)             *'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' * USER PROVIDED GRID DEFINITIONS ARE:                    *'
       formtext = write_coor_text (coor_undef)
       WRITE (IU06,*) ' *  (-1 AND ', formtext,                                    &
            &                                          ' INDICATED UNDEFINED VALUES)     *'
       WRITE (IU06,*) ' *                                                        *'
       formtext = write_coor_text (amowep)
       WRITE (IU06,*) ' * LONGITUDE             WEST = ', formtext,                &
            &                                      ' = ',M_SEC_TO_DEG (amowep), 'deg'
       formtext = write_coor_text (amoeap)
       WRITE (IU06,*) ' * LONGITUDE             EAST = ', formtext,                &
            &                                      ' = ',M_SEC_TO_DEG (amoeap), 'deg'
       formtext = write_coor_text (xdello)
       WRITE (IU06,*) ' * LONGITUDE INCREMENT  D_LON = ', formtext,                &
            &                                      ' = ',M_SEC_TO_DEG (xdello), 'deg'
       WRITE (IU06,*) ' * NO. OF LONGITUDES    N_LON = ', NX
       formtext = write_coor_text (amosop)
       WRITE (IU06,*) ' * LATITUDE             SOUTH = ', formtext,                &
            &                                      ' = ',M_SEC_TO_DEG (amosop), 'deg'
       formtext = write_coor_text (amonop)
       WRITE (IU06,*) ' * LATITUDE             NORTH = ', formtext,                &
            &                                      ' = ',M_SEC_TO_DEG (amonop), 'deg'
       formtext = write_coor_text (xdella)
       WRITE (IU06,*) ' * LATITUDE  INCREMENT  D_LAT = ', formtext,                &
            &                                      ' = ',M_SEC_TO_DEG (xdella), 'deg'
       WRITE (IU06,*) ' * NO. OF LATITUDE      N_LAT = ', NY
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' **********************************************************'
       CALL ABORT1
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     4. INPUT MODULE DATA ARE DEFINED.                                        !
    !        ------------------------------                                        !

    IF (L_INTERPOL_T .AND. L_OBSTRUCTION_T) THEN

       WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
       WRITE (IU06,*) ' +                                                        +'
       WRITE (IU06,*) ' +         WARNING FROM SUB. SET_GRID_DEF                 +'
       WRITE (IU06,*) ' +         ==============================                 +'
       WRITE (IU06,*) ' +                                                        +'
       WRITE (IU06,*) ' + MODEL DEPTH IS NEAREST NEIGHBOUR DEPTH OF INPUT',      &
            &                     ' TOPOGRAPHY:    L_INTERPOL = ', L_INTERPOL_T
       WRITE (IU06,*) ' + OBSTRUCTION FACTORS DUE TO SUB-GRID FEATURES ARE',      &
            &                     ' REQUESTED: L_OBSTRUCTION = ', L_OBSTRUCTION
       WRITE (IU06,*) ' +                                                        +'
       WRITE (IU06,*) ' +                                                        +'
       WRITE (IU06,*) ' +            PROGRAM WILL CONTINUE                       +'
       WRITE (IU06,*) ' +         WITHOUT OBSTRUCTION FACTORS                    +'
       WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
       L_OBSTRUCTION_T = .FALSE.
    END IF
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     5. INPUT MODULE DATA ARE DEFINED.                                        !
    !        ------------------------------                                        !

    SET_STATUS = (NX.GT.0 .AND. NY.GT.0 .AND. NX_T.GT.0 .AND. NY_T.GT.0)

  END SUBROUTINE SET_GRID_DEF_M

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE SET_HEADER (TEXT)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     INTERFACE VARIABLES.                                                     !
    !     --------------------                                                     !

    CHARACTER (LEN=*), INTENT(IN)  :: TEXT      !! MODEL HEADER.

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. COPY HEADER DEFINITION.                                               !
    !        -----------------------                                               !

    HEADER = TEXT(1:MIN(LEN_TRIM(TEXT), LEN(HEADER)))

  END SUBROUTINE SET_HEADER

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE SET_TOPOGRAPHY_D (N_LON, N_LAT, D_LON, D_LAT,                       &
       &                            SOUTH, NORTH, WEST, EAST, D_MAP)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     INTERFACE VARIABLES.                                                     !
    !     --------------------                                                     !

    INTEGER,            INTENT(IN) :: N_LON    !! NUMBER OF LONGITUDES IN GRID.
    INTEGER,            INTENT(IN) :: N_LAT    !! NUMBER OF LATITUDES IN GRID.
    REAL (KIND=KIND_D), INTENT(IN) :: D_LON    !! LONGITUDE INCREMENT OF GRID [DEG].
    REAL (KIND=KIND_D), INTENT(IN) :: D_LAT    !! LATITUDE INCREMENT OF GRID [DEG].
    REAL (KIND=KIND_D), INTENT(IN) :: SOUTH    !! SOUTH LATITUDE OF GRID [DEG].
    REAL (KIND=KIND_D), INTENT(IN) :: NORTH    !! NORTH LATITUDE OF GRID [DEG].
    REAL (KIND=KIND_D), INTENT(IN) :: WEST     !! WEST LONGITUDE OF GRID [DEG].
    REAL (KIND=KIND_D), INTENT(IN) :: EAST     !! EAST LONGITUDE OF GRID [DEG].
    REAL,               INTENT(IN) :: D_MAP(1:N_LON,1:N_LAT) !! WATER DEPTH [M].

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. CONVERT TO M_SEC AND CALL SET_TOPOGRAPHY_M.                           !
    !        -------------------------------------------                           !

    CALL SET_TOPOGRAPHY_M (N_LON, N_LAT,                                           & 
         &                      DEG_TO_M_SEC(D_LON),  DEG_TO_M_SEC(D_LAT),              &
         &                      DEG_TO_M_SEC(SOUTH),  DEG_TO_M_SEC(NORTH),              &
         &                      DEG_TO_M_SEC(WEST),   DEG_TO_M_SEC(EAST),               &
         &                      D_MAP)

  END SUBROUTINE SET_TOPOGRAPHY_D

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE SET_TOPOGRAPHY_M (N_LON, N_LAT, D_LON, D_LAT,                       &
       &                            SOUTH, NORTH, WEST, EAST, D_MAP)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     INTERFACE VARIABLES.                                                     !
    !     --------------------                                                     !

    INTEGER, INTENT(IN) :: N_LON      !! NUMBER OF LONGITUDES IN GRID.
    INTEGER, INTENT(IN) :: N_LAT      !! NUMBER OF LATITUDES IN GRID.
    INTEGER, INTENT(IN) :: D_LON      !! LONGITUDE INCREMENT OF GRID [M_SEC].
    INTEGER, INTENT(IN) :: D_LAT      !! LATITUDE INCREMENT OF GRID [M_SEC].
    INTEGER, INTENT(IN) :: SOUTH      !! SOUTH LATITUDE OF GRID [M_SEC].
    INTEGER, INTENT(IN) :: NORTH      !! NORTH LATITUDE OF GRID [M_SEC].
    INTEGER, INTENT(IN) :: WEST       !! WEST LONGITUDE OF GRID [M_SEC].
    INTEGER, INTENT(IN) :: EAST       !! EAST LONGITUDE OF GRID [M_SEC].
    REAL,    INTENT(IN)  :: D_MAP(1:N_LON,1:N_LAT) !! WATER DEPTH [M].

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     LOCAL VARIABLES.                                                         !
    !     ----------------                                                         !

    REAL    :: LAND
    LOGICAL :: R_GRID
    LOGICAL :: L_OBSTRUCTION
    LOGICAL :: L_INTERPOL
    LOGICAL :: ERROR
    character (len=len_coor) :: formtext

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. CLEAR INPUT TOPOGRAPHY.                                               !
    !        -----------------------                                               !

    NX_T = -1
    NY_T = -1
    IF (ALLOCATED(GRID_IN)) DEALLOCATE(GRID_IN)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. COPY GRID DEFINITIONS.                                                !
    !        ----------------------                                                !

    DY_T    = D_LAT                     !! LATITUDE INCREMENT.
    DX_T    = D_LON                     !! LONGITUDE INCREMENT.
    SOUTH_T = SOUTH                     !! SOUTH LATITUDE OF GRID.
    NORTH_T = NORTH                     !! NORTH LATITUDE OF GRID.
    WEST_T  = WEST                      !! WEST LONGITUDE OF GRID.
    EAST_T  = EAST                      !! EAST LONGITUDE OF GRID.
    CALL ADJUST (WEST_T, EAST_T)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. CHECK CONSISTENCY.                                                    !
    !        ------------------                                                    !

    CALL CHECK_GRID_DEFINITION (WEST_T, SOUTH_T, EAST_T, NORTH_T,                  &
         &                           DX_T, DY_T, NX_T, NY_T, ERROR)

    IF (ERROR) THEN
       WRITE (IU06,*) ' **********************************************************'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' *          FATAL ERROR IN SUB. SET_TOPOGRAPHY            *'
       WRITE (IU06,*) ' *          =================================             *'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' *        A TOPOGRAPY INPUT GRID COULD NOT BE DEFINED.    *'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' **********************************************************'
       CALL ABORT1
    END IF

    IF (N_LON.NE.NX_T .OR. N_LAT.NE.NY_T .OR.                                      &
         &   N_LON.NE.SIZE(D_MAP,1) .OR. N_LAT.NE.SIZE(D_MAP,2)) THEN
       WRITE (IU06,*) ' **********************************************************'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' *         FATAL  ERROR IN SUB. SET_TOPOGRAPHY            *'
       WRITE (IU06,*) ' *         ===================================            *'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' * GRID SPECIFICATIONS ARE NOT CONSISTENT                 *'
       WRITE (IU06,*) ' * USER PROVIDED GRID DEFINITIONS ARE:                    *'
       formtext = write_coor_text(WEST)
       WRITE (IU06,*) ' * LONGITUDE            WEST = ', formtext,                 &
            &                                      ' = ',M_SEC_TO_DEG (WEST), 'deg'
       formtext = write_coor_text(EAST)
       WRITE (IU06,*) ' * LONGITUDE            EAST = ', formtext,                 &
            &                                      ' = ',M_SEC_TO_DEG (EAST), 'deg'
       formtext = write_coor_text(D_LON)
       WRITE (IU06,*) ' * LONGITUDE INCREMENT D_LON = ', formtext,                 &
            &                                      ' = ',M_SEC_TO_DEG (D_LON), 'deg'
       WRITE (IU06,*) ' * NO. OF LONGITUDES   N_LON = ', N_LON
       formtext = write_coor_text(NORTH)
       WRITE (IU06,*) ' * LATITUDE            NORTH = ', formtext,                 &
            &                                      ' = ',M_SEC_TO_DEG (NORTH), 'deg'
       formtext = write_coor_text(SOUTH)
       WRITE (IU06,*) ' * LATITUDE            SOUTH = ', formtext,                 &
            &                                      ' = ',M_SEC_TO_DEG (SOUTH), 'deg'
       formtext = write_coor_text(D_LAT)
       WRITE (IU06,*) ' * LATITUDE  INCREMENT D_LAT = ', formtext,                 &
            &                                      ' = ',M_SEC_TO_DEG (D_LAT), 'deg'
       WRITE (IU06,*) ' * NO. OF LATITUDE     N_LAT = ', N_LAT
       WRITE (IU06,*) ' * COMPUTED GRID SIZES ARE:                               *'
       WRITE (IU06,*) ' * NO. OF LONGITUDES    NX_T = ', NX_T
       WRITE (IU06,*) ' * NO. OF LATITUDE      NY_T = ', NY_T
       WRITE (IU06,*) ' * DIMENSIONS OF DEPTH MAP ARRAY ARE :                    *'
       WRITE (IU06,*) ' * NO. OF LONGITUDES 1. DIMENSION = ', SIZE(D_MAP,1)
       WRITE (IU06,*) ' * NO. OF LATITUDE   2. DIMENSION = ', SIZE(D_MAP,2)
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
       WRITE (IU06,*) ' *                                                        *'
       WRITE (IU06,*) ' **********************************************************'
       NX_T = -1
       NY_T = -1
       CALL ABORT1
    END IF

    PER_T = PERIODIC (WEST_T, EAST_T, DX_T, NX_T)   !! PERIODIC ?

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     4. COPY DEPTH FIELD.                                                     !
    !        -----------------                                                     !

    ALLOCATE (GRID_IN(NX_T,NY_T))
    GRID_IN = D_MAP

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     5. INPUT MODULE DATA ARE DEFINED.                                        !
    !        ------------------------------                                        !

    SET_STATUS = (NX.GT.0 .AND. NY.GT.0 .AND. NX_T.GT.0 .AND. NY_T.GT.0)
    IF (.NOT.SET_STATUS) THEN
       LAND   = LAND_LIMIT
       R_GRID = REDUCED_GRID
       L_INTERPOL = L_INTERPOL_T
       L_OBSTRUCTION = .FALSE.
       CALL SET_GRID_DEF (N_LON, N_LAT, D_LON, D_LAT, SOUTH, NORTH, WEST, EAST,    &
            &                     LAND=LAND, R_GRID=R_GRID, L_INTERPOL=L_INTERPOL,         &
            &                     L_OBSTRUCTION=L_OBSTRUCTION)
    END IF

    EQUAL_GRID = EQUAL_TO_M_GRID (NX_T, NY_T, DX_T, DY_T,                          &
         &                             WEST_T, SOUTH_T, EAST_T, NORTH_T) 

  END SUBROUTINE SET_TOPOGRAPHY_M

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE SET_TOPOGRAPHY_C (N_LON, N_LAT, D_LON, D_LAT,                       &
       &                            SOUTH, NORTH, WEST, EAST, D_MAP)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     INTERFACE VARIABLES.                                                     !
    !     --------------------                                                     !

    INTEGER,                  INTENT(IN) :: N_LON !! NUMBER OF LONGITUDES IN GRID.
    INTEGER,                  INTENT(IN) :: N_LAT !! NUMBER OF LATITUDES IN GRID.
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: D_LON !! LONGITUDE INCREMENT OF GRID [DMS].
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: D_LAT !! LATITUDE INCREMENT OF GRID [DMS].
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: SOUTH !! SOUTH LATITUDE OF GRID [DMS].
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: NORTH !! NORTH LATITUDE OF GRID [DMS].
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: WEST  !! WEST LONGITUDE OF GRID [DMS].
    CHARACTER (LEN=LEN_COOR), INTENT(IN) :: EAST  !! EAST LONGITUDE OF GRID [DMS].
    REAL,             INTENT(IN) :: D_MAP(1:N_LON,1:N_LAT) !! WATER DEPTH [M].

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. CONVERT TO M_SEC AND CALL SET_TOPOGRAPHY_M.                           !
    !        -------------------------------------------                           !

    CALL SET_TOPOGRAPHY_M (N_LON, N_LAT,                                           & 
         &                      READ_COOR_TEXT(D_LON), READ_COOR_TEXT(D_LAT),           &
         &                      READ_COOR_TEXT(SOUTH), READ_COOR_TEXT(NORTH),           &
         &                      READ_COOR_TEXT(WEST),  READ_COOR_TEXT(EAST),            &
         &                      D_MAP)

  END SUBROUTINE SET_TOPOGRAPHY_C

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE PREPARE_CONST

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !   PREPARE_CONST - ROUTINE TO PREPARE WAM CONST MODULE.                       !
    !                                                                              !
    !     H.GUNTHER            ECMWF       04/04/1990                              !
    !     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
    !                                                                              !
    !     PURPOSE.                                                                 !
    !     -------                                                                  !
    !                                                                              !
    !       TO COMPUTE ALL VARAIABLES IN WAM CONST MODULE FROM THE USER INPUT.     !
    !                                                                              !
    !     METHOD.                                                                  !
    !     -------                                                                  !
    !                                                                              !
    !       DONE BY CALLS TO MANY SUBS.                                            !
    !                                                                              !
    !     REFERENCE.                                                               !
    !     ----------                                                               !
    !                                                                              !
    !       NONE.                                                                  !
    !                                                                              !
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. GENERATE MODEL GRID.                                                  !
    !        --------------------                                                  !

    CALL PREPARE_GRID
    IF (ITEST.GT.1) WRITE (IU06,*) ' SUB. PREPARE_GRID DONE'

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. COMPUTE NEST INFORMATION.                                             !
    !        -------------------------                                             !

    CALL PREPARE_BOUNDARY_nest
    IF (ITEST.GT.0) WRITE (IU06,*) ' SUB. PREPARE_BOUNDARY DONE'

  END SUBROUTINE PREPARE_CONST

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE PRINT_PREPROC_STATUS

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !   PRINT_PREPROC_STATUS - PRINT STATUS OF PREPROC.                            !
    !                                                                              !
    !     H.GUNTHER            ECMWF       04/04/1990                              !
    !     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
    !                                                                              !
    !     PURPOSE.                                                                 !
    !     -------                                                                  !
    !                                                                              !
    !       MAKE A PRINTER OUTPUT OF THE PREPROC RESULTS, WHICH ARE SAVED IN       !
    !       WAM_CONST_MODULE.                                                      !
    !                                                                              !
    !     METHOD.                                                                  !
    !     -------                                                                  !
    !                                                                              !
    !       NONE.                                                                  !
    !                                                                              !
    !     REFERENCE.                                                               !
    !     ----------                                                               !
    !                                                                              !
    !       NONE.                                                                  !
    !                                                                              !
    ! ---------------------------------------------------------------------------- !

    INTEGER             :: I
    INTEGER             :: IGRID(NX,NY)  !! ARRAY FOR GRIDDED PRINT OUTPUT.
    CHARACTER (LEN=1)   :: C_GRID(NX,NY) !! ARRAY FOR GRIDDED PRINT OUTPUT.
    CHARACTER (LEN=100) :: TITL
    CHARACTER (LEN=14)  :: ZERO = ' '
    character (len=len_coor) :: formtext, ftext1, ftext2, ftext3, ftext4

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. FREQUENCY DIRECTION.                                                  !
    !        --------------------                                                  !

    CALL PRINT_FRE_DIR_STATUS

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. INPUT GRID DEFINITIONS.                                               !
    !        -----------------------                                               !

    WRITE (IU06,'(/,'' ----------------------------------------'')')
    WRITE (IU06,'(  ''             INPUT TOPOGRAPHY '')')
    WRITE (IU06,'(  '' ----------------------------------------'')')
    WRITE(IU06,*) ' '

    IF (NX_T.GT.0 .AND. NY_T.GT.0) THEN
       WRITE (IU06,'('' TOPOGRAPHY INPUT GRID: '')')
       WRITE (IU06,'('' NUMBER OF LONGITUDES IS      NX_T = '',I5)') NX_T
       formtext = write_coor_text(west_t)
       WRITE (IU06,'('' MOST WESTERN LONGITUDE IS  WEST_T = '',A)') formtext
       formtext = write_coor_text(east_t)
       WRITE (IU06,'('' MOST EASTERN LONGITUDE IS  EAST_T = '',A)') formtext
       formtext = write_coor_text(dx_t)
       WRITE (IU06,'('' LONGITUDE INCREMENT IS       DX_T = '',A)') formtext
       WRITE (IU06,'('' NUMBER OF LATITUDES IS       NY_T = '',I5)') NY_T
       formtext = write_coor_text(south_t)
       WRITE (IU06,'('' MOST SOUTHERN LATITUDE IS SOUTH_T = '',A)') formtext
       formtext = write_coor_text(north_t)
       WRITE (IU06,'('' MOST NORTHERN LATITUDE IS NORTH_T = '',A)') formtext
       formtext = write_coor_text(dy_t)
       WRITE (IU06,'('' LATITUDE INCREMENT IS        DY_T = '',A)') formtext
       IF (PER_T) THEN
          WRITE (IU06,*) 'THE GRID IS EAST-WEST PERIODIC'
       ELSE
          WRITE (IU06,*) 'THE GRID IS NOT EAST-WEST PERIODIC'
       END IF
       IF (EQUAL_GRID) THEN
          WRITE (IU06,*) 'GRID IS IDENTICAL TO MODEL GRID: NO SPACE INTERPOLATION'
       ELSE
          WRITE (IU06,*) 'GRID IS NOT IDENTICAL TO MODEL GRID: SPACE INTERPOLATION'
       END IF
    ELSE
       WRITE (IU06,*) ' TOPOGRAPHY INPUT GRID IS NOT DEFINED'
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. GRID CORRECTIONS.                                                     !
    !        -----------------                                                     !

    IF (N_CA.GT.0) THEN
       WRITE (IU06,'(/4X,'' AREAS TO BE CORRECTED IN OUTPUT GRID'',                &
            &               /,4X,''  NO.   SOUTHERN LAT   NORTHERN LAT   WESTERN LONG '',  &
            &                    ''  EASTERN LONG     DEPTH'')')
       DO I = 1,N_CA
          ftext1 = write_coor_text (south_ca(i))
          ftext2 = write_coor_text (north_ca(i))
          ftext3 = write_coor_text (west_ca(i))
          ftext4 = write_coor_text (east_ca(i))
          WRITE (IU06,'(4X,I5,4(2X,A),F10.2 )') I, ftext1, ftext2, ftext3,          &
               &           ftext4, DEPTH_CA(I)
       END DO
    ELSE
       WRITE (IU06,*) ' CORRECTION AREAS ARE NOT DEFINED'
    END IF

    WRITE (IU06,'('' DRY LAND POINTS UP TO  LAND_LIMIT = '',F10.2)') LAND_LIMIT
    IF (L_INTERPOL_T) THEN
       WRITE (IU06,*) ' MODEL DEPTH IS NEAREST NEIGHBOUR DEPTH OF INPUT TOPOGRAPHY'
    ELSE
       WRITE (IU06,*) ' MODEL DEPTH IS MEAN OF INPUT TOPOGRAPHY DEPTH IN GRID CELL'
    END IF
    IF (L_OBSTRUCTION_T) THEN
       WRITE (IU06,*) ' OBSTRUCTION FACTORS DUE TO SUBGRID FEATURES ARE COMPUTED'
    ELSE
       WRITE (IU06,*) ' OBSTRUCTION FACTORS DUE TO SUBGRID FEATURES ARE NOT COMPUTED'
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     4. BASIC MODEL GRID.                                                     !
    !        -----------------                                                     !

    CALL PRINT_GRID_STATUS

    IF (NSEA.GT.0) THEN
       IF (ITEST.GT.3) THEN
          WRITE (IU06,*) ' '
          TITL = 'LAND - SEA MASK'
          C_GRID = 'L'
          WHERE (L_S_MASK) C_GRID = 'S'
          CALL PRINT_ARRAY (IU06, ZERO, TITL, C_GRID,                             &
               &                       AMOWEP, AMOSOP, AMOEAP, AMONOP,NG_R=NLON_RG)
       END IF
       IF (ITEST.GT.9) THEN
          WRITE (IU06,*) ' '
          IGRID=UNPACK((/(I,I=1,NSEA)/), L_S_MASK,-999)
          IGRID=MOD(IGRID,1000)
          TITL = 'SEA POINT NUMBER (MODULO 1000)'
          CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,       &
               &                       AMONOP, NG_R=NLON_RG)
          IGRID=UNPACK(KLON(:,1), L_S_MASK,-999)
          IGRID=MOD(IGRID,1000)
          TITL = 'WEST NEIGHBOUR SEA POINT NUMBER (MODULO 1000)'
          CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,       &
               &                       AMONOP, NG_R=NLON_RG)
          WRITE (IU06,*) ' '
          IGRID=UNPACK(KLON(:,2), L_S_MASK,-999)
          IGRID=MOD(IGRID,1000)
          TITL = 'EAST NEIGHBOUR SEA POINT NUMBER (MODULO 1000)'
          CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,       &
               &                       AMONOP, NG_R=NLON_RG)
          WRITE (IU06,*) ' '
          IGRID=UNPACK(KLAT(:,1,1), L_S_MASK,-999)
          IGRID=MOD(IGRID,1000)
          TITL = 'NEAREST SOUTH NEIGHBOUR SEA POINT NUMBER (MODULO 1000)'
          CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,       &
               &                       AMONOP, NG_R=NLON_RG)
          WRITE (IU06,*) ' '
          IGRID=UNPACK(KLAT(:,1,2), L_S_MASK,-999)
          IGRID=MOD(IGRID,1000)
          TITL = 'SECOND NEAREST SOUTH NEIGHBOUR SEA POINT NUMBER (MODULO 1000)'
          CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,       &
               &                       AMONOP, NG_R=NLON_RG)
          WRITE (IU06,*) ' '
          IGRID=UNPACK(KLAT(:,2,1), L_S_MASK,-999)
          IGRID=MOD(IGRID,1000)
          TITL = 'NEAREST NORTH NEIGHBOUR SEA POINT NUMBER (MODULO 1000)'
          CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,       &
               &                       AMONOP, NG_R=NLON_RG)
          WRITE (IU06,*) ' '
          IGRID=UNPACK(KLAT(:,2,2), L_S_MASK,-999)
          IGRID=MOD(IGRID,1000)
          TITL = 'SECOND NEAREST NORTH NEIGHBOUR SEA POINT NUMBER (MODULO 1000)'
          CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,       &
               &                       AMONOP, NG_R=NLON_RG)
          WRITE (IU06,*) ' '
          IGRID=NINT(UNPACK(WLAT(:,1), L_S_MASK,-999.)*100.)
          TITL = 'WEIGHT OF CLOSEST SOUTH NEIGHBOUR POINT (*100)'
          CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,       &
               &                       AMONOP, NG_R=NLON_RG)
          WRITE (IU06,*) ' '
          IGRID=NINT(UNPACK(WLAT(:,2), L_S_MASK,-999.)*100.)
          TITL = 'WEIGHT OF CLOSEST NORTH NEIGHBOUR POINT (*100)'
          CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,       &
               &                       AMONOP, NG_R=NLON_RG)
          WRITE (IU06,*) ' '

          IF (L_OBSTRUCTION_T) THEN
             IGRID=NINT(UNPACK(OBSLAT (1:NSEA,1,1), L_S_MASK,-999.)*100.)
             TITL = 'OBSTRUCTION (*100) FOR S-N ADVECTION FOR FREQUENCY 1'
             CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,    &
                  &                          AMONOP, NG_R=NLON_RG)
             WRITE (IU06,*) ' '
             IGRID=NINT(UNPACK(OBSLAT (1:NSEA,2,1), L_S_MASK,-999.)*100.)
             TITL = 'OBSTRUCTION (*100) FOR N-S ADVECTION FOR FREQUENCY 1'
             CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,    &
                  &                          AMONOP, NG_R=NLON_RG)
             WRITE (IU06,*) ' '
             IGRID=NINT(UNPACK(OBSLON (1:NSEA,1,1), L_S_MASK,-999.)*100.)
             TITL = 'OBSTRUCTION (*100) FOR W-E ADVECTION FOR FREQUENCY 1'
             CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,    &
                  &                          AMONOP, NG_R=NLON_RG)
             WRITE (IU06,*) ' '
             IGRID=NINT(UNPACK(OBSLON (1:NSEA,2,1), L_S_MASK,-999.)*100.)
             TITL = 'OBSTRUCTION (*100) FOR E-W ADVECTION FOR FREQUENCY 1'
             CALL PRINT_ARRAY (IU06, ZERO, TITL,IGRID, AMOWEP, AMOSOP, AMOEAP,    &
                  &                          AMONOP, NG_R=NLON_RG)
             WRITE (IU06,*) ' '
          END IF
       END IF
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     5. NEST INFORMATION.                                                     !
    !        -----------------                                                     !

    CALL PRINT_NEST_STATUS

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     6. GENERAL INFORMATION.                                                  !
    !        --------------------                                                  !

    CALL PRINT_GENERAL_MODULE

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     7. TABLES INFORMATION.                                                   !
    !        -------------------                                                   !

    CALL PRINT_TABLES_STATUS

  END SUBROUTINE PRINT_PREPROC_STATUS


  ! **************************************************************************** !

  SUBROUTINE check(status, var_name)
    use iso_fortran_env, only: stderr => error_unit, &
                               stdout => output_unit

    implicit none

    INTEGER, intent (in) :: status
    character( len = * ) :: var_name

    IF(status /= NF90_NOERR) THEN
       write(stderr, *) "Warning (",trim(var_name), "): ", TRIM(NF90_STRERROR(status))
       !       STOP "Error while netCDF operation ... Aborting!"
    END IF

  END SUBROUTINE check

  
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE WRITE_PREPROC_FILE

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !   WRITE_PREPROC_FILE - ROUTINE TO WRITE PREPROC OUTPUT TO FILE               !
    !                                                                              !
    !     H.GUNTHER            ECMWF       04/04/1990                              !
    !     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
    !     J. BENKE             FZJ         06/2025                                 !
    !                                                                              !
    !     PURPOSE.                                                                 !
    !     --------                                                                 !
    !                                                                              !
    !       TO WRITE OUT THE COMPUTED CONSTANTS WHICH ARE STORED IN MODULE         !
    !       WAM_CONST_MODULE.                                                      !
    !                                                                              !
    !     METHOD.                                                                  !
    !     -------                                                                  !
    !                                                                              !
    !       UNFORMATTED WRITE AS SPECIFIED TO UNIT = IU07.                         !
    !       FILENAME IS 'FILE07' AS DEFINED IN THE USER INPUT                      !
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

    implicit none

    integer, parameter                 :: stringLen = 200
    integer, parameter                 :: stringLen_c_name = 20
    character(len = stringLen)         :: header_copy
    character(len = stringLen_c_name)  :: c_name_copy

    
    character, dimension(200) :: FILE07_NC
    INTEGER      :: LEN, I, result_max_val
    
    integer :: ncid, varid, status !dimids(NDIMS)

    integer :: dimid_n_nest, dimid_ml, dimid_kl, dimid_max_nbounc, dimid_nbounf
    integer :: dimid_nx, dimid_ny, dimid_nsea, dimid_jumax, dimid_ndepth
    integer :: dimid_result_max_val, dimid_three, dimid_two
    integer :: dimid_stringlen, dimid_stringlen_c_name

    
    ! Section 1
    integer :: varid_header
    
    ! Section 2
    integer :: varid_n_nest, varid_max_nest
    integer :: varid_nbounc, varid_n_name, varid_n_code
    integer :: varid_ijarc, varid_xdello, varid_xdella
    integer :: varid_nsouth, varid_nnorth, varid_neast, varid_nwest
    integer :: varid_blngc, varid_blatc, varid_n_zdel

    ! Section 3
    integer :: varid_nbounf, varid_nbinp, varid_c_name
    integer :: varid_blngf, varid_blatf, varid_ijarf, varid_ibfl, varid_ibfr, varid_bfw

    ! Section 4
    integer :: varid_ml, varid_kl
    integer :: varid_fr, varid_dfim, varid_gom, varid_c, varid_delth, varid_deltr, varid_th
    integer :: varid_costh, varid_sinth, varid_inv_log_co, varid_df, varid_df_fr, varid_df_fr2
    integer :: varid_dfim_ofr

    integer :: varid_dfim_fr, varid_dfim_fr2, varid_fr5, varid_frm5, varid_rhowg_dfim
    integer :: varid_fmin, varid_mo_tail, varid_mm1_tail, varid_mp1_tail, varid_mp2_tail 
    integer :: varid_mpm, varid_kpm, varid_jxo, varid_jyo

    ! Section 5
    integer :: varid_nx, varid_ny, varid_nsea, varid_iper, varid_one_point
    integer :: varid_reduced_grid, varid_l_obstruction_t, varid_nlon_rg, varid_delphi, varid_dellam 
    integer :: varid_sinph, varid_cosph, varid_amowep, varid_amosop, varid_amoeap, varid_amonop

    integer :: varid_zdello, varid_ixlg, varid_kxlt, varid_l_s_mask, varid_klat, varid_klon, varid_wlat
    integer :: varid_depth_b, varid_obslat, varid_obslon

    ! section 6 (grid definition)
    integer :: varid_ndepth, varid_deptha, varid_depthd, varid_depthe
    integer :: varid_flminfr, varid_tcgond, varid_tfak, varid_tsihkd, varid_tfac_st, varid_t_tail
    integer :: varid_delu
 
    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. OPEN FILES and define dimensions                                      !
    !        --------------------------------                                      !

    LEN = LEN_TRIM(FILE07)
    ! OPEN (UNIT=IU07, FILE=FILE07(1:LEN), FORM='UNFORMATTED', STATUS='UNKNOWN')
    FILE07_NC = trim(FILE07) !// "nc"
!    FILE07_NC = trim(FILE07(1:LEN)) !// "nc"
    write(*, *) "File: ", trim(FILE07), "ausgabeende"
    write(*, *) "netCDF file: ", FILE07_NC

    result_max_val = maxval(NBOUNC)
    write( stdout, * ) "maxval (NBOUNC): ", result_max_val
    write( stdout, * ) "NBOUNC = ", NBOUNC

    ! Open File
    call check( nf90_create("./grid/grind_info.nc", NF90_NETCDF4, ncid), "nf90_create" )


    ! Define dimensions
    call check( nf90_def_dim(ncid, "n_nest", N_NEST, dimid_n_nest), "nf90_def_dim N_NEST" )
    call check( nf90_def_dim(ncid, "ml", ML, dimid_ml), "nf90_def_dim ML" ) 
    call check( nf90_def_dim(ncid, "kl", KL, dimid_kl), "nf90_def_dim KL" ) 
    call check( nf90_def_dim(ncid, "nbounf", NBOUNF, dimid_nbounf), "nf90_def_dim NBOUNF" ) 
    call check( nf90_def_dim(ncid, "nx", NX, dimid_nx), "nf90_def_dim NX" ) 
    call check( nf90_def_dim(ncid, "ny", NY, dimid_ny), "nf90_def_dim NY" ) 
    call check( nf90_def_dim(ncid, "dim_nsea", NSEA, dimid_nsea), "nf90_def_dim NSEA" ) 
    call check( nf90_def_dim(ncid, "jumax", JUMAX, dimid_jumax), "nf90_def_dim JUMAX" ) 
    call check( nf90_def_dim(ncid, "dim_ndepth", NDEPTH, dimid_ndepth), "nf90_def_dim NDEPTH" ) 
    call check( nf90_def_dim(ncid, "result_max_val", result_max_val, dimid_result_max_val), "nf90_def_dim maxval nbounc")
    call check( nf90_def_dim(ncid, "dim_three", 3, dimid_three), "nf90_def_dim maxval dim_three")
    call check( nf90_def_dim(ncid, "dim_two", 2, dimid_two), "nf90_def_dim maxval dim_two")
    call check( nf90_def_dim(ncid, "stringlen", stringlen, dimid_stringlen), "nf90_def_dim stringLen")
    call check( nf90_def_dim(ncid, "stringlen_c_name", stringlen_c_name, dimid_stringlen_c_name), "nf90_def_dim stringLen")
    
    
    ! Define variables for netCDF
    header_copy = HEADER
    call check( nf90_def_var(ncid, "header", NF90_CHAR, (/ dimid_stringlen /), varid_header), "nf90_def_var HEADER" )
    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !    2. WRITE COARSE GRID BOUNDARY OUTPUT INFORMATION.  (definition part)      !
    ! ---------------------------------------------------------------------------- !

    call check( nf90_def_var(ncid, "n_nest", NF90_INT, varid_n_nest), "nf90_def_var N_NEST" )
    call check( nf90_def_var(ncid, "max_nest", NF90_INT, varid_max_nest), "nf90_def_var MAX_NEST" )

    call check( nf90_def_var(ncid, "nbounc", NF90_INT, (/dimid_n_nest/), varid_nbounc), "nf90_def_var NBOUNC" )
    call check( nf90_def_var(ncid, "n_name", NF90_CHAR, (/dimid_stringlen, dimid_n_nest/), varid_n_name), "nf90_def_var N_NAME" )
    call check( nf90_def_var(ncid, "n_code", NF90_INT, (/dimid_n_nest/), varid_n_code), "nf90_def_var N_CODE" )

    
    if( result_max_val > 0 ) then
       call check( nf90_def_var(ncid, "xdello", NF90_INT, varid_xdello), "nf90_def_var XDELLO" )
       call check( nf90_def_var(ncid, "xdella", NF90_INT, varid_xdella), "nf90_def_var XDELLA" )
       call check( nf90_def_var(ncid, "n_south", NF90_INT, (/dimid_n_nest/), varid_nsouth), "nf90_def_var N_SOUTH" )
       call check( nf90_def_var(ncid, "n_north", NF90_INT, (/dimid_n_nest/), varid_nnorth), "nf90_def_var N_NORTH" )
       call check( nf90_def_var(ncid, "n_east", NF90_INT, (/dimid_n_nest/), varid_neast), "nf90_def_var N_EAST" )
       call check( nf90_def_var(ncid, "n_west", NF90_INT, (/dimid_n_nest/), varid_nwest), "nf90_def_var N_WEST" )
    
       call check( nf90_def_var(ncid, "ijarc", NF90_INT, (/ dimid_result_max_val, dimid_n_nest /), varid_ijarc), &
            "nf90_def_var IJARC" )
       call check( nf90_def_var(ncid, "blngc", NF90_INT, (/ dimid_result_max_val, dimid_n_nest /), varid_blngc), &
            "nf90_def_var BLONGC" )
       call check( nf90_def_var(ncid, "blatc", NF90_INT, (/ dimid_result_max_val, dimid_n_nest /), varid_blatc), &
            "nf90_def_var BLATC" )
       call check( nf90_def_var(ncid, "n_zdel", NF90_INT, (/ dimid_result_max_val, dimid_n_nest /), varid_n_zdel), &
            "nf90_def_var NZDEL" )
    end if

    write( *, * ) "Definition of nr 2 ended ..."

    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. WRITE FINE GRID BOUNDARY INPUT INFORMATION. (defintion part)          !
    !        -------------------------------------------                           !

    call check( nf90_def_var(ncid, "nbounf", NF90_INT, varid_nbounf), "nf90_def_var NBOUNF" )
    call check( nf90_def_var(ncid, "nbinp", NF90_INT, varid_nbinp), "nf90_def_var NBINP" )
    c_name_copy = C_NAME
    call check( nf90_def_var(ncid, "c_name", NF90_CHAR, (/ dimid_stringlen_c_name /), varid_c_name), "nf90_def_var C_NAME" )
    write(*, *) "C_NAME ... ", trim(C_NAME)
    
    if( NBOUNF > 0 ) then
       call check( nf90_def_var(ncid, "blngf", NF90_INT, (/ dimid_nbounf /), varid_blngf), "nf90_def_var BLNGF" )
       call check( nf90_def_var(ncid, "blatf", NF90_INT, (/ dimid_nbounf /), varid_blatf), "nf90_def_var BLATF" )
       call check( nf90_def_var(ncid, "ijarf", NF90_INT, (/ dimid_nbounf /), varid_ijarf), "nf90_def_var IJARF" )
       call check( nf90_def_var(ncid, "ibfl", NF90_INT, (/ dimid_nbounf /), varid_ibfl), "nf90_def_var IBFL" )
       call check( nf90_def_var(ncid, "ibfr", NF90_INT, (/ dimid_nbounf /), varid_ibfr), "nf90_def_var IBFR" )
       call check( nf90_def_var(ncid, "bfw", NF90_DOUBLE, (/ dimid_nbounf /), varid_bfw), "nf90_def_var BFW" )
    end if
    
    write( *, * ) "Definition of nr 3 ended ..."

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !                4. WRITE FREQUENCY DIRECTION GRID. (definition part)          !
    ! ---------------------------------------------------------------------------- !

    call check( nf90_def_var(ncid, "ml", NF90_INT, varid_ml), "nf90_def_var ML" )
    call check( nf90_def_var(ncid, "kl", NF90_INT, varid_kl), "nf90_def_var KL" )
    call check( nf90_def_var(ncid, "fr", NF90_DOUBLE, (/ dimid_ml /), varid_fr), "nf90_def_var FR" )
    call check( nf90_def_var(ncid, "dfim", NF90_DOUBLE, (/ dimid_ml /), varid_dfim), "nf90_def_var DFIM" )
    call check( nf90_def_var(ncid, "gom", NF90_DOUBLE, (/ dimid_ml /), varid_gom), "nf90_def_var GOM" )

    call check( nf90_def_var(ncid, "c", NF90_DOUBLE,  (/ dimid_ml /), varid_c), "nf90_def_var C" )
    call check( nf90_def_var(ncid, "th", NF90_DOUBLE, (/ dimid_kl /), varid_th), "nf90_def_var TH" )
    call check( nf90_def_var(ncid, "delth", NF90_DOUBLE, varid_delth), "nf90_def_var DELTH" )
    call check( nf90_def_var(ncid, "deltr", NF90_DOUBLE, varid_deltr), "nf90_def_var DELTR" )
    call check( nf90_def_var(ncid, "costh", NF90_DOUBLE, (/ dimid_kl /), varid_costh), "nf90_def_var COSTH" )

    call check( nf90_def_var(ncid, "sinth", NF90_DOUBLE, (/ dimid_kl /), varid_sinth), "nf90_def_var SIMTH" )
    call check( nf90_def_var(ncid, "inv_log_co", NF90_DOUBLE, varid_inv_log_co), "nf90_def_var INV_LOG_CO" )
    call check( nf90_def_var(ncid, "df", NF90_DOUBLE, (/ dimid_ml /), varid_df), "nf90_def_var DF" )
    call check( nf90_def_var(ncid, "df_fr", NF90_DOUBLE, (/ dimid_ml /),varid_df_fr), "nf90_def_var DF_FR" )
    call check( nf90_def_var(ncid, "df_fr2", NF90_DOUBLE, (/ dimid_ml /), varid_df_fr2), "nf90_def_var DF_FR2" )

    call check( nf90_def_var(ncid, "dfim_ofr", NF90_DOUBLE, (/ dimid_ml /), varid_dfim_ofr), "nf90_def_var OFR" )
    call check( nf90_def_var(ncid, "dfim_fr", NF90_DOUBLE, (/ dimid_ml /), varid_dfim_fr), "nf90_def_var DFIM_FR" )
    call check( nf90_def_var(ncid, "dfim_fr2", NF90_DOUBLE, (/ dimid_ml /), varid_dfim_fr2), "nf90_def_var DFIM_FR2" )
    call check( nf90_def_var(ncid, "fr5", NF90_DOUBLE, (/ dimid_ml /), varid_fr5), "nf90_def_var FR5" )
    call check( nf90_def_var(ncid, "frm5", NF90_DOUBLE, (/ dimid_ml /), varid_frm5), "nf90_def_var FRM5" )

    call check( nf90_def_var(ncid, "rhowg_dfim", NF90_DOUBLE, (/ dimid_ml /), varid_rhowg_dfim), "nf90_def_var RHOWG_DFIM" )
    call check( nf90_def_var(ncid, "fmin", NF90_DOUBLE, varid_fmin), "nf90_def_var FMIN" )
    call check( nf90_def_var(ncid, "mo_tail", NF90_DOUBLE, varid_mo_tail), "nf90_def_var MO_TAIL" )
    call check( nf90_def_var(ncid, "mm1_tail", NF90_DOUBLE, varid_mm1_tail), "nf90_def_var MM1_TAIL" )
    call check( nf90_def_var(ncid, "mp1_tail", NF90_DOUBLE, varid_mp1_tail), "nf90_def_var MP1_TAIL" )

    call check( nf90_def_var(ncid, "mp2_tail", NF90_DOUBLE, varid_mp2_tail), "nf90_def_var MP2_TAIL" )
    call check( nf90_def_var(ncid, "mpm", NF90_INT, (/ dimid_ml, dimid_three /), varid_mpm), "nf90_def_var MPM" )
    call check( nf90_def_var(ncid, "kpm", NF90_INT, (/ dimid_kl, dimid_three /), varid_kpm), "nf90_def_var KPM" )
    call check( nf90_def_var(ncid, "jxo", NF90_INT, (/ dimid_kl, dimid_two /), varid_jxo), "nf90_def_var JXO" )
    call check( nf90_def_var(ncid, "jyo", NF90_INT, (/ dimid_kl, dimid_two /), varid_jyo), "nf90_def_varJYO" )

    write( *, * ) "Definition of nr 4 ended ..."

    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     5. WRITE GRID INFORMATION. (definition part)                             !
    !        -----------------------                                               !

    call check( nf90_def_var(ncid, "nx", NF90_INT, varid_nx), "nf90_def_var NX" )
    call check( nf90_def_var(ncid, "ny", NF90_INT, varid_ny), "nf90_def_var NY" )
    call check( nf90_def_var(ncid, "nsea", NF90_INT, varid_nsea), "nf90_def_var NSEA" )
    call check( nf90_def_var(ncid, "iper", NF90_INT, varid_iper), "nf90_def_var IPER" )
    call check( nf90_def_var(ncid, "one_point", NF90_INT, varid_one_point), "nf90_def_var ONE_POINT" )

    call check( nf90_def_var(ncid, "reduced_grid", NF90_INT, varid_reduced_grid), "nf90_def_var REDUCED_GRID" )
    call check( nf90_def_var(ncid, "l_obstruction_t", NF90_INT, varid_l_obstruction_t), "nf90_def_var L_OBSTRUCTION_T" )
    call check( nf90_def_var(ncid, "nlon_rg", NF90_INT, (/ dimid_ny /), varid_nlon_rg), "nf90_def_var NLON_RG" )
    call check( nf90_def_var(ncid, "delphi", NF90_DOUBLE, varid_delphi), "nf90_def_var DELPHI" )
    call check( nf90_def_var(ncid, "dellam", NF90_DOUBLE, (/ dimid_ny /), varid_dellam), "nf90_def_var DELLAM" )

    call check( nf90_def_var(ncid, "sinph", NF90_DOUBLE, (/ dimid_ny /), varid_sinph), "nf90_def_var SINPH" )
    call check( nf90_def_var(ncid, "cosph", NF90_DOUBLE, (/ dimid_ny /), varid_cosph), "nf90_def_var COSPH" )
    call check( nf90_def_var(ncid, "amowep", NF90_INT, varid_amowep), "nf90_def_var AMOWEP" )
    call check( nf90_def_var(ncid, "amosop", NF90_INT, varid_amosop), "nf90_def_var AMOSOP" )
    call check( nf90_def_var(ncid, "amoeap", NF90_INT, varid_amoeap), "nf90_def_var AMOEAP" )

    call check( nf90_def_var(ncid, "amonop", NF90_INT, varid_amonop), "nf90_def_var AMONOP" )
    call check( nf90_def_var(ncid, "zdello", NF90_DOUBLE, (/ dimid_ny /), varid_zdello), "nf90_def_var ZDELLO" )
    call check( nf90_def_var(ncid, "ixlg", NF90_INT, (/ dimid_nsea /), varid_ixlg), "nf90_def_var IXLG" )
    call check( nf90_def_var(ncid, "kxlt", NF90_INT, (/ dimid_nsea /), varid_kxlt), "nf90_def_var KXLT" )
    call check( nf90_def_var(ncid, "l_s_mask", NF90_INT, varid_l_s_mask), "nf90_def_var L_S_MASK" )

    call check( nf90_def_var(ncid, "klat", NF90_INT, (/ dimid_nsea, dimid_two, dimid_two /), varid_klat), "nf90_def_var KLAT" )
    call check( nf90_def_var(ncid, "klon", NF90_INT, (/ dimid_nsea, dimid_two /), varid_klon), "nf90_def_var KLON" )
    call check( nf90_def_var(ncid, "wlat", NF90_INT, (/ dimid_nsea, dimid_two /), varid_wlat), "nf90_def_var WLAT" )
    call check( nf90_def_var(ncid, "depth_b", NF90_DOUBLE, (/ dimid_nsea /), varid_depth_b), "nf90_def_var DEPTH_B" )

    if( L_OBSTRUCTION_T .eqv. .TRUE.) then
       call check( nf90_def_var(ncid, "obslat", NF90_DOUBLE, (/ dimid_nsea, dimid_two, dimid_ml /), varid_obslat), &
            "nf90_def_var OBSLAT" )
       call check( nf90_def_var(ncid, "obslon", NF90_DOUBLE, (/ dimid_nsea, dimid_two, dimid_ml /), varid_obslon), &
            "nf90_def_var OBSLON" )
    end if

    write( *, * ) "Definition of nr 5 ended ..."


    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     6. WRITE TABLES. (definition part)                                       !
    !        -------------                                                         !

    call check( nf90_def_var(ncid, "ndepth", NF90_INT, varid_ndepth), "nf90_def_var NDEPTH" )
    call check( nf90_def_var(ncid, "deptha", NF90_DOUBLE, varid_deptha), "nf90_def_var DEPTHA" )
    call check( nf90_def_var(ncid, "depthd", NF90_DOUBLE, varid_depthd), "nf90_def_var DEPTHD" )
    call check( nf90_def_var(ncid, "depthe", NF90_DOUBLE, varid_depthe), "nf90_def_var DEPTHE" )
    call check( nf90_def_var(ncid, "flminfr", NF90_DOUBLE, (/ dimid_jumax, dimid_ml /), varid_flminfr), "nf90_def_var FLMINFR" )

    call check( nf90_def_var(ncid, "tcgond", NF90_DOUBLE, (/ dimid_ndepth, dimid_ml /), varid_tcgond), "nf90_def_var TCGOND" )
    call check( nf90_def_var(ncid, "tfak", NF90_DOUBLE, (/ dimid_ndepth, dimid_ml /), varid_tfak), "nf90_def_var TFAK" )
    call check( nf90_def_var(ncid, "tsihkd", NF90_DOUBLE, (/ dimid_ndepth, dimid_ml /), varid_tsihkd), "nf90_def_var TSIHKD" )
    call check( nf90_def_var(ncid, "tfac_st", NF90_DOUBLE, (/ dimid_ndepth, dimid_ml /), varid_tfac_st), "nf90_def_var TFAC_ST" )
    call check( nf90_def_var(ncid, "t_tail", NF90_DOUBLE, (/ dimid_ndepth, dimid_ml /), varid_t_tail), "nf90_def_var T_TAIL" )

    call check( nf90_def_var(ncid, "delu", NF90_DOUBLE, varid_delu), "nf90_def_var DELU" )


    call check( nf90_enddef(ncid), "nf90_enddef" )

    write( *, * ) "Definition of nr 6 ended ..."

    !
    ! End of definition
    !


    !
    ! Write to netCDF file
    !
    
    call check( nf90_put_var(ncid, varid_header, trim(header_copy)), "nf90_put_var HEADER" )

    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !      2. WRITE COARSE GRID BOUNDARY OUTPUT INFORMATION. (write part)          ! 
    ! ---------------------------------------------------------------------------- !

    call check( nf90_put_var(ncid, varid_n_nest, N_NEST), "nf90_put_var N_NEST" )
    call check( nf90_put_var(ncid, varid_max_nest, MAX_NEST), "nf90_put_var MAX_NEST" )

    call check( nf90_put_var(ncid, varid_nbounc, NBOUNC), "nf90_put_var NBOUNC_I" )
    call check( nf90_put_var(ncid, varid_n_name, N_NAME), "nf90_put_var N_NAME_I" )
    call check( nf90_put_var(ncid, varid_n_code, N_CODE), "nf90_put_var N_CODE_I" )

    if( result_max_val > 0 ) then
       call check( nf90_put_var(ncid, varid_xdello, XDELLO), "nf90_put_var XDELLO" )
       call check( nf90_put_var(ncid, varid_xdella, XDELLA), "nf90_put_var XDELLA" )
       call check( nf90_put_var(ncid, varid_nnorth, N_NORTH), "nf90_put_var N_NORTH" )
       call check( nf90_put_var(ncid, varid_nsouth, N_SOUTH), "nf90_put_var N_SOUTH" )
       call check( nf90_put_var(ncid, varid_neast, N_EAST), "nf90_put_var N_EAST" )
       call check( nf90_put_var(ncid, varid_nwest, N_WEST), "nf90_put_var N_WEST" )

       call check( nf90_put_var(ncid, varid_ijarc, IJARC), "nf90_put_var IJARC" )
       call check( nf90_put_var(ncid, varid_blngc, BLNGC), "nf90_put_var BLNGC" )
       call check( nf90_put_var(ncid, varid_blatc, BLATC), "nf90_put_var BLATC" )
       call check( nf90_put_var(ncid, varid_n_zdel, N_ZDEL), "nf90_put_var N_ZDEL" )
    end if
    
    write( *, * ) "Writing of nr 2 ended ..."

    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. WRITE FINE GRID BOUNDARY INPUT INFORMATION.                           !
    !        -------------------------------------------                           !

    call check( nf90_put_var(ncid, varid_nbounf, NBOUNF), "nf90_put_var NBOUNF" )
    call check( nf90_put_var(ncid, varid_nbinp, NBINP), "nf90_put_var NBINP" ) 
    call check( nf90_put_var(ncid, varid_c_name, trim(c_name_copy)), "nf90_put_var C_NAME" )
!    call check( nf90_put_var(ncid, varid_c_name, C_NAME), "nf90_put_var C_NAME" )

    if( NBOUNF > 0 ) then
       call check( nf90_put_var(ncid, varid_blngf, BLNGF), "nf90_put_var BLNGF" )
       call check( nf90_put_var(ncid, varid_blatf, BLATF), "nf90_put_var BLATF" )
       call check( nf90_put_var(ncid, varid_ijarf, IJARF), "nf90_put_var IJARF" )
       call check( nf90_put_var(ncid, varid_ibfl, IBFL), "nf90_put_var IBFL" )
       call check( nf90_put_var(ncid, varid_ibfr, IBFR), "nf90_put_var IBFR" )
       call check( nf90_put_var(ncid, varid_bfw, BFW), "nf90_put_var BFW" )
       ! before changing: call check( nf90_put_var(ncid, varid_bfw, BFW(1:NBOUNF)), "nf90_put_var BFW" )
    end if
       
    write( *, * ) "Writing of nr 3 ended ..."


    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !                4. WRITE FREQUENCY DIRECTION GRID. (write part)               !
    ! ---------------------------------------------------------------------------- !

    call check( nf90_put_var(ncid, varid_ml, ML), "nf90_put_var ML" )
    call check( nf90_put_var(ncid, varid_kl, KL), "nf90_put_var KL" )
    call check( nf90_put_var(ncid, varid_fr, FR), "nf90_put_var FR" )
    call check( nf90_put_var(ncid, varid_dfim, DFIM), "nf90_put_var DFIM" )
    call check( nf90_put_var(ncid, varid_gom, GOM), "nf90_put_var GOM" )

    
    call check( nf90_put_var(ncid, varid_c, C), "nf90_put_var C" )
    call check( nf90_put_var(ncid, varid_delth, DELTH), "nf90_put_var DELTH" )
    call check( nf90_put_var(ncid, varid_deltr, DELTR), "nf90_put_var DELTR" )
    call check( nf90_put_var(ncid, varid_th, TH), "nf90_put_var TH" )
    call check( nf90_put_var(ncid, varid_costh, COSTH), "nf90_put_var COSTH" )
    call check( nf90_put_var(ncid, varid_sinth, SINTH), "nf90_put_var SINTH" )
    
    call check( nf90_put_var(ncid, varid_inv_log_co, INV_LOG_CO), "nf90_put_var INV_LOG_CO" )
    call check( nf90_put_var(ncid, varid_df, DF), "nf90_put_var DF" )
    call check( nf90_put_var(ncid, varid_df_fr, DF_FR), "nf90_put_var DF_FR" )
    call check( nf90_put_var(ncid, varid_df_fr2, DF_FR2), "nf90_put_var DF_FR2" )
    call check( nf90_put_var(ncid, varid_dfim_ofr, DFIMOFR), "nf90_put_var DFIMOFR" )

    call check( nf90_put_var(ncid, varid_dfim_fr, DFIM_FR), "nf90_put_var DFIM_FR" )
    call check( nf90_put_var(ncid, varid_dfim_fr2, DFIM_FR2), "nf90_put_var DFIM_FR2" )
    call check( nf90_put_var(ncid, varid_fr5, FR5), "nf90_put_var FR5" )
    call check( nf90_put_var(ncid, varid_frm5, FRM5), "nf90_put_var FRM5" )
    call check( nf90_put_var(ncid, varid_rhowg_dfim, RHOWG_DFIM), "nf90_put_var RHOWG_DFIM" )

    call check( nf90_put_var(ncid, varid_fmin, FMIN), "nf90_put_var FMIN" )
    call check( nf90_put_var(ncid, varid_mo_tail, MO_TAIL), "nf90_put_var MO_TAIL" )
    call check( nf90_put_var(ncid, varid_mm1_tail, MM1_TAIL), "nf90_put_var MM1_TAIL" )
    call check( nf90_put_var(ncid, varid_mp1_tail, MP1_TAIL), "nf90_put_var MP1_TAIL" )
    call check( nf90_put_var(ncid, varid_mp2_tail, MP2_TAIL), "nf90_put_var MP2_TAIL" )

    call check( nf90_put_var(ncid, varid_mpm, MPM), "nf90_put_var MPM" )
    call check( nf90_put_var(ncid, varid_kpm, KPM), "nf90_put_var KPM" )
    call check( nf90_put_var(ncid, varid_jxo, JXO), "nf90_put_var JXO" )
    call check( nf90_put_var(ncid, varid_jyo, JYO), "nf90_put_var JYO" )

    write( *, * ) "Writing of nr 4 ended ..."
        

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     5. WRITE GRID INFORMATION.                                               !
    !        -----------------------                                               !

    call check( nf90_put_var(ncid, varid_nx, NX), "nf90_put_var NX" )
    call check( nf90_put_var(ncid, varid_ny, NY), "nf90_put_var NY" )
    call check( nf90_put_var(ncid, varid_nsea, NSEA), "nf90_put_var NSEA" )
    
    call check( nf90_put_var(ncid, varid_iper, merge(1, 0, IPER)), "nf90_put_var IPER" )
    call check( nf90_put_var(ncid, varid_one_point, merge(1, 0, ONE_POINT)), "nf90_put_var one_point" )
    call check( nf90_put_var(ncid, varid_reduced_grid, merge(1, 0, REDUCED_GRID)), "nf90_put_var REDUCED_GRID" )
    call check( nf90_put_var(ncid, varid_l_obstruction_t, merge(1, 0, L_OBSTRUCTION_T)), "nf90_put_var L_OBSTRUCTION_T" )
    call check( nf90_put_var(ncid, varid_nlon_rg, NLON_RG), "nf90_put_var NLON_RG" )
    call check( nf90_put_var(ncid, varid_delphi, DELPHI), "nf90_put_var DELPHI" )
    call check( nf90_put_var(ncid, varid_dellam, DELLAM), "nf90_put_var DELLAM" )
    
    call check( nf90_put_var(ncid, varid_sinph, SINPH), "nf90_put_var SINPH" )
    call check( nf90_put_var(ncid, varid_cosph, COSPH), "nf90_put_var COSPH" )
    call check( nf90_put_var(ncid, varid_amowep, AMOWEP), "nf90_put_var AMOWEP" )
    call check( nf90_put_var(ncid, varid_amosop, AMOSOP), "nf90_put_var AMOSOP" )
    call check( nf90_put_var(ncid, varid_amoeap, AMOEAP), "nf90_put_var AMOEAP" )
    call check( nf90_put_var(ncid, varid_amonop, AMONOP), "nf90_put_var AMONOP" )

    call check( nf90_put_var(ncid, varid_delphi, DELPHI), "nf90_put_var DELPHI" )
    call check( nf90_put_var(ncid, varid_dellam, DELLAM), "nf90_put_var DELLAM" )
    call check( nf90_put_var(ncid, varid_xdello, XDELLO), "nf90_put_var XDELLO" )
    call check( nf90_put_var(ncid, varid_xdella, XDELLA), "nf90_put_var XDELLA" )
    call check( nf90_put_var(ncid, varid_zdello, ZDELLO), "nf90_put_var ZDELLO" )

    call check( nf90_put_var(ncid, varid_ixlg, IXLG), "nf90_put_var IXLG" )
    call check( nf90_put_var(ncid, varid_kxlt, KXLT), "nf90_put_var KXLT" )
!    call check( nf90_put_var(ncid, varid_l_s_mask, L_S_MASK), "nf90_put_var L_S_MASK" )
    call check( nf90_put_var(ncid, varid_klat, KLAT), "nf90_put_var KLAT" )
    call check( nf90_put_var(ncid, varid_klon, KLON), "nf90_put_var KLON" )

    call check( nf90_put_var(ncid, varid_wlat, WLAT), "nf90_put_var WLAT" )
    call check( nf90_put_var(ncid, varid_depth_b, DEPTH_B), "nf90_put_var DEPTH_B" )

    if( L_OBSTRUCTION_T .eqv. .TRUE.) then
       call check( nf90_put_var(ncid, varid_obslat, OBSLAT), "nf90_put_var OBSLAT" )
       call check( nf90_put_var(ncid, varid_obslon, OBSLON), "nf90_put_var OBSLON" )
    end if

    write( *, * ) "Writing of nr 5 ended ..."

    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     6. WRITE TABLES.                                                         !
    !        -------------                                                         !

    call check( nf90_put_var(ncid, varid_ndepth, NDEPTH), "nf90_put_var NDEPTH" )
    call check( nf90_put_var(ncid, varid_deptha, DEPTHA), "nf90_put_var DEPTHA" )
    call check( nf90_put_var(ncid, varid_depthd, DEPTHD), "nf90_put_var DEPTHD" )
    call check( nf90_put_var(ncid, varid_depthe, DEPTHE), "nf90_put_var DEPTHE" )
    
    call check( nf90_put_var(ncid, varid_flminfr, FLMINFR), "nf90_put_var FLMINFR" )
    call check( nf90_put_var(ncid, varid_tcgond, TCGOND), "nf90_put_var TCGOND" ) 
    call check( nf90_put_var(ncid, varid_tfak, TFAK), "nf90_put_var TFAK" )
    call check( nf90_put_var(ncid, varid_tfac_st, TFAC_ST), "nf90_put_var TFAC_ST" )
    call check( nf90_put_var(ncid, varid_tsihkd, TSIHKD), "nf90_put_var TSIHKD" )
    call check( nf90_put_var(ncid, varid_t_tail, T_TAIL), "nf90_put_var T_TAIL" )
    call check( nf90_put_var(ncid, varid_delu, DELU), "nf90_put_var DELU" )

    write( *, *) "Wroting of nr 6 ended ..."
    
    
    !
    ! End of writing to netCDF file
    !

    call check( nf90_close(ncid), "NF90_CLOSE" )

    write(stdout, *) "netCDF file closed!"
    

    !
    ! Binary format
    !

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     LOCAL VARIABLES.                                                         !
    !     ----------------                                                         !

    !INTEGER      :: LEN, I

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. OPEN FILES.                                                           !
    !        -----------                                                           !

    LEN = LEN_TRIM(FILE07)
    OPEN (UNIT=IU07, FILE=FILE07(1:LEN), FORM='UNFORMATTED', STATUS='UNKNOWN')

    WRITE(IU07) HEADER

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. WRITE COARSE GRID BOUNDARY OUTPUT INFORMATION.                        !
    !        ----------------------------------------------                        !

    WRITE (IU07) N_NEST, MAX_NEST
    DO I=1,N_NEST
       WRITE (IU07) NBOUNC(I), N_NAME(I), n_code(i)
       IF (NBOUNC(I).GT.0) THEN
          WRITE(IU07) IJARC(1:NBOUNC(I),I)
          WRITE(IU07) XDELLO, XDELLA, N_SOUTH(I), N_NORTH(I), N_EAST(I), N_WEST(I),&
               &              BLNGC(1:NBOUNC(I),I), BLATC(1:NBOUNC(I),I), N_ZDEL(1:NBOUNC(I),I)
       END IF
    END DO

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. WRITE FINE GRID BOUNDARY INPUT INFORMATION.                           !
    !        -------------------------------------------                           !

    WRITE(IU07) NBOUNF, NBINP, C_NAME
    IF (NBOUNF.GT.0) THEN
       WRITE(IU07) BLNGF(1:NBOUNF), BLATF(1:NBOUNF), IJARF(1:NBOUNF),              &
            &              IBFL(1:NBOUNF), IBFR(1:NBOUNF), BFW(1:NBOUNF)
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     4. WRITE FREQUENCY DIRECTION GRID.                                       !
    !        ------------------------------                                        !

    WRITE (IU07) ML, KL
    WRITE (IU07) FR, DFIM, GOM, C, DELTH, DELTR, TH, COSTH, SINTH, INV_LOG_CO,     &
         &            DF, DF_FR, DF_FR2, DFIM, DFIMOFR, DFIM_FR, DFIM_FR2, FR5, FRM5,   &
         &            RHOWG_DFIM,                                                       &
         &            FMIN, MO_TAIL, MM1_TAIL, MP1_TAIL, MP2_TAIL
    WRITE (IU07) MPM, KPM, JXO, JYO

    write(*, *) "FR: ", FR
    WRITE (*, *) "MPM: ", MPM
    write(*, *) "KPM: ", KPM
    write(*, *) "JXO: ", JXO
    write(*, *) "JYO: ", JYO

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     5. WRITE GRID INFORMATION.                                               !
    !        -----------------------                                               !

    WRITE (IU07) NX, NY, NSEA, IPER, ONE_POINT, REDUCED_GRID, L_OBSTRUCTION_T
    WRITE (IU07) NLON_RG
    WRITE (IU07) DELPHI, DELLAM, SINPH, COSPH, AMOWEP, AMOSOP, AMOEAP, AMONOP,     &
         &            XDELLA, XDELLO, ZDELLO
    WRITE (IU07) IXLG, KXLT, L_S_MASK
    WRITE (IU07) KLAT, KLON, WLAT, DEPTH_B
    IF (L_OBSTRUCTION_T) THEN
       WRITE (IU07) OBSLAT, OBSLON
    END IF
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     6. WRITE TABLES.                                                         !
    !        -------------                                                         !

    WRITE (IU07) NDEPTH, DEPTHA, DEPTHD, DEPTHE
    WRITE (IU07) FLMINFR, TCGOND, TFAK, TSIHKD, TFAC_ST, T_TAIL
    WRITE (IU07) DELU

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !    10. CLOSE FILES.                                                          !
    !        ------------                                                          !

    CLOSE (UNIT=IU07, STATUS="KEEP")

     call read_preproc_file_netcdf
    
  END SUBROUTINE WRITE_PREPROC_FILE




  SUBROUTINE READ_PREPROC_FILE_NETCDF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !   WRITE_PREPROC_FILE - ROUTINE TO READ PREPROC NETCDF OUTPUT FROMO FILE      !
    !                                                                              !
    !     J. BENKE               FZJ          06/2025                              !
    !                                                                              !
    !     PURPOSE.                                                                 !
    !     --------                                                                 !
    !                                                                              !
    !       TO READ IN THE COMPUTED CONSTANTS FROM NETCDF WHICH ARE STORED         !
    !       IN MODULE WAM_CONST_MODULE.                                            !
    !                                                                              !
    !     METHOD.                                                                  !
    !     -------                                                                  !
    !                                                                              !
    !       NETCDF WRITE AS SPECIFIED TO UNIT = IU07.                              !
    !       FILENAME IS 'FILE07' AS DEFINED IN THE USER INPUT                      !
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
    
    implicit none
    
    character, dimension(200) :: FILE07_NC

    INTEGER :: LEN, i = 1
    integer :: n_dims, n_vars, n_vars_fixed, n_attrs, k_un
    integer :: ncid, varid, status !dimids(NDIMS)
    integer :: dimid_n_nest, dimid_ml, dimid_kl, dimid_max_nbounc, dimid_nbounf
    integer :: dimid_nx, dimid_ny, dimid_nsea, dimid_jumax, dimid_ndepth
    
    integer, allocatable, dimension(:) :: len_of_dim, id_of_dim
    character(len = 50), allocatable, dimension(:) :: name_of_dim

    integer, allocatable, dimension(:) :: id_of_var, ndim_of_var, xtype_of_var, dimids
    character(len = 30), allocatable, dimension(:) :: name_of_var
    !    character(len = *) :: name_of_string
    logical l_obstruction 
    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. OPEN FILES and define dimensions                                      !
    !        --------------------------------                                      !

    LEN = LEN_TRIM(FILE07)
    ! OPEN (UNIT=IU07, FILE=FILE07(1:LEN), FORM='UNFORMATTED', STATUS='UNKNOWN')
    ! FILE07_NC = trim(FILE07(1:LEN) // "_netcdf.nc")

    
    ! Open File
    call check( nf90_open("./grid/grind_info.nc", NF90_NOWRITE, ncid), "nf90_open" )
    write(stdout, *)
    write(stdout, *) "NetCDF file opened for reading ..."
    write(stdout, *)


    call check( nf90_inquire( ncid, n_dims, n_vars, n_attrs, k_un ), "nf90_inquire" )
    write(stdout, *)
    write(stdout, *) "n_dims = ", n_dims
    write(stdout, *) "n_vars = ", n_vars
    write(stdout, *) "n_attrs = ", n_attrs
    write(stdout, *) "k_un = ", k_un
    write(stdout, *)
    
    ! Create list of type dimension_attr and dimids
    write( stdout, *) "Allocation starts ..."
    allocate(len_of_dim(n_dims))
    allocate(name_of_dim(n_dims))
    allocate(id_of_dim(n_dims))
    allocate(dimids(n_dims))
    
    write( stdout, * ) "Allocation ends ..."

    write(*, *) "zuweisung starts ..."
    write( stdout, * ) "n_dims = ", n_dims
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

    
!    name_of_dim(10) = "header"
    write(*, *) "Zuweisung ends ..."


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
    call check( nf90_inquire_dimension(ncid, id_of_dim(6), name_of_dim(6), len_of_dim(6)), "nf90_inq_dim NX" )

    call check( nf90_inq_dimid(ncid, name_of_dim(7), id_of_dim(7)), "nf90_inq_dim NSEA" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(7), name_of_dim(7), len_of_dim(7)), "nf90_inq_dim NSEA" )

    call check( nf90_inq_dimid(ncid, name_of_dim(8), id_of_dim(8)), "nf90_inq_dim JUMAX" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(8), name_of_dim(8), len_of_dim(8)), "nf90_inq_dim JUMAX" )

    call check( nf90_inq_dimid(ncid, name_of_dim(9), id_of_dim(9)), "nf90_inq_dim NDEPTH" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(9), name_of_dim(9), len_of_dim(9)), "nf90_inq_dim NDEPTH" )

    call check( nf90_inq_dimid(ncid, name_of_dim(10), id_of_dim(10)), "nf90_inq_dim result_max_val" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(10), name_of_dim(10), len_of_dim(10)), "nf90_inq_dim result_max_val" )

    call check( nf90_inq_dimid(ncid, name_of_dim(11), id_of_dim(11)), "nf90_inq_dim DIM_THREE" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(11), name_of_dim(11), len_of_dim(11)), "nf90_inq_dim DIM_THREE" )

    call check( nf90_inq_dimid(ncid, name_of_dim(12), id_of_dim(12)), "nf90_inq_dim DIM_TWO" )
    call check( nf90_inquire_dimension(ncid, id_of_dim(12), name_of_dim(12), len_of_dim(12)), "nf90_inq_dim DIM_TWO" )

    do i = 1, n_dims
       write( stdout, * ) "name_of_dim(", i, "): ", name_of_dim(i)
       write( stdout, * ) "id_of_dim(", i, "): ", id_of_dim(i)
       write( stdout, * ) "len_of_dim(", i, "): ", len_of_dim(i)
    end do

    ! DEfine all variabless


    n_vars_fixed = 91
    allocate( name_of_var( n_vars_fixed ) )
    allocate( id_of_var( n_vars_fixed ) )
    allocate( xtype_of_var( n_vars_fixed ) )
    allocate( ndim_of_var( n_vars_fixed ) )

    name_of_var(1) = "n_nest"
    name_of_var(2) = "max_nest"
    name_of_var(3) = "nbounc"      
    name_of_var(4) = "n_name"
    name_of_var(5) = "n_code"
    name_of_var(6) = "xdello"
    name_of_var(7) = "xdella"
    name_of_var(8) = "n_south"
    name_of_var(9) = "n_north"
    name_of_var(10) = "n_east"
    name_of_var(11) = "n_west" 
    name_of_var(12) = "ijarc"
    name_of_var(13) = "blngc"
    name_of_var(14) = "blatc"
    name_of_var(15) = "n_zdel"
    name_of_var(16) = "ml"
    name_of_var(17) = "kl"
    name_of_var(18) = "fr"
    name_of_var(19) = "dfim"
    name_of_var(20) = "gom"
    name_of_var(21) =  "c"
    name_of_var(22) =  "th"
    name_of_var(23) = "costh"
    name_of_var(24) = "sinth"
    name_of_var(25) = "delth"
    name_of_var(26) = "deltr"
    name_of_var(27) = "inv_log_co"
    name_of_var(28) = "df"
    name_of_var(29) = "df_fr"
    name_of_var(30) = "df_fr2"
    name_of_var(31) = "dfim_ofr"
    name_of_var(32) = "dfim_fr"
    name_of_var(33) = "dfim_fr2"
    name_of_var(34) = "fr5"
    name_of_var(35) = "frm5"
    name_of_var(36) = "rhowg_dfim"
    name_of_var(37) = "fmin"
    name_of_var(38) = "mo_tail"
    name_of_var(39) = "mm1_tail"
    name_of_var(40) = "mp1_tail"
    name_of_var(41) = "mp2_tail"
    name_of_var(42) = "mpm"
    name_of_var(43) = "kpm"
    name_of_var(44) = "jxo"
    name_of_var(45) = "jyo"
    name_of_var(46) = "nbounf"
    name_of_var(47) = "nbinp"
    name_of_var(48) = "c_name"
    name_of_var(49) = "blngf"
    name_of_var(50) = "blatf"
    name_of_var(51) = "ijarf"
    name_of_var(52) = "ibfl"
    name_of_var(53) = "ibfr"
    name_of_var(54) = "bfw"
    name_of_var(55) = "nx"
    name_of_var(56) = "ny"
    name_of_var(57) = "nsea"
    name_of_var(58) = "iper"
    name_of_var(59) = "one_point"
    name_of_var(60) = "reduced_grid"
    name_of_var(61) = "l_obstruction"
    name_of_var(62) = "obslat"
    name_of_var(63) = "obslon"
    name_of_var(64) = "nlon_rg"
    name_of_var(65) = "delphi"
    name_of_var(66) = "dellam"
    name_of_var(67) = "sinph"
    name_of_var(68) = "cosph"
    name_of_var(69) = "amowep"
    name_of_var(70) = "amosop"
    name_of_var(71) = "amoeap"
    name_of_var(72) = "amonop"
    name_of_var(73) = "zdello"
    name_of_var(74) = "ixlg"
    name_of_var(75) = "kxlt"
    name_of_var(76) = "l_s_mask"
    name_of_var(77) = "klat"
    name_of_var(78) = "klon"
    name_of_var(79) = "wlat"
    name_of_var(80) = "depth_b"
    name_of_var(81) = "ndepth"
    name_of_var(82) = "deptha"
    name_of_var(83) = "depthb"
    name_of_var(84) = "depthe"
    name_of_var(85) = "flminfr"
    name_of_var(86) = "tcgond"
    name_of_var(87) = "tfak"
    name_of_var(88) = "tsihkd"
    name_of_var(89) = "tfac_st"
    name_of_var(90) = "t_tail"
    name_of_var(91) = "delu"

    
!    name_of_var = [ "n_nest  ", "max_nest", "nbounc", "n_name", "n_code", "xdello", "xdella", "n_south", "n_north", "n_east", &
!         "n_west", "ijarc", "blngc", "blatc", "n_zdel", "ml", "kl", "fr", "dfim", "gom", &
!         "c", "th", "costh", "sinth", "delth", "deltr", "inv_log_co", "df", "df_fr", "df_fr2", &
!         "dfim_ofr", "dfim_fr", "dfim_fr2", "fr5", "frm5", "rhowg_dfim", "fmin", "mo_tail", "mm1_tail", "mp1_tail", &
!         "mp2_tail", "mpm", "kpm", "jxo", "jyo", "nbounf", "nbinp", "c_name", "blngf", "blatf", &
!         "ijarf", "ibfl", "ibfr", "bfw", "nx", "ny", "nsea", "iper", "one_point", "reduced_grid", &
!         "l_obstruction", "obslat", "obslon", "nlon_rg", "delphi", "dellam", "sinph", "cosph", "amowep", "amosop", &
!         "amoeap", "amonop", "zdello", "ixlg", "kxlt", "l_s_mask", "klat", "klon", "wlat", "depth_b", &
!         "ndepth", "deptha", "depthb", "depthe", "flminfr", "tcgond", "tfak", "tsihkd", "tfac_st", "t_tail", &
!         "delu"]


    i = 1
    do 
       call check( nf90_inq_varid(ncid, trim(name_of_var(i)), id_of_var(i) ), "nf90_inq_varid " // trim(name_of_var(i)) )
       if( (i == 46) .and. (NBOUNF <= 0)) then
          i = i + 6
       else if( (i == 61) .and. (l_obstruction .eqv. .true.)) then
          i = i + 2
       else
          i = i + 1
       end if

       write( stdout, * ) "Id of var: ", id_of_var(i)

       
       if( i > n_vars_fixed ) then
          exit
       endif
    end do

    i = 1
    do 
       call check( nf90_inquire_variable( ncid = ncid, varid = id_of_var(i), xtype = xtype_of_var(i), &
            ndims = ndim_of_var(i), dimids = dimids ), "nf90_inquire_variable " // trim(name_of_var(i)) )
       if( (i == 46) .and. (NBOUNF <= 0)) then
          i = i + 6
       else if( (i == 61) .and. (l_obstruction .eqv. .true.)) then
          i = i + 2
       else
          i = i + 1
       end if

       dimids = -999
       if( i > n_vars_fixed ) then
          exit
       endif
    end do

    write( stdout, * ) "n_vars = ", n_vars
    do i = 1, n_vars
       write( stdout, * ) "varid = ", id_of_var(i)
       write( stdout, * ) "name of var = ", name_of_var(i)
       write( stdout, * ) "xtype of var = ", xtype_of_var(i)
       write( stdout, * ) "ndims of var = ", ndim_of_var(i)
       write( stdout, * ) "dimids = ", dimids 
       write( stdout, * )
    end do

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !    2. WRITE COARSE GRID BOUNDARY OUTPUT INFORMATION.  (definition part)      !
    ! ---------------------------------------------------------------------------- !

    call check( nf90_get_var(ncid, id_of_var(1), n_nest), "nf90_get_var n_nest" )
    call check( nf90_get_var(ncid, id_of_var(2), max_nest), "nf90_get_var max_nest" )
    call check( nf90_get_var(ncid, id_of_var(3), nbounc), "nf90_get_var nbounc" )
    call check( nf90_get_var(ncid, id_of_var(4), n_name), "nf90_get_var n_name" )
    call check( nf90_get_var(ncid, id_of_var(5), n_code), "nf90_get_var n_code" )
    call check( nf90_get_var(ncid, id_of_var(6), xdello), "nf90_get_var xdello" )
    call check( nf90_get_var(ncid, id_of_var(7), xdella), "nf90_get_var xdella" )
    call check( nf90_get_var(ncid, id_of_var(8), n_south), "nf90_get_var n_south" )
    call check( nf90_get_var(ncid, id_of_var(9), n_north), "nf90_get_var n_north" )
    call check( nf90_get_var(ncid, id_of_var(10), n_east), "nf90_get_var n_east" )

    write( stdout, *) "After reading: n_nest = ", n_nest, ", max_nest = ", max_nest
    write( stdout, *) "After reading: nbounc = ", nbounc, ", n_name = ", n_name
    write( stdout, *) "After reading: n_code = ", n_code, ", xdello = ", xdello
    write( stdout, *) "After reading: xdella = ", xdella, ", n_south = ", n_south
    write( stdout, *) "After reading: n_north = ", n_north, ", n_east = ", n_east

!  "n_west", "ijarc", "blngc", "blatc", "n_zdel", "ml", "kl", "fr", "dfim", "gom", &

    call check( nf90_get_var(ncid, id_of_var(11), n_west), "nf90_get_var n_wnest" )
    call check( nf90_get_var(ncid, id_of_var(12), ijarc), "nf90_get_varijarc" )
    call check( nf90_get_var(ncid, id_of_var(13), blngc), "nf90_get_var blngc" )
    call check( nf90_get_var(ncid, id_of_var(14), blatc), "nf90_get_var blatc" )
    call check( nf90_get_var(ncid, id_of_var(15), n_zdel), "nf90_get_var n_zdel" )
    call check( nf90_get_var(ncid, id_of_var(16), ml), "nf90_get_var ml" )
    call check( nf90_get_var(ncid, id_of_var(17), kl), "nf90_get_var kl" )
    call check( nf90_get_var(ncid, id_of_var(18), fr), "nf90_get_var fr" )
    call check( nf90_get_var(ncid, id_of_var(19), dfim), "nf90_get_var dfim" )
    call check( nf90_get_var(ncid, id_of_var(20), gom), "nf90_get_var gom" )

    write( stdout, *) "After reading: n_west = ", n_west, ", ijarc = ", ijarc
    write( stdout, *) "After reading: blngc = ", blngc, ", blatc = ", blatc
    write( stdout, *) "After reading: n_zdel = ", n_zdel, ", ml = ", ml
    write( stdout, *) "After reading: kl = ", kl, ", fr = ", fr
    write( stdout, *) "After reading: dfim = ", dfim, ", gom = ", gom

    ! "c", "th", "costh", "sinth", "delth", "deltr", "inv_log_co", "df", "df_fr", "df_fr2", &
 
    call check( nf90_get_var(ncid, id_of_var(21), c), "nf90_get_var c" )
    call check( nf90_get_var(ncid, id_of_var(22), th), "nf90_get_var th" )
    call check( nf90_get_var(ncid, id_of_var(23), costh), "nf90_get_var costh" )
    call check( nf90_get_var(ncid, id_of_var(24), sinth), "nf90_get_var sinth" )
    call check( nf90_get_var(ncid, id_of_var(25), delth), "nf90_get_var delth" )
    call check( nf90_get_var(ncid, id_of_var(26), deltr), "nf90_get_var deltr" )
    call check( nf90_get_var(ncid, id_of_var(27), inv_log_co), "nf90_get_var inv_log_co" )
    call check( nf90_get_var(ncid, id_of_var(28), df), "nf90_get_var df" )
    call check( nf90_get_var(ncid, id_of_var(29), df_fr), "nf90_get_var df_fr" )
    call check( nf90_get_var(ncid, id_of_var(30), df_fr2), "nf90_get_var df_fr2" )

    write( stdout, *) "After reading: c = ", c, ", th = ", th
    write( stdout, *) "After reading: costh = ", costh, ", sinth = ", sinth
    write( stdout, *) "After reading: delth = ", delth, ", deltr = ", deltr
    write( stdout, *) "After reading: inv_log_co = ", inv_log_co, ", df = ", df
    write( stdout, *) "After reading: df_fr = ", df_fr, ", df_fr2 = ", df_fr2

    ! "dfim_ofr", "dfim_fr", "dfim_fr2", "fr5", "frm5", "rhowg_dfim", "fmin", "mo_tail", "mm1_tail", "mp1_tail", &

!    call check( nf90_get_var(ncid, id_of_var(31), dfim_ofr), "nf90_get_var dfim_ofr" )
    call check( nf90_get_var(ncid, id_of_var(32), dfim_fr), "nf90_get_var dfim_fr" )
    call check( nf90_get_var(ncid, id_of_var(33), dfim_fr2), "nf90_get_var dfim_fr2" )
    call check( nf90_get_var(ncid, id_of_var(34), fr5), "nf90_get_var fr5" )
    call check( nf90_get_var(ncid, id_of_var(35), frm5), "nf90_get_var frm5" )
    call check( nf90_get_var(ncid, id_of_var(36), rhowg_dfim), "nf90_get_var rhowg_dfim" )
    call check( nf90_get_var(ncid, id_of_var(37), fmin), "nf90_get_var fmin" )
    call check( nf90_get_var(ncid, id_of_var(38), mo_tail), "nf90_get_var mo_tail" )
    call check( nf90_get_var(ncid, id_of_var(39), mm1_tail), "nf90_get_var mm1_tail" )
    call check( nf90_get_var(ncid, id_of_var(40), mp1_tail), "nf90_get_var mp1_tail" )

 !   write( stdout, *) "After reading: dfim_ofr = ", dfim_ofr, ", dfim_fr = ", dfim_fr
    write( stdout, *) "After reading: dfim_fr2 = ", dfim_fr2, ", fr5 = ", fr5
    write( stdout, *) "After reading: frm5 = ", frm5, ", rhowg_dfim = ", rhowg_dfim
    write( stdout, *) "After reading: fmin = ", fmin, ", mo_tail = ", mo_tail
    write( stdout, *) "After reading: mm1_tail = ", mm1_tail, ", mp1_tail = ", mp1_tail

!  "mp2_tail", "mpm", "kpm", "jxo", "jyo", "nbounf", "nbinp", "c_name", "blngf", "blatf", &
   
    call check( nf90_get_var(ncid, id_of_var(41), mp2_tail), "nf90_get_var mp2_tail" )
    call check( nf90_get_var(ncid, id_of_var(42), mpm), "nf90_get_var mpm" )
    call check( nf90_get_var(ncid, id_of_var(43), kpm), "nf90_get_var kpm" )
    call check( nf90_get_var(ncid, id_of_var(44), jxo), "nf90_get_var jxo" )
    call check( nf90_get_var(ncid, id_of_var(45), jyo), "nf90_get_var jyo" )
    call check( nf90_get_var(ncid, id_of_var(46), nbounf), "nf90_get_var nbounf" )
    call check( nf90_get_var(ncid, id_of_var(47), nbinp), "nf90_get_var nbinp" )
    call check( nf90_get_var(ncid, id_of_var(48), c_name), "nf90_get_var c_name" )
    call check( nf90_get_var(ncid, id_of_var(49), blngf), "nf90_get_var blngf" )
    call check( nf90_get_var(ncid, id_of_var(50), blatf), "nf90_get_var blatf" )

    write( stdout, *) "After reading: mp2_tail = ", mp2_tail, ", mpm = ", mpm
    write( stdout, *) "After reading: kpm = ", kpm, ", jxo = ", jxo
    write( stdout, *) "After reading: jyo = ", jyo, ", nbounf = ", nbounf
    write( stdout, *) "After reading: nbinp = ", nbinp, ", c_name = ", c_name
    write( stdout, *) "After reading: blngf = ", blngf, ", blatf = ", blatf

    
    ! "ijarf", "ibfl", "ibfr", "bfw", "nx", "ny", "nsea", "iper", "one_point", "reduced_grid", &
      
    call check( nf90_get_var(ncid, id_of_var(51), ijarf), "nf90_get_var ijarf" )
    call check( nf90_get_var(ncid, id_of_var(52), ibfl), "nf90_get_var ibfl" )
    call check( nf90_get_var(ncid, id_of_var(53), ibfr), "nf90_get_var ibfr" )
    call check( nf90_get_var(ncid, id_of_var(54), bfw), "nf90_get_var bfw" )
    call check( nf90_get_var(ncid, id_of_var(55), nx), "nf90_get_var nx" )
    call check( nf90_get_var(ncid, id_of_var(56), ny), "nf90_get_var ny" )
    call check( nf90_get_var(ncid, id_of_var(57), nsea), "nf90_get_var nsea" )
!    call check( nf90_get_var(ncid, id_of_var(58), iper), "nf90_get_var iper" )
!    call check( nf90_get_var(ncid, id_of_var(59), one_point), "nf90_get_var one_point" )
!    call check( nf90_get_var(ncid, id_of_var(60), reduced_grid), "nf90_get_var reduced_grid" )

    write( stdout, *) "After reading: ijarf = ", ijarf, ", ibfl = ", ibfl
    write( stdout, *) "After reading: ibfr = ", ibfr, ", bfw = ", bfw
    write( stdout, *) "After reading: nx = ", nx, ", ny = ", ny
!    write( stdout, *) "After reading: nsea = ", nsea, ", iper = ", iper
!    write( stdout, *) "After reading: one_point = ", one_point, ", reduced_grid = ", reduced_grid

    ! "l_obstruction", "nlon_rg", "delphi", "dellam", "sinph", "cosph", "amowep", "amosop", "amoeap", "amonop", &
!    call check( nf90_get_var(ncid, id_of_var(61), l_obstruction), "nf90_get_var l_obstruction" )
    call check( nf90_get_var(ncid, id_of_var(62), nlon_rg), "nf90_get_var nlon_rg" )
    call check( nf90_get_var(ncid, id_of_var(63), delphi), "nf90_get_var delphi" )
    call check( nf90_get_var(ncid, id_of_var(64), dellam), "nf90_get_var dellam" )
    call check( nf90_get_var(ncid, id_of_var(65), sinph), "nf90_get_var sinph" )
    call check( nf90_get_var(ncid, id_of_var(66), cosph), "nf90_get_var cosph" )
    call check( nf90_get_var(ncid, id_of_var(67), amowep), "nf90_get_var amowep" )
    call check( nf90_get_var(ncid, id_of_var(68), amosop), "nf90_get_var amosop" )
    call check( nf90_get_var(ncid, id_of_var(69), amoeap), "nf90_get_var amoeap" )
    call check( nf90_get_var(ncid, id_of_var(70), amonop), "nf90_get_var amonop" )

 !   write( stdout, *) "After reading: l_obstruction = ", l_obstruction, ", nlon_rg = ", nlon_rg
    write( stdout, *) "After reading: delphi = ", delphi, ", dellam = ", dellam
    write( stdout, *) "After reading: sinph = ", sinph, ", cosph = ", cosph
    write( stdout, *) "After reading: amowep = ", amowep, ", amosop = ", amosop
    write( stdout, *) "After reading: amoeap = ", amoeap, ", amonop = ", amonop

    ! "zdello", "ixlg", "kxlt", "l_s_mask", "klat", "klon", "wlat", "depth_b", "obslat", "obslon", &
    call check( nf90_get_var(ncid, id_of_var(71), zdello), "nf90_get_var zdello" )
    call check( nf90_get_var(ncid, id_of_var(72), ixlg), "nf90_get_var ixlg" )
    call check( nf90_get_var(ncid, id_of_var(73), kxlt), "nf90_get_var kxlt" )
!    call check( nf90_get_var(ncid, id_of_var(74), l_s_mask), "nf90_get_var l_s_mask" )
    call check( nf90_get_var(ncid, id_of_var(75), klat), "nf90_get_var klat" )
    call check( nf90_get_var(ncid, id_of_var(76), klon), "nf90_get_var klon" )
    call check( nf90_get_var(ncid, id_of_var(77), wlat), "nf90_get_var wlat" )
    call check( nf90_get_var(ncid, id_of_var(78), depth_b), "nf90_get_var depth_b" )

    if( L_OBSTRUCTION_T .eqv. .TRUE.) then
       call check( nf90_get_var(ncid, id_of_var(79), obslat), "nf90_get_var obslat" )
       call check( nf90_get_var(ncid, id_of_var(80), obslon), "nf90_get_var obslon" )
    end if
 
    write( stdout, *) "After reading: zdello = ", zdello, ", ixlg = ", ixlg
!    write( stdout, *) "After reading: kxlt = ", kxlt, ", l_s_mask = ", l_s_mask
    write( stdout, *) "After reading: klat = ", klat, ", klon = ", klon
    write( stdout, *) "After reading: wlat = ", wlat, ", depth_b = ", depth_b
    write( stdout, *) "After reading: obslat = ", obslat, ", obslon = ", obslon

    !  "ndepth", "deptha", "depthb", "depthe", "flminfr", "tcgond", "tfak", "tsihkd", "tfac_st", "t_tail", &
    !     "delu"
    call check( nf90_get_var(ncid, id_of_var(81), ndepth), "nf90_get_var ndepth" )
    call check( nf90_get_var(ncid, id_of_var(82), deptha), "nf90_get_var deptha" )
!    call check( nf90_get_var(ncid, id_of_var(83), depthb), "nf90_get_var depthb" )
    call check( nf90_get_var(ncid, id_of_var(84), depthe), "nf90_get_var depthe" )
    call check( nf90_get_var(ncid, id_of_var(85), flminfr), "nf90_get_var flminfr" )
    call check( nf90_get_var(ncid, id_of_var(86), tcgond), "nf90_get_var tcgond" )
    call check( nf90_get_var(ncid, id_of_var(87), tfak), "nf90_get_var tfak" )
    call check( nf90_get_var(ncid, id_of_var(88), tsihkd), "nf90_get_var tsihkd" )
    call check( nf90_get_var(ncid, id_of_var(89), tfac_st), "nf90_get_var tfac_st" )
    call check( nf90_get_var(ncid, id_of_var(90), t_tail), "nf90_get_var t_tail" )
    call check( nf90_get_var(ncid, id_of_var(91), delu), "nf90_get_var delu" )

    write( stdout, *) "After reading: ndepth = ", ndepth, ", deptha = ", deptha
    write( stdout, *) "After reading: depth_b = ", depth_b, ", depthe = ", depthe
    write( stdout, *) "After reading: flminfr = ", flminfr, ", tcgond = ", tcgond
    write( stdout, *) "After reading: tfak = ", tfak, ", tsihkd = ", tsihkd
    write( stdout, *) "After reading: tfac_st = ", tfac_st, ", t_tail = ", t_tail
    write( stdout, *) "After reading: delu = ", delu

    
    
    write( *, * ) "Reading of nr 2 ended ..."

    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. WRITE FINE GRID BOUNDARY INPUT INFORMATION. (defintion part)          !
    !        -------------------------------------------                           !

    write( *, * ) "Reading of nr 2 ended ..."
    
    
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !                4. WRITE FREQUENCY DIRECTION GRID. (definition part)          !
    ! ---------------------------------------------------------------------------- !


    write( *, * ) "Reading of nr 4 ended ..."

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     5. WRITE GRID INFORMATION. (definition part)                             !
    !        -----------------------                                               !


    write( *, * ) "Reading of nr 2 ended ..."


    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     6. WRITE TABLES. (definition part)                                       !
    !        -------------                                                         !

    
    write( *, * ) "Definition of nr 5 ended ..."


    !
    ! End of reading to netCDF file
    !

    call check( nf90_close(ncid), "NF90_CLOSE" )

  end SUBROUTINE READ_PREPROC_FILE_NETCDF


 

  
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !                                                                              !
  !     G. PRIVAT MODULE PROCEDURES.                                             !
  !                                                                              !
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE ADJUST_DEPTH

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !   ADJUST_DEPTH - ADJUST DEPTH IN AREAS.                                      !
    !                                                                              !
    !     S. HASSELMANN     MPIFM           1/6/86.                                !
    !                                                                              !
    !     MODIFIED BY       H. GUNTHER      1/4/90  -  REARANGEMENT OF CODE.       !
    !                                                                              !
    !     PURPOSE.                                                                 !
    !     --------                                                                 !
    !                                                                              !
    !       TO CHANGE THE MODEL DEPTH IN USER SPECIFIED AREAS.                     !
    !                                                                              !
    !     METHOD.                                                                  !
    !     -------                                                                  !
    !                                                                              !
    !       NONE.                                                                  !
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

    INTEGER :: I, K, L
    INTEGER :: XLAT, XLON

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. MANUAL ADJUSTMENT OF TOPOGRAPHY.                                      !
    !        --------------------------------                                      !

    IF (N_CA.GT.0) THEN
       DO K = 1,NY
          XLAT = AMOSOP+(K-1)*XDELLA
          DO I = 1,NLON_RG(K)
             XLON = AMOWEP+(I-1)*ZDELLO(K)
             IF (XLON.GE.M_S_PER) XLON = XLON-M_S_PER
             DO L = 1,N_CA
                IF (XLON.LT.WEST_CA(L)) XLON = XLON+M_S_PER
                IF (XLON.GT.EAST_CA(L)) XLON = XLON-M_S_PER
                IF (XLON.GE.WEST_CA(L) .AND. XLAT.GE.SOUTH_CA(L) .AND.             &
                     &               XLON.LE.EAST_CA(L) .AND. XLAT.LE.NORTH_CA(L))                  &
                     &                       GRD(I,K) = DEPTH_CA(L)
             END DO
          END DO
       END DO
    END IF

  END SUBROUTINE ADJUST_DEPTH

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE CLOSEST_GP_LAT (I, K, IP, XH, KH, IP1, IP2, WH)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !  CLOSEST_GP_LAT - ROUTINE TO FIND THE CLOSEST AND SECOND CLOSEST GRID POINT  !
    !                   ON LATITUDE NORTH OR SOUTH AND THE INTEPOLATION WEIGHT.    !
    !                                                                              !
    !     H.GUNTHER            ECMWF       04/04/1990                              !
    !     J. BIDLOT            ECMWF       APRIL 2000: add second closest          !
    !                                                  grid points.                !
    !     J. BIDLOT            ECMWF       CLOSEST AND SECOND CLOSEST GRID         !
    !                                      POINT FOR THE ROTATED CELL.             !
    !     H.GUNTHER            HZG         CODE TAKEN FROM SUB. UBUF               !
    !                                                                              !
    !     PURPOSE.                                                                 !
    !     -------                                                                  !
    !                                                                              !
    !       TO ARRANGE NEIGHBOUR GRID POINT INDICES FOR A GIVEN SEA POINT ON       !
    !       LATITUDE NORTH OR SOUTH AND THE INTEPOLATION WEIGHT.                   !
    !                                                                              !
    !     METHOD.                                                                  !
    !     -------                                                                  !
    !                                                                              !
    !       THE INDICES OF THE NEXT POINTS ON LAT. AND LONG. ARE                   !
    !       COMPUTED. ZERO INDICATES A LAND POINT IS NEIGHBOUR.                    !
    !                                                                              !
    !     REFERENCE.                                                               !
    !     ----------                                                               !
    !                                                                              !
    !       NONE.                                                                  !
    !                                                                              !
    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     INTERFACE VARIABLES.                                                     !
    !     --------------------                                                     !

    INTEGER, INTENT(IN)  :: I      !! LONGITUDE INDEX OF GRID POINT.
    INTEGER, INTENT(IN)  :: K      !! LATITUTE INDEX OF GRID POINT.
    INTEGER, INTENT(IN)  :: IP     !! SEA POINT NUMBER OF GRID POINT.
    REAL,    INTENT(IN)  :: XH     !! LONGITUDE OF GRID POINT.
    INTEGER, INTENT(IN)  :: KH     !! LATITUDE INDEX NORTH (K+1) OR SOUTH (K-1).
    INTEGER, INTENT(OUT) :: IP1    !! NUMBER OF CLOSEST SEA POINT TO IP
    !! ON LATITUDE KH.
    INTEGER, INTENT(OUT) :: IP2    !! NUMBER OF SECOND CLOSEST SEA POINT TO IP
    !! ON LATITUDE KH.
    REAL,    INTENT(OUT) :: WH     !! INTERPOLATION WEIGHT FOR CLOSEST POINT.

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     LOCAL VARIABLES.                                                         !
    !     ----------------                                                         !

    INTEGER  :: IC, ICA, ICE, ICS, IPH, IPH1, IPH2
    REAL     :: D3, D5, XP, D4, D6, Z

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. INITIAL.                                                              !
    !        --------                                                              !

    WH  = 1.               !! INTERPOLATION WEIGHT
    IP1 = 0                !! NUMBER OF CLOSEST SEA POINT
    IP2 = 0                !! NUMBER OF SECOND CLOSEST SEA POINT
    IF (KH.LT.K) THEN      !! SOUTH LATITUDE
       IF (KH.LT.1) RETURN
       ICA = IP            !! SEA POINT COUNTER START
       ICE = 1             !! SEA POINT COUNTER END
       ICS = -1            !! SEA POINT COUNTER STEP
    ELSE                   !! NORTH LATITUDE
       IF (KH.GT.NY) RETURN
       ICA = IP            !! SEA POINT COUNTER START
       ICE = NSEA          !! SEA POINT COUNTER END
       ICS = +1            !! SEA POINT COUNTER STEP
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     2. CLOSEST AND SECOND CLOSEST GRID POINT.                                !
    !        --------------------------------------                                !

    IF (NLON_RG(KH).EQ.NLON_RG(K)) THEN
       IPH1 = I
       IPH2 = I
    ELSE
       IPH1 = FLOOR(XH)
       IPH2 = CEILING(XH)
       IF (IPH1.EQ.IPH2) THEN
          IPH1 = IPH1+1
          IPH2 = IPH1
       ELSE
          IF (IPH2.EQ.NINT(XH)) THEN
             IPH = IPH1
             IPH1 = IPH2+1
             IPH2 = IPH+1
          ELSE
             IPH1 = IPH1+1
             IPH2 = IPH2+1
          END IF
       END IF
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     3. INTERPOLATION WEIGHT FOR CLOSEST POINT.                               !
    !        ---------------------------------------                               !

    IF (IPH1.NE.IPH2) THEN
       Z = REAL(ZDELLO(KH))/REAL(ZDELLO(K))
       D3 = XH*Z - 0.5
       D5 = XH*Z + 0.5
       XP = REAL(IPH1-1)*Z
       D4 = XP-0.5*Z
       D6 = XP+0.5*Z
       IF (D4.LT.D3 .OR. D6.GT.D5) THEN
          WH = MIN(1.,(MIN(D5,D6)-MAX(D3, D4)))
       END IF
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     4. SEAPOINT NUMMER OF FOR CLOSEST POINT.                                 !
    !        -------------------------------------                                 !

    IF (IPER) THEN
       IF (IPH1.LT.1) THEN
          IPH1 = IPH1 + NLON_RG(KH)
       END IF
       IF (IPH1.GT.NLON_RG(KH)) THEN
          IPH1 = IPH1 - NLON_RG(KH)
       END IF
    END IF
    IF (IPH1.GE.1.AND.IPH1.LE.NLON_RG(KH)) THEN
       IF (L_S_MASK(IPH1,KH)) THEN
          DO IC = ICA,ICE,ICS
             IF (IXLG(IC).EQ.IPH1 .AND.KXLT(IC).EQ.KH) EXIT
          END DO
          IP1 = IC
       END IF
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     5. SEAPOINT NUMMER OF FOR SECOND CLOSEST POINT.                          !
    !        --------------------------------------------                          !

    IF (IPH1.NE.IPH2) THEN
       IF (IPER) THEN
          IF (IPH2.LT.1) THEN
             IPH2 = IPH2 + NLON_RG(KH)
          END IF
          IF (IPH2.GT.NLON_RG(KH)) THEN
             IPH2 = IPH2 - NLON_RG(KH)
          END IF
       END IF
       IF (IPH2.GE.1.AND.IPH2.LE.NLON_RG(KH)) THEN
          IF (L_S_MASK(IPH2,KH)) THEN
             DO IC = ICA,ICE,ICS
                IF (IXLG(IC).EQ.IPH2 .AND.KXLT(IC).EQ.KH) EXIT
             END DO
             IP2 = IC
          END IF
       END IF
    ELSE
       IP2 = IP1
    END IF

  END SUBROUTINE CLOSEST_GP_LAT

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !


  SUBROUTINE COUNT_POINTS_E_W (NX_TL, NX_TR, NY_TB, NY_TT,                       &
       &                        IBLOCKDPT, ITHRSHOLD, IEXCLTHRSHOLD, GRD,             &
       &                        PERCENTSHALLOW, PSHALLOWTRHS, PERCENTLAND, PLANDTRHS, &
       &                        NOBSTRCT, NTOTPTS)
    INTEGER, INTENT(IN) :: NX_TL, NX_TR, NY_TB, NY_TT
    INTEGER, INTENT(IN) :: IBLOCKDPT, ITHRSHOLD, IEXCLTHRSHOLD
    REAL,    INTENT(IN) ::  GRD
    REAL,    INTENT(IN) ::  PERCENTSHALLOW, PSHALLOWTRHS, PERCENTLAND, PLANDTRHS

    INTEGER, INTENT(OUT) :: NOBSTRCT, NTOTPTS

    INTEGER :: II, I, J, NIOBSLON, NX_TH, IREINF, NBLOCKLAND

    LOGICAL :: LLAND, LREALLAND, LNSW, L1ST

    IREINF= 1
    NBLOCKLAND = 0
    NOBSTRCT = 0
    NX_TH = NX_TR
    IF (NX_TR.LT.NX_TL) NX_TH = NX_TR + NX_T-1
    NTOTPTS = (NY_TT-NY_TB+1)*(NX_TH-NX_TL+1)

    DO J = NY_TB, NY_TT
       NIOBSLON=0
       LLAND=.FALSE.
       LREALLAND=.FALSE.

       DO II = NX_TL, NX_TH
          I = II
          IF (II.GT.NX_T) I = II-NX_T+1
          IF (GRID_IN(I,J).LE.IBLOCKDPT ) THEN
             IF (GRID_IN(I,J).LE.0 ) LREALLAND = .TRUE.
             LLAND =.TRUE.
             NIOBSLON = NIOBSLON+1

             !         IF LAND THEN THE FULL LONGITUDE IS BLOCKED
             !         IF THERE IS A SWITCH BACK TO SEA OR VICE VERSA (SEE BELOW)
             !         LAND IS DEFINED AS ANYTHING ABOVE IBLOCKDPT(IX,K)
             !         -----------------------------------------------------------

          ELSEIF (GRID_IN(I,J) .LE. ITHRSHOLD .AND.  &
               &                GRD.GT.IEXCLTHRSHOLD) THEN

             !         IF SEA ABOVE THE THRESHOLD THEN ONLY THAT GRID POINTS BLOCKS
             !         ------------------------------------------------------------
             NIOBSLON = NIOBSLON + 1
          ENDIF
       ENDDO

       IF (LLAND) THEN
          IF (LREALLAND) THEN
             LNSW=.TRUE.
             IF (GRID_IN(NX_TL,J).LE.IBLOCKDPT) THEN
                L1ST=.TRUE.
             ELSE
                L1ST=.FALSE.
             ENDIF
             DO II = NX_TL+1,NX_TH
                I = II
                IF (II.GT.NX_T) I = II-NX_T+1
                IF ( ((GRID_IN(I,J).LE.IBLOCKDPT) .NEQV.L1ST)  &
                     &                                      .AND. LNSW ) THEN
                   LNSW=.FALSE.
                ENDIF
                IF (((GRID_IN(I,J).LE.IBLOCKDPT).EQV.L1ST)  &
                     &                                    .AND. .NOT. LNSW ) THEN

                   !                           LAND IS BLOCKING
                   NIOBSLON=IREINF*(NX_TH-NX_TL+1)
                   NBLOCKLAND=NBLOCKLAND+1
                   EXIT
                ENDIF
             ENDDO
             IF (LNSW) NIOBSLON = NX_TH-NX_TL+1
          ELSE
             IF (PERCENTSHALLOW.GT.PSHALLOWTRHS) THEN
                !                         mostly shallow, do not enhance obstruction
                NIOBSLON = NX_TH-NX_TL+1
             ELSEIF (PERCENTLAND.LT.PLANDTRHS) THEN
                !                         does not contain too much land
                NIOBSLON = IREINF*(NX_TH-NX_TL+1)
                NBLOCKLAND = NBLOCKLAND+1
             ELSE
                NIOBSLON = 0
             ENDIF
          ENDIF     !! LREALLAND
       ENDIF     !! LLAND

       NOBSTRCT = NOBSTRCT + NIOBSLON
    ENDDO

  END SUBROUTINE COUNT_POINTS_E_W

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE COUNT_POINTS_N_S (NX_TL, NX_TR, NY_TB, NY_TT,                       &
       &                        IBLOCKDPT, ITHRSHOLD, IEXCLTHRSHOLD, GRD,        &
       &                        PERCENTSHALLOW, PSHALLOWTRHS, PERCENTLAND, PLANDTRHS, &
       &                        NOBSTRCT, NTOTPTS)
    INTEGER, INTENT(IN)  :: NX_TL, NX_TR, NY_TB, NY_TT
    INTEGER, INTENT(IN)  :: IBLOCKDPT, ITHRSHOLD, IEXCLTHRSHOLD
    REAL,    INTENT(IN) ::  GRD
    REAL,    INTENT(IN) ::  PERCENTSHALLOW, PSHALLOWTRHS, PERCENTLAND, PLANDTRHS

    INTEGER, INTENT(OUT) :: NOBSTRCT, NTOTPTS

    INTEGER :: II, I, J, NIOBSLON, NX_TH, IREINF, NBLOCKLAND

    LOGICAL :: LLAND, LREALLAND, LNSW, L1ST

    IREINF= 1
    NBLOCKLAND = 0
    NOBSTRCT = 0
    NX_TH = NX_TR
    IF (NX_TR.LT.NX_TL) NX_TH = NX_TR + NX_T-1
    NTOTPTS = (NY_TT-NY_TB+1)*(NX_TH-NX_TL+1)

    DO II = NX_TL, NX_TH
       I = II
       IF (II.GT.NX_T) I = II-NX_T+1
       NIOBSLON=0
       LLAND=.FALSE.
       LREALLAND=.FALSE.

       DO J = NY_TB, NY_TT
          IF (GRID_IN(I,J).LE.IBLOCKDPT ) THEN
             IF (GRID_IN(I,J).LE.0 ) LREALLAND = .TRUE.
             LLAND =.TRUE.
             NIOBSLON = NIOBSLON+1

             !         IF LAND THEN THE FULL LONGITUDE IS BLOCKED
             !         IF THERE IS A SWITCH BACK TO SEA OR VICE VERSA (SEE BELOW)
             !         LAND IS DEFINED AS ANYTHING ABOVE IBLOCKDPT(IX,K)
             !         -----------------------------------------------------------

          ELSEIF (GRID_IN(I,J) .LE. ITHRSHOLD .AND.  &
               &                GRD.GT.IEXCLTHRSHOLD) THEN

             !         IF SEA ABOVE THE THRESHOLD THEN ONLY THAT GRID POINTS BLOCKS
             !         ------------------------------------------------------------

             NIOBSLON = NIOBSLON + 1
          ENDIF
       ENDDO

       IF (LLAND) THEN
          IF (LREALLAND) THEN
             LNSW=.TRUE.
             IF (GRID_IN(I,NY_TT).LE.IBLOCKDPT) THEN
                L1ST=.TRUE.
             ELSE
                L1ST=.FALSE.
             ENDIF
             DO J = NY_TT-1,NY_TB,-1
                IF ( ((GRID_IN(I,J).LE.IBLOCKDPT) .NEQV.L1ST)  &
                     &                                      .AND. LNSW ) THEN
                   LNSW=.FALSE.
                ENDIF
                IF (((GRID_IN(I,J).LE.IBLOCKDPT).EQV.L1ST)  &
                     &                                    .AND. .NOT. LNSW ) THEN

                   !                           LAND IS BLOCKING
                   NIOBSLON=IREINF*(NY_TT-NY_TB+1)
                   NBLOCKLAND=NBLOCKLAND+1
                   EXIT
                ENDIF
             ENDDO
             IF (LNSW) NIOBSLON = NY_TT-NY_TB+1
          ELSE
             IF (PERCENTSHALLOW.GT.PSHALLOWTRHS) THEN
                !                         mostly shallow, do not enhance obstruction
                NIOBSLON=NY_TT-NY_TB+1
             ELSEIF (PERCENTLAND.LT.PLANDTRHS) THEN
                !                         does not contain too much land
                NIOBSLON = IREINF*(NY_TT-NY_TB+1)
                NBLOCKLAND = NBLOCKLAND+1
             ELSE
                NIOBSLON = 0
             ENDIF
          ENDIF     !! LREALLAND
       ENDIF     !! LLAND

       NOBSTRCT = NOBSTRCT + NIOBSLON
    ENDDO

  END SUBROUTINE COUNT_POINTS_N_S

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE CREATE_OBSTRUCTIONS

    ! ---------------------------------------------------------------------------- !
    !

    INTEGER :: M, IS, K, IX
    REAL    :: OMEGA, XKDMAX, DEPTH, XX
    INTEGER :: KT, KB, NY_TB, NY_TT, NX_TL, NX_TR, IREINF
    INTEGER :: XLATT, XLATB, XLONL, XLONR
    INTEGER :: STEPT, STEPB, STEP_LON, STEP_LAT
    INTEGER :: NOBSTRCT, NTOTPTS

    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ITHRSHOLD     !! Threshold depth for blocking
    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: IEXCLTHRSHOLD !! exceeds threshold (no blocking)
    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: IBLOCKDPT     !! Blocking depth
    REAL, ALLOCATABLE, DIMENSION(:,:) :: HOBSLAT
    REAL, ALLOCATABLE, DIMENSION(:,:) :: HOBSLON

    REAL :: PLANDTRHS
    REAL :: PSHALLOWTRHS
    REAL (KIND=KIND_D) :: X_HELP

    ! ---------------------------------------------------------------------------- !
    !

    X_HELP = 2*DY_T
    IF (XDELLA .LE. X_HELP ) THEN !! COMPUTE OBSTRUCTIONS ONLY WHEN IT IS MEANINGFUL
       WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
       WRITE (IU06,*) ' +                                                  +'
       WRITE (IU06,*) ' +     WARNING ERROR SUB. CREATE_OBSTRUCTIONS.      +'
       WRITE (IU06,*) ' +     =======================================      +'
       WRITE (IU06,*) ' +                                                  +'
       WRITE (IU06,*) ' + THE REQUESTED RESOLUTION IS SMALL ENOUGH WITH    +'
       WRITE (IU06,*) ' + RESPECT TO THE INPUT BATHYMETRY DATA.            +'
       WRITE (IU06,*) ' + NO OBSTRUCTIONS WILL BE COMPUTED.                +'
       WRITE (IU06,*) ' +                                                  +'
       WRITE (IU06,*) ' +               MODEL CONTINUES                    +'
       WRITE (IU06,*) ' +                                                  +'
       WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
       L_OBSTRUCTION_T = .FALSE.
       RETURN
    END IF

    PLANDTRHS = 0.3
    PSHALLOWTRHS = 0.8

    !     IREINF IS USED TO REINFORCE LAND OBSTRUCTIONS FOR SMALL GRID SPACING.

    X_HELP = 0.5
    IF (XDELLA.LE.DEG_TO_M_SEC(X_HELP)) THEN
       IREINF = 2
    ELSE
       IREINF = 1
    ENDIF

    WRITE (*,*) ' IREINF', IREINF

    ALLOCATE(HOBSLAT(NX,NY))
    ALLOCATE(HOBSLON(NX,NY))

    HOBSLAT(:,:) = 1.
    HOBSLON(:,:) = 1.

    ALLOCATE(ITHRSHOLD(NX,NY))
    ALLOCATE(IBLOCKDPT(NX,NY))
    ALLOCATE(IEXCLTHRSHOLD(NX,NY))

    FREQ: DO M = 1,ML

       ! ---------------------------------------------------------------------------- !
       !
       !       COMPUTE THE THRESHOLD AT WHICH THE WAVES ARE OBSTRUCTED BY THE BOTTOM,
       !       EXCEPT IF WAM DEPTH OF THE SAME ORDER OF MAGITUDE.
       !       ALSO COMPUTE THE DEPTH THAT IS CONSIDERED TO BE FULLY BLOCKING
       !       AS IF IT WAS LAND.

       OMEGA  = ZPI*FR(M)
       XKDMAX = 2.
       DO K=1,NY
          DO IX=1,NLON_RG(K)
             IF (GRD(IX,K).GT.0.) THEN
                DEPTH = GRD(IX,K)
                XX = XKDMAX/AKI(OMEGA,DEPTH)
                ITHRSHOLD(IX,K) = NINT(XX)
                IEXCLTHRSHOLD(IX,K) = MIN(10*ITHRSHOLD(IX,K),998)
                IBLOCKDPT(IX,K) = INT(0.05*XX)
             END IF
          END DO
       END DO

       ! ---------------------------------------------------------------------------- !
       !
       !       NORTH-SOUTH OBSTRUCTIONS
       !       -----------------------
       !       IS=1 is for the south-north advection
       !       IS=2 is for the north-south advection

       DO IS = 1,2
          DO K = 1,NY
             IF (IS.EQ.1) THEN
                KT = K
                KB = K-1
                STEPT = -DY_T
                STEPB = 0
                STEP_LON = ZDELLO(K)/2
             ELSE
                KT = K+1
                KB = K
                STEPT = 0
                STEPB = DY_T
                STEP_LON = ZDELLO(K)/2
             END IF
             XLATT = XLAT(KT) + STEPT
             XLATB = XLAT(KB) + STEPB
             NY_TT = NINT(REAL(XLATT-SOUTH_T)/REAL(DY_T)) + 1
             NY_TT = MAX(1,MIN(NY_TT,NY_T))
             NY_TB = NINT(REAL(XLATB-SOUTH_T)/REAL(DY_T)) + 1
             NY_TB = MAX(1,MIN(NY_TB,NY_T))

             DO IX = 1,NLON_RG(K)
                IF (GRD(IX,K).LE.0) CYCLE
                XLONL = AMOWEP + (IX-1)*ZDELLO(K) - STEP_LON
                IF (XLONL.GT.EAST_T) XLONL = XLONL - M_S_PER
                XLONR = AMOWEP + (IX-1)*ZDELLO(K) + STEP_LON
                IF (XLONR.GT.EAST_T) XLONR = XLONR - M_S_PER
                NX_TL = NINT(REAL(XLONL - WEST_T)/REAL(DX_T)) + 1
                NX_TR = NINT(REAL(XLONR - WEST_T)/REAL(DX_T)) + 1
                CALL COUNT_POINTS_N_S (NX_TL, NX_TR, NY_TB, NY_TT,                 &
                     &                                  IBLOCKDPT(IX,K), ITHRSHOLD(IX,K),           &
                     &                                  IEXCLTHRSHOLD(IX,K), GRD(IX,K),             &
                     &                                  PERCENTSHALLOW(IX,K), PSHALLOWTRHS,         &
                     &                                  PERCENTLAND(IX,K), PLANDTRHS,               &
                     &                                  NOBSTRCT, NTOTPTS)

                HOBSLAT(IX,K) = (1.-FLOAT(NOBSTRCT)/NTOTPTS)

             ENDDO           !! LONGITUDES
          ENDDO              !! LATTITUDES
          OBSLAT (1:NSEA,IS,M) = PACK (HOBSLAT, L_S_MASK)
       ENDDO                 !! NORTH-SOUTH OBSTRUCTIONS


       ! ---------------------------------------------------------------------------- !
       !
       !       EAST-WEST OBSTRUCTIONS
       !       -----------------------
       !       IS=1 is for the west-east advection
       !       IS=2 is for the east-west advection

       DO IS=1,2
          DO K=1,NY
             STEP_LAT = NINT(REAL(XDELLA)/2.)
             XLATT = XLAT(K) + STEP_LAT
             XLATB = XLAT(K) - STEP_LAT
             NY_TT = NINT(REAL(XLATT-SOUTH_T)/REAL(DY_T)) + 1
             NY_TT = MAX(1,MIN(NY_TT,NY_T))
             NY_TB = NINT(REAL(XLATB-SOUTH_T)/REAL(DY_T)) + 1
             NY_TB = MAX(1,MIN(NY_TB,NY_T))

             DO IX=1,NLON_RG(K)
                IF (GRD(IX,K).LE.0) CYCLE
                IF(IS.EQ.1) THEN
                   XLONL = AMOWEP + (IX-2)*ZDELLO(K)
                   XLONR = AMOWEP + (IX-1)*ZDELLO(K) - DX_T
                ELSE
                   XLONL = AMOWEP + (IX-1)*ZDELLO(K) + DX_T
                   XLONR = AMOWEP + IX*ZDELLO(K)
                ENDIF
                IF (XLONL.GT.EAST_T) XLONL = XLONL - M_S_PER
                IF (XLONR.GT.EAST_T) XLONR = XLONR - M_S_PER

                NX_TL = NINT(REAL(XLONL - WEST_T)/REAL(DX_T)) + 1
                NX_TR = NINT(REAL(XLONR - WEST_T)/REAL(DX_T)) + 1

                CALL COUNT_POINTS_E_W (NX_TL, NX_TR, NY_TB, NY_TT,                 &
                     &                                  IBLOCKDPT(IX,K), ITHRSHOLD(IX,K),           &
                     &                                  IEXCLTHRSHOLD(IX,K), GRD(IX,K),        &
                     &                                  PERCENTSHALLOW(IX,K), PSHALLOWTRHS,         &
                     &                                  PERCENTLAND(IX,K), PLANDTRHS, &
                     &                                  NOBSTRCT, NTOTPTS)

                HOBSLON(IX,K)= (1.-FLOAT(NOBSTRCT)/NTOTPTS)

             ENDDO           !! LONGITUDES
          ENDDO              !! LATTITUDES
          OBSLON(1:NSEA,IS,M) = PACK (HOBSLON, L_S_MASK)
       ENDDO                 !! EAST-WEST OBSTRUCTIONS

    END DO FREQ

  END SUBROUTINE CREATE_OBSTRUCTIONS

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE MEAN_DEPTH

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !   MEAN_DEPTH - Average depth.                                                !
    !                                                                              !
    !     JEAN BIDLOT JUNE 2007                                                    !
    !                                                                              !
    !     MODIFIED BY       H. GUNTHER      1/4/2015  -  REARANGEMENT OF CODE.     !
    !                                                                              !
    !     PURPOSE.                                                                 !
    !     --------                                                                 !
    !                                                                              !
    !       TO CHANGE THE MODEL DEPTH IN USER SPECIFIED AREAS.                     !
    !                                                                              !
    !     METHOD.                                                                  !
    !     -------                                                                  !
    !                                                                              !
    !       NONE.                                                                  !
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

    INTEGER :: K, J, JJ, IX, I, II, IK

    INTEGER :: NLANDCENTREPM      !! HALF CENTER SIZE OF A GRID BOX
    INTEGER :: NLANDCENTREMAX     !! Number of points in HALF CENTER SIZE OF A GRID BOX
    INTEGER :: NLANDCENTRE        !! Number of Land points in the box centre.
    INTEGER :: NIM, NIP           !! LONGITUDE LEFT AND RIGHT AVARAGE LIMITS
    INTEGER :: NJM, NJP           !! LATITUDE BOTTOM AND TOP AVARAGE LIMITS
    INTEGER :: NSEA               !! counts sea points
    REAL    :: SEA                !! cumulates depth
    INTEGER :: NLAND              !! counts land points
    REAL    :: XLAND              !! cumulates land depth
    INTEGER :: NSEASH             !! counts shallow points (depth less than 500m)
    REAL    :: SEASH              !! cumulates depth at shallow points
    REAL    :: XLON

    ! ---------------------------------------------------------------------------- !

    ALLOCATE (PERCENTLAND(NX,NY))
    ALLOCATE (PERCENTSHALLOW(NX,NY))

    NLANDCENTREPM  = (NINT(0.2*REAL(XDELLA)/REAL(DY_T))-1)/2 !! HALF CENTER SIZE OF A GRID BOX
    NLANDCENTREPM  = MAX(NLANDCENTREPM,1)
    NLANDCENTREMAX = (2*NLANDCENTREPM+1)**2

    NJM = INT(0.5*REAL(XDELLA)/REAL(DY_T))            !! LATITUDE BOTTOM AND TOP AVARAGE LIMITS
    NJP = NJM
    ALLOCATE (ALON(NX_T))
    ALLOCATE (ALAT(NY_T))
    ALLOCATE (XLAT(0:NY+1))

    DO I = 1, NX_T
       ALON(I) = WEST_T +(I-1)*DX_T
    END DO
    DO J = 1, NY_T
       ALAT(J)  = SOUTH_T-(J-1)*DY_T
    END DO
    DO K = 0,NY+1
       XLAT(K) = AMOSOP + (K-1)*XDELLA
    END DO

    !        WE ASSUME THAT WAMGRID IS ALWAYS WITHIN ETOPO2

    DO K=1,NY

       !        DETERMINE CLOSEST ETO2 J INDEX TO WAM POINT

       DO J = NY_T-1,1,-1
          IF (ALAT(J+1).LT.XLAT(K) .AND. XLAT(K).LE.ALAT(J) ) EXIT
       ENDDO

       J = NINT(REAL(XLAT(K)-SOUTH_T)/REAL(DY_T)) + 1

       IF (J.GT.NY_T .OR. J.LT.1) THEN
          WRITE(*,*) 'PROBLEM WITH J !!!'
          CALL ABORT1
       ENDIF

       DO IX = 1,NLON_RG(K)
          XLON = AMOWEP + (IX-1)*ZDELLO(K)
          IF (XLON.GE.EAST_T) XLON = XLON - M_S_PER     ! ETOPO2 STARTS AT -180

          !          DETERMINE CLOSEST ETOPO2 I INDEX TO WAM POINT

          I = NINT(REAL(XLON-WEST_T)/REAL(DX_T)) + 1

          !          AVERAGE OVER LAND AND SEA SEPARATELY AROUND POINT I,J

          NIM = INT(0.5*REAL(ZDELLO(K))/REAL(DX_T))
          NIP = NIM

          NSEA   = 0     ! counts sea points
          SEA    = 0.    ! cumulates depth
          NLAND  = 0     ! counts land points
          XLAND  = 0.    ! cumulates land depth
          NSEASH = 0     ! counts shallow points (depth less than 500m)
          SEASH  = 0.    ! cumulates depth at shallow points

          DO JJ = J-NJM, J+NJP
             IF (JJ.GE.1 .AND. JJ.LE.NY_T) THEN
                DO II = I-NIM,I+NIP
                   IK = II
                   IF (II.LT.1)    IK = NX_T-1 + II
                   IF (II.GT.NX_T) IK = II - NX_T+1
                   IF (GRID_IN(IK,JJ).GE.LAND_LIMIT) THEN
                      NSEA = NSEA+1
                      SEA = SEA + MIN(999.,GRID_IN(IK,JJ))  ! IN WAM 999M IS THE MAXIMUM DEPTH

                      IF (GRID_IN(IK,JJ).LT.500.) THEN      ! FIND SHALLOWER AREAS
                         NSEASH = NSEASH+1
                         SEASH = SEASH + GRID_IN(IK,JJ)
                      ENDIF

                   ELSE
                      NLAND = NLAND+1
                      XLAND = XLAND+GRID_IN(IK,JJ)
                   ENDIF
                ENDDO
             ENDIF
          ENDDO

          !          SEARCH FOR LAND AT THE CENTER OF THE GRID BOX

          NLANDCENTRE = 0
          DO JJ = J-NLANDCENTREPM, J+NLANDCENTREPM
             IF (JJ.GE.1 .AND. JJ.LE.NY_T) THEN
                DO II = I-NLANDCENTREPM, I+NLANDCENTREPM
                   IK = II
                   IF (II.LT.1)    IK = NX_T-1+II
                   IF (II.GT.NX_T) IK = II-NX_T+1
                   IF (GRID_IN(IK,JJ).LT.0.) NLANDCENTRE = NLANDCENTRE+1
                ENDDO
             ENDIF
          ENDDO

          !   IF 40% OR MORE LAND, THEN AVERAGE OVER LAND POINTS OR THE CENTER OF THE GRID BOX IS LAND.
          !          ELSE AVERAGE OVER SEA POINTS

          PERCENTLAND(IX,K) = FLOAT(NLAND)/FLOAT(NLAND+NSEA)
          IF (PERCENTLAND(IX,K).GT.0.60 .OR. NLANDCENTRE.GE.NLANDCENTREMAX ) THEN
             GRD(IX,K) = XLAND/NLAND
          ELSE

             !   IF THERE IS A PERCENTAGE OF SHALLOWER POINTS THEN THE AVERAGE IS TAKEN OVER THOSE POINTS ALONE.

             PERCENTSHALLOW(IX,K) = FLOAT(NSEASH)/FLOAT(NSEA)
             IF (PERCENTSHALLOW(IX,K).GE.0.3) THEN
                GRD(IX,K) = SEASH/NSEASH
             ELSE 
                GRD(IX,K) = SEA/NSEA
             ENDIF
          ENDIF
       ENDDO
    ENDDO

    GRD = MAX(GRD,-999.)
    GRD = MIN(GRD, 999.)

  END SUBROUTINE MEAN_DEPTH

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE PREPARE_GRID

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !   PREPARE_GRID - ROUTINE TO ARRANGE WAMODEL GRID.                            !
    !                                                                              !
    !     H.GUNTHER            ECMWF       04/04/1990                              !
    !     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
    !                                                                              !
    !     PURPOSE.                                                                 !
    !     -------                                                                  !
    !                                                                              !
    !       TO ARRANGE WAMODEL GRID FOR A GIVEN AREA AND COMPUTE VARIOUS           !
    !       MODEL CONSTANTS.                                                       !
    !                                                                              !
    !     METHOD.                                                                  !
    !     -------                                                                  !
    !                                                                              !
    !       THE MODEL GRID AREA IS EXTRACTED FROM THE INPUT TOPOGRAGHY.            !
    !       THE NEAREST GRID POINT IS TAKEN FOR DEPTH INTERPOLATION.               !
    !       LAND POINTS ARE REMOVED FROM THE GRID.                                 !
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

    INTEGER :: I, K, KH, IP
    REAL    :: XLAT, XH

    character (len=len_coor) :: formtext

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. CLEAR MODEL GRID AND CHECK MODULE STATUS.                             !
    !        -----------------------------------------                             !

    NSEA = -1
    IF (ALLOCATED (SINPH))  DEALLOCATE (SINPH)
    IF (ALLOCATED (COSPH))  DEALLOCATE (COSPH)
    IF (ALLOCATED (DEPTH_B))  DEALLOCATE (DEPTH_B)
    IF (ALLOCATED (L_S_MASK))  DEALLOCATE (L_S_MASK)

    IF (ALLOCATED (IXLG))    DEALLOCATE (IXLG)
    IF (ALLOCATED (KXLT))    DEALLOCATE (KXLT)
    IF (ALLOCATED (KLAT))    DEALLOCATE (KLAT)
    IF (ALLOCATED (KLON))    DEALLOCATE (KLON)
    IF (ALLOCATED (WLAT))    DEALLOCATE (WLAT)
    IF (ALLOCATED (GRD))     DEALLOCATE (GRD)
    IF (ALLOCATED (NLON_RG)) DEALLOCATE (NLON_RG)
    IF (ALLOCATED (ZDELLO))  DEALLOCATE (ZDELLO)
    IF (ALLOCATED (DELLAM))  DEALLOCATE (DELLAM)

    IF (.NOT.SET_STATUS) THEN
       WRITE (IU06,*) ' ********************************************************'
       WRITE (IU06,*) ' *                                                      *'
       WRITE (IU06,*) ' *          FATAL  ERROR IN SUB. PREPARE_GRID           *'
       WRITE (IU06,*) ' *          =================================           *'
       IF (NX_T .EQ. -1 .OR. NY_T .EQ. -1) THEN
          WRITE (IU06,*) ' *                                                      *'
          WRITE (IU06,*) ' * TOPOGRAPHY INPUT DATA ARE NOT DEFINED IN GRID MODULE.*'
          WRITE (IU06,*) ' * DATA MUST BE DEFINED BY SUB. SET_TOPOGRAPHY.         *'
       END IF
       IF (NX .EQ. -1 .OR. NY .EQ. -1) THEN
          WRITE (IU06,*) ' *                                                      *'
          WRITE (IU06,*) ' * MODEL GRID DIMENISIONS ARE NOT DEFINED GRID MODULE.  *'
          WRITE (IU06,*) ' * DATA MUST BE DEFINED BY SUB. SET_GRID_DEF.           *'
       END IF
       WRITE (IU06,*) ' *                                                      *'
       WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS             *'
       WRITE (IU06,*) ' *                                                      *'
       WRITE (IU06,*) ' ********************************************************'
       CALL ABORT1
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !    2. INITIALISE SIN AND COS OF LATITUDES.                                   !
    !       ------------------------------------                                   !

    ALLOCATE (SINPH(1:NY))
    ALLOCATE (COSPH(1:NY))

    DO K = 1,NY
       XLAT = REAL(AMOSOP + (K-1)*XDELLA)/REAL(M_DEGREE)*RAD
       SINPH(K)   = SIN(XLAT)
       COSPH(K)   = COS(XLAT)
    END DO

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !    3. INITIALISE GRID INCREMENTS.                                            !
    !       ---------------------------                                            !

    DELPHI = REAL(XDELLA)/REAL(M_DEGREE)*CIRC/360.  !! LATITUDE INCREMENT IN  METRES.

    ALLOCATE (NLON_RG(NY))

    IF (REDUCED_GRID) THEN
       NLON_RG(:) = NINT(NX*COSPH(:)/MAXVAL(COSPH(:)))
       WHERE (MOD(NLON_RG(:),2).EQ.1) NLON_RG(:) = NLON_RG(:)+1
       NLON_RG(:) = MIN(NLON_RG(:),NX)
    ELSE
       NLON_RG(:) = NX
    END IF

    IF (REDUCED_GRID .AND. MINVAL(NLON_RG(:)).LE.0) THEN
       WRITE (IU06,*) '********************************************************'
       WRITE (IU06,*) '*                                                      *'
       WRITE (IU06,*) '*          FATAL ERROR IN SUB. PREPARE_GRID            *'
       WRITE (IU06,*) '*          ================================            *'
       WRITE (IU06,*) '*                                                      *'
       WRITE (IU06,*) '*  A REDUCED GRID CANNOT BE SET UP.                    *'
       WRITE (IU06,*) '*  THE INCREASED LONGITUDE INCREMENT IS GREATER THAN   *'
       WRITE (IU06,*) '*  THE TOTAL LATITUDE LENGTH IN THE GRID AREA.         *'
       WRITE (IU06,*) '*  THE LATITUDE HAS NO GRID POINT (NLON < 2).          *'
       WRITE (IU06,*) '*                                                      *'

       WRITE (IU06,*) '  NO. |    LATITUDE   |   NLON |'
       WRITE (IU06,*) '------|---------------|--------|'
       DO I = NY, 1, -1
          formtext = write_coor_text (AMOSOP + (I-1)*XDELLA)
          WRITE (IU06,'(I6,'' | '',A,'' | '',I6,'' | '')')                         &
               &             I, formtext, NLON_RG(I)  
       END DO

       WRITE (IU06,*) '*                                                      *'
       WRITE (IU06,*) '*   REDUCE THE GRID SOUTH-NORTH EXTENT OR              *'
       WRITE (IU06,*) '*   DECREASE THE BASIC LONGITUDE INCREMENT.            *'
       WRITE (IU06,*) '*                                                      *'
       WRITE (IU06,*) '*              THE PROGRAM ABORTS                      *'
       WRITE (IU06,*) '*                                                      *'
       WRITE (IU06,*) '********************************************************'
       CALL ABORT1
    ENDIF

    ALLOCATE (ZDELLO(NY))
    ALLOCATE (DELLAM(NY))

    IF (IPER) THEN             !! LONG. INCREMENTS IN (M_SEC)
       ZDELLO(:) = NINT(REAL(AMOEAP-AMOWEP+XDELLO)/REAL(NLON_RG(:)))
    ELSE
       ZDELLO(:) = NINT(REAL(AMOEAP-AMOWEP)/REAL(NLON_RG(:)-1))
    END IF

    DELLAM(:) = REAL(ZDELLO(:))/REAL(M_DEGREE)*CIRC/360.  !! LONG. INCREMENTS IN (M)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !    4. ARRANGE THE TOPOGRAPHY ON REQUESTED MODEL AREA AND RESOLUTION.         !
    !       -------------------------------------------------------------          !

    IF (.NOT.ALLOCATED(GRD)) ALLOCATE(GRD(NX,NY))

    GRD(1:NX,1:NY) = -999.

    IF (EQUAL_GRID) THEN
       GRD(1:NX,1:NY) = GRID_IN(1:NX_T,1:NY_T)
       IF (L_OBSTRUCTION_T) THEN
          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +       WARNING ERROR SUB. PREPARE_GRID.           +'
          WRITE (IU06,*) ' +       ================================           +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' + THE REQUESTED MODEL GRID AND                     +'
          WRITE (IU06,*) ' + THE INPUT BATHYMETRY DATA GRID ARE IDENTICAL.    +'
          WRITE (IU06,*) ' + NO OBSTRUCTIONS CAN BE COMPUTED.                 +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' +      MODEL CONTINUES WITHOUT OBSTRUCTIONS        +'
          WRITE (IU06,*) ' +                                                  +'
          WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
          L_OBSTRUCTION_T = .FALSE.
       END IF

       CALL ADJUST_DEPTH
       IF (ITEST.GT.1) WRITE (IU06,*) '   SUB. PREPARE_GRID: ADJUST_DEPTH DONE'

    ELSE

       IF (L_INTERPOL_T) THEN
          CALL SUBGRID_TOPOGRAPHY
          IF (ITEST.GT.1) WRITE (IU06,*) '   SUB. PREPARE_GRID: SUBGRID_TOPOGRAPHY DONE '
       ELSE
          CALL MEAN_DEPTH
          IF (ITEST.GT.1) WRITE (IU06,*) '   SUB. PREPARE_GRID: MEAN_DEPTH DONE '
       END IF

       CALL ADJUST_DEPTH
       IF (ITEST.GT.1) WRITE (IU06,*) '   SUB. PREPARE_GRID: ADJUST_DEPTH DONE'
    END IF

    WHERE (GRD.LT.LAND_LIMIT) GRD = -999.  !! MARK LAND BY -999

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     5. GENERATE LAND-SEA MASK AND COUNT NUMBER OF SEA POINTS.                !
    !        ------------------------------------------------------                !

    ALLOCATE (L_S_MASK(1:NX,1:NY))
    L_S_MASK = GRD.GE.LAND_LIMIT
    NSEA = COUNT (L_S_MASK)

    IF (NSEA.LE.0) THEN
       WRITE (IU06,*) '********************************************************'
       WRITE (IU06,*) '*                                                      *'
       WRITE (IU06,*) '*          FATAL ERROR IN SUB. PREPARE_GRID            *'
       WRITE (IU06,*) '*          ================================            *'
       WRITE (IU06,*) '*                                                      *'
       WRITE (IU06,*) '*  THE MODEL GRID DOES NOT CONTAIN SEA POINTS.         *'
       WRITE (IU06,*) '*  NUMBER OF SEA POINTS IS NSEA = ', NSEA
       formtext = write_coor_text (amowep)
       WRITE (IU06,*) ' + LONGITUDE               WEST = ', formtext
       formtext = write_coor_text (amoeap)
       WRITE (IU06,*) ' + LONGITUDE               EAST = ', formtext
       formtext = write_coor_text (xdello)
       WRITE (IU06,*) ' + LONGITUDE INCREMENT    D_LON = ', formtext
       WRITE (IU06,*) ' + NO. OF LONGITUDES      N_LON = ', NX
       formtext = write_coor_text (amosop)
       WRITE (IU06,*) ' + LATITUDE               SOUTH = ', formtext
       formtext = write_coor_text (amonop)
       WRITE (IU06,*) ' + LATITUDE               NORTH = ', formtext
       formtext = write_coor_text (xdella)
       WRITE (IU06,*) ' + LATITUDE  INCREMENT    D_LAT = ', formtext
       WRITE (IU06,*) ' + NO. OF LATITUDE        N_LAT = ', NY
       WRITE (IU06,*) '*                                                      *'
       WRITE (IU06,*) '*  DOES INPUT DEPTH DATA GRID INCLUDE MODEL GRID?      *'
       WRITE (IU06,*) '*              THE PROGRAM ABORTS                      *'
       WRITE (IU06,*) '*                                                      *'
       WRITE (IU06,*) '********************************************************'
       CALL ABORT1
    END IF

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     6. COMPUTE ARRAYS TO MAP POINT INDEX TO GRID POINT INDEX.                !
    !        ------------------------------------------------------                !

    ALLOCATE (IXLG (1:NSEA))
    ALLOCATE (KXLT (1:NSEA))

    IP = 0
    DO K = 1,NY
       DO I = 1,NX
          IF (L_S_MASK(I,K)) THEN
             IP = IP+1
             IXLG(IP) = I
             KXLT(IP) = K
          END IF
       END DO
    END DO

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     7. REMOVE LAND POINT FROM DEPTH ARRAY.                                   !
    !        -----------------------------------                                   !

    ALLOCATE (DEPTH_B(1:NSEA))
    DEPTH_B = PACK (GRD, L_S_MASK)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     8. COMPUTE INDICES OF NEIGHBOUR SEA POINTS AND WEIGHT IN ADVECTION SCHEME!
    !        FOR CLOSEST GRIDPOINT IN NORTH-SOUTH DIRECTION.                       !
    !        ----------------------------------------------------------------------!

    ALLOCATE (KLAT(1:NSEA,1:2,1:2))       !! CLOSEST AND SECOND CLOSEST POINTS 
    !! ON SOUTH AND NORTH LATITUDE.
    ALLOCATE (KLON(1:NSEA,1:2))           !! NEXT WEST AND EAST POINTS ON LATITUDE.
    KLON = 0
    ALLOCATE (WLAT(1:NSEA,1:2))           !! WEIGHT IN ADVECTION FOR N-S DIRECTION.

    IP = 0
    DO IP = 1, NSEA
       I = IXLG(IP)
       K = KXLT(IP)

       !     8.1 WEST LONGITUDE NEIGHBOURS.                                           !

       IF (I.GT.1) THEN
          IF (L_S_MASK(I-1,K)) KLON(IP,1) = IP-1
       ELSE
          IF (IPER.AND.L_S_MASK(NLON_RG(K),K)) KLON(IP,1) = IP+COUNT(L_S_MASK(2:NX,K))
       END IF

       !     8.2 EAST LONGITUDE NEIGHBOURS.                                           !

       IF (I.LT.NLON_RG(K)) THEN
          IF (L_S_MASK(I+1,K)) KLON(IP,2) = IP+1
       ELSE
          IF (IPER.AND.L_S_MASK(1,K)) KLON(IP,2) = IP+1-COUNT(L_S_MASK(1:NX,K))
       END IF

       !     8.3 CLOSEST AND SECOND CLOSEST SOUTH LATITUDE NEIGHBOURS.                !

       IF (K.GT.1) THEN
          KH = K-1
          XH = REAL(I-1)*ZDELLO(K)/ZDELLO(KH)     ! LONGITUDE OF GRID POINT
          CALL CLOSEST_GP_LAT (I, K, IP, XH, KH, KLAT(IP,1,1), KLAT(IP,1,2), WLAT(IP,1))
       ELSE
          KLAT(IP,1,1) = 0
          KLAT(IP,1,2) = 0
          WLAT(IP,1)   = 1
       ENDIF

       !     8.4 CLOSEST AND SECOND CLOSEST NORTH LATITUDE NEIGHBOURS.                !

       IF (K.LT.NY) THEN
          KH = K+1
          XH = REAL(I-1)*ZDELLO(K)/ZDELLO(KH)     ! LONGITUDE OF GRID POINT
          CALL CLOSEST_GP_LAT (I, K, IP, XH, KH, KLAT(IP,2,1), KLAT(IP,2,2), WLAT(IP,2))
       ELSE
          KLAT(IP,2,1) = 0
          KLAT(IP,2,2) = 0
          WLAT(IP,2)   = 1.
       ENDIF
    END DO

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     11. CREATE OBSTRUCTIONS.                                                 !
    !        ---------------------                                                 !

    IF (L_OBSTRUCTION_T) THEN
       ALLOCATE (OBSLAT (NSEA,2,ML))
       ALLOCATE (OBSLON (NSEA,2,ML))
       CALL CREATE_OBSTRUCTIONS
       IF (ITEST.GT.1) WRITE(IU06,*) '   SUB. OBSTRUCTION DONE'
    ENDIF
    IF (.NOT.L_OBSTRUCTION_T) THEN
       IF (ALLOCATED(OBSLAT)) DEALLOCATE(OBSLAT)
       IF (ALLOCATED(OBSLON)) DEALLOCATE(OBSLON)
    ENDIF

  END SUBROUTINE PREPARE_GRID

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

  SUBROUTINE SUBGRID_TOPOGRAPHY

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !   SUBGRID_TOPOGRAPHY - ARRANGE SUBGRID TOPOGRAPHY.                           !
    !                                                                              !
    !     S. HASSELMANN     MPIFM           1/6/86.                                !
    !                                                                              !
    !     MODIFIED BY       H. GUNTHER      1/4/90  -  REARANGEMENT OF CODE.       !
    !                                                                              !
    !     PURPOSE.                                                                 !
    !     --------                                                                 !
    !                                                                              !
    !       TO CONVERT THE INPUT GRID TO THE MODEL GRID.                           !
    !                                                                              !
    !     METHOD.                                                                  !
    !     -------                                                                  !
    !                                                                              !
    !       THE TOPOGRAPHIC DATA ARE PUT ON THE REQUESTED SUBGRID LAT-LONG         !
    !       RESOLUTION, ALWAYS USING THE NEAREST POINT.                            !
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

    INTEGER :: I, K
    INTEGER :: IH(NX), KH(NY)

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    !     1. STORING TOPOGRAPHIC DATA AT LONGITUDES AND LATITUDES OF GRID AREA.    !
    !        ------------------------------------------------------------------    !

    DO K = 1,NY
       KH(K) = NINT( REAL(AMOSOP + (K-1)*XDELLA - SOUTH_T)/REAL(DY_T) ) + 1
    END DO
    IF (MINVAL(KH).LT.1 .OR. MAXVAL(KH).GT.NY_T) THEN
       WRITE (IU06,*) ' *****************************************************'
       WRITE (IU06,*) ' *                                                   *'
       WRITE (IU06,*) ' *     FATAL  ERROR IN SUB. SUBGRID_TOPOGRAPHY       *'
       WRITE (IU06,*) ' *     =======================================       *'
       WRITE (IU06,*) ' *                                                   *'
       WRITE (IU06,*) ' * INPUT TOPOGRAPHY DOES NOT FIT TO REQUESTED GRID.  *'
       WRITE (IU06,*) ' *                                                   *'
       WRITE (IU06,*) ' *      PROGRAM ABORTS  PROGRAM ABORTS               *'
       WRITE (IU06,*) ' *                                                   *'
       WRITE (IU06,*) ' *****************************************************'
       CALL PRINT_PREPROC_STATUS
       CALL ABORT1
    END IF

    DO K=1,NY
       DO I=1,NLON_RG(K)
          IF (PER_T) THEN
             IH(I) = NINT( REAL(AMOWEP + (I-1)*ZDELLO(K)+M_S_PER - WEST_T)/REAL(DX_T) )
             IH(I) = MOD(IH(I)+NX_T,NX_T) + 1
          ELSE
             IH(I) = NINT( REAL(AMOWEP + (I-1)*ZDELLO(K) - WEST_T)/REAL(DX_T) ) + 1
          END IF
       END DO
       IF (MINVAL(IH(1:NLON_RG(K))).LT.1 .OR. MAXVAL(IH(1:NLON_RG(K))).GT.NX_T) THEN
          WRITE (IU06,*) ' *****************************************************'
          WRITE (IU06,*) ' *                                                   *'
          WRITE (IU06,*) ' *     FATAL  ERROR IN SUB. SUBGRID_TOPOGRAPHY       *'
          WRITE (IU06,*) ' *     =======================================       *'
          WRITE (IU06,*) ' *                                                   *'
          WRITE (IU06,*) ' * INPUT TOPOGRAPHY DOES NOT FIT TO REQUESTED GRID.  *'
          WRITE (IU06,*) ' *                                                   *'
          WRITE (IU06,*) ' *      PROGRAM ABORTS  PROGRAM ABORTS               *'
          WRITE (IU06,*) ' *                                                   *'
          WRITE (IU06,*) ' *****************************************************'
          CALL PRINT_PREPROC_STATUS
          CALL ABORT1
       END IF

       GRD(1:NLON_RG(K),K) = GRID_IN(IH(1:NLON_RG(K)),KH(K))
    END DO

  END SUBROUTINE SUBGRID_TOPOGRAPHY

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE PREPROC_MODULE
