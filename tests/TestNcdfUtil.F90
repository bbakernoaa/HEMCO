!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: TestNcdfUtil.F90
!
! !DESCRIPTION: Program to test the NcdfUtil library.
!
!EOP
!------------------------------------------------------------------------------
PROGRAM TestNcdfUtil
  !
  ! !USES:
  !
  USE HCO_PRECISION_MOD, ONLY: fp
  USE HCO_m_netcdf_io_define
  USE HCO_m_netcdf_io_create
  USE HCO_m_netcdf_io_write
  USE HCO_m_netcdf_io_open
  USE HCO_m_netcdf_io_get_dimlen
  USE HCO_m_netcdf_io_read
  USE HCO_m_netcdf_io_readattr
  USE HCO_m_netcdf_io_close
  USE netcdf, ONLY: nf90_noerr, nf90_fill, nf90_nofill, nf90_char, &
       nf90_int, nf90_float, nf90_double
  IMPLICIT NONE

  !
  ! !LOCAL VARIABLES:
  !
  INTEGER, PARAMETER :: ILONG = 72
  INTEGER, PARAMETER :: ILAT = 46
  INTEGER, PARAMETER :: IVERT = 55
  INTEGER, PARAMETER :: ITIME = 1
  INTEGER, PARAMETER :: ICHAR1 = 2
  INTEGER, PARAMETER :: ICHAR2 = 20
  INTEGER            :: i, longdeg, latdeg
  REAL(fp)           :: longDat(ILONG), latDat(ILAT), levDat(IVERT)
  INTEGER            :: timeDat(ITIME)
  INTEGER            :: n_failed
  CHARACTER(LEN=255) :: test_file

  n_failed = 0
  test_file = 'my_filename.nc'

  longdeg = 360 / ILONG
  DO i = 1, ILONG
     longDat(i) = i*longdeg
  ENDDO

  latdeg  = 180 / ILAT
  DO i = 1, ILAT
     latDat(i) = -90 + (i-0.5)*latdeg
  ENDDO

  DO i = 1, IVERT
     levDat(i) = 1000.00_fp - (i-1)*(920.00_fp/IVERT)
  ENDDO

  DO i = 1, ITIME
     timeDat(i) = 0
  ENDDO

  WRITE( 6, '(a)' ) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
  WRITE( 6, '(a)' ) '%%%  Testing NcdfUtil            %%%'
  WRITE( 6, '(a)' ) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

  CALL TestNcdfCreate(n_failed)
  CALL TestNcdfRead(n_failed)

  IF (n_failed > 0) THEN
     WRITE(*,*) 'Number of failed tests: ', n_failed
     STOP 1
  END IF

CONTAINS

  SUBROUTINE TestNcdfCreate(n_failed)
    INTEGER, INTENT(INOUT) :: n_failed
    INTEGER             :: idLon, idLat, idLev, idTime
    INTEGER             :: idChar1, idChar2
    INTEGER             :: fId, vId, omode, i
    INTEGER             :: ct1d(1), ct2d(2), ct3d(3), ct4d(4)
    INTEGER             :: st1d(1), st2d(2), st3d(3), st4d(4)
    INTEGER             :: var1(1), var2(2), var3(3), var4(4)
    CHARACTER(LEN=255)  :: units, delta_t, begin_d
    CHARACTER(LEN=255)  :: begin_t, incr
    REAL(fp)            :: PS(ILONG, ILAT, ITIME)
    REAL(fp)            :: T(ILONG, ILAT, IVERT, ITIME)
    CHARACTER :: DESC(ICHAR1, ICHAR2)
    LOGICAL, PARAMETER  :: COMPRESS = .TRUE.

    WRITE(6, '(a)') '=== Begin netCDF file creation test ==='
    CALL NcCr_Wr(fId, test_file)
    CALL NcSetFill(fId, nf90_nofill, omode)

    CALL NcDef_Dimension(fId, 'time', ITIME, idTime)
    CALL NcDef_Dimension(fId, 'lev', IVERT, idLev)
    CALL NcDef_Dimension(fId, 'lat', ILAT, idLat)
    CALL NcDef_Dimension(fId, 'lon', ILONG, idLon)
    CALL NcDef_Dimension(fId, 'cdim1', ICHAR1, idChar1)
    CALL NcDef_Dimension(fId, 'cdim2', ICHAR2, idChar2)

    CALL NcDef_Glob_Attributes(fId, 'Title', 'NcdfUtilities test file')
    CALL NcDef_Glob_Attributes(fId, 'History', 'test file - 24 Jan 2011')
    CALL NcDef_Glob_Attributes(fId, 'Conventions', 'COARDS')
    CALL NcDef_Glob_Attributes(fId, 'Model', 'GEOS4')

    var1 = (/ idTime /)
    units = 'minutes since 2011-01-01 00:00:00 GMT'
    CALL NcDef_Variable(fId, 'time', nf90_int, 1, var1, vId, COMPRESS)
    CALL NcDef_Var_Attributes(fId, vId, 'long_name', 'time')
    CALL NcDef_Var_Attributes(fId, vId, 'units', TRIM(units))

    var1 = (/ idLev /)
    CALL NcDef_Variable(fId, 'lev', nf90_double, 1, var1, vId, COMPRESS)
    CALL NcDef_Var_Attributes(fId, vId, 'long_name', 'Pressure')
    CALL NcDef_Var_Attributes(fId, vId, 'units', 'hPa')

    var1 = (/ idLat /)
    CALL NcDef_Variable(fId, 'lat', nf90_double, 1, var1, vId, COMPRESS)
    CALL NcDef_Var_Attributes(fId, vId, 'long_name', 'Latitude')
    CALL NcDef_Var_Attributes(fId, vId, 'units', 'degrees_north')

    var1 = (/ idLon /)
    CALL NcDef_Variable(fId, 'lon', nf90_double, 1, var1, vId, COMPRESS)
    CALL NcDef_Var_Attributes(fId, vId, 'long_name', 'Longitude')
    CALL NcDef_Var_Attributes(fId, vId, 'units', 'degrees_east')

    var3 = (/ idLon, idLat, idTime /)
    CALL NcDef_Variable(fId, 'PS', nf90_float, 3, var3, vId, COMPRESS)
    CALL NcDef_Var_Attributes(fId, vId, 'long_name', 'Surface Pressure')
    CALL NcDef_Var_Attributes(fId, vId, 'units', 'hPa')
    CALL NcDef_Var_Attributes(fId, vId, '_FillValue', 1e15_fp)

    CALL NcEnd_Def(fId)
    CALL NcBegin_Def(fId)

    var4 = (/ idLon, idLat, idLev, idTime /)
    CALL NcDef_Variable(fId, 'T', nf90_float, 4, var4, vId, COMPRESS)
    CALL NcDef_Var_Attributes(fId, vId, 'long_name', 'Temperature')
    CALL NcDef_Var_Attributes(fId, vId, 'units', 'K')
    CALL NcDef_Var_Attributes(fId, vId, '_FillValue', 1e15_fp)

    var2 = (/ idChar1, idChar2 /)
    CALL NcDef_Variable(fId, 'DESC', nf90_char, 2, var2, vId, COMPRESS)
    CALL NcDef_Var_Attributes(fId, vId, 'long_name', 'Description')

    CALL NcEnd_def(fId)

    st1d = (/ 1 /); ct1d = (/ ILONG /)
    CALL NcWr(longDat, fId, 'lon', st1d, ct1d)

    st1d = (/ 1 /); ct1d = (/ ILAT /)
    CALL NcWr(latDat, fId, 'lat', st1d, ct1d)

    st1d = (/ 1 /); ct1d = (/ IVERT /)
    CALL NcWr(levDat, fId, 'lev', st1d, ct1d)

    st1d = (/ 1 /); ct1d = (/ ITIME /)
    CALL NcWr(timeDat, fId, 'time', st1d, ct1d)

    PS = 1.0_fp
    st3d = (/ 1, 1, 1 /); ct3d = (/ ILONG, ILAT, ITIME /)
    CALL NcWr(PS, fId, 'PS', st3d, ct3d)

    T = 1.0_fp
    st4d = (/ 1, 1, 1, 1 /); ct4d = (/ ILONG, ILAT, IVERT, ITIME /)
    CALL NcWr(T, fId, 'T', st4d, ct4d)

    DO i = 1, ICHAR2
       DESC(1,i) = ACHAR(64+i)
       DESC(2,i) = ACHAR(96+i)
    ENDDO
    st2d = (/ 1, 1 /); ct2d = (/ ICHAR1, ICHAR2 /)
    CALL NcWr(DESC, fId, 'DESC', st2d, ct2d)

    CALL NcCl(fId)
    WRITE(6, '(a)') '=== End netCDF file creation test ==='
  END SUBROUTINE TestNcdfCreate

  SUBROUTINE TestNcdfRead(n_failed)
    INTEGER, INTENT(INOUT) :: n_failed
    INTEGER :: fId, rc, XDim, YDim, ZDim, TDim, CDim1, CDim2
    INTEGER :: ct1d(1), ct2d(2), ct3d(3), ct4d(4)
    INTEGER :: st1d(1), st2d(2), st3d(3), st4d(4)
    CHARACTER(LEN=255) :: attValue
    REAL(fp) :: attValR4
    REAL(fp), ALLOCATABLE :: lon(:), lat(:), lev(:), PS(:,:,:), T(:,:,:,:)
    INTEGER, ALLOCATABLE :: time(:)
    CHARACTER, ALLOCATABLE :: DESC(:,:)
    REAL(fp) :: valid(2)

    WRITE(6, '(a)') '=== Begin netCDF file reading test ==='
    CALL Ncop_Rd(fId, test_file)

    CALL Ncget_Dimlen(fId, 'lon', XDim)
    CALL Ncget_Dimlen(fId, 'lat', YDim)
    CALL Ncget_Dimlen(fId, 'lev', ZDim)
    CALL Ncget_Dimlen(fId, 'time', TDim)
    CALL Ncget_Dimlen(fId, 'cdim1', CDim1)
    CALL Ncget_Dimlen(fId, 'cdim2', CDim2)

    CALL Check('Reading lon dim', REAL(XDim - ILONG, fp), n_failed)
    CALL Check('Reading lat dim', REAL(YDim - ILAT, fp), n_failed)
    CALL Check('Reading lev dim', REAL(ZDim - IVERT, fp), n_failed)
    CALL Check('Reading time dim', REAL(TDim - ITIME, fp), n_failed)
    CALL Check('Reading cdim1 dim', REAL(CDim1 - ICHAR1, fp), n_failed)
    CALL Check('Reading cdim2 dim', REAL(CDim2 - ICHAR2, fp), n_failed)

    ALLOCATE(lon(XDim)); st1d = (/1/); ct1d = (/XDim/)
    CALL NcRd(lon, fId, 'lon', st1d, ct1d)
    CALL Check('Reading lon data', SUM(lon - longDat), n_failed)

    ALLOCATE(lat(YDim)); st1d = (/1/); ct1d = (/YDim/)
    CALL NcRd(lat, fId, 'lat', st1d, ct1d)
    CALL Check('Reading lat data', SUM(lat - latDat), n_failed)

    ALLOCATE(lev(ZDim)); st1d = (/1/); ct1d = (/ZDim/)
    CALL NcRd(lev, fId, 'lev', st1d, ct1d)
    CALL Check('Reading lev data', SUM(lev - levDat), n_failed)

    ALLOCATE(time(TDim)); st1d = (/1/); ct1d = (/TDim/)
    CALL NcRd(time, fId, 'time', st1d, ct1d)
    CALL Check('Reading time data', REAL(SUM(time - timeDat), fp), n_failed)

    ALLOCATE(ps(XDim, YDim, TDim)); st3d = (/1,1,1/); ct3d = (/XDim,YDim,TDim/)
    CALL NcRd(ps, fId, 'PS', st3d, ct3d)
    CALL Check('Reading PS data', SUM(ps) - SIZE(ps), n_failed)

    CALL NcGet_Var_Attributes(fId, 'PS', 'units', attValue)
    CALL Check('Reading PS units', REAL(MERGE(0, -1, TRIM(attValue) == 'hPa'), fp), n_failed)

    CALL NcGet_Var_Attributes(fId, 'PS', '_FillValue', attValR4)
    CALL Check('Reading PS _FillValue', REAL(MERGE(0, -1, attValR4 == 1e15_fp), fp), n_failed)

    ALLOCATE(T(XDim, YDim, ZDim, TDim)); st4d = (/1,1,1,1/); ct4d = (/XDim,YDim,ZDim,TDim/)
    CALL NcRd(T, fId, 'T', st4d, ct4d)
    CALL Check('Reading T data', SUM(T) - SIZE(T), n_failed)

    CALL NcGet_Var_Attributes(fId, 'T', 'units', attValue)
    CALL Check('Reading T units', REAL(MERGE(0, -1, TRIM(attValue) == 'K'), fp), n_failed)

    ALLOCATE(DESC(CDim1, CDim2)); st2d = (/1, 1/); ct2d = (/ICHAR1, ICHAR2/)
    CALL NcRd(DESC, fId, 'DESC', st2d, ct2d)

    rc = 0
    DO i = 1, ICHAR2
       IF (ICHAR(DESC(1,i)) - 64 /= i) rc = 1
       IF (ICHAR(DESC(2,i)) - 96 /= i) rc = 1
    ENDDO
    CALL Check('Reading DESC data', REAL(rc, fp), n_failed)

    CALL NcCl(fId)
    DEALLOCATE(lon, lat, lev, time, ps, T, DESC)
    WRITE(6, '(a)') '=== End of netCDF file read test! ==='
  END SUBROUTINE TestNcdfRead

  SUBROUTINE Check(msg, rc, n_failed)
    CHARACTER(LEN=*), INTENT(IN) :: msg
    REAL(fp), INTENT(IN) :: rc
    INTEGER, INTENT(INOUT) :: n_failed
    INTEGER :: s
    s = LEN(msg)
    IF (ABS(rc) < 1e-6_fp) THEN
       WRITE(6, '(a)') msg // REPEAT('.', 55-s) // 'PASSED'
    ELSE
       WRITE(6, '(a)') msg // REPEAT('.', 55-s) // 'FAILED'
       n_failed = n_failed + 1
    ENDIF
  END SUBROUTINE Check

END PROGRAM TestNcdfUtil
