module module_nf90_utilities
  use netcdf
  implicit none
  public unf90_io_varatt, unf90_io_var
  interface unf90_io_varatt
    module procedure unf90_io_varatt_char
    module procedure unf90_io_varatt_real
    module procedure unf90_io_varatt_int
!   module procedure unf90_io_varatt_double
  end interface
  interface unf90_out_varatt
    module procedure unf90_out_varatt_char
    module procedure unf90_out_varatt_real
    module procedure unf90_out_varatt_int
!   module procedure unf90_out_varatt_double
  end interface
  interface unf90_def_var
    module procedure unf90_def_var_str
    module procedure unf90_def_var_array
    module procedure unf90_def_var_type
  end interface unf90_def_var
  interface unf90_io_var
    module procedure unf90_io1d_int
    module procedure unf90_io1d_real
!   module procedure unf90_io1d_double
    module procedure unf90_io2d_int
    module procedure unf90_io2d_real
!   module procedure unf90_io2d_double
    module procedure unf90_io1d_time_real
!   module procedure unf90_io1d_time_double
    module procedure unf90_io2d_time_real
!   module procedure unf90_io2d_time_double
    module procedure unf90_io3d_real
    module procedure unf90_io3d_int
!   module procedure unf90_io3d_double
    module procedure unf90_io4d_real
!   module procedure unf90_io4d_double
  end interface
  interface unf90_rd_tile2vec
    module procedure unf90_rd1d_real_tile2vec
    module procedure unf90_rd2d_real_tile2vec
    module procedure unf90_rd3d_real_tile2vec
  end interface
  public unf90_get_var_type
  public unf90_check_var_type
  !public utools_subStrCounts
  contains
!-------------------------------------------
  subroutine unf90_op_ncfile(option, fname, ncid, ncformat)
  implicit none
  character(len=*),           intent(in)  :: option, fname
  character(len=*), optional, intent(in)  :: ncformat
  integer,                    intent(out) :: ncid
  integer                                 :: iret, mode
  if(trim(option) == 'C' .or. trim(option) == 'c') then
    if(present(ncformat) .and. trim(ncformat) == 'netcdf4') then
      iret = nf90_create(fname, nf90_netcdf4, ncid)
    else
      iret = nf90_create(fname, nf90_clobber, ncid)
    endif
  else
    if(trim(option) == 'R' .or. trim(option) == 'r') then
      mode = nf90_nowrite
    else if(trim(option) == 'W' .or. trim(option) == 'w') then
      mode = nf90_write
    else
      stop 'invalid mode option in op_ncfile'
    endif
    iret = nf90_open(trim(fname), mode, ncid)
    if(iret /= nf90_noerr) then
      iret = nf90_open(trim(fname)//'4', mode, ncid)
    endif
    if(iret /= nf90_noerr) then
      write(*,*) 'file '//trim(fname)//' not found!'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_op_ncfile
!-------------------------------------------
  subroutine unf90_def_dim(ncid, ii, dname)
  implicit none
  integer,          intent(in) :: ncid, ii
  character(len=*), intent(in) :: dname
  integer                      :: iret, id
  if(ii == 0) then
    iret = nf90_def_dim(ncid, trim(dname), nf90_unlimited, id)
  else
    iret = nf90_def_dim(ncid, trim(dname), ii, id)
  endif
  call unf90_check_err(iret)
  end subroutine unf90_def_dim
!-------------------------------------------
  subroutine unf90_get_dimid(ncid, dname, dimid)
  implicit none
  integer,          intent(in)  :: ncid
  character(len=*), intent(in)  :: dname
  integer,          intent(out) :: dimid
  integer                       :: iret
  iret = nf90_inq_dimid(ncid, trim(dname), dimid)
  call unf90_check_err(iret)
  end subroutine unf90_get_dimid
!-------------------------------------------
  subroutine unf90_get_dimlen(ncid, dname, len)
  implicit none
  integer,          intent(in)  :: ncid
  character(len=*), intent(in)  :: dname
  integer,          intent(out) :: len
  integer                       :: iret, dimid
  character(len=20)             :: dimname
  iret = nf90_inq_dimid(ncid, trim(dname), dimid)
  call unf90_check_err(iret)
  iret = nf90_inquire_dimension(ncid, dimid, dimname, len)
  call unf90_check_err(iret)
  end subroutine unf90_get_dimlen
!-------------------------------------------
  subroutine unf90_get_dimname(ncid, dimid, dname, len)
  implicit none
  integer,          intent(in)            :: ncid, dimid
  character(len=*), optional, intent(out) :: dname
  integer,          optional, intent(out) :: len
  integer                                 :: iret
  iret = nf90_inquire_dimension(ncid, dimid, name=dname, len=len)
  call unf90_check_err(iret)
  end subroutine unf90_get_dimname
!----------------------------------------
 subroutine unf90_get_var_info(ncid, varid, vname, xtype, vdims, natts)
  implicit none
  integer,          intent(in)  :: ncid, varid
  character(len=*), intent(out) :: vname
  integer,          intent(out) :: xtype, vdims, natts
  integer                       :: iret
  iret = nf90_inquire_variable(ncid, varid, name=vname, xtype=xtype, ndims=vdims, natts=natts)
  call unf90_check_err(iret)
  end subroutine unf90_get_var_info
!----------------------------------------
  subroutine unf90_get_var_dims(ncid, vname, vdims)
  implicit none
  integer,          intent(in)  :: ncid
  character(len=*), intent(in)  :: vname
  integer,          intent(out) :: vdims
  character(len=10)             :: varname
  integer                       :: iret, varid
  iret = nf90_inq_varid(ncid, vname, varid)
  call unf90_check_err(iret)
  iret = nf90_inquire_variable(ncid, varid, name=varname, ndims=vdims)
  call unf90_check_err(iret)
  end subroutine unf90_get_var_dims
!----------------------------------------
  subroutine unf90_get_var_dimids(ncid, varid, vdims, vdimids)
  implicit none
  integer, intent(in)  :: ncid, varid, vdims
  integer, intent(out) :: vdimids(vdims)
  integer              :: iret
  iret = nf90_inquire_variable(ncid, varid, dimids=vdimids(:))
  call unf90_check_err(iret)
  end subroutine unf90_get_var_dimids
!----------------------------------------
  subroutine unf90_get_ncinfo(ncid, ndimensions, nvariables, nattributes, unlimiteddimid)
  implicit none
  integer,           intent(in)  :: ncid
  integer, optional, intent(out) :: ndimensions, nvariables, nattributes, unlimiteddimid
  integer                        :: iret
  iret = nf90_inquire(ncid, ndimensions, nvariables, nattributes, unlimiteddimid)
  call unf90_check_err(iret)
  end subroutine unf90_get_ncinfo
!----------------------------------------
  subroutine unf90_copy_global_att(ncid_in, ncid_out)
  implicit none
  integer, intent(in) :: ncid_in, ncid_out
  integer             :: iret, natts, i
  character(len=128)  :: attname
    do i = 1, 50
      iret = nf90_inq_attname(ncid_in, nf90_global, i, attname)
      if(iret /= nf90_noerr) then
        natts = i
        exit
      endif
      iret = nf90_copy_att(ncid_in, nf90_global, attname, ncid_out, nf90_global)
      call unf90_check_err(iret)
    enddo
  end subroutine unf90_copy_global_att
!----------------------------------------
  subroutine unf90_copy_var_att(ncid_in, vname_in, ncid_out, vname_out)
  implicit none
  character(len=*), intent(in) :: vname_in, vname_out
  integer,          intent(in) :: ncid_in, ncid_out
  integer                      :: iret, varid_in, varid_out, natts, i
  character(len=20)            :: attname
  iret = nf90_inq_varid(ncid_in,  vname_in,  varid_in)
  call unf90_check_err(iret)
  iret = nf90_inquire_variable(ncid_in, varid_in, natts=natts)
  call unf90_check_err(iret)
  iret = nf90_inq_varid(ncid_out, vname_out, varid_out)
  call unf90_check_err(iret)
  do i = 1, natts
    iret = nf90_inq_attname(ncid_in, varid_in, i, attname)
    call unf90_check_err(iret)
    iret = nf90_copy_att(ncid_in, varid_in, attname, ncid_out, varid_out)
    call unf90_check_err(iret)
  enddo
  end subroutine unf90_copy_var_att
!----------------------------------------
  subroutine unf90_copy_var(incid, oncid, vname)
  implicit none
  integer, intent(in)           :: incid, oncid
  character(len=*), intent(in)  :: vname
  real, allocatable             :: r1d(:)
  double precision, allocatable :: d1d(:)
  real                          :: r0d
  double precision              :: d0d
  integer, allocatable          :: istart(:), icount(:), vdimids(:), dlen(:)
  integer                       :: xtype, vdims, iret, ivarid, ovarid, loops, loop
  integer                       :: iv, blocks
  loops = 1
  iret = nf90_inq_varid(incid, trim(vname), ivarid)
  call unf90_check_err(iret)
  iret = nf90_inq_varid(oncid, trim(vname), ovarid)
  call unf90_check_err(iret)
  iret = nf90_inquire_variable(incid, ivarid, xtype=xtype, ndims=vdims)
  call unf90_check_err(iret)
  if(vdims > 0) then
    allocate(vdimids(vdims))
    allocate(dlen(vdims))
    allocate(istart(vdims))
    allocate(icount(vdims))
    iret = nf90_inquire_variable(incid, ivarid, dimids=vdimids(:))
    call unf90_check_err(iret)
    do iv = 1, vdims
      iret = nf90_inquire_dimension(incid, vdimids(iv), len=dlen(iv))
      call unf90_check_err(iret)  
      loops = loops*dlen(iv)
    enddo
    loops = loops/dlen(1)
    blocks = 0
    istart = 1
    icount = 1
    icount(1) = dlen(1)
    do loop = 1, loops
      blocks = blocks + dlen(1)
      call unf90_get_istart(vdims, dlen, blocks, istart)
      if(xtype == nf90_float) then
        allocate(r1d(dlen(1)))
        call unf90_io1d_real('r', incid, dlen(1), r1d, vname, istart, icount)
        call unf90_io1d_real('w', oncid, dlen(1), r1d, vname, istart, icount)
        deallocate(r1d)
      else if(xtype == nf90_double) then
        allocate(d1d(dlen(1)))
        call unf90_io1d_double('r', incid, dlen(1), d1d, vname, istart, icount)
        call unf90_io1d_double('w', oncid, dlen(1), d1d, vname, istart, icount)
        deallocate(d1d)
      else
        write(*,*) 'error: invalid xtype in copy_var'
        stop 
      endif
    enddo
    deallocate(vdimids)
    deallocate(dlen)
    deallocate(istart)
    deallocate(icount)
  endif
  end subroutine unf90_copy_var
!----------------------------------------
  subroutine unf90_copy_var_vec2tile(incid, oncid, vname, location, cube_i, cube_j, cube_tile, nx, ny, tile, dfillValue)
  implicit none
  integer,          intent(in)  :: incid, oncid, location, nx, ny, tile
  integer,          intent(in)  :: cube_i(location), cube_j(location), cube_tile(location)
  character(len=*), intent(in)  :: vname
  double precision, intent(in)  :: dfillValue
  real,             allocatable :: r1d(:), r2d(:,:)
  double precision, allocatable :: d1d(:), d2d(:,:)
  integer,          allocatable :: istart_vec(:),  icount_vec(:),  vdimids_vec(:),  dlen_vec(:)
  integer,          allocatable :: istart_tile(:), icount_tile(:)
  integer                       :: vdims_vec, vdims_tile
  integer                       :: xtype, iret, ivarid, ovarid, loops, loop
  integer                       :: iv, blocks_vec, blocks_tile, lp
  loops = 1
  iret = nf90_inq_varid(incid, trim(vname), ivarid)
  call unf90_check_err(iret)
  iret = nf90_inq_varid(oncid, trim(vname), ovarid)
  call unf90_check_err(iret)
  iret = nf90_inquire_variable(incid, ivarid, xtype=xtype, ndims=vdims_vec)
  call unf90_check_err(iret)
  if(vdims_vec > 0) then
    allocate(vdimids_vec(vdims_vec))
    allocate(dlen_vec(vdims_vec))
    allocate(istart_vec(vdims_vec))
    allocate(icount_vec(vdims_vec))
    vdims_tile = vdims_vec + 1
    allocate(istart_tile(vdims_tile))
    allocate(icount_tile(vdims_tile))
    iret = nf90_inquire_variable(incid, ivarid, dimids=vdimids_vec(:))
    call unf90_check_err(iret)
    do iv = 1, vdims_vec
      iret = nf90_inquire_dimension(incid, vdimids_vec(iv), len=dlen_vec(iv))
      call unf90_check_err(iret)  
      loops = loops*dlen_vec(iv)
    enddo
    loops = loops/dlen_vec(1)
    istart_vec = 1
    icount_vec = 1
    icount_vec(1) = dlen_vec(1)
    istart_tile = 1
    icount_tile = 1
    icount_tile(1) = nx
    icount_tile(2) = ny
    blocks_vec = 0
    do loop = 1, loops
      blocks_vec = blocks_vec + dlen_vec(1)
      call unf90_get_istart(vdims_vec, dlen_vec, blocks_vec, istart_vec)
      if(vdims_vec > 1) then
        do iv = 2, vdims_vec
          istart_tile(iv+1) = istart_vec(iv)
        enddo
      endif
      if(xtype == nf90_float) then
        allocate(r1d(dlen_vec(1)))
        allocate(r2d(nx,ny))
        call unf90_io1d_real('r', incid, dlen_vec(1), r1d, vname, istart_vec, icount_vec)
        r2d = dfillValue
        do lp = 1, dlen_vec(1)
          if(cube_tile(lp) == tile) r2d(cube_i(lp),cube_j(lp)) = r1d(lp)
        enddo
        call unf90_io2d_real('w', oncid, nx,ny, r2d, vname, istart_tile, icount_tile)
        deallocate(r1d)
        deallocate(r2d)
      else if(xtype == nf90_double) then
        allocate(d1d(dlen_vec(1)))
        allocate(d2d(nx,ny))
        call unf90_io1d_double('r', incid, dlen_vec(1), d1d, vname, istart_vec,  icount_vec)
        d2d = dfillValue
        do lp = 1, dlen_vec(1)
          if(cube_tile(lp) == tile) d2d(cube_i(lp),cube_j(lp)) = d1d(lp)
        enddo
        call unf90_io2d_double('w', oncid, nx, ny,      d2d, vname, istart_tile, icount_tile)
        deallocate(d1d)
        deallocate(d2d)
      else
        write(*,*) 'error: invalid xtype in copy_var'
        stop 
      endif
    enddo
    deallocate(vdimids_vec)
    deallocate(dlen_vec)
    deallocate(istart_vec)
    deallocate(icount_vec)
    deallocate(istart_tile)
    deallocate(icount_tile)
  endif
  end subroutine unf90_copy_var_vec2tile
!----------------------------------------
  subroutine unf90_get_istart(dims, dimlens, blocks, istart)
  implicit none
  integer, intent(in)  :: dims, blocks
  integer, intent(in)  :: dimlens(dims)
  integer, intent(out) :: istart(dims)
  integer              :: j, k, m, acc
  istart = 1
  if(dims == 1) then
    return
  else if(dims == 2) then
    acc = 0
    do j = 1, dimlens(2)
      acc = acc + dimlens(1)
      if(acc == blocks) then
        istart(2) = j
        return
      endif
    enddo
  else if(dims == 3) then
    acc = 0
    do k = 1, dimlens(3)
      do j = 1, dimlens(2)
        acc = acc + dimlens(1)
        if(acc == blocks) then
          istart(2) = j
          istart(3) = k
          return
        endif
      enddo
    enddo
  else if(dims == 4) then
    acc = 0
    do m = 1, dimlens(4)
      do k = 1, dimlens(3)
        do j = 1, dimlens(2)
          acc = acc + dimlens(1)
          if(acc == blocks) then
            istart(2) = j
            istart(3) = k
            return
          endif
        enddo
      enddo
    enddo
  else
    stop 'invalid dims in unf90_get_istart'
  endif
  stop 'cannot form istart in unf90_get_istart'
end subroutine unf90_get_istart
!----------------------------------------
  function unf90_get_var_type(option) result(vartype)
  implicit none
  character(len=*), intent(in) :: option
  integer                      :: vartype
  if(trim(option) == 'float' .or. trim(option) == 'FLOAT') then
    vartype = nf90_float
  else if(trim(option) == 'double' .or. trim(option) == 'DOUBLE') then
    vartype = nf90_double
  else if(trim(option) == 'short' .or. trim(option) == 'SHORT') then
    vartype = nf90_short
  else if(trim(option) == 'char' .or. trim(option) == 'CHAR') then
    vartype = nf90_char
  else if(trim(option) == 'int' .or. trim(option) == 'INT') then
    vartype = nf90_int
  else
    stop 'invalid option in get_var_type'
  endif
  end function unf90_get_var_type
!----------------------------------------
  function unf90_check_var_type(vartype, option) result(flag)
  implicit none
  character(len=*), intent(in) :: option
  integer,          intent(in) :: vartype
  logical                      :: flag
  flag = .false.
  if(trim(option) == 'float' .or. trim(option) == 'FLOAT') then
    if(vartype == nf90_float) flag = .true.
  else if(trim(option) == 'double' .or. trim(option) == 'DOUBLE') then
    if(vartype == nf90_double) flag = .true.
  else if(trim(option) == 'short' .or. trim(option) == 'SHORT') then
    if(vartype == nf90_short) flag = .true.
  else if(trim(option) == 'char' .or. trim(option) == 'CHAR') then
    if(vartype == nf90_char) flag = .true.
  else if(trim(option) == 'int' .or. trim(option) == 'INT') then
    if(vartype == nf90_int) flag = .true.
  else
    stop 'invalid option in get_var_type'
  endif
  end function unf90_check_var_type
!----------------------------------------
  subroutine unf90_def_var_str(ncid, option, dnames, vname, varattname, varatt)
  implicit none
  integer,                    intent(in) :: ncid
  character(len=*),           intent(in) :: option, dnames, vname
  character(len=*), optional, intent(in) :: varattname, varatt
  character(len=256)                     :: tmpvaratt
  integer                                :: i, iret, varid, vartype, dims
  integer                                :: utools_subStrCounts
  character(len=20), allocatable         :: dname(:)
  integer, allocatable                   :: dimid(:)
  dims = utools_subStrCounts(dnames, ',')
  allocate(dname(dims))
  allocate(dimid(dims))
  call utools_split(dnames, dims, dname)
  do i = 1, dims
    call unf90_get_dimid(ncid, dname(i), dimid(i))
  enddo
  vartype = unf90_get_var_type(option)
  iret = nf90_def_var(ncid, trim(vname), vartype, dimid, varid)
  call unf90_check_err(iret)
  if(present(varattname)) then
    tmpvaratt = varatt
    call unf90_io_varatt_char('w', ncid, vname, varattname, tmpvaratt)
  endif
  if(dims > 0) then
    iret = nf90_def_var_deflate(ncid, varid, 1, 1, 5)
    call unf90_check_err(iret)
  endif
  deallocate(dname)
  deallocate(dimid)
  end subroutine unf90_def_var_str
!----------------------------------------
  subroutine unf90_def_var_type(ncid, xtype, dnames, vname, varattname, varatt)
  implicit none
  integer,                    intent(in) :: ncid, xtype
  character(len=*),           intent(in) :: dnames, vname
  character(len=*), optional, intent(in) :: varattname, varatt
  character(len=256)                     :: tmpvaratt
  integer                                :: i, iret, varid, vartype, dims
  integer                                :: utools_subStrCounts
  character(len=20), allocatable         :: dname(:)
  integer, allocatable                   :: dimid(:)
  dims = utools_subStrCounts(dnames, ',')
  allocate(dname(dims))
  allocate(dimid(dims))
  call utools_split(dnames, dims, dname)
  do i = 1, dims
    call unf90_get_dimid(ncid, dname(i), dimid(i))
  enddo
  iret = nf90_def_var(ncid, trim(vname), xtype, dimid, varid)
  call unf90_check_err(iret)
  if(present(varattname)) then
    tmpvaratt = varatt
    call unf90_io_varatt_char('w', ncid, vname, varattname, tmpvaratt)
  endif
  if(dims > 0) then
    iret = nf90_def_var_deflate(ncid, varid, 1, 1, 5)
    call unf90_check_err(iret)
  endif
  deallocate(dname)
  deallocate(dimid)
  end subroutine unf90_def_var_type
!----------------------------------------
  subroutine unf90_def_var_array(ncid, xtype, dims, dnames, vname, varattname, varatt)
  implicit none
  integer,                    intent(in) :: ncid, xtype, dims
  character(len=*),           intent(in) :: dnames(dims), vname
  character(len=*), optional, intent(in) :: varattname, varatt
  character(len=256)                     :: tmpvaratt
  integer                                :: i, iret, varid, vartype
  integer                                :: utools_subStrCounts
  integer, allocatable                   :: dimid(:)
  allocate(dimid(dims))
  do i = 1, dims
    call unf90_get_dimid(ncid, trim(dnames(i)), dimid(i))
  enddo
  iret = nf90_def_var(ncid, trim(vname), xtype, dimid, varid)
  call unf90_check_err(iret)
  if(present(varattname)) then
    tmpvaratt = varatt
    call unf90_io_varatt_char('w', ncid, vname, varattname, tmpvaratt)
  endif
  if(dims > 0) then
    iret = nf90_def_var_deflate(ncid, varid, 1, 1, 5)
    call unf90_check_err(iret)
  endif
  deallocate(dimid)
  end subroutine unf90_def_var_array
!----------------------------------------
  subroutine unf90_io_varatt_char(option, ncid, vname, aname, note)
  implicit none
  integer, intent(in)             :: ncid
  character(len=*), intent(in)    :: option, vname, aname
  character(len=*), intent(inout) :: note
  integer                         :: varid, iret
  if(trim(vname) == 'nf90_global') then
    if(trim(option) == 'r' .or. trim(option) == 'R') then
      iret = nf90_get_att(ncid, nf90_global, trim(aname), note)
    else if(trim(option) == 'w' .or. trim(option) == 'W') then
      iret = nf90_put_att(ncid, nf90_global, trim(aname), note)
    else
      stop 'invalid option'
    endif
  else
    iret = nf90_inq_varid(ncid, trim(vname), varid)
    call unf90_check_err(iret)
    if(trim(option) == 'r' .or. trim(option) == 'R') then
      iret = nf90_get_att(ncid, varid, trim(aname), note)
    else if(trim(option) == 'w' .or. trim(option) == 'W') then
      iret = nf90_put_att(ncid, varid, trim(aname), note)
    else
      stop 'invalid option'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io_varatt_char
!----------------------------------------
  subroutine unf90_out_varatt_char(ncid, vname, aname, note)
  implicit none
  integer, intent(in)          :: ncid
  character(len=*), intent(in) :: vname, aname
  character(len=*), intent(in) :: note
  integer                      :: varid, iret
  if(trim(vname) == 'nf90_global') then
    iret = nf90_put_att(ncid, nf90_global, trim(aname), note)
  else
    iret = nf90_inq_varid(ncid, trim(vname), varid)
    call unf90_check_err(iret)
    iret = nf90_put_att(ncid, varid, trim(aname), note)
  endif
  call unf90_check_err(iret)
  end subroutine unf90_out_varatt_char
!----------------------------------------
  subroutine unf90_io_varatt_int(option, ncid, vname, aname, value)
  implicit none
  integer,          intent(in)    :: ncid
  character(len=*), intent(in)    :: option, vname, aname
  integer,          intent(inout) :: value
  integer                         :: varid, iret
  if(trim(vname) == 'nf90_global') then
    if(trim(option) == 'r') then
      iret = nf90_get_att(ncid, nf90_global, trim(aname), value)
    else if(trim(option) == 'w') then
      iret = nf90_put_att(ncid, nf90_global, trim(aname), value)
    else
      stop 'invalid option'
    endif
  else
    iret = nf90_inq_varid(ncid, trim(vname), varid)
    call unf90_check_err(iret)
    if(trim(option) == 'r') then
      iret = nf90_get_att(ncid, varid, trim(aname), value)
    else if(trim(option) == 'w') then
      iret = nf90_put_att(ncid, varid, trim(aname), value)
    else
      stop 'invalid option'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io_varatt_int
!----------------------------------------
  subroutine unf90_out_varatt_int(ncid, vname, aname, value)
  implicit none
  integer,          intent(in)  :: ncid
  character(len=*), intent(in)  :: vname, aname
  integer,          intent(in)  :: value
  integer                       :: varid, iret
  if(trim(vname) == 'nf90_global') then
    iret = nf90_put_att(ncid, nf90_global, trim(aname), value)
  else
    iret = nf90_inq_varid(ncid, trim(vname), varid)
    call unf90_check_err(iret)
    iret = nf90_put_att(ncid, varid, trim(aname), value)
  endif
  call unf90_check_err(iret)
  end subroutine unf90_out_varatt_int
!----------------------------------------
  subroutine unf90_io_varatt_real(option, ncid, varname, vname, value)
  implicit none
  integer,          intent(in)    :: ncid
  character(len=*), intent(in)    :: option, varname, vname
  real,             intent(inout) :: value
  integer                         :: varid, iret
  if(trim(varname) == 'nf90_global') then
    if(trim(option) == 'r') then
      iret = nf90_get_att(ncid, nf90_global, trim(vname), value)
    else if(trim(option) == 'w') then
      iret = nf90_put_att(ncid, nf90_global, trim(vname), value)
    else
      stop 'invalid option'
    endif
  else
    iret = nf90_inq_varid(ncid, trim(varname), varid)
    call unf90_check_err(iret)
    if(trim(option) == 'r') then
      iret = nf90_get_att(ncid, varid, trim(vname), value)
    else if(trim(option) == 'w') then
      iret = nf90_put_att(ncid, varid, trim(vname), value)
    else
      stop 'invalid option'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io_varatt_real
!----------------------------------------
  subroutine unf90_out_varatt_real(ncid, varname, vname, value)
  implicit none
  integer,          intent(in) :: ncid
  character(len=*), intent(in) :: varname, vname
  real,             intent(in) :: value
  integer                         :: varid, iret
  if(trim(varname) == 'nf90_global') then
    iret = nf90_put_att(ncid, nf90_global, trim(vname), value)
  else
    iret = nf90_inq_varid(ncid, trim(varname), varid)
    call unf90_check_err(iret)
    iret = nf90_put_att(ncid, varid, trim(vname), value)
  endif
  call unf90_check_err(iret)
  end subroutine unf90_out_varatt_real
!----------------------------------------
  subroutine unf90_io_varatt_double(option, ncid, vname, aname, dvalue)
  use netcdf
  implicit none
  integer,          intent(in)    :: ncid
  character(len=*), intent(in)    :: option, vname, aname
  double precision, intent(inout) :: dvalue
  integer                         :: varid, iret
  if(trim(vname) == 'nf90_global') then
    if(trim(option) == 'r') then
      iret = nf90_get_att(ncid, nf90_global, trim(aname), dvalue)
    else if(trim(option) == 'w') then
      iret = nf90_put_att(ncid, nf90_global, trim(aname), dvalue)
    else
      stop 'invalid option'
    endif
  else
    iret = nf90_inq_varid(ncid, trim(vname), varid)
    call unf90_check_err(iret)
    if(trim(option) == 'r') then
      iret = nf90_get_att(ncid, varid, trim(aname), dvalue)
    else if(trim(option) == 'w') then
      iret = nf90_put_att(ncid, varid, trim(aname), dvalue)
    else
      stop 'invalid option'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io_varatt_double
!----------------------------------------
  subroutine unf90_out_varatt_double(ncid, vname, aname, dvalue)
  use netcdf
  implicit none
  integer,          intent(in) :: ncid
  character(len=*), intent(in) :: vname, aname
  double precision, intent(in) :: dvalue
  integer                      :: varid, iret
  if(trim(vname) == 'nf90_global') then
    iret = nf90_put_att(ncid, nf90_global, trim(aname), dvalue)
  else
    iret = nf90_inq_varid(ncid, trim(vname), varid)
    call unf90_check_err(iret)
    iret = nf90_put_att(ncid, varid, trim(aname), dvalue)
  endif
  call unf90_check_err(iret)
  end subroutine unf90_out_varatt_double
!----------------------------------------
  subroutine unf90_def_end(ncid)
  implicit none
  integer, intent(in) :: ncid
  integer             :: iret
  iret = nf90_enddef(ncid)
  call unf90_check_err(iret)
  end subroutine unf90_def_end
!----------------------------------------
  subroutine unf90_io0d_real(mode, ncid, rvalue, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid
  real,             intent(inout) :: rvalue
  character(len=*), intent(in)    :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, rvalue)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, rvalue)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io0d_real
!----------------------------------------
  subroutine unf90_io0d_double(mode, ncid, dvalue, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid
  double precision, intent(inout) :: dvalue
  character(len=*), intent(in)    :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, dvalue)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, dvalue)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io0d_double
!----------------------------------------
  subroutine unf90_io1d_int(mode, ncid, ii, i1d, vname, istart, icount)
  implicit none
  character(len=1),  intent(in)    :: mode
  integer,           intent(in)    :: ncid, ii
  integer,           intent(inout) :: i1d(ii)
  character(len=*),  intent(in)    :: vname
  integer, optional, intent(in)    :: istart(:), icount(:)
  integer                          :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(present(icount)) then
    if(mode == 'r' .or. mode == 'r') then
      iret = nf90_get_var(ncid, varid, i1d, istart, icount)
    else if(mode == 'w' .or. mode == 'w') then
      iret = nf90_put_var(ncid, varid, i1d, istart, icount)
    else
      stop 'invalid mode'
    endif
  else
    if(mode == 'r' .or. mode == 'r') then
      iret = nf90_get_var(ncid, varid, i1d)
    else if(mode == 'w' .or. mode == 'w') then
      iret = nf90_put_var(ncid, varid, i1d)
    else
      stop 'invalid mode'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io1d_int
!----------------------------------------
  subroutine unf90_io1d_real(mode, ncid, ii, r1d, vname, istart, icount)
  implicit none
  character(len=1),  intent(in)    :: mode
  integer,           intent(in)    :: ncid, ii
  real,              intent(inout) :: r1d(ii)
  character(len=*),  intent(in)    :: vname
  integer, optional, intent(in)    :: istart(:), icount(:)
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(present(icount)) then
    if(mode == 'r' .or. mode == 'R') then
      iret = nf90_get_var(ncid, varid, r1d, istart, icount)
    else if(mode == 'w' .or. mode == 'W') then
      iret = nf90_put_var(ncid, varid, r1d, istart, icount)
    else
      stop 'invalid mode'
    endif
  else
    if(mode == 'r' .or. mode == 'R') then
      iret = nf90_get_var(ncid, varid, r1d)
    else if(mode == 'w' .or. mode == 'W') then
      iret = nf90_put_var(ncid, varid, r1d)
    else
      stop 'invalid mode'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io1d_real
!----------------------------------------
  subroutine unf90_io1d_double(mode, ncid, ii, d1d, vname, istart, icount)
  implicit none
  character(len=1),  intent(in)    :: mode
  integer,           intent(in)    :: ncid, ii
  double precision,  intent(inout) :: d1d(ii)
  character(len=*),  intent(in)    :: vname
  integer, optional, intent(in)    :: istart(:), icount(:)
  integer                          :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(present(icount)) then
    if(mode == 'r' .or. mode == 'R') then
      iret = nf90_get_var(ncid, varid, d1d, istart, icount)
    else if(mode == 'w' .or. mode == 'W') then
      iret = nf90_put_var(ncid, varid, d1d, istart, icount)
    else
      stop 'invalid mode'
    endif
  else
    if(mode == 'r' .or. mode == 'R') then
      iret = nf90_get_var(ncid, varid, d1d)
    else if(mode == 'w' .or. mode == 'W') then
      iret = nf90_put_var(ncid, varid, d1d)
    else
      stop 'invalid mode'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io1d_double
!----------------------------------------
  subroutine unf90_io1d_time_real(mode, ncid, lon, tid, r1d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, tid
  real,             intent(inout) :: r1d(lon)
  character(len=*)                :: vname
  integer                         :: iret, varid, istart(2), icount(2)
  istart = 1
  icount = 1
  istart(2) = tid
  icount(1) = lon
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, r1d, istart, icount)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, r1d, istart, icount)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io1d_time_real
!----------------------------------------
  subroutine unf90_io1d_time_double(mode, ncid, lon, tid, d1d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, tid
  double precision, intent(inout) :: d1d(lon)
  character(len=*)                :: vname
  integer                         :: iret, varid, istart(2), icount(2)
  istart = 1
  icount = 1
  istart(2) = tid
  icount(1) = lon
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, d1d, istart, icount)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, d1d, istart, icount)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io1d_time_double
!----------------------------------------
  subroutine unf90_io2d_double(mode, ncid, ii, jj, d2d, vname, istart, icount)
  implicit none
  character(len=1),  intent(in)    :: mode
  integer,           intent(in)    :: ii, jj, ncid
  double precision,  intent(inout) :: d2d(ii,jj)
  character(len=*),  intent(in)    :: vname
  integer, optional, intent(in)    :: istart(:), icount(:)
  integer                          :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, d2d, istart, icount)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, d2d, istart, icount)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io2d_double
!----------------------------------------
  subroutine unf90_io2d_real(mode, ncid, ii, jj, r2d, vname, istart, icount)
  implicit none
  character(len=1),  intent(in)    :: mode
  integer,           intent(in)    :: ncid, ii, jj
  real,              intent(inout) :: r2d(ii,jj)
  character(len=*),  intent(in)    :: vname
  integer, optional, intent(in)    :: istart(:), icount(:)
  integer                          :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, r2d, istart, icount)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, r2d, istart, icount)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io2d_real
!----------------------------------------
  subroutine unf90_rd1d_real_tile2vec(ncid, ii, jj, vec_index, location, r1d, vname)
  implicit none
  integer,          intent(in)  :: ncid, ii, jj, location
  integer,          intent(in)  :: vec_index(ii, jj)
  real,             intent(out) :: r1d(location)
  real,             allocatable :: r2d(:,:)
  character(len=*), intent(in)  :: vname
  integer                       :: iret, varid, i, j, ip
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  allocate(r2d(ii,jj))
  iret = nf90_get_var(ncid, varid, r2d)
  do i = 1, ii
      do j = 1, jj
          ip = vec_index(i,j)
          if(ip > 0) r1d(ip) = r2d(i,j)
      enddo
  enddo
  call unf90_check_err(iret)
  deallocate(r2d)
  end subroutine unf90_rd1d_real_tile2vec
!----------------------------------------
  subroutine unf90_io2d_int(mode, ncid, lon, lat, i2d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, lat
  integer,          intent(inout) :: i2d(lon,lat)
  character(len=*)                :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, i2d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, i2d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io2d_int
!----------------------------------------
  subroutine unf90_io2d_time_real(mode, ncid, lon, lat, tid, r2d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, lat, tid
  real,             intent(inout) :: r2d(lon, lat)
  character(len=*), intent(in)    :: vname
  integer                         :: iret, varid, istart(3), icount(3)
  istart = 1
  icount = 1
  istart(3) = tid
  icount(1) = lon
  icount(2) = lat
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, r2d, istart, icount)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, r2d, istart, icount)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io2d_time_real
!----------------------------------------
  subroutine unf90_io2d_time_double(mode, ncid, lon, lat, tid, d2d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, lat, tid
  double precision, intent(inout) :: d2d(lon, lat)
  character(len=*), intent(in)    :: vname
  integer                         :: iret, varid, istart(3), icount(3)
  istart = 1
  icount = 1
  istart(3) = tid
  icount(1) = lon
  icount(2) = lat
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, d2d, istart, icount)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, d2d, istart, icount)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io2d_time_double
!----------------------------------------
  subroutine unf90_io3d_int(mode, ncid, lon, lat, lev, i3d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, lat, lev
  integer,          intent(inout) :: i3d(lon,lat,lev)
  character(len=*)                :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, i3d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, i3d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io3d_int
!----------------------------------------
  subroutine unf90_io3d_real(mode, ncid, lon, lat, lev, r3d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, lat, lev
  real,             intent(inout) :: r3d(lon,lat,lev)
  character(len=*)                :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, r3d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, r3d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io3d_real
!----------------------------------------
  subroutine unf90_rd2d_real_tile2vec(ncid, ii, jj, kk, vec_index, location, r2d, vname)
  implicit none
  integer,          intent(in)  :: ncid, ii, jj, kk, location
  integer,          intent(in)  :: vec_index(ii, jj)
  real,             intent(out) :: r2d(location,kk)
  real,             allocatable :: r3d(:,:,:)
  character(len=*), intent(in)  :: vname
  integer                       :: iret, varid, i, j, k, ip
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  allocate(r3d(ii,jj,kk))
  iret = nf90_get_var(ncid, varid, r3d)
  do i = 1, ii
    do j = 1, jj
      ip = vec_index(i,j)
      do k = 1, kk
        if(ip > 0) r2d(ip,k) = r3d(i,j,k)
      enddo
    enddo
  enddo
  call unf90_check_err(iret)
  deallocate(r3d)
  end subroutine unf90_rd2d_real_tile2vec
!----------------------------------------
  subroutine unf90_io3d_double(mode, ncid, lon, lat, lev, d3d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, lat, lev
  double precision, intent(inout) :: d3d(lon,lat,lev)
  character(len=*)                :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, d3d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, d3d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io3d_double
!----------------------------------------
  subroutine unf90_io4d_real(mode, ncid, xds, yds, zds, tds, r4d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, xds, yds, zds, tds
  real,             intent(inout) :: r4d(xds,yds,zds,tds)
  character(len=*)                :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, r4d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, r4d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io4d_real
!----------------------------------------
  subroutine unf90_rd3d_real_tile2vec(ncid, ii, jj, kk, mm, vec_index, location, r3d, vname)
  implicit none
  integer,          intent(in)  :: ncid, ii, jj, kk, mm, location
  integer,          intent(in)  :: vec_index(ii, jj)
  real,             intent(out) :: r3d(location,kk,mm)
  real,             allocatable :: r4d(:,:,:,:)
  character(len=*), intent(in)  :: vname
  integer                       :: iret, varid, i, j, k, m, ip
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  allocate(r4d(ii,jj,kk,mm))
  iret = nf90_get_var(ncid, varid, r4d)
  do i = 1, ii
    do j = 1, jj
      ip = vec_index(i,j)
      do k = 1, kk
        do m = 1, mm
          if(ip > 0) r3d(ip,k,m) = r4d(i,j,k,m)
        enddo
      enddo
    enddo
  enddo
  call unf90_check_err(iret)
  deallocate(r4d)
  end subroutine unf90_rd3d_real_tile2vec
!----------------------------------------
  subroutine unf90_io4d_double(mode, ncid, xds, yds, zds, tds, d4d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, xds, yds, zds, tds
  double precision, intent(inout) :: d4d(xds,yds,zds,tds)
  character(len=*)                :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, d4d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, d4d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io4d_double
!-------------------------------------------
  subroutine unf90_close_ncfile(ncid)
  implicit none
  integer, intent(in) :: ncid
  integer             :: iret
  iret = nf90_close(ncid)
  call unf90_check_err(iret)
  end subroutine unf90_close_ncfile
!-------------------------------------------
  subroutine unf90_check_err(iret, nonstop)
  implicit none
  integer,           intent(in) :: iret
  logical, optional, intent(in) :: nonstop
    if(iret /= nf90_noerr) then
      print*, nf90_strerror(iret)
      if(present(nonstop) .and. nonstop) return
      stop
    endif
  end subroutine unf90_check_err
end module module_nf90_utilities
!----------------------------------------
subroutine utools_split(line, dlen, str)
  implicit none
  integer, intent(in)           :: dlen
  character(len=*), intent(in)  :: line
  character(len=*), intent(out) :: str(dlen)
  read(line,*) str(1:dlen)
end subroutine utools_split
!----------------------------------------
function utools_subStrCounts(str, delim)
  implicit none
  character(len=*), intent(in) :: str, delim
  integer                      :: i, utools_subStrCounts
  utools_subStrCounts = COUNT([(str(i:i),i=1,len_trim(str))] .eq. trim(delim)) + 1
end function utools_subStrCounts
