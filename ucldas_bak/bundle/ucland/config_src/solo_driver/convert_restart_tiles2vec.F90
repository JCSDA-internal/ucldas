program convert_restart_tiles2vec
  use module_nf90_utilities
  integer,            allocatable :: vector_index(:,:,:), vdimids(:)
  integer,            allocatable :: vdims(:), found(:), dLen(:)
  character(len=128), allocatable :: vnames(:), vdname(:)
  real,               allocatable :: r1d(:), r2d(:,:), r3d(:,:,:)
  integer                         :: location, nx, ny, nk, nm, tiles
  integer                         :: incid, oncid, nDims, nVars, nAtts, uDimId
  integer                         :: iret, xtype, vid, dimid_x, dimid_y
  logical                         :: error
  character(len=256)              :: ifname, ofname, dname_in, dname_out, attname, vdnames
  character(len=256)              :: fname, tfname, exefile, script
  character(len=1)                :: ctile
  integer                         :: tile
! read in command line arguments
  call getarg(0, exefile)
  iargs = iargc()
  error = .false.
  if(iargs == 2) then
      call getarg(1, fname)
      call getarg(2, ofname)
      script = trim(exefile)//' '//trim(fname)//' '//trim(ofname)
  else
      error = .true.
  endif
  if(error) then
      write(*,*) 'Usage: '//trim(exefile)//' input_file_name output_file_name'
      stop
  else
      write(*,'(A168)') '  Run the program: '//trim(script)
      write(*,'(A168)') '    Input file:  '//trim(fname)
      write(*,'(A168)') '    Output file: '//trim(ofname)
  endif
! read in the transformation matrix
  ifname = '/scratch1/NCEPDEV/da/Zhichang.Guo/ucldas_build/ufs-land_C96_static_vec2tiles.nc4'
  call unf90_op_ncfile('R', ifname, incid)
  call unf90_get_dimlen(incid, 'fxdim',    nx)
  call unf90_get_dimlen(incid, 'fydim',    ny)
  call unf90_get_dimlen(incid, 'ntile',    tiles)
  call unf90_get_dimlen(incid, 'location', location)
  allocate(vector_index(nx,ny,tiles))
  call unf90_io_var('r', incid, nx, ny, tiles, vector_index, 'vector_index')
  call unf90_close_ncfile(incid)
!
! ifname = '/scratch1/NCEPDEV/da/Azadeh.Gholoubi/GlobalLand/ufs-land-driver/run/restart/ufs_land_output.2000-01-01_00-00-00.nc'
  call unf90_op_ncfile('C', ofname, oncid, 'netcdf4')
  tfname = trim(fname)//'tile1.nc4'
  call unf90_op_ncfile('r', tfname, incid)
  call unf90_get_ncinfo(incid, nDimensions=nDims, nVariables=nVars, nAttributes=nAtts, unlimitedDimID=uDimId)
  allocate(vnames(nvars))
  allocate(vdims(nvars))
  allocate(found(nvars))
  allocate(dLen(nDims))
! define dimensions
  do iv = 1, nDims
    iret = nf90_inquire_dimension(incid, iv, name=dname_in, len=dLen(iv))
    call unf90_check_err(iret)
    if(trim(dname_in) == 'xaxis_1' .or. trim(dname_in) == 'yaxis_1') then
      if(trim(dname_in) == 'xaxis_1') then
        dimid_x = iv
        call unf90_def_dim(oncid, location, 'location')
      else if(trim(dname_in) == 'yaxis_1') then
        dimid_y = iv
      endif
    else
      dname_out = dname_in
      if(iv == uDimId) then
        call unf90_def_dim(oncid, 0, dname_out)
      else
        call unf90_def_dim(oncid, dlen(iv), dname_out)
      endif
    endif
  enddo
! define the variables and copy their attributes
  do iv = 1, nVars
    call unf90_get_var_info(incid, iv, vnames(iv), xtype, vdims(iv), nAtts)
    allocate(vdimids(vdims(iv)))
    allocate(vdname(vdims(iv)))
    call unf90_get_var_dimids(incid, iv, vdims(iv), vdimids)
    vdnames = ''
    found(iv) = 0
    do vid = 1, vdims(iv)
      call unf90_get_dimname(incid, vdimids(vid), dname=vdname(vid))
      if(vdimids(vid) == dimid_x .or. vdimids(vid) == dimid_y) then
        found(iv) = 1
        if(vdimids(vid) == dimid_x) then
          if(vid==1) then
            vdnames = 'location'
          else
            vdnames = trim(vdnames)//',location'
          endif
        endif
      else
        if(vid==1) then
          vdnames = trim(vdname(vid))
        else
          vdnames = trim(vdnames)//','//trim(vdname(vid))
        endif
      endif
    enddo
    call unf90_def_var(oncid, xtype, vdnames, vnames(iv))
    call unf90_copy_var_att(incid, vnames(iv), oncid, vnames(iv))
    deallocate(vdimids)
    deallocate(vdname)
  enddo
! copy global attributes
  call unf90_copy_global_att(incid, oncid)
  call unf90_def_end(oncid)
  call unf90_close_ncfile(incid)
!
  do iv = 1, nvars
    tfname = trim(fname)//'tile1.nc4'
    call unf90_op_ncfile('r', tfname, incid)
    call unf90_get_var_dims(incid, vnames(iv), vdims(iv))
    if(found(iv) == 1) then
      allocate(vdimids(vdims(iv)))
      call unf90_get_var_dimids(incid, iv, vdims(iv), vdimids)
      call unf90_close_ncfile(incid)
      if(vdims(iv) == 2) then
        allocate(r1d(location))
        do tile = 1, tiles
          write(ctile,'(i1)') tile
          tfname = trim(fname)//'tile'//ctile//'.nc4'
          call unf90_op_ncfile('r', tfname, incid)
          call unf90_rd_tile2vec(incid, nx, ny, vector_index(:,:,tile), location, r1d, vnames(iv))
          call unf90_io_var('w', oncid, location, r1d, vnames(iv))
          call unf90_close_ncfile(incid)
        enddo
        deallocate(r1d)
      else if(vdims(iv) == 3) then
        nk = dLen(vdimids(3))
        allocate(r2d(location,nk))
        do tile = 1, tiles
          write(ctile,'(i1)') tile
          tfname = trim(fname)//'tile'//ctile//'.nc4'
          call unf90_op_ncfile('r', tfname, incid)
          call unf90_rd_tile2vec(incid, nx, ny, nk, vector_index(:,:,tile), location, r2d, vnames(iv))
          call unf90_io_var('w', oncid, location, nk, r2d, vnames(iv))
          call unf90_close_ncfile(incid)
        enddo
        deallocate(r2d)
      else if(vdims(iv) == 4) then
        nk = dLen(vdimids(3))
        nm = dLen(vdimids(4))
        allocate(r3d(location,nk,nm))
        do tile = 1, tiles
          write(ctile,'(i1)') tile
          tfname = trim(fname)//'tile'//ctile//'.nc4'
          call unf90_op_ncfile('r', tfname, incid)
          call unf90_rd_tile2vec(incid, nx, ny, nk, nm, vector_index(:,:,tile), location, r3d, vnames(iv))
          call unf90_io_var('w', oncid, location, nk, nm, r3d, vnames(iv))
          call unf90_close_ncfile(incid)
        enddo
        deallocate(r3d)
      else
        stop 'invalid dimension in convert_restart_tiles2vec'
      endif
      deallocate(vdimids)
    else
      call unf90_copy_var(incid, oncid, vnames(iv))
      call unf90_close_ncfile(incid)
    endif
  enddo
  call unf90_close_ncfile(oncid)
  deallocate(dLen)
  deallocate(found)
  deallocate(vdims)
  deallocate(vnames)
  deallocate(vector_index)
  write(*,*) '  The program: '//trim(script)//' is done normally!'
end program convert_restart_tiles2vec
