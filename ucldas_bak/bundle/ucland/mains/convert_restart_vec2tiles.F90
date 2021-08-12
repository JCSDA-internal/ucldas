program convert_restart_vec2tiles
  use module_nf90_utilities
  real,                 parameter :: rfillValue = 9.96921e+36
  double precision,     parameter :: dfillValue = 9.96921d+36
  integer,            allocatable :: cube_i(:), cube_j(:), cube_tile(:), vdimids(:)
  character(len=128), allocatable :: vnames(:), vdname(:)
  integer                         :: location, nx, ny, tiles
  integer                         :: incid, oncid, nDims, nVars, nAtts, uDimId
  integer                         :: iret, dlen, xtype, vdims, vid, dimid_location
  logical                         :: found, error
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
      write(*,'(A168)') '    Output file: '//fname(1:len_trim(fname)-3)//'tile?.nc4'
  endif
! read in the transformation matrix
  ifname = '/scratch1/NCEPDEV/da/Zhichang.Guo/ucldas_build/ufs-land_C96_static_vec2tiles.nc4'
  call unf90_op_ncfile('R', ifname, incid)
  call unf90_get_dimlen(incid, 'fxdim',    nx)
  call unf90_get_dimlen(incid, 'fydim',    ny)
  call unf90_get_dimlen(incid, 'ntile',    tiles)
  call unf90_get_dimlen(incid, 'location', location)
  allocate(cube_i(location))
  allocate(cube_j(location))
  allocate(cube_tile(location))
  call unf90_io_var('r', incid, location, cube_i,    'cube_i')
  call unf90_io_var('r', incid, location, cube_j,    'cube_j')
  call unf90_io_var('r', incid, location, cube_tile, 'cube_tile')
  call unf90_close_ncfile(incid)
!
! ifname = '/scratch1/NCEPDEV/da/Azadeh.Gholoubi/GlobalLand/ufs-land-driver/run/restart/ufs_land_output.2000-01-01_00-00-00.nc'
  ifname = fname
  call unf90_op_ncfile('R', ifname, incid)
  call unf90_get_ncinfo(incid, nDimensions=nDims, nVariables=nVars, nAttributes=nAtts, unlimitedDimID=uDimId)
  allocate(vnames(nvars))
  do tile = 1, tiles
      write(ctile,'(i1)') tile
      tfname = trim(ofname)//'tile'//ctile//'.nc4'
      call unf90_op_ncfile('C', tfname, oncid, 'netcdf4')
!     define dimensions
      do iv = 1, nDims
          iret = nf90_inquire_dimension(incid, iv, name=dname_in, len=dlen)
          call unf90_check_err(iret)
          if(trim(dname_in) == 'location') then
              if(dlen /= location) stop 'dimension does not match'
              call unf90_def_dim(oncid, nx, 'xaxis_1')
              call unf90_def_dim(oncid, ny, 'yaxis_1')
              dimid_location = iv
          else
              dname_out = dname_in
              if(iv == uDimId) then
                  call unf90_def_dim(oncid, 0, dname_out)
              else
                  call unf90_def_dim(oncid, dlen, dname_out)
              endif
          endif
      enddo
!     define the variables and copy their attributes
      do iv = 1, nVars
          call unf90_get_var_info(incid, iv, vnames(iv), xtype, vdims, nAtts)
          allocate(vdimids(vdims))
          allocate(vdname(vdims))
          call unf90_get_var_dimids(incid, iv, vdims, vdimids)
          vdnames = ''
          found = .false.
          do vid = 1, vdims
              call unf90_get_dimname(incid, vdimids(vid), dname=vdname(vid))
              if(vdimids(vid) == dimid_location) then
                  found = .true.
                  if(vid==1) then
                      vdnames = 'xaxis_1,yaxis_1'
                  else
                      vdnames = trim(vdnames)//',xaxis_1,yaxis_1'
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
          if(found) then
              if(unf90_check_var_type(xtype, 'float')) then
                  call unf90_out_varatt(oncid, vnames(iv), '_FillValue', rfillValue) 
              else if(unf90_check_var_type(xtype, 'double')) then
                  call unf90_out_varatt(oncid, vnames(iv), '_FillValue', dfillValue) 
              else
                  stop 'invalid xtype in convert_restart_vec2tiles'
              endif
          endif
          deallocate(vdimids)
          deallocate(vdname)
      enddo
!     copy global attributes
      call unf90_copy_global_att(incid, oncid)
      call unf90_def_end(oncid)
!     convert variables from the vectorized format to tile format and output to files
      do iv = 1, nvars
          call unf90_get_var_dims(incid, vnames(iv), vdims)
          allocate(vdimids(vdims))
          call unf90_get_var_dimids(incid, iv, vdims, vdimids)
          found = false
          do vid = 1, vdims
              if(vdimids(vid) == dimid_location) found = .true.
          enddo
          if(found) then
              call unf90_copy_var_vec2tile(incid, oncid, vnames(iv), location, cube_i, cube_j, cube_tile, nx, ny, tile, dfillValue)
          else 
              call unf90_copy_var(incid, oncid, vnames(iv))
          endif
          deallocate(vdimids)
      enddo
      call unf90_close_ncfile(oncid)
  enddo
  deallocate(vnames)
  deallocate(cube_i)
  deallocate(cube_j)
  deallocate(cube_tile)
  call unf90_close_ncfile(incid)
  write(*,*) '  The program: '//trim(script)//' is done normally!'
end program convert_restart_vec2tiles
