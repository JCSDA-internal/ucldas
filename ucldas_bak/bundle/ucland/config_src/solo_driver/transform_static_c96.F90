program transform_static_c96
use module_nf90_utilities
implicit none
  integer, parameter :: location   = 16196
  integer, parameter :: nx         = 96
  integer, parameter :: ny         = 96
  integer, parameter :: tiles      = 6
  real               :: rfillValue = 9.96921e+36
  integer            :: ifillValue = -2147483647
  integer            :: ifillMask  = 0
  integer            :: incid, oncid
  integer            :: cube_i(location), cube_j(location), cube_tile(location)
  integer            :: land_mask(nx,ny,tiles), vector_index(nx,ny,tiles)
  double precision   :: flon(nx,ny,tiles), flat(nx,ny,tiles), pi
  real               :: glon(nx,ny,tiles), glat(nx,ny,tiles), orog_raw(nx,ny,tiles)
  real               :: land_frac(nx,ny,tiles), slmsk(nx,ny,tiles)
  integer            :: i, j, tile, ip
  character(len=256) :: ifname, ofname
  character(len=1)   :: ctile
  do tile = 1, tiles
    write(ctile,'(i1)') tile
    ifname = '/scratch1/NCEPDEV/global/Helin.Wei/save/git/feature_ccpp/sorc/ufs_utils.fd/fix/fix_fv3_gmted2010/C96/C96_oro_data.tile'//ctile//'.nc'
    call unf90_op_ncfile('R', ifname, incid)
    call unf90_io_var('r', incid, nx, ny, glon(:,:,tile),      'geolon')
    call unf90_io_var('r', incid, nx, ny, glat(:,:,tile),      'geolat')
    call unf90_io_var('r', incid, nx, ny, land_frac(:,:,tile), 'land_frac')
    call unf90_io_var('r', incid, nx, ny, orog_raw(:,:,tile),  'orog_raw')
    call unf90_io_var('r', incid, nx, ny, slmsk(:,:,tile),     'slmsk')
    call unf90_close_ncfile(incid)
  enddo
  ifname = '/scratch1/NCEPDEV/stmp2/Michael.Barlage/forcing/gswp3/static/ufs-land_C96_static_fields.nc'
  ofname = 'ufs-land_C96_static_vec2tiles.nc4'
  pi = 4.*atan(1.d0)
  call unf90_op_ncfile('R', ifname, incid)
  call unf90_op_ncfile('C', ofname, oncid)
  call unf90_def_dim(oncid, nx,       'fxdim')
  call unf90_def_dim(oncid, ny,       'fydim')
  call unf90_def_dim(oncid, tiles,    'ntile')
  call unf90_def_dim(oncid, location, 'location')
  call unf90_def_var(oncid, 'int', 'location', 'cube_i')
  call unf90_out_varatt(oncid, 'cube_i', '_FillValue', ifillValue)
  call unf90_def_var(oncid, 'int', 'location', 'cube_j')
  call unf90_out_varatt(oncid, 'cube_j', '_FillValue', ifillValue)
  call unf90_def_var(oncid, 'int', 'location', 'cube_tile')
  call unf90_out_varatt(oncid, 'cube_tile', '_FillValue', ifillValue)
  call unf90_def_var(oncid, 'float', 'location', 'longitude')
  call unf90_out_varatt(oncid, 'longitude', '_FillValue', rfillValue)
  call unf90_def_var(oncid, 'float', 'location', 'latitude')
  call unf90_out_varatt(oncid, 'latitude', '_FillValue', rfillValue)
  call unf90_def_var(oncid, 'int', 'fxdim,fydim,ntile', 'land_mask')
  call unf90_out_varatt(oncid, 'land_mask', '_FillValue', ifillMask)
  call unf90_def_var(oncid, 'int', 'fxdim,fydim,ntile', 'vector_index')
  call unf90_out_varatt(oncid, 'vector_index', '_FillValue', ifillValue)
  call unf90_def_var(oncid, 'float', 'fxdim,fydim,ntile', 'orog_raw')
  call unf90_def_var(oncid, 'float', 'fxdim,fydim,ntile', 'geolon')
  call unf90_out_varatt(oncid, 'geolon', 'units', 'degrees_east')
  call unf90_out_varatt(oncid, 'geolon', '_FillValue', rfillValue)
  call unf90_def_var(oncid, 'float', 'fxdim,fydim,ntile', 'geolat')
  call unf90_out_varatt(oncid, 'geolat', 'units', 'degrees_north')
  call unf90_out_varatt(oncid, 'geolat', '_FillValue', rfillValue)
  call unf90_def_var(oncid, 'double', 'fxdim,fydim,ntile', 'flons')
  call unf90_out_varatt(oncid, 'flons', 'long_name', 'longitude of faces')
  call unf90_def_var(oncid, 'double', 'fxdim,fydim,ntile', 'flats')
  call unf90_out_varatt(oncid, 'flats', 'long_name', 'latitude of faces')
  call unf90_def_var(oncid, 'float', 'fxdim,fydim,ntile', 'land_frac')
  call unf90_def_var(oncid, 'float', 'fxdim,fydim,ntile', 'slmsk')
  call unf90_def_end(oncid)
  call unf90_io_var('r', incid, location, cube_i,    'cube_i')
  call unf90_io_var('r', incid, location, cube_j,    'cube_j')
  call unf90_io_var('r', incid, location, cube_tile, 'cube_tile')
  call unf90_copy_var(incid, oncid, 'longitude')
  call unf90_copy_var(incid, oncid, 'latitude')
  do i = 1, nx
    do j = 1, ny
      do tile = 1, tiles
        land_mask(i,j,tile) = 0
        vector_index(i,j,tile) = -1
        flon(i,j,tile) = glon(i,j,tile)*pi/180.d0
        flat(i,j,tile) = glat(i,j,tile)*pi/180.d0
      enddo
    enddo
  enddo
  do ip = 1, location
    land_mask(cube_j(ip),cube_i(ip),cube_tile(ip)) = 1
    vector_index(cube_j(ip),cube_i(ip),cube_tile(ip)) = ip
  enddo
  call unf90_io1d_int('w', oncid, location, cube_j,    'cube_i')
  call unf90_io1d_int('w', oncid, location, cube_i,    'cube_j')
  call unf90_io1d_int('w', oncid, location, cube_tile, 'cube_tile')
  call unf90_io_var('w', oncid, nx, ny, tiles, land_mask,    'land_mask') 
  call unf90_io_var('w', oncid, nx, ny, tiles, vector_index, 'vector_index') 
  call unf90_io_var('w', oncid, nx, ny, tiles, flon, 'flons') 
  call unf90_io_var('w', oncid, nx, ny, tiles, flat, 'flats') 
  call unf90_io_var('w', oncid, nx, ny, tiles, glon, 'geolon') 
  call unf90_io_var('w', oncid, nx, ny, tiles, glat, 'geolat') 
  call unf90_io_var('w', oncid, nx, ny, tiles, land_frac, 'land_frac') 
  call unf90_io_var('w', oncid, nx, ny, tiles, orog_raw,  'orog_raw') 
  call unf90_io_var('w', oncid, nx, ny, tiles, slmsk,  'slmsk') 
  call unf90_close_ncfile(incid)
  call unf90_close_ncfile(oncid)
end program transform_static_c96
