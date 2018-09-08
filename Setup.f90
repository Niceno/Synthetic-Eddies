!==============================================================================!
  subroutine Setup()
!----------------------------------[Modules]-----------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: i,j,k
!==============================================================================!

  !-----------------------!
  !   Constants for sem   !
  !-----------------------!
  n_eddies = 1024
  ny       =   96
  nz       =  256

  n_dt  = 6000
  dt    = 5e-3
  sigma = 0.20

  out_num = 100

  eps = 1e-8

  !------------------------!
  !   Allocate variables   !
  !------------------------!
  allocate( y     (ny)    )
  allocate( z        (nz) )
  allocate( u     (ny,nz) )
  allocate( v     (ny,nz) )
  allocate( w     (ny,nz) )
  allocate( t     (ny,nz) )
  allocate( u_in  (ny,nz) )
  allocate( v_in  (ny,nz) )
  allocate( w_in  (ny,nz) )
  allocate( u_comb(ny,nz) )
  allocate( v_comb(ny,nz) )
  allocate( w_comb(ny,nz) )
  allocate( t_in  (ny,nz) )
  allocate( t_comb(ny,nz) )
  allocate( rs  (6,ny,nz) )
  allocate( ths (4,ny,nz) )
  allocate( u_c   (ny,nz) )
  allocate( eddy(1:n_eddies),  &
            u_pr(4,1:ny),   &
            rms_pr(8,1:ny) )
!@allocate(u3d_comb(n_dt, ny, nz))
!@allocate(v3d_comb(n_dt, ny, nz))
!@allocate(w3d_comb(n_dt, ny, nz))
!@allocate(t3d_comb(n_dt, ny, nz))

  !------------------------!
  !   Initial conditions   !
  !------------------------!
  y(:) = 0.0
  z(:) = 0.0

  u(:,:) = 0.0
  v(:,:) = 0.0
  w(:,:) = 0.0
  t(:,:) = 0.0

  rs (1:6,:,:) = 0.0
  ths(1:4,:,:) = 0.0

  u_in(:,:) = 0.0
  v_in(:,:) = 0.0
  w_in(:,:) = 0.0
  t_in(:,:) = 0.0

  u_comb(:,:) = 0.0
  v_comb(:,:) = 0.0
  w_comb(:,:) = 0.0
  t_comb(:,:) = 0.0

!@u3d_comb(:,:,:) = 0.0
!@v3d_comb(:,:,:) = 0.0
!@w3d_comb(:,:,:) = 0.0
!@t3d_comb(:,:,:) = 0.0

  u_c(1:ny,1:nz)   = 0.0
  u_pr(1:4,1:ny)   = 0.0
  rms_pr(1:8,1:ny) = 0.0

  eddy(:) % num = 0
  eddy(:) % len   = 0.0
  eddy(:) % x     = 0.0
  eddy(:) % y     = 0.0
  eddy(:) % z     = 0.0
  eddy(:) % x_int = 0.0
  eddy(:) % y_int = 0.0
  eddy(:) % z_int = 0.0
  eddy(:) % t_int = 0.0

  end subroutine
