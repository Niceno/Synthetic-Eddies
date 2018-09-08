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

  n_dt  = 6000
  dt    = 5e-3
  sigma = 0.20

  out_num = 50

  eps = 1e-8

  !------------------------!
  !   Allocate variables   !
  !------------------------!
  mesh % ny       =   96
  mesh % nz       =  256
  mesh % n_nodes  =  mesh % ny * mesh % nz
  mesh % n_cells  = (mesh % ny - 1)  &
                  * (mesh % nz - 1)
  allocate(mesh % y(mesh % ny))
  allocate(mesh % z(mesh % nz))
  mesh % y(:) = 0.0
  mesh % z(:) = 0.0

  call Var_Mod_Allocate(u, mesh)
  call Var_Mod_Allocate(v, mesh)
  call Var_Mod_Allocate(w, mesh)
  call Var_Mod_Allocate(t, mesh)

  call Prof_Mod_Allocate(prof, mesh % ny)

  allocate( u_c   (mesh % ny, mesh % nz) )
  allocate( eddy(1:n_eddies),  &
            u_pr(4,1:mesh % ny),   &
            rms_pr(8,1:mesh % ny) )

  !------------------------!
  !   Initial conditions   !
  !------------------------!
  u_c(:,:)   = 0.0
  u_pr  (1:4,:)   = 0.0
  rms_pr(1:8,:) = 0.0

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
