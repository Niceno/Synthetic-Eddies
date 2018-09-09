!==============================================================================!
  subroutine Initialize()
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
  dt    = 5e-3
  sigma = 0.20

  out_num = 50

  !----------!
  !   Mesh   !
  !----------!
  mesh % ny       =   96
  mesh % nz       =  256
  mesh % n_nodes  =  mesh % ny * mesh % nz
  mesh % n_cells  = (mesh % ny - 1)  &
                  * (mesh % nz - 1)
  allocate(mesh % y(mesh % ny))
  allocate(mesh % z(mesh % nz))
  mesh % y(:) = 0.0
  do k = 1, mesh % nz
    mesh % z(k) =   0.5 * 6.0 / mesh % nz  &
                + (k-1) * 6.0 / mesh % nz
  end do

  !---------------------------------------!
  !   Variables holding unsteady values   !
  !---------------------------------------!
  call Var_Mod_Allocate(u, mesh)
  call Var_Mod_Allocate(v, mesh)
  call Var_Mod_Allocate(w, mesh)
  call Var_Mod_Allocate(t, mesh)

  !--------------------------------------------!
  !   Variables holding time-averaged values   !
  !--------------------------------------------!
  call Var_Mod_Allocate(u_avg, mesh)
  call Var_Mod_Allocate(v_avg, mesh)
  call Var_Mod_Allocate(w_avg, mesh)
  call Var_Mod_Allocate(t_avg, mesh)

  call Var_Mod_Allocate(uu_avg, mesh)
  call Var_Mod_Allocate(vv_avg, mesh)
  call Var_Mod_Allocate(ww_avg, mesh)
  call Var_Mod_Allocate(uv_avg, mesh)
  call Var_Mod_Allocate(uw_avg, mesh)
  call Var_Mod_Allocate(vw_avg, mesh)

  call Var_Mod_Allocate(tt_avg, mesh)
  call Var_Mod_Allocate(ut_avg, mesh)
  call Var_Mod_Allocate(vt_avg, mesh)
  call Var_Mod_Allocate(wt_avg, mesh)

  !-------------!
  !   Profile   !
  !-------------!
  call Prof_Mod_Allocate(prof, mesh % ny)

  !------------!
  !   Eddies   !
  !------------!
  call Eddy_Mod_Allocate(eddy, 1024)

  end subroutine
