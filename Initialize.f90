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
  call Mesh_Mod_Create_Cartesian(mesh, 96, 256)

  !---------------------------------------!
  !   Variables holding unsteady values   !
  !---------------------------------------!
  call Var_Mod_Create(u, mesh)
  call Var_Mod_Create(v, mesh)
  call Var_Mod_Create(w, mesh)
  call Var_Mod_Create(t, mesh)

  !--------------------------------------------!
  !   Variables holding time-averaged values   !
  !--------------------------------------------!
  call Var_Mod_Create(u_avg, mesh)
  call Var_Mod_Create(v_avg, mesh)
  call Var_Mod_Create(w_avg, mesh)
  call Var_Mod_Create(t_avg, mesh)

  call Var_Mod_Create(uu_avg, mesh)
  call Var_Mod_Create(vv_avg, mesh)
  call Var_Mod_Create(ww_avg, mesh)
  call Var_Mod_Create(uv_avg, mesh)
  call Var_Mod_Create(uw_avg, mesh)
  call Var_Mod_Create(vw_avg, mesh)

  call Var_Mod_Create(tt_avg, mesh)
  call Var_Mod_Create(ut_avg, mesh)
  call Var_Mod_Create(vt_avg, mesh)
  call Var_Mod_Create(wt_avg, mesh)

  !-------------!
  !   Profile   !
  !-------------!
  call Prof_Mod_Create(prof, mesh % ny)

  !------------!
  !   Eddies   !
  !------------!
  call Eddy_Mod_Create(eddy, 1024)

  end subroutine
