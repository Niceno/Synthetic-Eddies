!==============================================================================!
  module Sem_Mod
!------------------------------------------------------------------------------!
!   Module for Synthetic Eddy Method inflow generator                                            !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Mesh_Mod
  use Eddy_Mod
  use Prof_Mod
  use Var_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  integer :: out_num
  real    :: dt, sigma, v_b

  type(Eddy_Type) :: eddy

  type(Prof_Type) :: prof

  type(Mesh_Type) :: mesh

  ! Unsteady computed quantities
  type(Var_Type)  :: u, v, w, t

  ! Time averaged quantities
  type(Var_Type)  :: u_avg,  v_avg,  w_avg,  t_avg
  type(Var_Type)  :: uu_avg, vv_avg, ww_avg
  type(Var_Type)  :: uv_avg, uw_avg, vw_avg
  type(Var_Type)  :: tt_avg, ut_avg, vt_avg, wt_avg

  contains

  include 'Sem_Mod/Cholesky.f90'
  include 'Sem_Mod/Intensity_Det.f90'
  include 'Sem_Mod/Mat_Mul.f90'

  end module
