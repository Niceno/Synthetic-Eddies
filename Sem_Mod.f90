!==============================================================================!
  module Sem_Mod
!------------------------------------------------------------------------------!
!   Module for Synthetic Eddy Method inflow generator                                            !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Mesh_Mod
  use Eddy_Mod
  use Var_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  integer :: n_eddies, n_dt, out_num
  real    :: dt, sigma, v_b, time, eps

  real, allocatable :: u_pr(:,:), rms_pr(:,:), u_c(:,:)
  real, allocatable :: rs(:,:,:), ths(:,:,:)

  type(Eddy_Type), allocatable :: eddy(:)
  type(Mesh_Type) :: mesh

  type(Var_Type) :: u, v, w, t

  contains

  include 'Sem_Mod/Cholesky.f90'
  include 'Sem_Mod/Intensity_Det.f90'
  include 'Sem_Mod/Mat_Mul.f90'

  end module
