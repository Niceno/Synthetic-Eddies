!==============================================================================!
  module Sem_Mod
!------------------------------------------------------------------------------!
!   Module for SEM inflow generator                                            !
!                                                             2017.03.02 k.noh !
!   Variables : dt    : time step                                              !
!               n     : the number of eddies                                   !
!               sigma : eddy length scale                                      !
!               v_b   : volume of box including eddies                         !
!               nt    : the number of iterations                               !
!                                                                              !
!               y,z      : y,z coordinates                                     !
!               u,v,w    : mean velocity arrays                                !
!               t        : mean temperature arrays                             !
!               rs       : reynolds stress                                     !
!               sem_eddy : each eddies properties including positions,         !
!                          intensities, length scales.                         !
!               u,v,w,t_in : stochastic components of inflow surface           !
!               u,v,w,t_comb  : reconstructed components of inflow             !
!                                                                              !
!               u_c    : local convection velocities                           !
!               u_pr   : mean profiles (u,v,w,t)                               !
!               rms_pr : reynolds stress profiles (uu,vv,ww,tt,uv,ut,vt,wt)    !
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !---------------!
  !               !
  !   Eddy type   !
  !               !
  !---------------!
  type Eddy_Type
    integer :: num    ! eddy specification number
    real    :: len    ! eddy length scale
    real    :: x      ! eddy's x position
    real    :: y      ! eddy's y position
    real    :: z      ! eddy's z position
    real    :: x_int  ! eddy's x intensity
    real    :: y_int  ! eddy's y intensity
    real    :: z_int  ! eddy's z intensity
    real    :: t_int  ! eddy's t intensity
  end type

  integer           :: n_eddies, ny, nz, n_dt, out_num
  real              :: dt, sigma, v_b, time, eps

  real, allocatable :: y(:), z(:)
  real, allocatable :: u(:,:), v(:,:), w(:,:), t(:,:),       &
                       u_in(:,:), v_in(:,:), w_in(:,:),      &
                       u_comb(:,:),v_comb(:,:),w_comb(:,:),  &
                       t_in(:,:), t_comb(:,:),               &
                       u_pr(:,:), rms_pr(:,:), u_c(:,:)
  real, allocatable :: rs(:,:,:), ths(:,:,:)
!@real, allocatable :: u3d_comb(:,:,:), v3d_comb(:,:,:), w3d_comb(:,:,:)
!@real, allocatable :: t3d_comb(:,:,:)

  type(Eddy_Type), allocatable :: eddy(:)

  contains

  include 'Sem_Mod/Cholesky.f90'
  include 'Sem_Mod/Intensity_Det.f90'
  include 'Sem_Mod/Mat_Mul.f90'

  end module
