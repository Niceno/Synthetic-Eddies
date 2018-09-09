!==============================================================================!
  module Eddy_Mod
!------------------------------------------------------------------------------!
!   Module to define eddy type                                                 !
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !---------------!
  !               !
  !   Eddy type   !
  !               !
  !---------------!
  type Eddy_Type
    integer :: n_eddies
    real    :: sigma

    integer, allocatable :: num(:)    ! eddy specification number
    real,    allocatable :: len(:)    ! eddy length scale
    real,    allocatable :: x(:)      ! eddy's x position
    real,    allocatable :: y(:)      ! eddy's y position
    real,    allocatable :: z(:)      ! eddy's z position
    real,    allocatable :: x_int(:)  ! eddy's x intensity
    real,    allocatable :: y_int(:)  ! eddy's y intensity
    real,    allocatable :: z_int(:)  ! eddy's z intensity
    real,    allocatable :: t_int(:)  ! eddy's t intensity
  end type

  contains

  include 'Eddy_Mod/Create.f90'

  end module
