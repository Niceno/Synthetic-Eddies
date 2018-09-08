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

  end module
