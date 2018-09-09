!==============================================================================!
  subroutine Eddy_Setting(ed, msh)
!------------------------------------------------------------------------------!
!   Setup each eds characteristic including ed length, positions and       !
!   intensities of each diretions.                                             !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Eddy_Mod, only: Eddy_Type
  use Mesh_Mod, only: Mesh_Type
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Eddy_Type) :: ed
  type(Mesh_Type) :: msh
!-----------------------------------[Locals]-----------------------------------!
  integer :: e
  real    :: y_s, y_e, z_s, z_e
  real    :: int_x(1:ed % n_eddies), int_y(1:ed % n_eddies),  &
             int_z(1:ed % n_eddies), int_t(1:ed % n_eddies),  &
             tmp(1:3)
!------------------------------------------------------------------------------!

  write(*,*) '#=================================='
  write(*,*) '# Eddy setting process started ... '

  ! Determin the bounding box for the eddies
  y_s = minval(msh % yn(:)) - ed % sigma
  y_e = maxval(msh % yn(:)) + ed % sigma
  z_s = minval(msh % zn(:)) - ed % sigma
  z_e = maxval(msh % zn(:)) + ed % sigma

  call random_number(int_x)
  call random_number(int_y)
  call random_number(int_z)
  call random_number(int_t)

  do e = 1, ed % n_eddies
    ed % num(e) = e
    ed % len(e) = ed % sigma

    call random_number(tmp)
    ed % x(e) = -ed % sigma + 2 * ed % sigma * tmp(1)
    ed % y(e) = y_s + (y_e-y_s)*tmp(2)
    ed % z(e) = z_s + (z_e-z_s)*tmp(3)

    ed % x_int(e) = sign(1.0,int_x(e)-0.5)
    ed % y_int(e) = sign(1.0,int_y(e)-0.5)
    ed % z_int(e) = sign(1.0,int_z(e)-0.5)
    ed % t_int(e) = sign(1.0,int_t(e)-0.5)
  end do

  write(*,*) '# ... eddy setting process ended.'
  write(*,*) '#----------------------------------'
  write(*,*) ''

  end subroutine
