!==============================================================================!
  subroutine Eddy_Setting
!------------------------------------------------------------------------------!
!   Setup each eddys characteristic including eddy length, positions and       !
!   intensities of each diretions.                                             !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: e
  real    :: y_s, y_e, z_s, z_e
  real    :: int_x(1:eddy % n_eddies), int_y(1:eddy % n_eddies),  &
             int_z(1:eddy % n_eddies), int_t(1:eddy % n_eddies),  &
             tmp(1:3)
!------------------------------------------------------------------------------!

  write(*,*) '#=================================='
  write(*,*) '# Eddy setting process started ... '

  ! Determin the bounding box for the eddies
  y_s = minval(mesh % y(:)) - sigma
  y_e = maxval(mesh % y(:)) + sigma
  z_s = minval(mesh % z(:)) - sigma
  z_e = maxval(mesh % z(:)) + sigma

  call random_number(int_x)
  call random_number(int_y)
  call random_number(int_z)
  call random_number(int_t)

  do e = 1, eddy % n_eddies
    eddy % num(e) = e
    eddy % len(e) = sigma

    call random_number(tmp)
    eddy % x(e) = -sigma + 2*sigma*tmp(1)
    eddy % y(e) = y_s + (y_e-y_s)*tmp(2)
    eddy % z(e) = z_s + (z_e-z_s)*tmp(3)

    eddy % x_int(e) = Intensity_Det(int_x(e)-0.5)
    eddy % y_int(e) = Intensity_Det(int_y(e)-0.5)
    eddy % z_int(e) = Intensity_Det(int_z(e)-0.5)
    eddy % t_int(e) = Intensity_Det(int_t(e)-0.5)
  end do

  write(*,*) '# ... eddy setting process ended.'
  write(*,*) '#----------------------------------'
  write(*,*) ''

  end subroutine
