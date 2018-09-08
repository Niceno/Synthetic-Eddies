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
  real    :: int_x(1:n_eddies), int_y(1:n_eddies),  &
             int_z(1:n_eddies), int_t(1:n_eddies),  &
             tmp(1:3)
!------------------------------------------------------------------------------!

  write(*,*) '#=================================='
  write(*,*) '# Eddy setting process started ... '

  y_s = y(1)  - sigma
  y_e = y(ny) + sigma

  z_s = z(1)  - sigma
  z_e = z(nz) + sigma

  call random_number(int_x)
  call random_number(int_y)
  call random_number(int_z)
  call random_number(int_t)

  do e = 1, n_eddies
    eddy(e) % num = e
    eddy(e) % len = sigma

    call random_number(tmp)
    eddy(e) % x = -sigma + 2*sigma*tmp(1)
    eddy(e) % y = y_s + (y_e-y_s)*tmp(2)
    eddy(e) % z = z_s + (z_e-z_s)*tmp(3)

    eddy(e) % x_int = Intensity_Det(int_x(e)-0.5)
    eddy(e) % y_int = Intensity_Det(int_y(e)-0.5)
    eddy(e) % z_int = Intensity_Det(int_z(e)-0.5)
    eddy(e) % t_int = Intensity_Det(int_t(e)-0.5)
  end do

  write(*,*) '# ... eddy setting process ended.'
  write(*,*) '#----------------------------------'
  write(*,*) ''

  end subroutine
