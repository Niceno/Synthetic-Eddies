!==============================================================================!
  subroutine Read_Inlet_Profiles()
!------------------------------------------------------------------------------!
!   Reading data from another simulations including mean flow and reynolds     !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer           :: j, k
  real              :: tmp_y, tmp_z, y_s, y_e, z_s, z_e, s
  character(len=80) :: header, file_name
!==============================================================================!

  write(*,*) '#=================================='
  write(*,*) '# Reading process started ...'

  file_name = 'input_slice_tmp.dat'

  open(100, file=file_name, form='formatted', status='old')
  read(100,*) header
  read(100,*) header

  !-------------------------------------!
  !   Main loop of reading slice data   !
  !-------------------------------------!
  do j = 1, prof % n_points
    read(100,*) prof % y(j),     &
                prof % u(j),     &
                prof % v(j),     &
                prof % w(j),     &
                prof % t(j),     &
                prof % rs(1,j),  &
                prof % rs(2,j),  &
                prof % rs(3,j),  &
                prof % rs(4,j),  &
                prof % rs(5,j),  &
                prof % rs(6,j),  &
                prof % ts(1,j),  &
                prof % ts(2,j),  &
                prof % ts(3,j),  &
                prof % ts(4,j)
    mesh % y(j) = prof % y(j)  ! very dirty
  end do

  close(100)

  ! Surface area
  y_s = minval(mesh % y(:))
  y_e = maxval(mesh % y(:))
  z_s = minval(mesh % z(:))
  z_e = maxval(mesh % z(:))
  s = (y_e - y_s) * (z_e - z_s)

  ! Some kind of volume ... boundary volume?
  v_b = s * 2 * sigma

  write(*,*) '# ... reading process is completed'
  write(*,*) '#----------------------------------'
  write(*,*) ''

  end subroutine
