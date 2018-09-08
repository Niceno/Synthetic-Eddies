!==============================================================================!
  subroutine Read_Profiles()
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
  do j = 1, mesh % ny
    do k = 1, mesh % nz
      read(100,*) tmp_z, tmp_y,                                             &
                  u % pro(j,k), v % pro(j,k), w % pro(j,k), t % pro(j,k),   &
                  rs(1,j,k), rs(2,j,k), rs(3,j,k), rs(4,j,k),               &
                  rs(5,j,k), rs(6,j,k),                                     &
                  ths(1,j,k), ths(2,j,k), ths(3,j,k), ths(4,j,k)

      ! Hmm ... why on earth this twist?
      if (k==1) mesh % y(j) = tmp_y
      if (j==1) mesh % z(k) = tmp_z
    end do
  end do

  close(100)

  ! Surface
  y_s = minval(mesh % y(:))
  y_e = maxval(mesh % y(:))
  z_s = minval(mesh % z(:))
  z_e = maxval(mesh % z(:))
  s = (y_e - y_s) * (z_e - z_s)
  v_b = s * 2 * sigma

  write(*,*) '# ... reading process is completed'
  write(*,*) '#----------------------------------'
  write(*,*) ''

  end subroutine
