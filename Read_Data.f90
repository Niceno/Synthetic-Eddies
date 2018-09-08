!==============================================================================!
  subroutine Read_Data()
!------------------------------------------------------------------------------!
!   Reading data from another simulations including mean flow and reynolds     !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer           :: j, k
  real              :: tmp_y, tmp_z, s
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
  do j = 1,ny
    do k = 1,nz
      read(100,*) tmp_z, tmp_y, u(j,k), v(j,k), w(j,k), t(j,k),       &
                  rs(1,j,k), rs(2,j,k), rs(3,j,k), rs(4,j,k),         &
                  rs(5,j,k), rs(6,j,k),                               &
                  ths(1,j,k), ths(2,j,k), ths(3,j,k), ths(4,j,k)

      if (k==1) y(j) = tmp_y
      if (j==1) z(k) = tmp_z
    end do
  end do

  close(100)

  s   = ( y(ny) - y(1) + 2*sigma ) * ( z(nz) - z(1) + 2*sigma )
  v_b = s * 2 * sigma

  write(*,*) '# ... reading process is completed'
  write(*,*) '#----------------------------------'
  write(*,*) ''

  end subroutine
