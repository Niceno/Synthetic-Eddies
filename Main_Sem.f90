!==============================================================================!
  program Sem
!------------------------------------------------------------------------------!
!   To make inflow generation by using sem (synthetic eddy method)             !
!
!   References: https://goo.gl/eEgVpV, https://goo.gl/4B24AC
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: ts
!==============================================================================!

  call Setup()
  call Read_Profiles()
  call Eddy_Setting()

  do ts = 1, n_dt

    time = ts * dt

    call Generate_Fluctuations(ts)
    call Scale_Fluctuations   (ts)
    call Convect_Eddy()
    call Statistics           (ts)

    write(*,'(a,i5,a)') ' # Sem for',ts,'iteration'

    if( mod(ts,out_num) == 0 ) call Write_Statistics()
  end do

  end program
