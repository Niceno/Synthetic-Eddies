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
  integer            :: ts
  integer, parameter :: n_dt = 200
!==============================================================================!

  !--------------------!
  !   Initialization   !
  !--------------------!
  call Initialize()
  call Read_Inlet_Profiles()
  call Eddy_Setting()

  !---------------!
  !   Time loop   !
  !---------------!
  do ts = 1, n_dt

    call Generate_Fluctuations(ts)
    call Scale_Fluctuations   (ts)
    call Convect_Eddy()
    call Statistics           (ts)

    write(*,'(a,i5,a,i5)') ' # Time step ', ts, ' from ', n_dt

  end do

  !------------------------------!
  !    At the end of the run,    !
  !   write out the statistics   !
  !------------------------------!
  call Write_Statistics()

  end program
