!==============================================================================!
  subroutine Write_Statistics()
!------------------------------------------------------------------------------!
!   Write each variables in the result folder.                                 !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer           :: it,j,k
  real              :: time_sta, time_end, u_mean(4,1:mesh % ny), rms_mean(8,1:mesh % ny)
  character(len=80) :: file_name
!==============================================================================!

  u_mean(1:4,1:mesh % ny)   = 0.0
  rms_mean(1:8,1:mesh % ny) = 0.0

  !---------------------------------!
  !   Outputs for u mean profiles   !
  !---------------------------------!
  file_name = 'output_mean_profiles.dat'

  if ( int(time/dt) == n_dt) then
    open(100,file=file_name, form='formatted', position='append')
    write(100,*)'variables = y,u_ins,u_exac,v_ins,v_exac,w_ins,w_exac,t_ins,t_exac'

    do j = 1,mesh % ny
      do k = 1,mesh % nz
        u_mean(1,j) = u_mean(1,j) + u % pro(j,k)
        u_mean(2,j) = u_mean(2,j) + v % pro(j,k)
        u_mean(3,j) = u_mean(3,j) + w % pro(j,k)
        u_mean(4,j) = u_mean(4,j) + t % pro(j,k)
      end do
      u_mean(1:4,j) = u_mean(1:4,j)/mesh % nz
      write(100,"(9f15.9)") mesh % y(j), u_pr(1,j), u_mean(1,j),         &
                                         u_pr(2,j), u_mean(2,j),         &
                                         u_pr(3,j), u_mean(3,j),         &
                                         u_pr(4,j), u_mean(4,j)
    end do
    write(100,*)
    close(100)
  end if

  !------------------------------------------!
  !   Outputs for reynolds stress profiles   !
  !------------------------------------------!
  file_name = 'output_rms_profiles.dat'

  if ( int(time/dt) == n_dt) then
    open(100,file=file_name,form='formatted',position='append')
    write(100,*)'variables = y,uu,uu_exac,vv,vv_exac,ww,ww_exac,'// &
                              'tt,tt_exac,uv,uv_exac,ut,ut_exac,'// &
                              'vt,vt_exac,wt,wt_exac'

    do j = 1,mesh % ny
      do k = 1,mesh % nz
        rms_mean(1,j) = rms_mean(1,j) + rs(1,j,k)
        rms_mean(2,j) = rms_mean(2,j) + rs(2,j,k)
        rms_mean(3,j) = rms_mean(3,j) + rs(3,j,k)
        rms_mean(4,j) = rms_mean(4,j) + ths(1,j,k)

        rms_mean(5,j) = rms_mean(5,j) + rs(4,j,k)
        rms_mean(6,j) = rms_mean(6,j) + ths(2,j,k)
        rms_mean(7,j) = rms_mean(7,j) + ths(3,j,k)
        rms_mean(8,j) = rms_mean(8,j) + ths(4,j,k)
      end do
      rms_mean(1:8,j) = rms_mean(1:8,j)/mesh % nz
      write(100,"(17f15.9)") mesh % y(j), rms_pr(1,j), rms_mean(1,j),     &
                                          rms_pr(2,j), rms_mean(2,j),     &
                                          rms_pr(3,j), rms_mean(3,j),     &
                                          rms_pr(4,j), rms_mean(4,j),     &
                                          rms_pr(5,j), rms_mean(5,j),     &
                                          rms_pr(6,j), rms_mean(6,j),     &
                                          rms_pr(7,j), rms_mean(7,j),     &
                                          rms_pr(8,j), rms_mean(8,j)
    end do
    write(100,*)
    close(100)
  end if

  end subroutine
