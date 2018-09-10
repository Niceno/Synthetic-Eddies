!==============================================================================!
  program Synthetic_Eddy
!------------------------------------------------------------------------------!
!   To make inflow generation by using sem (synthetic eddy method)             !
!
!   References: https://goo.gl/eEgVpV, https://goo.gl/4B24AC
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Eddy_Mod
  use Prof_Mod
  use Mesh_Mod
  use Flow_Mod
!------------------------------------------------------------------------------!
  implicit none
!----------------------------------[Calling]-----------------------------------!
  include 'Save_Vtk_Flow.int'
!-----------------------------------[Locals]-----------------------------------!
  type(Eddy_Type) :: eddy
  type(Prof_Type) :: prof
  type(Mesh_Type) :: mesh
  type(Flow_Type) :: flow
  integer         :: ts
!------------------------------[Local parameters]------------------------------!
  integer, parameter :: N_DT = 12000
  real,    parameter :: DT   = 5.0e-3
!==============================================================================!

  !------------------!
  !   Constructors   !
  !------------------!
  call Mesh_Mod_Create_Cartesian(mesh, 97, 257)       ! node numbers
! call Mesh_Mod_Create_From_File(mesh, "circle.vtk")  ! mesh
  call Flow_Mod_Create(flow, mesh)                    ! flow
  call Prof_Mod_Create(prof, 'input_line_tmp.dat')    ! profile
  call Eddy_Mod_Create(eddy, 1024, 0.2)               ! eddies

  ! Copy DNS profiles to numerical mesh, and plot what you obtained
  call Flow_Mod_Fetch_Profile(flow, prof)
  call Save_Vtk_Flow(flow, 'dns-from-file', 0, dns=.true.)

  call Eddy_Setting(eddy, mesh)

  !---------------!
  !   Time loop   !
  !---------------!
  do ts = 1, N_DT

    write(*,'(a,i5,a,i5)') ' # Time step ', ts, ' from ', n_dt

    call Generate_Fluctuations(flow, eddy)
    call Scale_Fluctuations   (flow)
    call Convect_Eddy         (flow, eddy, DT)
    call Statistics           (flow, ts)

    ! For making movies
    if(mod(ts,10) .eq. 0) then
      call Save_Vtk_Flow(flow, 'com-and-raw', ts, com=.true., raw=.true.)
    end if

    ! To see statistics
    if(mod(ts,1000) .eq. 0) then
      call Save_Vtk_Flow(flow, 'avg-and-dns', ts, avg=.true., dns=.true.)
    end if

  end do

  end program
