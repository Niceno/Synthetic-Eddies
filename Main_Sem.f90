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
!-----------------------------------[Locals]-----------------------------------!
  type(Eddy_Type) :: eddy
  type(Prof_Type) :: prof
  type(Mesh_Type) :: mesh
  type(Flow_Type) :: flow
  integer         :: ts
!------------------------------[Local parameters]------------------------------!
  integer, parameter :: N_DT = 300
  real,    parameter :: DT   = 5.0e-3
!==============================================================================!

  !--------------------!
  !   Initialization   !
  !--------------------!
!  call Mesh_Mod_Create_Cartesian(mesh, 96, 256)
  call Mesh_Mod_Create_From_File(mesh, "circle.vtk")  ! mesh
  call Flow_Mod_Create(flow, mesh)                    ! flow
  call Prof_Mod_Create(prof, mesh % ny)               ! profile
  call Eddy_Mod_Create(eddy, 1024, 0.2)               ! n_eddies and sigma

  call Prof_Mod_Read(prof, mesh, 'input_line_tmp.dat')  ! this should be part of Prof_Mod_Create
  call Eddy_Setting(eddy, mesh)

  !---------------!
  !   Time loop   !
  !---------------!
  do ts = 1, N_DT

    write(*,'(a,i5,a,i5)') ' # Time step ', ts, ' from ', n_dt

    call Generate_Fluctuations(eddy, flow, ts)
    call Scale_Fluctuations   (flow, prof, ts)
    call Convect_Eddy         (eddy, flow, DT)
    call Statistics           (flow, prof, ts)

  end do

  !------------------------------!
  !    At the end of the run,    !
  !   write out the statistics   !
  !------------------------------!
  call Write_Statistics()

  end program
