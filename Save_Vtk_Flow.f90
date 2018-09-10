!==============================================================================!
  subroutine Save_Vtk_Flow(flw, stem_name, ts, com, raw, avg, dns)
!------------------------------------------------------------------------------!
!   Generate fluctuations without combining mean and rms data                  !
!------------------------------------------------------------------------------!
  use Flow_Mod, only: Flow_Type
  use Mesh_Mod, only: Mesh_Type, STRUCTURED
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Flow_Type)   :: flw
  character(len=*)  :: stem_name
  integer           :: ts
  logical, optional :: com
  logical, optional :: raw
  logical, optional :: avg
  logical, optional :: dns
!-----------------------------------[Locals]-----------------------------------!
  integer           :: j, k, l, var, n_var
  character(len=80) :: full_name
  type(Mesh_Type), target :: msh
!==============================================================================!

  msh = flw % pnt_mesh

  !-----------------------------!
  !   Set number of variables   !
  !-----------------------------!
  n_var = 0

  if(present(com)) then
    if(com) then
      n_var = n_var + 4
    end if
  end if

  if(present(raw)) then
    if(raw) then
      n_var = n_var + 4
    end if
  end if

  if(present(avg)) then
    if(avg) then
      n_var = n_var + 14
    end if
  end if

  if(present(dns)) then
    if(dns) then
      n_var = n_var + 14
    end if
  end if

  full_name = trim(stem_name)
  l = len_trim(full_name)

  write(full_name(l+ 1:l+10), '(a10)') '-00000.vtk'
  write(full_name(l+ 2:l+ 6), '(i5.5)') ts

  open(1, file=trim(full_name))

  !------------!
  !            !
  !   Header   !
  !            !
  !------------!
  write(1, '(a)')      '# vtk DataFile Version 3.0'
  write(1, '(a)')      'vtk output'
  write(1, '(a)')      'ASCII'

  !----------!
  !          !
  !   Mesh   !
  !          !
  !----------!
  write(1, '(a)')      'DATASET RECTILINEAR_GRID'
  write(1, '(a,3i6)')  'DIMENSIONS',1, msh % ny, msh % nz 
  write(1, '(a,i6,a)') 'X_COORDINATES',  1, ' float'
  write(1, '(a)')      '0.0'
  write(1, '(a,i6,a)') 'Y_COORDINATES', msh % ny, ' float'
  do j = 1, msh % ny
    write(1, '(es12.4)') msh % yn(j)
  end do
  write(1, '(a,i6,a)') 'Z_COORDINATES', msh % nz, ' float'
  do k = 1, msh % nz
    write(1, '(es12.4)') msh % zn(k)
  end do

  !---------------!
  !               !
  !   Variables   !
  !               !
  !---------------!
  write(1, '(a,i6)')   'CELL_DATA',   msh % n_cells
  write(1, '(a,i6)')   'FIELD FieldData ', n_var

  !-------------------------!
  !   Computed quantities   !
  !-------------------------!
  if(present(raw)) then
    if(raw) then
      do var = 1, 4
        select case(var)
          case(1)
            write(1, '(a,i6,a)') 'com-U  1', msh % n_cells, ' float'
          case(2)
            write(1, '(a,i6,a)') 'com-V  1', msh % n_cells, ' float'
          case(3)
            write(1, '(a,i6,a)') 'com-W  1', msh % n_cells, ' float'
          case(4)
            write(1, '(a,i6,a)') 'com-T  1', msh % n_cells, ' float'
        end select
        do k = 1, msh % nz - 1
          do j = 1, msh % ny - 1
            select case(var)
              case(1)
                write(1, '(es12.4)') flw % u % com(At(j,k))
              case(2)
                write(1, '(es12.4)') flw % v % com(At(j,k))
              case(3)
                write(1, '(es12.4)') flw % w % com(At(j,k))
              case(4)
                write(1, '(es12.4)') flw % t % com(At(j,k))
            end select
          end do
        end do
      end do    ! through variables
    end if
  end if

  !--------------------!
  !   Raw quantities   !
  !--------------------!
  if(present(raw)) then
    if(raw) then
      do var = 1, 4
        select case(var)
          case(1)
            write(1, '(a,i6,a)') 'raw-U  1', msh % n_cells, ' float'
          case(2)  
            write(1, '(a,i6,a)') 'raw-V  1', msh % n_cells, ' float'
          case(3)  
            write(1, '(a,i6,a)') 'raw-W  1', msh % n_cells, ' float'
          case(4)  
            write(1, '(a,i6,a)') 'raw-T  1', msh % n_cells, ' float'
        end select
        do k = 1, msh % nz - 1
          do j = 1, msh % ny - 1
            select case(var)
              case(1) 
                write(1, '(es12.4)') flw % u % raw(At(j,k))
              case(2) 
                write(1, '(es12.4)') flw % v % raw(At(j,k))
              case(3) 
                write(1, '(es12.4)') flw % w % raw(At(j,k))
              case(4) 
                write(1, '(es12.4)') flw % t % raw(At(j,k))
            end select
          end do
        end do
      end do    ! through variables
    end if    ! if(raw)
  end if    ! if(present(raw))

  !-------------------------!
  !   Averaged quantities   !
  !-------------------------!
  if(present(avg)) then
    if(avg) then
      do var = 1, 14
        select case(var)
          case(1)
            write(1, '(a,i6,a)') 'avg-U  1', msh % n_cells, ' float'
          case(2)
            write(1, '(a,i6,a)') 'avg-V  1', msh % n_cells, ' float'
          case(3)
            write(1, '(a,i6,a)') 'avg-W  1', msh % n_cells, ' float'
          case(4)
            write(1, '(a,i6,a)') 'avg-T  1', msh % n_cells, ' float'
          case(5)
            write(1, '(a,i6,a)') 'avg-UU  1', msh % n_cells, ' float'
          case(6)
            write(1, '(a,i6,a)') 'avg-VV  1', msh % n_cells, ' float'
          case(7)
            write(1, '(a,i6,a)') 'avg-WW  1', msh % n_cells, ' float'
          case(8)
            write(1, '(a,i6,a)') 'avg-UV  1', msh % n_cells, ' float'
          case(9)
            write(1, '(a,i6,a)') 'avg-UW  1', msh % n_cells, ' float'
          case(10)
            write(1, '(a,i6,a)') 'avg-VW  1', msh % n_cells, ' float'
          case(11)
            write(1, '(a,i6,a)') 'avg-TT  1', msh % n_cells, ' float'
          case(12)
            write(1, '(a,i6,a)') 'avg-UT  1', msh % n_cells, ' float'
          case(13)
            write(1, '(a,i6,a)') 'avg-VT  1', msh % n_cells, ' float'
          case(14)
            write(1, '(a,i6,a)') 'avg-WT  1', msh % n_cells, ' float'
        end select
        do k = 1, msh % nz - 1
          do j = 1, msh % ny - 1
            select case(var)
              case(1)
                write(1, '(es12.4)') flw % u_avg % com(At(j,k))
              case(2)
                write(1, '(es12.4)') flw % v_avg % com(At(j,k))
              case(3)
                write(1, '(es12.4)') flw % w_avg % com(At(j,k))
              case(4)
                write(1, '(es12.4)') flw % t_avg % com(At(j,k))
              case(5)
                write(1, '(es12.4)') flw % uu_avg % com(At(j,k))
              case(6)
                write(1, '(es12.4)') flw % vv_avg % com(At(j,k))
              case(7)
                write(1, '(es12.4)') flw % ww_avg % com(At(j,k))
              case(8)
                write(1, '(es12.4)') flw % uv_avg % com(At(j,k))
              case(9)
                write(1, '(es12.4)') flw % uw_avg % com(At(j,k))
              case(10)
                write(1, '(es12.4)') flw % vw_avg % com(At(j,k))
              case(11)
                write(1, '(es12.4)') flw % tt_avg % com(At(j,k))
              case(12)
                write(1, '(es12.4)') flw % ut_avg % com(At(j,k))
              case(13)
                write(1, '(es12.4)') flw % vt_avg % com(At(j,k))
              case(14)
                write(1, '(es12.4)') flw % wt_avg % com(At(j,k))
            end select
          end do
        end do
      end do    ! through variables
    end if    ! if(avg)
  end if    ! if(present(avg))

  !--------------------!
  !   DNS quantities   !  (to check if interpolation worked, for example)
  !--------------------!
  if(present(dns)) then
    if(dns) then
      do var = 1, 14
        select case(var)
          case(1)
            write(1, '(a,i6,a)') 'dns-U  1', msh % n_cells, ' float'
          case(2)
            write(1, '(a,i6,a)') 'dns-V  1', msh % n_cells, ' float'
          case(3)
            write(1, '(a,i6,a)') 'dns-W  1', msh % n_cells, ' float'
          case(4)
            write(1, '(a,i6,a)') 'dns-T  1', msh % n_cells, ' float'
          case(5)
            write(1, '(a,i6,a)') 'dns-UU  1', msh % n_cells, ' float'
          case(6)
            write(1, '(a,i6,a)') 'dns-VV  1', msh % n_cells, ' float'
          case(7)
            write(1, '(a,i6,a)') 'dns-WW  1', msh % n_cells, ' float'
          case(8)
            write(1, '(a,i6,a)') 'dns-UV  1', msh % n_cells, ' float'
          case(9)
            write(1, '(a,i6,a)') 'dns-UW  1', msh % n_cells, ' float'
          case(10)
            write(1, '(a,i6,a)') 'dns-VW  1', msh % n_cells, ' float'
          case(11)
            write(1, '(a,i6,a)') 'dns-TT  1', msh % n_cells, ' float'
          case(12)
            write(1, '(a,i6,a)') 'dns-UT  1', msh % n_cells, ' float'
          case(13)
            write(1, '(a,i6,a)') 'dns-VT  1', msh % n_cells, ' float'
          case(14)
            write(1, '(a,i6,a)') 'dns-WT  1', msh % n_cells, ' float'
        end select
        do k = 1, msh % nz - 1
          do j = 1, msh % ny - 1
            select case(var)
              case(1)
                write(1, '(es12.4)') flw % u_avg % dns(At(j,k))
              case(2)
                write(1, '(es12.4)') flw % v_avg % dns(At(j,k))
              case(3)
                write(1, '(es12.4)') flw % w_avg % dns(At(j,k))
              case(4)
                write(1, '(es12.4)') flw % t_avg % dns(At(j,k))
              case(5)
                write(1, '(es12.4)') flw % uu_avg % dns(At(j,k))
              case(6)
                write(1, '(es12.4)') flw % vv_avg % dns(At(j,k))
              case(7)
                write(1, '(es12.4)') flw % ww_avg % dns(At(j,k))
              case(8)
                write(1, '(es12.4)') flw % uv_avg % dns(At(j,k))
              case(9)
                write(1, '(es12.4)') flw % uw_avg % dns(At(j,k))
              case(10)
                write(1, '(es12.4)') flw % vw_avg % dns(At(j,k))
              case(11)
                write(1, '(es12.4)') flw % tt_avg % dns(At(j,k))
              case(12)
                write(1, '(es12.4)') flw % ut_avg % dns(At(j,k))
              case(13)
                write(1, '(es12.4)') flw % vt_avg % dns(At(j,k))
              case(14)
                write(1, '(es12.4)') flw % wt_avg % dns(At(j,k))
            end select
          end do
        end do
      end do    ! through variables
    end if    ! if(dns)
  end if    ! if(present(dns))

  write(1, '(a, i6)')  'POINT_DATA',  msh % n_nodes

  close(1)

  contains

  include 'At.f90'

  end subroutine
