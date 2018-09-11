!==============================================================================!
  subroutine Save_Vtk(flw, stem_name, ts, com, raw, avg, dns)
!------------------------------------------------------------------------------!
!   Generate fluctuations without combining mean and rms data                  !
!------------------------------------------------------------------------------!
  use Flow_Mod, only: Flow_Type
  use Mesh_Mod, only: Mesh_Type, STRUCTURED, UNSTRUCTURED
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Flow_Type)   :: flw
  character(len=*)  :: stem_name
  integer, optional :: ts
  logical, optional :: com
  logical, optional :: raw
  logical, optional :: avg
  logical, optional :: dns
!-----------------------------------[Locals]-----------------------------------!
  integer                 :: l, c, n, var, n_var
  character(len=80)       :: full_name
  type(Mesh_Type), target :: msh
!------------------------------[Local parameters]------------------------------!
  integer, parameter :: VTK=9
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

  ! If time step is present (given), add it to the file name
  if(present(ts)) then
    write(full_name(l+ 1:l+10), '(a10)') '-00000.vtk'
    write(full_name(l+ 2:l+ 6), '(i5.5)') ts
  else
    write(full_name(l+1:l+4), '(a4)') '.vtk'
  end if

  open(VTK, file=trim(full_name))

  !------------!
  !            !
  !   Header   !
  !            !
  !------------!
  write(VTK, '(a)') '# vtk DataFile Version 3.0'
  write(VTK, '(a)') 'vtk output'
  write(VTK, '(a)') 'ASCII'

  !----------!
  !          !
  !   Mesh   !
  !          !
  !----------!

  !------------------!
  !   Type of grid   !
  !------------------!
  if( msh % mode .eq. STRUCTURED) then
    write(VTK, '(a)')       'DATASET STRUCTURED_GRID'
    write(VTK, '(a,3i6)')   'DIMENSIONS', 1, msh % ny, msh % nz 
  end if
  if( msh % mode .eq. UNSTRUCTURED) then
    write(VTK, '(a)')       'DATASET UNSTRUCTURED_GRID'
    write(VTK, '(a,3i6)')   'DIMENSIONS', 1, msh % ny, msh % nz 
  end if

  !-----------!
  !   Nodes   !
  !-----------!
  write(VTK, '(a,i6,a)')  'POINTS',  msh % ny * msh % nz, ' float'
  do n = 1, msh % n_nodes
    write(VTK, '(3es12.4)') 1.0, msh % yn(n), msh % zn(n)
  end do

  !-----------!
  !   Cells   !
  !-----------!
  if( msh % mode .eq. UNSTRUCTURED) then
    write(VTK, '(a,2i6)')  'CELLS ', msh % n_cells, msh % n_cells*5

    ! Cells' nodes
    do c = 1, msh % n_cells
      write(VTK,'(5i6)')  4, msh % cells_nodes(1:4, c)
    end do

    ! Cell types
    write(VTK, '(a,i6)')  'CELL_TYPES ', msh % n_cells
    do c = 1, msh % n_cells
      write(VTK,'(i6)')  9
    end do
  end if

  ! If no variables are to be written, ...
  ! ... close the file and exit gracefully
  if(n_var .eq. 0) then
    close(VTK)
    return
  end if

  !---------------!
  !               !
  !   Variables   !
  !               !
  !---------------!
  write(VTK, '(a,i6)')  'CELL_DATA',   msh % n_cells
  write(VTK, '(a,i6)')  'FIELD FieldData ', n_var

  !-------------------------!
  !   Computed quantities   !
  !-------------------------!
  if(present(raw)) then
    if(raw) then
      do var = 1, 4
        select case(var)
          case(1)
            write(VTK, '(a,i6,a)') 'com-U  1', msh % n_cells, ' float'
          case(2)
            write(VTK, '(a,i6,a)') 'com-V  1', msh % n_cells, ' float'
          case(3)
            write(VTK, '(a,i6,a)') 'com-W  1', msh % n_cells, ' float'
          case(4)
            write(VTK, '(a,i6,a)') 'com-T  1', msh % n_cells, ' float'
        end select
        do c = 1, msh % n_cells
          select case(var)
            case(1)
              write(VTK, '(es12.4)') flw % u % com(c)
            case(2)
              write(VTK, '(es12.4)') flw % v % com(c)
            case(3)
              write(VTK, '(es12.4)') flw % w % com(c)
            case(4)
              write(VTK, '(es12.4)') flw % t % com(c)
          end select
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
            write(VTK, '(a,i6,a)') 'raw-U  1', msh % n_cells, ' float'
          case(2)  
            write(VTK, '(a,i6,a)') 'raw-V  1', msh % n_cells, ' float'
          case(3)  
            write(VTK, '(a,i6,a)') 'raw-W  1', msh % n_cells, ' float'
          case(4)  
            write(VTK, '(a,i6,a)') 'raw-T  1', msh % n_cells, ' float'
        end select
        do c = 1, msh % n_cells
          select case(var)
            case(1)
              write(VTK, '(es12.4)') flw % u % raw(c)
            case(2)
              write(VTK, '(es12.4)') flw % v % raw(c)
            case(3)
              write(VTK, '(es12.4)') flw % w % raw(c)
            case(4)
              write(VTK, '(es12.4)') flw % t % raw(c)
          end select
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
            write(VTK, '(a,i6,a)') 'avg-U  1', msh % n_cells, ' float'
          case(2)
            write(VTK, '(a,i6,a)') 'avg-V  1', msh % n_cells, ' float'
          case(3)
            write(VTK, '(a,i6,a)') 'avg-W  1', msh % n_cells, ' float'
          case(4)
            write(VTK, '(a,i6,a)') 'avg-T  1', msh % n_cells, ' float'
          case(5)
            write(VTK, '(a,i6,a)') 'avg-UU  1', msh % n_cells, ' float'
          case(6)
            write(VTK, '(a,i6,a)') 'avg-VV  1', msh % n_cells, ' float'
          case(7)
            write(VTK, '(a,i6,a)') 'avg-WW  1', msh % n_cells, ' float'
          case(8)
            write(VTK, '(a,i6,a)') 'avg-UV  1', msh % n_cells, ' float'
          case(9)
            write(VTK, '(a,i6,a)') 'avg-UW  1', msh % n_cells, ' float'
          case(10)
            write(VTK, '(a,i6,a)') 'avg-VW  1', msh % n_cells, ' float'
          case(11)
            write(VTK, '(a,i6,a)') 'avg-TT  1', msh % n_cells, ' float'
          case(12)
            write(VTK, '(a,i6,a)') 'avg-UT  1', msh % n_cells, ' float'
          case(13)
            write(VTK, '(a,i6,a)') 'avg-VT  1', msh % n_cells, ' float'
          case(14)
            write(VTK, '(a,i6,a)') 'avg-WT  1', msh % n_cells, ' float'
        end select
        do c = 1, msh % n_cells
          select case(var)
            case(1)
              write(VTK, '(es12.4)') flw % u_avg % com(c)
            case(2)
              write(VTK, '(es12.4)') flw % v_avg % com(c)
            case(3)
              write(VTK, '(es12.4)') flw % w_avg % com(c)
            case(4)
              write(VTK, '(es12.4)') flw % t_avg % com(c)
            case(5)
              write(VTK, '(es12.4)') flw % uu_avg % com(c)
            case(6)
              write(VTK, '(es12.4)') flw % vv_avg % com(c)
            case(7)
              write(VTK, '(es12.4)') flw % ww_avg % com(c)
            case(8)
              write(VTK, '(es12.4)') flw % uv_avg % com(c)
            case(9)
              write(VTK, '(es12.4)') flw % uw_avg % com(c)
            case(10)
              write(VTK, '(es12.4)') flw % vw_avg % com(c)
            case(11)
              write(VTK, '(es12.4)') flw % tt_avg % com(c)
            case(12)
              write(VTK, '(es12.4)') flw % ut_avg % com(c)
            case(13)
              write(VTK, '(es12.4)') flw % vt_avg % com(c)
            case(14)
              write(VTK, '(es12.4)') flw % wt_avg % com(c)
          end select
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
            write(VTK, '(a,i6,a)') 'dns-U  1', msh % n_cells, ' float'
          case(2)
            write(VTK, '(a,i6,a)') 'dns-V  1', msh % n_cells, ' float'
          case(3)
            write(VTK, '(a,i6,a)') 'dns-W  1', msh % n_cells, ' float'
          case(4)
            write(VTK, '(a,i6,a)') 'dns-T  1', msh % n_cells, ' float'
          case(5)
            write(VTK, '(a,i6,a)') 'dns-UU  1', msh % n_cells, ' float'
          case(6)
            write(VTK, '(a,i6,a)') 'dns-VV  1', msh % n_cells, ' float'
          case(7)
            write(VTK, '(a,i6,a)') 'dns-WW  1', msh % n_cells, ' float'
          case(8)
            write(VTK, '(a,i6,a)') 'dns-UV  1', msh % n_cells, ' float'
          case(9)
            write(VTK, '(a,i6,a)') 'dns-UW  1', msh % n_cells, ' float'
          case(10)
            write(VTK, '(a,i6,a)') 'dns-VW  1', msh % n_cells, ' float'
          case(11)
            write(VTK, '(a,i6,a)') 'dns-TT  1', msh % n_cells, ' float'
          case(12)
            write(VTK, '(a,i6,a)') 'dns-UT  1', msh % n_cells, ' float'
          case(13)
            write(VTK, '(a,i6,a)') 'dns-VT  1', msh % n_cells, ' float'
          case(14)
            write(VTK, '(a,i6,a)') 'dns-WT  1', msh % n_cells, ' float'
        end select
        do c = 1, msh % n_cells
          select case(var)
            case(1)
              write(VTK, '(es12.4)') flw % u_avg % dns(c)
            case(2)
              write(VTK, '(es12.4)') flw % v_avg % dns(c)
            case(3)
              write(VTK, '(es12.4)') flw % w_avg % dns(c)
            case(4)
              write(VTK, '(es12.4)') flw % t_avg % dns(c)
            case(5)
              write(VTK, '(es12.4)') flw % uu_avg % dns(c)
            case(6)
              write(VTK, '(es12.4)') flw % vv_avg % dns(c)
            case(7)
              write(VTK, '(es12.4)') flw % ww_avg % dns(c)
            case(8)
              write(VTK, '(es12.4)') flw % uv_avg % dns(c)
            case(9)
              write(VTK, '(es12.4)') flw % uw_avg % dns(c)
            case(10)
              write(VTK, '(es12.4)') flw % vw_avg % dns(c)
            case(11)
              write(VTK, '(es12.4)') flw % tt_avg % dns(c)
            case(12)
              write(VTK, '(es12.4)') flw % ut_avg % dns(c)
            case(13)
              write(VTK, '(es12.4)') flw % vt_avg % dns(c)
            case(14)
              write(VTK, '(es12.4)') flw % wt_avg % dns(c)
          end select
        end do
      end do    ! through variables
    end if    ! if(dns)
  end if    ! if(present(dns))

  write(VTK, '(a, i6)')  'POINT_DATA',  msh % n_nodes

  close(VTK)

  end subroutine
