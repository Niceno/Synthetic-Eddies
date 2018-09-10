!==============================================================================!
  subroutine Save_Vtk_Flow(flw, stem_name, ts, raw, avg, dns)
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
  logical, optional :: raw
  logical, optional :: avg
  logical, optional :: dns
!-----------------------------------[Locals]-----------------------------------!
  integer           :: e, j, k, l, var, n_var
  real              :: x0, y0, z0, f
  character(len=80) :: full_name
  type(Mesh_Type), target :: msh
!==============================================================================!

  msh = flw % pnt_mesh

  !-----------------------------!
  !   Set number of variables   !
  !-----------------------------!
  n_var = 4

  if(present(raw)) then
    if(raw) then
      n_var = n_var + 4
    end if
  end if

  if(present(avg)) then
    if(avg) then
      n_var = n_var + 4
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
  do var = 1, 4
    select case(var)
      case(1)  
        write(1, '(a,i6,a)') 'U-com  1', msh % n_cells, ' float'
      case(2)  
        write(1, '(a,i6,a)') 'V-com  1', msh % n_cells, ' float'
      case(3)  
        write(1, '(a,i6,a)') 'W-com  1', msh % n_cells, ' float'
      case(4)  
        write(1, '(a,i6,a)') 'T-com  1', msh % n_cells, ' float'
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

  !--------------------!
  !   Raw quantities   !
  !--------------------!
  if(present(raw)) then
    if(raw) then
      do var = 1, 4
        select case(var)
          case(1)
            write(1, '(a,i6,a)') 'U-raw  1', msh % n_cells, ' float'
          case(2)  
            write(1, '(a,i6,a)') 'V-raw  1', msh % n_cells, ' float'
          case(3)  
            write(1, '(a,i6,a)') 'W-raw  1', msh % n_cells, ' float'
          case(4)  
            write(1, '(a,i6,a)') 'T-raw  1', msh % n_cells, ' float'
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
      do var = 1, 4
        select case(var)
          case(1)
            write(1, '(a,i6,a)') 'U-avg  1', msh % n_cells, ' float'
          case(2)  
            write(1, '(a,i6,a)') 'V-avg  1', msh % n_cells, ' float'
          case(3)  
            write(1, '(a,i6,a)') 'W-avg  1', msh % n_cells, ' float'
          case(4)  
            write(1, '(a,i6,a)') 'T-avg  1', msh % n_cells, ' float'
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
            end select
          end do
        end do
      end do    ! through variables
    end if    ! if(avg)
  end if    ! if(present(avg))

  write(1, '(a, i6)')  'POINT_DATA',  msh % n_nodes

  close(1)

  contains

  include 'At.f90'

  end subroutine
