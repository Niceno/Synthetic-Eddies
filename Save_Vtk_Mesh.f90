!==============================================================================!
  subroutine Save_Vtk_Mesh(msh, file_name_out)
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Mesh_Mod, only: Mesh_Type, UNSTRUCTURED
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type)    :: msh
  character(len=80)  :: file_name_out
!-----------------------------------[Locals]-----------------------------------!
  integer            :: n, c
  integer, parameter :: VTK=9
!------------------------------------------------------------------------------!

  open(VTK, file=file_name_out, form='formatted')

  if(msh % mode .eq. UNSTRUCTURED) then

    !------------!
    !   Header   !
    !------------!

    ! Write VTK header
    write(VTK, '(a)') '# vtk DataFile Version 3.0'
    write(VTK, '(a)') 'vtk output'
    write(VTK, '(a)') 'ASCII'
    write(VTK, '(a)') 'DATASET UNSTRUCTURED_GRID'

    !-------------------!
    !   Nodes section   !
    !-------------------!
    write(VTK, '(a7,i6,a6)')  'POINTS ', msh % n_nodes, ' float'

    do n = 1, msh % n_nodes
      write(VTK,'(3es12.4)')  0.0, msh % yn(n), msh % zn(n)
    end do

    !-------------------!
    !   Cells section   !
    !-------------------!
    write(VTK, '(a,2i6)')  'CELLS ', msh % n_cells, msh % n_cells*5

    do c = 1, msh % n_cells
      write(VTK,'(5i6)')  4, msh % cells_nodes(1:4, c)
    end do

    write(VTK, '(a,i6)')  'CELL_TYPES ', msh % n_cells
    do c = 1, msh % n_cells
      write(VTK,'(i6)')  9
    end do

    close(VTK)

  end if

  end subroutine
