!==============================================================================!
  program Neu_2_Vtk
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  character(len=80)  :: line
  character(len=80)  :: file_name_in
  character(len=80)  :: file_name_out
  integer            :: tmp(8), n, n_nodes, n_cells
  real               :: y, z
  integer, parameter :: NEU=8, VTK=9
!------------------------------------------------------------------------------!

  file_name_in  = 'circle.neu'
  file_name_out = 'circle.vtk'
! file_name_in  = 'rectangle.neu'
! file_name_out = 'rectangle.vtk'

  open(NEU, file=file_name_in,  form='formatted')
  open(VTK, file=file_name_out, form='formatted')

  !-------------!
  !   Headers   !
  !-------------!

  ! Skip NEU header
  read(NEU,*) line;  read(NEU,*) line;  read(NEU,*) line
  read(NEU,*) line;  read(NEU,*) line;  read(NEU,*) line

  ! Write VTK header
  write(VTK, '(a)') '# vtk DataFile Version 3.0'
  write(VTK, '(a)') 'vtk output'
  write(VTK, '(a)') 'ASCII'
  write(VTK, '(a)') 'DATASET UNSTRUCTURED_GRID'

  ! Number of nodes and elements and end of the line
  read(NEU,*) n_nodes, n_cells, line

  ! Skip two lines
  read(NEU,*) line
  read(NEU,*) line

  !-------------------!
  !   Nodes section   !
  !-------------------!
  write(VTK, '(a7,i6,a6)')  'POINTS ', n_nodes, ' float'

  ! Read all nodes from NEU and write them to VTK
  do n = 1, n_nodes
    read (NEU, *)           tmp(1), y, z
    write(VTK,'(3es12.4)')     0.0, y, z
  end do

  ! Skip two lines
  read(NEU,*) line
  read(NEU,*) line

  !-------------------!
  !   Cells section   !
  !-------------------!
  write(VTK, '(a,2i6)')  'CELLS ', n_cells, n_cells*5

  ! Read all cells from NEU and write them to VTK
  do n = 1, n_cells
    read (NEU, *)       tmp(1:7)
    write(VTK,'(5i6)')  tmp(3), tmp(4:7)-1
  end do

  write(VTK, '(a,i6)')  'CELL_TYPES ', n_cells
  do n = 1, n_cells
    write(VTK,'(i6)')  9
  end do

  close(NEU)
  close(VTK)

  end program
