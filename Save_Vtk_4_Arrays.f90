!==============================================================================!
  subroutine Save_Vtk_4_Arrays(stem_name, arr_1, arr_2, arr_3, arr_4, ts)
!------------------------------------------------------------------------------!
!   Generate fluctuations without combining mean and rms data                  !
!------------------------------------------------------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(mesh % ny, mesh % nz) :: arr_1, arr_2, arr_3, arr_4
  character(len=*)                      :: stem_name
  integer                               :: ts
!-----------------------------------[Locals]-----------------------------------!
  integer           :: e, j, k, l
  real              :: x0, y0, z0, f
  character(len=80) :: full_name
!==============================================================================!

  full_name = trim(stem_name)
  l = len_trim(full_name)

  write(full_name(l+ 1:l+10), '(a10)') '-00000.vtk'
  write(full_name(l+ 2:l+ 6), '(i5.5)') ts

  open(1, file=trim(full_name))

  write(1, '(a)')      '# vtk DataFile Version 3.0'
  write(1, '(a)')      'vtk output'
  write(1, '(a)')      'ASCII'
  write(1, '(a)')      'DATASET RECTILINEAR_GRID'
  write(1, '(a,3i6)')  'DIMENSIONS',1, mesh % ny, mesh % nz 
  write(1, '(a,i6,a)') 'X_COORDINATES',  1, ' float'
  write(1, '(a)')      '0.0'
  write(1, '(a,i6,a)') 'Y_COORDINATES', mesh % ny, ' float'
  do j = 1, mesh % ny
    write(1, '(es12.4)') mesh % y(j)
  end do
  write(1, '(a,i6,a)') 'Z_COORDINATES', mesh % nz, ' float'
  do k = 1, mesh % nz
    write(1, '(es12.4)') mesh % z(k)
  end do
  write(1, '(a, i6)')  'CELL_DATA',   mesh % n_cells
  write(1, '(a, i6)')  'POINT_DATA',  mesh % n_nodes
  write(1, '(a)')      'FIELD FieldData 3'
  write(1, '(a,i6,a)') 'U-velocity 1', mesh % n_nodes, ' float'
  do k = 1, mesh % nz
    do j = 1, mesh % ny
      write(1, '(es12.4)') arr_1(j,k)
    end do
  end do
  write(1, '(a,i6,a)') 'V-velocity 1', mesh % n_nodes, ' float'
  do k = 1, mesh % nz
    do j = 1, mesh % ny
      write(1, '(es12.4)') arr_2(j,k)
    end do
  end do
  write(1, '(a,i6,a)') 'W-velocity 1', mesh % n_nodes, ' float'
  do k = 1, mesh % nz
    do j = 1, mesh % ny
      write(1, '(es12.4)') arr_3(j,k)
    end do
  end do
  write(1, '(a,i6,a)') 'Temperature 1', mesh % n_nodes, ' float'
  do k = 1, mesh % nz
    do j = 1, mesh % ny
      write(1, '(es12.4)') arr_4(j,k)
    end do
  end do

  close(1)

  end subroutine
