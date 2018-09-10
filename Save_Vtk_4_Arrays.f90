!==============================================================================!
  subroutine Save_Vtk_4_Arrays(msh, arr_1, arr_2, arr_3, arr_4, stem_name, ts)
!------------------------------------------------------------------------------!
!   Generate fluctuations without combining mean and rms data                  !
!------------------------------------------------------------------------------!
  use Mesh_Mod, only: Mesh_Type
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type)                     :: msh
  real, dimension(msh%ny-1, msh%nz-1) :: arr_1, arr_2, arr_3, arr_4
  character(len=*)                    :: stem_name
  integer                             :: ts
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
  write(1, '(a, i6)')  'CELL_DATA',   msh % n_cells
  write(1, '(a)')      'FIELD FieldData 4'
  write(1, '(a,i6,a)') 'U-velocity 1', msh % n_cells, ' float'
  do k = 1, msh % nz - 1
    do j = 1, msh % ny - 1
      write(1, '(es12.4)') arr_1(j,k)
    end do
  end do
  write(1, '(a,i6,a)') 'V-velocity 1', msh % n_cells, ' float'
  do k = 1, msh % nz - 1
    do j = 1, msh % ny - 1
      write(1, '(es12.4)') arr_2(j,k)
    end do
  end do
  write(1, '(a,i6,a)') 'W-velocity 1', msh % n_cells, ' float'
  do k = 1, msh % nz - 1
    do j = 1, msh % ny - 1
      write(1, '(es12.4)') arr_3(j,k)
    end do
  end do
  write(1, '(a,i6,a)') 'Temperature 1', msh % n_cells, ' float'
  do k = 1, msh % nz - 1
    do j = 1, msh % ny - 1
      write(1, '(es12.4)') arr_4(j,k)
    end do
  end do
  write(1, '(a, i6)')  'POINT_DATA',  msh % n_nodes

  close(1)

  end subroutine
