!==============================================================================!
  subroutine Generate_Fluctuations(ts)
!------------------------------------------------------------------------------!
!   Generate fluctuations without combining mean and rms data                  !
!------------------------------------------------------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: ts
!-----------------------------------[Locals]-----------------------------------!
  integer           :: e, j, k
  real              :: x0, y0, z0, f
  character(len=80) :: file_name
!==============================================================================!

  u % raw(:,:) = 0.0
  v % raw(:,:) = 0.0
  w % raw(:,:) = 0.0
  t % raw(:,:) = 0.0

  do k = 1, mesh % nz
    do j = 1, mesh % ny

      do e = 1, n_eddies
        x0 = (0           - eddy(e) % x) / eddy(e) % len
        y0 = (mesh % y(j) - eddy(e) % y) / eddy(e) % len
        z0 = (mesh % z(k) - eddy(e) % z) / eddy(e) % len

        !--------------------!
        !   Shape function   !
        !--------------------!
        if ( abs(x0) <=1 .and. abs(y0) <=1 .and. abs(z0) <=1) then
          f = sqrt(1.5) * (1- abs(x0)) *                        &
              sqrt(1.5) * (1- abs(y0)) *                        &
              sqrt(1.5) * (1- abs(z0))

          u % raw(j,k) = u % raw(j,k) +                         &
                         sqrt(v_b/eddy(e) % len**3) *           &
                         eddy(e) % x_int*f

          v % raw(j,k) = v % raw(j,k) +                         &
                        sqrt(v_b/eddy(e) % len**3) *            &
                        eddy(e) % y_int*f

          w % raw(j,k) = w % raw(j,k) +                         &
                        sqrt(v_b/eddy(e) % len**3) *            &
                        eddy(e) % z_int*f

          t % raw(j,k) = t % raw(j,k) +                         &
                        sqrt(v_b/eddy(e) % len**3) *            &
                        eddy(e) % t_int*f
        end if
      end do

    end do
  end do

  u % raw(:,:) = u % raw(:,:) / sqrt(real(n_eddies, 8))
  v % raw(:,:) = v % raw(:,:) / sqrt(real(n_eddies, 8))
  w % raw(:,:) = w % raw(:,:) / sqrt(real(n_eddies, 8))
  t % raw(:,:) = t % raw(:,:) / sqrt(real(n_eddies, 8))

  if( mod(ts,10) .eq. 0) then
    file_name = 'raw-velocities-00000.vtk'
    write(file_name(16:20), '(i5.5)') ts

    open(1, file=trim(file_name))

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
        write(1, '(es12.4)') u % raw(j,k)
      end do
    end do
    write(1, '(a,i6,a)') 'V-velocity 1', mesh % n_nodes, ' float'
    do k = 1, mesh % nz
      do j = 1, mesh % ny
        write(1, '(es12.4)') v % raw(j,k)
      end do
    end do
    write(1, '(a,i6,a)') 'W-velocity 1', mesh % n_nodes, ' float'
    do k = 1, mesh % nz
      do j = 1, mesh % ny
        write(1, '(es12.4)') w % raw(j,k)
      end do
    end do
  end if

  close(1)

  end subroutine
