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

  u_in(1:ny,1:nz) = 0.0
  v_in(1:ny,1:nz) = 0.0
  w_in(1:ny,1:nz) = 0.0
  t_in(1:ny,1:nz) = 0.0

  do k = 1, nz
    do j = 1, ny

      do e = 1, n_eddies
        x0 = (0    - eddy(e) % x) / eddy(e) % len
        y0 = (y(j) - eddy(e) % y) / eddy(e) % len
        z0 = (z(k) - eddy(e) % z) / eddy(e) % len

        !--------------------!
        !   Shape function   !
        !--------------------!
        if ( abs(x0) <=1 .and. abs(y0) <=1 .and. abs(z0) <=1) then
          f = sqrt(1.5) * (1- abs(x0)) *                        &
              sqrt(1.5) * (1- abs(y0)) *                        &
              sqrt(1.5) * (1- abs(z0))

          u_in(j,k) = u_in(j,k) +                               &
                         sqrt(v_b/eddy(e) % len**3) *           &
                         eddy(e) % x_int*f

          v_in(j,k) = v_in(j,k) +                               &
                        sqrt(v_b/eddy(e) % len**3) *            &
                        eddy(e) % y_int*f

          w_in(j,k) = w_in(j,k) +                               &
                        sqrt(v_b/eddy(e) % len**3) *            &
                        eddy(e) % z_int*f

          t_in(j,k) = t_in(j,k) +                               &
                        sqrt(v_b/eddy(e) % len**3) *            &
                        eddy(e) % t_int*f
        end if
      end do

    end do
  end do

  u_in(1:ny,1:nz) = u_in(1:ny,1:nz) / sqrt(real(n_eddies, 8))
  v_in(1:ny,1:nz) = v_in(1:ny,1:nz) / sqrt(real(n_eddies, 8))
  w_in(1:ny,1:nz) = w_in(1:ny,1:nz) / sqrt(real(n_eddies, 8))
  t_in(1:ny,1:nz) = t_in(1:ny,1:nz) / sqrt(real(n_eddies, 8))

  if( mod(ts,10) .eq. 0) then
    file_name = 'raw-velocities-00000.vtk'
    write(file_name(16:20), '(i5.5)') ts

    open(1, file=trim(file_name))

    write(1, '(a)')      '# vtk DataFile Version 3.0'
    write(1, '(a)')      'vtk output'
    write(1, '(a)')      'ASCII'
    write(1, '(a)')      'DATASET RECTILINEAR_GRID'
    write(1, '(a,3i6)')  'DIMENSIONS',1, ny, nz 
    write(1, '(a,i6,a)') 'X_COORDINATES',  1, ' float'
    write(1, '(a)')      '0.0'
    write(1, '(a,i6,a)') 'Y_COORDINATES', ny, ' float'
    do j = 1, ny
      write(1, '(es12.4)') y(j)
    end do
    write(1, '(a,i6,a)') 'Z_COORDINATES', nz, ' float'
    do k = 1, nz
      write(1, '(es12.4)') z(k)
    end do
    write(1, '(a, i6)')  'CELL_DATA',  (ny-1)*(nz-1)
    write(1, '(a, i6)')  'POINT_DATA',  ny * nz
    write(1, '(a)')      'FIELD FieldData 3'
    write(1, '(a,i6,a)') 'U-velocity 1', ny*nz, ' float'
    do k = 1, nz
      do j = 1, ny
        write(1, '(es12.4)') u_in(j,k)
      end do
    end do
    write(1, '(a,i6,a)') 'V-velocity 1', ny*nz, ' float'
    do k = 1, nz
      do j = 1, ny
        write(1, '(es12.4)') v_in(j,k)
      end do
    end do
    write(1, '(a,i6,a)') 'W-velocity 1', ny*nz, ' float'
    do k = 1, nz
      do j = 1, ny
        write(1, '(es12.4)') w_in(j,k)
      end do
    end do
  end if

  close(1)

  end subroutine
