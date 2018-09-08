!==============================================================================!
  subroutine Combine_Slice(ts)
!------------------------------------------------------------------------------!
!   Combine slice mean,rms data with generated fluctuation variables           !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: ts
!-----------------------------------[Locals]-----------------------------------!
  integer           :: i, j, k
  real              :: r_loc(4,4), a(4,4),                               &
                       u_ins(4,1), u_mean(4,1), u_fluc(4,1), u_tmp(4,1)
  character(len=80) :: file_name
!==============================================================================!

  do k = 1,nz
    do j = 1,ny
      a(1:4,1:4)     = 0.0
      r_loc(1:4,1:4) = 0.0
      u_ins(1:4,1)   = 0.0
      u_fluc(1:4,1)  = 0.0
      u_mean(1:4,1)  = 0.0
      u_tmp(1:4,1)   = 0.0

      u_mean(1:4,1) = (/u(j,k),v(j,k),w(j,k),t(j,k)/)
      u_tmp(1:4,1)  = (/u_in(j,k),v_in(j,k),                    &
                        w_in(j,k),t_in(j,k)/)
      r_loc(1,1:4)  = (/ rs(1,j,k), rs(4,j,k), rs(5,j,k), ths(2,j,k) /)
      r_loc(2,1:4)  = (/ rs(4,j,k), rs(2,j,k), rs(6,j,k), ths(3,j,k) /)
      r_loc(3,1:4)  = (/ rs(5,j,k), rs(6,j,k), rs(3,j,k), ths(4,j,k) /)
      r_loc(4,1:4)  = (/ths(2,j,k),ths(3,j,k),ths(4,j,k), ths(1,j,k) /)

      call Cholesky(a,r_loc,4)
      call Mat_Mul(a,u_tmp,u_fluc,4,1,4)

      u_ins(1:4,1) = u_mean(1:4,1) + u_fluc(1:4,1)

      u_comb(j,k) = u_ins(1,1)
      v_comb(j,k) = u_ins(2,1)
      w_comb(j,k) = u_ins(3,1)
      t_comb(j,k) = u_ins(4,1)

!@    if(mod(ts,10) .eq. 0) then
!@      u3d_comb(ts/10,j,k) = u_ins(1,1)
!@      v3d_comb(ts/10,j,k) = u_ins(2,1)
!@      w3d_comb(ts/10,j,k) = u_ins(3,1)
!@      t3d_comb(ts/10,j,k) = u_ins(4,1)
!@    end if
    end do
  end do

  if( mod(ts,10) .eq. 0) then
    file_name = 'scaled-velocities-00000.vtk'
    write(file_name(19:23), '(i5.5)') ts

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
        write(1, '(es12.4)') u_comb(j,k)
      end do
    end do
    write(1, '(a,i6,a)') 'V-velocity 1', ny*nz, ' float'
    do k = 1, nz
      do j = 1, ny
        write(1, '(es12.4)') v_comb(j,k)
      end do
    end do
    write(1, '(a,i6,a)') 'W-velocity 1', ny*nz, ' float'
    do k = 1, nz
      do j = 1, ny
        write(1, '(es12.4)') w_comb(j,k)
      end do
    end do
  end if

  close(1)

!@if(ts .eq. n_dt) then
!@  write(2, '(a)')      '# vtk DataFile Version 3.0'
!@  write(2, '(a)')      'vtk output'
!@  write(2, '(a)')      'ASCII'
!@  write(2, '(a)')      'DATASET RECTILINEAR_GRID'
!@  write(2, '(a,3i9)')  'DIMENSIONS',ts, ny, nz 
!@  write(2, '(a,i9,a)') 'X_COORDINATES', ts, ' float'
!@  do i = 1, ts
!@    write(2, '(es12.4)') 2.5 * dt * real(i-1)
!@  end do
!@  write(2, '(a,i9,a)') 'Y_COORDINATES', ny, ' float'
!@  do j = 1, ny
!@    write(2, '(es12.4)') y(j)
!@  end do
!@  write(2, '(a,i9,a)') 'Z_COORDINATES', nz, ' float'
!@  do k = 1, nz
!@    write(2, '(es12.4)') z(k)
!@  end do
!@  write(2, '(a, i9)')  'CELL_DATA',  (ts-1)*(ny-1)*(nz-1)
!@  write(2, '(a, i9)')  'POINT_DATA',  ts * ny * nz
!@  write(2, '(a)')      'FIELD FieldData 3'
!@  write(2, '(a,i9,a)') 'U-velocity 1', ts*ny*nz, ' float'
!@  do k = 1, nz
!@    do j = 1, ny
!@      do i = 1, ts
!@        write(2, '(es12.4)') u3d_comb(i,j,k)
!@      end do
!@    end do
!@  end do
!@  write(2, '(a,i9,a)') 'V-velocity 1', ts*ny*nz, ' float'
!@  do k = 1, nz
!@    do j = 1, ny
!@      do i = 1, ts
!@        write(2, '(es12.4)') v3d_comb(i,j,k)
!@      end do
!@    end do
!@  end do
!@  write(2, '(a,i9,a)') 'W-velocity 1', ts*ny*nz, ' float'
!@  do k = 1, nz
!@    do j = 1, ny
!@      do i = 1, ts
!@        write(2, '(es12.4)') w3d_comb(i,j,k)
!@      end do
!@    end do
!@  end do
!@end if

  end subroutine
