!==============================================================================!
  subroutine Convect_Eddy
!------------------------------------------------------------------------------!
!   Convect each eddies by convective velocity                                 !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: e
  real    :: y_s, y_e, z_s, z_e, u_conv, v_conv, w_conv, tmp_z
  real    :: tmp(1:6)
!==============================================================================!

  y_s = y(1) - sigma
  y_e   = y(ny) + sigma

  z_s = z(1) - sigma
  z_e   = z(nz) + sigma

  u_conv = sum(u_comb)/(ny*nz)
  v_conv = sum(v_comb)/(ny*nz)
  w_conv = sum(w_comb)/(ny*nz)

  do e = 1, n_eddies
    eddy(e) % x = eddy(e) % x + u_conv * dt
    eddy(e) % y = eddy(e) % y + v_conv * dt
    eddy(e) % z = eddy(e) % z + w_conv * dt

    if ( (eddy(e) % x -(-sigma)) *          &
         (eddy(e) % x -( sigma)) > 0 .or.   &
         (eddy(e) % y - y_s) *              &
         (eddy(e) % y - y_e) > 0 ) then

      eddy(e) % len =   sigma
      eddy(e) % x   = - sigma

      call random_number(tmp)

      eddy(e) % x = 0 - sigma
      eddy(e) % y = y_s + (y_e-y_s)*tmp(1)
      eddy(e) % z = z_s + (z_e-z_s)*tmp(2)

      eddy(e) % x_int = intensity_det(tmp(3)-0.5)
      eddy(e) % y_int = intensity_det(tmp(4)-0.5)
      eddy(e) % z_int = intensity_det(tmp(5)-0.5)
      eddy(e) % t_int = intensity_det(tmp(6)-0.5)
    end if

    !----------------------------------!
    !   Periodic boundary conditions   !
    !----------------------------------!
    if ( eddy(e) % z < z_s ) then
      tmp_z = z_s - eddy(e) % z
      eddy(e) % z = z_e - tmp_z
    end if

    if ( eddy(e) % z > z_e ) then
      tmp_z = eddy(e) % z - z_e
      eddy(e) % z = z_s + tmp_z
    end if

  end do

  end subroutine
