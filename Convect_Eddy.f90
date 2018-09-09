!==============================================================================!
  subroutine Convect_Eddy
!------------------------------------------------------------------------------!
!   Convect each eddies by convective velocity                                 !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Mesh_Mod
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: e
  real    :: y_s, y_e, z_s, z_e, u_conv, v_conv, w_conv, tmp_z
  real    :: tmp(1:6)
!==============================================================================!

  ! Determin the bounding box for the eddies
  y_s = minval(mesh % y(:))  - sigma
  y_e = maxval(mesh % y(:))  + sigma
  z_s = minval(mesh % z(:))  - sigma
  z_e = maxval(mesh % z(:))  + sigma

  u_conv = sum(u % com) / (mesh % ny * mesh % nz)
  v_conv = sum(v % com) / (mesh % ny * mesh % nz)
  w_conv = sum(w % com) / (mesh % ny * mesh % nz)

  do e = 1, eddy % n_eddies
    eddy % x(e) = eddy % x(e) + u_conv * dt
    eddy % y(e) = eddy % y(e) + v_conv * dt
    eddy % z(e) = eddy % z(e) + w_conv * dt

    if ( (eddy % x(e) -(-sigma)) *          &
         (eddy % x(e) -( sigma)) > 0 .or.   &
         (eddy % y(e) - y_s) *              &
         (eddy % y(e) - y_e) > 0 ) then

      eddy % len(e) =   sigma
      eddy % x(e)   = - sigma

      call random_number(tmp)

      eddy % x(e) = 0 - sigma
      eddy % y(e) = y_s + (y_e-y_s)*tmp(1)
      eddy % z(e) = z_s + (z_e-z_s)*tmp(2)

      eddy % x_int(e) = intensity_det(tmp(3)-0.5)
      eddy % y_int(e) = intensity_det(tmp(4)-0.5)
      eddy % z_int(e) = intensity_det(tmp(5)-0.5)
      eddy % t_int(e) = intensity_det(tmp(6)-0.5)
    end if

    !----------------------------------!
    !   Periodic boundary conditions   !
    !----------------------------------!
    if ( eddy % z(e) < z_s ) then
      tmp_z = z_s - eddy % z(e)
      eddy % z(e) = z_e - tmp_z
    end if

    if ( eddy % z(e) > z_e ) then
      tmp_z = eddy % z(e) - z_e
      eddy % z(e) = z_s + tmp_z
    end if

  end do

  end subroutine
