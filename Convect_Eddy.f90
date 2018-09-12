!==============================================================================!
  subroutine Convect_Eddy(flw, ed, dt)
!------------------------------------------------------------------------------!
!   Convect each eddies by convective velocity                                 !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Eddy_Mod, only: Eddy_Type
  use Flow_Mod, only: Flow_Type
  use Mesh_Mod, only: Mesh_Type
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Flow_Type) :: flw
  type(Eddy_Type) :: ed
  real            :: dt
!-----------------------------------[Locals]-----------------------------------!
  integer                 :: e, random_cell
  real                    :: y_s, y_e, z_s, z_e, u_conv, v_conv, w_conv, tmp_z
  real                    :: tmp(1:6)
  type(Mesh_Type), target :: msh
!==============================================================================!

  ! Take mesh pointer
  msh = flw % pnt_mesh

  ! Determin the bounding box for the eddies
  y_s = minval(msh % yn(:))  - ed % sigma
  y_e = maxval(msh % yn(:))  + ed % sigma
  z_s = minval(msh % zn(:))  - ed % sigma
  z_e = maxval(msh % zn(:))  + ed % sigma

  u_conv = sum(flw % u % com) / real(msh % n_cells)
  v_conv = sum(flw % v % com) / real(msh % n_cells)
  w_conv = sum(flw % w % com) / real(msh % n_cells)

  do e = 1, ed % n_eddies
    ed % x(e) = ed % x(e) + u_conv * dt
    ed % y(e) = ed % y(e) + v_conv * dt
    ed % z(e) = ed % z(e) + w_conv * dt

    if ( (ed % x(e) -(-ed % sigma)) *          &
         (ed % x(e) -( ed % sigma)) > 0 .or.   &
         (ed % y(e) - y_s) *              &
         (ed % y(e) - y_e) > 0 ) then

      ed % len(e) =   ed % sigma
      ed % x(e)   = - ed % sigma

      call random_number(tmp)

      ed % x(e) = 0 - ed % sigma

      random_cell = int( tmp(2) * real(msh % n_cells) )
      ed % y(e) = msh % yc(random_cell)
      ed % z(e) = msh % zc(random_cell)

      ed % x_int(e) = sign(1.0,tmp(3)-0.5)
      ed % y_int(e) = sign(1.0,tmp(4)-0.5)
      ed % z_int(e) = sign(1.0,tmp(5)-0.5)
      ed % t_int(e) = sign(1.0,tmp(6)-0.5)
    end if

    !----------------------------------!
    !   Periodic boundary conditions   !
    !----------------------------------!
    if ( ed % z(e) < z_s ) then
      tmp_z = z_s - ed % z(e)
      ed % z(e) = z_e - tmp_z
    end if

    if ( ed % z(e) > z_e ) then
      tmp_z = ed % z(e) - z_e
      ed % z(e) = z_s + tmp_z
    end if

  end do

  end subroutine
