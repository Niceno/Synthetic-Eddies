!==============================================================================!
  subroutine Cholesky(a,r,n)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer      :: n
  real(kind=8) :: a(n,n)
  real(kind=8) :: r(n,n)
!-----------------------------------[Locals]-----------------------------------!
  integer                 :: i, j, k
  real(kind=8), parameter :: eps = 1.0e-8
!==============================================================================!

  a(:,:) = 0.0

  do i = 1,n
    do j = 1,i
      a(i,j) = r(i,j)

      if (i==j) then
        do k = 1,j-1
          a(i,j) = a(i,j) - a(j,k)**2
        end do
        a(i,j) = sqrt(abs(a(i,j)))

      else
        do k = 1,j-1
          a(i,j) = a(i,j) - a(i,k)*a(j,k)
        end do
        a(i,j) = a(i,j)/(a(j,j) + eps)

      end if

    end do
  end do

  end subroutine
