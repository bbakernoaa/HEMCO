program test_core
  use HCO_PRECISION_MOD
  use HCO_Regrid_A2A_Mod
  implicit none

  integer, parameter :: nx = 10, ny = 10
  real(f8) :: lon1(nx+1), sin1(ny+1), q1(nx,ny)
  real(f8) :: lon2(nx+1), sin2(ny+1), q2(nx,ny)
  integer :: i, j

  print *, "Testing Map_A2A..."

  do i = 1, nx+1
     lon1(i) = real(i-1, f8)
     lon2(i) = real(i-1, f8)
  end do
  do j = 1, ny+1
     sin1(j) = real(j-ny/2-1, f8) / real(ny/2, f8)
     sin2(j) = real(j-ny/2-1, f8) / real(ny/2, f8)
  end do

  q1 = 1.0_f8

  call Map_A2A(nx, ny, lon1, sin1, q1, &
               nx, ny, lon2, sin2, q2, 0, 0)

  if (all(abs(q2 - 1.0_f8) < 1e-10_f8)) then
     print *, "Map_A2A PASSED"
  else
     print *, "Map_A2A FAILED"
     print *, "q2 sum: ", sum(q2)
     stop 1
  end if

  print *, "Core tests PASSED"
end program test_core
