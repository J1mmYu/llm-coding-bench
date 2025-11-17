program sum_n
  implicit none
  integer :: n
  integer(kind=8) :: s
  read(*,*) n
  s = int(n,8)*(int(n,8)+1)/2
  print '(I0)', s
end program
