program rev
  implicit none
  character(len=1000) :: s
  integer :: i, n
  s = ''
  read(*,'(A)') s
  n = len_trim(s)
  do i = n,1,-1
     write(*,'(A)',advance='no') s(i:i)
  end do
  write(*,*)
end program
