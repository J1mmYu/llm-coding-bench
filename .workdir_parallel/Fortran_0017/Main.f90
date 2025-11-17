program longest_substring
    implicit none
    character(len=100000) :: s
    integer :: n, i, j, maxlen, start
    logical :: found
    character(len=1), allocatable :: current(:)
    
    read(*,'(A)') s
    n = len_trim(s)
    
    maxlen = 0
    
    do i = 1, n
        allocate(current(0))
        do j = i, n
            found = .false.
            do start = 1, size(current)
                if (current(start) == s(j:j)) then
                    found = .true.
                    exit
                end if
            end do
            
            if (found) then
                exit
            else
                current = [current, s(j:j)]
                if (size(current) > maxlen) then
                    maxlen = size(current)
                end if
            end if
        end do
        deallocate(current)
    end do
    
    print *, maxlen
end program longest_substring
