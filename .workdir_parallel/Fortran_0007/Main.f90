program smallest_substring
    implicit none
    character(len=1000) :: s
    integer :: k, n, i, min_pos
    character(len=1000) :: min_substr, current_substr
    
    read(*,*) s
    read(*,*) k
    
    n = len_trim(s)
    
    min_substr = s(1:k)
    min_pos = 1
    
    do i = 2, n - k + 1
        current_substr = s(i:i+k-1)
        if (llt(current_substr, min_substr)) then
            min_substr = current_substr
            min_pos = i
        end if
    end do
    
    print '(a)', trim(min_substr)
    
end program smallest_substring
