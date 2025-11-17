program longest_increasing_subsequence
    implicit none
    integer :: n, i, j, max_len
    integer, allocatable :: arr(:), dp(:)
    
    read(*,*) n
    allocate(arr(n), dp(n))
    
    read(*,*) (arr(i), i=1,n)
    
    dp = 1
    
    do i = 2, n
        do j = 1, i-1
            if (arr(j) < arr(i)) then
                dp(i) = max(dp(i), dp(j) + 1)
            end if
        end do
    end do
    
    max_len = maxval(dp)
    print *, max_len
    
    deallocate(arr, dp)
end program longest_increasing_subsequence
