program longest_subarray
    implicit none
    integer :: n, i, j, max_len
    integer, allocatable :: arr(:)
    integer(kind=8) :: sum
    integer(kind=8), allocatable :: prefix(:)
    integer, allocatable :: first_occurrence(:)
    integer :: min_idx, max_idx
    integer(kind=8) :: min_sum, max_sum
    
    read*, n
    allocate(arr(n))
    allocate(prefix(0:n))
    
    do i = 1, n
        read*, arr(i)
    end do
    
    ! Calculate prefix sums
    prefix(0) = 0
    do i = 1, n
        prefix(i) = prefix(i-1) + arr(i)
    end do
    
    ! Find min and max prefix sums to determine hash table size
    min_sum = minval(prefix)
    max_sum = maxval(prefix)
    
    min_idx = int(min_sum)
    max_idx = int(max_sum)
    
    allocate(first_occurrence(min_idx:max_idx))
    first_occurrence = -1
    
    max_len = 0
    
    do i = 0, n
        sum = prefix(i)
        if (first_occurrence(int(sum)) == -1) then
            first_occurrence(int(sum)) = i
        else
            max_len = max(max_len, i - first_occurrence(int(sum)))
        end if
    end do
    
    print*, max_len
    
    deallocate(arr)
    deallocate(prefix)
    deallocate(first_occurrence)
    
end program longest_subarray
