program longest_increasing
    implicit none
    integer :: n, i, current_length, max_length
    integer, allocatable :: arr(:)
    
    read*, n
    allocate(arr(n))
    
    do i = 1, n
        read*, arr(i)
    end do
    
    max_length = 1
    current_length = 1
    
    do i = 2, n
        if (arr(i) > arr(i-1)) then
            current_length = current_length + 1
            if (current_length > max_length) then
                max_length = current_length
            end if
        else
            current_length = 1
        end if
    end do
    
    print*, max_length
    
    deallocate(arr)
end program longest_increasing
