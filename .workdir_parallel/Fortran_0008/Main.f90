program merge_intervals
    implicit none
    integer :: n, i, j
    integer, allocatable :: l(:), r(:)
    integer, allocatable :: sorted_l(:), sorted_r(:)
    integer :: current_start, current_end, total_length
    
    read*, n
    allocate(l(n), r(n))
    allocate(sorted_l(n), sorted_r(n))
    
    do i = 1, n
        read*, l(i), r(i)
    end do
    
    ! Sort intervals by start point (simple bubble sort)
    sorted_l = l
    sorted_r = r
    do i = 1, n-1
        do j = i+1, n
            if (sorted_l(j) < sorted_l(i)) then
                call swap(sorted_l(i), sorted_l(j))
                call swap(sorted_r(i), sorted_r(j))
            end if
        end do
    end do
    
    ! Merge intervals and calculate total length
    total_length = 0
    current_start = sorted_l(1)
    current_end = sorted_r(1)
    
    do i = 2, n
        if (sorted_l(i) <= current_end) then
            current_end = max(current_end, sorted_r(i))
        else
            total_length = total_length + (current_end - current_start)
            current_start = sorted_l(i)
            current_end = sorted_r(i)
        end if
    end do
    
    total_length = total_length + (current_end - current_start)
    
    print*, total_length
    
    deallocate(l, r, sorted_l, sorted_r)
    
contains
    subroutine swap(a, b)
        integer, intent(inout) :: a, b
        integer :: temp
        temp = a
        a = b
        b = temp
    end subroutine swap
    
end program merge_intervals
