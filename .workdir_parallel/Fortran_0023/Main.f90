program node_degrees
    implicit none
    integer :: n, m, i, u, v
    integer, allocatable :: degree(:)
    
    read*, n, m
    allocate(degree(n))
    degree = 0
    
    do i = 1, m
        read*, u, v
        degree(u) = degree(u) + 1
        degree(v) = degree(v) + 1
    end do
    
    do i = 1, n
        if (i == n) then
            write(*, '(I0)') degree(i)
        else
            write(*, '(I0, A)', advance='no') degree(i), ' '
        end if
    end do
    
    deallocate(degree)
end program node_degrees
