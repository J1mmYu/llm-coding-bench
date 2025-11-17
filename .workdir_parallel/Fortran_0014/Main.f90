program topological_sort
    implicit none
    integer :: n, m, i, j, u, v
    integer, allocatable :: adj(:,:), indegree(:), result(:)
    integer :: queue(100000), front, rear, count, node
    logical :: has_cycle
    
    read(*,*) n, m
    
    allocate(adj(n, n))
    allocate(indegree(n))
    allocate(result(n))
    
    adj = 0
    indegree = 0
    
    do i = 1, m
        read(*,*) u, v
        if (adj(u, v) == 0) then
            adj(u, v) = 1
            indegree(v) = indegree(v) + 1
        end if
    end do
    
    front = 1
    rear = 0
    
    do i = 1, n
        if (indegree(i) == 0) then
            rear = rear + 1
            queue(rear) = i
        end if
    end do
    
    count = 0
    
    do while (front <= rear)
        node = queue(front)
        front = front + 1
        count = count + 1
        result(count) = node
        
        do i = 1, n
            if (adj(node, i) == 1) then
                indegree(i) = indegree(i) - 1
                if (indegree(i) == 0) then
                    rear = rear + 1
                    queue(rear) = i
                end if
            end if
        end do
    end do
    
    if (count /= n) then
        print *, -1
    else
        do i = 1, n
            if (i == n) then
                print *, result(i)
            else
                write(*, '(I0, A)', advance='no') result(i), ' '
            end if
        end do
    end if
    
    deallocate(adj)
    deallocate(indegree)
    deallocate(result)
    
end program topological_sort
