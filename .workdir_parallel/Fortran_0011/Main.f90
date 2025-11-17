program connected_components
    implicit none
    integer :: n, m, i, j, count
    character(len=1), allocatable :: grid(:,:)
    logical, allocatable :: visited(:,:)
    
    read*, n, m
    allocate(grid(n,m))
    allocate(visited(n,m))
    
    do i = 1, n
        read*, (grid(i,j), j=1,m)
    end do
    
    visited = .false.
    count = 0
    
    do i = 1, n
        do j = 1, m
            if (grid(i,j) == '.' .and. .not. visited(i,j)) then
                count = count + 1
                call dfs(i, j, n, m, grid, visited)
            end if
        end do
    end do
    
    print*, count
    
    deallocate(grid)
    deallocate(visited)
    
contains
    
    recursive subroutine dfs(row, col, n, m, grid, visited)
        integer, intent(in) :: row, col, n, m
        character(len=1), intent(in) :: grid(n,m)
        logical, intent(inout) :: visited(n,m)
        integer :: dr(4), dc(4), i, nr, nc
        
        dr = [0, 0, 1, -1]
        dc = [1, -1, 0, 0]
        
        visited(row, col) = .true.
        
        do i = 1, 4
            nr = row + dr(i)
            nc = col + dc(i)
            
            if (nr >= 1 .and. nr <= n .and. nc >= 1 .and. nc <= m) then
                if (grid(nr,nc) == '.' .and. .not. visited(nr,nc)) then
                    call dfs(nr, nc, n, m, grid, visited)
                end if
            end if
        end do
    end subroutine dfs
    
end program connected_components
