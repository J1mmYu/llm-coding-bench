program factorial_divisibility
    implicit none
    integer :: n, k, fact, i
    logical :: found
    
    read*, n
    
    found = .false.
    do k = 1, 12
        fact = 1
        do i = 1, k
            fact = fact * i
        end do
        
        if (mod(fact, n) == 0) then
            print*, k
            found = .true.
            exit
        end if
    end do
    
    if (.not. found) then
        print*, -1
    end if
    
end program factorial_divisibility
