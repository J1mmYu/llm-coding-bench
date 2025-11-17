program count_set_bits
    implicit none
    integer :: n, count, temp
    
    read*, n
    
    count = 0
    temp = n
    
    do while (temp > 0)
        if (mod(temp, 2) == 1) then
            count = count + 1
        end if
        temp = temp / 2
    end do
    
    print*, count
end program count_set_bits
