program sum_of_digits
    implicit none
    integer :: n, digit_sum, digit
    
    read*, n
    
    n = abs(n)
    digit_sum = 0
    
    do while (n > 0)
        digit = mod(n, 10)
        digit_sum = digit_sum + digit
        n = n / 10
    end do
    
    print*, digit_sum
    
end program sum_of_digits
