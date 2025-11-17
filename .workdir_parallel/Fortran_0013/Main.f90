program big_integer_sum
    implicit none
    character(len=1000) :: a, b, result
    integer :: len_a, len_b, len_max, i, carry, digit_a, digit_b, sum_digit
    integer, allocatable :: num_a(:), num_b(:), num_result(:)
    
    read(*,*) a
    read(*,*) b
    
    a = adjustl(a)
    b = adjustl(b)
    len_a = len_trim(a)
    len_b = len_trim(b)
    len_max = max(len_a, len_b) + 1
    
    allocate(num_a(len_max))
    allocate(num_b(len_max))
    allocate(num_result(len_max))
    
    num_a = 0
    num_b = 0
    num_result = 0
    
    do i = 1, len_a
        num_a(len_max - len_a + i) = ichar(a(i:i)) - ichar('0')
    end do
    
    do i = 1, len_b
        num_b(len_max - len_b + i) = ichar(b(i:i)) - ichar('0')
    end do
    
    carry = 0
    do i = len_max, 1, -1
        sum_digit = num_a(i) + num_b(i) + carry
        num_result(i) = mod(sum_digit, 10)
        carry = sum_digit / 10
    end do
    
    result = ''
    i = 1
    do while (i <= len_max .and. num_result(i) == 0)
        i = i + 1
    end do
    
    if (i > len_max) then
        print *, '0'
    else
        do while (i <= len_max)
            result = trim(result) // char(num_result(i) + ichar('0'))
            i = i + 1
        end do
        print *, trim(result)
    end if
    
    deallocate(num_a)
    deallocate(num_b)
    deallocate(num_result)
    
end program big_integer_sum
