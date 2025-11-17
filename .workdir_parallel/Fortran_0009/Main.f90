program ipv4_validator
    implicit none
    character(len=100) :: input_str
    integer :: result
    
    read(*,'(A)') input_str
    result = validate_ipv4(trim(input_str))
    print '(I1)', result
    
contains
    
    function validate_ipv4(str) result(valid)
        character(len=*), intent(in) :: str
        integer :: valid
        integer :: parts(4)
        integer :: dot_count, i, j, start_pos, part_idx
        integer :: num, stat
        character(len=20) :: part_str
        logical :: has_digit
        
        valid = 0
        dot_count = 0
        part_idx = 0
        start_pos = 1
        
        ! Count dots
        do i = 1, len_trim(str)
            if (str(i:i) == '.') dot_count = dot_count + 1
        end do
        
        if (dot_count /= 3) return
        
        ! Parse parts
        do i = 1, len_trim(str) + 1
            if (i > len_trim(str) .or. str(i:i) == '.') then
                if (i == start_pos) return  ! empty part
                part_str = str(start_pos:i-1)
                
                ! Check for leading zeros
                if (len_trim(part_str) > 1 .and. part_str(1:1) == '0') return
                
                ! Check all characters are digits
                has_digit = .false.
                do j = 1, len_trim(part_str)
                    if (part_str(j:j) < '0' .or. part_str(j:j) > '9') return
                    has_digit = .true.
                end do
                
                if (.not. has_digit) return
                
                ! Convert to integer
                read(part_str, *, iostat=stat) num
                if (stat /= 0) return
                
                ! Check range
                if (num < 0 .or. num > 255) return
                
                part_idx = part_idx + 1
                if (part_idx > 4) return
                parts(part_idx) = num
                start_pos = i + 1
            end if
        end do
        
        if (part_idx == 4) valid = 1
        
    end function validate_ipv4
    
end program ipv4_validator
