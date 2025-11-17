program max_distinct_chars
    implicit none
    character(len=1000) :: line, max_line
    integer :: ios, max_count, count
    logical :: first_line
    
    first_line = .true.
    max_count = -1
    max_line = ''
    
    do
        read(*, '(A)', iostat=ios) line
        if (ios /= 0) exit
        
        count = count_distinct(trim(line))
        
        if (first_line .or. count > max_count) then
            max_count = count
            max_line = line
            first_line = .false.
        end if
    end do
    
    print '(A)', trim(max_line)
    
contains
    
    integer function count_distinct(str)
        character(len=*), intent(in) :: str
        logical :: seen(0:255)
        integer :: i, char_code
        
        seen = .false.
        count_distinct = 0
        
        do i = 1, len(str)
            char_code = ichar(str(i:i))
            if (.not. seen(char_code)) then
                seen(char_code) = .true.
                count_distinct = count_distinct + 1
            end if
        end do
    end function count_distinct
    
end program max_distinct_chars
