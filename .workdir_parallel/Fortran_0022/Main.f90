program palindrome_check
    implicit none
    character(len=1000) :: line
    character(len=1000) :: cleaned
    integer :: i, j, len_cleaned
    logical :: is_palindrome
    character(len=1) :: c
    
    read(*,'(A)') line
    
    ! Clean the string - keep only alphanumerics and convert to lowercase
    cleaned = ''
    j = 0
    do i = 1, len_trim(line)
        c = line(i:i)
        if ((c >= 'A' .and. c <= 'Z')) then
            j = j + 1
            cleaned(j:j) = char(ichar(c) + 32)
        else if ((c >= 'a' .and. c <= 'z') .or. (c >= '0' .and. c <= '9')) then
            j = j + 1
            cleaned(j:j) = c
        end if
    end do
    len_cleaned = j
    
    ! Check if palindrome
    is_palindrome = .true.
    do i = 1, len_cleaned / 2
        if (cleaned(i:i) /= cleaned(len_cleaned - i + 1:len_cleaned - i + 1)) then
            is_palindrome = .false.
            exit
        end if
    end do
    
    if (is_palindrome) then
        print '(A)', 'YES'
    else
        print '(A)', 'NO'
    end if
    
end program palindrome_check
