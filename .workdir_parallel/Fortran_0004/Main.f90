program balanced_brackets
    implicit none
    character(len=10000) :: line
    character(len=10000) :: stack
    integer :: i, top, n
    character :: ch, expected
    logical :: balanced
    
    read(*,'(A)') line
    n = len_trim(line)
    
    top = 0
    balanced = .true.
    
    do i = 1, n
        ch = line(i:i)
        
        if (ch == '(' .or. ch == '[' .or. ch == '{') then
            top = top + 1
            stack(top:top) = ch
        else if (ch == ')' .or. ch == ']' .or. ch == '}') then
            if (top == 0) then
                balanced = .false.
                exit
            end if
            
            if (ch == ')') then
                expected = '('
            else if (ch == ']') then
                expected = '['
            else if (ch == '}') then
                expected = '{'
            end if
            
            if (stack(top:top) /= expected) then
                balanced = .false.
                exit
            end if
            
            top = top - 1
        end if
    end do
    
    if (balanced .and. top == 0) then
        print *, 'YES'
    else
        print *, 'NO'
    end if
    
end program balanced_brackets
