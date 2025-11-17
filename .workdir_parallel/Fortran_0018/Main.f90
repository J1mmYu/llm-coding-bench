program sum_integers
    implicit none
    character(len=1000) :: line
    integer :: i, j, n, total, num, ios
    character(len=20) :: temp
    logical :: in_number
    
    read(*,'(A)') line
    
    total = 0
    temp = ''
    in_number = .false.
    n = len_trim(line)
    
    do i = 1, n
        if (line(i:i) >= '0' .and. line(i:i) <= '9' .or. &
            line(i:i) == '-' .or. line(i:i) == '+') then
            temp = trim(temp) // line(i:i)
            in_number = .true.
        else if (line(i:i) == ',' .or. line(i:i) == ' ') then
            if (in_number .and. len_trim(temp) > 0) then
                read(temp, *, iostat=ios) num
                if (ios == 0) total = total + num
                temp = ''
                in_number = .false.
            end if
        end if
    end do
    
    if (in_number .and. len_trim(temp) > 0) then
        read(temp, *, iostat=ios) num
        if (ios == 0) total = total + num
    end if
    
    print *, total
end program sum_integers
