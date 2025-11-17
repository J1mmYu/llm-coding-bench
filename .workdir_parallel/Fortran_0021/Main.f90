program wc
    implicit none
    integer :: ios, lines, words, bytes, i, len_trim
    character(len=10000) :: line
    character :: c, prev_c
    logical :: in_word
    
    lines = 0
    words = 0
    bytes = 0
    
    do
        read(*, '(A)', iostat=ios, advance='no', size=i) line
        
        if (ios > 0) then
            exit
        else if (ios < 0) then
            if (i > 0) then
                lines = lines + 1
                bytes = bytes + i
                
                in_word = .false.
                do i = 1, len_trim(line)
                    c = line(i:i)
                    if (c /= ' ' .and. c /= char(9) .and. c /= char(10) .and. c /= char(13)) then
                        if (.not. in_word) then
                            words = words + 1
                            in_word = .true.
                        end if
                    else
                        in_word = .false.
                    end if
                end do
            end if
            exit
        else
            lines = lines + 1
            bytes = bytes + i + 1
            
            in_word = .false.
            do i = 1, len_trim(line)
                c = line(i:i)
                if (c /= ' ' .and. c /= char(9) .and. c /= char(10) .and. c /= char(13)) then
                    if (.not. in_word) then
                        words = words + 1
                        in_word = .true.
                    end if
                else
                    in_word = .false.
                end if
            end do
        end if
    end do
    
    print '(I0, 1X, I0, 1X, I0)', lines, words, bytes
end program wc
