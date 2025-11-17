program wc
    implicit none
    integer :: lines, words, bytes
    integer :: iostat, i
    character(len=10000) :: line
    character :: ch
    logical :: in_word
    
    lines = 0
    words = 0
    bytes = 0
    
    do
        read(*, '(A)', iostat=iostat) line
        if (iostat /= 0) exit
        
        lines = lines + 1
        bytes = bytes + len_trim(line) + 1
        
        in_word = .false.
        do i = 1, len_trim(line)
            ch = line(i:i)
            if (ch == ' ' .or. ch == char(9) .or. ch == char(10) .or. ch == char(13)) then
                in_word = .false.
            else
                if (.not. in_word) then
                    words = words + 1
                    in_word = .true.
                end if
            end if
        end do
    end do
    
    print '(I0, 1X, I0, 1X, I0)', lines, words, bytes
end program wc
