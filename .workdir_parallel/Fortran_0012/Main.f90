program rle
    implicit none
    character(len=1000) :: line
    integer :: i, count, len_line
    character(len=1) :: current_char
    
    read(*,'(A)') line
    len_line = len_trim(line)
    
    if (len_line == 0) then
        stop
    end if
    
    current_char = line(1:1)
    count = 1
    
    do i = 2, len_line
        if (line(i:i) == current_char) then
            count = count + 1
        else
            write(*,'(A,I0)',advance='no') current_char, count
            current_char = line(i:i)
            count = 1
        end if
    end do
    
    write(*,'(A,I0)') current_char, count
    
end program rle
