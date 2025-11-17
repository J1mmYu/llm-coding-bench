program word_frequency
    implicit none
    character(len=10000) :: line
    character(len=100), allocatable :: words(:), unique_words(:)
    integer, allocatable :: counts(:)
    integer :: i, j, n_words, n_unique, ios
    character(len=100) :: temp_word
    integer :: temp_count
    logical :: found
    
    ! Read the line
    read(*, '(A)', iostat=ios) line
    if (ios /= 0) stop
    
    ! Count words by splitting on spaces
    n_words = 0
    allocate(words(10000))
    
    call split_line(line, words, n_words)
    
    ! Count unique words
    allocate(unique_words(n_words))
    allocate(counts(n_words))
    n_unique = 0
    
    do i = 1, n_words
        found = .false.
        do j = 1, n_unique
            if (trim(words(i)) == trim(unique_words(j))) then
                counts(j) = counts(j) + 1
                found = .true.
                exit
            end if
        end do
        if (.not. found) then
            n_unique = n_unique + 1
            unique_words(n_unique) = words(i)
            counts(n_unique) = 1
        end if
    end do
    
    ! Sort by count (descending), then by word (ascending)
    do i = 1, n_unique - 1
        do j = i + 1, n_unique
            if (counts(j) > counts(i) .or. &
                (counts(j) == counts(i) .and. trim(unique_words(j)) < trim(unique_words(i)))) then
                temp_count = counts(i)
                counts(i) = counts(j)
                counts(j) = temp_count
                temp_word = unique_words(i)
                unique_words(i) = unique_words(j)
                unique_words(j) = temp_word
            end if
        end do
    end do
    
    ! Print results
    do i = 1, n_unique
        write(*, '(A,1X,I0)') trim(unique_words(i)), counts(i)
    end do
    
contains
    
    subroutine split_line(str, words_out, n)
        character(len=*), intent(in) :: str
        character(len=100), intent(out) :: words_out(:)
        integer, intent(out) :: n
        integer :: i, start, str_len
        logical :: in_word
        
        n = 0
        in_word = .false.
        start = 1
        str_len = len_trim(str)
        
        do i = 1, str_len
            if (str(i:i) /= ' ' .and. .not. in_word) then
                in_word = .true.
                start = i
            else if ((str(i:i) == ' ' .or. i == str_len) .and. in_word) then
                n = n + 1
                if (str(i:i) == ' ') then
                    words_out(n) = str(start:i-1)
                else
                    words_out(n) = str(start:i)
                end if
                in_word = .false.
            end if
        end do
        
    end subroutine split_line
    
end program word_frequency
