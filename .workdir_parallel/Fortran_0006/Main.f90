program parse_csv
    implicit none
    character(len=10000) :: line
    character(len=1000), allocatable :: fields(:)
    integer :: field_count
    
    read(*, '(A)') line
    call parse_csv_line(trim(line), fields, field_count)
    print *, field_count
    
contains

    subroutine parse_csv_line(line, fields, field_count)
        character(len=*), intent(in) :: line
        character(len=1000), allocatable, intent(out) :: fields(:)
        integer, intent(out) :: field_count
        integer :: i, len_line, start_pos
        character(len=1000) :: current_field
        logical :: in_quotes
        character(len=1) :: ch, next_ch
        integer :: field_pos
        integer, parameter :: max_fields = 1000
        character(len=1000) :: temp_fields(max_fields)
        
        field_count = 0
        len_line = len_trim(line)
        if (len_line == 0) return
        
        i = 1
        in_quotes = .false.
        current_field = ''
        field_pos = 0
        
        do while (i <= len_line)
            ch = line(i:i)
            
            if (in_quotes) then
                if (ch == '"') then
                    if (i < len_line) then
                        next_ch = line(i+1:i+1)
                        if (next_ch == '"') then
                            field_pos = field_pos + 1
                            current_field(field_pos:field_pos) = '"'
                            i = i + 2
                            cycle
                        else
                            in_quotes = .false.
                            i = i + 1
                            cycle
                        end if
                    else
                        in_quotes = .false.
                        i = i + 1
                        cycle
                    end if
                else
                    field_pos = field_pos + 1
                    current_field(field_pos:field_pos) = ch
                    i = i + 1
                end if
            else
                if (ch == '"') then
                    in_quotes = .true.
                    i = i + 1
                else if (ch == ',') then
                    field_count = field_count + 1
                    temp_fields(field_count) = current_field(1:field_pos)
                    current_field = ''
                    field_pos = 0
                    i = i + 1
                else
                    field_pos = field_pos + 1
                    current_field(field_pos:field_pos) = ch
                    i = i + 1
                end if
            end if
        end do
        
        field_count = field_count + 1
        temp_fields(field_count) = current_field(1:field_pos)
        
        allocate(fields(field_count))
        fields(1:field_count) = temp_fields(1:field_count)
        
    end subroutine parse_csv_line

end program parse_csv
