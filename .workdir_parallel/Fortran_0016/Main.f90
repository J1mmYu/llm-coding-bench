program calculator
    implicit none
    character(len=1000) :: line
    integer :: pos
    real :: result
    
    read(*,'(A)') line
    pos = 1
    result = parse_expr(line, pos)
    print *, int(result)
    
contains

    recursive function parse_expr(s, p) result(val)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: p
        real :: val, right
        character(len=1) :: op
        
        val = parse_term(s, p)
        do while (p <= len_trim(s))
            call skip_spaces(s, p)
            if (p > len_trim(s)) exit
            if (s(p:p) /= '+' .and. s(p:p) /= '-') exit
            op = s(p:p)
            p = p + 1
            right = parse_term(s, p)
            if (op == '+') then
                val = val + right
            else
                val = val - right
            end if
        end do
    end function parse_expr
    
    recursive function parse_term(s, p) result(val)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: p
        real :: val, right
        character(len=1) :: op
        
        val = parse_factor(s, p)
        do while (p <= len_trim(s))
            call skip_spaces(s, p)
            if (p > len_trim(s)) exit
            if (s(p:p) /= '*' .and. s(p:p) /= '/') exit
            op = s(p:p)
            p = p + 1
            right = parse_factor(s, p)
            if (op == '*') then
                val = val * right
            else
                val = val / right
            end if
        end do
    end function parse_term
    
    recursive function parse_factor(s, p) result(val)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: p
        real :: val
        
        call skip_spaces(s, p)
        if (s(p:p) == '(') then
            p = p + 1
            val = parse_expr(s, p)
            call skip_spaces(s, p)
            p = p + 1  ! skip ')'
        else
            val = parse_number(s, p)
        end if
    end function parse_factor
    
    function parse_number(s, p) result(val)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: p
        real :: val
        integer :: start
        
        call skip_spaces(s, p)
        start = p
        do while (p <= len_trim(s) .and. (s(p:p) >= '0' .and. s(p:p) <= '9'))
            p = p + 1
        end do
        read(s(start:p-1), *) val
    end function parse_number
    
    subroutine skip_spaces(s, p)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: p
        
        do while (p <= len_trim(s) .and. s(p:p) == ' ')
            p = p + 1
        end do
    end subroutine skip_spaces

end program calculator
