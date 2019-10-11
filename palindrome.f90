program palindro
    
    implicit none
    integer :: i
    read(*,*) i
    print *, is_palindrome(i)
contains

function is_palindrome(inp) result(isp)
    implicit none
    integer :: inp, back
    logical :: isp
    if(inp<0.or.(MOD(inp,10)==0.and.inp/=0)) then
        isp = .false.
        return
    end if
    back = 0
    do while(inp>back)
        back = back*10 + mod(inp,10)
        inp = inp/10
    end do
    if(inp==back) then
        isp = .true.
    elseif(inp==back/10) then
        isp = .true.
    else
        isp = .false.
    end if
    end function is_palindrome
end program palindro

