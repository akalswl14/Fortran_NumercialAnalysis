program prime
    
    implicit none
    call prime_a()
    call prime_b()
contains

subroutine prime_a()
    implicit none
    integer :: inp, div, cnt
    logical :: isp
    div = 2
    isp = .true.
    cnt = 0
    print *, 2
    do inp = 3, 9999
        div = 2
        isp = .true.
        do while(div<=inp-1)
            cnt = cnt + 1
            if(mod(inp,div)==0) then
                isp=.false.
                exit
            end if
            div = div + 1
        end do
        if(isp.eqv..true.) then
            print *,inp
        end if
    end do
    print *, 'Method a : The number of integer divisions : ',cnt
end subroutine

subroutine prime_b()
    implicit none
    integer :: inp, div, cnt
    logical :: isp
    cnt = 0
    div=2
    isp=.true.
    do inp = 3, 9999
        div = 2
        isp = .true.
        do while(div<=sqrt(real(inp)))
            cnt = cnt + 1
            if(mod(inp,div)==0) then
                isp=.false.
                exit
            end if
            div = div + 1.0
        end do
        if(isp.eqv..true.) then
            print *,inp
        end if
    end do
    print *, 'Method b : The number of integer divisions : ',cnt
end subroutine

end program prime
