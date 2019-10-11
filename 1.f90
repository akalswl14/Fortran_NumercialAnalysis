program lastyear
    implicit none
    integer :: N
    read (*,*) N
    call max_dia(N)
contains

subroutine max_dia(N)
    implicit none
    real*8 :: cnt_tem,cnt
    integer  :: N ,i,j
    real*8, dimension(N,N) :: A
    cnt_tem = 0
    cnt = 0
    do i=1, N
        read (*,*) (A(i,j),j=1,N)
    end do
    do i=1, N
        j=0
        do while(j<i)
            cnt_tem = cnt_tem + A(N-i+1+j,j+1)
            j = j+1
        end do
        if (cnt < cnt_tem) then
            cnt = cnt_tem
        end if
        cnt_tem = 0
    end do
    do i=1,N-1
        j=1
        do while(j+i<=N)
            cnt_tem = cnt_tem + A(j,j+i)
            j=j+1
        end do
        if (cnt < cnt_tem) then
            cnt = cnt_tem
        end if
        cnt_tem = 0
    end do
    print *,cnt
end subroutine

end program lastyear
