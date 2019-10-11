program matrix
    implicit none
    call sum_val()
contains

subroutine sum_val()
    implicit none
    integer :: inp, sum, i,j,cnt2
    integer, dimension(100,100) :: arr
    sum = 0
    do i=1, 100
        read *, (arr(i,j),j=1,100)
    end do
    do cnt2 = 2, 100
        sum = sum + arr(cnt2,cnt2-1)
    end do
    print *, sum
end subroutine

end program matrix
