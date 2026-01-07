program jsonf_test
    use jsonf 

    implicit none 

    type(json_t) :: json
    integer(int64) :: start, finish, count_rate

    call system_clock(start, count_rate)

    call json%read_file('canada.json')

    call system_clock(finish)
    write(*,'(A30,1X,F7.4,1X,A)') 'jsonf test : ', (finish-start)/real(count_rate,real64), ' seconds'

end program jsonf_test