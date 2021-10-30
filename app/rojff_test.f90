program rojff_test

    use rojff
    use iso_fortran_env, only: int64,real64

    implicit none

    type(fallible_json_value_t) :: parsed_json

    integer(int64) :: start, finish, count_rate

    call system_clock(start, count_rate)
    parsed_json = parse_json_from_file('canada.json')
    call system_clock(finish)

    write(*,'(A30,1X,F7.4,1X,A)') 'rojff : ', (finish-start)/real(count_rate,real64), ' seconds'

    if (parsed_json%failed()) error stop 'error parsing JSON file'

end program rojff_test
