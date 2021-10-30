program example

    use rojff

    implicit none

    type(fallible_json_value_t) :: parsed_json

    integer :: start, finish, count_rate

    call system_clock(start, count_rate)
    parsed_json = parse_json_from_file('canada.json')
    call system_clock(finish)

    print *, 'rojff : ', (finish-start)/real(count_rate), ' seconds'

end program example
