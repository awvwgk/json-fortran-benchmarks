! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

! this is based on tjson_parser from TOML-F

program tomlf_test 
   use tomlf_test_module
   use iso_fortran_env, only: int64,real64

   implicit none

   type(toml_table), allocatable :: table
   integer(int64) :: start, finish, count_rate

   call system_clock(start, count_rate)
   call json_load_file(table, filename = 'canada.json')
   call system_clock(finish)

   write(*,'(A30,1X,F7.4,1X,A)') 'tomlf : ', (finish-start)/real(count_rate,real64), ' seconds'

   !if (error) error stop 'error parsing JSON file'

end program tomlf_test
