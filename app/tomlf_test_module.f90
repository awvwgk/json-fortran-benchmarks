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

module tomlf_test_module
   use tomlf_constants, only : tfc, tfi, tfr, toml_type
   use tomlf_datetime, only : toml_datetime
   use tomlf_de_context, only : toml_context
   use tjson_lexer, only : json_lexer, new_lexer_from_string, new_lexer_from_unit, &
      & new_lexer_from_file
   use tomlf_de_parser, only : parse, toml_parser_config
   use tomlf_diagnostic, only : toml_level
   use tomlf_build, only : get_value
   use tomlf_error, only : toml_error
   use tomlf_type, only : toml_table, toml_value, cast_to_table

   use iso_fortran_env, only: int64

   implicit none

   
   public :: json_load, json_loads
   public :: toml_context, toml_parser_config, toml_level


   !> Load a TOML data structure from the provided source
   interface json_load
      module procedure :: json_load_file
      module procedure :: json_load_unit
   end interface json_load

   !> Load a TOML data structure from a string
   interface json_loads
      module procedure :: json_load_string
   end interface json_loads

contains

!> Load TOML data structure from file
subroutine json_load_file(table, filename, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   character(*, tfc), intent(in) :: filename
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(json_lexer) :: lexer
   type(toml_error), allocatable :: error_

   call new_lexer_from_file(lexer, filename, error_)
   if (.not.allocated(error_)) then
      call parse(lexer, table, config, context, error)
      if (allocated(table)) call prune(table)
   else
      if (present(error)) call move_alloc(error_, error)
   end if
end subroutine json_load_file

!> Load TOML data structure from unit
subroutine json_load_unit(table, io, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   !> Unit to read from
   integer, intent(in) :: io
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(json_lexer) :: lexer
   type(toml_error), allocatable :: error_

   call new_lexer_from_unit(lexer, io, error_)
   if (.not.allocated(error_)) then
      call parse(lexer, table, config, context, error)
      if (allocated(table)) call prune(table)
   else
      if (present(error)) call move_alloc(error_, error)
   end if
end subroutine json_load_unit

!> Load TOML data structure from string
subroutine json_load_string(table, string, config, context, error)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(out) :: table
   !> String containing TOML document
   character(*, tfc), intent(in) :: string
   !> Configuration for the parser
   type(toml_parser_config), intent(in), optional :: config
   !> Context tracking the origin of the data structure to allow rich reports
   type(toml_context), intent(out), optional :: context
   !> Error handling, provides detailed diagnostic in case of error
   type(toml_error), allocatable, intent(out), optional :: error

   type(json_lexer) :: lexer

   call new_lexer_from_string(lexer, string)
   call parse(lexer, table, config, context, error)
   if (allocated(table)) call prune(table)
end subroutine json_load_string

!> Prune the artificial root table inserted by the lexer
subroutine prune(table)
   !> Instance of the TOML data structure, not allocated in case of error
   type(toml_table), allocatable, intent(inout) :: table

   type(toml_table), allocatable :: root
   type(toml_table), pointer :: ptr
   class(toml_value), pointer :: child

   call move_alloc(table, root)
   call root%get("_", child)

   ptr => cast_to_table(child)
   if (associated(ptr)) table = ptr
end subroutine prune

end module tomlf_test_module
