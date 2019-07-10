  !*******************************************************************************
  !> author: Jacob Williams
  !
  !  Utility routines.
  !*******************************************************************************

  module csv_util

  public :: to_real
  public :: to_integer
  public :: to_logical

  contains

  pure elemental subroutine to_real(str,val,status_ok)

  implicit none

  character(len=*),intent(in) :: str
  real,intent(out) :: val
  logical,intent(out) :: status_ok

  integer :: istat  !! read `iostat` error code

  read(str,fmt=*,iostat=istat) val
  if (istat==0) then
    status_ok = .true.
  else
    status_ok = .false.
    val = 0.0d0
  end if

  end subroutine to_real

  pure elemental subroutine to_integer(str,val,status_ok)

  implicit none

  character(len=*),intent(in) :: str
  integer,intent(out) :: val
  logical,intent(out) :: status_ok

  integer :: istat  !! read `iostat` error code

  read(str,fmt=*,iostat=istat) val
  if (istat==0) then
    status_ok = .true.
  else
    status_ok = .false.
    val = 0
  end if

  end subroutine to_integer

  pure elemental subroutine to_logical(str,val,status_ok)

  implicit none

  character(len=*),intent(in) :: str
  logical,intent(out) :: val
  logical,intent(out) :: status_ok

  integer :: istat  !! read `iostat` error code

  read(str,fmt=*,iostat=istat) val
  if (istat==0) then
    status_ok = .true.
  else
    status_ok = .false.
    val = 0
  end if

  end subroutine to_logical
  end module csv_util
