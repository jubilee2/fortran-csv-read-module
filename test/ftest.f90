  !  test.f90
  !
  !  FUNCTIONS:
  !  test - Entry point of console application.
  !

  !****************************************************************************
  !
  !  PROGRAM: test
  !
  !  PURPOSE:  Entry point for the console application.
  !
  !****************************************************************************

  program test
  use csv_module
  implicit none
  type(csv_file) :: f
  real,dimension(:),allocatable :: x,y,z,u,v
  integer,dimension(:),allocatable :: a,b
  logical,dimension(:),allocatable :: d,e
  logical :: status_ok
  integer :: i

  call f%read('csv/test.csv', header_row=1, status_ok=status_ok)

  call f%get_by_name('id',a,status_ok)
  print *, a

  call f%get(2,y,status_ok)
  print *, y

  call f%get_by_name('age',z,status_ok)
  print *, z

  call f%get(4,d,status_ok)
  print *, d

  call f%get(3,v,status_ok)
  print *, v

  print *, 'Test done!'

  end program test

