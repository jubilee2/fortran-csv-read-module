  module csv_module
  use csv_util
  use iso_fortran_env, only: error_unit
  implicit none
  private

  type,public :: csv_string
    !! a cell from a CSV file.
    !!
    !! This is used to store the data internally
    !! in the [[csv_file]] class.
    character(len=:),allocatable :: str
  end type csv_string

  type,public :: csv_file
    !! the main class for reading and writing CSV files.
    !!
    !!@note A CSV file is assumed to contain the same number
    !!      of columns in each row. It may optionally contain
    !!      a header row.
    private

    character(len=5) :: version   = '0.0.1'
    character(len=1) :: quote     = '"'  !! quotation character
    character(len=1) :: delimiter = ','  !! delimiter character
    integer :: chunk_size = 100 !! for expanding vectors

    ! for reading a csv file:
    integer :: n_rows = 0  !! number of rows in the file
    integer :: n_cols = 0  !! number of columns in the file
    type(csv_string),dimension(:),allocatable :: header      !! the header
    type(csv_string),dimension(:,:),allocatable :: csv_data  !! the data in the file

  contains

  private
  procedure,public :: read => read_csv_file
  procedure,public :: destroy => destroy_csv_data

  procedure :: tokenize => tokenize_csv_line
  procedure :: read_line_from_file

  generic,public :: get => get_real_column, &
    get_double_column, &
    get_integer_column, &
    get_logical_column

  generic,public :: get_by_name => get_real_column_by_name, &
    get_double_column_by_name, &
    get_integer_column_by_name, &
    get_logical_column_by_name

  procedure :: get_csv_string_column
  procedure :: get_header_icol
  procedure :: get_real_column
  procedure :: get_double_column
  procedure :: get_integer_column
  procedure :: get_logical_column
  procedure :: get_real_column_by_name
  procedure :: get_double_column_by_name
  procedure :: get_integer_column_by_name
  procedure :: get_logical_column_by_name

  end type csv_file

  contains

  subroutine destroy_csv_data(me)
  implicit none
  class(csv_file),intent(out) :: me
  end subroutine destroy_csv_data


  subroutine read_csv_file(me,filename,header_row,status_ok)
  implicit none
  class(csv_file),intent(inout) :: me
  character(len=*),intent(in) :: filename  !! the CSV file to open
  logical,intent(out) :: status_ok  !! status flag
  integer,intent(in),optional :: header_row  !! the header row

  type(csv_string),dimension(:),allocatable :: row_data  !! a tokenized row
  character(len=:),allocatable :: line  !! a line from the file
  integer :: i, j, irow      !! counter & row counter
  integer :: n_rows_in_file   !! number of lines in the file
  integer :: n_rows           !! number of rows in the output data matrix
  integer :: n_cols           !! number of columns in the file (and output data matrix)
  integer :: istat            !! open status flag
  integer :: iunit            !! open file unit
  logical :: arrays_allocated !! if the arrays in the
  !!! class have been allocated
  integer :: iheader          !! row number of header row
  !!! (0 if no header specified)

  call me%destroy()
  arrays_allocated=.false.

  open(newunit=iunit, file=filename, status='OLD', iostat=istat)

  if (istat==0) then

    !get number of lines in the file
    n_rows_in_file = number_of_lines_in_file(iunit)

    n_rows = n_rows_in_file

    if (present(header_row)) then
      iheader = max(0,header_row)
      n_rows = n_rows - 1
    else
      iheader = 0
    end if

    me%n_rows = n_rows

    ! we don't know the number of columns
    ! until we parse the first row (or the header)

    !read each line in the file, parse it, and populate data
    irow = 0
    do i=1,n_rows_in_file  !! rows in the file

      call me%read_line_from_file(iunit,line,status_ok)
      if (.not. status_ok) return ! file read error
      call me%tokenize(line,row_data)

      if (.not. arrays_allocated) then
        ! note: the number of columns is obtained
        ! from the first one read. It is assumed
        ! that each row has the same number of
        ! columns.
        n_cols = size(row_data)
        me%n_cols = n_cols
        allocate(me%csv_data(n_rows,n_cols))
        if (iheader/=0) allocate(me%header(n_cols))
        arrays_allocated = .true.
      end if

      if (i==iheader) then
        me%header = row_data
      else
        irow = irow + 1  !! row counter in data array
        if(n_cols > size(row_data)) then
          write(error_unit,'(A)') 'Error row data length < header length of file: '//trim(filename)
          status_ok = .false.
          return
        end if
        do j=1,n_cols
          me%csv_data(irow,j) = row_data(j) !%str
        end do
      end if

    end do

    ! close the file
    close(unit=iunit,iostat=istat)

    status_ok = .true.

  else
    write(error_unit,'(A)') 'Error opening file: '//trim(filename)
    status_ok = .false.
  end if
  end subroutine read_csv_file


  function number_of_lines_in_file(iunit) result(n_lines)
  implicit none
  integer, intent(in) :: iunit   !! the file unit number (assumed to be open)
  integer :: n_lines   !! the number of lines in the file

  character(len=1) :: tmp
  integer :: istat

  rewind(iunit)
  n_lines = 0
  do
    read(iunit,fmt='(A1)',iostat=istat) tmp
    if (is_iostat_end(istat)) exit
    n_lines = n_lines + 1
  end do
  rewind(iunit)
  end function number_of_lines_in_file

  subroutine read_line_from_file(me,iunit,line,status_ok)
  implicit none
  class(csv_file), intent(in) :: me
  integer, intent(in) :: iunit
  character(len=:), allocatable, intent(out) :: line
  logical, intent(out) :: status_ok !! true if no problems

  integer :: nread  !! character count specifier for read statement
  integer :: istat  !! file read io status flag
  character(len=me%chunk_size) :: buffer !! the file read buffer

  nread  = 0
  buffer = ''
  line   = ''
  status_ok = .true.

  do
    ! read in the next block of text from the line:
    read(iunit,fmt='(A)',advance='NO',size=nread,iostat=istat) buffer
    if (IS_IOSTAT_END(istat) .or. IS_IOSTAT_EOR(istat)) then
      ! add the last block of text before the end of record
      if (nread>0) line = line//buffer(1:nread)
      exit
    else if (istat==0) then ! all the characters were read
      line = line//buffer  ! add this block of text to the string
    else  ! some kind of error
      write(error_unit,'(A,1X,I5)') 'Read error for file unit: ',iunit
      status_ok = .false.
      exit
    end if
  end do
  end subroutine read_line_from_file

  subroutine tokenize_csv_line(me,line,cells)
  implicit none
  class(csv_file),intent(inout) :: me
  character(len=*),intent(in) :: line
  type(csv_string),dimension(:),allocatable,intent(out) :: cells

  integer :: i !! counter
  character(len=:),allocatable :: tmp !! a temp string with whitespace removed
  integer :: n !! length of compressed string

  call split(line, me%delimiter, cells)

  ! remove quotes if present:
  do i = 1, size(cells)

    ! remove whitespace from the string:
    tmp = trim(adjustl(cells(i)%str))
    n = len(tmp)

    if (n>1) then
      ! if the first and last non-blank character is
      ! a quote, then remove them and replace with what
      ! is inside the quotes. Otherwise, leave it as is.
      if (tmp(1:1)==me%quote .and. tmp(n:n)==me%quote) then
        if (n>2) then
          cells(i)%str = tmp(2:n-1)  ! remove the quotes
        else
          cells(i)%str = ''  ! empty string
        end if
      end if
    end if

  end do
  end subroutine tokenize_csv_line

  pure subroutine split(str, token, vals)
  implicit none
  character(len=*),intent(in)  :: str
  character(len=*),intent(in)  :: token
  type(csv_string),dimension(:),allocatable,intent(out) :: vals

  integer :: i          !! counter
  integer :: len_token  !! length of the token
  integer :: i1,i2      !! index
  character(len=:),allocatable :: string

  len_token = len(token)  ! length of the token
  allocate(vals(0))

  ! length of the string
  if (token == ' ') then
    ! in this case, we can't ignore trailing space
    string = str
  else
    ! safe to ignore trailing space when looking for tokens
    string = trim(str)
  end if

  do
    i = index(string,token) ! index of next token in remaining string
    if (i<=0) then
      vals = [vals, csv_string(string)]
      exit           ! no more tokens found
    end if
    i2 = i-1
    vals = [vals, csv_string(string(1:i2))]

    i1 = i+len_token
    i2=len(string)
    string = string(i1:i2)
  end do
  end subroutine split

  subroutine get_header_icol(me,headeName,r,status_ok)
  implicit none
  class(csv_file),intent(inout) :: me
  character(len=*),intent(in) :: headeName
  integer,intent(out) :: r
  logical,intent(out) :: status_ok

  integer :: i !! column counter
  status_ok = .false.
  r=0

  if (allocated(me%header)) then
    do i=1,me%n_cols
      if (me%header(i)%str == headeName) then
        status_ok = .true.
        r=i
        return
      end if
    end do

    if(.not. status_ok) write(error_unit,'(A)') &
      'Error: no column name of '//trim(headeName)

  else
    write(error_unit,'(A)') 'Error: no header in class.'
  end if
  end subroutine get_header_icol

  subroutine get_csv_string_column(me,icol,r,status_ok)
  implicit none

  class(csv_file),intent(inout) :: me
  integer,intent(in) :: icol  !! column number
  type(csv_string),dimension(:),allocatable,intent(out) :: r
  logical,intent(out) :: status_ok

  status_ok = .true.

  if (me%n_cols>=icol .and. icol>0) then
    if (allocated(me%csv_data)) then
      allocate(r(me%n_rows))  ! size the output vector
      r = me%csv_data(:,icol)
    else
      write(error_unit,'(A,1X,I5)') 'Error: class has not been initialized'
      status_ok = .false.
    end if
  else
    write(error_unit,'(A,1X,I5)') 'Error: invalid column number: ',icol
    status_ok = .false.
  end if
  end subroutine get_csv_string_column


  subroutine get_real_column(me,icol,r,status_ok)
  implicit none
  class(csv_file),intent(inout) :: me
  integer,intent(in) :: icol  !! column number
  real,dimension(:),allocatable,intent(out) :: r
  logical,intent(out) :: status_ok

  real,dimension(:),allocatable :: rTmp

  type(csv_string),dimension(:),allocatable :: columnData
  integer :: i

  call me%get_csv_string_column(icol,columnData,status_ok)
  if(.not. status_ok) return

  if (allocated(r)) then
    status_ok = .false.
    write(error_unit,'(A)') &
      'Error get_real_column allready allocated result: '
    return
  end if

  allocate(rTmp(me%n_rows))
  rTmp=0.0e0

  do i=1,me%n_rows  ! row loop
    call to_real(columnData(i)%str,rTmp(i),status_ok)
    if (.not. status_ok) then
      !deallocate(r)
      write(error_unit,'(A)') &
        'Error converting string to real: '//trim(me%csv_data(i,icol)%str)
      return
    end if
  end do
  allocate(r(me%n_rows))
  r=rTmp
  end subroutine get_real_column

  subroutine get_double_column(me,icol,r,status_ok)
  implicit none
  class(csv_file),intent(inout) :: me
  integer,intent(in) :: icol  !! column number
  real(8),dimension(:),allocatable,intent(out) :: r
  logical,intent(out) :: status_ok

  real(8),dimension(:),allocatable :: rTmp

  type(csv_string),dimension(:),allocatable :: columnData
  integer :: i

  call me%get_csv_string_column(icol,columnData,status_ok)
  if(.not. status_ok) return

  if (allocated(r)) then
    status_ok = .false.
    write(error_unit,'(A)') &
      'Error get_double_column allready allocated result: '
    return
  end if

  allocate(rTmp(me%n_rows))
  rTmp=0.0d0

  do i=1,me%n_rows  ! row loop
    call to_double(columnData(i)%str,rTmp(i),status_ok)
    if (.not. status_ok) then
      !deallocate(r)
      write(error_unit,'(A)') &
        'Error converting string to double: '//trim(me%csv_data(i,icol)%str)
      return
    end if
  end do
  allocate(r(me%n_rows))
  r=rTmp
  end subroutine get_double_column

  subroutine get_integer_column(me,icol,r,status_ok)
  implicit none
  class(csv_file),intent(inout) :: me
  integer,intent(in) :: icol  !! column number
  integer,dimension(:),allocatable,intent(out) :: r
  logical,intent(out) :: status_ok

  integer,dimension(:),allocatable :: rTmp

  type(csv_string),dimension(:),allocatable :: columnData
  integer :: i

  call me%get_csv_string_column(icol,columnData,status_ok)
  if(.not. status_ok) return

  if (allocated(r)) then
    status_ok = .false.
    write(error_unit,'(A)') &
      'Error get_integer_column allready allocated result: '
    return
  end if

  allocate(rTmp(me%n_rows))
  rTmp=0

  do i=1,me%n_rows  ! row loop
    call to_integer(columnData(i)%str,rTmp(i),status_ok)
    if (.not. status_ok) then
      !deallocate(r)
      write(error_unit,'(A)') &
        'Error converting string to integer: '//trim(me%csv_data(i,icol)%str)
      return
    end if
  end do
  allocate(r(me%n_rows))
  r=rTmp
  end subroutine get_integer_column

  subroutine get_logical_column(me,icol,r,status_ok)
  implicit none
  class(csv_file),intent(inout) :: me
  integer,intent(in) :: icol  !! column number
  logical,dimension(:),allocatable,intent(out) :: r
  logical,intent(out) :: status_ok

  logical,dimension(:),allocatable :: rTmp

  type(csv_string),dimension(:),allocatable :: columnData
  integer :: i

  call me%get_csv_string_column(icol,columnData,status_ok)
  if(.not. status_ok) return

  if (allocated(r)) then
    status_ok = .false.
    write(error_unit,'(A)') &
      'Error get_logical_column allready allocated result:'
    return
  end if

  allocate(rTmp(me%n_rows))
  rTmp = .false.

  do i=1,me%n_rows  ! row loop
    call to_logical(columnData(i)%str,rTmp(i),status_ok)
    if (.not. status_ok) then
      write(error_unit,'(A)') &
        'Error converting string to logical: '//trim(me%csv_data(i,icol)%str)
      return
    end if
  end do
  allocate(r(me%n_rows))
  r=rTmp
  end subroutine get_logical_column


  subroutine get_real_column_by_name(me,icolName,r,status_ok)
  implicit none
  class(csv_file),intent(inout) :: me
  character(len=*),intent(in) :: icolName
  real,dimension(:),allocatable,intent(out) :: r
  logical,intent(out) :: status_ok

  integer :: icol  !! column number

  call me%get_header_icol(icolName,icol,status_ok)
  if(.not. status_ok) return
  call me%get_real_column(icol,r,status_ok)
  end subroutine get_real_column_by_name

  subroutine get_double_column_by_name(me,icolName,r,status_ok)
  implicit none
  class(csv_file),intent(inout) :: me
  character(len=*),intent(in) :: icolName
  real(8),dimension(:),allocatable,intent(out) :: r
  logical,intent(out) :: status_ok

  integer :: icol  !! column number

  call me%get_header_icol(icolName,icol,status_ok)
  if(.not. status_ok) return
  call me%get_double_column(icol,r,status_ok)
  end subroutine get_double_column_by_name

  subroutine get_integer_column_by_name(me,icolName,r,status_ok)
  implicit none
  class(csv_file),intent(inout) :: me
  character(len=*),intent(in) :: icolName
  integer,dimension(:),allocatable,intent(out) :: r
  logical,intent(out) :: status_ok

  integer :: icol  !! column number

  call me%get_header_icol(icolName,icol,status_ok)
  if(.not. status_ok) return
  call me%get_integer_column(icol,r,status_ok)
  end subroutine get_integer_column_by_name

  subroutine get_logical_column_by_name(me,icolName,r,status_ok)
  implicit none
  class(csv_file),intent(inout) :: me
  character(len=*),intent(in) :: icolName
  logical,dimension(:),allocatable,intent(out) :: r
  logical,intent(out) :: status_ok

  integer :: icol  !! column number

  call me%get_header_icol(icolName,icol,status_ok)
  if(.not. status_ok) return
  call me%get_logical_column(icol,r,status_ok)
  end subroutine get_logical_column_by_name

  end module csv_module
