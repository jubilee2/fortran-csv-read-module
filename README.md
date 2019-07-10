# fortran-csv-read-module
This project base on https://github.com/jacobwilliams/fortran-csv-module and just keep read csv 

## Usage
1. Import module
```fortran
use csv_module
```
2. Declare `csv reader` object
```fortran
type(csv_file) :: f
```
3. Declare variables
currently support arrary of real, double, integer and logical
```fortran
  real,dimension(:),allocatable :: x
  integer,dimension(:),allocatable :: y
  logical,dimension(:),allocatable :: z
  logical :: status_ok
```
4. Read the csv file
currently just allow header in first row(header_row=1) and or no header(header_row=0)
currently no skip_row function
```fortran
call f%read('csv\test.csv', header_row=1, status_ok=status_ok)
if(.not. status_ok) stop 
```
5. read csv column by column index or name
```fortran
call f%get(3,x,status_ok)
call f%get_by_name('id',y,status_ok)
```

## Enjoy it
