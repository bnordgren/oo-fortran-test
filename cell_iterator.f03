module cell_iterator_mod
use iterator_mod
use grid_coordinate_mod
use grid_mod

implicit none
private
public :: CellIterator, AbstractCellIterator, ColumnFirstCellIterator
public :: RowFirstCellIterator, CWBorderIterator

type, extends(Iterator), abstract :: CellIterator
        private
    contains
        procedure(next_def), deferred :: next
end type

abstract interface 
    function next_def(self)
        import CellIterator, GridCoordinate
        CLASS(CellIterator) :: self
        CLASS(GridCoordinate), pointer :: next_def
    end function
end interface

type, extends(CellIterator), abstract :: AbstractCellIterator
        integer :: columns
        integer :: rows
        logical :: started
        integer :: current_col
        integer :: current_row
end type

type, extends(AbstractCellIterator) :: ColumnFirstCellIterator
        private
    contains
        procedure :: hasNext => hasnext_colfirst
        procedure :: next => next_colfirst
end type

interface ColumnFirstCellIterator
    procedure construct_colfirst
    procedure construct_colfirst_fromarray
end interface


type, extends(AbstractCellIterator) :: RowFirstCellIterator
        private
    contains
        procedure :: hasNext => hasnext_rowfirst
        procedure :: next    => next_rowfirst
end type

interface RowFirstCellIterator
    procedure construct_rowfirst
    procedure construct_rowfirst_fromarray
end interface


type, extends(AbstractCellIterator) :: CWBorderIterator
        private
    contains
        procedure :: hasNext => hasnext_cwborder
        procedure :: next    => next_cwborder
end type

interface CWBorderIterator
    procedure construct_cwborder
    procedure construct_cwborder_fromarray
end interface

contains

function hasnext_colfirst(self)
    CLASS(ColumnFirstCellIterator), intent(in) :: self
    logical                        :: hasnext_colfirst

    hasnext_colfirst = .false.
    if (.not. self%started) then 
        hasnext_colfirst = (self%columns>0) .and. (self%rows>0)
    else 
        hasnext_colfirst = (self%current_row < self%rows)
        if (.not. hasnext_colfirst) then
            hasnext_colfirst = (self%current_row == self%rows) .and. &
    &           (self%current_col < self%columns)
        end if 
    endif
end function

function next_colfirst(self)
    CLASS(ColumnFirstCellIterator) :: self
    CLASS(GridCoordinate), pointer :: next_colfirst 

    next_colfirst => NULL()
    if (self%hasNext() ) then 
        if (self%started) then
            self%current_col = self%current_col + 1
            if (self%current_col > self%columns) then 
                self%current_col = 1
                self%current_row = self%current_row + 1
            end if
        else
            self%current_col=1
            self%current_row=1
            self%started = .true.
        end if
        next_colfirst => DefaultGridCoordinate(self%current_col, self%current_row)
    end if 
end function

function construct_colfirst(source_grid)
    CLASS(Grid) :: source_grid
    CLASS(ColumnFirstCellIterator), pointer :: construct_colfirst

    ALLOCATE(construct_colfirst)
    construct_colfirst%columns = source_grid%getColumns()
    construct_colfirst%rows    = source_grid%getRows()
    construct_colfirst%started = .false.
end function

function construct_colfirst_fromarray(source_array)
    real, dimension(:,:)                   :: source_array
    TYPE(ColumnFirstCellIterator), pointer :: construct_colfirst_fromarray

    integer :: rank
    integer, dimension(2) :: max_idx

    max_idx = shape(source_array)
    rank    = size(max_idx)
    construct_colfirst_fromarray => NULL()
    print *, 'Shape = ', max_idx

    if (rank == 2) then
        ALLOCATE(construct_colfirst_fromarray)
        construct_colfirst_fromarray%columns = max_idx(1)
        construct_colfirst_fromarray%rows    = max_idx(2)
        construct_colfirst_fromarray%started = .false.
        construct_colfirst_fromarray%current_row = 0 
        construct_colfirst_fromarray%current_col = 0 
    end if
end function


function hasnext_rowfirst(self)
    CLASS(RowFirstCellIterator), intent(in) :: self
    logical                                 :: hasnext_rowfirst

    hasnext_rowfirst = .false.
    if (.not. self%started) then 
        hasnext_rowfirst = (self%columns>0) .and. (self%rows>0)
    else 
        hasnext_rowfirst = (self%current_col < self%columns)
        if (.not. hasnext_rowfirst) then
            hasnext_rowfirst = (self%current_col == self%columns) .and. &
    &           (self%current_row < self%rows)
        end if 
    endif
end function

function next_rowfirst(self)
    CLASS(RowFirstCellIterator) :: self
    CLASS(GridCoordinate), pointer :: next_rowfirst 

    next_rowfirst => NULL()
    if (self%hasNext() ) then 
        if (self%started) then
            self%current_row = self%current_row + 1
            if (self%current_row > self%rows) then 
                self%current_row = 1
                self%current_col = self%current_col + 1
            end if
        else
            self%current_col=1
            self%current_row=1
            self%started = .true.
        end if
        next_rowfirst => DefaultGridCoordinate(self%current_col, self%current_row)
    end if 
end function

function construct_rowfirst(source_grid)
    CLASS(Grid) :: source_grid
    CLASS(RowFirstCellIterator), pointer :: construct_rowfirst

    ALLOCATE(construct_rowfirst)
    construct_rowfirst%columns = source_grid%getColumns()
    construct_rowfirst%rows    = source_grid%getRows()
    construct_rowfirst%started = .false.
end function

function construct_rowfirst_fromarray(source_array)
    real, dimension(:,:)                 :: source_array
    CLASS(RowFirstCellIterator), pointer :: construct_rowfirst_fromarray

    integer :: rank
    integer, dimension(2) :: max_idx

    max_idx = shape(source_array)
    rank    = size(max_idx)
    construct_rowfirst_fromarray => NULL()

    if (rank == 2) then
        ALLOCATE(construct_rowfirst_fromarray)
        construct_rowfirst_fromarray%columns = max_idx(1)
        construct_rowfirst_fromarray%rows    = max_idx(2)
        construct_rowfirst_fromarray%started = .false.
    end if
end function


function hasnext_cwborder(self)
    CLASS(CWBorderIterator), INTENT(in) :: self
    logical                             :: hasnext_cwborder

    hasnext_cwborder = .not. ((self%current_col == 1) .and. (self%current_row==2))
end function

function next_cwborder(self)
    CLASS(CWBorderIterator) :: self
    CLASS(GridCoordinate), pointer :: next_cwborder

    next_cwborder => NULL()
    if (self%hasNext()) then
        if (self%started) then
            ! check top
            if (self%current_row == 1) then
                self%current_col = self%current_col + 1
                if (self%current_col > self%columns) then
                    self%current_col = self%columns
                    self%current_row = 2
                end if 
            else if (self%current_col == self%columns) then
                self%current_row = self%current_row + 1
                if (self%current_row > self%rows) then 
                    self%current_row = self%rows
                    self%current_col = self%columns-1
                end if 
            else if (self%current_row == self%rows) then 
                self%current_col = self%current_col -1 
                if (self%current_col < 1) then 
                    self%current_col = 1
                    self%current_row = self%rows-1
                end if 
            else if (self%current_col == 1) then 
                self%current_row = self%current_row - 1
            end if 
        else 
            self%current_row = 1
            self%current_col = 1
            self%started     = .true.
        end if 
        next_cwborder => DefaultGridCoordinate(self%current_col, self%current_row)
    end if
end function

function construct_cwborder(source_grid)
    CLASS(Grid) :: source_grid
    CLASS(CWBorderIterator), pointer :: construct_cwborder

    ALLOCATE(construct_cwborder)
    construct_cwborder%columns = source_grid%getColumns()
    construct_cwborder%rows    = source_grid%getRows()
    construct_cwborder%started = .false.
end function

function construct_cwborder_fromarray(source_array)
    real, dimension(:,:)             :: source_array
    CLASS(CWBorderIterator), pointer :: construct_cwborder_fromarray

    integer :: rank
    integer, dimension(2) :: max_idx

    max_idx = shape(source_array)
    rank    = size(max_idx)
    construct_cwborder_fromarray => NULL()

    if (rank == 2) then
        ALLOCATE(construct_cwborder_fromarray)
        construct_cwborder_fromarray%columns = max_idx(1)
        construct_cwborder_fromarray%rows    = max_idx(2)
        construct_cwborder_fromarray%started = .false.
    end if
end function

end module
