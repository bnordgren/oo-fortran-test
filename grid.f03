module grid_mod

use Grid_Coordinate_mod

implicit none
private
public :: Grid, DefaultGrid


type, abstract :: Grid
        private
    contains
        procedure(getValueGC_def), deferred :: getValueGC
        procedure(getValueInt_def), deferred :: getValueInt
        generic :: getValue => getValueGC, getValueInt

        procedure(setValueGC_def), deferred :: setValueGC
        procedure(setValueInt_def), deferred :: setValueInt
        generic :: setValue => setValueGC, setValueInt

        procedure(getColumns_def), deferred :: getColumns
        procedure(getRows_def), deferred    :: getRows
end type Grid

abstract interface
    function getValueGC_def(self, coord)
        import Grid, GridCoordinate
        CLASS(Grid) :: self
        CLASS(GridCoordinate) :: coord
        real :: getValueGC_def
    end function getValueGC_def
end interface

abstract interface 
    function getValueInt_def(self, col, row)
        import Grid
        CLASS(Grid) :: self
        integer :: col, row
        real getValueInt_def
    end function getValueInt_def
end interface

abstract interface 
    subroutine setValueGC_def(self, coord, value)
        import Grid, GridCoordinate
        CLASS(Grid) :: self
        CLASS(GridCoordinate) :: coord
        real :: value
    end subroutine setValueGC_def
end interface

abstract interface 
    subroutine setValueInt_def(self, col, row, value)
        import Grid
        CLASS(Grid) :: self
        integer     :: col, row
        real        :: value
    end subroutine setValueInt_def
end interface

abstract interface
    function getColumns_def(self)
        import Grid
        CLASS(grid) :: self
        integer     :: getColumns_def
    end function
end interface

abstract interface
    function getRows_def(self)
        import Grid
        CLASS(Grid) :: self
        integer     :: getRows_def
    end function
end interface

!
! Set up a default implementation of the Grid interface 
! which is backed by a 2D array of Reals.
!
type, extends(Grid) :: DefaultGrid
        private
        real, dimension(:,:), pointer :: grid_array
    contains
        procedure :: getValueGC => getValueGC_dg
        procedure :: getValueInt => getValueInt_dg
        procedure :: setValueGC => setValueGC_dg
        procedure :: setValueInt => setValueInt_dg
        procedure :: getColumns => getColumns_dg
        procedure :: getRows => getRows_dg 
end type

interface DefaultGrid
    procedure constructor
end interface

contains

function getColumns_dg(self)
    CLASS(DefaultGrid) :: self
    integer :: getColumns_dg

    integer :: rank
    integer, dimension(:), pointer :: max_idx
    integer :: cols

    
    cols = 0 
    ! Do we need to deallocate this manually? 
    max_idx = shape(self%grid_array)
    if (size(max_idx) == 2) then
        cols = max_idx(1)
    end if
    getColumns_dg = cols
end function

function getRows_dg(self)
    CLASS(DefaultGrid) :: self
    integer :: getRows_dg

    integer :: rank
    integer, dimension(:), pointer :: max_idx
    integer :: rows

    rows = 0
    ! Do we need to deallocate this manually? 
    max_idx = shape(self%grid_array)
    if (size(max_idx) == 2) then
        rows = max_idx(2)
    end if 
    getRows_dg = rows
end function

function getValueGC_dg(self, coord)
    CLASS(DefaultGrid) :: self
    CLASS(GridCoordinate) :: coord
    real :: getValueGC_dg

    getValueGC_dg = self%grid_array(coord%getColumn(), coord%getRow())
end function


function getValueInt_dg(self, col, row)
    CLASS(DefaultGrid) :: self
    integer :: col, row
    real    :: getValueInt_dg

    getValueInt_dg = self%grid_array(col, row)
end function

subroutine setValueGC_dg(self, coord, value)
    CLASS(DefaultGrid) :: self
    CLASS(GridCoordinate) :: coord
    real :: value

    self%grid_array(coord%getColumn(), coord%getRow()) = value
end subroutine

subroutine setValueInt_dg(self, col, row, value)
    CLASS(DefaultGrid) :: self
    integer            :: col, row 
    real               :: value

    self%grid_array(col, row) = value
end subroutine

function constructor(source_array)
    real, dimension(:,:), pointer :: source_array
    CLASS(DefaultGrid), pointer   :: constructor

    ALLOCATE(constructor)
    constructor%grid_array = source_array
end function

end module grid_mod
