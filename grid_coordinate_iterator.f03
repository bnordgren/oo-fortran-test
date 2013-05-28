module grid_coordinate_iterator_mod
use Iterator_mod
use Grid_Coordinate_mod

private
public :: GridCoordinateIterator

type, extends(Iterator), abstract :: GridCoordinateIterator
        private
    contains
        procedure(nextGC_def), deferred :: next
end type GridCoordinateIterator

abstract interface
    function nextGC_def(self)
        import GridCoordinateIterator
        import GridCoordinate
        CLASS(GridCoordinateIterator) :: self
        CLASS(GridCoordinate), pointer :: nextGC_def
    end function
end interface


end module
