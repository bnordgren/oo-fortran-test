module grid_iterator_mod
use iterator_mod
use grid_coordinate_mod

implicit none
private
public :: GridIterator

type, extends(RealIterator), abstract :: GridIterator
        private
    contains
        procedure(getCoordinate_def), deferred :: getCoordinate
end type GridIterator

abstract interface
    function getCoordinate_def(self) 
        import GridIterator, GridCoordinate
        CLASS(GridIterator) :: self
        CLASS(GridCoordinate), pointer :: getCoordinate_def
    end function getCoordinate_def
end interface

end module grid_iterator_mod
