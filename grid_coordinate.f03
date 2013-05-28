module grid_coordinate_mod

implicit none
private
public :: GridCoordinate, DefaultGridCoordinate

!
! GridCoordinate interface
!
type, abstract :: GridCoordinate
        private
    contains
        procedure (getterTemplate), deferred :: getRow, getColumn
end type GridCoordinate

abstract interface
    function getterTemplate(self)
        import GridCoordinate
        CLASS(GridCoordinate), intent(in) :: self
        integer                           :: getterTemplate
    end function
end interface


!
! Default GridCoordinate declaration
!
type, extends(GridCoordinate) :: DefaultGridCoordinate
        private
        integer :: row, col
    contains
        procedure :: getRow => defaultGetRow
        procedure :: getColumn => defaultGetColumn
end type DefaultGridCoordinate

!
! Here we tell the compiler which of the functions is the 
! constructor.
!
interface DefaultGridCoordinate
    procedure constructor
end interface DefaultGridCoordinate

contains

function defaultGetRow(self)
    CLASS(DefaultGridCoordinate), intent(in) :: self
    integer                                  :: defaultGetRow

    defaultGetRow = self%row
end function

function defaultGetColumn(self)
    CLASS(DefaultGridCoordinate), intent(in) :: self
    integer                                  :: defaultGetColumn

    defaultGetColumn = self%col
end function

!
! This function allocates and returns a new GridCoordinate object.
! Thou shalt deallocate it.
!
function constructor(col, row)
    integer, intent(in) :: col, row
    CLASS(DefaultGridCoordinate), pointer :: constructor

    ALLOCATE(constructor)
    constructor%row = row
    constructor%col = col
end function constructor

end module grid_coordinate_mod
