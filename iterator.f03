module iterator_mod

implicit none
private
public :: Iterator, RealIterator

!
! Top level class containing common characteristics 
! of all iterators.
!
type, abstract :: Iterator
        private
    contains
        procedure(hasNext_def), deferred :: hasNext
end type Iterator

abstract interface 
    function hasNext_def(self)
        import Iterator
        class(Iterator), intent(in) :: self
        logical :: hasNext_def
    end function hasNext_def
end interface

!
! An iterator which returns REAL values.
!
type, extends(Iterator), abstract :: RealIterator
        private
    contains
        procedure(nextReal_def), deferred :: next
end type RealIterator

abstract interface 
    function nextReal_def(self)
        import RealIterator
        class(RealIterator), intent(inout) :: self
        real :: next_def
    end function nextReal_def
end interface

end module
