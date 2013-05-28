program test_cell_iter
use cell_iterator_mod
use grid_coordinate_mod

implicit none

real, dimension(3,3) :: mygrid
CLASS(CellIterator), pointer :: my_iterator

!
! Print out the grid dimensions
print *, 'mygrid has dimensions =', shape(mygrid)

write(*,*) "Row First Navigation!"
write(*,*) "====================="

my_iterator => RowFirstCellIterator(mygrid)
call walk_path(my_iterator)

write(*,*) "Column First Navigation!"
write(*,*) "========================"
my_iterator => ColumnFirstCellIterator(mygrid)
call walk_path(my_iterator)

write(*,*) "Clockwise Border Navigation!"
write(*,*) "============================"
my_iterator => CWBorderIterator(mygrid)
call walk_path(my_iterator)

contains

subroutine walk_path(iter)
    CLASS(CellIterator), pointer :: iter

    CLASS(GridCoordinate), pointer :: cur_coord
    if (.not. associated(iter)) then 
        print *, "iterator is NULL!"
    else 
        do while (iter%hasNext())  
            cur_coord => iter%next()
            if (.not. associated(cur_coord)) then 
                print *, 'coordinate is NULL!'
            else
                write (*,*) "(",cur_coord%getColumn(),",",cur_coord%getRow(),")"
                DEALLOCATE(cur_coord)
            end if
        end do
        DEALLOCATE(iter)
    end if 
end subroutine


end program test_cell_iter


