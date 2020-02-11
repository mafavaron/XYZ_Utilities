module compare

    implicit none
    
    private
    
    ! Public interface
    public  :: FB
    
    ! Interfaces
    
    interface FB
        module procedure    :: FB1
        module procedure    :: FB2
    end interface FB

contains

    function FB1(rvA, rvB) result(rFB)
    
        ! Routine arguments
        real(8), dimension(:), intent(in)   :: rvA
        real(8), dimension(:), intent(in)   :: rvB
        real(8)                             :: rFB
        
        ! Locals
        ! --none--
        
        ! Check array dimensions
        if(size(rvA) /= size(rvB)) then
            rFB = -9999.9d0
            return
        end if
        if(size(rvA) <= 0) then
            rFB = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        rFB = 2.d0 * (sum(rvA) - sum(rvB)) / (sum(rvA) + sum(rvB))
        
    end function FB1


    function FB2(rmA, rmB) result(rFB)
    
        ! Routine arguments
        real(8), dimension(:,:), intent(in) :: rmA
        real(8), dimension(:,:), intent(in) :: rmB
        real(8)                             :: rFB
        
        ! Locals
        ! --none--
        
        ! Check array dimensions
        if(size(rmA) /= size(rmB)) then
            rFB = -9999.9d0
            return
        end if
        if(size(rmA) <= 0) then
            rFB = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        rFB = 2.d0 * (sum(rmA) - sum(rmB)) / (sum(rmA) + sum(rmB))
        
    end function FB2

end module compare
