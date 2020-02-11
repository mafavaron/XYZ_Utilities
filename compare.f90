module compare

    implicit none
    
    private
    
    ! Public interface
    public  :: FB
    public  :: NMSE
    
    ! Polymorphic interfaces
    
    interface FB
        module procedure    :: FB1
        module procedure    :: FB2
    end interface FB

    interface NMSE
        module procedure    :: NMSE1
        module procedure    :: NMSE2
    end interface NMSE

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


    function NMSE1(rvA, rvB) result(rNMSE)
    
        ! Routine arguments
        real(8), dimension(:), intent(in)   :: rvA
        real(8), dimension(:), intent(in)   :: rvB
        real(8)                             :: rNMSE
        
        ! Locals
        ! --none--
        
        ! Check array dimensions
        if(size(rvA) /= size(rvB)) then
            rNMSE = -9999.9d0
            return
        end if
        if(size(rvA) <= 0) then
            rNMSE = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        rNMSE = 2.d0 * sum((rvA - rvB)**2) / (sum(rvA) * sum(rvB))
        
    end function NMSE1


    function NMSE2(rmA, rmB) result(rNMSE)
    
        ! Routine arguments
        real(8), dimension(:,:), intent(in) :: rmA
        real(8), dimension(:,:), intent(in) :: rmB
        real(8)                             :: rNMSE
        
        ! Locals
        ! --none--
        
        ! Check array dimensions
        if(size(rmA) /= size(rmB)) then
            rNMSE = -9999.9d0
            return
        end if
        if(size(rmA) <= 0) then
            rNMSE = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        rNMSE = 2.d0 * sum((rmA - rmB)**2) / (sum(rmA) * sum(rmB))
        
    end function NMSE2

end module compare
