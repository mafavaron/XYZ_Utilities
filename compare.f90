module compare

    implicit none
    
    private
    
    ! Public interface
    public  :: FB
    public  :: NMSE
    public  :: GM
    public  :: GV
    public  :: FAC2
    public  :: NAD
    
    ! Polymorphic interfaces
    
    interface FB
        module procedure    :: FB1
        module procedure    :: FB2
    end interface FB

    interface NMSE
        module procedure    :: NMSE1
        module procedure    :: NMSE2
    end interface NMSE

    interface GM
        module procedure    :: GM1
        module procedure    :: GM2
    end interface GM

    interface GV
        module procedure    :: GV1
        module procedure    :: GV2
    end interface GV

    interface FAC2
        module procedure    :: FAC2_1
        module procedure    :: FAC2_2
    end interface FAC2

    interface NAD
        module procedure    :: NAD1
        module procedure    :: NAD2
    end interface NAD

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
        rNMSE = sum((rvA - rvB)**2) / (sum(rvA) * sum(rvB))
        
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
        rNMSE = sum((rmA - rmB)**2) / (sum(rmA) * sum(rmB))
        
    end function NMSE2


    function GM1(rvA, rvB) result(rGM)
    
        ! Routine arguments
        real(8), dimension(:), intent(in)   :: rvA
        real(8), dimension(:), intent(in)   :: rvB
        real(8)                             :: rGM
        
        ! Locals
        integer :: m
        
        ! Check array dimensions
        if(size(rvA) /= size(rvB)) then
            rGM = -9999.9d0
            return
        end if
        if(size(rvA) <= 0) then
            rGM = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        m   = count(rvA > 0.d0 .and. rvB > 0.d0)
        if(m > 0) then
            rGM = exp((sum(log(rvA)) - sum(log(rvB))) / m)
        else
            rGM = -9999.9d0
        end if
        
    end function GM1


    function GM2(rmA, rmB) result(rGM)
    
        ! Routine arguments
        real(8), dimension(:,:), intent(in) :: rmA
        real(8), dimension(:,:), intent(in) :: rmB
        real(8)                             :: rGM
        
        ! Locals
        integer :: m
        
        ! Check array dimensions
        if(size(rmA) /= size(rmB)) then
            rGM = -9999.9d0
            return
        end if
        if(size(rmA) <= 0) then
            rGM = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        m   = count(rmA > 0.d0 .and. rmB > 0.d0)
        if(m > 0) then
            rGM = exp((sum(log(rmA)) - sum(log(rmB))) / m)
        else
            rGM = -9999.9d0
        end if
        
    end function GM2


    function GV1(rvA, rvB) result(rGV)
    
        ! Routine arguments
        real(8), dimension(:), intent(in)   :: rvA
        real(8), dimension(:), intent(in)   :: rvB
        real(8)                             :: rGV
        
        ! Locals
        integer :: m
        
        ! Check array dimensions
        if(size(rvA) /= size(rvB)) then
            rGV = -9999.9d0
            return
        end if
        if(size(rvA) <= 0) then
            rGV = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        m   = count(rvA > 0.d0 .and. rvB > 0.d0)
        if(m > 0) then
            rGV = exp(((sum(log(rvA) - log(rvB))**2)) / m)
        else
            rGV = -9999.9d0
        end if
        
    end function GV1


    function GV2(rmA, rmB) result(rGV)
    
        ! Routine arguments
        real(8), dimension(:,:), intent(in) :: rmA
        real(8), dimension(:,:), intent(in) :: rmB
        real(8)                             :: rGV
        
        ! Locals
        integer :: m
        
        ! Check array dimensions
        if(size(rmA) /= size(rmB)) then
            rGV = -9999.9d0
            return
        end if
        if(size(rmA) <= 0) then
            rGV = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        m   = count(rmA > 0.d0 .and. rmB > 0.d0)
        if(m > 0) then
            rGV = exp(((sum(log(rmA) - log(rmB))**2)) / m)
        else
            rGV = -9999.9d0
        end if
        
    end function GV2


    function FAC2_1(rvA, rvB) result(rFAC2)
    
        ! Routine arguments
        real(8), dimension(:), intent(in)   :: rvA
        real(8), dimension(:), intent(in)   :: rvB
        real(8)                             :: rFAC2
        
        ! Locals
        integer :: m
        
        ! Check array dimensions
        if(size(rvA) /= size(rvB)) then
            rFAC2 = -9999.9d0
            return
        end if
        if(size(rvA) <= 0) then
            rFAC2 = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        m   = count(0.5d0*rvA <= rvB .and. rvB <= 2.d0*rvA)
        rFAC2 = real(m, kind=8) / size(rvA)
        
    end function FAC2_1


    function FAC2_2(rmA, rmB) result(rFAC2)
    
        ! Routine arguments
        real(8), dimension(:,:), intent(in) :: rmA
        real(8), dimension(:,:), intent(in) :: rmB
        real(8)                             :: rFAC2
        
        ! Locals
        integer :: m
        
        ! Check array dimensions
        if(size(rmA) /= size(rmB)) then
            rFAC2 = -9999.9d0
            return
        end if
        if(size(rmA) <= 0) then
            rFAC2 = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        m   = count(0.5d0*rmA <= rmB .and. rmB <= 2.d0*rmA)
        rFAC2 = real(m, kind=8) / size(rmA)
        
    end function FAC2_2


    function NAD1(rvA, rvB) result(rNAD)
    
        ! Routine arguments
        real(8), dimension(:), intent(in)   :: rvA
        real(8), dimension(:), intent(in)   :: rvB
        real(8)                             :: rNAD
        
        ! Locals
        ! --none--
        
        ! Check array dimensions
        if(size(rvA) /= size(rvB)) then
            rNAD = -9999.9d0
            return
        end if
        if(size(rvA) <= 0) then
            rNAD = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        rNAD = sum(abs(rvA - rvB)) / (sum(rvA + rvB))
        
    end function NAD1


    function NAD2(rmA, rmB) result(rNAD)
    
        ! Routine arguments
        real(8), dimension(:,:), intent(in) :: rmA
        real(8), dimension(:,:), intent(in) :: rmB
        real(8)                             :: rNAD
        
        ! Locals
        ! --none--
        
        ! Check array dimensions
        if(size(rmA) /= size(rmB)) then
            rNAD = -9999.9d0
            return
        end if
        if(size(rmA) <= 0) then
            rNAD = -9999.9d0
            return
        end if
        
        ! Compute the indicator
        rNAD = sum(abs(rmA - rmB)) / (sum(rmA + rmB))
        
    end function NAD2

end module compare
