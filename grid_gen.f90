module grid_gen

    implicit none
    
    private
    
    ! Public interface
    
    public  :: grid_space
    
    ! Data types
    
    type grid_space
        character(len=256)                  :: sDataFileName
        real(8)                             :: rDeltaX
        real(8)                             :: rDeltaY
        integer                             :: iNx
        integer                             :: iNy
        real(8), dimension(:), allocatable  :: rvX
        real(8), dimension(:), allocatable  :: rvY
        real(8), dimension(:), allocatable  :: rvZ
    contains
        procedure, public                   :: fileRead
        procedure, public                   :: fileWrite
    end type grid_space
    
contains

    function fileRead(this, iLUN, sFileName) result(iRetCode)
    
        ! Routine arguments
        class(grid_space), intent(inout)    :: this
        integer, intent(in)                 :: iLUN
        character(len=*), intent(in)        :: sFileName
        integer                             :: iRetCode
        
        ! Locals
        integer :: iErrCode
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
    end function fileRead
    

    function fileWrite(this, iLUN, sFileName) result(iRetCode)
    
        ! Routine arguments
        class(grid_space), intent(inout)    :: this
        integer, intent(in)                 :: iLUN
        character(len=*), intent(in)        :: sFileName
        integer                             :: iRetCode
        
        ! Locals
        integer :: iErrCode
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
    end function fileWrite

end module grid_gen
