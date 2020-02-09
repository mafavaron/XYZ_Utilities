module grid_gen

    implicit none
    
    private
    
    ! Public interface
    
    public  :: grid_space
    
    ! Data types
    
    type grid_space
        character(len=256)                  :: sDataFileName
        integer                             :: iVersion
        real(8)                             :: rX0
        real(8)                             :: rY0
        real(8)                             :: rDeltaX
        real(8)                             :: rDeltaY
        real(8)                             :: rZmin
        real(8)                             :: rZmax
        integer                             :: iNx
        integer                             :: iNy
        real(8)                             :: rInvalid
        real(8)                             :: rHold
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
        integer             :: iErrCode
        character(len=4)    :: sID
        integer             :: iLength
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Try opening the file and checking it really is a Surfer 12 grid
        open(iLUN, file=sFileName, status='old', action='read', access='stream', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        read(iLUN, iostat=iErrCode) sID
        if(iErrCode /= 0) then
            iRetCode = 2
            close(iLUN)
            return
        end if
        if(sID /= "DSRB") then
            iRetCode = 3
            close(iLUN)
            return
        end if
        read(iLUN, iostat=iErrCode) iLength
        if(iErrCode /= 0) then
            iRetCode = 4
            close(iLUN)
            return
        end if
        iLength = iLength / 4
        if(iLength /= 1) then
            iRetCode = 5
            close(iLUN)
            return
        end if
        
        ! Get GRD version (should be 2)
        read(iLUN, iostat=iErrCode) this % iVersion
        if(iErrCode /= 0) then
            iRetCode = 6
            close(iLUN)
            return
        end if
        if(this % iVersion /= 2) then
            iRetCode = 7
            close(iLUN)
            return
        end if
        
        ! Get GRID section
        read(iLUN, iostat=iErrCode) sID
        if(iErrCode /= 0) then
            iRetCode = 8
            close(iLUN)
            return
        end if
        if(sID /= "GRID") then
            iRetCode = 9
            close(iLUN)
            return
        end if
        read(iLUN, iostat=iErrCode) iLength
        if(iErrCode /= 0) then
            iRetCode = 10
            close(iLUN)
            return
        end if
        if(iLength /= 72) then
            iRetCode = 11
            close(iLUN)
            return
        end if
        
        ! Get grid dimensions
        read(iLUN, iostat=iErrCode) this % iNy, this % iNx
        if(iErrCode /= 0) then
            iRetCode = 12
            close(iLUN)
            return
        end if
        read(iLUN, iostat=iErrCode) this % rX0, this % rY0
        if(iErrCode /= 0) then
            iRetCode = 13
            close(iLUN)
            return
        end if
        read(iLUN, iostat=iErrCode) this % rDeltaX, this % rDeltaY
        if(iErrCode /= 0) then
            iRetCode = 14
            close(iLUN)
            return
        end if
        read(iLUN, iostat=iErrCode) this % rZmin, this % rZmax
        if(iErrCode /= 0) then
            iRetCode = 15
            close(iLUN)
            return
        end if
        read(iLUN, iostat=iErrCode) this % rHold
        if(iErrCode /= 0) then
            iRetCode = 16
            close(iLUN)
            return
        end if
        read(iLUN, iostat=iErrCode) this % rInvalid
        if(iErrCode /= 0) then
            iRetCode = 17
            close(iLUN)
            return
        end if
        
        ! Reserve workspace
        if(allocated(this % rvX)) deallocate(this % rvX)
        if(allocated(this % rvY)) deallocate(this % rvY)
        if(allocated(this % rvZ)) deallocate(this % rvZ)
        allocate(this % rvX(this % iNx * this % iNy))
        allocate(this % rvY(this % iNx * this % iNy))
        allocate(this % rvZ(this % iNx * this % iNy))
        
        ! Get actual data
        read(iLUN, iostat=iErrCode) sID
        if(iErrCode /= 0) then
            iRetCode = 18
            close(iLUN)
            return
        end if
        if(sID /= "DATA") then
            iRetCode = 19
            close(iLUN)
            return
        end if
        read(iLUN, iostat=iErrCode) iLength
        if(iErrCode /= 0) then
            iRetCode = 20
            close(iLUN)
            return
        end if
        print *, iLength/8 - this % iNx * this % iNy
        
        ! Leave
        close(iLUN)
        
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
