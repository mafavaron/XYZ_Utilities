module grid_gen

    implicit none
    
    private
    
    ! Public interface
    
    public  :: grid_space
    
    ! Constants
    real(8), parameter  :: INVALID = 1.70141d+38
    
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
        real(8)                             :: rRotation
        real(8), dimension(:), allocatable  :: rvX
        real(8), dimension(:), allocatable  :: rvY
        real(8), dimension(:), allocatable  :: rvZ
    contains
        procedure, public                   :: fileRead
        procedure, public                   :: fileWrite
        procedure, public                   :: build
        procedure, public                   :: getData
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
        integer             :: iX, iY
        integer             :: k
        real(8)             :: rX, rY
        
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
        read(iLUN, iostat=iErrCode) this % rRotation
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
        if(iLength/8 /= this % iNx * this % iNy) then
            iRetCode = 21
            close(iLUN)
            return
        end if
        k = 0
        do iY = 1, this % iNy
            rY = this % rY0 + this % rDeltaY * (iY - 1)
            do iX = 1, this % iNx
                rX = this % rX0 + this % rDeltaX * (iX - 1)
                k = k + 1
                this % rvX(k) = rX
                this % rvY(k) = rY
                read(iLUN, iostat=iErrCode) this % rvZ(k)
                if(iErrCode /= 0) then
                    iRetCode = 22
                    close(iLUN)
                    return
                end if
            end do
        end do
        
        ! Leave
        close(iLUN)
        
    end function fileRead
    

    function fileWrite(this, iLUN, sFileName) result(iRetCode)
    
        ! Routine arguments
        class(grid_space), intent(in)       :: this
        integer, intent(in)                 :: iLUN
        character(len=*), intent(in)        :: sFileName
        integer                             :: iRetCode
        
        ! Locals
        integer             :: iErrCode
        character(len=4)    :: sID
        integer             :: iLength
        integer             :: iX, iY
        integer             :: k
        real(8)             :: rX, rY
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check something can be made
        if(this % iVersion /= 2) then
            iRetCode = 1
            return
        end if
        
        ! Try opening the file
        open(iLUN, file=sFileName, status='unknown', action='write', access='stream', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 2
            return
        end if
        write(iLUN) "DSRB"
        iLength = 4
        write(iLUN) iLength
        
        ! Get GRD version (should be 2)
        write(iLUN) this % iVersion
        
        ! Get GRID section
        write(iLUN) "GRID"
        iLength = 72
        write(iLUN) iLength
        
        ! Get grid dimensions
        write(iLUN) this % iNy, this % iNx
        write(iLUN) this % rX0, this % rY0
        write(iLUN) this % rDeltaX, this % rDeltaY
        write(iLUN) this % rZmin, this % rZmax
        write(iLUN) this % rRotation
        write(iLUN) this % rInvalid
        
        ! Get actual data
        write(iLUN) "DATA"
        iLength = 8*(this % iNx * this % iNy)
        write(iLUN) iLength
        k = 0
        do iY = 1, this % iNy
            do iX = 1, this % iNx
                k = k + 1
                write(iLUN) this % rvZ(k)
            end do
        end do
        
        ! Leave
        close(iLUN)
        
    end function fileWrite
    
    
    function build(this, rvX, rvY, rvZ) result(iRetCode)
        
        ! Routine arguments
        class(grid_space), intent(inout)    :: this
        real(8), dimension(:), intent(in)   :: rvX
        real(8), dimension(:), intent(in)   :: rvY
        real(8), dimension(:), intent(in)   :: rvZ
        integer                             :: iRetCode
        
        ! Locals
        integer         :: i
        integer         :: j
        integer         :: k
        integer         :: n
        real(8)         :: rDeltaX
        real(8)         :: rDeltaY
        real(8)         :: rDeltaXmin
        real(8)         :: rDeltaYmin
        real(8)         :: rXmin
        real(8)         :: rYmin
        integer         :: iNx
        integer         :: iNy
        integer         :: iX
        integer         :: iY
        real(8), dimension(:,:), allocatable    :: rmZ
        real(8), dimension(:,:), allocatable    :: rmPackedX
        real(8), dimension(:,:), allocatable    :: rmPackedY
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Initialize the grid version to an absurd value, meaning
        ! "this grid is empty"
        this % iVersion = -1
        
        ! Check there is something to do
        if(size(rvX) <= 0 .or. size(rvY) <= 0 .or. size(rvZ) <= 0) then
            iRetCode = 1
            return
        end if
        n = size(rvZ)
        if(n /= size(rvX) .or. n /= size(rvY)) then
            iRetCode = 2
            return
        end if
        
        ! Get lower extrema
        rXmin = minval(rvX)
        rYmin = minval(rvY)
        
        ! Search the minimum non-zero displacement along
        ! the two X and Y directions: they *could* be
        ! the regular grid spacings.
        rDeltaXmin = huge(rDeltaXmin)
        rDeltaYmin = huge(rDeltaYmin)
        do i = 1, n
            rDeltaX = rvX(i) - rXmin
            rDeltaY = rvY(i) - rYmin
            if(rDeltaX > 0.d0 .and. rDeltaX < rDeltaXmin) rDeltaXmin = rDeltaX
            if(rDeltaY > 0.d0 .and. rDeltaY < rDeltaYmin) rDeltaYmin = rDeltaY
        end do
        
        ! Check the possible regular grid spacings really are: if so,
        ! then any receptor displacement from the SW grid point is
        ! an integer multiple of the "regular spacings". Which can be checked
        ! easily by a bit of floating point arithmetics.
        do i = 1, n
            rDeltaX = rvX(i) - rXmin
            rDeltaY = rvY(i) - rYmin
            if(abs(mod(rDeltaX, rDeltaXmin)) > 1.d4 .or. abs(mod(rDeltaY, rDeltaYmin)) > 1.d4) then
                iRetCode = 3
                return
            end if
        end do
        ! Post-condition: We can say it: The grid is regular! (Possibly consisting of 1 point only)
        
        ! Determine grid size along the X and Y directions
        iNx = nint((maxval(rvX) - minval(rvX)) / rDeltaXmin) + 1
        iNy = nint((maxval(rvY) - minval(rvY)) / rDeltaYmin) + 1
        if(iNx <= 0 .or. iNy <= 0) then
            iRetCode = 4
            return
        end if
        
        ! Reserve workspace, and fill it with actual data (which might be incomplete)
        allocate(rmPackedX(iNx, iNy), rmPackedY(iNx, iNy), rmZ(iNx, iNy))
        rmZ       = INVALID
        do i = 1, n
            iX = nint((rvX(i) - rXmin) / rDeltaXmin) + 1
            iY = nint((rvY(i) - rYmin) / rDeltaYmin) + 1
            rmPackedX(iX, iY) = rvX(i)
            rmPackedY(iX, iY) = rvY(i)
            rmZ(iX, iY)       = rvZ(i)
        end do
        
        ! Assign the grid fields their values
        this % rDeltaX   = rDeltaXmin
        this % rDeltaY   = rDeltaYmin
        this % rX0       = rXmin
        this % rY0       = rYmin
        this % iNx       = iNx
        this % iNy       = iNy
        this % rZmin     = minval(rmZ, mask=rmZ < INVALID)
        this % rZmax     = maxval(rmZ, mask=rmZ < INVALID)
        this % rInvalid  = INVALID
        this % rRotation = 0.d0
        
        ! Reserve grid workspace
        if(allocated(this % rvX)) deallocate(this % rvX)
        if(allocated(this % rvY)) deallocate(this % rvY)
        if(allocated(this % rvZ)) deallocate(this % rvZ)
        allocate(this % rvX(this % iNx * this % iNy))
        allocate(this % rvY(this % iNx * this % iNy))
        allocate(this % rvZ(this % iNx * this % iNy))
        
        ! Gather data in the right order
        k = 0
        do j = 1, iNy
            do i = 1, iNx
                k = k + 1
                this % rvX(k) = rmPackedX(i, j)
                this % rvY(k) = rmPackedY(i, j)
                this % rvZ(k) = rmZ(i, j)
            end do
        end do
        
        ! Confirm the grid is filled
        this % iVersion = 2
        
        ! Reclaim workspace
        deallocate(rmPackedX, rmPackedY, rmZ)
        
    end function build
    
    
    function getData(this, rvZ) result(iRetCode)
        
        ! Routine arguments
        class(grid_space), intent(in)                       :: this
        real(8), dimension(:), allocatable, intent(inout)   :: rvZ
        integer                                             :: iRetCode
        
        ! Locals
        ! --none--
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check something can be made
        if(this % iVersion /= 2) then
            iRetCode = 1
            return
        end if
        if(size(this % rvZ) <= 0) then
            iRetCode = 2
            return
        end if
        
        ! Reserve workspace, and fill it with data
        if(allocated(rvZ)) deallocate(rvZ)
        allocate(rvZ(size(this % rvZ)))
        rvZ = this % rvZ
        
    end function getData

end module grid_gen
