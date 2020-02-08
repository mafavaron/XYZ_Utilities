module grid_gen

    implicit none
    
    private
    
    ! Public interface
    
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
    end type grid_space
    
contains

end module grid_gen
