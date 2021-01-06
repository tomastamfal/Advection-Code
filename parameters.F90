!*********************************************************************************************************
!**                                     Developped by Tomas Tamfal 					**
!**                                       Version 0.1 January 2018	                               	**
!*********************************************************************************************************

!**********************************************************************************************************
!            Parameters.F90:  Sets different physical parameters and normalization
!**********************************************************************************************************

#include "choice.def"
module parameters

!**********************************************************************************************************
	implicit none    

!**********************************************************************************************************
#ifdef ONE
!**********************************************************************************************************
	integer, parameter :: nx 	= 1D4 ! resolution in x direction
        integer, parameter :: nghost    = 2
        integer, parameter :: ini       = 1 + nghost
        integer, parameter :: x_end     = nx + 2*nghost
        integer, parameter :: maxsteps  = 1D5

        integer, parameter :: freq_output = 1D3

        real(8), parameter :: soundspeed = 1D0 
#endif !ONE


!**********************************************************************************************************
#ifdef TWO
!**********************************************************************************************************
	integer, parameter :: nx 	= 2D2 ! resolution in x direction
	integer, parameter :: ny 	= 2D2 ! resolution in x direction
 
        integer, parameter :: nghost    = 2
        integer, parameter :: ini       = 1 + nghost
        integer, parameter :: x_end     = nx + 2*nghost
        integer, parameter :: y_end     = ny + 2*nghost

        integer, parameter :: maxsteps  = 1D3

        integer, parameter :: freq_output = 1D1
        
        real(8), parameter :: soundspeed_x = DSQRT(2D0)/2D0 
        real(8), parameter :: soundspeed_y = DSQRT(2D0)/2D0 
 
#endif !TWO

!**********************************************************************************************************
end module parameters
!**********************************************************************************************************
