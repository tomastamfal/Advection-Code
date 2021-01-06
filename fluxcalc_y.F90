!*********************************************************************************************************
!**                                     Developped by Tomas Tamfal 					**
!**                                       Version 0.1 January 2018	                               	**
!*********************************************************************************************************


#include "choice.def"
subroutine fluxcalc_y(u_l, u_r)

!**********************************************************************************************************
	use Parameters
	use Variables	

!**********************************************************************************************************
	implicit none
	integer :: i, j

#ifdef ONE
        real(8) :: u_l, u_r
#endif !ONE


#ifdef TWO
        real(8) :: u_l(1:x_end,1:y_end), u_r(1:x_end,1:y_end)

            do i =2,x_end-1
                do j=2,y_end-1
                        F_left_y(i,j)   = soundspeed_y * u_l(i,j) *dx(5)
                        F_right_y(i,j)  = soundspeed_y * u_r(i,j) *dx(5)
                enddo
            enddo 
#endif !TWO


return
end subroutine fluxcalc_y

