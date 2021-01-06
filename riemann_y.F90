!*********************************************************************************************************
!**                                     Developped by Tomas Tamfal 					**
!**                                       Version 0.1 January 2018	                               	**
!*********************************************************************************************************


#include "choice.def"
subroutine riemann_y(f_l,f_r)

!**********************************************************************************************************
	use Parameters
	use Variables	

!**********************************************************************************************************
	implicit none
	integer :: i, j
#ifdef ONE
        real(8) :: f_l, f_r
#endif !ONE


#ifdef TWO
        real(8) :: f_l(1:x_end,1:y_end),f_r(1:x_end,1:y_end)

        do i=2,x_end-1 
                do j=2,y_end-1
                        if (soundspeed_y < 0D0) then 
                                F_riemann_left_y(i,j)  = f_l(i,j)
                                F_riemann_right_y(i,j) = f_l(i,j+1)


                        elseif (soundspeed_y > 0D0) then 
                                F_riemann_left_y(i,j)  = f_r(i,j-1)
                                F_riemann_right_y(i,j) = f_r(i,j)
                        endif 
                enddo
        enddo
#endif !TWO


return
end subroutine riemann_y

