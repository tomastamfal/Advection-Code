!*********************************************************************************************************
!**                                     Developped by Tomas Tamfal 					**
!**                                       Version 0.1 January 2018	                               	**
!*********************************************************************************************************


#include "choice.def"
subroutine riemann_x(f_l,f_r)

!**********************************************************************************************************
	use Parameters
	use Variables	

!**********************************************************************************************************
	implicit none
	integer :: i, j

#ifdef ONE
        real(8) :: f_l(1:x_end),f_r(1:x_end)
#endif !ONE

#ifdef TWO
        real(8) :: f_l(1:x_end,1:y_end),f_r(1:x_end,1:y_end)
#endif !TWO

!**********************************************************************************************************
#ifdef ONE
!**********************************************************************************************************
        do i=2,x_end-1 
                if (soundspeed < 0D0) then 
                        F_riemann_left(i)  = f_l(i)
                        F_riemann_right(i) = f_l(i+1)


                elseif (soundspeed > 0D0) then 
                        F_riemann_left(i)  = f_r(i-1)
                        F_riemann_right(i) = f_r(i)
                endif 
        enddo

#endif !ONE


!**********************************************************************************************************
#ifdef TWO
!**********************************************************************************************************
        do i=2,x_end-1 
                do j=2,y_end-1
                        if (soundspeed_x < 0D0) then 
                                F_riemann_left_x(i,j)  = f_l(i,j)
                                F_riemann_right_x(i,j) = f_l(i+1,j)


                        elseif (soundspeed_x > 0D0) then 
                                F_riemann_left_x(i,j)  = f_r(i-1,j)
                                F_riemann_right_x(i,j) = f_r(i,j)
                        endif 
                enddo
        enddo
#endif !TWO


return
end subroutine riemann_x

