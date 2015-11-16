    subroutine DCO_3_levels(wdco)

!   Evaluates DCO for 3 levels
    
    use shared_DCO_parameters
    implicit none
    
    integer                                               :: mode=1             ! transition observed
    complex(4),dimension(0:68,-68:68)                     :: rhoout             ! intermediate state tensor
    real(4)                                               :: wdco               ! correlation value

!
!	calculate statistical tensor for intermediate state:
!
	call rhomQ(mode,I1,I2,L1_1,L2_1,del1,theta1,phi1,rhoI1,rhoout,kmax,Qk)
    call wobsQ(I2,I3,L1_2,L2_2,del2,theta2,phi2,rhoout,wdco,kmax,Qk)
!    write(6,*)' wdco =',wdco

    
    
    
    end subroutine DCO_3_levels