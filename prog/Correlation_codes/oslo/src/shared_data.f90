    module shared_data
    
!    use data_structures
    save


    type                                                  :: detector_map
        integer                                           :: Frame_pos          ! frame position
        integer                                           :: ADC_ID             ! data channel/ADC-ID
        integer                                           :: ring_no            ! ring number
        real(4)                                           :: theta_d            ! theta degrees
        real(4)                                           :: phi_d              ! phi degrees
        character(len=20)                                 :: notes              ! short note
    end type detector_map

    type                                                  :: angle_list
        integer                                           :: idet               ! i detector number in detector map 
        integer                                           :: jdet               ! j detector number in detector map
        real(4)                                           :: cos_angle          ! cosine of angular difference
        real(4)                                           :: angle_diff         ! angular difference
    end type angle_list

    type                                                  :: ac_list
        integer                                           :: idet               ! i detector number in detector map 
        integer                                           :: jdet               ! j detector number in detector map
        real(4)                                           :: wdc                ! directional correlation
    end type ac_list


    type(detector_map),dimension(40)                      :: gamma_map          ! Cactus map
    type(detector_map),dimension(80)                      :: Siri_map           ! particle segment map
    type(angle_list),dimension(400)                       :: cosine_list        ! cosine list
    type(ac_list),dimension(400)                          :: dco_list           ! dco list
    
    real(4),dimension(6)                                  :: Ring_Angles        ! Theta angles of the 6 rings
    real(4),dimension(30,30)                              :: cosij              ! cosines of angle-differences

    integer                                               :: Ngamma_det         ! number of gamma detectors
    integer                                               :: Npart_det          ! number of particle detectors
    integer                                               :: Nlist              ! Number of gamma-gamma combinations


    logical                                               :: L_data_loaded      ! true if angle data loaded


    character(len=11)                                     :: DAY                    ! day
    character(len=8)                                      :: ITIME                  ! time

!    character(len=80)                                     :: DataInFile             ! R(T) dat input file name including extension
!    character(len=80)                                     :: W_DataInFile           ! W(phi) dat input file name including extension




!    real(4)                                               :: I_nuc                  ! spin of probe state

    real(4),PARAMETER                                     :: PI = 3.14159265        ! pi
    real(4),parameter                                     :: sf=57.29577951         ! radians to degrees
    real(4),parameter                                     :: c_um_ps=299.792458     ! speed of light in microns/ps
    

    
    end module shared_data
!------------------------------------------------------------------------------------------------------     
!------------------------------------------------------------------------------------------------------
    module shared_DCO_parameters
!
!   this module shares directional correlation parameters
!    
    save

    real(4),dimension(0:68)                               :: Qk                 ! Qk for gamma detectors (hard wired to date - must read later) 
    real(4)                                               :: I1                 ! spin of first nuclear state
    real(4)                                               :: I2                 ! spin of second nuclear state
    real(4)                                               :: I3                 ! spin of third nuclear state

    real(4)                                               :: L1_1               ! L1 multiploarity of first radiation 
    real(4)                                               :: L2_1               ! L2 multiploarity of first radiation 
    real(4)                                               :: del1               ! L2/L1 mixing ratio for 1st transtition
    real(4)                                               :: L1_2               ! L1 multiploarity of second radiation 
    real(4)                                               :: L2_2               ! L2 multiploarity of second radiation 
    real(4)                                               :: del2               ! L2/L1 mixing ratio for 2st transtition

    real(4)                                               :: theta1             ! theta angle for 1st radiation detection
    real(4)                                               :: theta2             ! theta angle for 2nd radiation detection
    real(4)                                               :: phi1               ! phi angle for 1st radiation detection
    real(4)                                               :: phi2               ! phi angle for 2nd radiation detection


    integer                                               :: kmax               ! max rank

    complex(4),dimension(0:68,-68:68)                     :: rhoI1              ! initial state tensor


    end module shared_DCO_parameters
