! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module DataStructure
  ! Define the new type DataStructure
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  implicit none

  integer,parameter,public::Table_Line_Number_normal = 400
  integer,parameter,public::Table_Line_Number_PMS = 500
  integer,public,save::Table_Line_Number = 400
  integer,parameter,private::Data_Number_GE = 42    ! WARNING : adapt this value to the number of data hereafter:
  integer,parameter,private::Data_Number_SE = 145   ! WARNING : adapt this value to the number of data hereafter:
  integer,public::Data_Number = -1
  integer,parameter,private::i_time_GE=1,i_mass_GE=2,i_logL_GE=3,i_logTeff_corr_GE=4, &
    i_H1_surf_GE=5,i_He4_surf_GE=6,i_C12_surf_GE=7,i_C13_surf_GE=8,i_N14_surf_GE=9, &
    i_O16_surf_GE=10,i_O17_surf_GE=11,i_O18_surf_GE=12,i_Ne20_surf_GE=13,i_Ne22_surf_GE=14, &
    i_Al26_surf_GE=15,i_Mcc_GE=16,i_logTeff_GE=17,i_Mdot_GE=18,i_rhoc_GE=19,i_Tc_GE=20, &
    i_H1_cen_GE=21,i_He4_cen_GE=22,i_C12_cen_GE=23,i_C13_cen_GE=24,i_N14_cen_GE=25, &
    i_O16_cen_GE=26,i_O17_cen_GE=27,i_O18_cen_GE=28,i_Ne20_cen_GE=29,i_Ne22_cen_GE=30, &
    i_Al26_cen_GE=31,i_Omega_surf_GE=32,i_Omega_cen_GE=33,i_oblat_GE=34,i_Mdot_enhencement_GE=35, &
    i_v_crit1_GE=36,i_v_crit2_GE=37,i_v_equa_GE=38,i_Omega_Omcrit_GE=39,i_Gamma_Ed_GE=40, &
    i_Mdot_mec_GE=41,i_L_tot_GE=42
  integer,parameter,private::i_time_SE=1,i_logTeff_SE=2,i_logL_SE=3,i_logLgrav_SE=4, &
    i_mass_SE=5,i_R_SE=6,i_logg_SE=7,i_logrhophot_SE=8,i_Mdot_SE=9,i_Tc_SE=10,i_Pc_SE=11, &
    i_rhoc_SE=12,i_logTmax_SE=13,i_MTmax_SE=14,i_logrhoTmax_SE=15,i_epsnucl_SE=16,i_epsgrav_SE=17, &
    i_epsnu_SE=18,i_MrbCE_SE=19,i_RbCE_SE=20,i_logTbCE_SE=21,i_logrhobCE_SE=22,i_MrtCC_SE=23, &
    i_RtCC_SE=24,i_logTtCC_SE=25,i_logrhotCC_SE=26,i_Taumax_SE=27,i_Romax_SE=28,i_Taug_SE=29, &
    i_Rog_SE=30,i_TauHp2_SE=31,i_RoHp2_SE=32,i_TauHp_SE=33,i_RoHp_SE=34,i_TauR2_SE=35,i_RoR2_SE=36, &
    i_TauM2_SE=37,i_RoM2_SE=38,i_Taumax_cc_SE=39,i_Romax_cc_SE=40,i_Taug_cc_SE=41,i_Rog_cc_SE=42, &
    i_TauHp2_cc_SE=43,i_RoHp2_cc_SE=44,i_TauHp_cc_SE=45,i_RoHp_cc_SE=46,i_TauR2_cc_SE=47,i_RoR2_cc_SE=48, &
    i_TauM2_cc_SE=49,i_RoM2_cc_SE=50,i_k2conv_SE=51,i_k2rad_SE=52,i_Omega_surf_SE=53,i_Omega_cen_SE=54, &
    i_v_equa_SE=55,i_Prot_SE=56,i_Jact_SE=57,i_Jcore_SE=58,i_Omega_Omcrit_SE=59,i_v_crit1_SE=60, &
    i_torque_SE=61,i_Bequi_SE=62,i_Dnu_SE=63,i_Dnuech_SE=64,i_Dnuerr_SE=65,i_numax_SE=66, &
    i_dPiasym_SE=67,i_Racctot_SE=68,i_RaccbCE_SE=69,i_RaccHe_SE=70,i_MBolT_SE=71,i_BCT_SE=72, &
    i_UBT_SE=73,i_BVT_SE=74,i_VRT_SE=75,i_VIT_SE=76,i_JKT_SE=77,i_HKT_SE=78,i_VKT_SE=79, &
    i_GVT_SE=80,i_GbpVT_SE=81,i_GrpVT_SE=82,i_MUT_SE=83,i_MBT_SE=84,i_M_VT_SE=85,i_M_RT_SE=86, &
    i_M_IT_SE=87,i_M_HT_SE=88,i_M_JT_SE=89,i_M_KT_SE=90,i_M_GT_SE=91,i_M_GbpT_SE=92, &
    i_M_GrpT_SE=93,i_H1_surf_SE=94,i_H2_surf_SE=95,i_He3_surf_SE=96,i_He4_surf_SE=97, &
    i_Li6_surf_SE=98,i_Li7_surf_SE=99,i_Be7_surf_SE=100,i_Be9_surf_SE=101,i_B10_surf_SE=102, &
    i_B11_surf_SE=103,i_C12_surf_SE=104,i_C13_surf_SE=105,i_C14_surf_SE=106,i_N14_surf_SE=107, &
    i_N15_surf_SE=108,i_O16_surf_SE=109,i_O17_surf_SE=110,i_O18_surf_SE=111,i_F19_surf_SE=112, &
    i_Ne20_surf_SE=113,i_Ne21_surf_SE=114,i_Ne22_surf_SE=115,i_Na23_surf_SE=116,i_Mg24_surf_SE=117, &
    i_Mg25_surf_SE=118,i_Mg26_surf_SE=119,i_Al26_surf_SE=120,i_Al27_surf_SE=121,i_Si28_surf_SE=122, &
    i_H1_cen_SE=123,i_H2_cen_SE=124,i_He3_cen_SE=125,i_He4_cen_SE=126,i_C12_cen_SE=127, &
    i_C13_cen_SE=128,i_C14_cen_SE=129,i_N14_cen_SE=130,i_N15_cen_SE=131,i_O16_cen_SE=132, &
    i_O17_cen_SE=133,i_O18_cen_SE=134,i_F19_cen_SE=135,i_Ne20_cen_SE=136,i_Ne21_cen_SE=137, &
    i_Ne22_cen_SE=138,i_Na23_cen_SE=139,i_Mg24_cen_SE=140,i_Mg25_cen_SE=141,i_Mg26_cen_SE=142, &
    i_Al26_cen_SE=143,i_Al27_cen_SE=144,i_Si28_cen_SE=145
  integer,public::i_time=-1,i_mass=-1,i_logL=-1,i_logTeff_corr=-1, &
    i_H1_Surf=-1,i_He4_surf=-1,i_C12_surf=-1,i_C13_surf=-1,i_N14_surf=-1, &
    i_O16_surf=-1,i_O17_surf=-1,i_O18_surf=-1,i_Ne20_surf=-1,i_Ne22_surf=-1, &
    i_Al26_surf=-1,i_Mcc=-1,i_logTeff=-1,i_Mdot=-1,i_rhoc=-1,i_Tc=-1, &
    i_H1_cen=-1,i_He4_cen=-1,i_C12_cen=-1,i_C13_cen=-1,i_N14_cen=-1, &
    i_O16_cen=-1,i_O17_cen=-1,i_O18_cen=-1,i_Ne20_cen=-1,i_Ne22_cen=-1, &
    i_Al26_cen=-1,i_Omega_surf=-1,i_Omega_cen=-1,i_oblat=-1,i_Mdot_enhencement=-1, &
    i_v_crit1=-1,i_v_crit2=-1,i_v_equa=-1,i_Omega_Omcrit=-1,i_Gamma_Ed=-1, &
    i_Mdot_mec=-1,i_L_tot=-1,i_logLgrav=-1,i_R=-1,i_logg=-1,i_logrhophot=-1, &
    i_Pc=-1,i_logTmax=-1,i_MTmax=-1,i_logrhoTmax=-1, &
    i_epsnucl=-1,i_epsgrav=-1,i_epsnu=-1,i_MrbCE=-1,i_RbCE=-1,i_logTbCE=-1, &
    i_logrhobCE=-1,i_MrtCC=-1,i_RtCC=-1,i_logTtCC=-1,i_logrhotCC=-1,i_Taumax=-1, &
    i_Romax=-1,i_Taug=-1,i_Rog=-1,i_TauHp2=-1,i_RoHp2=-1,i_TauHp=-1,i_RoHp=-1,i_TauR2=-1, &
    i_RoR2=-1,i_TauM2=-1,i_RoM2=-1,i_Taumax_cc=-1,i_Romax_cc=-1,i_Taug_cc=-1,i_Rog_cc=-1, &
    i_TauHp2_cc=-1,i_RoHp2_cc=-1,i_TauHp_cc=-1,i_RoHp_cc=-1,i_TauR2_cc=-1,i_RoR2_cc=-1, &
    i_TauM2_cc=-1,i_RoM2_cc=-1,i_k2conv=-1,i_k2rad=-1,i_Prot=-1,i_Jact=-1,i_Jcore=-1, &
    i_torque=-1,i_Bequi=-1,i_Dnu=-1,i_Dnuech=-1,i_Dnuerr=-1,i_numax=-1,i_dPiasym=-1, &
    i_Racctot=-1,i_RaccbCE=-1,i_RaccHe=-1,i_MBolT=-1,i_BCT=-1,i_UBT=-1,i_BVT=-1,i_VRT=-1, &
    i_VIT=-1,i_JKT=-1,i_HKT=-1,i_VKT=-1,i_GVT=-1,i_GbpVT=-1,i_GrpVT=-1,i_MUT=-1,i_MBT=-1, &
    i_M_VT=-1,i_M_RT=-1,i_M_IT=-1,i_M_HT=-1,i_M_JT=-1,i_M_KT=-1,i_M_GT=-1,i_M_GbpT=-1, &
    i_M_GrpT=-1,i_H2_surf=-1,i_He3_surf=-1,i_Li6_surf=-1,i_Li7_surf=-1,i_Be7_surf=-1, &
    i_Be9_surf=-1,i_B10_surf=-1,i_B11_surf=-1,i_C14_surf=-1,i_N15_surf=-1,i_F19_surf=-1, &
    i_Ne21_surf=-1,i_Na23_surf=-1,i_Mg24_surf=-1,i_Mg25_surf=-1,i_Mg26_surf=-1, &
    i_Al27_surf=-1,i_Si28_surf=-1,i_H2_cen=-1,i_He3_cen=-1,i_C14_cen=-1,i_N15_cen=-1, &
    i_F19_cen=-1,i_Ne21_cen=-1,i_Na23_cen=-1,i_Mg24_cen=-1,i_Mg25_cen=-1,i_Mg26_cen=-1, &
    i_Al27_cen=-1,i_Si28_cen=-1
  integer,dimension(33),private::positive_GE=(/i_H1_surf_GE,i_He4_surf_GE,i_C12_surf_GE, &
    i_C13_surf_GE,i_N14_surf_GE,i_O16_surf_GE,i_O17_surf_GE,i_O18_surf_GE,i_Ne20_surf_GE, &
    i_Ne22_surf_GE,i_Al26_surf_GE,i_Mcc_GE,i_H1_cen_GE,i_He4_cen_GE,i_C12_cen_GE, &
    i_C13_cen_GE,i_N14_cen_GE,i_O16_cen_GE,i_O17_cen_GE,i_O18_cen_GE,i_Ne20_cen_GE, &
    i_Ne22_cen_GE,i_Al26_cen_GE,i_Omega_surf_GE,i_Omega_cen_GE,i_oblat_GE, &
    i_Mdot_enhencement_GE,i_v_crit1_GE,i_v_crit2_GE,i_v_equa_GE,i_Omega_Omcrit_GE, &
    i_Gamma_Ed_GE,i_L_tot_GE/)
  integer,dimension(25),private::less_than_one_GE=(/i_H1_surf_GE,i_He4_surf_GE,i_C12_surf_GE, &
    i_C13_surf_GE,i_N14_surf_GE,i_O16_surf_GE,i_O17_surf_GE,i_O18_surf_GE,i_Ne20_surf_GE, &
    i_Ne22_surf_GE,i_Al26_surf_GE,i_Mcc_GE,i_H1_cen_GE,i_He4_cen_GE,i_C12_cen_GE, &
    i_C13_cen_GE,i_N14_cen_GE,i_O16_cen_GE,i_O17_cen_GE,i_O18_cen_GE,i_Ne20_cen_GE, &
    i_Ne22_cen_GE,i_Al26_cen_GE,i_oblat_GE,i_Omega_Omcrit_GE/)
  integer,dimension(86),private::positive_SE=(/i_MrbCE_SE,i_RbCE_SE,i_MrtCC_SE,i_RtCC_SE, &
    i_Taumax_SE,i_Romax_SE,i_Taug_SE,i_Rog_SE,i_TauHp2_SE,i_RoHp2_SE,i_TauHp_SE,i_RoHp_SE, &
    i_TauR2_SE,i_RoR2_SE,i_TauM2_SE,i_RoM2_SE,i_Taumax_cc_SE,i_Romax_cc_SE,i_Taug_cc_SE, &
    i_Rog_cc_SE,i_TauHp2_cc_SE,i_RoHp2_cc_SE,i_TauHp_cc_SE,i_RoHp_cc_SE,i_TauR2_cc_SE, &
    i_RoR2_cc_SE,i_TauM2_cc_SE,i_RoM2_cc_SE,i_Omega_surf_SE,i_Omega_cen_SE,i_v_equa_SE, &
    i_Prot_SE,i_Omega_Omcrit_SE,i_v_crit1_SE,i_H1_surf_SE,i_H2_surf_SE,i_He3_surf_SE, &
    i_He4_surf_SE,i_Li6_surf_SE,i_Li7_surf_SE,i_Be7_surf_SE,i_Be9_surf_SE,i_B10_surf_SE, &
    i_B11_surf_SE,i_C12_surf_SE,i_C13_surf_SE,i_C14_surf_SE,i_N14_surf_SE,i_N15_surf_SE, &
    i_O16_surf_SE,i_O17_surf_SE,i_O18_surf_SE,i_F19_surf_SE,i_Ne20_surf_SE,i_Ne21_surf_SE, &
    i_Ne22_surf_SE,i_Na23_surf_SE,i_Mg24_surf_SE,i_Mg25_surf_SE,i_Mg26_surf_SE,i_Al26_surf_SE, &
    i_Al27_surf_SE,i_Si28_surf_SE,i_H1_cen_SE,i_H2_cen_SE,i_He3_cen_SE,i_He4_cen_SE,i_C12_cen_SE, &
    i_C13_cen_SE,i_C14_cen_SE,i_N14_cen_SE,i_N15_cen_SE,i_O16_cen_SE,i_O17_cen_SE,i_O18_cen_SE, &
    i_F19_cen_SE,i_Ne20_cen_SE,i_Ne21_cen_SE,i_Ne22_cen_SE,i_Na23_cen_SE,i_Mg24_cen_SE, &
    i_Mg25_cen_SE,i_Mg26_cen_SE,i_Al26_cen_SE,i_Al27_cen_SE,i_Si28_cen_SE/)
  integer,dimension(53),private::less_than_one_SE=(/i_Omega_Omcrit_SE,i_H1_surf_SE,i_H2_surf_SE, &
    i_He3_surf_SE,i_He4_surf_SE,i_Li6_surf_SE,i_Li7_surf_SE,i_Be7_surf_SE,i_Be9_surf_SE, &
    i_B10_surf_SE,i_B11_surf_SE,i_C12_surf_SE,i_C13_surf_SE,i_C14_surf_SE,i_N14_surf_SE, &
    i_N15_surf_SE,i_O16_surf_SE,i_O17_surf_SE,i_O18_surf_SE,i_F19_surf_SE,i_Ne20_surf_SE, &
    i_Ne21_surf_SE,i_Ne22_surf_SE,i_Na23_surf_SE,i_Mg24_surf_SE,i_Mg25_surf_SE,i_Mg26_surf_SE, &
    i_Al26_surf_SE,i_Al27_surf_SE,i_Si28_surf_SE,i_H1_cen_SE,i_H2_cen_SE,i_He3_cen_SE, &
    i_He4_cen_SE,i_C12_cen_SE,i_C13_cen_SE,i_C14_cen_SE,i_N14_cen_SE,i_N15_cen_SE,i_O16_cen_SE, &
    i_O17_cen_SE,i_O18_cen_SE,i_F19_cen_SE,i_Ne20_cen_SE,i_Ne21_cen_SE,i_Ne22_cen_SE, &
    i_Na23_cen_SE,i_Mg24_cen_SE,i_Mg25_cen_SE,i_Mg26_cen_SE,i_Al26_cen_SE,i_Al27_cen_SE, &
    i_Si28_cen_SE/)
  integer,dimension(:),allocatable::positive,less_than_one

  integer,parameter,public::Additional_Data_Number = 24  !WARNING : adapt this value to the number of data
                                                        !hereafter:
  integer,parameter,public::i_MBol=1,i_MV=2,i_UB=3,i_BV=4,i_B2V1=5,i_VR=6,i_VI=7,i_JK=8,i_HK=9,i_VK=10, &
    i_GV=11,i_GbpV=12,i_GrpV=13,i_Gflag=14,i_PolarRadius=15,i_polar_gravity=16,i_MV_noisy=17,i_BV_noisy=18, &
    i_BC=19,i_logL_gd=20,i_logTeff_gd=21,i_logL_lgd=22,i_logTeff_lgd=23,i_mean_gravity=24

  character(*),parameter,private::ReadFormat_GE = '(i3,1x,e22.15,1x,f11.6,2(1x,f9.6),2(1x,e14.7),1p,8(1x,e14.7),1x,e10.3,&
     &1x,0pf7.4,1x,f9.6,1x,f8.3,2(1x,f9.6),2(1x,e14.7),1p,8(1x,e14.7),5(1x,e10.3),3(1x,e9.2),&
     &0p,2(1x,f9.6),1x,1pe14.7,1x,e17.10)'
  character(*),parameter,private::ReadFormat_SE = '(1x,i3,1x,e17.10,3(1x,f9.6),1x,f13.8,1x,f11.5,1x,f5.2,1x,e10.3,&
      &1x,f10.6,6(1x,f9.6),3(1x,e11.4),1x,f10.7,1x,e10.3,2(1x,f8.5),1x,f10.7,1x,e10.3,2(1x,f8.5),28(1x,e11.4),1x,&
      &e9.2,1x,e11.4,2(1x,e9.2),1x,e11.4,1x,e9.2,1x,e11.4,1x,e9.2,8(1x,e11.4),23(1x,f8.4),52(1x,e11.4))'

  character(512),private::ReadFormat

  logical,public,save:: verbose

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  type type_DataStructure

    real(kind=8)::Metallicity,mass_ini,Omega_Omcrit_ini

    character(512)::FileName

    integer,dimension(:),allocatable::line
    real(kind=8),dimension(:,:),allocatable::Data_Table

  end type type_DataStructure
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  type type_TimeModel

    integer::star_ID,Is_a_Binary
    real(kind=8)::Metallicity,mass_ini,Omega_Omcrit_ini,Current_Time,mass_ratio,Angle_of_View

    real(kind=8),dimension(:),allocatable::Data_Line
    real(kind=8),dimension(Additional_Data_Number)::Additional_Data_Line

  end type type_TimeModel
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  public::FillData
  public::Init_DataStructure
  public::Del_DataStructure
  public::Init_Indices
  public::Init_TimeModel

contains
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Init_DataStructure(Number_of_lines,Number_of_data,MyDataStructure)
  ! Initiate the dimension of the DataStructure dynamically.
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer, intent(in):: Number_of_lines,Number_of_data
    type(type_DataStructure),intent(out)::MyDataStructure

    integer::i,j

    allocate(MyDataStructure%line(Number_of_lines))
    allocate(MyDataStructure%Data_Table(Number_of_lines,Number_of_data))

    do i=1,Number_of_lines
      MyDataStructure%line(i) = -1
    enddo
    do i=1,Number_of_lines
      do j=1,Number_of_data
        MyDataStructure%Data_Table(i,j) = -10.d0
      enddo
    enddo

    return
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  end subroutine Init_DataStructure
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Del_DataStructure(MyDataStructure)
  ! Delete DataStructure arrays.
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    type(type_DataStructure),intent(inout)::MyDataStructure

    deallocate(MyDataStructure%line)
    deallocate(MyDataStructure%Data_Table)

  end subroutine Del_DataStructure
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Init_Indices(MyFormat)
  ! Delete DataStructure arrays.
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer,intent(in):: MyFormat

    if (MyFormat == 1) then
    ! GENEC format
      allocate(positive(size(positive_GE)))
      allocate(less_than_one(size(less_than_one_GE)))
      positive(:) = positive_GE(:)
      less_than_one(:) = less_than_one_GE(:)
      ReadFormat = ReadFormat_GE
      Data_Number = Data_Number_GE
      i_time = i_time_GE
      i_mass = i_mass_GE
      i_logL = i_logL_GE
      i_logTeff_corr = i_logTeff_corr_GE
      i_H1_Surf = i_H1_Surf_GE
      i_He4_surf = i_He4_surf_GE
      i_C12_surf = i_C12_surf_GE
      i_C13_surf = i_C13_surf_GE
      i_N14_surf = i_N14_surf_GE
      i_O16_surf = i_O16_surf_GE
      i_O17_surf = i_O17_surf_GE
      i_O18_surf = i_O18_surf_GE
      i_Ne20_surf = i_Ne20_surf_GE
      i_Ne22_surf = i_Ne22_surf_GE
      i_Al26_surf = i_Al26_surf_GE
      i_Mcc = i_Mcc_GE
      i_logTeff = i_logTeff_GE
      i_Mdot = i_Mdot_GE
      i_rhoc = i_rhoc_GE
      i_Tc = i_Tc_GE
      i_H1_cen = i_H1_cen_GE
      i_He4_cen = i_He4_cen_GE
      i_C12_cen = i_C12_cen_GE
      i_C13_cen = i_C13_cen_GE
      i_N14_cen = i_N14_cen_GE
      i_O16_cen = i_O16_cen_GE
      i_O17_cen = i_O17_cen_GE
      i_O18_cen = i_O18_cen_GE
      i_Ne20_cen = i_Ne20_cen_GE
      i_Ne22_cen = i_Ne22_cen_GE
      i_Al26_cen = i_Al26_cen_GE
      i_Omega_surf = i_Omega_surf_GE
      i_Omega_cen = i_Omega_cen_GE
      i_oblat = i_oblat_GE
      i_Mdot_enhencement = i_Mdot_enhencement_GE
      i_v_crit1 = i_v_crit1_GE
      i_v_crit2 = i_v_crit2_GE
      i_v_equa = i_v_equa_GE
      i_Omega_Omcrit = i_Omega_Omcrit_GE
      i_Gamma_Ed = i_Gamma_Ed_GE
      i_Mdot_mec = i_Mdot_mec_GE
      i_L_tot = i_L_tot_GE
    else if (MyFormat == 2) then
    ! Starevol format
      allocate(positive(size(positive_SE)))
      allocate(less_than_one(size(less_than_one_SE)))
      positive(:) = positive_SE(:)
      less_than_one(:) = less_than_one_SE(:)
      ReadFormat = ReadFormat_SE
      Data_Number = Data_Number_SE
      i_time = i_time_SE
      i_logTeff = i_logTeff_SE
      i_logL = i_logL_SE
      i_logLgrav = i_logLgrav_SE
      i_mass = i_mass_SE
      i_R = i_R_SE
      i_logg = i_logg_SE
      i_logrhophot = i_logrhophot_SE
      i_Mdot = i_Mdot_SE
      i_Tc = i_Tc_SE
      i_Pc = i_Pc_SE
      i_rhoc = i_rhoc_SE
      i_logTmax = i_logTmax_SE
      i_MTmax = i_MTmax_SE
      i_logrhoTmax = i_logrhoTmax_SE
      i_epsnucl = i_epsnucl_SE
      i_epsgrav = i_epsgrav_SE
      i_epsnu = i_epsnu_SE
      i_MrbCE = i_MrbCE_SE
      i_RbCE = i_RbCE_SE
      i_logTbCE = i_logTbCE_SE
      i_logrhobCE = i_logrhobCE_SE
      i_MrtCC = i_MrtCC_SE
      i_RtCC = i_RtCC_SE
      i_logTtCC = i_logTtCC_SE
      i_logrhotCC = i_logrhotCC_SE
      i_Taumax = i_Taumax_SE
      i_Romax = i_Romax_SE
      i_Taug = i_Taug_SE
      i_Rog = i_Rog_SE
      i_TauHp2 = i_TauHp2_SE
      i_RoHp2 = i_RoHp2_SE
      i_TauHp = i_TauHp_SE
      i_RoHp = i_RoHp_SE
      i_TauR2 = i_TauR2_SE
      i_RoR2 = i_RoR2_SE
      i_TauM2 = i_TauM2_SE
      i_RoM2 = i_RoM2_SE
      i_k2conv = i_k2conv_SE
      i_k2rad = i_k2rad_SE
      i_Omega_surf = i_Omega_surf_SE
      i_Omega_cen = i_Omega_cen_SE
      i_v_equa = i_v_equa_SE
      i_Prot = i_Prot_SE
      i_Jact = i_Jact_SE
      i_Jcore = i_Jcore_SE
      i_Omega_Omcrit = i_Omega_Omcrit_SE
      i_v_crit1 = i_v_crit1_SE
      i_torque = i_torque_SE
      i_Bequi = i_Bequi_SE
      i_Dnu = i_Dnu_SE
      i_Dnuech = i_Dnuech_SE
      i_Dnuerr = i_Dnuerr_SE
      i_numax = i_numax_SE
      i_dPiasym = i_dPiasym_SE
      i_Racctot = i_Racctot_SE
      i_RaccbCE = i_RaccbCE_SE
      i_RaccHe = i_RaccHe_SE
      i_MBolT = i_MBolT_SE
      i_BCT = i_BCT_SE
      i_UBT = i_UBT_SE
      i_BVT = i_BVT_SE
      i_VRT = i_VRT_SE
      i_VIT = i_VIT_SE
      i_JKT = i_JKT_SE
      i_HKT = i_HKT_SE
      i_VKT = i_VKT_SE
      i_GVT = i_GVT_SE
      i_GbpVT = i_GbpVT_SE
      i_GrpVT = i_GrpVT_SE
      i_MUT = i_MUT_SE
      i_MBT = i_MBT_SE
      i_M_VT = i_M_VT_SE
      i_M_RT = i_M_RT_SE
      i_M_IT = i_M_IT_SE
      i_M_HT = i_M_HT_SE
      i_M_JT = i_M_JT_SE
      i_M_KT = i_M_KT_SE
      i_M_GT = i_M_GT_SE
      i_M_GbpT = i_M_GbpT_SE
      i_M_GrpT = i_M_GrpT_SE
      i_H1_surf = i_H1_surf_SE
      i_H2_surf = i_H2_surf_SE
      i_He3_surf = i_He3_surf_SE
      i_He4_surf = i_He4_surf_SE
      i_Li6_surf = i_Li6_surf_SE
      i_Li7_surf = i_Li7_surf_SE
      i_Be7_surf = i_Be7_surf_SE
      i_Be9_surf = i_Be9_surf_SE
      i_B10_surf = i_B10_surf_SE
      i_B11_surf = i_B11_surf_SE
      i_C12_surf = i_C12_surf_SE
      i_C13_surf = i_C13_surf_SE
      i_C14_surf = i_C14_surf_SE
      i_N14_surf = i_N14_surf_SE
      i_N15_surf = i_N15_surf_SE
      i_O16_surf = i_O16_surf_SE
      i_O17_surf = i_O17_surf_SE
      i_O18_surf = i_O18_surf_SE
      i_F19_surf = i_F19_surf_SE
      i_Ne20_surf = i_Ne20_surf_SE
      i_Ne21_surf = i_Ne21_surf_SE
      i_Ne22_surf = i_Ne22_surf_SE
      i_Na23_surf = i_Na23_surf_SE
      i_Mg24_surf = i_Mg24_surf_SE
      i_Mg25_surf = i_Mg25_surf_SE
      i_Mg26_surf = i_Mg26_surf_SE
      i_Al26_surf = i_Al26_surf_SE
      i_Al27_surf = i_Al27_surf_SE
      i_Si28_surf = i_Si28_surf_SE
      i_H1_cen = i_H1_cen_SE
      i_H2_cen = i_H2_cen_SE
      i_He3_cen = i_He3_cen_SE
      i_He4_cen = i_He4_cen_SE
      i_C12_cen = i_C12_cen_SE
      i_C13_cen = i_C13_cen_SE
      i_C14_cen = i_C14_cen_SE
      i_N14_cen = i_N14_cen_SE
      i_N15_cen = i_N15_cen_SE
      i_O16_cen = i_O16_cen_SE
      i_O17_cen = i_O17_cen_SE
      i_O18_cen = i_O18_cen_SE
      i_F19_cen = i_F19_cen_SE
      i_Ne20_cen = i_Ne20_cen_SE
      i_Ne21_cen = i_Ne21_cen_SE
      i_Ne22_cen = i_Ne22_cen_SE
      i_Na23_cen = i_Na23_cen_SE
      i_Mg24_cen = i_Mg24_cen_SE
      i_Mg25_cen = i_Mg25_cen_SE
      i_Mg26_cen = i_Mg26_cen_SE
      i_Al26_cen = i_Al26_cen_SE
      i_Al27_cen = i_Al27_cen_SE
      i_Si28_cen = i_Si28_cen_SE
      ! For starevol format, we set i_logTeff_corr to i_logTeff, since they are not distinguished.
      i_logTeff_corr = i_logTeff
    else
      write(*,*) 'Problems with the requested format, aborting...'
      stop
    endif

  end subroutine Init_Indices
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Init_TimeModel(Number_of_data,MyTimeModel)
  ! Initiate the dimension of the TimeModel dynamically.
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer, intent(in):: Number_of_data
    type(type_TimeModel),intent(out)::MyTimeModel

    integer::i

    allocate(MyTimeModel%Data_Line(Number_of_data))

    do i=1,Number_of_data
      MyTimeModel%Data_Line(i) = -1.d0
    enddo

    return
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  end subroutine Init_TimeModel
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine FillData(Z,Mini,Omini,FileNameIn,Structure)
    ! Read the evolution file and fill the structure.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    implicit none

    real(kind=8),intent(in)::Z,Mini,Omini
    character(512),intent(in)::FileNameIn

    type(type_DataStructure),intent(inout)::Structure

    integer::LineNumber,error=0,i,j
    integer,parameter::ReadUnit=30,Line_to_Remove = 3

    open(unit=ReadUnit,file=trim(FileNameIn),iostat=error,status='old')
    if (error /= 0) then
      write(*,*) 'Error opening file ',trim(FileNameIn),'. Aborting...'
      stop
    endif

    ! The table files have 18 lines at the beginning to remove.
    do i=1,Line_to_Remove
      read(ReadUnit,*)
    enddo

    ! Determine the number of lines
    LineNumber = 0
    do while (error == 0)
      read(ReadUnit,*,iostat=error)
      if (error /= 0) then
        exit
      endif
      LineNumber = LineNumber+1
    enddo

    ! Check if the input file has the right format.
    if (LineNumber /= Table_Line_Number) then
      write(*,*) 'The file ',trim(FileNameIn),' is not at the .grids format: not enough or too much lines.'
      stop
    endif

    ! Fill the structure
    Structure%Metallicity = Z
    Structure%mass_ini = Mini
    Structure%Omega_Omcrit_ini = Omini
    Structure%FileName = FileNameIn

    if (verbose) then
      write(*,*) 'Reading ',trim(Structure%FileName),'...'
    endif
    rewind(ReadUnit)
    ! The table files have 18 lines at the beginning to remove.
    do i=1,Line_to_Remove
      read(ReadUnit,*)
    enddo

    ! Read the data, and check the number of different lines.
    do i=1,LineNumber
      read(ReadUnit,trim(ReadFormat),iostat=error)Structure%line(i),(Structure%Data_Table(i,j),j=1,Data_Number)
      if (error /= 0) then
        write(*,*) 'Problem reading file ',trim(FileNameIn),'. Aborting...'
        stop
      endif
      ! To perform a better interpolation, the null mass loss rate (in log) are put to -32.
      if (i_Mdot > 0) then
        if (abs(Structure%Data_Table(i,i_Mdot)) < 1.d-30) then
          Structure%Data_Table(i,i_Mdot) = -32.d0
        endif
      endif
      if (i_Mdot_mec > 0) then
        if (abs(Structure%Data_Table(i,i_Mdot_mec)) < 1.d-30) then
          Structure%Data_Table(i,i_Mdot_mec) = -32.d0
        endif
      endif
    enddo

    ! Set the time on the ZAMS to 1 yr (to avoid complication due to log interpolation).
    if (Structure%Data_Table(1,i_time) /= 0.d0) then
      Structure%Data_Table(:,i_time) = Structure%Data_Table(:,i_time) - Structure%Data_Table(1,i_time) + 1.d0
    else
      Structure%Data_Table(1,i_time) = 1.d0
    endif

    if (verbose) then
      write(*,*) '      done !'
    endif

    close(ReadUnit)

    return

  end subroutine FillData
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module DataStructure
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module Constant
  ! Contains constants
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  implicit none

  real(kind=8),parameter::pi = 3.1415926535897932384626433d0, & ! Pi
    L_sun = 3.846d33, &                   ! Solar luminosity
    sigma_SB = 5.670373d-5, &             ! Stefan-Boltzman constant
    G_Newton = 6.67384d-8, &              ! Gravitational constant
    M_sun = 1.9891d33, &                  ! Solar mass
    Z_sun = 0.014d0, &                    ! Solar metallicity
    R_sun = 6.961d10, &                   ! Solar radius
    sL_sun = 10.61d0                      ! Solar log spectroscopic luminosity

end module Constant
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module VariousParameters
  ! Contains various parameters (among of them, some can be changed later
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  use DataStructure, only: type_DataStructure

  implicit none

  integer,public,save:: star_number=1000                        ! Current numbe of star in the cluster.
  integer,public,save:: table_format = 1                        ! Table format (1) GENEC, (2) Starevol
  integer,public,save::i_metallicity=0                          ! Metallicity distribution: dirac (0)
  integer,public,save::ivdist=1                                 ! Velocity distribution: uniform (0),
                                                                ! huang(1), huang-gies(2), dirac_om(3)
  integer,public,save::iangle=2                                 ! uniforme dist. (1), vsini (2), Dirac (3)
                                                                ! or not (0) of the angle of view
  integer,public,save::inoise=1                                 ! noise adjunction (1) or not (0)
                                                                ! (data without noise are saved whatever
                                                                ! the case)
  integer,public,save::grav_dark=2                              ! gravity darkening law: (1) vonZeipel (1924),
                                                                !                        (2) Espinosa-Lara & Reutord (2011)
  integer,public,save::limb_dark=1                              ! no limb-darkening (0), with LD (1)
  integer,public,save::Current_Number                           ! Current star number
  integer,public,save::SN_Number                                ! SN number (star more massive than m_norm_sup)
  integer,public,save::Small_Number                             ! small mass star number (< m_norm_inf).
  integer,public,save::Cepheid_Number                           ! Cepheid number
  integer,public,save::FastRot_Number                           ! Fast rotators number
  integer,public,save::Z_Number                                 ! Metallicity number
  integer,public,save::IMF_type = 1                             ! IMF. Salpeter (1), Kroupa (2)
  integer,public,save::Comp_Mode = 0                            ! Various computation mode : (1) cluster, (2)
                                                                ! isochrone, (3) Population mode, (4) Single model
  integer,public,save::Colour_Calibration_mode = 2              ! Colour-Teff calibration (1) as in Paper I 2011,
                                                                ! (2) Worthey & Lee, ApJS 193 1 (2011).

  integer,pointer::mass_Number_array(:)                         ! Array containing the number of mass for each Z.
  integer,pointer::omega_Number_array(:,:)                      ! Array containg for each Z and mass the number
                                                                ! of different rotation velocities
  real(kind=8),public,save::Fixed_AoV_latitude=22.d0            ! Fixed angle of view from pole to equator
  real(kind=8),public,save::Fixed_AoV                           ! Same angle, in colatitude.
  real(kind=8),public,parameter::m_IMF_inf_Grids2012=0.8d0, m_IMF_sup_Grids2012=120.d0
  real(kind=8),public,parameter::m_IMF_inf_BeGrids=1.7d0, m_IMF_sup_BeGrids=15.d0
  real(kind=8),public,parameter::m_IMF_inf_NamiGrids=0.5d0, m_IMF_sup_NamiGrids=3.5d0
  real(kind=8),public,save::m_IMF_inf=m_IMF_inf_BeGrids, m_IMF_sup=m_IMF_sup_BeGrids  ! IMF min and max mass
  real(kind=8),public,save::fixed_metallicity = 0.014d0         ! Metallicity used in case of Dirac distribution
  real(kind=8),public,save::om_ivdist=0.40d0                    ! In case of Dirac omega-distribution
  real(kind=8),public,save::binary_prob=0.30d0                  ! Binarity probability
  real(kind=8),public,save::Star_Z                              ! Current stellar metallicity
  real(kind=8),public,save::Star_mass                           ! Current stellar mass
  real(kind=8),public,save::Star_omega                          ! Current stellar velocity
  real(kind=8),public,save::Star_AoV                            ! Current stellar angle of view
  real(kind=8),public,parameter::log_age_max=11.0d0             ! Maximal log(age) for cluster allowed with the
  real(kind=8),public,save::sigma_mv=0.150d0, sigma_bv=0.10d0   ! standard deviation in MV et B-V
                                                                ! current tables.
  real(kind=8),public,save::age_log                             ! wanted log(age).
  real(kind=8),public,save::Cluster_mass                        ! Total cluster mass
  real(kind=8),dimension(:,:,:),public,pointer,save::omega_List=>null() ! List of the rotation velocities.
  real(kind=8),dimension(:,:),public,pointer,save::mass_List=>null() ! List of the masses.
  real(kind=8),dimension(:),public,pointer,save::Z_List=>null() ! List of the metallicities.

  logical,public,save::PMS=.false.
  logical,public,save::Compute                                  ! Used in the main loop

  type(type_DataStructure),dimension(:,:,:),public,pointer,save::All_Data_Array=>null() ! Array containing
                                                                ! in each coordinate (ie, Z,m and omega)
                                                                ! the data structure.
  character(9),public:: grid = "BeGrids"
  character(256),public::Std_Path,Std_Path_tables
                                                                ! directory of following files:
  character(*),parameter,public::HuangFile = 'HuangDist.dat', & ! File of the Huang distribution
    HGFile = 'HGDist.dat', &       ! File of the Huang & Gies distribution
    LumFileVZ = 'CorrLumVZ.dat', &     ! File of the luminosity corrections according to von Zeipel 1924
    TeffFileVZ = 'CorrTeffVZ.dat', &   ! File of the Teff corrections according to von Zeipel 1924
    LumFileELR = 'CorrLumELR.dat', &     ! File of the luminosity corrections according to ELR 2011
    TeffFileELR = 'CorrTeffELR.dat', &   ! File of the Teff corrections according to ELR 2011
    GravFileVZ = 'CorrGravVZ.dat', &   ! File of the Teff corrections according to von Zeipel 1924
    GravFileELR = 'CorrGravELR.dat', &   ! File of the Teff corrections according to ELR 2011
    LumGridVZ = 'CorrLumVZ_LD.dat', &   !new! Grid of files for lum corrections (vZ with LD)
    TeffGridVZ = 'CorrTeffVZ_LD.dat', & !new! Grid of files for Teff corrections (vZ with LD)
    LumGridELR = 'CorrLumELR_LD.dat', &   !new! Grid of files for lum corrections (ELR with LD)
    TeffGridELR = 'CorrTeffELR_LD.dat', & !new! Grid of files for Teff corrections (ELR with LD)
    VCrit_to_Omega_File = 'Vcrit2omega.dat', &  ! Vcrit to omega translation
    OmegaSurface_File = 'Surface_Omega.dat', & ! S vs omega/omega_crit data file.
    DataColor = 'DataColours.dat'   ! File of the data for color corrections.
  character(*),parameter,public::ListFileGrids2012='ListFileL.txt', &  ! File containing the path of the large grid files
                                 ListFileBeGrids='ListFileB.txt', &    ! File containing the path of the Be grid files
                                 ListFileFastBe='ListFileF.txt',&      ! File containing the path of the fast Be grid files
                                 ListFileNamiGrids='ListFileN.txt'     ! File containing the path of the Nami grid files

  character(*),parameter,public::GridsFormat = '(i3,1x,e22.15,1x,f11.6,2(1x,f9.6),2(1x,e14.7),1p,&
     &8(1x,e14.7),1x,e10.3,1x,0pf7.4,1x,f9.6,1x,f8.3,2(1x,f9.6),2(1x,e14.7),1p,8(1x,e14.7),&
     &5(1x,e10.3),3(1x,e9.2),0p,2(1x,f9.6),1x,1pe14.7,1x,e17.10)'

  public:: init_AoV

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine init_AoV()
    ! Transforms the given angle of view in colatitudinal angle.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use Constant, only:pi

    implicit none

    Fixed_AoV = pi/2.d0-Fixed_AoV_latitude*(pi/180.d0)

    return

  end subroutine init_AoV
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module VariousParameters
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module LoopVariables
  ! Module containing variables used only in the main loop, not useful elsewhere
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  use DataStructure, only: type_DataStructure,type_TimeModel

  implicit none

  integer, dimension(2,2), public, save::omega_Position         ! Position of the current velocity in the
                                                                ! table.
  integer, dimension(2), public, save::mass_Position            ! Position of the current mass in the
                                                                ! table.
  integer, public, save::Z_Position                             ! Position of the current metallicity in the
                                                                ! table.

  real(kind=8), dimension(2,2), public, save::omega_factor      ! Interpolation factor in omega.
  real(kind=8), dimension(2), public, save::mass_factor         ! Interpolation factor in mass.
  real(kind=8), public, save::Z_factor                          ! Interpolation factor in metllicity.

  type(type_DataStructure), public, save::Interpolated_Model
  type(type_TimeModel), dimension(:),public, pointer, save:: CurrentTime_Model=>null() ! Array containing
                                                              ! the data of the model at the given time.

end module LoopVariables
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module interpolmod
  ! Module containing routines useful for interpolation purpose.
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  public:: Linear_Interp
  public:: indice
  public:: give_factor
  public:: give_factorLog
  public:: All_Positions_and_factors
  public:: Make_InterpolatedModel
  public:: Make_TimeModel
  public:: Bilin_Interpol
  public:: Plane_Interpol

  private:: Interpol_factor
  private:: Interpol_factor_array
  private:: Interpolate_Model
  private:: Fill_Data_ZAMS
  private:: Time_Interpolation
  private:: Interpol_Mass
  private:: Interpol_Omega
  private:: omega_Position_and_factor
  private:: mass_Position_and_factor
  private:: Z_Position_and_factor
  private:: check0
  private:: check1

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8) function Linear_Interp(x,n,a,b)
    ! Linear interpolation routine
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    implicit none

    real(kind=8),intent(in)::x
    integer,intent(in)::n
    integer::k
    real(kind=8), dimension(n),intent(in):: a,b

    k=indice(x,a,n)
    Linear_Interp=b(k)+(b(k+1)-b(k))*(x-a(k))/(a(k+1)-a(k))

    return

  end function Linear_Interp
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  integer function indice(x0,x,m)
    ! Quick search of the position d of a value xo in a monotic table of m numbers x(i)
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8),intent(in):: x0
    integer,intent(in):: m
    integer:: i=0,k,n
    real(kind=8), intent(in),dimension(m):: x

    ! si  k = indice(x0,x,m)  on aura   x0 compris entre x(k) et x(k+1)
    !                              ou   x0 = x(k).
    ! si x0 exterieur a la table indice=1 si x0 du cote de x(1)
    !                            indice = m-1 si x0 du cote de x(m)

    n = m
    k = 1

    do while (n-k-1 /= 0)
      i=(k+n)/2
      if((x(i)-x0)*(x(n)-x(1)) <= 0) then
        k = i
      else
        n = i
      endif
    enddo

    indice = k

    return

  end function indice
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8) function give_factor(x_Current,x_i,x_ip1)
    ! Compute the fraction of the current position in the intervale of interpolation
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8), intent(in)::x_Current,x_i,x_ip1

    give_factor = (x_Current-x_i)/(x_ip1-x_i)

    return

  end function give_factor
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8) function give_factorLog(x_Current,x_i,x_ip1)
    ! Compute the fraction of the current position in the intervale of interpolation
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8), intent(in)::x_Current,x_i,x_ip1

    give_factorLog = (log10(x_Current)-log10(x_i))/(log10(x_ip1)-log10(x_i))

    return

  end function give_factorLog
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8) function Interpol_factor(in_1,in_2,factor)
    ! Simple linear function : out = in_1 + factor*(in_2 - in_1)
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8),intent(in)::in_1,in_2,factor

    if (factor == 0.d0) then
      Interpol_factor = in_1
    else if (factor == 1.d0) then
      Interpol_factor = in_2
    else
      Interpol_factor = in_1 + factor*(in_2 - in_1)
    endif

    return

  end function Interpol_factor
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Interpol_factor_array(in_1,in_2,factor,Number_dim1,Number_dim2,Array_out)
    ! Simple linear function : out = in_1 + factor*(in_2 - in_1), but for the data array.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer, intent(in)::Number_dim1,Number_dim2

    real(kind=8), intent(in)::factor
    real(kind=8),dimension(Number_dim1,Number_dim2),intent(in)::in_1,in_2
    real(kind=8),dimension(Number_dim1,Number_dim2),intent(out)::Array_out

    if (factor == 0.d0) then
      Array_out(:,:) = in_1(:,:)
    else if (factor == 1.d0) then
      Array_out(:,:) = in_2(:,:)
    else
      Array_out(:,:) = in_1(:,:) + factor*(in_2(:,:) - in_1(:,:))
    endif

    return

  end subroutine Interpol_factor_array
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8) function Bilin_Interpol(P1,P2,P3,P4,fact1,fact2)
    ! Perform a bilinear interpolation between 4 points. The disposition is:
    ! P4-----------P3
    !  .           .
    !  .           .
    !  .           .   fact2
    !  .           .   ^
    !  .           .   !
    ! P1-----------P2  !
    ! ---> fact1
    ! The first interpolation is done along fact1, and then along fact2.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8),intent(in)::P1,P2,P3,P4,fact1,fact2
    real(kind=8):: PA,PB

    PA = Interpol_factor(P1,P2,fact1)
    PB = Interpol_factor(P4,P3,fact1)

    Bilin_Interpol = Interpol_factor(PA,PB,fact2)

    return

  end function Bilin_Interpol
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8) function Plane_Interpol(x1,y1,P1,x2,y2,P2,x3,y3,P3,x,y)
    !
    !Esta funcion encuentra el plano que pasa por 3 vertices, para interpolar un valor intermedio.
    !
    implicit none
    real(kind=8),intent(in)::x1,y1,P1,x2,y2,P2,x3,y3,P3,x,y
    real(kind=8):: M1,M2,M3,D
    !
    M1  =   (y2 * P3) - (y3 * P2)  - ((y1 * P3) - (y3 * P1)) +  (y1 * P2) - (y2 * P1)
    M2  = -((x2 * P3) - (x3 * P2)) +  (x1 * P3) - (x3 * P1)  - ((x1 * P2) - (x2 * P1))
    M3  =   (x2 * y3) - (x3 * y2)  - ((x1 * y3) - (x3 * y1)) +  (x1 * y2) - (x2 * y1)
    D  = x1 *((y2 * P3) - (y3 * P2)) - x2 * ((y1 * P3) - (y3 * P1)) + x3 *((y1*P2) -(y2* P1))
    !
    Plane_Interpol  = (D - M1 * x - M2 * y)/ M3
    !
    return

  end function Plane_Interpol
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine omega_Position_and_factor(omega,Position_stage1,Position_stage2,Position,factor)
    ! Compute the fraction of the current position in the intervale of interpolation
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: omega_Number_array, omega_List

    implicit none

    integer, intent(in)::Position_stage1,Position_stage2
    integer, intent(out)::Position

    real(kind=8), intent(in)::omega
    real(kind=8), intent(out)::factor

    logical:: Test_List

    Test_List = .false.

    if (omega_Number_array(Position_stage1,Position_stage2) == 1) then
      if (omega_List(Position_stage1,Position_stage2,1) == 0.d0) then
        if (omega < 1.d-9) then
          Test_List = .true.
        endif
      else
        if (abs(omega/omega_List(Position_stage1,Position_stage2,1)-1.d0) < 1.d-9) then
          Test_List = .true.
        endif
      endif
      if (Test_List) then
        Position = 1
        factor = 0.d0
      else
        write(*,*) 'Not enough velocities to perform interpolation...'
        stop
      endif
    else
      ! In case where the velocity is above the largest tabulated velocity, extrapolation instead of
      ! interpolation.
      if (omega <= omega_List(Position_stage1,Position_stage2,omega_Number_array(Position_stage1,Position_stage2))) then
        Position = indice(omega,omega_List(Position_stage1,Position_stage2,:), &
          omega_Number_array(Position_stage1,Position_stage2))
        factor = give_factor(omega,omega_List(Position_stage1,Position_stage2,Position), &
          omega_List(Position_stage1,Position_stage2,Position+1))
      else
        Position = omega_Number_array(Position_stage1,Position_stage2)-1
        factor = give_factor(omega,omega_List(Position_stage1,Position_stage2,Position), &
          omega_List(Position_stage1,Position_stage2,Position+1))
      endif
    endif

    return

  end subroutine omega_Position_and_factor
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine mass_Position_and_factor(mass,Above_Position,Position,factor)
    ! Compute the fraction of the current position in the intervale of interpolation
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: mass_Number_array, mass_List

    implicit none

    integer, intent(in)::Above_Position
    integer, intent(out)::Position

    real(kind=8), intent(in)::mass
    real(kind=8), intent(out)::factor

    if (mass_Number_array(Above_Position) == 1) then
      if (abs(mass/mass_List(Above_Position,1)-1.d0) < 1.d-9) then
        Position = 1
        factor = 0.d0
      else
        write(*,*) 'Not enough mass to perform interpolation...'
        stop
      endif
    else
      ! Due to the binary treatment, we allow the companion mass to be smaller than the minimal mass
      ! of the model grid. In this case : extrapolation.
      if (mass >= mass_List(Above_Position,1)) then
        Position = indice(mass,mass_List(Above_Position,:),mass_Number_array(Above_Position))
        factor = give_factorLog(mass,mass_List(Above_Position,Position),mass_List(Above_Position,Position+1))
      else
        Position = 1
        factor = give_factorLog(mass,mass_List(Above_Position,Position),mass_List(Above_Position,Position+1))
      endif
    endif

    return

  end subroutine mass_Position_and_factor
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Z_Position_and_factor(Z,Position,factor)
    ! While we have only one metallicity, trivial routine
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: Z_Number, Z_List

    implicit none

    integer, intent(out)::Position

    real(kind=8), intent(in)::Z
    real(kind=8), intent(out)::factor

    if (Z_Number == 1) then
      if (abs(Z/Z_List(1)-1.d0) < 1.d-9) then
        Position = 1
        factor = 0.d0
      else
        write(*,*) 'Not enough metallicities to perform interpolation...'
        stop
      endif
    else
      if (Z >= Z_List(1) .and. Z <= Z_List(Z_Number)) then
        Position = indice(Z,Z_List,Z_Number)
        factor = give_factorLog(Z,Z_List(Position),Z_List(Position+1))
      else
        write(*,*) 'Metallicity out of the models metallicities. Aborting...'
        stop
      endif
    endif

    return

  end subroutine Z_Position_and_factor
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine All_Positions_and_factors(Star_Z,Z_Position,Z_factor,Star_mass,mass_Position,mass_factor, &
                                      Star_omega,omega_Position,omega_factor)
    ! Responsible for managing the determination of the model in the tables.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!    use VariousParameters, only: IMF_type,Star_Z,Star_mass,Star_omega,Star_AoV,ivdist,age_log,fixed_metallicity, &
!      om_ivdist,star_number,Comp_Mode,iangle,Z_Number,mass_Number_array,Fixed_AoV
    use VariousParameters, only: Z_Number,mass_Number_array

    implicit none

    integer:: i,Real_Z_Position
    integer, intent(out)::Z_Position
    integer, dimension(2),intent(out)::mass_Position
    integer, dimension(2,2), intent(out)::omega_Position

    real(kind=8), intent(in)::Star_Z,Star_mass,Star_omega
    real(kind=8), intent(out)::Z_factor
    real(kind=8), dimension(2),intent(out)::mass_factor
    real(kind=8), dimension(2,2),intent(out)::omega_factor

    call Z_Position_and_factor(Star_Z,Z_Position,Z_factor)
    if (Z_Number > 1) then
      ! In case where the table contains several Z, find the mass position for the nearest 2.
      if (Z_factor == 1.d0) then
        ! Case where we are at the upper limit of the Z table. As the function ...Position_and_factor return the final
        ! position - 1 in that case, we have to look for the mass at the position Z_Position+1.
        Real_Z_Position = Z_Position+1
        call mass_Position_and_factor(Star_mass,Real_Z_Position,mass_Position(1),mass_factor(1))
      else if (Z_factor == 0.d0) then
        ! In that case, no interpolation in Z will be needed, only one mass position is mandatory.
        Real_Z_Position = Z_Position
        call mass_Position_and_factor(Star_mass,Real_Z_Position,mass_Position(1),mass_factor(1))
      else
        ! Standard case.
        Real_Z_Position = Z_Position
        call mass_Position_and_factor(Star_mass,Real_Z_Position,mass_Position(1),mass_factor(1))
        call mass_Position_and_factor(Star_mass,Real_Z_Position+1,mass_Position(2),mass_factor(2))
      endif
    else
      Real_Z_Position = Z_Position
      call mass_Position_and_factor(Star_mass,Real_Z_Position,mass_Position(1),mass_factor(1))
    endif
    ! Same procedure for determining the position in the omega table.
    do i=1,2
      ! If the mass position is not set, cycle.
      if (mass_Position(i) < 0) then
        cycle
      endif
      if (mass_Number_array(Real_Z_Position-1+i) > 1) then
        ! In case where the table contains several mass, find the omega position for the nearest 2.
        if (mass_factor(i) == 1.d0) then
          ! Case where we are at the upper limit of the mass table. As the function ...Position_and_factor return the final
          ! position - 1 in that case, we have to look for the mass at the position Z_Position+1.
          call omega_Position_and_factor(Star_omega,Real_Z_Position-1+i,mass_Position(i)+1, &
            omega_Position(i,1),omega_factor(i,1))
        else if (mass_factor(i) == 0.d0) then
          ! In that case, no interpolation in omega will be needed, only one mass position is mandatory.
          call omega_Position_and_factor(Star_omega,Real_Z_Position-1+i,mass_Position(i), &
            omega_Position(i,1),omega_factor(i,1))
        else
          ! Standard case.
          call omega_Position_and_factor(Star_omega,Real_Z_Position-1+i,mass_Position(i), &
            omega_Position(i,1),omega_factor(i,1))
          call omega_Position_and_factor(Star_omega,Real_Z_Position-1+i,mass_Position(i)+1, &
            omega_Position(i,2),omega_factor(i,2))
        endif
      else
        call omega_Position_and_factor(Star_omega,Real_Z_Position-1+i,mass_Position(i), &
          omega_Position(i,1),omega_factor(i,1))
      endif
    enddo

  end subroutine All_Positions_and_factors
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Make_InterpolatedModel(Position_z,factor_z,Position_m,factor_m,Position_o,factor_o,New_Structure)
    ! Compute the interpolated model (given mass and omega). The interpolation factor and the position in
    ! the table are passed by arguments.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only:type_DataStructure,positive,less_than_one,Init_DataStructure, &
                            Del_DataStructure,Data_Number,Table_Line_Number
    use VariousParameters, only:All_Data_Array,Z_Number

    implicit none

    integer, intent(in)::Position_z
    integer, dimension(2), intent(in)::Position_m
    integer, dimension(2,2), intent(in):: Position_o
    real(kind=8), intent(in)::factor_z
    real(kind=8), dimension(2), intent(in)::factor_m
    real(kind=8), dimension(2,2), intent(in)::factor_o
    type(type_DataStructure),intent(out)::New_Structure

    type(type_DataStructure)::Structure_Below,Structure_Above

    integer::i

! Initialisation of the intermediate structures used here:
    call Init_DataStructure(Table_Line_Number,Data_Number,New_Structure)
    call Init_DataStructure(Table_Line_Number,Data_Number,Structure_Below)
    call Init_DataStructure(Table_Line_Number,Data_Number,Structure_Above)

    ! The interpolation is done in the order: 1. omega interpolation, 2. mass interpolation (log) and 3. metallicity
    ! interpolation (log). If only one m/z/omega is present, no interpolation is needed (the check of the values
    ! between the reqested one one the existence in the table is done previously).
    if (Z_Number > 1) then
      ! Enough metallicities to perform interpolation.
      if (factor_z == 1.d0) then
        ! If the metallicity is the last one in the metallicity list, no interpolation in metallicity is performed.
        call Interpol_Mass(Position_z+1,Position_m(1),factor_m(1),Position_o(1,:),factor_o(1,:), &
                           New_Structure)
      else if (factor_z == 0.d0) then
        ! If we are close to an existing metallicity, no interpolation is needed.
        call Interpol_Mass(Position_z,Position_m(1),factor_m(1),Position_o(1,:),factor_o(1,:), &
          New_Structure)
      else
        ! Normal case.
        call Interpol_Mass(Position_z,Position_m(1),factor_m(1),Position_o(1,:),factor_o(1,:),Structure_Below)
        call Interpol_Mass(Position_z+1,Position_m(2),factor_m(2),Position_o(2,:),factor_o(2,:),Structure_Above)
        ! The interpolation is done according to the log of the metallicity, we need to convert the initial and current
        ! metallicity into log scale.
        Structure_Below%Metallicity = log10(Structure_Below%Metallicity)
        Structure_Above%Metallicity = log10(Structure_Above%Metallicity)
        call Interpolate_Model(Structure_Below,Structure_Above,factor_z,New_Structure)
        ! Convert back into non log scale.
        New_Structure%Metallicity = 10.d0**New_Structure%Metallicity
      endif
    else
      call Interpol_Mass(Position_z,Position_m(1),factor_m(1),Position_o(1,:),factor_o(1,:),New_Structure)
    endif

    ! Among the interpolated variables, some of them cannot physically overcome 1 or be negative. They are corrected here.
    call check0(New_Structure,size(positive),positive,Table_Line_Number)
    call check1(New_Structure,size(less_than_one),less_than_one,Table_Line_Number)

    ! Delete intermediate strutures:
    call Del_DataStructure(Structure_Below)
    call Del_DataStructure(Structure_Above)

    return

  end subroutine Make_InterpolatedModel
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Interpol_Mass(Z_coord,Mass_Position,Mass_factor,Omega_Position,Omega_factor,Interpolated_Structure)
    ! Perform the interpolation in mass.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only:type_DataStructure,i_mass,i_time,Init_DataStructure,Del_DataStructure,Table_Line_Number,Data_Number
    use VariousParameters, only:All_Data_Array,mass_Number_array

    implicit none

    integer, intent(in)::Z_coord,Mass_Position
    integer, dimension(2), intent(in):: Omega_Position
    real(kind=8), intent(in):: Mass_factor
    real(kind=8), dimension(2), intent(in)::Omega_factor
    type(type_DataStructure),intent(inout)::Interpolated_Structure
    type(type_DataStructure)::Structure_Below,Structure_Above

    !Initialisation of the temporary structures:
    call Init_DataStructure(Table_Line_Number,Data_Number,Structure_Below)
    call Init_DataStructure(Table_Line_Number,Data_Number,Structure_Above)


    if (mass_Number_array(Z_coord) > 1) then
      ! Enough masses to perform interpolation.
      if (Mass_factor == 1.d0) then
        ! If the mass is the last one in the mass list, no interpolation in mass is performed.
        call Interpolate_Model(All_Data_Array(Z_coord,Mass_Position+1,Omega_Position(1)), &
          All_Data_Array(Z_coord,Mass_Position+1,Omega_Position(1)+1),Omega_factor(1), &
          Interpolated_Structure)
      else if (Mass_factor == 0.d0) then
        ! If the mass is close to an existing mass, no interpolation is needed.
        call Interpolate_Model(All_Data_Array(Z_coord,Mass_Position,Omega_Position(1)), &
          All_Data_Array(Z_coord,Mass_Position,Omega_Position(1)+1),Omega_factor(1), &
          Interpolated_Structure)
      else
        ! Normal case.
        call Interpol_Omega(Z_coord,Mass_Position,Omega_Position(1),Omega_factor(1),Structure_Below)
        call Interpol_Omega(Z_coord,Mass_Position+1,Omega_Position(2),Omega_factor(2),Structure_Above)
        ! The interpolation is done according to the log of the mass, we need to convert the initial and current masses
        ! into log scale.
        Structure_Below%mass_ini = log10(Structure_Below%mass_ini)
        Structure_Above%mass_ini = log10(Structure_Above%mass_ini)
        Structure_Below%Data_Table(:,i_mass) = log10(Structure_Below%Data_Table(:,i_mass))
        Structure_Above%Data_Table(:,i_mass) = log10(Structure_Above%Data_Table(:,i_mass))
        Structure_Below%Data_Table(:,i_time) = log10(Structure_Below%Data_Table(:,i_time))
        Structure_Above%Data_Table(:,i_time) = log10(Structure_Above%Data_Table(:,i_time))
        call Interpolate_Model(Structure_Below,Structure_Above,Mass_factor,Interpolated_Structure)
        ! Convert back into non log scale.
        Interpolated_Structure%mass_ini = 10.d0**Interpolated_Structure%mass_ini
        Interpolated_Structure%Data_Table(:,i_mass) = 10.d0**Interpolated_Structure%Data_Table(:,i_mass)
        Interpolated_Structure%Data_Table(:,i_time) = 10.d0**Interpolated_Structure%Data_Table(:,i_time)
      endif
    else
      ! No interpolation performed.
      call Interpol_Omega(Z_coord,Mass_Position,Omega_Position(1),Omega_factor(1),Interpolated_Structure)
    endif

    ! Delete intermediate strutures:
    call Del_DataStructure(Structure_Below)
    call Del_DataStructure(Structure_Above)

    return

  end subroutine Interpol_Mass
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Interpol_Omega(Z_coord,M_coord,Om_Pos,Om_fac,Interpolated_Structure)
    ! Perform the interpolation in omega.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only:type_DataStructure
    use VariousParameters, only:All_Data_Array,Omega_Number_array

    implicit none

    integer, intent(in)::Z_coord,M_coord,Om_Pos
    real(kind=8), intent(in)::Om_fac
    type(type_DataStructure),intent(inout)::Interpolated_Structure

    if (Omega_Number_array(Z_coord,M_coord) > 1) then
      ! Enough velocities to perform interpolation
      call Interpolate_Model(All_Data_Array(Z_coord,M_coord,Om_Pos), &
        All_Data_Array(Z_coord,M_coord,Om_Pos+1),Om_fac,Interpolated_Structure)
    else
      ! No interpolation performed.
      Interpolated_Structure = All_Data_Array(Z_coord,M_coord,Om_Pos)
    endif

    return

  end subroutine Interpol_Omega
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Interpolate_Model(Structure_in_1,Structure_in_2,factor,Structure_out)
    ! Real interpolation routine. Simply apply factor to obtain the interpolated model.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only:type_DataStructure,Table_Line_Number,Data_Number

    implicit none

    real(kind=8), intent(in)::factor
    type(type_DataStructure), intent(in)::Structure_in_1,Structure_in_2
    type(type_DataStructure), intent(inout)::Structure_out

    integer::i

    ! Interpolation of the generic data
    Structure_out%Metallicity = Interpol_factor(Structure_in_1%Metallicity,Structure_in_2%Metallicity,factor)
    Structure_out%mass_ini = Interpol_factor(Structure_in_1%mass_ini,Structure_in_2%mass_ini,factor)
    Structure_out%Omega_Omcrit_ini = Interpol_factor(Structure_in_1%Omega_Omcrit_ini, &
      Structure_in_2%Omega_Omcrit_ini,factor)
    Structure_out%line = Structure_in_1%line

    ! Initialisation of the structure name
    Structure_out%FileName = "Interpolated_model"

    ! Interpolation of the data array
    do i=1,Data_Number
      call Interpol_factor_array(Structure_in_1%Data_Table(:,:),Structure_in_2%Data_Table(:,:),factor, &
        Table_Line_Number,Data_Number,Structure_out%Data_Table(:,:))
    enddo

    return

  end subroutine Interpolate_Model
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Make_TimeModel(Interpolated_Model,agelog,CurrentTime_Line)
    ! Find and interpolate the data at the given age of the cluster.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: i_time,type_DataStructure,type_TimeModel,Table_Line_Number,i_Mdot,i_Mdot_mec, &
                             Data_Number,Init_TimeModel,i_logL
    use VariousParameters, only: table_format

    implicit none

    type(type_DataStructure), intent(in)::Interpolated_Model
    real(kind=8), intent(in)::agelog
    type(type_TimeModel), intent(out)::CurrentTime_Line

    real(kind=8)::Current_Time

    call Init_TimeModel(Data_Number,CurrentTime_Line)

    ! Convert the current time in years.
    Current_Time = 10.d0**agelog

    ! Check that the current time is larger than the ZAMS time. If smaller, the data are set to be the same
    ! than on the ZAMS.
    if (Current_Time <= Interpolated_Model%Data_Table(1,i_time)) then
      call Fill_Data_ZAMS(Interpolated_Model,Current_Time,CurrentTime_Line)
    else if (Current_Time <= Interpolated_Model%Data_Table(Table_Line_Number,i_time)) then
      call Time_Interpolation(Current_Time,Interpolated_Model,CurrentTime_Line)
    else
      write(*,'(a)') 'Current time larger than stellar life time. Should not occur here !'
      write(*,'(a,e15.9)') 'Current time : ',Current_Time
      write(*,'(a,f8.5,a,f10.6,a,f8.5)') 'Z = ',Interpolated_Model%Metallicity,' Mass : ', &
        Interpolated_Model%mass_ini,' velocity : ', &
        Interpolated_Model%Omega_Omcrit_ini
      stop
    endif

    ! Here, the value which were put to -32 during the reading of the initial files are set back to 0.
    if (CurrentTime_Line%Data_Line(i_Mdot) < -25.d0) then
      CurrentTime_Line%Data_Line(i_Mdot) = 0.d0
    endif
    if (table_format == 1) then
      if (CurrentTime_Line%Data_Line(i_Mdot_mec) < -25.d0) then
        CurrentTime_Line%Data_Line(i_Mdot_mec) = 0.d0
      endif
    endif

    return

  end subroutine Make_TimeModel
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Fill_Data_ZAMS(Interpolated_Model,time,CurrentTime_Line)
    ! If the actual time is smaller than the ZAMS time, copy of the ZAMS data in the result array.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure,only:type_DataStructure,type_TimeModel,i_time

    implicit none

    type(type_DataStructure),intent(in)::Interpolated_Model
    real(kind=8),intent(in)::time
    type(type_TimeModel),intent(inout)::CurrentTime_Line

    ! Copy the header of the interpolated model in the current time model header
    CurrentTime_Line%Metallicity = Interpolated_Model%Metallicity
    CurrentTime_Line%mass_ini = Interpolated_Model%mass_ini
    CurrentTime_Line%Omega_Omcrit_ini = Interpolated_Model%Omega_Omcrit_ini
    CurrentTime_Line%Current_Time = time

    ! Copy the ZAMS values of the interpolated model in the current time model
    CurrentTime_Line%Data_Line(:) = Interpolated_Model%Data_Table(1,:)
    ! Modify the value of time
    CurrentTime_Line%Data_Line(i_time) = time

    ! Initialise the additional data to 0.
    CurrentTime_Line%Additional_Data_Line(:) = 0.d0

    return

  end subroutine Fill_Data_ZAMS
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Time_Interpolation(Current_Time,Interpolated_Model,CurrentTime_Line)
    ! Prepare for the time interpolation, searching the lines in the interpolated model file.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_DataStructure,type_TimeModel,i_time,Data_Number,i_logL

    implicit none

    real(kind=8), intent(in):: Current_Time
    type(type_DataStructure), intent(in):: Interpolated_Model
    type(type_TimeModel), intent(inout):: CurrentTime_Line

    integer:: i,Current_time_line_number

    real(kind=8):: factor

    ! Find the lines corresponding to the current time. As the time is not monotonic (certain lines are copyed), it is not
    ! possible to use the indice function.
    i = 0
    Current_time_line_number = 0
    do
      i = i+1
      if (Current_Time <= Interpolated_Model%Data_Table(i,i_time)) then
        Current_time_line_number = i-1
        exit
      endif
    enddo
    factor = give_factor(Current_Time,Interpolated_Model%Data_Table(Current_time_line_number,i_time), &
      Interpolated_Model%Data_Table(Current_time_line_number+1,i_time))

    ! Copy the header of the interpolated model in the current time model header
    CurrentTime_Line%Metallicity = Interpolated_Model%Metallicity
    CurrentTime_Line%mass_ini = Interpolated_Model%mass_ini
    CurrentTime_Line%Omega_Omcrit_ini = Interpolated_Model%Omega_Omcrit_ini
    CurrentTime_Line%Current_Time = Current_Time

    call Interpol_factor_array(Interpolated_Model%Data_Table(Current_time_line_number,:), &
      Interpolated_Model%Data_Table(Current_time_line_number+1,:), factor,1,Data_Number, &
      CurrentTime_Line%Data_Line(:))

    ! Initialise the additional data to 0.
    CurrentTime_Line%Additional_Data_Line(:) = 0.d0

    return

  end subroutine Time_Interpolation
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine check0(A_Structure,n,array_positive,n_lines)
  ! Check that variables that should remain positive behaves well during the interpolation process.
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_DataStructure

    implicit none

    type(type_DataStructure), intent(inout):: A_Structure
    integer, intent(in):: n_lines,n
    integer, dimension(n):: array_positive

    integer::i,j

    do i=1,n_lines
      do j=1,n
        if (A_Structure%Data_Table(i,array_positive(j)) <= 0.d0) then
          A_Structure%Data_Table(i,array_positive(j)) = 0.d0
        endif
      enddo
    enddo

    return

  end subroutine check0
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine check1(A_Structure,n,array_less_than_one,n_lines)
  ! Check that variables that should be smaller than one behaves well during the interpolation process.
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_DataStructure

    implicit none

    type(type_DataStructure), intent(inout):: A_Structure
    integer, intent(in):: n_lines,n
    integer, dimension(n):: array_less_than_one

    integer::i,j

    do i=1,n_lines
      do j=1,n
        if (A_Structure%Data_Table(i,array_less_than_one(j)) >= 1.d0) then
          A_Structure%Data_Table(i,array_less_than_one(j)) = 1.d0
        endif
      enddo
    enddo

    return

  end subroutine check1
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


end module interpolmod
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module ReadData
  ! Reading various data from files
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  use VariousParameters,only: Std_Path

  implicit none

  integer,public,parameter::n_Huang=10000, &                    ! Line number in the Huang file
    n_HG=28, &                          ! Line number in the HG file
    n_Angle_Corr=91, &                  ! Line number in correction files
    n_omega_Corr=101, &                 ! omega number for each AoV data
    n_SurfaceOmega = 201, &             ! Number of lines in Surface_Omega.dat
    n_HeaderSurfaceOmega = 3            ! Header lines in the same file.

  integer,public,dimension(9),parameter::TableCorrection_Shape = (/10,7,6,5,4,3,2,2,1/)
                                                                ! Number of log(g) for each Teff in the table
                                                                ! containing the limb darkening correction.
  integer,public,parameter::n_CorrTable_column = (n_omega_Corr+1)*maxval(TableCorrection_Shape), &
    n_CorrTable_row = n_Angle_Corr*(size(TableCorrection_Shape,1)+1)
                                                                ! Number of rows and columns in the limb
                                                                ! darkening tables.
  integer,public,save:: n_ext,n_angle_ext
  real(kind=8),dimension(:),allocatable,public,save:: omega_ext,dist_ext,angle_ext,angle_dist_ext

  real(kind=8),dimension(n_Huang),public,save:: omega_Huang, &  ! omega read in Huang file
    dist_Huang_1, & ! small mass distribution
    dist_Huang_2, & ! medium mass distribution
    dist_Huang_3    ! large mass distribution
  real(kind=8),dimension(2),public,parameter::Huang_m_limit = (/4.d0,8.d0/) ! Limit mass between the 3 Huang et
                                                                ! al. 2010 distributions for B stars.
  real(kind=8),public,parameter::B_mass_inf = 1.7d0
  real(kind=8),public,parameter::B_mass_sup = 18.d0             ! min and max mass for a B star.
  real(kind=8),dimension(n_HG),public,save:: omega_HG, &        ! omega read in HG file
    dist_HG            ! HG distribution
  real(kind=8),dimension(n_Angle_Corr),public,save::AoV_Corr    ! Angle of view of the data file
  real(kind=8),dimension(n_Angle_Corr,n_omega_Corr),public,save::L_Corr, &    ! Luminosity correction
                                                                 Teff_Corr, & ! Teff correction
                                                                 Grav_Corr    ! Gravity correction

  real(kind=8),dimension(n_CorrTable_row,n_CorrTable_column),public,save:: &
    Correct_Lum, & ! grid for Luminosity correction (with LD)
    Correct_Teff   ! grid for Teff correction (with LD)
  real(kind=8),dimension(n_omega_Corr),public,save::omega_list_data  ! list of omega/omega_crit for the corrections.
  real(kind=8),dimension(n_SurfaceOmega),public,save:: omega_surface, & ! omega/omega_crit read in OmegaSurface.dat
    Surface          ! normalised surface corresponding to omega.


  public:: init_Huang
  public:: init_HG
  public:: init_external
  public:: init_Correction
  public:: init_VcritOmega
  public:: init_SurfaceOmega
  public:: init_Correct_fact

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine init_Huang
    ! Reads the Huang ditribution data file.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: HuangFile

    implicit none

    integer::i,error

    open(unit=10,file=trim(Std_Path)//HuangFile,iostat=error,status='old')
    if (error /= 0) then
      write(*,*) 'Problem reading file ',trim(HuangFile), '!'
    endif
    do i=1,n_Huang
      read(10,'(2x,e13.7,30x,3(2x,e13.7))',iostat=error)omega_Huang(i),dist_Huang_1(i),dist_Huang_2(i), &
        dist_Huang_3(i)
      if (error /= 0) then
        exit
      endif
    enddo

    close(10)

    return

  end subroutine init_Huang
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine init_HG
    ! Reads the Huang and Giess ditribution data file.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only:HGFile

    implicit none

    integer::i,error

    open(unit=11,file=trim(Std_Path)//HGFile,iostat=error,status='old')
    if (error /= 0) then
      write(*,*) 'Problem reading file ',trim(HGFile), '!'
    endif
    do i=1,n_HG
      read(11,'(f8.6,1x,f8.6)',iostat=error)omega_HG(i),dist_HG(i)
      if (error /= 0) then
        exit
      endif
    enddo

    close(11)

    return

  end subroutine init_HG
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine init_external
  ! Reads an external distribution data file.
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer:: i,error=0
    integer,dimension(2):: pos_dot=(0,0),ind_break=(0,0),first=(0,0),second=(0,0)
    character(512):: ext_file=''
    character(64):: line
    character(32):: file_format

    write(*,*) 'Enter the name of the external file for OOc distribution'
    write(*,*) 'with path: '
    read(5,*) ext_file
    write(*,*) 'reading from file',trim(ext_file)

    open(unit=11,file=trim(ext_file),iostat=error,status='old')
    if (error /= 0) then
      write(*,*) 'Problem reading file ',trim(ext_file), '!'
    endif
    error = 0
    n_ext = 0
    do
      read(11,'(a)',iostat=error) line
      if (error /= 0) then
        exit
      else
        if (n_ext == 0) then ! first line: definition of the format of the file to read it later
          pos_dot(1) = index(line,'.')
          ind_break(1) = pos_dot(1) + index(line(pos_dot(1)+1:),' ')
          first(1) = ind_break(1) - 1
          first(2) = first(1) - pos_dot(1)
          pos_dot(2) = pos_dot(1) + (index(line(pos_dot(1)+1:),'.'))
          ind_break(2) = pos_dot(2) + index(line(pos_dot(2)+1:),' ')
          second(1) = ind_break(2) - ind_break(1)
          second(2) = ind_break(2) - pos_dot(2) - 1
          write(file_format,'("(f",i0,".",i0,",f",i0,".",i0,")")') first(1),first(2),second(1),second(2)
        endif
      endif
      n_ext = n_ext+1
    enddo
    rewind(11)

    allocate(omega_ext(n_ext))
    allocate(dist_ext(n_ext))

    error = 0
    do i=1,n_ext
      read(11,file_format,iostat=error)omega_ext(i),dist_ext(i)
      if (error /= 0) then
        exit
      endif
    enddo

    close(11)

    return

  end subroutine init_external
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine init_angle_external
  ! Reads an external distribution data file.
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer:: i,error
    integer,dimension(2):: pos_dot=(0,0),ind_break=(0,0),first=(0,0),second=(0,0)
    character(512):: ext_file
    character(64):: line
    character(32):: file_format

    write(*,*) 'Enter the name of the external file for angle distribution'
    write(*,*) 'with path: '
    read(5,*) ext_file

    open(unit=11,file=trim(ext_file),iostat=error,status='old')
    if (error /= 0) then
      write(*,*) 'Problem reading file ',trim(ext_file), '!'
    endif
    error = 0
    n_angle_ext = 0
    do
      read(11,'(a)',iostat=error) line
      if (error /= 0) then
        exit
      else
        if (n_angle_ext == 0) then
          pos_dot(1) = index(line,'.')
          ind_break(1) = pos_dot(1) + index(line(pos_dot(1)+1:),' ')
          first(1) = ind_break(1) - 1
          first(2) = first(1) - pos_dot(1)
          pos_dot(2) = pos_dot(1) + (index(line(pos_dot(1)+1:),'.'))
          ind_break(2) = pos_dot(2) + index(line(pos_dot(2)+1:),' ')
          second(1) = ind_break(2) - ind_break(1)
          second(2) = ind_break(2) - pos_dot(2) - 1
          write(file_format,'("(f",i0,".",i0,",f",i0,".",i0,")")') first(1),first(2),second(1),second(2)
        endif
      endif
      n_angle_ext = n_angle_ext+1
    enddo
    rewind(11)

    allocate(angle_ext(n_angle_ext))
    allocate(angle_dist_ext(n_angle_ext))

    error = 0
    do i=1,n_angle_ext
      read(11,file_format,iostat=error)angle_ext(i),angle_dist_ext(i)
      if (error /= 0) then
        exit
      endif
    enddo

    close(11)

    return

  end subroutine init_angle_external
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine init_Correction
    ! Reads the correction files for luminosity and effective temperature (gravity darkening).
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only:grav_dark,LumFileVZ,TeffFileVZ,LumFileELR,TeffFileELR, &
                                GravFileVZ,GravFileELR

    implicit none

    character(256):: LumFile,TeffFile,GravFile

    integer::i,j,error

    select case (grav_dark)
      case (1)
        LumFile = LumFileVZ
        TeffFile = TeffFileVZ
        GravFile = GravFileVZ
      case (2)
        LumFile = LumFileELR
        TeffFile = TeffFileELR
        GravFile = GravFileELR
      case default
        write(*,*) 'Unexpected value for grav_dark in init_Correction, aborting...'
        stop
    end select

    open(12,file=trim(Std_Path)//LumFile,iostat=error,status='old')
    if (error /= 0) then
      write(*,*) 'Problem reading file ',trim(LumFile), '!'
    endif
    do i=1,n_Angle_Corr
      read(12,'(101(e14.7,2x),e14.7)',iostat=error)AoV_Corr(i),(L_Corr(i,j),j=1,n_omega_Corr)
      if (error /= 0) then
        exit
      endif
    enddo

    close(12)

    open(12,file=trim(Std_Path)//TeffFile,iostat=error,status='old')
    if (error /= 0) then
      write(*,*) 'Problem reading file ',trim(TeffFile), '!'
    endif
    do i=1,n_Angle_Corr
      read(12,'(101(e14.7,2x),e14.7)',iostat=error)AoV_Corr(i),(Teff_Corr(i,j),j=1,n_omega_Corr)
      if (error /= 0) then
        exit
      endif
    enddo

    close(12)

    open(12,file=trim(Std_Path)//GravFile,iostat=error,status='old')
    if (error /= 0) then
      write(*,*) 'Problem reading file ',trim(GravFile), '!'
    endif
    do i=1,n_Angle_Corr
      read(12,'(101(e14.7,2x),e14.7)',iostat=error)AoV_Corr(i),(Grav_Corr(i,j),j=1,n_omega_Corr)
      if (error /= 0) then
        exit
      endif
    enddo

    close(12)

    return

  end subroutine init_Correction
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine init_Correct_fact
    !
    !      This subroutine reads the Files containing the Luminosity and Effective temperature correction factors
    !      that have to be applied to the computed L and Teff, due to the gravity and limb darkening. Because these quantities depend on the stellar parameters (logg and teff),
    !      we read the correction factor for each inclination angle and rotational velocity, for different combinations of logg and teff. We use them to interpolate the correction_subroutine
    !      factors for other values of logg and teff.
    !
    !      teff[kK]-logg
    !
    !       5 - 4.5,4.0,3.5,3.0,2.5,2.0,1.5,1.0,0.5,0.0
    !      10 - 4.5,4.0,3.5,3.0,2.5,2.0,1.5
    !      15 - 4.5,4.0,3.5,3.0,2.5,2.0
    !      20 - 4.5,4.0,3.5,3.0,2.5
    !      25 - 4.5,4.0,3.5,3.0
    !      30 - 4.5,4.0,3.5
    !      35 - 4.5,4.0
    !      40 - 4.5,4.0
    !      45 - 4.5
    !
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: grav_dark,LumGridVZ,TeffGridVZ,LumGridELR,TeffGridELR

    implicit none

    integer,parameter:: File_L_Unit = 12, File_Teff_Unit = 13
    integer:: i,j,jmax,error
    character(256):: LumGrid,TeffGrid

    select case (grav_dark)
      case (1)
        LumGrid = LumGridVZ
        TeffGrid = TeffGridVZ
      case (2)
        LumGrid = LumGridELR
        TeffGrid = TeffGridELR
      case default
        write(*,*) 'Unexpected value for grav_dark in init_Correction, aborting...'
        stop
    end select


    ! Open the 2 files containing the data
    open(unit=File_L_Unit,file=trim(Std_Path)//LumGrid,iostat=error)
    open(unit=File_Teff_Unit,file=trim(Std_Path)//TeffGrid,iostat=error)

    ! Initialisation of the data
    Correct_Lum(:,:) = 0.d0
    Correct_Teff(:,:) = 0.d0

    do i=1,910

      if (i.le.91) then
        jmax=1020
      elseif (i.gt.91.and.i.le.2*91) then
        jmax=1020-(3*102)
      elseif (i.gt.2*91.and.i.le.3*91) then
        jmax=1020-(4*102)
      elseif (i.gt.3*91.and.i.le.4*91) then
        jmax=1020-(5*102)
      elseif (i.gt.4*91.and.i.le.5*91) then
        jmax=1020-(6*102)
      elseif (i.gt.5*91.and.i.le.6*91) then
        jmax=1020-(7*102)
      elseif (i.gt.6*91.and.i.le.7*91) then
        jmax=1020-(8*102)
      elseif (i.gt.7*91.and.i.le.8*91) then
        jmax=1020-(8*102)
      elseif (i.gt.8*91.and.i.le.9*91) then
        jmax=1020-(9*102)
      elseif (i.gt.9*91.and.i.le.10*91) then
        jmax=1020-(9*102)
      endif
      read(12,*,iostat=error)(Correct_Lum(i,j),j=1,jmax)
      read(13,*,iostat=error)(Correct_Teff(i,j),j=1,jmax)
    enddo

    ! Close the opened file.
    close(File_L_Unit)
    close(File_Teff_Unit)

    return

  end subroutine init_Correct_fact
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine init_VcritOmega
    ! Reads the file containing the translation between v/v_crit and omega/omega_crit
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only:VCrit_to_Omega_File

    implicit none

    integer::i,error

    open(unit=13,file=trim(Std_Path)//VCrit_to_Omega_File,iostat=error,status='old')
    if (error /= 0) then
      write(*,*) 'Problem reading file ',trim(VCrit_to_Omega_File), '!'
    endif
    do i=1,n_omega_Corr
      read(13,'(e14.7)',iostat=error) omega_list_data(i)
      if (error /= 0) then
        exit
      endif
    enddo

    close(13)

    return

  end subroutine init_VcritOmega
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine init_SurfaceOmega
  ! Reads the file containing the translation between v/v_crit and omega/omega_crit
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only:OmegaSurface_File

    implicit none

    integer::i,error

    open(unit=14,file=trim(Std_Path)//OmegaSurface_File,iostat=error,status='old')
    if (error /= 0) then
      write(*,*) 'Problem reading file ',trim(OmegaSurface_File), '!'
    endif

    ! Read the header
    do i=1,n_HeaderSurfaceOmega
      read(14,*)
    enddo
    do i=1,n_SurfaceOmega
      read(14,'(e14.7,2x,e14.7)',iostat=error) omega_surface(i),Surface(i)
      if (error /= 0) then
        exit
      endif
    enddo

    close(14)

    return

  end subroutine init_SurfaceOmega
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module ReadData
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module Color_and_Correction
  ! This module contains the ex-routine "mash.f" related to the paper "An Empirical UBV RI JHK
  ! Color-Temperature Calibration for Stars", Worthey & Lee, ApJS 193 1 (2011), rewritten in a f95 style.
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  implicit none

  integer, parameter::nind = 9, nvk = 75, nfe = 7, ng = 13
  !     The 8th nind is the temperature.
  !     The 9th nind is BC_v
  integer,public:: i_L,i_Teff

  real(kind=8), parameter:: theta_init_value = 5040.d0
  real(kind=8), dimension(ng), parameter:: g = (/-0.5d0,0.0d0,0.5d0,1.0d0,1.5d0,2.0d0,2.5d0,3.0d0,3.5d0,4.0d0,4.5d0, &
    5.0d0,5.5d0/)
  real(kind=8), dimension(nfe), parameter:: fe = (/-2.5d0,-2.0d0,-1.5d0,-1.0d0,-0.5d0,0.0d0,0.5d0/)

  logical, save:: Already_Read = .false.

  public:: Compute_ColorMagnitude_New
  private:: Empirical_CT
  private:: readtable
  private:: teffinterp
  private:: linear
  private:: locate
  private:: polint

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Compute_ColorMagnitude_New(Model)
  ! Interface routine, compute the data needed.
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_TimeModel,i_polar_gravity,i_MBol,i_MV,i_UB,i_BV,i_B2V1,i_VR,i_VI, &
      i_JK,i_HK,i_VK,i_BC,i_GV,i_GbpV,i_GrpV,i_Gflag,i_UBT,i_BVT,i_VRT,i_VIT,i_JKT,i_HKT,i_VKT,i_BCT, &
      i_MBolT,i_M_VT,i_GVT,i_GbpVT,i_GrpVT,i_MUT,i_MBT,i_M_RT,i_M_IT,i_M_HT,i_M_JT,i_M_KT,i_M_GT, &
      i_M_GbpT,i_M_GrpT
    use Constant, only: Z_sun
    use VariousParameters, only: table_format

    implicit none

    type(type_TimeModel), intent(inout)::Model

    real(kind=8):: log_g,Fe_H,Teff
    real(kind=8), dimension(nind):: Colours

    ! The routine Compute_ColorMagnitude_New needs log(g), Fe/H and Teff.
    log_g = log10(Model%Additional_Data_Line(i_polar_gravity))
    Fe_H = log10(Model%Metallicity/Z_sun)
    Teff = 10.d0**Model%Additional_Data_Line(i_Teff)

    ! Call the routine computing the colours and bolometric correction:
    call Empirical_CT(log_g,Fe_H,Teff,Colours)

    ! Fill the array:
    Model%Additional_Data_Line(i_UB) = Colours(1)
    Model%Additional_Data_Line(i_BV) = Colours(2)
    Model%Additional_Data_Line(i_VR) = Colours(3)
    Model%Additional_Data_Line(i_VI) = Colours(4)
    Model%Additional_Data_Line(i_JK) = Colours(5)
    Model%Additional_Data_Line(i_HK) = Colours(6)
    Model%Additional_Data_Line(i_VK) = Colours(7)
    Model%Additional_Data_Line(i_BC) = Colours(9)

    Model%Additional_Data_Line(i_Mbol) = -2.5d0*Model%Additional_Data_Line(i_L) + 4.75d0
    Model%Additional_Data_Line(i_MV) = -2.5d0*Model%Additional_Data_Line(i_L)+4.75d0-Model%Additional_Data_Line(i_BC)

    ! Computation of the Gaia colours according to the DR2 (Evans et al. 2018, arXiv 1804.09368). In case
    ! the data are off the recommended values for V-I, we set the flag to 1:
    Model%Additional_Data_Line(i_GV)   = -0.01746d0 + 0.008092d0*Model%Additional_Data_Line(i_VI) &
                                                    - 0.281000d0*Model%Additional_Data_Line(i_VI)**2.d0 &
                                                    + 0.036550d0*Model%Additional_Data_Line(i_VI)**3.d0
    Model%Additional_Data_Line(i_GbpV) = -0.05204d0 + 0.483000d0*Model%Additional_Data_Line(i_VI) &
                                                    - 0.200100d0*Model%Additional_Data_Line(i_VI)**2.d0
                                                    + 0.02186*Model%Additional_Data_Line(i_VI)**3.d0
    Model%Additional_Data_Line(i_GrpV) =  0.00024280d0 - 0.867500d0*Model%Additional_Data_Line(i_VI) &
                                                    - 0.028660d0*Model%Additional_Data_Line(i_VI)**2.d0
    if (Model%Additional_Data_Line(i_VI) >= -0.3d0 .and. Model%Additional_Data_Line(i_VI) <= 2.7d0) then
        Model%Additional_Data_Line(i_Gflag) = 0.d0
    else
        Model%Additional_Data_Line(i_Gflag) = 1.d0
    endif

    ! When using starevol format, the colours are interpolated as the other quantities (they are already in
    ! the tables). However, we prefer here to recompute them to be coherent.
    if (table_format == 2) then
      Model%Data_Line(i_UBT) = Model%Additional_Data_Line(i_UB)
      Model%Data_Line(i_BVT) = Model%Additional_Data_Line(i_BV)
      Model%Data_Line(i_VRT) = Model%Additional_Data_Line(i_VR)
      Model%Data_Line(i_VIT) = Model%Additional_Data_Line(i_VI)
      Model%Data_Line(i_JKT) = Model%Additional_Data_Line(i_JK)
      Model%Data_Line(i_HKT) = Model%Additional_Data_Line(i_HK)
      Model%Data_Line(i_VKT) = Model%Additional_Data_Line(i_VK)
      Model%Data_Line(i_BCT) = Model%Additional_Data_Line(i_BC)
      Model%Data_Line(i_MBolT) = Model%Additional_Data_Line(i_Mbol)
      Model%Data_Line(i_M_VT) = Model%Additional_Data_Line(i_MV)
      Model%Data_Line(i_GVT) = Model%Additional_Data_Line(i_GV)
      Model%Data_Line(i_GbpVT) = Model%Additional_Data_Line(i_GbpV)
      Model%Data_Line(i_GrpVT) = Model%Additional_Data_Line(i_GrpV)
      Model%Data_Line(i_MBT) = Model%Data_Line(i_BVT) + Model%Data_Line(i_M_VT)
      Model%Data_Line(i_MUT) = Model%Data_Line(i_UBT) + Model%Data_Line(i_MBT)
      Model%Data_Line(i_M_RT) = -Model%Data_Line(i_VRT) + Model%Data_Line(i_M_VT)
      Model%Data_Line(i_M_IT) = -Model%Data_Line(i_VIT) + Model%Data_Line(i_M_VT)
      Model%Data_Line(i_M_KT) = -Model%Data_Line(i_VKT) + Model%Data_Line(i_M_VT)
      Model%Data_Line(i_M_HT) = Model%Data_Line(i_HKT) + Model%Data_Line(i_M_KT)
      Model%Data_Line(i_M_JT) = Model%Data_Line(i_JKT) + Model%Data_Line(i_M_KT)
      Model%Data_Line(i_M_GT) = Model%Data_Line(i_GVT) + Model%Data_Line(i_M_VT)
      Model%Data_Line(i_M_GbpT) = Model%Data_Line(i_GbpVT) + Model%Data_Line(i_M_VT)
      Model%Data_Line(i_M_GrpT) = Model%Data_Line(i_GrpVT) + Model%Data_Line(i_M_VT)
    endif

    return

  end subroutine Compute_ColorMagnitude_New
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Empirical_CT(grav,feh,teff,clrs)
    ! Directly adapted from mash.f
    ! Inquires of the user stellar parameters and returns colors, with errors
    ! Inputs are: grav      : log(g)
    !             feh       : [Fe/H]
    !             teff      : T_eff
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    use DataStructure, only: verbose

    implicit none

    ! out-of-bounds flag. Nominal is zero.
    integer:: iflag

    real(kind=8), intent(in):: grav,feh,teff

    real(kind=8), dimension(nind), intent(out):: clrs

    integer:: i,iok

    real(kind=8):: theta,xbcerr,xerr
    real(kind=8), dimension(nind):: cerr

    ! Color (or BC) intrinsic precisions
    real(kind=8), dimension(7), parameter:: cpres = (/0.071d0,0.017d0,0.010d0,0.010d0,0.011d0,0.004d0,0.002d0/)

    ! theta and assumed percentage error arrays:
    real(kind=8), dimension(8), parameter:: ete = (/0.1008d0,0.252d0,0.504d0,0.84d0,1.26d0,1.44d0,1.68d0,2.52d9/), &
      !                                       Teff= 50000, 20000,10000,6000,4000,3500,3000,2000
      eterr = (/4.0d0,2.5d0,1.0d0,0.5d0,0.5d0,1.0d0,1.5d0,4.0d0/), &
      !                                       The cool errors probably ought to be a bit bigger for dwarfs,
      !                                       but that's not implemented...
      bcefloor = (/0.2d0,0.1d0,0.07d0,0.05d0,0.05d0,0.07d0,0.10d0,0.2d0/)
    !                                       Make bolometric correction floor error depend on theta

    real(kind=8), dimension(nvk,nfe,ng,nind), save:: a

    ! Set extrapolation flag to nominal value
    iflag = 0

    ! load data table
    if (.not. Already_Read) then
      call readtable(a)
      Already_Read = .true.
    endif

    ! switch Teff to THETA
    theta = theta_init_value/teff

    ! get colors for these parameters
    call teffinterp(a,grav,feh,theta,clrs,iflag)

    ! iflag warns of extrapolations.
    ! Nominally zero, it becomes -N if too hot or N if too cool,
    ! where N is the number of point spacings extrapolated.
    ! Obviously, abs(iflag) > 1 is of concern . . .
    if ( iflag /= 0 .and. verbose) then
      write(*,*) 'Extrapolation warning! Iflag =',iflag
    endif
    ! and colors for 1% lower temperature to compute errors
    call teffinterp(a,grav,feh,1.01d0*theta,cerr,iflag)
    call linear(ete,eterr,8,theta,xerr,iOK)

    if (iOK <= -2) then
      write(*,*) 'Panic! Interpolation error!'
      stop
    endif

    do i=1,7
      cerr(i) = sqrt(cpres(i)**2 + (xerr**2)*(cerr(i)-clrs(i))**2)
    end do

    ! BCv error
    call linear(ete,bcefloor,8,theta,xbcerr,iOK)
    cerr(9) = sqrt(xbcerr**2 + (xerr**2)*(cerr(9)-clrs(9))**2)

    return

  end subroutine Empirical_CT
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine readtable(a)
    ! Read the table during the first pass.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: Std_Path,DataColor

    implicit none

    integer, parameter:: ReadUnit = 60
    integer:: i,error,ife,ig,ivk

    real(kind=8), dimension(nvk,nfe,ng,nind), intent(out):: a

    real(kind=8):: x1,x2,xteff

    open(unit=ReadUnit,file=trim(Std_Path)//DataColor,status='old',iostat = error)
    if (error /= 0) then
      write(*,*) 'Problem opening file ',trim(DataColor),'...'
      stop
    endif

    do ife = 1,nfe
      do ig = 1,ng
        do ivk=1,nvk
          read(ReadUnit,'(1x,f4.1,1x,f4.1,1x,f6.0,5x,8(2x,f6.3))') x1,x2,xteff,(a(ivk,ife,ig,i),i=1,7),a(ivk,ife,ig,9)
          if (abs(x1-fe(ife)) .gt. 0.01 ) then
            write(*,*) 'bad fe in readtable.'
          endif
          if (abs(x2-g(ig)) .gt. 0.01 ) then
            write(*,*) 'bad g in readtable.'
          endif
          ! transform to theta rather than Teff . . .
          a(ivk,ife,ig,8) = theta_init_value/xteff
        end do
      end do
    end do

    return

  end subroutine readtable
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine teffinterp(a,grav,feh,theta,clrs,iflag)
    ! Performing the interpolation through the values read in the table.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8), intent(in)::grav,feh,theta
    real(kind=8), dimension(nvk,nfe,ng,nind), intent(in):: a

    integer, intent(out):: iflag

    real(kind=8), dimension(nind), intent(out)::clrs

    integer, parameter:: NpointInterp = 5
    integer:: jg,jfe,ivk,ind,i,jt,jt0

    real(kind=8):: dy,ffe,gg,frac
    real(kind=8),dimension(nvk,nind):: c

    ! clrs are  1 U-B  2 B-V  3 V-R  4 V-I  5 J-K  6 H-K  7 V-K  (8 Teff)
    ! a() is organised: a(nvk,nfe,ng,nind)                       (9 BCv)

    ! find jfe and jg interpolation corners
    call locate(g,ng,grav,jg)

    if (jg == 0) then
      jg = 1
    endif
    if (jg == ng) then
      jg = ng-1
    endif

    call locate(fe,nfe,feh,jfe)
    if (jfe == 0) then
      jfe=1
    endif
    if (jfe == nfe) then
      jfe=nfe-1
    endif

    ! fill c array with bilinear-interp results
    ffe = ( feh - fe(jfe) )/( fe(jfe+1)-fe(jfe)  )
    gg  = ( grav - g(jg) )/( g(jg+1) - g(jg)  )
    do ivk=1,nvk
      do ind=1,nind
        c(ivk,ind) = (1.d0-ffe)*(1.d0-gg)*a(ivk,jfe,jg,ind)+  ffe*(1.d0-gg)*a(ivk,jfe+1,jg,ind) &
          +  ffe*gg*a(ivk,jfe+1,jg+1,ind) + (1.d0-ffe)*gg*a(ivk,jfe,jg+1,ind)
      end do
    end do

    ! find temperature (it's really theta) and interpolate colors
    call locate(c(1,8),nvk,theta,jt)
    jt0 = jt
    if (jt == 0) then
      jt = 1
    endif
    if (jt > nvk - NpointInterp) then
      jt = nvk - NpointInterp + 1
    endif

    do i=1,7
      call polint(c(jt,8),c(jt,i),NpointInterp,theta,clrs(i),dy)
    end do
    call polint(c(jt,8),c(jt,9),NpointInterp,theta,clrs(9),dy)

    ! if requested temperature is out-of-bounds, use linear interpolation to
    ! extrapolate. Return iflag = int(number of segments beyond the tabulated)
    if ( jt0 == 0 ) then
      frac = (theta - c(1,8))/(c(2,8)-c(1,8))
      iflag = -1 + int(frac)
      do i=1,7
        clrs(i) = (1.d0-frac)*c(1,i) + frac*c(2,i)
      end do
      clrs(9) = (1.d0-frac)*c(1,9) + frac*c(2,9)
    end if
    if ( jt0 .eq. nvk ) then
      frac = (theta - c(nvk-1,8))/(c(nvk,8)-c(nvk-1,8))
      iflag = 1 + int(frac)
      do i=1,7
        clrs(i) = (1.d0-frac)*c(nvk-1,i) + frac*c(nvk,i)
      end do
      clrs(9) = (1.d0-frac)*c(nvk-1,9) + frac*c(nvk,9)
    end if

    return

  end subroutine teffinterp
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine linear(x,y,nxy,xin,yout,iOK)
    !     quick linear interpolation
    !     x and y are nxy long
    !     xin is the nontabulated input x value
    !     yout is the interpolated y guess.
    !     iOK is -3 if X is not in ascending order, -2 if input x is
    !     out-of-bounds by more than 1 xpoint spacing, -1 if out-of-bounds
    !     by less than 1 xpoint spacing, 0 if all is OK
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer, intent(in):: nxy

    real(kind=8), intent(in):: xin
    real(kind=8), dimension(nxy), intent(in):: x,y

    integer, intent(out):: iOK
    real(kind=8), intent(out):: yout

    integer:: j,jl,ju,jm
    real(kind=8):: frac

    iOK = 0
    if ( x(2) < x(1) ) then
      write(*,*) 'Error. Sub LINEAR. Input X array must be in ascending order.'
      yout = 0.0
      iOK = -3
      return
    endif

    ! locate correct array element by bisection
    jl=0
    ju=nxy+1


    do while (ju-jl > 1)
      jm = (ju+jl)/2
      if ((x(nxy) > x(1)) .eqv. (xin > x(jm))) then
        jl=jm
      else
        ju=jm
      endif
    enddo

    j = jl
    ! j is 0 or nxy if xin is off the grid

    ! if off-grid, reset j and set output flag iOK
    if ( j == 0) then
      if ( xin < (x(1)-(x(2)-x(1))) ) then
        iOK = -2
      else
        iOK = -1
      endif
      j=1
    endif
    if ( j == nxy) then
      if ( xin > (x(nxy)+(x(nxy)-x(nxy-1))) ) then
        iOK = -2
      else
        iOK = -1
      end if
      j = nxy-1
    end if

    ! now interpolate/extrapolate
    frac = (xin - x(j))/(x(j+1)-x(j))
    yout = (1.d0-frac)*y(j) + frac*y(j+1)

    return

  end subroutine linear
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine locate(xx,n,x,j)
    !     -----NUMERICAL RECIPES routines: locate and polint
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer, intent(in):: n

    real(kind=8), intent(in):: x
    real(kind=8), dimension(n), intent(in):: xx

    integer, intent(out):: j

    integer:: jl,ju,jm

    jl = 0
    ju = n+1

    do while (ju-jl > 1)
      jm = (ju+jl)/2
      if ((xx(n) > xx(1)) .eqv. (x > xx(jm))) then
        jl = jm
      else
        ju = jm
      endif
    enddo

    j = jl

    return

  end subroutine locate
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine polint(xa,ya,n,x,y,dy)
    ! From numerical recipes
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer, intent(in):: n

    real(kind=8), intent(in):: x
    real(kind=8), dimension(n), intent(in):: xa,ya

    real(kind=8), intent(out):: y, dy

    integer, parameter:: nmax = 10
    integer:: ns,i,m

    real(kind=8):: dif,dift,ho,hp,w,den
    real(kind=8), dimension(nmax):: c, d

    ns = 1
    dif = abs(x-xa(1))

    do i=1,n
      dift = abs(x-xa(i))
      if (dift < dif) then
        ns = i
        dif = dift
      endif
      c(i) = ya(i)
      d(i) = ya(i)
    enddo

    y = ya(ns)
    ns = ns-1

    do m=1,n-1
      do i=1,n-m
        ho = xa(i) - x
        hp = xa(i+m) - x
        w = c(i+1) - d(i)
        den = ho - hp
        if (den == 0.d0) then
          write(*,*) 'Problem in Polint...'
          stop
        endif
        den = w/den
        d(i) = hp*den
        c(i) = ho*den
      enddo
      if (2*ns < n-m) then
        dy = c(ns+1)
      else
        dy = d(ns)
        ns = ns-1
      endif
      y=y+dy
    enddo

    return

  end subroutine polint
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module Color_and_Correction
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module Additional_Data
  ! Module containing the routines computing the additional data (colours, magnitudes, ...)
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  use Color_and_Correction,only: i_L,i_Teff

  implicit none

  public:: Compute_Additional
  public:: Compute_Additional_Single
  private:: Compute_PolarRadius
  private:: Compute_PolarGravity
  private:: Compute_ColorMagnitude
  private:: Compute_ColorMagnitude_Old
  private:: Correction_Class_V
  private:: Correction_Class_III
  private:: Correction_Class_I
  private:: Perform_Average
  private:: correct_fact
  private:: Correct_AngleofView

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Compute_Additional(Model)
    ! Computation of the additional quantities (g_P,and color-magnitude).
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: iangle,limb_dark
    use DataStructure, only: type_TimeModel,Additional_Data_Number,i_logTeff_corr,i_logL, &
                             i_logL_gd,i_logL_lgd,i_logTeff_gd,i_logTeff_lgd,i_mean_gravity,&
                             i_polar_gravity
    implicit none

    type(type_TimeModel), intent(inout):: Model

    integer:: i

    if (limb_dark > 0) then
      i_L = i_logL_lgd
      i_Teff = i_logTeff_lgd
    else
      i_L = i_logL_gd
      i_Teff = i_logTeff_gd
    endif

    ! Initialise all the additional data to 0 in case some of them are not computed.
    do i=1,Additional_Data_Number
      Model%Additional_Data_Line(i) = 0.d0
    enddo
    Model%Additional_Data_Line(i_logL_gd) = Model%Data_Line(i_logL)
    Model%Additional_Data_Line(i_logL_lgd) = Model%Data_Line(i_logL)
    Model%Additional_Data_Line(i_logTeff_gd) = Model%Data_Line(i_logTeff_corr)
    Model%Additional_Data_Line(i_logTeff_lgd) = Model%Data_Line(i_logTeff_corr)

    call Compute_PolarRadius(Model)
    call Compute_PolarGravity(Model)

    Model%Additional_Data_Line(i_mean_gravity) = Model%Additional_Data_Line(i_polar_gravity)

    ! If wanted, correction of the Teff and luminosity due to the angle of view and actual stellar velocity.
    if (iangle > 0) then
      call Correct_AngleofView(Model)
      call correct_fact(Model)
    endif
    call Compute_ColorMagnitude(Model)

    return

  end subroutine Compute_Additional
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Compute_Additional_Single(Modele_Interpole,Angle,Modeles_Line,Dimen)
    ! Computation of the additional quantities (g_P,and color-magnitude).
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only:type_DataStructure,type_TimeModel,i_time,Data_Number,Init_TimeModel

    implicit none

    integer, intent(in):: Dimen
    real(kind=8), intent(in):: Angle
    type(type_DataStructure), intent(inout):: Modele_Interpole

    type(type_TimeModel),dimension(Dimen), intent(out):: Modeles_Line

    type(type_TimeModel):: Current_Line

    integer:: i

    ! Initialisation of Current_Line
    call Init_TimeModel(Data_Number,Current_Line)

    do i=1,Dimen
      Current_Line%star_ID = i
      Current_Line%Is_a_Binary = 0
      Current_Line%Metallicity = Modele_Interpole%Metallicity
      Current_Line%mass_ini = Modele_Interpole%mass_ini
      Current_Line%Omega_Omcrit_ini = Modele_Interpole%Omega_Omcrit_ini
      Current_Line%Current_Time = Modele_Interpole%Data_Table(i,i_time)
      Current_Line%mass_ratio = 0.d0
      Current_Line%Angle_of_View = Angle
      Current_Line%Data_Line(:) = Modele_Interpole%Data_Table(i,:)
      call Compute_Additional(Current_Line)
      Modeles_Line(i) = Current_Line
    enddo

    return

  end subroutine Compute_Additional_Single
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Compute_PolarRadius(Model)
    ! Computation of the polar radius of the star.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only:type_TimeModel,i_logL,i_logTeff,i_Omega_Omcrit,i_PolarRadius
    use Constant, only:L_sun,sigma_SB,pi
    use interpolmod, only:indice,Linear_Interp
    use ReadData, only: omega_surface,n_SurfaceOmega,Surface

    implicit none

    type(type_TimeModel), intent(inout):: Model

    real(kind=8)::Real_Surface,Normalised_Surface

    ! Compute the value of the stellar surface (in physical units)
    Real_Surface = 10.d0**Model%Data_Line(i_logL)*L_sun/(Sigma_SB*10.d0**(4.d0*Model%Data_Line(i_logTeff)))

    if (Model%Data_Line(i_Omega_Omcrit) <= 1.d-4) then
      ! In case of slow rotation, the star is assumed spherical
      Model%Additional_Data_Line(i_PolarRadius) = sqrt(Real_Surface/(4.d0*pi))
    else
      ! In the other case, the deformation is accounted for.
      Normalised_Surface = Linear_Interp(Model%Data_Line(i_Omega_Omcrit),n_SurfaceOmega,omega_surface,Surface)
      ! The polar radius of the normalised surface is set to 1, and we compute the scale factor between the real
      ! surface and the normalised one.
      Model%Additional_Data_Line(i_PolarRadius) = sqrt(Real_Surface/Normalised_Surface)
    endif

    return

  end subroutine Compute_PolarRadius
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Compute_PolarGravity(Model)
    ! Computation of the polar gravity.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only:type_TimeModel,i_mass,i_polar_gravity,i_PolarRadius
    use Constant, only:G_Newton, M_sun

    implicit none

    type(type_TimeModel), intent(inout):: Model

    Model%Additional_Data_Line(i_polar_gravity) = G_Newton*Model%Data_Line(i_mass)*M_sun/ &
      Model%Additional_Data_Line(i_PolarRadius)**2.d0
    return

  end subroutine Compute_PolarGravity
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Compute_ColorMagnitude(Model)
    ! Computation of the correction to obtain a color-magnitude diagram instead of a Teff-luminosity one.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only:type_TimeModel
    use VariousParameters, only:Colour_Calibration_mode
    use Color_and_Correction, only:Compute_ColorMagnitude_New

    implicit none

    type(type_TimeModel), intent(inout):: Model

    select case (Colour_Calibration_mode)
      case (1)
        call Compute_ColorMagnitude_Old(Model)
      case (2)
        call Compute_ColorMagnitude_New(Model)
      case default
        write(*,*) 'Error in Compute_ColorMagnitude, should not occur !'
        stop
    end select

    return

  end subroutine Compute_ColorMagnitude
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Compute_ColorMagnitude_Old(Model)
    ! Computation of the correction to obtain a color-magnitude diagram instead of a Teff-luminosity one.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only:type_TimeModel,i_polar_gravity,i_MBol,Additional_Data_Number

    implicit none

    type(type_TimeModel), intent(inout):: Model

    real(kind=8), parameter::L_limit = 3.d0,dg_polar = 0.1d0
    real(kind=8)::g_polar_limit
    real(kind=8), dimension(Additional_Data_Number)::Saved_Data



    ! The following criteria are applied:
    ! If log(g_P) > 3.0, we compute the correction for the luminosity class V
    ! If log(g_P) < 3.0,
    !                  if log(L) < 3, we use the correction for class III
    !                  if log(L) > 3, we use the correction for class I

    ! Above 3 Msun, the limit is set to log(g) = 3. Below, we use a linear fit in the log(g) log(L) plot
    ! which cross the tracks during the crossing of the HRD.
    if (Model%mass_ini >= 3.d0) then
      g_polar_limit = 3.d0
    else
      g_polar_limit = -0.5*Model%Additional_Data_Line(i_L) + 4.136d0
    endif

    if (log10(Model%Additional_Data_Line(i_polar_gravity)) >= g_polar_limit + dg_polar) then
      call Correction_Class_V(Model)
    else if (log10(Model%Additional_Data_Line(i_polar_gravity)) >= g_polar_limit - dg_polar) then
      call Correction_Class_V(Model)
      Saved_Data(:) = Model%Additional_Data_Line(:)
      if (Model%Additional_Data_Line(i_L) <= L_limit) then
        call Correction_Class_III(Model)
      else
        call Correction_Class_I(Model)
      endif
      call Perform_Average(Model,Saved_Data,g_polar_limit,dg_polar)
    else
      if (Model%Additional_Data_Line(i_L) <= L_limit) then
        call Correction_Class_III(Model)
      else
        call Correction_Class_I(Model)
      endif
    endif

    Model%Additional_Data_Line(i_Mbol) = -2.5d0*Model%Additional_Data_Line(i_L) + 4.75d0

    return

  end subroutine Compute_ColorMagnitude_Old
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Correction_Class_V(Model)
    ! pour la MS
    ! RELATION Teff vs B-V
    ! D'APRES
    ! SEKIGUCHI & FUKUGITA 2000, AJ 120, 1072
    ! BOHM-VITENSE:1981,ANN. REV. A&A,19,295
    ! CORR. BOLO. D'APRES MALAGNINI ET AL:1986,162,140
    ! RELATION UBV SCHMIDT-KALER:1982,LANDOLT & BORNSTEIN
    ! RELATION BV-B2V1 MEYLAN & HAUCK: 1981,46,281
    ! verifiee 17-4-89
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure,only: type_TimeModel,i_BV,i_UB,i_MV,i_B2V1,i_BC
    use interpolmod, only: Linear_Interp

    implicit none

    type(type_TimeModel), intent(inout):: Model

    integer, parameter:: Correction_Data_Size = 28,Correction_Data_Size_Extended = 36

    real(kind=8), dimension(Correction_Data_Size), parameter:: &
      zt = (/3.740d0,3.778d0,3.813d0,3.845d0,3.875d0,3.903d0,3.929d0,3.954d0,3.978d0,4.000d0,4.041d0,4.079d0,4.114d0, &
      4.146d0,4.176d0,4.204d0,4.230d0,4.255d0,4.301d0,4.342d0,4.352d0,4.380d0,4.398d0,4.415d0,4.447d0,4.477d0, &
      4.653d0,4.688d0/), &
      zbc = (/-0.21d0,-0.08d0, 0.00d0, 0.04d0, 0.04d0, 0.02d0,-0.02d0,-0.09d0,-0.16d0,-0.25d0,-0.45d0,-0.66d0,-0.87d0, &
      -1.07d0,-1.26d0,-1.43d0,-1.58d0,-1.72d0,-1.96d0,-2.17d0,-2.22d0,-2.37d0,-2.46d0,-2.56d0,-2.75d0,-2.91d0, &
      -4.00d0,-4.30d0/), &
      zbv = (/0.70d0,0.55d0,0.420d0,0.32d0,0.22d0,0.14d0,0.10d0,0.05d0,0.00d0,-0.03d0,-0.075d0,-0.11d0,-0.13d0,-0.15d0, &
      -0.17d0,-0.18d0,-0.19d0,-0.20d0,-0.220d0,-0.24d0,-0.24d0,-0.26d0,-0.26d0,-0.27d0,-0.29d0,-0.30d0,-0.360d0, &
      -0.37d0/), &
      zub = (/0.23d0, 0.04d0,-0.016d0, 0.02d0, 0.10d0, 0.10d0, 0.09d0, 0.05d0,0.01d0,-0.06d0,-0.218d0,-0.34d0,-0.43d0, &
      -0.50d0,-0.58d0,-0.62d0,-0.67d0,-0.71d0,-0.775d0,-0.84d0,-0.87d0,-0.92d0,-0.95d0,-0.98d0,-1.05d0,-1.08d0, &
      -1.300d0,-1.34d0/)
    real(kind=8), dimension(Correction_Data_Size_Extended), parameter:: &
      zt_Extended = (/3.589d0,3.607d0,3.624d0,3.642d0,3.660d0,3.679d0,3.699d0,3.720d0,3.740d0,3.778d0,3.813d0,3.845d0, &
      3.875d0,3.903d0,3.929d0,3.954d0,3.978d0,4.000d0,4.041d0,4.079d0,4.114d0,4.146d0,4.176d0,4.204d0, &
      4.230d0,4.255d0,4.301d0,4.342d0,4.352d0,4.380d0,4.398d0,4.415d0,4.447d0,4.477d0,4.653d0,4.688d0/), &
      zbv_Extended = (/1.50d0,1.40d0,1.30d0,1.20d0,1.10d0,1.00d0,0.90d0,0.80d0,0.70d0,0.55d0,0.420d0,0.32d0,0.25d0, &
      0.18d0,0.12d0,0.05d0,0.00d0,-0.03d0,-0.075d0,-0.11d0,-0.13d0,-0.15d0,-0.17d0,-0.18d0,-0.19d0, &
      -0.20d0,-0.220d0,-0.24d0,-0.24d0,-0.26d0,-0.26d0,-0.27d0,-0.29d0,-0.30d0,-0.360d0,-0.37d0/)

    Model%Additional_Data_Line(i_BV) = Linear_Interp(Model%Additional_Data_Line(i_Teff),Correction_Data_Size_Extended, &
      zt_Extended,zbv_Extended)
    Model%Additional_Data_Line(i_UB) = Linear_Interp(Model%Additional_Data_Line(i_Teff),Correction_Data_Size,zt,zub)
    Model%Additional_Data_Line(i_BC) = Linear_Interp(Model%Additional_Data_Line(i_Teff),Correction_Data_Size,zt,zbc)
    Model%Additional_Data_Line(i_MV) =-2.5d0*Model%Additional_Data_Line(i_L)+4.75d0-Model%Additional_Data_Line(i_BC)

    if(Model%Additional_Data_Line(i_BV) <= 0.197d0) then
      Model%Additional_Data_Line(i_B2V1) = 0.712d0*Model%Additional_Data_Line(i_BV)-0.144d0
    else
      Model%Additional_Data_Line(i_B2V1) = -0.149d0+0.707d0*Model%Additional_Data_Line(i_BV) + &
        0.708d0*Model%Additional_Data_Line(i_BV)**2.d0 - &
        1.083d0*Model%Additional_Data_Line(i_BV)**3.d0 + &
        0.442d0*Model%Additional_Data_Line(i_BV)**4.d0
    endif

    return

  end subroutine Correction_Class_V
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Correction_Class_III(Model)
    ! entre XHec=XHecmax et XHec=XHecmax-0.003
    ! RELATION Teff vs B-V ET
    ! CORR BOL D'APRES FLOWER A A 54,31 (1977)
    ! UBV (CLASSE III) SCHMIDT KALER,1982,LANDOLT ET B.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure,only: type_TimeModel,i_BV,i_UB,i_MV,i_B2V1,i_BC
    use interpolmod, only: Linear_Interp

    implicit none

    type(type_TimeModel), intent(inout):: Model

    integer, parameter:: Correction_Data_Size = 38

    real(kind=8), dimension(Correction_Data_Size),parameter:: &
      zt = (/3.544d0,3.574d0,3.592d0,3.622d0,3.650d0,3.674d0,3.698d0,3.706d0,3.724d0,3.747d0,3.769d0,3.793d0,3.813d0, &
      3.845d0,3.892d0,3.914d0,3.930d0,3.944d0,3.954d0,3.981d0,4.000d0,4.023d0,4.057d0,4.099d0,4.121d0,4.211d0, &
      4.234d0,4.256d0,4.301d0,4.324d0,4.369d0,4.414d0,4.437d0,4.482d0,4.504d0,4.527d0,4.549d0,4.594d0/), &
      zbc = (/-1.66d0,-1.19d0,-0.92d0,-0.66d0,-0.49d0,-0.37d0,-0.26d0,-0.22d0,-0.16d0,-0.10d0,-0.06d0,-0.03d0,-0.01d0, &
      0.01d0, 0.00d0,-0.02d0,-0.03d0,-0.06d0,-0.09d0,-0.19d0,-0.24d0,-0.32d0,-0.46d0,-0.72d0,-0.84d0,-1.34d0, &
      -1.46d0,-1.58d0,-1.84d0,-1.97d0,-2.22d0,-2.48d0,-2.60d0,-2.87d0,-2.99d0,-3.12d0,-3.25d0,-3.50d0/), &
      zbv = (/1.61d0,1.54d0,1.45d0,1.30d0,1.160d0,1.04d0,0.92d0,0.88d0,0.80d0,0.70d0,0.60d0,0.50d0,0.430d0,0.33d0, &
      0.20d0,0.14d0,0.10d0,0.07d0,0.05d0,0.00d0,-0.025d0,-0.05d0,-0.08d0,-0.11d0,-0.12d0,-0.16d0,-0.17d0, &
      -0.18d0,-0.200d0,-0.21d0,-0.23d0,-0.25d0,-0.26d0,-0.28d0,-0.29d0,-0.30d0,-0.310d0,-0.31d0/), &
      zub = (/1.88d0,1.84d0,1.72d0,1.44d0,1.160d0,0.94d0,0.66d0,0.59d0,0.45d0,0.28d0,0.16d0,0.10d0,0.090d0,0.08d0, &
      0.11d0,0.11d0,0.10d0,0.09d0,0.06d0,0.03d0,-0.053d0,-0.13d0,-0.24d0,-0.37d0,-0.40d0,-0.54d0,-0.58d0, &
      -0.63d0,-0.740d0,-0.78d0,-0.87d0,-0.94d0,-0.97d0,-1.04d0,-1.08d0,-1.10d0,-1.120d0,-1.13d0/)

    Model%Additional_Data_Line(i_BV) = Linear_Interp(Model%Additional_Data_Line(i_Teff),Correction_Data_Size,zt,zbv)
    Model%Additional_Data_Line(i_BC) = Linear_Interp(Model%Additional_Data_Line(i_Teff),Correction_Data_Size,zt,zbc)
    Model%Additional_Data_Line(i_UB) = Linear_Interp(Model%Additional_Data_Line(i_Teff),Correction_Data_Size,zt,zub)
    Model%Additional_Data_Line(i_MV) = -2.5d0*Model%Additional_Data_Line(i_L) + 4.75d0 - Model%Additional_Data_Line(i_BC)
    Model%Additional_Data_Line(i_B2V1) = -0.140d0 + 0.739d0*Model%Additional_Data_Line(i_BV) + &
      0.284d0*Model%Additional_Data_Line(i_BV)**2.d0 - &
      0.321d0*Model%Additional_Data_Line(i_BV)**3.d0 + &
      0.115d0*Model%Additional_Data_Line(i_BV)**4.d0

    return

  end subroutine Correction_Class_III
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Correction_Class_I(Model)
    ! Pour la fusion de l'helium
    ! RELATION Teff vs B-V ET
    ! CORR BOL D'APRES FLOWER A A 54,31 (1977)
    ! UBV (CLASSE Ia) SCHMIDT KALER,1982,LANDOLT ET B.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure,only: type_TimeModel,i_BV,i_UB,i_MV,i_B2V1,i_BC
    use interpolmod, only: Linear_Interp

    implicit none

    type(type_TimeModel), intent(inout):: Model

    integer, parameter:: Correction_Data_Size = 38

    real(kind=8), dimension(Correction_Data_Size)::&
      ZT = (/3.447d0,3.477d0,3.512d0,3.544d0,3.574d0,3.588d0,3.605d0,3.632d0,3.656d0,3.677d0,3.698d0,3.705d0,3.743d0, &
      3.766d0,3.793d0,3.845d0,3.892d0,3.914d0,3.930d0,3.944d0,3.960d0,4.011d0,4.038d0,4.081d0,4.137d0,4.194d0, &
      4.213d0,4.288d0,4.307d0,4.325d0,4.363d0,4.419d0,4.457d0,4.476d0,4.513d0,4.532d0,4.570d0,4.607d0/), &
      ZBC = (/-3.36d0,-2.50d0,-1.72d0,-1.43d0,-1.00d0,-0.84d0,-0.67d0,-0.46d0,-0.35d0,-0.22d0,-0.14d0,-0.12d0,-0.01d0, &
      0.04d0,0.08d0,0.13d0,0.14d0,0.09d0,0.00d0,-0.10d0,-0.17d0,-0.38d0,-0.51d0,-0.64d0,-0.82d0,-1.05d0, &
      -1.16d0,-1.56d0,-1.67d0,-1.76d0,-2.02d0,-2.40d0,-2.68d0,-2.79d0,-3.06d0,-3.20d0,-3.46d0,-3.73d0/), &
      ZBV = (/1.80d0,1.76d0,1.72d0,1.70d0,1.61d0,1.54d0,1.450d0,1.30d0,1.16d0,1.04d0,0.92d0,0.88d0,0.70d0,0.60d0, &
      0.500d0,0.33d0,0.20d0,0.14d0,0.10d0,0.07d0,0.05d0,0.00d0,-0.025d0,-0.05d0,-0.08d0,-0.11d0,-0.12d0, &
      -0.16d0,-0.17d0,-0.18d0,-0.200d0,-0.23d0,-0.25d0,-0.26d0,-0.28d0,-0.29d0,-0.31d0,-0.32d0/), &
      ZUB = (/2.11d0,2.05d0,1.99d0,1.92d0,1.81d0,1.71d0,1.572d0,1.25d0,1.06d0,0.84d0,0.70d0,0.66d0,0.50d0,0.45d0, &
      0.392d0,0.28d0,0.17d0,0.12d0,-0.04d0,-0.15d0,-0.20d0,-0.58d0,-0.635d0,-0.70d0,-0.76d0,-0.83d0,-0.85d0, &
      -0.96d0,-0.97d0,-0.99d0,-1.013d0,-1.05d0,-1.09d0,-1.11d0,-1.13d0,-1.13d0,-1.16d0,-1.17d0/)

    Model%Additional_Data_Line(i_BV) = Linear_Interp(Model%Additional_Data_Line(i_Teff),Correction_Data_Size,zt,zbv)
    Model%Additional_Data_Line(i_BC) = Linear_Interp(Model%Additional_Data_Line(i_Teff),Correction_Data_Size,zt,zbc)
    Model%Additional_Data_Line(i_UB) = Linear_Interp(Model%Additional_Data_Line(i_Teff),Correction_Data_Size,zt,zub)
    Model%Additional_Data_Line(i_MV) = -2.5d0*Model%Additional_Data_Line(i_L) + 4.75d0 - Model%Additional_Data_Line(i_BC)
    Model%Additional_Data_Line(i_B2V1) = -0.121d0 + 0.722d0*Model%Additional_Data_Line(i_BV) + &
      0.079d0*Model%Additional_Data_Line(i_BV)**2.d0

    return

  end subroutine Correction_Class_I
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Perform_Average(Model,Saved,g_lim,dg)
    ! Set the mass step in isochrone mode.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_TimeModel,Additional_Data_Number,i_MV,i_UB,i_BV,i_B2V1,i_polar_gravity

    implicit none

    type(type_TimeModel), intent(inout)::Model

    real(kind=8), intent(in)::g_lim,dg
    real(kind=8), dimension(Additional_Data_Number), intent(in)::Saved

    real(kind=8)::Fraction_V

    ! Fraction of the magnitude from V calibration to apply (if g_pol = g_lim + dg, Fraction_V = 1,
    ! if g_pol = g_lim - dg, Fraction_V = 0)
    Fraction_V = (log10(Model%Additional_Data_Line(i_polar_gravity)) - g_lim + dg)/(2.d0*dg)

    Model%Additional_Data_Line(i_MV) = Fraction_V*Saved(i_MV) + (1.d0 - Fraction_V)*Model%Additional_Data_Line(i_MV)
    Model%Additional_Data_Line(i_UB) = Fraction_V*Saved(i_UB) + (1.d0 - Fraction_V)*Model%Additional_Data_Line(i_UB)
    Model%Additional_Data_Line(i_BV) = Fraction_V*Saved(i_BV) + (1.d0 - Fraction_V)*Model%Additional_Data_Line(i_BV)
    Model%Additional_Data_Line(i_B2V1) = Fraction_V*Saved(i_B2V1) + (1.d0 - Fraction_V)*Model%Additional_Data_Line(i_B2V1)

    return

  end subroutine Perform_Average
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine correct_fact(Time_Model)
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_TimeModel,i_mass,i_logL,i_logTeff_corr,i_logL_gd,i_logTeff_gd, &
                             i_logL_lgd,i_logTeff_lgd,i_Omega_Omcrit
    use ReadData, only: n_omega_Corr,omega_list_data,AoV_Corr,n_Angle_Corr,Correct_Lum,Correct_Teff
    use interpolmod, only: indice,give_factor,Bilin_Interpol,Plane_Interpol
    use Constant, only: G_Newton, M_sun, pi, sigma_SB, L_sun

    implicit none

    type(type_TimeModel), intent(inout)::Time_Model
    integer :: k,l,m
    integer:: Position_Omega,Position_Angle
    real(kind=8):: P1, P2, P3, P4, fac_logg, fac_teff, teff_n, logg_n
    real(kind=8), dimension(2,2):: L_C,T_C
    real(kind=8)::factor_Omega,factor_Angle,Correction_Lum,Correction_Teff

    Position_Omega = indice(Time_Model%Data_Line(i_Omega_Omcrit),omega_list_data,n_omega_Corr)
    factor_Omega = give_factor(Time_Model%Data_Line(i_Omega_Omcrit),omega_list_data(Position_Omega), &
      omega_list_data(Position_Omega+1))
    Position_Angle = indice(Time_Model%Angle_of_View,AoV_Corr,n_Angle_Corr)
    factor_Angle = give_factor(Time_Model%Angle_of_View,AoV_Corr(Position_Angle), &
      AoV_Corr(Position_Angle+1))

    teff_n = 10.d0**(Time_Model%Data_Line(i_logTeff_corr))
    logg_n = log10(G_Newton*Time_Model%Data_Line(i_mass)*M_sun*pi*sigma_SB*&
      (10.d0**(Time_Model%Data_Line(i_logTeff_corr)))**4.d0/(10.d0**(Time_Model%Data_Line(i_logL))*L_sun))
    if (teff_n < 5000.d0) then
      teff_n=5000.d0
    endif
    k = int(teff_n/5000.d0)
    if (teff_n == dble(k)*5000.d0 .and. teff_n < 5000.d0) then
      k=k-1
    endif
    if (logg_n > 4.5d0) then
      logg_n = 4.5d0
    endif
    if (teff_n <= 40000 .and. logg_n >= 4.d0) then
      fac_logg = give_factor(logg_n,4.d0,4.5d0)
      fac_teff = give_factor(teff_n,5000.d0*k,(k+1)*5000.d0)
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum((k-1)*91+l,m+102)
          P2=Correct_Lum((k)*91+l,m+102)
          P3=Correct_Lum((k)*91+l,m)
          P4=Correct_Lum((k-1)*91+l,m)
          L_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
          P1=Correct_Teff((k-1)*91+l,m+102)
          P2=Correct_Teff((k)*91+l,m+102)
          P3=Correct_Teff((k)*91+l,m)
          P4=Correct_Teff((k-1)*91+l,m)
          T_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
        enddo
      enddo
    else if (teff_n <= 30000 .and. logg_n >= 3.5d0 .and. logg_n < 4.0d0) then
      fac_logg = give_factor(logg_n,3.5d0,4.0d0)
      fac_teff = give_factor(teff_n,k*5000.d0,(k+1)*5000.d0)
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum((k-1)*91+l,m+2*102)
          P2=Correct_Lum((k)*91+l,m+2*102)
          P3=Correct_Lum((k)*91+l,m+102)
          P4=Correct_Lum((k-1)*91+l,m+102)
          L_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
          P1=Correct_Teff((k-1)*91+l,m+2*102)
          P2=Correct_Teff((k)*91+l,m+2*102)
          P3=Correct_Teff((k)*91+l,m+102)
          P4=Correct_Teff((k-1)*91+l,m+102)
          T_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
        enddo
      enddo
    else if (teff_n <= 25000 .and. logg_n > 3.0d0 .and. logg_n <= 3.5d0) then
      fac_logg = give_factor(logg_n,3.0d0,3.5d0)
      fac_teff = give_factor(teff_n,k*5000.d0,(k+1)*5000.d0)
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum((k-1)*91+l,m+3*102)
          P2=Correct_Lum((k)*91+l,m+3*102)
          P3=Correct_Lum((k)*91+l,m+2*102)
          P4=Correct_Lum((k-1)*91+l,m+2*102)
          L_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
          P1=Correct_Teff((k-1)*91+l,m+3*102)
          P2=Correct_Teff((k)*91+l,m+3*102)
          P3=Correct_Teff((k)*91+l,m+2*102)
          P4=Correct_Teff((k-1)*91+l,m+2*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
        enddo
      enddo
    else if (teff_n <= 20000 .and. logg_n >= 2.5d0 .and. logg_n < 3.0d0) then
      fac_logg = give_factor(logg_n,2.5d0,3.0d0)
      fac_teff = give_factor(teff_n,k*5000.d0,(k+1)*5000.d0)
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum((k-1)*91+l,m+4*102)
          P2=Correct_Lum((k)*91+l,m+4*102)
          P3=Correct_Lum((k)*91+l,m+3*102)
          P4=Correct_Lum((k-1)*91+l,m+3*102)
          L_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
          P1=Correct_Teff((k-1)*91+l,m+4*102)
          P2=Correct_Teff((k)*91+l,m+4*102)
          P3=Correct_Teff((k)*91+l,m+3*102)
          P4=Correct_Teff((k-1)*91+l,m+3*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
        enddo
      enddo
    else if (teff_n <= 15000 .and. logg_n >= 2.0d0 .and. logg_n < 2.5d0) then
      fac_logg = give_factor(logg_n,2.0d0,2.5d0)
      fac_teff = give_factor(teff_n,k*5000.d0,(k+1)*5000.d0)
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum((k-1)*91+l,m+5*102)
          P2=Correct_Lum(k*91+l,m+5*102)
          P3=Correct_Lum(k*91+l,m+4*102)
          P4=Correct_Lum((k-1)*91+l,m+4*102)
          L_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
          P1=Correct_Teff((k-1)*91+l,m+5*102)
          P2=Correct_Teff(k*91+l,m+5*102)
          P3=Correct_Teff(k*91+l,m+4*102)
          P4=Correct_Teff((k-1)*91+l,m+4*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
        enddo
      enddo
    else if (teff_n <= 10000 .and. logg_n >= 1.5d0 .and. logg_n < 2.0d0) then
      fac_logg = give_factor(logg_n,1.5d0,2.0d0)
      fac_teff = give_factor(teff_n,k*5000.d0,(k+1)*5000.d0)
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum((k-1)*91+l,m+6*102)
          P2=Correct_Lum(k*91+l,m+6*102)
          P3=Correct_Lum(k*91+l,m+5*102)
          P4=Correct_Lum((k-1)*91+l,m+5*102)
          L_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
          P1=Correct_Teff((k-1)*91+l,m+6*102)
          P2=Correct_Teff(k*91+l,m+6*102)
          P3=Correct_Teff(k*91+l,m+5*102)
          P4=Correct_Teff((k-1)*91+l,m+5*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = Bilin_Interpol(P1,P2,P3,P4,fac_teff,fac_logg)
        enddo
      enddo
    else if (teff_n > 40000 .and. logg_n >= 4) then
      if (teff_n > 40000.d0+(5000d0/0.5)*(logg_n-4.d0)) then
        teff_n=40000.d0+10000d0*(logg_n-4.d0)
      endif
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum(7*91+l,m)
          P2=Correct_Lum(8*91+l,m)
          P3=Correct_Lum(7*91+l,m+102)
          L_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(4.5d0,40000.d0,P1,4.5d0,45000d0,P2,4.0d0,40000.d0,P3,logg_n,teff_n)
          P1=Correct_Teff(7*91+l,m)
          P2=Correct_Teff(8*91+l,m)
          P3=Correct_Teff(7*91+l,m+102)
          T_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(4.5d0,40000.d0,P1,4.5d0,45000d0,P2,4.0d0,40000.d0,P3,logg_n,teff_n)
        enddo
      enddo
    else if (teff_n > 30000 .and. logg_n >= 3.5 .and. logg_n < 4.0) then
      if (teff_n > 30000.d0+10000.d0*(logg_n-3.5d0)) then
        teff_n=30000.d0+10000.d0*(logg_n-3.5d0)
      endif
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum(5*91+l,m+102)
          P2=Correct_Lum(7*91+l,m+102)
          P3=Correct_Lum(5*91+l,m+2*102)
          L_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(4.0d0,40000d0,P1,4.0d0,30000d0,P2,3.5d0,30000d0,P3,logg_n,teff_n)
          P1=Correct_Teff(5*91+l,m+102)
          P2=Correct_Teff(7*91+l,m+102)
          P3=Correct_Teff(5*91+l,m+2*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(4.0d0,40000d0,P1,4.0d0,30000d0,P2,3.5d0,30000d0,P3,logg_n,teff_n)
        enddo
      enddo
    else if (teff_n > 25000 .and. logg_n > 3.0 .and. logg_n <= 3.5) then
      if (teff_n > 25000.d0+10000.d0*(logg_n-3.0d0)) then
        teff_n=25000.d0+10000.d0*(logg_n-3.0d0)
      endif
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum(4*91+l,m+2*102)
          P2=Correct_Lum(5*91+l,m+2*102)
          P3=Correct_Lum(4*91+l,m+3*102)
          L_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(3.5d0,30000d0,P1,3.5d0,25000d0,P2,3.0d0,25000d0,P3,logg_n,teff_n)
          P1=Correct_Teff(4*91+l,m+2*102)
          P2=Correct_Teff(5*91+l,m+2*102)
          P3=Correct_Teff(4*91+l,m+3*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(3.5d0,30000d0,P1,3.5d0,25000d0,P2,3.0d0,25000d0,P3,logg_n,teff_n)
        enddo
      enddo
    else if (teff_n > 20000 .and. logg_n >= 2.5 .and. logg_n < 3.0) then
      if (teff_n > 20000.d0+10000.d0*(logg_n-2.5d0)) then
        teff_n=20000.d0+10000.d0*(logg_n-2.5d0)
      endif
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum(3*91+l,m+3*102)
          P2=Correct_Lum(4*91+l,m+3*102)
          P3=Correct_Lum(3*91+l,m+4*102)
          L_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(3.0d0,25000d0,P1,3.0d0,20000d0,P2,2.5d0,20000d0,P3,logg_n,teff_n)
          P1=Correct_Teff(3*91+l,m+3*102)
          P2=Correct_Teff(4*91+l,m+3*102)
          P3=Correct_Teff(3*91+l,m+4*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(3.0d0,25000d0,P1,3.0d0,20000d0,P2,2.5d0,20000d0,P3,logg_n,teff_n)
        enddo
      enddo
    else if (teff_n > 15000.d0 .and. logg_n >= 2.0d0 .and. logg_n < 2.5d0) then
      if (teff_n > 15000.d0+10000.d0*(logg_n-2.0d0)) then
        teff_n=15000.d0+10000.d0*(logg_n-2.0d0)
      endif
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum(2*91+l,m+4*102)
          P2=Correct_Lum(3*91+l,m+4*102)
          P3=Correct_Lum(2*91+l,m+5*102)
          L_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(2.5d0,20000.d0,P1,2.5d0,15000.d0,P2,2.0d0,15000.d0,P3,logg_n,teff_n)
          P1=Correct_Teff(2*91+l,m+4*102)
          P2=Correct_Teff(3*91+l,m+4*102)
          P3=Correct_Teff(2*91+l,m+5*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(2.5d0,20000.d0,P1,2.5d0,15000.d0,P2,2.0d0,15000.d0,P3,logg_n,teff_n)
        enddo
      enddo
    else if (teff_n > 10000.d0 .and. logg_n >= 1.5d0 .and. logg_n < 2.0d0) then
      if (teff_n > 10000.d0+10000.d0*(logg_n-1.5d0)) then
        teff_n=10000.d0+10000.d0*(logg_n-1.5d0)
      endif
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum(1*91+l,m+5*102)
          P2=Correct_Lum(2*91+l,m+5*102)
          P3=Correct_Lum(1*91+l,m+6*102)
          L_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(2.0d0,15000.d0,P1,2.0d0,10000.d0,P2,1.5d0,10000.d0,P3,logg_n,teff_n)
          P1=Correct_Teff(1*91+l,m+5*102)
          P2=Correct_Teff(2*91+l,m+5*102)
          P3=Correct_Teff(1*91+l,m+6*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(2.0d0,15000.d0,P1,2.0d0,10000.d0,P2,1.5d0,10000.d0,P3,logg_n,teff_n)
        enddo
      enddo
    else if (logg_n >= 1.0d0 .and. logg_n < 1.5d0) then
      if (teff_n > 5000.d0+10000.d0*(logg_n-1.0d0)) then
        teff_n=5000.d0+10000.d0*(logg_n-1.0d0)
      endif
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          P1=Correct_Lum(l,m+6*102)
          P2=Correct_Lum(91+l,m+6*102)
          P3=Correct_Lum(l,m+7*102)
          L_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(1.5d0,10000.d0,P1,1.5d0,5000.d0,P2,1.0d0,5000.d0,P3,logg_n,teff_n)
          P1=Correct_Teff(l,m+6*102)
          P2=Correct_Teff(91+l,m+6*102)
          P3=Correct_Teff(l,m+7*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = &
            Plane_Interpol(1.5d0,10000.d0,P1,1.5d0,5000.d0,P2,1.0d0,5000.d0,P3,logg_n,teff_n)
        enddo
      enddo
    else if (logg_n >= 0.5 .and. logg_n < 1.0d0) then
      teff_n=5000.d0
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1 !
          L_C(l+1-Position_Angle,m-Position_Omega) = &
            (Correct_Lum(l,m+7*102)-Correct_Lum(l,m+8*102))*(logg_n-0.5d0)/(0.5d0)+Correct_Lum(l,m+8*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = &
            (Correct_Teff(l,m+7*102)-Correct_Teff(l,m+8*102))*(logg_n-0.5d0)/(0.5d0)+Correct_Teff(l,m+8*102)
        enddo
      enddo
    else if (logg_n < 0.5) then
      teff_n=5000.d0
      do l=Position_Angle,Position_Angle+1
        do m=Position_Omega+1,Position_Omega+1+1
          L_C(l+1-Position_Angle,m-Position_Omega) =  &
            (Correct_Lum(l,m+8*102) - Correct_Lum(l,m+9*102))*logg_n/(0.5d0)+ Correct_Lum(l,m+9*102)
          T_C(l+1-Position_Angle,m-Position_Omega) = &
            (Correct_Teff(l,m+8*102)-Correct_Teff(l,m+9*102))*logg_n/(0.5d0)+Correct_Teff(l,m+9*102)
        enddo
      enddo
    endif

    ! Call the bilinear interpolation routine.
    Correction_Lum = Bilin_Interpol(L_C(1,1),L_C(2,1), &
      L_C(2,2),L_C(1,2), &
      factor_Angle,factor_Omega)
    Correction_Teff = Bilin_Interpol(T_C(1,1),T_C(2,1), &
      T_C(2,2),T_C(1,2), &
      factor_Angle,factor_Omega)
    ! Add the correction.
    Time_Model%Additional_Data_Line(i_logL_lgd) = Time_Model%Data_Line(i_logL) +log10(Correction_Lum)
    Time_Model%Additional_Data_Line(i_logTeff_lgd) = Time_Model%Data_Line(i_logTeff_corr) +log10(Correction_Teff)

    return

  end subroutine correct_fact
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine  Correct_AngleofView(Time_Model)
    ! Correction of the Teff and Lum due to the inclination of the star and its rotational velocity.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_TimeModel,i_Omega_Omcrit,i_logL,i_logTeff_corr,i_logL_gd, &
                             i_logTeff_gd,i_logL_lgd,i_logTeff_lgd,i_mean_gravity, &
                             i_polar_gravity
    use ReadData, only: n_omega_Corr,omega_list_data,AoV_Corr,n_Angle_Corr,L_Corr,Teff_Corr, &
                        Grav_Corr
    use interpolmod, only: indice,give_factor,Bilin_Interpol

    implicit none

    type(type_TimeModel), intent(inout)::Time_Model

    integer:: Position_Omega,Position_Angle

    real(kind=8)::factor_Omega,factor_Angle,Correction_Lum,Correction_Teff,Correction_Grav

    ! Find the positions and the interpolation factors in the tables.
    Position_Omega = indice(Time_Model%Data_Line(i_Omega_Omcrit),omega_list_data,n_omega_Corr)
    factor_Omega = give_factor(Time_Model%Data_Line(i_Omega_Omcrit),omega_list_data(Position_Omega), &
      omega_list_data(Position_Omega+1))
    Position_Angle = indice(Time_Model%Angle_of_View,AoV_Corr,n_Angle_Corr)
    factor_Angle = give_factor(Time_Model%Angle_of_View,AoV_Corr(Position_Angle),AoV_Corr(Position_Angle+1))

    ! Call the bilinear interpolation routine.
    Correction_Lum = Bilin_Interpol(L_Corr(Position_Angle,Position_Omega),L_Corr(Position_Angle+1,Position_Omega), &
      L_Corr(Position_Angle+1,Position_Omega+1),L_Corr(Position_Angle,Position_Omega+1), &
      factor_Angle,factor_Omega)
    Correction_Teff = Bilin_Interpol(Teff_Corr(Position_Angle,Position_Omega),Teff_Corr(Position_Angle+1,Position_Omega), &
      Teff_Corr(Position_Angle+1,Position_Omega+1),Teff_Corr(Position_Angle,Position_Omega+1), &
      factor_Angle,factor_Omega)
    Correction_Grav = Bilin_Interpol(Grav_Corr(Position_Angle,Position_Omega),Grav_Corr(Position_Angle+1,Position_Omega), &
      Grav_Corr(Position_Angle+1,Position_Omega+1),Grav_Corr(Position_Angle,Position_Omega+1), &
      factor_Angle,factor_Omega)
    ! Add the correction.
    Time_Model%Additional_Data_Line(i_logL_gd) = Time_Model%Data_Line(i_logL) +log10(Correction_Lum)
    Time_Model%Additional_Data_Line(i_logTeff_gd) = Time_Model%Data_Line(i_logTeff_corr) +log10(Correction_Teff)
    Time_Model%Additional_Data_Line(i_mean_gravity) = Time_Model%Additional_Data_Line(i_polar_gravity)*Correction_Grav

    return

  end subroutine Correct_AngleofView
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module Additional_Data
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module Population_Mode
  ! Module containing the routines used in the mode which computes the time evolution of the population of
  ! B and Be stars.
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  implicit none

  integer,public,parameter:: Time_Step_data_Number = 17          ! Number of data stored in the Time_Step_data array.
  integer,public,parameter:: i_tsdata_Teff = 1, i_tsdata_L = 2, i_tsdata_Om_OmCr = 3, i_tsdata_v_vCr = 4, &
    i_tsdata_H = 5, i_tsdata_He = 6,i_tsdata_C = 7, i_tsdata_N = 8, i_tsdata_O = 9, &
    i_tsdata_Mass = 10, i_tsdata_MV = 11,i_tsdata_Hcen = 12, i_tsdata_Mbol = 13, &
    i_tsdata_Mass_ini=14,i_tsdata_BV = 15, i_tsdata_vsurf = 16, i_Omega_Omcrit_ini = 17
                                                                ! WARNING ! If changed, adapt
                                                                ! also the value of Time_Step_data_Number.
  integer,public,parameter:: Evolutionary_Values = 50           ! Number of variables that we want to follow as a
                                                                ! function of time.
  integer,public,parameter:: i_Total = 1, i_OStar = 2,i_BStar = 3, i_AStar = 4, i_FStar = 5, i_Oe = 6, i_Be = 7, &
    i_Ae = 8, i_Fe = 9, i_O50 = 10, i_O70 = 11, i_O80 = 12,i_O90 = 13, i_O95 = 14, &
    i_O98 = 15, i_B50 = 16, i_B70 = 17, i_B80 = 18, i_B90 = 19, i_B95 = 20,i_B98 = 21, &
    i_A50 = 22, i_A70 = 23, i_A80 = 24, i_A90 = 25, i_A95 = 26, i_A98 = 27, i_F50 = 28, &
    i_F70 = 29, i_F80 = 30, i_F90 = 31, i_F95 = 32, i_F98 = 33, i_RSG = 34, i_YSG= 35, &
    i_WNL = 36, i_WNE = 37, i_WC = 38, i_WO = 39, i_Cepheid = 40, i_Ceph_norm = 41, &
    i_RSGbox1 = 42, i_RSGbox2 = 43, i_RSGbox3 = 44, i_RSGbox4 = 45, i_BSG = 46,i_earlyBStar = 47, &
    i_earlyBStar80 = 48,i_MS2m = 49, i_Mtot = 50
                                                                ! WARNING ! If changed, adapt
                                                                ! also the value of Evolutionary_Values.
  integer,private,parameter:: i_TurnOff = 110                   ! WARNING : THIS IS TRUE ONLY FOR THE STANDARD GRID FORMAT
                                                                ! (400 LINES). IF THE FORMAT CHANGES, KEEP IN MIND THAT
                                                                ! THIS SHOULD BE CHANGE ACCORDINGLY.

  integer,public,save::Pop_Mass_Beam_Number = 1000              ! Number of mass beam in population mode
  integer,public,save::Pop_Omega_Beam_Number = 100              ! Number of velocity beam in population mode

  integer,public,save::N_Time_step = 500                        ! Number of timesteps in population mode

  real(kind=8), public, save:: Initial_Time = -1.d0,Final_Time = -1.d0
  real(kind=8), private, save:: logL_TurnOff_All,MV_TurnOff_All
  real(kind=8), dimension(:), public, pointer, save:: time_step_array
  real(kind=8), dimension(:), public, pointer, save:: Mass_Interfaces
  real(kind=8), dimension(:), public, pointer, save:: Velocity_Interfaces
  real(kind=8), dimension(:,:), private, pointer, save:: Teff_min, Teff_min_time
  real(kind=8), dimension(:,:), private, pointer, save:: logL_TurnOff,MV_TurnOff
  real(kind=8), dimension(:,:), private, pointer, save:: Omega_Number_interfaces
  real(kind=8), dimension(3), private, save:: Normalisation_Omega

  real(kind=8),dimension(:,:),public,save,pointer::Number_of_Star_Beam ! Array containg for each mass and velocity
                                                                       ! beam the number of stars (normalised to 1).
  real(kind=8),dimension(:,:,:,:),public,save,pointer::Time_Step_data ! Array containing the useful data in
                                                                      ! population mode.
  real(kind=8),dimension(:,:),public, pointer, save:: Evolution_Data

  character(8)::Name_Extension

  logical,public,save:: HRDdensity = .false.
  logical, dimension(:,:), private, pointer, save:: Found_TurnOff


  public:: Initial_population
  private:: Compute_Interfaces
  private:: Number_in_omega_beam
  public:: Make_Each_Time_step
  public:: Count_Populations
  public:: Is_a_Cepheid
  private:: WriteResultsMovie

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Initial_population
    ! Initialise the population
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: m_IMF_inf,m_IMF_sup,ivdist,om_ivdist
    use ReadData, only: Huang_m_limit

    implicit none

    integer::i,j

    real(kind=8), parameter:: IMF_slope = -1.35d0
    real(kind=8)::Normalisation_mass,N_star_mass,mean_mass

    ! Allocation of the memory.
    allocate(Mass_Interfaces(Pop_Mass_Beam_Number+1))
    allocate(Velocity_Interfaces(Pop_Omega_Beam_Number+1))
    allocate(Omega_Number_interfaces(Pop_Omega_Beam_Number+1,3))
    allocate(logL_TurnOff(N_Time_step,Pop_Omega_Beam_Number))
    allocate(MV_TurnOff(N_Time_step,Pop_Omega_Beam_Number))
    allocate(Found_TurnOff(N_Time_step,Pop_Omega_Beam_Number))
    allocate(Evolution_Data(N_Time_step,Evolutionary_Values))
    allocate(Teff_min(Pop_Mass_Beam_Number,Pop_Omega_Beam_Number))
    allocate(Teff_min_time(Pop_Mass_Beam_Number,Pop_Omega_Beam_Number))

    ! Initialisation :
    logL_TurnOff(:,:) = -1000.d0
    MV_TurnOff(:,:) = 1000.d0
    Found_TurnOff(:,:) = .false.
    Teff_min(:,:) = 0.d0
    Teff_min_time(:,:) = 0.d0

    ! Compute the values of the interfaces.
    call Compute_Interfaces
    ! Compute the number of the cumulative distribution of omega at the interfaces.
    call Number_in_omega_beam

    ! Compute the number of stars in each beam (normalised to a total population of 1).
    Normalisation_mass = m_IMF_inf**IMF_slope - m_IMF_sup**IMF_slope
    do i=1,Pop_Mass_Beam_Number
      N_star_mass = (Mass_Interfaces(i)**IMF_slope - Mass_Interfaces(i+1)**IMF_slope)/Normalisation_mass
      mean_mass = (Mass_Interfaces(i) + Mass_Interfaces(i+1))/2.d0
      do j=1,Pop_Omega_Beam_Number
        select case (ivdist)
          case (0)
            Number_of_Star_Beam(i,j) = N_star_mass*(Omega_Number_interfaces(j+1,1) - Omega_Number_interfaces(j,1))/ &
              Normalisation_Omega(1)
          case (1)
            if (mean_mass < Huang_m_limit(1)) then
              Number_of_Star_Beam(i,j) = (Omega_Number_interfaces(j+1,1) - Omega_Number_interfaces(j,1))/ &
                Normalisation_Omega(1)
            else if (mean_mass < Huang_m_limit(2)) then
              Number_of_Star_Beam(i,j) = (Omega_Number_interfaces(j+1,2) - Omega_Number_interfaces(j,2))/ &
                Normalisation_Omega(2)
            else
              Number_of_Star_Beam(i,j) = (Omega_Number_interfaces(j+1,3) - Omega_Number_interfaces(j,3))/ &
                Normalisation_Omega(3)
            endif
            Number_of_Star_Beam(i,j) = Number_of_Star_Beam(i,j)*N_star_mass
          case (2)
            Number_of_Star_Beam(i,j) = N_star_mass*(Omega_Number_interfaces(j+1,1) - Omega_Number_interfaces(j,1))/ &
              Normalisation_Omega(1)
          case (3)
            if (Velocity_Interfaces(j) <= om_ivdist .and. Velocity_Interfaces(j+1) > om_ivdist) then
              Number_of_Star_Beam(i,j) = N_star_mass
            endif
          case (4)
            Number_of_Star_Beam(i,j) = N_star_mass*(Omega_Number_interfaces(j+1,1) - Omega_Number_interfaces(j,1))/ &
              Normalisation_Omega(1)
          case default
            write(*,*) 'The mode "Population computation" need an omega distribution of Huang or Huang & Gies...'
            stop
        end select
      enddo
    enddo

    return

  end subroutine Initial_population
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Compute_Interfaces
    ! Compute tha values of the mass and velocities at the cells interfaces
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only:m_IMF_inf,m_IMF_sup

    implicit none

    integer::i

    real(kind=8)::dm,domega

    ! Compute the steps in mass and omega
    dm = (m_IMF_sup - m_IMF_inf)/dble(Pop_Mass_Beam_Number)
    domega = 1.d0/dble(Pop_Omega_Beam_Number)

    ! Compute the values at the edges of each beam
    Mass_Interfaces(1) = m_IMF_inf
    do i=2,Pop_Mass_Beam_Number + 1
      Mass_Interfaces(i) = m_IMF_inf + dble(i-1)*dm
    enddo

    Velocity_Interfaces(1) = 0.d0
    do i=2,Pop_Omega_Beam_Number + 1
      Velocity_Interfaces(i) = dble(i-1)*domega
    enddo

    return

  end subroutine Compute_Interfaces
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Number_in_omega_beam
    ! Compute the number of star in each omega_beam.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: ivdist
    use interpolmod, only: Linear_Interp
    use ReadData, only: n_Huang,omega_Huang,dist_Huang_1,dist_Huang_2,dist_Huang_3,n_HG,omega_HG,dist_HG, &
                        n_ext,omega_ext,dist_ext

    implicit none

    integer::i

    ! Compute the numbers for the Huang and Huang & Gies distributions.
    select case (ivdist)
      ! For each boarding value of the velocity (at the edge of each beam), we compute the value of the omega cumulative
      ! distribution.
      ! The normalisation is also computed.
      case(0)
        do i=1,Pop_Omega_Beam_Number
          Omega_Number_interfaces(i,1) = Velocity_Interfaces(i)
        enddo
        Omega_Number_interfaces(Pop_Omega_Beam_Number+1,1) = 1.d0
        Normalisation_Omega(1) = 1.d0

      case(1)
        do i=1,Pop_Omega_Beam_Number
          Omega_Number_interfaces(i,1) = Linear_Interp(Velocity_Interfaces(i),n_Huang,omega_Huang,dist_Huang_1)
          Omega_Number_interfaces(i,2) = Linear_Interp(Velocity_Interfaces(i),n_Huang,omega_Huang,dist_Huang_2)
          Omega_Number_interfaces(i,3) = Linear_Interp(Velocity_Interfaces(i),n_Huang,omega_Huang,dist_Huang_3)
        enddo
        Omega_Number_interfaces(Pop_Omega_Beam_Number+1,1) = 1.d0
        Omega_Number_interfaces(Pop_Omega_Beam_Number+1,2) = 1.d0
        Omega_Number_interfaces(Pop_Omega_Beam_Number+1,3) = 1.d0

        Normalisation_Omega(1) = dist_Huang_1(n_Huang) -dist_Huang_1(1)
        Normalisation_Omega(2) = dist_Huang_2(n_Huang) -dist_Huang_2(1)
        Normalisation_Omega(3) = dist_Huang_3(n_Huang) -dist_Huang_3(1)

      case(2)
        do i=1,Pop_Omega_Beam_Number
          Omega_Number_interfaces(i,1) = Linear_Interp(Velocity_Interfaces(i),n_HG,omega_HG,dist_HG)
        enddo
        Omega_Number_interfaces(Pop_Omega_Beam_Number+1,1) = 1.d0
        Normalisation_Omega(1) = dist_HG(n_HG) -dist_HG(1)

      case(3)
      
      case(4)
        do i=1,Pop_Omega_Beam_Number
          Omega_Number_interfaces(i,1) = Linear_Interp(Velocity_Interfaces(i),n_ext,omega_ext,dist_ext)
        enddo
        Omega_Number_interfaces(Pop_Omega_Beam_Number+1,1) = 1.d0
        Normalisation_Omega(1) = dist_ext(n_ext) -dist_ext(1)

      case default
        write(*,*) 'In population study mode, ivdist = 1 or 2 is mandatory !'
        stop
    end select


    return

  end subroutine Number_in_omega_beam
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Make_Each_Time_step(Model,Coord1,Coord2,TimeModel)
    ! For each time step, compute the interpolated model, check if it is still alive, and fill the data array.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_DataStructure,type_TimeModel,Table_Line_Number,i_time,i_logTeff_corr,i_logL, &
      i_Omega_Omcrit,i_H1_Surf,i_He4_surf,i_C12_surf,i_N14_surf,i_O16_surf,i_v_equa,i_v_crit1, &
      i_mass,i_MV,i_H1_cen,i_MBol,i_BV,i_v_equa
    use VariousParameters, only: age_log
    use interpolmod, only: Make_TimeModel
    use Additional_Data, only: Compute_Additional

    implicit none

    type(type_DataStructure), intent(in)::Model
    integer, intent(in):: Coord1,Coord2

    type(type_TimeModel), intent(out)::TimeModel

    integer::i

    Teff_min(Coord1,Coord2) = minval(Model%Data_Table(:,i_logTeff_corr))
    Teff_min_time(Coord1,Coord2) = Model%Data_Table(minloc(Model%Data_Table(:,i_logTeff_corr),1),i_time)

    do i=1,N_Time_step
      age_log = log10(time_step_array(i))
      ! Check if the star is still alive. If not, fill the data array with negative numbers.
      if (time_step_array(i) > Model%Data_Table(Table_Line_Number,i_time)) then
        Time_Step_data(Coord1,Coord2,i,:) = -10.d0
      else
        call Make_TimeModel(Model,age_log,TimeModel)
        call Compute_Additional(TimeModel)
        Time_Step_data(Coord1,Coord2,i,i_Omega_Omcrit_ini) = TimeModel%Omega_Omcrit_ini
        Time_Step_data(Coord1,Coord2,i,i_tsdata_Teff) = TimeModel%Data_Line(i_logTeff_corr)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_L) = TimeModel%Data_Line(i_logL)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_Om_OmCr) = TimeModel%Data_Line(i_Omega_Omcrit)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_v_vCr) = TimeModel%Data_Line(i_v_equa)/TimeModel%Data_Line(i_v_crit1)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_H) = TimeModel%Data_Line(i_H1_Surf)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_He) = TimeModel%Data_Line(i_He4_surf)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_C) = TimeModel%Data_Line(i_C12_surf)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_N) = TimeModel%Data_Line(i_N14_surf)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_O) = TimeModel%Data_Line(i_O16_surf)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_Mass) = TimeModel%Data_Line(i_mass)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_MV) = TimeModel%Additional_Data_Line(i_MV)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_Hcen) = TimeModel%Data_Line(i_H1_cen)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_Mbol) = TimeModel%Additional_Data_Line(i_MBol)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_Mass_ini) = TimeModel%mass_ini
        Time_Step_data(Coord1,Coord2,i,i_tsdata_BV) = TimeModel%Additional_Data_Line(i_BV)
        Time_Step_data(Coord1,Coord2,i,i_tsdata_vsurf) = TimeModel%Data_Line(i_v_equa)
        if (Model%Data_Table(i_TurnOff,i_time) <= time_step_array(i)) then
          if (.not. Found_TurnOff(i,Coord2)) then
            logL_TurnOff(i,Coord2) = Model%Data_Table(i_TurnOff,i_logL)
            MV_TurnOff(i,Coord2) = TimeModel%Additional_Data_Line(i_MV)
            Found_TurnOff(i,Coord2) = .true.
          endif
        endif
      endif
    enddo

    return

  end subroutine Make_Each_Time_step
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Count_Populations
    ! For each time step, sum the quantity of stars in a given type or above a given fraction of the critical velocity.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    use Constant,only: M_sun,L_sun,sL_sun,sigma_SB,pi,G_Newton

    implicit none

    integer, parameter:: HRD_cell_number = 200

    real(kind=8), parameter:: O_type_Limit = 4.47712d0,earlyB_type_Limit = 4.27875d0, &
!      B_type_Limit = 4.d0, A_type_Limit = 3.883, HRD_Teff_min = 0.6d0, &
      B_type_Limit = 4.d0, A_type_Limit = 3.883, &
!      HRD_Teff_min = 0.3d0, &
!      HRD_Teff_max = 4.6d0, HRD_L_min = 0.8d0, HRD_L_max = 5.2d0, HRD_Om_OmCr_Lim = 0.8d0, &
!      HRD_Teff_max = 2.5d0, HRD_L_min = 0.0d0, HRD_L_max = 120.d0, HRD_Om_OmCr_Lim = 0.8d0, &
!      HRD_Teff_max = 2.5d0, HRD_L_min = -11.5d0, HRD_L_max = -4.d0, HRD_Om_OmCr_Lim = 0.8d0, &
!      HRD_Teff_min = 0.d0, HRD_Teff_max = 450.d0, HRD_L_min = 7.75d0, HRD_L_max = 10.75d0, &
      HRD_Om_OmCr_Lim = 0.8d0, &
      RSG_Temp = 3.66d0, YSG_temp = 3.9d0, BSG_temp = 4.4d0, SG_L = 4.0d0, WR_temp = 4.d0, WR_H = 0.3d0, &
      WNL_H = 1.d-5, v_vcrit_max = 0.9171997d0, cepheid_Norm_M_min = 1.7d0, cepheid_Norm_M_max = 3.d0, &
      RSG_L_box1 = 4.5d0,RSG_L_box2 = 5.d0,RSG_L_box3 = 5.5d0,RSG_L_box4 = 6.d0
    real(kind=8):: v_vcrit_Teff,Additional_Var
    real(kind=8), dimension(2):: Delta_Teff, Delta_L
!    real(Kind=8), dimension(2)::HRD_Teff_min = (/0.3d0, -0.2d0/),HRD_Teff_max = (/1.2d0, 0.2d0/), &
!                                HRD_L_min = (/-3.d0, -2.d0/),HRD_L_max = (/0.5d0, 0.5d0/)
!    real(Kind=8), dimension(2)::HRD_Teff_min = (/0.d0, -0.d0/),HRD_Teff_max = (/450.d0, 450.d0/), &
!                                HRD_L_min = (/7.75d0, 7.75d0/),HRD_L_max = (/10.75d0, 10.75d0/)
!    real(Kind=8), dimension(2)::HRD_Teff_min = (/3.55d0, 3.55d0/),HRD_Teff_max = (/3.85d0, 3.85d0/), &
!                                HRD_L_min = (/1.4d0, 1.4d0/),HRD_L_max = (/3.3d0, 3.3d0/)
    real(Kind=8), dimension(2)::HRD_Teff_min = (/3.55d0, 3.55d0/),HRD_Teff_max = (/3.65d0, 3.65d0/), &
                                HRD_L_min = (/3.5d0, 3.5d0/),HRD_L_max = (/5.5d0, 5.5d0/)
    real(kind=8), dimension(HRD_cell_number,HRD_cell_number,2):: HRD_count

    integer:: i,j,k,l
    integer:: Teff_coord, L_coord

    logical:: is_WR

    ! Initialisation of Delta:
    Delta_Teff(:) = (HRD_Teff_max(:)-HRD_Teff_min(:))/dble(HRD_cell_number)
    Delta_L(:) = (HRD_L_max(:)-HRD_L_min(:))/dble(HRD_cell_number)


    do i=1,N_Time_step
      ! At each time step, count the number of various stars.
      ! Number initialised to 0 at each time steps.
      Evolution_Data(i,:) = 0.d0
!      HRD_count1(:,:) = 0.d0
!      HRD_count2(:,:) = 0.d0
      HRD_count(:,:,:) = 0.d0

      ! We also define the turn-off luminosity at each time-step (in case of several rotation velocity, we take the smallest L):
      logL_TurnOff_All = minval(logL_TurnOff(i,:))
      MV_TurnOff_All = maxval(MV_TurnOff(i,:))

      do j=1,Pop_Mass_Beam_Number
        do k=1,Pop_Omega_Beam_Number
          ! First test, to check if the star is still alive !
          is_WR = .false.
          if (Time_Step_data(j,k,i,i_tsdata_Teff) >= 0.d0) then

            ! Sum up the masses to obtain the total remaining mass at the time step i.
            Evolution_Data(i,i_Mtot) = Evolution_Data(i,i_Mtot) + Time_Step_data(j,k,i,i_tsdata_Mass)*Number_of_Star_Beam(j,k)

            ! Compute the Teff-dependant limit to be a "e" kind star according to Cranmer 2005 ApJ 634 585. The function
            ! is cut to 0.9 in v/v_Crit (corresponding to our limitation to omega/omega_crit = 0.99)
            ! Real Cranmer :
            !        v_vcrit_Teff = 0.28d0*tanh(13.3d0-10.d0**Time_Step_data(j,k,i,i_tsdata_Teff)/1500.d0) + 0.72d0
            ! Modified Cranmer (tanh between 0.44 and 0.85)
            !        v_vcrit_Teff = 0.205d0*tanh(13.3d0-10.d0**Time_Step_data(j,k,i,i_tsdata_Teff)/1500.d0) + 0.645d0
            ! Piecewise function Cranmer using mean Vcrit (see table 1):
            !        if (Time_Step_data(j,k,i,i_tsdata_Teff) >= log10(24000.d0)) then
            !          v_vcrit_Teff = 0.5416d0
            !        elseif (Time_Step_data(j,k,i,i_tsdata_Teff) >= log10(20500.d0)) then
            !          v_vcrit_Teff = 0.5756d0
            !        elseif (Time_Step_data(j,k,i,i_tsdata_Teff) >= log10(18500.d0)) then
            !          v_vcrit_Teff = 0.6562d0
            !        elseif (Time_Step_data(j,k,i,i_tsdata_Teff) >= log10(13200.d0)) then
            !          v_vcrit_Teff = 0.7361d0
            !        else
            !          v_vcrit_Teff = 0.9103d0
            !        endif
            ! Piecewise function Cranmer using min Vcrit (see table 1):
            if (Time_Step_data(j,k,i,i_tsdata_Teff) >= log10(24000.d0)) then
              v_vcrit_Teff = 0.4150d0
            elseif (Time_Step_data(j,k,i,i_tsdata_Teff) >= log10(20500.d0)) then
              v_vcrit_Teff = 0.3423d0
            elseif (Time_Step_data(j,k,i,i_tsdata_Teff) >= log10(18500.d0)) then
              v_vcrit_Teff = 0.5050d0
            elseif (Time_Step_data(j,k,i,i_tsdata_Teff) >= log10(13200.d0)) then
              v_vcrit_Teff = 0.7219d0
            else
              v_vcrit_Teff = 0.9084d0
            endif
            ! Mass dependant minimal v/vcrit for Be stars according to Huang et al. 2010 ApJ 722 605
            !if (Time_Step_data(j,k,i,i_tsdata_Mass) >= 8.6d0) then
            !  v_vcrit_Teff = 0.63d0
            !elseif (Time_Step_data(j,k,i,i_tsdata_Mass) >= 5.4d0) then
            !  v_vcrit_Teff = 0.86d0
            !elseif (Time_Step_data(j,k,i,i_tsdata_Mass) >= 4.0d0) then
            !  v_vcrit_Teff = 0.94d0
            !else
            !  v_vcrit_Teff = 0.99d0
            !endif

            v_vcrit_Teff = min(v_vcrit_max,v_vcrit_Teff)

            if (Time_Step_data(j,k,i,i_tsdata_Teff) >= O_type_Limit) then
              Evolution_Data(i,i_OStar) = Evolution_Data(i,i_OStar) + Number_of_Star_Beam(j,k)
              if (Time_Step_data(j,k,i,i_tsdata_v_vCr) > v_vcrit_Teff) then
                Evolution_Data(i,i_Oe) = Evolution_Data(i,i_Oe) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.5d0) then
                Evolution_Data(i,i_O50) = Evolution_Data(i,i_O50) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.7d0) then
                Evolution_Data(i,i_O70) = Evolution_Data(i,i_O70) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.8d0) then
                Evolution_Data(i,i_O80) = Evolution_Data(i,i_O80) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.9d0) then
                Evolution_Data(i,i_O90) = Evolution_Data(i,i_O90) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.95d0) then
                Evolution_Data(i,i_O95) = Evolution_Data(i,i_O95) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.98d0) then
                Evolution_Data(i,i_O98) = Evolution_Data(i,i_O98) + Number_of_Star_Beam(j,k)
              endif
            else if (Time_Step_data(j,k,i,i_tsdata_Teff) >= B_type_Limit) then
              Evolution_Data(i,i_BStar) = Evolution_Data(i,i_BStar) + Number_of_Star_Beam(j,k)
              if (Time_Step_data(j,k,i,i_tsdata_v_vCr) > v_vcrit_Teff) then
                Evolution_Data(i,i_Be) = Evolution_Data(i,i_Be) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.5d0) then
                Evolution_Data(i,i_B50) = Evolution_Data(i,i_B50) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.7d0) then
                Evolution_Data(i,i_B70) = Evolution_Data(i,i_B70) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.8d0) then
                Evolution_Data(i,i_B80) = Evolution_Data(i,i_B80) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.9d0) then
                Evolution_Data(i,i_B90) = Evolution_Data(i,i_B90) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.95d0) then
                Evolution_Data(i,i_B95) = Evolution_Data(i,i_B95) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.98d0) then
                Evolution_Data(i,i_B98) = Evolution_Data(i,i_B98) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Teff) >= earlyB_type_Limit) then
                Evolution_Data(i,i_earlyBStar) = Evolution_Data(i,i_earlyBStar) + Number_of_Star_Beam(j,k)
                if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.8d0) then
                  Evolution_Data(i,i_earlyBStar80) = Evolution_Data(i,i_earlyBStar80) + Number_of_Star_Beam(j,k)
                endif
              endif
            else if (Time_Step_data(j,k,i,i_tsdata_Teff) >= A_type_Limit) then
              Evolution_Data(i,i_AStar) = Evolution_Data(i,i_AStar) + Number_of_Star_Beam(j,k)
              if (Time_Step_data(j,k,i,i_tsdata_v_vCr) > v_vcrit_Teff) then
                Evolution_Data(i,i_Ae) = Evolution_Data(i,i_Ae) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.5d0) then
                Evolution_Data(i,i_A50) = Evolution_Data(i,i_A50) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.7d0) then
                Evolution_Data(i,i_A70) = Evolution_Data(i,i_A70) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.8d0) then
                Evolution_Data(i,i_A80) = Evolution_Data(i,i_A80) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.9d0) then
                Evolution_Data(i,i_A90) = Evolution_Data(i,i_A90) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.95d0) then
                Evolution_Data(i,i_A95) = Evolution_Data(i,i_A95) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.98d0) then
                Evolution_Data(i,i_A98) = Evolution_Data(i,i_A98) + Number_of_Star_Beam(j,k)
              endif
            else
              Evolution_Data(i,i_FStar) = Evolution_Data(i,i_FStar) + Number_of_Star_Beam(j,k)
              if (Time_Step_data(j,k,i,i_tsdata_v_vCr) > v_vcrit_Teff) then
                Evolution_Data(i,i_Fe) = Evolution_Data(i,i_Fe) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.5d0) then
                Evolution_Data(i,i_F50) = Evolution_Data(i,i_F50) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.7d0) then
                Evolution_Data(i,i_F70) = Evolution_Data(i,i_F70) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.8d0) then
                Evolution_Data(i,i_F80) = Evolution_Data(i,i_F80) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.9d0) then
                Evolution_Data(i,i_F90) = Evolution_Data(i,i_F90) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.95d0) then
                Evolution_Data(i,i_F95) = Evolution_Data(i,i_F95) + Number_of_Star_Beam(j,k)
              endif
              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= 0.98d0) then
                Evolution_Data(i,i_F98) = Evolution_Data(i,i_F98) + Number_of_Star_Beam(j,k)
              endif
            endif

            ! MS stars count, 2 mag below TO:
            if (Time_Step_data(j,k,i,i_tsdata_Hcen) >= 1.d-5 .and. &
                Time_Step_data(j,k,i,i_tsdata_MV) < MV_TurnOff_All+2.0d0 .and. &
                Time_Step_data(j,k,i,i_tsdata_MV) >= MV_TurnOff_All) then
              Evolution_Data(i,i_MS2m) = Evolution_Data(i,i_MS2m) + Number_of_Star_Beam(j,k)
            endif

            ! RSG and YSG count:
            if (Time_Step_data(j,k,i,i_tsdata_Teff) <= RSG_Temp .and.&
                Time_Step_data(j,k,i,i_tsdata_L) >= SG_L) then
              Evolution_Data(i,i_RSG) = Evolution_Data(i,i_RSG) + Number_of_Star_Beam(j,k)
            else if (Time_Step_data(j,k,i,i_tsdata_Teff) <= YSG_Temp .and. &
                     Time_Step_data(j,k,i,i_tsdata_L) >= SG_L) then
              Evolution_Data(i,i_YSG) = Evolution_Data(i,i_YSG) + Number_of_Star_Beam(j,k)
            endif

            ! RSG count in several luminosity ranges.
            if (Time_Step_data(j,k,i,i_tsdata_Teff) <= RSG_Temp) then
              if (Time_Step_data(j,k,i,i_tsdata_L) >= SG_L .and. &
                  Time_Step_data(j,k,i,i_tsdata_L) < RSG_L_box1) then
                Evolution_Data(i,i_RSGbox1) = Evolution_Data(i,i_RSGbox1) + Number_of_Star_Beam(j,k)
              else if (Time_Step_data(j,k,i,i_tsdata_L) >= RSG_L_box1 .and. &
                  Time_Step_data(j,k,i,i_tsdata_L) < RSG_L_box2) then
                Evolution_Data(i,i_RSGbox2) = Evolution_Data(i,i_RSGbox2) + Number_of_Star_Beam(j,k)
              else if (Time_Step_data(j,k,i,i_tsdata_L) >= RSG_L_box2 .and. &
                  Time_Step_data(j,k,i,i_tsdata_L) < RSG_L_box3) then
                Evolution_Data(i,i_RSGbox3) = Evolution_Data(i,i_RSGbox3) + Number_of_Star_Beam(j,k)
              else if (Time_Step_data(j,k,i,i_tsdata_L) >= RSG_L_box3 .and. &
                  Time_Step_data(j,k,i,i_tsdata_L) < RSG_L_box4) then
                Evolution_Data(i,i_RSGbox4) = Evolution_Data(i,i_RSGbox4) + Number_of_Star_Beam(j,k)
              endif
            endif

            if (Time_Step_data(j,k,i,i_tsdata_Teff) >= BSG_temp .and.&
                Time_Step_data(j,k,i,i_tsdata_L) >= SG_L) then
              Evolution_Data(i,i_BSG) = Evolution_Data(i,i_BSG) + Number_of_Star_Beam(j,k)
            endif

            ! WR count :
            if (Time_Step_data(j,k,i,i_tsdata_Teff) >= WR_temp .and. &
                Time_Step_data(j,k,i,i_tsdata_H) <= WR_H) then
              is_WR = .true.
              ! WNL
              if (Time_Step_data(j,k,i,i_tsdata_H) > WNL_H) then
                Evolution_Data(i,i_WNL) = Evolution_Data(i,i_WNL) + Number_of_Star_Beam(j,k)
              else
                ! WNE
                if (Time_Step_data(j,k,i,i_tsdata_C) <= Time_Step_data(j,k,i,i_tsdata_N)) then
                  Evolution_Data(i,i_WNE) = Evolution_Data(i,i_WNE) + Number_of_Star_Beam(j,k)
                else
                  ! WC
                  if ((Time_Step_data(j,k,i,i_tsdata_C)/3.d0 + Time_Step_data(j,k,i,i_tsdata_O)/4.d0)/ &
                    Time_Step_data(j,k,i,i_tsdata_He) <= 1.d0) then
                    Evolution_Data(i,i_WC) = Evolution_Data(i,i_WC) + Number_of_Star_Beam(j,k)
                  else
                    ! WO
                    Evolution_Data(i,i_WO) = Evolution_Data(i,i_WO) + Number_of_Star_Beam(j,k)
                  endif
                endif
              endif
            endif

            ! Cepheid count :
            if (Is_a_Cepheid(Time_Step_data(j,k,i,i_tsdata_L),Time_Step_data(j,k,i,i_tsdata_Teff))) then
               Evolution_Data(i,i_Cepheid) = Evolution_Data(i,i_Cepheid) + Number_of_Star_Beam(j,k)
            endif
            ! Cepheid normalisation :
            if (Time_Step_data(j,k,i,i_tsdata_Mass) >= cepheid_Norm_M_min .and. &
                Time_Step_data(j,k,i,i_tsdata_Mass) <= cepheid_Norm_M_max .and. &
                Time_Step_data(j,k,i,i_tsdata_Hcen) >= 1.d-5) then
               Evolution_Data(i,i_Ceph_norm) = Evolution_Data(i,i_Ceph_norm) + Number_of_Star_Beam(j,k)
            endif


            Evolution_Data(i,i_Total) = Evolution_Data(i,i_OStar) + Evolution_Data(i,i_BStar) + &
                                        Evolution_Data(i,i_AStar) + Evolution_Data(i,i_FStar)
            ! Here, we count the number of star with Omega/Omega_crit > Omega_limit in each cell of the HRD.
            if (HRDdensity) then
!              if (Time_Step_data(j,k,i,i_tsdata_Om_OmCr) >= HRD_Om_OmCr_Lim) then
!                Teff_coord = floor((Time_Step_data(j,k,i,i_tsdata_Teff)-HRD_Teff_min)/Delta_Teff)+1
!                L_coord = floor((Time_Step_data(j,k,i,i_tsdata_L)-HRD_L_min)/Delta_L)+1
!                HRD_count(Teff_coord,L_coord) = HRD_count(Teff_coord,L_coord) + Number_of_Star_Beam(j,k)
!              endif
!              Additional_Var = Time_Step_data(j,k,i,i_tsdata_L)+log10(L_sun)-log10(4.d0*pi*sigma_SB*G_Newton)- &
!                     log10(Time_Step_data(j,k,i,i_tsdata_Mass))-log10(M_sun)-sl_sun
              Additional_Var = log10(4.d0*pi*sigma_SB*G_Newton)+log10(Time_Step_data(j,k,i,i_tsdata_Mass)) + &
                       log10(M_sun)-Time_Step_data(j,k,i,i_tsdata_L)-log10(L_sun)+16.d0
!              L_coord = floor((Time_Step_data(j,k,i,i_tsdata_MV)-HRD_L_min)/Delta_L)+1
!              L_coord = floor((Time_Step_data(j,k,i,i_tsdata_Mass)-HRD_L_min)/Delta_L)+1
!              L_coord = floor((Time_Step_data(j,k,i,i_tsdata_L)-HRD_L_min)/Delta_L)+1
!              Teff_coord = floor((Time_Step_data(j,k,i,i_tsdata_Teff)-HRD_Teff_min)/Delta_Teff)+1
!              Teff_coord = floor((Additional_Var-HRD_Teff_min)/Delta_Teff)+1
!              Teff_coord = floor((Time_Step_data(j,k,i,i_tsdata_BV)-HRD_Teff_min)/Delta_Teff)+1
              ! Check that the coordinates are in the right range
!              if (L_coord>0 .and. L_coord<HRD_cell_number .and. Teff_coord>0 .and. Teff_coord<HRD_cell_number) then
                ! Check that the star is a BSG (work with Kudritzki, 3.9 < Teff < 4.4)
                !if (Time_Step_data(j,k,i,i_tsdata_Hcen) < 1.d-5 .and. &
                !    Time_Step_data(j,k,i,i_tsdata_Teff) >= 3.9d0 .and. &
                !    Time_Step_data(j,k,i,i_tsdata_Teff) <= 4.4d0 .and. &
                !    .not. is_WR) then

                ! Check that the star is a RSG
!                if (Time_Step_data(j,k,i,i_tsdata_Teff) <= RSG_Temp) then

                  ! Determines if the star is pre or post-RSG.
                  !if (Teff_min(j,k) <= 3.8d0 .and. time_step_array(i) >= Teff_min_time(j,k)) then
                  !  HRD_count2(Teff_coord,L_coord) = HRD_count2(Teff_coord,L_coord) + Number_of_Star_Beam(j,k)
                  !else
                  !  HRD_count1(Teff_coord,L_coord) = HRD_count1(Teff_coord,L_coord) + Number_of_Star_Beam(j,k)
                  !endif
!                  HRD_count1(Teff_coord,L_coord) = HRD_count1(Teff_coord,L_coord) + Number_of_Star_Beam(j,k)
!                endif
!              endif
!              do l=1,2
              do l=1,1
!                L_coord = floor((Time_Step_data(j,k,i,i_tsdata_MV)-HRD_L_min(l))/Delta_L(l))+1
!                Teff_coord = floor((Time_Step_data(j,k,i,i_tsdata_BV)-HRD_Teff_min(l))/Delta_Teff(l))+1
!                Additional_Var = log10(Time_Step_data(j,k,i,i_tsdata_N)/14.d0)-log10(Time_Step_data(j,k,i,i_tsdata_H))+12.d0
                ! Patrick stuff for RG:
               Teff_coord = floor((Time_Step_data(j,k,i,i_tsdata_Teff)-HRD_Teff_min(l))/Delta_Teff(l))+1
               L_coord = floor((Time_Step_data(j,k,i,i_tsdata_L)-HRD_L_min(l))/Delta_L(l))+1
!               if (l == 1) then
!                    Teff_coord = floor((Time_Step_data(j,k,i,i_tsdata_Teff)-HRD_Teff_min(l))/Delta_Teff(l))+1
!                    L_coord = floor((Time_Step_data(j,k,i,i_tsdata_L)-HRD_L_min(l))/Delta_L(l))+1
!                else
!                    Teff_coord = floor((Time_Step_data(j,k,i,i_tsdata_Mass)-HRD_Teff_min(l))/Delta_Teff(l))+1
!                    L_coord = floor((Time_Step_data(j,k,i,i_tsdata_Om_OmCr)-HRD_L_min(l))/Delta_L(l))+1
!                endif
!                L_coord = floor((Time_Step_data(j,k,i,i_tsdata_Mbol)-HRD_L_min(l))/Delta_L(l))+1
!                L_coord = floor((Additional_Var-HRD_L_min(l))/Delta_L(l))+1
!                Teff_coord = floor((Time_Step_data(j,k,i,i_tsdata_vsurf)-HRD_Teff_min(l))/Delta_Teff(l))+1
              ! Check that the coordinates are in the right range
                if (L_coord>0 .and. L_coord<=HRD_cell_number .and. Teff_coord>0 .and. Teff_coord<=HRD_cell_number) then
!                  if (Time_Step_data(j,k,i,i_tsdata_Hcen) >= 1.d-5) then
!                  if (Time_Step_data(j,k,i,i_tsdata_Hcen) < 1.d-5 .and. &
!                      Time_Step_data(j,k,i,i_tsdata_Teff) >= 3.9d0 .and. &
!                      Time_Step_data(j,k,i,i_tsdata_Teff) <= 4.4d0 .and. &
!                      .not. is_WR) then
!                    if (Teff_min(j,k) <= 3.8d0 .and. time_step_array(i) >= Teff_min_time(j,k)) then
!                    if (Time_Step_data(j,k,i,i_Omega_Omcrit_ini) >= 0.4d0) then
                      HRD_count(Teff_coord,L_coord,1) = HRD_count(Teff_coord,L_coord,1) + Number_of_Star_Beam(j,k)
!                    else
!                      HRD_count(Teff_coord,L_coord,2) = HRD_count(Teff_coord,L_coord,2) + Number_of_Star_Beam(j,k)
!                    endif
!                  endif
                endif
              enddo
            endif
          endif
        enddo
      enddo
      ! Normalisation is done according to the total number of remaining stars at time t.
      if (HRDdensity) then
      !  if (Evolution_Data(i,i_Total) /= 0.d0) then
      !  if (sum(HRD_count(:,:))/= 0.d0) then
      !    HRD_count(:,:) = HRD_count(:,:)/Evolution_Data(i,i_Total)
      !    HRD_count(:,:) = HRD_count(:,:)/sum(HRD_count(:,:))
      !  else
      !    HRD_count(:,:) = 0.d0
      !  endif
!        do j=1,HRD_cell_number
!          do k=1,HRD_cell_number
!            if (HRD_count(j,k) == 0.d0) then
!              HRD_count(j,k) = -20.d0
!            else
!              HRD_count(j,k) = log10(HRD_count(j,k))
!            endif
!          enddo
!        enddo
        Name_Extension = "RSGRotDistri"
        !Name_Extension = "RSG"
        !Name_Extension = "HunterPlot"
        call WriteResultsMovie(HRD_count(:,:,1),HRD_Teff_min(1),HRD_L_min(1), &
                               Delta_Teff(1),Delta_L(1),HRD_cell_number,i,Name_Extension)
        Name_Extension = "HRDm40"
!        Name_Extension = "MSTO"
!        call WriteResultsMovie(HRD_count(:,:,2),HRD_Teff_min(2),HRD_L_min(2), &
!                               Delta_Teff(2),Delta_L(2),HRD_cell_number,i,Name_Extension)
      endif

    enddo

    return

  end subroutine Count_Populations
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  logical function Is_a_Cepheid(L,Teff)
  ! Check if a star at a position (L,Teff) in the HRD is in the instability strip or not.
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8), parameter::Cepheid_Strip_Hot_alpha_Tammann  = -0.048d0, &
                             Cepheid_Strip_Hot_beta_Tammann   =  3.981d0, &
                             Cepheid_Strip_Cold_alpha_Tammann = -0.048d0, &
                             Cepheid_Strip_Cold_beta_Tammann  =  3.821d0, &
                             Cepheid_Strip_Hot_alpha_Bono     = -0.061d0, &
                             Cepheid_Strip_Hot_beta_Bono      =  3.959d0, &
                             Cepheid_Strip_Cold_alpha_Bono    = -0.124d0, &
                             Cepheid_Strip_Cold_beta_Bono     =  4.125d0
    real(kind=8), parameter::cepheid_L_min = 2.5d0

    real(kind=8), intent(in):: L, Teff

    real(kind=8):: Cepheid_Strip_Hot_alpha, Cepheid_Strip_Hot_beta, Cepheid_Strip_Cold_alpha, &
                   Cepheid_Strip_Cold_beta, logTeff_Ceph_min, logTeff_Ceph_max

    ! Initialisation of the Cepheid strip :
!    Cepheid_Strip_Hot_alpha = Cepheid_Strip_Hot_alpha_Tammann
!    Cepheid_Strip_Hot_beta = Cepheid_Strip_Hot_beta_Tammann
!    Cepheid_Strip_Cold_alpha = Cepheid_Strip_Cold_alpha_Tammann
!    Cepheid_Strip_Cold_beta = Cepheid_Strip_Cold_beta_Tammann
    Cepheid_Strip_Hot_alpha = Cepheid_Strip_Hot_alpha_Bono
    Cepheid_Strip_Hot_beta = Cepheid_Strip_Hot_beta_Bono
    Cepheid_Strip_Cold_alpha = Cepheid_Strip_Cold_alpha_Bono
    Cepheid_Strip_Cold_beta = Cepheid_Strip_Cold_beta_Bono

    logTeff_Ceph_min = Cepheid_Strip_Cold_alpha*L + Cepheid_Strip_Cold_beta
    logTeff_Ceph_max = Cepheid_Strip_Hot_alpha*L + Cepheid_Strip_Hot_beta

    if (Teff >= logTeff_Ceph_min .and. Teff <= logTeff_Ceph_max .and. L >= cepheid_L_min) then
      Is_a_Cepheid = .true.
    else
      Is_a_Cepheid = .false.
    endif

  return

  end function Is_a_Cepheid
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine WriteResultsMovie(Matrix,X_inf,Y_inf,Delta_X,Delta_Y,Dimen,time_step,file_extention)
    ! Generate the data for generating the HRD movie.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer, intent(in):: Dimen,time_step
    real(kind=8), intent(in):: X_inf,Y_inf,Delta_X,Delta_Y
    real(kind=8), dimension(Dimen,Dimen), intent(in):: Matrix

    character(8), intent(in):: file_extention

    integer:: i,j
    character(256):: FileNameHRD = '',Number='',WriteFormat=''

    ! Write the X and Y vector (Teff and L) only once.
    if (time_step == 1) then
      open(54, file=trim(adjustl(file_extention))//'_xVector.dat')
      open(55, file=trim(adjustl(file_extention))//'_yVector.dat')
      do i=1,Dimen
        write(54,'(f8.3)') X_inf + dble(i)*Delta_X - Delta_X/2.d0
        write(55,'(f8.3)') Y_inf + dble(i)*Delta_Y - Delta_Y/2.d0
      enddo
      close(54)
      close(55)
    endif

    ! Give a name to the file
    write(Number,'(i5)') time_step
    do i=4,floor(log10(dble(time_step)))+1,-1
      Number = "0"//trim(adjustl(Number))
    enddo
    FileNameHRD = "Matrix"//trim(adjustl(file_extention))//trim(adjustl(Number))//".dat"

    ! Automatic format:
    if (floor(log10(dble(Dimen))) >= 2) then
      write(WriteFormat,'(a,i3,a)') "(es13.4,",Dimen-1,"(2x,es13.4))"
    else
      write(WriteFormat,'(a,i2,a)') "(es13.4,",Dimen-1,"(2x,es13.4))"
    endif

    ! write results in matrix
    open(56,file=FileNameHRD)
    write(56,'(es10.3)') time_step_array(time_step)
    do i=1,Dimen
      write(56,WriteFormat) (Matrix(j,i),j=1,Dimen)
    enddo
    close(56)

    return

  end subroutine WriteResultsMovie
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module Population_Mode
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module random
  ! Random number gestion.
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  implicit none

  real(kind=8), parameter, private:: m0 = 0.01d0,m1 = 0.08d0,m2 = 0.5d0, alpha0 = 0.3d0, &
                                     alpha1 = 1.3d0,alpha2 = 2.3, k0 = 1.d0

  real(kind=8), private:: k1,k2,C0,C1,Phi_NN_Minf,Phi_NN_Msup,Phi1,Phi2

  public:: init_random
  public:: Omega_RandomDraw
  public:: Mass_RandomDraw
  public:: Init_Kroupa_IMF
  public:: Z_RandomDraw
  public:: Binary_RandomDraw
  public:: Binary_Mass_RandomDraw
  public:: AoV_RandomDraw

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine init_random()
    ! Seed initilisation
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer::i,n,clock
    integer, dimension(:),allocatable::seed

    call random_seed(size=n)
    allocate(seed(n))

    call system_clock(count=clock)

    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(put = seed)

    deallocate(seed)

    return

  end subroutine init_random
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Z_RandomDraw(Z)
    ! To date, no several tables in Z. Trivial routine
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only:fixed_metallicity

    implicit none

    real(kind=8),intent(out)::Z

    Z=fixed_metallicity

    return

  end subroutine Z_RandomDraw
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Omega_RandomDraw(distribution_type,mass_in,omega)
    ! Random draw for the angular velocity (according to the choosen distribution)
    ! Note - case (0) : Flat distribution has been tested over a sample of 10^7 draws sucessfuly.
    !      - case (1) : Huang et al. distribution has been tested over a sample of 10^7 draws sucessfuly.
    !      - case (2) : Huang and Giess distribution has been tested over a sample of 10^7 draws sucessfuly.
    !      - case (3) : Trivially tested.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: om_ivdist
    use ReadData, only: B_mass_inf,B_mass_sup,Huang_m_limit,n_Huang,omega_Huang,dist_Huang_1,dist_Huang_2, &
      dist_Huang_3,n_HG,omega_HG,dist_HG,n_ext,omega_ext,dist_ext
    use interpolmod, only: Linear_Interp

    implicit none

    integer, intent(in)::distribution_type         ! Velocity distribution chosen

    real(kind=8), intent(in)::mass_in
    real(kind=8), intent(out)::omega               ! Output random velocity
    real(kind=8)::Random_Draw

    call random_number(Random_Draw)

    ! Select the wanted velocity distribution
    select case (distribution_type)
      case (0) ! Uniform distribution
        omega = Random_Draw
      case (1) ! Huang et al. 2010 distributions
        ! Check if the star is actually a B star
        if (mass_in < B_mass_inf .or. mass_in > B_mass_sup) then
          write(*,'(a,f5.2,a)') 'Current star with M = ',mass_in,' Msun is out of the range of B stars !'
          stop
        endif
        ! Inverting the Huang et al. distribution.
        if (mass_in <= Huang_m_limit(1)) then
          if (Random_Draw < dist_Huang_1(1)) then
            omega = 0.d0
          else
            omega = Linear_Interp(Random_Draw,n_Huang,dist_Huang_1,omega_Huang)
          endif
        else if (mass_in <= Huang_m_limit(2)) then
          if (Random_Draw < dist_Huang_2(1)) then
            omega = 0.d0
          else
            omega = Linear_Interp(Random_Draw,n_Huang,dist_Huang_2,omega_Huang)
          endif
        else
          if (Random_Draw < dist_Huang_3(1)) then
            omega = 0.d0
          else
            omega = Linear_Interp(Random_Draw,n_Huang,dist_Huang_3,omega_Huang)
          endif
        endif
      case (2)
        ! Inverting the Huang and Giess distribution.
        omega = Linear_Interp(Random_Draw,n_HG,dist_HG,omega_HG)
      case (3)
        omega = om_ivdist
      case (4)
        omega = Linear_Interp(Random_Draw,n_ext,dist_ext,omega_ext)
      case default
        write(*,*) 'Unexpected omega determination...'
        stop
    end select

    return

  end subroutine Omega_RandomDraw
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Mass_RandomDraw(IMF,mass)
    ! Random draw for the mass (according to the chosen distribution)
    ! Note - case (1) : Salpeter IMF has been tested over a sample of 10^7 draws sucessfuly.
    ! Note - case (2) : Kroupa IMF has been tested over a sample of 10^5 draws sucessfuly.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only:m_IMF_inf,m_IMF_sup

    implicit none

    integer, intent(in)::IMF                       ! Chosen IMF

    real(kind=8), intent(out)::mass                ! Output random mass
    real(kind=8)::Random_Draw, Const_A, Const_B
    real(kind=8), parameter::Salpeter_Slope = -2.35d0

    call random_number(Random_Draw)

    ! Chose the IMF type
    select case(IMF)
      case (1) ! Salpeter IMF
        Const_A = m_IMF_inf**(Salpeter_Slope + 1.d0)
        Const_B = m_IMF_sup**(Salpeter_Slope + 1.d0) - Const_A
        ! Starting frome a random number between 0 and 1, we reconstruct the initial mass, inverting the IMF.
        mass = (Const_B*Random_Draw+Const_A)**(1.d0/(Salpeter_Slope+1.d0))
      case (2) ! Kroupa IMF
        mass = Get_Kroupa_IMF(Random_Draw)
      case default
        write(*,*) 'Bad IMF type.'
        stop
    end select

    return

  end subroutine Mass_RandomDraw
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Init_Kroupa_IMF()
    ! Initialisation of various quantities related to the Kroupa IMF
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    use VariousParameters, only:m_IMF_inf,m_IMF_sup

    implicit none

    k1 = k0*(m1/m0)**(-alpha0)
    k2 = k1*(m2/m1)**(-alpha1)
    C0 = k0*m0**alpha0*Get_Parenthesis(m0,m1,alpha0)/(-alpha0+1.d0)
    C1 = k1*m1**alpha1*Get_Parenthesis(m1,m2,alpha1)/(-alpha1+1.d0)

    ! Computes terms related to the normalisation of the cumulative distribution function:
    Phi_NN_Minf = Cumulative_Distribution_NotNormalised(m_IMF_inf)
    Phi_NN_Msup = Cumulative_Distribution_NotNormalised(m_IMF_sup)
    Phi1 = Cumulative_Distribution_Normalised(m1)
    Phi2 = Cumulative_Distribution_Normalised(m2)

    return

  end subroutine Init_Kroupa_IMF
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8) function Get_Kroupa_IMF(RD)
    ! Random draw for the mass according to the Kroupa IMF
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8), intent(in):: RD
    real(kind=8):: RN

    RN = RD*(Phi_NN_Msup-Phi_NN_Minf) + Phi_NN_Minf

    if (RD < Phi1) then
        Get_Kroupa_IMF = (RN*(1.d0-alpha0)/(k0*m0**alpha0) + m0**(-alpha0+1.d0))**(1.d0/(-alpha0+1.d0))
    else if (RD < Phi2) then
        Get_Kroupa_IMF = ((RN-C0)*(1.d0-alpha1)/(k1*m1**alpha1) + m1**(-alpha1+1.d0))**(1.d0/(-alpha1+1.d0))
    else
        Get_Kroupa_IMF = ((RN-C0-C1)*(1.d0-alpha2)/(k2*m2**alpha2) + m2**(-alpha2+1.d0))**(1.d0/(-alpha2+1.d0))
    endif

    return

  end function Get_Kroupa_IMF
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8)  function Get_Parenthesis(Mlow,Mhigh,alpha)
    ! Computes (Mhigh**(-alpha + 1) - Mlow**(-alpha + 1))
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8), intent(in):: Mlow,Mhigh,alpha

    Get_Parenthesis = Mhigh**(-alpha+1.d0) - Mlow**(-alpha+1.d0)

    return

  end function Get_Parenthesis
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8)  function Cumulative_Distribution_NotNormalised(M)
    ! Computes the non-normalised cumulative distribution function for the Kroupa IMF,
    ! from m0 to M.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8), intent(in):: M

    if (M < m1) then
      Cumulative_Distribution_NotNormalised = k0*m0**alpha0*Get_Parenthesis(m0,M,alpha0)/(-alpha0+1.d0)
    else if (M < m2) then
      Cumulative_Distribution_NotNormalised = C0 + k1*m1**alpha1*Get_Parenthesis(m1,M,alpha1)/(-alpha1+1.d0)
    else
      Cumulative_Distribution_NotNormalised = C0 + C1 + k2*m2**alpha2*Get_Parenthesis(m2,M,alpha2)/(-alpha2+1.d0)
    endif

    return

  end function Cumulative_Distribution_NotNormalised
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8)  function Cumulative_Distribution_Normalised(M)
    ! Computes the normalised cumulative distribution function for the Kroupa IMF,
    ! from 0 to 1.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8), intent(in):: M

    Cumulative_Distribution_Normalised = &
        (Cumulative_Distribution_NotNormalised(M)-Phi_NN_Minf)/(Phi_NN_Msup-Phi_NN_Minf)

    return

  end function Cumulative_Distribution_Normalised
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Binary_RandomDraw(Is_Binary)
    ! Random draw for the binarity probability
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only:binary_prob

    implicit none

    integer, intent(out):: Is_Binary

    real(kind=8)::Random_Draw

    call random_number(Random_Draw)

    ! Chose the IMF type
    if (Random_Draw < binary_prob) then
      Is_Binary = 1
    else
      Is_Binary = 0
    endif

    return

  end subroutine Binary_RandomDraw
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8) function Binary_Mass_RandomDraw(mass_ini)
    ! Random draw for the secondary mass. The mass is assumed having a flat distribution between 0.08 Msun and the
    ! mass of the primary.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8), intent(in):: mass_ini
    real(kind=8), parameter:: Minimal_star_Mass = 0.08d0
    real(kind=8)::Random_Draw

    call random_number(Random_Draw)

    Binary_Mass_RandomDraw = Minimal_star_Mass + Random_Draw*(mass_ini - Minimal_star_Mass)

    return

  end function Binary_Mass_RandomDraw
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine AoV_RandomDraw(angle)
    ! Random draw of the angle of view, according to the wanted distribution.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only:iangle,Fixed_AoV
    use constant, only:pi
    use interpolmod, only: Linear_Interp
    use ReadData,only: n_angle_ext,angle_ext,angle_dist_ext

    implicit none

    real(kind=8), intent(out):: angle
    real(kind=8):: Random_Draw,AoV

    call random_number(Random_Draw)

    select case (iangle)
      case (1)
        ! Case of a flat distribution of the angle of view. Quite artificial.
        angle = Random_Draw*pi/2.d0
      case (2)
        ! Case of a sin(i) distribution of the AoV. This is the real observed distribution if the AoV are randomly
        !  distributed in space. (the sin(i) comes from the projection on the sky).
        angle = asin(Random_Draw)
      case (3)
        ! Case of a Dirac distribution.
        angle = Fixed_AoV
      case (4)
        ! Case of external file for angle distribution
        AoV = Linear_Interp(Random_Draw,n_angle_ext,angle_dist_ext,angle_ext)
        angle = pi/2.d0-AoV*(pi/180.d0)
      case default
        write(*,*) 'Unexpected angle distribution...'
        stop
    end select

    return

  end subroutine AoV_RandomDraw
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module random
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module InOut
  ! Module containing the in- and out-subroutines
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  implicit none

  private::ChangeParam
  private::ClusterParameters
  public:: Intro
  public:: AskChange
  public:: IsochroneMode
  public:: BePopulationMode
  public:: InitialiseData
  public:: WriteResults
  private:: SingleModelMode

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Intro
    ! Introduction message and parameter printing
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only:grid,m_IMF_inf,m_IMF_sup,i_Metallicity,fixed_metallicity,ivdist,om_ivdist,iangle, &
      Fixed_AoV_latitude,grav_dark,limb_dark,binary_prob,inoise,sigma_mv,sigma_bv,Colour_Calibration_mode,PMS,&
      table_format,IMF_type

    implicit none

    write(*,*)
    write(*,*) '**********************************************'
    write(*,*) 'SYCLIST program to build synthetic clusters'
    write(*,*) 'To use with the .wg.grids table'
    write(*,*) 'New version, completely modulised'
    write(*,*) ''
    write(*,*) 'Cyril Georgy, Aurelien Wyttenbach,'
    write(*,*) 'Anahi Granada & Sylvia Ekstrom'
    write(*,*) 'Last Version : March 25 2019'
    write(*,*) '**********************************************'
    write(*,*)
    write(*,*) '**********************************************'
    write(*,*) 'The defaults parameters are :'
    if (.not. PMS) then
       write(*,*) 'Grids: ',grid, 'without pre-MS'
    else
       write(*,*) 'Grids: ',grid, 'with pre-MS'
    endif
    if (table_format == 1) then
       write(*,*) 'Tables are in GENEC format.'
    else if (table_format == 2) then
       write(*,*) 'Tables are in Starevol format.'
    else
       write(*,*) 'Problems with the table format, should be 1 or 2.'
    endif
    select case (IMF_type)
      case (1)
        write(*,'(a,f6.2,a,f6.2,a)') ' -Salpeter IMF between ', m_IMF_inf,' and ',m_IMF_sup,' solar masses'
      case (2)
        write(*,'(a,f6.2,a,f6.2,a)') ' -Kroupa IMF between ', m_IMF_inf,' and ',m_IMF_sup,' solar masses'
      case default
        write(*,*) 'Bad choice of IMF tyle, should not occur'
        stop
      end select
    write(*,*) 'Metallicity distribution :'
    select case (i_metallicity)
      case (0)
        write(*,'(a,f6.4)') '  delta distribution at Z=', fixed_metallicity
    end select

    write(*,'(a,i2)') ' -distribution in angular velocity:  ivdist=',ivdist

    select case (ivdist)
      case(0)
        write(*,*) ' (uniform distribution)'
      case(1)
        write(*,*) ' (distribution of Huang 2010)'
      case(2)
        write(*,*) ' (distribution of Huang-Gies 2006)'
      case(3)
        write(*,'(a,f4.2)') '  delta distribution at omega=',om_ivdist
      case(4)
        write(*,*) ' (distribution read from external file)'
      case default
        write(*,*) 'Bad ivdist value, aborting...'
        stop
    end select

    write(*,'(a,i2)') ' -distribution of the angle of view: iangle=',iangle
    select case (iangle)
      case(0)
        write(*,*) ' (angle of view not taken into account)'
      case(1)
        write(*,*) ' (uniform distribution)'
      case(2)
        write(*,*) ' (vsini distribution)'
      case(3)
        write(*,'(a,f5.2)') '  (delta distribution at angle=)',Fixed_AoV_latitude
      case(4)
        write(*,*) ' (distribution read from external file)'
      case default
        write(*,*) 'Bad iangle value, aborting...'
        stop
    end select

    if (iangle /= 0) then
      write(*,'(a,i2)') ' -gravity darkening law: grav_dark=',grav_dark
      select case (grav_dark)
        case (1)
          write(*,*) ' (gravity darkening according to von Zeipel 1924)'
        case (2)
          write(*,*) ' (gravity darkening according to Espinosa-Lara & Rieutord 2011)'
        case default
          write(*,*) 'Bad grav_dark value, aborting...'
          stop
      end select

      select case (limb_dark)
        case (0)
          write(*,'(a,i2)') ' -limb darkening not taken into account'
        case (1)
          write(*,'(a,i2)') ' -limb darkening taken into account (Claret 2010)'
        case default
          write(*,*) 'Bad limb darkening value, aborting...'
          stop
      end select
    endif

    write(*,'(a,f5.2)') ' -probability to have a binary system =', binary_prob

    write(*,*) 'Colour - T_eff calibration:'
    select case (Colour_Calibration_mode)
      case(1)
        write(*,*) 'Old calibrations, as in Paper I of grids 2011'
      case(2)
        write(*,*) 'Calibration of Worthey & Lee, ApJS 193 1 (2011)'
    end select

    select case (inoise)
      case (0)
        write(*,*) '-no noise'
      case (1)
        write(*,'(a,f7.4,a,f7.4)') ' -noise: sigma_Mv=',sigma_mv,' and sigma_B-V=',sigma_bv
      case default
        write(*,*) 'Bad inoise value, aborting...'
        stop
    end select

    write(*,*) '**********************************************'
    write(*,*) 'You can change the defaults parameters in the Main code'
    write(*,*)

    return

  end subroutine Intro
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine AskChange
    ! Ask if parameters are changed or not.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: Comp_Mode,table_format,ivdist
    use ReadData, only: init_external

    implicit none

    character(1)::answer

    answer = ''
    do while (answer /= 'y' .and. answer /= 'n')
      write(*,*) 'Do you want to change some parameters ? (y)es (n)o '
      read(5,*) answer
      if (answer /= 'y' .and. answer /= 'n') then
        write(*,*) 'Bad choice, try again...'
      endif
    enddo

    if (answer == 'y') then
      call ChangeParam
    endif

    write(*,*)
    write(*,*) 'Which computation mode do you want :'
    do while (Comp_Mode < 1 .or. Comp_Mode > 4)
      write(*,*) '(1) Computation of a stellar cluster'
      write(*,*) '(2) Computation of isochrones'
      write(*,*) '(3) Computation of a stellar population as a function of time'
      write(*,*) '(4) Computation of a single stellar model'
      read(5,*) Comp_Mode
      ! Starevol format not yet usable with population mode.
      if (Comp_Mode == 3 .and. table_format == 2) then
        write(*,*) 'This mode is not yet available for starevol formats. Chose another one'
        Comp_Mode = -1
      endif
    enddo
    
    if (ivdist == 4) then
      call init_external
    endif


    select case (Comp_Mode)
      case (1,2)
        call ClusterParameters
      case (3)
        call BePopulationMode
      case (4)
        call SingleModelMode
      case default
        write(*,*) 'Unexpected error...'
        stop
    end select

    return

  end subroutine AskChange
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine ChangeParam
    ! Change the required parameters.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only:grid,star_number,m_IMF_inf,m_IMF_sup,m_IMF_inf_Grids2012, &
      m_IMF_sup_Grids2012,m_IMF_inf_BeGrids,m_IMF_sup_BeGrids,ivdist,om_ivdist,iangle, &
      Fixed_AoV_latitude,binary_prob,inoise,IMF_type,sigma_mv,sigma_bv,fixed_metallicity, &
      Colour_Calibration_mode,grav_dark,limb_dark,PMS,table_format

    implicit none

    integer::Change_Param,Temp_Var_Int

    real(kind=8)::Temp_Var_real

    character(9):: Temp_Var_char

    Change_Param=100
    do while(Change_Param /= 0)
      write(*,*)
      write(*,*) 'Enter a parameter number, (0) to stop'
      write(*,*)
      write(*,*) 'Parameters you can change:'
      write(*,'(a,a)') '1. Grid                                     ',grid
      write(*,'(a,i1)') '2. Format of the tables                          ',table_format
      write(*,'(a,l)') '3. Tables with PMS                               ',PMS
      write(*,'(a,i8)') '4. maximum number of star in the cluster  ',star_number
      write(*,'(a,i5)') '5. IMF type                                  ',IMF_type
      write(*,'(a,f6.2)') '6. minimum mass for IMF                     ',m_IMF_inf
      write(*,'(a,f6.2)') '7. maximum mass for IMF                     ',m_IMF_sup
      write(*,'(a,f6.4)') '8. metallicity                              ',fixed_metallicity
      write(*,'(a,i5)') '9. angular velocity distribution             ',ivdist
      write(*,'(a,f5.2)') '10. special angular velocity (ivdist=3)      ',om_ivdist
      write(*,'(a,i5)') '11. angle of view distribution               ',iangle
      write(*,'(a,f5.2)') '12. special angle of view (iangle=3)         ',Fixed_AoV_latitude
      write(*,'(a,f5.2)') '13. probability of binarity                  ',binary_prob
      write(*,'(a,i5)') '14. Colour - Teff calibration                ',Colour_Calibration_mode
      write(*,'(a,i5)') '15. noise                                    ',inoise
      write(*,'(a,f5.3)') '16. sigma in M_V                             ',sigma_mv
      write(*,'(a,f5.3)') '17. sigma in B-V                             ',sigma_bv
      write(*,'(a,i5)') '18. Gravity Darkening                        ',grav_dark
      write(*,'(a,i5)') '19. Limb Darkening                           ',limb_dark
      read(*,*) Change_Param
      select case(Change_Param)
        case(0)
          write(*,*) 'Stop changing parameters'
        case(1)
          Temp_Var_char=''
          do while (Temp_Var_char /= 'Grids2012' .and. Temp_Var_char /= 'BeGrids  ' &
                     .and. Temp_Var_char /= 'FastBe  ' .and. Temp_Var_char /= 'NamiGrids ' .and. Temp_Var_char /= 'fromFile ')
            write(*,*) 'Choice for the grids ? Grids2012, BeGrids, FastBe, fromFile.'
            read(*,*) Temp_Var_char
            if (Temp_Var_char /= 'Grids2012' .and. Temp_Var_char /= 'BeGrids  ' &
                 .and. Temp_Var_char /= 'FastBe  ' .and. Temp_Var_char /= 'NamiGrids ' .and. Temp_Var_char /= 'fromFile ') then
              write(*,*) 'Please enter Grids2012, BeGrids, FastBe, NamiGrids, or fromFile.'
            endif
          enddo
          grid = Temp_Var_char
          if (grid == "Grids2012" .and. m_IMF_inf /= m_IMF_inf_Grids2012) then
            write(*,*) "Minimal IMF mass reset to ", m_IMF_inf_Grids2012," !"
            m_IMF_inf = m_IMF_inf_Grids2012
          endif
          if (grid == "Grids2012" .and. m_IMF_sup /= m_IMF_sup_Grids2012) then
            write(*,*) "Maximal IMF mass reset to ", m_IMF_sup_Grids2012," !"
             m_IMF_sup = m_IMF_sup_Grids2012
          endif
          if ((grid == "BeGrids" .or.grid == "FastBe") .and. m_IMF_inf < m_IMF_inf_BeGrids) then
            write(*,*) "Minimal IMF mass too low for this grid, reset to ", m_IMF_inf_BeGrids," !"
            m_IMF_inf = m_IMF_inf_BeGrids
          endif
          if ((grid == "BeGrids" .or.grid == "FastBe") .and. m_IMF_sup > m_IMF_sup_BeGrids) then
            write(*,*) "Maximal IMF mass too high for this grid, reset to ", m_IMF_sup_BeGrids," !"
             m_IMF_sup = m_IMF_sup_BeGrids
          endif
        case(2)
          Temp_Var_Int=10
          do while (Temp_Var_Int /= 1 .and. Temp_Var_Int /= 2)
            write(*,*) 'What is the wanted format for the tables?'
            write(*,*) '1. GENEC format'
            write(*,*) '2. Starevol format'
            read(5,*) Temp_Var_Int
            if (Temp_Var_Int /= 1 .and. Temp_Var_Int /= 2) then
              write(*,*) 'Please enter 1 or 2.'
            endif
          enddo
          table_format=Temp_Var_Int
        case(3)
          Temp_Var_Int=10
          do while (Temp_Var_Int /= 0 .and. Temp_Var_Int /= 1)
            write(*,*) 'Tables with PMS ? (1) yes (0) no.'
            read(*,*) Temp_Var_Int
            if(Temp_Var_Int /= 0 .and. Temp_Var_Int /= 1) then
              write(*,*) 'Please enter (0) no PMS (1) with PMS.'
            endif
          enddo
          if (Temp_Var_Int == 1) then
            PMS = .true.
          else
            PMS = .false.
          endif
        case(4)
          write(*,*) 'Number of stars in the synthetic cluster:'
          read(*,*) star_number
        case(5)
          Temp_Var_Int=10
          do while (Temp_Var_Int /= 1 .and. Temp_Var_Int /= 2)
            write(*,*) 'What do you want for the IMF?'
            write(*,*) '1. Salpeter IMF'
            write(*,*) '2. Kroupa IMF'
            read(5,*) Temp_Var_Int
            if (Temp_Var_Int /= 1 .and. Temp_Var_Int /= 2) then
              write(*,*) 'Please enter 1 or 2.'
            endif
          enddo
          IMF_type=Temp_Var_Int
        case(6)
          write(*,*) 'What do you want for minimal IMF mass ?'
          read(*,*) m_IMF_inf
          if (grid == "Grids2012" .and. m_IMF_inf < m_IMF_inf_Grids2012) then
            write(*,*) "Minimal IMF mass too low for this grid, reset to ", m_IMF_inf_Grids2012," !"
            m_IMF_inf = m_IMF_inf_Grids2012
          endif
          if ((grid == "BeGrids" .or.grid == "FastBe") .and. m_IMF_inf < m_IMF_inf_BeGrids) then
            write(*,*) "Minimal IMF mass too low for this grid, reset to ", m_IMF_inf_BeGrids," !"
            m_IMF_inf = m_IMF_inf_BeGrids
          endif
        case(7)
          write(*,*) 'What do you want for maximal IMF mass ?'
          read(*,*) m_IMF_sup
          if (grid == "Grids2012" .and. m_IMF_sup > m_IMF_sup_Grids2012) then
            write(*,*) "Maximal IMF mass too high for this grid, reset to ", m_IMF_sup_Grids2012," !"
             m_IMF_sup = m_IMF_sup_Grids2012
          endif
          if ((grid == "BeGrids" .or.grid == "FastBe") .and. m_IMF_sup > m_IMF_sup_BeGrids) then
            write(*,*) "Maximal IMF mass too high for this grid, reset to ", m_IMF_sup_BeGrids," !"
             m_IMF_sup = m_IMF_sup_BeGrids
          endif
        case(8)
          Temp_Var_real=-2.d0
          do while (Temp_Var_real < 0.d0)
            write(*,*) 'Value for the Dirac metallicity distribution :'
            read(*,*) Temp_Var_real
            if(Temp_Var_real < 0.d0) then
              write(*,*) 'Please enter a positive number.'
            endif
          enddo
          fixed_metallicity=Temp_Var_real
        case(9)
          Temp_Var_Int=10
          do while (Temp_Var_Int /= 0 .and. Temp_Var_Int /= 1 .and. Temp_Var_Int /= 2 .and. &
            Temp_Var_Int /= 3 .and. Temp_Var_Int /= 4)
            write(*,*) 'What do you want for the velocity distribution?'
            write(*,*) '0. uniform'
            write(*,*) '1. Huang10'
            write(*,*) '2. Huang-Gies06'
            write(*,*) '3. Dirac (see parameter 10)'
            write(*,*) '4. external file'
            read(*,*) Temp_Var_Int
            if (Temp_Var_Int /= 0 .and. Temp_Var_Int /= 1 .and. Temp_Var_Int /=2 .and. &
              Temp_Var_Int /= 3 .and. Temp_Var_Int /= 4) then
              write(*,*) 'Please enter 0,1,2,3, or 4.'
            endif
          enddo
          ivdist=Temp_Var_Int
        case(10)
          Temp_Var_real=2.d0
          do while (Temp_Var_real > 1.d0 .or. Temp_Var_real < 0.d0)
            write(*,*) 'Value for the Dirac velocity distribution :'
            read(*,*) Temp_Var_real
            if(Temp_Var_real > 1.d0 .or. Temp_Var_real < 0.d0) then
              write(*,*) 'Please enter a number in [0,1].'
            endif
          enddo
          om_ivdist=Temp_Var_real
        case(11)
          Temp_Var_Int=10
          do while (Temp_Var_Int /= 0 .and. Temp_Var_Int /= 1 .and. Temp_Var_Int /=2 .and. &
            Temp_Var_Int /= 3 .and. Temp_Var_Int /= 4)
            write(*,*) 'What do you want for the angle of view distribution ?'
            write(*,*) '0. not taken in account'
            write(*,*) '1. uniform'
            write(*,*) '2. sini'
            write(*,*) '3. Dirac (see parameter 7)'
            write(*,*) '4. external file'
            read(*,*) Temp_Var_Int
            if(Temp_Var_Int /= 0 .and. Temp_Var_Int /= 1 .and. Temp_Var_Int /= 2 .and. &
              Temp_Var_Int /= 3 .and. Temp_Var_Int /= 4) then
              write(*,*) 'Please enter 0,1,2,3, or 4.'
            endif
          enddo
          iangle=Temp_Var_Int
        case(12)
          Temp_Var_real=100.d0
          do while (Temp_Var_real > 90.d0 .or. Temp_Var_real < 0.d0)
            write(*,*) 'Angle of view of the Dirac distribution:'
            read(*,*) Temp_Var_real
            if(Temp_Var_real > 90.d0 .or. Temp_Var_real < 0.d0) then
              write(*,*) 'Please enter a number in [0,90].'
            endif
          enddo
          Fixed_AoV_latitude=Temp_Var_real
        case(13)
          Temp_Var_real=2.d0
          do while (Temp_Var_real > 1.d0 .or. Temp_Var_real < 0.d0)
            write(*,*) 'What do you want for binary probability ?'
            read(*,*) Temp_Var_real
            if(Temp_Var_real > 1.d0 .or. Temp_Var_real < 0.d0) then
              write(*,*) 'Please enter a number in [0,1].'
            endif
          enddo
          binary_prob=Temp_Var_real
        case(14)
          Temp_Var_Int=10
          do while (Temp_Var_Int /= 1 .and. Temp_Var_Int /= 2)
            write(*,*) 'Old calibration (grids 2011 paper I) (1) or Worthey & Lee, ApJS 193 1 (2011) (2) ?'
            read(*,*) Temp_Var_Int
            if(Temp_Var_Int /= 1 .and. Temp_Var_Int /= 2) then
              write(*,*) 'Please enter (1) old calibration (1) or Worthey & Lee, ApJS 193 1 (2011) (2).'
            endif
          enddo
          Colour_Calibration_mode=Temp_Var_Int
        case(15)
          Temp_Var_Int=10
          do while (Temp_Var_Int /= 0 .and. Temp_Var_Int /= 1)
            write(*,*) 'Add noise ? (1) yes (0) no.'
            read(*,*) Temp_Var_Int
            if(Temp_Var_Int /= 0 .and. Temp_Var_Int /= 1) then
              write(*,*) 'Please enter (0) no noise (1) noise.'
            endif
          enddo
          inoise=Temp_Var_Int
        case(16)
          Temp_Var_real=-1.d0
          do while (Temp_Var_real < 0.d0)
            write(*,*) 'What do you want for sigma M_V ?'
            read(*,*) Temp_Var_real
            if(Temp_Var_real < 0.d0) then
              write(*,*) 'Please enter a number bigger than 0.'
            endif
          enddo
          sigma_mv=Temp_Var_real
        case(17)
          Temp_Var_real=-1.d0
          do while (Temp_Var_real < 0.d0)
            write(*,*) 'What do you want for sigma B-V'
            read(*,*) Temp_Var_real
            if(Temp_Var_real < 0.d0) then
              write(*,*) 'Please enter a number bigger than 0.'
            endif
          enddo
          sigma_bv=Temp_Var_real
        case(18)
          Temp_Var_Int=10
          do while (Temp_Var_Int /= 1 .and. Temp_Var_Int /= 2)
            write(*,*) 'Gravity Darkening Correction ? (1) von Zeipel 1924 (2) Espinosa-Lara & Rieutord 2011.'
            read(*,*) Temp_Var_Int
            if(Temp_Var_Int /= 1 .and. Temp_Var_Int /= 2) then
              write(*,*) 'Please enter (1) von Zeipel (2) Espinosa-Lara & Rieutord.'
            endif
          enddo
          grav_dark = Temp_Var_Int
        case(19)
          Temp_Var_Int=10
          do while (Temp_Var_Int /= 0 .and. Temp_Var_Int /= 1)
            write(*,*) 'Limb Darkening Correction ? (1) yes (0) no.'
            read(*,*) Temp_Var_Int
            if(Temp_Var_Int /= 0 .and. Temp_Var_Int /= 1) then
              write(*,*) 'Please enter (0) no LD (1) with LD.'
            endif
          enddo
          limb_dark = Temp_Var_Int
        case default
          write(*,*) 'Bad choice'
          stop
      end select
    enddo

    return

  end subroutine ChangeParam
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine IsochroneMode
    ! Initialise the isochrone mode
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: ivdist,iangle,inoise

    implicit none

    ivdist = 3
    iangle = 0
    inoise = 0

    return

  end subroutine IsochroneMode
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine BePopulationMode
    ! Initialise the isochrone mode
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: iangle,inoise,star_number
    use Population_Mode, only: Pop_Mass_Beam_Number,Pop_Omega_Beam_Number,Number_of_Star_Beam,Time_Step_data, &
      N_Time_step,Final_Time,Initial_Time,time_step_array,Time_Step_data_Number, &
      Initial_Population,HRDdensity

    implicit none

    integer::i

    real(kind=8)::dt

    character(1)::answer,density_answer

    iangle = 0
    inoise = 0

    ! Check the wanted mass and omega beam number
    write(*,*) 'The current setting are:'
    write(*,'(a,i6)') '     number of mass beams    : ', Pop_Mass_Beam_Number
    write(*,'(a,i6)') '     number of velocity beams: ', Pop_Omega_Beam_Number
    write(*,'(a,i6)') '     number of timesteps     : ', N_Time_step
    write(*,'(a,l)')  '     HRD density plot outputs: ', HRDdensity
    write(*,*)
    answer = ''
    density_answer = 'X'
    do while (answer /= 'y' .and. answer /= 'n')
      write(*,*) 'Would you like to change ? (y)es (n)o'
      read(5,*) answer
      if (answer /= 'y' .and. answer /= 'n') then
        write(*,*) 'Bad choice, try again...'
      endif
    enddo
    if (answer == 'y') then
      write(*,*) 'New value for the number of mass beams :'
      read(5,*) Pop_Mass_Beam_Number
      write(*,*) 'New value for the number of velocity beams :'
      read(5,*) Pop_Omega_Beam_Number
      write(*,*) 'New value for the number of timesteps :'
      read(5,*) N_Time_step
      do while (density_answer /= 'T' .and. density_answer /= 'F' .and. density_answer /= '')
       write(*,*) 'Outputs for HRD density plot ?  (T)rue (F)alse (default F)'
       read(5,*) density_answer
       if (density_answer == '' .or. density_answer == 'F') then
         HRDdensity = .false.
       elseif (density_answer == 'T') then
         HRDdensity = .true.
       else
         write(*,*) 'Bad choice, should be T, F, or empty'
       endif
      enddo
    endif

    ! Allocation of the memory
    allocate(Number_of_Star_Beam(Pop_Mass_Beam_Number,Pop_Mass_Beam_Number))
    allocate(Time_Step_data(Pop_Mass_Beam_Number,Pop_Omega_Beam_Number,N_Time_step,Time_Step_data_Number))
    allocate(time_step_array(N_Time_step))

    ! Number of stars to compute
    star_number = Pop_Mass_Beam_Number*Pop_Omega_Beam_Number

    ! Time discretisation for the population.
    ! The time steps are log-discretised for standard population mode, and linear if density plots
    ! are generated
    if (HRDdensity) then
      write(*,*) 'Enter the initial time :'
    else
      write(*,*) 'Enter the initial time (must be different of zero, log discretisation !) :'
    endif
    read(*,*) Initial_Time
    Final_Time = -1.d0
    if (N_Time_step /= 1) then
      do while (Final_Time <= Initial_Time)
        write(*,*) 'Enter the final time :'
        read(*,*) Final_Time
        if (Final_Time <= Initial_Time) then
          write(*,*) 'Final time must be greater than initial one !'
        endif
      enddo
    endif
! For density plots, need for linear time spacing
    if (.not. HRDdensity) then
      Initial_Time = log10(Initial_Time)
      Final_Time = log10(Final_Time)
    endif

    dt = 0.d0
    if (N_Time_step /= 1) then
      dt = (Final_Time-Initial_Time)/dble(N_Time_step-1)
    endif

    time_step_array(1) = Initial_Time
    if (N_Time_step /= 1) then
      do i=2,N_Time_step-1
        time_step_array(i) = Initial_Time +dble(i-1)*dt
      enddo
      time_step_array(N_Time_step) = Final_Time
    endif
    if (.not. HRDdensity) then
      time_step_array(:) = 10.d0**time_step_array(:)
    endif

    ! Compute the initial population according to the current IMF and velocity distribution
    call Initial_Population

    return

  end subroutine BePopulationMode
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine SingleModelMode
    ! Initialise the isochrone mode
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: Table_Line_Number
    use VariousParameters, only: inoise,iangle,star_number

    implicit none

    inoise = 0
    if (iangle /= 3) then
      iangle = 0
    endif

    ! Number of stars to compute
    star_number = Table_Line_Number

    return

  end subroutine SingleModelMode
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine ClusterParameters
    ! Main parameters of the simulation.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: log_age_max,age_log,star_number,m_IMF_inf,m_IMF_sup,Comp_Mode

    implicit none

    age_log=log_age_max+1.d0

    ! Cluster age.
    write(*,*)
    do while (age_log >= log_age_max)
      write(*,*) 'Enter log(age) : '
      read(*,*) age_log
      if(age_log >= log_age_max) then
        write(*,*) 'age too big!'
        write(*,'(a,f5.2)') 'Please enter an age less than ',log_age_max
      endif
    enddo

    return

  end subroutine ClusterParameters
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine InitialiseData
    ! Search tables file, and treat the data in order to store them in RAM.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: Std_Path,Std_Path_tables,ListFileGrids2012,ListFileBeGrids,ListFileFastBe, &
      Z_Number,All_Data_Array,Z_List,mass_List,omega_list,mass_Number_array,omega_Number_array,grid
    use DataStructure, only: FillData,verbose,Init_DataStructure,Table_Line_Number,Data_Number

    implicit none

    integer, parameter::ReadUnit=20,Z_Length=2,mass_Length=6,omega_Length=2
    integer::error,FileNumber,i,j,k,Begin_in_String
    integer::omega_ini_int,M_ini_int,Z_ini_int
    integer::mass_Number,omega_Number

    real(kind=8)::omega_initial,M_initial,Z_initial

    character(512)::FullPath,table_name
    character(Z_Length)::Z_String,Z_String_prev
    character(mass_Length)::mass_String,mass_String_prev
    character(omega_Length)::omega_String,omega_String_prev
    character(*),parameter::Z_Pos='Z',mass_Pos='/M',omega_Pos='V'
    character(512):: Current_GridFile

    select case (grid)
      case ("Grids2012")
        Current_GridFile = trim(Std_Path)//trim(ListFileGrids2012)
      case ("BeGrids  ")
        Current_GridFile = trim(Std_Path)//trim(ListFileBeGrids)
      case ("FastBe  ")
        Current_GridFile = trim(Std_Path)//trim(ListFileFastBe)
      case ("fromFile ")
        write(*,*) 'Enter the name of the grid file (can be a full path):'
        read(5,*) Current_GridFile
    end select

    open(unit=ReadUnit,file=trim(Current_GridFile),iostat=error,status='old')
    if (error /= 0) then
      write(*,*) 'Unable to open file ',trim(Current_GridFile),'. Check your choice under "Grid" !'
      stop
    else
      write(*,*)
      if (verbose) then
        write(*,*) 'File ',trim(Current_GridFile),' opened sucessfully.'
        write(*,*) 'Be careful: in this file models must be sorted, first by increasing metallicity,'
        write(*,*) 'then by increasing mass and finally by increasing velocity in order for this'
        write(*,*) 'program to work ! The name of the models must mandatorily be under'
        write(*,*) 'the form MXXXpXXZYYVAA, with XXXpXX the mass, YY the metallicity and'
        write(*,*) 'AA the initial velocity (eg. M001p40Z14V00 or M060p00Z02V40)'
      endif
      write(*,*)
    endif

    ! Count the number of files.
    FileNumber = 0
    do while (error == 0)
      read(ReadUnit,*,iostat=error)
      if (error /= 0) then
        exit
      endif
      FileNumber = FileNumber + 1
    enddo

    ! Count the number of metallicities, masses and velocities.
    rewind(ReadUnit)
    Z_Number = 0
    Z_String_prev = ''
    do i=1,FileNumber
      read(ReadUnit,'(a)',iostat=error) FullPath
      Begin_in_String = index(FullPath,Z_Pos,.true.)
      Z_String = FullPath(Begin_in_String+1:Begin_in_String+1+Z_Length)
      if (Z_String /= Z_String_prev) then
        Z_Number = Z_Number + 1
      endif
      Z_String_prev = Z_String
    enddo

    allocate(mass_Number_array(Z_Number))

    ! For each metallicity, counts the number of masses.
    rewind(ReadUnit)
    mass_String_prev = ''
    mass_Number_array(:) = 0
    read(ReadUnit,'(a)',iostat=error) FullPath
    Begin_in_String = index(FullPath,mass_Pos,.true.)
    mass_String = FullPath(Begin_in_String+2:Begin_in_String+2+mass_Length)
    mass_String_prev = mass_String
    Begin_in_String = index(FullPath,Z_Pos,.true.)
    Z_String = FullPath(Begin_in_String+1:Begin_in_String+1+Z_Length)
    Z_String_prev = Z_String
    mass_Number_array(1) = mass_Number_array(1) + 1
    do i=1,Z_Number
      do while (Z_String == Z_String_prev)
        read(ReadUnit,'(a)',iostat=error) FullPath
        if (error /= 0) then
          exit
        endif
        Begin_in_String = index(FullPath,mass_Pos,.true.)
        mass_String = FullPath(Begin_in_String+2:Begin_in_String+2+mass_Length)
        Begin_in_String = index(FullPath,Z_Pos,.true.)
        Z_String = FullPath(Begin_in_String+1:Begin_in_String+1+Z_Length)
        if (Z_String /= Z_String_prev) then
          Z_String_prev = Z_String
          mass_String_prev = mass_String
          mass_Number_array(i+1) = mass_Number_array(i+1) + 1
          exit
        endif
        if (mass_String /= mass_String_prev) then
          mass_Number_array(i) = mass_Number_array(i) + 1
        endif
        mass_String_prev = mass_String
      enddo
    enddo

    ! Counts the number of rotation velocities for each Z and mass.
    allocate(omega_Number_array(Z_Number,maxval(mass_Number_array)))
    omega_Number_array(:,:) = 0

    rewind(ReadUnit)
    read(ReadUnit,'(a)',iostat=error) FullPath
    Begin_in_String = index(FullPath,omega_Pos,.true.)
    omega_String = FullPath(Begin_in_String+1:Begin_in_String+1+omega_Length)
    omega_String_prev = Omega_String
    Begin_in_String = index(FullPath,mass_Pos,.true.)
    mass_String = FullPath(Begin_in_String+2:Begin_in_String+2+mass_Length)
    Begin_in_String = index(FullPath,Z_Pos,.true.)
    Z_String = FullPath(Begin_in_String+1:Begin_in_String+1+Z_Length)
    Z_String_prev = Z_String
    mass_String_prev = mass_String
    omega_Number_array(1,1) = Omega_Number_array(1,1) + 1

    do i=1,Z_Number
      do j=1,mass_Number_array(i)
        do while (Z_String == Z_String_prev)
          read(ReadUnit,'(a)',iostat=error) FullPath
          if (error /= 0) then
            exit
          endif
          Begin_in_String = index(FullPath,omega_Pos,.true.)
          omega_String = FullPath(Begin_in_String+1:Begin_in_String+1+omega_Length)
          Begin_in_String = index(FullPath,mass_Pos,.true.)
          mass_String = FullPath(Begin_in_String+2:Begin_in_String+2+mass_Length)
          Begin_in_String = index(FullPath,Z_Pos,.true.)
          Z_String = FullPath(Begin_in_String+1:Begin_in_String+Z_Length)
          if (Z_String /= Z_String_prev) then
            Z_String_prev = Z_String
            mass_String_prev = mass_String
            omega_String_prev = omega_String
            omega_Number_array(i+1,1) = omega_Number_array(i+1,1) + 1
            exit
          endif
          if (mass_String /= mass_String_prev) then
            mass_String_prev = mass_String
            omega_String_prev = omega_String
            omega_Number_array(i,j+1) = omega_Number_array(i,j+1) + 1
            exit
          endif
          if (omega_String /= omega_String_prev) then
            omega_Number_array(i,j) = omega_Number_array(i,j) + 1
          endif
          omega_String_prev = omega_String
        enddo
        if (Z_String /= Z_String_prev) then
          exit
        endif
      enddo
    enddo

    mass_Number = maxval(mass_Number_Array)
    omega_Number = maxval(omega_Number_Array)

    ! Allocation of the memory for the data array
    allocate(All_Data_Array(Z_Number,mass_Number,omega_Number))

    rewind(ReadUnit)
    ! The file ListFile being normally well sorted, we can now fill an array containing all the data, on order to
    ! store it in memory.
    do i=1,Z_Number
      do j=1,mass_Number_array(i)
        do k=1,omega_Number_array(i,j)
          read(ReadUnit,'(a)',iostat=error) FullPath
          Begin_in_String = index(FullPath,omega_Pos,.true.)
          omega_String = FullPath(Begin_in_String+1:Begin_in_String+1+omega_Length)
          Begin_in_String = index(FullPath,mass_Pos,.true.)
          mass_String = FullPath(Begin_in_String+2:Begin_in_String+2+mass_Length)
          Begin_in_String = index(FullPath,Z_Pos,.true.)
          Z_String = FullPath(Begin_in_String+1:Begin_in_String+Z_Length)
          if (Z_String == "m4") then
            Z_initial = 4.d-4
          else
            read(Z_String,'(i2)') Z_ini_int
            Z_initial = dble(Z_ini_int)/1000.d0
          endif
          read(omega_String,'(i2)') omega_ini_int
          omega_initial = dble(omega_ini_int)/100.d0
          read(mass_String(1:3),'(i3)') M_ini_int
          M_initial = dble(M_ini_int)
          read(mass_String(5:6),'(i2)') M_ini_int
          M_initial = M_initial + 0.01d0*dble(M_ini_int)
          if (FullPath(1:1) == "/") then
            table_name = trim(FullPath)
          else
            table_name=trim(Std_Path_tables)//trim(FullPath)
          endif
          if (verbose) then
            write(*,'(a,f5.3,1x,f6.2,1x,f4.2)') 'Z,M,OOc: ',Z_initial,M_initial,omega_initial
            write(*,*) 'File ',trim(table_name)
          endif
          call Init_DataStructure(Table_Line_Number,Data_Number,All_Data_Array(i,j,k))
          call FillData(Z_initial,M_initial,omega_initial,table_name,All_Data_Array(i,j,k))
        enddo
      enddo
    enddo

    ! We also need a list of the masses and of the velocities:
    allocate(Z_List(Z_Number))
    allocate(mass_List(Z_Number,mass_Number))
    allocate(omega_List(Z_Number,mass_Number,omega_Number))

    ! Initialisation of the arrays
    Z_List(:) = -1.d0
    mass_List(:,:) = -1.d0
    omega_List(:,:,:) = -1.d0
    do i=1,Z_Number
      Z_List(i) = All_Data_Array(i,1,1)%Metallicity
      do j=1,mass_Number_array(i)
        mass_List(i,j) = All_Data_Array(i,j,1)%mass_ini
        do k=1,omega_Number_array(i,j)
          omega_List(i,j,k) = All_Data_Array(i,j,k)%Omega_Omcrit_ini
        enddo
      enddo
    enddo

    close(ReadUnit)

    return

  end subroutine InitialiseData
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine WriteResults(write_mode)
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: i_mass,i_logL,i_logTeff_corr,i_H1_Surf,i_He4_surf,i_C12_surf,i_C13_surf,i_N14_surf, &
      i_O16_surf,i_O17_surf,i_O18_surf,i_Ne20_surf,i_Ne22_surf,i_Al26_surf,i_logTeff,i_Mdot, &
      i_Omega_surf,i_oblat,i_v_crit1,i_v_crit2,i_v_equa,i_Omega_Omcrit,i_Gamma_Ed,i_Mdot_mec, &
      i_PolarRadius,i_polar_gravity,Table_Line_Number,Data_Number,i_MV_noisy,i_BV_noisy,&
      i_MBol,i_MV,i_UB,i_BV,i_B2V1,i_VK,i_VR,i_VI,i_JK,i_HK,i_BC,i_logL_gd,i_logTeff_gd,i_logL_lgd, &
      i_logTeff_lgd,i_mean_gravity,i_GV,i_GbpV,i_GrpV,i_Gflag
    use VariousParameters, only: Current_Number,age_log,Comp_Mode,iangle,limb_dark
    use LoopVariables, only: CurrentTime_Model
    use Population_mode, only: N_Time_step,Evolution_Data,time_step_array,Evolutionary_Values
    use Constant, only: pi,R_sun

    implicit none

    integer, intent(in):: write_mode

    integer::error,i,j

    character(*),parameter:: Output_Format_GE= '(f7.3,2x,f8.6,2x,f5.3,2x,f7.3,17(2x,f8.4),2x,1pe9.3,&
                                         &2x,0pf6.4,2x,f6.3,2x,e9.3,3(2x,f8.2),2x,f6.4,1x,f7.3,1x,f7.3,&
                                         &2x,f6.4,11(2x,e9.3))', &
      Output_Format_SE= '(f7.3,2x,f8.6,2x,f5.3,2x,3(1x,f9.6),1x,f13.8,1x,f11.5,1x,f5.2,1x,e10.3,&
      &1x,f10.6,6(1x,f9.6),3(1x,e11.4),1x,f10.7,1x,e10.3,2(1x,f8.5),1x,f10.7,1x,e10.3,2(1x,f8.5),28(1x,e11.4),1x,&
      &e9.2,1x,e11.4,2(1x,e9.2),1x,e11.4,1x,e9.2,1x,e11.4,1x,e9.2,8(1x,e11.4),23(1x,f8.4),52(1x,e11.4))', &
      Output_Format_Cluster_GE= '(f6.2,2x,f8.6,4x,f5.3,2x,f5.2,4x,i1,1x,f7.3,1x,f6.2,22(2x,f8.4),2x,&
                                         &1pe9.3,2x,0pf6.4,2x,f6.3,2x,f6.3,2x,e9.3,3(2x,es8.2),2x,f6.4,1x,f7.3,1x,&
                                         &f7.3,2x,f6.4,11(2x,e9.3))', &
      Output_Format_Cluster_SE= '(f7.3,2x,f8.6,4x,f5.3,2x,f5.2,4x,i1,1x,f7.3,1x,3(1x,f9.6),1x,f13.8,1x,f11.5,&
      &1x,f5.2,1x,e10.3,1x,f10.6,6(1x,f9.6),3(1x,e11.4),1x,f10.7,1x,e10.3,2(1x,f8.5),1x,f10.7,1x,e10.3,2(1x,f8.5),&
      &28(1x,e11.4),1x,e9.2,1x,e11.4,2(1x,e9.2),1x,e11.4,1x,e9.2,1x,e11.4,1x,e9.2,8(1x,e11.4),23(1x,f8.4),&
      &52(1x,e11.4)))', &
      Output_Format_Single_GE = '(i3,1x,e22.15,1x,f11.6,2(1x,f9.6),2(1x,e14.7),1p,8(1x,e14.7),1x,e10.3,&
                                  &1x,0pf7.4,1x,f9.6,1x,f8.3,2(1x,f9.6),2(1x,e14.7),1p,8(1x,e14.7),5(1x,e10.3),&
                                  &3(1x,e9.2),0p,2(1x,f9.6),1x,1es14.7,1x,es17.10,16(2x,f7.3))', &
      Output_Format_Single_SE = '(1x,i3,1x,e17.10,3(1x,f9.6),1x,f13.8,1x,f11.5,1x,f5.2,1x,e10.3,&
      &1x,f10.6,6(1x,f9.6),3(1x,e11.4),1x,f10.7,1x,e10.3,2(1x,f8.5),1x,f10.7,1x,e10.3,2(1x,f8.5),28(1x,e11.4),1x,&
      &e9.2,1x,e11.4,2(1x,e9.2),1x,e11.4,1x,e9.2,1x,e11.4,1x,e9.2,8(1x,e11.4),23(1x,f8.4),52(1x,e11.4))', &
      Header_GE = ' M_ini      Z_ini  OmOc_ini  M       logL     logTe_c  logTe_nc      MBol        &
                         &MV       U-B       B-V       V-K       V-R       V-I       J-K&
                         &       H-K       G-V     Gbp-V     Grp-V    G_flag&
                         &        BC      r_pol   oblat   g_pol    Omega_S      v_eq   v_crit1&
                         &   v_crit2  Om/Om_cr lg(Md)  lg(Md_M) Ga_Ed H1         He4        C12        C13&
                         &        N14        O16        O17        O18        Ne20       Ne22       Al26', &
      Header_SE = ' M_ini    Z_ini  OmOc_ini  logTeff      logL  logLgrav             M           R  &
                  &logg   rho_phot    logMdot     logTc     logPc   logrhoc   logTmax   Mr_Tmax  logrhomax   &
                  &eps_nucl    eps_grav      eps_nu   Mr_b_CE  normR_b_CE   logT_b logrho_b   Mr_t_CC  normR_t_CC   &
                  &logT_t logrho_t     tau_max      Ro_max       tau_g        Ro_g     tau_Hp2      Ro_Hp2      &
                  &tau_Hp       Ro_Hp      tau_R2       Ro_R2      tau_M2       Ro_M2   tau_max_c    Ro_max_c     &
                  &tau_g_c      Ro_g_c   tau_Hp2_c    Ro_Hp2_c    tau_Hp_c     Ro_Hp_c    tau_R2_c     Ro_R2_c    &
                  &tau_M2_c     Ro_M2_c     k2_conv      k2_rad     Omega_s     Omega_c     Vsurf        Prot     &
                  &J_act    J_core         OOc     Vcrit      torque    B_equi        D_nu    D_nu_ech    &
                  &D_nu_err      nu_max   DPi_asym.   R_acc_tot   R_acc_BCE    R_acc_He     Mbol       BC      &
                  &U-B      B-V      V-R      V-I      J-K      H-K      V-K      G-V    Gbp-V    Grp-V      &
                  &M_U      M_B      M_V      M_R      M_I      M_H      M_J      M_K      M_G    M_Gbp    &
                  &M_Grp         H1s         H2s        He3s        He4s        Li6s        Li7s        Be7s        &
                  &Be9s        B10s        B11s        C12s        C13s        C14s        N14s        N15s        &
                  &O16s        O17s        O18s        F19s       Ne20s       Ne21s       Ne22s       Na23s       &
                  &Mg24s       Mg25s       Mg26s       Al26s       Al27s       Si28s         H1c         H2c        &
                  &He3c        He4c        C12c        C13c        C14c        N14c        N15c        O16c        &
                  &O17c        O18c        F19c       Ne20c       Ne21c       Ne22c       Na23c       Mg24c       &
                  &Mg25c       Mg26c       Al26c       Al27c       Si28c', &
      Header_Be = '      time        #star       O-star       B-star       A-star       F-star&
                         &           Oe           Be           Ae           Fe       O > 50      O >  70&
                         &       O > 80       O > 90       O > 95       O > 98       B > 50      B >  70&
                         &       B > 80       B > 90       B > 95       B > 98       A > 50      A >  70&
                         &       A > 80       A > 90       A > 95       A > 98       F > 50      F >  70&
                         &       F > 80       F > 90       F > 95       F > 98          RSG          YSG&
                         &          WNL          WNE           WC           WO     Cepheids Cepheid_norm&
                         &  4<L_RSG<4.5  4.5<L_RSG<5  5<L_RSG<5.5  5.5<L_RSG<6          BSG      early B&
                         & early B > 80   MS 2mag<TO         Mtot', &
      Header_Cluster_GE = ' M_ini     Z_ini  OmOc_ini Angle  Bin  M1/M2    M       logL     logTe_c&
                           &  logTe_nc   logL_gd  logTe_gd  logL_lgd logTe_lgd      MBol&
                           &        MV       U-B       B-V       V-R       V-I&
                           &       J-K       H-K       V-K     MV_n     B-V_n&
                           &       G-V     Gbp-V     Grp-V     G_flag&
                           &      r_pol   oblat   g_pol  g_mean    Omega_S      v_eq   v_crit1   v_crit2 Om/Om_cr lg(Md)&
                           &  lg(Md_M) Ga_Ed         H1        He4        C12        C13        N14        O16&
                           &        O17        O18       Ne20       Ne22       Al26', &
      Header_Cluster_SE = '  M_ini     Z_ini  OmOc_ini Angle  Bin   M1/M2   lg(Teff)     lg(L) lg(Lgrav)&
                   &             M           R lg(g)&
                   &   rho_phot   lg(Mdot)    lg(Tc)    lg(Pc)  lg(rhoc)  lg(Tmax)  Mr(Tmax)lg(rhomax)&
                   &    eps_nucl    eps_grav      eps_nu   Mr_b(CE)  R_b(CE)/R  lg(T_b)lg(rho_b)   Mr_t(CC)&
                   &  R_t(CC)/R  lg(T_t)lg(rho_t)     tau_max      Ro_max       tau_g        Ro_g     tau_Hp2&
                   &      Ro_Hp2      tau_Hp       Ro_Hp      tau_R2       Ro_R2      tau_M2       Ro_M2&
                   &   tau_max_c    Ro_max_c     tau_g_c      Ro_g_c   tau_Hp2_c&
                   &    Ro_Hp2_c    tau_Hp_c     Ro_Hp_c    tau_R2_c     Ro_R2_c    tau_M2_c     Ro_M2_c&
                   &     k2_conv      k2_rad     Omega_s     Omega_c     Vsurf        Prot     J_act    J_core&
                   &         OOc     Vcrit      torque    B_equi        D_nu    D_nu_ech    D_nu_err      nu_max&
                   &   DPi_asym.   R_acc_tot   R_acc_BCE    R_acc_He     Mbol       BC      U-B      B-V      V-R&
                   &      V-I      J-K      H-K      V-K      G-V    Gbp-V    Grp-V      M_U      M_B      M_V&
                   &      M_R      M_I      M_H      M_J      M_K      M_G    M_Gbp    M_Grp         H1s&
                   &         H2s        He3s        He4s        Li6s        Li7s        Be7s        Be9s&
                   &        B10s        B11s        C12s        C13s        C14s        N14s        N15s&
                   &        O16s        O17s        O18s        F19s       Ne20s       Ne21s       Ne22s&
                   &       Na23s       Mg24s       Mg25s       Mg26s       Al26s       Al27s       Si28s&
                   &         H1c         H2c        He3c        He4c        C12c        C13c        C14c&
                   &        N14c        N15c        O16c        O17c        O18c        F19c       Ne20c&
                   &       Ne21c       Ne22c       Na23c       Mg24c       Mg25c       Mg26c       Al26c&
                   &       Al27c       Si28c'

    character(1)::Answer = 'k'
    character(256)::Output_FileName,formatPop
    character(512)::Output_Format,Output_Format_Single,Output_Format_Cluster
    character(2048)::Header,Header_Cluster

    integer:: DataToPrint_Single = -1, DataToPrint_Iso = -1, DataToPrint_Cluster = -1, bigswitch = -1
    integer,parameter:: DataToPrint_Single_GE = 58,DataToPrint_Iso_GE = 43,DataToPrint_Cluster_GE = 51
    integer,parameter:: DataToPrint_Single_SE = 145,DataToPrint_Iso_SE = 147,DataToPrint_Cluster_SE = 149
    real(kind=8),dimension(:,:),allocatable::TableToPrint

    ! Perform some stuff related to the writing mode:
    select case (write_mode)
      case (1)
      ! GENEC output
        Output_Format = Output_Format_GE
        Output_Format_Single = Output_Format_Single_GE
        Output_Format_Cluster = Output_Format_Cluster_GE
        DataToPrint_Single = DataToPrint_Single_GE
        DataToPrint_Iso = DataToPrint_Iso_GE
        DataToPrint_Cluster = DataToPrint_Cluster_GE
        Header = Header_GE
        Header_Cluster = Header_Cluster_GE
        write(formatPop,'(a,i2,a)') '(es10.4,',Evolutionary_Values,'(3x,es10.4))'
      case (2)
      ! Starevol output
        Output_Format = Output_Format_SE
        Output_Format_Single = Output_Format_Single_SE
        Output_Format_Cluster = Output_Format_Cluster_SE
        DataToPrint_Single = DataToPrint_Single_SE
        DataToPrint_Iso = DataToPrint_Iso_SE
        DataToPrint_Cluster = DataToPrint_Cluster_SE
        Header = Header_SE
        Header_Cluster = Header_Cluster_SE
      case default
        write(*,*) 'Unexpected problem in writing format. Check.'
        stop
    end select

    ! Allocate the memory for the data to be printed.
    ! Here, we set a new integer for switching mode to cover the whole possible options:
    ! so far, 4 computing modes and 2 formats. 1-4 are GENEC, and 5-8 are starevol
    bigswitch = 4*(write_mode-1)+Comp_Mode
    select case (bigswitch)
      case (1) ! GENEC cluster
        allocate(TableToPrint(Current_Number,DataToPrint_Cluster))
        do i=1,Current_Number
          TableToPrint(i,:) = (/CurrentTime_Model(i)%mass_ini,CurrentTime_Model(i)%Metallicity, &
            CurrentTime_Model(i)%Omega_Omcrit_ini,90.d0-CurrentTime_Model(i)%Angle_of_View*180.d0/pi, &
            CurrentTime_Model(i)%mass_ratio, &
            CurrentTime_Model(i)%Data_Line(i_mass),CurrentTime_Model(i)%Data_Line(i_logL), &
            CurrentTime_Model(i)%Data_Line(i_logTeff_corr),CurrentTime_Model(i)%Data_Line(i_logTeff), &
            CurrentTime_Model(i)%Additional_Data_Line(i_logL_gd),CurrentTime_Model(i)%Additional_Data_Line(i_logTeff_gd), &
            CurrentTime_Model(i)%Additional_Data_Line(i_logL_lgd),CurrentTime_Model(i)%Additional_Data_Line(i_logTeff_lgd), &
            CurrentTime_Model(i)%Additional_Data_Line(i_MBol),CurrentTime_Model(i)%Additional_Data_Line(i_MV), &
            CurrentTime_Model(i)%Additional_Data_Line(i_UB),CurrentTime_Model(i)%Additional_Data_Line(i_BV), &
            CurrentTime_Model(i)%Additional_Data_Line(i_VR),CurrentTime_Model(i)%Additional_Data_Line(i_VI),&
            CurrentTime_Model(i)%Additional_Data_Line(i_JK),CurrentTime_Model(i)%Additional_Data_Line(i_HK),&
            CurrentTime_Model(i)%Additional_Data_Line(i_VK),CurrentTime_Model(i)%Additional_Data_Line(i_MV_noisy), &
            CurrentTime_Model(i)%Additional_Data_Line(i_BV_noisy), &
            CurrentTime_Model(i)%Additional_Data_Line(i_GV),CurrentTime_Model(i)%Additional_Data_Line(i_GbpV),&
            CurrentTime_Model(i)%Additional_Data_Line(i_GrpV),CurrentTime_Model(i)%Additional_Data_Line(i_Gflag),&
            CurrentTime_Model(i)%Additional_Data_Line(i_PolarRadius),CurrentTime_Model(i)%Data_Line(i_oblat), &
            log10(CurrentTime_Model(i)%Additional_Data_Line(i_polar_gravity)), &
            log10(CurrentTime_Model(i)%Additional_Data_Line(i_mean_gravity)), &
            CurrentTime_Model(i)%Data_Line(i_Omega_surf),CurrentTime_Model(i)%Data_Line(i_v_equa), &
            CurrentTime_Model(i)%Data_Line(i_v_crit1),CurrentTime_Model(i)%Data_Line(i_v_crit2), &
            CurrentTime_Model(i)%Data_Line(i_Omega_Omcrit),CurrentTime_Model(i)%Data_Line(i_Mdot), &
            CurrentTime_Model(i)%Data_Line(i_Mdot_mec),CurrentTime_Model(i)%Data_Line(i_Gamma_Ed), &
            CurrentTime_Model(i)%Data_Line(i_H1_surf),CurrentTime_Model(i)%Data_Line(i_He4_surf), &
            CurrentTime_Model(i)%Data_Line(i_C12_Surf),CurrentTime_Model(i)%Data_Line(i_C13_Surf), &
            CurrentTime_Model(i)%Data_Line(i_N14_Surf),CurrentTime_Model(i)%Data_Line(i_O16_Surf), &
            CurrentTime_Model(i)%Data_Line(i_O17_Surf),CurrentTime_Model(i)%Data_Line(i_O18_Surf), &
            CurrentTime_Model(i)%Data_Line(i_Ne20_Surf),CurrentTime_Model(i)%Data_Line(i_Ne22_Surf), &
            CurrentTime_Model(i)%Data_Line(i_Al26_Surf)/)
        enddo
      case (2) ! GENEC isochrone
        allocate(TableToPrint(Current_Number,DataToPrint_Iso))
        do i=1,Current_Number
          TableToPrint(i,:) = (/CurrentTime_Model(i)%mass_ini,CurrentTime_Model(i)%Metallicity, &
            CurrentTime_Model(i)%Omega_Omcrit_ini,CurrentTime_Model(i)%Data_Line(i_mass), &
            CurrentTime_Model(i)%Data_Line(i_logL),CurrentTime_Model(i)%Data_Line(i_logTeff_corr), &
            CurrentTime_Model(i)%Data_Line(i_logTeff), &
            CurrentTime_Model(i)%Additional_Data_Line(i_MBol),CurrentTime_Model(i)%Additional_Data_Line(i_MV), &
            CurrentTime_Model(i)%Additional_Data_Line(i_UB),CurrentTime_Model(i)%Additional_Data_Line(i_BV),&
            CurrentTime_Model(i)%Additional_Data_Line(i_VK),&
            CurrentTime_Model(i)%Additional_Data_Line(i_VR),CurrentTime_Model(i)%Additional_Data_Line(i_VI),&
            CurrentTime_Model(i)%Additional_Data_Line(i_JK),CurrentTime_Model(i)%Additional_Data_Line(i_HK),&
            CurrentTime_Model(i)%Additional_Data_Line(i_GV),CurrentTime_Model(i)%Additional_Data_Line(i_GbpV),&
            CurrentTime_Model(i)%Additional_Data_Line(i_GrpV),CurrentTime_Model(i)%Additional_Data_Line(i_Gflag),&
            CurrentTime_Model(i)%Additional_Data_Line(i_BC),CurrentTime_Model(i)%Additional_Data_Line(i_PolarRadius), &
            CurrentTime_Model(i)%Data_Line(i_oblat),log10(CurrentTime_Model(i)%Additional_Data_Line(i_polar_gravity)), &
            CurrentTime_Model(i)%Data_Line(i_Omega_surf),CurrentTime_Model(i)%Data_Line(i_v_equa), &
            CurrentTime_Model(i)%Data_Line(i_v_crit1),CurrentTime_Model(i)%Data_Line(i_v_crit2), &
            CurrentTime_Model(i)%Data_Line(i_Omega_Omcrit),CurrentTime_Model(i)%Data_Line(i_Mdot), &
            CurrentTime_Model(i)%Data_Line(i_Mdot_mec),CurrentTime_Model(i)%Data_Line(i_Gamma_Ed), &
            CurrentTime_Model(i)%Data_Line(i_H1_surf),CurrentTime_Model(i)%Data_Line(i_He4_surf), &
            CurrentTime_Model(i)%Data_Line(i_C12_Surf),CurrentTime_Model(i)%Data_Line(i_C13_Surf), &
            CurrentTime_Model(i)%Data_Line(i_N14_Surf),CurrentTime_Model(i)%Data_Line(i_O16_Surf), &
            CurrentTime_Model(i)%Data_Line(i_O17_Surf),CurrentTime_Model(i)%Data_Line(i_O18_Surf), &
            CurrentTime_Model(i)%Data_Line(i_Ne20_Surf),CurrentTime_Model(i)%Data_Line(i_Ne22_Surf), &
            CurrentTime_Model(i)%Data_Line(i_Al26_Surf)/)
        enddo
      case (3) ! GENEC population
        allocate(TableToPrint(N_Time_step,size(Evolution_Data,2)+1))
        do i=1,N_Time_step
          TableToPrint(i,:) = (/time_step_array(i),Evolution_Data(i,:)/)
        enddo
      case (4) ! GENEC single model
        allocate(TableToPrint(Table_Line_Number,DataToPrint_Single))
        do i=1,Table_Line_Number
          if (iangle > 0) then
            if (limb_dark > 0) then
              CurrentTime_Model(i)%Data_Line(i_logL) = CurrentTime_Model(i)%Additional_Data_Line(i_logL_lgd)
              CurrentTime_Model(i)%Data_Line(i_logTeff_corr) = CurrentTime_Model(i)%Additional_Data_Line(i_logTeff_lgd)
            else
              CurrentTime_Model(i)%Data_Line(i_logL) = CurrentTime_Model(i)%Additional_Data_Line(i_logL_gd)
              CurrentTime_Model(i)%Data_Line(i_logTeff_corr) = CurrentTime_Model(i)%Additional_Data_Line(i_logTeff_gd)
            endif
          endif
          TableToPrint(i,:) = (/(CurrentTime_Model(i)%Data_Line(j),j=1,Data_Number), &
            log10(CurrentTime_Model(i)%Additional_Data_Line(i_polar_gravity)), &
            CurrentTime_Model(i)%Additional_Data_Line(i_PolarRadius)/R_sun, &
            (CurrentTime_Model(i)%Additional_Data_Line(j),j=i_MBol,i_BV), &
            (CurrentTime_Model(i)%Additional_Data_Line(j),j=i_VR,i_Gflag), &
            CurrentTime_Model(i)%Additional_Data_Line(i_BC)/)
        enddo
      case (5) ! starevol cluster
        allocate(TableToPrint(Current_Number,DataToPrint_Cluster))
        do i=1,Current_Number
          TableToPrint(i,:) = (/CurrentTime_Model(i)%mass_ini,CurrentTime_Model(i)%Metallicity, &
            CurrentTime_Model(i)%Omega_Omcrit_ini,90.d0-CurrentTime_Model(i)%Angle_of_View*180.d0/pi, &
            CurrentTime_Model(i)%mass_ratio, &
            CurrentTime_Model(i)%Data_Line(2:)/)
        enddo
      case (6) ! starevol isochrone
        allocate(TableToPrint(Current_Number,DataToPrint_Iso))
        do i=1,Current_Number
          TableToPrint(i,:) = (/CurrentTime_Model(i)%mass_ini,CurrentTime_Model(i)%Metallicity, &
            CurrentTime_Model(i)%Omega_Omcrit_ini,(CurrentTime_Model(i)%Data_Line(j),j=2,DataToPrint_Single)/)
        enddo
      case (7) ! starevol population
        write(*,*) 'This should not occur. Check input parameters'
        stop
      case (8) ! starevol single model
        allocate(TableToPrint(Table_Line_Number,DataToPrint_Single))
        do i=1,Table_Line_Number
          TableToPrint(i,:) = CurrentTime_Model(i)%Data_Line(:)
        enddo
      case default
        write(*,*) 'Unexpected mode...'
        stop
    end select

    ! Attribution of a file name.
    select case (Comp_Mode)
      case (1)
        write(Output_FileName,'(a,f6.4,a,f6.3,tl6,i2.2,4x,a)') 'Cluster_z',CurrentTime_Model(1)%Metallicity,&
          '_t',age_log,int(age_log),'.dat'
      case (2)
        write(Output_FileName,'(a,f6.4,a,f4.2,a,f6.3,tl6,i2.2,4x,a)') 'Isochr_Z',CurrentTime_Model(1)%Metallicity,&
          '_Vini',CurrentTime_Model(1)%Omega_Omcrit_ini,'_t',age_log,int(age_log),'.dat'
      case (3)
        Output_FileName='Population_time_evolution.dat'
      case(4)
        if (CurrentTime_Model(1)%mass_ini > 100.d0) then
          write(Output_FileName,'(a,f6.2,a,f6.4,a,f4.2,a)') 'P',CurrentTime_Model(1)%mass_ini,'z', &
            CurrentTime_Model(1)%Metallicity,'S',CurrentTime_Model(1)%Omega_Omcrit_ini,'.dat'
        else if (CurrentTime_Model(1)%mass_ini > 10.d0) then
          write(Output_FileName,'(a,f5.2,a,f6.4,a,f4.2,a)') 'P',CurrentTime_Model(1)%mass_ini,'z', &
            CurrentTime_Model(1)%Metallicity,'S',CurrentTime_Model(1)%Omega_Omcrit_ini,'.dat'
        else
          write(Output_FileName,'(a,f4.2,a,f6.4,a,f4.2,a)') 'P',CurrentTime_Model(1)%mass_ini,'z', &
            CurrentTime_Model(1)%Metallicity,'S',CurrentTime_Model(1)%Omega_Omcrit_ini,'.dat'
        endif
      case default
        write(*,*) 'Unexpected mode...'
        stop
    end select

    ! Open file
    open(unit=50,file=Output_FileName,status='new',iostat=error)
    if (error /= 0) then
      do while (Answer /= 'y' .and. Answer /= 'n')
        write(*,*) 'The file ',trim(Output_FileName),' already exists. Overwrite ? (y) / (n)'
        read(5,*) Answer
      enddo
      if (Answer == 'y') then
        open(unit=50,file=Output_FileName,status='unknown',iostat=error)
        if (error /= 0) then
          write(*,*) 'Problem opening ',trim(Output_FileName),'...'
          stop
        endif
      else
        write(*,*) 'Aborting...'
        return
      endif
    endif

    select case (Comp_Mode)
      case (1)
        write(50,'(a)') 'WARNING: Note that in case of binary star, the colours and the Teff are NOT the composite&
                    & one, but only the primary ones !'
        write(50,'(a)') trim(Header_Cluster)
        write(50,*)
        do i=1,Current_Number
          write(50,trim(Output_Format_Cluster)) TableToPrint(i,:4),CurrentTime_Model(i)%Is_a_Binary,TableToPrint(i,5:)
        enddo
      case(2)
        write(50,'(a)') trim(Header)
        write(50,*)
        do i=1,Current_Number
          write(50,trim(Output_Format)) TableToPrint(i,:)
        enddo
      case (3)
        write(50,'(a)') Header_Be

        do i=1,N_Time_step
          write(50,trim(formatPop)) TableToPrint(i,:)
        enddo
      case (4)
        do i=1,2
          write(50,*)
        enddo
        do i=1,Table_Line_Number
          write(50,Output_Format_Single) CurrentTime_Model(i)%star_ID,TableToPrint(i,:)
        enddo
      case default
        write(*,*) 'Should not occurs !'
        stop
    end select

    close(50)

    deallocate(CurrentTime_Model)
    deallocate(TableToPrint)

    return

  end subroutine WriteResults
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module InOut
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module CheckFunctions
  ! Module containing the in- and out-subroutines
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  implicit none

  public::Check_MassRange

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Check_MassRange(age_log,Model,test,near_the_end)
    ! Check if the current mass is in the accetped mass range.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_DataStructure,Table_Line_Number,i_time
    use VariousParameters, only: m_IMF_inf,m_IMF_sup,SN_Number,Small_Number,Cluster_mass,Compute,Current_Number,Comp_Mode

    implicit none

    real(kind=8), intent(in)::age_log
    type(type_DataStructure), intent(inout)::Model

    logical, intent(out)::test, near_the_end

    real(kind=8)::log_agemax

    log_agemax = log10(Model%Data_Table(Table_Line_Number,i_time))

    ! In isochrone mode, we need to know if we are close to the end of the curve:
    if (age_log > log_agemax - 0.1d0) then
      near_the_end = .true.
    else
      near_the_end = .false.
    endif

    ! Check if at the current time, the star is still alive
    ! or if the current mass is larger than the maximum mass allowed by the IMF (this should not arrise).
    ! If this is not the case, increment the SN counter.
    if (age_log > log_agemax .or. Model%mass_ini > m_IMF_sup) then
      SN_Number = SN_Number + 1
      Cluster_mass = Cluster_mass + Model%mass_ini
      ! In isochrone mode, we need to exit the loop anyway.
      if (Comp_Mode /= 2) then
        test = .true.
      else
        test = .false.
        Compute = .false.
        write(*,*) 'Current_Number : ',Current_Number
        write(*,*) 'Maximal mass at the current time : ',Model%mass_ini
      endif
      return
    endif

    ! Check if the current mass is smaller than the minimum mass allowed by the IMF (this should not arrise).
    ! If this is not the case, increment the small star counter.
    if (Model%mass_ini < m_IMF_inf) then
      Small_Number = Small_Number+1
      Cluster_mass = Cluster_mass + Model%mass_ini
      test = .true.
      return
    endif

    ! In all other cases, the star is still alive at the current time. Set test to exit the loop.
    test = .false.

    return

  end subroutine Check_MassRange
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module CheckFunctions
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module InterpolationLoop
  ! Main module, contains the loop for interpolating models
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  use VariousParameters, only:Current_Number,SN_Number,Small_Number,Cepheid_Number,FastRot_Number,Cluster_mass,Compute

  implicit none

  ! Some local variables used for the isochrone mode:
  integer, private::Number_of_Points_isochrone
  integer, private::Max_star_number_isomode = 100000

  real(kind=8),private,parameter::DiffmaxMV=2.0d-1,DiffmaxBV=1.0d-2,DiffmaxL=2.0d-2,DiffmaxTeff=2.0d-2
  real(kind=8),private,save::initial_mass_isochrone,dm_isochrone,dm_isochrone_ini,dm_isochrone_min

  logical,private,save::iso_initialise
  logical,private,save::Binary_Star                                  ! Used in the binary loop

  public::MainLoop
  private::Initialise
  private::set_dm
  private::Binary
  private::Add_Flux
  private::Add_Noise
  private::Initialise_Position_and_factor

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine MainLoop
    ! Driver for the interpolation loop
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_DataStructure,Table_Line_Number,i_logTeff,i_logL,i_logTeff_corr,i_time
    use VariousParameters, only: IMF_type,table_format,Star_Z,Star_mass,Star_omega,Star_AoV, &
      ivdist,age_log,fixed_metallicity,om_ivdist,star_number,Comp_Mode,iangle,Z_Number, &
      mass_Number_array,Fixed_AoV,All_Data_Array
    use LoopVariables, only:Z_Position,Z_factor,omega_Position,omega_factor,mass_Position,mass_factor, &
      Interpolated_Model,CurrentTime_Model
    use random, only: Z_RandomDraw,Mass_RandomDraw,Omega_RandomDraw,AoV_RandomDraw
    use interpolmod, only:All_Positions_and_factors,Make_InterpolatedModel,Make_TimeModel
    use InOut, only: InitialiseData,WriteResults
    use CheckFunctions, only:check_massrange
    use Population_Mode, only: Mass_Interfaces,Velocity_Interfaces,Pop_Omega_Beam_Number,Pop_Mass_Beam_Number, &
      Make_Each_Time_step,Count_Populations,Is_a_Cepheid
    use Additional_Data, only: Compute_Additional,Compute_Additional_Single

    implicit none

    integer:: Mass_Rank, Omega_Rank ! Used only in population mode.
    integer:: i

    logical::mass_in_mass_range,near_the_end

    ! Various initialisations
    call Initialise

    ! Searching, opening, sorting and reading evolution files. These files must be listed (with the whole
    ! access path) in a file called "ListFile.txt".
    call InitialiseData

    ! Initialisation of the counters used in Population mode.
    Mass_Rank = 1
    Omega_Rank = 1

    select case (Comp_Mode)
!--------------------------------------------------
      ! Cluster and isochrone mode
      case (1,2)
        do while (Compute)
          ! Initialisation of the mass range check
          ! Setting mass_in_mass_range to true at the beginning of each loop.
          mass_in_mass_range = .true.
          ! Incrementation of the star identifier
          Current_Number = Current_Number + 1
          ! Initialisation of the angle of view, for them cases where the random draw is not performed.
          Star_AoV = 0.d0
          do while (mass_in_mass_range)
            ! Drawing mass and omega. We loop untill we find a mass and a velocity authorised at the given age.
            ! Here, we are also able to determine if the current star is too small or too big, and to count the
            ! number of SN explosions occuring in the cluster.
            select case (Comp_Mode)
              case (1)
                call Z_RandomDraw(Star_Z)
                call Mass_RandomDraw(IMF_Type,Star_mass)
                call Omega_RandomDraw(ivdist,Star_mass,Star_omega)
                ! Perform the angle of view draw if wanted.
                if (iangle > 0) then
                  call AoV_RandomDraw(Star_AoV)
                endif
              case (2)
                Star_Z = fixed_metallicity
                if (iso_initialise) then
                  Star_mass = CurrentTime_Model(Current_Number-1)%mass_ini + dm_isochrone
                else
                  Star_mass = initial_mass_isochrone
                  iso_initialise = .true.
                endif
                Star_omega = om_ivdist
            end select

            call Initialise_Position_and_factor(Z_Position,Z_factor,mass_Position,mass_factor,omega_Position, &
                                                omega_factor)
            call All_Positions_and_factors(Star_Z,Z_Position,Z_factor,Star_mass,mass_Position(:),mass_factor(:), &
                                           Star_omega,omega_Position(:,:),omega_factor(:,:))

            ! Perform the interpolation
            call Make_InterpolatedModel(Z_Position,Z_factor,mass_Position,mass_factor,omega_Position, &
                                        omega_factor,Interpolated_Model)
            call Check_MassRange(age_log,Interpolated_Model,mass_in_mass_range,near_the_end)
            ! Near the maximal mass, try to find the maximal mass with a better accuracy.
            if (Comp_Mode == 2 .and. Current_Number > 1 .and. .not. Compute) then
              if (abs(CurrentTime_Model(Current_Number-1)%mass_ini - Interpolated_Model%mass_ini) > 1.d-4) then
                write(*,*) 'Try to improve maximal mass to a precision of 10e-4...'
                dm_isochrone = dm_isochrone/3.d0
                Compute = .true.
                mass_in_mass_range = .true.
              endif
            endif
          enddo
          ! In isochrone mode, it should happen that Compute is set to .false. in Check_MassRange. We have to exit the loop in
          ! that case.
          if (.not. Compute .and. Comp_Mode == 2) then
            Current_Number = Current_Number - 1
            exit
          endif

          ! Total mass in the cluster
          Cluster_mass = Cluster_mass + Star_mass
          ! Once we have the model interpolated in mass and velocity, we extract and interpolate the data
          ! at the current age of the cluster.
          call Make_TimeModel(Interpolated_Model,age_log,CurrentTime_Model(Current_Number))
          ! Star_id is used to sort the stars by mass at the end of the process.
          CurrentTime_Model(Current_Number)%Star_ID = Current_Number
          ! Attribution of the angle of view
          CurrentTime_Model(Current_Number)%Angle_of_View = Star_AoV
          ! Computation of the additional quantities.
          call Compute_Additional(CurrentTime_Model(Current_Number))

          ! In cluster mode, we can account for the binaries
          if (Comp_Mode == 1) then
            call Binary(CurrentTime_Model(Current_Number))
            call Add_Noise(CurrentTime_Model(Current_Number))
            if (Is_a_Cepheid(CurrentTime_Model(Current_Number)%Data_Line(i_logL), &
                             CurrentTime_Model(Current_Number)%Data_Line(i_logTeff))) then
              Cepheid_Number = Cepheid_Number + 1
            endif
          else
            CurrentTime_Model(Current_Number)%Is_a_Binary = 0
          endif

          ! In isochrone mode, check that the distance between two points is not too big. If necessary, the mass step is divided.
          if (Comp_Mode == 2 .and. Current_Number > 1) then
            call set_dm(CurrentTime_Model(Current_Number),CurrentTime_Model(Current_Number-1),near_the_end)
          endif

          ! In isochrone mode, check that we do not exceed the maximum memory space allocated.
          if (Comp_Mode == 2) then
            if (Current_Number .ge. Max_star_number_isomode) then
              write(*,*) 'Maximum number of stars reached. Exit.'
              Compute = .false.
            endif
          else
            if (Current_Number == star_number) then
              Compute = .false.
            endif
          endif
        enddo
!--------------------------------------------------
      ! Population mode
      case (3)
        do while (Compute)
          ! Initialisation of the mass range check
          ! Setting mass_in_mass_range to true at the beginning of each loop.
          mass_in_mass_range = .true.
          ! Incrementation of the star identifier
          Current_Number = Current_Number + 1
          ! Initialisation of the angle of view, for them cases where the random draw is not performed.
          Star_AoV = 0.d0
          ! Drawing mass and omega. We loop untill we find a mass and a velocity authorised at the given age.
          ! Here, we are also able to determine if the current star is too small or too big, and to count the
          ! number of SN explosions occuring in the cluster.

          Star_Z = fixed_metallicity
          Star_mass = (Mass_Interfaces(Mass_Rank) + Mass_Interfaces(Mass_Rank+1))/2.d0
          if (Pop_Omega_Beam_Number == 1 .and. ivdist == 3) then
            Star_omega = om_ivdist
          else
            Star_omega =(Velocity_Interfaces(Omega_Rank) + Velocity_Interfaces(Omega_Rank+1))/2.d0
          endif

          call Initialise_Position_and_factor(Z_Position,Z_factor,mass_Position,mass_factor,omega_Position, &
                                              omega_factor)
          call All_Positions_and_factors(Star_Z,Z_Position,Z_factor,Star_mass,mass_Position(:),mass_factor(:), &
                                         Star_omega,omega_Position(:,:),omega_factor(:,:))

          ! Perform the interpolation
          call Make_InterpolatedModel(Z_Position,Z_factor,mass_Position,mass_factor,omega_Position, &
                                      omega_factor,Interpolated_Model)

          ! Star_id is used to sort the stars by mass at the end of the process.
          CurrentTime_Model(Current_Number)%Star_ID = Current_Number
          ! Attribution of the angle of view
          CurrentTime_Model(Current_Number)%Angle_of_View = Star_AoV
          ! Once we have the model interpolated in mass and velocity, we extract and interpolate the data
          ! at the current age of the cluster.
          if (Omega_Rank == 1) then
            write(*,'(a,f8.4,a)') 'Mass ',Star_mass,' now computing.'
          endif
          call Make_Each_Time_step(Interpolated_Model,Mass_Rank,Omega_Rank,CurrentTime_Model(Current_Number))
          if (Omega_Rank == Pop_Omega_Beam_Number) then
            if (Mass_Rank == Pop_Mass_Beam_Number) then
              Compute = .false.
            endif
            Omega_Rank = 1
            Mass_Rank = Mass_Rank + 1
          else
            Omega_Rank = Omega_Rank + 1
          endif

          CurrentTime_Model(Current_Number)%Is_a_Binary = 0
        enddo
!--------------------------------------------------
      ! Single model mode
      case (4)
        ! Initialisation of the angle of view, for them cases where the random draw is not performed.
        Star_AoV = 0.d0
        ! Drawing mass and omega. We loop untill we find a mass and a velocity authorised at the given age.
        ! Here, we are also able to determine if the current star is too small or too big, and to count the
        ! number of SN explosions occuring in the cluster.
        write(*,*) 'Enter the wanted metallicity for the model :'
        read(5,*) Star_Z
        write(*,*) 'Enter the wanted mass for the model :'
        read(5,*) Star_mass
        write(*,*) 'Enter the wanted initial velocity for the model :'
        read(5,*) Star_omega
        Star_AoV = Fixed_AoV

        call Initialise_Position_and_factor(Z_Position,Z_factor,mass_Position,mass_factor,omega_Position, &
                                            omega_factor)
        call All_Positions_and_factors(Star_Z,Z_Position,Z_factor,Star_mass,mass_Position(:),mass_factor(:), &
                                       Star_omega,omega_Position(:,:),omega_factor(:,:))

        ! Perform the interpolation
        call Make_InterpolatedModel(Z_Position,Z_factor,mass_Position,mass_factor,omega_Position, &
                                    omega_factor,Interpolated_Model)

        ! Computation of the additional quantities.
        call Compute_Additional_Single(Interpolated_Model,Star_AoV,CurrentTime_Model,Table_Line_Number)
!--------------------------------------------------
      case default
        write(*,*) 'Unexpected problem...'
        stop

    end select

    if (Comp_Mode == 3) then
      call Count_Populations
    endif

    call WriteResults(table_format)

    return

  end subroutine MainLoop
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Initialise
    ! Initialisations of some parameters
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: Table_Line_Number,Table_Line_Number_normal,Table_Line_Number_PMS,Init_Indices
    use VariousParameters, only:m_IMF_inf,m_IMF_sup,star_number,Comp_Mode,age_log,PMS,table_format
    use LoopVariables, only:CurrentTime_Model

    implicit none

    Current_Number = 0
    SN_Number = 0
    Small_Number = 0
    Cepheid_Number = 0
    FastRot_Number = 0

    Cluster_mass = 0.d0

    Compute = .true.

    !Initialise parameters depending on PMS in the tables or not.
    if (PMS) then
      Table_Line_Number = Table_Line_Number_PMS
    else
      Table_Line_Number = Table_Line_Number_normal
    endif

    ! Initialissation of the indices.
    call Init_Indices(table_format)

    select case (Comp_Mode)
      case (1)
        write(*,*) 'starnumber: ',star_number
        allocate(CurrentTime_Model(star_number))
        write(*,*) 'Cluster mode, log(age)=',age_log
        write(*,*)
        write(*,*) 'calculating the synthetic cluster'
      case (2)
        ! If the isochrone mode is unsed, more initialisations are needed.
        initial_mass_isochrone = m_IMF_inf
        Number_of_Points_isochrone = star_number
        dm_isochrone = (m_IMF_sup - m_IMF_inf)/dble(Number_of_Points_isochrone - 1)
        dm_isochrone_ini = dm_isochrone
        dm_isochrone_min = min(dm_isochrone_ini/1000.d0,9.0d-5)
        star_number = Number_of_Points_isochrone
        Max_star_number_isomode = 100*star_number
        iso_initialise = .false.
        allocate(CurrentTime_Model(Max_star_number_isomode))
        write(*,*) 'Isochrone mode, log(age)=',age_log
        write(*,*)
        write(*,*) 'calculating the isochrone'
      case (3)
        allocate(CurrentTime_Model(star_number))
        write(*,*)
        write(*,*) 'calculating the synthetic cluster'
      case (4)
        ! In single model mode, we save each line of the model
        allocate(CurrentTime_Model(Table_Line_Number))
        write(*,*)
        write(*,*) 'calculating the stellar model'
      case default
        write(*,*) 'Error in Initialise, should not arrise !'
        stop
    end select


    return

  end subroutine Initialise
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine set_dm(Model1,Model2,near_the_end)
    ! Set the mass step in isochrone mode.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure,only: type_TimeModel,i_logL,i_logTeff_corr,i_BV,i_MV

    implicit none

    type(type_TimeModel), intent(in)::Model1,Model2
    logical, intent(in):: near_the_end

    real(kind=8):: factor

    if (abs(Model1%Additional_Data_Line(i_MV) - Model2%Additional_Data_Line(i_MV)) > DiffmaxMV .or. &
      abs(Model1%Additional_Data_Line(i_BV) - Model2%Additional_Data_Line(i_BV)) > DiffmaxBV .or. &
      abs(Model1%Data_Line(i_logL) - Model2%Data_Line(i_logL)) > DiffmaxL .or. &
      abs(Model1%Data_Line(i_logTeff_corr) - Model2%Data_Line(i_logTeff_corr)) > DiffmaxTeff) then
      if (dm_isochrone /= dm_isochrone_min) then
        if (dm_isochrone > 2.d0*dm_isochrone_min) then
          dm_isochrone = dm_isochrone/2.d0
        else
          dm_isochrone = dm_isochrone_min
        endif
        Current_Number = Current_Number - 1
      endif
    else
      ! near the end of the isochrone, we prevent the code to increase the mass step.
      if (2.d0*dm_isochrone < dm_isochrone_ini) then
        if (near_the_end) then
          factor = 1.3d0
        else
          factor = 2.d0
        endif
        dm_isochrone = dm_isochrone*factor
      else
        dm_isochrone = dm_isochrone_ini
      endif
    endif

    return

  end subroutine set_dm
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Binary(Time_Model)
    ! Determine if the star is a binary. In that case, compute the binary model, and sum the observed fluxes.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_TimeModel,type_DataStructure,i_MBol,i_MV,i_UB,i_BV,i_logL
    use VariousParameters, only: m_IMF_inf,age_log,Z_Number,mass_number_array
    use random, only: Binary_RandomDraw,Binary_Mass_RandomDraw
    use interpolmod, only: All_Positions_and_factors,Make_InterpolatedModel,Make_TimeModel
    use Additional_Data, only: Compute_Additional

    implicit none

    type(type_TimeModel), intent(inout):: Time_Model

    type(type_DataStructure):: Interpolated_Binary
    type(type_TimeModel):: Time_Binary

    integer:: Z_Position,Real_Z_Position,i
    integer, dimension(2)::mass_Position
    integer, dimension(2,2):: omega_Position

    real(kind=8):: Z_Binary,Omega_Binary,Mass_Binary,Z_factor,M_B_Prim,M_B_Secon,M_U_Prim,M_U_Secon
    real(kind=8), dimension(2):: mass_factor
    real(kind=8), dimension(2,2):: omega_factor

    logical::Too_Small ! Used if the mass of the secondary is smaller than the lower mass of the IMF considered.

    ! Determine if the star is a binary star.
    call Binary_RandomDraw(Time_Model%Is_a_Binary)

    Too_Small = .false.
    ! Behaviour if or not a binary
    select case (Time_Model%Is_a_Binary)
      ! If not a binary, nothing to do
      case (0)
        Time_Model%mass_ratio = 0.d0
      ! If binary, compute the new parameters
      case (1)
        ! The metallicity and the initial velocity of the binary star is assumed to be the same than the primary
        Z_Binary = Time_Model%Metallicity
        Omega_Binary = Time_Model%Omega_Omcrit_ini
        ! The mass is supposed to be comprised between m_IMF_inf and the initial mass of the primary
        Mass_Binary = Binary_Mass_RandomDraw(Time_Model%mass_ini)
        Time_Model%mass_ratio = Mass_Binary/Time_Model%mass_ini
        if (Mass_Binary < m_IMF_inf) then
          Too_Small = .true.
        else
          ! Compute the parameters of the binary.
          ! Initialise the array containing positions and factor.
          call Initialise_Position_and_factor(Z_Position,Z_factor,mass_Position,mass_factor,omega_Position, &
                                              omega_factor)
          call All_Positions_and_factors(Z_Binary,Z_Position,Z_factor,Mass_Binary,mass_Position(:),mass_factor(:), &
                                         Omega_Binary,omega_Position(:,:),omega_factor(:,:))

          call Make_InterpolatedModel(Z_Position,Z_factor,mass_Position,mass_factor,omega_Position, &
                                      omega_factor,Interpolated_Binary)

          call Make_TimeModel(Interpolated_Binary,age_log,Time_Binary)
          Time_Binary%Star_ID = 0
          Time_Binary%Angle_of_View = Time_Model%Angle_of_View
          call Compute_Additional(Time_Binary)
          ! Add the flux of the binary to the primary :
          M_B_Prim = Time_Model%Additional_Data_Line(i_BV) + Time_Model%Additional_Data_Line(i_MV)
          M_B_Secon = Time_Binary%Additional_Data_Line(i_BV) + Time_Binary%Additional_Data_Line(i_MV)
          M_U_Prim = Time_Model%Additional_Data_Line(i_UB) + M_B_Prim
          M_U_Secon = Time_Binary%Additional_Data_Line(i_UB) + M_B_Secon
          Time_Model%Additional_Data_Line(i_MBol) = Add_Flux(Time_Model%Additional_Data_Line(i_MBol), &
            Time_Binary%Additional_Data_Line(i_MBol))
          Time_Model%Additional_Data_Line(i_MV) = Add_Flux(Time_Model%Additional_Data_Line(i_MV), &
            Time_Binary%Additional_Data_Line(i_MV))
          M_B_Prim = Add_Flux(M_B_Prim,M_B_Secon)
          M_U_Prim = Add_Flux(M_U_Prim,M_U_Secon)
          Time_Model%Additional_Data_Line(i_BV) = M_B_Prim - Time_Model%Additional_Data_Line(i_MV)
          Time_Model%Additional_Data_Line(i_UB) = M_U_Prim - M_B_Prim
          ! With the current data, it is not possible to compute the composite B1V1 magnitude. Unchanged.
          Time_Model%Data_Line(i_logL) = log10(10.d0**Time_Model%Data_Line(i_logL) + 10.d0**Time_Binary%Data_Line(i_logL))
        ! It is difficult to define a composite TEff. Unchanged.
        endif
        Cluster_mass = Cluster_mass + Mass_Binary
      case default
        write(*,*) 'Unexpected error...'
        stop
    end select

    ! If the mass of the secondary is smaller than the minimal IMF mass, we change the tag.
    if (Time_Model%Is_a_Binary == 1 .and. Too_Small) then
      Time_Model%Is_a_Binary = 2
    endif

    return

  end subroutine Binary
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real(kind=8) function Add_Flux(Flux_Prim,Flux_Secon)
    ! Sum two fluxes in magnitude units.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    real(kind=8), intent(in):: Flux_Prim, Flux_Secon

    Add_Flux = -2.5d0*log10(10.d0**(Flux_Prim/(-2.5d0))+10.d0**(Flux_Secon/(-2.5d0)))

    return

  end function Add_Flux
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine  Add_Noise(Time_Model)
    ! Add a Gaussian independant noise on the couple L-Teff and MV-B-V. The method is a Box-M�ller method.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use DataStructure, only: type_TimeModel,i_MV_noisy,i_BV_noisy,i_MV,i_BV
    use VariousParameters, only: inoise,sigma_mv,sigma_bv
    use Constant, only: pi

    implicit none

    type(type_TimeModel), intent(inout)::Time_Model

    real(kind=8)::A,B,Noise_MV,Noise_BV


    select case (inoise)
      case (0)
        ! No noise
        Noise_MV = 0.d0
        Noise_BV = 0.d0

      case (1)
        ! Generation of two random numbers A and B.
        call random_number(A)
        call random_number(B)

        ! Computation of the noise in the M_V vs B-V plane.
        Noise_MV = sqrt(-2.d0*log(A))*cos(2.d0*pi*B)*sigma_mv
        Noise_BV = sqrt(-2.d0*log(A))*sin(2.d0*pi*B)*sigma_bv

      case default
        write(*,*) 'Unexpected value of i_noise...'
        stop
    end select

    ! Add the noise.
    Time_Model%Additional_Data_Line(i_MV_noisy) = Time_Model%Additional_Data_Line(i_MV) + Noise_MV
    Time_Model%Additional_Data_Line(i_BV_noisy) = Time_Model%Additional_Data_Line(i_BV) + Noise_BV

    return

  end subroutine Add_Noise
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine  Initialise_Position_and_factor(Z_Position,Z_factor,mass_Position,mass_factor,omega_Position, &
    omega_factor)
    ! Initialise the arrays.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    integer, dimension(2,2), intent(inout)::omega_Position
    integer, dimension(2), intent(inout)::mass_Position
    integer, intent(inout)::Z_Position

    real(kind=8), dimension(2,2), intent(inout)::omega_factor
    real(kind=8), dimension(2), intent(inout)::mass_factor
    real(kind=8), intent(inout)::Z_factor

    omega_Position(:,:) = -1
    mass_Position(:) = -1
    Z_Position = -1
    omega_factor(:,:) = -1.d0
    mass_factor(:) = -1.d0
    Z_factor = -1.d0

  end subroutine Initialise_Position_and_factor
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module InterpolationLoop
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module Configuration_File
  ! module containing the routines used to search, read and write the configuration file. If a
  ! configuration file exists in the repository, the value of the various parameters are read as during
  ! the last uasge of this program in the current repository.
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  use VariousParameters, only: grid,star_number,i_metallicity,ivdist,iangle,inoise,IMF_type,Fixed_AoV_latitude, &
    m_IMF_inf,m_IMF_sup,fixed_metallicity,om_ivdist,binary_prob,sigma_mv,sigma_bv, &
    Colour_Calibration_mode, limb_dark,grav_dark,PMS,table_format
  use Population_Mode, only: Pop_Mass_Beam_Number,Pop_Omega_Beam_Number,N_Time_step

  implicit none

  integer, parameter:: Unit_Config_File = 100

  character(*), parameter::Config_FileName=".Config_SYCLIST"

  public::Config
  public::Write_Config

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Config
    ! Search a local config file. If it exists, read the values
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use VariousParameters, only: Std_Path,Std_Path_tables

    implicit none

    integer,parameter:: Unit_Config_Path = 110
    integer:: ierror

    character(256):: Config_FilePath,root_path

    call getenv("HOME",root_path)

    Config_FilePath = trim(root_path)//'/.SYCLIST'

    open(Unit_Config_Path,file=Config_FilePath,iostat=ierror,status='old')
    if (ierror /= 0) then
      ! No configuration file found
      open(Unit_Config_Path,file=Config_FilePath,iostat=ierror,status='new')
      write(*,*) 'Enter the path to the inputs directory (no trailing "/"): '
      read(5,*) Std_Path
      write(Unit_Config_Path,'(a)') Std_Path
    else
      read(Unit_Config_Path,'(a)') Std_Path
    endif
    close(Unit_Config_Path)
    Std_Path_tables = trim(Std_Path)//'/tables/'
    Std_Path = trim(Std_Path)//'/inputs/'

    open(Unit_Config_File,file=Config_FileName,iostat=ierror,status='old')
    if (ierror /= 0) then
      ! No configuration file found
      return
    endif

    read(Unit_Config_File,*)
    read(Unit_Config_File,*)
    read(Unit_Config_File,*)
    read(Unit_Config_File,*)

    read(Unit_Config_File,'(8x,a)',iostat=ierror) grid
    if (ierror /= 0) then
      grid = 'BeGrids'
    endif
    read(Unit_Config_File,'(16x,i1)',iostat=ierror) table_format
    if (ierror /= 0) then
      table_format = 1
    endif
    read(Unit_Config_File,'(8x,l1)',iostat=ierror) PMS
    if (ierror /= 0) then
      PMS = .false.
    endif
    read(Unit_Config_File,'(15x,i9)',iostat=ierror) star_number
    if (ierror /= 0) then
      star_number = 1000
    endif
    read(Unit_Config_File,'(28x,i1)',iostat=ierror) i_metallicity
    if (ierror /= 0) then
      i_metallicity = 0
    endif
    read(Unit_Config_File,'(15x,f6.4)',iostat=ierror) fixed_metallicity
    if (ierror /= 0) then
      fixed_metallicity = 0.014d0
    endif
    read(Unit_Config_File,'(12x,i1)',iostat=ierror) IMF_type
    if (ierror /= 0) then
      IMF_type = 1
    endif
    read(Unit_Config_File,'(16x,f9.4)',iostat=ierror) m_IMF_inf
    if (ierror /= 0) then
      m_IMF_inf = 0.8d0
    endif
    read(Unit_Config_File,'(16x,f9.4)',iostat=ierror) m_IMF_sup
    if (ierror /= 0) then
      m_IMF_sup = 120.d0
    endif
    read(Unit_Config_File,'(25x,i1)',iostat=ierror) ivdist
    if (ierror /= 0) then
      ivdist = 2
    endif
    read(Unit_Config_File,'(26x,f6.4)',iostat=ierror) om_ivdist
    if (ierror /= 0) then
      om_ivdist = 0.4d0
    endif
    read(Unit_Config_File,'(30x,i1)',iostat=ierror) iangle
    if (ierror /= 0) then
      iangle = 0
    endif
    read(Unit_Config_File,'(25x,f7.2)',iostat=ierror) Fixed_AoV_latitude
    if (ierror /= 0) then
      Fixed_AoV_latitude = 22.d0
    endif
    read(Unit_Config_File,'(25x,i1)',iostat=ierror) inoise
    if (ierror /= 0) then
      inoise = 0
    endif
    read(Unit_Config_File,'(19x,f7.4)',iostat=ierror) sigma_mv
    if (ierror /= 0) then
      sigma_mv = 0.01d0
    endif
    read(Unit_Config_File,'(19x,f7.4)',iostat=ierror) sigma_bv
    if (ierror /= 0) then
      sigma_bv = 0.0025d0
    endif
    read(Unit_Config_File,'(22x,f7.2)',iostat=ierror) binary_prob
    if (ierror /= 0) then
      binary_prob = 0.d0
    endif
    read(Unit_Config_File,'(39x,i5)',iostat=ierror) Pop_Mass_Beam_Number
    if (ierror /= 0) then
      Pop_Mass_Beam_Number = 2000
    endif
    read(Unit_Config_File,'(43x,i5)',iostat=ierror) Pop_Omega_Beam_Number
    if (ierror /= 0) then
      Pop_Omega_Beam_Number = 100
    endif
    read(Unit_Config_File,'(30x,i5)',iostat=ierror) Colour_Calibration_mode
    if (ierror /= 0) then
      Colour_Calibration_mode = 2
    endif
    read(Unit_Config_File,'(20x,i5)',iostat=ierror) grav_dark
    if (ierror /= 0) then
      grav_dark = 2
    endif
    read(Unit_Config_File,'(17x,i5)',iostat=ierror) limb_dark
    if (ierror /= 0) then
      limb_dark = 0
    endif
    read(Unit_Config_File,'(14x,i9)',iostat=ierror) N_Time_step
    if (ierror /= 0) then
      N_Time_step = 500
    endif

    close(Unit_Config_File)

    return

  end subroutine Config
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine Write_Config
  ! Save the config file locally.
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    implicit none

    integer:: ierror

    open(Unit_Config_File,file=Config_FileName,iostat=ierror,status='unknown')
    if (ierror /= 0) then
      ! No configuration file found
      write(*,*) 'Problem writing configuration file, aborting...'
      stop
    endif

    write(Unit_Config_File,'(a)') '*******************************'
    write(Unit_Config_File,'(a)') 'Configuration file for SYCLIST'
    write(Unit_Config_File,'(a)') 'Do not edit unless knowing what you are doing !'
    write(Unit_Config_File,'(a)') '*******************************'

    write(Unit_Config_File,'(a,2x,a)') 'Grid: ', grid
    write(Unit_Config_File,'(a,2x,i1)') 'Table format: ',table_format
    write(Unit_Config_File,'(a,2x,l1)') 'PMS:  ',PMS
    write(Unit_Config_File,'(a,2x,i9)') 'Star Number: ',star_number
    write(Unit_Config_File,'(a,2x,i1)') 'Metallicity distribution: ',i_metallicity
    write(Unit_Config_File,'(a,2x,f6.4)') 'Metallicity: ', fixed_metallicity
    write(Unit_Config_File,'(a,2x,i1)') 'IMF_type: ', IMF_type
    write(Unit_Config_File,'(a,2x,f9.4)') 'Minimal mass: ',m_IMF_inf
    write(Unit_Config_File,'(a,2x,f9.4)') 'Maximal mass: ',m_IMF_sup
    write(Unit_Config_File,'(a,2x,i1)') 'Velocity distribution: ',ivdist
    write(Unit_Config_File,'(a,2x,f6.4)') 'Velocity value (dirac): ',om_ivdist
    write(Unit_Config_File,'(a,2x,i1)') 'Angle of view distribution: ',iangle
    write(Unit_Config_File,'(a,2x,f7.2)') 'Angle of view (dirac): ',Fixed_AoV_latitude
    write(Unit_Config_File,'(a,2x,i1)') 'Account for the noise: ',inoise
    write(Unit_Config_File,'(a,2x,f7.4)') 'Variance in M_V: ',sigma_mv
    write(Unit_Config_File,'(a,2x,f7.4)') 'Variance in B-V: ',sigma_bv
    write(Unit_Config_File,'(a,2x,f7.2)') 'Binary probability: ',binary_prob
    write(Unit_Config_File,'(a,2x,i5)') 'Number of beam in mass (population): ',Pop_Mass_Beam_Number
    write(Unit_Config_File,'(a,2x,i5)') 'Number of beam in velocity (population): ',Pop_Omega_Beam_Number
    write(Unit_Config_File,'(a,2x,i5)') 'Colour - Teff calibration: ',Colour_Calibration_mode
    write(Unit_Config_File,'(a,2x,i5)')  'Gravity Darkening:', grav_dark
    write(Unit_Config_File,'(a,2x,i5)')  'Limb Darkening:', limb_dark
    write(Unit_Config_File,'(a,2x,i9)')  'Time Steps: ', N_Time_step

    close(Unit_Config_File)

    return

  end subroutine
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module Configuration_File
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
program PopStarII
  ! Main part of the programm. Calls various modules.
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  use DataStructure, only: verbose,Del_DataStructure
  use VariousParameters, only:Comp_Mode,init_AoV,ivdist,iangle,All_Data_Array,Z_Number,mass_Number_array, &
                              omega_Number_array,IMF_type
  use random, only:init_random,Init_Kroupa_IMF
  use ReadData, only:init_Huang,init_HG,init_Correction,init_VcritOmega,init_SurfaceOmega, &
                     init_Correct_fact,init_angle_external
  use InOut, only:Intro,AskChange,IsochroneMode
  use InterpolationLoop, only:MainLoop
  use Configuration_File, only:Config,Write_Config

  implicit none

  integer:: argtot,iargc,i,j,k
  character(256):: argv

  argtot = iargc()
  if (argtot == 0) then
    verbose = .false.
  else if (argtot == 1) then
    call getarg(1,argv)
    if (trim(argv) == '0') then
      verbose = .false.
    else if (trim(argv) == '1') then
      verbose = .true.
    else
      stop 'Wrong argument for verbose mode'
    endif
  endif

  ! Search for a configuration file
  call Config

  ! Reading data
  call init_Huang
  call init_HG
  call init_Correction
  call init_VcritOmega
  call init_SurfaceOmega
  call init_Correct_fact

  ! Introduction and choice of parameters
  call Intro
  call AskChange
  call init_AoV
  if (iangle == 4) then
    call init_angle_external
  endif
  if (IMF_type == 2) then! Initialise the Kroupa IMF
    call Init_Kroupa_IMF()
  endif

  if (Comp_Mode == 2) then
    call IsochroneMode
  endif

  ! Initialises the seed for random numbers generation
  call init_random

  ! Beginning of the main loop
  call MainLoop

  ! Save the configuration file
  call Write_Config

  ! Delete the arrays:
  do i=1,Z_Number
    do j=1,mass_Number_array(i)
      do k=1,omega_Number_array(i,j)
        call Del_DataStructure(All_Data_Array(i,j,k))
      enddo
    enddo
  enddo

  print*, char(7)

end program PopStarII
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
