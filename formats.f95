! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module DataStructure
  ! Define the new type DataStructure
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  implicit none

  integer,parameter,public::Table_Line_Number_normal = 400
  integer,parameter,public::Table_Line_Number_PMS = 500
  integer,public,save::Table_Line_Number = 400
  integer,parameter,private::Data_Number_GE = 42    ! WARNING : adapt this value to the number of data hereafter:
  integer,parameter,private::Data_Number_GECep = 50    ! WARNING : adapt this value to the number of data hereafter:
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
  integer,parameter,private::i_time_GECep=1,i_mass_GECep=2,i_logL_GECep=3,i_logTeff_corr_GECep=4, &
    i_H1_surf_GECep=5,i_He4_surf_GECep=6,i_C12_surf_GECep=7,i_C13_surf_GECep=8,i_N14_surf_GECep=9, &
    i_O16_surf_GECep=10,i_O17_surf_GECep=11,i_O18_surf_GECep=12,i_Ne20_surf_GECep=13,i_Ne22_surf_GECep=14, &
    i_Al26_surf_GECep=15,i_Mcc_GECep=16,i_logTeff_GECep=17,i_Mdot_GECep=18,i_rhoc_GECep=19,i_Tc_GECep=20, &
    i_H1_cen_GECep=21,i_He4_cen_GECep=22,i_C12_cen_GECep=23,i_C13_cen_GECep=24,i_N14_cen_GECep=25, &
    i_O16_cen_GECep=26,i_O17_cen_GECep=27,i_O18_cen_GECep=28,i_Ne20_cen_GECep=29,i_Ne22_cen_GECep=30, &
    i_Al26_cen_GECep=31,i_Omega_surf_GECep=32,i_Omega_cen_GECep=33,i_oblat_GECep=34,i_Mdot_enhencement_GECep=35, &
    i_v_crit1_GECep=36,i_v_crit2_GECep=37,i_v_equa_GECep=38,i_Omega_Omcrit_GECep=39,i_Gamma_Ed_GECep=40, &
    i_Mdot_mec_GECep=41,i_L_tot_GECep=42,i_crossing_nb_F_GECep=43,i_crossing_nb_1O_GECep=44,i_P_F_GECep=45, &
    i_Pdot_P_F_GECep=46,i_omi_omr_F_GECep=47,i_P_1O_GECep=48,i_Pdot_P_1O_GECep=49,i_omi_omr_1O_GECep=50
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
    i_Al27_cen=-1,i_Si28_cen=-1,i_crossing_nb_F=-1,i_crossing_nb_1O=-1,i_P_F=-1, &
    i_Pdot_P_F=-1,i_omi_omr_F=-1,i_P_1O=-1,i_Pdot_P_1O=-1,i_omi_omr_1O=-1
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
  integer,dimension(33),private::positive_GECep=(/i_H1_surf_GECep,i_He4_surf_GECep,i_C12_surf_GECep, &
    i_C13_surf_GECep,i_N14_surf_GECep,i_O16_surf_GECep,i_O17_surf_GECep,i_O18_surf_GECep,i_Ne20_surf_GECep, &
    i_Ne22_surf_GECep,i_Al26_surf_GECep,i_Mcc_GECep,i_H1_cen_GECep,i_He4_cen_GECep,i_C12_cen_GECep, &
    i_C13_cen_GECep,i_N14_cen_GECep,i_O16_cen_GECep,i_O17_cen_GECep,i_O18_cen_GECep,i_Ne20_cen_GECep, &
    i_Ne22_cen_GECep,i_Al26_cen_GECep,i_Omega_surf_GECep,i_Omega_cen_GECep,i_oblat_GECep, &
    i_Mdot_enhencement_GECep,i_v_crit1_GECep,i_v_crit2_GECep,i_v_equa_GECep,i_Omega_Omcrit_GECep, &
    i_Gamma_Ed_GECep,i_L_tot_GECep/)
  integer,dimension(25),private::less_than_one_GECep=(/i_H1_surf_GECep,i_He4_surf_GECep,i_C12_surf_GECep, &
    i_C13_surf_GECep,i_N14_surf_GECep,i_O16_surf_GECep,i_O17_surf_GECep,i_O18_surf_GECep,i_Ne20_surf_GECep, &
    i_Ne22_surf_GECep,i_Al26_surf_GECep,i_Mcc_GECep,i_H1_cen_GECep,i_He4_cen_GECep,i_C12_cen_GECep, &
    i_C13_cen_GECep,i_N14_cen_GECep,i_O16_cen_GECep,i_O17_cen_GECep,i_O18_cen_GECep,i_Ne20_cen_GECep, &
    i_Ne22_cen_GECep,i_Al26_cen_GECep,i_oblat_GECep,i_Omega_Omcrit_GECep/)
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
  character(*),parameter,private::ReadFormat_GECep = '(i3,1x,e22.15,1x,f11.6,2(1x,f9.6),2(1x,e14.7),1p,8(1x,e14.7),1x,e10.3,&
     &1x,0pf7.4,1x,f9.6,1x,f8.3,2(1x,f9.6),2(1x,e14.7),1p,8(1x,e14.7),5(1x,e10.3),3(1x,e9.2),&
     &0p,2(1x,f9.6),1x,1pe14.7,1x,e17.10,1x,0pf6.2,1x,f6.2,2(4x,f11.7,1x,e14.7,1x,e14.7))'
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
    else if (MyFormat == 3) then
    ! GENEC format with Cepheids data
      allocate(positive(size(positive_GECep)))
      allocate(less_than_one(size(less_than_one_GECep)))
      positive(:) = positive_GECep(:)
      less_than_one(:) = less_than_one_GECep(:)
      ReadFormat = ReadFormat_GECep
      Data_Number = Data_Number_GECep
      i_time = i_time_GECep
      i_mass = i_mass_GECep
      i_logL = i_logL_GECep
      i_logTeff_corr = i_logTeff_corr_GECep
      i_H1_Surf = i_H1_Surf_GECep
      i_He4_surf = i_He4_surf_GECep
      i_C12_surf = i_C12_surf_GECep
      i_C13_surf = i_C13_surf_GECep
      i_N14_surf = i_N14_surf_GECep
      i_O16_surf = i_O16_surf_GECep
      i_O17_surf = i_O17_surf_GECep
      i_O18_surf = i_O18_surf_GECep
      i_Ne20_surf = i_Ne20_surf_GECep
      i_Ne22_surf = i_Ne22_surf_GECep
      i_Al26_surf = i_Al26_surf_GECep
      i_Mcc = i_Mcc_GECep
      i_logTeff = i_logTeff_GECep
      i_Mdot = i_Mdot_GECep
      i_rhoc = i_rhoc_GECep
      i_Tc = i_Tc_GECep
      i_H1_cen = i_H1_cen_GECep
      i_He4_cen = i_He4_cen_GECep
      i_C12_cen = i_C12_cen_GECep
      i_C13_cen = i_C13_cen_GECep
      i_N14_cen = i_N14_cen_GECep
      i_O16_cen = i_O16_cen_GECep
      i_O17_cen = i_O17_cen_GECep
      i_O18_cen = i_O18_cen_GECep
      i_Ne20_cen = i_Ne20_cen_GECep
      i_Ne22_cen = i_Ne22_cen_GECep
      i_Al26_cen = i_Al26_cen_GECep
      i_Omega_surf = i_Omega_surf_GECep
      i_Omega_cen = i_Omega_cen_GECep
      i_oblat = i_oblat_GECep
      i_Mdot_enhencement = i_Mdot_enhencement_GECep
      i_v_crit1 = i_v_crit1_GECep
      i_v_crit2 = i_v_crit2_GECep
      i_v_equa = i_v_equa_GECep
      i_Omega_Omcrit = i_Omega_Omcrit_GECep
      i_Gamma_Ed = i_Gamma_Ed_GECep
      i_Mdot_mec = i_Mdot_mec_GECep
      i_L_tot = i_L_tot_GECep
      i_crossing_nb_F = i_crossing_nb_F_GECep
      i_crossing_nb_1O = i_crossing_nb_1O_GECep
      i_P_F = i_P_F_GECep
      i_Pdot_P_F = i_Pdot_P_F_GECep
      i_omi_omr_F = i_omi_omr_F_GECep
      i_P_1O = i_P_1O_GECep
      i_Pdot_P_1O = i_Pdot_P_1O_GECep
      i_omi_omr_1O = i_omi_omr_1O_GECep
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