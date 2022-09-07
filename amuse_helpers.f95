module Amuse_Helpers
  use VariousParameters, only: grid,star_number,i_metallicity,ivdist,iangle,inoise,IMF_type,Fixed_AoV_latitude, &
    m_IMF_inf,m_IMF_sup,fixed_metallicity,om_ivdist,binary_prob,sigma_mv,sigma_bv, &
    Colour_Calibration_mode, limb_dark,grav_dark,PMS,table_format
  use VariousParameters, only:Current_Number,SN_Number,Small_Number,Cepheid_Number,FastRot_Number,Cluster_mass,Compute

  use Population_Mode, only: Pop_Mass_Beam_Number,Pop_Omega_Beam_Number,N_Time_step

  implicit none

  logical,private,save::iso_initialise
  logical,private,save::Binary_Star                                  ! Used in the binary loop
  real(kind=8),private,save::initial_mass_isochrone,dm_isochrone,dm_isochrone_ini,dm_isochrone_min
  integer, private::Number_of_Points_isochrone
  integer, private::Max_star_number_isomode = 100000

  public::Amuse_Config
  public::Amuse_SingleModelMode

  contains

  subroutine Amuse_Config
    ! Greatly simplified config: only set default values that should always work.
    ! These should all have AMUSE getters/setters.
    use VariousParameters, only: Std_Path,Std_Path_tables

    implicit none

    Std_Path = './src/SYCLIST'  ! Root path for Syclist data
    Std_Path_tables = trim(Std_Path)//'/tables/'
    Std_Path = trim(Std_Path)//'/inputs/'

    !grid = 'BeGrids'
    grid = 'Grids2012'
    table_format = 1
    PMS = .false.
    star_number = 1
    i_metallicity = 0
    fixed_metallicity = 0.014d0
    IMF_type = 1
    m_IMF_inf = 0.8d0
    m_IMF_sup = 120.d0
    ivdist = 2
    om_ivdist = 0.4d0
    iangle = 0
    Fixed_AoV_latitude = 22.d0
    inoise = 0
    sigma_mv = 0.01d0
    sigma_bv = 0.0025d0
    binary_prob = 0.d0
    Pop_Mass_Beam_Number = 2000
    Pop_Omega_Beam_Number = 100
    Colour_Calibration_mode = 2
    grav_dark = 2
    limb_dark = 0
    N_Time_step = 500

    return
  end subroutine

  subroutine Amuse_SingleModelMode
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

  end subroutine Amuse_SingleModelMode
end module
