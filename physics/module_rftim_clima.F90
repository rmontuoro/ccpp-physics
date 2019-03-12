!>\file module_rftim_clima.F90
!! This file contains two functions rfti_rh_clima() and rfti_ws_clima()
!! for the modified Red Flag Threat Index (RFTI) calculation.

module rftim_clima

   use machine, only : kind_phys

   implicit none

   private

   public rfti_rh_clima, rfti_ws_clima

contains

!>\ingroup rftim_cal
!!This function calculates the climatological relative humidity to integer.
!!This simplified version doesn't take into account
!!the geographical dependency (lat,lon) and returns
!!the climatology for the Modified Red Flag Threat
!!Index at the Rick Husband Amarillo International
!!Airport (KAMA), Amarillo, TX, for the relative
!!humidity component in percent.
!!
!!\param lat  real, latitude in radiance
!!\param lon  real, longitude in radiance
!!\param rh   real, relative humidity in percentage
!!\param rfti_rh  integer, the climatologcal relative humidity
   elemental function rfti_rh_clima(lat,lon,rh) result(rfti_rh)

      real(kind_phys), intent(in) :: lat
      real(kind_phys), intent(in) :: lon
      real(kind_phys), intent(in) :: rh
      integer                     :: rfti_rh

      select case(nint(rh))
        case(:2)   
           rfti_rh = 5
        case(3:8)  
           rfti_rh = 4
        case(9:10) 
           rfti_rh = 3
        case(11:12)
           rfti_rh = 2
        case(13:15)
           rfti_rh = 1
        case(16:)  
           rfti_rh = 0
      end select

   end function rfti_rh_clima

!>\ingroup rftim_cal
!! This function calculates the climatological wind speed to integer.
!! This simplified version doesn't take into account
!! the geographical dependency (lat,lon) and returns
!! the climatology for the Modified Red Flag Threat
!! Index at the Rick Husband Amarillo International
!! Airport (KAMA), Amarillo, TX, for the 6m wind
!! speed component in miles per hour.
!!
!!\param lat latitude in radiance
!!\param lon longitude in radiance
!!\param ws 10-m wind speed
!!\param rfti_ws integer, the climatological 10-m wind speed
   elemental function rfti_ws_clima(lat,lon,ws) result(rfti_ws)

      real(kind_phys), intent(in) :: lat
      real(kind_phys), intent(in) :: lon
      real(kind_phys), intent(in) :: ws
      integer                     :: rfti_ws

      select case(nint(ws))
        case(:19)  
           rfti_ws = 0
        case(20:21)
           rfti_ws = 1
        case(22:24)
           rfti_ws = 2
        case(25:27)
           rfti_ws = 3
        case(28:41)
           rfti_ws = 4
        case(42:)  
           rfti_ws = 5
      end select

   end function rfti_ws_clima

end module rftim_clima
