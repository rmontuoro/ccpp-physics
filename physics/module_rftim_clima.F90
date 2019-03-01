module rftim_clima

   use machine, only : kind_phys

   implicit none

   private

   public rfti_rh_clima, rfti_ws_clima

contains

   elemental function rfti_rh_clima(lat,lon,rh) result(rfti_rh)

      real(kind_phys), intent(in) :: lat
      real(kind_phys), intent(in) :: lon
      real(kind_phys), intent(in) :: rh
      integer                     :: rfti_rh

      ! This simplified version doesn't take into account
      ! the geographical dependency (lat,lon) and returns
      ! the climatology for the Modified Red Flag Threat
      ! Index at the Rick Husband Amarillo International
      ! Airport (KAMA), Amarillo, TX, for the 2m relative
      ! humidity component in percent.
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

   elemental function rfti_ws_clima(lat,lon,ws) result(rfti_ws)

      real(kind_phys), intent(in) :: lat
      real(kind_phys), intent(in) :: lon
      real(kind_phys), intent(in) :: ws
      integer                     :: rfti_ws

      ! This simplified version doesn't take into account
      ! the geographical dependency (lat,lon) and returns
      ! the climatology for the Modified Red Flag Threat
      ! Index at the Rick Husband Amarillo International
      ! Airport (KAMA), Amarillo, TX, for the 6m wind
      ! speed component in miles per hour.
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
