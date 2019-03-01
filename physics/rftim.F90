module rftim

   use machine, only : kind_phys
   use rftim_clima, only : rfti_rh_clima, rfti_ws_clima

   implicit none

   private

   public :: rftim_init, rftim_run, rftim_finalize

contains

   subroutine rftim_init ()
      ! This would be the place to read a lat-lon dependent
      ! climatology of the Modified Red Flag Threat Index
      ! and set up the corresponding 2D arrays
   end subroutine rftim_init

   subroutine rftim_finalize ()
      ! This would be the place to deallocate any arrays
      ! that were allocated in subroutine rftim_init
   end subroutine rftim_finalize

!> \section arg_table_rftim_run Argument Table
!! | local_name | standard_name                      | long_name                                     | units         | rank | type      |    kind   | intent | optional |
!! |------------|------------------------------------|-----------------------------------------------|---------------|------|-----------|-----------|--------|----------|
!! | im         | horizontal_loop_extent             | horizontal loop extent                        | count         |    0 | integer   |           | in     | F        |
!! | lat        | latitude                           | latitude                                      | radians       |    1 | real      | kind_phys | in     | F        |
!! | lon        | longitude                          | longitude                                     | radians       |    1 | real      | kind_phys | in     | F        |
!! | islmsk     | sea_land_ice_mask                  | sea/land/ice mask (=0/1/2)                    | flag          |    1 | integer   |           | in     | F        |
!! | rh2        | relative_humidity_at_2m            | relative humidity 2m above ground             | percent       |    1 | real      | kind_phys | in     | F        |
!! | ws6        | wind_speed_at_6m_in_miles_per_hour | wind speed 6m above ground in miles per hour  | mi h-1        |    1 | real      | kind_phys | in     | F        |
!! | rftim      | modified_red_flag_threat_index     | modified red flag threat index                | index         |    1 | real      | kind_phys | out    | F        |
!! | errmsg     | ccpp_error_message                 | error message for error handling in CCPP      | none          |    0 | character | len=*     | out    | F        |
!! | errflg     | ccpp_error_flag                    | error flag for error handling in CCPP         | flag          |    0 | integer   |           | out    | F        |
!!
   subroutine rftim_run (im, lat, lon, islmsk, rh2, ws6, rftim, errmsg, errflg)
      ! Input
      integer,                          intent(in)  :: im
      real(kind_phys), dimension(1:im), intent(in ) :: lat
      real(kind_phys), dimension(1:im), intent(in ) :: lon
      integer,         dimension(1:im), intent(in ) :: islmsk
      real(kind_phys), dimension(1:im), intent(in ) :: rh2
      real(kind_phys), dimension(1:im), intent(in ) :: ws6
      ! Output
      real(kind_phys), dimension(1:im), intent(out) :: rftim
      character(len=*),                 intent(out) :: errmsg
      integer,                          intent(out) :: errflg
      ! DH* DEBUGGING
      ! Local variables
      integer :: i
      integer :: rftim_int_tmp

      ! Set CCPP error handling variables
      errmsg = ''
      errflg = 0

      !do i=1,im
      !   rftim(i) = real(rfti_rh_clima(lat(i), lon(i), rh2(i)) + rfti_ws_clima(lat(i), lon(i), ws6(i)), kind_phys)
      !end do

      where (islmsk == 1)
         rftim = real(rfti_rh_clima(lat, lon, rh2) + rfti_ws_clima(lat, lon, ws6), kind_phys)
      else where
         rftim = 0.0
      end where

      do i=1,im
         if (islmsk(i) == 1) then
            rftim_int_tmp = rfti_rh_clima(lat(i), lon(i), rh2(i)) + rfti_ws_clima(lat(i), lon(i), ws6(i))
         else
            rftim_int_tmp = 0
         end if
         write(0,'(a,2i5,3e12.4,i5)') "DH rftim_run: i, islmsk, rh2, ws6, rftim, rftim_int_tmp", &
                 i, islmsk(i), rh2(i), ws6(i), rftim(i), rftim_int_tmp
      end do

   end subroutine rftim_run

end module rftim