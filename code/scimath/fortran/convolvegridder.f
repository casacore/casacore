      subroutine cgrd1d (ni, li, grid, value, support, sampling, posi,
     $     convFunc)
      implicit none
      integer ni
      integer li
      integer support, sampling
      complex value, grid(ni), nvalue
      double precision convFunc(*), norm
      double precision posi
      integer i, loci
      integer offi
      offi=nint((dble(nint(posi))-posi)*sampling)
      norm=0.0
      do i=-support,support
         loci=abs(sampling*i+offi)
         norm=norm+convFunc(loci+1)
      end do
      nvalue=nvalue/norm
      do i=-support,support
         loci=abs(sampling*i+offi)
         grid(i+li+1)=grid(i+li+1)+nvalue*convFunc(loci+1)
      end do
      return
      end
      subroutine cgrd2d (ni, nj, li, lj, grid, value, support,
     $     sampling, posi, posj, convFunc)
      implicit none
      integer ni, nj
      integer li, lj
      integer support, sampling
      complex value, grid(ni, nj), nvalue
      double precision convFunc(*), norm
      double precision posi, posj
      integer i, j, loci, locj
      integer offi, offj
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      norm=0.0
      do j=-support,support
         locj=abs(sampling*j+offj)
         do i=-support,support
	    loci=abs(sampling*i+offi)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)
         end do
      end do
      do j=-support,support
         locj=abs(sampling*j+offj)
         nvalue=value*convFunc(locj+1)/norm
         do i=-support,support
	    loci=abs(sampling*i+offi)
	    grid(i+li+1,j+lj+1)=grid(i+li+1,j+lj+1)+
     $           nvalue*convFunc(loci+1)
         end do
      end do
      return
      end
      subroutine cgrd3d (ni, nj, nk, li, lj, lk, grid, value, support,
     $     sampling, posi, posj, posk, convFunc)
      implicit none
      integer ni, nj, nk
      integer li, lj, lk
      integer support, sampling
      complex value, grid(ni, nj, nk), nvalue
      double precision convFunc(*), norm
      double precision posi, posj, posk
      integer i, j, k, loci, locj, lock
      integer offi, offj, offk
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      offk=nint((dble(nint(posk))-posk)*sampling)
      norm=0.0
      do k=-support,support
	lock=abs(sampling*k+offk)
	do j=-support,support
          locj=abs(sampling*j+offj)
	  do i=-support,support
	    loci=abs(sampling*i+offi)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)*
     $           convFunc(lock+1)
	  end do
	end do
      end do
      do k=-support,support
	lock=abs(sampling*k+offk)
	do j=-support,support
          locj=abs(sampling*j+offj)
          nvalue=value*convFunc(locj+1)*convFunc(lock+1)/norm
	  do i=-support,support
	    loci=abs(sampling*i+offi)
	    grid(i+li+1,j+lj+1,k+lk+1)=grid(i+li+1,j+lj+1,k+lk+1)+
     $           value*convFunc(loci+1)
	  end do
	end do
      end do
      return
      end
      subroutine cdgrd1d (ni, li, grid, value, support, sampling,
     $     posi, convFunc)
      implicit none
      integer ni
      integer li
      integer support, sampling
      complex value, grid(ni)
      double precision convFunc(*), norm
      double precision posi
      integer i, loci
      integer offi
      offi=nint((dble(nint(posi))-posi)*sampling)
      norm=0.0
      value=0.0
      do i=-support,support
         loci=abs(sampling*i+offi)
         value=value+grid(i+li+1)*convFunc(loci+1)
         norm=norm+convFunc(loci+1)
      end do
      value=value/norm
      return
      end
      subroutine cdgrd2d (ni, nj, li, lj, grid, value, support,
     $     sampling, posi, posj, convFunc)
      implicit none
      integer ni, nj
      integer li, lj
      integer support, sampling
      complex value, grid(ni, nj)
      double precision convFunc(*), norm
      double precision posi, posj
      integer i, j, loci, locj
      integer offi, offj
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      norm=0.0
      value=0.0
      do j=-support,support
         locj=abs(sampling*j+offj)
         do i=-support,support
	    loci=abs(sampling*i+offi)
	    value=value+
     $           grid(i+li+1,j+lj+1)*convFunc(loci+1)*convFunc(locj+1)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)
         end do
      end do
      value=value/norm
      return
      end
      subroutine cdgrd3d (ni, nj, nk, li, lj, lk, grid, value, 
     $     support, sampling, posi, posj, posk, convFunc)
      implicit none
      integer ni, nj, nk
      integer li, lj, lk
      integer support, sampling
      complex value, grid(ni, nj, nk)
      double precision convFunc(*), norm
      double precision posi, posj, posk
      integer i, j, k, loci, locj, lock
      integer offi, offj, offk
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      offk=nint((dble(nint(posk))-posk)*sampling)
      norm=0.0
      value=0.0
      do k=-support,support
	lock=abs(sampling*k+offk)
	do j=-support,support
          locj=abs(sampling*j+offj)
	  do i=-support,support
	    loci=abs(sampling*i+offi)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)*
     $           convFunc(lock+1)
	    value=value+grid(i+li+1,j+lj+1,k+lk+1)*convFunc(loci+1)*
     $           convFunc(locj+1)*convFunc(lock+1)
	  end do
	end do
      end do
      value=value/norm
      return
      end

      subroutine dgrd1d (ni, li, grid, value, support, sampling, posi,
     $     convFunc)
      implicit none
      integer ni
      integer li
      integer support, sampling
      double precision value, grid(ni), nvalue
      double precision convFunc(*), norm
      double precision posi
      integer i, loci
      integer offi
      offi=nint((dble(nint(posi))-posi)*sampling)
      norm=0.0
      do i=-support,support
         loci=abs(sampling*i+offi)
         norm=norm+convFunc(loci+1)
      end do
      nvalue=nvalue/norm
      do i=-support,support
         loci=abs(sampling*i+offi)
         grid(i+li+1)=grid(i+li+1)+nvalue*convFunc(loci+1)
      end do
      return
      end
      subroutine dgrd2d (ni, nj, li, lj, grid, value, support,
     $     sampling, posi, posj, convFunc)
      implicit none
      integer ni, nj
      integer li, lj
      integer support, sampling
      double precision value, grid(ni, nj), nvalue
      double precision convFunc(*), norm
      double precision posi, posj
      integer i, j, loci, locj
      integer offi, offj
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      norm=0.0
      do j=-support,support
         locj=abs(sampling*j+offj)
         do i=-support,support
	    loci=abs(sampling*i+offi)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)
         end do
      end do
      do j=-support,support
         locj=abs(sampling*j+offj)
         nvalue=value*convFunc(locj+1)/norm
         do i=-support,support
	    loci=abs(sampling*i+offi)
	    grid(i+li+1,j+lj+1)=grid(i+li+1,j+lj+1)+
     $           nvalue*convFunc(loci+1)
         end do
      end do
      return
      end
      subroutine dgrd3d (ni, nj, nk, li, lj, lk, grid, value, support,
     $     sampling, posi, posj, posk, convFunc)
      implicit none
      integer ni, nj, nk
      integer li, lj, lk
      integer support, sampling
      double precision value, grid(ni, nj, nk), nvalue
      double precision convFunc(*), norm
      double precision posi, posj, posk
      integer i, j, k, loci, locj, lock
      integer offi, offj, offk
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      offk=nint((dble(nint(posk))-posk)*sampling)
      norm=0.0
      do k=-support,support
	lock=abs(sampling*k+offk)
	do j=-support,support
          locj=abs(sampling*j+offj)
	  do i=-support,support
	    loci=abs(sampling*i+offi)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)*
     $           convFunc(lock+1)
	  end do
	end do
      end do
      do k=-support,support
	lock=abs(sampling*k+offk)
	do j=-support,support
          locj=abs(sampling*j+offj)
          nvalue=value*convFunc(locj+1)*convFunc(lock+1)/norm
	  do i=-support,support
	    loci=abs(sampling*i+offi)
	    grid(i+li+1,j+lj+1,k+lk+1)=grid(i+li+1,j+lj+1,k+lk+1)+
     $           value*convFunc(loci+1)
	  end do
	end do
      end do
      return
      end
      subroutine ddgrd1d (ni, li, grid, value, support, sampling,
     $     posi, convFunc)
      implicit none
      integer ni
      integer li
      integer support, sampling
      double precision value, grid(ni)
      double precision convFunc(*), norm
      double precision posi
      integer i, loci
      integer offi
      offi=nint((dble(nint(posi))-posi)*sampling)
      norm=0.0
      value=0.0
      do i=-support,support
         loci=abs(sampling*i+offi)
         value=value+grid(i+li+1)*convFunc(loci+1)
         norm=norm+convFunc(loci+1)
      end do
      value=value/norm
      return
      end
      subroutine ddgrd2d (ni, nj, li, lj, grid, value, support,
     $     sampling, posi, posj, convFunc)
      implicit none
      integer ni, nj
      integer li, lj
      integer support, sampling
      double precision value, grid(ni, nj)
      double precision convFunc(*), norm
      double precision posi, posj
      integer i, j, loci, locj
      integer offi, offj
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      norm=0.0
      value=0.0
      do j=-support,support
         locj=abs(sampling*j+offj)
         do i=-support,support
	    loci=abs(sampling*i+offi)
	    value=value+
     $           grid(i+li+1,j+lj+1)*convFunc(loci+1)*convFunc(locj+1)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)
         end do
      end do
      value=value/norm
      return
      end
      subroutine ddgrd3d (ni, nj, nk, li, lj, lk, grid, value, 
     $     support, sampling, posi, posj, posk, convFunc)
      implicit none
      integer ni, nj, nk
      integer li, lj, lk
      integer support, sampling
      double precision value, grid(ni, nj, nk)
      double precision convFunc(*), norm
      double precision posi, posj, posk
      integer i, j, k, loci, locj, lock
      integer offi, offj, offk
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      offk=nint((dble(nint(posk))-posk)*sampling)
      norm=0.0
      value=0.0
      do k=-support,support
	lock=abs(sampling*k+offk)
	do j=-support,support
          locj=abs(sampling*j+offj)
	  do i=-support,support
	    loci=abs(sampling*i+offi)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)*
     $           convFunc(lock+1)
	    value=value+grid(i+li+1,j+lj+1,k+lk+1)*convFunc(loci+1)*
     $           convFunc(locj+1)*convFunc(lock+1)
	  end do
	end do
      end do
      value=value/norm
      return
      end

      subroutine fgrd1d (ni, li, grid, value, support, sampling, posi,
     $     convFunc)
      implicit none
      integer ni
      integer li
      integer support, sampling
      real value, grid(ni), nvalue
      double precision convFunc(*), norm
      double precision posi
      integer i, loci
      integer offi
      offi=nint((dble(nint(posi))-posi)*sampling)
      norm=0.0
      do i=-support,support
         loci=abs(sampling*i+offi)
         norm=norm+convFunc(loci+1)
      end do
      nvalue=nvalue/norm
      do i=-support,support
         loci=abs(sampling*i+offi)
         grid(i+li+1)=grid(i+li+1)+nvalue*convFunc(loci+1)
      end do
      return
      end
      subroutine fgrd2d (ni, nj, li, lj, grid, value, support,
     $     sampling, posi, posj, convFunc)
      implicit none
      integer ni, nj
      integer li, lj
      integer support, sampling
      real value, grid(ni, nj), nvalue
      double precision convFunc(*), norm
      double precision posi, posj
      integer i, j, loci, locj
      integer offi, offj
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      norm=0.0
      do j=-support,support
         locj=abs(sampling*j+offj)
         do i=-support,support
	    loci=abs(sampling*i+offi)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)
         end do
      end do
      nvalue=value/norm
      do j=-support,support
         locj=abs(sampling*j+offj)
         nvalue=value*convFunc(locj+1)/norm
         do i=-support,support
	    loci=abs(sampling*i+offi)
	    grid(i+li+1,j+lj+1)=grid(i+li+1,j+lj+1)+
     $           nvalue*convFunc(loci+1)
         end do
      end do
      return
      end
      subroutine fgrd3d (ni, nj, nk, li, lj, lk, grid, value, support,
     $     sampling, posi, posj, posk, convFunc)
      implicit none
      integer ni, nj, nk
      integer li, lj, lk
      integer support, sampling
      real value, grid(ni, nj, nk), nvalue
      double precision convFunc(*), norm
      double precision posi, posj, posk
      integer i, j, k, loci, locj, lock
      integer offi, offj, offk
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      offk=nint((dble(nint(posk))-posk)*sampling)
      norm=0.0
      do k=-support,support
	lock=abs(sampling*k+offk)
	do j=-support,support
          locj=abs(sampling*j+offj)
	  do i=-support,support
	    loci=abs(sampling*i+offi)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)*
     $           convFunc(lock+1)
	  end do
	end do
      end do
      do k=-support,support
	lock=abs(sampling*k+offk)
	do j=-support,support
          locj=abs(sampling*j+offj)
          nvalue=value*convFunc(locj+1)*convFunc(lock+1)/norm
	  do i=-support,support
	    loci=abs(sampling*i+offi)
	    grid(i+li+1,j+lj+1,k+lk+1)=grid(i+li+1,j+lj+1,k+lk+1)+
     $           value*convFunc(loci+1)
	  end do
	end do
      end do
      return
      end
      subroutine fdgrd1d (ni, li, grid, value, support, sampling,
     $     posi, convFunc)
      implicit none
      integer ni
      integer li
      integer support, sampling
      real value, grid(ni)
      double precision convFunc(*), norm
      double precision posi
      integer i, loci
      integer offi
      offi=nint((dble(nint(posi))-posi)*sampling)
      norm=0.0
      value=0.0
      do i=-support,support
         loci=abs(sampling*i+offi)
         value=value+grid(i+li+1)*convFunc(loci+1)
         norm=norm+convFunc(loci+1)
      end do
      value=value/norm
      return
      end
      subroutine fdgrd2d (ni, nj, li, lj, grid, value, support,
     $     sampling, posi, posj, convFunc)
      implicit none
      integer ni, nj
      integer li, lj
      integer support, sampling
      real value, grid(ni, nj)
      double precision convFunc(*), norm
      double precision posi, posj
      integer i, j, loci, locj
      integer offi, offj
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      norm=0.0
      value=0.0
      do j=-support,support
         locj=abs(sampling*j+offj)
         do i=-support,support
	    loci=abs(sampling*i+offi)
	    value=value+
     $           grid(i+li+1,j+lj+1)*convFunc(loci+1)*convFunc(locj+1)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)
         end do
      end do
      value=value/norm
      return
      end
      subroutine fdgrd3d (ni, nj, nk, li, lj, lk, grid, value, 
     $     support, sampling, posi, posj, posk, convFunc)
      implicit none
      integer ni, nj, nk
      integer li, lj, lk
      integer support, sampling
      real value, grid(ni, nj, nk)
      double precision convFunc(*), norm
      double precision posi, posj, posk
      integer i, j, k, loci, locj, lock
      integer offi, offj, offk
      offi=nint((dble(nint(posi))-posi)*sampling)
      offj=nint((dble(nint(posj))-posj)*sampling)
      offk=nint((dble(nint(posk))-posk)*sampling)
      norm=0.0
      value=0.0
      do k=-support,support
	lock=abs(sampling*k+offk)
	do j=-support,support
          locj=abs(sampling*j+offj)
	  do i=-support,support
	    loci=abs(sampling*i+offi)
	    norm=norm+convFunc(loci+1)*convFunc(locj+1)*
     $           convFunc(lock+1)
	    value=value+grid(i+li+1,j+lj+1,k+lk+1)*convFunc(loci+1)*
     $           convFunc(locj+1)*convFunc(lock+1)
	  end do
	end do
      end do
      value=value/norm
      return
      end

