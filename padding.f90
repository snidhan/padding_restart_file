Program Main
  implicit none
  
  
  !number of grid points with ghost cells
!  integer ( kind = 4 ) :: nx,ny,nz,nxe,nye,nze

  integer ( kind = 4 ), parameter :: nx = 531
  integer ( kind = 4 ), parameter :: ny = 258
  integer ( kind = 4 ), parameter :: nz = 4610

  integer ( kind = 4 ), parameter :: nxe = 531
  integer ( kind = 4 ), parameter :: nye = 258
  integer ( kind = 4 ), parameter :: nze = 4610

  real    ( kind = 8 ) :: xu(nx),yv(ny),zw(nz),zwg(nz)
  real    ( kind = 8 ) :: xc(nx),yc(ny),zc(nz),zcg(nz)
  real    ( kind = 8 ) :: ru(nx),rp(nx)

  integer ( kind = 4 ) :: i,j,k,jp,nstep,imax,jmax,kmax,tmp,kstart,kend,s1,jend,expu
  integer ( kind = 4 ) :: tag
  real    ( kind = 8 ) :: time,dtm1,grav
  character(len=128)   :: p_filename,u_filename,u_filename_out,v_filename,v_filename_out,w_filename,w_filename_out,p_filename_out

  character(len=128)   :: dens_filename,dens_filename_out
  real    ( kind = 8 ) :: u(nx,ny,nz), v(nx,ny,nz), w(nx,ny,nz), p(nx,ny,nz), dens(nx,ny,nz)


!  real    ( kind = 8 ), allocatable, dimension(:,:,:) :: u,v,w,densf
!  real    ( kind = 8 ), allocatable, dimension(:,:,:) :: ue,ve,we,densfe


  ! In order to reduce the file size, the domain can be sliced in the streamwise and the radial direction
  ! kstart = 2
  ! kend = nz-1

   !jend = ny-5
 

  call read_grid(xu,yv,zw,zwg,xc,yc,zc,zcg,nx,ny,nz,nz,ru,rp,tag)

  print*, 'rp ', rp
  
!  allocate(w(1:nx,1:ny,1:nz),v(1:nx,1:ny,1:nz),u(1:nx,1:ny,1:nz),densf(1:nx,1:ny,1:nz))

!  allocate(we(1:nxe,1:nye,1:nze),ve(1:nxe,1:nye,1:nze),ue(1:nxe,1:nye,1:nze),densfe(1:nxe,1:nye,1:nze))


  u_filename = '/home/sheel/Work2/projects_data/spod_re5e4/fr10/resfiles/u_02490000.res' 
  u_filename_out = '/home/sheel/Work2/projects_data/spod_re5e4/fr10/resfiles/u_02490000_r_ex.res' 
 
  v_filename = '/home/sheel/Work2/projects_data/spod_re5e4/fr10/resfiles/v_02490000.res' 
  v_filename_out = '/home/sheel/Work2/projects_data/spod_re5e4/fr10/resfiles/v_02490000_r_ex.res' 
 
  w_filename = '/home/sheel/Work2/projects_data/spod_re5e4/fr10/resfiles/w_02490000.res' 
  w_filename_out = '/home/sheel/Work2/projects_data/spod_re5e4/fr10/resfiles/w_02490000_r_ex.res' 
 
  p_filename = '/home/sheel/Work2/projects_data/spod_re5e4/fr10/resfiles/p_02490000.res' 
  p_filename_out = '/home/sheel/Work2/projects_data/spod_re5e4/fr10/resfiles/p_02490000_r_ex.res' 
 
  dens_filename = '/home/sheel/Work2/projects_data/spod_re5e4/fr10/resfiles/dens_02490000.res' 
  dens_filename_out = '/home/sheel/Work2/projects_data/spod_re5e4/fr10/resfiles/dens_02490000_r_ex.res' 
 
  
  expu = 0

  w=0.0d0
  u=0.0d0
  v=0.0d0
  p=0.0d0
  dens=0.0d0

  call read_restart(u_filename,nx,ny,nz,u,time,dtm1,grav,jp,nstep)
  call read_restart(v_filename,nx,ny,nz,v,time,dtm1,grav,jp,nstep)
  call read_restart(w_filename,nx,ny,nz,w,time,dtm1,grav,jp,nstep)
  call read_restart(p_filename,nx,ny,nz,p,time,dtm1,grav,jp,nstep)
  call read_restart(dens_filename,nx,ny,nz,dens,time,dtm1,grav,jp,nstep)


 write(*,*) 'MINVAL(w),MAXVAL(w)', MINVAL(w),MAXVAL(w)
 write(*,*) 'MINVAL(u),MAXVAL(u)', MINVAL(u),MAXVAL(u)
 write(*,*) 'MINVAL(v),MAXVAL(v)', MINVAL(v),MAXVAL(v)
 write(*,*) 'MINVAL(p),MAXVAL(p)', MINVAL(p),MAXVAL(p)
 write(*,*) 'MINVAL(dens),MAXVAL(dens)', MINVAL(dens),MAXVAL(dens)


  ! extended points upstream 
  ! extended points downstream
  ! extended points radial direction

  do  k=1,nz
  do  j=1,ny
  do  i=nx-80,nx  

      u(i,j,k) = 0.0d0
      v(i,j,k) = 0.0d0
      w(i,j,k) = 1.0d0
      p(i,j,k) = 0.0d0
      dens(i,j,k) = -1*(rp(i))*sin(yc(j))


  enddo
  enddo
  enddo

 write(*,*) 'MINVAL(w),MAXVAL(w)', MINVAL(w),MAXVAL(w)
 write(*,*) 'MINVAL(u),MAXVAL(u)', MINVAL(u),MAXVAL(u)
 write(*,*) 'MINVAL(v),MAXVAL(v)', MINVAL(v),MAXVAL(v)
 write(*,*) 'MINVAL(p),MAXVAL(p)', MINVAL(p),MAXVAL(p)
 write(*,*) 'MINVAL(dens),MAXVAL(dens)', MINVAL(dens),MAXVAL(dens)


  call write_restart(u_filename_out,nxe,nye,nze,u,time,dtm1,grav,jp,nstep)
  call write_restart(v_filename_out,nxe,nye,nze,v,time,dtm1,grav,jp,nstep)
  call write_restart(w_filename_out,nxe,nye,nze,w,time,dtm1,grav,jp,nstep)
  call write_restart(p_filename_out,nxe,nye,nze,p,time,dtm1,grav,jp,nstep)
  call write_restart(dens_filename_out,nxe,nye,nze,dens,time,dtm1,grav,jp,nstep)

  !deallocate(ue,ve,we,densfe,u,v,w,densf)


  stop
end Program Main

subroutine read_grid(xu,yv,zw,zwg,xc,yc,zc,zcg,nx,ny,nz,nzg,ru,rp,tag)
  implicit none

  INTEGER nx,ny,nz,nzg,tag
  REAL ( kind = 8 ) :: xu(nx),yv(ny),zw(nz),zwg(nzg)
  REAL ( kind = 8 ) :: xc(nx),yc(ny),zc(nz),zcg(nzg)
  REAL ( kind = 8 ) :: ru(nx),rp(nx)

  INTEGER i,j,k

  real rdelx,rdely,rdelz, dtheta
  real, allocatable, dimension(:) :: cug,cvg

  ALLOCATE(cug(nzg),cvg(nzg))

  ! ! READ GRID

  OPEN(UNIT=1,FILE='./x1_grid.in',STATUS='OLD',FORM='FORMATTED')
  read(1,*) j
  do i= 1, nx-1
     read(1,*) j, xu(i)
     !write(6,*) "xu(",i,") = ", xu(i)
  enddo
  close(1)
  xc(2:nx-1) = .5*(xu(1:nx-2)+xu(2:nx-1))
  xc(1 ) = 2.*xu(1  )-xc(2  )
  xc(nx) = 2.*xu(nx-1)-xc(nx-1)

  do i= 2, nx-1
     write(6,*) "xc(",i,") = ", xc(i)
  enddo


  OPEN(UNIT=1,FILE='./x2_grid.in',STATUS='OLD',FORM='FORMATTED')
  read(1,*) j
  do i= 1, ny-1
     read(1,*) j, yv(i)
     !write(6,*) "yv(",i,") = ", yv(i)
  enddo
  close(1)
  yc(2:ny-1) = .5*(yv(1:ny-2)+yv(2:ny-1))
  yc(1 ) = 2.*yv(1  )-yc(2  )
  yc(ny) = 2.*yv(ny-1)-yc(ny-1)

  do i= 2, ny-1
     write(6,*) "yc(",i,") = ", yc(i)
  enddo


  OPEN(UNIT=1,FILE='./x3_grid.in',STATUS='OLD',FORM='FORMATTED')
  read(1,*) j
  do i= 1, nz-1
     read(1,*) j, zwg(i)
     !write(6,*) "zwg(",i,") = ", zwg(i)
  enddo
  close(1)
  zcg(2:nz-1) = .5*(zwg(1:nz-2)+zwg(2:nz-1))
  zcg(1  )= 2.*zwg(1  )-zcg(2  )
  zcg(nz) = 2.*zwg(nz-1)-zcg(nz-1)

  do i= 2, nz-1
     write(6,*) "zcg(",i,") = ", zcg(i)
  enddo


  ru(1:nx)=xu(1:nx) 
  rp(1:nx)=xc(1:nx)

  close(1)

  write(6,*) "READ GRID DONE"

  return
end subroutine read_grid

subroutine read_restart(filename,nx,ny,nz,var,time,dtm1,grav,jp,nstep)
  implicit none
  
  character(len=128)   :: filename
  integer ( kind = 4 ) :: i,j,k,jp,nx,ny,nz,nstep
  real    ( kind = 8 ) :: time,DTM1,grav
  real    (kind = 8) :: var(nx,ny,nz)

  ! READ RESTART FILE
  OPEN(19,FILE=TRIM(filename),STATUS='UNKNOWN',FORM='UNFORMATTED')  
  READ(19) I,J,K,JP 
  write(6,*) "I,J,K,JP = ", I,J,K,JP 
  DO K=1,NZ
     READ(19) ((var(I,J,K),I=1,NX),J=1,NY)
  ENDDO
  READ(19) nstep
  READ(19) TIME
  !write(6,*) 'time=',time
  READ(19) DTM1,grav
  CLOSE(19)
  write(6,*) "READING RESTART FILE DONE"
  write(6,*) MAXVAL(var),MINVAL(var)
    
  return
end subroutine read_restart


subroutine write_restart(filename,nx,ny,nz,var,time,dtm1,grav,jp,nstep)
  implicit none
  
  character(len=128)   :: filename
  integer ( kind = 4 ) :: i,j,k,jp,nx,ny,nz,nstep
  real    ( kind = 8 ) :: var(nx,ny,nz),time,DTM1,grav

  write(6,*) "nx,ny,nz = ", nx,ny,nz

  i=nx
  j=ny
  k=nz
  jp=1

  ! READ RESTART FILE
  OPEN(20,FILE=TRIM(filename),STATUS='UNKNOWN',FORM='UNFORMATTED')  
  WRITE(20) I,J,K,JP 
!  write(6,*) "I,J,K,JP = ", I,J,K,JP 
  DO K=1,NZ
!     write(6,*) " WRITE K = ", K
     write(20) ((var(I,J,K),I=1,NX),J=1,NY)
  ENDDO
  WRITE(20) nstep
  WRITE(20) TIME
!  WRITE(6,*) 'time=',time
  WRITE(20) DTM1,grav
  CLOSE(20)
  write(6,*) "WRITING RESTART FILE DONE"
  
  return
end subroutine write_restart
