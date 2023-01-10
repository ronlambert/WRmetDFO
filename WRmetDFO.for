      PROGRAM WRmetDFO

c This preprocesses WR meteorological data for BlastDFO

      character rec*120,mo(12)*3,site*4,infile*60,outfile*60
      character fname*12,ascnt*4, yn*1, ver*4, fext*60
      character outfile_entered*60, OutVal*20
      character*6 calt, cdir, cspd, ct, cdp, cpress, crh
      character*6 cabshum, cden, cir, csos
      real time, alt, press, t, rh, dp, ir, spde, dir, abshum, sos
      real ilvl, sosd, sosm
      real delp1B, delp2B
      real hold, pold, told, rold, dpold, irold, sold, dold
      real irsav, mtoft, psitomb, fpstokts, tref, rhoconv
      real tval(900),zval(900),pval(900),rval(900),sosval(900)
      real dpval(900),sval(900),dval(900),irval(900),dnval(900)
      real iavg(900)
      integer ipos, ifilelen, iitwr, I, J, n, sflag, ihdr, ibad
      integer ialt

      data mtoft /3.2808/
      data fpstokts /0.5924838/
      data psitomb /68.94757/
      data tref /273.15/
      data rhoconv /515378./   ! rhoconv converts slugs/ft**3 to g/m**3
      data ver /' 1.0'/

c     Write sceen banner
      write(*,*)
      write(*,*) 
     $  '**************************************************************'
      write(*,*)
     $  '   WRmetDFO - Meteorological Data Preprocessor for BlastDFO'
      write(*,*) 
     $  '**************************************************************'
      write(*,*)

c     Read in file names
      write(*,*)
  10  write(*,*) 'Enter name of input met data file to be processed:'
      write(*,*)
      read(*,*,err=20) infile
      go to 30
  20  write(*,*)'  Warning - error reading file = ', infile
      go to 10
  30  continue
      open(2,file=infile,status='old',err=40)
      go to 50
  40  continue
      write(*,*)
      write(*,*)'  Warning -- Error opening met file = ', infile
      write(*,*)
      go to 10
  50  continue

c     Set default output file to same name as input plus "_dfo"
      ifilelen = len_trim(infile)
      ipos = index(infile,'.')
      if(ipos.eq.0) then
        ipos = ifilelen + 1
      endif
      fext = infile(ipos+1:ifilelen)
      outfile(1:ipos-1) = infile(1:ipos-1)
      ifext = len_trim(fext)
      outfile(ipos:ipos+ifext+1) = '_dfo.'
      ipos = index(outfile,'.')
      outfile(ipos+1:ipos+1+ifext) = fext(1:ifext)
 
c     Open a file for data output
  60  write(*,*)
      write(*,*) 'Enter the output BlastDFO met file name:'
      write(*,'(A,A)') ' Default = ', trim(outfile)
      read(*,'(A60)',err=60) outfile_entered
      if(len_trim(outfile_entered).gt.0) then
        outfile = outfile_entered
      endif
      open(1,file=outfile)

c     Set constants
      delp1B = 1.05
      delp2B = 2.05

c     Set local variables
      i = 0
      ibad = 0
      ihdr = 1
      sflag = 0
      site = '0000'
      n = 0

c     Read first line and check that input file starts with $
      read(2,'(A)') rec
      if(rec(1:1).ne.'$') then
        close(1)
        close(2)
        write(*,*) ' Met file does not start with $'
        write(*,*)
        goto 10
      endif
      write(1,'(A)') rec
      
c     Read in and output tower block header
      do J = 1, 2
        read(2,'(A120)') rec
        if(trim(rec).eq.'') then
          write(*,*) ' Error reading tower block header - blank line'
          close(1)
          close(2) 
          write(*,*)
          goto 10
        endif
        write(1,'(A)') rec
      enddo

c     Read tower block data
      do while (trim(rec).ne.'')
        read(2,'(A120)') rec
        if(trim(rec).ne.'') then
          read(rec,*,err=80) iitwr, alt, ilvl, spd, dir,  ! Save last tower read
     $      t, rh, dp, sosd, sosm, press
          write(1,'(A)') rec
        endif
      enddo
      write(1,'(A)') rec

c     Read and write lines to start of DASS block
      read(2,'(A120)') rec
      do while (rec(1:1).ne.'$')
        write(1,'(A)') rec
        read(2,*) rec
      enddo
      write(1,'(A)') ''
      write(1,'(A)') rec

c     Read and write lines to start of balloon block
      read(2,'(A120)') rec
      do while (rec(1:1).ne.'$')
        write(1,'(A)') rec
        read(2,'(A120)') rec
      enddo
      write(1,'(A)') rec

c     Read and write balloon header
      read(2,'(A120)') rec
      do while (index(rec,'ALT').eq.0)
        write(1,'(A)') rec
        read(2,'(A120)') rec
      enddo
      write(1,'(A)') rec

      den = -9.99
      i = i + 1
cc
cc  Save last tower data record for comparison with upper levels
cc
          n = n + 1
          zval(n)  = alt
          dval(n)  = dir
          sval(n)  = spd
          tval(n)  = t
          pval(n)  = press
          dnval(n) = den
          rval(n)  = rh
          dpval(n) = dp
          irval(n) = ir
          sosval(n) = sos
          told  = t
          hold  = alt
          pold  = press
          rold  = rh
          dpold = dp
          irold = ir
          sold  = spd
          dold  = dir
          dnold = den
          sosold = sos
          plast = press

c     Begin reading and modifying balloon data
      k = 1
  140 continue
        read(2,'(a120)',end=99)rec
        nlen = len_trim(rec)
        if (nlen .eq. 0) go to 140
        read(rec,*,err=150) alt, dir, spd, t, dp, press, rh, abshum,
     $    den, ir, sos
        go to 160
  150   continue
cc
cc  Bad data encountered while reading data
cc
          write(*,*)' Warning - bad data in record number =',i
          ibad = ibad + 1
          pause
  160   continue
        i = i + 1
        isave  = 0
c        write(*,*)' Processing data at height = ',alt
        delp = 14 - int(press/100)
        if(alt .lt. 600.0) then
          delp = delp1B
        endif
        if(alt .ge. 600.0 .and. alt .le. 1000.0) then
          delp = delp2B
        endif
cc
cc  Save data at approximately 1 mb pressure change for the first 10 mb of pressure drop.
cc  This is to ensure that adequate resolution is obtained in the lower few hundered feet for BLAST analyses.
cc
        if(press .gt. pval(1) - 10.0) then
          if(press .lt. plast - delp) then
            n = n + 1
            zval(n)  = alt
            dval(n)  = dir
            sval(n)  = spd
            tval(n)  = t
            pval(n)  = press
            dnval(n) = den
            rval(n)  = rh
            dpval(n) = dp
            irval(n) = ir
            sosval(n) = sos
            told  = t
            hold  = alt
            pold  = press
            rold  = rh
            dpold = dp
            irold = ir
            sold  = spd
            dold  = dir
            dnold = den
            sosold = sos
            plast = press
          endif
          go to 140
        endif
cc
cc  Save pressure level at delp millibar pressure intervals and insert in saved records if no temperature
cc  wind speed or wind direction slope changes are recorded within +/- delp millibar of this pressure state
cc
        if(press .lt. plast - delp .and. sflag .eq. 0) then
          tsav  = t
          hsav  = alt
          psav  = press
          rsav  = rh
          dpsav = dp
          irsav = ir
          ssav  = spd
          dsav  = dir
          dnsav = den
          sossav = sos
          sflag = 1
        endif
        tslopen = (t-told)/(alt-hold)
        sslopen = (spd-sold)/(alt-hold)
        if(abs(tslopen) . lt. 0.00001) tslopen = 0.0
        if(abs(sslopen) . lt. 0.00001) sslopen = 0.0
        delta = dir - dold
        if(abs(delta) .gt. 180.0) then
          if(delta .lt. 0) then
            delta = delta+360.0
          elseif(delta .gt. 0)then 
            delta = delta-360.0
          endif
        endif
        dslopen = delta/(alt-hold)
        if(abs(dslopen) . lt. 0.00001) dslopen = 0.0
        if(i .gt. 2) then

c         Check temperature
          if(tslopeo .gt. 0.0  .and. tslopen .lt. 0.0) then
             isave = 1
          elseif(tslopeo .lt. 0.0  .and. tslopen .gt. 0.0) then
             isave = 1
          endif

c         Check wind speed
          if(sslopeo .gt. 0.0  .and. sslopen .lt. 0.0) then
             isave = 1
          elseif(sslopeo .lt. 0.0  .and. sslopen .gt. 0.0) then
             isave = 1
          endif

c         Check wind direction
          if(dslopeo .gt. 0.0  .and. dslopen .lt. 0.0) then
             isave = 1
          elseif(dslopeo .lt. 0.0  .and. dslopen .gt. 0.0) then
             isave = 1
          endif
        endif
        pcheck = 1.5*(press/1000.0)
        if(isave .eq. 1) then
cc
cc  Test new level to see if it is greater than 'pcheck' mb from last saved level.
cc  If the new and last levels are within 1.5*(press/1000.0) mb, average the two levels and
cc  replace the last saved level.
cc
          diff = abs(press-plast)
          if(diff .le. pcheck) then
            if(iavg(n) .lt. 2) then
              zval(n)   = (zval(n) + alt)/2.0
              ddir = abs(dval(n)-dir)
              if (ddir .ge. 180.0) then
	          dval(n) = ((dval(n) + dir) - 360.0)/2.0
                if(dval(n) .lt. 0.0) dval(n) = dval(n) + 360.0
              else
	          dval(n) = (dval(n) + dir)/2.0
              endif
              sval(n)   = (sval(n) + spd)/2.0
              tval(n)   = (tval(n) + t)/2.0
              dpval(n)  = (dpval(n) + dp)/2.0
              pval(n)   = (pval(n) + press)/2.0
              rval(n)   = (rval(n) + rh)/2.0
              irval(n)  = (irval(n) + ir)/2.0
              dnval(n)  = (dnval(n) + den)/2.0
              sosval(n)  = (sosval(n) + sos)/2.0
              iavg(n) = iavg(n) + 1
            else
              isave = 0
            endif
          else
            diff = abs(plast-pold)
            if(diff .gt. pcheck) then
              n = n + 1
              zval(n)  = hold
              dval(n)  = dold
              sval(n)  = sold
              tval(n)  = told
              dpval(n) = dpold
              pval(n)  = pold
              rval(n)  = rold
              irval(n) = irold
              dnval(n) = dnold
              sosval(n) = sosold
            endif
          endif
          plast = pval(n)
          sflag = 0
        endif
  165   continue
        pref = plast - delp 
        if(sflag .eq. 1 .and. press .lt. pref) then
          n = n + 1
          zval(n)  = hsav
          dval(n)  = dsav
          sval(n)  = ssav
          tval(n)  = tsav
          dpval(n) = dpsav
          pval(n)  = psav
          rval(n)  = rsav
          irval(n) = irsav
          dnval(n) = dnsav
          sosval(n) = sossav
          plast = psav
          sflag = 0
        endif
        if(tslopen .ne. 0.0) tslopeo = tslopen
        sslopeo = sslopen
        dslopeo = dslopen
        told  = t
        hold  = alt
        pold  = press
        rold  = rh
        dpold = dp
        irold = ir
        sold  = spd
        dold  = dir
        dnold  = den
        sosold  = sos
        go to 140
   99 continue

c     Write output ballon data records
      do m = 2, n
        call PadVal(zval(m),'   alt',calt)
        call PadVal(dval(m),'   dir',cdir)
        call PadVal(sval(m),'   spd',cspd)
        call PadVal(pval(m),' press',cpress)
        call PadVal(rval(m),'    rh',crh)
        write(1,102)  calt, cdir, cspd,
     $     tval(m), dpval(m), cpress, crh,
     $     abshum, dnval(m), irval(m), sosval(m)
  102 format(A5,1X,A3,1X,A5,1X,2(F5.1,1X),A6,1X,A5,1X,F5.2,2X,
     $F7.2,1X,F6.1,1X,F5.1)
        if(dval(m) .gt. 360.0 .or. dval(m) .lt. 0.0) then
          write(*,*)' Warning -- wind direction error'
          pause
        endif
      enddo
  175 continue
      write(*,*)
      write(*,*)' End of file - processing completed'
      write(*,*)' Number of balloon data records processed = ',i - 1
      write(*,*)' Number bad data records encountered = ',ibad
      write(*,*)
      write(*,*)' Output written to file = ',outfile
      write(*,*)' Number of met levels saved = ',n - 1
      write(*,*)
      write(*,*)
      close(1)
      close(2)
      pause                       ! rrl temp
      stop

   80 continue
      write(*,*) ' Bad tower data record.  Closing program.'
      close(1)
      close(2)
      
      stop
      end

c*******************************************************************

      subroutine PadVal(Val,cType,OutVal)
c     Pads a balloon record input integer value with zeroes to give it the same look as the input file
      
      real Val
      integer iVal
      character OutVal*6, cType*6
      
      if(adjustl(cType).eq.'alt') then                       ! alt
        iVal = int(Val)
        if(iVal.lt.10) then
          write(OutVal,'(A,I1)') '0000', IVal
        elseif(iVal.lt.100) then
          write(OutVal,'(A,I2)') '000', IVal
        elseif (iVal.lt.1000) then
          write(OutVal,'(A,I3)') '00', IVal
        elseif (iVal.lt.10000) then
          write(OutVal,'(A,I4)') '0', IVal
        else
          write(OutVal,'(I5)') IVal
        endif
        OutVal = adjustl(OutVal)
        return
        
      elseif(adjustl(cType).eq.'dir') then                   ! dir
        iVal = int(Val)
        if(iVal.lt.10) then
          write(OutVal,'(A,I1)') '00', IVal
        elseif(iVal.lt.100) then
          write(OutVal,'(A,I2)') '0', IVal
        else
          write(OutVal,'(I3)') IVal
        endif
        OutVal = adjustl(OutVal)
        return
        
      elseif(adjustl(cType).eq.'spd') then                   ! spd
        if(Val.lt.10) then
          write(OutVal,'(A,F3.1)') '00', Val
        elseif(Val.lt.100) then
          write(OutVal,'(A,F4.1)') '0', Val
        else
          write(OutVal,'(F5.1)') Val
        endif
        OutVal = adjustl(OutVal)
        return
      elseif(adjustl(cType).eq.'t'.or.adjustl(cType).eq.'dp') then 
        if(Val.lt.0.0d0) then                                ! t and dp
          write(OutVal,'(F5.1)') Val
        elseif(Val.lt.10) then
          write(OutVal,'(A,F3.1)') '0', Val
        else
          write(OutVal,'(F4.1)') Val
        OutVal = adjustl(OutVal)
        return
        endif
      elseif(adjustl(cType).eq.'press') then                 ! press
        if(Val.lt.10) then
          write(OutVal,'(A,F3.1)') '000', Val
        elseif(Val.lt.100) then
          write(OutVal,'(A,F4.1)') '00', Val
        elseif(Val.lt.100) then
          write(OutVal,'(A,F5.1)') '0', Val
        elseif(Val.lt.1000) then
          write(OutVal,'(A,F5.1)') '0', Val
        else
          write(OutVal,'(F6.1)') Val
        endif
        OutVal = adjustl(OutVal)
        return
      elseif(adjustl(cType).eq.'rh') then                    ! rh
        if(Val.lt.10) then
          write(OutVal,'(A,F3.1)') '00', Val
        elseif(Val.lt.100) then
          write(OutVal,'(A,F4.1)') '0', Val
        else
          write(OutVal,'(F5.1)') Val
        endif
        OutVal = adjustl(OutVal)
        return
      elseif(adjustl(cType).eq.'abshum'.or.adjustl(cType).eq.'den') then
        write(OutVal,'(F5.2)') Val                          ! abshum
        return                                              !  and den
      elseif(adjustl(cType).eq.'ir') then
        write(OutVal,'(F6.1)') Val
        OutVal = adjustl(OutVal)
        return
      elseif(adjustl(cType).eq.'sos') then
        write(OutVal,'(F5.1)') Val
        OutVal = adjustl(OutVal)
        return
      else
        write(OutVal,'(A)') 'NaN'
        OutVal = adjustl(OutVal)
        return
      endif
      OutVal = adjustl(OutVal)
      return
      end


