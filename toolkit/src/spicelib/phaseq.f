C$Procedure PHASEQ ( Phase angle quantity between bodies centers )

      DOUBLE PRECISION FUNCTION PHASEQ ( ET,     TARGET, ILLMN,
     .                                   OBSRVR, ABCORR )

C$ Abstract
C
C     Compute the apparent phase angle for a target, observer,
C     illuminator set of ephemeris objects.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     EPHEMERIS
C     GEOMETRY
C     PHASE ANGLE
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'zzabcorr.inc'
      INCLUDE               'zzctr.inc'

      DOUBLE PRECISION      ET
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLMN
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         ABCORR

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB.
C     TARGET     I   Target body name.
C     ILLMN      I   Illuminating body name.
C     OBSRVR     I   Observer body.
C     ABCORR     I   Aberration correction flag.
C
C     The function returns the value of phase angle.
C
C$ Detailed_Input
C
C     ET       is the time in ephemeris seconds past J2000 TDB at which
C              to compute the phase angle.
C
C     TARGET   is the name of the target body. Optionally, you may
C              supply a string containing the integer ID code
C              for the object. For example both 'MOON' and '301'
C              are legitimate strings that indicate the Moon is the
C              target body. The TARGET string lack sensitivity to
C              case, leading and trailing blanks.
C
C     ILLMN    is the name of the illuminating body. Optionally, you may
C              supply a string containing the integer ID code
C              for the object. For example both 'SUN' and '10'
C              are legitimate strings that indicate the sun is the
C              illuminating body. The ILLMN string lack sensitivity
C              to case, leading and trailing blanks.
C
C              In most cases, ILLMN is the sun.
C
C     OBSRVR   is the name of the observer body. Optionally, you may
C              supply a string containing the integer ID code
C              for the object. For example both 'MOON' and '301'
C              are legitimate strings that indicate the Moon is the
C              observer body. The OBSRVR string lack sensitivity
C              to case, leading and trailing blanks.
C
C     ABCORR   is the string description of the aberration corrections
C              to apply to the state evaluations to account for one-way
C              light time and stellar aberration. The ABCORR string lack
C              sensitivity to case, leading and trailing blanks.
C
C              This routine accepts only reception mode aberration
C              corrections. See the header of SPKEZR for a detailed
C              description of the aberration correction options.
C              For convenience, the appropriate aberration options are
C              listed below:
C
C                 'NONE'     Apply no correction. Returns the "true"
C                            geometric state.
C
C                 'LT'       "Reception" case: correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'LT+S'     "Reception" case: correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'CN'       "Reception" case: converged
C                            Newtonian light time correction.
C
C                 'CN+S'     "Reception" case: converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C$ Detailed_Output
C
C     The function returns the optionally light-time corrected phase
C     angle between TARGET and ILLMN as observed from OBSRVR.
C
C     The range of the phase angle is [0, pi].
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the body name to SPICE ID look-up fails for any of the
C         TARGET, ILLMN, or OBSRVR names, the error
C         SPICE(IDCODENOTFOUND) is signaled.
C
C     2)  If the aberration correct, ABCORR, indicates a transmission
C         based correction, the error SPICE(INVALIDOPTION) is signaled.
C
C     3)  If the TARGET, ILLMN, and OBSRVR are not unique, the error
C         SPICE(BODIESNOTDISTINCT) is signaled.
C
C$ Files
C
C   Appropriate kernels must be loaded by the calling program before
C   this routine is called.
C
C   The following data are required:
C
C   -  SPK data: ephemeris data for the observer, illuminator, and
C      target must be loaded. If aberration corrections are used,
C      the states of the ephemeris bodies relative to the solar 
C      system barycenter must be calculable from the available
C      ephemeris data. Typically ephemeris data are made
C      available by loading one or more SPK files using FURNSH.
C
C   The following data may be required:
C
C   -  Frame data: if a frame definition not built into SPICE is
C      required, that definition must be available in the kernel 
C      pool. Typically frame definitions are supplied by loading a
C      frame kernel using FURNSH.
C
C   -  Orientation data: if a CK based frame is used in this routine's
C      state computation, then at least one CK and corresponding SCLK
C      kernel is required. If dynamic frames are used, additional
C      SPK, PCK, CK, or SCLK kernels may be required.
C
C$ Particulars
C
C     This routine returns the phase angle using the location of the
C     bodies (if point objects) or the center of the bodies (if finite
C     bodies).
C
C
C
C                       ILLMN     OBSRVR
C       ILLMN as seen      ^       /
C       from TARGET at     |      /
C       ET - LT.           |     /
C                         >|..../< phase angle
C                          |   /
C                        . |  /
C                      .   | /
C                     .    |v        TARGET as seen from OBSRVR
C               SEP   .  TARGET      at ET
C                      .  /
C                        /
C                       v
C
C
C
C        PI = SEP + PHASE
C
C        so
C
C        PHASE = PI - SEP
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Determine the time windows from December 1, 2006 UTC to
C        January 31, 2007 UTC for which the sun-moon-earth configuration
C        phase angle satisfies the relation conditions with respect to a
C        reference value of 0.57598845 radians (the phase angle at
C        January 1, 2007 00:00:00.000 UTC, 33.001707 degrees). Also
C        determine the time windows corresponding to the local maximum
C        and minimum phase angles, and the absolute maximum and minimum
C        phase angles during the search interval. The configuration
C        defines the Sun as the illuminator, the Moon as the target, and
C        the Earth as the observer.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: phaseq_ex1.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              de421.bsp                     Planetary ephemeris
C              pck00009.tpc                  Planet orientation and
C                                            radii
C              naif0009.tls                  Leapseconds
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
C                                  'pck00009.tpc',
C                                  'naif0009.tls'  )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM PHASEQ_EX1
C              IMPLICIT NONE
C
C        C
C        C     Include GF parameter declarations:
C        C
C              INCLUDE 'gf.inc'
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      SPD
C              DOUBLE PRECISION      PHASEQ
C
C              INTEGER               WNCARD
C
C        C
C        C     Local parameters
C        C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C        C
C        C     Use the parameter MAXWIN for both the result window size
C        C     and the workspace size.
C        C
C              INTEGER               MAXWIN
C              PARAMETER           ( MAXWIN = 1000 )
C
C        C
C        C     Length of strings:
C        C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 26 )
C
C              INTEGER               NLOOPS
C              PARAMETER           ( NLOOPS = 7 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(TIMLEN)    RELATE (NLOOPS)
C              CHARACTER*(6)         ABCORR
C              CHARACTER*(6)         ILLMN
C              CHARACTER*(6)         OBSRVR
C              CHARACTER*(6)         TARGET
C              CHARACTER*(TIMLEN)    TIMSTR
C
C              DOUBLE PRECISION      CNFINE ( LBCELL : 2 )
C              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C              DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWPA )
C              DOUBLE PRECISION      ADJUST
C              DOUBLE PRECISION      ET0
C              DOUBLE PRECISION      ET1
C              DOUBLE PRECISION      FINISH
C              DOUBLE PRECISION      PHASE
C              DOUBLE PRECISION      REFVAL
C              DOUBLE PRECISION      START
C              DOUBLE PRECISION      STEP
C
C              INTEGER               I
C              INTEGER               J
C
C
C        C
C        C     The relation values for the search.
C        C
C              DATA                  RELATE / '=',
C             .                               '<',
C             .                               '>',
C             .                               'LOCMIN',
C             .                               'ABSMIN',
C             .                               'LOCMAX',
C             .                               'ABSMAX'  /
C
C
C        C
C        C     Load kernels.
C        C
C              CALL FURNSH ( 'phaseq_ex1.tm' )
C
C        C
C        C     Initialize windows.
C        C
C              CALL SSIZED ( MAXWIN, RESULT )
C              CALL SSIZED ( 2,      CNFINE )
C
C        C
C        C     Store the time bounds of our search interval in
C        C     the confinement window.
C        C
C              CALL STR2ET ( '2006 DEC 01', ET0 )
C              CALL STR2ET ( '2007 JAN 31', ET1 )
C
C              CALL WNINSD ( ET0, ET1, CNFINE )
C
C        C
C        C     Search using a step size of 1 day (in units of seconds).
C        C     The reference value is 0.57598845. We're not using the
C        C     adjustment feature, so we set ADJUST to zero.
C        C
C              STEP   = SPD()
C              REFVAL = 0.57598845D0
C              ADJUST = 0.D0
C
C        C
C        C     Define the values for target, observer, illuminator, and
C        C     aberration correction.
C        C
C              TARGET = 'MOON'
C              ILLMN  = 'SUN'
C              ABCORR = 'LT+S'
C              OBSRVR = 'EARTH'
C
C              DO J=1, NLOOPS
C
C                 WRITE(*,*) 'Relation condition: ', RELATE(J)
C
C        C
C        C        Perform the search. The SPICE window RESULT contains
C        C        the set of times when the condition is met.
C        C
C                 CALL GFPA (  TARGET,    ILLMN,  ABCORR, OBSRVR,
C             .                RELATE(J), REFVAL, ADJUST, STEP,
C             .                CNFINE,    MAXWIN, NWPA,   WORK,
C             .                RESULT )
C
C        C
C        C        Display the results.
C        C
C                 IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C
C                    WRITE (*, '(A)') 'Result window is empty.'
C
C                 ELSE
C
C                    DO I = 1, WNCARD(RESULT)
C        C
C        C              Fetch the endpoints of the Ith interval
C        C              of the result window.
C        C
C                       CALL WNFETD ( RESULT, I, START, FINISH )
C
C                       PHASE = PHASEQ( START, TARGET, ILLMN, OBSRVR,
C             .                         ABCORR )
C                       CALL TIMOUT ( START,
C             .                       'YYYY-MON-DD HR:MN:SC.###',
C             .                       TIMSTR                          )
C
C                       WRITE (*, '(2X,A,F16.9)') 'Start time = '
C             .                               //   TIMSTR, PHASE
C
C
C                       PHASE = PHASEQ( FINISH, TARGET, ILLMN, OBSRVR,
C             .                         ABCORR )
C                       CALL TIMOUT ( FINISH,
C             .                       'YYYY-MON-DD HR:MN:SC.###',
C             .                       TIMSTR                          )
C
C                       WRITE (*, '(2X,A,F16.9)') 'Stop time  = '
C             .                                //  TIMSTR, PHASE
C
C                    END DO
C
C                 END IF
C
C                 WRITE(*,*) ' '
C
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Relation condition: =
C          Start time = 2006-DEC-02 13:31:34.414       0.575988450
C          Stop time  = 2006-DEC-02 13:31:34.414       0.575988450
C          Start time = 2006-DEC-07 14:07:55.470       0.575988450
C          Stop time  = 2006-DEC-07 14:07:55.470       0.575988450
C          Start time = 2006-DEC-31 23:59:59.997       0.575988450
C          Stop time  = 2006-DEC-31 23:59:59.997       0.575988450
C          Start time = 2007-JAN-06 08:16:25.512       0.575988450
C          Stop time  = 2007-JAN-06 08:16:25.512       0.575988450
C          Start time = 2007-JAN-30 11:41:32.557       0.575988450
C          Stop time  = 2007-JAN-30 11:41:32.557       0.575988450
C
C         Relation condition: <
C          Start time = 2006-DEC-02 13:31:34.414       0.575988450
C          Stop time  = 2006-DEC-07 14:07:55.470       0.575988450
C          Start time = 2006-DEC-31 23:59:59.997       0.575988450
C          Stop time  = 2007-JAN-06 08:16:25.512       0.575988450
C          Start time = 2007-JAN-30 11:41:32.557       0.575988450
C          Stop time  = 2007-JAN-31 00:00:00.000       0.468279091
C
C         Relation condition: >
C          Start time = 2006-DEC-01 00:00:00.000       0.940714974
C          Stop time  = 2006-DEC-02 13:31:34.414       0.575988450
C          Start time = 2006-DEC-07 14:07:55.470       0.575988450
C          Stop time  = 2006-DEC-31 23:59:59.997       0.575988450
C          Start time = 2007-JAN-06 08:16:25.512       0.575988450
C          Stop time  = 2007-JAN-30 11:41:32.557       0.575988450
C
C         Relation condition: LOCMIN
C          Start time = 2006-DEC-05 00:16:50.317       0.086121423
C          Stop time  = 2006-DEC-05 00:16:50.317       0.086121423
C          Start time = 2007-JAN-03 14:18:31.977       0.079899769
C          Stop time  = 2007-JAN-03 14:18:31.977       0.079899769
C
C         Relation condition: ABSMIN
C          Start time = 2007-JAN-03 14:18:31.977       0.079899769
C          Stop time  = 2007-JAN-03 14:18:31.977       0.079899769
C
C         Relation condition: LOCMAX
C          Start time = 2006-DEC-20 14:09:10.392       3.055062862
C          Stop time  = 2006-DEC-20 14:09:10.392       3.055062862
C          Start time = 2007-JAN-19 04:27:54.600       3.074603891
C          Stop time  = 2007-JAN-19 04:27:54.600       3.074603891
C
C         Relation condition: ABSMAX
C          Start time = 2007-JAN-19 04:27:54.600       3.074603891
C          Stop time  = 2007-JAN-19 04:27:54.600       3.074603891
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 31-AUG-2021 (JDR) (EDW)
C
C        Edited the header to comply with NAIF standards.
C
C        Fixed typos in the Header. Renamed example's meta-kernel.
C        Removed reference to private routine ZZGFPAQ from $Particulars
C        section.
C
C-    SPICELIB Version 1.0.0, 27-MAR-2014 (EDW) (BVS)
C
C-&


C$ Index_Entries
C
C     compute phase angle for arbitrary illumination source
C
C-&


C
C     SPICELIB functions.
C
      LOGICAL               FAILED
      LOGICAL               RETURN


C
C     Local parameters
C
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'PHASEQ' )

C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL   = 36 )



C
C     Local Variables.
C
      INTEGER               TARG
      INTEGER               ILLUM
      INTEGER               OBS

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FND

      CHARACTER*(32)        XBCORR

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVTARG
      INTEGER               SVTGID
      LOGICAL               SVFND1

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(MAXL)      SVILMN
      INTEGER               SVICDE
      LOGICAL               SVFND2

      INTEGER               SVCTR3 ( CTRSIZ )
      CHARACTER*(MAXL)      SVOBSR
      INTEGER               SVOBSN
      LOGICAL               SVFND3

      LOGICAL               FIRST

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVTARG
      SAVE                  SVTGID
      SAVE                  SVFND1

      SAVE                  SVCTR2
      SAVE                  SVILMN
      SAVE                  SVICDE
      SAVE                  SVFND2

      SAVE                  SVCTR3
      SAVE                  SVOBSR
      SAVE                  SVOBSN
      SAVE                  SVFND3

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


      PHASEQ = 0.D0

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( RNAME )

C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counters.
C
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )
         CALL ZZCTRUIN( SVCTR3 )

         FIRST = .FALSE.

      END IF

C
C     Obtain integer codes for the target, illuminator, and observer.
C
      CALL ZZBODS2C ( SVCTR1, SVTARG, SVTGID, SVFND1,
     .                TARGET, TARG, FND    )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The target, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT (  RNAME                                     )
         RETURN

      END IF


      CALL ZZBODS2C ( SVCTR2, SVILMN, SVICDE, SVFND2,
     .                ILLMN, ILLUM, FND    )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The illuminator, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', ILLMN                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT (  RNAME                                     )
         RETURN

      END IF


      CALL ZZBODS2C ( SVCTR3, SVOBSR, SVOBSN, SVFND3,
     .                OBSRVR, OBS, FND    )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT (  RNAME                                     )
         RETURN

      END IF


C
C     Squeeze all blanks out of the aberration correction
C     string; ensure the string is in upper case.
C
      CALL LJUCRS ( 0, ABCORR, XBCORR )


C
C     Check the aberration correction. If SPKEZR can't handle it,
C     neither can we.
C
      CALL ZZVALCOR ( XBCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF


C
C     Restrict correction to reception cases. The attribute block ID
C     for transmit corrections is XMTIDX.
C
      IF( ATTBLK(XMTIDX) ) THEN

         CALL SETMSG ( 'Invalid aberration correction ''#''. '
     .   //            'Phase angle geometry calculations currently '
     .   //            'restricted to reception cases.')
         CALL ERRCH  ( '#', ABCORR )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)' )
         CALL CHKOUT ( RNAME )
         RETURN

      END IF


C
C     Make sure the observer, illuminator, and target are distinct.
C
      IF ( (TARG .EQ. OBS  )   .OR.
     .     (TARG .EQ. ILLUM)   .OR.
     .     (OBS  .EQ. ILLUM)      ) THEN

         CALL SETMSG ( 'The observer, illuminator, and '
     .   //            'target must be distinct objects, but '
     .   //            'are not: OBSRVR = #, TARGET = #, '
     .   //            'are not: ILLMN= #.' )
         CALL ERRCH  ( '#', OBSRVR                         )
         CALL ERRCH  ( '#', TARGET                         )
         CALL ERRCH  ( '#', ILLMN                          )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'          )
         CALL CHKOUT ( RNAME )
         RETURN

      END IF

C
C     Call the routine to calculate the phase angle
C
      CALL ZZGFPAQ ( ET, TARG, ILLUM, OBS, XBCORR, PHASEQ )


C
C     All done.
C
      CALL CHKOUT ( RNAME )
      RETURN

      END
