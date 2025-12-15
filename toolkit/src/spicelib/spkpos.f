C$Procedure SPKPOS ( S/P Kernel, position )

      SUBROUTINE SPKPOS ( TARG, ET, REF, ABCORR, OBS, PTARG, LT )

C$ Abstract
C
C     Return the position of a target body relative to an observing
C     body, optionally corrected for light time (planetary aberration)
C     and stellar aberration.
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
C     ABCORR
C     SPK
C     NAIF_IDS
C     FRAMES
C     TIME
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBS
      DOUBLE PRECISION      PTARG    ( 3 )
      DOUBLE PRECISION      LT

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TARG       I   Target body name.
C     ET         I   Observer epoch.
C     REF        I   Reference frame of output position vector.
C     ABCORR     I   Aberration correction flag.
C     OBS        I   Observing body name.
C     PTARG      O   Position of target.
C     LT         O   One way light time between observer and target.
C
C$ Detailed_Input
C
C     TARG     is the name of a target body. Optionally, you may
C              supply the integer ID code for the object as an
C              integer string. For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C              The target and observer define a position vector
C              which points from the observer to the target.
C
C     ET       is the ephemeris time, expressed as seconds past J2000
C              TDB, at which the position of the target body relative to
C              the observer is to be computed. ET refers to time at the
C              observer's location.
C
C     REF      is the name of the reference frame relative to which
C              the output position vector should be expressed. This
C              may be any frame supported by the SPICE system,
C              including built-in frames (documented in the Frames
C              Required Reading) and frames defined by a loaded
C              frame kernel (FK).
C
C              When REF designates a non-inertial frame, the
C              orientation of the frame is evaluated at an epoch
C              dependent on the selected aberration correction. See
C              the description of the output position vector PTARG
C              for details.
C
C     ABCORR   indicates the aberration corrections to be applied to
C              the position of the target body to account for
C              one-way light time and stellar aberration. See the
C              discussion in the $Particulars section for
C              recommendations on how to choose aberration
C              corrections.
C
C              ABCORR may be any of the following:
C
C                 'NONE'     Apply no correction. Return the
C                            geometric position of the target body
C                            relative to the observer.
C
C              The following values of ABCORR apply to the
C              "reception" case in which photons depart from the
C              target's location at the light-time corrected epoch
C              ET-LT and *arrive* at the observer's location at ET:
C
C                 'LT'       Correct for one-way light time (also
C                            called "planetary aberration") using a
C                            Newtonian formulation. This correction
C                            yields the position of the target at
C                            the moment it emitted photons arriving
C                            at the observer at ET.
C
C                            The light time correction uses an
C                            iterative solution of the light time
C                            equation (see $Particulars for details).
C                            The solution invoked by the 'LT' option
C                            uses one iteration.
C
C                 'LT+S'     Correct for one-way light time and
C                            stellar aberration using a Newtonian
C                            formulation. This option modifies the
C                            position obtained with the 'LT' option
C                            to account for the observer's velocity
C                            relative to the solar system
C                            barycenter. The result is the apparent
C                            position of the target---the position
C                            as seen by the observer.
C
C                 'CN'       Converged Newtonian light time
C                            correction. In solving the light time
C                            equation, the 'CN' correction iterates
C                            until the solution converges (three
C                            iterations on all supported platforms).
C                            Whether the 'CN+S' solution is
C                            substantially more accurate than the
C                            'LT' solution depends on the geometry
C                            of the participating objects and on the
C                            accuracy of the input data. In all
C                            cases this routine will execute more
C                            slowly when a converged solution is
C                            computed. See the $Particulars section
C                            below for a discussion of precision of
C                            light time corrections.
C
C                 'CN+S'     Converged Newtonian light time
C                            correction and stellar aberration
C                            correction.
C
C
C              The following values of ABCORR apply to the
C              "transmission" case in which photons *depart* from
C              the observer's location at ET and arrive at the
C              target's location at the light-time corrected epoch
C              ET+LT:
C
C                 'XLT'      "Transmission" case: correct for
C                            one-way light time using a Newtonian
C                            formulation. This correction yields the
C                            position of the target at the moment it
C                            receives photons emitted from the
C                            observer's location at ET.
C
C                 'XLT+S'    "Transmission" case: correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation. This option modifies the
C                            position obtained with the 'XLT' option
C                            to account for the observer's velocity
C                            relative to the solar system
C                            barycenter. The computed target
C                            position indicates the direction that
C                            photons emitted from the observer's
C                            location must be "aimed" to hit the
C                            target.
C
C                 'XCN'      "Transmission" case: converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case: converged
C                            Newtonian light time correction and
C                            stellar aberration correction.
C
C
C              Neither special nor general relativistic effects are
C              accounted for in the aberration corrections applied
C              by this routine.
C
C              Case and blanks are not significant in the string
C              ABCORR.
C
C     OBS      is the name of an observing body. Optionally, you
C              may supply the ID code of the object as an integer
C              string. For example, both 'EARTH' and '399' are
C              legitimate strings to supply to indicate the
C              observer is Earth.
C
C$ Detailed_Output
C
C     PTARG    is a Cartesian 3-vector representing the position of
C              the target body relative to the specified observer.
C              PTARG is corrected for the specified aberrations, and
C              is expressed with respect to the reference frame
C              specified by REF. The three components of PTARG
C              represent the x-, y- and z-components of the target's
C              position.
C
C              PTARG points from the observer's location at ET to
C              the aberration-corrected location of the target.
C              Note that the sense of this position vector is
C              independent of the direction of radiation travel
C              implied by the aberration correction.
C
C              Units are always km.
C
C              Non-inertial frames are treated as follows: letting
C              LTCENT be the one-way light time between the observer
C              and the central body associated with the frame, the
C              orientation of the frame is evaluated at ET-LTCENT,
C              ET+LTCENT, or ET depending on whether the requested
C              aberration correction is, respectively, for received
C              radiation, transmitted radiation, or is omitted.
C              LTCENT is computed using the method indicated by
C              ABCORR.
C
C     LT       is the one-way light time between the observer and
C              target in seconds. If the target position is
C              corrected for aberrations, then LT is the one-way
C              light time between the observer and the light time
C              corrected target location.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If name of target or observer cannot be translated to its
C         NAIF ID code, the error SPICE(IDCODENOTFOUND) is signaled.
C
C     2)  If the reference frame REF is not a recognized reference
C         frame, the error SPICE(UNKNOWNFRAME) is signaled.
C
C     3)  If the loaded kernels provide insufficient data to compute the
C         requested position vector, an error is signaled by a routine
C         in the call tree of this routine.
C
C     4)  If an error occurs while reading an SPK or other kernel file,
C         the error  is signaled by a routine in the call tree
C         of this routine.
C
C$ Files
C
C     This routine computes positions using SPK files that have been
C     loaded into the SPICE system, normally via the kernel loading
C     interface routine FURNSH. See the routine FURNSH and the SPK
C     and KERNEL Required Reading for further information on loading
C     (and unloading) kernels.
C
C     If the output position PTARG is to be expressed relative to a
C     non-inertial frame, or if any of the ephemeris data used to
C     compute PTARG are expressed relative to a non-inertial frame in
C     the SPK files providing those data, additional kernels may be
C     needed to enable the reference frame transformations required to
C     compute the position. These additional kernels may be C-kernels,
C     PCK files or frame kernels. Any such kernels must already be
C     loaded at the time this routine is called.
C
C$ Particulars
C
C     This routine is part of the user interface to the SPICE ephemeris
C     system. It allows you to retrieve position information for any
C     ephemeris object relative to any other in a reference frame that
C     is convenient for further computations.
C
C     This routine is identical in function to the routine SPKEZP
C     except that it allows you to refer to ephemeris objects by name
C     (via a character string).
C
C     Please refer to the Aberration Corrections Required Reading
C     abcorr.req for detailed information describing the nature and
C     calculation of the applied corrections.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Load a planetary ephemeris SPK, then look up a series of
C        geometric positions of the moon relative to the earth,
C        referenced to the J2000 frame.
C
C        Use the SPK kernel below to load the required Earth and
C        Moon ephemeris data.
C
C           de421.bsp
C
C
C        Example code begins here.
C
C
C              IMPLICIT NONE
C        C
C        C     Local constants
C        C
C              CHARACTER*(*)         FRAME
C              PARAMETER           ( FRAME  = 'J2000' )
C
C              CHARACTER*(*)         ABCORR
C              PARAMETER           ( ABCORR = 'NONE' )
C
C              CHARACTER*(*)         SPK
C              PARAMETER           ( SPK    = 'de421.bsp' )
C
C        C
C        C     ET0 represents the date 2000 Jan 1 12:00:00 TDB.
C        C
C              DOUBLE PRECISION      ET0
C              PARAMETER           ( ET0    = 0.0D0 )
C
C        C
C        C     Use a time step of 1 hour; look up 4 positions.
C        C
C              DOUBLE PRECISION      STEP
C              PARAMETER           ( STEP   = 3600.0D0 )
C
C              INTEGER               MAXITR
C              PARAMETER           ( MAXITR = 4 )
C
C              CHARACTER*(*)         OBSRVR
C              PARAMETER           ( OBSRVR = 'Earth' )
C
C              CHARACTER*(*)         TARGET
C              PARAMETER           ( TARGET = 'Moon' )
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      POS ( 3 )
C
C              INTEGER               I
C
C        C
C        C     Load the SPK file.
C        C
C              CALL FURNSH ( SPK )
C
C        C
C        C     Step through a series of epochs, looking up a
C        C     position vector at each one.
C        C
C              DO I = 1, MAXITR
C
C                 ET = ET0 + (I-1)*STEP
C
C                 CALL SPKPOS ( TARGET, ET, FRAME, ABCORR, OBSRVR,
C             .                 POS,    LT                        )
C
C                 WRITE (*,*) 'ET = ', ET
C                 WRITE (*,*) ' '
C                 WRITE (*,*) 'J2000 x-position (km):   ', POS(1)
C                 WRITE (*,*) 'J2000 y-position (km):   ', POS(2)
C                 WRITE (*,*) 'J2000 z-position (km):   ', POS(3)
C                 WRITE (*,*) ' '
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
C         ET =    0.0000000000000000
C
C         J2000 x-position (km):     -291608.38530964090
C         J2000 y-position (km):     -266716.83294678747
C         J2000 z-position (km):     -76102.487146783606
C
C         ET =    3600.0000000000000
C
C         J2000 x-position (km):     -289279.89831331203
C         J2000 y-position (km):     -269104.10842893779
C         J2000 z-position (km):     -77184.242072912006
C
C         ET =    7200.0000000000000
C
C         J2000 x-position (km):     -286928.00140550011
C         J2000 y-position (km):     -271469.99024601618
C         J2000 z-position (km):     -78259.908307700243
C
C         ET =    10800.000000000000
C
C         J2000 x-position (km):     -284552.90265547187
C         J2000 y-position (km):     -273814.30975274299
C         J2000 z-position (km):     -79329.406046598189
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
C     C.H. Acton         (JPL)
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.2.0, 01-OCT-2021 (JDR) (NJB)
C
C        Deleted include statement for frmtyp.inc.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example from existing fragment.
C
C        Updated $Particulars to refer to Aberration Corrections
C        Required Reading document, which was added to
C        $Required_Reading list.
C
C-    SPICELIB Version 3.1.0, 03-JUL-2014 (NJB) (BVS)
C
C        Discussion of light time corrections was updated. Assertions
C        that converged light time corrections are unlikely to be
C        useful were removed.
C
C        Updated to save the input body names and ZZBODTRN state
C        counters and to do name-ID conversions only if the counters
C        have changed.
C
C-    SPICELIB Version 3.0.3, 04-APR-2008 (NJB)
C
C        Corrected minor error in description of XLT+S aberration
C        correction.
C
C-    SPICELIB Version 3.0.2, 20-OCT-2003 (EDW)
C
C        Added mention that LT returns in seconds.
C
C-    SPICELIB Version 3.0.1, 29-JUL-2003 (NJB) (CHA)
C
C        Various minor header changes were made to improve clarity.
C
C-    SPICELIB Version 3.0.0, 31-DEC-2001 (NJB)
C
C        Updated to handle aberration corrections for transmission
C        of radiation. Formerly, only the reception case was
C        supported. The header was revised and expanded to explain
C        the functionality of this routine in more detail.
C
C-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
C
C-&


C$ Index_Entries
C
C     using body names get position relative to an observer
C     get position relative observer corrected for aberrations
C     read ephemeris data
C     read trajectory data
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )

C
C     Local variables
C
      INTEGER               TARGID
      INTEGER               OBSID

      LOGICAL               FOUND

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVTARG
      INTEGER               SVTGID
      LOGICAL               SVFND1

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(MAXL)      SVOBSN
      INTEGER               SVOBSI
      LOGICAL               SVFND2

      LOGICAL               FIRST

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVTARG
      SAVE                  SVTGID
      SAVE                  SVFND1

      SAVE                  SVCTR2
      SAVE                  SVOBSN
      SAVE                  SVOBSI
      SAVE                  SVFND2

      SAVE                  FIRST

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKPOS' )
      END IF

C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counters.
C
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )

         FIRST = .FALSE.

      END IF

C
C     Starting from translation of target name to its code
C
      CALL ZZBODS2C ( SVCTR1, SVTARG, SVTGID, SVFND1,
     .                TARG, TARGID, FOUND    )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The target, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '
     .   //            'Alternatively you may call SPKEZP '
     .   //            'directly if you know the SPICE id-codes '
     .   //            'for both ''#'' and ''#'' '                )
         CALL ERRCH  ( '#', TARG                                  )
         CALL ERRCH  ( '#', TARG                                  )
         CALL ERRCH  ( '#', OBS                                   )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SPKPOS'                                   )
         RETURN

      END IF

C
C     Now do the same for observer.
C
      CALL ZZBODS2C ( SVCTR2, SVOBSN, SVOBSI, SVFND2,
     .                OBS, OBSID, FOUND    )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '
     .   //            'Alternatively you may call SPKEZP '
     .   //            'directly if you know the SPICE id-codes '
     .   //            'for both ''#'' and ''#'' '                )
         CALL ERRCH  ( '#', OBS                                   )
         CALL ERRCH  ( '#', TARG                                  )
         CALL ERRCH  ( '#', OBS                                   )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SPKPOS'                                   )
         RETURN

      END IF

C
C     After all translations are done we can call SPKEZP.
C
      CALL SPKEZP ( TARGID, ET, REF, ABCORR, OBSID, PTARG, LT )


      CALL CHKOUT ( 'SPKPOS' )
      RETURN
      END
