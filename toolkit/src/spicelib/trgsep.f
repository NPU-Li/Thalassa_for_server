C$Procedure TRGSEP ( Separation quantity from observer )

      DOUBLE PRECISION FUNCTION TRGSEP ( ET,
     .                                   TARG1,  SHAPE1, FRAME1,
     .                                   TARG2,  SHAPE2, FRAME2,
     .                                   OBSRVR, ABCORR )

C$ Abstract
C
C     Compute the angular separation in radians between two spherical
C     or point objects.
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
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'
      INCLUDE 'zzdyn.inc'

      DOUBLE PRECISION      ET
      CHARACTER*(*)         TARG1
      CHARACTER*(*)         SHAPE1
      CHARACTER*(*)         FRAME1
      CHARACTER*(*)         TARG2
      CHARACTER*(*)         SHAPE2
      CHARACTER*(*)         FRAME2
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         ABCORR

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB.
C     TARG1      I   First target body name.
C     SHAPE1     I   First target body shape.
C     FRAME1     I   Reference frame of first target.
C     TARG2      I   Second target body name.
C     SHAPE2     I   First target body shape.
C     FRAME2     I   Reference frame of second target.
C     OBSRVR     I   Observing body name.
C     ABCORR     I   Aberration corrections flag.
C
C     The function returns the angular separation between two targets,
C     TARG1 and TARG2, as seen from an observer OBSRVR, possibly
C     corrected for aberration corrections.
C
C$ Detailed_Input
C
C     ET       is the time in ephemeris seconds past J2000 TDB at
C              which the separation is to be measured.
C
C     TARG1    is the string naming the first body of interest. You can
C              also supply the integer ID code for the object as an
C              integer string. For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C     SHAPE1   is the string naming the geometric model used to
C              represent the shape of the TARG1 body. Models
C              supported by this routine:
C
C                 'SPHERE'        Treat the body as a sphere with
C                                 radius equal to the maximum value of
C                                 BODYnnn_RADII.
C
C                 'POINT'         Treat the body as a point;
C                                 radius has value zero.
C
C              The SHAPE1 string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     FRAME1   is the string naming the body-fixed reference frame
C              corresponding to TARG1. TRGSEP does not currently use
C              this argument's value, its use is reserved for future
C              shape models. The value 'NULL' will suffice for
C              'POINT' and 'SPHERE' shaped bodies.
C
C     TARG2    is the string naming the second body of interest. You can
C              also supply the integer ID code for the object as an
C              integer string. For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C     SHAPE2   is the string naming the geometric model used to
C              represent the shape of the TARG2. Models supported by
C              this routine:
C
C                 'SPHERE'        Treat the body as a sphere with
C                                 radius equal to the maximum value of
C                                 BODYnnn_RADII.
C
C                 'POINT'         Treat the body as a single point;
C                                 radius has value zero.
C
C              The SHAPE2 string lacks sensitivity to case, leading
C              and trailing blanks.
C
C     FRAME2   is the string naming the body-fixed reference frame
C              corresponding to TARG2. TRGSEP does not currently use
C              this argument's value, its use is reserved for future
C              shape models. The value 'NULL' will suffice for
C              'POINT' and 'SPHERE' shaped bodies.
C
C     OBSRVR   is the string naming the observing body. Optionally, you
C              may supply the ID code of the object as an integer
C              string. For example, both 'EARTH' and '399' are
C              legitimate strings to supply to indicate the
C              observer is Earth.
C
C     ABCORR   is the string description of the aberration corrections
C              to apply to the state evaluations to account for
C              one-way light time and stellar aberration.
C
C              This routine accepts the same aberration corrections
C              as does the SPICE routine SPKEZR. See the header of
C              SPKEZR for a detailed description of the aberration
C              correction options. For convenience, the options are
C              listed below:
C
C                 'NONE'     Apply no correction.
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
C                 'XLT'      "Transmission" case: correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'XLT+S'    "Transmission" case: correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'XCN'      "Transmission" case: converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case: converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C              The ABCORR string lacks sensitivity to case, leading
C              and trailing blanks.
C
C$ Detailed_Output
C
C     The function returns the angular separation between two targets,
C     TARG1 and TARG2, as seen from an observer OBSRVR expressed in
C     radians.
C
C     The observer is the angle's vertex. The angular separation between
C     the targets may be measured between the centers or figures (limbs)
C     of the targets, depending on whether the target shapes are modeled
C     as spheres or points.
C
C     If the target shape is either a spheroid or an ellipsoid, the
C     radius used to compute the limb will be the largest of the radii
C     of the target's tri-axial ellipsoid model.
C
C     If the targets are modeled as points the result ranges from 0
C     to Pi radians or 180 degrees.
C
C     If the target shapes are modeled as spheres or ellipsoids, the
C     function returns a negative value when the bodies overlap
C     (occult). Note that in this situation the function returns 0 when
C     the limbs of the bodies start or finish the overlap.
C
C     The positions of the targets may optionally be corrected for light
C     time and stellar aberration.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the three objects TARG1, TARG2 and OBSRVR are not
C         distinct, an error is signaled by a routine in the call tree
C         of this routine.
C
C     2)  If the object names for TARG1, TARG2 or OBSRVR cannot resolve
C         to a NAIF body ID, an error is signaled by a routine in the
C         call tree of this routine.
C
C     3)  If the reference frame associated with TARG1, FRAME1, is not
C         centered on TARG1, or if the reference frame associated with
C         TARG2, FRAME2, is not centered on TARG2, an error is signaled
C         by a routine in the call tree of this routine. This
C         restriction does not apply to shapes 'SPHERE' and 'POINT', for
C         which the frame input is ignored.
C
C     4)  If the frame name for FRAME1 or FRAME2 cannot resolve to a
C         NAIF frame ID, an error is signaled by a routine in the call
C         tree of this routine.
C
C     5)  If the body shape for TARG1, SHAPE1, or the body shape for
C         TARG2, SHAPE2, is not recognized, an error is signaled by a
C         routine in the call tree of this routine.
C
C     6)  If the requested aberration correction ABCORR is not
C         recognized, an error is signaled by a routine in the call tree
C         of this routine.
C
C     7)  If either one or both targets' shape is modeled as sphere, and
C         the required PCK data has not been loaded, an error is
C         signaled by a routine in the call tree of this routine.
C
C     8)  If the ephemeris data required to perform the needed state
C         look-ups are not loaded, an error is signaled by a routine in
C         the call tree of this routine.
C
C     9)  If the observer OBSRVR is located within either one of the
C         targets, an error is signaled by a routine in the call tree of
C         this routine.
C
C     10) If an error is signaled, the function returns a meaningless
C         result.
C
C$ Files
C
C     Appropriate SPICE kernels must be loaded by the calling program
C     before this routine is called.
C
C     The following data are required:
C
C     -  An SPK file (or files) containing ephemeris data sufficient to
C        compute the position of each of the targets with respect to the
C        observer. If aberration corrections are used, the states of
C        target and observer relative to the solar system barycenter
C        must be calculable from the available ephemeris data.
C
C     -  A PCK file containing the targets' tri-axial ellipsoid model,
C        if the targets are modeled as spheres.
C
C     -  If non-inertial reference frames are used, then PCK files,
C        frame kernels, C-kernels, and SCLK kernels may be needed.
C
C$ Particulars
C
C     This routine determines the apparent separation between the
C     two objects as observed from a third. The value reported is
C     corrected for light time. Moreover, if at the time this routine
C     is called, stellar aberration corrections are enabled, this
C     correction will also be applied to the apparent positions of the
C     centers of the two objects.
C
C     Please refer to the Aberration Corrections Required Reading
C     (abcorr.req) for detailed information describing the nature and
C     calculation of the applied corrections.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Calculate the apparent angular separation of the Earth and
C        Moon as observed from the Sun at a TDB time known as a time
C        of maximum separation. Calculate and output the separation
C        modeling the Earth and Moon as point bodies and as spheres.
C        Provide the result in degrees.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: trgsep_ex1.tm
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
C              PROGRAM TRGSEP_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      TRGSEP
C              DOUBLE PRECISION      DPR
C
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(32)        TARG  (2)
C              CHARACTER*(32)        SHAPE (2)
C              CHARACTER*(32)        FRAME (2)
C              CHARACTER*(64)        TDBSTR
C              CHARACTER*(32)        OBSRVR
C              CHARACTER*(32)        ABCORR
C
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      VALUE
C
C              DATA             FRAME  / 'IAU_MOON', 'IAU_EARTH' /
C
C              DATA             TARG   / 'MOON', 'EARTH'   /
C
C              DATA             SHAPE  / 'POINT', 'SPHERE' /
C
C
C        C
C        C     Load the kernels.
C        C
C              CALL FURNSH( 'trgsep_ex1.tm')
C
C              TDBSTR = '2007-JAN-11 11:21:20.213872 (TDB)'
C              OBSRVR = 'SUN'
C              ABCORR = 'LT+S'
C
C              CALL STR2ET ( TDBSTR, ET )
C
C              VALUE = TRGSEP( ET,
C             .             TARG(1),  SHAPE(1), FRAME(1),
C             .             TARG(2),  SHAPE(1), FRAME(2),
C             .             OBSRVR,   ABCORR )
C
C              WRITE(*, FMT='(A,A6,A6)') 'Bodies:          ',
C             .                                      TARG(1), TARG(2)
C              WRITE(*, FMT='(A,A6)')    'as seen from:    ', OBSRVR
C              WRITE(*, FMT='(A,A36)')   'at TDB time:     ', TDBSTR
C              WRITE(*, FMT='(A,A)')     'with correction: ', ABCORR
C              WRITE(*,*)
C
C              WRITE(*, FMT='(A)') 'Apparent angular separation:'
C              WRITE(*, FMT='(A,F12.8)')
C             .     '   point body models  (deg.): ',
C             .                                        VALUE * DPR()
C
C              VALUE = TRGSEP( ET,
C             .             TARG(1),  SHAPE(2), FRAME(1),
C             .             TARG(2),  SHAPE(2), FRAME(2),
C             .             OBSRVR, ABCORR )
C
C              WRITE(*, FMT='(A,F12.8)')
C             .     '   sphere body models (deg.): ',
C             .                                        VALUE * DPR()
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Bodies:          MOON  EARTH
C        as seen from:    SUN
C        at TDB time:     2007-JAN-11 11:21:20.213872 (TDB)
C        with correction: LT+S
C
C        Apparent angular separation:
C           point body models  (deg.):   0.15729276
C           sphere body models (deg.):   0.15413221
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
C     M. Costa Sitja     (JPL)
C     J. Diaz del Rio    (ODC Space)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 07-AUG-2021 (EDW) (JDR) (MCS)
C
C        Based on code originally found in zzgfspu.f.
C
C-&


C$ Index_Entries
C
C     compute the angular separation between two target bodies
C
C-&


C
C     SPICELIB functions.
C
      LOGICAL               RETURN
      LOGICAL               FAILED
      DOUBLE PRECISION      ZZSEPQ

C
C     Local Variables
C
      CHARACTER*(5)         REF

      DOUBLE PRECISION      RAD    (2)

      INTEGER               BOD    (2)
      INTEGER               FRAMES (2)
      INTEGER               OBS

C
C     Set an initial value to return in case of error.
C
      TRGSEP = 0.D0
      REF    = 'J2000'

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN( 'TRGSEP' )

C
C     Argument check and initialization.
C
      CALL ZZSPIN ( TARG1,  SHAPE1, FRAME1,
     .              TARG2,  SHAPE2, FRAME2,
     .              OBSRVR, ABCORR,
     .              BOD,    FRAMES, RAD, OBS )

      IF ( FAILED () ) THEN
         TRGSEP = 0.D0
         CALL CHKOUT ('TRGSEP')
         RETURN
      END IF

C
C     Perform the calculation.
C
      TRGSEP = ZZSEPQ ( ET,
     .                  BOD(1), BOD(2),
     .                  RAD(1), RAD(2),
     .                  OBS,    ABCORR, REF )

      IF ( FAILED () ) THEN
         TRGSEP = 0.D0
         CALL CHKOUT ('TRGSEP')
         RETURN
      END IF

      CALL CHKOUT ('TRGSEP')
      RETURN
      END
