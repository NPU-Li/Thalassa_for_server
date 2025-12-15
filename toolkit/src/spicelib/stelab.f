C$Procedure STELAB     ( Stellar Aberration )

      SUBROUTINE STELAB ( POBJ, VOBS, APPOBJ )

C$ Abstract
C
C     Correct the apparent position of an object for stellar
C     aberration.
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
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   POBJ   ( 3 )
      DOUBLE PRECISION   VOBS   ( 3 )
      DOUBLE PRECISION   APPOBJ ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     POBJ       I   Position of an object with respect to the
C                    observer.
C     VOBS       I   Velocity of the observer with respect to the
C                    Solar System barycenter.
C     APPOBJ     O   Apparent position of the object with respect to
C                    the observer, corrected for stellar aberration.
C
C$ Detailed_Input
C
C     POBJ     is the position (x, y, z, km) of an object with
C              respect to the observer, possibly corrected for
C              light time.
C
C     VOBS     is the velocity (dx/dt, dy/dt, dz/dt, km/sec)
C              of the observer with respect to the Solar System
C              barycenter.
C
C$ Detailed_Output
C
C     APPOBJ   is the apparent position of the object relative
C              to the observer, corrected for stellar aberration.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the velocity of the observer is greater than or equal
C         to the speed of light, the error SPICE(VALUEOUTOFRANGE)
C         is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Let r be the vector from the observer to the object, and v be
C         -                                                    -
C     the velocity of the observer with respect to the Solar System
C     barycenter. Let w be the angle between them. The aberration
C     angle phi is given by
C
C          sin(phi) = v sin(w) / c
C
C     Let h be the vector given by the cross product
C         -
C
C           h = r X v
C           -   -   -
C
C     Rotate r by phi radians about h to obtain the apparent position
C            -                      -
C     of the object.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Compute the apparent position of the Moon relative to the
C        Earth, corrected for one light-time and stellar aberration,
C        given the geometric state of the Earth relative to the Solar
C        System Barycenter, and the difference between the stellar
C        aberration corrected and uncorrected position vectors, taking
C        several steps.
C
C        First, compute the light-time corrected state of the Moon body
C        as seen by the Earth, using its geometric state. Then apply
C        the correction for stellar aberration to the light-time
C        corrected state of the target body.
C
C        The code in this example could be replaced by a single call
C        to SPKPOS:
C
C            CALL SPKPOS ( 'MOON', ET, 'J2000', 'LT+S', 'EARTH',
C           .               POS,   LT                           )
C
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: stelab_ex1.tm
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
C              de418.bsp                     Planetary ephemeris
C              naif0009.tls                  Leapseconds
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de418.bsp',
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
C              PROGRAM STELAB_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(6)         REFFRM
C              CHARACTER*(12)        UTCSTR
C
C              DOUBLE PRECISION      APPDIF ( 3 )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      PCORR  ( 3 )
C              DOUBLE PRECISION      POS    ( 3 )
C              DOUBLE PRECISION      SOBS   ( 6 )
C
C              INTEGER               IDOBS
C              INTEGER               IDTARG
C
C        C
C        C     Assign an observer, Earth, target, Moon, time of interest
C        C     and reference frame for returned vectors.
C        C
C              IDOBS  = 399
C              IDTARG = 301
C              UTCSTR = 'July 4 2004'
C              REFFRM = 'J2000'
C
C        C
C        C     Load the needed kernels.
C        C
C              CALL FURNSH ( 'stelab_ex1.tm' )
C
C        C
C        C     Convert the time string to ephemeris time, J2000.
C        C
C              CALL STR2ET ( UTCSTR, ET )
C
C        C
C        C     Get the state of the observer with respect to the solar
C        C     system barycenter.
C        C
C              CALL SPKSSB ( IDOBS, ET, REFFRM, SOBS )
C
C        C
C        C     Get the light-time corrected position POS of the target
C        C     body IDTARG as seen by the observer.
C        C
C              CALL SPKAPO ( IDTARG, ET, REFFRM, SOBS, 'LT', POS, LT )
C
C        C
C        C     Output the uncorrected vector.
C        C
C              WRITE(*,*) 'Uncorrected position vector'
C              WRITE(*,'(A,3F19.6)') '   ', POS(1), POS(2), POS(3)
C
C        C
C        C     Apply the correction for stellar aberration to the
C        C     light-time corrected position of the target body.
C        C
C              CALL STELAB ( POS, SOBS(4), PCORR )
C
C        C
C        C     Output the corrected position vector and the apparent
C        C     difference from the uncorrected vector.
C        C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Corrected position vector'
C              WRITE(*,'(A,3F19.6)') '   ', PCORR(1), PCORR(2),
C             .                             PCORR(3)
C
C        C
C        C     Apparent difference.
C        C
C              CALL VSUB ( POS, PCORR, APPDIF )
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Apparent difference'
C              WRITE(*,'(A,3F19.6)') '   ', APPDIF(1), APPDIF(2),
C             .                            APPDIF(3)
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Uncorrected position vector
C                 201738.725087     -260893.141602     -147722.589056
C
C         Corrected position vector
C                 201765.929516     -260876.818077     -147714.262441
C
C         Apparent difference
C                    -27.204429         -16.323525          -8.326615
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1]  W. Owen, "The Treatment of Aberration in Optical Navigation",
C          JPL IOM #314.8-524, 8 February 1985.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     H.A. Neilan        (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 05-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added example's
C        meta-kernel and problem statement. Created complete code
C        example from existing code fragments.
C
C-    SPICELIB Version 1.1.1, 08-JAN-2008 (NJB)
C
C        The header example was updated to remove references
C        to SPKAPP.
C
C-    SPICELIB Version 1.1.0, 08-FEB-1999 (WLT)
C
C        The example was corrected so that SOBS(4) is passed
C        into STELAB instead of STARG(4).
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 08-AUG-1990 (HAN)
C
C        $Examples section of the header was updated to replace
C        calls to the GEF ephemeris readers by calls to the
C        new SPK ephemeris reader.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN)
C
C-&


C$ Index_Entries
C
C     stellar aberration
C
C-&


C$ Revisions
C
C-    Beta Version 2.1.0, 9-MAR-1989 (HAN)
C
C        Declaration of the variable LIGHT was removed from the code.
C        The variable was declared but never used.
C
C-    Beta Version 2.0.0, 28-DEC-1988 (HAN)
C
C        Error handling was added to check the velocity of the
C        observer. If the velocity of the observer is greater
C        than or equal to the speed of light, the error
C        SPICE(VALUEOUTOFRANGE) is signaled.
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      LOGICAL               RETURN


C
C     Local variables
C

      DOUBLE PRECISION      ONEBYC
      DOUBLE PRECISION      U      ( 3 )
      DOUBLE PRECISION      VBYC   ( 3 )
      DOUBLE PRECISION      LENSQR
      DOUBLE PRECISION      H      ( 3 )
      DOUBLE PRECISION      SINPHI
      DOUBLE PRECISION      PHI


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STELAB' )
      END IF


C
C     We are not going to compute the aberrated vector in exactly the
C     way described in the particulars section.  We can combine some
C     steps and we take some precautions to prevent floating point
C     overflows.
C
C
C     Get a unit vector that points in the direction of the object
C     ( u_obj ).
C
      CALL VHAT ( POBJ, U )

C
C     Get the velocity vector scaled with respect to the speed of light
C     ( v/c ).
C
      ONEBYC = 1.0D0 / CLIGHT()

      CALL VSCL  ( ONEBYC, VOBS, VBYC )

C
C     If the square of the length of the velocity vector is greater than
C     or equal to one, the speed of the observer is greater than or
C     equal to the speed of light. The observer speed is definitely out
C     of range. Signal an error and check out.
C
      LENSQR = VDOT ( VBYC, VBYC )

      IF ( LENSQR .GE. 1.0D0 ) THEN

         CALL SETMSG ( 'Velocity components of observer were:  '      //
     .                 'dx/dt = *, dy/dt = *, dz/dt = *.'              )
         CALL ERRDP  ( '*', VOBS (1)            )
         CALL ERRDP  ( '*', VOBS (2)            )
         CALL ERRDP  ( '*', VOBS (3)            )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)' )
         CALL CHKOUT ( 'STELAB'                 )
         RETURN

      END IF

C
C     Compute u_obj x (v/c)
C
      CALL VCRSS ( U, VBYC, H )


C
C     If the magnitude of the vector H is zero, the observer is moving
C     along the line of sight to the object, and no correction is
C     required. Otherwise, rotate the position of the object by phi
C     radians about H to obtain the apparent position.
C
      SINPHI  = VNORM ( H )

      IF ( SINPHI .NE. 0.D0 ) THEN

         PHI = DASIN ( SINPHI )
         CALL  VROTV ( POBJ, H, PHI, APPOBJ )

      ELSE

         CALL MOVED ( POBJ, 3, APPOBJ )

      END IF


      CALL CHKOUT ( 'STELAB' )
      RETURN
      END
