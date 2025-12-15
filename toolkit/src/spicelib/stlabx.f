C$Procedure STLABX ( Stellar aberration, transmission case )

      SUBROUTINE STLABX ( POBJ, VOBS, CORPOS )

C$ Abstract
C
C     Correct the position of a target for the stellar aberration
C     effect on radiation transmitted from a specified observer to
C     the target.
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
      DOUBLE PRECISION   CORPOS ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     POBJ       I   Position of an object with respect to the
C                    observer.
C     VOBS       I   Velocity of the observer with respect to the
C                    Solar System barycenter.
C     CORPOS     O   Corrected position of the object.
C
C$ Detailed_Input
C
C     POBJ     is the cartesian position vector of an object with
C              respect to the observer, possibly corrected for
C              light time. Units are km.
C
C     VOBS     is the cartesian velocity vector of the observer
C              with respect to the Solar System barycenter. Units
C              are km/s.
C
C$ Detailed_Output
C
C     CORPOS   is the  position of the object relative to the
C              observer, corrected for the stellar aberration
C              effect on radiation directed toward the target. This
C              correction is the inverse of the usual stellar
C              aberration correction: the corrected vector
C              indicates the direction in which radiation must be
C              emitted from the observer, as seen in an inertial
C              reference frame having velocity equal to that of the
C              observer, in order to reach the position indicated by
C              the input vector POBJ.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the velocity of the observer is greater than or equal to
C         the speed of light, an error is signaled by a routine in the
C         call tree of this routine. The outputs are undefined.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     In order to transmit radiation from an observer to a specified
C     target, the emission direction must be corrected for one way
C     light time and for the motion of the observer relative to the
C     solar system barycenter. The correction for the observer's
C     motion when transmitting to a target is the inverse of the
C     usual stellar aberration correction applied to the light-time
C     corrected position of the target as seen by the observer.
C
C     Below is the description of the stellar aberration correction
C     used in the SPICELIB routine STELAB (with the notation changed
C     slightly):
C
C        Let R be the vector from the observer to the object, and V be
C        the velocity of the observer with respect to the Solar System
C        barycenter. Let W be the angle between them. The aberration
C        angle PHI is given by
C
C           sin(PHI) = V * sin(W) / C
C
C        Let H be the vector given by the cross product
C
C           H = R x V
C
C        Rotate R by PHI radians about H to obtain the apparent position
C        of the object.
C
C     This routine applies the inverse correction, so here the rotation
C     about H is by -PHI radians.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Compute the apparent position of the Moon relative to the
C        Earth, corrected for one way light-time and stellar aberration
C        effect on radiation transmitted from the Earth to the Moon,
C        given the geometric state of the Earth relative to the Solar
C        System Barycenter, and the difference between the stellar
C        aberration corrected and uncorrected position vectors, taking
C        several steps.
C
C        First, compute the light-time corrected state of the Moon body
C        as seen by the Earth, using its geometric state. Then apply
C        the correction for stellar aberration to the light-time
C        corrected state of the target body, both for the transmission
C        case.
C
C        The code in this example could be replaced by a single call
C        to SPKPOS:
C
C            CALL SPKPOS ( 'MOON', ET, 'J2000', 'XLT+S', 'EARTH',
C           .               POS,   LT                            )
C
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: stlabx_ex1.tm
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
C              PROGRAM STLABX_EX1
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
C              CALL FURNSH ( 'stlabx_ex1.tm' )
C
C        C
C        C     Convert the time string to ephemeris time.
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
C        C     body IDTARG as seen by the observer. Normally we would
C        C     call SPKPOS to obtain this vector, but we already have
C        C     the state of the observer relative to the solar system
C        C     barycenter, so we can avoid looking up that state twice
C        C     by calling SPKAPO.
C        C
C              CALL SPKAPO ( IDTARG, ET, REFFRM, SOBS, 'XLT', POS, LT )
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
C              CALL STLABX ( POS, SOBS(4), PCORR )
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
C                 201809.933536     -260878.049826     -147716.077987
C
C         Corrected position vector
C                 201782.730972     -260894.375627     -147724.405897
C
C         Apparent difference
C                     27.202563          16.325802           8.327911
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
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 13-AUG-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added example's
C        meta-kernel and problem statement. Created complete code
C        example from existing code fragments.
C
C-    SPICELIB Version 1.0.1, 08-JAN-2008 (NJB)
C
C        The header example was updated to remove references
C        to SPKAPP.
C
C-    SPICELIB Version 1.0.0, 02-JAN-2002 (IMU) (WLT) (NJB)
C
C-&


C$ Index_Entries
C
C     stellar aberration for transmission case
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      NEGVEL ( 3 )

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STLABX' )
      END IF

C
C     Obtain the negative of the observer's velocity.  This
C     velocity, combined with the target's position, will yield
C     the inverse of the usual stellar aberration correction,
C     which is exactly what we seek.
C
      CALL VMINUS ( VOBS, NEGVEL )

      CALL STELAB ( POBJ, NEGVEL, CORPOS )

      CALL CHKOUT ( 'STLABX' )
      RETURN
      END
