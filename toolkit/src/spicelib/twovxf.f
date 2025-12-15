C$Procedure TWOVXF ( Two states defining a frame transformation )

      SUBROUTINE TWOVXF ( AXDEF, INDEXA, PLNDEF, INDEXP, XFORM )

C$ Abstract
C
C     Find the state transformation from a base frame to the
C     right-handed frame defined by two state vectors: one state
C     vector defining a specified axis and a second state vector
C     defining a specified coordinate plane.
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
C     AXES
C     FRAMES
C     MATRIX
C     TRANSFORMATION
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   AXDEF  ( 6 )
      INTEGER            INDEXA
      DOUBLE PRECISION   PLNDEF ( 6 )
      INTEGER            INDEXP
      DOUBLE PRECISION   XFORM  ( 6, 6 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     AXDEF      I   State defining a principal axis.
C     INDEXA     I   Principal axis number of AXDEF (X=1, Y=2, Z=3).
C     PLNDEF     I   State defining (with AXDEF) a principal plane.
C     INDEXP     I   Second axis number (with INDEXA) of principal
C                    plane.
C     XFORM      O   Output state transformation matrix.
C
C$ Detailed_Input
C
C     AXDEF    is a "generalized" state vector defining one of the
C              principal axes of a reference frame. This vector
C              consists of three components of a vector-valued
C              function of one independent variable `t' followed by
C              the derivatives of the components with respect to that
C              variable:
C
C                 ( a, b, c, da/dt, db/dt, dc/dt )
C
C              This routine treats the input states as unitless, but
C              in most applications the input states represent
C              quantities that have associated units. The first three
C              components must have the same units, and the units of
C              the last three components must be compatible with
C              those of the first three: if the first three
C              components of AXDEF
C
C                 ( a, b, c )
C
C              have units U and `t' has units T, then the units of
C              AXDEF normally would be
C
C                 ( U, U, U, U/T, U/T, U/T )
C
C              Note that the direction and angular velocity defined
C              by AXDEF are actually independent of U, so scaling
C              AXDEF doesn't affect the output of this routine.
C
C              AXDEF could represent position and velocity; it could
C              also represent velocity and acceleration. AXDEF could
C              for example represent the velocity and acceleration of
C              a time-dependent position vector ( x(t), y(t), z(t) ),
C              in which case AXDEF would be defined by
C
C                 a     = dx/dt
C                 b     = dy/dt
C                 c     = dz/dt
C
C                          2      2
C                 da/dt = d x / dt
C
C                          2      2
C                 db/dt = d y / dt
C
C                          2      2
C                 dc/dt = d z / dt
C
C              Below, we'll call the normalized (unit length) version
C              of
C
C                 ( a, b, c )
C
C              the "direction" of AXDEF.
C
C              We call the frame relative to which AXDEF is specified
C              the "base frame." The input state PLNDEF must be
C              specified relative to the same base frame.
C
C     INDEXA   is the index of the reference frame axis that is
C              parallel to the direction of AXDEF.
C
C                 INDEXA   Axis
C                 ------   ----
C                    1       X
C                    2       Y
C                    3       Z
C
C     PLNDEF   is a state vector defining (with AXDEF) a principal
C              plane of the reference frame. This vector consists
C              of three components followed by their derivatives with
C              respect to the independent variable `t' associated with
C              AXDEF, so PLNDEF is
C
C                 ( e, f, g, de/dt, df/dt, dg/dt )
C
C              Below, we'll call the unitized version of
C
C                 ( e, f, g )
C
C              the "direction" of PLNDEF.
C
C              The second axis of the principal plane containing the
C              direction vectors of AXDEF and PLNDEF is perpendicular
C              to the first axis and has positive dot product with
C              the direction vector of PLNDEF.
C
C              The first three components of PLNDEF must have the
C              same units, and the units of the last three components
C              must be compatible with those of the first three: if
C              the first three components of PLNDEF
C
C                 ( e, f, g )
C
C              have units U2 and `t' has units T, then the units of
C              PLNDEF normally would be
C
C                 ( U2, U2, U2, U2/T, U2/T, U2/T )
C
C              Note that ***for meaningful results, the angular
C              velocities defined by AXDEF and PLNDEF must both have
C              units of 1/T.***
C
C              As with AXDEF, scaling PLNDEF doesn't affect the
C              output of this routine.
C
C              AXDEF and PLNDEF must be specified relative to a
C              common reference frame, which we call the "base
C              frame."
C
C     INDEXP   is the index of  second axis of the principal frame
C              determined by AXDEF and PLNDEF. The association of
C              integer values and axes is the same as for INDEXA.
C
C$ Detailed_Output
C
C     XFORM    is the 6x6 matrix that transforms states from the
C              frame relative to which AXDEF and PLNDEF are specified
C              (the "base frame") to the frame whose axes and
C              derivative are determined by AXDEF, PLNDEF, INDEXA and
C              INDEXP.
C
C              The matrix XFORM has the structure shown below:
C
C                 .-              -.
C                 |        :       |
C                 |    R   :   0   |
C                 |        :       |
C                 | .......:.......|
C                 |        :       |
C                 |  dR/dt :   R   |
C                 |        :       |
C                 `-              -'
C
C              where R is a rotation matrix that is a function of
C              the independent variable associated with AXDEF and
C              PLNDEF, and where dR/dt is the derivative of R
C              with respect to that independent variable.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If INDEXA or INDEXP is not in the set {1,2,3}, the error
C         SPICE(BADINDEX) is signaled.
C
C     2)  If INDEXA and INDEXP are the same, the error
C         SPICE(UNDEFINEDFRAME) is signaled.
C
C     3)  If the cross product of the vectors AXDEF and PLNDEF is zero,
C         the error SPICE(DEPENDENTVECTORS) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given two linearly independent state vectors AXDEF and PLNDEF,
C     define vectors DIR1 and DIR2 by
C
C        DIR1 = ( AXDEF(1),   AXDEF(2),   AXDEF(3)  )
C        DIR2 = ( PLNDEF(1),  PLNDEF(2),  PLNDEF(3) )
C
C     Then there is a unique right-handed reference frame F having:
C
C        DIR1 lying along the INDEXA axis.
C
C        DIR2 lying in the INDEXA-INDEXP coordinate plane, such that
C        the dot product of DIR2 with the positive INDEXP axis is
C        positive.
C
C     This routine determines the 6x6 matrix that transforms states
C     from the base frame used to represent the input vectors to the
C     the frame F determined by AXDEF and PLNDEF. Thus a state vector
C
C        S       = ( x, y, z, dx/dt, dy/dt, dz/dt )
C         base
C
C     in the input reference frame will be transformed to
C
C        S       = XFORM * S
C         F                 base
C
C     in the frame F determined by AXDEF and PLNDEF.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) The time-dependent Sun-Canopus reference frame associated with
C        a spacecraft uses the spacecraft-sun state to define the Z axis
C        and the Canopus direction to define the X-Z plane.
C
C        Find the apparent position of the Earth as seen from the Mars
C        Reconnaissance Orbiter spacecraft (MRO) at a specified time,
C        relative to the Sun-Canopus reference frame associated with
C        MRO.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File: twovxf_ex1.tm
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
C              File name                        Contents
C              ---------                        --------
C              naif0012.tls                     Leapseconds
C              de430.bsp                        Planetary ephemeris
C              mro_psp4_ssd_mro95a.bsp          MRO ephemeris
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'naif0012.tls',
C                                  'de430.bsp',
C                                  'mro_psp4_ssd_mro95a.bsp' )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM TWOVXF_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              DOUBLE PRECISION      RPD
C              DOUBLE PRECISION      JYEAR
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         META
C              PARAMETER           ( META   = 'twovxf_ex1.tm' )
C
C        C
C        C     Define the Right Ascension and Declination, and the
C        C     proper motion in both coordinates, of Canopus, relative
C        C     to the J2000 frame at J2000 epoch, in degrees and
C        C     arcsecond/yr respectively. Note that the values used here
C        C     may not be suitable for real applications.
C        C
C              DOUBLE PRECISION      RAJ2K
C              PARAMETER           ( RAJ2K   =  90.3991968556D0 )
C
C              DOUBLE PRECISION      DECJ2K
C              PARAMETER           ( DECJ2K  = -52.6956610556D0 )
C
C              DOUBLE PRECISION      PMRA
C              PARAMETER           ( PMRA    =  19.93D-3        )
C
C              DOUBLE PRECISION      PMDEC
C              PARAMETER           ( PMDEC   =  23.24D-3        )
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      CANREC ( 3    )
C              DOUBLE PRECISION      DEC
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      LT
C              DOUBLE PRECISION      PCANO  ( 3    )
C              DOUBLE PRECISION      RA
C              DOUBLE PRECISION      RPMRA
C              DOUBLE PRECISION      RPMDEC
C              DOUBLE PRECISION      STATE  ( 6    )
C              DOUBLE PRECISION      STCANO ( 6    )
C              DOUBLE PRECISION      STERTH ( 6    )
C              DOUBLE PRECISION      STSUN  ( 6    )
C              DOUBLE PRECISION      XFISC  ( 6, 6 )
C              DOUBLE PRECISION      XFORM  ( 3, 3 )
C
C              INTEGER               I
C
C        C
C        C     Load kernel files via the meta-kernel.
C        C
C              CALL FURNSH ( META )
C
C        C
C        C     Convert the TDB input time string to seconds past
C        C     J2000, TDB.
C        C
C              CALL STR2ET ( '2007 SEP 30 00:00:00 TDB', ET )
C
C        C
C        C     Define an approximate "state vector" for Canopus using
C        C     the J2000-relative, unit direction vector toward Canopus
C        C     at a specified time ET (time is needed to compute proper
C        C     motion) as position and the zero vector as velocity.
C        C
C              CALL CONVRT ( PMRA,  'ARCSECONDS', 'RADIANS', RPMRA  )
C              CALL CONVRT ( PMDEC, 'ARCSECONDS', 'RADIANS', RPMDEC )
C
C              RA  = RAJ2K  * RPD() + RPMRA  * ET/JYEAR()
C              DEC = DECJ2K * RPD() + RPMDEC * ET/JYEAR()
C
C              CALL RADREC ( 1.D0, RA, DEC, PCANO )
C
C        C
C        C     Compute MRO geometric velocity w.r.t. the Solar System
C        C     Barycenter, and use it to correct the Canopus direction
C        C     for stellar aberration.
C        C
C              CALL SPKEZR ( 'MRO', ET,    'J2000', 'NONE',
C             .              'SSB', STATE,  LT             )
C
C              CALL STELAB ( PCANO, STATE(4), STCANO       )
C
C              CALL VPACK  ( 0.D0, 0.D0, 0.D0, STCANO(4)   )
C
C        C
C        C     Let STSUN be the J2000-relative apparent state of the Sun
C        C     relative to the spacecraft at ET.
C        C
C              CALL SPKEZR ( 'SUN', ET,    'J2000', 'CN+S',
C             .              'MRO', STSUN, LT               )
C
C        C
C        C     The matrix XFISC transforms states from J2000 frame
C        C     to the Sun-Canopus reference frame at ET.
C        C
C              CALL TWOVXF ( STSUN, 3, STCANO, 1, XFISC )
C
C        C
C        C     Compute the apparent state of the Earth as seen from MRO
C        C     in the J2000 frame at ET and transform that vector into
C        C     the Sun-Canopus reference frame.
C        C
C              CALL SPKEZR ( 'EARTH', ET, 'J2000', 'CN+S',
C             .              'MRO', STATE, LT              )
C
C              CALL MXVG ( XFISC, STATE, 6, 6, STERTH )
C
C        C
C        C     Display the results.
C        C
C              WRITE(*,'(A)') 'Earth as seen from MRO in Sun-Canopus '
C             .           //  'frame (km and km/s):'
C              WRITE(*,'(A,3F16.3)') '   position:',
C             .                     ( STERTH(I), I=1,3 )
C              WRITE(*,'(A,3F16.3)') '   velocity:',
C             .                     ( STERTH(I), I=4,6 )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Earth as seen from MRO in Sun-Canopus frame (km and km/s):
C           position:   -16659764.322    97343706.915   106745539.738
C           velocity:           2.691         -10.345          -7.877
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     W.M. Owen          (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 03-SEP-2020 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example, based on existing fragment.
C
C-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) (WMO) (WLT)
C
C-&


C$ Index_Entries
C
C     define a state transformation matrix from two states
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local Variables
C
      DOUBLE PRECISION      XI     ( 6, 6 )


C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'TWOVXF' )


C
C     Get the matrix XI that transforms states from the frame
C     defined by AXDEF and PLNDEF to their base frame.
C
      CALL ZZTWOVXF ( AXDEF, INDEXA, PLNDEF, INDEXP, XI )

C
C     Invert XI.
C
      CALL INVSTM ( XI, XFORM )

      CALL CHKOUT ( 'TWOVXF' )
      RETURN
      END
