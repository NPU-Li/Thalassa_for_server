C$Procedure EDLIMB   ( Ellipsoid Limb )

      SUBROUTINE EDLIMB ( A, B, C, VIEWPT, LIMB )

C$ Abstract
C
C     Find the limb of a triaxial ellipsoid, viewed from a specified
C     point.
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
C     ELLIPSES
C
C$ Keywords
C
C     ELLIPSE
C     ELLIPSOID
C     GEOMETRY
C     MATH
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               UBEL
      PARAMETER           ( UBEL   =    9 )

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      VIEWPT (    3 )
      DOUBLE PRECISION      LIMB   ( UBEL )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     A          I   Length of ellipsoid semi-axis lying on the x-axis.
C     B          I   Length of ellipsoid semi-axis lying on the y-axis.
C     C          I   Length of ellipsoid semi-axis lying on the z-axis.
C     VIEWPT     I   Location of viewing point.
C     LIMB       O   Limb of ellipsoid as seen from viewing point.
C
C$ Detailed_Input
C
C     A,
C     B,
C     C        are the lengths of the semi-axes of a triaxial
C              ellipsoid. The ellipsoid is centered at the
C              origin and oriented so that its axes lie on the
C              x, y and z axes.  A, B, and C are the lengths of
C              the semi-axes that point in the x, y, and z
C              directions respectively.
C
C     VIEWPT   is a point from which the ellipsoid is viewed.
C              VIEWPT must be outside of the ellipsoid.
C
C$ Detailed_Output
C
C     LIMB     is a SPICE ellipse that represents the limb of
C              the ellipsoid.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the length of any semi-axis of the ellipsoid is
C         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled.
C         LIMB is not modified.
C
C     2)  If the length of any semi-axis of the ellipsoid is zero after
C         the semi-axis lengths are scaled by the reciprocal of the
C         magnitude of the longest semi-axis and then squared, the error
C         SPICE(DEGENERATECASE) is signaled. LIMB is not modified.
C
C     3)  If the viewing point VIEWPT is inside the ellipse, the error
C         SPICE(INVALIDPOINT) is signaled. LIMB is not modified.
C
C     4)  If the geometry defined by the input ellipsoid and viewing
C         point is so extreme that the limb cannot be found, the error
C         SPICE(DEGENERATECASE) is signaled.
C
C     5)  If the shape of the ellipsoid and the viewing geometry are
C         such that the limb is an excessively flat ellipsoid, the
C         limb may be a degenerate ellipse. You must determine whether
C         this possibility poses a problem for your application.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The limb of a body, as seen from a viewing point, is the boundary
C     of the portion of the body's surface that is visible from that
C     viewing point. In this definition, we consider a surface point
C     to be `visible' if it can be connected to the viewing point by a
C     line segment that doesn't pass through the body. This is a purely
C     geometrical definition that ignores the matter of which portions
C     of the surface are illuminated, or whether the view is obscured by
C     any additional objects.
C
C     If a body is modeled as a triaxial ellipsoid, the limb is always
C     an ellipse. The limb is determined by its center, a semi-major
C     axis vector, and a semi-minor axis vector.
C
C     We note that the problem of finding the limb of a triaxial
C     ellipsoid is mathematically identical to that of finding its
C     terminator, if one makes the simplifying assumption that the
C     terminator is the limb of the body as seen from the vertex of the
C     umbra. So, this routine can be used to solve this simplified
C     version of the problem of finding the terminator.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given an ellipsoid and a viewpoint exterior to it, calculate
C        the limb ellipse as seen from that viewpoint.
C
C
C        Example code begins here.
C
C
C              PROGRAM EDLIMB_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 UBEL
C              PARAMETER             ( UBEL =   9 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION        A
C              DOUBLE PRECISION        B
C              DOUBLE PRECISION        C
C              DOUBLE PRECISION        ECENTR ( 3    )
C              DOUBLE PRECISION        LIMB   ( UBEL )
C              DOUBLE PRECISION        SMAJOR ( 3    )
C              DOUBLE PRECISION        SMINOR ( 3    )
C              DOUBLE PRECISION        VIEWPT ( 3    )
C
C        C
C        C     Define a viewpoint exterior to the ellipsoid.
C        C
C              DATA                    VIEWPT /  2.D0,  0.D0,  0.D0 /
C
C        C
C        C     Define an ellipsoid.
C        C
C              A = SQRT( 2.D0 )
C              B = 2.D0 * SQRT( 2.D0 )
C              C = SQRT( 2.D0 )
C
C        C
C        C     Calculate the limb ellipse as seen by from the
C        C     viewpoint.
C        C
C              CALL EDLIMB ( A, B, C, VIEWPT, LIMB )
C
C        C
C        C     Output the structure components.
C        C
C              CALL EL2CGV ( LIMB, ECENTR, SMAJOR, SMINOR )
C
C              WRITE(*,'(A)') 'Limb ellipse as seen from viewpoint:'
C              WRITE(*,'(A,3F11.6)') '   Semi-minor axis:', SMINOR
C              WRITE(*,'(A,3F11.6)') '   Semi-major axis:', SMAJOR
C              WRITE(*,'(A,3F11.6)') '   Center         :', ECENTR
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Limb ellipse as seen from viewpoint:
C           Semi-minor axis:   0.000000   0.000000  -1.000000
C           Semi-major axis:   0.000000   2.000000  -0.000000
C           Center         :   1.000000   0.000000   0.000000
C
C
C     2) We'd like to find the apparent limb of Jupiter, corrected for
C        light time and stellar aberration, as seen from JUNO
C        spacecraft's position at a given UTC time.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: edlimb_ex2.tm
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
C              File name                           Contents
C              ---------                           --------
C              juno_rec_160522_160729_160909.bsp   JUNO s/c ephemeris
C              pck00010.tpc                        Planet orientation
C                                                  and radii
C              naif0012.tls                        Leapseconds
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'juno_rec_160522_160729_160909.bsp',
C                                  'pck00010.tpc',
C                                  'naif0012.tls'  )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM EDLIMB_EX2
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)           UTCSTR
C              PARAMETER             ( UTCSTR = '2016 Jul 14 19:45:00' )
C
C              INTEGER                 UBEL
C              PARAMETER             ( UBEL =   9 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION        CENTER ( 3    )
C              DOUBLE PRECISION        ET
C              DOUBLE PRECISION        JPOS   ( 3    )
C              DOUBLE PRECISION        LIMB   ( UBEL )
C              DOUBLE PRECISION        LT
C              DOUBLE PRECISION        RAD    ( 3    )
C              DOUBLE PRECISION        SMAJOR ( 3    )
C              DOUBLE PRECISION        SMINOR ( 3    )
C              DOUBLE PRECISION        SCPJFC ( 3    )
C              DOUBLE PRECISION        SCPOS  ( 3    )
C              DOUBLE PRECISION        TIPM   ( 3, 3 )
C
C              INTEGER                 N
C
C        C
C        C     Load the required kernels.
C        C
C              CALL FURNSH ( 'edlimb_ex2.tm' )
C
C        C
C        C     Find the viewing point in Jupiter-fixed coordinates. To
C        C     do this, find the apparent position of Jupiter as seen
C        C     from the spacecraft in Jupiter-fixed coordinates and
C        C     negate this vector. In this case we'll use light time
C        C     and stellar aberration corrections to arrive at the
C        C     apparent limb. JPOS is the Jupiter's position as seen
C        C     from the spacecraft.  SCPOS is the spacecraft's position
C        C     relative to Jupiter.
C        C
C              CALL STR2ET ( UTCSTR,    ET )
C              CALL SPKPOS ( 'JUPITER', ET, 'J2000', 'LT+S', 'JUNO',
C             .               JPOS,     LT                         )
C
C              CALL VMINUS ( JPOS, SCPOS )
C
C        C
C        C     Get Jupiter's semi-axis lengths...
C        C
C              CALL BODVRD ( 'JUPITER', 'RADII', 3, N, RAD )
C
C        C
C        C     ...and the transformation from J2000 to Jupiter
C        C     equator and prime meridian coordinates. Note that we
C        C     use the orientation of Jupiter at the time of
C        C     emission of the light that arrived at the
C        C     spacecraft at time ET.
C        C
C              CALL PXFORM ( 'J2000', 'IAU_JUPITER', ET-LT, TIPM )
C
C        C
C        C     Transform the spacecraft's position into Jupiter-
C        C     fixed coordinates.
C        C
C              CALL MXV ( TIPM, SCPOS, SCPJFC )
C
C        C
C        C     Find the apparent limb.  LIMB is a SPICE ellipse
C        C     representing the limb.
C        C
C              CALL EDLIMB ( RAD(1), RAD(2), RAD(3), SCPJFC, LIMB )
C
C        C
C        C     CENTER, SMAJOR, and SMINOR are the limb's center,
C        C     semi-major axis of the limb, and a semi-minor axis
C        C     of the limb.  We obtain these from LIMB using the
C        C     SPICELIB routine EL2CGV ( Ellipse to center and
C        C     generating vectors ).
C        C
C              CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )
C
C        C
C        C     Output the structure components.
C        C
C              WRITE(*,'(A)') 'Apparent limb of Jupiter as seen '
C             .            // 'from JUNO:'
C              WRITE(*,'(2A)')       '   UTC time       : ', UTCSTR
C              WRITE(*,'(A,3F14.6)') '   Semi-minor axis:', SMINOR
C              WRITE(*,'(A,3F14.6)') '   Semi-major axis:', SMAJOR
C              WRITE(*,'(A,3F14.6)') '   Center         :', CENTER
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Apparent limb of Jupiter as seen from JUNO:
C           UTC time       : 2016 Jul 14 19:45:00
C           Semi-minor axis:  12425.547643  -5135.572410  65656.053303
C           Semi-major axis:  27305.667297  66066.222576      0.000000
C           Center         :    791.732472   -327.228993   -153.408849
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
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.4.0, 24-AUG-2021 (NJB) (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C        Corrected several header comment typos.
C
C-    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCLG call. Updated header to refer to BODVCD instead
C        of BODVAR.
C
C-    SPICELIB Version 1.2.0, 06-OCT-1993 (NJB)
C
C        Declaration of unused local variable NEAR was removed.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB)
C
C        Error message and description changed for non-positive
C        axis length error.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
C
C-&


C$ Index_Entries
C
C     ellipsoid limb
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB)
C
C        Error message and description changed for non-positive
C        axis length error. The former message and description did
C        not match, and the description was incorrect: it described
C        `zero-length', rather than `non-positive' axes as invalid.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               UBPL
      PARAMETER           ( UBPL   =    4 )


C
C     Local variables
C
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      LPLANE ( UBPL )
      DOUBLE PRECISION      NORMAL (    3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLA
      DOUBLE PRECISION      SCLA2
      DOUBLE PRECISION      SCLB
      DOUBLE PRECISION      SCLB2
      DOUBLE PRECISION      SCLC
      DOUBLE PRECISION      SCLC2
      DOUBLE PRECISION      TMPEL  ( UBEL )
      DOUBLE PRECISION      V      (    3 )

      LOGICAL               FOUND


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EDLIMB' )
      END IF

C
C     The semi-axes must have positive length.
C
      IF (       ( A .LE. 0.D0 )
     .     .OR.  ( B .LE. 0.D0 )
     .     .OR.  ( C .LE. 0.D0 )   )   THEN

         CALL SETMSG ( 'Semi-axis lengths:  A = #, B = #, C = #. ' )
         CALL ERRDP  ( '#', A                                      )
         CALL ERRDP  ( '#', B                                      )
         CALL ERRDP  ( '#', C                                      )
         CALL SIGERR ( 'SPICE(INVALIDAXISLENGTH)'                  )
         CALL CHKOUT ( 'EDLIMB'                                    )
         RETURN

      END IF

C
C     Scale the semi-axes lengths for better numerical behavior.
C     If squaring any one of the scaled lengths causes it to
C     underflow to zero, we cannot continue the computation. Otherwise,
C     scale the viewing point too.
C
      SCALE  =  MAX ( DABS(A), DABS(B), DABS(C) )

      SCLA   =  A / SCALE
      SCLB   =  B / SCALE
      SCLC   =  C / SCALE

      SCLA2  =  SCLA**2
      SCLB2  =  SCLB**2
      SCLC2  =  SCLC**2

      IF (       ( SCLA2  .EQ.  0.D0 )
     .     .OR.  ( SCLB2  .EQ.  0.D0 )
     .     .OR.  ( SCLC2  .EQ.  0.D0 )   )   THEN

         CALL SETMSG ( 'Semi-axis too small:  A = #, B = #, C = #. ' )
         CALL ERRDP  ( '#', A                                        )
         CALL ERRDP  ( '#', B                                        )
         CALL ERRDP  ( '#', C                                        )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                       )
         CALL CHKOUT ( 'EDLIMB'                                      )
         RETURN

      END IF

      CALL VSCL ( 1.0D0 / SCALE, VIEWPT, V )

C
C     The viewing point must be outside of the ellipsoid.  LEVEL is the
C     constant of the level surface that V lies on.  The ellipsoid
C     itself is the level surface corresponding to LEVEL = 1.
C
      LEVEL   =     ( V(1)**2 / SCLA2 )
     .           +  ( V(2)**2 / SCLB2 )
     .           +  ( V(3)**2 / SCLC2 )

      IF ( LEVEL .LT. 1.D0 ) THEN

         CALL SETMSG ( 'Viewing point is inside the ellipsoid.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                  )
         CALL CHKOUT ( 'EDLIMB'                                 )
         RETURN

      END IF

C
C     Find a normal vector for the limb plane.
C
C     To compute this vector, we use the fact that the surface normal at
C     each limb point is orthogonal to the line segment connecting the
C     viewing point and the limb point.   Let the notation
C
C        < a, b >
C
C     indicate the dot product of the vectors a and b.  If we call the
C     viewing point V and the limb point X, then
C
C
C
C                            X(1)         X(2)         X(3)
C        0  =   < V - X,  ( -------- ,   -------- ,   --------  )  >
C                                2           2             2
C                            SCLA        SCLB          SCLC
C
C
C                            X(1)         X(2)         X(3)
C           =   <   V,    ( -------- ,   -------- ,   --------  )  >
C                                2           2             2
C                            SCLA        SCLB          SCLC
C
C
C                            X(1)         X(2)         X(3)
C            - <   X,    ( -------- ,   -------- ,   --------  )  >
C                                2           2             2
C                            SCLA        SCLB          SCLC
C
C                                2           2             2
C                            X(1)        X(2)          X(3)
C           =             --------  +   --------  +  --------
C                               2            2             2
C                           SCLA         SCLB          SCLC
C
C
C           =   1
C
C
C     This last equation is just the equation of the scaled ellipsoid.
C     We can combine the last two equalities and interchange the
C     positions of X and V to obtain
C
C
C                      V(1)         V(2)         V(3)
C        <   X,    ( -------- ,   -------- ,   --------  )  >   =   1
C                          2           2             2
C                      SCLA        SCLB          SCLC
C
C
C     This is the equation of the limb plane.
C

C
C     Put together a SPICE plane, LPLANE, that represents the limb
C     plane.
C
      NORMAL(1)  =  V(1) / SCLA2
      NORMAL(2)  =  V(2) / SCLB2
      NORMAL(3)  =  V(3) / SCLC2

      CALL NVC2PL ( NORMAL, 1.0D0, LPLANE )

C
C     Find the limb by intersecting the limb plane with the ellipsoid.
C
      CALL INEDPL ( SCLA,  SCLB,  SCLC,  LPLANE,  LIMB,  FOUND )

C
C     FOUND should be true unless we've encountered numerical problems.
C
      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Ellipsoid shape and viewing geometry are too '//
     .                 'extreme; the limb was not found. '             )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                         )
         CALL CHKOUT ( 'EDLIMB'                                        )
         RETURN

      END IF

C
C     Undo the scaling before returning the limb.
C
      CALL VSCLG ( SCALE, LIMB, UBEL, TMPEL )
      CALL MOVED ( TMPEL, UBEL,       LIMB  )

      CALL CHKOUT ( 'EDLIMB' )
      RETURN
      END
