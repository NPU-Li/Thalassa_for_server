C$Procedure VPRJP ( Vector projection onto plane )

      SUBROUTINE VPRJP ( VIN, PLANE, VOUT )

C$ Abstract
C
C     Project a vector onto a specified plane, orthogonally.
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
C     PLANES
C
C$ Keywords
C
C     GEOMETRY
C     MATH
C     PLANE
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               UBPL
      PARAMETER           ( UBPL   =   4 )

      DOUBLE PRECISION      VIN   (    3 )
      DOUBLE PRECISION      PLANE ( UBPL )
      DOUBLE PRECISION      VOUT  (    3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VIN        I   Vector to be projected.
C     PLANE      I   A SPICE plane onto which VIN is projected.
C     VOUT       O   Vector resulting from projection.
C     UBPL       P   SPICE plane upper bound.
C
C$ Detailed_Input
C
C     VIN      is a 3-vector that is to be orthogonally projected
C              onto a specified plane.
C
C     PLANE    is a SPICE plane that represents the geometric
C              plane onto which VIN is to be projected.
C
C              The normal vector component of a SPICE plane has
C              unit length.
C
C$ Detailed_Output
C
C     VOUT     is the vector resulting from the orthogonal
C              projection of VIN onto PLANE. VOUT is the closest
C              point in the specified plane to VIN.
C
C$ Parameters
C
C     UBPL     is the upper bound of a SPICE plane array.
C
C$ Exceptions
C
C     1)  If the normal vector of the input plane does not have unit
C         length (allowing for round-off error), the error
C         SPICE(NONUNITNORMAL) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Projecting a vector VIN orthogonally onto a plane can be thought
C     of as finding the closest vector in the plane to VIN. This
C     "closest vector" always exists; it may be coincident with the
C     original vector.
C
C     Two related routines are VPRJPI, which inverts an orthogonal
C     projection of a vector onto a plane, and VPROJ, which projects
C     a vector orthogonally onto another vector.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Find the closest point in the ring plane of a planet to a
C        spacecraft located at a point (in body-fixed coordinates).
C
C
C        Example code begins here.
C
C
C              PROGRAM VPRJP_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C        C     Upper bound of plane length.
C        C
C              INTEGER                 UBPL
C              PARAMETER             ( UBPL = 4 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      NORM   ( 3    )
C              DOUBLE PRECISION      ORIG   ( 3    )
C              DOUBLE PRECISION      PROJ   ( 3    )
C              DOUBLE PRECISION      RINGPL ( UBPL )
C              DOUBLE PRECISION      SCPOS  ( 3    )
C
C        C
C        C     Set the spacecraft location and define the normal
C        C     vector as the normal to the equatorial plane, and
C        C     the origin at the body/ring center.
C        C
C              DATA                  SCPOS /  -29703.16955D0,
C             .                               879765.72163D0,
C             .                              -137280.21757D0   /
C
C              DATA                  NORM  /  0.D0, 0.D0, 1.D0 /
C
C              DATA                  ORIG  /  0.D0, 0.D0, 0.D0 /
C
C        C
C        C     Create the plane structure.
C        C
C              CALL NVP2PL ( NORM, ORIG, RINGPL )
C
C        C
C        C     Project the position vector onto the ring plane.
C        C
C              CALL VPRJP ( SCPOS, RINGPL, PROJ )
C
C              WRITE(*,'(A)') 'Projection of S/C position onto ring '
C             .            // 'plane:'
C              WRITE(*,'(3F17.5)') PROJ
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Projection of S/C position onto ring plane:
C             -29703.16955     879765.72163          0.00000
C
C
C$ Restrictions
C
C     1)  It is recommended that the input plane be created by one of
C         the SPICELIB routines
C
C            NVC2PL ( Normal vector and constant to plane )
C            NVP2PL ( Normal vector and point to plane    )
C            PSV2PL ( Point and spanning vectors to plane )
C
C         In any case the input plane must have a unit length normal
C         vector and a plane constant consistent with the normal
C         vector.
C
C$ Literature_References
C
C     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
C          7th Edition, Addison Wesley, 1988.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 24-AUG-2021 (NJB) (JDR)
C
C        Added error check for non-unit plane normal vector.
C        Changed check-in style to discovery.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example. Added documentation of the parameter UBPL.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
C
C-&


C$ Index_Entries
C
C     vector projection onto plane
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               APPROX
      LOGICAL               RETURN

C
C     Local parameters
C
C     Tolerance for deviation from unit length of the normal
C     vector of the input plane.
C
      DOUBLE PRECISION      MAGTOL
      PARAMETER           ( MAGTOL = 1.D-14 )

C
C     Local variables
C
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      NORMAL ( 3 )

C
C     Check RETURN but use discovery check-in.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

C
C     Obtain a unit vector normal to the input plane, and a constant
C     for the plane.
C
      CALL PL2NVC ( PLANE, NORMAL, CONST )

C
C     The normal vector returned by PL2NVC should be a unit vector.
C
      IF (  .NOT.  APPROX( VNORM(NORMAL), 1.D0, MAGTOL )  ) THEN

         CALL CHKIN  ( 'VPRJP' )
         CALL SETMSG ( 'Normal vector returned by PL2NVC does not '
     .   //            'have unit length; the difference of the '
     .   //            'length from 1 is #. The input plane is '
     .   //            'invalid. '                                )
         CALL ERRDP  ( '#',  VNORM(NORMAL) - 1.D0                 )
         CALL SIGERR ( 'SPICE(NONUNITNORMAL)'                     )
         CALL CHKOUT ( 'VPRJP' )
         RETURN

      END IF

C
C     Let the notation < a, b > indicate the inner product of vectors
C     a and b.
C
C     VIN differs from its projection onto PLANE by some multiple of
C     NORMAL. That multiple is
C
C
C               < VIN - VOUT, NORMAL >                 *  NORMAL
C
C        =   (  < VIN, NORMAL > - < VOUT, NORMAL >  )  *  NORMAL
C
C        =   (  < VIN, NORMAL > - CONST             )  *  NORMAL
C
C
C     Subtracting this multiple of NORMAL from VIN yields VOUT.
C
      CALL VLCOM (  1.0D0,
     .              VIN,
     .              CONST - VDOT ( VIN, NORMAL ),
     .              NORMAL,
     .              VOUT                          )

      RETURN
      END
