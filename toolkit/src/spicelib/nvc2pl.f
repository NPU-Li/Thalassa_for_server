C$Procedure NVC2PL ( Normal vector and constant to plane )

      SUBROUTINE NVC2PL ( NORMAL, KONST, PLANE )

C$ Abstract
C
C     Make a SPICE plane from a normal vector and a constant.
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
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               UBPL
      PARAMETER           ( UBPL    =   4 )

      DOUBLE PRECISION      NORMAL (    3 )
      DOUBLE PRECISION      KONST
      DOUBLE PRECISION      PLANE  ( UBPL )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NORMAL,
C     KONST      I   A normal vector and constant defining a plane.
C     PLANE      O   An array representing the plane.
C
C$ Detailed_Input
C
C     NORMAL,
C     KONST    are, respectively, a normal vector and constant
C              defining a plane. NORMAL need not be a unit
C              vector. Let the symbol < a, b > indicate the inner
C              product of vectors a and b; then the geometric
C              plane is the set of vectors X in three-dimensional
C              space that satisfy
C
C                 < X,  NORMAL >  =  KONST.
C
C$ Detailed_Output
C
C     PLANE    is a SPICE plane that represents the geometric
C              plane defined by NORMAL and KONST.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input vector NORMAL is the zero vector, the error
C         SPICE(ZEROVECTOR) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     SPICELIB geometry routines that deal with planes use the `plane'
C     data type to represent input and output planes. This data type
C     makes the subroutine interfaces simpler and more uniform.
C
C     The SPICELIB routines that produce SPICE planes from data that
C     define a plane are:
C
C        NVC2PL ( Normal vector and constant to plane )
C        NVP2PL ( Normal vector and point to plane    )
C        PSV2PL ( Point and spanning vectors to plane )
C
C     The SPICELIB routines that convert SPICE planes to data that
C     define a plane are:
C
C        PL2NVC ( Plane to normal vector and constant )
C        PL2NVP ( Plane to normal vector and point    )
C        PL2PSV ( Plane to point and spanning vectors )
C
C     Any of these last three routines may be used to convert this
C     routine's output, PLANE, to another representation of a
C     geometric plane.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Construct a SPICE plane from a normal vector and a constant.
C
C
C        Example code begins here.
C
C
C              PROGRAM NVC2PL_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 UBPL
C              PARAMETER             ( UBPL =   4 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION        KONST
C              DOUBLE PRECISION        PLANE  ( UBPL )
C              DOUBLE PRECISION        NORMAL ( 3    )
C              DOUBLE PRECISION        OKONST
C              DOUBLE PRECISION        ONORML ( 3    )
C
C        C
C        C     Set the normal vector and the constant defining the
C        C     plane.
C        C
C              DATA                    NORMAL /  1.D0, 1.D0, 1.D0 /
C
C              KONST = 23.D0
C
C              WRITE(*,'(A)') 'Inputs:'
C              WRITE(*,'(A,3F12.7)') '   Normal vector:', NORMAL
C              WRITE(*,'(A,F12.7)')  '   Constant     :', KONST
C              WRITE(*,*) ' '
C
C        C
C        C     Make a SPICE plane from NORMAL and KONST.
C        C     NORMAL need not be a unit vector.
C        C
C              CALL NVC2PL ( NORMAL, KONST,  PLANE         )
C
C        C
C        C     Print the results.
C        C
C              CALL PL2NVC ( PLANE, ONORML, OKONST )
C              WRITE(*,'(A)') 'Generated plane:'
C              WRITE(*,'(A,3F12.7)') '   Normal vector:', ONORML
C              WRITE(*,'(A,F12.7)')  '   Constant     :', OKONST
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Inputs:
C           Normal vector:   1.0000000   1.0000000   1.0000000
C           Constant     :  23.0000000
C
C        Generated plane:
C           Normal vector:   0.5773503   0.5773503   0.5773503
C           Constant     :  13.2790562
C
C
C     2) Apply a linear transformation represented by a matrix to
C        a plane represented by a normal vector and a constant.
C
C        Find a normal vector and constant for the transformed plane.
C
C
C        Example code begins here.
C
C
C              PROGRAM NVC2PL_EX2
C              IMPLICIT NONE
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 UBPL
C              PARAMETER             ( UBPL =   4 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION        AXDEF  ( 3    )
C              DOUBLE PRECISION        KONST
C              DOUBLE PRECISION        PLANE  ( UBPL )
C              DOUBLE PRECISION        M      ( 3, 3 )
C              DOUBLE PRECISION        NORMAL ( 3    )
C              DOUBLE PRECISION        PLNDEF ( 3    )
C              DOUBLE PRECISION        POINT  ( 3    )
C              DOUBLE PRECISION        SPAN1  ( 3    )
C              DOUBLE PRECISION        SPAN2  ( 3    )
C              DOUBLE PRECISION        TKONST
C              DOUBLE PRECISION        TNORML ( 3    )
C              DOUBLE PRECISION        TPLANE ( UBPL )
C              DOUBLE PRECISION        TPOINT ( 3    )
C              DOUBLE PRECISION        TSPAN1 ( 3    )
C              DOUBLE PRECISION        TSPAN2 ( 3    )
C
C        C
C        C     Set the normal vector and the constant defining the
C        C     initial plane.
C        C
C              DATA                    NORMAL /
C             .               -0.1616904D0, 0.8084521D0, -0.5659165D0 /
C
C              DATA                    KONST  /  4.8102899D0 /
C
C        C
C        C     Define a transformation matrix to the right-handed
C        C     reference frame having the +i unit vector as primary
C        C     axis, aligned to the original frame's +X axis, and
C        C     the -j unit vector as second axis, aligned to the +Y
C        C     axis.
C        C
C              DATA                    AXDEF  /  1.D0,  0.D0,  0.D0 /
C              DATA                    PLNDEF /  0.D0, -1.D0,  0.D0 /
C
C
C              CALL TWOVEC ( AXDEF, 1, PLNDEF, 2, M )
C
C        C
C        C     Make a SPICE plane from NORMAL and KONST, and then
C        C     find a point in the plane and spanning vectors for the
C        C     plane.  NORMAL need not be a unit vector.
C        C
C              CALL NVC2PL ( NORMAL, KONST,  PLANE         )
C              CALL PL2PSV ( PLANE,  POINT,  SPAN1,  SPAN2 )
C
C        C
C        C     Apply the linear transformation to the point and
C        C     spanning vectors.  All we need to do is multiply
C        C     these vectors by M, since for any linear
C        C     transformation T,
C        C
C        C           T ( POINT  +  t1 * SPAN1     +  t2 * SPAN2 )
C        C
C        C        =  T (POINT)  +  t1 * T(SPAN1)  +  t2 * T(SPAN2),
C        C
C        C     which means that T(POINT), T(SPAN1), and T(SPAN2)
C        C     are a point and spanning vectors for the transformed
C        C     plane.
C        C
C              CALL MXV ( M, POINT, TPOINT )
C              CALL MXV ( M, SPAN1, TSPAN1 )
C              CALL MXV ( M, SPAN2, TSPAN2 )
C
C        C
C        C     Make a new SPICE plane TPLANE from the
C        C     transformed point and spanning vectors, and find a
C        C     unit normal and constant for this new plane.
C        C
C              CALL PSV2PL ( TPOINT, TSPAN1, TSPAN2, TPLANE )
C              CALL PL2NVC ( TPLANE, TNORML, TKONST         )
C
C        C
C        C     Print the results.
C        C
C              WRITE(*,'(A,3F12.7)') 'Unit normal vector:', TNORML
C              WRITE(*,'(A,F12.7)')  'Constant          :', TKONST
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Unit normal vector:  -0.1616904  -0.8084521   0.5659165
C        Constant          :   4.8102897
C
C
C$ Restrictions
C
C     1)  No checking is done to prevent arithmetic overflow.
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
C-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
C
C        Changed the input argument name CONST to KONST for consistency
C        with other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section.
C
C        Added complete code examples.
C
C-    SPICELIB Version 1.1.1, 02-NOV-2009 (NJB)
C
C        Corrected header typo.
C
C-    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VMINUS call.
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
C     normal vector and constant to plane
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN


C
C     Local parameters
C

C
C     The contents of SPICE planes are as follows:
C
C        Elements NMLPOS through NMLPOS + 2 contain a unit normal
C        vector for the plane.
C
C        Element CONPOS contains a constant for the plane;  every point
C        X in the plane satisfies
C
C           < X, PLANE(NMLPOS) >  =  PLANE(CONPOS).
C
C        The plane constant is the distance of the plane from the
C        origin; the normal vector, scaled by the constant, is the
C        closest point in the plane to the origin.
C
C
      INTEGER               NMLPOS
      PARAMETER           ( NMLPOS = 1 )

      INTEGER               CONPOS
      PARAMETER           ( CONPOS = 4 )

C
C     Local variables
C
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      TMPVEC ( 3 )

C
C     This routine checks in only if an error is discovered.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL UNORM ( NORMAL, PLANE(NMLPOS), MAG )

C
C     The normal vector must be non-zero.
C
      IF ( MAG .EQ. 0 ) THEN

         CALL CHKIN  ( 'NVC2PL'                            )
         CALL SETMSG ( 'Plane''s normal must be non-zero.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                 )
         CALL CHKOUT ( 'NVC2PL'                            )
         RETURN

      END IF

C
C     To find the plane constant corresponding to the unitized normal
C     vector, we observe that
C
C        < X, NORMAL > = KONST,
C
C     so
C
C        < X, NORMAL / || NORMAL || >   =   KONST / || NORMAL ||
C
C
      PLANE(CONPOS) =  KONST / MAG

C
C     The constant should be the distance of the plane from the
C     origin.  If the constant is negative, negate both it and the
C     normal vector.
C
      IF ( PLANE(CONPOS) .LT. 0.D0 ) THEN

         PLANE(CONPOS) =   -PLANE(CONPOS)
         CALL VMINUS     (  PLANE(NMLPOS), TMPVEC        )
         CALL VEQU       (  TMPVEC,        PLANE(NMLPOS) )

      END IF

      RETURN
      END
