C$Procedure PL2NVC ( Plane to normal vector and constant )

      SUBROUTINE PL2NVC ( PLANE, NORMAL, KONST )

C$ Abstract
C
C     Return a unit normal vector and constant that define a specified
C     plane.
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

      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      NORMAL (    3 )
      DOUBLE PRECISION      KONST

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PLANE      I   A SPICE plane.
C     NORMAL,
C     KONST      O   A normal vector and constant defining the
C                    geometric plane represented by PLANE.
C     UBPL       P   SPICE plane upper bound.
C
C$ Detailed_Input
C
C     PLANE    is a SPICE plane.
C
C$ Detailed_Output
C
C     NORMAL,
C     KONST    are, respectively, a unit normal vector and
C              constant that define the geometric plane
C              represented by PLANE. Let the symbol < A, B >
C              indicate the inner product of vectors A and B;
C              then the geometric plane is the set of vectors X
C              in three-dimensional space that satisfy
C
C                 < X,  NORMAL >  =  KONST.
C
C              NORMAL is a unit vector. KONST is the distance of
C              the plane from the origin;
C
C                 KONST * NORMAL
C
C              is the closest point in the plane to the origin.
C
C$ Parameters
C
C     UBPL     is the upper bound of a SPICE plane array.
C
C$ Exceptions
C
C     Error free.
C
C     1)  The input plane MUST have been created by one of the SPICELIB
C         routines
C
C            NVC2PL ( Normal vector and constant to plane )
C            NVP2PL ( Normal vector and point to plane    )
C            PSV2PL ( Point and spanning vectors to plane )
C
C         Otherwise, the results of this routine are unpredictable.
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
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Determine the distance of a plane from the origin, and
C        confirm the result by calculating the dot product (inner
C        product) of a vector from the origin to the plane and a
C        vector in that plane.
C
C        The dot product between these two vectors should be zero,
C        to double precision round-off, so orthogonal to that
C        precision.
C
C
C        Example code begins here.
C
C
C              PROGRAM PL2NVC_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION        VDOT
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
C              DOUBLE PRECISION        DOTP
C              DOUBLE PRECISION        KONST
C              DOUBLE PRECISION        PLANE  ( UBPL )
C              DOUBLE PRECISION        NORMAL ( 3    )
C              DOUBLE PRECISION        PLNVEC ( 3    )
C              DOUBLE PRECISION        POINT  ( 3    )
C              DOUBLE PRECISION        VEC    ( 3    )
C
C        C
C        C     Define the plane with a vector normal to the plan
C        C     and a point in the plane.
C        C
C              DATA                    NORMAL / -1.D0,  5.D0,   -3.5D0 /
C              DATA                    POINT  /  9.D0, -0.65D0, -12.D0 /
C
C        C
C        C     Create the SPICE plane from the normal and point.
C        C
C              CALL NVP2PL ( NORMAL, POINT, PLANE )
C
C        C
C        C     Calculate the normal vector and constant defining
C        C     the plane. The constant value is the distance from
C        C     the origin to the plane.
C        C
C              CALL PL2NVC ( PLANE, NORMAL, KONST )
C              WRITE(*,'(A,F12.7)') 'Distance to the plane:', KONST
C
C        C
C        C     Confirm the results. Calculate a vector
C        C     from the origin to the plane.
C        C
C              CALL VSCL ( KONST, NORMAL, VEC )
C              WRITE(*,'(A,3F12.7)') 'Vector from origin   :', VEC
C              WRITE(*,*) ' '
C
C        C
C        C     Now calculate a vector in the plane from the
C        C     location in the plane defined by VEC.
C        C
C              CALL VSUB ( VEC, POINT, PLNVEC )
C
C        C
C        C     These vectors should be orthogonal.
C        C
C              WRITE(*,'(A,F12.7)') 'dot product          :',
C             .                     VDOT( PLNVEC, VEC )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Distance to the plane:   4.8102899
C        Vector from origin   :  -0.7777778   3.8888889  -2.7222222
C
C        dot product          :  -0.0000000
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
C              PROGRAM PL2NVC_EX2
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
C     None.
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
C        Changed the output argument name CONST to KONST for consistency
C        with other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code examples. Added documentation of the parameter UBPL.
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
C     plane to normal vector and constant
C
C-&


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
C     Note for programmers: validity of the input plane is not
C     checked in the interest of efficiency. The input plane will be
C     valid if it was created by one of the SPICE plane construction
C     routines.
C
C     Unpack the plane.
C
      CALL VEQU ( PLANE(NMLPOS), NORMAL )
      KONST   =   PLANE(CONPOS)

      RETURN
      END
