C$Procedure FRAME ( Build a right handed coordinate frame )

      SUBROUTINE FRAME ( X, Y, Z )

C$ Abstract
C
C     Build a right handed orthonormal frame (x,y,z) from a
C     3-dimensional input vector, where the X-axis of the resulting
C     frame is parallel to the original input vector.
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
C     FRAME
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION    X ( 3 )
      DOUBLE PRECISION    Y ( 3 )
      DOUBLE PRECISION    Z ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ------------------------------------------------
C     X         I-O  Input vector. A parallel unit vector on output.
C     Y          O   Unit vector in the plane orthogonal to X.
C     Z          O   Unit vector given by the cross product <X,Y>.
C
C$ Detailed_Input
C
C     X        is a 3-dimensional vector used to form the first vector
C              of a right-handed orthonormal triple.
C
C$ Detailed_Output
C
C     X,
C     Y,
C     Z        are the 3-dimensional unit vectors that form a right
C              handed orthonormal frame, where X is now a unit vector
C              parallel to the original input vector in X. There are no
C              special geometric properties connected to Y and Z (other
C              than that they complete the right handed frame).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If X on input is the zero vector the "standard" frame (ijk)
C         is returned.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given an input vector X, this routine returns unit vectors X,
C     Y, and Z such that XYZ forms a right-handed orthonormal frame
C     where the output X is parallel to the input X.
C
C     This routine is intended primarily to provide a basis for
C     the plane orthogonal to X. There are no special properties
C     associated with Y and Z other than that the resulting XYZ frame
C     is right handed and orthonormal. There are an infinite
C     collection of pairs (Y,Z) that could be used to this end.
C     Even though for a given X, Y and Z are uniquely
C     determined, users
C     should regard the pair (Y,Z) as a random selection from this
C     infinite collection.
C
C     For instance, when attempting to determine the locus of points
C     that make up the limb of a triaxial body, it is a straightforward
C     matter to determine the normal to the limb plane. To find
C     the actual parametric equation of the limb one needs to have
C     a basis of the plane. This routine can be used to get a basis
C     in which one can describe the curve and from which one can
C     then determine the principal axes of the limb ellipse.
C
C$ Examples
C
C     In addition to using a vector to construct a right handed frame
C     with the x-axis aligned with the input vector, one can construct
C     right handed frames with any of the axes aligned with the input
C     vector.
C
C     For example suppose we want a right hand frame XYZ with the
C     Z-axis aligned with some vector V. Assign V to Z
C
C           Z(1) = V(1)
C           Z(2) = V(2)
C           Z(3) = V(3)
C
C     Then call FRAME with the arguments X,Y,Z cycled so that Z
C     appears first.
C
C           CALL FRAME (Z, X, Y)
C
C     The resulting XYZ frame will be orthonormal with Z parallel
C     to the vector V.
C
C     To get an XYZ frame with Y parallel to V perform the following
C
C           Y(1) = V(1)
C           Y(2) = V(2)
C           Y(3) = V(3)
C
C           CALL FRAME (Y, Z, X)
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
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 03-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Improved
C        argument descriptions.
C
C-    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VHAT call.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&


C$ Index_Entries
C
C     build a right handed coordinate frame
C
C-&


C$ Revisions
C
C-    Beta Version 2.0.0, 29-DEC-1988 (WLT) (IMU)
C
C        The routine was modified so that it now accepts any input
C        vector in the X slot (it originally was assumed to be a unit
C        vector).  Moreover, the original algorithm has been streamlined
C        a great deal to take advantage of our knowledge of the
C        internal structure of the orthonormal triple.
C
C-&


C
C
C     Local variables
C
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      F

      INTEGER               S1
      INTEGER               S2
      INTEGER               S3



C
C     First make X into a unit vector.
C
      CALL VHATIP ( X )

C
C     We'll need the squares of the components of X in a bit.
C
      A = X(1)*X(1)
      B = X(2)*X(2)
      C = X(3)*X(3)

C
C     If X is zero, then just return the ijk frame.
C
      IF ( A+B+C .EQ. 0.0D0 ) THEN

         X(1) = 1.0D0
         X(2) = 0.0D0
         X(3) = 0.0D0

         Y(1) = 0.0D0
         Y(2) = 1.0D0
         Y(3) = 0.0D0

         Z(1) = 0.0D0
         Z(2) = 0.0D0
         Z(3) = 1.0D0

         RETURN

      END IF


C
C     If we make it this far, determine which component of X has the
C     smallest magnitude.  This component will be zero in Y. The other
C     two components of X will put into Y swapped with the sign of
C     the first changed.  From there, Z can have only one possible
C     set of values which it gets from the smallest component
C     of X, the non-zero components of Y and the length of Y.
C
      IF      (       ( A .LE. B )
     .          .AND. ( A .LE. C ) ) THEN

         F  = DSQRT ( B + C )
         S1 = 1
         S2 = 2
         S3 = 3

      ELSE IF (       ( B .LE. A )
     .          .AND. ( B .LE. C ) ) THEN

         F  = DSQRT ( A + C )
         S1 = 2
         S2 = 3
         S3 = 1

      ELSE

         F  = DSQRT ( A + B )
         S1 = 3
         S2 = 1
         S3 = 2

      END IF

C
C     Note: by construction, F is the magnitude of the large components
C     of X.  With this in mind, one can verify by inspection that X, Y
C     and Z yield an orthonormal frame.  The right handedness follows
C     from the assignment of values to S1, S2 and S3 (they are merely
C     cycled from one case to the next).
C
      Y(S1)  =   0.0D0
      Y(S2)  = - X(S3) / F
      Y(S3)  =   X(S2) / F

      Z(S1)  =   F
      Z(S2)  = - X(S1) * Y(S3)
      Z(S3)  =   X(S1) * Y(S2)

      RETURN
      END
