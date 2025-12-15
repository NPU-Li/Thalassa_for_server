C$Procedure INVORT ( Invert nearly orthogonal matrices )

      SUBROUTINE INVORT ( M, MIT )

C$ Abstract
C
C     Construct the inverse of a 3x3 matrix with orthogonal columns and
C     non-zero column norms using a numerically stable algorithm. The
C     rows of the output matrix are the columns of the input matrix
C     divided by the length squared of the corresponding columns.
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
C     MATRIX
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      M   ( 3, 3 )
      DOUBLE PRECISION      MIT ( 3, 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M          I   A 3x3 matrix.
C     MIT        O   M after transposition and scaling of rows.
C
C$ Detailed_Input
C
C     M        is a 3x3 matrix.
C
C$ Detailed_Output
C
C     MIT      is the matrix obtained by transposing M and dividing
C              the rows by squares of their norms.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If any of the columns of M have zero length, the error
C         SPICE(ZEROLENGTHCOLUMN) is signaled.
C
C     2)  If any column is too short to allow computation of the
C         reciprocal of its length without causing a floating
C         point overflow, the error SPICE(COLUMNTOOSMALL) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Suppose that M is the matrix
C
C            .-                      -.
C            |   A*u    B*v     C*w   |
C            |      1      1       1  |
C            |                        |
C            |   A*u    B*v     C*w   |
C            |      2      2       2  |
C            |                        |
C            |   A*u    B*v     C*w   |
C            |      3      3       3  |
C            `-                      -'
C
C     where the vectors (u , u , u ),  (v , v , v ),  and (w , w , w )
C                         1   2   3      1   2   3          1   2   3
C     are unit vectors. This routine produces the matrix:
C
C
C            .-                      -.
C            |   a*u    a*u     a*u   |
C            |      1      2       3  |
C            |                        |
C            |   b*v    b*v     b*v   |
C            |      1      2       3  |
C            |                        |
C            |   c*w    c*w     c*w   |
C            |      1      2       3  |
C            `-                      -'
C
C     where a = 1/A, b = 1/B, and c = 1/C.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a double precision 3x3 matrix with mutually orthogonal
C        rows of arbitrary length, compute its inverse. Check that the
C        original matrix times the computed inverse produces the
C        identity matrix.
C
C        Example code begins here.
C
C
C              PROGRAM INVORT_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      IMAT ( 3, 3 )
C              DOUBLE PRECISION      M    ( 3, 3 )
C              DOUBLE PRECISION      MOUT ( 3, 3 )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define a matrix to invert.
C        C
C              DATA                  M  /  0.D0,  0.5D0, 0.D0,
C             .                           -1.D0,  0.D0,  0.D0,
C             .                            0.D0,  0.D0,  1.D0 /
C
C              WRITE(*,*) 'Original Matrix:'
C              DO I=1, 3
C
C                 WRITE(*,'(3F16.7)') ( M(I,J), J=1,3 )
C
C              END DO
C        C
C        C     Invert the matrix, then output.
C        C
C              CALL INVORT ( M, MOUT )
C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Inverse Matrix:'
C              DO I=1, 3
C
C                 WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 )
C
C              END DO
C
C        C
C        C     Check the M times MOUT produces the identity matrix.
C        C
C              CALL MXM ( M, MOUT, IMAT )
C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Original times inverse:'
C              DO I=1, 3
C
C                 WRITE(*,'(3F16.7)') ( IMAT(I,J), J=1,3 )
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
C         Original Matrix:
C               0.0000000      -1.0000000       0.0000000
C               0.5000000       0.0000000       0.0000000
C               0.0000000       0.0000000       1.0000000
C
C         Inverse Matrix:
C               0.0000000       2.0000000       0.0000000
C              -1.0000000       0.0000000       0.0000000
C               0.0000000       0.0000000       1.0000000
C
C         Original times inverse:
C               1.0000000       0.0000000       0.0000000
C               0.0000000       1.0000000       0.0000000
C               0.0000000       0.0000000       1.0000000
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
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 26-OCT-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Fixed I/O type
C        of argument MIT in $Brief_I/O table. Extended $Abstract
C        section.
C
C        Added complete code example to $Examples section.
C
C-    SPICELIB Version 1.1.1, 14-NOV-2013 (EDW)
C
C        Edit to $Abstract. Eliminated unneeded $Revisions section.
C
C-    SPICELIB Version 1.1.0, 02-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL call.
C
C-    SPICELIB Version 1.0.0, 02-JAN-2002 (WLT)
C
C-&


C$ Index_Entries
C
C     Transpose a matrix and invert the lengths of the rows
C     Invert a pseudo orthogonal matrix
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX

C
C     Local Variables
C
      DOUBLE PRECISION      BOUND
      DOUBLE PRECISION      LENGTH
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      TEMP   ( 3, 3 )

      INTEGER               I

      LOGICAL               FIRST

C
C     Saved variables
C
      SAVE                  BOUND
      SAVE                  FIRST

C
C     Initial values
C
      DATA                  FIRST   / .TRUE. /


C
C     Use discovery check-in.
C

C
C     The first time through, get a copy of DPMAX.
C
      IF ( FIRST ) THEN
         BOUND =  DPMAX()
         FIRST = .FALSE.
      END IF

C
C     For each column, construct a scaled copy. However, make sure
C     everything is do-able before trying something.
C
      DO I = 1, 3

         CALL UNORM ( M(1,I), TEMP(1,I), LENGTH )

         IF ( LENGTH .EQ. 0.0D0 ) THEN

            CALL CHKIN  ( 'INVORT'                                     )
            CALL SETMSG ( 'Column # of the input matrix has a norm '  //
     .                    'of zero. '                                  )
            CALL ERRINT ( '#',        I                                )
            CALL SIGERR ( 'SPICE(ZEROLENGTHCOLUMN)'                    )
            CALL CHKOUT ( 'INVORT'                                     )
            RETURN

         END IF

C
C        Make sure we can actually rescale the rows.
C
         IF    ( LENGTH .LT. 1.0D0 ) THEN

            IF (  ( LENGTH * BOUND ) .LT. 1.0D0 ) THEN

               CALL CHKIN  ( 'INVORT' )
               CALL SETMSG ( 'The length of column # is #. This '     //
     .                       'number cannot be inverted.  For this '  //
     .                       'reason, the scaled transpose of the '   //
     .                       'input matrix cannot be formed. '        )
               CALL ERRINT ( '#',   I                                 )
               CALL ERRDP  ( '#',   LENGTH                            )
               CALL SIGERR ( 'SPICE(COLUMNTOOSMALL)'                  )
               CALL CHKOUT ( 'INVORT'                                 )
               RETURN

            END IF

         END IF

         SCALE = 1.0D0 / LENGTH

         CALL VSCLIP ( SCALE, TEMP(1,I) )

      END DO

C
C     If we make it this far, we just need to transpose TEMP into MIT.
C
      CALL XPOSE ( TEMP, MIT )
      RETURN
      END
