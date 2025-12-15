C$Procedure INVSTM ( Inverse of state transformation matrix )

      SUBROUTINE INVSTM ( MAT, INVMAT )

C$ Abstract
C
C     Return the inverse of a state transformation matrix.
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
C     ROTATION
C
C$ Keywords
C
C     MATH
C     MATRIX
C     TRANSFORMATION
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      MAT    ( 6, 6 )
      DOUBLE PRECISION      INVMAT ( 6, 6 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MAT        I   A state transformation matrix.
C     INVMAT     O   The inverse of MAT.
C
C$ Detailed_Input
C
C     MAT      is a state transformation matrix for converting states
C              relative to one frame to states relative to another.
C              The state transformation of a state vector, S, is
C              performed by the matrix-vector product.
C
C                 MAT * S.
C
C              For MAT to be a "true" state transformation matrix
C              it must have the form
C
C                  .-            -.
C                  |       :      |
C                  |   R   :   0  |
C                  |.......:......|
C                  |       :      |
C                  |  W*R  :   R  |
C                  |       :      |
C                  `-            -'
C
C              where R is a 3x3 rotation matrix, 0 is the 3x3 zero
C              matrix and W is a 3x3 skew-symmetric matrix.
C
C              NOTE: no checks are performed on MAT to ensure that it
C                    does indeed have the form described above.
C
C$ Detailed_Output
C
C     INVMAT   is the inverse of MAT under the operation of matrix
C              multiplication.
C
C              If MAT has the form described above, then INVMAT has
C              the form shown below.
C
C                 .-             -.
C                 |     t  :      |
C                 |    R   :   0  |
C                 |........:......|
C                 |      t :    t |
C                 | (W*R)  :   R  |
C                 |        :      |
C                 `-             -'
C
C              (The superscript "t" denotes the matrix transpose
C              operation.)
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  No checks are performed to ensure that the input matrix is
C         indeed a state transformation matrix.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given a matrix for transforming states relative frame 1 to
C     states relative frame 2, the routine produces the inverse
C     matrix. That is, it returns the matrix for transforming states
C     relative to frame 2 to states relative to frame 1.
C
C     This special routine exists because unlike the inverse of a
C     rotation matrix, the inverse of a state transformation matrix,
C     is NOT simply the transpose of the matrix.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose you have a geometric state of a spacecraft in Earth
C        body-fixed reference frame and wish to express this state
C        relative to an Earth centered J2000 frame. The following
C        example code illustrates how to carry out this computation.
C
C        Use the PCK kernel below to load the required high precision
C        orientation of the ITRF93 Earth body-fixed reference frame.
C        Note that the body ID code used in this file for the Earth is
C        3000.
C
C           earth_720101_070426.bpc
C
C
C        Example code begins here.
C
C
C              PROGRAM INVSTM_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      INVMAT ( 6, 6 )
C              DOUBLE PRECISION      ISTAT1 ( 6    )
C              DOUBLE PRECISION      ISTAT2 ( 6    )
C              DOUBLE PRECISION      MAT    ( 6, 6 )
C              DOUBLE PRECISION      STATE  ( 6    )
C              DOUBLE PRECISION      XMAT   ( 6, 6 )
C
C              INTEGER               EARTH
C
C        C
C        C     Define the state of the spacecraft, in km and
C        C     km/s, and the ET epoch, in seconds past J2000.
C        C
C              DATA                  ET    /  0.0D0 /
C              DATA                  STATE /  175625246.29100420D0,
C             .                               164189388.12540060D0,
C             .                               -62935198.26067264D0,
C             .                                   11946.73372264D0,
C             .                                  -12771.29732556D0,
C             .                                      13.84902914D0 /
C
C        C
C        C     Load the required high precision Earth PCK.
C        C
C              CALL FURNSH ( 'earth_720101_070426.bpc' )
C
C        C
C        C     First get the state transformation from J2000 frame
C        C     to Earth body-fixed frame at the time of interest ET.
C        C     The body ID code used in high precision PCK files for
C        C     the Earth is 3000; this number indicates that the
C        C     terrestrial frame used is ITRF93.
C        C
C              EARTH = 3000
C              CALL TISBOD ( 'J2000', EARTH, ET, MAT )
C
C        C
C        C     Get the inverse of MAT.
C        C
C              CALL INVSTM ( MAT,  INVMAT          )
C
C        C
C        C     Transform from bodyfixed state to inertial state.
C        C
C              CALL MXVG ( INVMAT, STATE, 6, 6, ISTAT1 )
C
C        C
C        C     Print the resulting state.
C        C
C              WRITE(*,'(A)') 'Input state in Earth centered J2000 '
C             .            // 'frame, using INVSTM:'
C              WRITE(*,'(A,3F16.3)') '   Position:', ISTAT1(1:3)
C              WRITE(*,'(A,3F16.3)') '   Velocity:', ISTAT1(4:6)
C
C        C
C        C     Compute the same state using SXFORM.
C        C
C              CALL SXFORM ( 'ITRF93', 'J2000', ET, XMAT )
C              CALL MXVG   ( XMAT, STATE, 6, 6, ISTAT2 )
C
C              WRITE(*,*)
C              WRITE(*,'(A)') 'Input state in Earth centered J2000 '
C             .            // 'frame, using SXFORM:'
C              WRITE(*,'(A,3F16.3)') '   Position:', ISTAT2(1:3)
C              WRITE(*,'(A,3F16.3)') '   Velocity:', ISTAT2(4:6)
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Input state in Earth centered J2000 frame, using INVSTM:
C           Position:   192681395.921  -143792821.383   -62934296.473
C           Velocity:          30.312          32.007          13.876
C
C        Input state in Earth centered J2000 frame, using SXFORM:
C           Position:   192681395.921  -143792821.383   -62934296.473
C           Velocity:          30.312          32.007          13.876
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
C-    SPICELIB Version 1.1.0, 25-NOV-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Removed unnecessary Standard SPICE error handling calls to
C        register/unregister this routine in the error handling
C        subsystem; this routine is Error free.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example based on the existing fragment.
C
C-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 29-OCT-1990 (WLT)
C
C-&


C$ Index_Entries
C
C     inverse of state transformation matrix
C
C-&


C
C     Local parameters
C
      INTEGER               NROWS
      PARAMETER           ( NROWS = 6 )

      INTEGER               NCOLS
      PARAMETER           ( NCOLS = 6 )

      INTEGER               BLOCK
      PARAMETER           ( BLOCK = 3 )


C
C     Not much to this. Just call the more general routine XPOSBL.
C
      CALL XPOSBL ( MAT, NROWS, NCOLS, BLOCK, INVMAT )


      RETURN
      END
