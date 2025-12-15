C$Procedure VCRSS ( Vector cross product, 3 dimensions )

      SUBROUTINE VCRSS ( V1, V2, VOUT )

C$ Abstract
C
C     Compute the cross product of two 3-dimensional vectors.
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
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION    V1   ( 3 )
      DOUBLE PRECISION    V2   ( 3 )
      DOUBLE PRECISION    VOUT ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   Left hand vector for cross product.
C     V2         I   Right hand vector for cross product.
C     VOUT       O   Cross product V1 x V2.
C
C$ Detailed_Input
C
C     V1,
C     V2       are two 3-dimensional vectors. Typically, these might
C              represent the (possibly unit) vector to a planet, Sun,
C              or a star which defines the orientation of axes of some
C              reference frame.
C
C$ Detailed_Output
C
C     VOUT     is the cross product of V1 and V2.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     VCRSS calculates the three dimensional cross product of two
C     vectors according to the definition.
C
C     If V1 and V2 are large in magnitude (taken together, their
C     magnitude surpasses the limit allowed by the computer) then it
C     may be possible to generate a floating point overflow from an
C     intermediate computation even though the actual cross product may
C     be well within the range of double precision numbers. VCRSS does
C     NOT check the magnitude of V1 or V2 to insure that overflow will
C     not occur.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Define two sets of vectors and compute the cross product of
C        each vector in first set and the corresponding vector in
C        the second set.
C
C
C        Example code begins here.
C
C
C              PROGRAM VCRSS_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM   = 3 )
C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 2 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      SETA ( NDIM, SETSIZ )
C              DOUBLE PRECISION      SETB ( NDIM, SETSIZ )
C              DOUBLE PRECISION      VOUT ( NDIM )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define the two vector sets.
C        C
C              DATA                  SETA / 0.D0,  1.D0,  0.D0,
C             .                             5.D0,  5.D0,  5.D0  /
C
C              DATA                  SETB / 1.D0,  0.D0,  0.D0,
C             .                            -1.D0, -1.D0, -1.D0  /
C
C        C
C        C     Calculate the cross product of each pair of vectors
C        C
C              DO I=1, SETSIZ
C
C                 CALL VCRSS ( SETA(1,I), SETB(1,I), VOUT )
C
C                 WRITE(*,'(A,3F5.1)') 'Vector A     : ',
C             .                        ( SETA(J,I), J=1,3 )
C                 WRITE(*,'(A,3F5.1)') 'Vector B     : ',
C             .                        ( SETB(J,I), J=1,3 )
C                 WRITE(*,'(A,3F5.1)') 'Cross product: ', VOUT
C                 WRITE(*,*) ' '
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
C        Vector A     :   0.0  1.0  0.0
C        Vector B     :   1.0  0.0  0.0
C        Cross product:   0.0  0.0 -1.0
C
C        Vector A     :   5.0  5.0  5.0
C        Vector B     :  -1.0 -1.0 -1.0
C        Cross product:   0.0  0.0  0.0
C
C
C$ Restrictions
C
C     1)  No checking of V1 or V2 is done to prevent floating point
C         overflow. The user is required to determine that the
C         magnitude of each component of the vectors is within an
C         appropriate range so as not to cause floating point overflow.
C         In almost every case there will be no problem and no checking
C         actually needs to be done.
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
C-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing example.
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
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&


C$ Index_Entries
C
C     vector cross product
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION VTEMP(3)

C
C  Calculate the cross product of V1 and V2, store in VTEMP
C
      VTEMP(1) = V1(2)*V2(3) - V1(3)*V2(2)
      VTEMP(2) = V1(3)*V2(1) - V1(1)*V2(3)
      VTEMP(3) = V1(1)*V2(2) - V1(2)*V2(1)
C
C  Now move the result into VOUT
C
      VOUT(1) = VTEMP(1)
      VOUT(2) = VTEMP(2)
      VOUT(3) = VTEMP(3)
C
      RETURN
      END
