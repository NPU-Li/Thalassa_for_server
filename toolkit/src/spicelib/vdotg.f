C$Procedure VDOTG ( Vector dot product, general dimension )

      DOUBLE PRECISION FUNCTION VDOTG ( V1, V2, NDIM )

C$ Abstract
C
C     Compute the dot product of two vectors of arbitrary dimension.
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

      DOUBLE PRECISION   V1  ( * )
      DOUBLE PRECISION   V2  ( * )
      INTEGER            NDIM

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   First vector in the dot product.
C     V2         I   Second vector in the dot product.
C     NDIM       I   Dimension of V1 and V2.
C
C     The function returns the value of the dot product of V1 and V2.
C
C$ Detailed_Input
C
C     V1,
C     V2       are two arbitrary double precision n-dimensional
C              vectors.
C
C     NDIM     is the dimension of V1 and V2.
C
C$ Detailed_Output
C
C     The function returns the value of the dot product (inner product)
C     of V1 and V2:
C
C        < V1, V2 >
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
C     VDOTG calculates the dot product of V1 and V2 by a simple
C     application of the definition:
C
C                   NDIM
C                 .------
C                  \
C        VDOTG  =   )  V1(I) * V2(I)
C                  /
C                 '------
C                    I=1
C
C     No error checking is performed to prevent or recover from numeric
C     overflow.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose that you have a set of double precision n-dimensional
C        vectors. Check if they are orthogonal to the Z-axis in
C        n-dimensional space.
C
C
C        Example code begins here.
C
C
C              PROGRAM VDOTG_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              DOUBLE PRECISION      VDOTG
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM   = 4 )
C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 5 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      V1   ( NDIM, SETSIZ )
C              DOUBLE PRECISION      Z    ( NDIM         )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define the vector set.
C        C
C              DATA                  V1  / 1.D0,  0.D0,  0.D0, 0.D0,
C             .                            0.D0,  1.D0,  0.D0, 3.D0,
C             .                            0.D0,  0.D0, -6.D0, 0.D0,
C             .                           10.D0,  0.D0, -1.D0, 0.D0,
C             .                            0.D0,  0.D0,  0.D0, 1.D0  /
C
C              DATA                  Z   / 0.D0,  0.D0,  1.D0, 0.D0  /
C
C        C
C        C     Check the orthogonality with respect to Z of each
C        C     vector in V1.
C        C
C              DO I = 1, SETSIZ
C
C                 WRITE(*,*)
C                 WRITE(*,'(A,4F6.1)') 'Input vector (V1): ',
C             .                         ( V1(J,I), J=1,NDIM )
C
C                 IF ( VDOTG( V1(1,I), Z, NDIM ) .EQ. 0.D0 ) THEN
C
C                    WRITE(*,'(A)') 'V1 and Z are orthogonal.'
C
C                 ELSE
C
C                    WRITE(*,'(A)') 'V1 and Z are NOT orthogonal.'
C
C                 END IF
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
C        Input vector (V1):    1.0   0.0   0.0   0.0
C        V1 and Z are orthogonal.
C
C        Input vector (V1):    0.0   1.0   0.0   3.0
C        V1 and Z are orthogonal.
C
C        Input vector (V1):    0.0   0.0  -6.0   0.0
C        V1 and Z are NOT orthogonal.
C
C        Input vector (V1):   10.0   0.0  -1.0   0.0
C        V1 and Z are NOT orthogonal.
C
C        Input vector (V1):    0.0   0.0   0.0   1.0
C        V1 and Z are orthogonal.
C
C
C$ Restrictions
C
C     1)  The user is responsible for determining that the vectors V1
C         and V2 are not so large as to cause numeric overflow. In
C         most cases this will not present a problem.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     W.M. Owen          (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 28-MAY-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example. Improved $Particulars section.
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
C     dot product of n-dimensional vectors
C
C-&


      INTEGER I
C
      VDOTG = 0.D0
      DO I = 1, NDIM
         VDOTG = VDOTG + V1(I)*V2(I)
      END DO

      RETURN
      END
