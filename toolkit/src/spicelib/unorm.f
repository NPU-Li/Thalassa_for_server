C$Procedure UNORM ( Unit vector and norm, 3 dimensional )

      SUBROUTINE UNORM ( V1, VOUT, VMAG )

C$ Abstract
C
C     Normalize a double precision 3-vector and return its magnitude.
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

      DOUBLE PRECISION  V1   ( 3 )
      DOUBLE PRECISION  VOUT ( 3 )
      DOUBLE PRECISION  VMAG

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   Vector to be normalized.
C     VOUT       O   Unit vector V1 / |V1|.
C     VMAG       O   Magnitude of V1, i.e. |V1|.
C
C$ Detailed_Input
C
C     V1       is an arbitrary 3-vector, including the
C              zero vector.
C
C$ Detailed_Output
C
C     VOUT     is the unit vector in the direction of V1. If V1 is
C              the zero vector, then VOUT will also be the zero
C              vector.
C
C     VMAG     is the magnitude of V1.
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
C     UNORM references a function called VNORM (which itself is
C     numerically stable) to calculate the norm of the input vector V1.
C     If the norm is equal to zero, then each component of the output
C     vector VOUT is set to zero. Otherwise, VOUT is calculated by
C     dividing V1 by the norm.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Define a set of vectors and compute their corresponding unit
C        vector and magnitudes.
C
C
C        Example code begins here.
C
C
C              PROGRAM UNORM_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 2 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      VMAG
C              DOUBLE PRECISION      SETA ( 3, SETSIZ )
C              DOUBLE PRECISION      VOUT ( 3 )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define the vector set.
C        C
C              DATA                  SETA / 5.D0,  12.D0,  0.D0,
C             .                             1.D-7,  2.D-7, 2.D-7 /
C
C        C
C        C     Calculate the unit vectors and magnitudes.
C        C
C              DO I=1, SETSIZ
C
C                 CALL UNORM ( SETA(1,I), VOUT, VMAG )
C
C                 WRITE(*,'(A,3F13.8)') 'Vector     : ',
C             .                                   ( SETA(J,I), J=1, 3 )
C                 WRITE(*,'(A,3F13.8)') 'Unit vector: ', VOUT
C                 WRITE(*,'(A,F13.8)')  'Magnitude:   ', VMAG
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
C        Vector     :    5.00000000  12.00000000   0.00000000
C        Unit vector:    0.38461538   0.92307692   0.00000000
C        Magnitude:     13.00000000
C
C        Vector     :    0.00000010   0.00000020   0.00000020
C        Unit vector:    0.33333333   0.66666667   0.66666667
C        Magnitude:      0.00000030
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
C-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section. Added complete example code.
C
C-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
C
C-&


C$ Index_Entries
C
C     3-dimensional unit vector and norm
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION   VNORM

C
C  Obtain the magnitude of V1
C
      VMAG = VNORM(V1)

C
C   If VMAG is nonzero, then normalize.  Note that this process is
C   numerically stable: overflow could only happen if VMAG were small,
C   but this could only happen if each component of V1 were small.
C   In fact, the magnitude of any vector is never less than the
C   magnitude of any component.
C
      IF ( VMAG .GT. 0.D0 ) THEN
         VOUT(1) = V1(1) / VMAG
         VOUT(2) = V1(2) / VMAG
         VOUT(3) = V1(3) / VMAG
      ELSE
         VOUT(1) = 0.D0
         VOUT(2) = 0.D0
         VOUT(3) = 0.D0
      END IF

      RETURN
      END
