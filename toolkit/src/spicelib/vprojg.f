C$Procedure VPROJG ( Vector projection, general dimension )

      SUBROUTINE VPROJG ( A, B, NDIM, P )

C$ Abstract
C
C     Compute the projection of one vector onto another vector. All
C     vectors are of arbitrary dimension.
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

      INTEGER            NDIM
      DOUBLE PRECISION   A ( NDIM )
      DOUBLE PRECISION   B ( NDIM )
      DOUBLE PRECISION   P ( NDIM )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     A          I   The vector to be projected.
C     B          I   The vector onto which A is to be projected.
C     NDIM       I   Dimension of A, B, and P.
C     P          O   The projection of A onto B.
C
C$ Detailed_Input
C
C     A        is a double precision vector of arbitrary dimension.
C              This vector is to be projected onto the vector B.
C
C     B        is a double precision vector of arbitrary dimension.
C              This vector is the vector which receives the
C              projection.
C
C     NDIM     is the dimension of A, B and P.
C
C$ Detailed_Output
C
C     P        is a double precision vector of arbitrary dimension
C              containing the projection of A onto B. (P is
C              necessarily parallel to B.)
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
C     The projection of a vector A onto a vector B is, by definition,
C     that component of A which is parallel to B. To find this
C     component it is enough to find the scalar ratio of the length of
C     B to the projection of A onto B, and then use this number to
C     scale the length of B. This ratio is given by
C
C         RATIO = < A, B > / < B, B >
C
C     where <,> denotes the general vector dot product. This routine
C     does not attempt to divide by zero in the event that B is the
C     zero vector.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Define two sets of vectors and compute the projection of
C        each vector of the first set on the corresponding vector of
C        the second set.
C
C        Example code begins here.
C
C
C              PROGRAM VPROJG_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM   = 4 )
C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 4 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      SETA ( NDIM, SETSIZ )
C              DOUBLE PRECISION      SETB ( NDIM, SETSIZ )
C              DOUBLE PRECISION      PVEC ( NDIM )
C
C              INTEGER               I
C              INTEGER               M
C
C        C
C        C     Define the two vector sets.
C        C
C              DATA                  SETA / 6.D0,  6.D0,  6.D0,  0.D0,
C             .                             6.D0,  6.D0,  6.D0,  0.D0,
C             .                             6.D0,  6.D0,  0.D0,  0.D0,
C             .                             6.D0,  0.D0,  0.D0,  0.D0 /
C
C              DATA                  SETB / 2.D0,  0.D0,  0.D0,  0.D0,
C             .                            -3.D0,  0.D0,  0.D0,  0.D0,
C             .                             0.D0,  7.D0,  0.D0,  0.D0,
C             .                             0.D0,  0.D0,  9.D0,  0.D0 /
C
C        C
C        C     Calculate the projection
C        C
C              DO I=1, SETSIZ
C
C                 CALL VPROJG ( SETA(1,I), SETB(1,I), NDIM, PVEC )
C                 WRITE(*,'(A,4F5.1)') 'Vector A  : ',
C             .                     ( SETA(M,I), M = 1, NDIM )
C                 WRITE(*,'(A,4F5.1)') 'Vector B  : ',
C             .                     ( SETB(M,I), M = 1, NDIM )
C                 WRITE(*,'(A,4F5.1)') 'Projection: ', PVEC
C                 WRITE(*,*)
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
C        Vector A  :   6.0  6.0  6.0  0.0
C        Vector B  :   2.0  0.0  0.0  0.0
C        Projection:   6.0  0.0  0.0  0.0
C
C        Vector A  :   6.0  6.0  6.0  0.0
C        Vector B  :  -3.0  0.0  0.0  0.0
C        Projection:   6.0 -0.0 -0.0 -0.0
C
C        Vector A  :   6.0  6.0  0.0  0.0
C        Vector B  :   0.0  7.0  0.0  0.0
C        Projection:   0.0  6.0  0.0  0.0
C
C        Vector A  :   6.0  0.0  0.0  0.0
C        Vector B  :   0.0  0.0  9.0  0.0
C        Projection:   0.0  0.0  0.0  0.0
C
C
C$ Restrictions
C
C     1)  No error detection or recovery schemes are incorporated into
C         this routine except to insure that no attempt is made to
C         divide by zero. Thus, the user is required to make sure that
C         the vectors A and B are such that no floating point overflow
C         will occur when the dot products are calculated.
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
C     H.A. Neilan        (JPL)
C     W.L. Taber         (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Corrected math
C        expression in $Particulars section. Removed unnecessary
C        $Revisions section.
C
C        Added complete code example to $Examples section based on
C        existing example.
C
C-    SPICELIB Version 1.0.3, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW)
C
C        Corrected ENDIF to END IF.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) (NJB)
C
C-&


C$ Index_Entries
C
C     n-dimensional vector projection
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION VDOTG

C
C     Local variables
C
      DOUBLE PRECISION ADOTB
      DOUBLE PRECISION BDOTB
      DOUBLE PRECISION SCALE


      ADOTB  = VDOTG (A,B,NDIM)
      BDOTB  = VDOTG (B,B,NDIM)

      IF ( BDOTB .EQ. 0.D0 ) THEN
         SCALE = 0.D0
      ELSE
         SCALE = ADOTB/BDOTB
      END IF

      CALL VSCLG (SCALE, B, NDIM, P)

      RETURN
      END
