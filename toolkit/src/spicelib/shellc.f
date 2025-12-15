C$Procedure SHELLC ( Shell sort a character array )

      SUBROUTINE SHELLC ( NDIM, ARRAY )

C$ Abstract
C
C     Sort an array of character strings according to the ASCII
C     collating sequence using the Shell Sort algorithm.
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
C     ARRAY
C     SORT
C
C$ Declarations

      IMPLICIT NONE

      INTEGER          NDIM
      CHARACTER*(*)    ARRAY ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NDIM       I   Dimension of the array.
C     ARRAY     I-O  The array.
C
C$ Detailed_Input
C
C     NDIM     is the number of elements in the array to be sorted.
C
C     ARRAY    on input, is the array to be sorted.
C
C$ Detailed_Output
C
C     ARRAY    on output, contains the same elements, sorted
C              according to the ASCII collating sequence.
C              The actual sorting is done in place in ARRAY.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If NDIM < 2, this routine does not modify the array.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The Shell Sort Algorithm is well known.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a list of words, sort it according to the ASCII
C        collating sequence using the Shell Sort algorithm.
C
C
C        Example code begins here.
C
C
C              PROGRAM SHELLC_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM  = 6 )
C
C              INTEGER               WRDSZ
C              PARAMETER           ( WRDSZ = 8 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(WRDSZ)     ARRAY  ( NDIM )
C              INTEGER               I
C
C        C
C        C     Let ARRAY contain the following elements:
C        C
C              ARRAY(1) = 'FEYNMAN'
C              ARRAY(2) = 'NEWTON'
C              ARRAY(3) = 'EINSTEIN'
C              ARRAY(4) = 'GALILEO'
C              ARRAY(5) = 'EUCLID'
C              ARRAY(6) = 'Galileo'
C
C        C
C        C     Print ARRAY before calling SHELLC.
C        C
C              WRITE(*,*) 'Array before calling SHELLC:'
C              WRITE(*,*)
C              DO I = 1, NDIM
C                 WRITE(*,*) '   ', ARRAY(I)
C              END DO
C              WRITE(*,*)
C
C        C
C        C     Call SHELLC and print ARRAY again.
C        C
C              CALL SHELLC ( NDIM, ARRAY )
C
C              WRITE(*,*) 'Array after calling SHELLC:'
C              WRITE(*,*)
C              DO I = 1, NDIM
C                 WRITE(*,*) '   ', ARRAY(I)
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Array before calling SHELLC:
C
C            FEYNMAN
C            NEWTON
C            EINSTEIN
C            GALILEO
C            EUCLID
C            Galileo
C
C         Array after calling SHELLC:
C
C            EINSTEIN
C            EUCLID
C            FEYNMAN
C            GALILEO
C            Galileo
C            NEWTON
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing example. Extended $Exceptions
C        section to explain what happens if NDIM < 2.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     shell sort a character array
C
C-&


C
C     Local variables
C
      INTEGER          GAP
      INTEGER          I
      INTEGER          J
      INTEGER          JG


C
C     This is a straightforward implementation of the Shell Sort
C     algorithm.
C
      GAP = NDIM / 2

      DO WHILE ( GAP .GT. 0 )

         DO I = GAP+1, NDIM

            J = I - GAP
            DO WHILE ( J .GT. 0 )
               JG = J + GAP

               IF ( LLE (ARRAY(J), ARRAY(JG)) ) THEN
                  J = 0
               ELSE
                  CALL SWAPC ( ARRAY(J), ARRAY(JG) )
               END IF

               J = J - GAP
            END DO

         END DO

         GAP = GAP / 2

      END DO

      RETURN
      END
