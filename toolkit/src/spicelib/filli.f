C$Procedure FILLI ( Fill an integer array )

      SUBROUTINE FILLI ( VALUE, NDIM, ARRAY )

C$ Abstract
C
C     Fill an integer array with a specified value.
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
C     ASSIGNMENT
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               VALUE
      INTEGER               NDIM
      INTEGER               ARRAY ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VALUE      I   Integer value to be placed in all the elements of
C                    ARRAY.
C     NDIM       I   The number of elements in ARRAY.
C     ARRAY      O   Integer array which is to be filled.
C
C$ Detailed_Input
C
C     VALUE    is the value to be assigned to the array elements
C              1 through NDIM.
C
C     NDIM     is the number of elements in the array.
C
C$ Detailed_Output
C
C     ARRAY    is a integer array whose elements are to be set
C              to VALUE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If NDIM < 1, the array is not modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Initialize all members of an integer array to the same value.
C
C
C        Example code begins here.
C
C
C              PROGRAM FILLI_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM = 4 )
C
C        C
C        C     Local variables.
C        C
C              INTEGER               ARRAY ( NDIM )
C              INTEGER               I
C
C        C
C        C     Initialize all members of the array ARRAY to 11, and
C        C     print out its contents.
C        C
C              CALL FILLI ( 11, NDIM, ARRAY )
C
C              WRITE(*,'(A)') 'Contents of ARRAY:'
C              DO I=1, NDIM
C
C                 WRITE(*,'(A,I2,A,I3)') '   Index:', I,
C             .                          '; value:', ARRAY(I)
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
C        Contents of ARRAY:
C           Index: 1; value: 11
C           Index: 2; value: 11
C           Index: 3; value: 11
C           Index: 4; value: 11
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
C     W.M. Owen          (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 19-FEB-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Updated the header to comply with NAIF standard. Added
C        full code example.
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
C     fill an integer array
C
C-&


C
C     Local variables
C
      INTEGER I


      DO I = 1, NDIM
        ARRAY(I) = VALUE
      END DO


      RETURN
      END
