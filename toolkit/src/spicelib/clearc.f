C$Procedure CLEARC ( Clear an array of strings )

      SUBROUTINE CLEARC ( NDIM, ARRAY )

C$ Abstract
C
C     Fill an array of strings with blank strings.
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

      INTEGER             NDIM
      CHARACTER*(*)       ARRAY ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NDIM       I   Number of elements of ARRAY to be set to blank.
C     ARRAY      O   Array of strings to be filled with blank strings.
C
C$ Detailed_Input
C
C     NDIM     is the number of elements in ARRAY which are to be set to
C              blank.
C
C$ Detailed_Output
C
C     ARRAY    is the array of strings with each of its NDIM elements
C              set to a blank string. If NDIM is smaller than the
C              declared dimension of ARRAY, only the first NDIM
C              elements of ARRAY are set to blank; all other elements
C              remain unchanged.
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
C     1) Initialize a one dimensional string array and then clear
C        the first two strings.
C
C
C        Example code begins here.
C
C
C              PROGRAM CLEARC_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               STRSZ
C              PARAMETER           ( STRSZ = 21 )
C
C              INTEGER               NDIM
C              PARAMETER           ( NDIM = 4   )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(STRSZ)     ARRAY(NDIM)
C              INTEGER               I
C
C        C
C        C     Initialize ARRAY.
C        C
C              ARRAY(1) = 'Element #1'
C              ARRAY(2) = 'Element #2'
C              ARRAY(3) = 'Element #3'
C              ARRAY(4) = 'Element #4'
C
C              WRITE(*,'(A)') 'Contents of ARRAY before CLEARC:'
C              WRITE(*,*)
C              DO I = 1, NDIM
C                 WRITE(*,'(A,I1,2A)') 'Position #', I , ': ', ARRAY(I)
C              END DO
C
C        C
C        C     Clear the first 2 elements.
C        C
C              CALL CLEARC ( 2, ARRAY )
C
C              WRITE(*,*)
C              WRITE(*,'(A)') 'Contents of ARRAY after CLEARC:'
C              WRITE(*,*)
C              DO I = 1, NDIM
C                 WRITE(*,'(A,I1,2A)') 'Position #', I , ': ', ARRAY(I)
C              END DO
C
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Contents of ARRAY before CLEARC:
C
C        Position #1: Element #1
C        Position #2: Element #2
C        Position #3: Element #3
C        Position #4: Element #4
C
C        Contents of ARRAY after CLEARC:
C
C        Position #1:
C        Position #2:
C        Position #3: Element #3
C        Position #4: Element #4
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
C-    SPICELIB Version 1.1.0, 19-MAY-2021 (JDR) (NJB)
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
C     clear a character array
C
C-&


C
C     Local variables
C
      INTEGER I


      DO I = 1, NDIM
         ARRAY(I) = ' '
      END DO


      RETURN
      END
