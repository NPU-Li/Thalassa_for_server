C$Procedure CLEARD ( Clear a double precision array )

      SUBROUTINE CLEARD ( NDIM, ARRAY )

C$ Abstract
C
C     Fill a double precision array with zeros.
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

      INTEGER            NDIM
      DOUBLE PRECISION   ARRAY ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NDIM       I   The number of elements of ARRAY which are to be
C                    set to zero.
C     ARRAY      O   Double precision array to be filled.
C
C$ Detailed_Input
C
C     NDIM     is the number of elements in ARRAY which are to be
C              set to zero.
C
C$ Detailed_Output
C
C     ARRAY    is the double precision array which is to be filled
C              with zeros.
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
C     1) Initialize all members of a double precision array to the
C        same value and clear it afterwards.
C
C
C        Example code begins here.
C
C
C              PROGRAM CLEARD_EX1
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
C              DOUBLE PRECISION      ARRAY ( NDIM )
C
C              INTEGER               I
C
C        C
C        C     Initialize all member of the array ARRAY to 11.5, and
C        C     print out its contents.
C        C
C              CALL FILLD ( 11.5D0, NDIM, ARRAY )
C
C              WRITE(*,'(A)') 'Contents of ARRAY before CLEARD:'
C              WRITE(*,'(4F6.1)') ( ARRAY(I), I=1, NDIM )
C
C        C
C        C     Clear the contents of ARRAY and print it.
C        C
C              CALL CLEARD ( NDIM, ARRAY )
C
C              WRITE(*,*)
C              WRITE(*,'(A)') 'Contents of ARRAY after CLEARD:'
C              WRITE(*,'(4F6.1)') ( ARRAY(I), I=1, NDIM )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Contents of ARRAY before CLEARD:
C          11.5  11.5  11.5  11.5
C
C        Contents of ARRAY after CLEARD:
C           0.0   0.0   0.0   0.0
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
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 29-MAY-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Updated the header to comply with NAIF standard. Added
C        full code example.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&


C$ Index_Entries
C
C     clear a d.p. array
C
C-&


C
C     Local variables
C
      INTEGER I


      DO I = 1, NDIM
        ARRAY(I) = 0.D0
      END DO


      RETURN
      END
