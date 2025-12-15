C$Procedure BSRCHD ( Binary search for double precision value )

      INTEGER FUNCTION BSRCHD ( VALUE, NDIM, ARRAY )

C$ Abstract
C
C     Do a binary search for a given value within a double precision
C     array, assumed to be in nondecreasing order. Return the index of
C     the matching array entry, or zero if the key value is not found.
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
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   VALUE
      INTEGER            NDIM
      DOUBLE PRECISION   ARRAY ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VALUE      I   Value to find in ARRAY.
C     NDIM       I   Dimension of ARRAY.
C     ARRAY      I   Array to be searched.
C
C     The function returns the index of VALUE in ARRAY, or zero if not
C     found.
C
C$ Detailed_Input
C
C     VALUE    is the double precision value to be found in the input
C              array.
C
C     NDIM     is the number of elements in the input array.
C
C     ARRAY    is the double precision array to be searched. The
C              elements in ARRAY are assumed to sorted in increasing
C              order.
C
C$ Detailed_Output
C
C     The function returns the index of the specified value in the input
C     array. Indices range from 1 to NDIM.
C
C     If the input array does not contain the specified value, the
C     function returns zero.
C
C     If the input array contains more than one occurrence of the
C     specified value, the returned index may point to any of the
C     occurrences.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If NDIM < 1, the value of the function is zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     A binary search is performed on the input array. If an element of
C     the array is found to match the input value, the index of that
C     element is returned. If no matching element is found, zero is
C     returned.
C
C$ Examples
C
C     Let ARRAY contain the following elements:
C
C             -11.D0
C               0.D0
C              22.491D0
C             750.0D0
C
C     Then
C
C           BSRCHD ( -11.D0,    4, ARRAY )    = 1
C           BSRCHD (  22.491D0, 4, ARRAY )    = 3
C           BSRCHD ( 751.D0,    4, ARRAY )    = 0
C
C$ Restrictions
C
C     1)  ARRAY is assumed to be sorted in increasing order. If this
C         condition is not met, the results of BSRCHD are unpredictable.
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
C-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section. Improved $Detailed_Output
C        section.
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
C     binary search for d.p. value
C
C-&


C
C     Local variables
C
      INTEGER          LEFT
      INTEGER          RIGHT
      INTEGER          I



C
C     Set the initial bounds for the search area.
C
      LEFT  = 1
      RIGHT = NDIM

      DO WHILE ( LEFT .LE. RIGHT )

C
C        Check the middle element.
C
         I = (LEFT+RIGHT)/2

C
C        If the middle element matches, return its location.
C
         IF ( VALUE .EQ. ARRAY(I) ) THEN
            BSRCHD = I
            RETURN

C
C        Otherwise narrow the search area.
C
         ELSE IF ( VALUE .LT. ARRAY(I) ) THEN
            RIGHT = I-1
         ELSE
            LEFT  = I+1
         END IF

      END DO

C
C     If the search area is empty, return zero.
C
      BSRCHD = 0

      RETURN
      END
