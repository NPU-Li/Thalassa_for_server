C$Procedure BSCHOI ( Binary search with order vector, integer )

      INTEGER FUNCTION BSCHOI ( VALUE, NDIM, ARRAY, ORDER )

C$ Abstract
C
C     Do a binary search for a given value within an integer array,
C     accompanied by an order vector. Return the index of the
C     matching array entry, or zero if the key value is not found.
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

      INTEGER          VALUE
      INTEGER          NDIM
      INTEGER          ARRAY ( * )
      INTEGER          ORDER ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VALUE      I   Value to find in ARRAY.
C     NDIM       I   Dimension of ARRAY.
C     ARRAY      I   Array to be searched.
C     ORDER      I   Order vector.
C
C     The function returns the index of the first matching array element
C     or zero if the value is not found.
C
C$ Detailed_Input
C
C     VALUE    is the value to be found in the input array.
C
C     NDIM     is the number of elements in the input array.
C
C     ARRAY    is the array to be searched.
C
C     ORDER    is an order vector which can be used to access the
C              elements of ARRAY in order. The contents of order are a
C              permutation of the sequence of integers ranging from 1 to
C              NDIM.
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
C     A binary search is performed on the input array, whose order is
C     given by an associated order vector. If an element of the array is
C     found to match the input value, the index of that element is
C     returned. If no matching element is found, zero is returned.
C
C$ Examples
C
C     Let ARRAY and ORDER contain the following elements:
C
C           ARRAY         ORDER
C           -----------   -----
C             100             2
C               1             3
C              10             1
C           10000             5
C            1000             4
C
C     Then
C
C           BSCHOI (  1000, 5, ARRAY, ORDER )  = 5
C           BSCHOI (     1, 5, ARRAY, ORDER )  = 2
C           BSCHOI ( 10000, 5, ARRAY, ORDER )  = 4
C           BSCHOI (    -1, 5, ARRAY, ORDER )  = 0
C           BSCHOI (    17, 5, ARRAY, ORDER )  = 0
C
C     That is,
C
C           ARRAY(5) =  1000
C           ARRAY(2) =     1
C           ARRAY(4) = 10000
C
C$ Restrictions
C
C     1)  ORDER is assumed to give the order of the elements of ARRAY
C         in increasing order. If this condition is not met, the results
C         of BSCHOI are unpredictable.
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
C-    SPICELIB Version 1.0.1, 09-APR-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 1.0.0, 18-SEP-1995 (IMU) (WLT)
C
C-&


C$ Index_Entries
C
C     binary search for an integer using an order vector
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
         IF ( VALUE .EQ. ARRAY(ORDER(I)) ) THEN
            BSCHOI = ORDER(I)
            RETURN

C
C        Otherwise narrow the search area.
C
         ELSE IF ( VALUE .LT. ARRAY(ORDER(I)) ) THEN
            RIGHT = I-1
         ELSE
            LEFT  = I+1
         END IF

      END DO

C
C     If the search area is empty, return zero.
C
      BSCHOI = 0

      RETURN
      END
