C$Procedure LSTLED ( Last double precision element less than or equal)

      INTEGER FUNCTION  LSTLED ( X, N, ARRAY )

C$ Abstract
C
C     Find the index of the largest array element less than or equal
C     to a given number X in an array of non-decreasing numbers.
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

      DOUBLE PRECISION   X
      INTEGER            N
      DOUBLE PRECISION   ARRAY ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     X          I   Upper bound value to search against.
C     N          I   Number of elements in ARRAY.
C     ARRAY      I   Array of possible lower bounds.
C
C     The function returns the index of the last element of ARRAY that
C     is less than or equal to X.
C
C$ Detailed_Input
C
C     X        is a double precision value acting as an upper bound: the
C              element of ARRAY that is the greatest element less than
C              or equal to X is to be found.
C
C     N        is the total number of elements in ARRAY.
C
C     ARRAY    is an array of double precision numbers that forms a
C              non-decreasing sequence. The elements of array need not
C              be distinct.
C
C$ Detailed_Output
C
C     The function returns the index of the highest-indexed element in
C     the input array that is less than or equal to X. The routine
C     assumes the array elements are sorted in non-decreasing order.
C
C     Indices range from 1 to N.
C
C     If all elements of ARRAY are greater than X, the routine returns
C     the value 0. If N is less than or equal to zero, the routine
C     returns the value 0.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If N is less than or equal to zero, the function returns 0.
C         This case is not treated as an error.
C
C     2)  If the input array is not sorted in non-decreasing order, the
C         output of this routine is undefined. No error is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine uses a binary search algorithm and so requires
C     at most on the order of
C
C        log (N)
C           2
C
C     steps to compute the value of LSTLED.
C
C     Note: If you need to find the first element of the array that is
C     greater than X, simply add 1 to the result returned by this
C     function and check to see if the result is within the array bounds
C     given by N.
C
C$ Examples
C
C     If ARRAY(I) = -1 + 4*I/3 (real arithmetic implied here)
C
C     N        = 10
C     X        = 7.12
C
C     then
C
C     LSTLED will be I where
C             (4*I/3) - 1       < or = 7.12
C     but
C             (4*(I+1)/3) - 1   >      7.12 .
C
C     In this case our subsequence is:
C            1/3, 5/3, 9/3, 13/3, 17/3, 21/3, 25/3, .... 37/3
C
C     index:  1    2    3    4     5     6     7    ....  10
C
C     Thus LSTLED will be returned as 6
C
C     The following table shows the values of LSTLED that would be
C     returned for various values of X
C
C            X       LSTLED
C          -----     -------
C           0.12        0
C           1.34        1
C           5.13        4
C           8.00        6
C          15.10       10
C
C$ Restrictions
C
C     1)  If the sequence of double precision numbers in the input array
C         ARRAY is not non-decreasing, the program will run to
C         completion but the index found will not mean anything.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary $Revisions section. Improved $Detailed_Input,
C        $Detailed_Output, $Particulars, $Exceptions and $Restrictions
C        sections.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (NJB)
C
C-&


C$ Index_Entries
C
C     last d.p. element less_than_or_equal_to
C
C-&


C
C     Local variables
C
      INTEGER          J

      INTEGER          BEGIN
      INTEGER          END
      INTEGER          MIDDLE
      INTEGER          ITEMS

      ITEMS = N

      BEGIN = 1
      END   = N

      IF      ( N .LE. 0            ) THEN

C
C        There's nobody home---that is there is nothing in the array
C        to compare against.  Zero is the only sensible thing to return.
C
         LSTLED = 0

      ELSE IF ( X .LT. ARRAY(BEGIN) ) THEN

C
C        None of the array elements are less than or equal to X
C
         LSTLED = 0

      ELSE IF ( X .GE. ARRAY(END)   ) THEN

C
C        X is greater than or equal to all elements of the array.  Thus
C        the last element of the array is the last item less than or
C        equal to X.
C
         LSTLED = END

      ELSE

C
C        X lies between some pair of elements of the array
C

         DO WHILE ( ITEMS .GT. 2 )

            J        = ITEMS/2
            MIDDLE   = BEGIN + J

            IF ( ARRAY(MIDDLE) .LE. X ) THEN
               BEGIN = MIDDLE
            ELSE
               END   = MIDDLE
            END IF

            ITEMS    = 1 + (END - BEGIN)
         END DO

         LSTLED = BEGIN

      END IF

      RETURN
      END
