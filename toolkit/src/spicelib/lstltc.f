C$Procedure LSTLTC ( Last character element less than )

      INTEGER FUNCTION  LSTLTC ( STRING, N, ARRAY )

C$ Abstract
C
C     Find the index of the largest array element less than a given  
C     character string in an ordered array of character strings.     
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

      CHARACTER*(*)    STRING
      INTEGER          N
      CHARACTER*(*)    ARRAY ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   Upper bound value to search against.
C     N          I   Number of elements in ARRAY.
C     ARRAY      I   Array of possible lower bounds.
C
C     The function returns the index of the last element of ARRAY that
C     is lexically less than STRING.
C
C$ Detailed_Input
C
C     STRING   is a string acting as an upper bound: the element of
C              ARRAY that is lexically the greatest element less than
C              STRING is to be found. Trailing blanks in this bound
C              value are not significant.
C
C     N        is the total number of elements in ARRAY.
C
C     ARRAY    is an array of character strings to be searched. Trailing
C              blanks in the strings in this array are not significant.
C              The strings in ARRAY must be sorted in non-decreasing
C              order. The elements of ARRAY need not be distinct.
C
C$ Detailed_Output
C
C     The function returns the index of the highest-indexed element in
C     the input array that is lexically less than STRING. The routine
C     assumes the array elements are sorted in non-decreasing order.
C
C     Indices range from 1 to N.
C
C     If all elements of ARRAY are lexically greater than or equal to
C     STRING, the routine returns the value 0. If N is less than or
C     equal to zero, the routine returns the value 0.
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
C     steps to compute the value of LSTLTC.
C
C     Note: If you need to find the first element of the array that is
C     lexically greater than or equal to STRING, simply add 1 to the
C     result returned by this function and check to see if the result is
C     within the array bounds given by N.
C
C$ Examples
C
C     Suppose that you have a long list of words, sorted alphabetically
C     and entirely in upper case. Furthermore suppose you wished to
C     find all words that begin the sequence of letters PLA,  then
C     you could execute the following code.
C
C           START = 0
C           I     = 1
C
C           DO I = 1, NWORDS
C
C              IF ( WORD(I)(1:3) .EQ. 'PLA' ) THEN
C
C                 IF ( START .EQ. 0 ) THEN
C                    START = I
C                 END IF
C
C                 END = I
C              END IF
C
C           END DO
C
C     This can of course be improved by stopping the loop once START
C     is non-zero and END remains unchanged after a pass through the
C     loop. However, this is a linear search  and on average can be
C     expected to take NWORDS/2 comparisons. The above algorithm
C     fails to take advantage of the structure of the list of words
C     (they are sorted).
C
C     The code below is much simpler to code, simpler to check, and
C     much faster than the code above.
C
C           START = LSTLTC( 'PLA', NWORDS, WORDS ) + 1
C           END   = LSTLTC( 'PLB', NWORDS, WORDS )
C
C           do something in case there are no such words.
C
C           IF ( START .GT. END ) THEN
C              START = 0
C              END   = 0
C           END IF
C
C     This code will never exceed 2 * LOG_2 ( NWORDS ) comparisons.
C     For a large list of words (say 4096) the second method will
C     take 24 comparisons  the first method requires on average
C     2048 comparisons. About 200 times as much time. Its clear
C     that if searches such as this must be performed often, that
C     the second approach could make the difference between being
C     able to perform the task in a few minutes as opposed to
C     several hours.
C
C     For more ideas regarding the use of this routine see LSTLEI
C     and LSTLTI.
C
C$ Restrictions
C
C     1)  If the sequence of character strings in the input array ARRAY
C         is not non-decreasing, the program will run to completion but
C         the index found will not mean anything.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     H.A. Neilan        (JPL)
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
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN)
C
C-&


C$ Index_Entries
C
C     last character element less_than
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
C        to compare against.  Zero is the only sensible thing to return
C
         LSTLTC = 0

      ELSE IF ( LLE(STRING, ARRAY(BEGIN)) ) THEN

C
C        None of the array elements are less than STRING
C
         LSTLTC = 0

      ELSE IF ( LLT(ARRAY(END), STRING) ) THEN

C
C        STRING is greater than all elements of the array.  Thus the las
C        element of the array is the last item less than STRING.
C
         LSTLTC = END

      ELSE

C
C        STRING lies between some pair of elements of the array
C

         DO WHILE ( ITEMS .GT. 2 )

            J        = ITEMS/2
            MIDDLE   = BEGIN + J

            IF ( LLT(ARRAY(MIDDLE), STRING) ) THEN
               BEGIN = MIDDLE
            ELSE
               END   = MIDDLE
            END IF

            ITEMS    = 1 + (END - BEGIN)
         END DO


         LSTLTC = BEGIN

      END IF

      RETURN
      END
