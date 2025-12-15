C$Procedure LSTLTI ( Last integer element less than )

      INTEGER FUNCTION  LSTLTI ( X, N, ARRAY )

C$ Abstract
C
C     Find the index of the largest array element less than 
C     a given integer X in an array of non-decreasing integers.
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

      INTEGER          X
      INTEGER          N
      INTEGER          ARRAY ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     X          I   Upper bound value to search against.
C     N          I   Number of elements in ARRAY.
C     ARRAY      I   Array of possible lower bounds.
C
C     The function returns the index of the last element of ARRAY that
C     is less than X.
C
C$ Detailed_Input
C
C     X        is an integer value acting as an upper bound: the element
C              of ARRAY that is the greatest element less than X is to
C              be found.
C
C     N        is the total number of elements in ARRAY.
C
C     ARRAY    is an array of integers that forms a non-decreasing
C              sequence. The elements of array need not be distinct.
C
C$ Detailed_Output
C
C     The function returns the index of the highest-indexed element in
C     the input array that is less than X. The routine assumes the array
C     elements are sorted in non-decreasing order.
C
C     Indices range from 1 to N.
C
C     If all elements of ARRAY are greater than or equal to X, the
C     routine returns the value 0. If N is less than or equal to zero,
C     the routine returns the value 0.
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
C     steps to compute the value of LSTLTI.
C
C     Note: If you need to find the first element of the array that is
C     greater than or equal to X, simply add 1 to the result returned by
C     this function and check to see if the result is within the array
C     bounds given by N.
C
C$ Examples
C
C     Suppose that you have an reasonably large ordered array of
C     integers, into which you want to insert a few more without
C     destroying the ordering.
C
C     Depending upon your application, it may be desirable to
C     not insert duplicates, to insert duplicates before
C     existing entries or to insert them after existing entries.
C
C     The code fragment below, illustrates an insertion scheme
C     that will insert duplicate items before existing items
C     and simultaneously update a second parallel array of
C     double precision numbers.
C
C           get the pair to insert
C
C           READ (*,*) KEY, VALUE
C
C           locate the place to insert the new KEY into the sorted
C           array of keys.
C
C           LOC = LSTLTI ( KEY, NKEYS, KEYS ) + 1
C
C           insert the key and its associated value into the
C           KEYS and  VALUES arrays at location LOC
C
C           CALL INSLAI ( KEY,   1, LOC, NKEYS, KEYS   )
C           CALL INSLAD ( VALUE, 1, LOC, NVALS, VALUES )
C
C     If at the READ statement the arrays KEYS and VALUES looked like:
C
C           KEYS     VALUES     NKEYS = 6, NVALS = 6
C           ----     -------
C             2       3.00D0
C             5       1.00D0
C             7       3.14D0
C            16       7.11D0
C            18       2.14D0
C            23      12.12D0
C
C     and 9 and 33.33D3 were read into KEY and VALUE respectively
C     then LSTLEI (KEY, NKEYS, KEYS ) would be 3 and LOC would be 4.
C     After the calls to the routines INSLAI and INSLAD we would have
C
C           KEYS     VALUES     NKEYS = 7, NVALS = 7
C           ----     -------
C             2       3.00D0
C             5       1.00D0
C             7       3.14D0
C             9      33.33D3     <===== inserted items.
C            16       7.11D0
C            18       2.14D0
C            23      12.12D0
C
C     If 7 and 33.33D3 were read into KEY and VALUE respectively
C     then again LSTLEI (KEY, NKEYS, KEYS ) would be 2 and LOC would
C     be 3. After the calls to the routines INSLAI and INSLAD we
C     would have:
C
C           KEYS     VALUES     NKEYS = 7, NVALS = 7
C           ----     -------
C             2       3.00D0
C             5       1.00D0
C             7      33.33D3     <===== inserted items.
C             7       3.14D0
C            16       7.11D0
C            18       2.14D0
C            23      12.12D0
C
C     If we replaced the line of code
C
C           LOC = LSTLTI ( KEY, NKEYS, KEYS ) + 1
C     by
C
C           LOC = LSTLEI ( KEY, NKEYS, KEYS ) + 1
C
C     we would obtain a routine that inserted duplicates before
C     existing entries. (LSTLEI is similar to LSTLTI except it finds
C     the last occurrence of an integer less than or equal to a value.)
C     Using 7 and 33.33D3 for KEY and VALUE again, the modified code
C     fragment would yield the results shown below.
C
C           KEYS     VALUES     NKEYS = 7, NVALS = 7
C           ----     -------
C             2       3.00D0
C             5       1.00D0
C             7       3.14D0
C             7      33.33D3     <===== inserted items.
C            16       7.11D0
C            18       2.14D0
C            23      12.12D0
C
C
C     Note: you should NOT use the code outlined above as the basis of
C     a sorting algorithm. The SPICELIB routines SHELLI, SHELLD, SHELLC,
C     ORDERI, ORDERD, ORDERC, REORDI, REORDD and REORDC are much more
C     efficient routines for sorting arrays or sorting a set of
C     parallel arrays using one of the set as a key. The fragment
C     presented here is useful for performing update insertions into
C     previously ordered arrays.
C
C     For more ideas regarding the use of this routine, see LSTLTC
C     and LSTLTC.
C
C$ Restrictions
C
C     1)  If the sequence of integer numbers in the input array ARRAY is
C         not non-decreasing, the program will run to completion but the
C         index found will not mean anything.
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
C     last integer element less_than
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
         LSTLTI = 0

      ELSE IF ( X .LE. ARRAY(BEGIN) ) THEN

C
C        None of the array elements are less than X
C
         LSTLTI = 0

      ELSE IF ( ARRAY(END) .LT. X ) THEN

C
C        X is greater than all elements of the array.  Thus the last
C        element of the array is the last item less than X.
C
         LSTLTI = END

      ELSE

C
C        X lies between some pair of elements of the array
C

         DO WHILE ( ITEMS .GT. 2 )

            J        = ITEMS/2
            MIDDLE   = BEGIN + J

            IF ( ARRAY(MIDDLE) .LT. X ) THEN
               BEGIN = MIDDLE
            ELSE
               END   = MIDDLE
            END IF

            ITEMS    = 1 + (END - BEGIN)
         END DO


         LSTLTI = BEGIN

      END IF

      RETURN
      END
