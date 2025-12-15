C$Procedure BSCHOC ( Binary search with order vector, character )

      INTEGER FUNCTION BSCHOC ( VALUE, NDIM, ARRAY, ORDER )

C$ Abstract
C
C     Do a binary search for a given value within an array of character
C     strings, accompanied by an order vector. Return the index of the
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

      CHARACTER*(*)    VALUE
      INTEGER          NDIM
      CHARACTER*(*)    ARRAY ( * )
      INTEGER          ORDER ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VALUE      I   Key value to be found in ARRAY.
C     NDIM       I   Dimension of ARRAY.
C     ARRAY      I   Character string array to search.
C     ORDER      I   Order vector.
C
C     The function returns the index of the first matching array element
C     or zero if the value is not found.
C
C$ Detailed_Input
C
C     VALUE    is the key value to be found in the array. Trailing
C              blanks in this key are not significant: string matches
C              found by this routine do not require trailing blanks in
C              value to match those in the corresponding element of
C              array.
C
C     NDIM     is the number of elements in the input array.
C
C     ARRAY    is the array of character strings to be searched.
C              Trailing blanks in the strings in this array are not
C              significant.
C
C     ORDER    is an order array that can be used to access the elements
C              of ARRAY in order (according to the ASCII collating
C              sequence). The contents of ORDER are a permutation of
C              the sequence of integers ranging from 1 to NDIM.
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
C     1)  If NDIM < 1, the value of the function is zero. This is not
C         considered an error.
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
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Search for different character strings in an array that
C        is sorted following a given criteria, not necessarily
C        alphabetically.
C
C        Example code begins here.
C
C
C              PROGRAM BSCHOC_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              INTEGER                 BSCHOC
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 NDIM
C              PARAMETER             ( NDIM   = 5  )
C
C              INTEGER                 STRLEN
C              PARAMETER             ( STRLEN = 8  )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(STRLEN)      ARRAY  ( NDIM )
C              CHARACTER*(STRLEN)      NAMES  ( NDIM )
C
C              INTEGER                 I
C              INTEGER                 IDX
C              INTEGER                 ORDER  ( NDIM )
C
C
C        C
C        C     Let ARRAY and ORDER contain the following elements:
C        C
C              DATA                    ARRAY  / 'FEYNMAN', 'BOHR',
C             .                     'EINSTEIN', 'NEWTON',  'GALILEO' /
C
C              DATA                    ORDER  / 2, 3, 1, 5, 4 /
C
C        C
C        C     Set the list of NAMES to be searched.
C        C
C              DATA                    NAMES /  'NEWTON',  'EINSTEIN',
C             .                      'GALILEO', 'Galileo', 'BETHE'    /
C
C        C
C        C     Search for the NAMES.
C        C
C              DO I = 1, NDIM
C
C                 IDX = BSCHOC ( NAMES(I), NDIM, ARRAY, ORDER )
C
C                 IF ( IDX .EQ. 0 ) THEN
C
C                    WRITE(*,*) 'Name ', NAMES(I),
C             .                 ' not found in ARRAY.'
C
C                 ELSE
C
C                    WRITE(*,*) 'Name ', NAMES(I),
C             .                 ' found in position', IDX
C
C                 END IF
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
C         Name NEWTON   found in position           4
C         Name EINSTEIN found in position           3
C         Name GALILEO  found in position           5
C         Name Galileo  not found in ARRAY.
C         Name BETHE    not found in ARRAY.
C
C
C        Note that these results indicate that:
C
C            ARRAY(4) = 'NEWTON'
C            ARRAY(3) = 'EINSTEIN'
C            ARRAY(5) = 'GALILEO'
C
C$ Restrictions
C
C     1)  ORDER is assumed to give the order of the elements of ARRAY in
C         increasing order according to the ASCII collating sequence. If
C         this condition is not met, the results of BSCHOC are
C         unpredictable.
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
C-    SPICELIB Version 1.0.1, 17-JUN-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Updated $Brief_I/O, $Detailed_Input and $Detailed_Output
C        sections to improve the description of the arguments and
C        returned values of the function.
C
C-    SPICELIB Version 1.0.0, 18-SEP-1995 (IMU) (WLT)
C
C-&


C$ Index_Entries
C
C     binary search for a string using an order vector
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
            BSCHOC = ORDER(I)
            RETURN

C
C        Otherwise narrow the search area.
C
         ELSE IF ( LLT (VALUE, ARRAY(ORDER(I))) ) THEN
            RIGHT = I-1
         ELSE
            LEFT  = I+1
         END IF

      END DO

C
C     If the search area is empty, return zero.
C
      BSCHOC = 0

      RETURN
      END
