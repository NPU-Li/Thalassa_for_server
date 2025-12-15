C$Procedure BSRCHC ( Binary search for a character string )

      INTEGER FUNCTION BSRCHC ( VALUE, NDIM, ARRAY )

C$ Abstract
C
C     Do a binary search for a given value within a character array,
C     assumed to be in nondecreasing order. Return the index of the
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

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VALUE      I   Key value to be found in ARRAY.
C     NDIM       I   Dimension of ARRAY.
C     ARRAY      I   Character string array to search.
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
C              significant. The elements in ARRAY are assumed to
C              sorted according to the ASCII collating sequence.
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
C     A binary search is performed on the input array. If an element of
C     the array is found to match the input value, the index of that
C     element is returned. If no matching element is found, zero is
C     returned.
C
C$ Examples
C
C     Let ARRAY contain the following elements:
C
C           'BOHR'
C           'EINSTEIN'
C           'FEYNMAN'
C           'GALILEO'
C           'NEWTON'
C
C     Then
C
C           BSRCHC ( 'NEWTON',   5, ARRAY )    = 5
C           BSRCHC ( 'EINSTEIN', 5, ARRAY )    = 2
C           BSRCHC ( 'GALILEO',  5, ARRAY )    = 4
C           BSRCHC ( 'Galileo',  5, ARRAY )    = 0
C           BSRCHC ( 'BETHE',    5, ARRAY )    = 0
C
C$ Restrictions
C
C     1)  ARRAY is assumed to be sorted in increasing order according to
C         the ASCII collating sequence. If this condition is not met,
C         the results of BSRCHC are unpredictable.
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
C        unnecessary $Revisions section. Improved $Detailed_Input and
C        $Detailed_Output section.
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
C     binary search for a character_string
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
            BSRCHC = I
            RETURN

C
C        Otherwise narrow the search area.
C
         ELSE IF ( LLT (VALUE, ARRAY(I)) ) THEN
            RIGHT = I-1
         ELSE
            LEFT  = I+1
         END IF

      END DO

C
C     If the search area is empty, return zero.
C
      BSRCHC = 0

      RETURN
      END
