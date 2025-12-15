C$Procedure ESRCHC ( Equivalence search, character )

      INTEGER FUNCTION ESRCHC ( VALUE, NDIM, ARRAY )

C$ Abstract
C
C     Search for a given value within a character string array.
C     Return the index of the first equivalent array entry, or zero
C     if no equivalent element is found.
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
C     The function returns the index of the first array entry equivalent
C     to VALUE, or zero if none is found.
C
C$ Detailed_Input
C
C     VALUE    is an arbitrary character string.
C
C     NDIM     is the dimension of (number of elements in) an array of
C              character strings.
C
C     ARRAY    is the array.
C
C$ Detailed_Output
C
C     The function returns the index of the first element of the input
C     array equivalent to the input value, or zero if the array contains
C     no such elements.
C
C     Two strings are equivalent if they contain the same characters in
C     the same order, when blanks are ignored and uppercase and
C     lowercase characters are considered equal.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ESRCHC is identical to ISRCHC, except that it looks for the first
C     equivalent string (as defined by EQSTR) instead of the first
C     identical one.
C
C$ Examples
C
C     Let ARRAY contain the following elements:
C
C        ARRAY(1) = 'This'
C        ARRAY(2) = 'little'
C        ARRAY(3) = 'piggy'
C        ARRAY(4) = 'went'
C        ARRAY(5) = 'to'
C        ARRAY(6) = 'market'
C
C     Then
C
C        ESRCHC ( 'PIGGY',      6, ARRAY )  =  3
C        ESRCHC ( ' LiTtLe  ',  6, ARRAY )  =  2
C        ESRCHC ( 'W e n t',    6, ARRAY )  =  4
C        ESRCHC ( 'mall',       6, ARRAY )  =  0
C
C$ Restrictions
C
C     1)  ESRCHC assumes that the function EQSTR does not participate
C         in normal SPICELIB error handling.
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
C-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
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
C     search array for equivalent character_string
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               EQSTR

C
C     Local variables
C
      INTEGER               I


C
C     Just like ISRCHC.
C
      ESRCHC = 0

      DO I = 1, NDIM

         IF ( EQSTR ( ARRAY(I), VALUE ) ) THEN
            ESRCHC = I
            RETURN
         END IF

      END DO

      RETURN
      END
