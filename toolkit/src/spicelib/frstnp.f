C$Procedure FRSTNP ( First non-printable character )

      INTEGER FUNCTION FRSTNP ( STRING )

C$ Abstract
C
C     Return the index of the first non-printable character in a
C     character string. ASCII characters 32-126 are considered
C     printable by this routine. (Blanks are considered printable.)
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
C     ASCII
C     CHARACTER
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    STRING

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   Input character string.
C
C     The function returns the index of first non-printable character in
C     STRING.
C
C$ Detailed_Input
C
C     STRING   is the input character string.
C
C$ Detailed_Output
C
C     The function returns the index of the first non-printable
C     character in the input string. Characters having integer codes
C     outside the range 32-126 are considered to be non-printable
C     characters. Blanks are considered to be printable characters. If
C     the input string contains no non-printable characters, FRSTNP is
C     zero.
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
C     This routine may be used to assist in validating strings that
C     are intended to be free of non-printable characters.
C
C     This routine treats blanks as printable characters. This choice
C     prevents embedded blanks from causing false positive results in
C     tests of strings for invalid characters. Note that the routine
C     FRSTPC and LASTPC treat blanks as non-printable.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Find the first non-printable character in a given string,
C        considering blanks as printable characters.
C
C
C        Example code begins here.
C
C
C              PROGRAM FRSTNP_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               FRSTNP
C
C        C
C        C     Local variables
C        C
C              CHARACTER*10    S
C
C        C
C        C     Add some random characters to the string, some of them
C        C     non-printable.
C        C
C              S( 1: 1) = 'A'
C              S( 2: 2) = ' '
C              S( 3: 3) = CHAR ( 3 )
C              S( 4: 4) = 'A'
C              S( 5: 5) = 'B'
C              S( 6: 6) = 'C'
C              S( 7: 7) = CHAR ( 7 )
C              S( 8: 8) = CHAR ( 8 )
C              S( 9: 9) = CHAR ( 9 )
C              S(10:10) = ' '
C
C              WRITE (*,'(A,I2)') 'First non-printable character '
C             .//                 'found at index', FRSTNP(S)
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        First non-printable character found at index 3
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
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 07-APR-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Created
C        complete code example from existing fragment.
C
C-    SPICELIB Version 1.0.0, 16-JUN-1995 (NJB) (IMU)
C
C-&


C$ Index_Entries
C
C     first non-printable character
C
C-&


C
C     Local variables
C
      INTEGER          I

C
C     Look for the first character outside the range [32,126], and
C     return its index.
C
      DO I = 1, LEN ( STRING )

         IF (       (ICHAR ( STRING(I:I) ) .LT.  32)
     .        .OR.  (ICHAR ( STRING(I:I) ) .GT. 126) ) THEN

            FRSTNP = I
            RETURN
         END IF

      END DO

C
C     Still here? All characters are printable. Return zero.
C
      FRSTNP = 0

      RETURN
      END
