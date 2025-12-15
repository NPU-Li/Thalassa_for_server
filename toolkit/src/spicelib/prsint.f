C$Procedure PRSINT   ( Parse integer with error checking )

      SUBROUTINE PRSINT ( STRING, INTVAL )

C$ Abstract
C
C     Parse a string as an integer, encapsulating error handling.
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
C     INTEGER
C     PARSING
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         STRING
      INTEGER               INTVAL

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   String representing a numeric value.
C     INTVAL     O   Integer value obtained by parsing STRING.
C
C$ Detailed_Input
C
C     STRING   is a string representing a numeric value. Commas and
C              spaces may be used in this string for ease of reading
C              and writing the number. They are treated as
C              insignificant but non-error-producing characters.
C
C              For exponential representation any of the characters
C              'E','D','e','d' may be used.
C
C              The following are legitimate numeric expressions
C
C                 +12.2 e-1
C                 -3. 1415 9276
C                 1e6
C                 E8
C
C              The program also recognizes the following  mnemonics
C
C                 'PI',  'pi',  'Pi',  'pI'
C                 '+PI', '+pi', '+Pi', '+pI'
C                 '-PI', '-pi', '-Pi', '-pI'
C
C              and returns the value ( + OR - ) 3 as appropriate.
C
C$ Detailed_Output
C
C     INTVAL   is the integer obtained by parsing STRING. If an error is
C              encountered, INTVAL is not changed from whatever the
C              input value was. If the input string has a fractional
C              part, the fractional part will be truncated. Thus
C              3.18 is interpreted as 3. -4.98 is interpreted as -4.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input string cannot be parsed or if the string
C         represents a number that is outside the range of
C         representable integers, as defined by INTMIN and INTMAX, the
C         error SPICE(NOTANINTEGER) is signaled. The value of INTVAL is
C         not changed from whatever the input value was.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The purpose of this routine is to enable safe parsing of numeric
C     values into an INTEGER variable without the necessity of in-line
C     error checking.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Parse into an INTEGER variable a set of strings representing
C        numeric values.
C
C
C        Example code begins here.
C
C
C              PROGRAM PRSINT_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 10 )
C
C              INTEGER               STRLEN
C              PARAMETER           ( STRLEN = 11 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(STRLEN)    STRVAL ( SETSIZ )
C
C              INTEGER               I
C              INTEGER               INTVAL
C
C        C
C        C     Initialize the array of strings.
C        C
C              DATA                  STRVAL / '100,000,000',
C             .                               ' -2 690 192',
C             .                               '  +12.2 e-1',
C             .                               '-3. 141 592',
C             .                               '      1.2e8',
C             .                               '         E6',
C             .                               '         Pi',
C             .                               '        -PI',
C             .                               '-2147483648',
C             .                               ' 2147483647' /
C
C        C
C        C     Parse each string into an INTEGER variable.
C        C
C              WRITE(*,'(A)') '   STRVAL       INTVAL'
C              WRITE(*,'(A)') '-----------  ------------'
C              DO I = 1, SETSIZ
C
C                 CALL PRSINT ( STRVAL(I), INTVAL )
C
C                 WRITE(*,'(A11,2X,I12)') STRVAL(I), INTVAL
C
C              END DO
C
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C          STRVAL        INTVAL
C        -----------  ------------
C        100,000,000     100000000
C         -2 690 192      -2690192
C          +12.2 e-1             1
C        -3. 141 592            -3
C              1.2e8     120000000
C                 E6       1000000
C                 Pi             3
C                -PI            -3
C        -2147483648   -2147483648
C         2147483647    2147483647
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
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 04-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Updated the header to properly describe its input, output,
C        exceptions and particulars.
C
C-    SPICELIB Version 1.0.0, 22-JUL-1997 (NJB)
C
C-&


C$ Index_Entries
C
C     parse integer with encapsulated error handling
C
C-&


C
C     Local parameters
C
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 320 )

C
C     Local variables
C
      CHARACTER*(MSGLEN)    ERRMSG
      INTEGER               PTR

C
C     Use discovery check-in.
C
      CALL NPARSI ( STRING, INTVAL, ERRMSG, PTR )

      IF ( ERRMSG .NE. ' ' ) THEN

         CALL CHKIN  ( 'PRSINT'               )
         CALL SETMSG ( ERRMSG                 )
         CALL SIGERR ( 'SPICE(NOTANINTEGER)'  )
         CALL CHKOUT ( 'PRSINT'               )
         RETURN

      END IF

      RETURN
      END
