C$Procedure PRSDP   ( Parse d.p. number with error checking )

      SUBROUTINE PRSDP ( STRING, DPVAL )

C$ Abstract
C
C     Parse a string as a double precision number, encapsulating error
C     handling.
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
C     NUMBER
C     PARSING
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         STRING
      DOUBLE PRECISION      DPVAL

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   String representing a numeric value.
C     DPVAL      O   D.p. value obtained by parsing STRING.
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
C                 1e12
C                 E10
C
C              The program also recognizes the following  mnemonics
C
C                 'PI',  'pi',  'Pi',  'pI'
C                 '+PI', '+pi', '+Pi', '+pI'
C                 '-PI', '-pi', '-Pi', '-pI'
C
C              and returns the value
C
C                 ( + OR - ) 3.1415 9265 3589 7932 3846 26 ...
C
C              as appropriate.
C
C$ Detailed_Output
C
C     DPVAL    is the double precision number obtained by parsing
C              STRING.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input string cannot be parsed  due to use of an
C         unexpected or misplaced character or due to a string
C         representing a number too large for double precision, the
C         error SPICE(NOTADPNUMBER) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The purpose of this routine is to enable safe parsing of double
C     precision numbers without the necessity of in-line error checking.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Parse into a DOUBLE PRECISION variable a set of strings
C        representing numeric values.
C
C
C        Example code begins here.
C
C
C              PROGRAM PRSDP_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 8  )
C
C              INTEGER               STRLEN
C              PARAMETER           ( STRLEN = 11 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(STRLEN)    STRVAL ( SETSIZ )
C
C              DOUBLE PRECISION      DPVAL
C
C              INTEGER               I
C
C        C
C        C     Initialize the array of strings.
C        C
C              DATA                  STRVAL / '100,000,000',
C             .                               ' -2 690 192',
C             .                               '  +12.2 e-1',
C             .                               '-3. 141 592',
C             .                               '     1.2e12',
C             .                               '        E10',
C             .                               '         Pi',
C             .                               '        -PI' /
C
C        C
C        C     Parse each string into a DOUBLE PRECISION variable.
C        C
C              WRITE(*,'(A)') '   STRVAL               DPVAL'
C              WRITE(*,'(A)') '-----------  --------------------------'
C              DO I = 1, SETSIZ
C
C                 CALL PRSDP ( STRVAL(I), DPVAL )
C
C                 WRITE(*,'(A11,F28.12)') STRVAL(I), DPVAL
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
C           STRVAL               DPVAL
C        -----------  --------------------------
C        100,000,000      100000000.000000000000
C         -2 690 192       -2690192.000000000000
C          +12.2 e-1              1.220000000000
C        -3. 141 592             -3.141592000000
C             1.2e12  1200000000000.000000000000
C                E10    10000000000.000000000000
C                 Pi              3.141592653590
C                -PI             -3.141592653590
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
C-    SPICELIB Version 1.1.1, 28-MAY-2020 (JDR)
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Updated the header to properly describe its input, output,
C        exceptions and particulars.
C
C-    SPICELIB Version 1.1.0, 15-SEP-1997 (NJB)
C
C        Bug fix: output argument declaration changed from INTEGER
C        to DOUBLE PRECISION.
C
C-    SPICELIB Version 1.0.0, 22-JUL-1997 (NJB)
C
C-&


C$ Index_Entries
C
C     parse d.p. number with encapsulated error handling
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
      CALL NPARSD ( STRING, DPVAL, ERRMSG, PTR )

      IF ( ERRMSG .NE. ' ' ) THEN

         CALL CHKIN  ( 'PRSDP'               )
         CALL SETMSG ( ERRMSG                )
         CALL SIGERR ( 'SPICE(NOTADPNUMBER)' )
         CALL CHKOUT ( 'PRSDP'               )
         RETURN

      END IF

      RETURN
      END
