C$Procedure HX2DP ( Hexadecimal string to d.p. number )

      SUBROUTINE HX2DP ( STRING, NUMBER, ERROR, ERRMSG )

C$ Abstract
C
C     Convert a string representing a double precision number in a
C     base 16 "scientific notation" into its equivalent double
C     precision number.
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
C     ALPHANUMERIC
C     CONVERSION
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               MAXMAN
      PARAMETER           ( MAXMAN = 31 )


      CHARACTER*(*)         STRING
      DOUBLE PRECISION      NUMBER
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MAXMAN     P   Maximum number of digits in a hex mantissa.
C     STRING     I   Hex form string to convert to double precision.
C     NUMBER     O   Double precision value to be returned.
C     ERROR      O   A logical flag which is .TRUE. on error.
C     ERRMSG     O   A descriptive error message.
C
C$ Detailed_Input
C
C     STRING   is a character string containing a base 16 "scientific
C              notation" representation of a double precision number
C              which is to be converted to a double precision number.
C              Examples of such a string are:
C
C                 '2A^3' = ( 2/16 + 10/( 16**2 ) ) * 16**3 = 672.0
C
C              and
C
C                 '-B^1' = - ( 11/16 ) * 16**1             = -11.0
C
C              The following table describes the character set used to
C              represent the hexadecimal digits and their corresponding
C              values.
C
C                 Character     Value         Character     Value
C                 ---------    -------        ---------    -------
C                    '0'         0.0D0           '8'         8.0D0
C                    '1'         1.0D0           '9'         9.0D0
C                    '2'         2.0D0         'A','a'      10.0D0
C                    '3'         3.0D0         'B','b'      11.0D0
C                    '4'         4.0D0         'C','c'      12.0D0
C                    '5'         5.0D0         'D','d'      13.0D0
C                    '6'         6.0D0         'E','e'      14.0D0
C                    '7'         7.0D0         'F','f'      15.0D0
C
C              The caret, or hat, character, '^', is used to
C              distinguish the exponent.
C
C              The plus sign, '+', and the minus sign, '-', are used,
C              and they have their usual meanings.
C
C              A base 16 "scientific notation" character string which
C              is to be parsed by this routine should consist of a sign,
C              '+' or '-' (the plus sign is optional for nonnegative
C              numbers), followed immediately by a contiguous sequence
C              of hexadecimal digits, the exponent character, and a
C              signed hexadecimal exponent. The exponent is required,
C              but the sign is optional for a nonnegative exponent.
C
C              A number in base 16 "scientific notation" consists of
C              a contiguous sequence of characters with one of the
C              following formats:
C
C                 (1)   h h h h  ... h ^H H  ... H
C                        1 2 3 4      n  1 2      m
C
C                 (2)   +h h h h  ... h ^H H  ... H
C                         1 2 3 4      n  1 2      m
C
C                 (3)   -h h h h  ... h ^H H  ... H
C                         1 2 3 4      n  1 2      m
C
C                 (4)    h h h h  ... h ^+H H  ... H
C                         1 2 3 4      n   1 2      m
C
C                 (5)   +h h h h  ... h ^+H H  ... H
C                         1 2 3 4      n   1 2      m
C
C                 (6)   -h h h h  ... h ^+H H  ... H
C                         1 2 3 4      n   1 2      m
C
C                 (7)   h h h h  ... h ^-H H  ... H
C                        1 2 3 4      n   1 2      m
C
C                 (8)   +h h h h  ... h ^-H H  ... H
C                         1 2 3 4      n   1 2      m
C
C                 (9)   -h h h h  ... h ^-H H  ... H
C                         1 2 3 4      n   1 2      m
C
C              where
C
C                 h  and H  denote hexadecimal digits;
C                  i      j
C
C                 ^         denotes exponentiation;
C
C              and
C
C                 + and - have their usual interpretations.
C
C              STRING may have leading and trailing blanks, but blanks
C              embedded within the significant portion of the input
C              string are not allowed.
C
C$ Detailed_Output
C
C     NUMBER   is the double precision value to be returned. The value
C              of this argument is not changed if an error occurs while
C              parsing the input string.
C
C     ERROR    is a logical flag which indicates whether an error
C              occurred while attempting to parse NUMBER from the input
C              character string STRING. ERROR will have the value
C              .TRUE. if an error occurs. It will have the value
C              .FALSE. otherwise.
C
C     ERRMSG   is a descriptive error message if an error occurs while
C              attempting to parse the number NUMBER from the
C              hexadecimal character string STRING, blank otherwise.
C
C$ Parameters
C
C     MAXMAN   is the maximum number of digits in a hexadecimal
C              mantissa. The value of MAXMAN is 31.
C
C              The current value of MAXMAN is more than sufficient for
C              most double precision implementations, providing almost
C              twice as many digits as can actually be produced. This
C              value may be changed when a greater precision is known
C              to exist among all of the supported platforms.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If an unexpected character is encountered, an appropriate
C         error message will be set, and the routine will exit. The
C         value of NUMBER will be unchanged.
C
C     2)  If the input string represents a number that is larger in
C         absolute magnitude than the maximum representable
C         double precision number an appropriate error message
C         will be set, and the routine will exit. The value of
C         NUMBER will be unchanged.
C
C     3)  If the input string is blank, an appropriate error message
C         will be set, and the routine will exit. The value of
C         NUMBER will be unchanged.
C
C     4)  If the string has too many digits in the mantissa, then an
C         appropriate error message will be set, and the routine will
C         exit. The value of NUMBER will be unchanged.
C
C     5)  If the output error message string is not long enough to
C         contain the entire error message, the error message will be
C         truncated on the right.
C
C     6)  This routine does NOT check for underflow errors when
C         constructing a double precision number.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine will convert a character string containing a number
C     in base 16 "scientific notation" into its equivalent double
C     precision number.
C
C     This routine is one of a pair of routines which are used to
C     perform conversions between double precision numbers and
C     an equivalent base 16 "scientific notation" character string
C     representation:
C
C           DP2HX  -- Convert a double precision number into a base 16
C                     "scientific notation" character string.
C
C           HX2DP  -- Convert a base 16 "scientific notation"
C                     character string into a double precision number.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Convert a set of character strings containing a base 16
C        "scientific notation" representation of a double precision
C        number, to their double precision values.
C
C
C        Example code begins here.
C
C
C              PROGRAM HX2DP_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local constants.
C        C
C              INTEGER               ERRLEN
C              PARAMETER           ( ERRLEN = 80 )
C
C              INTEGER               STRLEN
C              PARAMETER           ( STRLEN = 17 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(ERRLEN)    ERRMSG
C              CHARACTER*(STRLEN)    NUMBER ( 16 )
C
C              DOUBLE PRECISION      VALUE
C
C              INTEGER               I
C
C              LOGICAL               ERROR
C
C        C
C        C     Assign an array of strings representing, in base 16
C        C     "scientific notation", double precision numbers.
C        C     Not all of them are valid representations.
C        C
C              DATA                  NUMBER /
C             .                  '89705F4136B4A6^-7', '12357898765X34',
C             .                  '1^1',               '-1^1',
C             .                  '4^3',               '-4^3',
C             .                  '7F5EB^5',           '7F5eb^5',
C             .                  '1B^2',              '+1B^2',
C             .                  '+1B^+2',            '0^0',
C             .                  ' ',                 '-AB238Z^2',
C             .                  '234ABC',            '234ABC^'    /
C
C        C
C        C     Loop over the NUMBER array, call HX2DP for each
C        C     element of NUMBER.
C        C
C              WRITE(*,'(A)') 'string             number'
C              WRITE(*,'(A)') '-----------------  ----------------'
C
C              DO I= 1, 16
C
C                 CALL HX2DP ( NUMBER(I), VALUE, ERROR, ERRMSG )
C
C                 IF ( ERROR ) THEN
C
C                    WRITE(*,'(A17,2X,A)') NUMBER(I), ERRMSG
C
C                 ELSE
C
C                    WRITE(*,'(A17,X,E17.9)') NUMBER(I), VALUE
C
C                 END IF
C
C              END DO
C
C        C
C        C     Finally, try with a number that has too many digits in
C        C     the mantissa.
C        C
C              CALL HX2DP ( '4ABC123AB346523BDC568798C2473678^1',
C             .             VALUE, ERROR, ERRMSG )
C
C              WRITE(*,*)
C              WRITE(*,*) 'String 4ABC123AB346523BDC568798C2473678^1 '
C             .        // 'produces:'
C              WRITE(*,*) '   ', ERRMSG
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        string             number
C        -----------------  ----------------
C        89705F4136B4A6^-7   0.200000000E-08
C        12357898765X34     ERROR: Illegal character 'X' encountered.
C        1^1                 0.100000000E+01
C        -1^1               -0.100000000E+01
C        4^3                 0.102400000E+04
C        -4^3               -0.102400000E+04
C        7F5EB^5             0.521707000E+06
C        7F5eb^5             0.521707000E+06
C        1B^2                0.270000000E+02
C        +1B^2               0.270000000E+02
C        +1B^+2              0.270000000E+02
C        0^0                 0.000000000E+00
C                           ERROR: A blank input string is not allowed.
C        -AB238Z^2          ERROR: Illegal character 'Z' encountered.
C        234ABC             ERROR: Missing exponent.
C        234ABC^            ERROR: Missing exponent.
C
C         String 4ABC123AB346523BDC568798C2473678^1 produces:
C            ERROR: Too many digits in the mantissa (> 31).
C
C
C        Note: The hat or caret, '^', signals an exponent.
C
C        Note that some errors are machine dependent. For example,
C        for a VAX using D_floating arithmetic we get:
C
C           STRING = '23BCE^30'
C           NUMBER = ( Not defined )
C           ERROR  = .TRUE.
C           ERRMSG = 'ERROR: Number is too large to be represented.'
C
C           STRING = '-2abc3^22'
C           NUMBER = ( Not defined )
C           ERROR  = .TRUE.
C           ERRMSG = 'ERROR: Number is too small to be represented.'
C
C$ Restrictions
C
C     1)  The current value of MAXMAN is more than sufficient for most
C         double precision implementations, providing almost twice as
C         many digits as can actually be produced.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) (BVS)
C
C        Added IMPLICIT NONE statement.
C
C        The declaration of MAXMAN has been promoted to the
C        $Declarations section and the error produced when the maximum
C        number of digits for the mantissa is exceeded has been updated
C        to inform about MAXMAN value.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing example.
C
C        Updated $Brief_I/O, $Parameters, $Exceptions and $Restrictions
C        sections to properly describe MAXMAN.
C
C        Corrected $Revisions entries.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1994 (KRG)
C
C        Fixed a typo in the description of the input argument STRING.
C        The example showing the expansion of 160 into hexadecimal
C        was incorrect. 160 was replaced with 672 which makes the
C        example correct.
C
C-    SPICELIB Version 1.0.0, 26-OCT-1992 (KRG)
C
C-&


C$ Index_Entries
C
C     convert signed normalized hexadecimal string to d.p.
C     convert encoded d.p. number to d.p. number
C     convert base 16 scientific notation d.p. number
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      DPMAX
C
C     Local Parameters
C
      CHARACTER*(1)         EXPCHR
      PARAMETER           ( EXPCHR = '^' )

      DOUBLE PRECISION      BASE
      PARAMETER           ( BASE = 16.0D0 )

      DOUBLE PRECISION      INVBAS
      PARAMETER           ( INVBAS = 1.0D0/BASE )

      INTEGER               MAXDIG
      PARAMETER           ( MAXDIG = 16 )


C
C     Local variables
C
      DOUBLE PRECISION      DPVAL(0:MAXDIG-1)
      DOUBLE PRECISION      MAXDP
      DOUBLE PRECISION      MINDP
      DOUBLE PRECISION      SCALES(MAXMAN)
      DOUBLE PRECISION      TMPNUM

      INTEGER               DIGBEG
      INTEGER               DIGEND
      INTEGER               I
      INTEGER               IEXPCH
      INTEGER               IEXPON
      INTEGER               IMINUS
      INTEGER               IPLUS
      INTEGER               ISPACE
      INTEGER               IVAL(MAXMAN+1)
      INTEGER               LCCBEG
      INTEGER               LCCEND
      INTEGER               LETTER
      INTEGER               NDIGIT
      INTEGER               POSITN
      INTEGER               STRBEG
      INTEGER               STREND
      INTEGER               UCCBEG
      INTEGER               UCCEND

      LOGICAL               FIRST
      LOGICAL               MORE
      LOGICAL               NEGTIV
      LOGICAL               FNDEXP
C
C     Saved variables
C
      SAVE                  DPVAL
      SAVE                  MAXDP
      SAVE                  MINDP
      SAVE                  SCALES

      SAVE                  DIGBEG
      SAVE                  DIGEND
      SAVE                  IEXPCH
      SAVE                  IMINUS
      SAVE                  IPLUS
      SAVE                  ISPACE
      SAVE                  LCCBEG
      SAVE                  LCCEND
      SAVE                  UCCBEG
      SAVE                  UCCEND

      SAVE                  FIRST
C
C     Initial values
C
      DATA                  DPVAL   /
     .                                 0.0D0,  1.0D0,  2.0D0,  3.0D0,
     .                                 4.0D0,  5.0D0,  6.0D0,  7.0D0,
     .                                 8.0D0,  9.0D0, 10.0D0, 11.0D0,
     .                                12.0D0, 13.0D0, 14.0D0, 15.0D0
     .                              /

      DATA                  FIRST   / .TRUE. /



C
C     Here is a brief outline of the algorithm used to convert the
C     character string into its equivalent double precision number.
C
C        The input hexadecimal string is scanned from left to right.
C
C        0) Any leading white space is skipped.
C
C        1) The length of the significant portion of the string
C           is determined.
C
C        2) The sign of the mantissa is determined.
C
C        3) The digits of the hexadecimal mantissa are parsed.
C
C        4) The exponent of the number is parsed.
C
C        5) The mantissa of the double precision number is generated
C           by summing appropriately scaled values of the hexadecimal
C           mantissa digits which were collected in step 2. The
C           summation is performed so that the summands are added
C           in order of increasing magnitude to eliminate a potential
C           loss of significance which might occur otherwise. This
C           yields a number in the range of 1/BASE and 1.0 or zero.
C
C        6) The double precision number is then scaled by the exponent
C           obtained in step 3.
C
      IF ( FIRST ) THEN
C
C        If this is the first call, set up the array that is used to
C        properly scale each of the hexadecimal digits when summing
C        them to build a double precision number. Right now, the value
C        of MAXMAN, the maximum number of digits in a hexadecimal
C        mantissa, is 31. MAXMAN = 31 is more than sufficient for most
C        current double precision implementations, providing almost
C        twice as many digits as can actually be produced. This value
C        may be changed when a greater precision is known to exist on
C        any of the supported platforms.
C
         FIRST = .FALSE.

         SCALES(1) = INVBAS
         DO I = 2, MAXMAN

            SCALES(I) = INVBAS * SCALES(I-1)

         END DO
C
C        Initialize the upper and lower bounds for the decimal digits,
C        the upper and lower bounds for the uppercase hexadecimal
C        digits, the upper and lower bounds for the lowercase
C        hexadecimal digits, the space, the plus sign, and the minus
C        sign in the character sequence.
C
         DIGBEG = ICHAR( '0' )
         DIGEND = ICHAR( '9' )
         UCCBEG = ICHAR( 'A' )
         UCCEND = ICHAR( 'F' )
         LCCBEG = ICHAR( 'a' )
         LCCEND = ICHAR( 'f' )
         IMINUS = ICHAR( '-' )
         IPLUS  = ICHAR( '+' )
         ISPACE = ICHAR( ' ' )
C
C        Also get the integer value for the exponent character.
C
         IEXPCH = ICHAR( EXPCHR )
C
C        Initialize some boundary values for error checking while
C        constructing the desired double precision number. These
C        are used to help determine whether an overflow condition
C        is imminent due to the overly large magnitude of a positive
C        or negative number.
C
         MINDP = DPMIN() * INVBAS
         MAXDP = DPMAX() * INVBAS

      END IF
C
C     There are no errors initially, so set the error flag to
C     .FALSE.
C
      ERROR = .FALSE.
C
C     If the string is blank, set the error flag and return immediately.
C
      IF ( STRING .EQ. ' ' ) THEN

         ERROR  = .TRUE.
         ERRMSG = 'ERROR: A blank input string is not allowed.'
         RETURN

      END IF
C
C     Initialize a few other things.
C
      ERRMSG = ' '
      TMPNUM = 0.0D0
C
C     Assume that the number is nonnegative.
C
      NEGTIV = .FALSE.
C
C     Skip any leading white space. We know that there is at least
C     one nonblank character at this point, so we will not loop
C     off the end of the string.
C
      STRBEG = 1

      DO WHILE ( ICHAR( STRING(STRBEG:STRBEG ) ) .EQ. ISPACE )

         STRBEG = STRBEG + 1

      END DO
C
C     Now, we want to find the end of the significant portion of
C     the input string and the position of the exponent character.
C
      STREND = STRBEG + 1
      MORE   = .TRUE.

      DO WHILE ( MORE )

         IF ( STREND .LE. LEN( STRING ) ) THEN

            IF ( STRING(STREND:) .NE. ' ' ) THEN

               STREND = STREND + 1

            ELSE

               MORE = .FALSE.

            END IF

         ELSE

            MORE = .FALSE.

         END IF

      END DO
C
C     At this point, STREND is one larger than the length of the
C     significant portion of the string because we incremented
C     its value after the test. We will subtract one from the
C     value of STREND so that it exactly represents the position
C     of the last significant character in the string.
C
      STREND = STREND - 1
C
C     Set the position pointer to the beginning of the significant
C     part, i.e., the nonblank part, of the string, because we are
C     now ready to try and parse the number.
C
      POSITN = STRBEG
C
C     The first character should be either a plus sign, '+', a
C     minus sign, '-', or a digit, '0' - '9', 'A' - 'F', or
C     'a' - 'f'. Anything else is bogus and we will catch it in
C     the main loop below.
C
C     If the character is a minus sign, we want to set the value of
C     NEGTIV to .TRUE. and increment the position.
C
C     If the character is a plus sign, we want to increment the
C     position.
C
      IF ( ICHAR( STRING(POSITN:POSITN) ) .EQ. IMINUS ) THEN

         NEGTIV = .TRUE.
         POSITN = POSITN + 1

      ELSE IF ( ICHAR( STRING(POSITN:POSITN) ) .EQ. IPLUS ) THEN

         POSITN = POSITN + 1

      END IF
C
C     Collect all of the digits in the mantissa, storing them
C     for later conversion. We do this because we want to add
C     the digits of the mantissa in increasing order so that we
C     do not lose any significance.
C
C     A normalized hexadecimal number must have an exponent,
C     which is represented by the hat character, EXPCHR, which
C     s why that test is part of the loop termination.
C
C     We currently have no digits, and we have not found the
C     exponent character yet.
C
      NDIGIT = 0
      FNDEXP = .FALSE.
      DO WHILE ( ( POSITN .LE. STREND ) .AND. ( .NOT. FNDEXP ) )

         LETTER = ICHAR( STRING(POSITN:POSITN) )

         IF ( ( LETTER .GE. DIGBEG ) .AND.
     .        ( LETTER .LE. DIGEND ) ) THEN

            POSITN       = POSITN + 1
            NDIGIT       = NDIGIT + 1
            IVAL(NDIGIT) = LETTER - DIGBEG

         ELSE IF ( ( LETTER .GE. UCCBEG ) .AND.
     .             ( LETTER .LE. UCCEND ) ) THEN

            POSITN       = POSITN + 1
            NDIGIT       = NDIGIT + 1
            IVAL(NDIGIT) = 10 + LETTER - UCCBEG

         ELSE IF ( ( LETTER .GE. LCCBEG ) .AND.
     .             ( LETTER .LE. LCCEND ) ) THEN

            POSITN       = POSITN + 1
            NDIGIT       = NDIGIT + 1
           IVAL(NDIGIT) = 10 + LETTER - LCCBEG

         ELSE IF ( LETTER .EQ. IEXPCH ) THEN
C
C           We have found the exponent character, so set the
C           indicator and increment the position.
C
            FNDEXP = .TRUE.
            POSITN = POSITN + 1

         ELSE

            ERROR  = .TRUE.
            ERRMSG = 'ERROR: Illegal character ''#'' encountered.'
            CALL REPMC( ERRMSG, '#', CHAR( LETTER ), ERRMSG )
            RETURN

         END IF
C
C        We need to make sure that the number of mantissa digits
C        remains less than or equal to the number of mantissa
C        digits that we declared, see the MAXMAN parameter.
C
         IF ( NDIGIT .GT. MAXMAN ) THEN

            ERROR  = .TRUE.
            ERRMSG = 'ERROR: Too many digits in the mantissa (> #).'
            CALL REPMI ( ERRMSG, '#', MAXMAN, ERRMSG )
            RETURN

         END IF

      END DO
C
C     At this point, we have found an exponent character, and:
C
C        1) We are beyond the end of the significant portion of the
C           string, which is an error: no exponent digits were found.
C
C        2) We are positioned on the first digit of the exponent,
C           and are ready to try and parse it.
C
      IF ( POSITN .LE. STREND ) THEN
C
C        If there is at least one significant character left in the
C        string, we need to try and parse it as an exponent.
C
         CALL HX2INT( STRING(POSITN:), IEXPON, ERROR, ERRMSG )

         IF ( ERROR ) THEN
C
C           If an error occurred while attempting to parse the
C           exponent, we simply want to exit. The error message
C           is already set.
C
            RETURN

         END IF

      ELSE

         ERROR  = .TRUE.
         ERRMSG = 'ERROR: Missing exponent.'
         RETURN

      END IF
C
C     We now have everything that we need to build the double
C     precision number, a mantissa and an exponent. So, let's
C     start building the number. We need to be careful that we
C     do not overflow when we scale the number using the exponent.
C
C     First, we build up the mantissa ...
C
      IF ( NEGTIV ) THEN

         DO WHILE ( NDIGIT .GT. 0 )

            TMPNUM = TMPNUM - DPVAL(IVAL(NDIGIT)) * SCALES(NDIGIT)
            NDIGIT = NDIGIT - 1

         END DO

      ELSE

         DO WHILE ( NDIGIT .GT. 0 )

            TMPNUM = TMPNUM + DPVAL(IVAL(NDIGIT)) * SCALES(NDIGIT)
            NDIGIT = NDIGIT - 1

         END DO

      END IF
C
C     At this point, one of the following is true:
C
C        1)  -1     <  TMPNUM <= -1/BASE
C
C        2)  1/BASE <= TMPNUM <  1
C
C     or
C
C        3) TMPNUM = 0.0D0
C
C     Now we to scale the normalized number using the exponent. If
C     the exponent is zero, we will simply fall through the loop
C     structures below at no greater cost than a few comparisons.
C
      IF ( IEXPON .LT. 0 ) THEN
C
C        We do not check for any sort of underflow conditions.
C
         DO I = 1, -IEXPON

            TMPNUM = TMPNUM * INVBAS

         END DO

      ELSE

         IF ( NEGTIV ) THEN

            DO I = 1, IEXPON

               IF ( TMPNUM .GE. MINDP ) THEN

                  TMPNUM = TMPNUM * BASE

               ELSE

                  ERROR  = .TRUE.
                  ERRMSG = 'ERROR: Number is too small to be'//
     .                     ' represented.'
                  RETURN

               END IF

            END DO

         ELSE

            DO I = 1, IEXPON

               IF ( TMPNUM .LE. MAXDP ) THEN

                  TMPNUM = TMPNUM * BASE

               ELSE

                  ERROR  = .TRUE.
                  ERRMSG = 'ERROR: Number is too large to be'//
     .                     ' represented.'
                  RETURN

               END IF

            END DO

         END IF

      END IF
C
C     If we got to here, we have successfully parsed the hexadecimal
C     string into a double precision number. So, set the value and
C     return.
C
      NUMBER = TMPNUM

      RETURN
      END
