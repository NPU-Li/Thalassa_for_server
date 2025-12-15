C$Procedure EQSTR ( Equivalent strings )

      LOGICAL FUNCTION EQSTR ( A, B )

C$ Abstract
C
C     Determine whether two strings are equivalent.
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
C     ASCII
C     CHARACTER
C     COMPARE
C     PARSING
C     SEARCH
C     STRING
C     TEXT
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         A
      CHARACTER*(*)         B

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     A,
C     B          I   Arbitrary character strings.
C
C     The function returns .TRUE. if A and B are equivalent.
C
C$ Detailed_Input
C
C     A,
C     B        are arbitrary character strings.
C
C$ Detailed_Output
C
C     The function returns .TRUE. if A and B are equivalent: that is,
C     if A and B contain  the same characters in the same order,
C     when blanks are ignored and uppercase and lowercase characters
C     are considered equal.
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
C     This routine is provided for those cases in which two strings
C     must be compared, and in which allowances are to be made for
C     extra (leading, trailing, and embedded) blanks and differences
C     in case. For the most part,
C
C        EQSTR ( A, B )
C
C     is .TRUE. whenever
C
C        CALL CMPRSS ( ' ', 0, A, TEMPA )
C        CALL UCASE  (            TEMPA, TEMPA )
C
C        CALL CMPRSS ( ' ', 0, B, TEMPB )
C        CALL UCASE  (            TEMPB, TEMPB )
C
C        EQVLNT = TEMPA .EQ. TEMPB
C
C     is .TRUE. There are two important differences, however.
C
C        1) The single reference to EQSTR is much simpler to
C           write, and simpler to understand.
C
C        2) The reference to EQSTR does not require any temporary
C           storage, nor does it require that the strings A and B
C           be changed. This feature is especially useful when
C           comparing strings received as subprogram arguments
C           against strings stored internally within the subprogram.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) This code provides examples of equivalent and non-equivalent
C        strings according to the algorithm implemented in EQSTR.
C
C        Example code begins here.
C
C
C              PROGRAM EQSTR_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              LOGICAL               EQSTR
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 9  )
C
C              INTEGER               STRLEN
C              PARAMETER           ( STRLEN = 22 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(STRLEN)    STR1   ( SETSIZ )
C              CHARACTER*(STRLEN)    STR2   ( SETSIZ )
C
C              INTEGER               I
C
C        C
C        C     Initialize the two arrays of strings.
C        C
C              DATA                  STR1   / 'A short string   ',
C             .                               'Embedded        blanks',
C             .                               'Embedded        blanks',
C             .                               ' ',
C             .                               'One word left out',
C             .                               'Extra [] delimiters',
C             .                               'Testing 1, 2, 3',
C             .                               'Case insensitive',
C             .                               'Steve'  /
C
C              DATA                  STR2   / 'ashortstring',
C             .                               'Em be dd ed bl an ks',
C             .                               '   Embeddedblanks',
C             .                               '          ',
C             .                               'WORD LEFT OUT',
C             .                               'extradelimiters',
C             .                               'TESTING123',
C             .                               'Case Insensitive',
C             .                               '  S t E v E  '  /
C
C
C        C
C        C     Compare the two arrays.
C        C
C              DO I = 1, SETSIZ
C
C                 WRITE(*,*)
C                 WRITE(*,*) 'STR1 : ', STR1(I)
C                 WRITE(*,*) 'STR2 : ', STR2(I)
C
C                 IF ( EQSTR( STR1(I), STR2(I) ) ) THEN
C
C                    WRITE(*,*) 'EQSTR: equivalent.'
C
C                 ELSE
C
C                    WRITE(*,*) 'EQSTR: NOT equivalent.'
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
C         STR1 : A short string
C         STR2 : ashortstring
C         EQSTR: equivalent.
C
C         STR1 : Embedded        blanks
C         STR2 : Em be dd ed bl an ks
C         EQSTR: equivalent.
C
C         STR1 : Embedded        blanks
C         STR2 :    Embeddedblanks
C         EQSTR: equivalent.
C
C         STR1 :
C         STR2 :
C         EQSTR: equivalent.
C
C         STR1 : One word left out
C         STR2 : WORD LEFT OUT
C         EQSTR: NOT equivalent.
C
C         STR1 : Extra [] delimiters
C         STR2 : extradelimiters
C         EQSTR: NOT equivalent.
C
C         STR1 : Testing 1, 2, 3
C         STR2 : TESTING123
C         EQSTR: NOT equivalent.
C
C         STR1 : Case insensitive
C         STR2 : Case Insensitive
C         EQSTR: equivalent.
C
C         STR1 : Steve
C         STR2 :   S t E v E
C         EQSTR: equivalent.
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
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing example fragments.
C
C-    SPICELIB Version 1.2.0, 03-AUG-1994 (NJB)
C
C        Code changed to eliminate DO WHILE ( .TRUE. ) construct.
C        The purpose of the change was to eliminate compilation
C        diagnostics relating to unreachable statements. The code
C        ran just fine before this change.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 10-MAY-1990 (NJB)
C
C        Loop termination condition fixed.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     equivalent strings
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.2.0, 03-AUG-1994 (NJB)
C
C        Code changed to eliminate DO WHILE ( .TRUE. ) construct.
C        The purpose of the change was to eliminate compilation
C        diagnostics relating to unreachable statements.
C
C        Changed some statements of form
C
C           IF <condition> <statement>
C
C        to
C
C           IF <condition> THEN
C
C             <statement>
C
C           END IF
C
C-    SPICELIB Version 1.1.0, 10-MAY-1990 (NJB)
C
C        Loop termination condition fixed. The routine now checks
C        the termination case where both string pointers are pointing
C        to blanks, and at least one pointer has a value greater than
C        the length of the string it corresponds to. Internal comments
C        were updated accordingly.
C
C-&


C
C     Local variables
C
      INTEGER               PA
      INTEGER               PB

      INTEGER               LENA
      INTEGER               LENB

      INTEGER               CA
      INTEGER               CB

      INTEGER               LBOUND
      INTEGER               UBOUND
      INTEGER               DELTA

      LOGICAL               DONE


C
C     The general plan is to move a pair of pointers (PA, PB)
C     through strings A and B, skipping blank characters and
C     comparing others one-for-one.
C
C        Repeat:
C
C           If (A is blank) then
C              Increment A
C
C           Else if (B is blank) then
C              Increment B
C
C           Else
C              If (A and B are equivalent) then
C                 Increment A and B
C              Else
C                 Return .FALSE.
C
C           If (A and B are past end) then
C              Return .TRUE.
C
C           Else if (A or B is past end and other is non-blank) then
C              Return .FALSE.
C
C           Else if (A or B is past end and other is blank) then
C              Return .TRUE.
C
C     Note that no pointer gets incremented more than once on each
C     pass through the loop.
C
C     On the other hand, in many cases the strings will be exactly
C     equal. If so, why knock ourselves out?
C
      IF ( A .EQ. B ) THEN

         EQSTR  = .TRUE.
         RETURN

      ELSE

         PA = 1
         PB = 1

         LENA = LEN ( A )
         LENB = LEN ( B )

         LBOUND = ICHAR ( 'a' )
         UBOUND = ICHAR ( 'z' )
         DELTA  = ICHAR ( 'A' ) - ICHAR ( 'a' )

         DONE   = .FALSE.

         DO WHILE ( .NOT. DONE )

C
C           At this point, we're guaranteed that
C
C             ( PA .LE. LENA )   and   ( PB .LE. LENB )
C

            IF ( A(PA:PA) .EQ. ' ' ) THEN
               PA = PA + 1

            ELSE IF ( B(PB:PB) .EQ. ' ' ) THEN
               PB = PB + 1

            ELSE

               CA = ICHAR ( A(PA:PA) )
               CB = ICHAR ( B(PB:PB) )

               IF ( CA .GE. LBOUND  .AND.  CA .LE. UBOUND ) THEN
                  CA = CA + DELTA
               END IF

               IF ( CB .GE. LBOUND  .AND.  CB .LE. UBOUND ) THEN
                  CB = CB + DELTA
               END IF

               IF ( CA .EQ. CB ) THEN

                  PA = PA + 1
                  PB = PB + 1

               ELSE

                  EQSTR  = .FALSE.
                  DONE   = .TRUE.
C
C                 We'll return from this point, having taken no further
C                 action.
C
               END IF

            END IF


            IF ( .NOT. DONE ) THEN


               IF ( PA .GT. LENA ) THEN
C
C                 Whichever of the following tests passes, we're going
C                 to have a verdict at the end of the IF block below.
C
                  IF ( PB .GT. LENB ) THEN
                     EQSTR  = .TRUE.

                  ELSE IF ( B(PB: ) .NE. ' ' ) THEN
                     EQSTR  = .FALSE.

                  ELSE
                     EQSTR  = .TRUE.

                  END IF

                  DONE  =  .TRUE.
C
C                 We'll return from this point, having taken no further
C                 action.
C

               ELSE IF ( PB .GT. LENB ) THEN
C
C                 Whichever of the following tests passes, we're going
C                 to have a verdict at the end of the IF block below.
C
                  IF ( A(PA: ) .NE. ' ' ) THEN
                     EQSTR  = .FALSE.
                  ELSE
                     EQSTR  = .TRUE.
                  END IF

                  DONE  =  .TRUE.
C
C                 We'll return from this point, having taken no further
C                 action.
C
               END IF


            END IF


         END DO


      END IF

      END
