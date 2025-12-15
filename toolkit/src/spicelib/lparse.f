C$Procedure LPARSE ( Parse items from a list )

      SUBROUTINE LPARSE ( LIST, DELIM, NMAX, N, ITEMS )

C$ Abstract
C
C     Parse a list of items delimited by a single character.
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
C     CHARACTER
C     LIST
C     PARSING
C     STRING
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         LIST
      CHARACTER*(1)         DELIM
      INTEGER               NMAX
      INTEGER               N
      CHARACTER*(*)         ITEMS ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LIST       I   List of items delimited by DELIM.
C     DELIM      I   Single character used to delimit items.
C     NMAX       I   Maximum number of items to return.
C     N          O   Number of items in the list.
C     ITEMS      O   Items in the list, left justified.
C
C$ Detailed_Input
C
C     LIST     is a list of items delimited by the single character
C              DELIM. Consecutive delimiters, and delimiters at the
C              beginning and end of the list, are considered to
C              delimit blank items. A blank list is considered to
C              contain a single (blank) item.
C
C     DELIM    is the character delimiting the items in the list.
C              This may be any ASCII character, including a blank.
C              However, by definition, consecutive blanks are NOT
C              considered to be consecutive delimiters. In addition,
C              leading and trailing blanks are ignored.
C
C     NMAX     is the maximum number of items to be returned from
C              the list. This allows the user to guard against
C              overflow from a list containing more items than
C              expected.
C
C$ Detailed_Output
C
C     N        is the number of items in the list. N may be
C              any number between one and NMAX. N is always the
C              number of delimiters plus one.
C
C     ITEMS    are the items in the list, left justified. Any item
C              in the list too long to fit into an element of ITEMS
C              is truncated on the right.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the string length of ITEMS is too short to accommodate
C         an item, the item will be truncated on the right.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Parse a character string to retrieve the words contained
C        within.
C
C        Example code begins here.
C
C
C              PROGRAM LPARSE_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 NMAX
C              PARAMETER             ( NMAX   = 25  )
C
C              INTEGER                 STRLEN
C              PARAMETER             ( STRLEN = 255 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(1)           DELIM
C              CHARACTER*(STRLEN)      ITEMS  ( NMAX )
C              CHARACTER*(STRLEN)      LIST
C
C              INTEGER                 I
C              INTEGER                 N
C
C        C
C        C     Define the list of delimited items.
C        C
C        C     Think of a sentence as a list delimited by a space.
C        C     DELIM is assigned to a space.
C        C
C              LIST  = 'Run and find out.'
C              DELIM = ' '
C
C        C
C        C     Parse the items from LIST.
C        C
C              CALL LPARSE ( LIST, DELIM, NMAX, N, ITEMS )
C
C        C
C        C     Output the ITEMS.
C        C
C              DO I = 1, N
C
C                 WRITE(*,'(A,I3,2A)') 'Item', I, ': ', ITEMS(I)
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
C        Item  1: Run
C        Item  2: and
C        Item  3: find
C        Item  4: out.
C
C
C     2) Repeat the previous example with different character
C        delimiting the items in the list and different maximum number
C        of items to return.
C
C        Example code begins here.
C
C
C              PROGRAM LPARSE_EX2
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              INTEGER                 RTRIM
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 NCASES
C              PARAMETER             ( NCASES = 2   )
C
C              INTEGER                 NMAXT
C              PARAMETER             ( NMAXT  = 25  )
C
C              INTEGER                 STRLEN
C              PARAMETER             ( STRLEN = 255 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(1)           DELIM  ( NCASES )
C              CHARACTER*(STRLEN)      ITEMS  ( NMAXT  )
C              CHARACTER*(STRLEN)      LIST   ( NCASES )
C
C              INTEGER                 I
C              INTEGER                 J
C              INTEGER                 N
C              INTEGER                 NMAX   ( NCASES )
C
C        C
C        C     Define the lists of delimited items, the delimiting
C        C     character and the maximum number of items to return.
C        C
C              LIST(1)  = '//option1//option2/ //'
C              DELIM(1) = '/'
C              NMAX(1)  = 20
C
C              LIST(2)  = ' ,bob,   carol,, ted,  alice'
C              DELIM(2) = ','
C              NMAX(2)  = 4
C
C              DO I = 1, NCASES
C
C                 WRITE(*,'(A,I2,A)') 'Case', I, ':'
C                 WRITE(*,'(3A)')   '   String: ''',
C             .                     LIST(I)(:RTRIM(LIST(I))), ''''
C                 WRITE(*,'(3A)')   '   DELIM : ''', DELIM(I), ''''
C                 WRITE(*,'(A,I3)') '   NMAX  :', NMAX(I)
C                 WRITE(*,'(A)')    '   Output items:'
C
C        C
C        C        Parse the items from LIST.
C        C
C                 CALL LPARSE ( LIST(I), DELIM(I), NMAX(I), N, ITEMS )
C
C        C
C        C        Output the ITEMS.
C        C
C                 DO J = 1, N
C
C                    WRITE(*,'(A,I3,3A)') '      Item', J, ': ''',
C             .                  ITEMS(J)(:RTRIM(ITEMS(J))), ''''
C
C                 END DO
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
C        Case 1:
C           String: '//option1//option2/ //'
C           DELIM : '/'
C           NMAX  : 20
C           Output items:
C              Item  1: ' '
C              Item  2: ' '
C              Item  3: 'option1'
C              Item  4: ' '
C              Item  5: 'option2'
C              Item  6: ' '
C              Item  7: ' '
C              Item  8: ' '
C        Case 2:
C           String: ' ,bob,   carol,, ted,  alice'
C           DELIM : ','
C           NMAX  :  4
C           Output items:
C              Item  1: ' '
C              Item  2: 'bob'
C              Item  3: 'carol'
C              Item  4: ' '
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
C     H.A. Neilan        (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary entries from $Revisions section.
C
C        Added complete code example.
C
C-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
C
C        Bug fix: code was modified to avoid out-of-range
C        substring bound conditions.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB)
C
C-&


C$ Index_Entries
C
C     parse items from a list
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
C
C        Bug fix: code was modified to avoid out-of-range
C        substring bound conditions. The previous version
C        of this routine used DO WHILE statements of the form
C
C                  DO WHILE (      ( B         .LE. EOL   )
C           .                .AND. ( LIST(B:B) .EQ. BLANK ) )
C
C        Such statements can cause index range violations when the
C        index B is greater than the length of the string LIST.
C        Whether or not such violations occur is platform-dependent.
C
C-&


C
C     Local parameters
C
      CHARACTER*(1)         BLANK
      PARAMETER           ( BLANK = ' ' )

      INTEGER               ISPACE
      PARAMETER           ( ISPACE = 32 )

C
C     Local variables
C
      CHARACTER*(1)         BCHR
      CHARACTER*(1)         ECHR

      INTEGER               B
      INTEGER               E
      INTEGER               EOL


C
C     Because speed is essential in many list parsing applications,
C     LPARSE parses the input list in a single pass.
C

C
C     Nothing yet.
C
      N = 0

C
C     Blank list contains a blank item.
C
      IF ( LIST .EQ. BLANK ) THEN

         N        = 1
         ITEMS(1) = BLANK

      ELSE
C
C        Eliminate trailing blanks. EOL is the last non-blank
C        character in the list.
C
         EOL = LEN ( LIST )

         DO WHILE ( LIST(EOL:EOL) .EQ. BLANK )

            EOL = EOL - 1

         END DO

C
C        As the king said to Alice: 'Begin at the beginning.
C        Continue until you reach the end. Then stop.'
C
C        When searching for items, B is the beginning of the current
C        item; E is the end.  E points to the next non-blank delimiter,
C        if any; otherwise E points to either the last character
C        preceding the next item, or to the last character of the list.
C
         B = 1

         DO WHILE ( B .LE. EOL )
C
C           Skip any blanks before the next item or delimiter.
C
C           At this point in the loop, we know
C
C              B <= EOL
C
            BCHR = LIST(B:B)

            DO WHILE (       ( B           .LE. EOL    )
     .                 .AND. ( ICHAR(BCHR) .EQ. ISPACE ) )

               B = B + 1

               IF ( B .LE. EOL ) THEN
                  BCHR = LIST(B:B)
               END IF

            END DO

C
C           At this point B is the index of the next non-blank
C           character BCHR, or else
C
C              B == EOL + 1
C
C           The item ends at the next delimiter.
C
            E = B

            IF ( E .LE. EOL ) THEN
               ECHR = LIST(E:E)
            ELSE
               ECHR = BLANK
            END IF

            DO WHILE (       (  E    .LE. EOL   )
     .                 .AND. (  ECHR .NE. DELIM )  )

               E = E + 1

               IF ( E .LE. EOL ) THEN
                  ECHR = LIST(E:E)
               END IF

            END DO

C
C           The item now lies between B and E. Unless, of course, B and
C           E are the same character; this can happen if the list
C           starts or ends with a non-blank delimiter, or if we have
C           stumbled upon consecutive delimiters.
C
            N = N + 1

            IF ( E .GT. B ) THEN
               ITEMS(N) = LIST(B:E-1)
            ELSE
               ITEMS(N) = BLANK
            END IF

C
C           If there are more items to be found, continue with
C           character following E (which is a delimiter).
C
            IF ( N .LT. NMAX ) THEN
               B = E + 1
            ELSE
               RETURN
            END IF

         END DO

C
C        If the list ended with a (non-blank) delimiter, add a blank
C        item to the end.
C
         IF ( ( LIST(EOL:EOL) .EQ. DELIM ) .AND. ( N .LT. NMAX ) ) THEN

            N        = N + 1
            ITEMS(N) = BLANK

         END IF

      END IF

      RETURN
      END
