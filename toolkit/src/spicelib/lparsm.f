C$Procedure LPARSM ( Parse a list of items )

      SUBROUTINE LPARSM ( LIST, DELIMS, NMAX, N, ITEMS )

C$ Abstract
C
C     Parse a list of items separated by multiple delimiters.
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

      CHARACTER*(*)    LIST
      CHARACTER*(*)    DELIMS
      INTEGER          NMAX
      INTEGER          N
      CHARACTER*(*)    ITEMS ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LIST       I    List of items delimited by DELIMS.
C     DELIMS     I    Single characters which delimit items.
C     NMAX       I    Maximum number of items to return.
C     N          O    Number of items in the list.
C     ITEMS      O    Items in the list, left justified.
C
C$ Detailed_Input
C
C     LIST     is a list of items delimited by any one of the
C              characters in the string DELIMS. Consecutive
C              delimiters, and delimiters at the beginning and
C              end of the list, are considered to delimit blank
C              items. A blank list is considered to contain
C              a single (blank) item.
C
C     DELIMS   contains the individual characters which delimit
C              the items in the list. These may be any ASCII
C              characters, including blanks.
C
C              However, by definition, consecutive blanks are NOT
C              considered to be consecutive delimiters. Nor are
C              a blank and any other delimiter considered to be
C              consecutive delimiters. In addition, leading and
C              trailing blanks are ignored.
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
C              in the list to long to fit into an element of ITEMS
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
C
C        Example code begins here.
C
C
C              PROGRAM LPARSM_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 DELMLN
C              PARAMETER             ( DELMLN = 1   )
C
C              INTEGER                 NMAX
C              PARAMETER             ( NMAX   = 25  )
C
C              INTEGER                 STRLEN
C              PARAMETER             ( STRLEN = 255 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(DELMLN)      DELIMS
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
C        C     DELIMS is assigned to a space.
C        C
C              LIST   = 'Run and find out.'
C              DELIMS = ' '
C
C        C
C        C     Parse the items from LIST.
C        C
C              CALL LPARSM ( LIST, DELIMS, NMAX, N, ITEMS )
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
C     2) Parse a character string to retrieve the items contained
C        within, when then items are separated by multiple delimiters.
C
C
C        Example code begins here.
C
C
C              PROGRAM LPARSM_EX2
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
C              INTEGER                 DELMLN
C              PARAMETER             ( DELMLN = 5   )
C
C              INTEGER                 NMAX
C              PARAMETER             ( NMAX   = 25  )
C
C              INTEGER                 STRLEN
C              PARAMETER             ( STRLEN = 255 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(DELMLN)      DELIMS
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
C        C     DELIMS is assigned to a space.
C        C
C              LIST   = '  1986-187// 13:15:12.184 '
C              DELIMS = ' ,/-:'
C
C        C
C        C     Parse the items from LIST.
C        C
C              CALL LPARSM ( LIST, DELIMS, NMAX, N, ITEMS )
C
C        C
C        C     Output the ITEMS.
C        C
C              DO I = 1, N
C
C                 WRITE(*,'(A,I3,3A)') 'Item', I, ': ''',
C             .                        ITEMS(I)(:RTRIM(ITEMS(I))), ''''
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
C        Item  1: '1986'
C        Item  2: '187'
C        Item  3: ' '
C        Item  4: '13'
C        Item  5: '15'
C        Item  6: '12.184'
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
C-    SPICELIB Version 1.1.1, 13-AUG-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
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
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     parse a list of items
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
C     LPARSM parses the input list in a single pass. What follows
C     is nearly identical to LPARSE, except the Fortran INDEX function
C     is used to test for delimiters, instead of testing each character
C     for simple equality.
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

         DO WHILE ( ICHAR(LIST(EOL:EOL)) .EQ. ISPACE )
            EOL = EOL - 1
         END DO

C
C        As the King said to Alice: 'Begin at the beginning.
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

            DO WHILE (       (  E                     .LE. EOL )
     .                 .AND. (  INDEX( DELIMS, ECHR ) .EQ. 0   )  )

               E = E + 1

               IF ( E .LE. EOL ) THEN
                  ECHR = LIST(E:E)
               END IF

            END DO

C
C           (This is different from LPARSE. If the delimiter was
C           a blank, find the next non-blank character. If it's not
C           a delimiter, back up. This prevents constructions
C           like 'a , b', where the delimiters are blank and comma,
C           from being interpreted as three items instead of two.
C           By definition, consecutive blanks, or a blank and any
C           other delimiter, do not count as consecutive delimiters.)
C
            IF (       ( E           .LE. EOL    )
     .           .AND. ( ICHAR(ECHR) .EQ. ISPACE ) ) THEN
C
C              Find the next non-blank character.
C
               DO WHILE (       ( E           .LE. EOL    )
     .                    .AND. ( ICHAR(ECHR) .EQ. ISPACE )  )

                  E = E + 1

                  IF ( E .LE. EOL ) THEN
                     ECHR = LIST(E:E)
                  END IF

               END DO

               IF ( E .LE. EOL ) THEN

                  IF (  INDEX( DELIMS, ECHR ) .EQ. 0  ) THEN
C
C                    We're looking at a non-delimiter character.
C
C                    E is guaranteed to be > 1 if we're here, so the
C                    following subtraction is valid.
C
                     E = E - 1

                  END IF

               END IF

            END IF

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
C        If the list ended with a (non-blank) delimiter, add a
C        blank item to the end.
C
         IF (       ( INDEX( DELIMS, LIST(EOL:EOL) ) .NE. 0    )
     .        .AND. (  N                             .LT. NMAX )  ) THEN

            N        = N + 1
            ITEMS(N) = BLANK

         END IF

      END IF

      RETURN
      END
