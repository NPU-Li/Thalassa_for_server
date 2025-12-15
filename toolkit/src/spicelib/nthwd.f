C$Procedure NTHWD ( N'th word in a character string )

      SUBROUTINE NTHWD ( STRING, NTH, WORD, LOC )

C$ Abstract
C
C     Return the Nth word in a character string, and its location
C     in the string.
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
C     PARSING
C     SEARCH
C     WORD
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    STRING
      INTEGER          NTH
      CHARACTER*(*)    WORD
      INTEGER          LOC

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   Input character string.
C     NTH        I   Index of the word to be returned.
C     WORD       O   The NTH word in STRING.
C     LOC        O   Location of WORD in STRING.
C
C$ Detailed_Input
C
C     STRING   is the input string to be parsed. Each word of this
C              string is a maximal sequence of consecutive non-blank
C              characters.
C
C     NTH      is the index of the word to be returned. (One for the
C              first word, two for the second, and so on.)
C
C$ Detailed_Output
C
C     WORD     is the NTH word in STRING. If STRING is blank, or NTH is
C              non-positive or too large, WORD is blank.
C
C              WORD may overwrite STRING.
C
C     LOC      is the location of WORD in STRING. (That is, WORD begins
C              at STRING(LOC:LOC)). If STRING is blank, or NTH is
C              non-positive or too large, LOC is zero.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the declared length of WORD is not large enough to contain
C         the NTH word in STRING, the word will be truncated on the
C         right.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     NTHWD, like NEXTWD, is useful primarily for parsing input commands
C     consisting of one or more words, where a word is defined to be a
C     maximal sequence of consecutive non-blank characters. Each word is
C     bounded on both sides by a blank character, or by the start or end
C     of the input string. Successive calls to NEXTWD allow the calling
C     routine to neatly parse and process one word at a time.
C
C     The chief difference between the two routines is that
C     NTHWD allows the calling routine to access the words making
C     up the input string in random order. (NEXTWD allows only
C     sequential access.)
C
C     NTHWD may be more efficient than NEXTWD, since NTHWD doesn't
C     update an output string consisting of the remaining, unparsed
C     string.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a character string, get the N'th word within, and the
C        word's location.
C
C
C        Example code begins here.
C
C
C              PROGRAM NTHWD_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              INTEGER               RTRIM
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         STRING
C              PARAMETER           ( STRING = ' Now is the time,   '
C             .                  //  'for all good men     to come.' )
C
C              CHARACTER*(*)         FMT
C              PARAMETER           ( FMT = '(A,I3,3A,I3)' )
C
C              INTEGER               WRDSZ
C              PARAMETER           ( WRDSZ = 5 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(WRDSZ)     WORD
C              INTEGER               LOC
C              INTEGER               NTH
C
C
C              DO NTH = -1, 11
C
C                 CALL NTHWD ( STRING, NTH, WORD, LOC )
C                 WRITE(*,FMT) 'Word #', NTH, '  is <',
C             .                WORD(:RTRIM(WORD)),
C             .                '>, starting at position', LOC
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
C        Word # -1  is < >, starting at position  0
C        Word #  0  is < >, starting at position  0
C        Word #  1  is <Now>, starting at position  2
C        Word #  2  is <is>, starting at position  6
C        Word #  3  is <the>, starting at position  9
C        Word #  4  is <time,>, starting at position 13
C        Word #  5  is <for>, starting at position 21
C        Word #  6  is <all>, starting at position 25
C        Word #  7  is <good>, starting at position 29
C        Word #  8  is <men>, starting at position 34
C        Word #  9  is <to>, starting at position 42
C        Word # 10  is <come.>, starting at position 45
C        Word # 11  is < >, starting at position  0
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
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 26-OCT-2021 (NJB) (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on the existing fragment.
C
C        Updated header documentation. Added entry #1 in $Exceptions
C        section.
C
C-    SPICELIB Version 1.1.0, 10-MAY-2006 (EDW)
C
C        Added logic to prevent the evaluation of STRING(I:I)
C        if I exceeds the length of STRING. Functionally, the
C        evaluation had no effect on NTHWD's output, but the ifort
C        F95 compiler flagged the evaluation as an array
C        overrun error. This occurred because given:
C
C           A .AND. B
C
C        ifort evaluates A then B then performs the logical
C        comparison.
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
C     N'th word in a character_string
C
C-&


C
C     Local variables
C
      INTEGER          N
      INTEGER          I
      INTEGER          LENGTH

      LOGICAL          LOOP



C
C     Trivial cases first. Blank STRING? Nonpositive NTH?
C
      IF ( STRING .EQ. ' ' .OR. NTH .LT. 1 ) THEN

         WORD = ' '
         LOC  =  0
         RETURN

      END IF

C
C     Skip leading blanks.
C
      LOC = 1

      DO WHILE ( STRING(LOC:LOC) .EQ. ' ' )
         LOC = LOC + 1
      END DO

C
C     If we wanted the first word, we have the location. Otherwise,
C     keep stepping through STRING. Quit when the N'TH word is found,
C     or when the end of the string is reached. (The current word is
C     ended whenever a blank is encountered.)
C
C     N is the number of words found so far.
C     I is the current location in STRING.
C
      N      = 1
      I      = LOC
      LENGTH = LEN ( STRING )

      DO WHILE ( I .LT. LENGTH .AND. N .LT. NTH )

         I = I + 1

C
C        Blank signals end of the current word.
C
         IF ( STRING(I:I) .EQ. ' ' ) THEN

C
C           Skip ahead to the next one.  The logic ensures no
C           evaluation of STRING(I:I) if I > LEN(STRING).
C
            LOOP = I .LE. LENGTH
            IF( LOOP ) THEN
               LOOP = LOOP .AND. STRING(I:I) .EQ. ' '
            END IF

            DO WHILE ( LOOP )
               I = I + 1

               IF( I .GT. LENGTH ) THEN
                  LOOP = .FALSE.
               ELSE IF ( STRING(I:I) .NE. ' ' ) THEN
                  LOOP = .FALSE.
               ELSE
                  LOOP = .TRUE.
               END IF

            END DO

C
C           If not at the end of the string, we have another word.
C
            IF ( I .LE. LENGTH ) THEN
               N   = N + 1
               LOC = I
            END IF

         END IF

      END DO

C
C     Couldn't find enough words? Return blank and zero.
C
      IF ( N .LT. NTH ) THEN
         WORD = ' '
         LOC  =  0

C
C     Otherwise, find the rest of WORD (it continues until the next
C     blank), and return the current LOC.
C
      ELSE
         I = INDEX ( STRING(LOC: ), ' ' )

         IF ( I .EQ. 0 ) THEN
            WORD = STRING(LOC: )
         ELSE
            WORD = STRING(LOC:LOC+I-1)
         END IF
      END IF

      RETURN
      END
