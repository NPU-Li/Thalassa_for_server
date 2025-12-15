C$Procedure NEXTWD ( Next word in a character string )

      SUBROUTINE NEXTWD ( STRING, NEXT, REST )

C$ Abstract
C
C     Return the next word in a given character string, and
C     left justify the rest of the string.
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
C     WORD
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)     STRING
      CHARACTER*(*)     NEXT
      CHARACTER*(*)     REST

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   Input character string.
C     NEXT       O   The next word in the string.
C     REST       O   The remaining part of STRING, left-justified.
C
C$ Detailed_Input
C
C     STRING   is the input string to be parsed. Each word of this
C              string is a maximal sequence of consecutive non-blank
C              characters.
C
C$ Detailed_Output
C
C     NEXT     is the first word in STRING. It is called the "next" word
C              because NEXTWD is typically called repeatedly to find the
C              words of the input string in left-to-right order. A word
C              is a maximal sequence of consecutive non-blank
C              characters. NEXT is always returned left-justified.
C
C              If STRING is blank, NEXT is blank.
C
C              NEXT may NOT overwrite STRING.
C
C     REST     is the remaining part of STRING, left-justified after the
C              removal of NEXT.
C
C              REST may overwrite STRING.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the declared lengths of NEXT and REST are not large enough
C         to hold the output strings, they are truncated on the right.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     NEXTWD is used primarily for parsing input commands consisting
C     of one or more words, where a word is defined to be any sequence
C     of consecutive non-blank characters. Successive calls to NEXTWD,
C     each using the previous value of REST as the input string, allow
C     the calling routine to neatly parse and process one word at a
C     time.
C
C     NEXTWD cuts the input string into two pieces, and returns them
C     separately. The first piece is the first word in the string.
C     (Leading blanks are ignored. The first word, which is returned in
C     the output argument NEXT, runs from the first non-blank character
C     in the string up to the first blank that follows it.) The second
C     piece is whatever is left after the first word is removed. The
C     second piece is left justified, to simplify later calls to NEXTWD.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a character string, get the sequence of words within.
C
C
C        Example code begins here.
C
C
C              PROGRAM NEXTWD_EX1
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
C              INTEGER               LINZS
C              PARAMETER           ( LINZS = 47 )
C
C              INTEGER               WRDSZ
C              PARAMETER           ( WRDSZ = 5  )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(WRDSZ)     NEXT
C              CHARACTER*(LINZS)     REST
C              CHARACTER*(LINZS)     STRING
C
C              REST = '  Now is the time,  for all good men to come.'
C
C              WRITE(*,'(A)') 'Next   Rest of the string'
C              WRITE(*,'(A)') '-----  ---'
C             .            // '---------------------------------------'
C
C              DO WHILE ( .NOT. EQSTR ( REST, ' ' ) )
C
C                 STRING = REST
C                 CALL NEXTWD ( STRING, NEXT, REST )
C
C                 WRITE(*,'(A5,2X,A)') NEXT, REST
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
C        Next   Rest of the string
C        -----  ------------------------------------------
C        Now    is the time,  for all good men   to come.
C        is     the time,  for all good men   to come.
C        the    time,  for all good men   to come.
C        time,  for all good men   to come.
C        for    all good men   to come.
C        all    good men   to come.
C        good   men   to come.
C        men    to come.
C        to     come.
C        come.
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
C     K.R. Gehringer     (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 19-MAY-2021 (JDR) (NJB)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on the existing fragment.
C
C        Updated header documentation. Added entry #1 in $Exceptions
C        section.
C
C-    SPICELIB Version 1.2.0, 04-APR-1996 (KRG)
C
C        Fixed a problem that could occur when STRING and REST are
C        the same character string. Simplified the algorithm a bit
C        while I was at it.
C
C        Single character comparisons now make use of ICHAR to
C        perform the comparisons as integers for speed.
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
C     next word in a character_string
C
C-&


C
C     Local Parameters
C
      INTEGER               ISPACE
      PARAMETER           ( ISPACE = 32 )

C
C     Local variables
C
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               I

      LOGICAL               INWORD

C
C     The trivial case.
C
      IF ( STRING .EQ. ' ' ) THEN

         NEXT = ' '
         REST = ' '

C
C     The non-trivial case.
C
      ELSE
C
C        Get the length of the string.
C
         END = LEN ( STRING )

C
C        Skip leading blanks and set flags indicating that we are
C        not in a word and that we do not have a word.
C
         BEGIN  = 1
         INWORD = .FALSE.
C
C        We know the string is not blank, so we will eventually
C        get to a word, thus no need to check against END here.
C
         DO WHILE ( .NOT. INWORD )
            IF ( ICHAR( STRING(BEGIN:BEGIN) ) .EQ. ISPACE ) THEN
               BEGIN = BEGIN + 1
            ELSE
               INWORD = .TRUE.
            END IF
         END DO

C
C        We are now in a word. Step through the input string until the
C        next blank is encountered or until the end of the string is
C        found. We start at BEGIN even though we know from above that
C        STRING(BEGIN:BEGIN) is not blank; this allows us to deal
C        cleanly with the case where the string is a single character
C        long and not blank (because we're in that case).
C
         I = BEGIN
         DO WHILE ( INWORD )
            IF ( ICHAR ( STRING(I:I) ) .NE. ISPACE ) THEN
               I = I + 1
               IF ( I .GT. END ) THEN
                  I      = I - 1
                  INWORD = .FALSE.
               END IF
            ELSE
               I      = I - 1
               INWORD = .FALSE.
            END IF
         END DO

C
C        Our word is the substring between BEGIN and I. Note that I
C        might be equal to END, so we have to be careful about setting
C        the REST. We also left justify REST as we set it. LJUST does
C        the right thing if STRING and REST overlap. If we do not have
C        a word, the NEXT and REST are both blank.
C
         NEXT = STRING(BEGIN:I)

         IF ( I .LT. END ) THEN
            CALL LJUST ( STRING(I+1:), REST )
         ELSE
            REST = ' '
         END IF

      END IF

      RETURN
      END
