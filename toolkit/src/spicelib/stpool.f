C$Procedure STPOOL ( String from pool )

      SUBROUTINE STPOOL ( ITEM, NTH, CONTIN, NTHSTR, SIZE, FOUND )

C$ Abstract
C
C     Retrieve the Nth string from a kernel pool variable, where the
C     string may be continued across several components of the kernel
C     pool variable.
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
C     POOL
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         ITEM
      INTEGER               NTH
      CHARACTER*(*)         CONTIN
      CHARACTER*(*)         NTHSTR
      INTEGER               SIZE
      LOGICAL               FOUND

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ITEM       I   Name of the kernel pool variable.
C     NTH        I   Index of the full string to retrieve.
C     CONTIN     I   Character sequence used to indicate continuation.
C     NTHSTR     O   A full string concatenated across continuations.
C     SIZE       O   The number of characters in the full string value.
C     FOUND      O   Flag indicating success or failure of request.
C
C$ Detailed_Input
C
C     ITEM     is the name of a kernel pool variable for which
C              the caller wants to retrieve a full (potentially
C              continued) string component.
C
C     NTH      is the number of the string to retrieve from the kernel
C              pool. The range of NTH is 1 to the number of full strings
C              that are present.
C
C     CONTIN   is a sequence of characters which (if they appear as the
C              last non-blank sequence of characters in a component of a
C              value of a kernel pool variable) act as a continuation
C              marker: the marker indicates that the string associated
C              with the component is continued into the next literal
C              component of the kernel pool variable.
C
C              If CONTIN is blank, all of the components of ITEM will be
C              retrieved as a single string.
C
C$ Detailed_Output
C
C     NTHSTR   is the NTH full string associated with the kernel pool
C              variable specified by ITEM.
C
C              Note that if NTHSTR is not sufficiently long to hold the
C              fully continued string, the value will be truncated. You
C              can determine if NTHSTR has been truncated by examining
C              the variable SIZE.
C
C     SIZE     is the index of last non-blank character of the continued
C              string as it is represented in the kernel pool. This is
C              the actual number of characters needed to hold the
C              requested string. If NTHSTR contains a truncated portion
C              of the full string, RTRIM(NTHSTR) will be less than SIZE.
C
C              If the value of NTHSTR should be a blank, then SIZE will
C              be set to 1.
C
C     FOUND    is a logical variable indicating success of the request
C              to retrieve the NTH string associated with ITEM. If an
C              Nth string exists, FOUND will be set to .TRUE.;
C              otherwise FOUND will be set to .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the variable specified by ITEM is not present in the
C         kernel pool or is present but is not character valued,
C         NTHSTR will be returned as a blank, SIZE will be
C         returned with the value 0 and FOUND will be set to .FALSE. In
C         particular if NTH is less than 1, NTHSTR will be returned as a
C         blank, SIZE will be zero and FOUND will be .FALSE.
C
C     2)  If the variable specified has a blank string associated
C         with its NTH full string, NTHSTR will be blank, SIZE
C         will be 1 and FOUND will be set to .TRUE.
C
C     3)  If NTHSTR is not long enough to hold all of the characters
C         associated with the NTH string, it will be truncated on the
C         right.
C
C     4)  If the continuation character is a blank, every component
C         of the variable specified by ITEM will be inserted into
C         the output string.
C
C     5)  If the continuation character is blank, then a blank component
C         of a variable is treated as a component with no letters.
C         For example:
C
C            STRINGS = ( 'This is a variable'
C                        'with a blank'
C                        ' '
C                        'component.' )
C
C         Is equivalent to
C
C
C            STRINGS = ( 'This is a variable'
C                        'with a blank'
C                        'component.' )
C
C         from the point of view of STPOOL if CONTIN is set to the
C         blank character.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The SPICE Kernel Pool provides a very convenient interface
C     for supplying both numeric and textual data to user application
C     programs. However, any particular component of a character
C     valued component of a kernel pool variable is limited to 80
C     or fewer characters in length.
C
C     This routine allows you to overcome this limitation by
C     "continuing" a character component of a kernel pool variable.
C     To do this you need to select a continuation sequence
C     of characters and then insert this sequence as the last non-blank
C     set of characters that make up the portion of the component
C     that should be continued.
C
C     For example, you may decide to use the sequence '//' to indicate
C     that a string should be continued to the next component of
C     a kernel pool variable. Then set up the
C     kernel pool variable as shown below
C
C        LONG_STRINGS = ( 'This is part of the first component //'
C                         'that needs more than one line when //'
C                         'inserting it into the kernel pool.'
C                         'This is the second string that is split //'
C                         'up as several components of a kernel pool //'
C                         'variable.' )
C
C     When loaded into the kernel pool, the variable LONG_STRINGS
C     will have six literal components:
C
C        COMPONENT (1) = 'This is part of the first component //'
C        COMPONENT (2) = 'that needs more than one line when //'
C        COMPONENT (3) = 'inserting it into the kernel pool.'
C        COMPONENT (4) = 'This is the second string that is split //'
C        COMPONENT (5) = 'up as several components of a kernel pool //'
C        COMPONENT (6) = 'variable.'
C
C     These are the components that would be retrieved by the call
C
C        CALL GCPOOL ( 'LONG_STRINGS', 1, 6, N, COMPONENT, FOUND )
C
C     However, using the routine STPOOL you can view the variable
C     LONG_STRINGS as having two long components.
C
C        STRGNA = 'This is part of the first component that '
C       . //      'needs more than one line when inserting '
C       . //      'it into the kernel pool. '
C
C        STRGNB = 'This is the second string that is split '
C       . //      'up as several components of a kernel pool '
C       . //      'variable. '
C
C
C     These string components would be retrieved by the following two
C     calls.
C
C        CALL STPOOL ( 'LONG_STRINGS', 1, '//', STRGNA, SIZE, FOUND )
C        CALL STPOOL ( 'LONG_STRINGS', 2, '//', STRGNB, SIZE, FOUND )
C
C$ Examples
C
C     Example 1. Retrieving file names.
C
C     Suppose a you have used the kernel pool as a mechanism for
C     specifying SPK files to load at startup but that the full
C     names of the files are too long to be contained in a single
C     text line of a kernel pool assignment.
C
C     By selecting an appropriate continuation character ('*' for
C     example)  you can insert the full names of the SPK files
C     into the kernel pool and then retrieve them using this
C     routine.
C
C     First set up the kernel pool specification of the strings
C     as shown here:
C
C           SPK_FILES = ( 'this_is_the_full_path_specification_*'
C                         'of_a_file_with_a_long_name'
C                         'this_is_the_full_path_specification_*'
C                         'of_a_second_file_with_a_very_long_*'
C                         'name' )
C
C     Now to retrieve and load the SPK_FILES one at a time,
C     exercise the following loop.
C
C     INTEGER               FILSIZ
C     PARAMETER           ( FILSIZ = 255 )
C
C     CHARACTER*(FILSIZ)    FILE
C     INTEGER               I
C
C     I = 1
C
C     CALL STPOOL ( 'SPK_FILES', I, '*', FILE, SIZE, FOUND )
C
C     DO WHILE ( FOUND .AND. RTRIM(FILE) .EQ. SIZE )
C
C        CALL SPKLEF ( FILE, HANDLE )
C        I = I + 1
C        CALL STPOOL ( 'SPK_FILES', I, '*', FILE, SIZE, FOUND )
C
C     END DO
C
C     IF ( FOUND .AND. RTRIM(FILE) .NE. SIZE ) THEN
C
C        WRITE (*,*) 'The ', I, '''th file name was too long.'
C
C     END IF
C
C
C     Example 2. Retrieving all components as a string.
C
C
C     Occasionally, it may be useful to retrieve the entire
C     contents of a kernel pool variable as a single string. To
C     do this you can use the blank character as the
C     continuation character. For example if you place the
C     following assignment in a text kernel
C
C         COMMENT = (  'This is a long note '
C                      ' about the intended '
C                      ' use of this text kernel that '
C                      ' can be retrieved at run time.' )
C
C     you can retrieve COMMENT as single string via the call below.
C
C        CALL STPOOL ( 'COMMENT', 1, ' ', COMMNT, SIZE, FOUND )
C
C     The result will be that COMMNT will have the following value.
C
C        COMMNT = 'This is a long note about the intended use of '
C       . //      'this text kernel that can be retrieved at run '
C       . //      'time. '
C
C     Note that the leading blanks of each component of COMMENT are
C     significant, trailing blanks are not significant.
C
C     If COMMENT had been set as
C
C         COMMENT = (  'This is a long note '
C                      'about the intended '
C                      'use of this text kernel that '
C                      'can be retrieved at run time.' )
C
C     Then the call to STPOOL above would have resulted in several
C     words being run together as shown below.
C
C
C        COMMNT = 'This is a long noteabout the intendeduse of '
C       . //      'this text kernel thatcan be retrieved at run '
C       . //      'time. '
C
C
C     resulted in several words being run together as shown below.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
C
C        Changed the output argument name "STRING" to "NTHSTR" for
C        consistency with other routines.
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 1.0.0, 11-JUL-1997 (WLT)
C
C-&


C$ Index_Entries
C
C     Retrieve a continued string value from the kernel pool
C
C-&


C     SPICELIB Variables
C
      INTEGER               RTRIM
      LOGICAL               RETURN


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(LNSIZE)    PART


      INTEGER               CFIRST
      INTEGER               CLAST
      INTEGER               COMP
      INTEGER               CSIZE
      INTEGER               N
      INTEGER               PUTAT
      INTEGER               ROOM
      INTEGER               STRNO

      LOGICAL               GOTIT
      LOGICAL               MORE

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      IF ( NTH .LT. 1 ) THEN
         FOUND  = .FALSE.
         NTHSTR = ' '
         SIZE   = 0
         RETURN
      END IF


      CALL CHKIN ( 'STPOOL')

      ROOM  = LEN(NTHSTR)
      CSIZE = RTRIM(CONTIN)
      PUTAT = 1

C
C     Retrieve components until we've gone past the first NTH-1
C     strings.
C
      STRNO = 1
      COMP  = 1
      FOUND = .FALSE.

      DO WHILE ( STRNO .LT. NTH )

         CALL GCPOOL ( ITEM, COMP, 1, N, PART, GOTIT )

         GOTIT = N .GT. 0

         IF ( .NOT. GOTIT ) THEN
            NTHSTR = ' '
            SIZE   = 0
            FOUND  = .FALSE.
            CALL CHKOUT ( 'STPOOL' )
            RETURN
         END IF

         CLAST  = RTRIM(PART)
         CFIRST = CLAST - CSIZE + 1

         IF ( CFIRST .LT. 0 ) THEN
            STRNO = STRNO + 1
         ELSE IF ( PART(CFIRST:CLAST) .NE. CONTIN ) THEN
            STRNO = STRNO + 1
         END IF

         COMP = COMP + 1

      END DO

C
C     Once we've reached this point, COMP points to the component
C     of the kernel pool variable that is the beginning of the NTH
C     string.  Now just retrieve components until we run out or
C     one is not continued.
C
      MORE   = .TRUE.
      NTHSTR = ' '
      N      =  0

      DO WHILE ( MORE )

         CALL GCPOOL ( ITEM, COMP, 1, N, PART, MORE )

         MORE = MORE .AND. N .GT. 0

         IF ( MORE ) THEN

            FOUND  = .TRUE.

            CLAST  = RTRIM(PART)
            CFIRST = CLAST - CSIZE + 1

            IF ( CFIRST .LT. 0 ) THEN

               IF ( PUTAT .LE. ROOM ) THEN
                  NTHSTR(PUTAT:) = PART(1:CLAST)
               END IF

               PUTAT          = PUTAT + CLAST
               MORE           = .FALSE.

            ELSE IF ( PART(CFIRST:CLAST) .NE. CONTIN ) THEN

               IF ( PUTAT .LE. ROOM ) THEN
                  NTHSTR(PUTAT:) = PART(1:CLAST)
               END IF
               PUTAT          = PUTAT + CLAST
               MORE           = .FALSE.

            ELSE IF ( CFIRST .GT. 1 ) THEN

               IF ( PUTAT .LE. ROOM ) THEN
                  NTHSTR(PUTAT:) = PART(1:CFIRST-1)
               END IF
               PUTAT          = PUTAT + CFIRST - 1

            END IF

         END IF

         COMP = COMP + 1

      END DO

C
C     We are done.  Get the size of the full string and checkout.
C
      SIZE = PUTAT - 1

      CALL CHKOUT ( 'STPOOL' )

      RETURN
      END
