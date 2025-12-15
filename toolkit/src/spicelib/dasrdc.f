C$Procedure DASRDC ( DAS, read data, character )

      SUBROUTINE DASRDC ( HANDLE, FIRST, LAST, BPOS, EPOS, DATA )

C$ Abstract
C
C     Read character data from a range of DAS logical addresses.
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
C     DAS
C
C$ Keywords
C
C     ARRAY
C     ASSIGNMENT
C     DAS
C     FILES
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               BPOS
      INTEGER               EPOS
      CHARACTER*(*)         DATA   ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     FIRST,
C     LAST       I   Range of DAS character logical addresses.
C     BPOS,
C     EPOS       I   Begin and end positions of substrings.
C     DATA       O   Data having addresses FIRST through LAST.
C
C$ Detailed_Input
C
C     HANDLE   is a file handle for an open DAS file.
C
C     FIRST,
C     LAST     are a range of DAS character logical addresses.
C              FIRST and LAST must be greater than or equal to
C              1 and less than or equal to the highest character
C              logical address in the DAS file designated by
C              HANDLE.
C
C     BPOS,
C     EPOS     are the begin and end character positions that define the
C              substrings in each of the elements of the output array
C              DATA into which character data is to be read.
C
C$ Detailed_Output
C
C     DATA     is an array of strings. On output, the character words in
C              the logical address range FIRST through LAST are copied
C              into the characters
C
C                 DATA(1)(BPOS:BPOS),
C                 DATA(1)(BPOS+1:BPOS+1),
C                             .
C                             .
C                             .
C                 DATA(1)(EPOS:EPOS),
C                 DATA(2)(BPOS:BPOS),
C                 DATA(2)(BPOS+1:BPOS+1),
C                             .
C                             .
C                             .
C                 DATA(R)(BPOS:BPOS)
C                 DATA(R)(BPOS+1:BPOS+1)
C                             .
C                             .
C                             .
C
C              in that order. Note that the character positions of DATA
C              **other** than the ones shown in the diagram remain
C              unmodified.
C
C              DATA must be declared at least as
C
C                 CHARACTER*(EPOS)        DATA   ( R )
C
C              with the dimension R being at least
C
C                 R = INT( ( LAST - FIRST + SUBLEN ) / SUBLEN )
C
C              and SUBLEN, the length of each of the substrings read
C              into the array elements from the DAS file, being
C
C                 SUBLEN  =  EPOS - BPOS + 1
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, an error is signaled
C         by a routine in the call tree of this routine. DATA will
C         not be modified.
C
C     2)  If EPOS or BPOS are outside of the range
C
C            [  1,  LEN( DATA(1) )  ]
C
C         or if EPOS < BPOS, the error SPICE(BADSUBSTRINGBOUNDS) is
C         signaled.
C
C     3)  If FIRST or LAST are out of range, an error is signaled by a
C         routine in the call tree of this routine. DATA will not be
C         modified.
C
C     4)  If FIRST is greater than LAST, DATA is left unchanged.
C
C     5)  If DATA is declared with length less than
C
C            ( LAST - FIRST + ( EPOS-BPOS+1 )  ) / ( EPOS-BPOS+1 )
C
C         the error cannot be diagnosed by this routine.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     DAS is a low-level format meant to store and transmit data. As
C     such, character data in DAS files are not interpreted by SPICELIB
C     DAS input or output routines. There are no limits on which
C     character values may be placed in the virtual character array of a
C     DAS file.
C
C     This routine provides random read access to the character data in
C     a DAS file. These data are logically structured as a
C     one-dimensional array of characters.
C
C     However, since Fortran programs usually use strings rather than
C     arrays of individual characters, the interface of this routine
C     provides for extraction of data from a DAS file into an array of
C     strings.
C
C     DASRDC allows the caller to control the amount of character data
C     read into each array element. This feature allows a program to
C     read character data into an array that has a different string
C     length from the one used to write the character data, without
C     losing the correspondence between input and output array elements.
C     For example, an array of strings of 32 characters can be written
C     to a DAS file and read back by DASRDC into a buffer of strings
C     having length 80 characters, mapping each 32-character string to
C     characters 1--32 of the output buffer.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) The following example demonstrates the capabilities of the
C        DAS character data routines. The reader should notice that
C        in these interfaces, the character data are treated not as
C        strings (or arrays of strings) but as a stream of single
C        characters: DAS character data are not limited to
C        human-readable text. For example, one can store images or
C        DEM data as DAS character data.
C
C        The example shows how to add a variable amount of character
C        data to a new DAS file, how to update some of the character
C        logical addresses within that file, and how to read that
C        data out to a different array.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASRDC_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'dasrdc_ex1.das' )
C
C              CHARACTER*(*)         TYPE
C              PARAMETER           ( TYPE  = 'TEST'           )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(22)        CDATIN ( 3  )
C              CHARACTER*(30)        CDATOU ( 10 )
C
C              INTEGER               HANDLE
C              INTEGER               I
C
C              DATA CDATOU  / '..............................',
C             .               '..............................',
C             .               '..............................',
C             .               '..............................',
C             .               '..............................',
C             .               '..............................',
C             .               '..............................',
C             .               '..............................',
C             .               '         1         2         3',
C             .               '123456789012345678901234567890' /
C
C        C
C        C     Open a new DAS file. Use the file name as the internal
C        C     file name, and reserve no records for comments.
C        C
C              CALL DASONW ( FNAME, TYPE, FNAME, 0, HANDLE )
C
C        C
C        C     Set the input data. Note that these data will be
C        C     considered as a binary data stream: DAS character data
C        C     are not limited to human-readable text. For example,
C        C     one can store images or DEM data as DAS character data.
C        C
C              CDATIN ( 1 ) = '--F-345678901234567890'
C              CDATIN ( 2 ) = '--S-345678901234567890'
C              CDATIN ( 3 ) = '--T-IRDxxxxxxxxxxxxxxx'
C
C        C
C        C     Add the last 20 characters of the first two elements
C        C     of CDATIN, and the 3rd character from the third one.
C        C
C              CALL DASADC ( HANDLE, 41, 3, 22, CDATIN )
C
C        C
C        C     Update the 10th, 20th and 30th character in the DAS
C        C     file with a vertical bar.
C        C
C              DO I = 1, 3
C
C                 CALL DASUDC ( HANDLE, I*10, I*10, 1, 1, '|' )
C
C              END DO
C
C        C
C        C     Close the file.
C        C
C              CALL DASCLS ( HANDLE )
C
C        C
C        C     Now verify the addition of data by opening the
C        C     file for read access and retrieving the data.
C        C
C              CALL DASOPR ( FNAME, HANDLE )
C
C        C
C        C     Read the 41 characters that we stored on the DAS
C        C     file. Update the data on the CDATOU array, placing
C        C     6 characters on each element, starting from the
C        C     10th position.
C        C
C              CALL DASRDC ( HANDLE, 1, 41, 10, 15, CDATOU )
C
C        C
C        C     Dump the data to the screen. Note that the last
C        C     three lines should remain unmodified, and that
C        C     only 5 characters will be written on the 7th line.
C        C
C              WRITE (*,*)
C              WRITE (*,*) 'Data from "', FNAME, '":'
C              WRITE (*,*)
C
C              DO I = 1, 10
C                 WRITE (*,*) CDATOU(I)
C              END DO
C
C        C
C        C     Close the file.
C        C
C              CALL DASCLS ( HANDLE )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Data from "dasrdc_ex1.das":
C
C         .........F-3456...............
C         .........789|12...............
C         .........345678...............
C         .........9|S-34...............
C         .........56789|...............
C         .........123456...............
C         .........7890T................
C         ..............................
C                  1         2         3
C         123456789012345678901234567890
C
C
C        Note that after run completion, a new DAS file exists in the
C        output directory.
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
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 09-OCT-2021 (JDR) (NJB)
C
C        Added IMPLICIT NONE statement.
C
C        Added FAILED call following DASA2L call.
C
C        Updated entries in $Revisions section.
C
C        Edited the header to comply with NAIF standard.
C
C        Replaced example code with one that demonstrates the usage and
C        effect of all DAS character data routines.
C
C-    SPICELIB Version 1.2.2, 03-JUL-1996 (NJB)
C
C        Various errors in the header comments were fixed.
C
C-    SPICELIB Version 1.2.1, 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB)
C
C        Routine now uses discovery check-in. FAILED test moved inside
C        loops.
C
C-    SPICELIB Version 1.2.0, 14-SEP-1995 (NJB)
C
C        Bug fix: reference to DASADS in CHKOUT calls corrected.
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination conditions.
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C        Modified the $Examples section to demonstrate the new ID word
C        format which includes a file type and to include a call to the
C        new routine DASONW, open new for write, which makes use of the
C        file type. Also,  a variable for the type of the file to be
C        created was added.
C
C-    SPICELIB Version 1.0.0, 12-NOV-1992 (NJB) (WLT)
C
C-&


C$ Index_Entries
C
C     read character data from a DAS file
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB)
C
C        Routine now uses discovery check-in. FAILED test moved inside
C        loops.
C
C-    SPICELIB Version 1.2.0, 03-NOV-1995 (NJB)
C
C        Bug fix: reference to DASADS in CHKOUT calls corrected.
C        These references have been changed to 'DASRDC'.
C
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination conditions. Without
C        this test, an infinite loop could result if DASA2L or DASRRC
C        signaled an error inside the loops.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED

C
C     Local parameters
C
      INTEGER               NWC
      PARAMETER           ( NWC =  1024 )

      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )



C
C     Local variables
C
      INTEGER               CHR
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               ELT
      INTEGER               L
      INTEGER               N
      INTEGER               NMOVE
      INTEGER               NMOVED
      INTEGER               NREAD
      INTEGER               NUMCHR
      INTEGER               RECNO
      INTEGER               RCPOS
      INTEGER               WORDNO



C
C     Make sure BPOS and EPOS are ok; stop here if not.
C
      IF (      ( BPOS .LT. 1              )
     .     .OR. ( EPOS .LT. 1              )
     .     .OR. ( BPOS .GT. LEN( DATA(1) ) )
     .     .OR. ( EPOS .GT. LEN( DATA(1) ) )  )  THEN

         CALL CHKIN  ( 'DASRDC'                                        )
         CALL SETMSG ( 'Substring bounds must be in range [1,#]. '    //
     .                 'Actual range [BPOS,EPOS] was [#,#].'           )
         CALL ERRINT ( '#',  LEN(DATA(1))                              )
         CALL ERRINT ( '#',  BPOS                                      )
         CALL ERRINT ( '#',  EPOS                                      )
         CALL SIGERR ( 'SPICE(BADSUBSTRINGBOUNDS)'                     )
         CALL CHKOUT ( 'DASRDC'                                        )
         RETURN


      ELSE IF ( EPOS .LT. BPOS ) THEN

         CALL CHKIN  ( 'DASRDC'                                        )
         CALL SETMSG ( 'Substring upper bound must not be less than ' //
     .                 'lower bound.  Actual range [BPOS,EPOS] was '  //
     .                 '[#,#].'                                        )
         CALL ERRINT ( '#',  BPOS                                      )
         CALL ERRINT ( '#',  EPOS                                      )
         CALL SIGERR ( 'SPICE(BADSUBSTRINGBOUNDS)'                     )
         CALL CHKOUT ( 'DASRDC'                                        )
         RETURN

      END IF

C
C     Find out the physical location of the first character to read.  If
C     FIRST is out of range, DASA2L will cause an error to be signaled.
C
      CALL DASA2L ( HANDLE, CHAR, FIRST, CLBASE, CLSIZE, RECNO, WORDNO )

      IF ( FAILED() ) THEN
         RETURN
      END IF

C
C     Get the length of the elements of DATA.  Count the total number
C     of characters to read.
C
      L       =  EPOS - BPOS  + 1
      N       =  LAST - FIRST + 1
      NREAD   =  0

C
C     Read as much data from record RECNO as is necessary and possible.
C
      NUMCHR  =  MIN ( N,  NWC - WORDNO + 1 )

      ELT     =  1
      CHR     =  BPOS
      NMOVED  =  0
      RCPOS   =  WORDNO


      DO WHILE ( NMOVED .LT. NUMCHR )

         IF ( FAILED() ) THEN
            RETURN
         END IF

         IF ( CHR .GT. EPOS ) THEN
            ELT = ELT + 1
            CHR = BPOS
         END IF
C
C        Find out how many characters to move from the current record
C        to the current array element.
C
         NMOVE   =  MIN (  NUMCHR - NMOVED,   EPOS - CHR + 1  )

         CALL DASRRC (  HANDLE,
     .                  RECNO,
     .                  RCPOS,
     .                  RCPOS + NMOVE - 1,
     .                  DATA(ELT) ( CHR  :  CHR + NMOVE - 1 )   )

         NMOVED  =  NMOVED + NMOVE
         RCPOS   =  RCPOS  + NMOVE
         CHR     =  CHR    + NMOVE

      END DO

      NREAD  =  NUMCHR
      RECNO  =  RECNO + 1

C
C     Read from as many additional records as necessary.
C

      DO WHILE ( NREAD .LT. N )

         IF ( FAILED() ) THEN
            RETURN
         END IF

C
C        At this point, RECNO is the correct number of the
C        record to read from next.  CLBASE is the number
C        of the first record of the cluster we're about
C        to read from.
C
C
         IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN
C
C           We can continue reading from the current cluster.  Find
C           out how many elements to read from the current record,
C           and read them.
C
            NUMCHR  =  MIN ( N - NREAD,  NWC )
            NMOVED  =  0
            RCPOS   =  1

            DO WHILE ( ( NMOVED .LT. NUMCHR ) .AND. ( .NOT. FAILED() ) )

               IF ( CHR .GT. EPOS ) THEN
                  ELT = ELT + 1
                  CHR = BPOS
               END IF
C
C              Find out how many characters to move from the current
C              record to the current array element.
C
               NMOVE   =  MIN (  NUMCHR - NMOVED,   EPOS - CHR + 1  )

               CALL DASRRC (  HANDLE,
     .                        RECNO,
     .                        RCPOS,
     .                        RCPOS + NMOVE - 1,
     .                        DATA(ELT) ( CHR  :  CHR + NMOVE - 1 )   )

               NMOVED  =  NMOVED + NMOVE
               RCPOS   =  RCPOS  + NMOVE
               CHR     =  CHR    + NMOVE

            END DO

            NREAD   =   NREAD + NUMCHR
            RECNO   =   RECNO + 1


         ELSE
C
C           We must find the next character cluster to
C           read from.  The first character in this
C           cluster has address FIRST + NREAD.
C
            CALL DASA2L ( HANDLE,
     .                    CHAR,
     .                    FIRST + NREAD,
     .                    CLBASE,
     .                    CLSIZE,
     .                    RECNO,
     .                    WORDNO  )

         END IF

      END DO

      RETURN
      END
