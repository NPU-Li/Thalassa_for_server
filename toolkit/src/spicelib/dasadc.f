C$Procedure DASADC ( DAS, add data, character )

      SUBROUTINE DASADC ( HANDLE, N, BPOS, EPOS, DATA )

C$ Abstract
C
C     Add character data to a DAS file.
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
      INTEGER               N
      INTEGER               BPOS
      INTEGER               EPOS
      CHARACTER*(*)         DATA   ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     N          I   Number of characters to add to file.
C     BPOS,
C     EPOS       I   Begin and end positions of substrings.
C     DATA       I   Array providing the set of substrings to be added
C                    to the character data in the DAS file.
C
C$ Detailed_Input
C
C     HANDLE   is a file handle of a DAS file opened for writing.
C
C     N        is the total number of characters to add to the specified
C              DAS file.
C
C     BPOS,
C     EPOS     are the begin and end character positions that define the
C              substrings in each of the elements of the input array.
C              This routine writes the first N characters from the
C              specified set of substrings to the specified DAS file.
C
C     DATA     is an array of strings, some portion of whose contents
C              are to be added to the specified DAS file. Specifically,
C              the first N characters of the substrings
C
C                 DATA(I)(BPOS:EPOS),    I = 1, ...
C
C              are appended to the character data in the file.
C
C              DATA must be declared at least as
C
C                 CHARACTER*(EPOS)      DATA   ( R )
C
C              with the dimension R being at least
C
C                 R = INT( ( N + SUBLEN - 1 ) / SUBLEN )
C
C              and SUBLEN, the length of each of the substrings in
C              the array to be added to the DAS file, being
C
C                 SUBLEN  =  EPOS - BPOS + 1
C
C              The order of characters in the input substrings is
C              considered to increase from left to right within each
C              element of DATA, and to increase with the indices of the
C              elements of DATA.
C
C$ Detailed_Output
C
C     None.
C
C     See $Particulars for a description of the effect of this routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, an error is signaled
C         by a routine in the call tree of this routine.
C
C     2)  If EPOS or BPOS are outside of the range
C
C            [  1,  LEN( DATA(1) )  ]
C
C         or if EPOS < BPOS, the error SPICE(BADSUBSTRINGBOUNDS) is
C         signaled.
C
C     3)  If the input count N is less than 1, no data will be
C         added to the specified DAS file.
C
C     4)  If an I/O error occurs during the data addition attempted
C         by this routine, the error is signaled by a routine in the
C         call tree of this routine.
C
C     5)  If N is greater than the number of characters in the
C         specified set of input substrings, the results of calling
C         this routine are unpredictable. This routine cannot
C         detect this error.
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
C     This routine adds character data to a DAS file by "appending" them
C     after any character data already in the file. The sense in which
C     the data are "appended" is that the data will occupy a range of
C     logical addresses for character data that immediately follow the
C     last logical address of a character that is occupied at the time
C     this routine is called. The diagram below illustrates this
C     addition:
C
C        +-------------------------+
C        |    (already in use)     |  Character logical address 1
C        +-------------------------+
C                    .
C                    .
C                    .
C        +-------------------------+  Last character logical address
C        |   (already in use)      |  in use before call to DASADC
C        +-------------------------+
C        |  DATA(1)(BPOS:BPOS)     |  First added character
C        +-------------------------+
C        |  DATA(1)(BPOS+1:BPOS+1) |
C        +-------------------------+
C                     .
C                     .
C                     .
C        +-------------------------+
C        |  DATA(1)(EPOS:EPOS)     |
C        +-------------------------+
C        |  DATA(2)(BPOS:BPOS)     |
C        +-------------------------+
C                     .
C                     .
C                     .
C        +-------------------------+
C        |  DATA(R)(C:C)           |  N'th added character---here R is
C        +-------------------------+
C                                        INT( (N+L-1)/L )
C
C                                     where L = EPOS - BPOS + 1, and
C                                     C is
C
C                                        BPOS + ( N - (R-1)*L ) - 1
C
C
C     The logical organization of the characters in the DAS file is
C     independent of the order of addition to the file or physical
C     location of any data of integer or double precision type.
C
C     The actual physical write operations that add the input array
C     DATA to the indicated DAS file may not take place before this
C     routine returns, since the DAS system buffers data that are
C     written as well as data that are read. In any case, the data
C     will be flushed to the file at the time the file is closed, if
C     not earlier. A physical write of all buffered records can be
C     forced by calling the SPICELIB routine DASWBR (DAS, write
C     buffered records).
C
C     In order to update character logical addresses that already
C     contain data, the SPICELIB routine DASUDC (DAS, update data,
C     character) should be used.
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
C              PROGRAM DASADC_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'dasadc_ex1.das' )
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
C         Data from "dasadc_ex1.das":
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
C-    SPICELIB Version 1.3.0, 08-OCT-2021 (JDR) (NJB)
C
C        Added IMPLICIT NONE statement. Updated the code to avoid that
C        DASCUD is called with a negative number of character words
C        when the input count N is negative.
C
C        Made local variable RECORD a saved variable which is
C        initialized by a DATA statement.
C
C        Bug fix: added FAILED call after DASHFS call.
C
C        Edited the header to comply with NAIF standard.
C
C        Replaced example code with one that demonstrates the usage and
C        effect of all DAS character data routines.
C
C        Updated entries in the $Revisions section.
C
C-    SPICELIB Version 1.2.0, 10-APR-2014 (NJB)
C
C        Deleted declarations of unused parameters.
C
C        Corrected header comments: routine that flushes
C        written, buffered records is DASWBR, not DASWUR.
C
C-    SPICELIB Version 1.1.1, 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination condition.
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C        Modified the $Examples section to demonstrate the new ID word
C        format which includes a file type and to include a call to the
C        new routine DASONW, open new, which makes use of the file
C        type. Also, a variable for the type of the file to be created
C        was added.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
C
C-&


C$ Index_Entries
C
C     add character data to a DAS file
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination condition. Without
C        this test, an infinite loop could result if DASA2L, DASURC or
C        DASWRC signaled an error inside the loop.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )

      INTEGER               NWC
      PARAMETER           ( NWC    = 1024 )

C
C     Local variables
C
      CHARACTER*(NWC)       RECORD

      INTEGER               CHR
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               ELT
      INTEGER               FREE
      INTEGER               LASTC
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NMOVE
      INTEGER               NMOVED
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NUMCHR
      INTEGER               NWRITN
      INTEGER               RCPOS
      INTEGER               RECNO
      INTEGER               WORDNO

C
C     Saved variables
C
      SAVE                  RECORD

C
C     Initial values
C
      DATA                  RECORD / ' ' /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
      CALL CHKIN ( 'DASADC' )


C
C     Make sure BPOS and EPOS are OK; stop here if not.
C
      IF (      ( BPOS .LT. 1              )
     .     .OR. ( EPOS .LT. 1              )
     .     .OR. ( BPOS .GT. LEN( DATA(1) ) )
     .     .OR. ( EPOS .GT. LEN( DATA(1) ) )  )  THEN

         CALL SETMSG ( 'Substring bounds must be in range [1,#]. '    //
     .                 'Actual range [BPOS,EPOS] was [#,#].'           )
         CALL ERRINT ( '#',  LEN(DATA(1))                              )
         CALL ERRINT ( '#',  BPOS                                      )
         CALL ERRINT ( '#',  EPOS                                      )
         CALL SIGERR ( 'SPICE(BADSUBSTRINGBOUNDS)'                     )
         CALL CHKOUT ( 'DASADC'                                        )
         RETURN


      ELSE IF ( EPOS .LT. BPOS ) THEN

         CALL SETMSG ( 'Substring upper bound must not be less than ' //
     .                 'lower bound.  Actual range [BPOS,EPOS] was '  //
     .                 '[#,#].'                                        )
         CALL ERRINT ( '#',  BPOS                                      )
         CALL ERRINT ( '#',  EPOS                                      )
         CALL SIGERR ( 'SPICE(BADSUBSTRINGBOUNDS)'                     )
         CALL CHKOUT ( 'DASADC'                                        )
         RETURN

      END IF

C
C     Get the file summary for this DAS.
C
      CALL DASHFS ( HANDLE,
     .              NRESVR,
     .              NRESVC,
     .              NCOMR,
     .              NCOMC,
     .              FREE,
     .              LASTLA,
     .              LASTRC,
     .              LASTWD )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASADC' )
         RETURN
      END IF

      LASTC  =  LASTLA(CHAR)

C
C     We will keep track of the location that we wish to write to
C     with the variables RECNO and WORDNO.  RECNO will be the record
C     number of the record we'll write to; WORDNO will be the number
C     preceding the word index, within record number RECNO, that we'll
C     write to.  For example, if we're about to write to the first
C     character in record 10, RECNO will be 10 and WORDNO will be 0.  Of
C     course, when WORDNO reaches NWC, we'll have to find a free record
C     before writing anything.
C
C     Prepare the variables RECNO and WORDNO:  use the physical location
C     of the last character address, if there are any character data in
C     the file.  Otherwise, RECNO becomes the first record available for
C     character data.
C
      IF ( LASTC .GE. 1 ) THEN

         CALL DASA2L ( HANDLE,  CHAR,    LASTC,
     .                 CLBASE,  CLSIZE,  RECNO,  WORDNO )
      ELSE

         RECNO   =  FREE
         WORDNO  =  0

      END IF

C
C     Set the number of character words already written.  Keep
C     writing to the file until this number equals the number of
C     elements in DATA.
C
C     Note that if N is non-positive, the loop doesn't get
C     exercised.
C
C     Also initialize the array element index and position of the
C     character to be moved next.
C
      NWRITN  =  0
      ELT     =  1
      CHR     =  BPOS

      DO WHILE (  ( NWRITN .LT. N ) .AND. ( .NOT. FAILED() )  )

C
C        Write as much data as we can (or need to) into the current
C        record.  We assume that RECNO, WORDNO, and NWRITN have
C        been set correctly at this point.
C
C        Find out how many words to write into the current record.
C        There may be no space left in the current record.
C
         NUMCHR  =  MIN (  N - NWRITN,   NWC - WORDNO )

         IF ( NUMCHR .GT. 0 ) THEN

C
C           Write NUMCHR words into the current record.  If the record
C           is new, write the entire record.  Otherwise, just update
C           the part we're interested in.
C
C           In either case, we'll first fill in characters WORDNO+1
C           through WORDNO + NUMCHR of the string RECORD.
C
C
C           So far, we haven't moved any characters.
C
            NMOVED = 0
            RCPOS  = WORDNO

            DO WHILE ( NMOVED .LT. NUMCHR )

C
C              Find out how many characters in the current array
C              element we should move.
C
               IF ( CHR .GT. EPOS ) THEN
                  ELT = ELT + 1
                  CHR = BPOS
               END IF

               NMOVE  =  MIN (  NUMCHR - NMOVED,   EPOS - CHR + 1  )

               RECORD( RCPOS+1 : RCPOS+NMOVE )   =   DATA(ELT) (CHR:)

               NMOVED =  NMOVED + NMOVE
               RCPOS  =  RCPOS  + NMOVE
               CHR    =  CHR    + NMOVE

            END DO

C
C           Now we can write or update the file with RECORD.
C
            IF ( WORDNO .EQ. 0 ) THEN
C
C              The record has not yet been written, so write out the
C              entire record.
C
               CALL DASWRC (  HANDLE,  RECNO,  RECORD  )

            ELSE
C
C              Update elements WORDNO+1 through WORDNO+NUMCHR.
C
               CALL DASURC (  HANDLE,
     .                        RECNO,
     .                        WORDNO + 1,
     .                        WORDNO + NUMCHR,
     .                        RECORD ( WORDNO+1 : WORDNO+NUMCHR )  )

            END IF


            NWRITN  =  NWRITN + NUMCHR
            WORDNO  =  WORDNO + NUMCHR

         ELSE

C
C           It's time to start on a new record.  If the record we
C           just finished writing to (or just attempted writing to,
C           if it was full) was FREE or a higher-numbered record,
C           then we are writing to a contiguous set of data records:
C           the next record to write to is the immediate successor
C           of the last one.  Otherwise, FREE is the next record
C           to write to.
C
C           We intentionally leave FREE at the value it had before
C           we starting adding data to the file.
C
            IF ( RECNO .GE. FREE ) THEN
               RECNO  =  RECNO + 1
            ELSE
               RECNO  =  FREE
            END IF

            WORDNO = 0

         END IF

      END DO

C
C     Update the DAS file directories to reflect the addition of NWRITN
C     character words.  DASCUD will also update the file summary
C     accordingly.
C
      CALL DASCUD ( HANDLE, CHAR, NWRITN )

      CALL CHKOUT ( 'DASADC' )
      RETURN
      END
