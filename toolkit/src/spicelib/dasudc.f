C$Procedure DASUDC ( DAS, update data, character )

      SUBROUTINE DASUDC ( HANDLE, FIRST, LAST, BPOS, EPOS, DATA )

C$ Abstract
C
C     Update character data in a specified range of DAS logical
C     addresses with substrings of a character array.
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
C     DATA       I   Data having addresses FIRST through LAST.
C
C$ Detailed_Input
C
C     HANDLE   is a file handle of a DAS file opened for writing.
C
C     FIRST,
C     LAST     are the first and last of a range of DAS logical
C              addresses of characters. These addresses satisfy
C              the inequality
C
C                 1  <=   FIRST   <=   LAST   <=   LASTC
C
C              where LASTC is the last character logical address
C              in use in the DAS file designated by HANDLE.
C
C     BPOS,
C     EPOS     are the begin and end character positions that define the
C              substrings in each of the elements of the input array
C              that are to replace the data in the range of DAS
C              character addresses given by FIRST and LAST.
C
C     DATA     is an array of strings. The contents of the specified
C              substrings of the elements of the array DATA will be
C              written to the indicated DAS file in order:
C              DATA(1)(BPOS:BPOS) will be written to character logical
C              address FIRST; DATA(1)(BPOS+1:BPOS+1) will be written to
C              the character logical address FIRST+1, and so on; in this
C              ordering scheme, character (BPOS:BPOS) of DATA(I+1) is
C              the successor of character (EPOS:EPOS) of DATA(I).
C
C              DATA must be declared at least as
C
C                 CHARACTER*(EPOS)      DATA   ( R )
C
C              with the dimension R being at least
C
C                 R = INT( ( LAST - FIRST + SUBLEN ) / SUBLEN )
C
C              and SUBLEN, the length of each of the substrings in
C              the array to be written to the DAS file, being
C
C                 SUBLEN  =  EPOS - BPOS + 1
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
C     1)  If the input file handle is invalid, an error is signaled by
C         a routine in the call tree of this routine.
C
C     2)  Only logical addresses that already contain data may be
C         updated: if either FIRST or LAST are outside the range
C
C            [ 1,  LASTC ]
C
C         where LASTC is the last character logical address that
C         currently contains data in the indicated DAS file, the error
C         SPICE(INVALIDADDRESS) is signaled. The DAS file will not be
C         modified.
C
C     3)  If EPOS or BPOS are outside of the range
C
C            [  1,  LEN( DATA(1) )  ]
C
C         the error SPICE(INVALIDINDEX) is signaled.
C
C     4)  If BPOS is greater than EPOS, the error
C         SPICE(INDICESOUTOFORDER) is signaled.
C
C     5)  If FIRST > LAST but both addresses are valid, this routine
C         will not modify the indicated DAS file. No error will be
C         signaled.
C
C     6)  If an I/O error occurs during the data update attempted
C         by this routine, the error is signaled by a routine in the
C         call tree of this routine. FIRST and LAST will not be
C         modified.
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
C     This routine replaces the character data in the specified range
C     of logical addresses within a DAS file with the contents of the
C     specified substrings of the input array DATA.
C
C     The actual physical write operations that update the indicated
C     DAS file with the contents of the input array DATA may not take
C     place before this routine returns, since the DAS system buffers
C     data that are written as well as data that are read. In any case,
C     the data will be flushed to the file at the time the file is
C     closed, if not earlier. A physical write of all buffered
C     records can be forced by calling the SPICELIB routine DASWBR
C     (DAS, write buffered records).
C
C     In order to append character data to a DAS file, filling in a
C     range of character logical addresses that starts immediately
C     after the last character logical address currently in use, the
C     SPICELIB routine DASADC (DAS add data, character) should be
C     used.
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
C              PROGRAM DASUDC_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'dasudc_ex1.das' )
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
C         Data from "dasudc_ex1.das":
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
C-    SPICELIB Version 2.0.0, 19-MAY-2021 (NJB) (JDR)
C
C        Added error checks for invalid begin and end indices BPOS
C        and EPOS.
C
C        Added IMPLICIT NONE statement.
C
C        Updated entries in $Exceptions and $Revisions sections and
C        removed reference to nonexistent API from $Particulars.
C
C        Edited the header to comply with NAIF standard.
C
C        Replaced example code with one that demonstrates the usage and
C        effect of all DAS character data routines.
C
C        Updated entries in $Revisions section.
C
C-    SPICELIB Version 1.3.0, 10-APR-2014 (NJB)
C
C        Deleted declarations of unused parameters.
C
C        Corrected header comments: routine that flushes
C        written, buffered records is DASWBR, not DASWUR.
C
C-    SPICELIB Version 1.2.1, 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    SPICELIB Version 1.2.0, 12-MAY-1995 (NJB)
C
C        Bug fix: routine handled values of BPOS incorrectly when
C        BPOS > 1.
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
C     update a range of DAS logical addresses using substrings
C     write substrings to a range of DAS logical addresses
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.2.0, 12-MAY-1995 (NJB)
C
C        Bug fix: routine handled values of BPOS incorrectly when
C        BPOS > 1. This was due to the incorrect initialization
C        of the internal variables CHR and ELT. The initialization
C        was corrected.
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Tests of FAILED() added to loop termination conditions.
C        Without these tests, infinite loops could result if DASA2L or
C        DASURC signaled an error inside the loops.
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
      INTEGER               NWC
      PARAMETER           ( NWC  =  1024 )

      INTEGER               CHAR
      PARAMETER           ( CHAR =  1  )

C
C     Local variables
C
      INTEGER               CHR
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               ELT
      INTEGER               L
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
      INTEGER               N
      INTEGER               NMOVE
      INTEGER               NMOVED
      INTEGER               NUMCHR
      INTEGER               NWRITN
      INTEGER               RECNO
      INTEGER               RCPOS
      INTEGER               WORDNO


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASUDC' )

C
C     Get the last logical addresses in use in this DAS file.
C
      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )

C
C     Validate the input addresses.
C
      IF (      ( FIRST .LT. 1     )
     .     .OR. ( FIRST .GT. LASTC )
     .     .OR. ( LAST  .LT. 1     )
     .     .OR. ( LAST  .GT. LASTC )  ) THEN

         CALL SETMSG ( 'FIRST was #. LAST was #. Valid range is [1,#].')
         CALL ERRINT ( '#',   FIRST                                    )
         CALL ERRINT ( '#',   LAST                                     )
         CALL ERRINT ( '#',   LASTC                                    )
         CALL SIGERR ( 'SPICE(INVALIDADDRESS)'                         )
         CALL CHKOUT ( 'DASUDC'                                        )
         RETURN

      END IF

C
C     Make sure BPOS and EPOS are valid and compatible with the string
C     length of the input array.
C
      IF (  ( BPOS .LT. 1 ) .OR. ( BPOS .GT. LEN( DATA(1) ) )  ) THEN

         CALL SETMSG ( 'String begin index must be '
     .   //            'in the range #:# but was #.' )
         CALL ERRINT ( '#', 1                        )
         CALL ERRINT ( '#', LEN( DATA(1) )           )
         CALL ERRINT ( '#', BPOS                     )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'         )
         CALL CHKOUT ( 'DASUDC'                      )
         RETURN

      END IF

      IF (  ( EPOS .LT. 1 ) .OR. ( EPOS .GT. LEN( DATA(1) ) )  ) THEN

         CALL SETMSG ( 'String end index must be in '
     .   //            'the range #:# but was #.'    )
         CALL ERRINT ( '#', 1                        )
         CALL ERRINT ( '#', LEN( DATA(1) )           )
         CALL ERRINT ( '#', EPOS                     )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'         )
         CALL CHKOUT ( 'DASUDC'                      )
         RETURN

      END IF

      IF ( BPOS .GT. EPOS ) THEN

         CALL SETMSG ( 'String begin index # must be less '
     .   //            'than or equal to the end index #.' )
         CALL ERRINT ( '#', BPOS                           )
         CALL ERRINT ( '#', EPOS                           )
         CALL SIGERR ( 'SPICE(INDICESOUTOFORDER)'          )
         CALL CHKOUT ( 'DASUDC'                            )
         RETURN

      END IF
C
C     Get the length of the substrings of DATA.  Count the total number
C     of characters to write.
C
      L       =  EPOS - BPOS  + 1
      N       =  LAST - FIRST + 1
      NWRITN  =  0

C
C     Find out the physical location of the first character to update.
C
      CALL DASA2L ( HANDLE, CHAR, FIRST, CLBASE, CLSIZE, RECNO, WORDNO )

C
C     Write as much data into record RECNO as is necessary and possible.
C
C     NUMCHR is the number of characters to write to the current record.
C
C     ELT is the index of the element of the input array that we're
C     taking data from.  CHR is the position in that array element of
C     the next character to move to the file.
C
C     NMOVED is the number of characters we've moved into the current
C     record so far.
C
C     RCPOS is the character position we'll write to next in the current
C     record.
C
      NUMCHR  =  MIN ( N,  NWC - WORDNO + 1 )
      ELT     =  1
      CHR     =  BPOS
      NMOVED  =  0
      RCPOS   =  WORDNO


      DO WHILE (  ( NMOVED .LT. NUMCHR ) .AND. ( .NOT. FAILED() )  )

         IF ( CHR .GT. EPOS ) THEN
            ELT = ELT + 1
            CHR = BPOS
         END IF

C
C        Find out how many characters to move from the current array
C        element to the current record.
C
         NMOVE   =  MIN (  NUMCHR - NMOVED,   EPOS - CHR + 1  )

C
C        Update the current record.
C
         CALL DASURC (  HANDLE,
     .                  RECNO,
     .                  RCPOS,
     .                  RCPOS + NMOVE - 1,
     .                  DATA(ELT) ( CHR  :  CHR + NMOVE - 1 )   )

         NMOVED  =  NMOVED + NMOVE
         RCPOS   =  RCPOS  + NMOVE
         CHR     =  CHR    + NMOVE

      END DO

      NWRITN =  NUMCHR
      RECNO  =  RECNO + 1

C
C     Update as many additional records as necessary.
C

      DO WHILE (  ( NWRITN .LT. N ) .AND. ( .NOT. FAILED() )  )

C
C        At this point, RECNO is the correct number of the record to
C        write to next.  CLBASE is the number of the first record of
C        the cluster we're about to write to.
C
         IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN

C
C           We can continue writing the current cluster.  Find
C           out how many elements to write to the current record,
C           and write them.
C
            NUMCHR  =  MIN ( N - NWRITN,  NWC )
            NMOVED  =  0
            RCPOS   =  1

            DO WHILE ( ( NMOVED .LT. NUMCHR ) .AND. ( .NOT. FAILED() ) )

               IF ( CHR .GT. L ) THEN
                  ELT = ELT + 1
                  CHR = BPOS
               END IF

C
C              Find out how many characters to move from the array
C              element to the current record.
C
               NMOVE   =  MIN (  NUMCHR - NMOVED,   EPOS - CHR + 1  )

               CALL DASURC (  HANDLE,
     .                        RECNO,
     .                        RCPOS,
     .                        RCPOS + NMOVE - 1,
     .                        DATA(ELT) ( CHR  :  CHR + NMOVE - 1 )  )

               NMOVED  =  NMOVED + NMOVE
               RCPOS   =  RCPOS  + NMOVE
               CHR     =  CHR    + NMOVE

            END DO

            NWRITN  =   NWRITN + NUMCHR
            RECNO   =   RECNO  + 1


         ELSE

C
C           We must find the next character cluster to write to.
C           The first character in this cluster has address FIRST +
C           NWRITN.
C
            CALL DASA2L ( HANDLE,
     .                    CHAR,
     .                    FIRST + NWRITN,
     .                    CLBASE,
     .                    CLSIZE,
     .                    RECNO,
     .                    WORDNO  )

         END IF

      END DO


      CALL CHKOUT ( 'DASUDC' )
      RETURN
      END
