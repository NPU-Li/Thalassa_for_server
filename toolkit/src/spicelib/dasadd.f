C$Procedure DASADD ( DAS, add data, double precision )

      SUBROUTINE DASADD ( HANDLE, N, DATA )

C$ Abstract
C
C     Add an array of double precision numbers to a DAS file.
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
      DOUBLE PRECISION      DATA   ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     N          I   Number of d.p. numbers to add to DAS file.
C     DATA       I   Array of d.p. numbers to add.
C
C$ Detailed_Input
C
C     HANDLE   is a file handle of a DAS file opened for writing.
C
C     N        is the number of double precision "words" to add to the
C              DAS file specified by HANDLE.
C
C     DATA     is an array of double precision numbers to be added to
C              the specified DAS file. Elements 1 through N are appended
C              to the double precision data in the file.
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
C     1)  If the input file handle is invalid, an error is signaled by a
C         routine in the call tree of this routine.
C
C     2)  If an I/O error occurs during the data addition attempted by
C         this routine, the error is signaled by a routine in the call
C         tree of this routine.
C
C     3)  If the input count N is less than 1, no data will be added to
C         the specified DAS file. No error will be signaled.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine adds double precision data to a DAS file by
C     "appending" them after any double precision data already in the
C     file. The sense in which the data are "appended" is that the
C     data will occupy a range of logical addresses for double precision
C     data that immediately follow the last logical address of a double
C     precision number that is occupied at the time this routine is
C     called. The diagram below illustrates this addition:
C
C        +-------------------------+
C        |    (already in use)     |  D.p. logical address 1
C        +-------------------------+
C                    .
C                    .
C                    .
C        +-------------------------+
C        |    (already in use)     |  Last d.p. logical address
C        +-------------------------+  in use before call to DASADD
C        |        DATA(1)          |
C        +-------------------------+
C                    .
C                    .
C                    .
C        +-------------------------+
C        |        DATA(N)          |
C        +-------------------------+
C
C
C     The logical organization of the double precision numbers in the
C     DAS file is independent of the location in the file of any data
C     of integer or character type.
C
C     The actual physical write operations that add the input array
C     DATA to the indicated DAS file might not take place before this
C     routine returns, since the DAS system buffers data that are
C     written as well as data that are read. In any case, the data
C     will be flushed to the file at the time the file is closed, if
C     not earlier. A physical write of all buffered records can be
C     forced by calling the SPICELIB routine DASWBR (DAS, write
C     buffered records).
C
C     In order to update double precision logical addresses that
C     already contain data, the SPICELIB routine DASUDD
C     (DAS update data, double precision) should be used.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a new DAS file and add 200 double precision numbers
C        to it. Close the file, then re-open it and read the data back
C        out.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASADD_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'dasadd_ex1.das' )
C
C              CHARACTER*(*)         TYPE
C              PARAMETER           ( TYPE  = 'TEST' )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      DATA   ( 200 )
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Open a new DAS file. Use the file name as the internal
C        C     file name, and reserve no records for comments.
C        C
C              CALL DASONW ( FNAME, TYPE, FNAME, 0, HANDLE )
C
C        C
C        C     Fill the array DATA with the double precision
C        C     numbers 1.D0 through 100.D0, and add this array
C        C     to the file.
C        C
C              DO I = 1, 100
C                 DATA(I) = DBLE(I)
C              END DO
C
C              CALL DASADD ( HANDLE, 100, DATA )
C
C        C
C        C     Now append the array DATA to the file again.
C        C
C              CALL DASADD ( HANDLE, 100, DATA )
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
C              CALL DASRDD ( HANDLE, 1, 200, DATA )
C
C        C
C        C     Dump the data to the screen. We should see the
C        C     sequence 1.0, 2.0, ..., 100.0, 1.0, 2.0, ... , 100.0.
C        C     The numbers will be represented as double precision
C        C     numbers in the output.
C        C
C              WRITE (*,*)
C              WRITE (*,*) 'Data from "', FNAME, '":'
C              WRITE (*,*)
C              DO I = 1, 25
C                 WRITE (*,'(8F7.1)') (DATA((I-1)*8+J), J = 1, 8)
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
C         Data from "dasadd_ex1.das":
C
C            1.0    2.0    3.0    4.0    5.0    6.0    7.0    8.0
C            9.0   10.0   11.0   12.0   13.0   14.0   15.0   16.0
C           17.0   18.0   19.0   20.0   21.0   22.0   23.0   24.0
C           25.0   26.0   27.0   28.0   29.0   30.0   31.0   32.0
C           33.0   34.0   35.0   36.0   37.0   38.0   39.0   40.0
C           41.0   42.0   43.0   44.0   45.0   46.0   47.0   48.0
C           49.0   50.0   51.0   52.0   53.0   54.0   55.0   56.0
C           57.0   58.0   59.0   60.0   61.0   62.0   63.0   64.0
C           65.0   66.0   67.0   68.0   69.0   70.0   71.0   72.0
C           73.0   74.0   75.0   76.0   77.0   78.0   79.0   80.0
C           81.0   82.0   83.0   84.0   85.0   86.0   87.0   88.0
C           89.0   90.0   91.0   92.0   93.0   94.0   95.0   96.0
C           97.0   98.0   99.0  100.0    1.0    2.0    3.0    4.0
C            5.0    6.0    7.0    8.0    9.0   10.0   11.0   12.0
C           13.0   14.0   15.0   16.0   17.0   18.0   19.0   20.0
C           21.0   22.0   23.0   24.0   25.0   26.0   27.0   28.0
C           29.0   30.0   31.0   32.0   33.0   34.0   35.0   36.0
C           37.0   38.0   39.0   40.0   41.0   42.0   43.0   44.0
C           45.0   46.0   47.0   48.0   49.0   50.0   51.0   52.0
C           53.0   54.0   55.0   56.0   57.0   58.0   59.0   60.0
C           61.0   62.0   63.0   64.0   65.0   66.0   67.0   68.0
C           69.0   70.0   71.0   72.0   73.0   74.0   75.0   76.0
C           77.0   78.0   79.0   80.0   81.0   82.0   83.0   84.0
C           85.0   86.0   87.0   88.0   89.0   90.0   91.0   92.0
C           93.0   94.0   95.0   96.0   97.0   98.0   99.0  100.0
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
C        Added IMPLICIT NONE statement. Updated the code to prevent
C        DASCUD from being called with a negative number of double
C        precision words when the input count N is negative.
C
C        Edited the header to comply with NAIF standard. Fixed
C        bugs in the code example and modified the output presentation
C        to comply with the maximum line length for header comments.
C
C        Made local variable RECORD a saved variable which is
C        initialized by a DATA statement.
C
C        Bug fix: added FAILED call after DASHFS call.
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
C     add double precision data to a DAS file
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination condition. Without
C        this test, an infinite loop could result if DASA2L, DASURD or
C        DASWRD signaled an error inside the loop.
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
      INTEGER               DP
      PARAMETER           ( DP     =  2  )

      INTEGER               NWD
      PARAMETER           ( NWD    = 128 )

C
C     Local variables
C
      DOUBLE PRECISION      RECORD ( NWD )

      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               FREE
      INTEGER               LASTD
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NUMDP
      INTEGER               NWRITN
      INTEGER               RECNO
      INTEGER               WORDNO


C
C     Saved variables
C
      SAVE                  RECORD

C
C     Initial values
C
      DATA                  RECORD / NWD * 0.D0 /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
      CALL CHKIN ( 'DASADD' )


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
         CALL CHKOUT ( 'DASADD' )
         RETURN
      END IF

      LASTD  =  LASTLA(DP)

C
C     We will keep track of the location that we wish to write to
C     with the variables RECNO and WORDNO.  RECNO will be the record
C     number of the record we'll write to; WORDNO will be the number
C     preceding the word index, within record number RECNO, that we'll
C     write to.  For example, if we're about to write to the first
C     double precision number in record 10, RECNO will be 10 and
C     WORDNO will be 0.  Of course, when WORDNO reaches NWD, we'll
C     have to find a free record before writing anything.
C
C     Prepare the variables RECNO and WORDNO:  use the physical
C     location of the last double precision address, if there are any
C     double precision data in the file.  Otherwise, RECNO becomes the
C     first record available for double precision data.
C
      IF ( LASTD .GE. 1 ) THEN

         CALL DASA2L ( HANDLE,  DP,      LASTD,
     .                 CLBASE,  CLSIZE,  RECNO,  WORDNO )

      ELSE

         RECNO   =  FREE
         WORDNO  =  0

      END IF

C
C     Set the number of double precision words already written.  Keep
C     writing to the file until this number equals the number of
C     elements in DATA.
C
C     Note that if N is non-positive, the loop doesn't get exercised.
C
C
      NWRITN  =  0

      DO WHILE (  ( NWRITN .LT. N ) .AND. ( .NOT. FAILED() )  )

C
C        Write as much data as we can (or need to) into the current
C        record.  We assume that RECNO, WORDNO, and NWRITN have been
C        set correctly at this point.
C
C        Find out how many words to write into the current record.
C        There may be no space left in the current record.
C
         NUMDP  =  MIN (  N - NWRITN,   NWD - WORDNO )

         IF ( NUMDP .GT. 0 ) THEN

C
C           Write NUMDP words into the current record.  If the record
C           is new, write the entire record.  Otherwise, just update
C           the part we're interested in.
C
            IF ( WORDNO .EQ. 0 ) THEN

               CALL MOVED  (  DATA(NWRITN+1),  NUMDP,   RECORD  )
               CALL DASWRD (  HANDLE,          RECNO,   RECORD  )

            ELSE

               CALL DASURD (  HANDLE,
     .                        RECNO,
     .                        WORDNO + 1,
     .                        WORDNO + NUMDP,
     .                        DATA( NWRITN + 1 )   )

            END IF

            NWRITN  =  NWRITN + NUMDP
            WORDNO  =  WORDNO + NUMDP

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
C     double precision words.  DASCUD will also update the file summary
C     accordingly.
C
      CALL DASCUD ( HANDLE, DP, NWRITN )

      CALL CHKOUT ( 'DASADD' )
      RETURN
      END
