C$Procedure DASADI ( DAS, add data, integer )

      SUBROUTINE DASADI ( HANDLE, N, DATA )

C$ Abstract
C
C     Add an array of integers to a DAS file.
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
      INTEGER               DATA   ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     N          I   Number of integers to add to DAS file.
C     DATA       I   Array of integers to add.
C
C$ Detailed_Input
C
C     HANDLE   is a file handle of a DAS file opened for writing.
C
C     N        is the number of integer "words" to add to the DAS file
C              specified by HANDLE.
C
C     DATA     is an array of integers to be added to the specified DAS
C              file. Elements 1 through N are appended to the integer
C              data in the file.
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
C     This routine adds integer data to a DAS file by "appending" them
C     after any integer data already in the file. The sense in which
C     the data are "appended" is that the data will occupy a range of
C     logical addresses for integer data that immediately follow the
C     last logical address of a integer that is occupied at the time
C     this routine is called. The diagram below illustrates this
C     addition:
C
C        +-------------------------+
C        |    (already in use)     |  Integer logical address 1
C        +-------------------------+
C                    .
C                    .
C                    .
C        +-------------------------+
C        |    (already in use)     |  Last integer logical address
C        +-------------------------+  in use before call to DASADI
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
C     The logical organization of the integers in the DAS file is
C     independent of the location in the file of any data of double
C     precision or character type.
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
C     In order to update integer logical addresses that already contain
C     data, the SPICELIB routine DASUDI (DAS update data, integer)
C     should be used.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a new DAS file and add 200 integers to it. Close the
C        file, then re-open it and read the data back out.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASADI_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'dasadi_ex1.das' )
C
C              CHARACTER*(*)         TYPE
C              PARAMETER           ( TYPE  = 'TEST' )
C
C        C
C        C     Local variables.
C        C
C              INTEGER               DATA   ( 200 )
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
C        C     Fill the array DATA with the integers 1 through
C        C     100, and add this array to the file.
C        C
C              DO I = 1, 100
C                 DATA(I) = I
C              END DO
C
C              CALL DASADI ( HANDLE, 100, DATA )
C
C        C
C        C     Now append the array DATA to the file again.
C        C
C              CALL DASADI ( HANDLE, 100, DATA )
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
C              CALL DASRDI ( HANDLE, 1, 200, DATA )
C
C        C
C        C     Dump the data to the screen.  We should see the
C        C     sequence  1, 2, ..., 100, 1, 2, ... , 100.
C        C
C              WRITE (*,*)
C              WRITE (*,*) 'Data from "', FNAME, '":'
C              WRITE (*,*)
C              DO I = 1, 20
C                 WRITE (*,'(10I5)') (DATA((I-1)*10+J), J = 1, 10)
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
C         Data from "dasadi_ex1.das":
C
C            1    2    3    4    5    6    7    8    9   10
C           11   12   13   14   15   16   17   18   19   20
C           21   22   23   24   25   26   27   28   29   30
C           31   32   33   34   35   36   37   38   39   40
C           41   42   43   44   45   46   47   48   49   50
C           51   52   53   54   55   56   57   58   59   60
C           61   62   63   64   65   66   67   68   69   70
C           71   72   73   74   75   76   77   78   79   80
C           81   82   83   84   85   86   87   88   89   90
C           91   92   93   94   95   96   97   98   99  100
C            1    2    3    4    5    6    7    8    9   10
C           11   12   13   14   15   16   17   18   19   20
C           21   22   23   24   25   26   27   28   29   30
C           31   32   33   34   35   36   37   38   39   40
C           41   42   43   44   45   46   47   48   49   50
C           51   52   53   54   55   56   57   58   59   60
C           61   62   63   64   65   66   67   68   69   70
C           71   72   73   74   75   76   77   78   79   80
C           81   82   83   84   85   86   87   88   89   90
C           91   92   93   94   95   96   97   98   99  100
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
C-    SPICELIB Version 1.3.0, 07-OCT-2021 (JDR) (NJB)
C
C        Added IMPLICIT NONE statement. Updated the code to prevent
C        DASCUD from being called with a negative number of integer
C        words when the input count N is negative.
C
C        Made local variable RECORD a saved variable which is
C        initialized by a DATA statement.
C
C        Bug fix: added FAILED call after DASHFS call.
C
C        Edited the header to comply with NAIF standard. Fixed
C        bugs in the code example and modified the output presentation
C        to comply with the maximum line length for header comments.
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
C        Test of FAILED() added to loop termination conditions.
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
C     add integer data to a DAS file
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination condition. Without
C        this test, an infinite loop could result if DASA2L, DASURI or
C        DASWRI signaled an error inside the loop.
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
      INTEGER               INT
      PARAMETER           ( INT    =  3  )

      INTEGER               NWI
      PARAMETER           ( NWI    = 256 )

C
C     Local variables
C
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               FREE
      INTEGER               LASTI
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NUMINT
      INTEGER               NWRITN
      INTEGER               RECNO
      INTEGER               RECORD ( NWI )
      INTEGER               WORDNO

C
C     Saved variables
C
      SAVE                  RECORD

C
C     Initial values
C
      DATA                  RECORD / NWI * 0 /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASADI' )

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
         CALL CHKOUT ( 'DASADI' )
         RETURN
      END IF

      LASTI  =  LASTLA(INT)

C
C     We will keep track of the location that we wish to write to
C     with the variables RECNO and WORDNO.  RECNO will be the record
C     number of the record we'll write to; WORDNO will be the number
C     preceding the word index, within record number RECNO, that we'll
C     write to.  For example, if we're about to write to the first
C     integer in record 10, RECNO will be 10 and WORDNO will be 0.  Of
C     course, when WORDNO reaches NWI, we'll have to find a free record
C     before writing anything.
C
C     Prepare the variables RECNO and WORDNO:  use the physical
C     location of the last integer address, if there are any integer
C     data in the file.  Otherwise, RECNO becomes the first record
C     available for integer data.
C
      IF ( LASTI .GE. 1 ) THEN

         CALL DASA2L ( HANDLE,  INT,     LASTI,
     .                 CLBASE,  CLSIZE,  RECNO,  WORDNO )
      ELSE

         RECNO   =  FREE
         WORDNO  =  0

      END IF

C
C     Set the number of integer words already written.  Keep
C     writing to the file until this number equals the number of
C     elements in DATA.
C
C     Note that if N is non-positive, the loop doesn't get exercised.
C
C
      NWRITN  =  0

      DO WHILE  (  ( NWRITN .LT. N ) .AND. ( .NOT. FAILED() )  )
C
C        Write as much data as we can (or need to) into the current
C        record.  We assume that RECNO, WORDNO, and NWRITN have been
C        set correctly at this point.
C
C        Find out how many words to write into the current record.
C        There may be no space left in the current record.
C
         NUMINT  =  MIN (  N - NWRITN,   NWI - WORDNO )

         IF ( NUMINT .GT. 0 ) THEN
C
C           Write NUMINT words into the current record.  If the record
C           is new, write the entire record.  Otherwise, just update
C           the part we're interested in.
C
            IF ( WORDNO .EQ. 0 ) THEN

               CALL MOVEI  (  DATA(NWRITN+1),  NUMINT,  RECORD  )
               CALL DASWRI (  HANDLE,          RECNO,   RECORD  )

            ELSE

               CALL DASURI (  HANDLE,
     .                        RECNO,
     .                        WORDNO + 1,
     .                        WORDNO + NUMINT,
     .                        DATA(NWRITN + 1)  )

            END IF


            NWRITN  =  NWRITN + NUMINT
            WORDNO  =  WORDNO + NUMINT

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
C     integer words.  DASCUD will also update the file summary
C     accordingly.
C
      CALL DASCUD ( HANDLE, INT, NWRITN )

      CALL CHKOUT ( 'DASADI' )
      RETURN
      END
