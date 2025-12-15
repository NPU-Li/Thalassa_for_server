C$Procedure DASUDI ( DAS, update data, integer )

      SUBROUTINE DASUDI ( HANDLE, FIRST, LAST, DATA )

C$ Abstract
C
C     Update data in a specified range of integer addresses in a DAS
C     file.
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
      INTEGER               DATA   ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     FIRST,
C     LAST       I   Range of integer addresses to write to.
C     DATA       I   An array of integers.
C
C$ Detailed_Input
C
C     HANDLE   is a file handle of a DAS file opened for writing.
C
C     FIRST,
C     LAST     are the first and last of a range of DAS logical
C              addresses of integers to update. These addresses satisfy
C              the inequality
C
C                 1  <=   FIRST   <=   LAST   <=   LASTI
C
C              where LASTI is the last integer logical address in
C              use in the DAS file designated by HANDLE.
C
C     DATA     is an array of integers. The array elements
C              DATA(1) through DATA(N) will be written to the
C              indicated DAS file, where N is LAST - FIRST + 1.
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
C     1)  If the input file handle is invalid, an error is
C         signaled by a routine in the call tree of this routine.
C
C     2)  Only logical addresses that already contain data may be
C         updated: if either FIRST or LAST are outside the range
C
C            [ 1,  LASTI ]
C
C         where LASTI is the last integer logical address that
C         currently contains data in the indicated DAS file, the error
C         SPICE(INVALIDADDRESS) is signaled. The DAS file will not be
C         modified.
C
C     3)  If FIRST > LAST but both addresses are valid, this routine
C         will not modify the indicated DAS file. No error will be
C         signaled.
C
C     4)  If an I/O error occurs during the data update attempted
C         by this routine, the error is signaled by a routine in the
C         call tree of this routine.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine replaces the integer data in the specified range of
C     logical addresses within a DAS file with the contents of the
C     input array DATA.
C
C     The actual physical write operations that update the indicated
C     DAS file with the contents of the input array DATA might not take
C     place before this routine returns, since the DAS system buffers
C     data that is written as well as data that is read. In any case,
C     the data will be flushed to the file at the time the file is
C     closed, if not earlier. A physical write of all buffered
C     records can be forced by calling the SPICELIB routine DASWBR
C     (DAS, write buffered records).
C
C     In order to append integer data to a DAS file, filling in a range
C     of integer logical addresses that starts immediately after the
C     last integer logical address currently in use, the SPICELIB
C     routine DASADI (DAS add data, integer) should be used.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Write to addresses 1 through 200 in a DAS file in random-access
C        fashion by updating the file. Recall that data must be present
C        in the file before it can be updated.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASUDI_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'dasudi_ex1.das' )
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
C        C     Append 200 integers to the file; after the data are
C        C     present, we're free to update it in any order we
C        C     please.  (CLEARI zeros out an integer array.)
C        C
C              CALL CLEARI (           200,  DATA )
C              CALL DASADI (  HANDLE,  200,  DATA )
C
C        C
C        C     Now the integer logical addresses 1:200 can be
C        C     written to in random-access fashion.  We'll fill them
C        C     in reverse order.
C        C
C              DO I = 200, 1, -1
C                 CALL DASUDI ( HANDLE, I, I, I )
C              END DO
C
C        C
C        C     Close the file.
C        C
C              CALL DASCLS ( HANDLE )
C
C        C
C        C     Now make sure that we updated the file properly.
C        C     Open the file for reading and dump the contents
C        C     of the integer logical addresses 1:200.
C        C
C              CALL DASOPR ( FNAME, HANDLE )
C
C              CALL CLEARI (              200,  DATA  )
C              CALL DASRDI (  HANDLE,  1, 200,  DATA  )
C
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
C         Data from "dasudi_ex1.das":
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
C          101  102  103  104  105  106  107  108  109  110
C          111  112  113  114  115  116  117  118  119  120
C          121  122  123  124  125  126  127  128  129  130
C          131  132  133  134  135  136  137  138  139  140
C          141  142  143  144  145  146  147  148  149  150
C          151  152  153  154  155  156  157  158  159  160
C          161  162  163  164  165  166  167  168  169  170
C          171  172  173  174  175  176  177  178  179  180
C          181  182  183  184  185  186  187  188  189  190
C          191  192  193  194  195  196  197  198  199  200
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
C-    SPICELIB Version 1.3.0, 16-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Updated entries in $Revisions section.
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
C        new routine DASONW, open new for write, which makes use of the
C        file type. Also,  a variable for the type of the file to be
C        created was added.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
C
C-&


C$ Index_Entries
C
C     update integer data in a DAS file
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination condition. Without
C        this test, an infinite loop could result if DASA2L or DASURI
C        signaled an error inside the loop.
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
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
      INTEGER               N
      INTEGER               NUMINT
      INTEGER               NWRITN
      INTEGER               RECNO
      INTEGER               WORDNO



C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASUDI' )
      END IF

C
C     Get the last logical addresses in use in this DAS file.
C
      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )

C
C     Validate the input addresses.
C
      IF (      ( FIRST .LT. 1     )
     .     .OR. ( FIRST .GT. LASTI )
     .     .OR. ( LAST  .LT. 1     )
     .     .OR. ( LAST  .GT. LASTI )  ) THEN

         CALL SETMSG ( 'FIRST was #. LAST was #. Valid range is [1,#].')
         CALL ERRINT ( '#',   FIRST                                    )
         CALL ERRINT ( '#',   LAST                                     )
         CALL ERRINT ( '#',   LASTI                                    )
         CALL SIGERR ( 'SPICE(INVALIDADDRESS)'                         )
         CALL CHKOUT ( 'DASUDI'                                        )
         RETURN

      END IF

C
C     Let N be the number of addresses to update.
C
      N  =  LAST - FIRST + 1

C
C     We will use the variables RECNO and WORDNO to determine where to
C     write data in the DAS file.  RECNO will be the record containing
C     the physical location to write to;  WORDNO will be the word
C     location that we will write to next.
C
C     Find the first location to write to.  CLBASE and CLSIZE are the
C     base record number and size of the cluster of integer records that
C     the address FIRST lies within.
C
      CALL DASA2L ( HANDLE, INT, FIRST, CLBASE, CLSIZE, RECNO, WORDNO )

C
C     Set the number of integer words already written.  Keep
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
C        record.  We assume that CLBASE, RECNO, WORDNO, and NWRITN have
C        been set correctly at this point.
C
C        Find out how many words to write into the current record.
C        There may be no space left in the current record.
C
         NUMINT  =  MIN (  N - NWRITN,   NWI - WORDNO + 1  )

         IF ( NUMINT .GT. 0 ) THEN
C
C           Write NUMINT words into the current record.
C
            CALL DASURI ( HANDLE,
     .                    RECNO,
     .                    WORDNO,
     .                    WORDNO + NUMINT - 1,
     .                    DATA( NWRITN + 1 )   )

            NWRITN  =  NWRITN + NUMINT
            WORDNO  =  WORDNO + NUMINT

         ELSE
C
C           It's time to start on a new record.  If the record we
C           just finished writing to (or just attempted writing to,
C           if it was full) was not the last of the cluster, the next
C           record to write to is the immediate successor of the last
C           one.  Otherwise, we'll have to look up the location of the
C           next integer logical address.
C
            IF (  RECNO  .LT.  ( CLBASE + CLSIZE - 1 )  ) THEN

               RECNO   =  RECNO + 1
               WORDNO  =  1

            ELSE

               CALL DASA2L ( HANDLE,
     .                       INT,
     .                       FIRST + NWRITN,
     .                       CLBASE,
     .                       CLSIZE,
     .                       RECNO,
     .                       WORDNO               )
            END IF

         END IF

      END DO


      CALL CHKOUT ( 'DASUDI' )
      RETURN
      END
