C$Procedure DASRDI ( DAS, read data, integer )

      SUBROUTINE DASRDI ( HANDLE, FIRST, LAST, DATA )

C$ Abstract
C
C     Read integer data from a range of DAS logical addresses.
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
C     LAST       I   Bounds of range of DAS integer logical addresses.
C     DATA       O   Data having addresses FIRST through LAST.
C
C$ Detailed_Input
C
C     HANDLE   is a file handle for an open DAS file.
C
C     FIRST,
C     LAST     are the lower and upper bounds of a range of DAS integer
C              logical addresses. The range includes these bounds. FIRST
C              and LAST must be greater than or equal to 1 and less than
C              or equal to the highest integer DAS address in the DAS
C              file designated by HANDLE.
C
C$ Detailed_Output
C
C     DATA     is an array of integers. DATA should have length
C              at least LAST - FIRST + 1.
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
C     2)  If FIRST or LAST are out of range, an error is signaled
C         by a routine in the call tree of this routine.
C
C     3)  If FIRST is greater than LAST, DATA is left unchanged.
C
C     4)  If DATA is declared with length less than FIRST - LAST + 1,
C         the error cannot be diagnosed by this routine.
C
C     5)  If a file read error occurs, the error is signaled by a
C         routine in the call tree of this routine.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine provides random read access to the integer data in
C     a DAS file. This data are logically structured as a
C     one-dimensional array of integers.
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
C              PROGRAM DASRDI_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'dasrdi_ex1.das' )
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
C         Data from "dasrdi_ex1.das":
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
C-    SPICELIB Version 1.3.0, 09-OCT-2021 (JDR) (NJB)
C
C        Added IMPLICIT NONE statement.
C
C        Added FAILED call following DASA2L call.
C
C        Updated entries in $Revisions section.
C
C        Edited the header to comply with NAIF standard. Fixed
C        bugs in the code example and modified the output presentation
C        to comply with the maximum line length for header comments.
C
C        Added entry #5 to $Exceptions section.
C
C-    SPICELIB Version 1.2.1, 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    SPICELIB Version 1.2.0, 30-OCT-1995 (NJB)
C
C        Routine now uses discovery check-in. FAILED test moved inside
C        loop.
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
C        new routine DASONW, open new for write, which makes use of the
C        file type. Also,  a variable for the type of the file to be
C        created was added.
C
C-    SPICELIB Version 1.0.0, 13-JUN-1992 (NJB) (WLT)
C
C-&


C$ Index_Entries
C
C     read integer data from a DAS file
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination condition. Without
C        this test, an infinite loop could result if DASA2L or DASRRI
C        signaled an error inside the loop.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED

C
C     Local parameters
C
      INTEGER               NWI
      PARAMETER           ( NWI    =  256 )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )

C
C     Local variables
C
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               N
      INTEGER               NREAD
      INTEGER               NUMINT
      INTEGER               RECNO
      INTEGER               WORDNO



C
C     Find out the physical location of the first integer.  If FIRST
C     is invalid, DASA2L will take care of the problem.
C
      CALL DASA2L ( HANDLE, INT, FIRST, CLBASE, CLSIZE, RECNO, WORDNO )

      IF ( FAILED() ) THEN
         RETURN
      END IF

C
C     Decide how many integers to read.
C
      NUMINT = LAST - FIRST + 1
      NREAD  = 0

C
C     Read as much data from record RECNO as necessary.
C
      N  =  MIN ( NUMINT,  NWI - WORDNO + 1 )

      CALL DASRRI ( HANDLE, RECNO, WORDNO, WORDNO + N-1, DATA )

      NREAD  =  N
      RECNO  =  RECNO + 1

C
C     Read from as many additional records as necessary.
C
      DO WHILE ( NREAD .LT. NUMINT )

         IF ( FAILED() ) THEN
            RETURN
         END IF

C
C        At this point, RECNO is the correct number of the
C        record to read from next.  CLBASE is the number
C        of the first record of the cluster we're about
C        to read from.
C
         IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN
C
C           We can continue reading from the current
C           cluster.
C
            N  =  MIN ( NUMINT - NREAD,  NWI )

            CALL DASRRI ( HANDLE, RECNO, 1, N, DATA(NREAD + 1) )

            NREAD   =   NREAD + N
            RECNO   =   RECNO + 1

         ELSE
C
C           We must find the next integer cluster to
C           read from.  The first integer in this
C           cluster has address FIRST + NREAD.
C
            CALL DASA2L ( HANDLE,
     .                    INT,
     .                    FIRST + NREAD,
     .                    CLBASE,
     .                    CLSIZE,
     .                    RECNO,
     .                    WORDNO  )

         END IF

      END DO

      RETURN
      END
