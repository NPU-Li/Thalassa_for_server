C$Procedure DASRDD ( DAS, read data, double precision )

      SUBROUTINE DASRDD ( HANDLE, FIRST, LAST, DATA )

C$ Abstract
C
C     Read double precision data from a range of DAS logical addresses.
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
      DOUBLE PRECISION      DATA   ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     FIRST,
C     LAST       I   Bounds of range of DAS double precision logical
C                    addresses.
C     DATA       O   Data having addresses FIRST through LAST.
C
C$ Detailed_Input
C
C     HANDLE   is a file handle for an open DAS file.
C
C     FIRST,
C     LAST     are the lower and upper bounds of a range of DAS double
C              precision logical addresses. The range includes these
C              bounds. FIRST and LAST must be greater than or equal to 1
C              and less than or equal to the highest double precision
C              DAS address in the DAS file designated by HANDLE.
C
C$ Detailed_Output
C
C     DATA     is an array of double precision numbers. DATA
C              should have length at least LAST - FIRST + 1.
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
C     This routine provides random read access to the double precision
C     data in a DAS file. This data are logically structured as a
C     one-dimensional array of double precision numbers.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a new DAS file TEST.DAS and add 200 double
C        precision numbers to it. Close the file, then re-open
C        it and read the data back out.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASRDD_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'dasrdd_ex1.das' )
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
C        C     Dump the data to the screen.  We should see the
C        C     sequence 1.0, 2.0, ..., 100.0, 1.0, 2.0, ..., 100.0.
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
C         Data from "dasrdd_ex1.das":
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
C-    SPICELIB Version 1.2.0, 01-NOV-1995 (NJB)
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
C     read double precision data from a DAS file
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB)
C
C        Test of FAILED() added to loop termination condition. Without
C        this test, an infinite loop could result if DASA2L or DASRRD
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
      INTEGER               NWD
      PARAMETER           ( NWD   =  128 )

      INTEGER               DP
      PARAMETER           ( DP     =  2  )


C
C     Local variables
C
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               N
      INTEGER               NREAD
      INTEGER               NUMDP
      INTEGER               RECNO
      INTEGER               WORDNO



C
C     Find out the physical location of the first double precision
C     number.  If FIRST is invalid, DASA2L will take care of the
C     problem.
C
      CALL DASA2L ( HANDLE, DP, FIRST, CLBASE, CLSIZE, RECNO, WORDNO )

      IF ( FAILED() ) THEN
         RETURN
      END IF

C
C     Decide how many double precision numbers to read.
C
      NUMDP = LAST - FIRST + 1
      NREAD = 0

C
C     Read as much data from record RECNO as necessary.
C
      N   =   MIN ( NUMDP,  NWD - WORDNO + 1 )

      CALL DASRRD ( HANDLE, RECNO, WORDNO, WORDNO + N-1, DATA )

      NREAD  =  N
      RECNO  =  RECNO + 1

C
C     Read from as many additional records as necessary.
C
      DO WHILE ( NREAD .LT. NUMDP )

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
            N  =  MIN ( NUMDP - NREAD,  NWD )

            CALL DASRRD ( HANDLE, RECNO, 1, N, DATA(NREAD + 1) )

            NREAD   =   NREAD + N
            RECNO   =   RECNO + 1

         ELSE
C
C           We must find the next double precision cluster to
C           read from.  The first double precision number in this
C           cluster has address FIRST + NREAD.
C
            CALL DASA2L ( HANDLE,
     .                    DP,
     .                    FIRST + NREAD,
     .                    CLBASE,
     .                    CLSIZE,
     .                    RECNO,
     .                    WORDNO  )

         END IF

      END DO

      RETURN
      END
