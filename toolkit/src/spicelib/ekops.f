C$Procedure EKOPS ( EK, open scratch file )

      SUBROUTINE EKOPS ( HANDLE )

C$ Abstract
C
C     Open a scratch (temporary) E-kernel file and prepare the file
C     for writing.
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
C     EK
C
C$ Keywords
C
C     EK
C     FILES
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'ektype.inc'
      INCLUDE 'ekfilpar.inc'

      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     O   File handle attached to new EK file.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     HANDLE   is the EK file handle of the file opened by this
C              routine. This handle is used to identify the file
C              to other EK routines.
C
C$ Parameters
C
C     FTSIZE   is the maximum number of DAS files that a user can
C              have open simultaneously. This includes any files used
C              by the DAS system.
C
C              See the include file das.inc for the actual value of
C              this parameter.
C
C$ Exceptions
C
C     1)  If the indicated file cannot be opened, an error is signaled
C         by a routine in the call tree of this routine. The new file
C         will be deleted.
C
C     2)  If an I/O error occurs while reading or writing the indicated
C         file, the error is signaled by a routine in the call tree of
C         this routine.
C
C$ Files
C
C     This routine creates a temporary EK file; the file is deleted
C     when the calling program terminates or when the file is closed
C     using the SPICELIB routine EKCLS.
C
C     See the EK Required Reading ek.req for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine operates by side effects: it opens and prepares
C     an EK for addition of data. "Scratch" files are automatically
C     deleted when the calling program terminates normally or when
C     closed using the SPICELIB routine EKCLS.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose we want to create an E-kernel which contains a table
C        of items that have been ordered but we do not want to keep
C        the file. The columns of this table are shown below:
C
C           DATAITEMS
C
C              Column Name     Data Type
C              -----------     ---------
C              ITEM_ID         INTEGER
C              ORDER_ID        INTEGER
C              ITEM_NAME       CHARACTER*(*)
C              DESCRIPTION     CHARACTER*(*)
C              PRICE           DOUBLE PRECISION
C
C
C        This examples demonstrates how to open a scratch EK file;
C        create the segment described above, how to insert a new record
C        into it, and how to summarize its contents.
C
C
C        Example code begins here.
C
C
C              PROGRAM EKOPS_EX1
C              IMPLICIT NONE
C
C        C
C        C     Include the EK Column Name Size (CNAMSZ); the maximum
C        C     length of an input query (MAXQRY), the maximum number of
C        C     columns per segment (MXCLSG); and the maximum length of
C        C     a table name (TNAMSZ).
C        C
C              INCLUDE 'ekcnamsz.inc'
C              INCLUDE 'ekglimit.inc'
C              INCLUDE 'ekqlimit.inc'
C              INCLUDE 'ektnamsz.inc'
C
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               EKNSEG
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         TABLE
C              PARAMETER           ( TABLE   = 'DATAITEMS'      )
C
C              INTEGER               DECLEN
C              PARAMETER           ( DECLEN = 200 )
C
C              INTEGER               DESCLN
C              PARAMETER           ( DESCLN = 80  )
C
C        C
C        C     One value per row/column element.
C        C
C              INTEGER               MAXVAL
C              PARAMETER           ( MAXVAL = 1  )
C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 40  )
C
C              INTEGER               COLSLN
C              PARAMETER           ( COLSLN = 5   )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(DECLEN)    CDECLS ( MXCLSG )
C              CHARACTER*(CNAMSZ)    CNAMES ( MXCLSG )
C              CHARACTER*(NAMLEN)    CVALS  ( MAXVAL )
C              CHARACTER*(DESCLN)    DESCRP
C              CHARACTER*(4)         DTYPES ( MXCLSG )
C              CHARACTER*(NAMLEN)    ITEMNM
C              CHARACTER*(TNAMSZ)    TABNAM
C
C              DOUBLE PRECISION      DVALS  ( MAXVAL )
C              DOUBLE PRECISION      PRICE
C
C              INTEGER               ESIZE
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               ITEMID
C              INTEGER               IVALS  ( MAXVAL )
C              INTEGER               NCOLS
C              INTEGER               NROWS
C              INTEGER               NSEG
C              INTEGER               NVALS
C              INTEGER               ORDID
C              INTEGER               RECNO
C              INTEGER               SEGNO
C              INTEGER               SIZES  ( MXCLSG )
C              INTEGER               STRLNS ( MXCLSG )
C
C              LOGICAL               INDEXD ( MXCLSG )
C              LOGICAL               ISNULL
C              LOGICAL               NULLOK ( MXCLSG )
C
C        C
C        C     Open a scratch EK file to use for temporary
C        C     storage.
C        C
C              CALL EKOPS ( HANDLE )
C
C        C
C        C     Set up the table and column names and declarations
C        C     for the DATAITEMS segment. We'll index all of
C        C     the columns. All columns are scalar, so we omit
C        C     the size declaration.
C        C
C              CNAMES(1) =  'ITEM_ID'
C              CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE'
C
C              CNAMES(2) =  'ORDER_ID'
C              CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE'
C
C              CNAMES(3) =  'ITEM_NAME'
C              CDECLS(3) =  'DATATYPE = CHARACTER*(*),' //
C             .             'INDEXED  = TRUE'
C
C              CNAMES(4) =  'DESCRIPTION'
C              CDECLS(4) =  'DATATYPE = CHARACTER*(*),' //
C             .             'INDEXED  = TRUE'
C
C              CNAMES(5) =  'PRICE'
C              CDECLS(5) =  'DATATYPE = DOUBLE PRECISION,' //
C             .             'INDEXED  = TRUE'
C
C
C        C
C        C     Start the segment. Since we have no data for this
C        C     segment, start the segment by just defining the new
C        C     segment's schema.
C        C
C              CALL EKBSEG ( HANDLE, TABLE,  COLSLN,
C             .              CNAMES, CDECLS, SEGNO  )
C
C        C
C        C     Append a new, empty record to the DATAITEMS
C        C     table. Recall that the DATAITEMS table
C        C     is in the first segment.  The call will return
C        C     the number of the new, empty record.
C        C
C              SEGNO = 1
C              CALL EKAPPR ( HANDLE, SEGNO, RECNO )
C
C        C
C        C     At this point, the new record is empty. We fill in the
C        C     data here. Data items are filled in one column at a
C        C     time. The order in which the columns are filled in is
C        C     not important.  We use the different add column entry
C        C     routines to fill in column entries.  We'll assume
C        C     that no entries are null. All entries are scalar,
C        C     so the entry size is 1.
C        C
C              ISNULL   =  .FALSE.
C              ESIZE    =  1
C
C        C
C        C     The following variables will contain the data for
C        C     the new record.
C        C
C              ORDID    =   10011
C              ITEMID   =   531
C              ITEMNM   =  'Sample item'
C              DESCRP   =  'This sample item is used only in tests.'
C              PRICE    =   1345.67D0
C
C        C
C        C     Note that the names of the routines called
C        C     correspond to the data types of the columns.
C        C
C              CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'ORDER_ID',
C             .              ESIZE,  ORDID,  ISNULL               )
C
C              CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'ITEM_ID',
C             .              ESIZE,  ITEMID, ISNULL               )
C
C              CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'ITEM_NAME',
C             .              ESIZE,  ITEMNM, ISNULL               )
C
C              CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'DESCRIPTION',
C             .              ESIZE,  DESCRP, ISNULL               )
C
C              CALL EKACED ( HANDLE, SEGNO,  RECNO, 'PRICE',
C             .              ESIZE,  PRICE,  ISNULL               )
C
C
C        C
C        C     At this point, we could perform read operations
C        C     on the EK.
C        C
C        C     Return the number of segments in the EK. Dump the
C        C     desired summary information for each one.
C        C
C              NSEG = EKNSEG( HANDLE )
C              WRITE(*,'(A,I3)') 'Number of segments =', NSEG
C              WRITE(*,*)
C
C              DO SEGNO = 1, NSEG
C
C                 CALL EKSSUM (  HANDLE,  SEGNO,   TABNAM,  NROWS,
C             .                  NCOLS,   CNAMES,  DTYPES,  SIZES,
C             .                  STRLNS,  INDEXD,  NULLOK         )
C
C                 WRITE(*,'(2A)')   'Table containing segment: ', TABNAM
C                 WRITE(*,'(A,I2)') 'Number of rows          : ', NROWS
C                 WRITE(*,'(A,I2)') 'Number of columns       : ', NCOLS
C                 WRITE(*,'(A)')    'Table data              : '
C
C                 DO I = 1, NCOLS
C
C                    WRITE(*,'(2A)') '  Column: ', CNAMES(I)
C                    WRITE(*,'(2A)') '  Type  : ', DTYPES(I)
C
C                    DO RECNO = 1, NROWS
C
C                       IF ( DTYPES(I) .EQ. 'CHR' ) THEN
C
C                          CALL EKRCEC ( HANDLE,    SEGNO, RECNO,
C             .                          CNAMES(I), NVALS,
C             .                          CVALS,     ISNULL       )
C
C                          IF ( ISNULL ) THEN
C
C                             WRITE(*,'(A)') '  Data  : <null>'
C
C                          ELSE
C
C                             WRITE(*,'(2A)') '  Data  : ', CVALS
C
C                          END IF
C
C                       ELSE IF ( DTYPES(I) .EQ. 'DP' ) THEN
C
C                          CALL EKRCED ( HANDLE,    SEGNO, RECNO,
C             .                          CNAMES(I), NVALS,
C             .                          DVALS,     ISNULL       )
C
C                          IF ( ISNULL ) THEN
C
C                             WRITE(*,'(A)') '  Data  : <null>'
C
C                          ELSE
C
C                             WRITE(*,'(A,F9.2)') '  Data  : ', DVALS
C
C                          END IF
C
C                       ELSE IF ( DTYPES(I) .EQ. 'INT' ) THEN
C
C                          CALL EKRCEI ( HANDLE,    SEGNO, RECNO,
C             .                          CNAMES(I), NVALS,
C             .                          IVALS,     ISNULL       )
C
C                          IF ( ISNULL ) THEN
C
C                             WRITE(*,'(A)') '  Data  : <null>'
C
C                          ELSE
C
C                             WRITE(*,'(A,I6)') '  Data  : ', IVALS
C
C                          END IF
C
C                       ENDIF
C
C        C
C        C              There is no time data. Otherwise, we would need
C        C              to use an LSK and EKRCED to read it
C        C              (internally, it is stored as double precision).
C        C
C                       WRITE(*,*)
C
C                    END DO
C
C                 END DO
C
C                 WRITE(*,*) '----------------------------------------'
C
C              END DO
C
C        C
C        C     Close the file. This will delete the scratch file
C        C     and all the data will be lost.
C        C
C              CALL EKCLS ( HANDLE )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Number of segments =  1
C
C        Table containing segment: DATAITEMS
C        Number of rows          :  1
C        Number of columns       :  5
C        Table data              :
C          Column: ITEM_ID
C          Type  : INT
C          Data  :    531
C
C          Column: ORDER_ID
C          Type  : INT
C          Data  :  10011
C
C          Column: ITEM_NAME
C          Type  : CHR
C          Data  : Sample item
C
C          Column: DESCRIPTION
C          Type  : CHR
C          Data  : This sample item is used only in tests.
C
C          Column: PRICE
C          Type  : DP
C          Data  :   1345.67
C
C         ----------------------------------------
C
C
C        Note that after run completion, there is no EK file in the
C        output directory as scratch files are deleted when they are
C        closed or when the calling program terminates.
C
C$ Restrictions
C
C     1)  No more than FTSIZE DAS files may be opened simultaneously.
C         See the include file das.inc for the value of FTSIZE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard and improved
C        the API documentation. Added complete code example and updated
C        $Parameters section.
C
C-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     open scratch E-kernel
C     open scratch EK
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local variables
C
      INTEGER               BASE
      INTEGER               P

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKOPS' )
      END IF

      CALL DASOPS ( HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKOPS' )
         RETURN
      END IF

C
C     Initialize the file for paged access.  The EK architecture
C     code is automatically set by the paging initialization routine.
C
      CALL ZZEKPGIN ( HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKOPS' )
         RETURN
      END IF

C
C     Allocate the first integer page for the file's metadata.  We
C     don't need to examine the page number; it's 1.
C
      CALL ZZEKPGAN ( HANDLE, INT, P, BASE )

C
C     Initialize a new tree.  This tree will point to the file's
C     segments.
C
      CALL ZZEKTRIT ( HANDLE, P )

C
C     Save the segment pointer's root page number.
C
      CALL DASUDI ( HANDLE, BASE+SGTIDX, BASE+SGTIDX, P )

C
C     That's it.  We're ready to add data to the file.
C
      CALL CHKOUT ( 'EKOPS' )
      RETURN
      END
