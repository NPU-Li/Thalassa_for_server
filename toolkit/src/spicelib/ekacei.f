C$Procedure EKACEI ( EK, add integer data to column )

      SUBROUTINE EKACEI (  HANDLE,  SEGNO,  RECNO,  COLUMN,
     .                     NVALS,   IVALS,  ISNULL          )

C$ Abstract
C
C     Add data to an integer column in a specified EK record.
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

      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RECNO
      CHARACTER*(*)         COLUMN
      INTEGER               NVALS
      INTEGER               IVALS  ( * )
      LOGICAL               ISNULL

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   EK file handle.
C     SEGNO      I   Index of segment containing record.
C     RECNO      I   Record to which data is to be added.
C     COLUMN     I   Column name.
C     NVALS      I   Number of values to add to column.
C     IVALS      I   Integer values to add to column.
C     ISNULL     I   Flag indicating whether column entry is null.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of an EK file open for write access.
C
C     SEGNO    is the index of the segment to which data is to
C              be added.
C
C     RECNO    is the index of the record to which data is to be
C              added. This record number is relative to the start
C              of the segment indicated by SEGNO; the first
C              record in the segment has index 1.
C
C     COLUMN   is the name of the column to which data is to be
C              added.
C
C     NVALS,
C     IVALS    are, respectively, the number of values to add to
C              the specified column and the set of values
C              themselves. The data values are written into the
C              specified column and record.
C
C              If the column has fixed-size entries, then NVALS
C              must equal the entry size for the specified column.
C
C              Only one value can be added to a virtual column.
C
C
C     ISNULL   is a logical flag indicating whether the entry is
C              null. If ISNULL is .FALSE., the column entry
C              defined by NVALS and IVALS is added to the
C              specified kernel file.
C
C              If ISNULL is .TRUE., NVALS and IVALS are ignored.
C              The contents of the column entry are undefined.
C              If the column has fixed-length, variable-size
C              entries, the number of entries is considered to
C              be 1.
C
C$ Detailed_Output
C
C     None. See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, an error is signaled by a routine in the
C         call tree of this routine.
C
C     2)  If SEGNO is out of range, an error is signaled by a routine in
C         the call tree of this routine.
C
C     3)  If COLUMN is not the name of a declared column, an error
C         is signaled by a routine in the call tree of this routine.
C
C     4)  If COLUMN specifies a column of whose data type is not
C         integer, the error SPICE(WRONGDATATYPE) is signaled.
C
C     5)  If RECNO is out of range, an error is signaled by a routine in
C         the call tree of this routine.
C
C     6)  If the specified column has fixed-size entries and NVALS does
C         not match this size, an error is signaled by a routine in the
C         call tree of this routine.
C
C     7)  If the specified column has variable-size entries and NVALS is
C         non-positive, an error is signaled by a routine in the call
C         tree of this routine.
C
C     8)  If an attempt is made to add a null value to a column that
C         doesn't take null values, an error is signaled by a routine in
C         the call tree of this routine.
C
C     9)  If COLUMN specifies a column of whose class is not
C         an character class known to this routine, the error
C         SPICE(NOCLASS) is signaled.
C
C     10) If an I/O error occurs while reading or writing the indicated
C         file, the error is signaled by a routine in the call tree of
C         this routine.
C
C$ Files
C
C     See the EK Required Reading ek.req for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine operates by side effects: it modifies the named
C     EK file by adding data to the specified record in the specified
C     column. Data may be added to a segment in random order; it is not
C     necessary to fill in columns or rows sequentially. Data may only
C     be added one column entry at a time.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) This example demonstrates how to add integer values
C        to a column in three different cases: single values,
C        variable-size arrays and static-size arrays.
C
C        Create an EK that contains a table TAB that has the following
C        columns:
C
C           Column name   Data Type   Size
C           -----------   ---------   ----
C           INT_COL_1     INT         1
C           INT_COL_2     INT         VARIABLE
C           INT_COL_3     INT         3
C
C
C        Issue the following query
C
C            QUERY = 'SELECT INT_COL_1, INT_COL2, INT_COL3 FROM TAB'
C
C        to fetch and dump column values from the rows that satisfy the
C        query.
C
C
C        Example code begins here.
C
C
C              PROGRAM EKACEI_EX1
C              IMPLICIT NONE
C
C        C
C        C     Include the EK Column Name Size (CNAMSZ)
C        C     and EK Query Limit Parameters (MAXQRY)
C        C
C              INCLUDE 'ekcnamsz.inc'
C              INCLUDE 'ekqlimit.inc'
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         EKNAME
C              PARAMETER           ( EKNAME  = 'ekacei_ex1.bdb' )
C
C              CHARACTER*(*)         TABLE
C              PARAMETER           ( TABLE   = 'TAB' )
C
C              INTEGER               COL3SZ
C              PARAMETER           ( COL3SZ = 3   )
C
C              INTEGER               DECLEN
C              PARAMETER           ( DECLEN = 200 )
C
C              INTEGER               ERRLEN
C              PARAMETER           ( ERRLEN = 1840 )
C
C              INTEGER               MXC2SZ
C              PARAMETER           ( MXC2SZ = 4   )
C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 40  )
C
C              INTEGER               NCOLS
C              PARAMETER           ( NCOLS  = 3   )
C
C              INTEGER               NROWS
C              PARAMETER           ( NROWS  = 4   )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(DECLEN)    CDECLS ( NCOLS  )
C              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS  )
C              CHARACTER*(ERRLEN)    ERRMSG
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(MAXQRY)    QUERY
C
C              INTEGER               COL1
C              INTEGER               COL2   ( MXC2SZ )
C              INTEGER               COL3   ( COL3SZ )
C              INTEGER               ELTIDX
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               IVALS  ( MXC2SZ )
C              INTEGER               J
C              INTEGER               NELT
C              INTEGER               NMROWS
C              INTEGER               NRESVC
C              INTEGER               RECNO
C              INTEGER               ROW
C              INTEGER               SEGNO
C              INTEGER               SELIDX
C
C              LOGICAL               ERROR
C              LOGICAL               FOUND
C              LOGICAL               ISNULL
C
C        C
C        C     Open a new EK file.  For simplicity, we will not
C        C     reserve any space for the comment area, so the
C        C     number of reserved comment characters is zero.
C        C     The variable IFNAME is the internal file name.
C        C
C              NRESVC  =  0
C              IFNAME  =  'Test EK/Created 13-JUN-2019'
C
C              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
C
C        C
C        C     Set up the column names and declarations
C        C     for the TAB segment.  We'll index all of
C        C     the columns.
C        C
C              CNAMES(1) = 'INT_COL_1'
C              CDECLS(1) = 'DATATYPE = INTEGER, INDEXED  = TRUE'
C
C              CNAMES(2) = 'INT_COL_2'
C              CDECLS(2) = 'DATATYPE = INTEGER, SIZE = VARIABLE, ' //
C             .            'NULLS_OK = TRUE'
C
C              CNAMES(3) = 'INT_COL_3'
C              CDECLS(3) = 'DATATYPE = INTEGER, SIZE = 3'
C
C        C
C        C     Start the segment.
C        C
C              CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
C             .              CNAMES, CDECLS, SEGNO )
C
C        C
C        C     At the records to the table.
C        C
C              DO I = 1, NROWS
C
C        C
C        C        Append a new record to the EK.
C        C
C                 CALL EKAPPR ( HANDLE, SEGNO, RECNO )
C
C        C
C        C        Add INT_COL_1
C        C
C                 COL1 = I * 100
C
C                 CALL EKACEI ( HANDLE,    SEGNO, RECNO,
C             .                 CNAMES(1), 1,     COL1,  .FALSE. )
C
C        C
C        C        Add I items to INT_COL_2
C        C
C                 DO J = 1, I
C                    COL2(J) = J + I*200
C                 END DO
C
C                 ISNULL = ( I .EQ. 2 )
C
C                 CALL EKACEI ( HANDLE,    SEGNO, RECNO,
C             .                 CNAMES(2), I,     COL2,  ISNULL )
C
C        C
C        C        Add 3 items to INT_COL_3
C        C
C                 DO J = 1, 3
C                    COL3(J) =  I + J*100.D0
C                 END DO
C
C                 CALL EKACEI ( HANDLE,    SEGNO, RECNO,
C             .                 CNAMES(3), 3,     COL3, .FALSE. )
C
C              END DO
C
C        C
C        C     Close the file.
C        C
C              CALL EKCLS ( HANDLE )
C
C        C
C        C     Open the created file. Perform the query and show the
C        C     results.
C        C
C              CALL FURNSH ( EKNAME )
C
C              QUERY = 'SELECT INT_COL_1, INT_COL_2, INT_COL_3 '
C             .   //   'FROM TAB'
C
C        C
C        C     Query the EK system for data rows matching the
C        C     SELECT constraints.
C        C
C              CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
C
C        C
C        C     Check whether an error occurred while processing the
C        C     SELECT clause. If so, output the error message.
C        C
C              IF ( ERROR ) THEN
C
C                 WRITE(*,*) 'SELECT clause error: ', ERRMSG
C
C              ELSE
C
C                 DO ROW = 1, NMROWS
C
C                    WRITE(*,*) ' '
C                    WRITE(*,'(A,I3)') 'ROW  = ', ROW
C
C        C
C        C           Fetch values from column INT_COL_1.  Since
C        C           INT_COL_1 was the first column selected, the
C        C           selection index SELIDX is set to 1.
C        C
C                    SELIDX = 1
C                    ELTIDX = 1
C                    CALL EKGI ( SELIDX,    ROW,     ELTIDX,
C             .                  IVALS(1),  ISNULL,  FOUND   )
C
C                    IF ( ISNULL ) THEN
C
C                       WRITE(*,*) '  COLUMN = INT_COL_1: <Null>'
C
C                    ELSE
C
C                       WRITE(*,'(A,I6)') '   COLUMN = INT_COL_1:',
C             .                           IVALS(1)
C
C                    END IF
C
C        C
C        C           Fetch values from column INT_COL_2 in the current
C        C           row.  Since INT_COL_2 contains variable-size array
C        C           elements, we call EKNELT to determine how many
C        C           elements to fetch.
C        C
C                    SELIDX = 2
C                    CALL EKNELT ( SELIDX, ROW, NELT )
C
C                    ELTIDX = 1
C                    ISNULL = .FALSE.
C
C                    DO WHILE (       ( ELTIDX .LE.  NELT   )
C             .                 .AND. (        .NOT. ISNULL )  )
C
C                       CALL EKGI ( SELIDX,         ROW,     ELTIDX,
C             .                     IVALS(ELTIDX),  ISNULL,  FOUND   )
C
C                       ELTIDX = ELTIDX + 1
C
C        C
C        C           If the column entry is null, we'll be kicked
C        C           out of this loop after the first iteration.
C        C
C                    END DO
C
C                    IF ( ISNULL ) THEN
C
C                       WRITE(*,*) '  COLUMN = INT_COL_2: <Null>'
C
C                    ELSE
C
C                       WRITE(*,'(A,4I6)') '   COLUMN = INT_COL_2:',
C             .                            ( IVALS(I), I = 1, NELT )
C
C                    END IF
C
C        C
C        C           Fetch values from column INT_COL_3 in the current
C        C           row.  We need not call EKNELT since we know how
C        C           many elements are in each column entry.
C        C
C                    SELIDX = 3
C                    ELTIDX = 1
C                    ISNULL = .FALSE.
C
C                    DO WHILE (       ( ELTIDX .LE.  COL3SZ )
C             .                 .AND. (        .NOT. ISNULL )  )
C
C                       CALL EKGI ( SELIDX,         ROW,     ELTIDX,
C             .                     IVALS(ELTIDX),  ISNULL,  FOUND   )
C
C                       ELTIDX = ELTIDX + 1
C
C                    END DO
C
C                    IF ( ISNULL ) THEN
C
C                       WRITE(*,*) '  COLUMN = INT_COL_3: <Null>'
C
C                    ELSE
C
C                       WRITE(*,'(A,3I6)') '   COLUMN = INT_COL_3:',
C             .                            ( IVALS(I), I = 1, COL3SZ )
C
C                    END IF
C
C                 END DO
C
C        C
C        C     We either parsed the SELECT clause or had an error.
C        C
C              END IF
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        ROW  =   1
C           COLUMN = INT_COL_1:   100
C           COLUMN = INT_COL_2:   201
C           COLUMN = INT_COL_3:   101   201   301
C
C        ROW  =   2
C           COLUMN = INT_COL_1:   200
C           COLUMN = INT_COL_2: <Null>
C           COLUMN = INT_COL_3:   102   202   302
C
C        ROW  =   3
C           COLUMN = INT_COL_1:   300
C           COLUMN = INT_COL_2:   601   602   603
C           COLUMN = INT_COL_3:   103   203   303
C
C        ROW  =   4
C           COLUMN = INT_COL_1:   400
C           COLUMN = INT_COL_2:   801   802   803   804
C           COLUMN = INT_COL_3:   104   204   304
C
C
C        Note that after run completion, a new EK file exists in the
C        output directory.
C
C
C     2) Suppose we want to create an E-kernel which contains a table
C        of items that have been ordered. The columns of this table
C        are shown below:
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
C        This EK file will have one segment containing the DATAITEMS
C        table.
C
C        This examples demonstrates how to open a new EK file; create
C        the segment described above and how to insert a new record
C        into it.
C
C
C        Example code begins here.
C
C
C              PROGRAM EKACEI_EX2
C              IMPLICIT NONE
C
C        C
C        C     Include the EK Column Name Size (CNAMSZ)
C        C
C              INCLUDE 'ekcnamsz.inc'
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         EKNAME
C              PARAMETER           ( EKNAME  = 'ekacei_ex2.bdb' )
C
C              CHARACTER*(*)         TABLE
C              PARAMETER           ( TABLE   = 'DATAITEMS'      )
C
C              INTEGER               DECLEN
C              PARAMETER           ( DECLEN = 200 )
C
C              INTEGER               DESCLN
C              PARAMETER           ( DESCLN = 80  )
C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 40  )
C
C              INTEGER               NCOLS
C              PARAMETER           ( NCOLS  = 5   )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(DECLEN)    CDECLS ( NCOLS )
C              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS )
C              CHARACTER*(DESCLN)    DESCRP
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(NAMLEN)    ITEMNM
C
C              DOUBLE PRECISION      PRICE
C
C              INTEGER               ESIZE
C              INTEGER               HANDLE
C              INTEGER               ITEMID
C              INTEGER               NRESVC
C              INTEGER               ORDID
C              INTEGER               RECNO
C              INTEGER               SEGNO
C
C              LOGICAL               ISNULL
C
C        C
C        C     Open a new EK file.  For simplicity, we will not
C        C     reserve any space for the comment area, so the
C        C     number of reserved comment characters is zero.
C        C     The variable IFNAME is the internal file name.
C        C
C              NRESVC  =  0
C              IFNAME  =  'Test EK;Created 21-JUN-2019'
C
C              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
C
C        C
C        C     Set up the table and column names and declarations
C        C     for the DATAITEMS segment.  We'll index all of
C        C     the columns.  All columns are scalar, so we omit
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
C              CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
C             .              CNAMES, CDECLS, SEGNO )
C
C        C
C        C     Append a new, empty record to the DATAITEMS
C        C     table. Recall that the DATAITEMS table
C        C     is in segment number 1.  The call will return
C        C     the number of the new, empty record.
C        C
C              SEGNO = 1
C              CALL EKAPPR ( HANDLE, SEGNO, RECNO )
C
C        C
C        C     At this point, the new record is empty.  A valid EK
C        C     cannot contain empty records.  We fill in the data
C        C     here.  Data items are filled in one column at a time.
C        C     The order in which the columns are filled in is not
C        C     important.  We use the EKACEx (add column entry)
C        C     routines to fill in column entries.  We'll assume
C        C     that no entries are null.  All entries are scalar,
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
C              PRICE    =   1345.678D0
C
C        C
C        C     Note that the names of the routines called
C        C     correspond to the data types of the columns:  the
C        C     last letter of the routine name is C, I, or D,
C        C     depending on the data type.
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
C        C
C        C     Close the file to make the update permanent.
C        C
C              CALL EKCLS ( HANDLE )
C
C              END
C
C
C        When this program is executed, no output is presented on
C        screen. After run completion, a new EK file exists in the
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
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code examples from existing fragments.
C
C-    SPICELIB Version 1.2.0, 05-FEB-2015 (NJB)
C
C        Updated to use ERRHAN.
C
C-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Removed an unbalanced call to CHKOUT.
C
C-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     add integer data to EK column
C     add data to EK
C     write integer data to EK column
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED

C
C     Local variables
C
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               CLASS
      INTEGER               DTYPE
      INTEGER               RECPTR
      INTEGER               SEGDSC ( SDSCSZ )

C
C     Use discovery check-in.
C
C     First step:  find the descriptor for the named segment.  Using
C     this descriptor, get the column descriptor.
C
      CALL ZZEKSDSC (  HANDLE,  SEGNO,  SEGDSC          )
      CALL ZZEKCDSC (  HANDLE,  SEGDSC, COLUMN, COLDSC  )

      IF ( FAILED() ) THEN
         RETURN
      END IF

C
C     This column had better be of integer type.
C
      DTYPE  =  COLDSC(TYPIDX)

      IF ( DTYPE .NE. INT ) THEN

         CALL CHKIN  ( 'EKACEI'                                     )
         CALL SETMSG ( 'Column # is of type #; EKACEI only works '
     .   //            'with integer columns.  RECNO = #; SEGNO = '
     .   //            '#; EK = #.'                                 )
         CALL ERRCH  ( '#',  COLUMN                                 )
         CALL ERRINT ( '#',  DTYPE                                  )
         CALL ERRINT ( '#',  RECNO                                  )
         CALL ERRINT ( '#',  SEGNO                                  )
         CALL ERRHAN ( '#',  HANDLE                                 )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                       )
         CALL CHKOUT ( 'EKACEI'                                     )
         RETURN

      END IF

C
C     Look up the record pointer for the target record.
C
      CALL ZZEKTRDP ( HANDLE, SEGDSC(RTIDX), RECNO, RECPTR )
C
C
C     Now it's time to add data to the file.
C
      CLASS  =  COLDSC ( CLSIDX )


      IF ( CLASS .EQ. 1 ) THEN
C
C        Class 1 columns contain scalar integer data.
C
         CALL ZZEKAD01 ( HANDLE, SEGDSC, COLDSC, RECPTR, IVALS, ISNULL )


      ELSE IF ( CLASS .EQ. 4 ) THEN
C
C        Class 4 columns contain array-valued integer data.
C
         CALL ZZEKAD04 ( HANDLE,  SEGDSC, COLDSC,
     .                   RECPTR,  NVALS,  IVALS,  ISNULL )

      ELSE
C
C        This is an unsupported integer column class.
C
         SEGNO  =  SEGDSC ( SNOIDX )

         CALL CHKIN  ( 'EKACEI'                                        )
         CALL SETMSG ( 'Class # from input column descriptor is not '
     .   //            'a supported integer class.  COLUMN = #; '
     .   //            'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'EKACEI'                                        )
         RETURN

      END IF


      RETURN
      END
