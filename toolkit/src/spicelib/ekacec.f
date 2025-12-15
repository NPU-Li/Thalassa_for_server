C$Procedure EKACEC ( EK, add character data to column )

      SUBROUTINE EKACEC (  HANDLE,  SEGNO,  RECNO,  COLUMN,
     .                     NVALS,   CVALS,  ISNULL          )

C$ Abstract
C
C     Add data to a character column in a specified EK record.
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
      CHARACTER*(*)         CVALS  ( * )
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
C     CVALS      I   Character values to add to column.
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
C     CVALS    are, respectively, the number of values to add to
C              the specified column and the set of values
C              themselves. The data values are written into the
C              specified column and record.
C
C              If the  column has fixed-size entries, then NVALS
C              must equal the entry size for the specified column.
C
C              Only one value can be added to a virtual column.
C
C
C     ISNULL   is a logical flag indicating whether the entry is
C              null. If ISNULL is .FALSE., the column entry
C              defined by NVALS and CVALS is added to the
C              specified kernel file.
C
C              If ISNULL is .TRUE., NVALS and CVALS are ignored.
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
C         character, the error SPICE(WRONGDATATYPE) is signaled.
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
C     1) The following program demonstrates how to create a new EK and
C        add data to a character column in a given record within the
C        file, and how to read the data from it.
C
C        Example code begins here.
C
C
C              PROGRAM EKACEC_EX1
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
C              PARAMETER           ( EKNAME  = 'ekacec_ex1.bdb' )
C
C              CHARACTER*(*)         TABLE
C              PARAMETER           ( TABLE   = 'TABLENAME'      )
C
C              INTEGER               CBUFSZ
C              PARAMETER           ( CBUFSZ = 4   )
C
C              INTEGER               DECLEN
C              PARAMETER           ( DECLEN = 200 )
C
C              INTEGER               LINESZ
C              PARAMETER           ( LINESZ = 6   )
C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 40  )
C
C              INTEGER               NCOLS
C              PARAMETER           ( NCOLS  = 2   )
C
C              INTEGER               STRLEN
C              PARAMETER           ( STRLEN = 6   )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(STRLEN)    CBUF   ( CBUFSZ )
C              CHARACTER*(DECLEN)    CDECLS ( NCOLS  )
C              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS  )
C              CHARACTER*(LINESZ)    CVALS  ( CBUFSZ )
C              CHARACTER*(NAMLEN)    IFNAME
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               J
C              INTEGER               K
C              INTEGER               NRESVC
C              INTEGER               NVALS
C              INTEGER               RECNO
C              INTEGER               SEGNO
C
C              LOGICAL               ISNULL
C
C        C
C        C     Create a list of character strings.
C        C
C              DATA                  CBUF / 'CHSTR1', 'CHSTR2',
C             .                             'CHSTR3', 'CHSTR4' /
C
C        C
C        C     Open a new EK file.  For simplicity, we will not
C        C     reserve any space for the comment area, so the
C        C     number of reserved comment characters is zero.
C        C     The variable IFNAME is the internal file name.
C        C
C              NRESVC  =  0
C              IFNAME  =  'Test EK/Created 31-MAY-2019'
C
C              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
C
C        C
C        C     Define the column names and formats.
C        C
C              CNAMES(1) = 'CCOL'
C              CDECLS(1) = 'DATATYPE = CHARACTER*(*), ' //
C             .            'INDEXED = TRUE, NULLS_OK = TRUE'
C
C              CNAMES(2) = 'CARRAY'
C              CDECLS(2) = 'DATATYPE = CHARACTER*(6), ' //
C             .            'SIZE = VARIABLE, NULLS_OK = TRUE'
C
C        C
C        C     Start the segment.
C        C
C              CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
C             .              CNAMES, CDECLS, SEGNO )
C
C        C
C        C     Append a new record to the EK.
C        C
C              CALL EKAPPR ( HANDLE, SEGNO, RECNO )
C
C        C
C        C     Add the value '999' to the first record of the column
C        C     CCOL in the SEGNO segment of the EK file designated
C        C     by HANDLE.
C        C
C              CALL EKACEC ( HANDLE, SEGNO, RECNO,
C             .              'CCOL', 1,     '999', .FALSE. )
C
C        C
C        C     Add an array CBUF of 4 values to the first record of
C        C     the column CARRAY in the SEGNO segment of the EK file
C        C     designated by HANDLE.
C        C
C              CALL EKACEC ( HANDLE,   SEGNO,  RECNO,
C             .              'CARRAY', CBUFSZ, CBUF, .FALSE. )
C
C        C
C        C     Append a second record to the EK.
C        C
C              CALL EKAPPR ( HANDLE, SEGNO, RECNO )
C
C        C
C        C     Repeat the operation again for the second record, but
C        C     this time, add only 2 values of CBUF.
C        C
C              CALL EKACEC ( HANDLE,   SEGNO, RECNO,
C             .              'CARRAY', 2,     CBUF, .FALSE. )
C
C        C
C        C     Add a null value to the CCOL in the second record.
C        C     The argument 999 is ignored because the null flag is
C        C     set to .TRUE.
C        C
C              CALL EKACEC ( HANDLE, SEGNO, RECNO,
C             .              'CCOL', 1,     '999', .TRUE. )
C
C        C
C        C     Close the file.
C        C
C              CALL EKCLS ( HANDLE )
C
C        C
C        C     Open the created file. Show the values added.
C        C
C              CALL EKOPR ( EKNAME, HANDLE )
C
C        C
C        C     The file we have created has only one segment and
C        C     two records within. Each record has two columns.
C        C
C              SEGNO = 1
C
C        C
C        C     Go over each record...
C        C
C              DO I = 1, 2
C
C                 WRITE(*,'(A,I4)') 'Record', I
C
C        C
C        C        ... and each column.
C        C
C                 DO J = 1, NCOLS
C
C        C
C        C        Read the data from the first column.
C        C
C                    CALL EKRCEC ( HANDLE, SEGNO, I, CNAMES(J),
C             .                    NVALS,  CVALS,    ISNULL    )
C
C                    IF ( ISNULL ) THEN
C
C                       WRITE(*,'(A,A6,A)') '   ', CNAMES(J), ': NULL '
C
C                    ELSE
C
C                       WRITE(*,'(A,A6,A,4A9)') '   ', CNAMES(J), ': ',
C             .                              ( CVALS(K), K = 1, NVALS )
C
C                    END IF
C
C                 END DO
C
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Record   1
C           CCOL  :    999
C           CARRAY:    CHSTR1   CHSTR2   CHSTR3   CHSTR4
C        Record   2
C           CCOL  : NULL
C           CARRAY:    CHSTR1   CHSTR2
C
C
C        Note that after run completion, a new EK file exists in the
C        output directory.
C
C
C     2) A more detailed example.
C
C        Suppose we have an E-kernel which contains records of orders
C        for data products. The E-kernel has a table called DATAORDERS
C        that consists of the set of columns listed below:
C
C           DATAORDERS
C
C              Column Name     Data Type
C              -----------     ---------
C              ORDER_ID        INTEGER
C              CUSTOMER_ID     INTEGER
C              LAST_NAME       CHARACTER*(*)
C              FIRST_NAME      CHARACTER*(*)
C              ORDER_DATE      TIME
C              COST            DOUBLE PRECISION
C
C        The order database also has a table of items that have been
C        ordered. The columns of this table are shown below:
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
C        We'll suppose that the EK file contains two segments, the
C        first containing the DATAORDERS table and the second
C        containing the DATAITEMS table.
C
C        This examples demonstrates how to open a new EK file; create
C        the two segments described above, using fast writers; and
C        how to insert a new record into one of the tables.
C
C
C        Use the LSK kernel below to load the leap seconds and time
C        constants required for the conversions.
C
C           naif0012.tls
C
C
C        Example code begins here.
C
C
C              PROGRAM EKACEC_EX2
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
C              PARAMETER           ( EKNAME  = 'ekacec_ex2.bes' )
C
C              CHARACTER*(*)         LSK
C              PARAMETER           ( LSK     = 'naif0012.tls'   )
C
C              CHARACTER*(*)         TABLE
C              PARAMETER           ( TABLE   = 'DATAORDERS'     )
C
C              INTEGER               DECLEN
C              PARAMETER           ( DECLEN = 200 )
C
C              INTEGER               DESCLN
C              PARAMETER           ( DESCLN = 80  )
C
C              INTEGER               FNMLEN
C              PARAMETER           ( FNMLEN = 50  )
C
C              INTEGER               LNMLEN
C              PARAMETER           ( LNMLEN = 50  )
C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 40  )
C
C              INTEGER               NCOLS
C              PARAMETER           ( NCOLS  = 6   )
C
C              INTEGER               NROWS
C              PARAMETER           ( NROWS  = 9   )
C
C              INTEGER               UTCLEN
C              PARAMETER           ( UTCLEN = 30  )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(DECLEN)    CDECLS ( NCOLS )
C              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS )
C              CHARACTER*(DESCLN)    DESCRP
C              CHARACTER*(FNMLEN)    FNAMES ( NROWS )
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(NAMLEN)    ITEMNM
C              CHARACTER*(LNMLEN)    LNAMES ( NROWS )
C              CHARACTER*(UTCLEN)    ODATE
C
C              DOUBLE PRECISION      COSTS  ( NROWS )
C              DOUBLE PRECISION      ETS    ( NROWS )
C              DOUBLE PRECISION      PRICE
C
C              INTEGER               CSTIDS ( NROWS )
C              INTEGER               ESIZE
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               ITEMID
C              INTEGER               NRESVC
C              INTEGER               ORDID
C              INTEGER               ORDIDS ( NROWS )
C              INTEGER               RCPTRS ( NROWS )
C              INTEGER               RECNO
C              INTEGER               SEGNO
C              INTEGER               SIZES  ( NROWS )
C              INTEGER               WKINDX ( NROWS )
C
C              LOGICAL               ISNULL
C              LOGICAL               NLFLGS ( NROWS )
C
C        C
C        C     Load a leapseconds kernel for UTC/ET conversion.
C        C
C              CALL FURNSH ( 'naif0012.tls' )
C
C        C
C        C     Open a new EK file.  For simplicity, we will not
C        C     reserve any space for the comment area, so the
C        C     number of reserved comment characters is zero.
C        C     The variable IFNAME is the internal file name.
C        C
C              NRESVC  =  0
C              IFNAME  =  'Test EK/Created 01-JUN-2019'
C
C              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
C
C        C
C        C     Set up the table and column names and declarations
C        C     for the DATAORDERS segment.  We'll index all of
C        C     the columns.  All columns are scalar, so we omit
C        C     the size declaration.  Only the COST column may take
C        C     null values.
C        C
C              CNAMES(1) =  'ORDER_ID'
C              CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE'
C
C              CNAMES(2) =  'CUSTOMER_ID'
C              CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE'
C
C              CNAMES(3) =  'LAST_NAME'
C              CDECLS(3) =  'DATATYPE = CHARACTER*(*), ' //
C             .             'INDEXED  = TRUE'
C
C              CNAMES(4) =  'FIRST_NAME'
C              CDECLS(4) =  'DATATYPE = CHARACTER*(*), ' //
C             .             'INDEXED  = TRUE'
C
C              CNAMES(5) =  'ORDER_DATE'
C              CDECLS(5) =  'DATATYPE = TIME, INDEXED  = TRUE'
C
C              CNAMES(6) =  'COST'
C              CDECLS(6) =  'DATATYPE = DOUBLE PRECISION,' //
C             .             'INDEXED  = TRUE,'             //
C             .             'NULLS_OK = TRUE'
C
C
C        C
C        C     Start the segment.  We presume the number of  rows
C        C     of data is known in advance.
C        C
C              CALL EKIFLD ( HANDLE,  TABLE,  NCOLS, NROWS,
C             .              CNAMES,  CDECLS, SEGNO, RCPTRS )
C
C
C        C
C        C     At this point, arrays containing data for the
C        C     segment's columns may be filled in.  The names
C        C     of the data arrays are shown below.
C        C
C        C        Column           Data array
C        C
C        C        'ORDER_ID'       ORDIDS
C        C        'CUSTOMER_ID'    CSTIDS
C        C        'LAST_NAME'      LNAMES
C        C        'FIRST_NAME'     FNAMES
C        C        'ORDER_DATE'     ETS
C        C        'COST'           COSTS
C        C
C              DO I = 1, NROWS
C
C                 ORDIDS(I) = I
C                 CSTIDS(I) = I * 100
C                 COSTS(I)  = I * 100.D0
C
C                 CALL REPMI ( 'Order # Customer first name',
C             .                '#', I, FNAMES(I)             )
C                 CALL REPMI ( 'Order # Customer last name',
C             .                '#', I, LNAMES(I)             )
C                 CALL REPMI ( '1998 Mar #', '#', I, ODATE   )
C
C                 CALL UTC2ET ( ODATE,  ETS(I) )
C
C                 NLFLGS(I) = .FALSE.
C
C              END DO
C
C              NLFLGS(2) = .TRUE.
C
C        C
C        C     The SIZES array shown below is ignored for scalar
C        C     and fixed-size array columns, so we need not
C        C     initialize it.  For variable-size arrays, the
C        C     Ith element of the SIZES array must contain the size
C        C     of the Ith column entry in the column being written.
C        C     Normally, the SIZES array would be reset for each
C        C     variable-size column.
C        C
C        C     The NLFLGS array indicates which entries are null.
C        C     It is ignored for columns that don't allow null
C        C     values.  In this case, only the COST column allows
C        C     nulls.
C        C
C        C     Add the columns of data to the segment.  All of the
C        C     data for each column is written in one shot.
C        C
C              CALL EKACLI ( HANDLE, SEGNO,  'ORDER_ID',
C             .              ORDIDS, SIZES,  NLFLGS,  RCPTRS, WKINDX )
C
C              CALL EKACLI ( HANDLE, SEGNO,  'CUSTOMER_ID',
C             .              CSTIDS, SIZES,  NLFLGS,  RCPTRS, WKINDX )
C
C              CALL EKACLC ( HANDLE, SEGNO,  'LAST_NAME',
C             .              LNAMES, SIZES,  NLFLGS,  RCPTRS, WKINDX )
C
C              CALL EKACLC ( HANDLE, SEGNO,  'FIRST_NAME',
C             .              FNAMES, SIZES,  NLFLGS,  RCPTRS, WKINDX )
C
C              CALL EKACLD ( HANDLE, SEGNO,  'ORDER_DATE',
C             .              ETS,    SIZES,  NLFLGS,  RCPTRS, WKINDX )
C
C              CALL EKACLD ( HANDLE, SEGNO,  'COST',
C             .              COSTS,  SIZES,  NLFLGS,  RCPTRS, WKINDX )
C
C        C
C        C     Complete the segment.  The RCPTRS array is that
C        C     returned by EKIFLD.
C        C
C              CALL EKFFLD ( HANDLE, SEGNO, RCPTRS )
C
C        C
C        C     At this point, the second segment could be
C        C     created by an analogous process.  In fact, the
C        C     second segment could be created at any time; it is
C        C     not necessary to populate the first segment with
C        C     data before starting the second segment.
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
C        C     Start the new segment. Since we have no data for this
C        C     segment, start the segment by just defining the new
C        C     segment's schema.
C        C
C              CALL EKBSEG ( HANDLE, 'DATAITEMS', 5,
C             .              CNAMES, CDECLS,      SEGNO )
C
C        C
C        C     Close the file by a call to EKCLS.
C        C
C              CALL EKCLS ( HANDLE )
C
C        C
C        C     Now, we want to insert a new record into the DATAITEMS
C        C     table.
C        C
C        C     Open the database for write access.  This call is
C        C     made when the file already exists.
C        C
C              CALL EKOPW ( EKNAME, HANDLE )
C
C        C
C        C     Append a new, empty record to the DATAITEMS
C        C     table. Recall that the DATAITEMS table
C        C     is in segment number 2.  The call will return
C        C     the number of the new, empty record.
C        C
C              SEGNO = 2
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
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard and
C        created complete code example from existing fragment.
C
C-    SPICELIB Version 1.1.0, 05-FEB-2015 (NJB)
C
C        Updated to use ERRHAN.
C
C-    Beta Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     add character data to EK column
C     add data to EK
C     write character data to EK column
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
C     This column had better be of character type.
C
      DTYPE  =  COLDSC(TYPIDX)

      IF ( DTYPE .NE. CHR ) THEN

         CALL CHKIN  ( 'EKACEC'                                        )
         CALL SETMSG ( 'Column # is of type #; EKACEC only works '    //
     .                 'with character columns.  RECNO = #; SEGNO = ' //
     .                 '#; EK = #.'                                    )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                          )
         CALL CHKOUT ( 'EKACEC'                                        )
         RETURN

      END IF

C
C     Look up the record pointer for the target record.
C
      CALL ZZEKTRDP ( HANDLE, SEGDSC(RTIDX), RECNO, RECPTR )

C
C     Now it's time to add data to the file.
C
      CLASS  =  COLDSC ( CLSIDX )


      IF ( CLASS .EQ. 3 ) THEN
C
C        Class 3 columns contain scalar character data.
C
         CALL ZZEKAD03 ( HANDLE, SEGDSC, COLDSC, RECPTR, CVALS, ISNULL )


      ELSE IF ( CLASS .EQ. 6 ) THEN
C
C        Class 6 columns contain array-valued character data.
C
         CALL ZZEKAD06 ( HANDLE,  SEGDSC,  COLDSC,
     .                   RECPTR,  NVALS,   CVALS,  ISNULL )

      ELSE

C
C        This is an unsupported character column class.
C
         CALL CHKIN  ( 'EKACEC'                                        )
         CALL SETMSG ( 'Class # from input column descriptor is not ' //
     .                 'a supported character class.  COLUMN = #; '   //
     .                 'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'EKACEC'                                        )
         RETURN


      END IF


      RETURN
      END
