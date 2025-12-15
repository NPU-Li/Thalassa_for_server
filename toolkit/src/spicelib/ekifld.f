C$Procedure EKIFLD ( EK, initialize segment for fast write )

      SUBROUTINE EKIFLD (  HANDLE,  TABNAM,  NCOLS,  NROWS,
     .                     CNAMES,  DECLS,   SEGNO,  RCPTRS  )

C$ Abstract
C
C     Initialize a new E-kernel segment to allow fast writing.
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
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      CHARACTER*(*)         TABNAM
      INTEGER               NCOLS
      INTEGER               NROWS
      CHARACTER*(*)         CNAMES ( * )
      CHARACTER*(*)         DECLS  ( * )
      INTEGER               SEGNO
      INTEGER               RCPTRS ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TABNAM     I   Table name.
C     NCOLS      I   Number of columns in the segment.
C     NROWS      I   Number of rows in the segment.
C     CNAMES     I   Names of columns.
C     DECLS      I   Declarations of columns.
C     SEGNO      O   Segment number.
C     RCPTRS     O   Array of record pointers.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of an EK file open for write access. A new
C              segment is to be created in this file.
C
C     TABNAM   is the name of the EK table to which the current segment
C              belongs. All segments in the EK file designated by HANDLE
C              must have identical column attributes. TABNAM must not
C              exceed TNAMSZ (64) characters in length. Case is not
C              significant. Table names must start with a letter and
C              contain only characters from the set {A-Z,a-z,0-9,$,_}.
C
C     NCOLS    is the number of columns in a new segment.
C
C     NROWS    is the number of rows in a new segment. Each column to be
C              added to the segment must contain the number of entries
C              indicated by NROWS.
C
C     CNAMES,
C     DECLS    are, respectively, an array of column names and their
C              corresponding declarations: the Ith element of CNAMES and
C              the Ith element of DECLS apply to the Ith column in the
C              segment.
C
C              Column names must not exceed CNAMSZ (32) characters in
C              length. Case is not significant. Column names must start
C              with a letter and contain only characters from the set
C              {A-Z,a-z,0-9,$,_}.
C
C              The declarations are strings that contain "keyword=value"
C              assignments that define the attributes of the columns to
C              which they apply. The column attributes that are defined
C              by a column declaration are:
C
C                 DATATYPE
C                 SIZE
C                 <is the column indexed?>
C                 <does the column allow null values?>
C
C              The form of a declaration is
C
C                 'DATATYPE  = <type>,
C                  SIZE      = <size>,
C                  INDEXED   = <boolean>,
C                  NULLS_OK  = <boolean>'
C
C              For example, an indexed, scalar, integer column that
C              allows null values would have the declaration
C
C                 'DATATYPE  = INTEGER,
C                  SIZE      = 1,
C                  INDEXED   = TRUE,
C                  NULLS_OK  = TRUE'
C
C              Commas are required to separate the assignments within
C              declarations; white space is optional; case is not
C              significant.
C
C              The order in which the attribute keywords are listed in
C              declaration is not significant.
C
C              Every column in a segment must be declared.
C
C              Each column entry is effectively an array, each element
C              of which has the declared data type. The SIZE keyword
C              indicates how many elements are in each entry of the
C              column in whose declaration the keyword appears. Note
C              that only scalar-valued columns (those for which SIZE =
C              1) may be referenced in query constraints. A size
C              assignment has the syntax
C
C                 'SIZE = <integer>'
C
C              or
C                 'SIZE = VARIABLE'
C
C              The size value defaults to 1 if omitted.
C
C              The DATATYPE keyword defines the data type of column
C              entries. The DATATYPE assignment syntax has any of the
C              forms
C
C                 'DATATYPE = CHARACTER*(<length>)'
C                 'DATATYPE = CHARACTER*(*)'
C                 'DATATYPE = DOUBLE PRECISION'
C                 'DATATYPE = INTEGER'
C                 'DATATYPE = TIME'
C
C              As the datatype declaration syntax suggests, character
C              strings may have fixed or variable length.
C              Variable-length strings are allowed only in columns of
C              size 1.
C
C              Optionally, scalar-valued columns may be indexed. To
C              create an index for a column, use the assignment
C
C                 'INDEXED = TRUE'
C
C              By default, columns are not indexed.
C
C              Optionally, any column can allow null values. To indicate
C              that a column may allow null values, use the assignment
C
C                 'NULLS_OK = TRUE'
C
C              in the column declaration. By default, null values are
C              not allowed in column entries.
C
C$ Detailed_Output
C
C     SEGNO    is the number of the segment created by this routine.
C              Segment numbers are used as unique identifiers by other
C              EK access routines.
C
C     RCPTRS   is an array of record pointers for the input segment.
C              This array must not be modified by the caller.
C
C              The array RCPTRS must be passed as an input to each
C              column addition routine called while writing the
C              specified segment.
C
C              RCPTRS must be declared with dimension NROWS.
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
C     2)  If TABNAM is more than TNAMSZ characters long, an error
C         is signaled by a routine in the call tree of this routine.
C
C     3)  If TABNAM contains any nonprintable characters, an error
C         is signaled by a routine in the call tree of this routine.
C
C     4)  If NCOLS is non-positive or greater than the maximum allowed
C         number MXCLSG, an error is signaled by a routine in the call
C         tree of this routine.
C
C     5)  If NROWS is non-positive, the error SPICE(INVALIDCOUNT)
C         is signaled.
C
C     6)  If any column name exceeds CNAMSZ characters in length, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     7)  If any column name contains non-printable characters, an error
C         is signaled by a routine in the call tree of this routine.
C
C     8)  If a declaration cannot be understood by this routine, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     9)  If an non-positive string length or element size is specified,
C         an error is signaled by a routine in the call tree of this
C         routine.
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
C     This routine prepares an EK for the creation of a new segment via
C     the fast column writer routines. After this routine is called,
C     the columns of the segment are filled in by calls to the fast
C     column writer routines of the appropriate data types. The fast
C     column writer routines are:
C
C        EKACLC {EK, add column, character}
C        EKACLD {EK, add column, double precision}
C        EKACLI {EK, add column, integer}
C
C     When all of the columns have been added, the write operation is
C     completed by a call to EKFFLD {EK, finish fast write}.
C
C     The segment is not valid until EKFFLD has been called.
C
C     The EK system supports only one fast write at a time. It is
C     not possible use the fast write routines to simultaneously write
C     multiple segments, either in the same EK file or in different
C     files.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose we want to create an Sequence Component E-kernel
C        named 'ekifld_ex1.bes' which contains records of orders for
C        data products. The E-kernel has a table called DATAORDERS that
C        consists of the set of columns listed below:
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
C        The file "ekifld_ex1.bdb" will contain two segments, the first
C        containing the DATAORDERS table and the second containing the
C        DATAITEMS table.
C
C        This example demonstrates how to open a new EK file and create
C        the first of the segments described above.
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
C              PROGRAM EKIFLD_EX1
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
C              CHARACTER*(*)         LSK
C              PARAMETER           ( LSK    = 'naif0012.tls' )
C
C              CHARACTER*(*)         TABLE
C              PARAMETER           ( TABLE  = 'DATAORDERS'   )
C
C              INTEGER               DECLEN
C              PARAMETER           ( DECLEN = 200 )
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
C              CHARACTER*(32)        CNAMES ( NCOLS )
C              CHARACTER*(FNMLEN)    FNAMES ( NROWS )
C              CHARACTER*(LNMLEN)    LNAMES ( NROWS )
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(UTCLEN)    ODATE
C
C              DOUBLE PRECISION      COSTS  ( NROWS )
C              DOUBLE PRECISION      ETS    ( NROWS )
C
C              INTEGER               CSTIDS ( NROWS )
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               NRESVC
C              INTEGER               ORDIDS ( NROWS )
C              INTEGER               RCPTRS ( NROWS )
C              INTEGER               SEGNO
C              INTEGER               SIZES  ( NROWS )
C              INTEGER               WKINDX ( NROWS )
C
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
C              IFNAME  =  'Test EK/Created 20-SEP-1995'
C
C              CALL EKOPN ( 'ekifld_ex1.bes', IFNAME, NRESVC, HANDLE )
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
C              CDECLS(3) =  'DATATYPE = CHARACTER*(*),' //
C             .             'INDEXED  = TRUE'
C
C              CNAMES(4) =  'FIRST_NAME'
C              CDECLS(4) =  'DATATYPE = CHARACTER*(*),' //
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
C        C     The file must be closed by a call to EKCLS.
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
C     1)  Only one segment can be created at a time using the fast
C         write routines.
C
C     2)  No other EK operation may interrupt a fast write. For
C         example, it is not valid to issue a query while a fast write
C         is in progress.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR) (BVS)
C
C        Added missing IMPLICIT NONE.
C
C        Edited the header to comply with NAIF standard. and
C        created complete code example from existing fragment.
C
C        Corrected entry #4 in $Exceptions: the same error is signaled
C        in the requested number of columns exceeds the maximum allowed
C        per segment.
C
C-    SPICELIB Version 1.1.1, 10-JAN-2002 (NJB)
C
C        Documentation change: instances of the phrase "fast load"
C        were replaced with "fast write." Corrected value of table
C        name size in header comment.
C
C-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Balanced CHKIN/CHKOUT calls.
C
C-    SPICELIB Version 1.0.0, 25-OCT-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     start new E-kernel segment for fast writing
C     start new EK segment for fast writing
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
      INTEGER               MBASE
      INTEGER               P
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               STYPE

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKIFLD' )
      END IF

C
C     Check out NROWS.
C
      IF ( NROWS .LT. 1 ) THEN

         CALL SETMSG ( 'Number of rows must be > 0, was #. ' )
         CALL ERRINT ( '#',  NROWS                           )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                 )
         CALL CHKOUT ( 'EKIFLD'                              )
         RETURN

      END IF

C
C     Create the segment's metadata.
C
      CALL EKBSEG ( HANDLE, TABNAM, NCOLS, CNAMES, DECLS, SEGNO )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKIFLD' )
         RETURN
      END IF

C
C     Fill the number of rows into the (file's) segment descriptor.
C
      CALL ZZEKMLOC (  HANDLE,  SEGNO,        P,            MBASE  )
      CALL DASUDI   (  HANDLE,  MBASE+NRIDX,  MBASE+NRIDX,  NROWS  )

C
C     Read in the segment descriptor, and get the segment's type.
C
      CALL ZZEKSDSC (  HANDLE,  SEGNO,  SEGDSC )

      STYPE  =  SEGDSC(EKTIDX)

C
C     Complete the fast write preparations appropriate to the segment's
C     type.
C
      IF ( STYPE .EQ. 1 ) THEN

         CALL ZZEKIF01 ( HANDLE, SEGNO, RCPTRS )


      ELSE IF ( STYPE .EQ. 2 ) THEN

         CALL ZZEKIF02 ( HANDLE, SEGNO )

      ELSE

         CALL SETMSG ( 'Segment type # is not currently supported.' )
         CALL ERRINT ( '#', STYPE                                   )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'EKIFLD'                                     )
         RETURN

      END IF


      CALL CHKOUT ( 'EKIFLD' )
      RETURN
      END
