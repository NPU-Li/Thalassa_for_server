C$Procedure EKBSEG ( EK, start new segment )

      SUBROUTINE EKBSEG ( HANDLE, TABNAM, NCOLS, CNAMES, DECLS, SEGNO )

C$ Abstract
C
C     Start a new segment in an E-kernel.
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

      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekfilpar.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      CHARACTER*(*)         TABNAM
      INTEGER               NCOLS
      CHARACTER*(*)         CNAMES ( * )
      CHARACTER*(*)         DECLS  ( * )
      INTEGER               SEGNO

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TABNAM     I   Table name.
C     NCOLS      I   Number of columns in the segment.
C     CNAMES     I   Names of columns.
C     DECLS      I   Declarations of columns.
C     SEGNO      O   Segment number.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of an EK file that is open for writing.
C
C     TABNAM   is the name of the EK table to which the current
C              segment belongs. All segments in the EK file
C              designated by HANDLE must have identical column
C              attributes. TABNAM must not exceed 32 characters
C              in length. Case is not significant. Table names
C              must start with a letter and contain only
C              characters from the set {A-Z,a-z,0-9,$,_}.
C
C     NCOLS    is the number of columns in a new segment.
C
C     CNAMES,
C     DECLS    are, respectively, and array of column names and
C              their corresponding declarations: the Ith element
C              of CNAMES and the Ith element of DECLS apply to
C              the Ith column in the segment.
C
C              Column names must not exceed CNAMSZ (32) characters
C              in length. Case is not significant. Column names
C              must start with a letter and contain only
C              characters from the set {A-Z,a-z,0-9,$,_}.
C
C              The declarations are strings that contain
C              `keyword=value' assignments that define the
C              attributes of the columns to which they apply. The
C              column attributes that are defined by a column
C              declaration are:
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
C              For example, an indexed, scalar, integer column
C              that allows null values would have the declaration
C
C                 'DATATYPE  = INTEGER,
C                  SIZE      = 1,
C                  INDEXED   = TRUE,
C                  NULLS_OK  = TRUE'
C
C              Commas are required to separate the assignments
C              within declarations; white space is optional;
C              case is not significant.
C
C              The order in which the attribute keywords are
C              listed in declaration is not significant.
C
C              Every column in a segment must be declared.
C
C              Each column entry is effectively an array, each
C              element of which has the declared data type. The
C              SIZE keyword indicates how many elements are in
C              each entry of the column in whose declaration the
C              keyword appears. Note that only scalar-valued
C              columns (those for which SIZE = 1) may be
C              referenced in query constraints. A size
C              assignment has the syntax
C
C                 'SIZE = <integer>'
C
C              or
C
C                 'SIZE = VARIABLE'
C
C              The size value defaults to 1 if omitted.
C
C              The DATATYPE keyword defines the data type of
C              column entries. The DATATYPE assignment syntax
C              has any of the forms
C
C                 'DATATYPE = CHARACTER*(<length>)'
C                 'DATATYPE = CHARACTER*(*)'
C                 'DATATYPE = DOUBLE PRECISION'
C                 'DATATYPE = INTEGER'
C                 'DATATYPE = TIME'
C
C              As the datatype declaration syntax suggests,
C              character strings may have fixed or variable
C              length. Variable-length strings are allowed only
C              in columns of size 1.
C
C              Optionally, scalar-valued columns may be indexed.
C              To create an index for a column, use the assignment
C
C                 'INDEXED = TRUE'
C
C              By default, columns are not indexed.
C
C              Optionally, any column can allow null values. To
C              indicate that a column may allow null values, use
C              the assignment
C
C                 'NULLS_OK = TRUE'
C
C              in the column declaration. By default, null
C              values are not allowed in column entries.
C
C$ Detailed_Output
C
C     SEGNO    is the number of the segment created by this
C              routine. Segment numbers are used as unique
C              identifiers by other EK access routines.
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
C         number MXCLSG, the error SPICE(INVALIDCOUNT) is signaled.
C
C     5)  If any column name exceeds CNAMSZ characters in length, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     6)  If any column name contains non-printable characters, an error
C         is signaled by a routine in the call tree of this routine.
C
C     7)  If a declaration cannot be understood by this routine, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     8)  If an non-positive string length or element size is specified,
C         an error is signaled by a routine in the call tree of this
C         routine.
C
C     9)  If an I/O error occurs while reading or writing the indicated
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
C     This routine operates by side effects: it prepares an EK for
C     the addition of a new segment. It is not necessary to take
C     any special action to `complete' a segment; segments are readable
C     after the completion of any record insertion, deletion, write,
C     or update operation.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose we want to create an E-kernel which contains a table
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
C              PROGRAM EKBSEG_EX1
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
C              PARAMETER           ( EKNAME  = 'ekbseg_ex1.bdb' )
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
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 13-AUG-2021 (JDR)
C
C        Added IMPLICI NONE statement.
C
C        Edited the header to comply with NAIF standard and
C        created complete code example from existing fragment.
C
C-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
C
C        Erroneous error message for invalid column names was fixed.
C        Previous version line was changed from "Beta" to "SPICELIB."
C
C-    SPICELIB Version 1.0.0, 06-NOV-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     start new E-kernel segment
C     start new EK segment
C
C-&


C
C     SPICELIB functions
C
      INTEGER               LASTNB

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKSTYP

C
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MXSPEC
      PARAMETER           ( MXSPEC = 512 )

      INTEGER               NAMLIM
      PARAMETER           ( NAMLIM = 32 )

C
C     Local variables
C
      INTEGER               CDSCRS ( CDSCSZ, MXCLSG )
      INTEGER               I
      INTEGER               IDEND
      INTEGER               IDSPEC ( LBCELL : MXSPEC )
      INTEGER               NCHARS
      INTEGER               STYPE

      LOGICAL               FIRST

C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  IDSPEC

C
C     Initial values
C
      DATA                  FIRST / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKBSEG' )
      END IF

C
C     Before trying to actually write anything, do every error
C     check we can.
C
C     Is this file handle valid--is the file open for paged write
C     access?  Signal an error if not.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKBSEG' )
         RETURN
      END IF

C
C     Get the default identifier specification the first time through.
C
      IF ( FIRST ) THEN

         CALL SSIZEI ( MXSPEC, IDSPEC )
         CALL LXDFID (         IDSPEC )
         FIRST = .FALSE.

      END IF

C
C     The table name must not be too long, and all of its characters
C     must be printable (it's ok for it to unprintable).
C
      CALL CHCKID ( 'EK table name', NAMLIM, TABNAM )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKBSEG' )
         RETURN
      END IF

C
C     Make sure the table name satisfies all of our restrictions on
C     allowed characters.
C
      CALL LXIDNT ( IDSPEC, TABNAM, 1, IDEND, NCHARS )

      IF ( ( NCHARS .EQ. 0 ) .OR. ( NCHARS .LT. LASTNB(TABNAM))  ) THEN

         CALL SETMSG ( 'Table name <#> violates syntax rules.' )
         CALL ERRCH  ( '#', TABNAM                             )
         CALL SIGERR ( 'SPICE(INVALIDNAME)'                    )
         CALL CHKOUT ( 'EKBSEG'                                )
         RETURN

      END IF

C
C     Check out NCOLS.
C
      IF (  ( NCOLS .LT. 1 ) .OR. ( NCOLS .GT. MXCLSG )  ) THEN

         CALL SETMSG ( 'Number of columns must be in range 1:#, was #.')
         CALL ERRINT ( '#',  MXCLSG                                    )
         CALL ERRINT ( '#',  NCOLS                                     )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                           )
         CALL CHKOUT ( 'EKBSEG'                                        )
         RETURN

      END IF

C
C     Check the column names for length and printability.
C
      DO I = 1, NCOLS

         CALL CHCKID ( 'EK column name', CNAMSZ, CNAMES(I) )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'EKBSEG' )
            RETURN
         END IF

C
C        Make sure each column name satisfies all of our restrictions on
C        allowed characters.
C
         CALL LXIDNT ( IDSPEC, CNAMES(I), 1, IDEND, NCHARS )

         IF (       ( NCHARS .EQ. 0                    )
     .         .OR. ( NCHARS .LT. LASTNB( CNAMES(I) )  )   ) THEN

            CALL SETMSG ( 'Column name <#> violates syntax rules.' )
            CALL ERRCH  ( '#', CNAMES(I)                           )
            CALL SIGERR ( 'SPICE(INVALIDNAME)'                     )
            CALL CHKOUT ( 'EKBSEG'                                 )
            RETURN

         END IF

      END DO

C
C     Parse the column declarations before proceeding.
C
      DO I = 1, NCOLS
C
C        Parse the declaration of the Ith column.  The descriptor is
C        returned with all elements other than pointers initialized.
C
         CALL ZZEKPDEC ( DECLS(I), CDSCRS(1,I) )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'EKBSEG' )
            RETURN
         END IF

      END DO

C
C     Determine the segment type.
C
      STYPE  =  ZZEKSTYP ( NCOLS, CDSCRS )


C
C     Create the segment metadata according to the segment's type.
C
      IF ( STYPE .EQ. 1 ) THEN

         CALL ZZEKBS01 ( HANDLE, TABNAM, NCOLS, CNAMES, CDSCRS, SEGNO )

      ELSE IF ( STYPE .EQ. 2 ) THEN

         CALL ZZEKBS02 ( HANDLE, TABNAM, NCOLS, CNAMES, CDSCRS, SEGNO )

      ELSE

         CALL SETMSG ( 'Segment type # is not currently supported.' )
         CALL ERRINT ( '#', STYPE                                   )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'EKBSEG'                                     )
         RETURN

      END IF


      CALL CHKOUT ( 'EKBSEG' )
      RETURN
      END
