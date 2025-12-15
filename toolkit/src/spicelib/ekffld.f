C$Procedure EKFFLD ( EK, finish fast write )

      SUBROUTINE EKFFLD ( HANDLE, SEGNO, RCPTRS )

C$ Abstract
C
C     Complete a fast write operation on a new E-kernel segment.
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

      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RCPTRS ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGNO      I   Segment number.
C     RCPTRS     I   Record pointers.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of an EK file that is open for writing.
C              A "begin segment for fast write" operation must
C              have already been performed for the designated
C              segment.
C
C     SEGNO    is the number of the segment to complete.
C
C     RCPTRS   is an array of record pointers for the input
C              segment. This array is obtained as an output
C              from EKIFLD, the routine called to initiate a
C              fast write.
C
C$ Detailed_Output
C
C     None.
C
C     See the $Particulars section for a description of the
C     effects of this routine.
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
C     2)  If an attempt is made to finish a segment other than the one
C         last initialized by EKIFLD, an error is signaled by a routine
C         in the call tree of this routine.
C
C     3)  If an I/O error occurs while reading or writing the indicated
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
C     This routine completes an EK segment after the data has been
C     written via the fast column writer routines. The segment must
C     have been created by a call to EKIFLD. The fast column
C     writer routines are:
C
C        EKACLC {EK, add column, character}
C        EKACLD {EK, add column, double precision}
C        EKACLI {EK, add column, integer}
C
C     The segment is not guaranteed to be readable until all columns
C     have been added. After the columns have been added, the segment
C     may be extended by inserting more records and filling in those
C     records using the EKACEx routines.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Suppose we want to create an Sequence Component E-kernel
C        named 'ekffld_ex1.bes' which contains records of orders for
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
C        The file "ekffld_ex1.bdb" will contain two segments, the first
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
C              PROGRAM EKFFLD_EX1
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
C              CALL EKOPN ( 'ekffld_ex1.bes', IFNAME, NRESVC, HANDLE )
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
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 24-NOV-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. and
C        created complete code example from existing fragment.
C
C-    SPICELIB Version 1.1.2, 09-JAN-2002 (NJB)
C
C        Documentation change: instances of the phrase "fast load"
C        were replaced with "fast write."
C
C-    SPICELIB Version 1.1.1, 18-JUN-1999 (WLT)
C
C        Corrected CHKOUT value to be same as CHKIN.
C
C-    SPICELIB Version 1.0.1, 31-MAR-1998 (NJB)
C
C        Made miscellaneous header corrections.
C
C-    SPICELIB Version 1.0.0, 08-NOV-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     finish fast write of an EK segment
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               STYPE

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKFFLD' )
      END IF

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

         CALL ZZEKFF01 ( HANDLE, SEGNO, RCPTRS )


      ELSE IF ( STYPE .EQ. 2 ) THEN
C
C        Currently, no actions are taken to complete a type 2 segment.
C

      ELSE

         CALL SETMSG ( 'Segment type # is not currently supported.' )
         CALL ERRINT ( '#', STYPE                                   )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'EKFFLD'                                     )
         RETURN

      END IF

      CALL CHKOUT ( 'EKFFLD' )
      RETURN
      END
