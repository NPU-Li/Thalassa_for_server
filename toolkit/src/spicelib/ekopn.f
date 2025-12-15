C$Procedure EKOPN ( EK, open new file )

      SUBROUTINE EKOPN ( FNAME, IFNAME, NCOMCH, HANDLE )

C$ Abstract
C
C     Open a new E-kernel file and prepare the file for writing.
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
C     NAIF_IDS
C     TIME
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

      CHARACTER*(*)         FNAME
      CHARACTER*(*)         IFNAME
      INTEGER               NCOMCH
      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of EK file.
C     IFNAME     I   Internal file name.
C     NCOMCH     I   The number of characters to reserve for comments.
C     HANDLE     O   Handle attached to new EK file.
C
C$ Detailed_Input
C
C     FNAME    is the name of a new E-kernel file to be created.
C
C     IFNAME   is the internal file name of a new E-kernel. The
C              internal file name may be up to 60 characters in
C              length.
C
C     NCOMCH   is the amount of space, measured in characters, to
C              be allocated in the comment area when the new EK
C              file is created. It is not necessary to allocate
C              space in advance in order to add comments, but
C              doing so may greatly increase the efficiency with
C              which comments may be added. Making room for
C              comments after data has already been added to the
C              file involves moving the data, and thus is slower.
C
C              NCOMCH must be greater than or equal to zero.
C
C$ Detailed_Output
C
C     HANDLE   is the EK handle of the file designated by FNAME.
C              This handle is used to identify the file to other
C              EK routines.
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
C     1)  If NCOMCH is less than zero, the error SPICE(INVALIDCOUNT)
C         is signaled. No file will be created.
C
C     2)  If IFNAME is invalid, an error is signaled by a routine in the
C         call tree of this routine.
C
C     3)  If the indicated file cannot be opened, an error is signaled
C         by a routine in the call tree of this routine. The new file
C         will be deleted.
C
C     4)  If an I/O error occurs while reading or writing the indicated
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
C     This routine operates by side effects: it opens and prepares
C     an EK for addition of data.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) This example demonstrates how to open a new EK file, creating
C        the two segments described below, without any records.
C
C        The EK file will contain two segments, the first containing
C        the DATAORDERS table and the second containing the DATAITEMS
C        table.
C
C        The E-kernel DATAORDERS table called consists of the set of
C        columns listed below:
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
C        The columns of the DATAITEMS table are shown below:
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
C        Note that it is not necessary to populate the first segment
C        with data before starting the second segment, or before
C        closing the EK.
C
C
C        Example code begins here.
C
C
C              PROGRAM EKOPN_EX1
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
C              PARAMETER           ( EKNAME  = 'ekopn_ex1.bdb' )
C
C              INTEGER               DECLEN
C              PARAMETER           ( DECLEN = 200 )
C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 40  )
C
C              INTEGER               NCOLS
C              PARAMETER           ( NCOLS  = 6   )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(DECLEN)    CDECLS ( NCOLS )
C              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS )
C              CHARACTER*(NAMLEN)    IFNAME
C
C              INTEGER               HANDLE
C              INTEGER               NRESVC
C              INTEGER               SEGNO
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
C        C     Start the first segment. Since we have no data for this
C        C     segment, start the segment by just defining the new
C        C     segment's schema.
C        C
C              CALL EKBSEG ( HANDLE, 'DATAORDERS', 6,
C             .              CNAMES, CDECLS,       SEGNO )
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
C              END
C
C
C        When this program is executed, no output is presented on
C        screen. After run completion, a new EK file exists in the
C        output directory.
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
C        Edited the header to comply with NAIF standard.
C        Added complete code example, and updated $Restrictions and
C        $Parameters sections.
C
C-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     open new E-kernel
C     open new EK
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
      INTEGER               NWC
      PARAMETER           ( NWC    = 1024 )

C
C     Local variables
C
      INTEGER               BASE
      INTEGER               NCR
      INTEGER               P

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKOPN' )
      END IF

C
C     Check the comment character count.
C
      IF ( NCOMCH .LT. 0 ) THEN

         CALL SETMSG ( 'The number of reserved comment characters ' //
     .                 'must be non-negative but was #.'            )
         CALL ERRINT ( '#',  NCOMCH                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                        )
         CALL CHKOUT ( 'EKOPN'                                      )
         RETURN

      END IF

C
C     A new DAS file is a must.  The file type is EK.
C     Reserve enough comment records to accommodate the requested
C     number of comment characters.
C
      NCR  =  ( NWC + NCOMCH - 1 ) / NWC

      CALL DASONW ( FNAME, 'EK', IFNAME, NCR, HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKOPN' )
         RETURN
      END IF

C
C     Initialize the file for paged access.  The EK architecture
C     code is automatically set by the paging initialization routine.
C
      CALL ZZEKPGIN ( HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKOPN' )
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
      CALL CHKOUT ( 'EKOPN' )
      RETURN
      END
