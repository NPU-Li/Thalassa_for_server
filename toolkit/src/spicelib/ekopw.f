C$Procedure EKOPW ( EK, open file for writing )

      SUBROUTINE EKOPW ( FNAME, HANDLE )

C$ Abstract
C
C     Open an existing E-kernel file for writing.
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

      CHARACTER*(*)         FNAME
      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of EK file.
C     HANDLE     O   Handle attached to EK file.
C
C$ Detailed_Input
C
C     FNAME    is the name of an existing E-kernel file to be
C              opened for write access.
C
C$ Detailed_Output
C
C     HANDLE   is the DAS file handle of the EK designate by
C              FNAME. This handle is used to identify the file
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
C         by a routine in the call tree of this routine.
C
C     2)  If the indicated file has the wrong architecture version, an
C         error is signaled by a routine in the call tree of this
C         routine.
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
C     This routine should be used to open an EK existing file for write
C     access.
C
C     Opening an EK file with this routine makes the EK accessible to
C     the following SPICELIB EK access routines, all of which modify
C     the target EK file:
C
C        Begin segment:
C
C           EKBSEG
C
C        Append, insert, delete records:
C
C           EKAPPR
C           EKINSR
C           EKDELR
C
C        Add column entries:
C
C           EKACEC
C           EKACED
C           EKACEI
C
C        Update existing column entries:
C
C           EKUCEC
C           EKUCED
C           EKUCEI
C
C        Execute fast write:
C
C           EKIFLD
C           EKFFLD
C           EKACLC
C           EKACLD
C           EKACLI
C
C     An EK opened for write access is also accessible for reading.
C     The file may be accessed by the SPICELIB EK readers
C
C           EKRCEC
C           EKRCED
C           EKRCEI
C
C        and summary routines:
C
C           EKNSEG
C           EKSSUM
C
C
C     An EK opened for write access cannot be queried. To make an EK
C     available to the EK query system, the file must be loaded via
C     FURNSH or EKLEF, rather than by this routine. See the EK Required
C     Reading for further information.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) The following program demonstrates how to create a new EK and
C        add data to a character column in a given record within the
C        file, how to re-open the file for write access and update the
C        data, and how to read the data from it.
C
C
C        Example code begins here.
C
C
C              PROGRAM EKOPW_EX1
C              IMPLICIT NONE
C
C        C
C        C     Include the EK Column Name Size (CNAMSZ)
C        C
C              INCLUDE 'ekcnamsz.inc'
C
C        C
C        C     Local constants.
C        C
C              CHARACTER*(*)         EKNAME
C              PARAMETER           ( EKNAME = 'ekopw_ex1.bdb' )
C
C              CHARACTER*(*)         IFNAME
C              PARAMETER           ( IFNAME = 'Test EK'  )
C
C              CHARACTER*(*)         TABLE
C              PARAMETER           ( TABLE  = 'CHR_DATA' )
C
C              INTEGER               CVLEN
C              PARAMETER           ( CVLEN  = 9  )
C
C              INTEGER               DECLEN
C              PARAMETER           ( DECLEN = 200 )
C
C              INTEGER               MAXVAL
C              PARAMETER           ( MAXVAL = 4  )
C
C              INTEGER               NCOLS
C              PARAMETER           ( NCOLS  = 2  )
C
C              INTEGER               NROWS
C              PARAMETER           ( NROWS  = 6  )
C
C              INTEGER               NRESVC
C              PARAMETER           ( NRESVC = 0  )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(DECLEN)    CDECLS ( NCOLS  )
C              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS  )
C              CHARACTER*(CVLEN)     CVALS  ( MAXVAL )
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               J
C              INTEGER               NVALS
C              INTEGER               RECNO
C              INTEGER               SEGNO
C
C              LOGICAL               ISNULL
C
C
C        C
C        C     Open a new EK file.  For simplicity, we won't
C        C     reserve space for the comment area, so the
C        C     number of reserved comment characters is zero.
C        C     The constant IFNAME is the internal file name.
C        C
C              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
C
C        C
C        C     Set up the table and column names and declarations
C        C     for the CHR_DATA segment.  We'll index all of
C        C     the columns.
C        C
C              CNAMES(1) =  'CHR_COL_1'
C              CDECLS(1) =  'DATATYPE = CHARACTER*(*), '
C             .       //    'INDEXED = TRUE, NULLS_OK = TRUE'
C
C              CNAMES(2) =  'CHR_COL_2'
C              CDECLS(2) =  'DATATYPE = CHARACTER*(9), '
C             .       //    'SIZE = VARIABLE, NULLS_OK = TRUE'
C
C        C
C        C     Start the segment.
C        C
C              CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
C             .              CNAMES, CDECLS, SEGNO )
C
C              DO I = 0, NROWS-1
C
C                 CALL EKAPPR ( HANDLE, SEGNO, RECNO )
C
C                 ISNULL = ( I .EQ. 1 )
C
C                 CALL INTSTR ( I, CVALS(1) )
C                 CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(1),
C             .                 1,      CVALS, ISNULL           )
C
C        C
C        C        Array-valued columns follow.
C        C
C                 CALL INTSTR ( 10*I,     CVALS(1) )
C                 CALL INTSTR ( 10*I + 1, CVALS(2) )
C                 CALL INTSTR ( 10*I + 2, CVALS(3) )
C                 CALL INTSTR ( 10*I + 3, CVALS(4) )
C                 CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(2),
C             .                 4,      CVALS, ISNULL           )
C
C              END DO
C
C        C
C        C     End the file.
C        C
C              CALL EKCLS ( HANDLE )
C
C        C
C        C     Open the EK for write access.
C        C
C              CALL EKOPW ( EKNAME, HANDLE )
C
C        C
C        C     Negate the values in the even-numbered records
C        C     using the update routines.
C        C
C              DO I = 1, NROWS, 2
C
C                 RECNO  = I+1
C
C                 ISNULL = ( I .EQ. 1 )
C
C                 CALL INTSTR ( -I, CVALS(1) )
C                 CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(1),
C             .                 1,      CVALS, ISNULL           )
C
C        C
C        C        Array-valued columns follow.
C        C
C                 CALL INTSTR ( -10*I,       CVALS(1) )
C                 CALL INTSTR ( -(10*I + 1), CVALS(2) )
C                 CALL INTSTR ( -(10*I + 2), CVALS(3) )
C                 CALL INTSTR ( -(10*I + 3), CVALS(4) )
C                 CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(2),
C             .                 4,      CVALS, ISNULL           )
C
C              END DO
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
C              DO I = 1, NROWS
C
C                 CALL EKRCEC ( HANDLE, SEGNO, I,     CNAMES(1),
C             .                 NVALS,  CVALS, ISNULL           )
C
C                 IF ( .NOT. ISNULL ) THEN
C
C                    WRITE(*,*) 'Data from column: ', CNAMES(1)
C                    WRITE(*,*) '   record number: ', I
C                    WRITE(*,*) '   values       : ',
C             .                                ( CVALS(J), J=1,NVALS )
C                    WRITE(*,*) ' '
C
C                 ELSE
C
C                    WRITE(*,*) 'Record ', I, 'flag is NULL.'
C                    WRITE(*,*) ' '
C
C                 END IF
C
C        C
C        C        Array-valued columns follow.
C        C
C                 CALL EKRCEC ( HANDLE, SEGNO, I,     CNAMES(2),
C             .                 NVALS,  CVALS, ISNULL           )
C
C                 IF ( .NOT. ISNULL ) THEN
C
C                    WRITE(*,*) 'Data from column: ', CNAMES(2)
C                    WRITE(*,*) '   record number: ', I
C                    WRITE(*,*) '   values       : ',
C             .                                ( CVALS(J), J=1,NVALS )
C                    WRITE(*,*) ' '
C
C                 END IF
C
C              END DO
C
C        C
C        C     Close the file.
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
C         Data from column: CHR_COL_1
C            record number:            1
C            values       : 0
C
C         Data from column: CHR_COL_2
C            record number:            1
C            values       : 0        1        2        3
C
C         Record            2 flag is NULL.
C
C         Data from column: CHR_COL_1
C            record number:            3
C            values       : 2
C
C         Data from column: CHR_COL_2
C            record number:            3
C            values       : 20       21       22       23
C
C         Data from column: CHR_COL_1
C            record number:            4
C            values       : -3
C
C         Data from column: CHR_COL_2
C            record number:            4
C            values       : -30      -31      -32      -33
C
C         Data from column: CHR_COL_1
C            record number:            5
C            values       : 4
C
C         Data from column: CHR_COL_2
C            record number:            5
C            values       : 40       41       42       43
C
C         Data from column: CHR_COL_1
C            record number:            6
C            values       : -5
C
C         Data from column: CHR_COL_2
C            record number:            6
C            values       : -50      -51      -52      -53
C
C
C        Note that the second record does not appear due to setting the
C        ISNULL flag to .TRUE. for that record. The odd value record
C        numbers have negative values as a result of the update calls.
C
C        After run completion, a new EK exists in the output directory.
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
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Updated exception #1 to remove the statement about deleting the
C        opened file upon failure.
C
C        Updated "fast write" list of API, which was listing the wrong
C        APIs for adding data.
C
C        Added FTSIZE parameter description. Updated index entry.
C
C-    SPICELIB Version 1.0.1, 09-JAN-2002 (NJB)
C
C        Documentation change: instances of the phrase "fast load"
C        were replaced with "fast write."
C
C-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     open existing EK for writing
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKOPW' )
      END IF

C
C     Open the file as a DAS file.
C
      CALL DASOPW ( FNAME, HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKOPW' )
         RETURN
      END IF

C
C     Nothing doing unless the architecture is correct.  This file
C     should be a paged DAS EK.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )


      CALL CHKOUT ( 'EKOPW' )
      RETURN
      END
