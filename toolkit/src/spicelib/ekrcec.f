C$Procedure EKRCEC ( EK, read column entry element, character )

      SUBROUTINE EKRCEC ( HANDLE,  SEGNO,  RECNO,  COLUMN,
     .                    NVALS,   CVALS,  ISNULL          )

C$ Abstract
C
C     Read data from a character column in a specified EK record.
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
C     HANDLE     I   Handle attached to EK file.
C     SEGNO      I   Index of segment containing record.
C     RECNO      I   Record from which data is to be read.
C     COLUMN     I   Column name.
C     NVALS      O   Number of values in column entry.
C     CVALS      O   Character values in column entry.
C     ISNULL     O   Flag indicating whether column entry is null.
C
C$ Detailed_Input
C
C     HANDLE   is an EK file handle. The file may be open for read or
C              write access.
C
C     SEGNO    is the index of the segment from which data is to be
C              read.
C
C     RECNO    is the index of the record from which data is to be read.
C              This record number is relative to the start of the
C              segment indicated by SEGNO; the first record in the
C              segment has index 1.
C
C     COLUMN   is the name of the column from which data is to be read.
C
C$ Detailed_Output
C
C     NVALS,
C     CVALS    are, respectively, the number of values found in the
C              specified column entry and the set of values themselves.
C              The array CVALS must have sufficient string length to
C              accommodate the longest string in the returned column
C              entry.
C
C              For columns having fixed-size entries, when a column
C              entry is null, NVALS is still set to the column entry
C              size. For columns having variable- size entries, NVALS is
C              set to 1 for null entries.
C
C     ISNULL   is a logical flag indicating whether the returned column
C              entry is null.
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
C     3)  If RECNO is out of range, an error is signaled by a routine in
C         the call tree of this routine.
C
C     4)  If COLUMN is not the name of a declared column, an error
C         is signaled by a routine in the call tree of this routine.
C
C     5)  If COLUMN specifies a column of whose data type is not
C         character, the error SPICE(WRONGDATATYPE) is signaled.
C
C     6)  If COLUMN specifies a column of whose class is not
C         a character class known to this routine, the error
C         SPICE(NOCLASS) is signaled.
C
C     7)  If an attempt is made to read an uninitialized column entry,
C         an error is signaled by a routine in the call tree of this
C         routine. A null entry is considered to be initialized, but
C         entries do not contain null values by default.
C
C     8)  If an I/O error occurs while reading or writing the indicated
C         file, the error is signaled by a routine in the call tree of
C         this routine.
C
C     9)  If any element of the column entry would be truncated when
C         assigned to an element of CVALS, an error is signaled by a
C         routine in the call tree of this routine.
C
C$ Files
C
C     See the EK Required Reading ek.req for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine is a utility that allows an EK file to be read
C     directly without using the high-level query interface.
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
C        file, and how to read the data from it.
C
C
C        Example code begins here.
C
C
C              PROGRAM EKRCEC_EX1
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
C              PARAMETER           ( EKNAME = 'ekrcec_ex1.bdb' )
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
C            values       : 3
C
C         Data from column: CHR_COL_2
C            record number:            4
C            values       : 30       31       32       33
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
C            values       : 5
C
C         Data from column: CHR_COL_2
C            record number:            6
C            values       : 50       51       52       53
C
C
C        Note that the second record does not appear due to setting the
C        ISNULL flag to true for that record.
C
C        After run completion, a new EK exists in the output directory.
C
C$ Restrictions
C
C     1)  EK files open for write access are not necessarily readable.
C         In particular, a column entry can be read only if it has been
C         initialized. The caller is responsible for determining
C         when it is safe to read from files open for write access.
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
C-    SPICELIB Version 1.4.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C-    SPICELIB Version 1.3.0, 06-FEB-2015 (NJB)
C
C        Now uses ERRHAN to insert DAS file name into
C        long error messages.
C
C-    SPICELIB Version 1.2.0, 20-JUN-1999 (WLT)
C
C        Removed unbalanced call to CHKOUT.
C
C-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
C
C        Bug fix: Record number, not record pointer, is now supplied
C        to look up data in the class 9 case. Miscellaneous header
C        changes were made as well. Check for string truncation on
C        output has been added.
C
C-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     read character data from EK column
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
C
C        Bug fix: Record number, not record pointer, is now supplied
C        to look up data in the class 9 case. For class 9 columns,
C        column entry locations are calculated directly from record
C        numbers, no indirection is used.
C
C        Miscellaneous header changes were made as well.
C
C        The routines
C
C           ZZEKRD03
C           ZZEKRD06
C           ZZEKRD09
C
C        now check for string truncation on output and signal errors
C        if truncation occurs.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED

C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKESIZ

C
C     Local variables
C
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               CLASS
      INTEGER               CVLEN
      INTEGER               DTYPE
      INTEGER               RECPTR
      INTEGER               SEGDSC ( SDSCSZ )

      LOGICAL               FOUND

C
C     Use discovery check-in.
C
C     First step:  find the descriptor for the named segment.  Using
C     this descriptor, get the column descriptor.
C
      CALL ZZEKSDSC ( HANDLE, SEGNO,  SEGDSC          )
      CALL ZZEKCDSC ( HANDLE, SEGDSC, COLUMN, COLDSC  )

      IF ( FAILED() ) THEN
         RETURN
      END IF

C
C     This column had better be of character type.
C
      DTYPE  =  COLDSC(TYPIDX)

      IF ( DTYPE .NE. CHR ) THEN

         CALL CHKIN  ( 'EKRCEC'                                        )
         CALL SETMSG ( 'Column # is of type #; EKRCEC only works '    //
     .                 'with character columns.  RECNO = #; SEGNO = ' //
     .                 '#; EK = #.'                                    )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                          )
         CALL CHKOUT ( 'EKRCEC'                                        )
         RETURN

      END IF

C
C     Now it's time to read data from the file.  Call the low-level
C     reader appropriate to the column's class.
C
      CLASS  =  COLDSC ( CLSIDX )


      IF ( CLASS .EQ. 3 ) THEN
C
C        Look up the record pointer for the target record.
C
         CALL ZZEKTRDP (  HANDLE,  SEGDSC(RTIDX),  RECNO, RECPTR )

         CALL ZZEKRD03 (  HANDLE,  SEGDSC, COLDSC,
     .                    RECPTR,  CVLEN,  CVALS,  ISNULL  )
         NVALS  =  1


      ELSE IF ( CLASS .EQ. 6 ) THEN

         CALL ZZEKTRDP ( HANDLE, SEGDSC(RTIDX), RECNO, RECPTR )

         NVALS  =  ZZEKESIZ ( HANDLE, SEGDSC, COLDSC, RECPTR )

         CALL ZZEKRD06 (  HANDLE,  SEGDSC,  COLDSC,  RECPTR,
     .                    1,       NVALS,   CVALS,   ISNULL,  FOUND  )


      ELSE IF ( CLASS .EQ. 9 ) THEN
C
C        Records in class 9 columns are identified by a record number
C        rather than a pointer.
C
         CALL ZZEKRD09 (  HANDLE,  SEGDSC, COLDSC,
     .                    RECNO,   CVLEN,  CVALS,  ISNULL  )
         NVALS  =  1

      ELSE

C
C        This is an unsupported character column class.
C
         SEGNO  =  SEGDSC ( SNOIDX )

         CALL CHKIN  ( 'EKRCEC'                                        )
         CALL SETMSG ( 'Class # from input column descriptor is not ' //
     .                 'a supported character class.  COLUMN = #; '   //
     .                 'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'EKRCEC'                                        )
         RETURN

      END IF


      RETURN
      END
