C$Procedure EKUCEC ( EK, update character column entry )

      SUBROUTINE EKUCEC (  HANDLE,  SEGNO,  RECNO,  COLUMN,
     .                     NVALS,   CVALS,  ISNULL          )

C$ Abstract
C
C     Update a character column entry in a specified EK record.
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
C     RECNO      I   Record in which entry is to be updated.
C     COLUMN     I   Column name.
C     NVALS      I   Number of values in new column entry.
C     CVALS      I   Character string values to add to column.
C     ISNULL     I   Flag indicating whether column entry is null.
C
C$ Detailed_Input
C
C     HANDLE   is a file handle attached to an EK open for
C              write access.
C
C     SEGNO    is the index of the segment containing the column
C              entry to be updated.
C
C     RECNO    is the index of the record containing the column
C              entry to be updated. This record number is
C              relative to the start of the segment indicated by
C              SEGNO; the first record in the segment has index 1.
C
C     COLUMN   is the name of the column containing the entry to
C              be updated.
C
C     NVALS,
C     CVALS    are, respectively, the number of values to add to
C              the specified column and the set of values
C              themselves. The data values are written in to the
C              specified column and record.
C
C              If the  column has fixed-size entries, then NVALS
C              must equal the entry size for the specified column.
C
C              For columns with variable-sized entries, the size
C              of the new entry need not match the size of the
C              entry it replaces. In particular, the new entry
C              may be larger.
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
C              The new entry may be null even though it replaces
C              a non-null value, and vice versa.
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
C         CHARACTER, the error SPICE(WRONGDATATYPE) is signaled.
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
C         a character class known to this routine, the error
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
C     be added one logical element at a time. Partial assignments of
C     logical elements are not supported.
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
C        file, how to update the data in this record, and how to read
C        the data from it.
C
C
C        Example code begins here.
C
C
C              PROGRAM EKUCEC_EX1
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
C              PARAMETER           ( EKNAME = 'ekucec_ex1.bdb' )
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
C        C     Negate the values in the odd-numbered records
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
C        ISNULL flag to true for that record. The odd value record
C        numbers have negative values as a result of the update calls.
C
C        After run completion, a new EK exists in the output directory.
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
C-    SPICELIB Version 1.2.1, 06-AUG-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C-    SPICELIB Version 1.2.0, 06-FEB-2015 (NJB)
C
C        Now uses ERRHAN to insert DAS file name into
C        long error messages.
C
C        Corrected some header comment errors (cut-and-paste
C        errors referring to double precision or time data).
C
C-    SPICELIB Version 1.1.0, 20-JUN-1999 (WLT)
C
C        Removed unbalanced call to CHKOUT.
C
C-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     replace character entry in an EK column
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

      LOGICAL               ISSHAD

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

         CALL CHKIN  ( 'EKUCEC'                                        )
         CALL SETMSG ( 'Column # is of type #; EKUCEC only works '    //
     .                 'with character columns.  RECNO = #; '      //
     .                 'SEGNO = #; EK = #.'                            )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                          )
         CALL CHKOUT ( 'EKUCEC'                                        )
         RETURN

      END IF

C
C     Look up the record pointer for the target record.
C
      CALL ZZEKTRDP ( HANDLE, SEGDSC(RTIDX), RECNO, RECPTR )

C
C     Determine whether the EK is shadowed.
C
      CALL EKSHDW ( HANDLE, ISSHAD )

C
C     If the EK is shadowed, we must back up the current column entry
C     if the entry has not already been backed up.  ZZEKRBCK will
C     handle this task.
C
      IF ( ISSHAD ) THEN
         CALL ZZEKRBCK ( 'UPDATE', HANDLE, SEGDSC, COLDSC, RECNO )
      END IF

C
C     Now it's time to carry out the replacement.
C
      CLASS  =  COLDSC ( CLSIDX )


      IF ( CLASS .EQ. 3 ) THEN
C
C        Class 3 columns contain scalar character data.
C
         CALL ZZEKUE03 ( HANDLE, SEGDSC, COLDSC, RECPTR, CVALS, ISNULL )


      ELSE IF ( CLASS .EQ. 6 ) THEN
C
C        Class 6 columns contain array-valued character data.
C
         CALL ZZEKUE06 ( HANDLE,  SEGDSC, COLDSC,
     .                   RECPTR,  NVALS,  CVALS,  ISNULL )

      ELSE
C
C        This is an unsupported character column class.
C
         SEGNO  =  SEGDSC ( SNOIDX )

         CALL CHKIN  ( 'EKUCEC'                                        )
         CALL SETMSG ( 'Class # from input column descriptor is not ' //
     .                 'a supported character class.  COLUMN = #; '   //
     .                 'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'EKUCEC'                                        )
         RETURN

      END IF

      RETURN
      END
