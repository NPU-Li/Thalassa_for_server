C$Procedure DAFGDA ( DAF, read data from address )

      SUBROUTINE DAFGDA ( HANDLE, BADDR, EADDR, DATA )

C$ Abstract
C
C     Read the double precision data bounded by two addresses within
C     a DAF.
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
C     DAF
C
C$ Keywords
C
C     FILES
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               BADDR
      INTEGER               EADDR
      DOUBLE PRECISION      DATA     ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a DAF.
C     BADDR,
C     EADDR      I   Initial, final address within file.
C     DATA       O   Data contained between BADDR and EADDR.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of a DAF.
C
C     BADDR,
C     EADDR    are the initial and final addresses of a contiguous
C              set of double precision numbers within a DAF.
C              Presumably, these make up all or part of a particular
C              array.
C
C$ Detailed_Output
C
C     DATA     are the double precision data contained between
C              the specified addresses within the specified file.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If BADDR is zero or negative, the error SPICE(DAFNEGADDR)
C         is signaled.
C
C     2)  If BADDR > EADDR, the error SPICE(DAFBEGGTEND) is signaled.
C
C     3)  If HANDLE is invalid, an error is signaled by a routine in the
C         call tree of this routine.
C
C     4)  If the range of addresses covered between BADDR and EADDR
C         includes records that do not contain strictly double
C         precision data, then the values returned in DATA are
C         undefined. See the $Restrictions section below for details.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The principal reason that DAFs are so easy to use is that
C     the data in each DAF are considered to be one long contiguous
C     set of double precision numbers. You can grab data from anywhere
C     within a DAF without knowing (or caring) about the physical
C     records in which they are stored.
C
C     This routine replaces DAFRDA as the principle mechanism for
C     reading the contents of DAF arrays.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Open a type 8 SPK for read access, retrieve the data for
C        the first segment and identify the beginning and end addresses,
C        the number of data elements within, the size of the data
C        array, and print the first two records.
C
C        Use the SPK kernel below as input type 8 SPK file for the
C        example.
C
C           mer1_ls_040128_iau2000_v1.bsp
C
C        Each segment contains only two records which provide the start
C        and end position for the MER-1 rover landing site in the
C        IAU_MARS frame. Since the landing site does not change over
C        time, it is expected that both records are equal.
C
C
C        Example code begins here.
C
C
C              PROGRAM DAFGDA_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local constants.
C        C
C              CHARACTER*(*)         FMT
C              PARAMETER           ( FMT = '(6F10.3)' )
C
C              INTEGER               MAXDAT
C              PARAMETER           ( MAXDAT = 1000 )
C
C              INTEGER               MAXSUM
C              PARAMETER           ( MAXSUM = 125  )
C
C              INTEGER               ND
C              PARAMETER           ( ND     = 2    )
C
C              INTEGER               NI
C              PARAMETER           ( NI     = 6    )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      DAFSUM ( MAXSUM )
C              DOUBLE PRECISION      DATA   ( MAXDAT )
C              DOUBLE PRECISION      DC     ( ND )
C
C              INTEGER               BADDR
C              INTEGER               EADDR
C              INTEGER               HANDLE
C              INTEGER               IC     ( NI )
C              INTEGER               SIZE
C
C              LOGICAL               FOUND
C
C        C
C        C     Open the type 8 SPK for read access, then read the
C        C     data from the first segment.
C        C
C              CALL DAFOPR ( 'mer1_ls_040128_iau2000_v1.bsp', HANDLE )
C
C        C
C        C     Begin a forward search; find the first segment; read the
C        C     segment summary.
C        C
C              CALL DAFBFS ( HANDLE )
C              CALL DAFFNA ( FOUND  )
C              CALL DAFGS  ( DAFSUM )
C              CALL DAFUS  ( DAFSUM, ND, NI, DC, IC )
C
C        C
C        C     Retrieve the data begin and end addresses.
C        C
C              BADDR = IC(5)
C              EADDR = IC(6)
C
C              WRITE(*,'(A,I4)') 'Beginning address       : ', BADDR
C              WRITE(*,'(A,I4)') 'Ending address          : ', EADDR
C              WRITE(*,'(A,I4)') 'Number of data elements : ',
C             .                                    EADDR - BADDR + 1
C
C        C
C        C     Extract all data bounded by the begin and end addresses.
C        C
C              CALL DAFGDA ( HANDLE, BADDR, EADDR, DATA )
C
C        C
C        C     Check the data.
C        C
C              WRITE(*,'(A)') 'The first and second states '
C             .            // 'stored in the segment:'
C              WRITE(*,FMT) DATA(1),  DATA(2),  DATA(3),
C             .             DATA(4),  DATA(5),  DATA(6)
C              WRITE(*,FMT) DATA(7),  DATA(8),  DATA(9),
C             .             DATA(10), DATA(11), DATA(12)
C
C        C
C        C     Safely close the file
C        C
C              CALL DAFCLS ( HANDLE )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Beginning address       :  897
C        Ending address          :  912
C        Number of data elements :   16
C        The first and second states stored in the segment:
C          3376.422  -326.649  -115.392     0.000     0.000     0.000
C          3376.422  -326.649  -115.392     0.000     0.000     0.000
C
C
C$ Restrictions
C
C     1)  There are several types of records in a DAF. This routine
C         is only to be used to read double precision data bounded
C         between two DAF addresses. The range of addresses input
C         may not cross data and summary record boundaries.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     F.S. Turner        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
C
C        Changed the input argument names BEGIN and END to BADDR to
C        EADDR for consistency with other routines.
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added
C        complete code example. Removed unnecessary $Revisions section.
C
C-    SPICELIB Version 1.0.0, 16-NOV-2001 (FST)
C
C-&


C$ Index_Entries
C
C     read data from DAF address
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      INTEGER               BSIZE
      PARAMETER           ( BSIZE = 128 )

      INTEGER               BEGR
      INTEGER               BEGW
      INTEGER               ENDR
      INTEGER               ENDW

      INTEGER               RECNO
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               NEXT

      LOGICAL               FOUND

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

C
C     Bad addresses?
C
      IF ( BADDR .LE. 0 ) THEN
         CALL CHKIN  ( 'DAFGDA' )
         CALL SETMSG ( 'Negative value for BADDR address: #' )
         CALL ERRINT ( '#', BADDR )
         CALL SIGERR ( 'SPICE(DAFNEGADDR)' )
         CALL CHKOUT ( 'DAFGDA' )
         RETURN

      ELSE IF ( BADDR .GT. EADDR ) THEN
         CALL CHKIN  ( 'DAFGDA' )
         CALL SETMSG ( 'Beginning address (#) greater than ending '   //
     .                 'address (#).' )
         CALL ERRINT ( '#', BADDR )
         CALL ERRINT ( '#', EADDR   )
         CALL SIGERR ( 'SPICE(DAFBEGGTEND)' )
         CALL CHKOUT ( 'DAFGDA' )
         RETURN
      END IF

C
C     Convert raw addresses to record/word representations.
C
      CALL DAFARW ( BADDR, BEGR, BEGW )
      CALL DAFARW ( EADDR, ENDR, ENDW )

C
C     Get as many records as needed. Return the last part of the
C     first record, the first part of the last record, and all of
C     every record in between. Any record not found is assumed to
C     be filled with zeros.
C
      NEXT = 1

      DO RECNO = BEGR, ENDR

         IF ( BEGR .EQ. ENDR ) THEN
            FIRST = BEGW
            LAST  = ENDW

         ELSE IF ( RECNO .EQ. BEGR ) THEN
            FIRST = BEGW
            LAST  = BSIZE

         ELSE IF ( RECNO .EQ. ENDR ) THEN
            FIRST = 1
            LAST  = ENDW

         ELSE
            FIRST = 1
            LAST  = BSIZE
         END IF

         CALL DAFGDR ( HANDLE, RECNO, FIRST, LAST, DATA( NEXT ), FOUND )

         IF ( .NOT. FOUND ) THEN
            CALL CLEARD ( LAST-FIRST+1, DATA( NEXT ) )
         END IF

         NEXT = NEXT + ( LAST-FIRST+1 )

      END DO

      RETURN
      END
