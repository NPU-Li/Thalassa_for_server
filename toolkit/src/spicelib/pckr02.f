C$Procedure PCKR02 ( PCK, read record from type 2 segment )

      SUBROUTINE PCKR02 ( HANDLE, DESCR, ET, RECORD )

C$ Abstract
C
C     Read a single PCK data record from a segment of type 2
C     (Chebyshev, 3-vector only).
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
C     PCK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     DESCR      I   Segment descriptor.
C     ET         I   Target epoch.
C     RECORD     O   Data record.
C
C$ Detailed_Input
C
C     HANDLE,
C     DESCR    are the file handle and segment descriptor for
C              a PCK segment of type 2.
C
C     ET       is a target epoch, for which a data record from
C              a specific segment is required.
C
C$ Detailed_Output
C
C     RECORD   is the record from the specified segment which,
C              when evaluated at epoch ET, will give the Euler
C              angles (orientation) of some body.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the PCK Required Reading file for a description of the
C     structure of a data type 2 (Chebyshev polynomials, Euler
C     angles only) segment.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Dump the record of a type 2 PCK which, when evaluated at
C        a given epoch, will give the Euler angles (orientation) of
C        the Moon body-fixed frame with class ID 31004 with respect
C        to J2000.
C
C        Note that the data returned is in its rawest form, taken
C        directly from the segment. As such, it will be meaningless to
C        a user unless he/she understands the structure of the data
C        type completely. Given that understanding, however, the PCKR02
C        routine might be used to "dump" and check segment data for a
C        particular epoch.
C
C        Use the PCK kernel below to obtain the record.
C
C           moon_pa_de418_1950-2050.bpc
C
C
C        Example code begins here.
C
C
C              PROGRAM PCKR02_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters
C        C
C              INTEGER               BODY
C              PARAMETER           ( BODY   = 31004 )
C
C              INTEGER               DESCSZ
C              PARAMETER           ( DESCSZ = 5     )
C
C              INTEGER               IDSIZE
C              PARAMETER           ( IDSIZE = 40    )
C
C        C
C        C     Set the maximum record size:
C        C
C        C        RSIZE = 2 + 3 * (PDEG +1)
C        C
C        C     Assume a maximum polynomial degree of 25, and
C        C     knowing that PCKR02 returns RSIZE as first element
C        C     of the output record...
C        C
C              INTEGER               RECRSZ
C              PARAMETER           ( RECRSZ = 81    )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(IDSIZE)    SEGID
C
C              DOUBLE PRECISION      BEGET
C              DOUBLE PRECISION      DESCR  ( DESCSZ )
C              DOUBLE PRECISION      ENDET
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      RECORD ( RECRSZ )
C
C              INTEGER               BADDR
C              INTEGER               BODYID
C              INTEGER               EADDR
C              INTEGER               FRAMID
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               INDEX
C              INTEGER               PCKHDL
C              INTEGER               PCKTYP
C              INTEGER               PDEG
C              INTEGER               RSIZE
C
C              LOGICAL               FOUND
C
C        C
C        C     Load the PCK file.
C        C
C              CALL PCKLOF ( 'moon_pa_de418_1950-2050.bpc', PCKHDL )
C
C        C
C        C     Set the epoch. Use ephemeris time of J2000 epoch.
C        C
C              ET = 0.D0
C
C        C
C        C     Get a segment applicable to a specified body and epoch.
C        C
C              CALL PCKSFS ( BODY, ET, HANDLE, DESCR, SEGID, FOUND )
C
C              IF ( FOUND ) THEN
C
C        C
C        C        Unpack the segment.
C        C
C                 CALL PCKUDS ( DESCR, BODYID, FRAMID, PCKTYP,
C             .                 BEGET, ENDET,  BADDR,  EADDR  )
C
C
C                 IF ( PCKTYP .EQ. 2 ) THEN
C
C                    CALL PCKR02 ( HANDLE, DESCR, ET, RECORD )
C
C                    RSIZE = RECORD(1)
C                    PDEG  = ( RSIZE - 2 ) / 3 - 1
C
C        C
C        C           Output the data.
C        C
C                    WRITE(*,*) 'Record size      : ', RSIZE
C                    WRITE(*,*) 'Polynomial degree: ', PDEG
C
C                    WRITE(*,*) 'Record data      :'
C                    WRITE(*,*) '   Interval midpoint: ', RECORD(2)
C                    WRITE(*,*) '   Interval radius  : ', RECORD(3)
C
C                    INDEX = 4
C                    WRITE(*,*) '   RA coefficients  : '
C                    DO I = 0, PDEG
C                       WRITE(*,*) '      ', RECORD(INDEX+I)
C                    END DO
C
C                    INDEX = 4 + ( PDEG + 1 )
C                    WRITE(*,*) '   DEC coefficients : '
C                    DO I = 0, PDEG
C                       WRITE(*,*) '      ', RECORD(INDEX+I)
C                    END DO
C
C                    INDEX = 4 + 2 * ( PDEG + 1 )
C                    WRITE(*,*) '   W coefficients   : '
C                    DO I = 0, PDEG
C                       WRITE(*,*) '      ', RECORD(INDEX+I)
C                    END DO
C
C                 ELSE
C
C                    WRITE(*,*) 'PCK is not type 2'
C
C                 END IF
C
C              ELSE
C
C                 WRITE(*,*) '   ***** SEGMENT NOT FOUND *****'
C
C              END IF
C
C        C
C        C     Unload the PCK file.
C        C
C              CALL PCKUOF ( PCKHDL )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Record size      :           32
C         Polynomial degree:            9
C         Record data      :
C            Interval midpoint:    302400.00000000000
C            Interval radius  :    345600.00000000000
C            RA coefficients  :
C                 -5.4242086033301107E-002
C                 -5.2241405162792561E-005
C                  8.9751456289930307E-005
C                 -1.5288696963234620E-005
C                  1.3218870864581395E-006
C                  5.9822156790328180E-007
C                 -6.5967702052551211E-008
C                 -9.9084309118396298E-009
C                  4.9276055963541578E-010
C                  1.1612267413829385E-010
C            DEC coefficients :
C                 0.42498898565916610
C                  1.3999219324235620E-004
C                 -1.8855140511098865E-005
C                 -2.1964684808526649E-006
C                  1.4229817868138752E-006
C                 -1.6991716166847001E-007
C                 -3.4824688140649506E-008
C                  2.9208428745895990E-009
C                  4.4217757657060300E-010
C                 -3.9211207055305402E-012
C            W coefficients   :
C                  2565.0633504619473
C                 0.92003769451305328
C                 -8.0503797901914501E-005
C                  1.1960860244433900E-005
C                 -1.2237900518372542E-006
C                 -5.3651349407824562E-007
C                  6.0843372260403005E-008
C                  9.0211287487688797E-009
C                 -4.6460429330339309E-010
C                 -1.0446918704281774E-010
C
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
C     J. Diaz del Rio    (ODC Space)
C     E.D. Wright        (JPL)
C     K.S. Zukor         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.2, 06-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example from existing fragment.
C
C-    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW)
C
C        Minor edits to $Procedure; clean trailing whitespace.
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.0, 11-MAR-1993 (KSZ)
C
C-&


C$ Index_Entries
C
C     read record from type_2 PCK segment
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Parameters
C
      INTEGER               ND
      PARAMETER           ( ND      =   2 )

      INTEGER               NI
      PARAMETER           ( NI     =    5 )

C
C     Local variables
C

      DOUBLE PRECISION      DC       (   ND )
      DOUBLE PRECISION      INIT
      DOUBLE PRECISION      INTLEN

      INTEGER               BEGIN
      INTEGER               END
      INTEGER               IC       (   NI )
      INTEGER               NREC
      INTEGER               RECADR
      INTEGER               RECNO
      INTEGER               RECSIZ


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKR02' )
      END IF

C
C     Unpack the segment descriptor.
C
      CALL DAFUS ( DESCR, ND, NI, DC, IC )

      BEGIN = IC( NI-1 )
      END   = IC( NI )

C
C     The segment is made up of a number of logical records, each
C     having the same size, and covering the same length of time.
C
C     We can determine which record to return by comparing the input
C     epoch with the initial time of the segment and the length of the
C     interval covered by each record.  These final two constants are
C     located at the end of the segment, along with the size of each
C     logical record and the total number of records.
C
      CALL DAFGDA ( HANDLE, END-3, END, RECORD )

      INIT   = RECORD( 1 )
      INTLEN = RECORD( 2 )
      RECSIZ = INT( RECORD( 3 ) )
      NREC   = INT( RECORD( 4 ) )

      RECNO = INT( (ET - INIT) / INTLEN ) + 1
      RECNO = MIN( RECNO, NREC )

C
C     Compute the address of the desired record.
C
      RECADR = ( RECNO - 1 )*RECSIZ + BEGIN

C
C     Along with the record, return the size of the record.
C
      RECORD( 1 ) = RECORD( 3 )
      CALL DAFGDA ( HANDLE, RECADR, RECADR + RECSIZ - 1, RECORD( 2 ) )

      CALL CHKOUT ( 'PCKR02' )
      RETURN
      END
