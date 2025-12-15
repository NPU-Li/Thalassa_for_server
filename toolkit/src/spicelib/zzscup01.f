C$Procedure ZZSCUP01 ( Private, SCLK database update, type 01 )

      SUBROUTINE ZZSCUP01 ( SC,     POLCTR, HDSCLK, SCPOOL, CLKLST,
     .                      DPFREE, DPBUFF, IFREE,  INTBUF, SCBASE,
     .                      PRVSC,  NFIELD, DELCDE, TIMSYS, NCOEFF,
     .                      NPART,  COFBAS, STRBAS, ENDBAS, MODBAS,
     .                      OFFBAS                                 )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     If necessary, update the SCLK type 01 database and associated
C     SCLK parameters.
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
C     SCLK
C     TIME
C
C$ Keywords
C
C     SCLK
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzsc01.inc'

      INTEGER               SC
      INTEGER               POLCTR
      INTEGER               HDSCLK ( * )
      INTEGER               SCPOOL ( LBSNGL : * )
      INTEGER               CLKLST ( * )
      INTEGER               DPFREE
      DOUBLE PRECISION      DPBUFF ( * )
      INTEGER               IFREE
      INTEGER               INTBUF ( * )
      INTEGER               SCBASE ( * )
      INTEGER               PRVSC
      INTEGER               NFIELD
      INTEGER               DELCDE
      INTEGER               TIMSYS
      INTEGER               NCOEFF
      INTEGER               NPART
      INTEGER               COFBAS
      INTEGER               STRBAS
      INTEGER               ENDBAS
      INTEGER               MODBAS
      INTEGER               OFFBAS

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   SCLK ID code.
C     POLCTR    I-O  Kernel pool update counter.
C     HDSCLK    I-O  Array of heads of hash collision lists.
C     SCPOOL    I-O  Singly linked collision list pool.
C     CLKLST    I-O  Array of SCLK codes.
C     DPFREE    I-O  Index of first free entry in d.p. data buffer.
C     DPBUFF    I-O  Double precision SCLK data buffer.
C     IFREE     I-O  Index of first free entry in integer data buffer.
C     INTBUF    I-O  Integer SCLK data buffer.
C     SCBASE    I-O  Array of SCLK integer buffer base indices.
C     PRVSC     I-O  Previous SCLK ID code.
C     NFIELD    I-O  Number of fields of current SCLK.
C     DELCDE    I-O  Delimiter code of current SCLK.
C     TIMSYS    I-O  Time system code of current SCLK.
C     NCOEFF    I-O  Number of coefficients current SCLK.
C     NPART     I-O  Number of partitions of current SCLK.
C     COFBAS    I-O  Base index of coefficients in d.p. buffer.
C     STRBAS    I-O  Base index of partition starts in d.p. buffer.
C     ENDBAS    I-O  Base index of partition stops in d.p. buffer.
C     MODBAS    I-O  Base index of SCLK moduli in d.p. buffer.
C     OFFBAS    I-O  Base index of SCLK offsets in d.p. buffer.
C
C$ Detailed_Input
C
C     SC          is the SCLK ID of a type 1 clock. Data for this clock
C                 are to be added to the type 1 database.
C
C     POLCTR      is the type 1 SCLK database's kernel pool tracking
C                 counter.
C
C     HDSCLK      is an array containing the head node indices of the
C                 SCLK ID collision lists stored in SCPOOL.
C
C     SCPOOL      is a singly linked list pool containing collision
C                 lists for sets of SCLK IDs that have the same hash
C                 values.
C
C     CLKLST      is an array of SCLK IDs, which is parallel to the
C                 data portion of SCPOOL. Each allocated node of
C                 SCPOOL is the index in CLKLST of the associated
C                 SCLK ID.
C
C     DPFREE      is index in the database's double precision buffer of
C                 the first free element.
C
C     DPBUFF      is the double precision buffer of the type 1
C                 database.
C
C     IFREE       is index in the database's integer buffer of the
C                 first free element.
C
C     INTBUF      is the integer buffer of the type 1 database.
C
C     SCBASE      is an array of base addresses in the array INTBUF of
C                 the integer data sets associated with the clocks
C                 in the type 1 database. The Ith element of SCBASE is
C                 the base address of the clock at index I in the
C                 array CLKLST.
C
C                 The "base" address of an integer data set immediately
C                 precedes the first index of that data set: the index
C                 in INTBUF of the first element of the integer data of
C                 CLKLST(I) is SCBASE(I)+1.
C
C     PRVSC       is the SCLK ID code of the type 1 clock seen on the
C                 previous, successful call to any SC01 entry point. If
C                 the last call was unsuccessful, or if the current
C                 call is the first, PRVSC will be 0, which is never a
C                 valid SCLK ID code. If a clock of type other than 1
C                 was seen by SCTY01 on the previous call, PRVSC will
C                 be 0.
C
C     NFIELD      is the number of fields of the type 1 SCLK seen on
C                 the previous, successful call to any SC01 entry
C                 point. NFIELD is valid only if PRVSC is non-zero.
C
C     DELCDE      is the delimiter code of the type 1 SCLK seen on the
C                 previous, successful call to any SC01 entry point.
C                 DELCDE is valid only if PRVSC is non-zero.
C
C     TIMSYS      is the parallel time system of the type 1 SCLK seen
C                 on the previous, successful call to any SC01 entry
C                 point. TIMSYS is valid only if PRVSC is non-zero.
C
C     NCOEFF      is the number of individual coefficients of the type
C                 1 SCLK seen on the previous, successful call to any
C                 SC01 entry point. NCOEFF is valid only if PRVSC is
C                 non-zero.
C
C     NPART       is the number partitions of the type 1 SCLK seen on
C                 the previous, successful call to any SC01 entry
C                 point. NPART is valid only if PRVSC is non-zero.
C
C     COFBAS      is the base index in the double precision buffer of
C                 the set of SCLK coefficients of the type 1 SCLK seen
C                 on the previous, successful call to any SC01 entry
C                 point. COFBAS is valid only if PRVSC is non-zero.
C
C                 The first coefficient associated with PRVSC is
C                 located at index COFBAS+1.
C
C     STRBAS      is the base index in the double precision buffer of
C                 the set of partition start times of the type 1 SCLK
C                 seen on the previous, successful call to any SC01
C                 entry point. STRBAS is valid only if PRVSC is
C                 non-zero.
C
C                 The first partition start time associated with PRVSC
C                 is located at index STRBAS+1.
C
C     ENDBAS      is the base index in the double precision buffer of
C                 the set of partition end times of the type 1 SCLK
C                 seen on the previous, successful call to any SC01
C                 entry point. ENDBAS is valid only if PRVSC is
C                 non-zero.
C
C                 The first partition end time associated with PRVSC is
C                 located at index ENDBAS+1.
C
C     MODBAS      is the base index in the double precision buffer of
C                 the set of SCLK moduli of the type 1 SCLK seen on the
C                 previous, successful call to any SC01 entry point.
C                 MODBAS is valid only if PRVSC is non-zero.
C
C                 The first SCLK modulus associated with PRVSC is
C                 located at index MODBAS+1.
C
C     OFFBAS      is the base index in the double precision buffer of
C                 the set of SCLK offsets of the type 1 SCLK seen on
C                 the previous, successful call to any SC01 entry
C                 point. OFFBAS is valid only if PRVSC is non-zero.
C
C                 The first SCLK offset associated with PRVSC is
C                 located at index OFFBAS+1.
C
C$ Detailed_Output
C
C     POLCTR      is the input kernel pool tracking counter, updated if
C                 necessary to reflect a kernel pool update.
C
C     HDSCLK      is the input list head node array, updated if
C                 necessary to reflect addition to the type 1 database
C                 of data for a new clock.
C
C     SCPOOL      is the input singly linked list pool, updated if
C                 necessary to reflect addition to the type 1 database
C                 of data for a new clock.
C
C     CLKLST      is the input array of SCLK IDs, updated if necessary
C                 to reflect addition to the type 1 database of data
C                 for a new clock.
C
C     DPFREE      is the input index of the first free element of the
C                 type 1 database's double precision buffer, updated if
C                 necessary to reflect addition to the type 1 database
C                 of data for a new clock.
C
C     DPBUFF      is the input double precision buffer of the type 1
C                 database, updated if necessary to reflect addition to
C                 the type 1 database of data for a new clock.
C
C     IFREE       is input index in the database's integer buffer of
C                 the first free element, updated if necessary to
C                 reflect addition to the type 1 database of data for a
C                 new clock.
C
C     INTBUF      is the input integer buffer of the type 1 database,
C                 updated if necessary to reflect addition to the type
C                 1 database of data for a new clock.
C
C     SCBASE      is the input array of base addresses in the array
C                 INTBUF, updated if necessary to reflect addition to
C                 the type 1 database of data for a new clock.
C
C     PRVSC       is set to the input SC if the call to this routine
C                 was successful. Otherwise PRVSC is set to zero.
C
C     NFIELD      is the number of fields of the type 1 SCLK designated
C                 by SC.
C
C     DELCDE      is the delimiter code of the type 1 SCLK designated
C                 by SC.
C
C     TIMSYS      is the parallel time system of the type 1 SCLK
C                 designated by SC.
C
C     NCOEFF      is the number of individual coefficients of the type
C                 1 SCLK designated by SC.
C
C     NPART       is the number partitions of the type 1 SCLK
C                 designated by SC.
C
C     COFBAS      is the base index in the double precision buffer of
C                 the set of SCLK coefficients of the type 1 SCLK
C                 designated by SC.
C
C     STRBAS      is the base index in the double precision buffer of
C                 the set of partition start times of the type 1 SCLK
C                 designated by SC.
C
C     ENDBAS      is the base index in the double precision buffer of
C                 the set of partition end times of the type 1 SCLK
C                 designated by SC.
C
C     MODBAS      is the base index in the double precision buffer of
C                 the set of SCLK moduli of the type 1 SCLK designated
C                 by SC.
C
C     OFFBAS      is the base index in the double precision buffer of
C                 the set of SCLK offsets of the type 1 SCLK
C                 designated by SC.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the total number of double precision values associated
C         with the input clock is greater than the double precision
C         buffer size, the error will be signaled by a routine in the
C         call tree of this routine.
C
C     2)  If the database contains entries for the maximum number
C         of clocks and an addition is requested, the database will
C         be re-initialized, and an entry for the new clock will be
C         created. This case is not an error.
C
C     3)  If an error occurs while this routine looks up data from the
C         kernel pool, the error will signaled by a routine in the call
C         tree of this routine.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - An SCLK kernel providing data for the SCLK designated by
C          the input ID code SC.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine determines whether an update to the SCLK type 01
C     database is needed and updates the database and associated
C     SCLK parameter output arguments if so. Any updates to the kernel
C     pool trigger SCLK database updates. If the database doesn't
C     need to be updated, but the input SCLK ID doesn't match the
C     SCLK ID from the previous, successful call to any SC01 entry
C     point, as indicated by the condition
C
C         SC .EQ. PRVSC
C
C     then the output SCLK parameters are updated. If the input SCLK ID
C     matches that of the previous, successful call to an entry point
C     of SC01, and if the kernel pool has not been modified since that
C     call, the database is left unchanged, and the output SCLK
C     parameters are left untouched.
C
C     Note that a successful call to SCTY01 for a non-type 1 SCLK will
C     leave PRVSC set to 0, so the condition above won't be met.
C
C     If the input SCLK ID doesn't match the previous one and the
C     input ID is not present in the database, data for the input SCLK
C     will be looked up from the kernel pool, if possible. If the
C     required kernel pool data are found, the database is updated to
C     reflect addition of the new clock, and the integer SCLK
C     parameter output arguments are updated. If the data are not
C     found, an error will be signaled.
C
C     The type 1 SCLK database uses an add-or-clear policy for data
C     additions. If there is room in the SCLK ID hash and the data
C     buffers, data for a new clock are added to the database. If there
C     is not enough room, the database is re-initialized, after which
C     data for the new clock are added. If a new clock has too much
C     data to be buffered, an error is signaled.
C
C     Note that the database entry for a given SCLK is never updated
C     with new data for that clock: if the data change, that means the
C     kernel pool state has changed, so the type 01 SCLK database is
C     re-initialized before data for the SCLK are fetched.
C
C$ Examples
C
C     None. See usage in SC01.
C
C$ Restrictions
C
C     This is a SPICE-private routine. It should not be called directly
C     by user application code.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 01-DEC-2021 (NJB)
C
C-&

C$ Index_Entries
C
C     update current sclk data and type 1 sclk database
C
C-&


C$ Revisions
C
C     None.
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
      INTEGER               IBASE
      INTEGER               SCLKAT

      LOGICAL               SAMCLK
      LOGICAL               UPDATE


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSCUP01' )
C
C     Reinitialize local database if the kernel pool has been
C     updated. The call below syncs POLCTR.
C
      CALL ZZPCTRCK ( POLCTR, UPDATE )

      IF ( UPDATE ) THEN
C
C        Initialize local data structures.
C
         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST,
     .                   DPFREE, IFREE,  PRVSC )

         SAMCLK = .FALSE.
      ELSE
         SAMCLK = ( SC .NE. 0 ) .AND. ( SC .EQ. PRVSC )
      END IF

C
C     Set the output arguments only if the clock has changed or if
C     a kernel pool update has occurred.
C
      IF ( .NOT. SAMCLK )  THEN
C
C        Look up the input clock. If the clock's information is not
C        buffered, try to look up the information from the kernel pool.
C
         CALL ZZHSICHK ( HDSCLK, SCPOOL, CLKLST, SC, SCLKAT )

         IF ( SCLKAT .EQ. 0 ) THEN
C
C           This clock is unknown to the SC01 subsystem. Try to look up
C           the clock's specification from the kernel pool.
C
            CALL ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                      DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )

            IF ( FAILED() ) THEN
C
C              ZZSCAD01 will have initialized the type 01 SCLK
C              database; there's no need to do it again. Initialize
C              the SCLK integer parameters.
C
               NFIELD = 0
               DELCDE = 0
               TIMSYS = 0
               NCOEFF = 0
               NPART  = 0

C
C              Indicate valid data for the previous were not obtained.
C
               PRVSC = 0

               CALL CHKOUT ( 'ZZSCUP01' )
               RETURN

            END IF

         END IF

C
C        IBASE is the base address in the integer buffer of the integer
C        data for this clock.
C
         IBASE = SCBASE( SCLKAT )
         
C
C        Set integer SCLK parameters to the correct values for
C        the clock designated by SC.
C
         NFIELD = INTBUF( IBASE+IXNFLD )
         DELCDE = INTBUF( IBASE+IXDLIM )
         TIMSYS = INTBUF( IBASE+IXTSYS )
         NCOEFF = INTBUF( IBASE+IXNCOF )
         NPART  = INTBUF( IBASE+IXNPRT )
         COFBAS = INTBUF( IBASE+IXBCOF )
         STRBAS = INTBUF( IBASE+IXBSTR )
         ENDBAS = INTBUF( IBASE+IXBEND )
         MODBAS = INTBUF( IBASE+IXBMOD )
         OFFBAS = INTBUF( IBASE+IXBOFF )

      END IF

      CALL CHKOUT ( 'ZZSCUP01' )
      RETURN
      END
