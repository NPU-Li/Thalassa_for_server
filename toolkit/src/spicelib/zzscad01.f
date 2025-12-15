C$Procedure ZZSCAD01 ( SPICE private, add clock to database, type 01 )

      SUBROUTINE ZZSCAD01 ( SC,     HDSCLK, SCPOOL, CLKLST, DPFREE,
     .                      DPBUFF, IFREE,  INTBUF, SCBASE, SCLKAT )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Add to the SCLK type 01 database data for a specified SCLK.
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
C
C$ Keywords
C
C     SCLK
C     TIME
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'errhnd.inc'
      INCLUDE 'zzsc01.inc'

      INTEGER               SC
      INTEGER               HDSCLK ( * )
      INTEGER               SCPOOL ( LBSNGL : * )
      INTEGER               CLKLST ( * )
      INTEGER               DPFREE
      DOUBLE PRECISION      DPBUFF ( * )
      INTEGER               IFREE
      INTEGER               INTBUF ( * )
      INTEGER               SCBASE ( * )
      INTEGER               SCLKAT

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   SCLK ID code.
C     HDSCLK    I-O  Array of heads of hash collision lists.
C     SCPOOL    I-O  Singly linked collision list pool.
C     CLKLST    I-O  Array of SCLK codes.
C     DPFREE    I-O  Index of first free entry in d.p. data buffer.
C     DPBUFF    I-O  D.p. (double precision) data buffer
C     IFREE     I-O  Index of first free entry in integer data buffer.
C     INTBUF    I-O  Integer data buffer.
C     SCBASE    I-O  Base index in integer buffer corresponding to SC.
C     SCLKAT     O   Index in CLKLST of SC.
C
C$ Detailed_Input
C
C     SC          is the SCLK ID of a type 1 clock. Data for this clock
C                 are to be added to the type 1 database.
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
C$ Detailed_Output
C
C     HDSCLK      is the input list head array, updated as needed to
C                 reflect the addition of the new clock. Note that the
C                 array won't be updated if the clock is added to a
C                 pre-existing collision list.
C
C     SCPOOL      is the input singly linked list pool, updated to
C                 reflect the addition of the new clock.
C
C     CLKLST      is an array of SCLK IDs, updated to reflect the
C                 addition of the new clock.
C
C     DPFREE      is index in the database's double precision buffer of
C                 the first free element after addition of data for the
C                 new clock.
C
C     DPBUFF      is the double precision buffer of the type 1
C                 database, updated to reflect the addition of the new
C                 clock.
C
C     IFREE       is index in the database's integer buffer of the
C                 first free element after addition of data for the new
C                 clock.
C
C     INTBUF      is the integer buffer of the type 1 database, updated
C                 to reflect the addition of the new clock.
C
C     SCBASE      is an array of base addresses in the array INTBUF,
C                 updated to reflect the addition of the new clock.
C
C     SCLKAT      is the index in CLKLST at which the input clock ID
C                 SC is stored.
C
C$ Parameters
C
C     See the include files zzsc01.inc and zzctr.inc.
C
C$ Exceptions
C
C
C     1)  If any of the kernel variables containing SCLK coefficients,
C         partition start or stop times, moduli, or offsets are not
C         found, the error SPICE(KERNELVARNOTFOUND) will be signaled.
C
C     2)  If any integer kernel variables are not found, the error will
C         be signaled by a routine in the call tree of this routine.
C
C     3)  If any kernel variable used by this routine does not have
C         numeric type, the error will be signaled by a routine in the
C         call tree of this routine.
C
C     4)  If the counts of partition start or stop times do not match,
C         the error SPICE(NUMPARTSUNEQUAL) will be signaled.
C
C     5)  If the count of partitions exceeds the limit MXPART,
C         the error SPICE(TOOMANYPARTITIONS) will be signaled.
C
C     6)  If the count of coefficients exceeds the limit 3*MXCOEF,
C         the error SPICE(TOOMANYCOEFFS) will be signaled.
C
C     7)  If any kernel variable other than the coefficient set or
C         the partition bounds has size that exceeds the applicable
C         limit, the error SPICE(KERNELVARTOOLARGE) will be signaled.
C
C     8)  If an error occurs while this routine looks up data from the
C         kernel pool, the error will be signaled by a routine in the
C         call tree of this routine.
C
C     9)  If an error occurs while this routine adds data to a hash,
C         the error will be signaled by a routine in the call tree of
C         this routine.
C
C     10) If the database contains entries for the maximum number
C         of clocks and an addition is requested, the database will
C         be re-initialized, and an entry for the new clock will be
C         created. This case is not an error.
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
C     None.
C
C$ Examples
C
C     None. See usage in ZZSCUP01.
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
C     add sclk to type 1 sclk database
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
C     Local parameters
C
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN =     60 )

C
C     Following are parameters for the indices within the
C     array NAMLST of the kernel variable names used by the
C     SC01 entry points.
C
C     NOTE: indices for the d.p. variables must be listed first.
C
      INTEGER               COFIDX
      PARAMETER           ( COFIDX =          1 )

      INTEGER               PRSIDX
      PARAMETER           ( PRSIDX = COFIDX + 1 )

      INTEGER               PREIDX
      PARAMETER           ( PREIDX = PRSIDX + 1 )

      INTEGER               OFFIDX
      PARAMETER           ( OFFIDX = PREIDX + 1 )

      INTEGER               MODIDX
      PARAMETER           ( MODIDX = OFFIDX + 1 )

      INTEGER               NFLIDX
      PARAMETER           ( NFLIDX = MODIDX + 1 )

      INTEGER               DELIDX
      PARAMETER           ( DELIDX = NFLIDX + 1 )

      INTEGER               SYSIDX
      PARAMETER           ( SYSIDX = DELIDX + 1 )

      INTEGER               NKITEM
      PARAMETER           ( NKITEM = SYSIDX )

      INTEGER               NDP
      PARAMETER           ( NDP    = MODIDX )

C
C     Maximum number of elements in coefficient array:
C
      INTEGER               MXCFSZ
      PARAMETER           ( MXCFSZ =  3 * MXCOEF )

C
C     Local variables
C
      CHARACTER*(1)         KVTYPE ( NKITEM )
      CHARACTER*(NAMLEN)    KVNAME ( NKITEM )
      CHARACTER*(NAMLEN)    NAMLST ( NKITEM )
      CHARACTER*(SMSGLN)    SHRTMS

      INTEGER               DPBASE
      INTEGER               DPROOM
      INTEGER               I
      INTEGER               IBASE
      INTEGER               IBIX   ( NKITEM )
      INTEGER               IROOM
      INTEGER               KVMAXN ( NKITEM )
      INTEGER               KVSIZE ( NKITEM )
      INTEGER               N
      INTEGER               NCOEFF
      INTEGER               NDPVAL
      INTEGER               NMOD
      INTEGER               NOFF
      INTEGER               NPART
      INTEGER               NPARTB
      INTEGER               NPARTE
      INTEGER               NSCAVL
      INTEGER               NSYS
      INTEGER               PRVSC

      LOGICAL               FOUND
      LOGICAL               NEW

C
C     Saved values
C
      SAVE                 IBIX
      SAVE                 KVMAXN
      SAVE                 NAMLST

C
C     Initial values
C
      DATA ( NAMLST(I), KVMAXN(I), IBIX(I), I = 1, NKITEM ) /
     .  'SCLK01_COEFFICIENTS',    MXCFSZ,    IXBCOF,
     .  'SCLK_PARTITION_START',   MXPART,    IXBSTR,
     .  'SCLK_PARTITION_END',     MXPART,    IXBEND,
     .  'SCLK01_OFFSETS',         MXNFLD,    IXBOFF,
     .  'SCLK01_MODULI',          MXNFLD,    IXBMOD,
     .  'SCLK01_N_FIELDS',        1,         0,
     .  'SCLK01_OUTPUT_DELIM',    1,         0,
     .  'SCLK01_TIME_SYSTEM',     1,         0              /


      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSCAD01' )

C
C     Make up a list of names of kernel variables that we'll use.
C     The first name in the list is SCLK_KERNEL_ID, which does not
C     require the addition of a spacecraft code suffix.  For the
C     rest of the names, we'll have to add the suffix.
C
      CALL MOVEC ( NAMLST, NKITEM, KVNAME )

      DO I = 1, NKITEM
         CALL SUFFIX ( '_#',        0,         KVNAME(I) )
         CALL REPMI  ( KVNAME(I),  '#',  -SC,  KVNAME(I) )
      END DO

C
C     Check available room in the clock hash and in the numeric data
C     buffers. If there's no room, re-initialize the entire data
C     structure.
C
      CALL ZZHSIAVL ( SCPOOL, NSCAVL )

      IF ( NSCAVL .EQ. 0 ) THEN

         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

      END IF

C
C     Add this clock to the hash.
C
      CALL ZZHSIADD ( HDSCLK, SCPOOL, CLKLST, SC, SCLKAT, NEW )

      IF ( FAILED() ) THEN
C
C        This code should be unreachable but is provided for safety.
C
         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )
         CALL CHKOUT ( 'ZZSCAD01' )
         RETURN

      END IF

C
C     Check room in the integer buffer.
C
      IROOM = IBUFSZ + 1 - IFREE

      IF (  ( IROOM .LT. NIVALS ) .OR. ( IROOM .GT. IBUFSZ )  ) THEN
C
C        This code should be unreachable but is provided for safety.
C
         I = IFREE

         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

         CALL SETMSG ( 'IROOM was #; must be in range #:#. '
     .   //            'IFREE was #; must be in range 1:#.' )
         CALL ERRINT ( '#', IROOM    )
         CALL ERRINT ( '#', NIVALS   )
         CALL ERRINT ( '#', IBUFSZ   )
         CALL ERRINT ( '#', I        )
         CALL ERRINT ( '#', IBUFSZ+1 )
         CALL SIGERR ( 'SPICE(BUG)'  )
         CALL CHKOUT ( 'ZZSCAD01'    )
         RETURN

      END IF

C
C     Compute available room for double precision data.
C
      DPROOM = DBUFSZ + 1 - DPFREE

      IF (  ( DPROOM .LT. 0 ) .OR. ( DPROOM .GT. DBUFSZ )  ) THEN
C
C        This code should be unreachable but is provided for safety.
C
         I = DPFREE

         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

         CALL SETMSG ( 'DPROOM was #; must be in range 0:#. '
     .   //            'DPFREE was #; must be in range 1:#.' )
         CALL ERRINT ( '#', DPROOM   )
         CALL ERRINT ( '#', DBUFSZ   )
         CALL ERRINT ( '#', I        )
         CALL ERRINT ( '#', DBUFSZ+1 )
         CALL SIGERR ( 'SPICE(BUG)'  )
         CALL CHKOUT ( 'ZZSCAD01'    )
         RETURN

      END IF

C
C     Set the base address for the next item to be added to the
C     integer buffer. The index of the first element of the item
C     is the successor of IBASE.
C
      IBASE            = IFREE-1
      SCBASE( SCLKAT ) = IBASE

C
C     Fetch from the kernel pool the integer items we need:
C
C        -  The number of fields in an (unabridged) SCLK string
C        -  The output delimiter code
C        -  The parallel time system code (optional; defaults to TDB)
C
      CALL SCLI01 ( NAMLST(NFLIDX), SC, 1, N,    INTBUF(IBASE+IXNFLD) )
      CALL SCLI01 ( NAMLST(DELIDX), SC, 1, N,    INTBUF(IBASE+IXDLIM) )
      CALL SCLI01 ( NAMLST(SYSIDX), SC, 1, NSYS, INTBUF(IBASE+IXTSYS) )

      IF ( FAILED() ) THEN

         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )
         CALL CHKOUT ( 'ZZSCAD01' )
         RETURN

      END IF

C
C     The time system code need not be provided in the kernel pool.
C     Set it to the default value if it's not present.
C
      IF ( NSYS .EQ. 0 ) THEN
         INTBUF(IBASE+IXTSYS) = TDB
      END IF

C
C     Check for presence in the kernel pool of all required d.p. kernel
C     variables.
C
C     Get and check the sizes of the d.p. arrays: coefficients,
C     partitions, moduli, and offsets.
C
C     We obtain sizes of d.p. kernel variables rather than using sizes
C     returned by SCLD01 because we'll read data directly into our
C     passed-in buffers. We need to know in advance how much room is
C     needed.
C
      DO I = 1, NDP

         CALL DTPOOL ( KVNAME(I), FOUND, KVSIZE(I), KVTYPE(I) )

         IF ( FAILED() ) THEN
C
C           This code should be unreachable but is provided for
C           safety. Initialize local data structures. ZZSCIN01 sets
C           PRVSC to 0.
C
            CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST,
     .                      DPFREE, IFREE,  PRVSC  )

            CALL CHKOUT ( 'ZZSCAD01' )
            RETURN

         END IF

C
C        Check that each required d.p. kernel variable was found.
C
         IF ( .NOT. FOUND ) THEN

            CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST,
     .                      DPFREE, IFREE,  PRVSC  )

            CALL SETMSG ( 'Kernel variable # for spacecraft clock '
     .      //            '# was not found. An SCLK kernel for '
     .      //            'this clock may not have been loaded.'   )
            CALL ERRCH  ( '#', KVNAME(I)                           )
            CALL ERRINT ( '#', SC                                  )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)'               )
            CALL CHKOUT ( 'ZZSCAD01'                               )
            RETURN

         END IF

C
C        Check the number of values associated with the kernel
C        variable.
C
         IF ( KVSIZE(I) .GT. KVMAXN(I) ) THEN

            CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST,
     .                      DPFREE, IFREE,  PRVSC  )

            CALL SETMSG ( 'The number of values associated with '
     .      //            'the kernel variable # for clock # '
     .      //            'is #, which exceeds the limit #.'     )
            CALL ERRCH  ( '#', KVNAME(I)                         )
            CALL ERRINT ( '#', SC                                )
            CALL ERRINT ( '#', KVSIZE(I)                         )
            CALL ERRINT ( '#', KVMAXN(I)                         )
C
C           Customize the short error message a bit.
C
            IF ( I .EQ. COFIDX ) THEN

               SHRTMS = 'SPICE(TOOMANYCOEFFS)'

            ELSE IF (     ( I .EQ. PRSIDX )
     .               .OR. ( I .EQ. PREIDX ) ) THEN

               SHRTMS = 'SPICE(TOOMANYPARTITIONS)'
            ELSE
               SHRTMS = 'SPICE(KERNELVARTOOLARGE)'
            END IF

            CALL SIGERR (  SHRTMS    )
            CALL CHKOUT ( 'ZZSCAD01' )
            RETURN

         END IF

      END DO

C
C     Store kernel variable sizes as scalars for convenience.
C
      NCOEFF = KVSIZE(COFIDX)
      NPARTB = KVSIZE(PRSIDX)
      NPARTE = KVSIZE(PREIDX)
      NMOD   = KVSIZE(MODIDX)
      NOFF   = KVSIZE(OFFIDX)

C
C     NDPVAL is the number of d.p. values that must be buffered to
C     support this clock.
C
      NDPVAL = NCOEFF + NPARTB + NPARTE + NMOD + NOFF

C
C     Check whether sizes of partition start and end kernel variables
C     match.
C
      IF ( NPARTB .NE. NPARTE  ) THEN

         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

         CALL SETMSG ( 'The numbers of partition start times # and '
     .   //            'stop times # are unequal for spacecraft '
     .   //            'clock #.'                                  )
         CALL ERRINT ( '#', NPARTB                                 )
         CALL ERRINT ( '#', NPARTE                                 )
         CALL ERRINT ( '#', SC                                     )
         CALL SIGERR ( 'SPICE(NUMPARTSUNEQUAL)'                    )
         CALL CHKOUT ( 'ZZSCAD01'                                  )
         RETURN

      ELSE
         NPART = NPARTB
      END IF

      IF ( NDPVAL .GT. DBUFSZ ) THEN
C
C        We couldn't make enough room even if we dumped all buffered
C        data.
C
C        This code should be unreachable but is provided for safety.
C        We've already checked the existence, types, and sizes of the
C        d.p. kernel variables.
C
         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

         CALL SETMSG ( 'Total number of double precision data '
     .   //            'values for SCLK # is #; this count exceeds '
     .   //            'the maximum supported count #.' )
         CALL ERRINT ( '#', SC                          )
         CALL ERRINT ( '#', NDPVAL                      )
         CALL ERRINT ( '#', DBUFSZ                      )
         CALL SIGERR ( 'SPICE(BUG)'                     )
         CALL CHKOUT ( 'ZZSCAD01'                       )
         RETURN

      ELSE IF ( NDPVAL .GT. DPROOM ) THEN
C
C        We don't have room for this clock, but we will if we dump
C        the existing buffered data.
C
         CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST, DPFREE, IFREE, PRVSC )

      END IF

C
C     Store in the integer buffer the sizes of the coefficient array
C     and the number of partitions.
C
      INTBUF( IBASE+IXNCOF ) = NCOEFF
      INTBUF( IBASE+IXNPRT ) = NPART

C
C     Fetch from the kernel pool the d.p. items we need:
C
C        -  The SCLK coefficients array
C        -  The partition start times
C        -  The partition end times
C        -  The moduli of the fields of an SCLK string
C        -  The offsets for each clock field.
C
C     Set the base index in the d.p. buffer of the next d.p. item
C     to store. The first element of the item has index DPBASE+1.
C
C     We don't update DPFREE until all d.p. data have been fetched
C     successfully.
C
      DPBASE = DPFREE - 1

      DO I = 1, NDP
C
C        Store in the integer buffer the base index in the d.p.
C        buffer of the kernel variable's values.
C
         INTBUF( IBASE + IBIX(I) ) = DPBASE
C
C        Fetch d.p. values into buffer.
C
         CALL SCLD01 ( NAMLST(I), SC,
     .                 KVMAXN(I), N,  DPBUFF(DPBASE + 1) )

         IF ( FAILED() ) THEN
C
C           This code could be reached if the kernel variable has
C           character type.
C
            CALL ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST,
     .                      DPFREE, IFREE,  PRVSC  )

            CALL CHKOUT ( 'ZZSCAD01' )
            RETURN

         END IF
C
C        Update the d.p. buffer base index.
C
         DPBASE = DPBASE + N

      END DO

C
C     Account for buffer usage for the new clock.
C
      DPFREE = DPFREE + NDPVAL
      IFREE  = IFREE  + NIVALS

      CALL CHKOUT ( 'ZZSCAD01' )
      RETURN
      END
