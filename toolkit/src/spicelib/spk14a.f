C$Procedure SPK14A ( SPK type 14: Add data to a segment )

      SUBROUTINE SPK14A ( HANDLE, NCSETS, COEFFS, EPOCHS )

C$ Abstract
C
C     Add data to a type 14 SPK segment associated with HANDLE.
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
C     SPK
C
C$ Keywords
C
C     SPK
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               NCSETS
      DOUBLE PRECISION      COEFFS ( * )
      DOUBLE PRECISION      EPOCHS ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of an SPK file open for writing.
C     NCSETS     I   The number of coefficient sets and epochs.
C     COEFFS     I   The collection of coefficient sets.
C     EPOCHS     I   The epochs associated with the coefficient sets.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of an SPK file that has been
C              opened for writing.
C
C     NCSETS   is the number of Chebyshev coefficient sets and epochs
C              to be stored in the segment.
C
C     COEFFS   is a time-ordered array of NCSETS of Chebyshev polynomial
C              coefficients to be placed in the segment of the SPK file.
C              Each set has size SETSZ = 2 + 6*(CHBDEG+1), where CHBDEG
C              is the degree of the Chebyshev polynomials used to
C              represent the ephemeris information.
C
C              These sets are used to compute the state vector, which
C              consists of position components X, Y, Z and velocity
C              components dX/dt, dY/dt, dZ/dt, of a body relative to
C              a center of motion.
C
C              See the $Particulars section for details on how to store
C              the coefficient sets in the array.
C
C     EPOCHS   contains the initial epochs (ephemeris seconds past
C              J2000) corresponding to the Chebyshev coefficients in
C              COEFFS. The I'th epoch is associated with the I'th
C              Chebyshev coefficient set. The epochs must form a
C              strictly increasing sequence.
C
C$ Detailed_Output
C
C     None. The ephemeris data is stored in a segment in the SPK file
C     associated with HANDLE.
C
C     See the $Particulars section for details about the structure of a
C     type 14 SPK segment.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the number of coefficient sets and epochs is not positive,
C         the error SPICE(INVALIDARGUMENT) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine adds data to a type 14 SPK segment that is associated
C     with HANDLE. The segment must have been started by a call to the
C     routine SPK14B, the routine which begins a type 14 SPK segment.
C
C     This routine is one of a set of three routines for creating and
C     adding data to type 14 SPK segments. These routines are:
C
C        SPK14B: Begin a type 14 SPK segment. This routine must be
C                called before any data may be added to a type 14
C                segment.
C
C        SPK14A: Add data to a type 14 SPK segment. This routine may be
C                called any number of times after a call to SPK14B to
C                add type 14 records to the SPK segment that was
C                started.
C
C        SPK14E: End a type 14 SPK segment. This routine is called to
C                make the type 14 segment a permanent addition to the
C                SPK file. Once this routine is called, no further type
C                14 records may be added to the segment. A new segment
C                must be started.
C
C     A type 14 SPK segment consists of coefficient sets for fixed order
C     Chebyshev polynomials over consecutive time intervals, where the
C     time intervals need not all be of the same length. The Chebyshev
C     polynomials represent the position, X, Y, and Z coordinates, and
C     the velocities, dX/dt, dY/dt, and dZ/dt, of BODY relative to
C     CENTER.
C
C     The ephemeris data supplied to the type 14 SPK writer is packed
C     into an array as a sequence of logical records,
C
C        -----------------------------------------------------
C        | Record 1 | Record 2 | ... | Record N-1 | Record N |
C        -----------------------------------------------------
C
C     with each record has the following format.
C
C           ------------------------------------------------
C           |  The midpoint of the approximation interval  |
C           ------------------------------------------------
C           |  The radius of the approximation interval    |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the X coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Y coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Z coordinate  |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the X velocity    |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Y velocity    |
C           ------------------------------------------------
C           |  CHBDEG+1 coefficients for the Z velocity    |
C           ------------------------------------------------
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) This example demonstrates how to create an SPK type 14 kernel
C        containing only one segment, given a set of Chebyshev
C        coefficients and their associated epochs, if all of the data
C        for the segment is available at one time.
C
C
C        Example code begins here.
C
C
C              PROGRAM SPK14A_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 42 )
C
C        C
C        C     Define the segment identifier parameters.
C        C
C              CHARACTER*(*)         SPK14
C              PARAMETER           ( SPK14  = 'spk14a_ex1.bsp' )
C
C              CHARACTER*(*)         REF
C              PARAMETER           ( REF    = 'J2000'          )
C
C              INTEGER               BODY
C              PARAMETER           ( BODY   = 3  )
C
C              INTEGER               CENTER
C              PARAMETER           ( CENTER = 10 )
C
C              INTEGER               CHBDEG
C              PARAMETER           ( CHBDEG = 2  )
C
C              INTEGER               NRECS
C              PARAMETER           ( NRECS  = 4  )
C
C              INTEGER               RECSIZ
C              PARAMETER           ( RECSIZ = 2 + 6*(CHBDEG+1) )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(NAMLEN)    SEGID
C
C              DOUBLE PRECISION      EPOCHS ( NRECS + 1 )
C              DOUBLE PRECISION      FIRST
C              DOUBLE PRECISION      LAST
C              DOUBLE PRECISION      RECRDS ( RECSIZ, NRECS )
C
C              INTEGER               HANDLE
C              INTEGER               NCOMCH
C
C        C
C        C     Define the epochs and coefficients.
C        C
C              DATA                  EPOCHS /
C             .                100.D0, 200.D0, 300.D0, 400.D0, 500.D0 /
C
C              DATA                  RECRDS /
C             .     150.0D0, 50.0D0, 1.0101D0, 1.0102D0, 1.0103D0,
C             .                      1.0201D0, 1.0202D0, 1.0203D0,
C             .                      1.0301D0, 1.0302D0, 1.0303D0,
C             .                      1.0401D0, 1.0402D0, 1.0403D0,
C             .                      1.0501D0, 1.0502D0, 1.0503D0,
C             .                      1.0601D0, 1.0602D0, 1.0603D0,
C             .     250.0D0, 50.0D0, 2.0101D0, 2.0102D0, 2.0103D0,
C             .                      2.0201D0, 2.0202D0, 2.0203D0,
C             .                      2.0301D0, 2.0302D0, 2.0303D0,
C             .                      2.0401D0, 2.0402D0, 2.0403D0,
C             .                      2.0501D0, 2.0502D0, 2.0503D0,
C             .                      2.0601D0, 2.0602D0, 2.0603D0,
C             .     350.0D0, 50.0D0, 3.0101D0, 3.0102D0, 3.0103D0,
C             .                      3.0201D0, 3.0202D0, 3.0203D0,
C             .                      3.0301D0, 3.0302D0, 3.0303D0,
C             .                      3.0401D0, 3.0402D0, 3.0403D0,
C             .                      3.0501D0, 3.0502D0, 3.0503D0,
C             .                      3.0601D0, 3.0602D0, 3.0603D0,
C             .     450.0D0, 50.0D0, 4.0101D0, 4.0102D0, 4.0103D0,
C             .                      4.0201D0, 4.0202D0, 4.0203D0,
C             .                      4.0301D0, 4.0302D0, 4.0303D0,
C             .                      4.0401D0, 4.0402D0, 4.0403D0,
C             .                      4.0501D0, 4.0502D0, 4.0503D0,
C             .                      4.0601D0, 4.0602D0, 4.0603D0 /
C
C
C        C
C        C     Set the start and end times of interval covered by
C        C     segment.
C        C
C              FIRST = EPOCHS(1)
C              LAST  = EPOCHS(NRECS + 1)
C
C        C
C        C     NCOMCH is the number of characters to reserve for the
C        C     kernel's comment area. This example doesn't write
C        C     comments, so set to zero.
C        C
C              NCOMCH = 0
C
C        C
C        C     Internal file name and segment ID.
C        C
C              IFNAME = 'Type 14 SPK internal file name.'
C              SEGID  = 'SPK type 14 test segment'
C
C        C
C        C     Open a new SPK file.
C        C
C              CALL SPKOPN( SPK14, IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Begin the segment.
C        C
C              CALL SPK14B ( HANDLE, SEGID, BODY, CENTER, REF,
C             .              FIRST,  LAST,  CHBDEG            )
C
C        C
C        C     Add the data to the segment all at once.
C        C
C              CALL SPK14A ( HANDLE, NRECS, RECRDS, EPOCHS )
C
C        C
C        C     End the segment, making the segment a permanent addition
C        C     to the SPK file.
C        C
C              CALL SPK14E ( HANDLE )
C
C        C
C        C     Close the SPK file.
C        C
C              CALL SPKCLS ( HANDLE )
C
C              END
C
C
C        When this program is executed, no output is presented on
C        screen. After run completion, a new SPK type 14 exists in
C        the output directory.
C
C     2) This example demonstrates how to add type 14 SPK records to the
C        segment being written, one at a time. The ability to write the
C        records in this way is useful if computer memory is limited. It
C        may also be convenient from a programming perspective to write
C        the records this way.
C
C
C        Example code begins here.
C
C
C              PROGRAM SPK14A_EX2
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               NAMLEN
C              PARAMETER           ( NAMLEN = 42 )
C
C        C
C        C     Define the segment identifier parameters.
C        C
C              CHARACTER*(*)         SPK14
C              PARAMETER           ( SPK14  = 'spk14a_ex2.bsp' )
C
C              CHARACTER*(*)         REF
C              PARAMETER           ( REF    = 'J2000'          )
C
C              INTEGER               BODY
C              PARAMETER           ( BODY   = 3  )
C
C              INTEGER               CENTER
C              PARAMETER           ( CENTER = 10 )
C
C              INTEGER               CHBDEG
C              PARAMETER           ( CHBDEG = 2  )
C
C              INTEGER               NRECS
C              PARAMETER           ( NRECS  = 4  )
C
C              INTEGER               RECSIZ
C              PARAMETER           ( RECSIZ = 2 + 6*(CHBDEG+1) )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(NAMLEN)    IFNAME
C              CHARACTER*(NAMLEN)    SEGID
C
C              DOUBLE PRECISION      EPOCHS ( NRECS + 1 )
C              DOUBLE PRECISION      FIRST
C              DOUBLE PRECISION      LAST
C              DOUBLE PRECISION      RECRDS ( RECSIZ, NRECS )
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               NCOMCH
C
C        C
C        C     Define the epochs and coefficients.
C        C
C              DATA                  EPOCHS /
C             .                100.D0, 200.D0, 300.D0, 400.D0, 500.D0 /
C
C              DATA                  RECRDS /
C             .     150.0D0, 50.0D0, 1.0101D0, 1.0102D0, 1.0103D0,
C             .                      1.0201D0, 1.0202D0, 1.0203D0,
C             .                      1.0301D0, 1.0302D0, 1.0303D0,
C             .                      1.0401D0, 1.0402D0, 1.0403D0,
C             .                      1.0501D0, 1.0502D0, 1.0503D0,
C             .                      1.0601D0, 1.0602D0, 1.0603D0,
C             .     250.0D0, 50.0D0, 2.0101D0, 2.0102D0, 2.0103D0,
C             .                      2.0201D0, 2.0202D0, 2.0203D0,
C             .                      2.0301D0, 2.0302D0, 2.0303D0,
C             .                      2.0401D0, 2.0402D0, 2.0403D0,
C             .                      2.0501D0, 2.0502D0, 2.0503D0,
C             .                      2.0601D0, 2.0602D0, 2.0603D0,
C             .     350.0D0, 50.0D0, 3.0101D0, 3.0102D0, 3.0103D0,
C             .                      3.0201D0, 3.0202D0, 3.0203D0,
C             .                      3.0301D0, 3.0302D0, 3.0303D0,
C             .                      3.0401D0, 3.0402D0, 3.0403D0,
C             .                      3.0501D0, 3.0502D0, 3.0503D0,
C             .                      3.0601D0, 3.0602D0, 3.0603D0,
C             .     450.0D0, 50.0D0, 4.0101D0, 4.0102D0, 4.0103D0,
C             .                      4.0201D0, 4.0202D0, 4.0203D0,
C             .                      4.0301D0, 4.0302D0, 4.0303D0,
C             .                      4.0401D0, 4.0402D0, 4.0403D0,
C             .                      4.0501D0, 4.0502D0, 4.0503D0,
C             .                      4.0601D0, 4.0602D0, 4.0603D0 /
C
C
C        C
C        C     Set the start and end times of interval covered by
C        C     segment.
C        C
C              FIRST = EPOCHS(1)
C              LAST  = EPOCHS(NRECS + 1)
C
C        C
C        C     NCOMCH is the number of characters to reserve for the
C        C     kernel's comment area. This example doesn't write
C        C     comments, so set to zero.
C        C
C              NCOMCH = 0
C
C        C
C        C     Internal file name and segment ID.
C        C
C              IFNAME = 'Type 14 SPK internal file name.'
C              SEGID  = 'SPK type 14 test segment'
C
C        C
C        C     Open a new SPK file.
C        C
C              CALL SPKOPN( SPK14, IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Begin the segment.
C        C
C              CALL SPK14B ( HANDLE, SEGID, BODY, CENTER, REF,
C             .              FIRST,  LAST,  CHBDEG            )
C
C        C
C        C     Write the records to the segment in the
C        C     SPK file one at at time.
C        C
C              DO I = 1, NRECS
C
C                 CALL SPK14A ( HANDLE, 1, RECRDS(1,I), EPOCHS(I) )
C
C              END DO
C
C        C
C        C     End the segment, making the segment a permanent addition
C        C     to the SPK file.
C        C
C              CALL SPK14E ( HANDLE )
C
C        C
C        C     Close the SPK file.
C        C
C              CALL SPKCLS ( HANDLE )
C
C              END
C
C
C        When this program is executed, no output is presented on
C        screen. After run completion, a new SPK type 14 exists in
C        the output directory.
C
C$ Restrictions
C
C     1)  The type 14 SPK segment to which we are adding data must have
C         been started by the routine SPK14B, the routine which begins a
C         type 14 SPK segment.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 27-AUG-2021 (JDR) (NJB)
C
C        Edited the header to comply with NAIF standard. Added
C        complete examples code from existing fragments.
C
C        Extended COEFFS argument description to provide the size of
C        the Chebyshev polynomials sets.
C
C        Re-ordered header sections. Removed references to other
C        routines from $Abstract section (present in $Particulars).
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Removed DAFHLU call; replaced ERRFN call with ERRHAN.
C
C-    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG)
C
C-&


C$ Index_Entries
C
C     add data to a type_14 SPK segment
C
C-&


C
C     Spicelib functions
C
      LOGICAL               RETURN

C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPK14A' )
      END IF

C
C     First, check to see if the number of coefficient sets and epochs
C     is positive.
C
      IF ( NCSETS .LE. 0 ) THEN

         CALL SETMSG ( 'The number of coefficient sets and epochs'
     .   //            ' to be added to the SPK segment in the file'
     .   //            ' ''#'' was not positive. Its value was: #.'  )
         CALL ERRHAN ( '#', HANDLE                                   )
         CALL ERRINT ( '#', NCSETS                                   )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                      )
         CALL CHKOUT ( 'SPK14A'                                      )
         RETURN

      END IF

C
C     Add the data.
C
      CALL SGWFPK ( HANDLE, NCSETS, COEFFS, NCSETS, EPOCHS )

C
C     No need to check FAILED() here, since all we do is check out.
C     Leave it up to the caller.
C

      CALL CHKOUT ( 'SPK14A' )
      RETURN

      END
