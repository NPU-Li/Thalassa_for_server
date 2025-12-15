C$Procedure CKFROT ( CK frame, find position rotation )

      SUBROUTINE CKFROT ( INST, ET, ROTATE, REF, FOUND )

C$ Abstract
C
C     Find the position rotation matrix from a C-kernel (CK) frame with
C     the specified frame class ID (CK ID) to the base frame of the
C     highest priority CK segment containing orientation data for this
C     CK frame at the time requested.
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
C     CK
C
C$ Keywords
C
C     POINTING
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               INST
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ROTATE  ( 3, 3 )
      INTEGER               REF
      LOGICAL               FOUND

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INST       I   Frame class ID (CK ID) of a CK frame.
C     ET         I   Epoch measured in seconds past J2000 TDB.
C     ROTATE     O   Rotation matrix from CK frame to frame REF.
C     REF        O   Frame ID of the base reference.
C     FOUND      O   .TRUE. when requested pointing is available.
C
C$ Detailed_Input
C
C     INST     is the unique frame class ID (CK ID) of the CK frame for
C              which data is being requested.
C
C     ET       is the epoch for which the position rotation is desired.
C              ET should be given in seconds past the epoch of J2000
C              TDB.
C
C$ Detailed_Output
C
C     ROTATE   is a position rotation matrix that converts positions
C              relative to the CK frame given by its frame class ID,
C              INST, to positions relative to the base frame given by
C              its frame ID, REF.
C
C              Thus, if a position S has components x,y,z in the CK
C              frame, then S has components x', y', z' in the base
C              frame.
C
C                 .-  -.     .-        -. .- -.
C                 | x' |     |          | | x |
C                 | y' |  =  |  ROTATE  | | y |
C                 | z' |     |          | | z |
C                 `-  -'     `-        -' `- -'
C
C
C     REF      is the ID code of the base reference frame to which
C              ROTATE will transform positions.
C
C     FOUND    is .TRUE. if a record was found to satisfy the pointing
C              request. FOUND will be .FALSE. otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If no CK files were loaded prior to calling this routine, an
C         error is signaled by a routine in the call tree of this
C         routine.
C
C     2)  If no SCLK correlation data needed to read CK files were
C         loaded prior to calling this routine, an error is signaled by
C         a routine in the call tree of this routine.
C
C     3)  If the input time ET cannot be converted to an encoded SCLK
C         time, using SCLK data associated with INST, an error is
C         signaled by a routine in the call tree of this routine.
C
C$ Files
C
C     CKFROT searches through loaded CK files to locate a segment that
C     can satisfy the request for position rotation data for the CK
C     frame with the specified frame class ID at time ET. You must load
C     a CK file containing such data before calling this routine. You
C     must also load SCLK and possibly LSK files needed to convert the
C     input ET time to the encoded SCLK time with which the orientation
C     data stored inside that CK is tagged.
C
C$ Particulars
C
C     CKFROT searches through loaded CK files to satisfy a pointing
C     request. Last-loaded files are searched first, and individual
C     files are searched in backwards order, giving priority to
C     segments that were added to a file later than the others.
C
C     The search ends when a segment is found that can give pointing
C     for the specified CK frame at the request time.
C
C     Segments with and without angular velocities are considered by
C     this routine.
C
C     This routine uses the CKMETA routine to determine the SCLK ID
C     used to convert the input ET time to the encoded SCLK time used
C     to look up pointing data in loaded CK files.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Use CKFROT to compute the instantaneous angular velocity
C        vector for the Mars Global Surveyor (MGS) spacecraft frame,
C        'MGS_SPACECRAFT', relative to the inertial frame used as the
C        base frame in CK files containing MGS spacecraft orientation
C        at 2003-JUL-25 13:00:00. The frame class ID (CK ID) for the
C        'MGS_SPACECRAFT' frame is -94000.
C
C
C        Suppose that R(t) is the rotation matrix whose columns
C        represent the inertial pointing vectors of the MGS spacecraft
C        axes at time `t'.
C
C        Then the angular velocity vector points along the vector given
C        by:
C
C                                T
C            limit  AXIS( R(t+h)R )
C            h-->0
C
C
C        And the magnitude of the angular velocity at time `t' is given
C        by:
C
C                                T
C            d ANGLE ( R(t+h)R(t) )
C           ------------------------   at   h = 0
C                      dh
C
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C
C           KPL/MK
C
C           File name: ckfrot_ex1.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              naif0012.tls                  Leapseconds
C              mgs_sclkscet_00061.tsc        MGS SCLK coefficients
C              mgs_sc_ext12.bc               MGS s/c bus attitude
C
C           \begindata
C
C           KERNELS_TO_LOAD = ( 'naif0012.tls',
C                               'mgs_sclkscet_00061.tsc',
C                               'mgs_sc_ext12.bc' )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C
C              PROGRAM CKFROT_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         EPOCH
C              PARAMETER           ( EPOCH  = '2003-JUL-25 13:00:00' )
C
C              INTEGER               INST
C              PARAMETER           ( INST   = -94000 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      ANGLE
C              DOUBLE PRECISION      ANGVEL ( 3    )
C              DOUBLE PRECISION      AXIS   ( 3    )
C              DOUBLE PRECISION      ET
C              DOUBLE PRECISION      INFROT ( 3, 3 )
C              DOUBLE PRECISION      H
C              DOUBLE PRECISION      RET    ( 3, 3 )
C              DOUBLE PRECISION      RETH   ( 3, 3 )
C
C              INTEGER               REF
C              INTEGER               REFH
C
C              LOGICAL               FOUND
C              LOGICAL               FOUNDH
C
C        C
C        C     Load the required LSK, SCLK and CK. Use a
C        C     meta-kernel for convenience.
C        C
C              CALL FURNSH ( 'ckfrot_ex1.tm' )
C
C        C
C        C     First convert the time to seconds past J2000. Set the
C        C     delta time (1 ms).
C        C
C              CALL STR2ET ( EPOCH, ET )
C              H = 1.D-3
C
C        C
C        C     Now, look up the rotation from the MGS spacecraft
C        C     frame specified by its frame class ID (CK ID) to a
C        C     base reference frame (returned by CKFROT), at ET
C        C     and ET+H.
C        C
C              CALL CKFROT ( INST, ET,   RET,  REF,  FOUND  )
C              CALL CKFROT ( INST, ET+H, RETH, REFH, FOUNDH )
C
C        C
C        C     If both rotations were computed and if the base
C        C     reference frames are the same, compute the
C        C     instantaneous angular velocity vector.
C        C
C              IF ( FOUND .AND. FOUNDH .AND. REF .EQ. REFH ) THEN
C
C        C
C        C        Compute the infinitesimal rotation R(t+h)R(t)**T.
C        C
C                 CALL MXMT ( RETH, RET, INFROT )
C
C        C
C        C        Compute the AXIS and ANGLE of the infinitesimal
C        C        rotation.
C        C
C                 CALL RAXISA ( INFROT, AXIS, ANGLE )
C
C        C
C        C        Scale AXIS to get the angular velocity vector.
C        C
C                 CALL VSCL ( ANGLE/H, AXIS, ANGVEL )
C
C        C
C        C        Output the results.
C        C
C                 WRITE(*,'(A)')
C             .           'Instantaneous angular velocity vector:'
C                 WRITE(*,'(3F15.10)') ANGVEL
C                 WRITE(*,'(A,I5)') 'Reference frame ID:', REF
C
C              ELSE
C
C                 WRITE(*,*) 'ERROR: data not found or frame mismatch.'
C
C              END IF
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Instantaneous angular velocity vector:
C           0.0001244121   0.0008314866   0.0003028634
C        Reference frame ID:    1
C
C
C$ Restrictions
C
C     1)  A CK file must be loaded prior to calling this routine.
C
C     2)  LSK and SCLK files needed for time conversions must be loaded
C         prior to calling this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 13-DEC-2021 (JDR) (BVS) (NJB)
C
C        Edited the header to comply with NAIF standard and modern
C        SPICE CK and frames terminology. Added initialization of local
C        variable SFND.
C
C        Added complete code example.
C
C-    SPICELIB Version 1.2.0, 17-FEB-2000 (WLT)
C
C        The routine now checks to make sure convert ET to TICKS
C        and that at least one C-kernel is loaded before trying
C        to look up the transformation. Also the routine now calls
C        SCE2C instead of SCE2T.
C
C-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
C
C-&


C$ Index_Entries
C
C     get instrument frame rotation and reference frame
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
      LOGICAL               ZZSCLK

C
C     Local parameters
C
C        NDC        is the number of double precision components in an
C                   unpacked C-kernel segment descriptor.
C
C        NIC        is the number of integer components in an unpacked
C                   C-kernel segment descriptor.
C
C        NC         is the number of components in a packed C-kernel
C                   descriptor.  All DAF summaries have this formulaic
C                   relationship between the number of its integer and
C                   double precision components and the number of packed
C                   components.
C
C        IDLEN      is the length of the C-kernel segment identifier.
C                   All DAF names have this formulaic relationship
C                   between the number of summary components and
C                   the length of the name (You will notice that
C                   a name and a summary have the same length in bytes.)
C

      INTEGER               NDC
      PARAMETER           ( NDC = 2 )

      INTEGER               NIC
      PARAMETER           ( NIC = 6 )

      INTEGER               NC
      PARAMETER           ( NC = NDC + ( NIC + 1 )/2 )

      INTEGER               IDLEN
      PARAMETER           ( IDLEN = NC * 8 )

C
C     Local variables
C
      INTEGER               HANDLE
      INTEGER               ICD      ( NIC  )
      INTEGER               SCLKID

      DOUBLE PRECISION      AV       ( 3    )
      DOUBLE PRECISION      CLKOUT
      DOUBLE PRECISION      DESCR    ( NC   )
      DOUBLE PRECISION      DCD      ( NDC  )
      DOUBLE PRECISION      ROT      ( 3, 3 )
      DOUBLE PRECISION      TIME
      DOUBLE PRECISION      TOL

      CHARACTER*(IDLEN)     SEGID

      LOGICAL               NEEDAV
      LOGICAL               SFND
      LOGICAL               PFND
      LOGICAL               HAVE

C
C     Set FOUND to .FALSE. right now in case we end up
C     returning before doing any work.
C
      FOUND = .FALSE.
      REF   =  0

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CKFROT' )

C
C     We don't need angular velocity data.
C     Assume the segment won't be found until it really is.
C
      NEEDAV = .FALSE.
      TOL    =  0.0D0

C
C     Begin a search for this instrument and time, and get the first
C     applicable segment.
C
      CALL CKHAVE ( HAVE )
      CALL CKMETA ( INST,  'SCLK', SCLKID )

      IF ( .NOT. HAVE ) THEN
         CALL CHKOUT ( 'CKFROT' )
         RETURN
      ELSE IF ( .NOT. ZZSCLK ( INST, SCLKID ) ) THEN
         CALL CHKOUT ( 'CKFROT' )
         RETURN
      END IF

C
C     Initialize SFND here in case an error occurs before CKSNS can
C     set its value.
C
      SFND = .FALSE.

      CALL SCE2C  ( SCLKID, ET,            TIME   )
      CALL CKBSS  ( INST,   TIME,   TOL,   NEEDAV )
      CALL CKSNS  ( HANDLE, DESCR,  SEGID, SFND   )

C
C     Keep trying candidate segments until a segment can produce a
C     pointing instance within the specified time tolerance of the
C     input time.
C
C     Check FAILED to prevent an infinite loop if an error is detected
C     by a SPICELIB routine and the error handling is not set to abort.
C
      DO WHILE ( SFND .AND. ( .NOT. FAILED () ) )

         CALL CKPFS ( HANDLE, DESCR, TIME,   TOL, NEEDAV,
     .                ROT,    AV,    CLKOUT, PFND          )

         IF ( PFND ) THEN
C
C           Found one. Fetch the ID code of the reference frame
C           from the descriptor.
C
            CALL DAFUS  ( DESCR, NDC, NIC, DCD, ICD )
            REF   =  ICD( 2 )
            FOUND = .TRUE.
C
C           We now have the rotation matrix from
C           REF to INS. We invert ROT to get the rotation
C           from INST to REF.
C
            CALL XPOSE ( ROT, ROTATE )

            CALL CHKOUT ( 'CKFROT' )
            RETURN

         END IF

         CALL CKSNS ( HANDLE, DESCR, SEGID, SFND )

      END DO


      CALL CHKOUT ( 'CKFROT' )
      RETURN
      END
