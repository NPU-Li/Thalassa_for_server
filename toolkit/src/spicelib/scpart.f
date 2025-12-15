C$Procedure SCPART ( Spacecraft Clock Partition Information )

      SUBROUTINE SCPART ( SC, NPARTS, PSTART, PSTOP )

C$ Abstract
C
C     Get spacecraft clock partition information from a spacecraft
C     clock kernel file.
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
C     TIME
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'sclk.inc'
      INCLUDE               'zzctr.inc'

      INTEGER               SC
      INTEGER               NPARTS
      DOUBLE PRECISION      PSTART  ( * )
      DOUBLE PRECISION      PSTOP   ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft identification code.
C     NPARTS     O   The number of spacecraft clock partitions.
C     PSTART     O   Array of partition start times.
C     PSTOP      O   Array of partition stop times.
C     MXPART     P   Maximum number of partitions.
C
C$ Detailed_Input
C
C     SC       is the NAIF ID for the spacecraft whose clock partition
C              information is being requested.
C
C$ Detailed_Output
C
C     NPARTS   is the number of spacecraft clock time partitions
C              described in the kernel file for spacecraft SC.
C
C     PSTART   is an array containing NPARTS partition start times
C              represented as double precision, encoded SCLK
C              ("ticks"). The values contained in PSTART are whole
C              numbers.
C
C     PSTOP    is an array containing NPARTS partition end times
C              represented as double precision, encoded SCLK
C              ("ticks"). The values contained in PSTOP are whole
C              numbers.
C
C$ Parameters
C
C     See the include file
C
C        sclk.inc
C
C     for sizes and limits used by the SCLK system.
C
C     MXPART   is the maximum number of partitions for any spacecraft
C              clock. SCLK kernels contain start and stop times for
C              each partition. See the include file sclk.inc for this
C              parameter's value.
C
C$ Exceptions
C
C     1)  If the kernel variables containing the spacecraft clock
C         partition start and stop times have not been loaded in the
C         kernel pool, an error is signaled by a routine in the call
C         tree of this routine.
C
C     2)  If the number of start and stop times are different,
C         the error SPICE(NUMPARTSUNEQUAL) is signaled.
C
C$ Files
C
C     An SCLK kernel containing spacecraft clock partition start
C     and stop times for the spacecraft clock indicated by SC must
C     be loaded into the kernel pool.
C
C$ Particulars
C
C     SCPART looks for two variables in the kernel pool for each
C     spacecraft's partition information. If SC = -nn, then the names of
C     the variables are
C
C        SCLK_PARTITION_START_nn
C        SCLK_PARTITION_END_nn
C
C     The start and stop times returned are in units of "ticks".
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as input,
C     the compiler and supporting libraries, and the machine specific
C     arithmetic implementation.
C
C     1) The following code example finds partition start and stop
C        times for the Stardust spacecraft from a spacecraft clock
C        kernel file. Since those times are always returned in units
C        of ticks, the program uses SCFMT to print the times in
C        Stardust clock format.
C
C        Use the SCLK kernel below to load the Stardust time
C        correlation data and spacecraft clock partition information.
C
C           sdu_sclkscet_00074.tsc
C
C
C        Example code begins here.
C
C
C              PROGRAM SCPART_EX1
C              IMPLICIT NONE
C
C        C
C        C     Include the parameters that define the sizes and limits
C        C     of the SCLK system.
C        C
C              INCLUDE 'sclk.inc'
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               CLKLEN
C              PARAMETER           ( CLKLEN = 30 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(CLKLEN)    START
C              CHARACTER*(CLKLEN)    STOP
C
C              DOUBLE PRECISION      PSTART (MXPART)
C              DOUBLE PRECISION      PSTOP  (MXPART)
C
C              INTEGER               I
C              INTEGER               NPARTS
C              INTEGER               SC
C
C        C
C        C     Assign the value for the Stardust spacecraft ID.
C        C
C              SC = -29
C
C        C
C        C     Load the SCLK file.
C        C
C              CALL FURNSH ( 'sdu_sclkscet_00074.tsc' )
C
C        C
C        C     Retrieve the arrays for PSTART and PSTOP and the
C        C     number of partitions within the SCLK.
C        C
C              CALL SCPART ( SC, NPARTS, PSTART, PSTOP )
C
C        C
C        C     Loop over each array value.
C        C
C              DO I= 1, NPARTS
C
C                 CALL SCFMT ( SC, PSTART( I ), START )
C                 CALL SCFMT ( SC, PSTOP ( I ), STOP )
C
C                 WRITE(*,*)
C                 WRITE(*,*) 'Partition: ', I
C                 WRITE(*,*) '   Start : ', START
C                 WRITE(*,*) '   Stop  : ', STOP
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
C         Partition:            1
C            Start : 0000000000.000
C            Stop  : 0602741011.080
C
C         Partition:            2
C            Start : 0602741014.217
C            Stop  : 0605660648.173
C
C         Partition:            3
C            Start : 0605660649.000
C            Stop  : 0631375256.224
C
C         Partition:            4
C            Start : 0631375257.000
C            Stop  : 0633545577.218
C
C         Partition:            5
C            Start : 0633545578.000
C            Stop  : 0644853954.043
C
C         Partition:            6
C            Start : 0644853954.000
C            Stop  : 0655316480.089
C
C         Partition:            7
C            Start : 0655316480.000
C            Stop  : 0660405279.066
C
C         Partition:            8
C            Start : 0660405279.000
C            Stop  : 0670256568.229
C
C         Partition:            9
C            Start : 0670256569.000
C            Stop  : 0674564039.091
C
C         Partition:           10
C            Start : 0674564040.000
C            Stop  : 4294537252.255
C
C
C$ Restrictions
C
C     1)  This routine assumes that an SCLK kernel appropriate to the
C         spacecraft identified by SC has been loaded into the kernel
C         pool.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     J.M. Lynch         (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     R.E. Thurman       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.4.0, 06-JUL-2021 (JDR) (NJB)
C
C        Code was re-written to fetch partition data from the SC01
C        subsystem. This routine no longer sets watches on kernel
C        variables.
C
C        Edited the header to comply with NAIF standard. Removed
C        unnecessary entries from $Revisions section.
C
C        Added complete code example based on existing code fragment.
C
C-    SPICELIB Version 2.3.1, 19-MAR-2014 (NJB)
C
C        Minor header comment updates were made.
C
C-    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS)
C
C        Updated to keep track of the POOL counter and call ZZCVPOOL.
C
C-    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to this routine.
C
C-    SPICELIB Version 2.1.0, 05-FEB-2008 (NJB)
C
C        The values of the parameter MXPART is now
C        provided by the INCLUDE file sclk.inc.
C
C-    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references
C        to FURNSH.
C
C-    SPICELIB Version 1.1.0, 22-MAR-1993 (JML)
C
C        The routine now uses the kernel pool watch capability.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB) (JML) (RET)
C
C-&


C$ Index_Entries
C
C     spacecraft_clock partition information
C
C-&


C$ Revisions
C
C-    SPICELIB Version 2.4.0, 04-NOV-2020 (JDR) (NJB)
C
C        Code was re-written to fetch partition data from the SC01
C        subsystem. This routine no longer sets watches on kernel
C        variables. Setting watches "touches" the kernel pool, which
C        thwarts optimizations of many SPICE subsystems.
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SCPART' )

C
C     Get the partition values from the type 1 subsystem. If the
C     clock is not type 1, an error will be signaled.
C
      CALL SCPR01 ( SC, NPARTS, PSTART, PSTOP )

      CALL CHKOUT ( 'SCPART' )
      RETURN
      END
