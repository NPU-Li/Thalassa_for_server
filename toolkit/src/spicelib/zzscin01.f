C$Procedure ZZSCIN01 ( Private, SCLK database initialization, type 01 )

      SUBROUTINE ZZSCIN01 ( HDSCLK, SCPOOL, CLKLST,
     .                      DPFREE, IFREE,  PRVSC  )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Initialize the SCLK type 1 database.
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
C     None.
C
C$ Keywords
C
C     None.
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzsc01.inc'

      INTEGER               HDSCLK ( * )
      INTEGER               SCPOOL ( LBSNGL : * )
      INTEGER               CLKLST ( * )
      INTEGER               DPFREE
      INTEGER               IFREE
      INTEGER               PRVSC

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HDSCLK     O   Array of heads of hash collision lists.
C     SCPOOL     O   Singly linked collision list pool.
C     CLKLST     O   Array of SCLK codes.
C     DPFREE     O   Index of first free entry in d.p. data buffer.
C     IFREE      O   Index of first free entry in integer data buffer.
C     PRVSC      O   Previous SCLK ID code.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     HDSCLK      is an array containing the head node indices of the
C                 SCLK ID collision lists stored in SCPOOL. On output,
C                 this array is zero-filled.
C
C     SCPOOL      is a singly linked list pool containing collision
C                 lists for sets of SCLK IDs that have the same hash
C                 values. On output, this data structure has the
C                 initial state set by ZZHSIINI.
C
C     CLKLST      is an array of SCLK IDs, which is parallel to the
C                 data portion of SCPOOL. Each allocated node of
C                 SCPOOL is the index in CLKLST of the associated
C                 SCLK ID. On output, this array is zero-filled.
C
C     DPFREE      is the index in the database's double precision
C                 buffer of the first free element. On output, this
C                 index is 1.
C
C     IFREE       is index in the database's integer buffer of the
C                 first free element. On output, this index is 1.
C
C     PRVSC       is the previous ID code passed to the SC01 subsystem.
C                 On output, this code is set to 0, which is never a
C                 valid SCLK ID code.
C
C$ Parameters
C
C     See the include file zzsc01.inc.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     None. See usage in SC01.
C
C$ Restrictions
C
C     This is a SPICE-private routine. It should not
C     be called directly by user application code.
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
C     initialize sclk type 1 database
C
C-&


C$ Revisions
C
C     None.
C`
C-&

C
C     This routine must execute when SPICE error handling code is in
C     the process of responding to an error, so this routine doesn't
C     participate in SPICE error handling or tracing. The routines
C     called here are error-free.
C
C     Initialize SC01 data structures.
C
C     ZZHSIINI won't signal an error as long as the parameter MXNCLK
C     is positive, so we don't check FAILED here.
C
      CALL ZZHSIINI ( MXNCLK, HDSCLK, SCPOOL )
      CALL CLEARI   ( MXNCLK, CLKLST         )

      DPFREE = 1
      IFREE  = 1
      PRVSC  = 0

      RETURN
      END
