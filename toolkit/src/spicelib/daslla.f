C$Procedure DASLLA ( DAS, last logical addresses )

      SUBROUTINE DASLLA ( HANDLE, LASTC, LASTD, LASTI )

C$ Abstract
C
C     Return last DAS logical addresses of character, double precision
C     and integer type that are currently in use in a specified DAS
C     file.
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
C     DAS
C
C$ Keywords
C
C     ARRAY
C     DAS
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     LASTC      O   Last character address in use.
C     LASTD      O   Last double precision address in use.
C     LASTI      O   Last integer address in use.
C
C$ Detailed_Input
C
C     HANDLE   is the file handle of a DAS file whose active
C              logical address ranges are desired.
C
C$ Detailed_Output
C
C     LASTC,
C     LASTD,
C     LASTI    are, respectively, the last 1-based logical addresses of
C              character, double precision, and integer type in use in
C              the specified DAS file.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, an error is signaled by
C         a routine in the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is a utility that allows a calling program to
C     find the range of logical addresses currently in use in any
C     DAS file.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a DAS file containing 10 integers, 5 double precision
C        numbers, and 4 characters, then use DASLLA to find the logical
C        address ranges in use.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASLLA_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'daslla_ex1.das' )
C
C              INTEGER               IFNMLN
C              PARAMETER           ( IFNMLN = 60 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(IFNMLN)    IFNAME
C              CHARACTER*(4)         TYPE
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               LASTC
C              INTEGER               LASTD
C              INTEGER               LASTI
C
C        C
C        C     Open a new DAS file. Use the file name as the internal
C        C     file name, and reserve no records for comments.
C        C
C              TYPE   = 'TEST'
C              IFNAME = 'TEST.DAS/NAIF/NJB/11-NOV-1992-20:12:20'
C
C              CALL DASONW ( FNAME, TYPE, IFNAME, 0, HANDLE )
C
C              DO I = 1, 10
C                 CALL DASADI ( HANDLE, 1, I )
C              END DO
C
C              DO I = 1, 5
C                 CALL DASADD ( HANDLE, 1, DBLE(I) )
C              END DO
C
C        C
C        C     Add character data to the file. DAS character data are
C        C     treated as a character array, not as a string. The
C        C     following call adds only the first 4 characters to the
C        C     DAS file.
C        C
C              CALL DASADC ( HANDLE, 4, 1, 4, 'SPUDWXY' )
C
C        C
C        C     Now check the logical address ranges.
C        C
C              CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )
C
C              WRITE (*,*) 'Last character address in use: ', LASTC
C              WRITE (*,*) 'Last d.p. address in use     : ', LASTD
C              WRITE (*,*) 'Last integer address in use  : ', LASTI
C
C        C
C        C     Close the DAS file.
C        C
C              CALL DASCLS ( HANDLE )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Last character address in use:            4
C         Last d.p. address in use     :            5
C         Last integer address in use  :           10
C
C
C        Note that after run completion, a new DAS file exists in the
C        output directory.
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
C-    SPICELIB Version 1.1.0, 30-JUN-2021 (JDR)
C
C        Added IMPLICIT NONE statement. Local parameter declarations
C        have been moved from the $Declarations section to the
C        procedure's code.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example from existing fragment.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
C
C-&


C$ Index_Entries
C
C     return last logical addresses in DAS file
C     return logical address range of DAS file
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters.
C
      INTEGER               CHR
      PARAMETER           ( CHR    =  1  )

      INTEGER               DP
      PARAMETER           ( DP     =  2  )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )

C
C     Local variables
C
      INTEGER               FREE
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASLLA' )


C
C     The file summary for the indicated DAS file contains all of the
C     information we need.
C
      CALL DASHFS ( HANDLE,
     .              NRESVR,
     .              NRESVC,
     .              NCOMR,
     .              NCOMC,
     .              FREE,
     .              LASTLA,
     .              LASTRC,
     .              LASTWD )

      LASTC = LASTLA(CHR)
      LASTD = LASTLA(DP )
      LASTI = LASTLA(INT)

      CALL CHKOUT ( 'DASLLA' )
      RETURN
      END
