C$Procedure SPCOPN ( SPK or CK, open new file )

      SUBROUTINE SPCOPN ( FNAME, IFNAME, HANDLE )

C$ Abstract
C
C     Open a new SPK or CK file for subsequent write requests.
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
C     SPC
C
C$ Keywords
C
C     EPHEMERIS
C     FILES
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         FNAME
      CHARACTER*(*)         IFNAME
      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of SPK or CK file to be created.
C     IFNAME     I   Internal file name.
C     HANDLE     O   Handle of new SPK or CK file.
C
C$ Detailed_Input
C
C     FNAME    is the name of a new SPK or CK file to be created.
C
C     IFNAME   is the internal file name of the file to be created.
C              IFNAME may contain up to 60 characters.
C
C$ Detailed_Output
C
C     HANDLE   is the file handle assigned to the new file. This
C              should be used to refer to the file in all subsequent
C              calls to DAF and SPC routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified file cannot be opened without exceeding the
C         maximum number of files, an error is signaled by a routine in
C         the call tree of this routine.
C
C     2)  If the specified file cannot be opened without exceeding the
C         maximum number of DAF files, an error is signaled by a routine
C         in the call tree of this routine.
C
C     3)  If an I/O error occurs in the process of opening the file,
C         the error is signaled by a routine in the call tree of this
C         routine.
C
C     4)  If (for some reason) the initial records in the file cannot be
C         written, an error is signaled by a routine in the call tree of
C         this routine.
C
C     5)  If no logical units are available, an error is signaled by a
C         routine in the call tree of this routine.
C
C     6)  If the file name is blank or otherwise inappropriate, an error
C         is signaled by a routine in the call tree of this routine.
C
C$ Files
C
C     See argument FNAME above.
C
C$ Particulars
C
C     SPCOPN opens a new SPK or CK file. It is identical to DAFOPN
C     except SPCOPN defines several of the inputs that DAFOPN requires
C     and which specify that the DAF to be opened is an SPK or CK file.
C     Use DAFCLS to close any DAF including SPK and CK files.
C
C     SPCOPN, is not to be confused with the routines that load and
C     unload files to and from a buffer for use by the readers such as
C     SPKLEF (SPK, load ephemeris file) and CKLPF (CK, load pointing
C     file). The loading and unloading routines open and close the files
C     internally, so there is no need to call SPCOPN when loading or
C     unloading SPK or CK files.
C
C$ Examples
C
C     In the following code fragment, SPCOPN opens a new file,
C     to which an array is then added. GETDAT is a ficticious
C     non-SPICELIB routine whose function is to get the array data.
C     DAFBNA begins a new array, DAFADA adds data to an array,
C     and DAFENA ends a new array.
C
C               CALL SPCOPN  ( FNAME,  IFNAME, HANDLE )
C               CALL DAFBNA  ( HANDLE, SUM,    NAME   )
C
C               CALL GETDAT  ( N, DATA, FOUND )
C
C               DO WHILE ( FOUND )
C
C                  CALL DAFADA ( N, DATA )
C                  CALL GETDAT ( N, DATA, FOUND )
C
C               END DO
C
C               CALL DAFENA
C
C               CALL DAFCLS ( HANDLE )
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
C     J.E. McLean        (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 19-APR-2021 (JDR)
C
C        Added IMPLICIT NONE statement. Changed input argument name
C        "SPC" to "FNAME" for consistency with other routines.
C
C        Edited the header to comply with NAIF standard. Updated the
C        $Exceptions section to cover all errors detected by this
C        routine and remove unnecessary introduction referencing DAF
C        required reading.
C
C        Added DAF required reading to $Required_Reading.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
C
C-&


C$ Index_Entries
C
C     open new SPK or CK file
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C
C     ND, NI      are the Number of Double precision and the Number of
C                 Integer components in an SPK or CK segment descriptor.
C
C     RESV        is the number of records to reserve when opening the
C                 file.
C
C
      INTEGER               ND
      PARAMETER           ( ND   = 2 )

      INTEGER               NI
      PARAMETER           ( NI   = 6 )

      INTEGER               RESV
      PARAMETER           ( RESV = 0 )


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCOPN' )
      END IF

C
C     DAFOPN does all the work.  We just handle the values of
C     ND and NI which are specific to SPK and CK.  We'll not
C     reserve any records.
C
      CALL DAFOPN ( FNAME, ND, NI, IFNAME, RESV, HANDLE )


      CALL CHKOUT ( 'SPCOPN' )
      RETURN
      END
