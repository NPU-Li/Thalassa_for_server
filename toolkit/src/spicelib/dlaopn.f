C$Procedure DLAOPN ( DLA, open new file )

      SUBROUTINE DLAOPN ( FNAME, FTYPE, IFNAME, NCOMCH, HANDLE )

C$ Abstract
C
C     Open a new DLA file and set the file type.
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
C     DLA
C
C$ Keywords
C
C     DAS
C     DLA
C     FILES
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'

      CHARACTER*(*)         FNAME
      CHARACTER*(*)         FTYPE
      CHARACTER*(*)         IFNAME
      INTEGER               NCOMCH
      INTEGER               HANDLE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of a DLA file to be opened.
C     FTYPE      I   Mnemonic code for type of data in the DLA file.
C     IFNAME     I   Internal file name.
C     NCOMCH     I   Number of comment characters to allocate.
C     HANDLE     O   Handle assigned to the opened DLA file.
C
C$ Detailed_Input
C
C     FNAME    is the name of a new DLA file to be created. The file
C              will be left opened for write access.
C
C     FTYPE    is a code for type of data placed into a DLA file. The
C              non-blank part of FTYPE is used as the "file type"
C              portion of the ID word in the DLA file.
C
C              The first nonblank character and the three, or fewer,
C              characters immediately following it, giving four
C              characters, are used to represent the type of the data
C              placed in the DLA file. This is provided as a convenience
C              for higher level software. It is an error if this string
C              is blank. Also, the file type may not contain any
C              nonprinting characters. When written to the DLA file, the
C              value for the type IS case sensitive.
C
C              NAIF has reserved for its own use file types consisting
C              of the upper case letters (A-Z) and the digits 0-9. NAIF
C              recommends lower case or mixed case file types be used by
C              all others in order to avoid any conflicts with NAIF file
C              types.
C
C     IFNAME   is the internal file name for the new file. The name may
C              contain as many as 60 characters. This name should
C              uniquely identify the file.
C
C     NCOMCH   is the number of comment characters to allocate.
C
C              NCOMCH is used to establish the number of comment records
C              that will be allocated to the new DLA file. The number of
C              comment records allocated is the minimum required to
C              store the specified number of comment characters.
C
C              Allocating comment records at file creation time may
C              reduce the likelihood of having to expand the
C              comment area later.
C
C$ Detailed_Output
C
C     HANDLE   is the file handle associated with the file. This handle
C              is used to identify the file in subsequent calls to other
C              DLA routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input filename is blank, an error is signaled by a
C         routine in the call tree of this routine. No file will be
C         created.
C
C     2)  If the specified file cannot be opened without exceeding the
C         maximum allowed number of open DAS files, an error is signaled
C         by a routine in the call tree of this routine. No file will be
C         created.
C
C     3)  If the file cannot be opened properly, an error is signaled by
C         a routine in the call tree of this routine. No file will be
C         created.
C
C     4)  If the initial records in the file cannot be written, an error
C         is signaled by a routine in the call tree of this routine. No
C         file will be created.
C
C     5)  If no logical units are available, an error is signaled by a
C         routine in the call tree of this routine. No file will be
C         created.
C
C     6)  If the file type is blank, an error is signaled by a routine
C         in the call tree of this routine. No file will be created.
C
C     7)  If the file type contains nonprinting characters, decimal 0-31
C         and 127-255, an error is signaled by a routine in the call
C         tree of this routine. No file will be created.
C
C     8)  If the number of comment characters allocated to be allocated,
C         NCOMCH, is negative, the error SPICE(BADRECORDCOUNT) is
C         signaled. No file will be created.
C
C$ Files
C
C     See argument FNAME.
C
C$ Particulars
C
C     DLA files are built using the DAS low-level format; DLA files are
C     a specialized type of DAS file in which data are organized as a
C     doubly linked list of segments. Each segment's data belong to
C     contiguous components of character, double precision, and integer
C     type.
C
C     This routine creates a new DLA file and sets the type of the
C     file to the mnemonic code passed to it.
C
C     DLA files created by this routine have initialized file records.
C     The ID word in a DLA file record has the form
C
C        DAS/xxxx
C
C     where the characters following the slash are supplied by the
C     caller of this routine.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a DLA file containing one segment; the segment
C        contains character, double precision, and integer data.
C        After writing and closing the file, open the file for
C        read access; dump the data to standard output.
C
C
C        Example code begins here.
C
C
C              PROGRAM DLAOPN_EX1
C              IMPLICIT NONE
C
C              INCLUDE 'dla.inc'
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         DLA
C              PARAMETER           ( DLA    = 'dlaopn_ex1.dla' )
C
C              INTEGER               IFNLEN
C              PARAMETER           ( IFNLEN =  60 )
C
C              INTEGER               LNSIZE
C              PARAMETER           ( LNSIZE =  61 )
C
C              INTEGER               MAXC
C              PARAMETER           ( MAXC   =  5 )
C
C              INTEGER               MAXD
C              PARAMETER           ( MAXD   =  50 )
C
C              INTEGER               MAXI
C              PARAMETER           ( MAXI   =  100 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(LNSIZE)    CVALS   ( MAXC )
C              CHARACTER*(LNSIZE)    CVALS2  ( MAXC )
C              CHARACTER*(IFNLEN)    IFNAME
C
C              DOUBLE PRECISION      DVALS   ( MAXD )
C              DOUBLE PRECISION      DVALS2  ( MAXD )
C
C              INTEGER               BASE
C              INTEGER               DESCR   ( DLADSZ )
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               IVALS   ( MAXI )
C              INTEGER               IVALS2  ( MAXI )
C              INTEGER               J
C              INTEGER               K
C              INTEGER               N
C              INTEGER               NCOMCH
C
C              LOGICAL               FOUND
C
C        C
C        C     Set the internal file name.  Don't reserve characters in
C        C     the DAS comment area.
C        C
C              IFNAME = 'Example DLA file for testing'
C              NCOMCH = 0
C
C        C
C        C     Open a new DLA file.
C        C
C              CALL DLAOPN ( DLA, 'DLA', IFNAME, NCOMCH, HANDLE )
C
C        C
C        C     Begin a new segment.
C        C
C              CALL DLABNS ( HANDLE )
C
C        C
C        C     Add character data to the segment.
C        C
C              DO I = 1, MAXC
C
C                 DO J = 1, LNSIZE
C
C                    K = MOD( J+I-1, 10 )
C
C                    CALL INTSTR ( K,  CVALS(I)(J:J) )
C
C                 END DO
C
C              END DO
C
C              CALL DASADC ( HANDLE, MAXC*LNSIZE, 1, LNSIZE, CVALS )
C
C        C
C        C     Add integer and double precision data to the segment.
C        C
C              DO I = 1, MAXI
C                 IVALS(I) = I
C              END DO
C
C              CALL DASADI ( HANDLE, MAXI, IVALS )
C
C              DO I = 1, MAXD
C                 DVALS(I) = I
C              END DO
C
C              CALL DASADD ( HANDLE, MAXD, DVALS )
C
C        C
C        C     End the segment.
C        C
C              CALL DLAENS ( HANDLE )
C
C        C
C        C     Close the file.  The routine DASCLS flushes the DAS
C        C     buffers and segregates the file before closing it.
C        C
C              CALL DASCLS ( HANDLE )
C
C        C
C        C     Now read the file and check the data.
C        C
C              CALL DASOPR ( DLA, HANDLE )
C
C        C
C        C     Obtain the segment descriptor for the sole segment
C        C     in the file. We need not check the found flag
C        C     in this case because we know there is one segment
C        C     in the file.
C        C
C              CALL DLABFS ( HANDLE, DESCR, FOUND )
C
C        C
C        C     Fetch character data from the segment.  Obtain the
C        C     base address of the character data and the
C        C     character count from the descriptor.
C        C
C              BASE = DESCR(CBSIDX)
C              N    = DESCR(CSZIDX)
C
C              CALL DASRDC ( HANDLE, BASE+1, BASE+N, 1, LNSIZE, CVALS2 )
C
C        C
C        C     Display the character data.
C        C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Character array:'
C
C              DO I = 1, N/LNSIZE
C                 WRITE (*,*) CVALS2(I)
C              END DO
C
C        C
C        C     Fetch and display the integer and double precision data.
C        C
C              BASE = DESCR(IBSIDX)
C              N    = DESCR(ISZIDX)
C
C              CALL DASRDI( HANDLE, BASE+1, BASE+N, IVALS2 )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Integer array:'
C              DO I = 1, N/10
C                 WRITE (*,'(10I6)') (IVALS2((I-1)*10 + J), J=1, 10)
C              END DO
C
C              BASE = DESCR(DBSIDX)
C              N    = DESCR(DSZIDX)
C
C              CALL DASRDD( HANDLE, BASE+1, BASE+N, DVALS2 )
C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Double precision array:'
C              DO I = 1, N/10
C                 WRITE (*,'(10F6.1)') (DVALS2((I-1)*10 + J), J=1, 10)
C              END DO
C
C        C
C        C     Close the file.  This step is unnecessary in this
C        C     program, but is a good practice in general
C        C     because closing the file frees resources.
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
C         Character array:
C         1234567890123456789012345678901234567890123456789012345678901
C         2345678901234567890123456789012345678901234567890123456789012
C         3456789012345678901234567890123456789012345678901234567890123
C         4567890123456789012345678901234567890123456789012345678901234
C         5678901234567890123456789012345678901234567890123456789012345
C
C         Integer array:
C             1     2     3     4     5     6     7     8     9    10
C            11    12    13    14    15    16    17    18    19    20
C            21    22    23    24    25    26    27    28    29    30
C            31    32    33    34    35    36    37    38    39    40
C            41    42    43    44    45    46    47    48    49    50
C            51    52    53    54    55    56    57    58    59    60
C            61    62    63    64    65    66    67    68    69    70
C            71    72    73    74    75    76    77    78    79    80
C            81    82    83    84    85    86    87    88    89    90
C            91    92    93    94    95    96    97    98    99   100
C
C         Double precision array:
C           1.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0   9.0  10.0
C          11.0  12.0  13.0  14.0  15.0  16.0  17.0  18.0  19.0  20.0
C          21.0  22.0  23.0  24.0  25.0  26.0  27.0  28.0  29.0  30.0
C          31.0  32.0  33.0  34.0  35.0  36.0  37.0  38.0  39.0  40.0
C          41.0  42.0  43.0  44.0  45.0  46.0  47.0  48.0  49.0  50.0
C
C
C        Note that after run completion, a new DLA file exists in the
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
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 14-SEP-2021 (JDR) (NJB)
C
C        Edited the header to comply with NAIF standard.
C
C        Updated the header to describe the usage of input argument
C        NCOMCH instead of the previously documented NCOMR.
C
C        Added complete code example based on that provided for DLABNS
C        and DLAENS.
C
C        Replaced sequence of asterisks with string 'xxxx'
C        in the comment line illustrating the DAS ID word syntax.
C
C-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
C
C        Updated version info.
C
C        01-APR-2016 (NJB)
C
C           Changed short error message for invalid comment
C           count. Corrected reference to "DASCLU" in comments.
C
C        08-OCT-2009 (NJB)
C
C           Updated header.
C
C        09-FEB-2005 (NJB) (KRG)
C
C-&


C$ Index_Entries
C
C     open a new DLA file
C     open a new DLA file with write access
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C

C
C     Local variables
C
      INTEGER               NCOMR


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DLAOPN' )

C
C     Compute the number of comment records required.
C
      IF ( NCOMCH .GT. 0 ) THEN

         NCOMR = ( (NCOMCH-1) / NCHREC )  +  1

      ELSE IF ( NCOMCH .EQ. 0 ) THEN

         NCOMR = 0

      ELSE

         CALL SETMSG ( 'Requested number of comment characters '
     .   //            'must be non-negative but was #.'         )
         CALL ERRINT ( '#',  NCOMCH                              )
         CALL SIGERR ( 'SPICE(BADRECORDCOUNT)'                   )
         CALL CHKOUT ( 'DLAOPN'                                  )
         RETURN

      END IF

C
C     Let the DAS "open new" routine do the work.
C
      CALL DASONW ( FNAME, FTYPE, IFNAME, NCOMR, HANDLE )

C
C     Write the format version.
C
      CALL DASADI ( HANDLE, 1, FMTVER )

C
C     Initialize the forward and backward segment list pointers.
C
      CALL DASADI ( HANDLE, 1, NULPTR )
      CALL DASADI ( HANDLE, 1, NULPTR )

C
C     We leave the file open, since further writes to the file
C     should occur next.  The file will eventually be closed
C     by a call to DASCLS or DASLLC, if all goes well.
C
      CALL CHKOUT ( 'DLAOPN' )
      RETURN
      END
