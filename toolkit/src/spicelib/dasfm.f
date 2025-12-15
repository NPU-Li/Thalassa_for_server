C$Procedure DASFM ( DAS, file manager )
 
      SUBROUTINE DASFM ( FNAME,
     .                   FTYPE,
     .                   IFNAME,
     .                   HANDLE,
     .                   UNIT,
     .                   FREE,
     .                   LASTLA,
     .                   LASTRC,
     .                   LASTWD,
     .                   NRESVR,
     .                   NRESVC,
     .                   NCOMR,
     .                   NCOMC,
     .                   FHSET,
     .                   ACCESS  )
 
C$ Abstract
C
C     Manage open DAS files.
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
C     DAS
C     FILES
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE 'das.inc'
      INCLUDE 'errhnd.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL  =   -5 )
 
      CHARACTER*(*)         FNAME
      CHARACTER*(*)         FTYPE
      CHARACTER*(*)         IFNAME
      INTEGER               HANDLE
      INTEGER               UNIT
      INTEGER               FREE
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC
      INTEGER               FHSET  ( LBCELL : * )
      CHARACTER*(*)         ACCESS
 
C
C     The record length should be big enough to hold the greatest of the
C     following:
C
C        -- NWD double precision numbers.
C        -- NWI integers.
C        -- NWC characters.
C
C     These parameters are declared in das.inc.
C
 
 
 
C     For the following environments, record length is measured in
C     characters (bytes) with eight characters per double precision
C     number.
 
C     Environment: Sun, Sun FORTRAN
C     Source:      Sun Fortran Programmer's Guide
 
C     Environment: PC, MS FORTRAN
C     Source:      Microsoft Fortran Optimizing Compiler User's Guide
 
C     Environment: Macintosh, Language Systems FORTRAN
C     Source:      Language Systems FORTRAN Reference Manual,
C                  Version 1.2, page 12-7
 
C     Environment: PC/Linux, Fort77
C     Source:      Determined by experiment.
 
C     Environment: PC, Lahey F77 EM/32 Version 4.0
C     Source:      Lahey F77 EM/32 Language Reference Manual,
C                  page 144
 
C     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
C     Source:      FORTRAN/9000 Reference-Series 700 Computers,
C                  page 5-110
 
C     Environment: NeXT Mach OS (Black Hardware),
C                  Absoft Fortran Version 3.2
C     Source:      NAIF Program
 
      INTEGER               RECL
      PARAMETER           ( RECL   = 1024 )
      INTEGER               FILEN
      PARAMETER           ( FILEN  =  255 )
 
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  ENTRY POINTS
C     --------  ---  --------------------------------------------------
C     FNAME     I-O  OPR, OPW, ONW, OPN (Obsolete), HFN, FNH
C     FTYPE      I   ONW
C     IFNAME     I   ONW, OPN (Obsolete)
C     HANDLE    I-O  OPR, OPW, ONW, OPN (Obsolete), OPS, LLC, HLU, LUH,
C                    HFN, FNH, HAM, SIH
C     UNIT      I-O  HLU, LUH
C     FREE      I-O  HFS, UFS
C     LASTLA    I-O  HFS, UFS
C     LASTRC    I-O  HFS, UFS
C     LASTWD    I-O  HFS, UFS
C     NRESVR     O   HFS
C     NRESVC     O   HFS
C     NCOMR      O   HFS
C     NCOMC      O   HFS
C     FHSET      O   HOF
C     ACCESS    I-O  SIH, HAM
C     RECL       P   OPR, OPW, ONW, OPN (Obsolete)
C     FTSIZE     P   OPR, OPW, ONW, OPN (Obsolete), LLC, HLU, LUH, HFN,
C                    FNH
C
C$ Detailed_Input
C
C     FNAME    on input is the name of a DAS file to be opened, or
C              the name of a DAS file about which some information
C              (handle, logical unit) is requested.
C
C     FTYPE    on input is a code for the type of data that is
C              contained in the DAS file. This code has no meaning or
C              interpretation at the level of the DAS file
C              architecture, but is provided as a convenience for
C              higher level software. The maximum length for the file
C              type is four (4) characters. If the input string is
C              longer than four characters, the first nonblank
C              character and its three, at most, immediate successors
C              will be used as the file type. The file type may not
C              contain nonprinting characters, and it IS case
C              sensitive.
C
C     IFNAME   is the internal file name for a DAS file to be
C              created.
C
C     HANDLE   on input is the handle of a DAS file about which some
C              information (file name, logical unit) is requested,
C              or the handle of a DAS file to be closed.
C
C     UNIT     on input is the logical unit connected to a DAS file
C              about which some information (file name, handle) is
C              requested.
C
C     FREE     is the Fortran record number of the first free record
C              in a specified DAS file.
C
C     LASTLA   is an array containing the highest current logical
C              addresses, in the specified DAS file, of data of
C              character, double precision, and integer types, in
C              that order.
C
C     LASTRC   is an array containing the Fortran record numbers, in
C              the specified DAS file, of the directory records
C              containing the current last descriptors of clusters
C              of character, double precision, and integer data
C              records, in that order.
C
C     LASTWD   is an array containing the word positions, in the
C              specified DAS file, of the current last descriptors
C              of clusters of character, double precision, and
C              integer data records, in that order.
C
C     ACCESS   is the type of access for which a DAS file is open.
C              The values of ACCESS may be
C
C                 'READ'
C                 'WRITE'
C
C              Leading and trailing blanks are ignored, and case
C              is not significant.
C
C              DAS files that are open for writing may also be read.
C
C$ Detailed_Output
C
C     FNAME    on output is the name of a DAS file for which
C              the corresponding handle or logical unit has been
C              supplied.
C
C     HANDLE   on output is the handle of a DAS file for which
C              the corresponding file name or logical unit has been
C              supplied.
C
C     UNIT     on output is the logical unit connected to a DAS file
C              for which the corresponding file name or handle has
C              been supplied.
C
C     FREE     is the Fortran record number of the first free record
C              in a specified DAS file.
C
C     LASTLA   is an array containing the highest current logical
C              addresses, in the specified DAS file, of data of
C              character, double precision, and integer types, in
C              that order.
C
C     LASTRC   is an array containing the Fortran record numbers, in
C              the specified DAS file, of the directory records
C              containing the current last descriptors of clusters
C              of character, double precision, and integer data
C              records, in that order.
C
C     LASTWD   is an array containing the word positions, in the
C              specified DAS file, of the current last descriptors
C              of clusters of character, double precision, and
C              integer data records, in that order.
C
C     NRESVR   is the number of reserved records in a specified DAS
C              file.
C
C     NRESVC   is the number of characters in use in the reserved
C              record area of a specified DAS file.
C
C     NCOMR    is the number of comment records in a specified DAS
C              file.
C
C     NCOMC    is the number of characters in use in the comment area
C              of a specified DAS file.
C
C     FHSET    is a SPICE set containing the handles of the
C              currently open DAS files.
C
C$ Parameters
C
C     RECL     is the record length of a DAS file. Each record
C              must be large enough to hold the greatest of NWI
C              integers, NWD double precision numbers, or NWC
C              characters, whichever is greater. The units in which
C              the record length must be specified vary from
C              environment to environment. For example, VAX Fortran
C              requires record lengths to be specified in longwords,
C              where two longwords equal one double precision
C              number.
C
C     FTSIZE   is the maximum number of DAS files that a user can
C              have open simultaneously. This includes any files used
C              by the DAS system when closing files opened with write
C              access. Currently, DASCLS (via DASSDR) opens a scratch
C              DAS file using DASOPS to segregate (sort by data
C              type) the records in the DAS file being closed.
C              Segregating the data by type improves the speed of
C              access to the data.
C
C              In order to avoid the possibility of overflowing the
C              DAS file table we recommend, when at least one DAS
C              file is open with write access, that users of this
C              software limit themselves to at most FTSIZE - 2  other
C              open DAS files. If no files are to be open with write
C              access, then users may open FTSIZE files with no
C              possibility of overflowing the DAS file table.
C
C$ Exceptions
C
C     1)  If DASFM is called directly, the error SPICE(BOGUSENTRY)
C         is signaled.
C
C     2)  See entry points DASOPR, DASOPW, DASONW, DASOPN, DASOPS,
C         DASLLC, DASHFS, DASUFS, DASHLU, DASLUH, DASHFN, DASFNH,
C         DASHOF, and DASSIH for exceptions specific to those entry
C         points.
C
C$ Files
C
C     This set of routines is intended to support the creation,
C     updating, and reading of Fortran direct access files that
C     conform to the DAS file format. This format is described in
C     detail in the DAS Required Reading.
C
C     See FTSIZE in the $Parameters section for a description of a
C     potential problem with overflowing the DAS file table when at
C     least one DAS file is opened with write access.
C
C$ Particulars
C
C     As of the N0066 version of the SPICE Toolkit, DASFM uses the
C     SPICE handle manager subsystem and makes use of run-time
C     translation.
C
C     DASFM serves as an umbrella, allowing data to be shared by its
C     entry points:
C
C        DASOPR         Open for read.
C        DASOPW         Open for write.
C        DASONW         Open new.
C        DASOPN         Open new. (Obsolete: Use DASONW instead.)
C        DASOPS         Open as scratch file.
C
C        DASLLC         Low-level close.
C
C        DASHFS         Handle to file summary.
C        DASUFS         Update file summary.
C
C        DASHLU         Handle to logical unit.
C        DASLUH         Logical to handle.
C
C        DASHFN         Handle to name.
C        DASFNH         File name to handle.
C
C        DASHAM         Handle to access method.
C
C        DASHOF         Handles of open files.
C        DASSIH         Signal invalid handles.
C
C
C     Before a DAS file  can be used, it must be opened. Entry points
C     DASOPR and DASOPW provide the only means for opening an
C     existing DAS file.
C
C     Several files may be opened for use simultaneously. (This makes
C     it convenient to combine data from several files to produce a
C     single result, or to route subsets of data from a single source
C     to multiple DAS files.) As each DAS file is opened, it is
C     assigned a file handle, which is used to keep track of the file
C     internally, and which is used by the calling program to refer to
C     the file in all subsequent calls to DAS routines.
C
C     DAS files may be opened for either read or write access. Files
C     open for read access may not be changed in any way. Files opened
C     for write access may be both read from and written to.
C
C     DASONW is used to open a new DAS file. This routine extends the
C     functionality of DASOPN by providing a mechanism for associating a
C     type with the data in the DAS file. The use of this entry over
C     DASOPN is highly recommended.
C
C     Since the only reason for creating a new file is to write
C     something in it, all new files are opened for write access.
C
C     Entry point DASOPN, for opening a new DAS file, has been rendered
C     obsolete by the new entry point DASONW. The entry point DASOPN
C     will continue to be supported for purposes of backward
C     compatibility, but its use in new software development is strongly
C     discouraged.
C
C     Entry point DASOPS creates a new scratch DAS file. As with new
C     permanent files, these files are opened for write access.  DAS
C     files opened by DASOPS are automatically deleted when they are
C     closed.
C
C     Entry point DASLLC is used by DASCLS ( DAS, close file ) to close
C     an open DAS file and update DASFM's bookkeeping information
C     accordingly. DASCLS provides the only official means of closing
C     a DAS file that is currently open. Closing a DAS file any other
C     way (for example, by determining its logical unit and using the
C     Fortran CLOSE statement directly) may affect your calling program
C     in mysterious ways. Normally, DASLLC should not be called by
C     non-SPICELIB routines; these should call DASCLS instead.
C
C     Entry point DASHFS allows you to obtain a file summary for any
C     DAS file that is currently open, without calling DASRFR to
C     re-read the file record. Entry point DASUFS can be used to
C     update a file summary at run-time. Normally, there is no
C     need for routines outside of SPICELIB to modify a DAS file's
C     summary.
C
C     Entry point DASHAM allows you to determine which access method
C     a DAS file has been opened for.
C
C     Entry point DASHOF allows you to determine which DAS files are
C     open at any time. In particular, you can use DASHOF to determine
C     whether any file handle points to an open DAS file.
C
C     Entry point DASSIH signals errors when it is supplied with invalid
C     handles, so it serves to centralize error handling associated
C     with invalid handles.
C
C     The remaining entry points exist mainly to translate between
C     alternative representations of DAS files. There are three ways to
C     identify any open DAS file: by name, by handle, and by logical
C     unit. Given any one of these, you may use these entry points to
C     find the other two.
C
C$ Examples
C
C     See $Examples in entry points DASOPR, DASOPW, DASONW,
C     DASOPN (Obsolete), DASLLC, DASHFS, DASUFS, DASHLU, DASLUH,
C     DASHFN, DASFNH, DASHAM, DASHOF, and DASSIH for examples specific
C     to those entry points.
C
C$ Restrictions
C
C     1)  The value of parameter RECL may need to be changed when DASFM
C         and its entry points are ported to a new environment (CPU and
C         compiler).
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
C     H.A. Neilan        (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C     I.M. Underwood     (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.1.0, 28-NOV-2021 (BVS)
C
C        Updated for MAC-OSX-M1-64BIT-CLANG_C.
C
C-    SPICELIB Version 8.0.1, 24-AUG-2021 (JDR)
C
C        Edited the header of the DASFM umbrella routine and all its
C        entry points.
C
C        Removed non-existent argument "SUM" from the umbrella's
C        $Brief_I/O section.
C
C-    SPICELIB Version 8.0.0, 06-APR-2016 (NJB)
C
C        Corrected call to ZZDDHCLS in DASUFS.
C
C        26-FEB-2015 (NJB)
C
C           Now uses DAF/DAS handle manager subsystem.
C
C           FTSIZE is now 5000.
C
C           Corrected miscellaneous spelling errors in comments
C           throughout this file.
C
C-    SPICELIB Version 7.24.0, 10-APR-2014 (NJB)
C
C        Added initializers for file table arrays. This was done
C        to suppress compiler warnings. Deleted declaration of
C        unused parameter BWDLOC.
C
C        Corrected header comments in entry point DASLLC: routine that
C        flushes written, buffered records is DASWBR, not DASWUR.
C
C-    SPICELIB Version 7.23.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 7.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 7.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 7.20.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 7.19.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 7.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 7.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 7.16.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 7.15.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 7.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 7.13.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 7.12.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 7.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 7.10.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 7.9.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 7.8.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 7.7.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 7.6.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 7.5.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 7.4.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 7.3.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 7.2.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 7.1.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
C
C        Error handling for non-native files was added to
C        entry points DASOPR and DASOPW.
C
C        Bug in code for constructing long error message in entry
C        point DASUFS was corrected.
C
C-    SPICELIB Version 6.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 6.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 6.0.3, 24-APR-2003 (EDW)
C
C        Added MAC-OSX-F77 to the list of platforms
C        that require READONLY to read write protected
C        kernels.
C
C-    SPICELIB Version 6.0.2, 21-FEB-2003 (NJB)
C
C        Corrected inline comment in DASLLC: determination of
C        whether file is open is done by searching the handle column of
C        the file table, not the unit column.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.0.0, 11-DEC-2001 (NJB) (FST)
C
C        To accommodate future updates to the DAS system, including
C        integration with the handle manager and FTP validation
C        checks, the following entry points were modified:
C
C           DASONW, DASOPN
C
C        See their headers and code for the details of the changes.
C
C        Bug fix: removed local buffering of the DAS file ID word
C        and the internal file name, as this was causing DASWFR
C        to exhibit improper behavior.
C
C        Bug fix: missing call to CHKIN was added to an error
C        handling branch in entry point DASUFS. This call is
C        required because DASUFS uses discovery check-in.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 5.0.0, 05-APR-1998 (NJB)
C
C        Added references to the PC-LINUX environment. Repaired some
C        format errors involving placement of comment markers in
C        column 1.
C
C-    SPICELIB Version 4.0.1, 19-DEC-1995 (NJB)
C
C        Added permuted index entry section.
C
C-    SPICELIB Version 4.0.0, 31-AUG-1995 (NJB)
C
C        Changed argument list of the entry point DASONW. The input
C        argument NCOMR, which indicates the number of comment records
C        to reserve, was added to the argument list.
C
C-    SPICELIB Version 3.1.0, 05-JAN-1995 (HAN)
C
C        Removed Sun Solaris environment since it is now the same
C        as the Sun OS 4.1.x environment.
C        Removed DEC Alpha/OpenVMS environment since it is now the same
C        as the VAX environment.
C        Entry points affected are: DASFM, DASOPR.
C
C-    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG)
C
C        Modified the umbrella routine DASFM to allow the inclusion of
C        a file type in the creation and manipulation of DAS files.
C
C-    SPICELIB Version 2.0.0, 11-APR-1994 (HAN)
C
C        Updated module to include values for the Silicon Graphics/IRIX,
C        DEC Alpha-OSF/1, and Next/Absoft Fortran platforms. Entry
C        points affected are: DASFM, DASOPR.
C
C-    SPICELIB Version 1.0.0, 15-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     manage open DAS files
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
C
C        Error handling for non-native files was added to
C        entry points DASOPR and DASOPW.
C
C        Bug in code for constructing long error message in entry
C        point DASUFS was corrected.
C
C        Local variable DAS was renamed to DASFIL in DASSIH.
C
C-    SPICELIB Version 6.0.0, 11-DEC-2001 (NJB) (FST)
C
C        Binary File Format Identification:
C
C        The file record now contains an 8 character string that
C        identifies the binary file format utilized by DAS files.
C        The purpose of this string's inclusion in the file record
C        is preparatory in nature, to accelerate the migration to
C        files that support the runtime translation update that
C        is scheduled.
C
C        FTP Validation:
C
C        The file record now contains a sequence of characters
C        commonly corrupted by improper FTP transfers. These
C        characters will be examined by the handle manager when
C        existing files are opened.
C
C        FTIDW and FTIFN have been removed from the elements of
C        the DAS file table. Their presence and use in DASUFS
C        was causing DASWFR difficulties in updating the internal
C        filename under situations where changes to the comment and
C        reserved record parameters in the file record were updated.
C        This change effects DASOPR, DASOPN, DASONW, DASOPW, and
C        DASUFS.
C
C-    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG)
C
C        Modified the umbrella routine DASFM to allow the inclusion of
C        a file type in the creation and manipulation of DAS files. In
C        particular, the following changes were made:
C
C           1) Added variable FTYPE to the SUBROUTINE declaration, and
C              added appropriate entries for this variable in the
C              $Brief_I/O and $Detailed_Input sections of the header.
C
C           2) Removed erroneous references to OPC from the $Brief_I/O
C              section.
C
C           3) Added a new entry point, DASONW, which will support the
C              ability to associate a data type with a new DAS file
C              when it is created. The addition of this new entry point
C              makes the entry point DASOPN obsolete.
C
C           4) Added a description of the new entry point DASONW to the
C              $Particulars section. Also added a statement that the
C              entry point DASOPN has been made obsolete by this new
C              entry point, and its use in new code development is
C              discouraged.
C
C           5) Added a new variable to the file table, FTIDW, which
C              will be used to store the ID words from successfully
C              opened DAS files. We need to maintain this information
C              when writing the file record, as we do not want to
C              modify the ID word in the file.
C
C           6) Removed the parameter DASID as it is no longer needed.
C
C           7) Added new variables TARCH and TTYPE for temporary
C              storage of the file architecture and type. Also added a
C              new variable FNB for storing the position of the first
C              nonblank in a string.
C
C           8) Added new parameters:
C
C                 ARCLEN The maximum length of a file architecture
C                 TYPLEN The maximum length of a file type
C                 MAXPC  Decimal value for the upper limit of printable
C                        ASCII characters.
C                 MINPC  Decimal value for the lower limit of printable
C                        ASCII characters.
C
C           9) Modified entry points which open DAS files: OPR, OPW,
C              OPS, OPN, ONW to support the new file ID word format.
C
C          10) Made all occurrences of error message formatting of
C              filenames consistent. All filenames will be single
C              quoted in output error messages.
C
C          11) Added a test for a blank filename before the inquire
C              to obtain information about a file in the entry points:
C              DASOPR, DASOPW, DASONW, and DASOPN.
C
C          12) Modified the description of FTSIZE in the $Parameters
C              section to reflect the possibility of overflowing the
C              DAS file table when at least one DAS file had been
C              opened with write access.
C
C              The problem occurs when the file table is full, the
C              number of open DAS files equals FTSIZE, and at least one
C              of the open files was opened with write access. If an
C              attempt to close a file opened with write access is made
C              under these conditions, by calling DASCLS, it will fail.
C              DASCLS (via DASSDR) calls DASOPS to open a scratch DAS
C              file, but the scratch file CANNOT be opened because the
C              file table is full. If this occurs, close a file open
C              for read access, or restrict the number of open files
C              in use to be at most FTSIZE - 1 when there will be at
C              least one file opened with write access.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               LNKNFN
      INTEGER               LNKNXT
      INTEGER               LTRIM
      INTEGER               RTRIM
 
      LOGICAL               ELEMI
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN =   10 )
 
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN =    8 )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN =   60 )
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL =   -5 )
 
      INTEGER               MAXPC
      PARAMETER           ( MAXPC  =  126 )
 
      INTEGER               MINPC
      PARAMETER           ( MINPC  =   32 )
 
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN =    4 )
C
C     Access method parameters:
C
      INTEGER               READ
      PARAMETER           ( READ   =          1 )
 
      INTEGER               WRITE
      PARAMETER           ( WRITE  =  READ  + 1 )
 
C
C     File summary parameters:
C
C        A DAS file summary has the following structure:
C
C           +----------------------------------------+
C           | <number of reserved records>           |
C           +----------------------------------------+
C           | <number of characters in r.r. area>    |
C           +----------------------------------------+
C           | <number of comment records>            |
C           +----------------------------------------+
C           | <number of characters in comment area> |
C           +----------------------------------------+
C           | <first free record number>             |
C           +----------------------------------------+
C           | <last character logical address>       |
C           +----------------------------------------+
C           | <last d.p. logical address>            |
C           +----------------------------------------+
C           | <last integer logical address>         |
C           +----------------------------------------+
C           | <last character descriptor record>     |
C           +----------------------------------------+
C           | <last d.p. descriptor record>          |
C           +----------------------------------------+
C           | <last integer descriptor record>       |
C           +----------------------------------------+
C           | <last character descriptor word>       |
C           +----------------------------------------+
C           | <last d.p. descriptor word>            |
C           +----------------------------------------+
C           | <last integer descriptor word>         |
C           +----------------------------------------+
C
 
      INTEGER               RRCIDX
      PARAMETER           ( RRCIDX  =           1 )
 
      INTEGER               RCHIDX
      PARAMETER           ( RCHIDX  =  RRCIDX + 1 )
 
      INTEGER               CRCIDX
      PARAMETER           ( CRCIDX  =  RCHIDX + 1 )
 
      INTEGER               CCHIDX
      PARAMETER           ( CCHIDX  =  CRCIDX + 1 )
 
      INTEGER               FREIDX
      PARAMETER           ( FREIDX =   CCHIDX + 1 )
 
C
C     Base indices for:
C
C        -- last logical addresses
C        -- records containing last descriptor for a given type
C        -- word containing last descriptor for a given type
C
C     The offset into the file summary for any of these items
C     is obtained by adding the appropriate data type parameter
C     (DP, INT, or CHAR) to the base index for the item.
C
      INTEGER               LLABAS
      PARAMETER           ( LLABAS =   FREIDX     )
 
      INTEGER               LRCBAS
      PARAMETER           ( LRCBAS =   LLABAS + 3 )
 
      INTEGER               LWDBAS
      PARAMETER           ( LWDBAS =   LRCBAS + 3 )
 
      INTEGER               SUMSIZ
      PARAMETER           ( SUMSIZ =   LWDBAS + 3 )
 
C
C     Descriptor record pointer locations (within descriptor records):
C
      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC =  2 )
 
C
C     Directory address range location parameters:
C
      INTEGER               RNGBAS
      PARAMETER           ( RNGBAS =   2 )
 
C
C     First descriptor position in descriptor record:
C
      INTEGER               BEGDSC
      PARAMETER           ( BEGDSC =   9 )
 
C
C     Length of the Binary File Format string:
C
      INTEGER               FMTLEN
      PARAMETER           ( FMTLEN = 8 )
 
C
C     The parameter TAILEN determines the tail length of a DAS file
C     record.  This is the number of bytes (characters) that
C     occupy the portion of the file record that follows the
C     integer holding the first free address.  For environments
C     with a 32 bit word length, 1 byte characters, and DAS
C     record sizes of 1024 bytes, we have:
C
C           8 bytes - IDWORD
C          60 bytes - IFNAME
C           4 bytes - NRESVR (32 bit integer)
C           4 bytes - NRESVC (32 bit integer)
C           4 bytes - NCOMR  (32 bit integer)
C         + 4 bytes - NCOMC  (32 bit integer)
C          ---------
C          84 bytes - (All file records utilize this space.)
C
C     So the size of the remaining portion (or tail) of the DAS
C     file record for computing environments as described above
C     would be:
C
C        1024 bytes - DAS record size
C      -    8 bytes - DAS Binary File Format Word
C      -   84 bytes - (from above)
C       ------------
C         932 bytes - DAS file record tail length
C
C     Note: environments that do not have a 32 bit word length,
C     1 byte characters, and a DAS record size of 1024 bytes, will
C     require the adjustment of this parameter.
C
      INTEGER               TAILEN
      PARAMETER           ( TAILEN = 932 )
 
C
C     Local variables
C
 
C
C     The file table consists of a set of arrays which serve as
C     `columns' of the table.  The sets of elements having the same
C     index in the arrays form the `rows' of the table.  Each column
C     contains a particular type of information; each row contains
C     all of the information pertaining to a particular DAS file.
C
C     All column names in the file table begin with `FT'.  The
C     columns are:
C
C        HAN      Handle
C        ACC      Access method
C        LNK      Number of links
C        SUM      File summary
C
C     The rows of the file table are indexed by a doubly linked
C     list pool.  The pool contains an active list and a free list.
C     when a file is opened, a pointer to the file (the pointer
C     is called a `node').  it is placed at the head of the active
C     list; when a file is closed, the node in the active list that
C     pointed to the file is placed on the free list.
C
C     NEXT is incremented each time a file is opened to become the
C     next file handle assigned.
C
      INTEGER               FTHAN  (         FTSIZE )
      INTEGER               FTACC  (         FTSIZE )
      INTEGER               FTLNK  (         FTSIZE )
      INTEGER               FTSUM  ( SUMSIZ, FTSIZE )
 
      INTEGER               POOL   ( 2,  LBPOOL : FTSIZE )
 
C
C     FTHEAD is a pointer to the head of the active file list.
C
      INTEGER               FTHEAD
 
C
C     NEXT and PREV map the DAS data type codes to their
C     successors and predecessors, respectively.
C
      INTEGER               NEXT   ( 3 )
      INTEGER               PREV   ( 3 )
 
C
C     Other local variables
C
      CHARACTER*(ACCLEN)    ACC
      CHARACTER*(FMTLEN)    FORMAT
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(FILEN)     LOCDAS
      CHARACTER*(IFNLEN)    LOCIFN
      CHARACTER*(FMTLEN)    LOCFMT
      CHARACTER*(TAILEN)    TAIL
      CHARACTER*(TYPLEN)    TTYPE
 
      INTEGER               CURTYP
      INTEGER               DIRREC ( NWI )
      INTEGER               DSCTYP
      INTEGER               ENDREC
      INTEGER               FNB
      INTEGER               FHLIST ( LBCELL : FTSIZE )
      INTEGER               FINDEX
      INTEGER               I
      INTEGER               INQSTA
      INTEGER               IOSTAT
      INTEGER               LAST
      INTEGER               LDREC  ( 3 )
      INTEGER               LDRMAX
      INTEGER               LOC
      INTEGER               LOCCCH
      INTEGER               LOCCRC
      INTEGER               LOCRRC
      INTEGER               LOCRCH
      INTEGER               MAXADR
      INTEGER               NEW
      INTEGER               NREC
      INTEGER               NUMBER
      INTEGER               NW     ( 3 )
      INTEGER               NXTDIR
      INTEGER               NXTREC
      INTEGER               POS
      INTEGER               PRVTYP
      INTEGER               TYPE
 
      LOGICAL               FOUND
      LOGICAL               PASS1
 
C
C     Save everything between calls.
C
      SAVE
 
C
C     Initial values
C
      DATA                  FTACC   / FTSIZE * -1    /
      DATA                  FTHAN   / FTSIZE *  0    /
      DATA                  FTLNK   / FTSIZE * -1    /
 
      DATA                  PASS1   / .TRUE.         /
      DATA                  FTHEAD  / 0              /
 
      DATA                  NEXT    /  2,   3,   1   /
      DATA                  PREV    /  3,   1,   2   /
      DATA                  NW      /  NWC, NWD, NWI /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'DASFM' )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
         CALL CHKOUT ( 'DASFM' )
      END IF
 
      RETURN
 
 
 
 
C$Procedure DASOPR ( DAS, open for read )
 
      ENTRY DASOPR ( FNAME, HANDLE )
 
C$ Abstract
C
C     Open a DAS file for reading.
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
C     DAS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of a DAS file to be opened.
C     HANDLE     O   Handle assigned to the opened DAS file.
C
C$ Detailed_Input
C
C     FNAME    is the name of a DAS file to be opened with read
C              access.
C
C$ Detailed_Output
C
C     HANDLE   is the handle that is  associated with the file. This
C              handle is used to identify the file in subsequent
C              calls to other DAS routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input filename is blank, the error
C         SPICE(BLANKFILENAME) is signaled.
C
C     2)  If the specified file does not exist, the error
C         SPICE(FILENOTFOUND) is signaled.
C
C     3)  If the specified file has already been opened for read
C         access, the handle already associated with the file is
C         returned.
C
C     4)  If the specified file has already been opened for write
C         access, the error SPICE(DASRWCONFLICT) is signaled.
C
C     5)  If the specified file has already been opened by a non-DAS
C         routine, the error SPICE(DASIMPROPOPEN) is signaled.
C
C     6)  If the specified file cannot be opened without exceeding
C         the maximum allowed number of open DAS files, the error
C         SPICE(DASFTFULL) is signaled.
C
C     7)  If the named file cannot be opened properly, an error is
C         signaled by a routine in the call tree of this routine.
C
C     8)  If the file record cannot be read, the error
C         SPICE(FILEREADFAILED) is signaled.
C
C     9)  If the specified file is not a DAS file, as indicated by the
C         file's ID word, an error is signaled by a routine in the call
C         tree of this routine.
C
C     10) If no logical units are available, an error is signaled
C         by a routine in the call tree of this routine.
C
C$ Files
C
C     See argument FNAME.
C
C$ Particulars
C
C     Most DAS files require only read access. If you do not need to
C     change the contents of a file, you should open it using DASOPR.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Dump several parameters from the first DLA segment of a DSK
C        file. Note that DSK files are based on DAS. The segment is
C        assumed to be of type 2.
C
C        Example code begins here.
C
C
C              PROGRAM DASOPR_EX1
C              IMPLICIT NONE
C
C              INCLUDE 'dla.inc'
C              INCLUDE 'dskdsc.inc'
C              INCLUDE 'dsk02.inc'
C
C        C
C        C     Local parameters
C        C
C              INTEGER               FILSIZ
C              PARAMETER           ( FILSIZ = 255 )
C
C              INTEGER               LNSIZE
C              PARAMETER           ( LNSIZE = 80 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(FILSIZ)    DSK
C              CHARACTER*(LNSIZE)    OUTLIN
C
C              DOUBLE PRECISION      VOXORI ( 3 )
C              DOUBLE PRECISION      VOXSIZ
C              DOUBLE PRECISION      VTXBDS ( 2, 3 )
C
C              INTEGER               CGSCAL
C              INTEGER               DLADSC ( DLADSZ )
C              INTEGER               HANDLE
C              INTEGER               NP
C              INTEGER               NV
C              INTEGER               NVXTOT
C              INTEGER               VGREXT ( 3 )
C              INTEGER               VOXNPL
C              INTEGER               VOXNPT
C              INTEGER               VTXNPL
C
C              LOGICAL               FOUND
C
C
C        C
C        C     Prompt for the name of the DSK to read.
C        C
C              CALL PROMPT ( 'Enter DSK name > ', DSK )
C        C
C        C     Open the DSK file for read access.
C        C     We use the DAS-level interface for
C        C     this function.
C        C
C              CALL DASOPR ( DSK, HANDLE )
C
C        C
C        C     Begin a forward search through the
C        C     kernel, treating the file as a DLA.
C        C     In this example, it's a very short
C        C     search.
C        C
C              CALL DLABFS ( HANDLE, DLADSC, FOUND )
C
C              IF ( .NOT. FOUND ) THEN
C        C
C        C        We arrive here only if the kernel
C        C        contains no segments.  This is
C        C        unexpected, but we're prepared for it.
C        C
C                 CALL SETMSG ( 'No segments found '
C             .   //            'in DSK file #.'    )
C                 CALL ERRCH  ( '#',  DSK           )
C                 CALL SIGERR ( 'SPICE(NODATA)'     )
C
C              END IF
C
C        C
C        C     If we made it this far, DLADSC is the
C        C     DLA descriptor of the first segment.
C        C
C        C     Read and display type 2 bookkeeping data.
C        C
C              CALL DSKB02 ( HANDLE, DLADSC, NV,     NP,     NVXTOT,
C             .              VTXBDS, VOXSIZ, VOXORI, VGREXT, CGSCAL,
C             .              VTXNPL, VOXNPT, VOXNPL                 )
C
C        C
C        C     Show vertex and plate counts.
C        C
C              OUTLIN = 'Number of vertices:                 #'
C              CALL REPMI  ( OUTLIN, '#', NV, OUTLIN )
C              CALL TOSTDO ( OUTLIN )
C
C              OUTLIN = 'Number of plates:                   #'
C              CALL REPMI  ( OUTLIN, '#', NP, OUTLIN )
C              CALL TOSTDO ( OUTLIN )
C
C              OUTLIN = 'Voxel edge length (km):             #'
C              CALL REPMF  ( OUTLIN, '#', VOXSIZ, 6, 'E', OUTLIN )
C              CALL TOSTDO ( OUTLIN )
C
C              OUTLIN = 'Number of voxels:                   #'
C              CALL REPMI  ( OUTLIN, '#', NVXTOT, OUTLIN )
C              CALL TOSTDO ( OUTLIN )
C
C        C
C        C     Close the kernel.  This isn't necessary in a stand-
C        C     alone program, but it's good practice in subroutines
C        C     because it frees program and system resources.
C        C
C              CALL DASCLS ( HANDLE )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, using the DSK file named phobos512.bds, the output
C        was:
C
C
C        Enter DSK name > phobos512.bds
C        Number of vertices:                 1579014
C        Number of plates:                   3145728
C        Voxel edge length (km):             1.04248E-01
C        Number of voxels:                   11914500
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     H.A. Neilan        (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.0.1, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C        Added code example using example from DSKB02.
C
C        Updated $Exceptions entries #7 and #9: error handling for the
C        DAS open failure and "file is not a DAS file" cases is now
C        performed by lower level code (ZZDDHOPN).
C
C-    SPICELIB Version 8.0.0, 30-JUL-2014 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C
C-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
C
C        Error handling for non-native files was added.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.0.0, 14-DEC-2001 (FST)
C
C        The DAS file ID word and internal file name are no longer
C        buffered by this routine. See DASFM's $Revisions section
C        for details.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG)
C
C        Modified the entry point to use the new file ID format which
C        contains a mnemonic code for the data type. Added error
C        checks on file names. Fixed bug involving use of sign of
C        file handles. Improved some error messages. (delete rest)
C
C-    SPICELIB Version 2.0.0, 11-APR-1994 (HAN)
C
C        Updated module to include values for the Silicon Graphics/IRIX,
C        DEC Alpha-OSF/1, and Next/Absoft Fortran platforms. Entry
C        points affected are: DASFM, DASOPR.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     open a DAS file for reading
C     open a DAS file for read access
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 8.0.0, 30-JUL-2014 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C
C-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
C
C        Error handling for non-native files was added.
C
C-    SPICELIB Version 3.0.1, 24-APR-2003 (EDW)
C
C        Added MAC-OSX-F77 to the list of platforms
C        that require READONLY to read write protected
C        kernels.
C
C-    SPICELIB Version 3.0.0, 15-JUN-1994 (KRG)
C
C        Modified the entry point to use the new file ID format which
C        contains a mnemonic code for the data type.
C
C        Split an IF ... ELSE IF ... statement into 2 IF statements of
C        equivalent behavior to allow testing of the file architecture.
C
C        Added code to test the file architecture and to verify that the
C        file is a DAS file.
C
C        Removed the error SPICE(DASNOIDWORD) as it was no longer
C        relevant.
C
C        Added the error SPICE(NOTADASFILE) if this routine is called
C        with a file that does not contain an ID word identifying the
C        file as a DAS file.
C
C        Added a test for a blank filename before attempting to use the
C        filename in the routine. If the filename is blank, the error
C        SPICE(BLANKFILENAME) will be signaled.
C
C        Fixed a bug when dealing with a read/write open conflict for
C        DAS files: the code used the DAF positive/negative handle
C        method to determine read/write access rather than the DAS file
C        table column FTACC. Replaced the code:
C
C           IF ( FTHAN(FINDEX) .LT. 0 ) THEN
C
C        with
C
C           IF ( FTACC(FINDEX) .EQ. WRITE ) THEN
C
C        Changed the long error message when the error
C        SPICE(NOTADASFILE) is signaled to suggest that a common error
C        is attempting to use a text version of the desired file rather
C        than the binary version.
C
C-    SPICELIB Version 2.0.0, 11-APR-1994 (HAN)
C
C        Updated module to include values for the Silicon Graphics/IRIX,
C        DEC Alpha-OSF/1, and Next/Absoft Fortran platforms. Entry
C        points affected are: DASFM, DASOPR.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DASOPR' )
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
C
C     Try to open the file for READ access.
C
C
C        The file may or may not already be open. If so, it should have
C        not been opened for writing (FTACC .EQ. WRITE). If opened for
C        reading, ZZDDHOPN will just increment the number of links and
C        return the handle.
C
      CALL ZZDDHOPN ( FNAME, 'READ', 'DAS', HANDLE )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASOPR' )
         RETURN
      END IF
 
C
C     Determine whether this file is already in the file table.
C     Search for a matching handle.
C
      FINDEX = FTHEAD
      FOUND  = .FALSE.
 
      DO WHILE (  ( .NOT. FOUND )  .AND.  ( FINDEX .GT. 0 )  )
 
         IF ( FTHAN(FINDEX) .EQ. HANDLE ) THEN
            FOUND = .TRUE.
         ELSE
            FINDEX = LNKNXT ( FINDEX, POOL )
         END IF
 
      END DO
 
      IF ( FOUND ) THEN
C
C        The file is already open for read access. Increment the number
C        of links to this file.
C
         FTLNK(FINDEX) = FTLNK(FINDEX) + 1
         HANDLE        = FTHAN(FINDEX)
C
C        There's nothing else to do.
C
         CALL CHKOUT ( 'DASOPR' )
         RETURN
 
      END IF
 
C
C     This file has no entry in the file table. We need to
C     create a new entry. See whether there's room for it.
C
      IF ( LNKNFN(POOL) .EQ. 0  ) THEN
 
         CALL SETMSG ( 'The file table is full, with # entries. '
     .   //            'Could not open ''#''.'                    )
         CALL ERRINT ( '#', FTSIZE                                )
         CALL ERRCH  ( '#', FNAME                                 )
         CALL SIGERR ( 'SPICE(DASFTFULL)'                         )
         CALL CHKOUT ( 'DASOPR'                                   )
         RETURN
 
      END IF
 
C
C     Read the file's file record.
C
      CALL ZZDASRFR ( HANDLE, IDWORD, LOCIFN, LOCRRC,
     .                LOCRCH, LOCCRC, LOCCCH         )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASOPR' )
         RETURN
      END IF
 
C
C     Update the file table to include information about our newly
C     opened DAS file. Link the information for this file at the
C     head of the file table list.
C
      CALL LNKAN  ( POOL, NEW          )
      CALL LNKILB ( NEW,  FTHEAD, POOL )
 
      FTHEAD           =   NEW
      FTHAN (FTHEAD)   =   HANDLE
      FTACC (FTHEAD)   =   READ
      FTLNK (FTHEAD)   =   1
 
C
C     Fill in the file summary. We already know how many reserved
C     records and comment records there are. To find the number of the
C     first free record, the last logical address of each type, and the
C     locations of the last descriptors of each type, we must examine
C     the directory records. We do not assume that the data records in
C     the DAS file have been segregated.
C
      CALL CLEARI ( SUMSIZ, FTSUM (1,FTHEAD) )
 
      FTSUM ( RRCIDX, FTHEAD )  =  LOCRRC
      FTSUM ( RCHIDX, FTHEAD )  =  LOCRCH
      FTSUM ( CRCIDX, FTHEAD )  =  LOCCRC
      FTSUM ( CCHIDX, FTHEAD )  =  LOCCCH
 
C
C     We'll find the values for each data type separately.
C
      DO TYPE = 1, 3
C
C        The first directory record is located right after the
C        last comment record.
C
         NREC  =  LOCRRC + LOCCRC + 2
 
C
C        Keep track of the record number of the last data
C        record of the current type.
C
         LDREC(TYPE)  =  0
 
C
C        Find the last directory containing a descriptor of a
C        record cluster of the current type.
C
         CALL ZZDASGRI( HANDLE, NREC, DIRREC )
 
         MAXADR  =  DIRREC ( RNGBAS  +  2 * TYPE )
         NXTDIR  =  DIRREC ( FWDLOC              )
 
 
         DO WHILE ( NXTDIR .GT. 0 )
C
C           Read the directory record. If this record contains
C           descriptors for clusters we're interested in, update the
C           directory record number.
C
            CALL ZZDASGRI ( HANDLE, NXTDIR, DIRREC )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASOPR' )
               RETURN
            END IF
 
 
            IF (  DIRREC(RNGBAS + 2*TYPE)  .GT.  0  ) THEN
 
               MAXADR  =  DIRREC ( RNGBAS  +  2 * TYPE  )
               NREC    =  NXTDIR
 
            END IF
 
            NXTDIR = DIRREC ( FWDLOC )
 
         END DO
 
C
C        At this point, NREC is the record number of the directory
C        containing the last descriptor for clusters of TYPE, if
C        there are any such descriptors.
C
C        MAXADR is the maximum logical address of TYPE.
C
         FTSUM ( LLABAS + TYPE,  FTHEAD )  =  MAXADR
 
         IF ( MAXADR .GT. 0 ) THEN
            FTSUM ( LRCBAS + TYPE,  FTHEAD )  =  NREC
         ELSE
            FTSUM ( LRCBAS + TYPE,  FTHEAD )  =  0
         END IF
C
C        We still need to set the word location of the final
C        descriptor of TYPE, if there are any descriptors of TYPE.
C
         IF ( MAXADR .GT. 0 ) THEN
C
C           Re-read the directory record containing the last descriptor
C           of the current type.
C
            CALL ZZDASGRI ( HANDLE, NREC, DIRREC )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASOPR' )
               RETURN
            END IF
 
C
C           Traverse the directory record, looking for the last
C           descriptor of TYPE. We'll keep track of the maximum logical
C           address of TYPE for each cluster of TYPE whose descriptor
C           we examine. When this value is the maximum logical address
C           of TYPE, we've found the last descriptor of TYPE.
C
C           Also keep track of the end record numbers for each
C           cluster.
C
            LAST    =  DIRREC( RNGBAS  +  (2*TYPE - 1)  )  -  1
            DSCTYP  =  DIRREC( BEGDSC                   )
            PRVTYP  =  PREV  ( DSCTYP                   )
            ENDREC  =  NREC
            POS     =  BEGDSC
 
            DO WHILE ( LAST .LT. MAXADR )
 
               POS = POS + 1
 
               IF ( DIRREC(POS) .GT. 0 ) THEN
                  CURTYP = NEXT(PRVTYP)
               ELSE
                  CURTYP = PREV(PRVTYP)
               END IF
 
               IF ( CURTYP .EQ. TYPE ) THEN
                  LAST = LAST  +  NW(TYPE) * ABS( DIRREC(POS) )
               END IF
 
               ENDREC = ENDREC + ABS( DIRREC(POS) )
               PRVTYP = CURTYP
 
            END DO
C
C           At this point, POS is the word position of the last
C           descriptor of TYPE, and ENDREC is the record number
C           of the last data record of TYPE.
C
            FTSUM ( LWDBAS + TYPE, FTHEAD )  =  POS
            LDREC ( TYPE )                   =  ENDREC
 
 
         ELSE
C
C           There's no data of TYPE in the file.
C
            FTSUM ( LWDBAS + TYPE, FTHEAD )  =  0
            LDREC ( TYPE )                   =  0
 
         END IF
 
      END DO
 
C
C     We're almost done; we need to find the number of the first free
C     record. This record follows all of the data records and all of
C     the directory records. It may happen that the last record in use
C     is an empty directory.
C
      CALL MAXAI ( LDREC, 3, LDRMAX, LOC )
 
      NREC   = LOCRRC + LOCCRC + 2
 
      CALL ZZDASGRI ( HANDLE,  NREC,  DIRREC  )
 
      NXTREC = DIRREC(FWDLOC)
 
 
      DO WHILE ( NXTREC .NE. 0 )
 
         NREC   = NXTREC
 
         CALL ZZDASGRI ( HANDLE,  NREC,  DIRREC  )
 
         NXTREC = DIRREC(FWDLOC)
 
      END DO
 
C
C     Now NREC is the last directory record.
C
      FTSUM ( FREIDX, FTHEAD)   =   MAX ( LDRMAX, NREC )  +  1
 
C
C     Insert the new handle into our handle set.
C
      CALL INSRTI ( HANDLE, FHLIST )
 
      CALL CHKOUT ( 'DASOPR' )
      RETURN
 
 
 
 
C$Procedure DASOPW ( DAS, open for write )
 
      ENTRY DASOPW ( FNAME, HANDLE )
 
C$ Abstract
C
C     Open a DAS file for writing.
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
C     DAS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of a DAS file to be opened.
C     HANDLE     O   Handle assigned to the opened DAS file.
C
C$ Detailed_Input
C
C     FNAME    is the name of a DAS file to be opened with write
C              access.
C
C$ Detailed_Output
C
C     HANDLE   is the handle that is associated with the file. This
C              handle is used to identify the file in subsequent
C              calls to other DAS routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input filename is blank, an error is signaled by a
C         routine in the call tree of this routine.
C
C     2)  If the specified file does not exist, an error is signaled by
C         a routine in the call tree of this routine.
C
C     3)  If the specified file has already been opened, either by the
C         DAS file routines or by other code, an error is signaled by a
C         routine in the call tree of this routine. Note that this
C         response is not paralleled by DASOPR, which allows you to open
C         a DAS file for reading even if it is already open for reading.
C
C     4)  If the specified file cannot be opened without exceeding
C         the maximum allowed number of open DAS files, the error
C         SPICE(DASFTFULL) is signaled.
C
C     5)  If the specified file cannot be opened properly, an error
C         is signaled by a routine in the call tree of this routine.
C
C     6)  If the file record cannot be read, an error is signaled by a
C         routine in the call tree of this routine.
C
C     7)  If the specified file is not a DAS file, as indicated by the
C         file's ID word, an error is signaled by a routine in the call
C         tree of this routine.
C
C     8)  If no logical units are available, an error is signaled
C         by a routine in the call tree of this routine.
C
C$ Files
C
C     See argument FNAME.
C
C$ Particulars
C
C     Most DAS files require only read access. If you do not need to
C     change the contents of a file, you should open it with DASOPR.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a new DAS file containing 200 integer addresses set
C        to zero. Re-open the file for write access again, and write
C        to its addresses 1 through 200 in random-access fashion by
C        updating the file.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASOPW_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local Parameters
C        C
C              CHARACTER*(*)         DASNAM
C              PARAMETER           ( DASNAM   = 'dasopw_ex1.das' )
C
C              INTEGER               IDATLN
C              PARAMETER           ( IDATLN = 200 )
C
C              INTEGER               TYPELN
C              PARAMETER           ( TYPELN = 4   )
C
C        C
C        C     Local Variables
C        C
C              CHARACTER*(TYPELN)    TYPE
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               IDATA   ( IDATLN )
C              INTEGER               J
C
C        C
C        C     Open a new DAS file. Reserve no comment records.
C        C
C              TYPE = 'TEST'
C              CALL DASONW ( DASNAM, TYPE,  'TEST/DASOPW_EX1',
C             .              0,      HANDLE                  )
C
C        C
C        C     Append 200 integers to the file; after the data are
C        C     present, we're free to update it in any order we
C        C     please. (CLEARI zeros out an integer array.)
C        C
C              CALL CLEARI (         IDATLN, IDATA )
C              CALL DASADI ( HANDLE, IDATLN, IDATA )
C
C        C
C        C     Close the file.
C        C
C              CALL DASCLS ( HANDLE )
C
C        C
C        C     Open the file again for writing.
C        C
C              CALL DASOPW ( DASNAM, HANDLE )
C
C        C
C        C     Reset the data array, and read the data into it.
C        C
C              CALL FILLI  ( -1, IDATLN, IDATA )
C              CALL DASRDI ( HANDLE,  1, IDATLN, IDATA )
C
C        C
C        C     Print the contents of the file before updating it.
C        C
C              WRITE(*,*) 'Contents of ' // DASNAM // ' before update:'
C              WRITE(*,*) ' '
C              DO I = 1, 20
C                 WRITE (*,'(10I5)') (IDATA((I-1)*10+J), J = 1, 10)
C              END DO
C
C        C
C        C     Now the integer logical addresses 1:200 can be
C        C     written to in random-access fashion. We'll fill them
C        C     in reverse order.
C        C
C              DO I = IDATLN, 1, -1
C                 CALL DASUDI ( HANDLE, I, I, I )
C              END DO
C
C        C
C        C     Close the file.
C        C
C              CALL DASCLS ( HANDLE )
C
C        C
C        C     Now make sure that we updated the file properly.
C        C     Open the file for reading and dump the contents
C        C     of the integer logical addresses 1:200.
C        C
C              CALL DASOPR ( DASNAM, HANDLE )
C
C              CALL CLEARI (             IDATLN, IDATA )
C              CALL DASRDI ( HANDLE,  1, IDATLN, IDATA )
C
C              WRITE(*,*) ' '
C              WRITE(*,*) 'Contents of ' // DASNAM // ' after update:'
C              WRITE(*,*) ' '
C              DO I = 1, 20
C                 WRITE (*,'(10I5)') (IDATA((I-1)*10+J), J = 1, 10)
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Contents of dasopw_ex1.das before update:
C
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C            0    0    0    0    0    0    0    0    0    0
C
C         Contents of dasopw_ex1.das after update:
C
C            1    2    3    4    5    6    7    8    9   10
C           11   12   13   14   15   16   17   18   19   20
C           21   22   23   24   25   26   27   28   29   30
C           31   32   33   34   35   36   37   38   39   40
C           41   42   43   44   45   46   47   48   49   50
C           51   52   53   54   55   56   57   58   59   60
C           61   62   63   64   65   66   67   68   69   70
C           71   72   73   74   75   76   77   78   79   80
C           81   82   83   84   85   86   87   88   89   90
C           91   92   93   94   95   96   97   98   99  100
C          101  102  103  104  105  106  107  108  109  110
C          111  112  113  114  115  116  117  118  119  120
C          121  122  123  124  125  126  127  128  129  130
C          131  132  133  134  135  136  137  138  139  140
C          141  142  143  144  145  146  147  148  149  150
C          151  152  153  154  155  156  157  158  159  160
C          161  162  163  164  165  166  167  168  169  170
C          171  172  173  174  175  176  177  178  179  180
C          181  182  183  184  185  186  187  188  189  190
C          191  192  193  194  195  196  197  198  199  200
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
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 8.0.1, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C        Updated $Exceptions #3 -- short error message is
C        SPICE(FILEOPENCONFLICT) -- #5 and #7 -- error handling for
C        the DAS open failure and "file is not a DAS file" cases is
C        now performed by lower level code (ZZDDHOPN).
C
C-    SPICELIB Version 8.0.0, 30-JUL-2014 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C
C-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
C
C        Error handling for non-native files was added.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.0.0, 14-DEC-2001 (FST)
C
C        The DAS file ID word and internal file name are no longer
C        buffered by this routine. See DASFM's $Revisions section
C        for details.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG)
C
C        Modified the entry point to use the new file ID format which
C        contains a mnemonic code for the data type.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     open a DAS file for writing
C     open a DAS file for write access
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 8.0.0, 30-JUL-2014 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C
C-    SPICELIB Version 7.0.0, 28-SEP-2005 (NJB)
C
C        Error handling for non-native files was added.
C
C-    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG)
C
C        Modified the entry point to use the new file ID format which
C        contains a mnemonic code for the data type.
C
C        Split an IF ... ELSE IF ... statement into 2 IF statements of
C        equivalent behavior to allow testing of the file architecture.
C
C        Added code to test the file architecture and to verify that the
C        file is a DAS file.
C
C        Removed the error SPICE(DASNOIDWORD) as it was no longer
C        relevant.
C
C        Added the error SPICE(NOTADASFILE) if this routine is called
C        with a file that does not contain an ID word identifying the
C        file as a DAF file.
C
C        Added a test for a blank filename before attempting to use the
C        filename in the routine. If the filename is blank, the error
C        SPICE(BLANKFILENAME) will be signaled.
C
C        Changed the long error message when the error
C        SPICE(NOTADASFILE) is signaled to suggest that a common error
C        is attempting to load a text version of the desired file rather
C        than the binary version.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DASOPW' )
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
 
      IF ( LNKNFN(POOL) .EQ. 0  ) THEN
 
         CALL SETMSG ( 'The file table is full, with # entries. '     //
     .                 'Could not open ''#''.'                         )
         CALL ERRINT ( '#', FTSIZE                                     )
         CALL ERRCH  ( '#', FNAME                                      )
         CALL SIGERR ( 'SPICE(DASFTFULL)'                              )
         CALL CHKOUT ( 'DASOPW'                                        )
         RETURN
 
      END IF
 
C
C     Open the file for writing:   open the file, get the
C     internal file name, and set the number of links to one.
C
      CALL ZZDDHOPN ( FNAME, 'WRITE', 'DAS', HANDLE )
 
C
C     Read the file record.
C
      CALL ZZDASRFR ( HANDLE, IDWORD, LOCIFN, LOCRRC,
     .                LOCRCH, LOCCRC, LOCCCH         )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASOPW' )
         RETURN
      END IF
 
C
C     At this point, we know that we have a valid DAS file, and
C     we're set up to read from it, so ...
C
C     Update the file table to include information about
C     our newly opened DAS file. Link the information
C     for this file at the head of the file table list.
C
C     Set the output argument HANDLE as well.
C
      CALL LNKAN  ( POOL, NEW          )
      CALL LNKILB ( NEW,  FTHEAD, POOL )
 
      FTHEAD           =   NEW
      FTHAN (FTHEAD)   =   HANDLE
      FTACC (FTHEAD)   =   WRITE
      FTLNK (FTHEAD)   =   1
 
C
C     Fill in the file summary. We already know how many reserved
C     records and comment records there are. To find the number of the
C     first free record, the last logical address of each type, and the
C     locations of the last descriptors of each type, we must examine
C     the directory records. We do not assume that the data records in
C     the DAS file have been segregated.
C
      CALL CLEARI ( SUMSIZ, FTSUM (1,FTHEAD) )
 
      FTSUM ( RRCIDX, FTHEAD )  =  LOCRRC
      FTSUM ( RCHIDX, FTHEAD )  =  LOCRCH
      FTSUM ( CRCIDX, FTHEAD )  =  LOCCRC
      FTSUM ( CCHIDX, FTHEAD )  =  LOCCCH
 
C
C     We'll need the logical unit connected to the file, so
C     we can read the file's directory records.
C
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., NUMBER )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASOPW' )
         RETURN
      END IF
 
C
C     We'll find the values for each data type separately.
C
      DO TYPE = 1, 3
C
C        The first directory record is located right after the
C        last comment record.
C
         NREC  =  LOCRRC + LOCCRC + 2
 
C
C        Keep track of the record number of the last data
C        record of the current type.
C
         LDREC(TYPE)  =  0
 
C
C        Find the last directory containing a descriptor of a
C        record cluster of the current type.
C
         CALL ZZDASGRI( HANDLE, NREC, DIRREC )
 
         MAXADR  =  DIRREC ( RNGBAS  +  2 * TYPE )
         NXTDIR  =  DIRREC ( FWDLOC              )
 
 
         DO WHILE ( NXTDIR .GT. 0 )
C
C           Read the directory record. If this record contains
C           descriptors for clusters we're interested in, update the
C           directory record number.
C
            CALL ZZDASGRI ( HANDLE, NXTDIR, DIRREC )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASOPW' )
               RETURN
            END IF
 
 
            IF (  DIRREC(RNGBAS + 2*TYPE)  .GT.  0  ) THEN
 
               MAXADR  =  DIRREC ( RNGBAS  +  2 * TYPE  )
               NREC    =  NXTDIR
 
            END IF
 
            NXTDIR = DIRREC ( FWDLOC )
 
         END DO
 
C
C        At this point, NREC is the record number of the directory
C        containing the last descriptor for clusters of TYPE, if
C        there are any such descriptors.
C
C        MAXADR is the maximum logical address of TYPE.
C
         FTSUM ( LLABAS + TYPE,  FTHEAD )  =  MAXADR
 
         IF ( MAXADR .GT. 0 ) THEN
            FTSUM ( LRCBAS + TYPE,  FTHEAD )  =  NREC
         ELSE
            FTSUM ( LRCBAS + TYPE,  FTHEAD )  =  0
         END IF
C
C        We still need to set the word location of the final
C        descriptor of TYPE, if there are any descriptors of TYPE.
C
         IF ( MAXADR .GT. 0 ) THEN
C
C           Re-read the directory record containing the last descriptor
C           of the current type.
C
            CALL ZZDASGRI ( HANDLE, NREC, DIRREC )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASOPW' )
               RETURN
            END IF
 
C
C           Traverse the directory record, looking for the last
C           descriptor of TYPE. We'll keep track of the maximum logical
C           address of TYPE for each cluster of TYPE whose descriptor
C           we examine. When this value is the maximum logical address
C           of TYPE, we've found the last descriptor of TYPE.
C
C           Also keep track of the end record numbers for each
C           cluster.
C
            LAST    =  DIRREC( RNGBAS  +  (2*TYPE - 1)  )  -  1
            DSCTYP  =  DIRREC( BEGDSC                   )
            PRVTYP  =  PREV  ( DSCTYP                   )
            ENDREC  =  NREC
            POS     =  BEGDSC
 
            DO WHILE ( LAST .LT. MAXADR )
 
               POS = POS + 1
 
               IF ( DIRREC(POS) .GT. 0 ) THEN
                  CURTYP = NEXT(PRVTYP)
               ELSE
                  CURTYP = PREV(PRVTYP)
               END IF
 
               IF ( CURTYP .EQ. TYPE ) THEN
                  LAST = LAST  +  NW(TYPE) * ABS( DIRREC(POS) )
               END IF
 
               ENDREC = ENDREC + ABS( DIRREC(POS) )
               PRVTYP = CURTYP
 
            END DO
C
C           At this point, POS is the word position of the last
C           descriptor of TYPE, and ENDREC is the record number
C           of the last data record of TYPE.
C
            FTSUM ( LWDBAS + TYPE, FTHEAD )  =  POS
            LDREC ( TYPE )                   =  ENDREC
 
 
         ELSE
C
C           There's no data of TYPE in the file.
C
            FTSUM ( LWDBAS + TYPE, FTHEAD )  =  0
            LDREC ( TYPE )                   =  0
 
         END IF
 
      END DO
 
C
C     We're almost done; we need to find the number of the first free
C     record. This record follows all of the data records and all of
C     the directory records. It may happen that the last record in use
C     is an empty directory.
C
      CALL MAXAI ( LDREC, 3, LDRMAX, LOC )
 
      NREC   = LOCRRC + LOCCRC + 2
 
      CALL DASIOI ( 'READ',  NUMBER,  NREC,  DIRREC  )
 
      NXTREC = DIRREC(FWDLOC)
 
 
      DO WHILE ( NXTREC .NE. 0 )
 
         NREC   = NXTREC
 
         CALL DASIOI ( 'READ',  NUMBER,  NREC,  DIRREC  )
 
         NXTREC = DIRREC(FWDLOC)
 
      END DO
 
C
C     Now NREC is the last directory record.
C
      FTSUM ( FREIDX, FTHEAD)  =  MAX ( LDRMAX, NREC )  +  1
 
C
C     Insert the new handle into our handle set.
C
      CALL INSRTI ( HANDLE, FHLIST )
 
 
      CALL CHKOUT ( 'DASOPW' )
      RETURN
 
 
 
 
 
C$Procedure DASONW ( DAS, open new file )
 
      ENTRY DASONW ( FNAME, FTYPE, IFNAME, NCOMR, HANDLE )
 
C$ Abstract
C
C     Open a new DAS file and set the file type.
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
C     DAS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     CHARACTER*(*)         FTYPE
C     CHARACTER*(*)         IFNAME
C     INTEGER               NCOMR
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of a DAS file to be opened.
C     FTYPE      I   Mnemonic code for type of data in the DAS file.
C     IFNAME     I   Internal file name.
C     NCOMR      I   Number of comment records to allocate.
C     HANDLE     O   Handle assigned to the opened DAS file.
C
C$ Detailed_Input
C
C     FNAME    is the name of a new DAS file to be created (and
C              consequently opened for write access).
C
C     FTYPE    is a string indicating the type of data placed into a DAS
C              file. The first nonblank character and the three, or
C              fewer, characters immediately following it are stored as
C              the part of the file's ID word following the forward
C              slash. It is an error if FTYPE is blank.
C
C              The file type may not contain any nonprinting characters.
C              FTYPE is case sensitive.
C
C              NAIF has reserved for its own use file types
C              consisting of the upper case letters (A-Z) and the
C              digits 0-9. NAIF recommends lower case or mixed case
C              file types be used by all others in order to avoid any
C              conflicts with NAIF file types.
C
C     IFNAME   is a string containing the internal file name for the new
C              file. The name may contain as many as 60 characters. This
C              should uniquely identify the file.
C
C     NCOMR    is the number of comment records to allocate.
C              Allocating comment records at file creation time may
C              reduce the likelihood of having to expand the
C              comment area later.
C
C$ Detailed_Output
C
C     HANDLE   is the file handle associated with the file. This
C              handle is used to identify the file in subsequent
C              calls to other DAS routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input filename is blank, the error
C         SPICE(BLANKFILENAME) is signaled.
C
C     2)  If the specified file cannot be opened without exceeding
C         the maximum allowed number of open DAS files, the error
C         SPICE(DASFTFULL) is signaled. No file will be created.
C
C     3)  If the file cannot be opened properly, an error is signaled
C         by a routine in the call tree of this routine. No file will
C         be created.
C
C     4)  If the initial records in the file cannot be written, an
C         error is signaled by a routine in the call tree of this
C         routine. No file will be created.
C
C     5)  If the file type is blank, the error SPICE(BLANKFILETYPE) is
C         signaled.
C
C     6)  If the file type contains nonprinting characters---decimal
C         0-31 and 127-255---, the error SPICE(ILLEGALCHARACTER) is
C         signaled.
C
C     7)  If the number of comment records allocated NCOMR is negative,
C         the error SPICE(INVALIDCOUNT) is signaled.
C
C$ Files
C
C     See argument FNAME.
C
C$ Particulars
C
C     The DAS files created by this routine have initialized file
C     records.
C
C     This entry point creates a new DAS file and sets the type of the
C     file to the mnemonic code passed to it.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a new DAS file and add 200 integers to it. Close the
C        file, then re-open it and read the data back out.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASONW_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'dasonw_ex1.das' )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(4)         TYPE
C
C              INTEGER               DATA   ( 200 )
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Open a new DAS file. Use the file name as the internal
C        C     file name, and reserve no records for comments.
C        C
C              TYPE = 'TEST'
C              CALL DASONW ( FNAME, TYPE, FNAME, 0, HANDLE )
C
C        C
C        C     Fill the array DATA with the integers 1 through
C        C     100, and add this array to the file.
C        C
C              DO I = 1, 100
C                 DATA(I) = I
C              END DO
C
C              CALL DASADI ( HANDLE, 100, DATA )
C
C        C
C        C     Now append the array DATA to the file again.
C        C
C              CALL DASADI ( HANDLE, 100, DATA )
C
C        C
C        C     Close the file.
C        C
C              CALL DASCLS ( HANDLE )
C
C        C
C        C     Now verify the addition of data by opening the
C        C     file for read access and retrieving the data.
C        C
C              CALL DASOPR ( FNAME, HANDLE )
C              CALL DASRDI ( HANDLE, 1, 200, DATA )
C
C        C
C        C     Dump the data to the screen.  We should see the
C        C     sequence  1, 2, ..., 100, 1, 2, ... , 100.
C        C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Data from "', FNAME, '":'
C              WRITE (*,*) ' '
C              DO I = 1, 20
C                 WRITE (*,'(10I5)') (DATA((I-1)*10+J), J = 1, 10)
C              END DO
C
C        C
C        C     Close the file.
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
C         Data from "dasonw_ex1.das":
C
C            1    2    3    4    5    6    7    8    9   10
C           11   12   13   14   15   16   17   18   19   20
C           21   22   23   24   25   26   27   28   29   30
C           31   32   33   34   35   36   37   38   39   40
C           41   42   43   44   45   46   47   48   49   50
C           51   52   53   54   55   56   57   58   59   60
C           61   62   63   64   65   66   67   68   69   70
C           71   72   73   74   75   76   77   78   79   80
C           81   82   83   84   85   86   87   88   89   90
C           91   92   93   94   95   96   97   98   99  100
C            1    2    3    4    5    6    7    8    9   10
C           11   12   13   14   15   16   17   18   19   20
C           21   22   23   24   25   26   27   28   29   30
C           31   32   33   34   35   36   37   38   39   40
C           41   42   43   44   45   46   47   48   49   50
C           51   52   53   54   55   56   57   58   59   60
C           61   62   63   64   65   66   67   68   69   70
C           71   72   73   74   75   76   77   78   79   80
C           81   82   83   84   85   86   87   88   89   90
C           91   92   93   94   95   96   97   98   99  100
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
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C
C$ Version
C
C-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C        Updated $Exceptions entry #3: error handling for the DAS open
C        failure is now performed by lower level code (ZZDDHOPN).
C
C-    SPICELIB Version 7.0.0, 26-FEB-2015 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.0.0, 11-DEC-2001 (FST)
C
C        The DAS file ID word and internal file name are no longer
C        buffered by this routine. See DASFM's $Revisions section
C        for details.
C
C        The entry point was modified to insert the FTP validation
C        string, as well as the binary file format into the file record.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 31-AUG-1995 (NJB)
C
C        Changed argument list of the entry point DASONW. The input
C        argument NCOMR, which indicates the number of comment records
C        to reserve, was added to the argument list.
C
C-    SPICELIB Version 1.0.0, 29-OCT-1993 (KRG)
C
C-&
 
 
C$ Index_Entries
C
C     open a new DAS file
C     open a new DAS file with write access
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 6.0.0, 11-DEC-2001 (NJB) (FST)
C
C        See the $Revisions section under DASFM for a discussion of
C        the various changes made for this version.
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DASONW' )
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
C
C     Check to see whether the filename is blank. If it is, signal an
C     error, check out, and return.
C
      IF ( FNAME .EQ. ' ' ) THEN
 
         CALL SETMSG ( 'The file name is blank. ' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'     )
         CALL CHKOUT ( 'DASONW'                   )
         RETURN
 
      END IF
C
C     Check if the file type is blank.
C
      IF ( FTYPE .EQ. ' ' ) THEN
 
         CALL SETMSG ( 'The file type is blank. ' )
         CALL SIGERR ( 'SPICE(BLANKFILETYPE)'     )
         CALL CHKOUT ( 'DASONW'                   )
         RETURN
 
      END IF
C
C     Check for nonprinting characters in the file type.
C
      FNB =  LTRIM ( FTYPE )
 
      DO I = FNB, RTRIM ( FTYPE )
 
         IF ( ( ICHAR ( FTYPE(I:I) ) .GT. MAXPC )   .OR.
     .        ( ICHAR ( FTYPE(I:I) ) .LT. MINPC ) ) THEN
 
            CALL SETMSG ( 'The file type contains nonprinting' //
     .                    ' characters. '                       )
            CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'             )
            CALL CHKOUT ( 'DASONW'                              )
            RETURN
 
         END IF
 
      END DO
 
C
C     Validate the comment record count.
C
      IF ( NCOMR .LT. 0 ) THEN
 
         CALL SETMSG ( 'The number of comment records allocated ' //
     .                 'must be non-negative but was #.'           )
         CALL ERRINT ( '#',  NCOMR                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'DASONW'                                    )
         RETURN
 
      END IF
 
C
C     Set the value the file type in a temporary variable to be sure of
C     its length and then set the value of the ID word. Only 4
C     characters are allowed for the file type, and they are the first
C     nonblank character and its three (3) immediate successors in the
C     input string FTYPE.
C
      TTYPE  = FTYPE(FNB:)
      IDWORD = 'DAS/' // TTYPE
 
C
C     The file can be opened only if there is room for another file.
C
      IF ( LNKNFN(POOL) .EQ. 0  ) THEN
 
         CALL SETMSG ( 'The file table is full, with # entries. '     //
     .                 'Could not open ''#''.'                         )
         CALL ERRINT ( '#', FTSIZE                                     )
         CALL ERRCH  ( '#', FNAME                                      )
         CALL SIGERR ( 'SPICE(DASFTFULL)'                              )
         CALL CHKOUT ( 'DASONW'                                        )
         RETURN
 
      END IF
 
C
C     Open a new file.
C
      CALL ZZDDHOPN ( FNAME, 'NEW', 'DAS', HANDLE )
 
C
C     Get a logical unit for the file.
C
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., NUMBER )
 
C
C     Fetch the system file format.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', FORMAT )
 
C
C     Prepare to write the file record.
C
C     Use a local variable for the internal file name to ensure
C     that IFNLEN characters are written. The remaining
C     elements of the file record are:
C
C        -- the number of reserved records
C
C        -- the number of characters in use in the reserved
C           record area
C
C        -- the number of comment records
C
C        -- the number of characters in use in the comment area
C
C     Initially, all of these counts are zero, except for the
C     comment record count, which is set by the caller.
C
      LOCIFN = IFNAME
 
      CALL ZZDASNFR ( NUMBER,
     .                IDWORD,
     .                LOCIFN,
     .                0,
     .                0,
     .                NCOMR,
     .                0,
     .                FORMAT  )
 
C
C     Check to see whether or not ZZDASNFR generated an error
C     writing the file record to the logical unit. In the event
C     an error occurs, checkout and return.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASONW' )
         RETURN
      END IF
 
C
C     Zero out the first directory record in the file. If this
C     write fails, close the file with delete status and return
C     immediately. The first directory record follows the
C     comment records and reserved records. Currently there
C     are no reserved records, so the directory occupies record
C     NCOMR+2.
C
      CALL CLEARI ( NWI, DIRREC )
 
      WRITE ( UNIT   = NUMBER,
     .        REC    = NCOMR + 2,
     .        IOSTAT = IOSTAT     ) DIRREC
 
      IF ( IOSTAT .NE. 0 ) THEN
C
C        We had a write error. Ask the handle manager to
C        close and delete the new file.
C
         CALL ZZDDHCLS ( HANDLE, 'DAS', .TRUE. )
 
         CALL CHKOUT ( 'DASONW' )
         RETURN
 
      END IF
 
C
C     Update the file table to include information about
C     our newly opened DAS file. Link the information
C     for this file at the head of the file table list.
C
C     Set the output argument HANDLE as well.
C
      CALL LNKAN  ( POOL,   NEW             )
      CALL LNKILB ( NEW,    FTHEAD,   POOL  )
 
C
C     Clear out the file summary, except for the number of comment
C     records and the free record pointer. The free record pointer
C     should point to the first record AFTER the first directory.
C
      FTHEAD                     =   NEW
 
      CALL CLEARI ( SUMSIZ, FTSUM(1,FTHEAD) )
 
      FTHAN (         FTHEAD )   =   HANDLE
      FTACC (         FTHEAD )   =   WRITE
      FTLNK (         FTHEAD )   =   1
      FTSUM ( FREIDX, FTHEAD )   =   NCOMR + 3
      FTSUM ( CRCIDX, FTHEAD )   =   NCOMR
 
C
C     Insert the new handle into our handle set.
C
      CALL INSRTI ( HANDLE, FHLIST )
 
 
      CALL CHKOUT ( 'DASONW' )
      RETURN
 
 
 
 
C$Procedure DASOPN ( DAS, open new )
 
      ENTRY DASOPN ( FNAME, IFNAME, HANDLE )
 
C$ Abstract
C
C     Obsolete: This routine has been superseded by DASONW, and it is
C     supported for purposes of backward compatibility only.
C
C     Open a new DAS file for writing.
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
C     DAS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     CHARACTER*(*)         IFNAME
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of a DAS file to be opened.
C     IFNAME     I   Internal file name.
C     HANDLE     O   Handle assigned to the opened DAS file.
C
C$ Detailed_Input
C
C     FNAME    is the name of a new DAS file to be created (and
C              consequently opened for write access).
C
C     IFNAME   is the internal file name for the new file. The name
C              may contain as many as 60 characters. This should
C              uniquely identify the file.
C
C$ Detailed_Output
C
C     HANDLE   is the file handle associated with the file. This
C              handle is used to identify the file in subsequent
C              calls to other DAS routines.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input filename is blank, the error
C         SPICE(BLANKFILENAME) is signaled.
C
C     2)  If the specified file cannot be opened without exceeding
C         the maximum allowed number of open DAS files, the error
C         SPICE(DASFTFULL) is signaled. No file will be created.
C
C     3)  If the file cannot be opened properly, an error is signaled
C         by a routine in the call tree of this routine. No file will
C         be created.
C
C     4)  If the initial records in the file cannot be written, an
C         error is signaled by a routine in the call tree of this
C         routine. No file will be created.
C
C     5)  If no logical units are available, an error is signaled by a
C         routine in the call tree of this routine. No file will be
C         created.
C
C$ Files
C
C     See argument FNAME.
C
C$ Particulars
C
C     The DAS files created by this routine have initialized file
C     records.
C
C     This entry point has been made obsolete by the entry point DASONW,
C     and it is supported for reasons of backward compatibility only.
C     New software development should use the entry point DASONW.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a new DAS file and add 200 integers to it. Close the
C        file, then re-open it and read the data back out.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASOPN_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'dasopn_ex1.das' )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(4)         TYPE
C
C              INTEGER               DATA   ( 200 )
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Open a new DAS file. Use the file name as the internal
C        C     file name.
C        C
C              TYPE = 'TEST'
C              CALL DASOPN ( FNAME, FNAME, HANDLE )
C
C        C
C        C     Fill the array DATA with the integers 1 through
C        C     100, and add this array to the file.
C        C
C              DO I = 1, 100
C                 DATA(I) = I
C              END DO
C
C              CALL DASADI ( HANDLE, 100, DATA )
C
C        C
C        C     Now append the array DATA to the file again.
C        C
C              CALL DASADI ( HANDLE, 100, DATA )
C
C        C
C        C     Close the file.
C        C
C              CALL DASCLS ( HANDLE )
C
C        C
C        C     Now verify the addition of data by opening the
C        C     file for read access and retrieving the data.
C        C
C              CALL DASOPR ( FNAME, HANDLE )
C              CALL DASRDI ( HANDLE, 1, 200, DATA )
C
C        C
C        C     Dump the data to the screen.  We should see the
C        C     sequence  1, 2, ..., 100, 1, 2, ... , 100.
C        C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Data from "', FNAME, '":'
C              WRITE (*,*) ' '
C              DO I = 1, 20
C                 WRITE (*,'(10I5)') (DATA((I-1)*10+J), J = 1, 10)
C              END DO
C
C        C
C        C     Close the file.
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
C         Data from "dasopn_ex1.das":
C
C            1    2    3    4    5    6    7    8    9   10
C           11   12   13   14   15   16   17   18   19   20
C           21   22   23   24   25   26   27   28   29   30
C           31   32   33   34   35   36   37   38   39   40
C           41   42   43   44   45   46   47   48   49   50
C           51   52   53   54   55   56   57   58   59   60
C           61   62   63   64   65   66   67   68   69   70
C           71   72   73   74   75   76   77   78   79   80
C           81   82   83   84   85   86   87   88   89   90
C           91   92   93   94   95   96   97   98   99  100
C            1    2    3    4    5    6    7    8    9   10
C           11   12   13   14   15   16   17   18   19   20
C           21   22   23   24   25   26   27   28   29   30
C           31   32   33   34   35   36   37   38   39   40
C           41   42   43   44   45   46   47   48   49   50
C           51   52   53   54   55   56   57   58   59   60
C           61   62   63   64   65   66   67   68   69   70
C           71   72   73   74   75   76   77   78   79   80
C           81   82   83   84   85   86   87   88   89   90
C           91   92   93   94   95   96   97   98   99  100
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
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C        Updated $Exceptions entry #3: error handling for the DAS open
C        failure is now performed by lower level code (ZZDDHOPN).
C
C-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.0.0, 11-DEC-2001 (FST)
C
C        The DAS file ID word and internal file name are no longer
C        buffered by this routine. See DASFM's $Revisions section
C        for details.
C
C        This entry point was modified to insert the FTP validation
C        string, as well as the binary file format into the file record.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG)
C
C        The effect of this routine is unchanged. It still uses the ID
C        word 'NAIF/DAS'. This is for backward compatibility only.
C
C        Added statements to the $Abstract and $Particulars sections
C        to document that this entry is now considered to be obsolete,
C        and that it has been superseded by the entry point DASONW.
C
C        Added a test for a blank filename before attempting to use the
C        filename in the routine. If the filename is blank, the error
C        SPICE(BLANKFILENAME) will be signaled.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     open a new DAS file for writing
C     open a new DAS file for write access
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C
C-    SPICELIB Version 6.0.0, 11-DEC-2001 (FST)
C
C        See the $Revisions section under DASFM for a discussion
C        of the changes made for this version.
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DASOPN' )
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
C
C     Check to see whether the filename is blank. If it is, signal an
C     error, check out, and return.
C
      IF ( FNAME .EQ. ' ' ) THEN
 
         CALL SETMSG ( 'The file name is blank. ' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'     )
         CALL CHKOUT ( 'DASOPN'                   )
         RETURN
 
      END IF
 
C
C     The file can be opened only if there is room for another file.
C
      IF ( LNKNFN(POOL) .EQ. 0  ) THEN
 
         CALL SETMSG ( 'The file table is full, with # entries. '     //
     .                 'Could not open ''#''.'                         )
         CALL ERRINT ( '#', FTSIZE                                     )
         CALL ERRCH  ( '#', FNAME                                      )
         CALL SIGERR ( 'SPICE(DASFTFULL)'                              )
         CALL CHKOUT ( 'DASOPN'                                        )
         RETURN
 
 
      END IF
 
C
C     To open a new file: get a free unit, open the file, write
C     the file record, and set the number of links to one.
C
C     Look out for:
C
C        -- No free logical units.
C
C        -- Error opening the file.
C
C        -- Error writing to the file.
C
C     If anything goes wrong after the file has been opened, delete
C     the file.
C
C
      CALL ZZDDHOPN ( FNAME, 'NEW', 'DAS', HANDLE )
 
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., NUMBER )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASOPN' )
         RETURN
      END IF
 
C
C     Fetch the system file format.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', FORMAT )
 
C
C     Prepare to write the file record.
C
C     Use a local variable for the internal file name to ensure that
C     IFNLEN characters are written. The remaining elements of the file
C     record are:
C
C        -- the number of reserved records
C
C        -- the number of characters in use in the reserved
C           record area
C
C        -- the number of comment records
C
C        -- the number of characters in use in the comment area
C
C     Initially, all of these counts are zero.
C
C
      LOCIFN = IFNAME
      IDWORD = 'NAIF/DAS'
 
      CALL ZZDASNFR ( NUMBER,
     .                IDWORD,
     .                LOCIFN,
     .                0,
     .                0,
     .                0,
     .                0,
     .                FORMAT  )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASOPN' )
         RETURN
      END IF
 
C
C     Zero out the first directory record (record #2) in the file. If
C     this write fails, close the file with delete status and return
C     immediately.
C
 
C
C     NOTE: re-write this using ZZDDHCLS.
C
      CALL CLEARI (  NWI,               DIRREC )
      CALL DASIOI ( 'WRITE', NUMBER, 2, DIRREC )
 
      IF ( FAILED() ) THEN
         CLOSE       ( UNIT = NUMBER,  STATUS = 'DELETE' )
         CALL CHKOUT ( 'DASOPN' )
         RETURN
      END IF
 
C
C     Update the file table to include information about
C     our newly opened DAS file. Link the information
C     for this file at the head of the file table list.
C
C     Set the output argument HANDLE as well.
C
      CALL LNKAN  ( POOL,   NEW             )
      CALL LNKILB ( NEW,    FTHEAD,   POOL  )
 
C
C     Clear out the file summary, except for the free record pointer.
C     The free record pointer should point to the first record AFTER
C     the first directory.
C
      FTHEAD                     =   NEW
 
      CALL CLEARI ( SUMSIZ, FTSUM(1,FTHEAD) )
 
      FTHAN (         FTHEAD )   =   HANDLE
      FTACC (         FTHEAD )   =   WRITE
      FTLNK (         FTHEAD )   =   1
      FTSUM ( FREIDX, FTHEAD )   =   3
 
C
C     Insert the new handle into our handle set.
C
      CALL INSRTI ( HANDLE, FHLIST )
 
      CALL CHKOUT ( 'DASOPN' )
      RETURN
 
 
 
 
C$Procedure DASOPS ( DAS, open scratch )
 
      ENTRY DASOPS ( HANDLE )
 
C$ Abstract
C
C     Open a scratch DAS file for writing.
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
C     DAS
C     FILES
C     UTILITY
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     O   Handle assigned to a scratch DAS file.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     HANDLE   is the file handle associated with the scratch file
C              opened by this routine. This handle is used to
C              identify the file in subsequent calls to other DAS
C              routines.
C
C$ Parameters
C
C     FTSIZE   is the maximum number of DAS files that a user can have
C              open simultaneously. This includes any files used by the
C              DAS system when closing files opened with write access.
C              Currently, DASCLS (via the SPICELIB routine DASSDR) opens
C              a scratch DAS file using DASOPS to segregate (sort by
C              data type) the records in the DAS file being closed.
C              Segregating the data by type improves the speed of access
C              to the data.
C
C              In order to avoid the possibility of overflowing the
C              DAS file table we recommend, when at least one DAS
C              file is open with write access, that users of this
C              software limit themselves to at most FTSIZE - 2 other
C              open DAS files. If no files are to be open with write
C              access, then users may open FTSIZE files with no
C              possibility of overflowing the DAS file table.
C
C$ Exceptions
C
C     1)  If the specified file cannot be opened without exceeding
C         the maximum allowed number of open DAS files, the error
C         SPICE(DASFTFULL) is signaled. No file will be created.
C
C     2)  If file cannot be opened properly, an error is signaled by a
C         routine in the call tree of this routine. No file will be
C         created.
C
C     3)  If the initial records in the file cannot be written, the
C         error SPICE(DASWRITEFAIL) is signaled. No file will be
C         created.
C
C     4)  If no logical units are available, an error is signaled by a
C         routine in the call tree of this routine. No file will be
C         created.
C
C$ Files
C
C     See output argument HANDLE.
C
C     See FTSIZE in the $Parameters section for a description of a
C     potential problem with overflowing the DAS file table when at
C     least one DAS file is opened with write access.
C
C$ Particulars
C
C     This routine is a utility used by the DAS system to provide
C     work space needed when creating new DAS files.
C
C     The DAS files created by this routine have initialized file
C     records. The file type for a DAS scratch file is 'SCR ', so the
C     file type 'SCR ' is not available for general use. As with new
C     permanent files, these files are opened for write access. DAS
C     files opened by DASOPS are automatically deleted when they are
C     closed.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a DAS scratch file containing 10 integers, 5 double
C        precision numbers, and 4 characters, then print the logical
C        address ranges in use.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASOPS_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               LASTC
C              INTEGER               LASTD
C              INTEGER               LASTI
C
C        C
C        C     Use a scratch file, since there's no reason to keep
C        C     the file.
C        C
C              CALL DASOPS ( HANDLE )
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
C        C     Scratch files are automatically deleted when they are
C        C     closed.
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
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Extended $Particulars section to indicate that scratch files
C        are deleted when they are closed.
C
C        Updated $Exceptions entry #2: error handling for the DAS open
C        failure is now performed by lower level code (ZZDDHOPN), and
C        added FTSIZE parameter description.
C
C-    SPICELIB Version 7.0.0, 07-APR-2016 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG)
C
C        Modified the entry point to use the new file ID format which
C        contains a mnemonic code for the data type.
C
C        Put meaningful values into the type and internal filename
C        for a DAS scratch file, rather than leaving them blank.
C
C        Documented the potential problem of overflowing the DAS file
C        table when attempting to close a DAS file opened with write
C        access when the file table is full. Modified the long error
C        message to indicate this as a cause of the problem.
C
C-    SPICELIB Version 1.1.0, 04-MAY-1993 (NJB)
C
C        Bug fix: removed file name variable from error message.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     open a scratch DAS file
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 29-OCT-1993 (KRG)
C
C        Modified the entry point to use the new file ID format which
C        contains a mnemonic code for the data type.
C
C        DAS scratch files use the type 'SCR ', so the ID word for a DAS
C        scratch file would be: 'DAS/SCR '
C
C        Changed the internal filename from blank to the string:
C
C           'DAS SCRATCH FILE'
C
C        It's probably better to have something written there than
C        nothing.
C
C        Documented the potential problem of overflowing the DAS file
C        table when attempting to close a DAS file opened with write
C        access when the file table is full. Modified the long error
C        message to indicate this as a cause of the problem.
C
C        The problem occurs when the file table is full, the number of
C        open DAS files equals FTSIZE, and at least one of the open
C        files was opened with write access. If an attempt to close a
C        file opened with write access is made under these conditions,
C        by calling DASCLS, it will fail. DASCLS (via DASSDR) calls
C        DASOPS to open a scratch DAS file, but the scratch file CANNOT
C        be opened because the file table is full. If this occurs, close
C        a file open for read access, or restrict the number of open
C        files in use to be at most FTSIZE - 1 when there will be at
C        least one file opened with write access.
C
C-    SPICELIB Version 1.1.0, 04-MAY-1993 (NJB)
C
C        Bug fix: removed unneeded file name variable FNAME from
C        error message.
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DASOPS' )
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
C
C     The file can be opened only if there is room for another file.
C
      IF ( LNKNFN(POOL) .EQ. 0  ) THEN
 
         CALL SETMSG ( 'The file table is full, with # entries.'      //
     .                 ' Could not open a scratch file. If a call'    //
     .                 ' to DASOPS was not made and this error'       //
     .                 ' occurred, it is likely that the DAS file'    //
     .                 ' table was full and an attempt to close a'    //
     .                 ' file opened with write access was made. See' //
     .                 ' the DAS required reading and DASFM for'      //
     .                 ' details.'                                     )
         CALL ERRINT ( '#', FTSIZE                                     )
         CALL SIGERR ( 'SPICE(DASFTFULL)'                              )
         CALL CHKOUT ( 'DASOPS'                                        )
         RETURN
 
      END IF
 
C
C     Assign a name to the scratch file. This name is required
C     by the DDH subsystem.
C
      LOCDAS = 'DAS SCRATCH FILE'
 
C
C     Open a DAS file for scratch access.
C
      CALL ZZDDHOPN ( LOCDAS, 'SCRATCH', 'DAS', HANDLE )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASOPS' )
         RETURN
      END IF
 
C
C     Get a logical unit for the file.
C
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., NUMBER )
 
C
C     Fetch the system file format.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', FORMAT )
 
C
C     Prepare to write the file record.
C
C     Use a local variable for the internal file name to ensure
C     that IFNLEN characters are written. The remaining
C     elements of the file record are:
C
C        -- the number of reserved records
C
C        -- the number of characters in use in the reserved
C           record area
C
C        -- the number of comment records
C
C        -- the number of characters in use in the comment area
C
C     Initially, all of these counts are zero, except for the
C     comment record count, which is set by the caller.
C
      LOCIFN = LOCDAS(:IFNLEN)
 
      CALL ZZDASNFR ( NUMBER,
     .                IDWORD,
     .                LOCIFN,
     .                0,
     .                0,
     .                0,
     .                0,
     .                FORMAT  )
 
C
C     Check to see whether or not ZZDASNFR generated an error
C     writing the file record to the logical unit. In the event
C     an error occurs, checkout and return.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASOPS' )
         RETURN
      END IF
 
C
C     Update the file table to include information about
C     our newly opened DAS file. Link the information
C     for this file at the head of the file table list.
C
C     Set the output argument HANDLE as well.
C
      CALL LNKAN  ( POOL,   NEW             )
      CALL LNKILB ( NEW,    FTHEAD,   POOL  )
 
      FTHEAD                     =   NEW
 
C
C     Clear out the file summary, except for the free record pointer.
C     The free record pointer should point to the first record AFTER
C     the first directory.
C
      CALL CLEARI ( SUMSIZ, FTSUM(1,FTHEAD) )
 
      FTHAN (         FTHEAD )   =   HANDLE
      FTACC (         FTHEAD )   =   WRITE
      FTLNK (         FTHEAD )   =   1
      FTSUM ( FREIDX, FTHEAD )   =   3
 
C
C     Insert the new handle into our handle set.
C
      CALL INSRTI ( HANDLE, FHLIST )
 
 
      CALL CHKOUT ( 'DASOPS' )
      RETURN
 
 
 
C$Procedure DASLLC ( DAS, low-level close )
 
      ENTRY DASLLC ( HANDLE )
 
C$ Abstract
C
C     Close the DAS file associated with a given handle, without
C     flushing buffered data or segregating the file.
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
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a DAS file to be closed.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of a previously opened DAS file.
C
C$ Detailed_Output
C
C     None.
C
C     See $Particulars for a description of the effect of this routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified handle does not belong to a DAS file that is
C         currently open, this routine returns without signaling an
C         error.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     Normally, routines outside of SPICELIB will not need to call this
C     routine. Application programs should close DAS files by calling
C     the SPICELIB routine DASCLS. This routine is a lower-level
C     routine that is called by DASCLS, but (obviously) does not have
C     the full functionality of DASCLS.
C
C     This routine closes a DAS file and updates the DAS file manager's
C     bookkeeping information on open DAS files. Because the DAS file
C     manager must keep track of which files are open at any given time,
C     it is important that DAS files be closed only with DASCLS or
C     DASLLC, to prevent the remaining DAS routines from failing,
C     sometimes mysteriously.
C
C     Note that when a file is opened more than once for read or write
C     access, DASOPR returns the same handle each time it is re-opened.
C     Each time the file is closed, DASLLC checks to see if any other
C     claims on the file are still active before physically closing
C     the file.
C
C     Unlike DASCLS, this routine does not force a write of updated,
C     buffered records to the indicated file, nor does it segregate the
C     data records in the file.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Write a DAS file by adding data to it over multiple passes.
C        Avoid spending time on file segregation between writes.
C
C        Each pass opens the file, adds character, double precision,
C        and integer data to the file, writes out buffered data by
C        calling DASWBR, and closes the file without segregating the
C        data by calling DASLLC.
C
C        The program also checks the file: after the final write,
C        the program reads the data and compares it to expected values.
C
C        Note that most user-oriented applications should segregate a
C        DAS file after writing it, since this greatly enhances file
C        reading efficiency. The technique demonstrated here may be
C        useful for cases in which a file will be written via many
C        small data additions, and in which the file is read between
C        write operations.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASLLC_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters
C        C
C              INTEGER               FILSIZ
C              PARAMETER           ( FILSIZ = 255 )
C
C              INTEGER               FTYPLN
C              PARAMETER           ( FTYPLN = 3 )
C
C              INTEGER               CHRLEN
C              PARAMETER           ( CHRLEN = 50 )
C
C              INTEGER               IBUFSZ
C              PARAMETER           ( IBUFSZ = 20 )
C
C              INTEGER               DBUFSZ
C              PARAMETER           ( DBUFSZ = 30 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(CHRLEN)    CHRBUF
C              CHARACTER*(FILSIZ)    FNAME
C              CHARACTER*(FTYPLN)    FTYPE
C              CHARACTER*(CHRLEN)    XCHRBF
C
C              DOUBLE PRECISION      DPBUF  ( DBUFSZ )
C              DOUBLE PRECISION      XDPBUF ( DBUFSZ )
C
C              INTEGER               FIRSTC
C              INTEGER               FIRSTD
C              INTEGER               FIRSTI
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               INTBUF ( IBUFSZ )
C              INTEGER               J
C              INTEGER               LASTC
C              INTEGER               LASTD
C              INTEGER               LASTI
C              INTEGER               NCALL
C              INTEGER               NCOMR
C              INTEGER               NPASS
C              INTEGER               PASSNO
C              INTEGER               XINTBF ( IBUFSZ )
C
C
C        C
C        C     Initial values
C        C
C              DATA                  FNAME  / 'dasllc_ex1.das' /
C              DATA                  FTYPE  / 'ANG' /
C              DATA                  NCALL  / 1000  /
C              DATA                  NCOMR  / 10    /
C              DATA                  NPASS  / 3     /
C
C        C
C        C     Open a new DAS file. We'll allocate NCOMR records
C        C     for comments. The file type is not one of the standard
C        C     types recognized by SPICE; however it can be used to
C        C     ensure the database file is of the correct type.
C        C
C        C     We'll use the file name as the internal file name.
C        C
C              CALL DASONW ( FNAME, FTYPE, FNAME, NCOMR, HANDLE )
C
C        C
C        C     Add data of character, integer, and double precision
C        C     types to the file in interleaved fashion. We'll add to
C        C     the file over NPASS "passes," in each of which we close
C        C     the file after writing.
C        C
C              DO PASSNO = 1, NPASS
C
C                 IF ( PASSNO .GT. 1 ) THEN
C
C                    WRITE (*,*) 'Opening file for write access...'
C
C                    CALL DASOPW( FNAME, HANDLE )
C
C                 END IF
C
C                 DO I = 1, NCALL
C        C
C        C           Add string data to the file.
C        C
C                    CHRBUF = 'Character value #'
C                    CALL REPMI( CHRBUF, '#', I, CHRBUF )
C
C                    CALL DASADC ( HANDLE, CHRLEN, 1, CHRLEN, CHRBUF )
C
C        C
C        C           Add double precision data to the file.
C        C
C                    DO J = 1, DBUFSZ
C                       DPBUF(J) = DBLE( 100000000*PASSNO + 100*I + J )
C                    END DO
C
C                    CALL DASADD ( HANDLE, DBUFSZ, DPBUF )
C
C        C
C        C           Add integer data to the file.
C        C
C                    DO J = 1, IBUFSZ
C                       INTBUF(J) = 100000000*PASSNO  +  100 * I  +  J
C                    END DO
C
C                    CALL DASADI ( HANDLE, IBUFSZ, INTBUF )
C
C                 END DO
C
C        C
C        C        Write buffered data to the file.
C        C
C                 WRITE (*,*) 'Writing buffered data...'
C                 CALL DASWBR ( HANDLE )
C
C        C
C        C        Close the file without segregating it.
C        C
C                 WRITE (*,*) 'Closing DAS file...'
C                 CALL DASLLC ( HANDLE )
C
C              END DO
C
C              WRITE (*,*) 'File write is done.'
C
C        C
C        C     Check file contents.
C        C
C              CALL DASOPR( FNAME, HANDLE )
C
C        C
C        C     Read data from the file; compare to expected values.
C        C
C        C     Initialize end addresses.
C        C
C              LASTC = 0
C              LASTD = 0
C              LASTI = 0
C
C              DO PASSNO = 1, NPASS
C
C                 DO I = 1, NCALL
C        C
C        C           Check string data.
C        C
C                    XCHRBF = 'Character value #'
C                    CALL REPMI( XCHRBF, '#', I, XCHRBF )
C
C                    FIRSTC = LASTC + 1
C                    LASTC  = LASTC + CHRLEN
C
C                    CALL DASRDC ( HANDLE, FIRSTC, LASTC,
C             .                    1,      CHRLEN, CHRBUF )
C
C                    IF ( CHRBUF .NE. XCHRBF ) THEN
C                       WRITE (*,*) 'Character data mismatch: '
C                       WRITE (*,*) 'PASS     = ', PASSNO
C                       WRITE (*,*) 'I        = ', I
C                       WRITE (*,*) 'Expected = ', XCHRBF
C                       WRITE (*,*) 'Actual   = ', CHRBUF
C                       STOP
C                    END IF
C
C        C
C        C           Check double precision data.
C        C
C                    DO J = 1, DBUFSZ
C                       XDPBUF(J) = DBLE(   100000000*PASSNO
C             .                           + 100*I + J        )
C                    END DO
C
C                    FIRSTD = LASTD + 1
C                    LASTD  = LASTD + DBUFSZ
C
C                    CALL DASRDD ( HANDLE, FIRSTD, LASTD, DPBUF )
C
C                    DO J = 1, DBUFSZ
C
C                       IF ( DPBUF(J) .NE. XDPBUF(J) ) THEN
C
C                          WRITE (*,*)
C             .                      'Double precision data mismatch: '
C                          WRITE (*,*) 'PASS     = ', PASSNO
C                          WRITE (*,*) 'I        = ', I
C                          WRITE (*,*) 'J        = ', J
C                          WRITE (*,*) 'Expected = ', XDPBUF(J)
C                          WRITE (*,*) 'Actual   = ', DPBUF(J)
C                          STOP
C
C                       END IF
C
C                    END DO
C
C        C
C        C           Check integer data.
C        C
C                    DO J = 1, IBUFSZ
C                       XINTBF(J) = 100000000*PASSNO  +  100 * I  +  J
C                    END DO
C
C                    FIRSTI = LASTI + 1
C                    LASTI  = LASTI + IBUFSZ
C
C                    CALL DASRDI ( HANDLE, FIRSTI, LASTI, INTBUF )
C
C                    DO J = 1, IBUFSZ
C
C                       IF ( INTBUF(J) .NE. XINTBF(J) ) THEN
C
C                          WRITE (*,*) 'Integer data mismatch: '
C                          WRITE (*,*) 'PASS     = ', PASSNO
C                          WRITE (*,*) 'I        = ', I
C                          WRITE (*,*) 'J        = ', J
C                          WRITE (*,*) 'Expected = ', XINTBF(J)
C                          WRITE (*,*) 'Actual   = ', INTBUF(J)
C                          STOP
C
C                       END IF
C
C                    END DO
C
C                 END DO
C
C              END DO
C
C              WRITE (*,*) 'File check is done.'
C
C        C
C        C     Close the file.
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
C         Writing buffered data...
C         Closing DAS file...
C         Opening file for write access...
C         Writing buffered data...
C         Closing DAS file...
C         Opening file for write access...
C         Writing buffered data...
C         Closing DAS file...
C         File write is done.
C         File check is done.
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
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 7.0.1, 19-JUL-2021 (NJB) (JDR)
C
C        Updated the header to comply with NAIF standard. Added
C        complete code example.
C
C-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C
C-    SPICELIB Version 6.0.3, 10-APR-2014 (NJB)
C
C        Corrected header comments: routine that flushes
C        written, buffered records is DASWBR, not DASWUR.
C
C-    SPICELIB Version 6.0.2, 21-FEB-2003 (NJB)
C
C        Corrected inline comment: determination of whether file
C        is open is done by searching the handle column of the file
C        table, not the unit column.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     close a DAS file
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DASLLC' )
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
C
C     Is this file even open?  Peruse the `handle' column of the file
C     table; see whether this handle is present.
C
      FINDEX  =  FTHEAD
      FOUND   = .FALSE.
 
      DO WHILE (  ( .NOT. FOUND )  .AND.  ( FINDEX .GT. 0 )  )
 
         IF ( FTHAN(FINDEX) .EQ. HANDLE ) THEN
            FOUND = .TRUE.
         ELSE
            FINDEX = LNKNXT ( FINDEX, POOL )
         END IF
 
      END DO
 
C
C     If the file is not open: no harm, no foul.  Otherwise, decrement
C     the number of links to the file.  If the number of links drops to
C     zero, physically close the file and remove it from the file
C     buffer.
C
      IF ( FOUND ) THEN
 
         FTLNK(FINDEX) = FTLNK(FINDEX) - 1
 
         IF ( FTLNK(FINDEX) .EQ. 0 ) THEN
C
C           Close this file and delete it from the active list.
C           If this was the head node of the list, the head node
C           becomes the successor of this node (which may be NIL).
C           Delete the handle from our handle set.
C
            CALL ZZDDHCLS ( HANDLE, 'DAS', .FALSE. )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASLLC' )
               RETURN
            END IF
 
            IF ( FINDEX .EQ. FTHEAD ) THEN
               FTHEAD = LNKNXT ( FINDEX, POOL )
            END IF
 
            CALL LNKFSL ( FINDEX, FINDEX, POOL )
 
            CALL REMOVI ( HANDLE, FHLIST       )
 
         END IF
 
      END IF
 
      CALL CHKOUT ( 'DASLLC' )
      RETURN
 
 
C$Procedure DASHFS ( DAS, handle to file summary )
 
      ENTRY DASHFS ( HANDLE,
     .               NRESVR,
     .               NRESVC,
     .               NCOMR,
     .               NCOMC,
     .               FREE,
     .               LASTLA,
     .               LASTRC,
     .               LASTWD )
 
C$ Abstract
C
C     Return a file summary for a specified DAS file.
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
C     DAS
C     FILES
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     INTEGER               HANDLE
C     INTEGER               NRESVR
C     INTEGER               NRESVC
C     INTEGER               NCOMR
C     INTEGER               NCOMC
C     INTEGER               FREE
C     INTEGER               LASTLA ( 3 )
C     INTEGER               LASTRC ( 3 )
C     INTEGER               LASTWD ( 3 )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a DAS file.
C     NRESVR     O   Number of reserved records in file.
C     NRESVC     O   Number of characters in use in reserved rec. area.
C     NCOMR      O   Number of comment records in file.
C     NCOMC      O   Number of characters in use in comment area.
C     FREE       O   Number of first free record.
C     LASTLA     O   Array of last logical addresses for each data type.
C     LASTRC     O   Record number of last descriptor of each data type.
C     LASTWD     O   Word number of last descriptor of each data type.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of a previously opened DAS file. The file
C              may be open for read or write access.
C
C$ Detailed_Output
C
C     NRESVR   is the number of reserved records in a specified DAS
C              file.
C
C     NRESVC   is the number of characters in use in the reserved record
C              area of a specified DAS file.
C
C     NCOMR    is the number of comment records in a specified DAS file.
C
C     NCOMC    is the number of characters in use in the comment area of
C              a specified DAS file.
C
C     FREE     is the 1-based record number of the first free record in
C              a specified DAS file.
C
C     LASTLA   is an array containing the highest current 1-based
C              logical addresses, in the specified DAS file, of data of
C              character, double precision, and integer types, in that
C              order.
C
C     LASTRC   is an array containing the 1-based record numbers, in the
C              specified DAS file, of the directory records containing
C              the current last descriptors of clusters of character,
C              double precision, and integer data records, in that
C              order.
C
C     LASTWD   is an array containing the 1-based word indices, within
C              the respective descriptor records identified by the
C              elements of LASTRC, of the current last descriptors of
C              clusters of character, double precision, and integer data
C              records, in that order.
C
C$ Parameters
C
C     See INCLUDE file das.inc for declarations and descriptions of
C     parameters used throughout the DAS system.
C
C     CHARDT,
C     DPDT,
C     INTDT    are data type specifiers that indicate CHARACTER,
C              DOUBLE PRECISION, and INTEGER respectively. These
C              parameters are used in all DAS routines that require a
C              data type specifier.
C
C$ Exceptions
C
C     1)  If the specified handle does not belong to any file that is
C         currently known to be open, the error SPICE(DASNOSUCHHANDLE)
C         is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The quantities
C
C        NRESVR
C        NRESVC
C        NCOMR
C        NCOMC
C        FREE
C        LASTLA
C        LASTRC
C        LASTWD
C
C     define the "state" of a DAS file, and in particular the state of
C     the directory structure of the file. This information is needed by
C     other DAS routines, but application programs will usually have no
C     need for it. The one exception is the array of "last" logical
C     addresses LASTLA: these addresses indicate how many words of data
C     of each type are contained in the specified DAS file. The elements
C     of LASTLA can be conveniently retrieved by calling DASLLA.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a DAS file containing 10 integers, 5 double precision
C        numbers, and 4 characters. Print the summary of the file and
C        dump its contents.
C
C
C        Example code begins here.
C
C
C              PROGRAM DASHFS_EX1
C              IMPLICIT NONE
C
C              INCLUDE 'das.inc'
C
C        C
C        C     Local parameters.
C        C
C              CHARACTER*(*)         FNAME
C              PARAMETER           ( FNAME = 'dashfs_ex1.das' )
C
C              INTEGER               IFNMLN
C              PARAMETER           ( IFNMLN = 60 )
C
C              INTEGER               LINLEN
C              PARAMETER           ( LINLEN = 2  )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(IFNMLN)    IFNAME
C              CHARACTER*(4)         TYPE
C              CHARACTER*(LINLEN)    LINE
C
C              DOUBLE PRECISION      X
C
C              INTEGER               FIRST
C              INTEGER               FREE
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               LAST
C              INTEGER               LASTLA ( 3 )
C              INTEGER               LASTRC ( 3 )
C              INTEGER               LASTWD ( 3 )
C              INTEGER               N
C              INTEGER               NCOMC
C              INTEGER               NCOMR
C              INTEGER               NREAD
C              INTEGER               NRESVC
C              INTEGER               NRESVR
C              INTEGER               REMAIN
C
C        C
C        C     Open a new DAS file. Reserve no records for comments.
C        C
C              TYPE   = 'TEST'
C              IFNAME = 'TEST.DAS/NAIF/NJB/11-NOV-1992-20:12:20'
C
C              CALL DASONW ( FNAME, TYPE, IFNAME, 0, HANDLE )
C
C        C
C        C     Obtain the file summary.
C        C
C              CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
C             .              FREE,   LASTLA, LASTRC, LASTWD       )
C
C        C
C        C     Print the summary of the new file.
C        C
C              WRITE(*,*) 'Summary before adding data:'
C              WRITE(*,*) '   Number of reserved records     :', NRESVR
C              WRITE(*,*) '   Characters in reserved records :', NRESVC
C              WRITE(*,*) '   Number of comment records      :', NCOMR
C              WRITE(*,*) '   Characters in comment area     :', NCOMC
C              WRITE(*,*) '   Number of first free record    :', FREE
C              WRITE(*,*) '   Last logical character address :',
C             .                                          LASTLA(CHARDT)
C              WRITE(*,*) '   Last logical d.p. address      :',
C             .                                          LASTLA(DPDT)
C              WRITE(*,*) '   Last logical integer address   :',
C             .                                          LASTLA(INTDT)
C              WRITE(*,*) '   Last character descriptor      :',
C             .                                          LASTRC(CHARDT)
C              WRITE(*,*) '   Last d.p descriptor            :',
C             .                                          LASTRC(DPDT)
C              WRITE(*,*) '   Last integer descriptor        :',
C             .                                          LASTRC(INTDT)
C              WRITE(*,*) '   Character word position in desc:',
C             .                                          LASTWD(CHARDT)
C              WRITE(*,*) '   d.p. word position in desc     :',
C             .                                          LASTWD(DPDT)
C              WRITE(*,*) '   Integer word position in desc  :',
C             .                                          LASTWD(INTDT)
C              WRITE(*,*)
C
C        C
C        C     Add the data.
C        C
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
C        C     Close the file and open it for reading.
C        C
C              CALL DASCLS ( HANDLE        )
C              CALL DASOPR ( FNAME, HANDLE )
C
C        C
C        C     Obtain again the file summary.
C        C
C              CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
C             .              FREE,   LASTLA, LASTRC, LASTWD       )
C
C              WRITE(*,*) 'Summary after adding data:'
C              WRITE(*,*) '   Number of reserved records     :', NRESVR
C              WRITE(*,*) '   Characters in reserved records :', NRESVC
C              WRITE(*,*) '   Number of comment records      :', NCOMR
C              WRITE(*,*) '   Characters in comment area     :', NCOMC
C              WRITE(*,*) '   Number of first free record    :', FREE
C              WRITE(*,*) '   Last logical character address :',
C             .                                          LASTLA(CHARDT)
C              WRITE(*,*) '   Last logical d.p. address      :',
C             .                                          LASTLA(DPDT)
C              WRITE(*,*) '   Last logical integer address   :',
C             .                                          LASTLA(INTDT)
C              WRITE(*,*) '   Last character descriptor      :',
C             .                                          LASTRC(CHARDT)
C              WRITE(*,*) '   Last d.p descriptor            :',
C             .                                          LASTRC(DPDT)
C              WRITE(*,*) '   Last integer descriptor        :',
C             .                                          LASTRC(INTDT)
C              WRITE(*,*) '   Character word position in desc:',
C             .                                          LASTWD(CHARDT)
C              WRITE(*,*) '   d.p. word position in desc     :',
C             .                                          LASTWD(DPDT)
C              WRITE(*,*) '   Integer word position in desc  :',
C             .                                          LASTWD(INTDT)
C              WRITE(*,*)
C
C        C
C        C     Read the integers and dump them.
C        C
C              WRITE(*,*) 'Integer data in the DAS file:'
C              DO I = 1, LASTLA(INTDT)
C                 CALL DASRDI ( HANDLE, I, I, N )
C                 WRITE (*,*) '   ', N
C              END DO
C
C        C
C        C     Now the d.p. numbers:
C        C
C              WRITE(*,*)
C              WRITE(*,*) 'Double precision data in the DAS file:'
C              DO I = 1, LASTLA(DPDT)
C                 CALL DASRDD ( HANDLE, I, I, X )
C                 WRITE (*,*) '   ', X
C              END DO
C
C        C
C        C     Now the characters. In this case, we read the
C        C     data one line at a time.
C        C
C              FIRST   =  0
C              LAST    =  0
C              REMAIN  =  LASTLA(CHARDT)
C
C              WRITE(*,*)
C              WRITE(*,*) 'Character data in the DAS file:'
C              DO WHILE ( REMAIN .GT. 0 )
C
C                 NREAD = MIN ( LINLEN, REMAIN )
C                 FIRST = LAST + 1
C                 LAST  = LAST + NREAD
C
C                 CALL DASRDC ( HANDLE, FIRST, LAST, 1, LINLEN, LINE )
C
C                 WRITE (*,*) '   ', LINE(:NREAD)
C
C                 REMAIN = REMAIN - NREAD
C
C              END DO
C
C        C
C        C     Close the file.
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
C         Summary before adding data:
C            Number of reserved records     :           0
C            Characters in reserved records :           0
C            Number of comment records      :           0
C            Characters in comment area     :           0
C            Number of first free record    :           3
C            Last logical character address :           0
C            Last logical d.p. address      :           0
C            Last logical integer address   :           0
C            Last character descriptor      :           0
C            Last d.p descriptor            :           0
C            Last integer descriptor        :           0
C            Character word position in desc:           0
C            d.p. word position in desc     :           0
C            Integer word position in desc  :           0
C
C         Summary after adding data:
C            Number of reserved records     :           0
C            Characters in reserved records :           0
C            Number of comment records      :           0
C            Characters in comment area     :           0
C            Number of first free record    :           6
C            Last logical character address :           4
C            Last logical d.p. address      :           5
C            Last logical integer address   :          10
C            Last character descriptor      :           2
C            Last d.p descriptor            :           2
C            Last integer descriptor        :           2
C            Character word position in desc:          10
C            d.p. word position in desc     :          11
C            Integer word position in desc  :          12
C
C         Integer data in the DAS file:
C                       1
C                       2
C                       3
C                       4
C                       5
C                       6
C                       7
C                       8
C                       9
C                      10
C
C         Double precision data in the DAS file:
C               1.0000000000000000
C               2.0000000000000000
C               3.0000000000000000
C               4.0000000000000000
C               5.0000000000000000
C
C         Character data in the DAS file:
C            SP
C            UD
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
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.0.2, 19-JUL-2021 (JDR)
C
C        Updated the header to comply with NAIF standard. Added
C        complete code example.
C
C        Described the public parameters used together with this module
C        in the $Parameters section.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT)
C
C-&
 
 
C$ Index_Entries
C
C     return the file summary of a DAS file
C     find the amount of data in a DAS file
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASHFS' )
      END IF
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
 
      FINDEX  =  FTHEAD
      FOUND   = .FALSE.
 
      DO WHILE (  ( .NOT. FOUND )  .AND.  ( FINDEX .GT. 0 )  )
 
         IF ( FTHAN(FINDEX) .EQ. HANDLE ) THEN
            FOUND = .TRUE.
         ELSE
            FINDEX = LNKNXT ( FINDEX, POOL )
         END IF
 
      END DO
 
 
      IF ( FOUND ) THEN
C
C        Give the caller the current summary from the file table.
C
         NRESVR  =  FTSUM ( RRCIDX, FINDEX )
         NRESVC  =  FTSUM ( RCHIDX, FINDEX )
         NCOMR   =  FTSUM ( CRCIDX, FINDEX )
         NCOMC   =  FTSUM ( CCHIDX, FINDEX )
         FREE    =  FTSUM ( FREIDX, FINDEX )
 
         DO I = 1, 3
            LASTLA(I)  =  FTSUM ( LLABAS + I,  FINDEX )
            LASTRC(I)  =  FTSUM ( LRCBAS + I,  FINDEX )
            LASTWD(I)  =  FTSUM ( LWDBAS + I,  FINDEX )
         END DO
 
      ELSE
         CALL SETMSG ( 'There is no DAS file open with handle = #' )
         CALL ERRINT ( '#', HANDLE                                 )
         CALL SIGERR ( 'SPICE(DASNOSUCHHANDLE)'                    )
      END IF
 
      CALL CHKOUT ( 'DASHFS' )
      RETURN
 
 
C$Procedure DASUFS ( DAS, update file summary )
 
      ENTRY DASUFS ( HANDLE,
     .               NRESVR,
     .               NRESVC,
     .               NCOMR,
     .               NCOMC,
     .               FREE,
     .               LASTLA,
     .               LASTRC,
     .               LASTWD )
 
C$ Abstract
C
C     Update the file summary in a specified DAS file.
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
C     CONVERSION
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               NRESVR
C     INTEGER               NRESVC
C     INTEGER               NCOMR
C     INTEGER               NCOMC
C     INTEGER               FREE
C     INTEGER               LASTLA ( 3 )
C     INTEGER               LASTRC ( 3 )
C     INTEGER               LASTWD ( 3 )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of an open DAS file.
C     NRESVR     I   Number of reserved records in file.
C     NRESVC     I   Number of characters in use in reserved rec. area.
C     NCOMR      I   Number of comment records in file.
C     NCOMC      I   Number of characters in use in comment area.
C     FREE       I   Number of first free record.
C     LASTLA     I   Array of last logical addresses for each data type.
C     LASTRC     I   Record number of last descriptor of each data type.
C     LASTWD     I   Word number of last descriptor of each data type.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of a previously opened DAS file.
C
C     NRESVR   is the number of reserved records in a specified DAS
C              file.
C
C     NRESVC   is the number of characters in use in the reserved
C              record area of a specified DAS file.
C
C     NCOMR    is the number of comment records in a specified DAS
C              file.
C
C     NCOMC    is the number of characters in use in the comment area
C              of a specified DAS file.
C
C     FREE     is the Fortran record number of the first free record
C              in a specified DAS file.
C
C     LASTLA   is an array containing the highest current logical
C              addresses, in the specified DAS file, of data of
C              character, double precision, and integer types, in
C              that order.
C
C     LASTRC   is an array containing the Fortran record numbers, in
C              the specified DAS file, of the directory records
C              containing the current last descriptors of clusters
C              of character, double precision, and integer data
C              records, in that order.
C
C     LASTWD   is an array containing the word positions, in the
C              specified DAS file, of the current last descriptors
C              of clusters of character, double precision, and
C              integer data records, in that order.
C
C$ Detailed_Output
C
C     None. See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified handle does not belong to any file that is
C         currently known to be open, the error SPICE(DASNOSUCHHANDLE)
C         is signaled.
C
C     2)  If the specified handle is not open for WRITE access, the
C         error SPICE(DASINVALIDACCESS) is signaled.
C
C     3)  If this routine's attempts to read the DAS file record
C         fail before an update, the error SPICE(DASREADFAIL) is
C         signaled.
C
C     4)  If the attempt to write to the DAS file record fails, the
C         error SPICE(DASWRITEFAIL) is signaled.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     The quantities NRESVR, NRESRC, NCOMR, NCOMC, FREE, LASTLA,
C     LASTRC, and LASTWD define the `state' of a DAS file, and in
C     particular the state of the directory structure of the file.
C     These quantities should normally be updated only by DAS routines.
C
C     The higher-level DAS routines that affect a DAS file's summary,
C     such as
C
C        DASADx
C        DASUDx
C        DASARR
C
C     automatically update the file summary, so there is no need for
C     the calling program to perform the update explicitly.
C
C$ Examples
C
C     1)  Update the last d.p. logical address for a DAS file, leaving
C         the rest of the file summary intact.
C
C            C
C            C     Read the file summary.
C            C
C                  CALL DASHFS ( HANDLE,
C                 .              NRESVR,
C                 .              RRESVC,
C                 .              NCOMR,
C                 .              NCOMC,
C                 .              FREE,
C                 .              LASTLA,
C                 .              LASTRC,
C                 .              LASTWD )
C
C            C
C            C     Update the d.p. component of the `last logical
C            C     address' array.
C            C
C                  LASTLA(DP) = NEWVAL
C
C                  CALL DASUFS ( HANDLE,
C                 .              NRESVR,
C                 .              RRESVC,
C                 .              NCOMR,
C                 .              NCOMC,
C                 .              FREE,
C                 .              LASTLA,
C                 .              LASTRC,
C                 .              LASTWD )
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
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C
C$ Version
C
C-    SPICELIB Version 7.1.1, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 7.1.0, 07-OCT-2015 (NJB)
C
C        Corrected call to ZZDDHCLS.
C
C-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C
C-    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB)
C
C        Bug fix: file name is now correctly inserted into long
C        error message generated when target file is not open for
C        write access.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.0.0, 15-OCT-2001 (FST) (NJB)
C
C        Bug fix: this routine now reads the file record
C        before attempting to update it. The buffered values
C        of IDWORD and IFN are no longer present.
C
C        Bug fix: missing call to CHKIN was added to an error
C        handling branch in entry point DASUFS. This call is
C        required because DASUFS uses discovery check-in.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT)
C
C-&
 
 
C$ Index_Entries
C
C     update the file summary of a DAS file
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB)
C
C        Bug fix: file name is now correctly inserted into long
C        error message generated when target file is not open for
C        write access.
C
C-    SPICELIB Version 5.1.0, 15-OCT-2001 (NJB)
C
C        Bug fix: missing call to CHKIN was added to an error
C        handling branch in entry point DASUFS. This call is
C        required because DASUFS uses discovery check-in.
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 30-JUL-1992 (NJB) (WLT)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DASUFS' )
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
C
C     Find the file table entries for this file.
C
      FINDEX  =  FTHEAD
      FOUND   = .FALSE.
 
      DO WHILE (  ( .NOT. FOUND )  .AND.  ( FINDEX .GT. 0 )  )
 
         IF ( FTHAN(FINDEX) .EQ. HANDLE ) THEN
            FOUND  = .TRUE.
         ELSE
            FINDEX = LNKNXT ( FINDEX, POOL )
         END IF
 
      END DO
 
      IF ( FOUND ) THEN
C
C        Obtain a logical unit for this file.
C
         CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., NUMBER )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DASUFS' )
            RETURN
         END IF
 
C
C        Now check to see that HANDLE is open for write, as one has
C        no business updating a file summary for files that are
C        open for read access only.
C
         IF ( FTACC(FINDEX) .NE. WRITE ) THEN
 
            CALL SETMSG ( 'DAS file not open for writing. Handle = #,'
     .      //            ' file = ''#''.'                             )
            CALL ERRINT ( '#', HANDLE                                  )
            CALL ERRFNM ( '#', NUMBER                                  )
            CALL SIGERR ( 'SPICE(DASINVALIDACCESS)'                    )
            CALL CHKOUT ( 'DASUFS'                                     )
            RETURN
 
         END IF
 
C
C        If any of the counts pertaining to the reserved record are or
C        the comment area were changed, we need to record the new
C        counts in the file record.  Otherwise, leave the file alone.
C
         IF (      (  NRESVR .NE. FTSUM( RRCIDX, FINDEX )  )
     .        .OR. (  NRESVC .NE. FTSUM( RCHIDX, FINDEX )  )
     .        .OR. (  NCOMR  .NE. FTSUM( CRCIDX, FINDEX )  )
     .        .OR. (  NCOMC  .NE. FTSUM( CCHIDX, FINDEX )  )   )  THEN
 
C
C           Read the file record.
C
            READ ( NUMBER,
     .             REC    = 1,
     .             IOSTAT = IOSTAT )  IDWORD,
     .                                LOCIFN,
     .                                LOCRRC,
     .                                LOCRCH,
     .                                LOCCRC,
     .                                LOCCCH,
     .                                LOCFMT,
     .                                TAIL
 
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL SETMSG ( 'Attempt to read file record failed. '
     .         //            'File was ''#''.  Value of IOSTAT was'
     .         //            ' ''#''.'                              )
               CALL ERRFNM ( '#', NUMBER                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(DASREADFAIL)'                   )
               CALL CHKOUT ( 'DASUFS'                               )
               RETURN
 
            END IF
 
            WRITE ( NUMBER,
     .              REC     =  1,
     .              IOSTAT  =  IOSTAT )  IDWORD,
     .                                   LOCIFN,
     .                                   NRESVR,
     .                                   NRESVC,
     .                                   NCOMR,
     .                                   NCOMC,
     .                                   LOCFMT,
     .                                   TAIL
 
            IF ( IOSTAT .NE. 0 ) THEN
C
C              Try to obtain the DAS file's name.
C
               INQUIRE ( UNIT   = NUMBER,
     .                   NAME   = LOCDAS,
     .                   IOSTAT = INQSTA )
 
               CALL ZZDDHCLS ( HANDLE, 'DAS', .FALSE. )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'DASUFS' )
                  RETURN
               END IF
 
               CALL SETMSG ( 'Attempt to update file record failed. '
     .         //            'File was ''#''.  Value of IOSTAT was'
     .         //             ' ''#''.'                                )
               CALL ERRCH  ( '#', LOCDAS                               )
               CALL ERRINT ( '#', IOSTAT                               )
               CALL SIGERR ( 'SPICE(DASWRITEFAIL)'                     )
               CALL CHKOUT ( 'DASUFS'                                  )
               RETURN
 
            END IF
 
         END IF
 
C
C        Update the file table.
C
         FTSUM ( RRCIDX, FINDEX )  =  NRESVR
         FTSUM ( RCHIDX, FINDEX )  =  NRESVC
         FTSUM ( CRCIDX, FINDEX )  =  NCOMR
         FTSUM ( CCHIDX, FINDEX )  =  NCOMC
         FTSUM ( FREIDX, FINDEX )  =  FREE
 
         DO I = 1, 3
            FTSUM ( LLABAS + I,  FINDEX )  =  LASTLA(I)
            FTSUM ( LRCBAS + I,  FINDEX )  =  LASTRC(I)
            FTSUM ( LWDBAS + I,  FINDEX )  =  LASTWD(I)
         END DO
 
      ELSE
 
         CALL SETMSG ( 'There is no file open with handle = #' )
         CALL ERRINT ( '#', HANDLE                             )
         CALL SIGERR ( 'SPICE(DASNOSUCHHANDLE)'                )
         CALL CHKOUT ( 'DASUFS'                                )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'DASUFS' )
      RETURN
 
 
 
C$Procedure DASHLU ( DAS, handle to logical unit )
 
      ENTRY DASHLU ( HANDLE, UNIT )
 
C$ Abstract
C
C     Return the logical unit associated with a handle. The unit
C     is "locked" to the handle by the DAF/DAS handle manager
C     subsystem.
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
C     CONVERSION
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     INTEGER               UNIT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a DAS file.
C     UNIT       O   Corresponding logical unit.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of a previously opened DAS file.
C
C$ Detailed_Output
C
C     UNIT     is the Fortran logical unit to which the file is
C              connected.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified handle does not belong to any file that is
C         currently known to be open, the error SPICE(DASNOSUCHHANDLE)
C         is signaled.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine is a utility used by the DAS system to support
C     file I/O. DASHLU may also prove useful to general SPICELIB
C     users for constructing error messages.
C
C$ Examples
C
C     1)  Obtain the logical unit associated with a DAS file having
C         a known handle.
C
C            CALL DASHLU ( HANDLE, UNIT )
C
C$ Restrictions
C
C     1)  Successfully invoking this module has the side effect of
C         locking UNIT to HANDLE. This 'lock' guarantees until
C         HANDLE is closed (or unlocked) that the file associated
C         with HANDLE is always open and attached to logical unit
C         UNIT. To unlock a handle without closing the file, use
C         ZZDDHUNL, an entry point in the handle manager umbrella,
C         ZZDDHMAN.
C
C         The system can lock at most UTSIZE-SCRUNT-RSVUNT
C         simultaneously (see the include file 'zzddhman.inc' for
C         specific values of these parameters), but unnecessarily
C         locking handles to their logical units may cause performance
C         degradation. The handle manager will have fewer logical
C         units to utilize when disconnecting and reconnecting
C         loaded files.
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
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 7.0.0, 04-FEB-2015 (NJB) (FST)
C
C        Now uses DAF/DAS handle manager subsystem.
C        Note that this routine is now considered obsolete.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     map DAS file handle to logical unit
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C
C     We use discovery check-ins in this routine.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL CHKIN  ( 'DASHLU'       )
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         CALL CHKOUT ( 'DASHLU'       )
 
         PASS1 = .FALSE.
 
      END IF
 
C
C     Find the file table entries for this file.
C
      FINDEX  =  FTHEAD
      FOUND   = .FALSE.
 
      DO WHILE (  ( .NOT. FOUND )  .AND.  ( FINDEX .GT. 0 )  )
 
         IF ( FTHAN(FINDEX) .EQ. HANDLE ) THEN
            FOUND = .TRUE.
         ELSE
            FINDEX = LNKNXT ( FINDEX, POOL )
         END IF
 
      END DO
 
 
      IF ( FOUND ) THEN
C
C        For backward compatibility, the logical unit must be
C        locked to the file.
C
         CALL ZZDDHHLU ( HANDLE, 'DAS', .TRUE., UNIT )
 
      ELSE
 
         CALL CHKIN  ( 'DASHLU'                                )
         CALL SETMSG ( 'There is no file open with handle = #' )
         CALL ERRINT ( '#', HANDLE                             )
         CALL SIGERR ( 'SPICE(DASNOSUCHHANDLE)'                )
         CALL CHKOUT ( 'DASHLU'                                )
 
      END IF
 
      RETURN
 
 
 
C$Procedure DASLUH ( DAS, logical unit to handle )
 
      ENTRY DASLUH ( UNIT, HANDLE )
 
C$ Abstract
C
C     Return the handle associated with a logical unit.
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
C     CONVERSION
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               UNIT
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       I   Logical unit connected to a DAS file.
C     HANDLE     O   Corresponding handle.
C
C$ Detailed_Input
C
C     UNIT     is the logical unit to which a DAS file has been
C              connected when it was opened.
C
C$ Detailed_Output
C
C     HANDLE   is the handle associated with the file.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified unit is not connected to any DAS file that is
C         currently known to be open, the error SPICE(DASNOSUCHUNIT)
C         is signaled.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     It is unlikely, but possible, that a calling program would know
C     the logical unit to which a file is connected without knowing the
C     handle associated with the file. DASLUH is provided mostly for
C     completeness.
C
C$ Examples
C
C     In the following code fragment, the handle associated with
C     a DAS file is retrieved using the logical unit to which the
C     file is connected. The handle is then used to determine the
C     name of the file.
C
C        CALL DASLUH ( UNIT,   HANDLE )
C        CALL DASHFN ( HANDLE, FNAME  )
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
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C        Note that this routine is now considered obsolete.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     map logical unit to DAS file handle
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DASLUH' )
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
C
C     Try to locate the handle associated with this unit.
C
      CALL ZZDDHLUH ( UNIT, HANDLE, FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'There is no DAS file open with unit = #' )
         CALL ERRINT ( '#', UNIT                                 )
         CALL SIGERR ( 'SPICE(DASNOSUCHUNIT)'                    )
 
      END IF
 
      CALL CHKOUT ( 'DASLUH' )
      RETURN
 
 
 
C$Procedure DASHFN ( DAS, handle to file name )
 
      ENTRY DASHFN ( HANDLE, FNAME )
 
C$ Abstract
C
C     Return the name of the DAS file associated with a handle.
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
C     CONVERSION
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               HANDLE
C     CHARACTER*(*)         FNAME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a DAS file.
C     FNAME      O   Corresponding file name.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of a previously opened DAS file.
C
C$ Detailed_Output
C
C     FNAME    is the name of the DAS file associated with the input
C              file handle.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified handle does not belong to any file that is
C         currently known to be open, the error SPICE(DASNOSUCHHANDLE)
C         is signaled.
C
C     2)  If FNAME string is too short to contain the output string,
C         the name is truncated on the right.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     It may be desirable to recover the names of one or more DAS
C     files in a different part of the program from the one in which
C     they were opened. Note that the names returned by DASHFN may
C     not be identical to the names used to open the files. Under
C     most operating systems, a particular file can be accessed using
C     many different names. DASHFN returns one of them.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) In the following program, the name of a DAS file is
C        recovered using the handle associated with the file.
C
C
C        Use the DSK kernel below as input DAS file for the example.
C
C           phobos512.bds
C
C
C        Example code begins here.
C
C
C              PROGRAM DASHFN_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               RTRIM
C
C        C
C        C     Local constants
C        C
C              CHARACTER*(*)         DAS
C              PARAMETER           ( DAS    = 'phobos512.bds' )
C
C              INTEGER               FILSIZ
C              PARAMETER           ( FILSIZ = 256 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(FILSIZ)    FNAME
C
C              INTEGER               HANDLE
C
C        C
C        C     Open the DAS file for read access.
C        C
C              CALL DASOPR ( DAS, HANDLE )
C
C        C
C        C     Map the handle to a file name.
C        C
C              CALL DASHFN ( HANDLE, FNAME )
C
C              WRITE(*,*) 'DAS file name = <', FNAME(:RTRIM(FNAME)),
C             .           '>.'
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         DAS file name = <phobos512.bds>.
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example. Extended $Exceptions section to describe what
C        happens in case of output string being too short.
C
C-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C        Note that this routine is now considered obsolete.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     map DAS handle to file name
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASHFN' )
      END IF
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
C
C     Find the file table entries for this file.
C
      FINDEX  =  FTHEAD
      FOUND   = .FALSE.
 
      DO WHILE (  ( .NOT. FOUND )  .AND.  ( FINDEX .GT. 0 )  )
 
         IF ( FTHAN(FINDEX) .EQ. HANDLE ) THEN
            FOUND  = .TRUE.
         ELSE
            FINDEX = LNKNXT ( FINDEX, POOL )
         END IF
 
      END DO
 
      IF ( FOUND ) THEN
 
         CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., NUMBER )
 
         INQUIRE ( UNIT = NUMBER,
     .             NAME = FNAME  )
 
      ELSE
 
         CALL SETMSG ( 'There is no DAS file open with handle = #' )
         CALL ERRINT ( '#', HANDLE                                 )
         CALL SIGERR ( 'SPICE(DASNOSUCHHANDLE)'                    )
 
      END IF
 
      CALL CHKOUT ( 'DASHFN' )
      RETURN
 
 
C$Procedure DASFNH ( DAS, file name to handle )
 
      ENTRY DASFNH ( FNAME, HANDLE )
 
C$ Abstract
C
C     Return handle associated with a file name.
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
C     CONVERSION
C     DAS
C     FILES
C
C$ Declarations
C
C     CHARACTER*(*)         FNAME
C     INTEGER               HANDLE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of a DAS file.
C     HANDLE     O   Corresponding handle.
C
C$ Detailed_Input
C
C     FNAME    is the name of a previously opened DAS file.
C
C$ Detailed_Output
C
C     HANDLE   is the handle associated with the file.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified name does not specify any DAS file currently
C         known to be open, the error SPICE(DASNOSUCHFILE) is signaled.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     It is sometimes easier to work with file names (which are
C     meaningful, and often predictable) than with file handles
C     (which are neither), especially in interactive situations.
C     However, nearly every DAS routine requires that you use file
C     handles to refer to files. DASFNH is provided to bridge the gap
C     between the two representations.
C
C$ Examples
C
C     In the following code fragment, the handle associated with a
C     DAS file is recovered using the name of the file.
C
C        CALL DASOPR ( 'sample.DAS', HANDLE )
C         .
C         .
C
C        CALL DASFNH ( 'sample.DAS', HANDLE )
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
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 7.0.1, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 7.0.0, 30-JUL-2014 (NJB)
C
C        Now uses DAF/DAS handle manager subsystem.
C        Note that this routine is now considered obsolete.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     map file name to DAS handle
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DASFNH' )
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
      CALL ZZDDHFNH ( FNAME, HANDLE, FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'There is no DAS file in the table with file ' //
     .                 'name = ''#'''                                  )
         CALL ERRCH  ( '#', FNAME                                      )
         CALL SIGERR ( 'SPICE(DASNOSUCHFILE)'                          )
 
      END IF
 
      CALL CHKOUT ( 'DASFNH' )
      RETURN
 
 
C$Procedure DASHOF ( DAS, handles of open files )
 
      ENTRY DASHOF ( FHSET )
 
C$ Abstract
C
C     Return a SPICE set containing the handles of all currently
C     open DAS files.
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
C     SETS
C
C$ Keywords
C
C     DAS
C     FILES
C
C$ Declarations
C
C     INTEGER               LBCELL
C     PARAMETER           ( LBCELL = -5 )
C
C     INTEGER               FHSET ( LBCELL : * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FHSET      O   A set containing handles of currently open DAS
C                    files.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     FHSET    is a SPICE set containing the file handles of
C              all currently open DAS files.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the set FHSET is not initialized, an error is signaled by a
C         routine in the call tree of this routine.
C
C     2)  If the set FHSET is too small to accommodate the set of
C         handles to be returned, an error is signaled by a routine in
C         the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows subroutines to test DAS file handles for
C     validity before using them. Many DAS operations that rely on
C     handles to identify DAS files cause errors to be signaled if
C     the handles are invalid.
C
C$ Examples
C
C     1)  Find out how may DAS files are open for writing.
C
C            C
C            C    Find out which DAS files are open.
C            C
C                 CALL DASHOF  ( FHSET )
C
C            C
C            C    Count the ones open for writing.
C            C
C                 COUNT = 0
C
C                 DO I = 1, CARDC(FHSET)
C
C                    CALL DASHAM ( FHSET(I), METHOD )
C
C                    IF ( METHOD .EQ. WRITE ) THEN
C                       COUNT = COUNT + 1
C                    END IF
C
C                 END DO
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
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.0.2, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     return set of handles of open DAS files
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASHOF' )
      END IF
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
C
C     Just stuff our local list into the set.
C
      CALL COPYI ( FHLIST, FHSET )
 
      CALL CHKOUT ( 'DASHOF' )
      RETURN
 
 
 
C$Procedure DASSIH ( DAS, signal invalid handles )
 
      ENTRY DASSIH ( HANDLE, ACCESS )
 
C$ Abstract
C
C     Signal an error if a DAS file file handle does not designate a
C     DAS file that is open for a specified type of access.
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
C     ERROR
C     SETS
C
C$ Keywords
C
C     DAS
C     FILES
C     UTILITY
C
C$ Declarations
C
C     INTEGER               HANDLE
C     CHARACTER*(*)         ACCESS
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   HANDLE to be validated.
C     ACCESS     I   String indicating access type.
C
C$ Detailed_Input
C
C     HANDLE   is a DAS file handle to validate. For HANDLE to be
C              considered valid, it must specify a DAS file that
C              is open for the type of access specified by the
C              input argument ACCESS.
C
C
C     ACCESS   is a string indicating the type of access that
C              the DAS file specified by the input argument HANDLE
C              must be open for. The values of ACCESS may be
C
C                 'READ'      File must be open for read access
C                             by DAS routines. DAS files opened
C                             for read or write access may be
C                             read.
C
C                 'WRITE'     File must be open for write access
C                             by DAS routines. Note that files
C                             open for write access may be read as
C                             well as written.
C
C              Leading and trailing blanks in ACCESS are ignored,
C              and case is not significant.
C
C$ Detailed_Output
C
C     None. See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input argument ACCESS has an unrecognized value,
C         the error SPICE(INVALIDOPTION) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine signals the error SPICE(DASINVALIDACCESS) if the
C     DAS designated by the input argument HANDLE is not open
C     for the specified type of access. If HANDLE does not designate
C     an open DAS file, the error SPICE(DASNOSUCHHANDLE) is signaled.
C
C     This routine allows subroutines to test file handles for
C     validity before attempting to access the files they designate,
C     or before performing operations on the handles themselves, such
C     as finding the name of the file designated by a handle. This
C     routine should be used in situations where the appropriate action
C     to take upon determining that a handle is invalid is to signal
C     an error. DASSIH centralizes the error response for this type of
C     error in a single routine.
C
C     In cases where it is necessary to determine the validity of a
C     file handle, but it is not an error for the handle to refer
C     to a closed file, the entry point DASHOF should be used instead
C     of DASSIH.
C
C$ Examples
C
C     1)  Make sure that a file handle designates a DAS file that can
C         be read. Signal an error if not.
C
C         Note that if a DAS file is open for reading or writing, read
C         access is allowed.
C
C                  CALL DASSIH ( HANDLE, 'READ' )
C
C                  IF ( FAILED() ) THEN
C                     RETURN
C                  END IF
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
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.1.1, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB)
C
C        Local variable DAS was renamed to DASFIL. This
C        was done to avoid future conflict with parameters
C        in zzddhman.inc.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     detect invalid DAS handles
C     validate DAS handles
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 6.1.0, 26-SEP-2005 (NJB)
C
C        Local variable DAS was renamed to DASFIL. This
C        was done to avoid future conflict with parameters
C        in zzddhman.inc.
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if these open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) (IMU)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DASSIH' )
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
C
C     Get an upper case, left-justified copy of ACCESS.
C
      CALL LJUST ( ACCESS, ACC )
      CALL UCASE ( ACC,    ACC )
 
C
C     Make sure we recognize the access type specified by the caller.
C
      IF (  ( ACC .NE. 'READ' ) .AND. ( ACC .NE. 'WRITE' )  ) THEN
 
         CALL SETMSG ( 'Unrecognized access type.  Type was #. ' )
         CALL ERRCH  ( '#', ACCESS                               )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'                    )
         CALL CHKOUT ( 'DASSIH'                                  )
         RETURN
 
      END IF
 
C
C     See whether the input handle is in our list at all.  It's
C     unlawful for the handle to be absent.
C
      IF (  .NOT.  ELEMI ( HANDLE, FHLIST )  )  THEN
 
         CALL SETMSG ( 'Handle # is not attached to an open DAS file.' )
         CALL ERRINT ( '#', HANDLE                                     )
         CALL SIGERR ( 'SPICE(DASNOSUCHHANDLE)'                        )
         CALL CHKOUT ( 'DASSIH'                                        )
         RETURN
 
      ELSE
C
C        Find the file table entries for this file.  We know they
C        must exist.
C
         FINDEX  =  FTHEAD
         FOUND   = .FALSE.
 
         DO WHILE (  ( .NOT. FOUND )  .AND.  ( FINDEX .GT. 0 )  )
 
            IF ( FTHAN(FINDEX) .EQ. HANDLE ) THEN
               FOUND  = .TRUE.
            ELSE
               FINDEX = LNKNXT ( FINDEX, POOL )
            END IF
 
         END DO
C
C        At this point, FINDEX points to the file table entries
C        for this file.
C
 
         IF (       ( ACC           .EQ. 'WRITE' )
     .        .AND. ( FTACC(FINDEX) .NE.  WRITE  )  ) THEN
C
C           If the access type is 'WRITE', the DAS file must be open
C           for writing.
C
            CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., NUMBER )
 
            CALL SETMSG ( 'DAS file not open for writing. Handle = #,'//
     .                    ' file = ''#''.'                             )
            CALL ERRINT ( '#', HANDLE                                  )
            CALL ERRFNM ( '#', NUMBER                                  )
            CALL SIGERR ( 'SPICE(DASINVALIDACCESS)'                    )
            CALL CHKOUT ( 'DASSIH'                                     )
            RETURN
 
         END IF
 
      END IF
 
C
C     The DAS file's handle is o.k.
C
      CALL CHKOUT ( 'DASSIH' )
      RETURN
 
 
C$Procedure DASHAM ( DAS, handle to access method )
 
      ENTRY DASHAM ( HANDLE, ACCESS )
 
C$ Abstract
C
C     Return the allowed access method for a specified DAS file.
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
C     DAS
C     FILES
C     UTILITY
C
C$ Declarations
C
C     INTEGER               HANDLE
C     CHARACTER*(*)         ACCESS
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   HANDLE of a DAS file.
C     ACCESS     O   String indicating allowed access method.
C
C$ Detailed_Input
C
C     HANDLE   is the handle of a previously opened DAS file.
C
C$ Detailed_Output
C
C     ACCESS   is a string indicating the type of access that
C              the DAS file specified by the input argument HANDLE
C              is open for. The values of ACCESS may be
C
C                 'READ'      File is open for read access by DAS
C                             routines. Both the data area and
C                             the comment area may be read. The
C                             file may not be modified.
C
C                 'WRITE'     File is open for write access by
C                             DAS routines. Files open for
C                             write access may be read as well as
C                             written.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input handle is invalid, the error SPICE(INVALIDHANDLE)
C         is signaled. ACCESS is not modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows subroutines to determine the access methods
C     allowed for a given DAS file.
C
C$ Examples
C
C     1)  Make sure that a file handle designates a DAS file that can
C         be read. Signal an error if not.
C
C         Note that if a DAS file is open for reading or writing, read
C         access is allowed.
C
C                  CALL DASHAM ( HANDLE, 'READ' )
C
C                  IF ( FAILED() ) THEN
C                     RETURN
C                  END IF
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
C     K.R. Gehringer     (JPL)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.0.2, 19-JUL-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 6.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 5.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.3, 16-SEP-1999 (NJB)
C
C        CSPICE environments were added. Some typos were corrected.
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitly given. Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input and $ Output sections of the header. This was
C        done in order to minimize documentation changes if these open
C        routines ever change.
C
C-    SPICELIB Version 1.0.0, 01-FEB-1993 (NJB) (WLT) (IMU)
C
C-&
 
 
C$ Index_Entries
C
C     return allowed access methods for DAS files
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 01-NOV-1993 (KRG)
C
C        Removed references to specific DAS file open routines in the
C        $Detailed_Input and $ Output sections of the header. This was
C        done in order to minimize documentation changes if these open
C        routines ever change.
C
C-    SPICELIB Version 1.0.0, 01-FEB-1993 (NJB) (WLT) (IMU)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'DASHAM' )
 
C
C     Initialize the file table pool and handle list, if necessary.
C
      IF ( PASS1 ) THEN
 
         CALL LNKINI ( FTSIZE, POOL   )
         CALL SSIZEI ( FTSIZE, FHLIST )
 
         PASS1 = .FALSE.
 
      END IF
 
C
C     See whether the input handle is in our list at all.  It's
C     unlawful for the handle to be absent.
C
      FINDEX  =  FTHEAD
      FOUND   = .FALSE.
 
      DO WHILE (  ( .NOT. FOUND )  .AND.  ( FINDEX .GT. 0 )  )
 
         IF ( FTHAN(FINDEX) .EQ. HANDLE ) THEN
            FOUND  = .TRUE.
         ELSE
            FINDEX = LNKNXT ( FINDEX, POOL )
         END IF
 
      END DO
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The handle # does not designate a known DAS ' //
     .                 'file '                                         )
         CALL ERRINT ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(INVALIDHANDLE)'                          )
         CALL CHKOUT ( 'DASHAM'                                        )
         RETURN
 
      END IF
 
C
C     We know about the file if we got this far.  Set the output
C     argument accordingly.
C
      IF (  FTACC(FINDEX) .EQ. READ ) THEN
         ACCESS = 'READ'
      ELSE
         ACCESS = 'WRITE'
      END IF
 
      CALL CHKOUT ( 'DASHAM' )
      RETURN
      END
