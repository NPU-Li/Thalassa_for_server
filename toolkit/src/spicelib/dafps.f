C$Procedure DAFPS ( DAF, pack summary )

      SUBROUTINE DAFPS ( ND, NI, DC, IC, SUM )

C$ Abstract
C
C     Pack (assemble) an array summary from its double precision and
C     integer components.
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
C
C$ Keywords
C
C     CONVERSION
C     FILES
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               ND
      INTEGER               NI
      DOUBLE PRECISION      DC       ( * )
      INTEGER               IC       ( * )
      DOUBLE PRECISION      SUM      ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ND         I   Number of double precision components.
C     NI         I   Number of integer components.
C     DC         I   Double precision components.
C     IC         I   Integer components.
C     SUM        O   Array summary.
C
C$ Detailed_Input
C
C     ND       is the number of double precision components in
C              the summary to be packed.
C
C     NI       is the number of integer components in the summary.
C
C     DC       are the double precision components of the summary.
C
C     IC       are the integer components of the summary.
C
C$ Detailed_Output
C
C     SUM      is an array summary containing the components in DC
C              and IC. This identifies the contents and location of
C              a single array within a DAF.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If ND is zero or negative, no DP components are stored.
C
C     2)  If NI is zero or negative, no integer components are stored.
C
C     3)  If the total size of the summary is greater than 125 double
C         precision words, some components may not be stored. See
C         $Particulars for details.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The components of array summaries are packed into double
C     precision arrays for reasons outlined in [1]. Two routines,
C     DAFPS (pack summary) and DAFUS (unpack summary) are provided
C     for packing and unpacking summaries.
C
C     The total size of the summary is
C
C             (NI - 1)
C        ND + -------- + 1
C                 2
C
C     double precision words (where ND, NI are nonnegative).
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Replace the body ID code 301 (Moon) with a test body ID,
C        e.g. -999, in every descriptor of an SPK file.
C
C
C        Example code begins here.
C
C
C              PROGRAM DAFPS_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              INTEGER               CARDI
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5   )
C
C              INTEGER               DSCSIZ
C              PARAMETER           ( DSCSIZ = 5    )
C
C              INTEGER               FILSIZ
C              PARAMETER           ( FILSIZ = 256  )
C
C              INTEGER               MAXOBJ
C              PARAMETER           ( MAXOBJ = 1000 )
C
C              INTEGER               ND
C              PARAMETER           ( ND     = 2    )
C
C              INTEGER               NI
C              PARAMETER           ( NI     = 6    )
C
C              INTEGER               NEWCOD
C              PARAMETER           ( NEWCOD = -999 )
C
C              INTEGER               OLDCOD
C              PARAMETER           ( OLDCOD =  301 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(FILSIZ)    FNAME
C
C              DOUBLE PRECISION      DC     ( ND )
C              DOUBLE PRECISION      SUM    ( DSCSIZ )
C
C              INTEGER               HANDLE
C              INTEGER               I
C              INTEGER               IC     ( NI )
C              INTEGER               IDS    ( LBCELL : MAXOBJ )
C
C              LOGICAL               FOUND
C
C        C
C        C     Get the SPK file name.
C        C
C              CALL PROMPT ( 'Enter name of the SPK file > ', FNAME )
C
C        C
C        C     Initialize the set IDS.
C        C
C              CALL SSIZEI ( MAXOBJ, IDS )
C
C        C
C        C     Open for writing the SPK file.
C        C
C              CALL DAFOPW ( FNAME, HANDLE )
C
C        C
C        C     Search the file in forward order.
C        C
C              CALL DAFBFS ( HANDLE )
C              CALL DAFFNA ( FOUND )
C
C              DO WHILE ( FOUND )
C
C        C
C        C        Fetch and unpack the descriptor (aka summary)
C        C        of the current segment.
C        C
C                 CALL DAFGS ( SUM )
C                 CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C        C
C        C        Replace ID codes if necessary.
C        C
C                 IF ( IC(1) .EQ. OLDCOD ) THEN
C
C                    IC(1) = NEWCOD
C
C                 END IF
C
C                 IF ( IC(2) .EQ. OLDCOD ) THEN
C
C                    IC(2) = NEWCOD
C
C                 END IF
C
C        C
C        C        Re-pack the descriptor; replace the descriptor
C        C        in the file.
C        C
C                 CALL DAFPS ( ND, NI, DC, IC, SUM )
C                 CALL DAFRS ( SUM )
C
C        C
C        C        Find the next segment.
C        C
C                 CALL DAFFNA ( FOUND )
C
C              END DO
C
C        C
C        C     Close the file.
C        C
C              CALL DAFCLS ( HANDLE )
C
C        C
C        C     Find the set of objects in the SPK file.
C        C
C              CALL SPKOBJ ( FNAME, IDS )
C
C              WRITE(*,'(A)') 'Objects in the DAF file:'
C              WRITE(*,*) ' '
C              WRITE(*,'(20I4)') ( IDS(I), I= 1, CARDI ( IDS ) )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, using the SPK file named de430.bsp, the output was:
C
C
C        Enter name of the SPK file > de430.bsp
C        Objects in the DAF file:
C
C        -999   1   2   3   4   5   6   7   8   9  10 199 299 399
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
C     J. Diaz del Rio    (ODC Space)
C     H.A. Neilan        (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C-    SPICELIB Version 1.0.3, 10-OCT-2012 (EDW)
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C        Corrected ordering of header section.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     pack DAF summary
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION      DEQUIV   ( 125 )
      INTEGER               IEQUIV   ( 250 )

      INTEGER               N
      INTEGER               M

C
C     Equivalences
C
      EQUIVALENCE         ( DEQUIV, IEQUIV )

C
C     Here's the deal: the DP components always precede the integer
C     components, avoiding alignment problems. The DP components can
C     be stored directly.
C
      N = MIN ( 125, MAX ( 0, ND ) )

      CALL MOVED ( DC, N, SUM )

C
C     The integer components must detour through an equivalence.
C
      M = MIN ( 250 - 2 * N, MAX ( 0, NI ) )

      CALL MOVEI ( IC,          M,              IEQUIV   )
      CALL MOVED ( DEQUIV,     (M - 1) / 2 + 1, SUM(N+1) )

      RETURN



C$Procedure DAFUS ( DAF, unpack summary )

      ENTRY DAFUS ( SUM, ND, NI, DC, IC )

C$ Abstract
C
C     Unpack an array summary into its double precision and integer
C     components.
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
C
C$ Keywords
C
C     CONVERSION
C     FILES
C
C$ Declarations
C
C     DOUBLE PRECISION      SUM      ( * )
C     INTEGER               ND
C     INTEGER               NI
C     DOUBLE PRECISION      DC       ( * )
C     INTEGER               IC       ( * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SUM        I   Array summary.
C     ND         I   Number of double precision components.
C     NI         I   Number of integer components.
C     DC         O   Double precision components.
C     IC         O   Integer components.
C
C$ Detailed_Input
C
C     SUM      is an array summary. This identifies the contents and
C              location of a single array within a DAF.
C
C     ND       is the number of double precision components in
C              the summary.
C
C     NI       is the number of integer components in the summary.
C
C$ Detailed_Output
C
C     DC       are the double precision components of the summary.
C
C     IC       are the integer components of the summary.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If ND is zero or negative, no double precision components
C         are returned.
C
C     2)  If NI is zero or negative, no integer components are returned.
C
C     3)  If the total size of the summary is greater than 125 double
C         precision words, some components may not be returned.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The components of array summaries are packed into double
C     precision arrays for reasons outlined in [1]. Two routines,
C     DAFPS (pack summary) and DAFUS (unpack summary) are provided
C     for packing and unpacking summaries.
C
C     The total size of the summary is
C
C             (NI - 1)
C        ND + -------- + 1
C                 2
C
C     double precision words (where ND, NI are nonnegative).
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Use a simple routine to output the double precision and
C        integer values stored in an SPK's segments descriptors. This
C        function opens a DAF for read, performs a forwards search for
C        the DAF arrays, prints segments description for each array
C        found, then closes the DAF.
C
C        Use the SPK kernel below as input DAF file for the program.
C
C           de421.bsp
C
C
C        Example code begins here.
C
C
C              PROGRAM DAFUS_EX1
C              IMPLICIT NONE
C
C        C
C        C     Define the summary parameters appropriate
C        C     for an SPK file.
C        C
C              INTEGER               MAXSUM
C              PARAMETER           ( MAXSUM = 125 )
C
C              INTEGER               ND
C              PARAMETER           ( ND = 2       )
C
C              INTEGER               NI
C              PARAMETER           ( NI = 6       )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(32)        KERNEL
C
C              DOUBLE PRECISION      DC     ( ND     )
C              DOUBLE PRECISION      SUM    ( MAXSUM )
C
C              INTEGER               HANDLE
C              INTEGER               IC     ( NI )
C
C              LOGICAL               FOUND
C
C
C        C
C        C     Open a DAF for read. Return a HANDLE referring to the
C        C     file.
C        C
C              KERNEL = 'de421.bsp'
C              CALL DAFOPR ( KERNEL, HANDLE )
C
C        C
C        C     Begin a forward search on the file.
C        C
C              CALL DAFBFS ( HANDLE )
C
C        C
C        C     Search until a DAF array is found.
C        C
C              CALL DAFFNA ( FOUND )
C
C        C
C        C     Loop while the search finds subsequent DAF arrays.
C        C
C              DO WHILE ( FOUND )
C
C                 CALL DAFGS ( SUM )
C                 CALL DAFUS ( SUM, ND, NI, DC, IC )
C
C                 WRITE(*,*)                'Doubles:', DC(1:ND)
C                 WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
C
C        C
C        C        Check for another segment.
C        C
C                 CALL DAFFNA ( FOUND )
C
C              END DO
C
C        C
C        C     Safely close the DAF.
C        C
C              CALL DAFCLS ( HANDLE )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:        1        0        1        2      641   310404
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:        2        0        1        2   310405   423048
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:        3        0        1        2   423049   567372
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:        4        0        1        2   567373   628976
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:        5        0        1        2   628977   674740
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:        6        0        1        2   674741   715224
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:        7        0        1        2   715225   750428
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:        8        0        1        2   750429   785632
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:        9        0        1        2   785633   820836
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:       10        0        1        2   820837   944040
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:      301        3        1        2   944041  1521324
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:      399        3        1        2  1521325  2098608
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:      199        1        1        2  2098609  2098620
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:      299        2        1        2  2098621  2098632
C         Doubles:  -3169195200.0000000        1696852800.0000000
C        Integers:      499        4        1        2  2098633  2098644
C
C
C        Note, the final entries in the integer array contains the
C        segment start/end indexes. The output indicates the search
C        proceeded from the start of the file (low value index) towards
C        the end (high value index).
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
C     H.A. Neilan        (JPL)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 04-AUG-2020 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard..
C        Added undeclared variables to code example.
C
C-    SPICELIB Version 1.0.3, 10-OCT-2012 (EDW)
C
C        Added a functional code example to the $Examples section.
C
C        Removed the obsolete Reference citation to "NAIF
C        Document 167.0."
C
C        Corrected ordering of header section.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     unpack DAF summary
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 04-AUG-2020 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C-&


C
C     Just undo whatever DAFPS did.
C
      N = MIN ( 125, MAX ( 0, ND ) )

      CALL MOVED ( SUM, N, DC )

      M = MIN ( 250 - 2 * N, MAX ( 0, NI ) )

      CALL MOVED ( SUM(N+1), (M - 1) / 2 + 1, DEQUIV )
      CALL MOVEI ( IEQUIV,    M,              IC     )

      RETURN
      END
