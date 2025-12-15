C$Procedure ORDERC ( Order of a character array )

      SUBROUTINE ORDERC ( ARRAY, NDIM, IORDER )

C$ Abstract
C
C     Determine the order of elements in an array of character strings.
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
C     ARRAY
C     SORT
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    ARRAY  ( * )
      INTEGER          NDIM
      INTEGER          IORDER ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ARRAY      I    Input array.
C     NDIM       I    Dimension of ARRAY.
C     IORDER     O    Order vector for ARRAY.
C
C$ Detailed_Input
C
C     ARRAY    is the input array.
C
C     NDIM     is the number of elements in the input array.
C
C$ Detailed_Output
C
C     IORDER   is the order vector for the input array.
C              IORDER(1) is the index of the smallest element
C              of ARRAY; IORDER(2) is the index of the next
C              smallest; and so on. Strings are ordered according
C              to the ASCII collating sequence.
C
C$ Parameters
C
C     None.
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
C     ORDERC finds the index of the smallest element of the input
C     array. This becomes the first element of the order vector.
C     The process is repeated for the rest of the elements.
C
C     The order vector returned by ORDERC may be used by any of
C     the REORD routines to sort sets of related arrays, as shown
C     in the example below.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Sort four related arrays (containing the names, masses,
C        integer ID codes, and flags indicating whether they have
C        a ring system, for a group of planets).
C
C
C        Example code begins here.
C
C
C              PROGRAM ORDERC_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 NDIM
C              PARAMETER             ( NDIM   = 8  )
C
C              INTEGER                 STRLEN
C              PARAMETER             ( STRLEN = 7  )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(STRLEN)      NAMES  ( NDIM )
C
C              DOUBLE PRECISION        MASSES ( NDIM )
C
C              INTEGER                 CODES  ( NDIM )
C              INTEGER                 I
C              INTEGER                 IORDER ( NDIM )
C
C              LOGICAL                 RINGS  ( NDIM )
C
C        C
C        C     Set the arrays containing the names, masses (given as
C        C     ratios to of Solar GM to barycenter GM), integer ID
C        C     codes, and flags indicating whether they have a ring
C        C     system.
C        C
C              DATA                    NAMES  /
C             .            'MERCURY', 'VENUS',  'EARTH',  'MARS',
C             .            'JUPITER', 'SATURN', 'URANUS', 'NEPTUNE' /
C
C              DATA                    MASSES /
C             .                       22032.080D0,   324858.599D0,
C             .                      398600.436D0,    42828.314D0,
C             .                   126712767.881D0, 37940626.068D0,
C             .                     5794559.128D0,  6836534.065D0 /
C
C              DATA                    CODES  / 199, 299, 399, 499,
C             .                                 599, 699, 799, 899 /
C
C              DATA                    RINGS  /
C             .                   .FALSE., .FALSE., .FALSE., .FALSE.,
C             .                   .TRUE.,  .TRUE.,  .TRUE., .TRUE.   /
C
C        C
C        C     Sort the object arrays by name.
C        C
C              CALL ORDERC ( NAMES,  NDIM, IORDER )
C
C              CALL REORDC ( IORDER, NDIM, NAMES  )
C              CALL REORDD ( IORDER, NDIM, MASSES )
C              CALL REORDI ( IORDER, NDIM, CODES  )
C              CALL REORDL ( IORDER, NDIM, RINGS  )
C
C        C
C        C     Output the resulting table.
C        C
C              WRITE(*,'(A)') ' Planet   Mass(GMS/GM)  ID Code  Rings?'
C              WRITE(*,'(A)') '-------  -------------  -------  ------'
C
C              DO I = 1, NDIM
C
C                 WRITE(*,'(A,F15.3,I9,L5)') NAMES(I), MASSES(I),
C             .                              CODES(I), RINGS(I)
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
C         Planet   Mass(GMS/GM)  ID Code  Rings?
C        -------  -------------  -------  ------
C        EARTH       398600.436      399    F
C        JUPITER  126712767.881      599    T
C        MARS         42828.314      499    F
C        MERCURY      22032.080      199    F
C        NEPTUNE    6836534.065      899    T
C        SATURN    37940626.068      699    T
C        URANUS     5794559.128      799    T
C        VENUS       324858.599      299    F
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
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&


C$ Index_Entries
C
C     order of a character array
C
C-&


C
C     Local variables
C
      INTEGER          GAP
      INTEGER          I
      INTEGER          J
      INTEGER          JG


C
C     Begin with the initial ordering.
C
      DO I = 1, NDIM
         IORDER(I) = I
      END DO

C
C     Find the smallest element, then the next smallest, and so on.
C     This uses the Shell Sort algorithm, but swaps the elements of
C     the order vector instead of the array itself.
C
      GAP = NDIM / 2

      DO WHILE ( GAP .GT. 0 )

         DO I = GAP+1, NDIM

            J = I - GAP
            DO WHILE ( J .GT. 0 )
               JG = J + GAP

               IF ( LLE ( ARRAY(IORDER(J)), ARRAY(IORDER(JG)) ) ) THEN
                  J = 0
               ELSE
                  CALL SWAPI ( IORDER(J), IORDER(JG) )
               END IF

               J = J - GAP
            END DO

         END DO

         GAP = GAP / 2

      END DO

      RETURN
      END
