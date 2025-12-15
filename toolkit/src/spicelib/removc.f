C$Procedure REMOVC ( Remove an item from a character set )

      SUBROUTINE REMOVC ( ITEM, A )

C$ Abstract
C
C     Remove an item from a character set.
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
C     SETS
C
C$ Keywords
C
C     CELLS
C     SETS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         ITEM
      CHARACTER*(*)         A       ( LBCELL:* )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ITEM       I   Item to be removed.
C     A         I-O  Removal set.
C
C$ Detailed_Input
C
C     ITEM     is an item which is to be removed from the specified set.
C              ITEM may or may not already be an element of the set.
C              Trailing blanks in ITEM are not significant.
C
C     A        is a SPICE set.
C
C              On input, A may or may not contain the input item as an
C              element.
C
C$ Detailed_Output
C
C     A        on output, contains the difference of the input set and
C              the input item. If the item is not an element of the set,
C              the set is not changed.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input set A has invalid cardinality, an error is
C         signaled by a routine in the call tree of this routine.
C
C     2)  If the input set A has invalid size, an error is signaled by a
C         routine in the call tree of this routine.
C
C     3)  The data values in set A must be monotone strictly increasing.
C         This is not checked. If this condition is not met, the results
C         are unpredictable.
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
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Create a set with all the original planets of the Solar
C        System and then remove Pluto from that set.
C
C
C        Example code begins here.
C
C
C              PROGRAM REMOVC_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              INTEGER                 CARDC
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 LBCELL
C              PARAMETER             ( LBCELL = -5  )
C
C              INTEGER                 PNAMSZ
C              PARAMETER             ( PNAMSZ   = 7 )
C
C              INTEGER                 SETDIM
C              PARAMETER             ( SETDIM   = 9 )
C
C        C
C        C     Local variables.
C        C
C              CHARACTER*(PNAMSZ)      LIST   ( SETDIM        )
C              CHARACTER*(PNAMSZ)      PLNETS ( LBCELL:SETDIM )
C
C              INTEGER                 I
C
C        C
C        C     Create the original planets list.
C        C
C              DATA                    LIST  /
C             .                'MERCURY', 'VENUS',   'EARTH',
C             .                'MARS',    'JUPITER', 'SATURN',
C             .                'URANUS',  'NEPTUNE', 'PLUTO'   /
C
C        C
C        C     Initialize the empty set.
C        C
C              CALL VALIDC ( SETDIM, 0, PLNETS )
C
C        C
C        C     Insert the list of planets into the set. If the item is
C        C     an element of the set, the set is not changed.
C        C
C              DO I = 1, SETDIM
C
C                 CALL INSRTC ( LIST(I), PLNETS )
C
C              END DO
C
C        C
C        C     Remove the Pluto from the set. If the Pluto is not an
C        C     element of the set, the set is not changed.
C        C
C              CALL REMOVC ( 'PLUTO', PLNETS )
C
C        C
C        C     Output the contents of PLNETS.
C        C
C              WRITE(*,*) 'Planets of the Solar System:'
C
C              DO I = 1, CARDC ( PLNETS )
C
C                 WRITE(*,*) '   ', PLNETS(I)
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
C         Planets of the Solar System:
C            EARTH
C            JUPITER
C            MARS
C            MERCURY
C            NEPTUNE
C            SATURN
C            URANUS
C            VENUS
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
C     C.A. Curzon        (JPL)
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example. Extended the $Exceptions section.
C
C        Updated description of argument ITEM to indicate that trailing
C        blanks are not significant
C
C        Removed unnecessary $Revisions section.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
C
C-&


C$ Index_Entries
C
C     remove an item from a character set
C
C-&


C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               BSRCHC
      LOGICAL               RETURN
C
C     Local variables
C
      INTEGER          CARD
      INTEGER          LOC
      LOGICAL          IN
      INTEGER          I


C
C     Standard error handling:
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'REMOVC' )
      END IF
C
C     What is the cardinality of the set?
C
      CARD = CARDC ( A )

C
C     Determine the location (if any) of the item within the set.
C
      LOC = BSRCHC ( ITEM, CARD, A(1) )

C
C     Is the item in the set? If so, it needs to be removed.
C
      IN = ( LOC .GT. 0 )

      IF ( IN ) THEN

C
C        Move succeeding elements forward to take up the slack left
C        by the departing element. And update the cardinality for
C        future reference.
C
         DO I = LOC, CARD-1
            A(I) = A(I+1)
         END DO

         CALL SCARDC ( CARD-1, A )

      END IF


      CALL CHKOUT ( 'REMOVC' )
      RETURN
      END
