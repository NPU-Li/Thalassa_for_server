C$Procedure APPNDC ( Append an item to a character cell )

      SUBROUTINE APPNDC ( ITEM, CELL )

C$ Abstract
C
C     Append an item to a character cell.
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
C     CELLS
C
C$ Keywords
C
C     CELLS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         ITEM
      CHARACTER*(*)         CELL ( LBCELL:* )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ITEM       I   The item to append.
C     CELL      I-O  The cell to which ITEM will be appended.
C
C$ Detailed_Input
C
C     ITEM     is a character string which is to be appended to CELL.
C
C     CELL     is a character SPICE cell to which ITEM will be
C              appended.
C
C$ Detailed_Output
C
C     CELL     is the input cell with ITEM appended. ITEM is the last
C              member of CELL.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input cell has invalid cardinality, an error is
C         signaled by a routine in the call tree of this routine.
C
C     2)  If the input cell has invalid size, an error is signaled by a
C         routine in the call tree of this routine.
C
C     3)  If the cell is not large enough to accommodate the addition
C         of a new element, the error SPICE(CELLTOOSMALL) is signaled.
C
C     4)  If the length of the item is longer than the length of the
C         cell, ITEM is truncated on the right.
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
C     In the following example, the item 'PLUTO' is appended to
C     the character cell PLANETS.
C
C     Before appending 'PLUTO', the cell contains:
C
C     PLANETS (1) = 'MERCURY'
C     PLANETS (2) = 'VENUS'
C     PLANETS (3) = 'EARTH'
C     PLANTES (4) = 'MARS'
C     PLANETS (5) = 'JUPITER'
C     PLANETS (6) = 'SATURN'
C     PLANETS (7) = 'URANUS'
C     PLANETS (8) = 'NEPTUNE'
C
C     The call
C
C        CALL APPNDC ( 'PLUTO', PLANETS )
C
C     appends the element 'PLUTO' at the location PLANETS (9), and the
C     cardinality is updated.
C
C     If the cell is not big enough to accommodate the addition of
C     the item, an error is signaled. In this case, the cell is not
C     altered.
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
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C
C        Improved the documentation of CELL in $Detailed_Input and
C        $Detailed_Output. Added entries #1 and #2 to $Exceptions.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
C
C-&


C$ Index_Entries
C
C     append an item to a character cell
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               CARDC
      INTEGER               SIZEC

C
C     Local variables
C
      INTEGER               NWCARD





C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'APPNDC' )
      END IF

C
C     Check to see if the cell can accommodate the addition of a
C     new item. If there is room, append the item to the cell and
C     reset the cardinality. If the cell cannot accommodate the
C     addition of a new item, signal an error.
C

      NWCARD = CARDC (CELL) + 1

      IF (  ( NWCARD )  .LE.  ( SIZEC (CELL) )  ) THEN

         CELL (NWCARD) = ITEM
         CALL SCARDC ( NWCARD, CELL )

      ELSE

        CALL SETMSG ( 'The cell cannot accommodate the '     //
     .                'addition of the item *.'                 )

        CALL ERRCH  ( '*', ITEM )
        CALL SIGERR ( 'SPICE(CELLTOOSMALL)' )

      END IF


      CALL CHKOUT ( 'APPNDC' )
      RETURN
      END
