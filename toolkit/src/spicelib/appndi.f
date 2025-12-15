C$Procedure APPNDI ( Append an item to an integer cell )

      SUBROUTINE APPNDI ( ITEM, CELL )

C$ Abstract
C
C     Append an item to an integer cell.
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

      INTEGER               ITEM
      INTEGER               CELL ( LBCELL:* )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ITEM       I   The item to append.
C     CELL      I-O  The cell to which ITEM will be appended.
C
C$ Detailed_Input
C
C     ITEM     is an integer value which is to be appended to CELL.
C
C     CELL     is an integer SPICE cell to which ITEM will be
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
C     3)  If the cell is not big enough to accommodate the addition
C         of a new element, the error SPICE(CELLTOOSMALL) is signaled.
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
C     1) Create a cell for fifteen elements, add a first element to
C        it and then append several more integer numbers. Validate
C        the cell into a set and print the result.
C
C
C        Example code begins here.
C
C
C              PROGRAM APPNDI_EX1
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions.
C        C
C              INTEGER                 CARDI
C
C        C
C        C     Local constants.
C        C
C              INTEGER                 LBCELL
C              PARAMETER             ( LBCELL = -5 )
C
C              INTEGER                 LISTSZ
C              PARAMETER             ( LISTSZ = 9  )
C
C              INTEGER                 SIZE
C              PARAMETER             ( SIZE   = 15 )
C
C        C
C        C     Local variables.
C        C
C              INTEGER                 A      ( LBCELL:SIZE )
C
C              INTEGER                 I
C              INTEGER                 ITEMS  ( LISTSZ      )
C
C        C
C        C     Set the list of integers to be appended to the cell.
C        C
C              DATA                    ITEMS /  3,  1,  1 , 2, 5,
C             .                                 8, 21, 13, 34    /
C
C        C
C        C     Initialize the cell.
C        C
C              CALL SSIZEI ( SIZE, A )
C
C        C
C        C     Add a single item to the new cell.
C        C
C              CALL APPNDI ( 0, A )
C
C        C
C        C     Now insert a list of items.
C        C
C              DO I= 1, LISTSZ
C
C                 CALL APPNDI ( ITEMS(I), A )
C
C              END DO
C
C        C
C        C     Output the original contents of cell A.
C        C
C              WRITE(*,*) 'Items in original cell A:'
C              WRITE(*,'(15I6)') ( A(I), I = 1, CARDI ( A ) )
C
C        C
C        C     Validate the set: remove duplicates and sort the
C        C     elements.
C        C
C              CALL VALIDI ( SIZE, CARDI( A ), A )
C
C        C
C        C     Output the contents of the set A.
C        C
C              WRITE(*,*) 'Items in cell A after VALIDI (now a set):'
C              WRITE(*,'(15I6)') ( A(I), I = 1, CARDI ( A ) )
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         Items in original cell A:
C             0     3     1     1     2     5     8    21    13    34
C         Items in cell A after VALIDI (now a set):
C             0     1     2     3     5     8    13    21    34
C
C
C        Note that if the cell is not big enough to accommodate the
C        addition of an item, an error is signaled. In this case, the
C        cell is not altered.
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
C     H.A. Neilan        (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example.
C
C        Improved the documentation of CELL in $Detailed_Input and
C        $Detailed_Output. Added entries #1 and #2 to $Exceptions.
C
C-    SPICELIB Version 1.0.2, 31-JUL-2002 (NJB)
C
C        Corrected miscellaneous typos in header and in the long
C        error message text.
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
C     append an item to an integer cell
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               CARDI
      INTEGER               SIZEI

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
         CALL CHKIN ( 'APPNDI' )
      END IF

C
C     Check to see if the cell can accommodate the addition of a
C     new item. If there is room, append the item to the cell and
C     reset the cardinality. If the cell cannot accommodate the
C     addition of a new item, signal an error.
C

      NWCARD = CARDI (CELL) + 1

      IF (  ( NWCARD )  .LE.  ( SIZEI (CELL) )  ) THEN

         CELL (NWCARD) = ITEM
         CALL SCARDI ( NWCARD, CELL )

      ELSE

        CALL SETMSG ( 'The cell cannot accommodate the '          //
     .                'addition of the element *. '                )

        CALL ERRINT ( '*', ITEM )
        CALL SIGERR ( 'SPICE(CELLTOOSMALL)' )

      END IF


      CALL CHKOUT ( 'APPNDI' )
      RETURN
      END
