C$Procedure SYTRNC (Transpose two values associated with a symbol)

      SUBROUTINE SYTRNC ( NAME, IDX1, IDX2, TABSYM, TABPTR, TABVAL )

C$ Abstract
C
C     Transpose two values associated with a particular symbol in a
C     character symbol table.
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
C     SYMBOLS
C
C$ Keywords
C
C     SYMBOLS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         NAME
      INTEGER               IDX1
      INTEGER               IDX2
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      CHARACTER*(*)         TABVAL     ( LBCELL:* )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the symbol whose associated values are to
C                    be transposed.
C     IDX1       I   Index of first associated value to be transposed.
C     IDX2       I   Index of second associated value to be transposed.
C     TABSYM,
C     TABPTR,
C     TABVAL    I-O  Components of the symbol table.
C
C$ Detailed_Input
C
C     NAME     is the name of the symbol whose associated values are to
C              be transposed.
C
C     IDX1     is the index of the first associated value to be
C              transposed.
C
C     IDX2     is the index of the second associated value to be
C              transposed.
C
C     TABSYM,
C     TABPTR,
C     TABVAL   are components of the character symbol table.
C
C$ Detailed_Output
C
C     TABSYM,
C     TABPTR,
C     TABVAL   are components of the character symbol table.
C
C              If the symbol NAME is not in the symbol table the symbol
C              tables are not modified. Otherwise, the values that IDX1
C              and IDX2 refer to are transposed in the value table.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If IDX1 < 1, IDX2 < 1, IDX1 > the dimension of NAME, or
C         IDX2 > the dimension of NAME, the error SPICE(INVALIDINDEX)
C         is signaled.
C
C     2)  If NAME is not in the symbol table, the symbol tables are not
C         modified.
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
C     The contents of the symbol table are:
C
C        BOHR      -->   HYDROGEN ATOM
C        EINSTEIN  -->   SPECIAL RELATIVITY
C                        PHOTOELECTRIC EFFECT
C                        BROWNIAN MOTION
C        FERMI     -->   NUCLEAR FISSION
C        PAULI     -->   EXCLUSION PRINCIPLE
C                        NEUTRINO
C
C     The call,
C
C     CALL SYTRNC ( 'EINSTEIN', 2, 3, TABSYM, TABPTR, TABVAL )
C
C     modifies the contents of the symbol table to be:
C
C        BOHR      -->   HYDROGEN ATOM
C        EINSTEIN  -->   SPECIAL RELATIVITY
C                        BROWNIAN MOTION
C                        PHOTOELECTRIC EFFECT
C        FERMI     -->   NUCLEAR FISSION
C        PAULI     -->   EXCLUSION PRINCIPLE
C                        NEUTRINO
C
C     The next call,
C
C     CALL SYTRNC ( 'PAULI', 2, 4, TABSYM, TABPTR, TABVAL )
C
C     causes the error SPICE(INVALIDINDEX) to be signaled.
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
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 08-APR-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Changed the name of input arguments "I" and "J" to "IDX1" and
C        "IDX2" for consistency with other routines.
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
C
C        Updated so no "exchange" occurs if IDX1 equals IDX2.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
C
C-&


C$ Index_Entries
C
C     transpose two values associated with a symbol
C
C-&


C$ Revisions
C
C-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
C
C        Updated so no "exchange" occurs if IDX1 equals IDX2.
C
C-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
C
C         If one of the indices of the values to be transposed is
C         invalid, an error is signaled and the symbol table is
C         not modified.
C
C-&


C
C     SPICELIB functions
C
      INTEGER               BSRCHC
      INTEGER               CARDC
      LOGICAL               RETURN
      INTEGER               SUMAI

C
C     Local variables
C
      INTEGER               NSYM

      INTEGER               LOCSYM
      INTEGER               LOCVAL
      INTEGER               N


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
      CALL CHKIN ( 'SYTRNC' )

C
C     How many symbols?
C
      NSYM = CARDC ( TABSYM )

C
C     Is this symbol even in the table?
C
      LOCSYM = BSRCHC ( NAME, NSYM, TABSYM(1) )

      IF ( LOCSYM .GT. 0 ) THEN

C
C        Are there enough values associated with the symbol?
C
         N = TABPTR(LOCSYM)

C
C        Are the indices valid?
C
         IF (        IDX1 .GE. 1
     .        .AND.  IDX1 .LE. N
     .        .AND.  IDX2 .GE. 1
     .        .AND.  IDX2 .LE. N ) THEN

C
C           Exchange the values in place.
C
            IF ( IDX1 .NE. IDX2 ) THEN

               LOCVAL = SUMAI ( TABPTR(1), LOCSYM-1 ) + 1

               CALL SWAPC ( TABVAL(LOCVAL+IDX1-1),
     .                      TABVAL(LOCVAL+IDX2-1) )

            END IF

         ELSE

            CALL SETMSG ( 'The first index was *. '
     .      //            'The second index was *.' )
            CALL ERRINT ( '*', IDX1                 )
            CALL ERRINT ( '*', IDX2                 )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'     )

         END IF


      END IF


      CALL CHKOUT ( 'SYTRNC' )
      RETURN
      END
