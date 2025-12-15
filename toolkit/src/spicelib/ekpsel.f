C$Procedure EKPSEL ( EK, parse SELECT clause )

      SUBROUTINE EKPSEL (  QUERY,   N,     XBEGS,  XENDS,  XTYPES,
     .                     XCLASS,  TABS,  COLS,   ERROR,  ERRMSG  )

C$ Abstract
C
C     Parse the SELECT clause of an EK query, returning full particulars
C     concerning each selected item.
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
C     PRIVATE
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'ekattdsc.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'

      CHARACTER*(*)         QUERY
      INTEGER               N
      INTEGER               XBEGS  ( * )
      INTEGER               XENDS  ( * )
      CHARACTER*(*)         XTYPES ( * )
      CHARACTER*(*)         XCLASS ( * )
      CHARACTER*(*)         TABS   ( * )
      CHARACTER*(*)         COLS   ( * )
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     QUERY      I   EK query.
C     N          O   Number of items in SELECT clause of QUERY.
C     XBEGS      O   Begin positions of expressions in SELECT clause.
C     XENDS      O   End positions of expressions in SELECT clause.
C     XTYPES     O   Data types of expressions.
C     XCLASS     O   Classes of expressions.
C     TABS       O   Names of tables qualifying SELECT columns.
C     COLS       O   Names of columns in SELECT clause of QUERY.
C     ERROR      O   Error flag.
C     ERRMSG     O   Parse error message.
C
C$ Detailed_Input
C
C     QUERY    is a character string containing an EK query.
C              EK queries have the general form
C
C                 SELECT <select expr>, <select expr>, ...
C                 FROM <table spec>, <table spec>, ...
C                 [WHERE <constraint list>]
C                 [ORDER BY <order-by column list>]
C
C              Here the symbol <select expr> indicates any
C              expression representing an entity that can be
C              selected. Commonly, the selected items are
C              columns, with or without qualifying table names,
C              having the form
C
C                 <column name>
C                 <table name>.<column name>
C                 <table alias>.<column name>
C
C              but more general expressions may also be selected.
C              Examples are functions, such as
C
C                 COUNT(*)
C                 COUNT( <table name>.<column name> )
C                 MAX  ( <table name>.<column name> )
C
C              or expressions involving constants, such as
C
C                 2 * <column name>
C
C$ Detailed_Output
C
C     N        is the number of items specified in the
C              SELECT clause of the input query.
C
C     XBEGS,
C     XENDS    are, respectively, arrays of begin and end
C              positions of expressions designating items in the
C              SELECT clause of the input query. The Ith
C              expression is located in the substring
C
C                 QUERY ( XBEGS(I) : XENDS(I) )
C
C     XTYPES   is an array of short strings indicating the data
C              types of the expressions in the SELECT clause.
C              Values and meanings of XTYPES are:
C
C                 'CHR'        Character type
C                 'DP'         Double precision type
C                 'INT'        Integer type
C                 'TIME'       Time type
C
C              The Ith element of XTYPES refers to the Ith
C              selected item.
C
C              The data type of an expression indicates which
C              fetch routine to use to obtain values of the
C              selected expression. The mapping of data types
C              to fetch routines is shown below:
C
C                 'CHR'        EKGC
C                 'DP'         EKGD
C                 'INT'        EKGI
C                 'TIME'       EKGD
C
C              Note that time values are stored as d.p. numbers.
C
C     XCLASS   is an array of short strings giving the classes
C              of the expressions occurring in the SELECT clause
C              of the input query. Values and meanings of
C              XCLASS are:
C
C                 'COL'        Selected item was a column. The
C                              column may qualified.
C
C                 'FUNC'       Selected item was a simple
C                              function invocation of the form
C
C                                 F ( <column> )
C
C                              or else was
C
C                                 COUNT(*)
C
C                 'EXPR'       Selected item was a more general
C                              expression than those shown above.
C
C              The Ith element of XCLASS refers to the Ith
C              selected item.
C
C              When a selected item is a column, the values of
C              the arguments TABS and COLS (discussed below) are
C              defined.
C
C     TABS     is an array of names of tables corresponding to
C              the columns in the SELECT clause. The Ith element
C              of TABS corresponds to the table containing the
C              Ith SELECT column. Table names returned in TABS
C              are the actual names of tables in loaded EK, not
C              aliases supplied in the input query. Table names
C              are supplied even if the corresponding column was
C              unqualified in the input query, as long as the
C              column name was unambiguous.
C
C              The contents of TABS(I) are defined if and only if
C              the returned value of XCLASS(I) is 'COL'.
C
C     COLS     is an array containing the columns of the SELECT
C              clause. The contents of COLS(I) are defined if and
C              only if the returned value of XCLASS(I) is 'COL'.
C
C     ERROR    is a logical flag indicating whether the input
C              QUERY parsed correctly. The other outputs of this
C              routine, except for ERRMSG, are undefined if a
C              parse error occurred. ERROR is returned .TRUE. if
C              a parse error occurred, .FALSE. otherwise.
C
C     ERRMSG   is a character string describing the cause of a
C              parse error, if such an error occurred. Otherwise,
C              ERRMSG is returned blank.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Parse failures do not cause this routine to signal errors;
C         instead, the ERROR and ERRMSG outputs indicate invalid
C         QUERY.
C
C     2)  Queries cannot be parsed correctly unless at least one EK
C         is loaded.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows callers of the EK fetch routines to determine
C     at run time the attributes of the columns from which data is to be
C     fetched.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Query the EK system and fetch data matching that query.
C
C        The program shown here does not rely on advance
C        knowledge of the input query or the contents of any loaded EK
C        files.
C
C        To simplify the example, we assume that all data are scalar.
C        This assumption relieves us of the need to test the size of
C        column entries before fetching them. In the event that a
C        column contains variable-size array entries, the entry point
C        EKNELT may be called to obtain the size of column entries to
C        be fetched. See EKNELT for an example.
C
C
C        Use the EK kernel below to load the information from the
C        original Supplementary Engineering Data Record (SEDR) data
C        set generated by the Viking Project.
C
C           vo_sedr.bdb
C
C        Use the LSK kernel below to load the leap seconds and time
C        constants required for the conversions.
C
C           naif0012.tls
C
C
C        Example code begins here.
C
C
C              PROGRAM EKPSEL_EX1
C              IMPLICIT NONE
C
C        C
C        C     Include EK Query Limit Parameters
C        C
C              INCLUDE 'ekqlimit.inc'
C
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               RTRIM
C
C        C
C        C     Local parameters
C        C
C              CHARACTER*(*)         EKNAME
C              PARAMETER           ( EKNAME = 'vo_sedr.bdb' )
C
C              CHARACTER*(*)         LSKNAM
C              PARAMETER           ( LSKNAM = 'naif0012.tls' )
C
C              INTEGER               DESCSZ
C              PARAMETER           ( DESCSZ = 31   )
C
C              INTEGER               ERRLEN
C              PARAMETER           ( ERRLEN = 1840 )
C
C              INTEGER               ITEMSZ
C              PARAMETER           ( ITEMSZ = DESCSZ + 4 )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 27   )
C
C              INTEGER               TYPLEN
C              PARAMETER           ( TYPLEN = 4    )
C
C              INTEGER               XCLSLN
C              PARAMETER           ( XCLSLN = 4    )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(MAXSTR)    CDATA
C              CHARACTER*(MAXCLN)    COLS   ( MAXSEL )
C              CHARACTER*(ERRLEN)    ERRMSG
C              CHARACTER*(ITEMSZ)    ITEM
C              CHARACTER*(DESCSZ)    OUTSTR
C              CHARACTER*(MAXQRY)    QUERY
C              CHARACTER*(TIMLEN)    UTCSTR
C              CHARACTER*(MAXCLN)    TABS   ( MAXTAB )
C              CHARACTER*(XCLSLN)    XCLASS ( MAXSEL )
C              CHARACTER*(TYPLEN)    XTYPES ( MAXSEL )
C
C              DOUBLE PRECISION      DDATA
C              DOUBLE PRECISION      TDATA
C
C              INTEGER               B
C              INTEGER               COLNO
C              INTEGER               E
C              INTEGER               HANDLE
C              INTEGER               IDATA
C              INTEGER               N
C              INTEGER               NMROWS
C              INTEGER               ROW
C              INTEGER               XBEGS  ( MAXSEL )
C              INTEGER               XENDS  ( MAXSEL )
C
C              LOGICAL               ERROR
C              LOGICAL               FOUND
C              LOGICAL               NULL
C
C        C
C        C     Load leapseconds file for time conversion.
C        C
C              CALL FURNSH ( LSKNAM )
C
C        C
C        C     Load EK.
C        C
C              CALL EKLEF  ( EKNAME, HANDLE )
C
C        C
C        C     Setup the query.  Parse the SELECT clause using
C        C     EKPSEL.
C        C
C              QUERY = 'Select IMAGE_NUMBER, IMAGE_ID, '
C             .   //          'PLATFORM_CLOCK, IMAGE_TIME '
C             .   //   'from VIKING_SEDR_DATA '
C             .   //   'where IMAGE_NUMBER < 25850000 '
C             .   //   'order by IMAGE_NUMBER'
C
C              CALL EKPSEL ( QUERY,  N,    XBEGS, XENDS, XTYPES,
C             .              XCLASS, TABS, COLS,  ERROR, ERRMSG )
C
C              IF ( ERROR ) THEN
C
C                 WRITE(*,*) ERRMSG
C
C              ELSE
C
C        C
C        C        Submit query to the EK query system.
C        C
C                 CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
C
C                 IF ( ERROR ) THEN
C
C                    WRITE(*,*) ERRMSG
C
C                 ELSE
C
C        C
C        C           Fetch the rows that matched the query.
C        C
C                    DO ROW = 1, NMROWS
C
C        C
C        C              Fetch data from the Ith row.
C        C
C                       WRITE (*,*) ' '
C                       WRITE (*,*) 'ROW = ', ROW
C
C                       DO COLNO = 1, N
C
C        C
C        C                 Fetch the data from the Jth selected
C        C                 column.
C        C
C                          IF ( XCLASS(COLNO) .EQ. 'COL' ) THEN
C
C                             OUTSTR  =  COLS(COLNO)
C                             CALL PREFIX ( '.',         0, OUTSTR )
C                             CALL PREFIX ( TABS(COLNO), 0, OUTSTR )
C                             ITEM = '  ' // OUTSTR // ':'
C
C                          ELSE
C
C                             B  =  XBEGS(COLNO)
C                             E  =  XENDS(COLNO)
C                             ITEM = '  ITEM = ' // QUERY(B:E)
C
C                          END IF
C
C                          IF ( XTYPES(COLNO) .EQ. 'CHR' ) THEN
C
C                             CALL EKGC ( COLNO,  ROW,  1,
C             .                           CDATA, NULL, FOUND )
C
C                             IF ( NULL ) THEN
C                                WRITE(*,*) ITEM, '<Null>'
C                             ELSE
C                                WRITE(*,*) ITEM, CDATA(:RTRIM(CDATA))
C                             END IF
C
C
C                          ELSE IF ( XTYPES(COLNO) .EQ. 'DP' ) THEN
C
C                             CALL EKGD ( COLNO,  ROW,  1,
C             .                           DDATA, NULL, FOUND )
C
C                             IF ( NULL ) THEN
C                                WRITE(*,*) ITEM, '<Null>'
C                             ELSE
C                                WRITE(*,*) ITEM, DDATA
C                             END IF
C
C
C                          ELSE IF ( XTYPES(COLNO) .EQ. 'INT' ) THEN
C
C                             CALL EKGI ( COLNO,  ROW,  1,
C             .                           IDATA, NULL, FOUND )
C
C                             IF ( NULL ) THEN
C                                WRITE(*,*) ITEM, '<Null>'
C                             ELSE
C                                WRITE(*,*) ITEM, IDATA
C                             END IF
C
C
C                          ELSE
C        C
C        C                    The item is a time value.  Convert it
C        C                    to UTC for output.
C        C
C                             CALL EKGD ( COLNO,  ROW,  1,
C             .                           TDATA, NULL, FOUND )
C
C                             IF ( NULL ) THEN
C                                WRITE(*,*) ITEM, '<Null>'
C                             ELSE
C                                CALL ET2UTC ( TDATA, 'C', 3, UTCSTR )
C                                WRITE(*,*) ITEM, UTCSTR
C                             END IF
C
C                          END IF
C
C        C
C        C              We're done with the column having index COLNO.
C        C
C                       END DO
C
C        C
C        C           We're done with the row having index ROW.
C        C
C                    END DO
C
C        C
C        C        We either processed the query or had an error.
C        C
C                 END IF
C
C        C
C        C     We either parsed the SELECT clause or had an error.
C        C
C              END IF
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C         ROW =            1
C           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25837050
C           VIKING_SEDR_DATA.IMAGE_ID      : 168C09
C           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.88000000000000
C           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 16:50:55.925
C
C         ROW =            2
C           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25837051
C           VIKING_SEDR_DATA.IMAGE_ID      : 168C10
C           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.27000000000000
C           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 16:51:00.269
C
C         ROW =            3
C           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25840344
C           VIKING_SEDR_DATA.IMAGE_ID      : 168C11
C           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.88000000000000
C           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 20:56:53.051
C
C         ROW =            4
C           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25840345
C           VIKING_SEDR_DATA.IMAGE_ID      : 168C12
C           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.27000000000000
C           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 20:56:57.395
C
C         ROW =            5
C           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25843638
C           VIKING_SEDR_DATA.IMAGE_ID      : 169C01
C           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.88000000000000
C           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 01:02:50.177
C
C         ROW =            6
C           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25843639
C           VIKING_SEDR_DATA.IMAGE_ID      : 169C02
C           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.27000000000000
C           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 01:02:54.521
C
C         ROW =            7
C           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25846934
C           VIKING_SEDR_DATA.IMAGE_ID      : 169C03
C           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000
C           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 05:08:56.263
C
C         ROW =            8
C           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25846935
C           VIKING_SEDR_DATA.IMAGE_ID      : 169C04
C           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.52000000000000
C           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 05:09:00.607
C
C         ROW =            9
C           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25848026
C           VIKING_SEDR_DATA.IMAGE_ID      : 169C05
C           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000
C           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:28.424
C
C         ROW =           10
C           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25848030
C           VIKING_SEDR_DATA.IMAGE_ID      : 169C09
C           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000
C           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:46.174
C
C         ROW =           11
C           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25848032
C           VIKING_SEDR_DATA.IMAGE_ID      : 169C11
C           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000
C           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:55.168
C
C
C$ Restrictions
C
C     1)  Currently, column names are the only supported expressions.
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
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing fragment.
C
C-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB)
C
C-&


C$ Index_Entries
C
C     parse select clause of EK query
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NTYPES
      PARAMETER           ( NTYPES = 4 )

      INTEGER               SHORT
      PARAMETER           ( SHORT  = 4 )

C
C     Local variables
C
      CHARACTER*(TNAMSZ)    AKA
      CHARACTER*(SHORT)     CHRTYP ( NTYPES )
      CHARACTER*(CNAMSZ)    COLUMN
      CHARACTER*(MAXCLN)    EQRYC
      CHARACTER*(TNAMSZ)    QTAB

      DOUBLE PRECISION      EQRYD  ( MAXQNM )

      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               COLIDX
      INTEGER               EQRYI  ( LBCELL : EQIMIN )
      INTEGER               ERRPTR
      INTEGER               I
      INTEGER               TABIDX

C
C     Saved values
C
      SAVE                  CHRTYP

C
C     Initial values
C
      DATA                  CHRTYP / 'CHR', 'DP', 'INT', 'TIME' /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKPSEL' )
      END IF

C
C     Initialize the encoded query each time, for safety.
C
      CALL ZZEKQINI ( EQIMIN, MAXQNM, EQRYI, EQRYC, EQRYD )

C
C     Encode the input query.
C
      CALL ZZEKENCD (  QUERY,
     .                 EQRYI,  EQRYC,   EQRYD,
     .                 ERROR,  ERRMSG,  ERRPTR  )

      IF ( ERROR ) THEN
         CALL CHKOUT ( 'EKPSEL' )
         RETURN
      END IF

C
C     Look up the number of SELECT columns.  For each column, look up
C     the parent table, the alias, and the column's name.
C
      CALL ZZEKREQI (  EQRYI,  'NUM_SELECT_COLS',  N   )

      DO I = 1, N

         CALL ZZEKQSEL ( EQRYI,  EQRYC,   I,        XBEGS(I),  XENDS(I),
     .                   QTAB,   TABIDX,  COLS(I),  COLIDX             )

C
C        Make the table index to the table name.
C
         CALL ZZEKQTAB ( EQRYI, EQRYC, TABIDX, TABS(I), AKA )

C
C        Currently, every expression is a column.
C
         XCLASS(I)  =  'COL'

C
C        Look up the data type of the column.
C
         CALL EKCII ( TABS(I), COLIDX, COLUMN, ATTDSC )

         XTYPES(I)  =  CHRTYP (  ATTDSC(ATTTYP)  )

      END DO

      CALL CHKOUT ( 'EKPSEL' )
      RETURN
      END
