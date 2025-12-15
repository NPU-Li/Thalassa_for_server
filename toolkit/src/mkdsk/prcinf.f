C$Procedure PRCINF ( Process an information request )

      SUBROUTINE PRCINF ( INFTYP )

C$ Abstract
C
C     Process an information request:  display "help," "usage," 
C     "template, or program version information.
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
C     MKDSK
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE 'mkdsk.inc'

      CHARACTER*(*)         INFTYP
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INFTYP     I   Type of information to display.
C
C$ Detailed_Input
C
C     INFTYP         is a character string indicating the type
C                    of information to display.  The options are:
C
C                       'HELP'        Dump the introductory 
C                                     paragraph of the user's guide.
C
C                       'TEMPLATE'    Display a setup file template.
C
C                       'USAGE'       Display a terse description
C                                     of the program's invocation
C                                     syntax.
C
C                       'VERSION'     Display the program version
C                                     and creation date.  
C                     
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the value of INFTYP is not recognized, the error
C        SPICE(NOTSUPPORTED) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine centralizes message display functions for 
C     MKDSK.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) For use only within program MKDSK.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    MKDSK Version 4.0.0, 04-APR-2017 (NJB)
C
C        Updated setup file template. Moved declaration of 
C        version string to the include file mkdsk.inc.
C
C-    MKDSK Version 3.0.0, 30-JUN-2014 (NJB)
C
C        Updated version string.
C
C-    MKDSK Version 2.0.0, 29-JUN-2010 (NJB)
C
C        Updated template to reflect keyword changes.
C
C-    MKDSK Version 1.0.0, 08-JUN-2010 (NJB)
C
C-&

      
C
C     SPICELIB functions
C
      INTEGER               RTRIM

      LOGICAL               EQSTR      
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               HSIZE
      PARAMETER           ( HSIZE  = 18 )

      INTEGER               TXTLSZ
      PARAMETER           ( TXTLSZ = 80 )

      INTEGER               TSIZE
      PARAMETER           ( TSIZE  = 132 )

      INTEGER               USIZE
      PARAMETER           ( USIZE  = 17 )

      INTEGER               VSTRLN
      PARAMETER           ( VSTRLN = 80 )

C
C     Local variables
C
      CHARACTER*(TXTLSZ)    BEGMRK
      CHARACTER*(TXTLSZ)    ENDMRK
      CHARACTER*(TXTLSZ)    HLPTXT ( HSIZE )
      CHARACTER*(TXTLSZ)    TMPTXT ( TSIZE )
      CHARACTER*(TXTLSZ)    USGTXT ( USIZE )
      CHARACTER*(VSTRLN)    VERSTR     

      INTEGER               I

      LOGICAL               FIRST

C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  HLPTXT
      SAVE                  TMPTXT
      SAVE                  USGTXT
      SAVE                  VERSTR

C
C     Initial values
C
      DATA                  FIRST / .TRUE. /



      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'PRCINF' )


      IF ( FIRST ) THEN
C
C        This lovely mess was created using Bill Taber's ImportText
C        pipe.
C

         BEGMRK(1:1)  =  CHAR( 92 )
         BEGMRK(2: )  =  'begindata'
         ENDMRK(1:1)  =  CHAR( 92 )
         ENDMRK(2: )  =  'begintext'
 
         

         HLPTXT(  1 ) = '   MKDSK is a SPICE Toolkit utility '
     .   //             'program that converts a shape'
         HLPTXT(  2 ) = '   data file having a recognized for'
     .   //             'mat to a SPICE DSK ("Digital'
         HLPTXT(  3 ) = '   Shape Kernel") file.'
         HLPTXT(  4 ) = ' '
         HLPTXT(  5 ) = '   MKDSK requires as inputs a shape '
     .   //             'data file, a leapseconds file, and'
         HLPTXT(  6 ) = '   a setup file containing commands '
     .   //             'that control MKDSK''s operation.'
         HLPTXT(  7 ) = '   Execute MKDSK with the -t option '
     .   //             'to see a setup file template:'
         HLPTXT(  8 ) = ' '
         HLPTXT(  9 ) = '       % mkdsk -t'
         HLPTXT( 10 ) = ' '
         HLPTXT( 11 ) = '   The user may optionally specify a'
     .   //             ' text file containing descriptive'
         HLPTXT( 12 ) = '   text to be placed in the comment '
     .   //             'area of the DSK (doing this is'
         HLPTXT( 13 ) = '   highly recommended).'
         HLPTXT( 14 ) = ' '
         HLPTXT( 15 ) = '   For documentation purposes the co'
     .   //             'ntents of the MKDSK setup file are'
         HLPTXT( 16 ) = '   automatically placed at the end o'
     .   //             'f the comment area of the DSK file.'
         HLPTXT( 17 ) = ' '
         HLPTXT( 18 ) = '   See the MKDSK User''s Guide for f'
     .   //             'urther information.'


        
 
         TMPTXT(  1 ) = '  Complete MKDSK Setup File Template'
     .   //             ':'
         TMPTXT(  2 ) = ' '
         TMPTXT(  3 ) = BEGMRK
         TMPTXT(  4 ) = ' '
         TMPTXT(  5 ) = '   INPUT_SHAPE_FILE       = ''Name o'
     .   //             'f input shape data file'''
         TMPTXT(  6 ) = '   OUTPUT_DSK_FILE        = ''Name o'
     .   //             'f output DSK file'''
         TMPTXT(  7 ) = ' '
         TMPTXT(  8 ) = '   Optional assignment:'
         TMPTXT(  9 ) = '   COMMENT_FILE           = ''Name o'
     .   //             'f optional comment file'''
         TMPTXT( 10 ) = ' '
         TMPTXT( 11 ) = '   Optional assignment:'
         TMPTXT( 12 ) = '   LEAPSECONDS_FILE       = ''Name o'
     .   //             'f leapseconds file'''
         TMPTXT( 13 ) = '                            A leapse'
     .   //             'conds kernel is required;'
         TMPTXT( 14 ) = '                            it can b'
     .   //             'e named using the'
         TMPTXT( 15 ) = '                            KERNELS_'
     .   //             'TO_LOAD assignment.'
         TMPTXT( 16 ) = ' '
         TMPTXT( 17 ) = '   Optional assignment:'
         TMPTXT( 18 ) = '   KERNELS_TO_LOAD        = ( ''Kern'
     .   //             'el_1'' ''Kernel_2'' ... )'
         TMPTXT( 19 ) = '                            List any'
     .   //             ' additional kernels needed.'
         TMPTXT( 20 ) = '                            Note tha'
     .   //             't a leapseconds kernel can be'
         TMPTXT( 21 ) = '                            supplied'
     .   //             ' using this assignment.'
         TMPTXT( 22 ) = ' '
         TMPTXT( 23 ) = '   CENTER_NAME            = ''Centra'
     .   //             'l body name'''
         TMPTXT( 24 ) = '   SURFACE_NAME           = ''Surfac'
     .   //             'e name'''
         TMPTXT( 25 ) = '   REF_FRAME_NAME         = ''Refere'
     .   //             'nce frame name'''
         TMPTXT( 26 ) = '   START_TIME             = ''Start '
     .   //             'time'''
         TMPTXT( 27 ) = '   STOP_TIME              = ''Stop t'
     .   //             'ime'''
         TMPTXT( 28 ) = '   DATA_CLASS             = 1 for si'
     .   //             'ngle-valued surface'
         TMPTXT( 29 ) = '                              topogr'
     .   //             'aphy (for latitudinal'
         TMPTXT( 30 ) = '                              coordi'
     .   //             'nates, this implies each'
         TMPTXT( 31 ) = '                              ray em'
     .   //             'anating from the reference'
         TMPTXT( 32 ) = '                              frame'''
     .   //             's origin intersects the'
         TMPTXT( 33 ) = '                              surfac'
     .   //             'e once)'
         TMPTXT( 34 ) = ' '
         TMPTXT( 35 ) = '                              or'
         TMPTXT( 36 ) = ' '
         TMPTXT( 37 ) = '                            2 for ar'
     .   //             'bitrary topography (e.g.'
         TMPTXT( 38 ) = '                              that o'
     .   //             'f a dumbbell-shaped asteroid)'
         TMPTXT( 39 ) = ' '
         TMPTXT( 40 ) = '   INPUT_DATA_UNITS       = ( ''ANGL'
     .   //             'ES    = angular unit'''
         TMPTXT( 41 ) = '                              ''DIST'
     .   //             'ANCES = distance unit'' )'
         TMPTXT( 42 ) = ' '
         TMPTXT( 43 ) = '   COORDINATE_SYSTEM      = ''LATITU'
     .   //             'DINAL''  or'
         TMPTXT( 44 ) = '                            ''RECTAN'
     .   //             'GULAR''  or'
         TMPTXT( 45 ) = '                            ''PLANET'
     .   //             'ODETIC'''
         TMPTXT( 46 ) = ' '
         TMPTXT( 47 ) = '      For latitudinal coordinates:'
         TMPTXT( 48 ) = ' '
         TMPTXT( 49 ) = '      MINIMUM_LATITUDE    = lower la'
     .   //             'titude bound in selected units'
         TMPTXT( 50 ) = '      MAXIMUM_LATITUDE    = upper la'
     .   //             'titude bound in selected units'
         TMPTXT( 51 ) = '      MINIMUM_LONGITUDE   = lower lo'
     .   //             'ngitude bound in selected units'
         TMPTXT( 52 ) = '      MAXIMUM_LONGITUDE   = upper lo'
     .   //             'ngitude bound in selected units'
         TMPTXT( 53 ) = ' '
         TMPTXT( 54 ) = '      For rectangular coordinates:'
         TMPTXT( 55 ) = ' '
         TMPTXT( 56 ) = '      MINIMUM_X           = lower X '
     .   //             'coordinate bound in selected units'
         TMPTXT( 57 ) = '      MAXIMUM_X           = upper X '
     .   //             'coordinate bound in selected units'
         TMPTXT( 58 ) = '      MINIMUM_Y           = lower Y '
     .   //             'coordinate bound in selected units'
         TMPTXT( 59 ) = '      MAXIMUM_Y           = upper Y '
     .   //             'coordinate bound in selected units'
         TMPTXT( 60 ) = ' '
         TMPTXT( 61 ) = '      For planetodetic coordinates:'
         TMPTXT( 62 ) = ' '
         TMPTXT( 63 ) = '      MINIMUM_LATITUDE    = lower la'
     .   //             'titude bound in selected units'
         TMPTXT( 64 ) = '      MAXIMUM_LATITUDE    = upper la'
     .   //             'titude bound in selected units'
         TMPTXT( 65 ) = '      MINIMUM_LONGITUDE   = lower lo'
     .   //             'ngitude bound in selected units'
         TMPTXT( 66 ) = '      MAXIMUM_LONGITUDE   = upper lo'
     .   //             'ngitude bound in selected units'
         TMPTXT( 67 ) = ' '
         TMPTXT( 68 ) = '      EQUATORIAL_RADIUS   = equatori'
     .   //             'al spheroid radius in selected units'
         TMPTXT( 69 ) = '      POLAR_RADIUS        = polar sp'
     .   //             'heroid radius in selected units'
         TMPTXT( 70 ) = ' '
         TMPTXT( 71 ) = ' '
         TMPTXT( 72 ) = '   DATA_TYPE              = 2 (trian'
     .   //             'gular plates)'
         TMPTXT( 73 ) = ' '
         TMPTXT( 74 ) = '      For data type 2:'
         TMPTXT( 75 ) = ' '
         TMPTXT( 76 ) = '      PLATE_TYPE          = 1  for p'
     .   //             'late-vertex table'
         TMPTXT( 77 ) = '                            2  for G'
     .   //             'askell shape model'
         TMPTXT( 78 ) = '                            3  for v'
     .   //             'ertex-facet table'
         TMPTXT( 79 ) = '                            4  for R'
     .   //             'osetta/OSIRIS "ver" table'
         TMPTXT( 80 ) = '                            5  for r'
     .   //             'ectangular height grid'
         TMPTXT( 81 ) = ' '
         TMPTXT( 82 ) = '      The following two assignments '
     .   //             'are optional.'
         TMPTXT( 83 ) = ' '
         TMPTXT( 84 ) = '      Optional assignments:'
         TMPTXT( 85 ) = '      FINE_VOXEL_SCALE    = Double p'
     .   //             'recision value > 0.0'
         TMPTXT( 86 ) = '      COARSE_VOXEL_SCALE  = Integer '
     .   //             '>= 1'
         TMPTXT( 87 ) = '                            If these'
     .   //             ' assignments are not provided,'
         TMPTXT( 88 ) = '                            MKDSK wi'
     .   //             'll set the voxel scales automaticall'
     .   //             'y.'
         TMPTXT( 89 ) = ' '
         TMPTXT( 90 ) = '      Optional assignment:'
         TMPTXT( 91 ) = '      MAKE_VERTEX_PLATE_MAP =  ''YES'
     .   //             ''' or ''NO'''
         TMPTXT( 92 ) = '                               If th'
     .   //             'is assignment is not provided,'
         TMPTXT( 93 ) = '                               MKDSK'
     .   //             ' will not create a vertex-plate mapp'
     .   //             'ing.'
         TMPTXT( 94 ) = ' '
         TMPTXT( 95 ) = '         For plate type 5, the follo'
     .   //             'wing additional assignments'
         TMPTXT( 96 ) = '         are required:'
         TMPTXT( 97 ) = ' '
         TMPTXT( 98 ) = '         WRAP_LONGITUDE             '
     .   //             ' = connect leftmost column to'
         TMPTXT( 99 ) = '                                    '
     .   //             '   rightmost column: ''YES'' or ''NO'
     .   //             ''''
         TMPTXT( 100 ) = '         MAKE_NORTH_POLAR_CAP      '
     .   //              '  = extend plate set to north pole:'
         TMPTXT( 101 ) = '                                   '
     .   //              '    ''YES'' or ''NO'''
         TMPTXT( 102 ) = '         MAKE_SOUTH_POLAR_CAP      '
     .   //              '  = extend plate set to south pole:'
         TMPTXT( 103 ) = '                                   '
     .   //              '    ''YES'' or ''NO'''
         TMPTXT( 104 ) = '         INPUT_GRID_ORDER_ROW_MAJOR'
     .   //              '  = input data set is row-major:'
         TMPTXT( 105 ) = '                                   '
     .   //              '    ''YES'' or ''NO'''
         TMPTXT( 106 ) = '         COLUMN_VALUE_ORDER_TOP_DOW'
     .   //              'N = input data set is top-down:'
         TMPTXT( 107 ) = '                                   '
     .   //              '    ''YES'' or ''NO'''
         TMPTXT( 108 ) = '         ROW_VALUE_ORDER_LEFT_RIGHT'
     .   //              '  = input data set is left-right:'
         TMPTXT( 109 ) = '                                   '
     .   //              '    ''YES'' or ''NO'''
         TMPTXT( 110 ) = '         HEIGHT_SCALE              '
     .   //              '  = value by which to multiply the'
         TMPTXT( 111 ) = '                                   '
     .   //              '    height data to convert to km'
         TMPTXT( 112 ) = '         NUMBER_OF_ROWS            '
     .   //              '  = number of rows in grid'
         TMPTXT( 113 ) = '         NUMBER_OF_COLUMNS         '
     .   //              '  = number of columns in grid'
         TMPTXT( 114 ) = '         COLUMN_STEP_SIZE          '
     .   //              '  = column separation: longitude or'
     .   //              ' X step'
         TMPTXT( 115 ) = '         ROW_STEP_SIZE             '
     .   //              '  = row separation: latitude or Y s'
     .   //              'tep'
         TMPTXT( 116 ) = ' '
         TMPTXT( 117 ) = '            For plate type 5, latit'
     .   //              'udinal or planetodetic coordinates:'
         TMPTXT( 118 ) = ' '
         TMPTXT( 119 ) = '            LEFT_COLUMN_LONGITUDE  '
     .   //              '  = longitude of leftmost column of'
     .   //              ' grid'
         TMPTXT( 120 ) = '            TOP_ROW_LATITUDE       '
     .   //              '  = latitude of top row of grid'
         TMPTXT( 121 ) = ' '
         TMPTXT( 122 ) = '            For plate type 5, recta'
     .   //              'ngular coordinates:'
         TMPTXT( 123 ) = ' '
         TMPTXT( 124 ) = '            LEFT_COLUMN_X_COORDINAT'
     .   //              'E = X-coordinate of leftmost column'
     .   //              ' of grid'
         TMPTXT( 125 ) = '            TOP_ROW_Y_COORDINATE   '
     .   //              '  = Y-coordinate of top row of grid'
         TMPTXT( 126 ) = ' '
         TMPTXT( 127 ) = '            For plate type 5, latit'
     .   //              'udinal or rectangular coordinates:'
         TMPTXT( 128 ) = ' '
         TMPTXT( 129 ) = '            HEIGHT_REFERENCE       '
     .   //              '  = value to add to input heights; '
     .   //              'units'
         TMPTXT( 130 ) = '                                   '
     .   //              '    are given by INPUT_DATA_UNITS'
         TMPTXT( 131 ) = ' '
         TMPTXT( 132 ) = ENDMRK



         USGTXT(  1 ) = '     Program usage:'
         USGTXT(  2 ) = ' '
         USGTXT(  3 ) = '              > mkdsk   [-setup <set'
     .   //             'up file name>]'
         USGTXT(  4 ) = '                        [-input <inp'
     .   //             'ut shape data file name>]'
         USGTXT(  5 ) = '                        [-output <ou'
     .   //             'tput DSK file name>]'
         USGTXT(  6 ) = '                        [-h|-help]'
         USGTXT(  7 ) = '                        [-t|-templat'
     .   //             'e]'
         USGTXT(  8 ) = '                        [-u|-usage]'
         USGTXT(  9 ) = '                        [-v|-version'
     .   //             ']'
         USGTXT( 10 ) = ' '
         USGTXT( 11 ) = '     If a setup file name isn''t pro'
     .   //             'vided on the command line, the'
         USGTXT( 12 ) = '     program will prompt for it. It '
     .   //             'will not prompt for the input'
         USGTXT( 13 ) = '     or output file names; these fil'
     .   //             'e names must be provided on'
         USGTXT( 14 ) = '     the command line or in the setu'
     .   //             'p file. If input and output'
         USGTXT( 15 ) = '     file names are provided on the '
     .   //             'command line, any file names'
         USGTXT( 16 ) = '     assigned using setup keywords a'
     .   //             're ignored. The input file'
         USGTXT( 17 ) = '     must already exist and the outp'
     .   //             'ut file must be a new file.'


         FIRST = .FALSE.

      END IF

 
      IF ( EQSTR(INFTYP, 'TEMPLATE') ) THEN
C
C        Display the template text.
C
         DO I = 1, TSIZE
            CALL TOSTDO ( TMPTXT(I) )
         END DO

         CALL TOSTDO ( ' ' )


      ELSE IF ( EQSTR(INFTYP, 'USAGE') ) THEN
C
C        Display the usage text.
C
         DO I = 1, USIZE
            CALL TOSTDO ( USGTXT(I) )
         END DO

         CALL TOSTDO ( ' ' )


      ELSE IF ( EQSTR(INFTYP, 'VERSION') ) THEN
C
C        Create and display "version" message.
C
         CALL TKVRSN ( 'TOOLKIT', VERSTR )

         CALL TOSTDO ( ' '                                           )
         CALL TOSTDO ( 'MKDSK Program; Ver. ' // VER // 
     .                 '; Toolkit Ver. ' // VERSTR(:RTRIM(VERSTR))   )
         CALL TOSTDO ( ' '                                           )


      ELSE IF ( EQSTR(INFTYP, 'HELP') ) THEN

         DO I = 1, HSIZE
            CALL TOSTDO ( HLPTXT(I) )
         END DO

         CALL TOSTDO ( ' ' )

      ELSE
C
C        We shouldn't arrive here.
C
         CALL SETMSG ( 'Informational message type # is not '   //
     .                 'supported.'                             )
         CALL ERRCH  ( '#', INFTYP                              )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                    )
         CALL CHKOUT ( 'PRCINF'                                 )
         RETURN

      END IF

      CALL CHKOUT ( 'PRCINF' )
      RETURN
      END
