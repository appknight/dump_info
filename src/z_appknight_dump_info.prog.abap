REPORT z_appknight_dump_info.

" display specific variables of a short dump

TABLES rsdumpinfo.

SELECT-OPTIONS so_date FOR  rsdumpinfo-sydate DEFAULT sy-datum.
SELECT-OPTIONS so_dump FOR  rsdumpinfo-dumpid.

PARAMETERS     pa_fld01  TYPE c LENGTH 40 DEFAULT 'SY-DATUM'.
PARAMETERS     pa_fld02  TYPE c LENGTH 40.
PARAMETERS     pa_fld03  TYPE c LENGTH 40.
PARAMETERS     pa_fld04  TYPE c LENGTH 40.
PARAMETERS     pa_fld05  TYPE c LENGTH 40.


CLASS lcl_dump DEFINITION.
  PUBLIC SECTION.

    TYPES tv_value TYPE c LENGTH 100.

    METHODS constructor
      IMPORTING
        is_snap TYPE snap.

    METHODS get_variable
      IMPORTING
        iv_varname      TYPE clike
      RETURNING
        VALUE(rv_value) TYPE tv_value.

  PROTECTED SECTION.

    TYPES:
      BEGIN OF ts_varinfo,
        name   TYPE c LENGTH 80,
        length TYPE i,
        value  TYPE tv_value,
      END OF ts_varinfo,
      tt_varinfo TYPE SORTED TABLE OF ts_varinfo WITH UNIQUE KEY name.

    DATA ms_dumpinfo     TYPE rsdumpinfo.
    DATA ms_snap         TYPE snap.
    DATA mt_dump_fields  TYPE rsdump_ft_it.
    DATA mt_varinfo      TYPE tt_varinfo.

    METHODS retrieve_dump_info.
    METHODS retrieve_dump_fields.
    METHODS retrieve_variables.


ENDCLASS.

CLASS lcl_dump IMPLEMENTATION.

  METHOD constructor.
    ms_snap = is_snap.
    retrieve_dump_info( ).
    retrieve_dump_fields( ).
    retrieve_variables( ).
  ENDMETHOD.

  METHOD retrieve_dump_info.

    DATA x TYPE i.
    DATA y TYPE i.
    DATA cnt TYPE i.
    DATA wa_snap TYPE snap.

    FIELD-SYMBOLS <buffer> TYPE c.

    ms_dumpinfo-sydate = ms_snap-datum.
    ms_dumpinfo-sytime = ms_snap-uzeit.
    ms_dumpinfo-syhost = ms_snap-ahost.
    ms_dumpinfo-syuser = ms_snap-uname.

    wa_snap = ms_snap.

*   Flist: stream of char
    ASSIGN wa_snap-flist(1600) TO <buffer> RANGE wa_snap.
    CLEAR: x, cnt.

*   in case of inconsistent stream
    CATCH SYSTEM-EXCEPTIONS conversion_errors = 0 data_access_errors = 0.

*     until end '%' or all needed fields are found
      WHILE <buffer>+x(1) <> '%' AND cnt < 4.
        CASE <buffer>+x(2).
          WHEN 'FC'.
*           FC = Fehler-Code (RABAX-Name)
            ADD 1 TO cnt.
            ADD 2 TO x.
            y = <buffer>+x(3).
            ADD 3 TO x.
            ms_dumpinfo-dumpid = <buffer>+x(y).
            ADD y TO x.
          WHEN 'AP'.
*           AP = Application-Program
            ADD 1 TO cnt.
            ADD 2 TO x.
            y = <buffer>+x(3).
            ADD 3 TO x.
            ms_dumpinfo-programname = <buffer>+x(y).
            ADD y TO x.
          WHEN 'AI'.
*           AI = Application-Include
            ADD 1 TO cnt.
            ADD 2 TO x.
            y = <buffer>+x(3).
            ADD 3 TO x.
            ms_dumpinfo-includename = <buffer>+x(y).
            ADD y TO x.
          WHEN 'AL'.
*           AL = Application-Line
            ADD 1 TO cnt.
            ADD 2 TO x.
            y = <buffer>+x(3).
            ADD 3 TO x.
            ms_dumpinfo-linenumber = <buffer>+x(y).
            ADD y TO x.
          WHEN OTHERS.
*           jump over not needed fields
            ADD 2 TO x.
            x = x + 3 + <buffer>+x(3).
        ENDCASE.
      ENDWHILE.

    ENDCATCH.
  ENDMETHOD.

  METHOD retrieve_dump_fields.

    CALL FUNCTION 'RS_ST22_GET_FT'
      EXPORTING
        datum = ms_snap-datum
        uzeit = ms_snap-uzeit
        uname = ms_snap-uname
        ahost = ms_snap-ahost
        modno = ms_snap-modno
        mandt = ms_snap-mandt
      IMPORTING
        ft    = mt_dump_fields.

  ENDMETHOD.

  METHOD get_variable.

    TRY.

        rv_value = mt_varinfo[ name = iv_varname ]-value.
      CATCH cx_sy_itab_line_not_found.
        CLEAR rv_value.
    ENDTRY.


  ENDMETHOD.

  METHOD retrieve_variables.

    FIELD-SYMBOLS <n0> TYPE rsdump_ft. "NAME
    FIELD-SYMBOLS <l0> TYPE rsdump_ft. "LENGTH
    FIELD-SYMBOLS <v0> TYPE rsdump_ft. "VALUE
    DATA lv_index              TYPE i.
    DATA ls_varinfo            TYPE ts_varinfo.
    FIELD-SYMBOLS <varinfo>    TYPE ts_varinfo.


    LOOP AT mt_dump_fields ASSIGNING <n0> USING KEY by_id WHERE id = 'N0' .
      CHECK <n0>-value(1) <> '%'. "not needed: System variables
      lv_index = <n0>-prim_line_nr + 1.
      ls_varinfo-name = <n0>-value.

      READ TABLE mt_dump_fields ASSIGNING <l0> INDEX lv_index.
      IF sy-subrc = 0 AND <l0>-id = 'L0'.
        lv_index = lv_index + 1.
        ls_varinfo-length = <l0>-value.
        READ TABLE mt_dump_fields ASSIGNING <v0> INDEX lv_index.
        IF sy-subrc = 0 AND <v0>-id = 'V0'.
          ls_varinfo-value        = <v0>-value.
          INSERT ls_varinfo INTO TABLE mt_varinfo.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "Sy-values are stored different:
    "SY-TITLE[AppKnight] dump info
    LOOP AT mt_dump_fields ASSIGNING <n0> USING KEY by_id WHERE id = 'SY'.
      ls_varinfo-name         = <n0>-value(8). "SY-ABCDE
      ls_varinfo-length       = <n0>-len - 8.  "reduce length of length of variable name
      ls_varinfo-value        = <n0>-value+8.  "get SY-value
      INSERT ls_varinfo INTO TABLE mt_varinfo.
    ENDLOOP.


  ENDMETHOD.

ENDCLASS.




CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    TYPES tt_snap TYPE STANDARD TABLE OF snap WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ts_dumpvar,
        symandt     TYPE symandt,
        sydate      TYPE sydatum,
        sytime      TYPE syuzeit,
        syhost      TYPE syhost,
        syuser      TYPE syuname,
        symodno     TYPE snap-modno,
        syseqno     TYPE snap-seqno,
        dumpid      TYPE dumpid,
        programname TYPE progname,
        includename TYPE progname,
        linenumber  TYPE  abp_sline,
        value01     TYPE c LENGTH 80,
        value02     TYPE c LENGTH 80,
        value03     TYPE c LENGTH 80,
        value04     TYPE c LENGTH 80,
        value05     TYPE c LENGTH 80,
      END OF ts_dumpvar,
      tt_dumpvar TYPE STANDARD TABLE OF ts_dumpvar WITH DEFAULT KEY.

    CLASS-DATA mt_dumpvars     TYPE tt_dumpvar.


    CLASS-METHODS get_dump_info
      IMPORTING
        it_snap            TYPE tt_snap
      RETURNING
        VALUE(rt_dumpvars) TYPE tt_dumpvar.

    CLASS-METHODS display
      IMPORTING
        it_dumpvars TYPE tt_dumpvar.

    CLASS-METHODS handle_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        row
        column.


ENDCLASS.


CLASS lcl_main IMPLEMENTATION.

  METHOD get_dump_info.

    DATA ls_dumpinfo     TYPE rsdumpinfo.
    DATA lt_dumpinfo     TYPE rsdumptab.
    DATA lt_dumpinfo_day TYPE rsdumptab.

    DATA ls_dumpvar TYPE ts_dumpvar.

    DATA lo_dump TYPE REF TO lcl_dump.

    FIELD-SYMBOLS <buffer> TYPE c.

    TYPES:
      BEGIN OF ts_day,
        date TYPE d,
      END OF ts_day,
      tt_days TYPE SORTED TABLE OF ts_day WITH UNIQUE KEY date.

    DATA(lt_days) = VALUE tt_days( FOR GROUPS <day> OF day IN it_snap GROUP BY day-datum ( date = <day> ) ).

    LOOP AT lt_days INTO DATA(ls_day).

      CALL FUNCTION 'RS_ST22_GET_DUMPS'
        EXPORTING
          p_day        = ls_day-date
        IMPORTING
          p_infotab    = lt_dumpinfo_day
        EXCEPTIONS
          no_authority = 1
          OTHERS       = 2.
      IF sy-subrc = 0.
        APPEND LINES OF lt_dumpinfo_day TO lt_dumpinfo.
      ENDIF.
    ENDLOOP.

    LOOP AT it_snap INTO DATA(ls_snap).

      CLEAR ls_dumpvar.

      lo_dump = NEW lcl_dump( ls_snap ).

      ls_dumpvar-sydate        = ls_snap-datum.
      ls_dumpvar-sytime        = ls_snap-uzeit.
      ls_dumpvar-syhost        = ls_snap-ahost.
      ls_dumpvar-syuser        = ls_snap-uname.
      ls_dumpvar-symodno       = ls_snap-modno.
      ls_dumpvar-symandt       = ls_snap-mandt.
      ls_dumpvar-syseqno       = ls_snap-seqno.

      TRY.
          ls_dumpinfo = lt_dumpinfo[
            sydate = ls_snap-datum
            sytime = ls_snap-uzeit
            syhost = ls_snap-ahost
            syuser = ls_snap-uname ].

          CHECK ls_dumpinfo-dumpid IN so_dump.

          ls_dumpvar-dumpid        = ls_dumpinfo-dumpid.
          ls_dumpvar-programname   = ls_dumpinfo-programname.
          ls_dumpvar-includename   = ls_dumpinfo-includename.
          ls_dumpvar-linenumber    = ls_dumpinfo-linenumber.



        CATCH cx_sy_itab_line_not_found.
          ls_dumpvar-dumpid        = space.
          ls_dumpvar-programname   = space.
          ls_dumpvar-includename   = space.
          ls_dumpvar-linenumber    = space.
      ENDTRY.

      ls_dumpvar-value01 = lo_dump->get_variable( iv_varname = pa_fld01 ).
      ls_dumpvar-value02 = lo_dump->get_variable( iv_varname = pa_fld02 ).
      ls_dumpvar-value03 = lo_dump->get_variable( iv_varname = pa_fld03 ).
      ls_dumpvar-value04 = lo_dump->get_variable( iv_varname = pa_fld04 ).
      ls_dumpvar-value05 = lo_dump->get_variable( iv_varname = pa_fld05 ).

      IF ls_dumpvar-value01 IS NOT INITIAL
      OR ls_dumpvar-value02 IS NOT INITIAL
      OR ls_dumpvar-value03 IS NOT INITIAL
      OR ls_dumpvar-value04 IS NOT INITIAL
      OR ls_dumpvar-value05 IS NOT INITIAL.
        INSERT ls_dumpvar INTO TABLE rt_dumpvars.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD display.

    mt_dumpvars = it_dumpvars.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(lr_salv_table)
          CHANGING
            t_table        = mt_dumpvars ).

        lr_salv_table->get_functions( )->set_all( ).
        DATA(lr_columns) = lr_salv_table->get_columns( ).
        lr_columns->set_optimize( abap_true ).

        DO 5 TIMES.
          DATA(lv_index) = CONV numc2( sy-index ).
          DATA(lv_fld) = |PA_FLD{ lv_index  }|.
          ASSIGN (lv_fld) TO FIELD-SYMBOL(<lfv_val>).
          DATA(lr_column)  = lr_columns->get_column( |VALUE{ lv_index }| ).
          IF <lfv_val> IS INITIAL.
            lr_column->set_technical( ).
          ELSE.
            lr_column->set_fixed_header_text( CONV #( <lfv_val> ) ).
            lr_column->set_short_text( CONV #( <lfv_val> ) ).
            lr_column->set_medium_text( CONV #( <lfv_val> ) ).
            lr_column->set_long_text( <lfv_val> ).
          ENDIF.
        ENDDO.

        SET HANDLER handle_double_click FOR lr_salv_table->get_event( ).

        lr_salv_table->display( ).

      CATCH cx_salv_msg cx_salv_not_found INTO DATA(lo_error).
        MESSAGE lo_error TYPE 'I'.
    ENDTRY.

  ENDMETHOD.

  METHOD handle_double_click.

    TRY.
        DATA(ls_dump) = mt_dumpvars[ row ].
        CALL FUNCTION 'RS_SNAP_DUMP_DISPLAY'
          EXPORTING
            ahost          = ls_dump-syhost
            datum          = ls_dump-sydate
            mandt          = ls_dump-symandt
            modno          = ls_dump-symodno
            seqno          = '000'
            uname          = ls_dump-syuser
            uzeit          = ls_dump-sytime
          EXCEPTIONS
            no_entry_found = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  PERFORM read.

FORM read.

  DATA lt_snap TYPE STANDARD TABLE OF snap.

  SELECT * FROM snap
    INTO TABLE lt_snap
   WHERE datum IN so_date
     AND seqno = '000'.

  CHECK sy-subrc = 0.


  DATA(lt_dumpvars) = lcl_main=>get_dump_info( lt_snap ).
  lcl_main=>display( lt_dumpvars ).

ENDFORM.
