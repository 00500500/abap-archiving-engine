CLASS zcl_are_database_session DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  methods CONSTRUCTOR
    importing
      !IO_CACHE type ref to ZCL_ARE_CACHE .
  methods DELETE_FROM_TABLES
    importing
      !IO_STATISTICS type ref to ZCL_ARE_STATISTICS .
  methods FILL_INTERNAL_TABLES
    importing
      !IG_KEY type ANY .
  methods INSERT_FROM_TABLE
    importing
      !IO_STATISTICS type ref to ZCL_ARE_STATISTICS .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA o_cache TYPE REF TO zcl_are_cache.
    DATA o_dynamic_selct TYPE REF TO zcl_dynamic_select.


ENDCLASS.



CLASS zcl_are_database_session IMPLEMENTATION.

  METHOD constructor.

    o_cache = io_cache.

  ENDMETHOD.


  METHOD delete_from_tables.

    FIELD-SYMBOLS: <lta_table_content> TYPE ANY TABLE.


    LOOP AT o_cache->t_internal_tables REFERENCE INTO DATA(lr_table).

      ASSIGN lr_table->content_ref->* TO <lta_table_content>.
      IF <lta_table_content> IS INITIAL.
        CONTINUE.
      ENDIF.

      IF o_cache->test_run EQ abap_false.

        DELETE (lr_table->tabname) FROM TABLE <lta_table_content>.
        IF sy-subrc EQ 0.
          io_statistics->increment_statistics( i_tabname = lr_table->tabname
                                               i_amount  = sy-dbcnt ).
        ELSE.
          o_cache->o_log->log_error( |Delete from { lr_table->tabname } failed| ).
        ENDIF.

      ELSE.

        io_statistics->increment_statistics( i_tabname = lr_table->tabname
                                             i_amount  = lines( <lta_table_content> ) ).

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_internal_tables.

    FIELD-SYMBOLS: <lt_table_content> TYPE ANY TABLE.


    LOOP AT o_cache->t_internal_tables REFERENCE INTO DATA(lr_table).

      UNASSIGN <lt_table_content>.
      ASSIGN lr_table->content_ref->* TO <lt_table_content>.


      o_dynamic_selct = zcl_dynamic_select=>get_instance( lr_table->tabname ).
      TRY.
          o_dynamic_selct->add_where_cond_key_to_key( ig_key ).

        CATCH cx_mi_no_value_found.

          o_cache->o_log->log_error( |Dynamic Select value could not be inserted into where cond| ).
          CONTINUE.

        CATCH cx_taan_field_not_found.

          o_cache->o_log->log_error( |Dynamic Select mapping failed to table { lr_table->tabname }| ).
          CONTINUE.

      ENDTRY.
      o_dynamic_selct->select( IMPORTING et_select_result = <lt_table_content> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD insert_from_table.

    FIELD-SYMBOLS: <lta_table_content> TYPE ANY TABLE.


    LOOP AT o_cache->t_internal_tables REFERENCE INTO DATA(lr_table).

      ASSIGN lr_table->content_ref->* TO <lta_table_content>.
      IF <lta_table_content> IS INITIAL.
        CONTINUE.
      ENDIF.

      IF o_cache->test_run EQ abap_false.

        INSERT (lr_table->tabname) FROM TABLE <lta_table_content>.

        IF sy-subrc EQ 0.
          io_statistics->increment_statistics( i_tabname = lr_table->tabname
                                               i_amount  = sy-dbcnt ).
        ELSE.
          o_cache->o_log->log_error( |Insert into { lr_table->tabname } failed| ).
        ENDIF.

      ELSE.

        io_statistics->increment_statistics( i_tabname = lr_table->tabname
                                             i_amount  = lines( <lta_table_content> ) ).

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
