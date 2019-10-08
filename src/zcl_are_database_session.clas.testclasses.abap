*"* use this source file for your ABAP unit test classes
class lclt_ar_db_session definition final for testing
  duration short
  risk level harmless.

  private section.

    data o_cut TYPE REF TO zcl_are_database_session.
    data o_cache TYPE REF TO zcl_are_cache.

    methods:
      setup,

      fill_internal_tables FOR TESTING RAISING cx_static_check.

endclass.


class lclt_ar_db_session implementation.

  method setup.

    TRY.
        o_cache = new zcl_are_cache( VALUE #( archive_object = 'SD_VBAK'
                                                  test_run = abap_true ) ).
    CATCH cx_root.
        cl_abap_unit_assert=>assert_bound( act = o_cache ).
    ENDTRY.

    o_cut = new zcl_are_database_session( o_cache ).

  endmethod.

  method fill_internal_tables.

    SELECT SINGLE vbeln
    FROM VBAK
    INTO @DATA(l_vbeln).

    IF sy-subrc NE 0.
        cl_abap_unit_assert=>fail('NO TEST DATA FOUND').
    ENDIF.

    o_cut->fill_internal_tables( l_vbeln ).

    DATA(lr_content) = o_cache->get_table_content_by_tabname( 'VBAK' ).
    ASSIGN lr_content->* TO FIELD-SYMBOL(<lg_content>).

    cl_abap_unit_assert=>assert_not_initial( act = <lg_content> ).

  ENDMETHOD.

endclass.
