*"* use this source file for your ABAP unit test classes
class lclt_archive_cache definition final for testing
  duration short
  risk level harmless.

  private section.

    data o_cut TYPE REF TO zcl_are_cache.

    methods:
      setup,
      get_table_content_by_name for testing raising cx_static_check,
      clear_internal_tables FOR TESTING RAISING cx_static_check.
endclass.


class lclt_archive_cache implementation.

  method setup.

    try.
        o_cut = new zcl_are_cache( VALUE #( archive_object = 'SD_VBKA'
                                                test_run = abap_true ) ).
    catch cx_root.
        cl_abap_unit_assert=>assert_bound( act = o_cut ).
    ENDTRY.

  endmethod.

  method get_table_content_by_name.

    cl_abap_unit_assert=>assert_bound( act = o_cut->get_table_content_by_tabname( 'VBKA' ) ).

  endmethod.

  method clear_internal_tables.

    FIELD-SYMBOLS: <lg_content_row> TYPE any.


    o_cut->clear_internal_tables( ).

    LOOP AT o_cut->t_internal_tables REFERENCE INTO DATA(lr_content).

        ASSIGN lr_content->content_ref->* TO <lg_content_row>.
        cl_abap_unit_assert=>assert_initial( act = <lg_content_row> ).

    ENDLOOP.

  ENDMETHOD.

endclass.
