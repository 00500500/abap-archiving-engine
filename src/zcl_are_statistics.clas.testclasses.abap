*"* use this source file for your ABAP unit test classes
CLASS lclt_archive_statistics DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA o_cut TYPE REF TO zcl_are_statistics.

    METHODS:
      setup,
      increment_stats IMPORTING i_tabname TYPE tabname
                                i_amount  TYPE i,
      increment_statistics FOR TESTING RAISING cx_static_check,
      clear FOR TESTING RAISING cx_static_check,
      get_stats_works FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS lclt_archive_statistics IMPLEMENTATION.

  METHOD setup.

    o_cut = NEW zcl_are_statistics( ).

  ENDMETHOD.

  METHOD clear.

    o_cut->increment_statistics( i_tabname = 'VBAK'
                     i_amount = 1 ).

    o_cut->clear( ).

    cl_abap_unit_assert=>assert_initial( act = o_cut->get_statistcs( ) ).

  ENDMETHOD.

  METHOD get_stats_works.

    o_cut->clear( ).
    o_cut->increment_statistics( i_tabname = 'VBAK'
                                 i_amount = 1 ).

    DATA(lt_stats) = o_cut->get_statistcs( ).

    TRY.
        DATA(ls_statistics) = lt_stats[ tabname = 'VBAK' ].
      CATCH cx_sy_itab_line_not_found.
        cl_abap_unit_assert=>fail( `Error in get_statistics` ).
    ENDTRY.

    DATA ls_exp TYPE arch_stat.
    ls_exp-tabname = 'VBAK'.
    ls_exp-count = 1.



    cl_abap_unit_assert=>assert_equals( act = ls_statistics
                                        exp = ls_exp ).

  ENDMETHOD.

  METHOD increment_stats.

    DATA(lt_stats) = o_cut->get_statistcs( ).
    DATA ls_stat_before TYPE arch_stat.

    TRY.
        ls_stat_before = lt_stats[ tabname = i_tabname ].
      CATCH cx_root.
        ls_stat_before-tabname = i_tabname.
    ENDTRY.

    ADD i_amount TO ls_stat_before-count.

    o_cut->increment_statistics( i_tabname = i_tabname
                                 i_amount = i_amount ).

    lt_stats = o_cut->get_statistcs( ).
    DATA(ls_stat_after) = lt_stats[ tabname = i_tabname ].
    cl_abap_unit_assert=>assert_equals( act = ls_stat_after
                                        exp = ls_stat_before ).
  ENDMETHOD.

  METHOD increment_statistics.
    increment_stats( i_tabname = 'VBAK'
                     i_amount = 1 ).
    increment_stats( i_tabname = 'VBAK'
                     i_amount = 10 ).
    increment_stats( i_tabname = 'VBUK'
                     i_amount = 25 ).
    increment_stats( i_tabname = 'VBRK'
                     i_amount = 11 ).
    increment_stats( i_tabname = 'VBAK'
                     i_amount = 12 ).
  ENDMETHOD.

ENDCLASS.
