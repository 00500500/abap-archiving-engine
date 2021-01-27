class ZCL_ARE_CACHE definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF gtys_internal_tables,
        tabname             TYPE tabname,
        archive_customizing TYPE arch_def,
        content_ref         TYPE REF TO data,
      END OF gtys_internal_tables .
  types:
    gtyt_internal_tables TYPE TABLE OF gtys_internal_tables WITH KEY tabname .

  types:
    BEGIN OF gtys_are_param,
      archive_object TYPE objct_tr01,
      test_run TYPE abap_bool,
    END OF gtys_are_param.


  data ARCHIVE_OBJECT type OBJCT_TR01 .
  data TEST_RUN type ABAP_BOOL .
  data T_INTERNAL_TABLES type GTYT_INTERNAL_TABLES .
  data:
    t_garbage_tables TYPE TABLE OF arch_dele .
  data:
    t_archive_classes TYPE TABLE OF arch_oclas .
  data O_LOG type ref to ZCL_ARE_LOGGER .

  methods CLEAR_INTERNAL_TABLES .
  methods CONSTRUCTOR
    importing
      !IS_PARAMETERS type gtys_are_param
    raising
      CX_ABAP_NOT_A_TABLE
      CX_ICL_CUSTOMIZING_NO_ENTRY .
  methods GET_TABLE_CONTENT_BY_TABNAME
    importing
      !I_TABNAME type TABNAME
    returning
      value(RR_CONTENT) type ref to DATA .
private section.

  methods BUILD_CONTENT_TABLE
    importing
      !I_TABNAME type TABNAME
    returning
      value(RR_RESULT) type ref to DATA
    raising
      CX_ABAP_NOT_A_TABLE .
  methods FILL_INTERNAL_TABLES
    raising
      CX_ABAP_NOT_A_TABLE
      CX_ICL_CUSTOMIZING_NO_ENTRY .
  methods VALIDATE_ARCHIVE_OBJECT
    raising
      CX_ARCHOBJ_NOT_FOUND .
ENDCLASS.



CLASS ZCL_ARE_CACHE IMPLEMENTATION.

  METHOD build_content_table.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          lo_tabledescr  TYPE REF TO cl_abap_tabledescr.

    TRY.
        lo_structdescr ?= cl_abap_typedescr=>describe_by_name( i_tabname ).
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION TYPE cx_abap_not_a_table.
    ENDTRY.

    lo_tabledescr ?= cl_abap_tabledescr=>create( lo_structdescr ).
    CREATE DATA rr_result TYPE HANDLE lo_tabledescr.

  ENDMETHOD.


  METHOD clear_internal_tables.

    FIELD-SYMBOLS: <lg_content_row> TYPE any.

    LOOP AT t_internal_tables REFERENCE INTO DATA(lr_table).

        ASSIGN lr_table->content_ref->* TO <lg_content_row>.
        CLEAR <lg_content_row>.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    archive_object = is_parameters-archive_object.
    test_run = is_parameters-test_run.
    validate_archive_object( ).

    o_log = new zcl_are_logger( ).

    fill_internal_tables( ).

  ENDMETHOD.


  METHOD fill_internal_tables.

    SELECT *
    FROM arch_def
    INTO TABLE @DATA(lt_customizing)
    WHERE object = @archive_object.

    IF lt_customizing IS INITIAL.
      RAISE EXCEPTION TYPE cx_icl_customizing_no_entry.
    ENDIF.

    SORT lt_customizing BY sequence ASCENDING.

    LOOP AT lt_customizing REFERENCE INTO DATA(lr_customizing_row).

      INSERT VALUE #( tabname = lr_customizing_row->son
                      archive_customizing = lr_customizing_row->*
                      content_ref = build_content_table( lr_customizing_row->son ) ) INTO TABLE t_internal_tables.
    ENDLOOP.

    SELECT *
    FROM arch_dele
    INTO TABLE t_garbage_tables
    WHERE object = archive_object.

    SELECT *
    FROM arch_oclas
    INTO TABLE t_archive_classes
    WHERE object = archive_object.


  ENDMETHOD.


  METHOD get_table_content_by_tabname.

    rr_content = VALUE #( t_internal_tables[ tabname = i_tabname ]-content_ref OPTIONAL ).

  ENDMETHOD.


  METHOD validate_archive_object.

    SELECT SINGLE @abap_true
    FROM arch_obj
    INTO @DATA(l_arch_obj_exists)
    WHERE object = @archive_object.

    IF l_arch_obj_exists EQ abap_false.
      RAISE EXCEPTION TYPE cx_archobj_not_found.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
