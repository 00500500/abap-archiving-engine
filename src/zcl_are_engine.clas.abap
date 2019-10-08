class ZCL_ARE_ENGINE definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IS_PARAMETER type ZCL_ARE_CACHE=>GTYS_ARE_PARAM
    raising
      CX_ABAP_NOT_A_TABLE
      CX_ICL_CUSTOMIZING_NO_ENTRY .
  "! <p class="shorttext synchronized" lang="en">Write</p>
  "!
  "! @parameter ita_keys | <p class="shorttext synchronized" lang="en">Tab of business object keys</p>
  methods WRITE
    importing
      !ITA_KEYS type ANY TABLE .
  "! <p class="shorttext synchronized" lang="en">Delete</p>
  methods DELETE .
  "! <p class="shorttext synchronized" lang="en">Reload</p>
  methods RELOAD .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA o_cache TYPE REF TO zcl_are_cache.
    DATA o_archive_session TYPE REF TO zcl_are_archive_session.
    DATA o_database_session TYPE REF TO zcl_are_database_session.


ENDCLASS.



CLASS ZCL_ARE_ENGINE IMPLEMENTATION.


  METHOD constructor.

    o_cache = NEW Zcl_are_cache( is_parameter ).
    o_archive_session = NEW Zcl_are_archive_session( o_cache ).
    o_database_session = NEW Zcl_are_database_session( o_cache ).

  ENDMETHOD.


  METHOD delete.

    o_archive_session->open( areg_mode_delete ).
    o_archive_session->delete( ).
    o_archive_session->close( ).

  ENDMETHOD.


  METHOD reload.

    o_archive_session->open( areg_mode_reload ).
    o_archive_session->reload( ).
    o_archive_session->close( ).

  ENDMETHOD.


  METHOD write.

    IF ita_keys IS INITIAL.
      MESSAGE e007(00) WITH |Table with keys|.
    ENDIF.

    o_archive_session->open( areg_mode_write ).


    LOOP AT ita_keys ASSIGNING FIELD-SYMBOL(<lg_key>).

      DATA(l_value_as_string) = zcl_string=>create_from_structure( <lg_key> )->get_value( ).

      o_cache->o_log->new_object( l_value_as_string ).
      o_cache->clear_internal_tables( ).
      o_database_session->fill_internal_tables( <lg_key> ).
      o_archive_session->write( l_value_as_string ).

    ENDLOOP.

    o_archive_session->close( ).

  ENDMETHOD.
ENDCLASS.
