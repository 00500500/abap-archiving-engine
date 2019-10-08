class ZCL_ARE_LOGGER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF gtys_layout,
            objkey TYPE char32,
            risk_level TYPE i,
            msg TYPE char256,
           END OF gtys_layout .
  types:
    gtyt_layout TYPE TABLE OF gtys_layout WITH KEY objkey .

  constants GC_NOTICE type I value 3 ##NO_TEXT.
  constants GC_WARNING type I value 2 ##NO_TEXT.
  constants GC_ERROR type I value 1 ##NO_TEXT.

  methods LOG_ERROR
    importing
      !I_MSG type STRING .
  methods LOG_HEADER_MSG
    importing
      !I_MSG type STRING .
  methods LOG_NOTICE
    importing
      !I_MSG type STRING .
  methods LOG_WARNING
    importing
      !I_MSG type STRING .
  methods NEW_OBJECT
    importing
      !I_OBJKEY type STRING .
  methods WRITE .
  methods GET_LOG
            RETURNING
              VALUE(rt_log) TYPE gtyt_layout.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: t_log TYPE TABLE OF gtys_layout.
    DATA: t_log_header TYPE TABLE OF string with EMPTY KEY.

    DATA: current_object TYPE string.
    METHODS set_color
      IMPORTING
        i_log_entry_risk_level TYPE i.

endclass.



class ZCL_ARE_LOGGER implementation.

  METHOD get_log.

    rt_log = t_log.

  ENDMETHOD.

  METHOD log_error.

    APPEND VALUE #( objkey = current_object
                    risk_level = gc_error
                    msg = i_msg ) TO t_log.

  ENDMETHOD.


  METHOD log_header_msg.

    APPEND i_msg TO t_log_header.

  ENDMETHOD.


  METHOD log_notice.

    APPEND VALUE #( objkey = current_object
                    risk_level = gc_notice
                    msg = i_msg ) TO t_log.

  ENDMETHOD.


  METHOD log_warning.

    APPEND VALUE #( objkey = current_object
                    risk_level = gc_warning
                    msg = i_msg ) TO t_log.

  ENDMETHOD.


  METHOD new_object.

    current_object = i_objkey.

  ENDMETHOD.


  METHOD write.

    DATA: l_last_objkey TYPE string.

    LOOP AT t_log_header REFERENCE INTO DATA(lr_header_entry).

        WRITE / lr_header_entry->*.

    ENDLOOP.

    LOOP AT t_log REFERENCE INTO DATA(lr_log_entry).

        IF lr_log_entry->objkey NE l_last_objkey.
            WRITE / space.
        ENDIF.

        set_color( lr_log_entry->risk_level ).

        WRITE: / |{ CONV string( lr_log_entry->objkey ) },{ COND string( WHEN lr_log_entry->risk_level EQ 1
                                                                         THEN |[ERROR]|
                                                                         WHEN lr_log_entry->risk_level EQ 2
                                                                         THEN |[WARNING]|
                                                                         WHEN lr_log_entry->risk_level EQ 3
                                                                         THEN |[NOTICE]| ) },{ CONV string( lr_log_entry->msg ) }|.
        l_last_objkey = lr_log_entry->objkey.

    ENDLOOP.

  ENDMETHOD.

  METHOD set_color.

    CASE i_log_entry_risk_level.
        WHEN 1.
            FORMAT COLOR COL_NEGATIVE. "red
        WHEN 2.
            FORMAT COLOR COL_TOTAL. "yellow
        WHEN 3.
            FORMAT COLOR COL_NORMAL. "light grey
        WHEN OTHERS.
            FORMAT COLOR COL_BACKGROUND. "background color
    ENDCASE.

  ENDMETHOD.


endclass.
