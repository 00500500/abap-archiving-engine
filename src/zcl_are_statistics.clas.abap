CLASS zcl_are_statistics DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      gtyt_arch_stat TYPE TABLE OF arch_stat WITH KEY tabname .

    METHODS clear.
    METHODS get_statistcs
      RETURNING VALUE(rt_statistics) TYPE gtyt_arch_stat.
    METHODS increment_statistics
      IMPORTING
        i_tabname TYPE tabname
        i_amount  TYPE i DEFAULT 1.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA t_statistics TYPE TABLE OF arch_stat.
ENDCLASS.



CLASS zcl_are_statistics IMPLEMENTATION.


  METHOD clear.

    CLEAR me->t_statistics.

  ENDMETHOD.


  METHOD get_statistcs.

    rt_statistics = me->t_statistics.

  ENDMETHOD.


  METHOD increment_statistics.

    DATA(l_table_index) = line_index( me->t_statistics[ tabname = i_tabname ] ).

    IF i_amount LE 0.
        RAISE EXCEPTION TYPE cx_cdc_invalid_number
          EXPORTING
            textid   = VALUE #( msgid = 'FDT_EXPRESSIONS'
                                msgno = 750 ).
    ENDIF.

    IF l_table_index GT 0.

      ASSIGN me->t_statistics[ l_table_index ] TO FIELD-SYMBOL(<ls_table_statistic>).
      ADD i_amount TO <ls_table_statistic>-count.

    ELSE.

      APPEND VALUE #( tabname = i_tabname
                      count   = i_amount ) TO me->t_statistics.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
