class ZCL_ARE_ARCHIVE_SESSION definition
  public
  final
  create public .

public section.

  methods CLOSE .
  methods CONSTRUCTOR
    importing
      !IO_CACHE type ref to ZCL_ARE_CACHE .
  methods DELETE .
  methods OPEN
    importing
      !I_MODE type CHAR1 .
  methods RELOAD .
  methods WRITE
    importing
      !IG_KEY type ANY .
  PROTECTED SECTION.
private section.

  data HANDLE type SY-TABIX .
  data WRITE_HANDLE_FOR_RELOAD type SY-TABIX .
  data OBJECTS_NEEDED_FOR_COMMIT type ARCH_USR-ARCH_COMIT .
  data OBJECT_COUNTER type ARCH_USR-ARCH_COMIT .
  data O_DB_SESSION type ref to ZCL_ARE_DATABASE_SESSION .
  data O_CACHE type ref to ZCL_ARE_CACHE .
  data O_STATISTICS type ref to ZCL_ARE_STATISTICS .

  methods ARCHIVE_CLASSES
    importing
      !IG_KEY type ANY .
  methods ARCHIVE_CLASSES_DELETE_DATA .
  methods ARCHIVE_CLASSES_RELOAD_DATA .
  methods CLASS_CHANGEDOCU_WRITE
    importing
      !IG_KEY type ANY .
  methods CLASS_CLASSIFY_WRITE
    importing
      !IG_KEY type ANY .
  methods CLASS_STATUS_WRITE
    importing
      !IG_KEY type ANY .
  methods CLASS_TEXT_WRITE
    importing
      !IG_KEY type ANY .
  methods CLEAR_AFTER_COMMIT .
  methods COMMIT_DB_DELETE .
  methods COMMIT_DB_INSERT .
  methods GIVE_STATISTICS_TO_ARCHIVE .
  methods NEW_OBJECT .
  methods OPEN_FOR_DELETE
    raising
      CX_ICL_CUSTOMIZING_NO_ENTRY .
  methods OPEN_FOR_RELOAD .
  methods OPEN_FOR_WRITE .
  methods PUT_RECORD
    importing
      !IG_RECORD type ANY
      !I_TABNAME type TABNAME .
  methods READ_ARCHIVE_OBJECT
    returning
      value(R_NOT_END_OF_FILE) type ABAP_BOOL .
  methods READ_ARCHIVE_RECORDS .
  methods READ_OBJECTS_NEEDED_FOR_COMMIT
    raising
      CX_ICL_CUSTOMIZING_NO_ENTRY .
  methods REGISTER_CLASSES .
  methods SAVE_OBJECT .
  methods WRITE_STATISTICS .
ENDCLASS.



CLASS ZCL_ARE_ARCHIVE_SESSION IMPLEMENTATION.


  METHOD archive_classes.

    LOOP AT o_cache->t_archive_classes REFERENCE INTO DATA(lr_arch_class).

      CASE lr_arch_class->arch_class.
        WHEN 'CHANGEDOCU'.
          class_changedocu_write( ig_key ).
        WHEN 'CLASSIFY'.
          class_classify_write( ig_key ).
        WHEN 'STATUS'.
          class_status_write( ig_key ).
        WHEN 'TEXT'.
          class_text_write( ig_key ).
        WHEN OTHERS.
          o_cache->o_log->log_warning( |Archive class { lr_arch_class->arch_class } not implemented in ARE or does not exist| ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD archive_classes_delete_data.

    CALL FUNCTION 'ARCHIVE_DELETE_OBJECT_DATA'
      EXPORTING
        archive_handle          = handle
      EXCEPTIONS
        internal_error          = 1
        wrong_access_to_archive = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      o_cache->o_log->log_warning( |Archive class delete data with handle { handle } { sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv4 }| ).
    ENDIF.

  ENDMETHOD.


  METHOD archive_classes_reload_data.

    CALL FUNCTION 'ARCHIVE_RELOAD_OBJECT_DATA'
      EXPORTING
        archive_handle          = handle    " Open Archive Handle
      EXCEPTIONS
        internal_error          = 1
        wrong_access_to_archive = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD class_changedocu_write.

    DATA(l_objectid) = CONV cdhdr-objectid( ig_key ).

    SELECT SINGLE objectclas                            "#EC CI_NOFIRST
    FROM cdhdr
    INTO @DATA(l_class)
    WHERE objectid = @l_objectid.

    IF sy-subrc NE 0.
      o_cache->o_log->log_notice( |Change document data expected but not found| ).
      RETURN.
    ENDIF.

    DATA(lo_changedocu) = NEW cl_ishmed_adk_class_changedocu( i_class = l_class
                                                              i_id = l_objectid ).

    TRY.
        lo_changedocu->if_ishmed_adk_class_data~write( i_handle =  handle ).
        lo_changedocu->if_ishmed_adk_class_data~finalize( ).
      CATCH cx_ish_static_handler.
        o_cache->o_log->log_notice( |Change document data found -> archiving it threw an error| ).
    ENDTRY.

  ENDMETHOD.


  METHOD class_classify_write.

    "classify main table
    DATA(l_tabname) =  o_cache->t_internal_tables[ 1 ]-tabname.

    DATA(lo_classify) = NEW cl_ishmed_adk_class_classify( i_object_key = CONV #( ig_key )
                                                          i_object_table = l_tabname ).

    TRY.
        lo_classify->if_ishmed_adk_class_data~write( i_handle = handle ).
        lo_classify->if_ishmed_adk_class_data~finalize( ).
      CATCH cx_ish_static_handler.
        o_cache->o_log->log_notice( |Classify data found -> archiving it threw an error| ).
    ENDTRY.

  ENDMETHOD.


  METHOD class_status_write.

    DATA(lo_status) = NEW cl_ishmed_adk_class_status( i_objnr = CONV #( ig_key ) ).

    TRY.
        lo_status->if_ishmed_adk_class_data~write( i_handle =  handle ).
        lo_status->if_ishmed_adk_class_data~finalize( ).
      CATCH cx_ish_static_handler.
        o_cache->o_log->log_notice( |Status data found -> archiving it threw an error| ).
    ENDTRY.

  ENDMETHOD.


  METHOD class_text_write.

    DATA(lo_text) = NEW cl_ishmed_adk_class_text( i_id = '*'
                                                  i_name = |{ ig_key }*|
                                                  i_object = '*' ).

    TRY.
        lo_text->if_ishmed_adk_class_data~write( i_handle =  handle ).
        lo_text->if_ishmed_adk_class_data~finalize( ).
      CATCH cx_ish_static_handler.
        o_cache->o_log->log_notice( |Texts found -> but archiving threw an error| ).
    ENDTRY.

  ENDMETHOD.


  METHOD clear_after_commit.

    CLEAR object_counter.
    o_cache->clear_internal_tables( ).
    o_statistics->clear( ).

  ENDMETHOD.


  METHOD close.

    write_statistics( ).

    CALL FUNCTION 'ARCHIVE_CLOSE_FILE'
      EXPORTING
        archive_handle               = handle    " Open Archive Handle
      EXCEPTIONS
        internal_error               = 1
        wrong_access_to_archive      = 2
        archiving_standard_violation = 3
        OTHERS                       = 4.
    IF sy-subrc <> 0.
      o_cache->o_log->log_error( |Archive close { sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv4 }| ).
    ENDIF.

    o_cache->o_log->write( ).

  ENDMETHOD.


  METHOD commit_db_delete.

    o_db_session->delete_from_tables( o_statistics ).
    give_statistics_to_archive( ).

    COMMIT WORK.
    clear_after_commit( ).


  ENDMETHOD.


  METHOD commit_db_insert.

    o_db_session->insert_from_table( o_statistics ).
    give_statistics_to_archive( ).

    COMMIT WORK.
    clear_after_commit( ).

  ENDMETHOD.


  METHOD constructor.

    o_cache = io_cache.
    o_db_session = NEW Zcl_are_database_session( o_cache ).
    o_statistics = NEW Zcl_are_statistics( ).

  ENDMETHOD.


  METHOD delete.

    "as long as it's not the last object
    WHILE read_archive_object( ).

      archive_classes_delete_data( ).

      ADD 1 TO object_counter.

      read_archive_records( ).

      IF object_counter GE objects_needed_for_commit.

        commit_db_delete( ).

      ENDIF.

    ENDWHILE.

    "for last package of records
    IF object_counter LT objects_needed_for_commit
    AND object_counter NE 0.

      commit_db_delete( ).

    ENDIF.

  ENDMETHOD.


  METHOD give_statistics_to_archive.

    DATA(lt_statistics) = o_statistics->get_statistcs( ).

    CALL FUNCTION 'ARCHIVE_GIVE_STATISTICS'
      EXPORTING
        archive_handle          = handle
      TABLES
        table                   = lt_statistics
      EXCEPTIONS
        internal_error          = 1
        wrong_access_to_archive = 2
        OTHERS                  = 3.
    IF sy-subrc NE 0.
      o_cache->o_log->log_warning( |Archive give statistics { sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv4 }| ).
    ENDIF.

  ENDMETHOD.


  METHOD new_object.
    CALL FUNCTION 'ARCHIVE_NEW_OBJECT'
      EXPORTING
        archive_handle          = me->handle    " Handle to the Open Archive
      EXCEPTIONS
        internal_error          = 1
        wrong_access_to_archive = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      o_cache->o_log->log_error( |Archive new object { sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv4 }| ).
    ENDIF.
  ENDMETHOD.


  METHOD open.

    TRY.
        CASE i_mode.
          WHEN areg_mode_write.
            me->open_for_write( ).
          WHEN areg_mode_delete.
            me->open_for_delete( ).
          WHEN areg_mode_reload.
            me->open_for_reload( ).
        ENDCASE.
      CATCH cx_root.
        o_cache->o_log->write( ).
        MESSAGE e004(1b_archiving).
    ENDTRY.

  ENDMETHOD.


  METHOD open_for_delete.
    CALL FUNCTION 'ARCHIVE_OPEN_FOR_DELETE'
      EXPORTING
        object                       = o_cache->archive_object    " Archiving Object Name
        test_mode                    = o_cache->test_run    " Test Mode Switch
      IMPORTING
        archive_handle               = handle    " Handle to the Open Files
      EXCEPTIONS
        file_already_open            = 1
        file_io_error                = 2
        internal_error               = 3
        no_files_available           = 4
        object_not_found             = 5
        open_error                   = 6
        not_authorized               = 7
        archiving_standard_violation = 8
        OTHERS                       = 9.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    read_objects_needed_for_commit( ).

  ENDMETHOD.


  METHOD open_for_reload.
    CALL FUNCTION 'ARCHIVE_OPEN_FOR_MOVE'
      EXPORTING
        object                       = o_cache->archive_object    " Archiving Object Name
        test_mode                    = o_cache->test_run    " Reload Program Runs in Test Mode
      IMPORTING
        archive_read_handle          = handle    " Handle to the Open Files for Reading
        archive_write_handle         = write_handle_for_reload   " Handle to the Files Open for Writing
      EXCEPTIONS
        file_already_open            = 1
        file_io_error                = 2
        internal_error               = 3
        no_files_available           = 4
        object_not_found             = 5
        open_error                   = 6
        not_authorized               = 7
        archiving_standard_violation = 8
        OTHERS                       = 9.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    objects_needed_for_commit = 5.

  ENDMETHOD.


  METHOD open_for_write.

    "function call would dump without the helper variable
    DATA(l_create_files) = xsdbool( o_cache->test_run IS INITIAL ).

    CALL FUNCTION 'ARCHIVE_OPEN_FOR_WRITE'
      EXPORTING
        call_delete_job_in_test_mode = o_cache->test_run
        create_archive_file          = l_create_files
        object                       = o_cache->archive_object    " Archiving Object Name
      IMPORTING
        archive_handle               = handle    " Pointer to Open File
      EXCEPTIONS
        internal_error               = 1
        object_not_found             = 2
        open_error                   = 3
        not_authorized               = 4
        archiving_standard_violation = 5
        OTHERS                       = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    register_classes( ).
  ENDMETHOD.


  METHOD put_record.

    CALL FUNCTION 'ARCHIVE_PUT_RECORD'
      EXPORTING
        archive_handle           = me->handle    " Handle to the Open Archive Files
        record                   = ig_record    " Data Record
        record_structure         = i_tabname    " Name of the Structure to Be Written
      EXCEPTIONS
        internal_error           = 1
        wrong_access_to_archive  = 2
        invalid_record_structure = 3
        OTHERS                   = 4.
    IF sy-subrc <> 0.
      o_cache->o_log->log_error( |Archive put record { sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv4 }| ).
    ENDIF.

  ENDMETHOD.


  METHOD read_archive_object.

    CALL FUNCTION 'ARCHIVE_GET_NEXT_OBJECT'
      EXPORTING
        archive_handle          = handle
      EXCEPTIONS
        end_of_file             = 1
        file_io_error           = 2
        internal_error          = 3
        open_error              = 4
        wrong_access_to_archive = 5
        OTHERS                  = 6.
    IF sy-subrc EQ 0.
      r_not_end_of_file = abap_true.
    ELSEIF sy-subrc EQ 1.
      "end of file
      r_not_end_of_file = abap_false.
      RETURN.
    ELSEIF sy-subrc GT 1.
      o_cache->o_log->log_error( |Archive next object with handle { handle } { sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv4 }| ).
    ENDIF.

  ENDMETHOD.


  METHOD read_archive_records.

    DATA: lr_record    TYPE REF TO data,
          l_struc_name TYPE tabname.

    DATA: lr_table_content TYPE REF TO data.

    FIELD-SYMBOLS: <lta_table_content> TYPE ANY TABLE.
    FIELD-SYMBOLS: <lg_record> TYPE any.


    DO.

      CALL FUNCTION 'ARCHIVE_GET_NEXT_RECORD'
        EXPORTING
          archive_handle          = handle
        IMPORTING
          record_ref              = lr_record
          record_structure        = l_struc_name
        EXCEPTIONS
          end_of_object           = 1
          internal_error          = 2
          wrong_access_to_archive = 3
          OTHERS                  = 4.
      IF sy-subrc EQ 1.
        "end of object
        EXIT.
      ELSEIF sy-subrc GT 1.
        o_cache->o_log->log_error( |Archive next record with handle { handle } { sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv4 }| ).
      ENDIF.


      UNASSIGN <lg_record>.
      ASSIGN lr_record->* TO <lg_record>.
      o_cache->o_log->new_object( zcl_string=>create_from_structure( <lg_record> )->get_value( ) ).

      lr_table_content = o_cache->get_table_content_by_tabname( l_struc_name ).
      ASSIGN lr_table_content->* TO <lta_table_content>.

      IF <lg_record> IS ASSIGNED.

        INSERT <lg_record> INTO TABLE <lta_table_content>.
        IF sy-subrc NE 0.
          o_cache->o_log->log_warning( |Insert into internal tables failed| ).
        ENDIF.

      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD read_objects_needed_for_commit.

    CALL FUNCTION 'ARCHIVE_GET_CUSTOMIZING_DATA'
      EXPORTING
        object                      = o_cache->archive_object
      IMPORTING
        commit_count_for_delete_prg = objects_needed_for_commit
      EXCEPTIONS
        object_not_found            = 1
        OTHERS                      = 2.
    IF sy-subrc NE 0.
      o_cache->o_log->log_error( |Archive customizing not found| ).
      RAISE EXCEPTION TYPE cx_icl_customizing_no_entry.
    ENDIF.

  ENDMETHOD.


  METHOD register_classes.

    LOOP AT o_cache->t_archive_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
      CALL FUNCTION 'ARCHIVE_REGISTER_CLASS'
        EXPORTING
          archive_class           = <ls_class>-arch_class   " Name der Archivierungsklasse
          archive_handle          = handle    " Handle auf die geöffneten Archivdateien
        EXCEPTIONS
          class_not_found         = 1
          internal_error          = 2
          wrong_access_to_archive = 3
          file_io_error           = 4
          OTHERS                  = 5.
      IF sy-subrc <> 0.
        o_cache->o_log->log_warning( |Archive class not registered { sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv4 }| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD reload.

    WHILE read_archive_object( ).

      ADD 1 TO object_counter.

      read_archive_records( ).
      archive_classes_reload_data( ).

      IF object_counter GE objects_needed_for_commit.

        commit_db_insert( ).

      ENDIF.

    ENDWHILE.

    "for last package of records
    IF object_counter LT objects_needed_for_commit
    AND object_counter NE 0.

      commit_db_insert( ).

    ENDIF.

  ENDMETHOD.


  METHOD save_object.
    CALL FUNCTION 'ARCHIVE_SAVE_OBJECT'
      EXPORTING
        archive_handle          = handle    " Handle to the Open Archive File
      EXCEPTIONS
        file_io_error           = 1
        internal_error          = 2
        open_error              = 3
        termination_requested   = 4
        wrong_access_to_archive = 5
        data_object_not_saved   = 6
        OTHERS                  = 7.
    IF sy-subrc <> 0.
      o_cache->o_log->log_error( |Archive save object { sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv4 }| ).
    ENDIF.
  ENDMETHOD.


  METHOD write.

    FIELD-SYMBOLS: <lt_content> TYPE ANY TABLE.


    new_object( ).

    LOOP AT o_cache->t_internal_tables REFERENCE INTO DATA(lr_table).

      UNASSIGN <lt_content>.
      ASSIGN lr_table->content_ref->* TO <lt_content>.

      LOOP AT <lt_content> ASSIGNING FIELD-SYMBOL(<lg_content_row>). "#EC CI_NESTED

        put_record( ig_record = <lg_content_row>
                    i_tabname = lr_table->tabname ).

      ENDLOOP.

    ENDLOOP.

    archive_classes( ig_key ).

    save_object( ).

  ENDMETHOD.


  METHOD write_statistics.

    CALL FUNCTION 'ARCHIVE_WRITE_STATISTICS'
      EXPORTING
        archive_handle          = me->handle    " Handle to the Open Archive Files
      EXCEPTIONS
        internal_error          = 1
        wrong_access_to_archive = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      o_cache->o_log->log_warning( |Archive write statistics { sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv4 }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
