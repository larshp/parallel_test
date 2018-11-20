REPORT zparallel_test.

PARAMETERS: max  TYPE i DEFAULT 4 OBLIGATORY,
            iter TYPE i DEFAULT 2 OBLIGATORY.

* quick and dirty

START-OF-SELECTION.
  PERFORM run.

CLASS lcl_test DEFINITION.

  PUBLIC SECTION.
    CLASS-DATA:
      free     TYPE i,
      gt_files TYPE cft_rawline.

    CLASS-METHODS:
      run
        RAISING
          zcx_abapgit_exception,
      on_task_complete
        IMPORTING p_task TYPE clike,
      parallel
        IMPORTING
          it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
        RAISING
          zcx_abapgit_exception,
      sequential
        IMPORTING
          it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
        RAISING
          zcx_abapgit_exception,
      find_tadir
        RETURNING
          VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
        RAISING
          zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_test IMPLEMENTATION.

  METHOD run.

    DATA(lt_tadir) = find_tadir( ).
    WRITE: / 'TADIR objects:', lines( lt_tadir ), /.

    DO iter TIMES.
      GET RUN TIME FIELD DATA(t1).
      CLEAR gt_files.
      parallel( lt_tadir ).
      GET RUN TIME FIELD DATA(t2).
      t1 = ( t2 - t1 ) / 1000000.
      WRITE: / 'Parallel:', t1, 'seconds'.

      GET RUN TIME FIELD t1.
      sequential( lt_tadir ).
      GET RUN TIME FIELD t2.
      t1 = ( t2 - t1 ) / 1000000.
      WRITE: / 'Sequential:', t1, 'seconds'.

      WRITE: /.
    ENDDO.

  ENDMETHOD.

  METHOD on_task_complete.

    DATA: lt_files TYPE cft_rawline.

    RECEIVE RESULTS FROM FUNCTION 'ZPARALLEL'
      TABLES
        files                 = lt_files
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2.

    APPEND LINES OF lt_files TO gt_files.

    free = free + 1.

  ENDMETHOD.

  METHOD parallel.

    DATA: lt_files TYPE cft_rawline,
          lv_task  TYPE c LENGTH 5.

    free = max.

    LOOP AT it_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>).
      lv_task = sy-tabix.

      CALL FUNCTION 'ZPARALLEL'
        STARTING NEW TASK lv_task
        CALLING on_task_complete ON END OF TASK
        EXPORTING
          obj_type = <ls_tadir>-object
          obj_name = <ls_tadir>-obj_name
          devclass = <ls_tadir>-devclass
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
        BREAK-POINT.
      ENDIF.

      free = free - 1.

      WAIT UNTIL free > 0.

    ENDLOOP.

    WAIT UNTIL free = max.

    WRITE: / 'file rows:', lines( gt_files ).

  ENDMETHOD.

  METHOD sequential.

    LOOP AT it_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>).

      DATA(ls_item) = VALUE zif_abapgit_definitions=>ty_item(
        obj_type = <ls_tadir>-object
        obj_name = <ls_tadir>-obj_name
        devclass = <ls_tadir>-devclass ).

      DATA(lt_files) = zcl_abapgit_objects=>serialize(
        is_item     = ls_item
        iv_language = sy-langu ).

    ENDLOOP.

  ENDMETHOD.

  METHOD find_tadir.
    DATA(lt_repos) = zcl_abapgit_persist_factory=>get_repo( )->list( ).

    LOOP AT lt_repos INTO DATA(ls_repo).
      DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get( ls_repo-key ).

      APPEND LINES OF zcl_abapgit_factory=>get_tadir( )->read(
        iv_package            = lo_repo->get_package( )
        iv_ignore_subpackages = lo_repo->get_local_settings( )-ignore_subpackages
        iv_only_local_objects = lo_repo->get_local_settings( )-only_local_objects
        io_dot                = lo_repo->get_dot_abapgit( ) ) TO rt_tadir.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

FORM run.

  TRY.
      lcl_test=>run( ).
    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      WRITE: / 'Error:', lx_error->get_text( ).
  ENDTRY.

ENDFORM.
