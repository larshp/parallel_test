class ZCL_ABAPGIT_SERIALIZE definition
  public
  create public .

public section.

  methods ON_END_OF_TASK
    importing
      !P_TASK type CLIKE .
  methods SERIALIZE
    importing
      !IT_TADIR type ZIF_ABAPGIT_DEFINITIONS=>TY_TADIR_TT
      !IV_LANGUAGE type LANGU default SY-LANGU
      !IO_LOG type ref to ZCL_ABAPGIT_LOG optional
      !IV_FORCE_SEQUENTIAL type ABAP_BOOL default ABAP_FALSE
    returning
      value(RT_FILES) type ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_ITEM_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
protected section.

  data MT_FILES type ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_ITEM_TT .
  data MV_FREE type I .

  methods RUN_PARALLEL
    importing
      !IS_TADIR type ZIF_ABAPGIT_DEFINITIONS=>TY_TADIR
      !IV_LANGUAGE type LANGU
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods RUN_SEQUENTIAL
    importing
      !IS_TADIR type ZIF_ABAPGIT_DEFINITIONS=>TY_TADIR
      !IV_LANGUAGE type LANGU
      !IO_LOG type ref to ZCL_ABAPGIT_LOG
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DETERMINE_MAX_THREADS
    importing
      !IV_FORCE_SEQUENTIAL type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_THREADS) type I
    raising
      ZCX_ABAPGIT_EXCEPTION .
private section.
ENDCLASS.



CLASS ZCL_ABAPGIT_SERIALIZE IMPLEMENTATION.


  METHOD determine_max_threads.

    IF iv_force_sequential = abap_true.
      rv_threads = 1.
      RETURN.
    ENDIF.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = 'Z_ABAPGIT_SERIALIZE_PARALLEL'
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      rv_threads = 1.
      RETURN.
    ENDIF.

* todo, add possibility to set group name in user exit

    CALL FUNCTION 'SPBT_INITIALIZE'
      EXPORTING
        group_name                     = 'parallel_generators'
      IMPORTING
        free_pbt_wps                   = rv_threads
      EXCEPTIONS
        invalid_group_name             = 1
        internal_error                 = 2
        pbt_env_already_initialized    = 3
        currently_no_resources_avail   = 4
        no_pbt_resources_found         = 5
        cant_init_different_pbt_groups = 6
        OTHERS                         = 7.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from SPBT_INITIALIZE: { sy-subrc }| ).
    ENDIF.

    ASSERT rv_threads >= 1.

  ENDMETHOD.


  METHOD on_end_of_task.

    DATA: lt_files TYPE cft_rawline.

    RECEIVE RESULTS FROM FUNCTION 'Z_ABAPGIT_SERIALIZE_PARALLEL'
      TABLES
        files                 = lt_files
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2.

* todo
*     APPEND LINES OF lt_files TO gt_files.

    mv_free = mv_free + 1.

  ENDMETHOD.


  METHOD run_parallel.

    DATA: lv_task TYPE c LENGTH 44.


    CONCATENATE is_tadir-object is_tadir-obj_name INTO lv_task.

* todo, how to handle setting "<ls_file>-path = <ls_tadir>-path." ?
    CALL FUNCTION 'Z_ABAPGIT_SERIALIZE_PARALLEL'
      STARTING NEW TASK lv_task
      CALLING on_end_of_task ON END OF TASK
      EXPORTING
        obj_type              = is_tadir-object
        obj_name              = is_tadir-obj_name
        devclass              = is_tadir-devclass
        language              = iv_language
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

    mv_free = mv_free - 1.

    WAIT UNTIL mv_free > 0.

  ENDMETHOD.


  METHOD run_sequential.

    DATA: ls_fils_item TYPE zcl_abapgit_objects=>ty_serialization.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF ls_fils_item-files,
                   <ls_return> LIKE LINE OF mt_files.


    ls_fils_item-item-obj_type = is_tadir-object.
    ls_fils_item-item-obj_name = is_tadir-obj_name.
    ls_fils_item-item-devclass = is_tadir-devclass.

    ls_fils_item = zcl_abapgit_objects=>serialize(
      is_item     = ls_fils_item-item
      iv_language = iv_language
      io_log      = io_log ).

    LOOP AT ls_fils_item-files ASSIGNING <ls_file>.
      <ls_file>-path = is_tadir-path.

      APPEND INITIAL LINE TO mt_files ASSIGNING <ls_return>.
      <ls_return>-file = <ls_file>.
      <ls_return>-item = ls_fils_item-item.
    ENDLOOP.

  ENDMETHOD.


  METHOD serialize.

    DATA: lv_max       TYPE i,
          ls_fils_item TYPE zcl_abapgit_objects=>ty_serialization.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF ls_fils_item-files,
                   <ls_return> LIKE LINE OF rt_files,
                   <ls_tadir>  LIKE LINE OF it_tadir.


* todo, handle "unsupported object type" in log, https://github.com/larshp/abapGit/issues/2121

    CLEAR mt_files.

    lv_max = determine_max_threads( iv_force_sequential ).

    mv_free = lv_max.

    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      IF lv_max = 1.
        run_sequential(
          is_tadir    = <ls_tadir>
          iv_language = iv_language
          io_log      = io_log ).
      ELSE.
        run_parallel(
          is_tadir    = <ls_tadir>
          iv_language = iv_language ).
      ENDIF.
    ENDLOOP.

    WAIT UNTIL mv_free = lv_max.

    rt_files = mt_files.

  ENDMETHOD.
ENDCLASS.
