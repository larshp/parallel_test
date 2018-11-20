CLASS ltcl_determine_max_threads DEFINITION DEFERRED.
CLASS zcl_abapgit_serialize DEFINITION LOCAL FRIENDS ltcl_determine_max_threads.

CLASS ltcl_determine_max_threads DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      determine_max_threads FOR TESTING RAISING zcx_abapgit_exception,
      force FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_determine_max_threads IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD determine_max_threads.

    DATA: lv_threads TYPE i.

    lv_threads = mo_cut->determine_max_threads( ).

    cl_abap_unit_assert=>assert_differs(
      act = lv_threads
      exp = 0 ).

  ENDMETHOD.

  METHOD force.

    DATA: lv_threads TYPE i.

    lv_threads = mo_cut->determine_max_threads( abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_threads
      exp = 1 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_serialize DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_serialize.

    METHODS:
      setup,
      test FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_serialize IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test.

    DATA: lt_tadir      TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_sequential TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_parallel   TYPE zif_abapgit_definitions=>ty_files_item_tt.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


    APPEND INITIAL LINE TO lt_tadir ASSIGNING <ls_tadir>.
    <ls_tadir>-object   = 'PROG'.
    <ls_tadir>-obj_name = 'RSABAPPROGRAM'.
    <ls_tadir>-path     = 'foobar'.

    lt_sequential = mo_cut->serialize(
      it_tadir            = lt_tadir
      iv_force_sequential = abap_true ).

    lt_parallel = mo_cut->serialize(
      it_tadir            = lt_tadir
      iv_force_sequential = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_sequential
      exp = lt_parallel ).

  ENDMETHOD.

ENDCLASS.
