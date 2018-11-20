FUNCTION z_abapgit_serialize_parallel.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_OBJ_TYPE) TYPE  TADIR-OBJECT
*"     VALUE(IV_OBJ_NAME) TYPE  TADIR-OBJ_NAME
*"     VALUE(IV_DEVCLASS) TYPE  TADIR-DEVCLASS
*"     VALUE(IV_LANGUAGE) TYPE  SY-LANGU
*"     VALUE(IV_PATH) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_RESULT) TYPE  XSTRING
*"     VALUE(EV_PATH) TYPE  STRING
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  TRY.
* todo, downport
      DATA(ls_item) = VALUE zif_abapgit_definitions=>ty_item(
        obj_type = iv_obj_type
        obj_name = iv_obj_name
        devclass = iv_devclass ).

      DATA(lt_files) = zcl_abapgit_objects=>serialize(
        is_item     = ls_item
        iv_language = iv_language ).

      EXPORT data = lt_files TO DATA BUFFER ev_result.
      ev_path = iv_path.

    CATCH zcx_abapgit_exception.
* todo, better error handling
      RAISE error.
  ENDTRY.

ENDFUNCTION.
