FUNCTION zparallel.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJ_TYPE) TYPE  TADIR-OBJECT
*"     VALUE(OBJ_NAME) TYPE  TADIR-OBJ_NAME
*"     VALUE(DEVCLASS) TYPE  TADIR-DEVCLASS
*"  TABLES
*"      FILES TYPE  CFT_RAWLINE
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  TRY.
      DATA(ls_item) = VALUE zif_abapgit_definitions=>ty_item(
        obj_type = obj_type
        obj_name = obj_name
        devclass = devclass ).

      DATA(lt_files) = zcl_abapgit_objects=>serialize(
        is_item     = ls_item
        iv_language = sy-langu ).

* this is not correct, but will likely give similar performance
* to the right implementation
      LOOP AT lt_files INTO DATA(ls_file).
        DATA(lv_len) = xstrlen( ls_file-data ).
        DATA(lv_off) = 0.
        DO.
          IF lv_len >= 128.
            lv_len = lv_len - 128.
            APPEND VALUE rspolpbi( data = ls_file-data+lv_off(128) ) TO files.
          ELSE.
            APPEND VALUE rspolpbi( data = ls_file-data+lv_off(lv_len) ) TO files.
            lv_len = 0.
          ENDIF.
          IF lv_len = 0.
            EXIT.
          ENDIF.
        ENDDO.
      ENDLOOP.

    CATCH zcx_abapgit_exception.
      RAISE error.
  ENDTRY.

ENDFUNCTION.
