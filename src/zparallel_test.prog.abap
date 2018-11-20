REPORT zparallel_test.

PARAMETERS: iter TYPE i DEFAULT 1 OBLIGATORY.

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
      DATA(par) = NEW zcl_abapgit_serialize( )->serialize(
        it_tadir = lt_tadir
        iv_force_sequential = abap_false ).
      GET RUN TIME FIELD DATA(t2).
      t1 = ( t2 - t1 ) / 1000000.
      WRITE: / 'Parallel:', t1, 'seconds'.
      WRITE: / lines( par ).

      GET RUN TIME FIELD t1.
      DATA(seq) = NEW zcl_abapgit_serialize( )->serialize(
        it_tadir = lt_tadir
        iv_force_sequential = abap_true ).
      GET RUN TIME FIELD t2.
      t1 = ( t2 - t1 ) / 1000000.
      WRITE: / 'Sequential:', t1, 'seconds'.
      WRITE: / lines( seq ).

      WRITE: /.
    ENDDO.

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
