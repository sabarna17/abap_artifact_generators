REPORT zcreate_domain_from_json.

TYPES: BEGIN OF ty_fixed_value,
         value TYPE string,
         text  TYPE string,
       END OF ty_fixed_value,
       ty_fixed_values TYPE STANDARD TABLE OF ty_fixed_value WITH EMPTY KEY.

TYPES: BEGIN OF ty_json_domain,
         domain_name  TYPE string,
         data_type    TYPE string,
         length       TYPE string,
         description  TYPE string,
         fixed_values TYPE ty_fixed_values,
       END OF ty_json_domain.

DATA: lt_json_lines TYPE STANDARD TABLE OF string,
      ls_domain     TYPE ty_json_domain.

PARAMETERS: p_file TYPE string LOWER CASE.  " User input for file path

DATA: lv_json_string TYPE string,
      lv_xstring     TYPE xstring,
      lt_json        TYPE STANDARD TABLE OF string.
" Read file content
CALL METHOD cl_gui_frontend_services=>gui_upload
  EXPORTING
    filename                = p_file
    filetype                = 'ASC'
  IMPORTING
    filelength              = DATA(lv_length)
  CHANGING
    data_tab                = lt_json
  EXCEPTIONS
    file_open_error         = 1
    file_read_error         = 2
    no_batch                = 3
    gui_refuse_filetransfer = 4
    invalid_type            = 5
    no_authority            = 6
    unknown_error           = 7
    bad_data_format         = 8
    header_not_allowed      = 9
    separator_not_allowed   = 10
    header_too_long         = 11.

IF sy-subrc <> 0.
  MESSAGE 'Error reading the file. Please check the path.' TYPE 'E'.
ENDIF.

" Combine lines into single JSON string
LOOP AT lt_json INTO DATA(lv_line).
  CONCATENATE lv_json_string lv_line INTO lv_json_string SEPARATED BY space.
ENDLOOP.

FORM f4_get_file_path.
  DATA: lt_file_table TYPE filetable,
        lv_rc         TYPE i,
        lv_file       TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select JSON File'
      default_extension       = 'json'
      file_filter             = 'JSON Files (*.json)|*.json|All Files (*.*)|*.*'
      initial_directory       = 'C:\'
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc = 0 AND lv_rc > 0.
    READ TABLE lt_file_table INTO lv_file INDEX 1.
    IF sy-subrc = 0.
      p_file = lv_file.
    ENDIF.
  ENDIF.
ENDFORM.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_get_file_path.

  IF sy-subrc <> 0.
    MESSAGE 'Error reading JSON file' TYPE 'E'.
  ENDIF.

  LOOP AT lt_json_lines INTO DATA(line).
    CONCATENATE lv_json_string line INTO lv_json_string.
  ENDLOOP.

  " Parse JSON string to ABAP structure
  CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
    EXPORTING
      intext  = lv_json_string
    IMPORTING
      outtext = lv_json_string.

  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = lv_json_string
    CHANGING
      data = ls_domain.

  DATA: ls_dd01l TYPE dd01l,
        l_dd01v  TYPE dd01v,
        l_dd07v  TYPE TABLE OF dd07v,
        ls_dd01t TYPE dd01t,
        lt_dd07l TYPE STANDARD TABLE OF dd07l,
        lt_dd07t TYPE STANDARD TABLE OF dd07t.

  " Fill domain definition (technical)
  l_dd01v-domname  = ls_domain-domain_name.
  l_dd01v-datatype = ls_domain-data_type.
  l_dd01v-leng     = ls_domain-length.
  l_dd01v-as4user  = sy-uname.
  l_dd01v-as4date  = sy-datum.
  l_dd01v-as4time  = sy-uzeit.

  " Fill fixed values
  LOOP AT ls_domain-fixed_values INTO DATA(lv_val).
    " Value line
    APPEND VALUE dd07v(
      domname    = ls_domain-domain_name
      ddtext = ls_domain-description
      valpos     = sy-tabix
      domvalue_l = lv_val-value
      domvalue_h = lv_val-value
    ) TO lt_dd07l.

  ENDLOOP.
  DATA: lv_domname LIKE dd01l-domname. " LIKE  DD01L-DOMNAME
  lv_domname = ls_domain-domain_name.
  BREAK-POINT.
  CALL FUNCTION 'DD_DOMA_PUT'
    EXPORTING
      dd01v_wa                   = l_dd01v
      domain_name                = lv_domname
      prid                       = 0
      ctrl_doma_put              = 'N'
      save_abap_language_version = ' '
    TABLES
      dd07v_tab                  = l_dd07v
    EXCEPTIONS
      illegal_value              = 1
      op_failure                 = 2
      object_inconsistent        = 3
      OTHERS                     = 4.
  IF sy-subrc = 0.
    CALL FUNCTION 'DD_DOMA_ACTIVATE'
      EXPORTING
        doma_name     = lv_domname
      EXCEPTIONS
        illegal_value = 1
        act_failure   = 2
        act_refused   = 3
        OTHERS        = 4.

    IF sy-subrc = 0.
      MESSAGE 'Domain created and activated successfully!' TYPE 'S'.
    ELSE.
      MESSAGE 'Domain activation failed!' TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE 'Domain creation failed!' TYPE 'E'.
  ENDIF.
