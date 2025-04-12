REPORT zcreate_data_element_from_json.

TYPES: BEGIN OF ty_json_data_element,
         data_element TYPE string,
         domain_name  TYPE string,
         short_text   TYPE string,
         field_label  TYPE string,
       END OF ty_json_data_element.

DATA: lt_json_lines TYPE STANDARD TABLE OF string,
      ls_data_elem  TYPE ty_json_data_element.

PARAMETERS: p_file TYPE string LOWER CASE.

DATA: lv_json_string TYPE string,
      lv_xstring     TYPE xstring,
      lt_json        TYPE STANDARD TABLE OF string.

" Upload JSON File
CALL METHOD cl_gui_frontend_services=>gui_upload
  EXPORTING
    filename                = p_file
    filetype                = 'ASC'
  IMPORTING
    filelength              = DATA(lv_length)
  CHANGING
    data_tab                = lt_json
  EXCEPTIONS
    OTHERS                  = 1.

IF sy-subrc <> 0.
  MESSAGE 'Error reading file' TYPE 'E'.
ENDIF.

LOOP AT lt_json INTO DATA(lv_line).
  CONCATENATE lv_json_string lv_line INTO lv_json_string SEPARATED BY space.
ENDLOOP.

" Optional: Clean up strange characters
CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
  EXPORTING
    intext  = lv_json_string
  IMPORTING
    outtext = lv_json_string.

" Deserialize JSON to structure
CALL METHOD /ui2/cl_json=>deserialize
  EXPORTING
    json = lv_json_string
  CHANGING
    data = ls_data_elem.

" Prepare structures for data element
DATA: ls_dd04v TYPE dd04v,
      lt_dd04t TYPE STANDARD TABLE OF dd04t,
      ls_dd04t TYPE dd04t.

ls_dd04v-rollname = ls_data_elem-data_element.
ls_dd04v-datatype = ''. " Leave empty since we're using a domain
ls_dd04v-domname  = ls_data_elem-domain_name.
ls_dd04v-as4user  = sy-uname.
ls_dd04v-as4date  = sy-datum.
ls_dd04v-as4time  = sy-uzeit.
ls_dd04v-ddlanguage = sy-langu.
ls_dd04v-applclass = 'SD'.


ls_dd04t-rollname = ls_data_elem-data_element.
ls_dd04t-ddlanguage = sy-langu.
ls_dd04t-ddtext = CONV #( ls_data_elem-short_text ).
ls_dd04t-scrtext_s = CONV #( ls_data_elem-field_label ).
ls_dd04t-scrtext_m = CONV #( ls_data_elem-field_label ).
ls_dd04t-scrtext_l = CONV #( ls_data_elem-field_label ).

APPEND ls_dd04t TO lt_dd04t.

" Create data element
CALL FUNCTION 'DD_DTEL_PUT'
  EXPORTING
    dd04v_wa  = ls_dd04v
    prid      = 0
  TABLES
    dd04t_tab = lt_dd04t
  EXCEPTIONS
    OTHERS    = 1.

IF sy-subrc = 0.
  " Activate data element
  CALL FUNCTION 'DD_DTEL_ACTIVATE'
    EXPORTING
      dtelname = ls_data_elem-data_element
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc = 0.
    MESSAGE 'Data element created and activated successfully!' TYPE 'S'.
  ELSE.
    MESSAGE 'Activation failed!' TYPE 'E'.
  ENDIF.
ELSE.
  MESSAGE 'Data element creation failed!' TYPE 'E'.
ENDIF.
