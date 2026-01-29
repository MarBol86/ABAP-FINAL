CLASS zcm_incidents_mb DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_abap_behv_message.
    INTERFACES: if_t100_dyn_msg,
      if_t100_message.

    CONSTANTS gc_msgid TYPE symsgid VALUE 'ZMC_INCIDENTS_MB'.

    CONSTANTS: BEGIN OF enter_title,
                 mysid TYPE symsgid VALUE gc_msgid,
                 msgno TYPE symsgno VALUE '001',
                 attr1 TYPE scx_attrname VALUE '', "Para las &1
                 attr2 TYPE scx_attrname VALUE '',
                 attr3 TYPE scx_attrname VALUE '',
                 attr4 TYPE scx_attrname VALUE '',
               END OF enter_title.

    CONSTANTS: BEGIN OF enter_description,
                 mysid TYPE symsgid VALUE gc_msgid,
                 msgno TYPE symsgno VALUE '002',
                 attr1 TYPE scx_attrname VALUE '',
                 attr2 TYPE scx_attrname VALUE '',
                 attr3 TYPE scx_attrname VALUE '',
                 attr4 TYPE scx_attrname VALUE '',
               END OF enter_description.

    CONSTANTS: BEGIN OF enter_priority,
                 mysid TYPE symsgid VALUE gc_msgid,
                 msgno TYPE symsgno VALUE '003',
                 attr1 TYPE scx_attrname VALUE '',
                 attr2 TYPE scx_attrname VALUE '',
                 attr3 TYPE scx_attrname VALUE '',
                 attr4 TYPE scx_attrname VALUE '',
               END OF enter_priority.

    CONSTANTS: BEGIN OF invalid_priority,
                 mysid TYPE symsgid VALUE gc_msgid,
                 msgno TYPE symsgno VALUE '004',
                 attr1 TYPE scx_attrname VALUE '',
                 attr2 TYPE scx_attrname VALUE '',
                 attr3 TYPE scx_attrname VALUE '',
                 attr4 TYPE scx_attrname VALUE '',
               END OF invalid_priority.

    METHODS constructor
      IMPORTING
        textid   TYPE scx_t100key OPTIONAL
        attr1    TYPE string OPTIONAL
        attr2    TYPE string OPTIONAL
        attr3    TYPE string OPTIONAL
        attr4    TYPE string OPTIONAL
        previous LIKE previous OPTIONAL
        severity TYPE if_abap_behv_message~t_severity OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mv_attr1 TYPE string,
          mv_attr2 TYPE string,
          mv_attr3 TYPE string,
          mv_attr4 TYPE string.

ENDCLASS.



CLASS zcm_incidents_mb IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    me->mv_attr1 = attr1.
    me->mv_attr2 = attr2.
    me->mv_attr3 = attr3.
    me->mv_attr4 = attr4.

    if_abap_behv_message~m_severity = severity.

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
