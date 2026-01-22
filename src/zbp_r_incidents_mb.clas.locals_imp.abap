CLASS lhc_Incidents DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF mcs_status,
        open        TYPE c LENGTH 2 VALUE 'OP',
        In_Progress TYPE c LENGTH 2 VALUE 'IP',
        Pending     TYPE c LENGTH 2 VALUE 'PE',
        Completed   TYPE c LENGTH 2 VALUE 'CO',
        Closed      TYPE c LENGTH 2 VALUE 'CL',
        Canceled    TYPE c LENGTH 2 VALUE 'CN',
      END OF mcs_status,
      BEGIN OF mcs_priority,
        high   TYPE c LENGTH 1 VALUE 'H',
        Medium TYPE c LENGTH 1 VALUE 'M',
        Low    TYPE c LENGTH 1 VALUE 'L',
      END OF mcs_priority.


    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Incidents RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Incidents RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Incidents RESULT result.

    METHODS changeStatus FOR MODIFY
      IMPORTING keys FOR ACTION Incidents~changeStatus RESULT result.

    METHODS setvalues FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Incidents~setvalues.

    METHODS createHistory FOR DETERMINE ON SAVE
      IMPORTING keys FOR Incidents~createHistory.

ENDCLASS.

CLASS lhc_Incidents IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD changeStatus.
  ENDMETHOD.

  METHOD setvalues.

    "Read Entity
    READ ENTITIES OF z_r_incidents_mb IN LOCAL MODE
    ENTITY Incidents
    FIELDS ( IncidentId CreationDate Status )
    WITH CORRESPONDING #( keys )
    RESULT DATA(incidents).

    DELETE incidents WHERE IncidentId IS NOT INITIAL.

    CHECK incidents IS NOT INITIAL.

    " Get Incidet ID
    SELECT SINGLE FROM zdt_inct_mb
    FIELDS MAX( Incident_Id )
    INTO @DATA(lv_max_incident_id).



  ENDMETHOD.

  METHOD createHistory.

  ENDMETHOD.

ENDCLASS.
