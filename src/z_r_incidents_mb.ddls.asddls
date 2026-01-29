@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Root Entity'
@Metadata.ignorePropagatedAnnotations: true
define root view entity Z_R_INCIDENTS_MB
  as select from zdt_inct_mb
  composition [0..*] of Z_R_HISTORY_MB     as _History
  association [1..1] to ZDD_STATUS_MB_VH   as _Status   on _Status.status_code = $projection.Status
  association [1..1] to ZDD_PRIORITY_MB_VH as _Priority on _Priority.priority_code = $projection.Priority
{
  key inc_uuid              as IncUUID,
      //Clave semántica
      incident_id           as IncidentId,
      title                 as Title,
      description           as Description,
      @ObjectModel.foreignKey.association: '_Status'
      status                as Status,
      @ObjectModel.foreignKey.association: '_Priority'
      priority              as Priority,
      creation_date         as CreationDate,
      changed_date          as ChangedDate,
      //Campos de auditoría
      @Semantics.user.createdBy: true
      local_created_by      as LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      local_created_at      as LocalCreatedAt,
      @Semantics.user.lastChangedBy: true
      local_last_changed_by as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,

      //    _association_name // Make association public
      _Status,
      _Priority,
      _History
}
