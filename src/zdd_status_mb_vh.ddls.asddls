@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help for Status'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #S,
    dataClass: #MASTER
}
@ObjectModel.representativeKey: 'status_code'
@ObjectModel.dataCategory: #VALUE_HELP
@Search.searchable: true
@Consumption.ranked: true

define view entity ZDD_STATUS_MB_VH
  as select from zdt_status_mb
{
      @EndUserText.label: 'Code'
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      @ObjectModel.text.element: ['status_description']
  key status_code,
      @EndUserText.label: 'Description'
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #LOW
      @Semantics.text: true
      status_description
}
