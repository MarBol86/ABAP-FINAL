@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help for Priority'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #S,
    dataClass: #MASTER
}
@ObjectModel.representativeKey: 'priority_code'
@ObjectModel.dataCategory: #VALUE_HELP
@Search.searchable: true
@Consumption.ranked: true
define view entity ZDD_PRIORITY_MB_VH
  as select from zdt_priority_mb
{
      @EndUserText.label: 'Code'
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      @ObjectModel.text.element: ['priority_description']
  key priority_code,
      @EndUserText.label: 'Description'
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #LOW
      @Semantics.text: true
      priority_description
}
