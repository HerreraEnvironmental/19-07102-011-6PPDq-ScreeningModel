
library(sf)
library(dplyr)

kc_roads_summary_attrib<-read_excel('inputs/KC_Street_Address_SummaryAttributes_20240223_v2.xlsx') 

WQBE_6PPDQ_20231205_layers<-st_layers("K:\\Projects\\Y2019\\19-07102-011\\Geodatabase\\GIS_Working\\WQBE_6-PPDQ_20231205.gdb")

WQBE6ppdq_20240201_layers<-st_layers("K:\\Projects\\Y2019\\19-07102-011\\Geodatabase\\GIS_Working\\WQBE6ppdq_20240201.gdb")

RoadBuffer_20240221<-st_read(dsn="K:\\Projects\\Y2019\\19-07102-011\\Geodatabase\\GIS_Working\\WQBE6ppdq_20240201.gdb",
                             layer='King_County_St_Addresses_6PPDQ_Metrics_RoadBuffer_20240221')


WSDOT_StormwaterConvey_DitchSubset<-st_read(dsn="K:\\Projects\\Y2019\\19-07102-011\\Geodatabase\\GIS_Working\\WQBE6ppdq_20240201.gdb",
                             layer='WSDOT_StormwaterConvey_DitchSubset')

Stormwater_Open_Conveyance_Projected_DNLD240123<-
  st_read("K:\\Projects\\Y2019\\19-07102-011\\Geodatabase\\GIS_Working\\WQBE_6-PPDQ_20231205.gdb",
          'Stormwater_Open_Conveyance_Projected_DNLD240123')

ditches<-bind_rows(
  WSDOT_StormwaterConvey_DitchSubset %>% transmute(Owner='WSDOT',ID=WSDOTFeatureNumber),
  Stormwater_Open_Conveyance_Projected_DNLD240123  %>% filter(Jurisdicti =='King County'&CurrentSta=='Active')%>% 
    transmute(Owner='King County',ID=as.character(AssetID))
) %>%
  st_zm()

ditch_estimates<-RoadBuffer_20240221 %>% filter(RoadSegmentID %in% kc_roads_summary_attrib$RoadSegmentID) %>%
  select(RoadSegmentID) %>%
  st_intersection(ditches) %>%
  mutate(DitchLength=st_length(.)) %>%
  st_drop_geometry() %>%
  group_by(RoadSegmentID) %>%
  summarise(DitchLength=sum(DitchLength))

