library(dplyr)
library(sf)




# library(leaflet)
# road_polys %>%
#   filter(Jurisdiction=='King County') %>%
#    slice(1:1000) %>%
#   st_transform(4326) %>%
#   leaflet() %>%
#   addProviderTiles('Esri.WorldImagery') %>%
#   addPolygons()

road_polys %>%
 # slice(1:10) %>%
  as_data_frame() %>%
  select(RoadSegmentID,Shape_Area) %>%
  left_join(kc_roads_score%>% as_data_frame() %>% select(RoadSegmentID,KC_FCC,Juris,Imputed_SW_Score)) %>%
  filter(Juris=='King County') %>%
  mutate(ScoreRange=cut(Imputed_SW_Score,breaks=c(0,1,2,3,4,5.01),right=F)) %>%
  group_by(ScoreRange) %>%
  summarise(RoadSurfaceAcres=sum(Shape_Area)/43560)



file.remove('outputs/GIS/road_polygon_scored.geojson')
road_polys %>%
  select(RoadSegmentID,Shape_Area) %>%
  left_join(kc_roads_score%>% as_data_frame() %>% select(RoadSegmentID,KC_FCC,Juris,Score=Imputed_SW_Score))  %>%
  st_write(dsn='outputs/GIS/road_polygon_scored.geojson')
