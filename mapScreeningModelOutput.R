#screening Model Leaflet Maps

source('ScreeningModel.R')
# plot up a leaflet map for exploration

pal_alt<-colorNumeric('RdYlBu',0:5,reverse=T)
kc_roads_score %>% 
  filter(!st_is_empty(.)&SW_DATA_Available) %>%
  leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addPolylines(color=~pal_alt(TotalScore),popup = ~paste(FULLNAME,RoadSegmentID,KC_FCC,
                                                         paste('Juris:',Juris),
                                                         #paste('Juris from Spatial Join:',JurisFinal),
                                                         paste('Total Score:',TotalScore),
                                                         paste('ADT:',round(TrafficIntensity)),
                                                         paste('Hvy Veh:',round(HeavyVehicleCount)),
                                                         paste('Med Veh:',round(MediumVehicleCount)),
                                                         paste('%Imp:',round(RoadwaySkirtImperviousness,0)),
                                                         paste('%Wtr:',round(RoadwaySkirtOverWater,0)),
                                                         paste('Drainage:',RoadwayDrainage),
                                                         paste('%Conveyed:',round(PctConveyed,0)),
                                                         paste('OverWater:',ifelse(StreamWaterCrossing==1,'Yes','No')),
                                                         # paste('Connectedness:',RoadwayConnectednessScore),
                                                         sep='<br>'),
               opacity = 1) %>%
  addLegend('bottomright',pal_alt,0:5,title='Screening Model\nScore')


#top 10000 map:
kc_roads_score %>% 
  arrange(desc(Imputed_SW_Score)) %>%
  slice(1:20000)%>%
  filter(!st_is_empty(.)) %>%
  leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addPolylines(color=~pal_alt(Imputed_SW_Score),popup = ~paste(FULLNAME,RoadSegmentID,KC_FCC,
                                                               paste('Juris:',Juris),
                                                               # paste('Juris from Spatial Join:',JurisFinal),
                                                               paste('Total Score:',Imputed_SW_Score),
                                                               paste('ADT:',round(TrafficIntensity)),
                                                               paste('Hvy Veh:',round(HeavyVehicleCount)),
                                                               paste('Med Veh:',round(MediumVehicleCount)),
                                                               paste('%Imp:',round(RoadwaySkirtImperviousness,0)),
                                                               paste('%Wtr:',round(RoadwaySkirtOverWater,0)),
                                                               paste('Drainage:',RoadwayDrainage),
                                                               paste('%Conveyed:',round(PctConveyed_PREDICTED,0)),
                                                               paste('OverWater:',ifelse(StreamWaterCrossing==1,'Yes','No')),
                                                               # paste('Connectedness:',RoadwayConnectednessScore),
                                                               sep='<br>'),
               opacity = 1) %>%
  addLegend('bottomright',pal_alt,0:5,title='Screening Model\nScore')