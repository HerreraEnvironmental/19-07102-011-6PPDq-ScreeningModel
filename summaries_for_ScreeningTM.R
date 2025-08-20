#plotting for site screening memo

source('plottingArguments.R')
library(dplyr)
library(ggplot2)
library(sf)

hec_gdb_work<-'\\\\herrera.local\\hecnet\\gis-k\\Projects\\Y2019\\19-07102-011\\Geodatabase\\GIS_Working\\WQBE6ppdq_20240717.gdb'

road_scores_info<-st_read('outputs/GIS/kc_roads_scored.geojson') %>% st_drop_geometry() %>%
  mutate(KC_FCC=factor(KC_FCC,levels=c('Local','Collector','Minor','Primary','Freeway')))

skirts<-st_read(dsn=hec_gdb_work,layer='King_County_St_Addresses_6PPDQ_Metrics_RoadSkirt_20240717') %>%
  select(RoadSegmentID )

new_station_drains<-st_read('K:\\Projects\\Y2023\\23-08252-000\\Geodatabase\\GIS_Working\\BasinDelin.gdb',
                            'DrainBasins_SouthSites_20250819_Poly_V3') %>%
  transmute(KC_AssetID=c('SNS-8272','SNS-11611')) %>%
  rename(geometry=Shape) %>%
  st_transform(2925)

station_drains<-st_read('K:\\Projects\\Y2023\\23-08252-000\\Shape\\Other_Working\\DrainBasins_Merged_Modified_20250325.shp') %>%
  select(KC_AssetID) %>%
  filter(KC_AssetID!='NA_WKC_MLKJR'&KC_AssetID!='SNS-29085') %>%
  st_transform(2925) %>%
  bind_rows(new_station_drains) 


road_polygon_scored<-st_read('outputs/GIS/road_polygon_scored.geojson')

drain_road_poly_intersection<-station_drains %>%
  st_transform(2926 )%>% 
  mutate(BasinArea_SQFT=as.numeric(st_area(.))) %>%
  select(KC_AssetID,BasinArea_SQFT) %>%
  st_intersection(road_polygon_scored,.) %>%
  mutate(RoadArea_SQFT=as.numeric(st_area(.)))

drain_skirt_poly_intersection<-station_drains %>%
  st_transform(2926 )%>% 
  select(KC_AssetID) %>%
  st_intersection(skirts,.) %>%
  mutate(SkirtArea_SQFT=as.numeric(st_area(.))) %>%
  left_join(drain_road_poly_intersection %>%st_drop_geometry() %>% select(RoadSegmentID,Score)) %>%
  st_intersection() %>%
  group_by(KC_AssetID) %>%
  filter(n.overlaps==1|(Score==which.max(Score))) %>%
  select(RoadSegmentID:SkirtArea_SQFT)


drain_road_poly_intersection %>%
  left_join(drain_skirt_poly_intersection %>% st_drop_geometry()) %>%
  mutate(SkirtArea_SQFT=ifelse(is.na(SkirtArea_SQFT),0,SkirtArea_SQFT)) %>%
  group_by(KC_AssetID,BasinArea_SQFT,RoadSegmentID) %>%
  summarise(RoadArea_SQFT=sum(RoadArea_SQFT),
            SkirtArea_SQFT=sum(SkirtArea_SQFT),
            RoadSkirtArea_SQFT=RoadArea_SQFT+SkirtArea_SQFT,
            Score=mean(Score))

aw_score<-drain_road_poly_intersection %>%
  left_join(drain_skirt_poly_intersection %>% st_drop_geometry()) %>%
  #bring in roads info to calculate score assuming convey is 100%
  left_join(road_scores_info %>% 
              st_drop_geometry() %>%
              mutate(Score_Convey100=GenScore+ImpScore) %>%
              select(RoadSegmentID,Score_Convey100)) %>%
  mutate(SkirtArea_SQFT=ifelse(is.na(SkirtArea_SQFT),0,SkirtArea_SQFT)) %>%
  group_by(KC_AssetID,BasinArea_SQFT,RoadSegmentID) %>%
  summarise(RoadArea_SQFT=sum(RoadArea_SQFT),
            SkirtArea_SQFT=sum(SkirtArea_SQFT),
            RoadSkirtArea_SQFT=RoadArea_SQFT+SkirtArea_SQFT,
            Score=mean(Score),
            Score_Convey100=mean(Score_Convey100)) %>%
  group_by(KC_AssetID,BasinArea_SQFT) %>%
  summarise(Score=log10(sum(10^Score*RoadSkirtArea_SQFT)/mean(BasinArea_SQFT)),
            Score_Convey100=log10(sum(10^Score_Convey100*RoadSkirtArea_SQFT)/mean(BasinArea_SQFT)))


selected_basins<-drain_road_poly_intersection %>%
  st_drop_geometry() %>%
  select(RoadSegmentID,KC_AssetID,RoadArea_SQFT) %>%
  left_join(road_scores_info %>% select(RoadSegmentID,KC_FCC,PV_LT_Count,MediumVehicleCount,HeavyVehicleCount,
                                        RoadwaySkirtImperviousness,PctConveyed_PREDICTED) %>% st_drop_geometry(),
            by=c('RoadSegmentID'='RoadSegmentID')) %>%
  group_by(KC_AssetID) %>%
  summarise(across(PV_LT_Count:PctConveyed_PREDICTED, function(x) weighted.mean(x,RoadArea_SQFT )),
            KC_FCC=KC_FCC[which.max(RoadArea_SQFT)]) %>%
  left_join(aw_score %>% select(KC_AssetID,Score))

plot_siteSelection_Score<-road_scores_info %>%
  ggplot(aes(y=Imputed_SW_Score,x=1))+
  geom_jitter(alpha=0.4,height = 0,shape=21)+
  geom_violin(draw_quantiles = c(0.25, 0.75),linetype='dashed',col='black')+
  geom_violin(draw_quantiles = c(0.5),col='black',fill='transparent')+
  theme_bw()+
  geom_jitter(data=selected_basins,aes(y=Score,x=1),
             col='red',size=3,height = 0,width=.05)+
  xlab('All Road Segments')+
  ylab('Model Score')+
  coord_flip()+
  theme(legend.position = 'none',axis.text.y = element_blank(),axis.ticks.y = element_blank())


plot_siteSelection_PassengerVeh<-road_scores_info %>%
  ggplot(aes(y=PV_LT_Count,x=1))+
  geom_jitter(alpha=0.4,height = 0,shape=21)+
  geom_violin(draw_quantiles = c(0.25, 0.75),linetype='dashed',col='black')+
  geom_violin(draw_quantiles = c(0.5),col='black',fill='transparent')+
  theme_bw()+
  geom_jitter(data=selected_basins,aes(y=Basin_SumPassengerVeh,x=1),
              col='red',size=3,height = 0,width=.05)+
  xlab('All Road Segments')+
  scale_y_log10('# Average Passenger Vehicles per Day',
                         limits=c(1,NA),
                         breaks=10^(0:5),
                         minor_breaks=c(.1*1:10,1:10,10*1:10,100*1:10,1000*1:10,10^4*1:10,10^5*1:10),
                         labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  coord_flip()+
  theme(legend.position = 'none',axis.text.y = element_blank(),axis.ticks.y = element_blank())

plot_siteSelection_Truck<-road_scores_info %>%
  ggplot(aes(y=HeavyVehicleCount+MediumVehicleCount,x=1))+
  geom_jitter(alpha=0.4,height = 0,shape=21)+
  geom_violin(draw_quantiles = c(0.25, 0.75),linetype='dashed',col='black')+
  geom_violin(draw_quantiles = c(0.5),col='black',fill='transparent')+
  theme_bw()+
  geom_jitter(data=selected_basins,aes(y=Basin_SumHTruck+Basin_SumMTruck,x=1),
              col='red',size=3,height = 0,width=.05)+
  xlab('All Road Segments')+
  scale_y_log10('# Average Trucks per Day',
                limits=c(1,NA),
                breaks=10^(0:5),
                minor_breaks=c(.1*1:10,1:10,10*1:10,100*1:10,1000*1:10,10^4*1:10,10^5*1:10),
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  coord_flip()+
  theme(legend.position = 'none',axis.text.y = element_blank(),axis.ticks.y = element_blank())

plot_siteSelection_Skirt<-road_scores_info %>%
  ggplot(aes(y=RoadwaySkirtImperviousness ,x=1))+
  geom_jitter(alpha=0.4,height = 0,shape=21)+
  geom_violin(draw_quantiles = c(0.25, 0.75),linetype='dashed',col='black')+
  geom_violin(draw_quantiles = c(0.5),col='black',fill='transparent')+
  theme_bw()+
  geom_jitter(data=selected_basins,aes(y=Basin_PctImp_AreaWt,x=1),
              col='red',size=3,height = 0,width=.05)+
  xlab('All Road Segments')+
  ylab('Road Skirt Imperviousness (%)')+
  coord_flip()+
  theme(legend.position = 'none',axis.text.y = element_blank(),axis.ticks.y = element_blank())

plot_siteSelection_Convey<-road_scores_info %>%
  ggplot(aes(y=PctConveyed_PREDICTED ,x=1))+
  geom_jitter(alpha=0.4,height = 0,shape=21)+
  geom_violin(draw_quantiles = c(0.25, 0.75),linetype='dashed',col='black')+
  geom_violin(draw_quantiles = c(0.5),col='black',fill='transparent')+
  theme_bw()+
  theme(legend.position = 'none',axis.text.y = element_blank(),axis.ticks.y = element_blank())+
  geom_jitter(data=selected_basins,aes(y=Basin_PctConvey_AreaWt,x=1),
              col='red',size=3,height = 0,width=.05)+
  xlab('All Road Segments')+
  ylab('Road Conveyance (%)')+
  coord_flip()

ggsave('siteSelection_plots/scores.png',plot=plot_siteSelection_Score,scale=0.8,width=10,height=4.5)
ggsave('siteSelection_plots/passVeh.png',plot=plot_siteSelection_PassengerVeh,scale=0.8,width=10,height=4.5)
ggsave('siteSelection_plots/trucks.png',plot=plot_siteSelection_Truck,scale=0.8,width=10,height=4.5)
ggsave('siteSelection_plots/convey.png',plot=plot_siteSelection_Convey,scale=0.8,width=10,height=4.5)
ggsave('siteSelection_plots/skirtImp.png',plot=plot_siteSelection_Skirt,scale=0.8,width=10,height=4.5)


plot_siteSelection_Score_fcc<-road_scores_info %>%
  violin_plot_arguments_by_fcc(Imputed_SW_Score,'Model Score')+
  geom_point(data=selected_basins,aes(y=Score),
             col='red',size=3)

plot_siteSelection_PassengerVeh_fcc<-road_scores_info %>%
  violin_plot_arguments_by_fcc(PV_LT_Count,'# Average Passenger Vehicles per Day',
                               LOG = T)+
  geom_point(data=selected_basins,aes(y=PV_LT_Count),
             col='red',size=3)

plot_siteSelection_trucks_fcc<-road_scores_info %>%
  mutate(TruckCount=MediumVehicleCount+HeavyVehicleCount) %>%
  violin_plot_arguments_by_fcc(TruckCount,'# Average Trucks per Day',
                               LOG = T)+
  geom_point(data=selected_basins,aes(y=MediumVehicleCount+HeavyVehicleCount),
             col='red',size=3) 

plot_siteSelection_convey_fcc<-road_scores_info %>%
  violin_plot_arguments_by_fcc(PctConveyed_PREDICTED,'Percent Conveyance')+
  geom_point(data=selected_basins,aes(y=PctConveyed_PREDICTED),
             col='red',size=3)

plot_siteSelection_rdskirtimp_fcc<-road_scores_info %>%
  violin_plot_arguments_by_fcc(RoadwaySkirtImperviousness,'Road Skirt Imperviousness (%)')+
  geom_point(data=selected_basins,aes(y=RoadwaySkirtImperviousness),
             col='red',size=3)

ggsave('siteSelection_plots/scores_fcc.png',plot=plot_siteSelection_Score_fcc,scale=0.8,width=10,height=4.5)
ggsave('siteSelection_plots/passVeh_fcc.png',plot=plot_siteSelection_PassengerVeh_fcc,scale=0.8,width=10,height=4.5)
ggsave('siteSelection_plots/trucks_fcc.png',plot=plot_siteSelection_trucks_fcc,scale=0.8,width=10,height=4.5)
ggsave('siteSelection_plots/convey_fcc.png',plot=plot_siteSelection_convey_fcc,scale=0.8,width=10,height=4.5)
ggsave('siteSelection_plots/skirtImp_fcc.png',plot=plot_siteSelection_rdskirtimp_fcc,scale=0.8,width=10,height=4.5)


selected_basins %>%
  ggplot(aes(PV_LT_Count,HeavyVehicleCount))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()
