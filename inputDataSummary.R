source('dataImport.R')

library(ggplot2)
library(cowplot)
library(tidyr)
library(purrr)

source('plottingArguments.R')

nrow(kc_roads_SM) #89309
#nrow(st_address) #89458

kc_roads_SM %>%
  st_drop_geometry() %>%
  group_by(KC_FCC) %>%
  summarise(n=n(),
            AveRoadLength=mean(RoadMiles),
            TotalRoadMiles=sum(RoadMiles),
            AveRoadArea_Acres=mean(RoadArea_SQFT,na.rm=T)/43560,
            TotalRoadArea_Acres=sum(RoadArea_SQFT,na.rm=T)/43560,
            AveADT=mean(ADT_PSRC,na.rm=T),
            #  VehicleRoadMiles=sum(RoadMiles*ADT_PSRC,na.rm=T),
            AveMediumVehicles=mean(MediumVehicle,na.rm=T),
            #  HeavyMediumRoadMiles=sum(RoadMiles*MediumVehicle,na.rm=T),
            #HeavyVehicles=mean(FreightRank,na.rm=T),
            AveHeavyVehicles=mean(HeavyVehicle,na.rm=T),
            # HeavyVehicleRoadMiles=sum(RoadMiles*HeavyVehicle,na.rm=T),
            #   RoadMilesConvey=sum(RoadMiles*Convey,na.rm=T),
            SegmentsWithSWData=length(which(SW_DATA_Available)),
            PctConveyed=mean(PctConveyed,na.rm=T),
            SegmentsWithAggrSWData=length(which(Aggr_SW_DATA_Available)),
            Aggr_PctConveyed=mean(Aggr_PctConveyed,na.rm=T),
            PctOverwater=mean(RdSkirtPctOverwater,na.rm=T),
            RdSkirtPctImp=mean(RdSkirtPctImp,na.rm=T)) %>%
  write.csv('outputs/kc_wsdot_roads_summar.csv',row.names = F)

#length of each road segement
kc_roads_SM %>%
  density_plot_arguments_by_fcc(.,RoadMiles,'Road Miles',F)

plot_ADT_FCC<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(.,ADT_PSRC,'Average Daily Trafic',T)
ggsave('plots/adt_by_fcc.png',plot_ADT_FCC,scale=0.8,width=10,height=4.5)

plot_ADT_FCC_violin<-kc_roads_SM %>%
  violin_plot_arguments_by_fcc(ADT_PSRC,'Average Daily Traffic',LOG=T)
ggsave('plots/adt_by_fcc_violin.png',plot_ADT_FCC_violin,scale=0.8,width=10,height=4.5)


plot_dist_med_freight_count<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(MediumVehicle,'Medium Vehicle Count (per day)',T)
ggsave('plots/freight_med_by_fcc.png',plot_dist_med_freight_count,scale=0.8,width=10,height=4.5)

plot_dist_med_freight_violin<-kc_roads_SM %>%
  violin_plot_arguments_by_fcc(MediumVehicle,'Medium Vehicle Count (per day)',LOG=T)
ggsave('plots/freight_med_by_fcc_violin.png',plot_dist_med_freight_violin,scale=0.8,width=10,height=4.5)

plot_dist_freight_count<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(HeavyVehicle,'Heavy Vehicle Count (per day)',T)
ggsave('plots/freight_by_fcc.png',plot_dist_freight_count,scale=0.8,width=10,height=4.5)

plot_dist_heavy_freight_violin<-kc_roads_SM %>%
  violin_plot_arguments_by_fcc(HeavyVehicle,'Heavy Vehicle Count (per day)',LOG=T)
ggsave('plots/freight_heavy_by_fcc_violin.png',plot_dist_heavy_freight_violin,scale=0.8,width=10,height=4.5)


plot_Imp_FCC<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(RdSkirtPctImp,'Road Skirt Imperviousness (%)')
ggsave('plots/imperviousness_by__FCC.png',plot_Imp_FCC,scale=0.8,width=10,height=4.5)

plot_Imp_FCC_violin<-kc_roads_SM %>%
  violin_plot_arguments_by_fcc(RdSkirtPctImp,'Road Skirt Imperviousness (%)')
ggsave('plots/imperviousness_by_FCC_violin.png',plot_Imp_FCC_violin,scale=0.8,width=10,height=4.5)

plot_convey_FCC<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(PctConveyed,'Conveyance (%)')
ggsave('plots/conveyance_by_FCC.png',plot_convey_FCC,scale=0.8,width=10,height=4.5)

plot_convey_FCC_violin<-kc_roads_SM %>%
  violin_plot_arguments_by_fcc(PctConveyed,'Conveyance (%)')
ggsave('plots/conveyance_by_FCC_violin.png',plot_convey_FCC_violin,scale=0.8,width=10,height=4.5)

##Aggregated stormwater
plot_aggr_convey_FCC<-kc_roads_SM %>%
  density_plot_arguments_by_fcc(Aggr_PctConveyed,'Conveyance (%)')
ggsave('plots/conveyance_aggr_by_FCC.png',plot_aggr_convey_FCC,scale=0.8,width=10,height=4.5)

plot_aggr_convey_FCC_violin<-kc_roads_SM %>%
  violin_plot_arguments_by_fcc(Aggr_PctConveyed,'Conveyance (%)')
ggsave('plots/conveyance_aggr_by_FCC_violin.png',plot_aggr_convey_FCC_violin,scale=0.8,width=10,height=4.5)

plot_convey_imp<-kc_roads_SM %>%
  filter(SW_DATA_Available) %>%
  ggplot(aes(RdSkirtPctImp,fill=ConveyType))+
  geom_density(alpha=.5)+
  facet_wrap(~KC_FCC,scales = 'free_y')+
  theme_bw()+
  scale_fill_viridis_d()
ggsave('plots/imperviousness_by_conveyance_FCC.png',plot_convey_imp,scale=0.8,width=10,height=4.5)

plot_convey_imp_violin<-kc_roads_SM %>%
  filter(SW_DATA_Available) %>%
  mutate(ConveyType=if_else(ConveyType=='Swale','Ditched',ConveyType) %>%
           factor(levels=c('None','Ditched','Piped'))) %>%
  ggplot(aes(ConveyType,y=RdSkirtPctImp))+
  geom_jitter(alpha=0.4,height = 0,aes(fill=ConveyType),shape=21)+
  geom_violin(draw_quantiles = c(0.25, 0.75),linetype='dashed',col='black')+
  geom_violin(draw_quantiles = c(0.5),col='black',fill='transparent')+
  facet_wrap(~KC_FCC,scales = 'free_y')+
  theme_bw()+
  scale_fill_viridis_d('Type')+
  theme(legend.position = 'none')+
  xlab('Conveyance Type')+
  ylab('Skirt Imperviousness (%)')
ggsave('plots/imperviousness_by_conveyance_violin.png',plot_convey_imp_violin,scale=0.8,width=10,height=4.5)

plot_convey_type_impcat<-kc_roads_SM %>%
  mutate(ImpervCat=ifelse(is.na(RdSkirtPctImp)|RdSkirtPctImp<25,'<25%',
                          ifelse(RdSkirtPctImp<50,'>=25 to 50%','>=50%'))) %>%
  filter(ConveyType!='Stream'&SW_DATA_Available) %>%
  ggplot()+
  geom_bar(aes(x=ImpervCat,y=after_stat(count),fill=ConveyType,group=ConveyType))+
  facet_wrap(~KC_FCC,
             scales='free_y')+
  scale_x_discrete('Imperviousness Range')+
  scale_y_continuous('Count (# Segments)')+
  scale_fill_viridis_d('Conveyance Type')+
  theme_bw()
ggsave('plots/conveyance_by_type_FCC_impcat.png',plot_convey_type_impcat,scale=0.8,width=10,height=4.5)

# predict conveyance based on imperviousness and conveyance
conveyance_by_impervious_FCC<-kc_roads_SM %>%
  filter(SW_DATA_Available) %>%
  mutate(ConveyType=if_else(ConveyType=='Swale','Ditched',ConveyType) %>%
           factor(levels=c('None','Ditched','Piped'))) %>%
  #  filter(Juris %in% c('Seattle','King County')|(Juris=='WSDOT'&PctConveyed>0)) %>%
  ggplot(aes(x=RdSkirtPctImp,y=PctConveyed))+
  geom_point(aes(col=ConveyType))+
  facet_wrap(~KC_FCC)+
  geom_smooth(span=.9)+
  theme_bw()+
  scale_colour_viridis_d('Type')+
  theme(legend.position = 'bottom')+
  scale_x_continuous('Road Skirt Imperviousness (%)')+
  scale_y_continuous('Conveyance (%)')
ggsave('plots/conveyance_by_impervious_FCC.png',conveyance_by_impervious_FCC,scale=0.8,width=10,height=4.5)

conveyance_aggr_by_impervious_FCC<-kc_roads_SM %>%
  filter(Aggr_SW_DATA_Available) %>%
  #  filter(Juris %in% c('Seattle','King County')|(Juris=='WSDOT'&PctConveyed>0)) %>%
  ggplot(aes(x=RdSkirtPctImp,y=Aggr_PctConveyed))+
  geom_point()+
  facet_wrap(~KC_FCC)+
  geom_smooth(span=.9)+
  theme_bw()+
  scale_x_continuous('Road Skirt Imperviousness (%)')+
  scale_y_continuous('Conveyance (%)')
ggsave('plots/conveyance_aggr_by_impervious_FCC.png',conveyance_aggr_by_impervious_FCC,scale=0.8,width=10,height=4.5)

conveyance_aggr_by_impervious_FCC_KCSeattleVsOther<-kc_roads_SM %>%
  filter(Aggr_SW_DATA_Available) %>%
  #  filter(Juris %in% c('Seattle','King County')|(Juris=='WSDOT'&PctConveyed>0)) %>%
  ggplot(aes(x=RdSkirtPctImp,y=Aggr_PctConveyed,
             col=ifelse(SW_DATA_Available,'King County, Seattle, WSDOT','All Others Compiled')))+
  geom_point(alpha=.2)+
  facet_wrap(~KC_FCC)+
  geom_smooth(span=.9,se=F)+
  theme_bw()+
  ylab('Conveyance (%)')+
  scale_color_discrete('Data Source')