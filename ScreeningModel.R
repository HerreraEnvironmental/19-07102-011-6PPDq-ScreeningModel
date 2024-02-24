library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(cowplot)

# emme_links<-st_read('inputs/emme_links_with_hvy_med/emme_links.shp') 
# st_crs(emme_links)<-2926
# 
# # st_write(emme_links,'outputs/emme_links_proj.shp')
# # 
# # zip('outputs/emme_links_project.zip',list.files('outputs/','emme',full.names = T))
# 
st_address<-st_read(paste0("https://gisdata.kingcounty.gov/arcgis/rest/services/",
                           "OpenDataPortal/transportation__st_address_line/MapServer/108/query?outFields=*&where=1%3D1&f=geojson"))
# 
# jurisdictions<-st_read(paste0("https://gisdata.kingcounty.gov/arcgis/rest/",
#                           "services/OpenDataPortal/admin___base/MapServer/",
#                           "446/query?outFields=*&where=1%3D1&f=geojson"))
# st_address_juris<-st_address %>%
#   select(OBJECTID,FULLNAME) %>%
#   st_join(jurisdictions %>% select(CITYNAME)) %>%
#   mutate(Juris=ifelse(grepl('FWY|SR|RAMP',FULLNAME),'WSDOT',CITYNAME)) %>%
#   filter(!grepl('.',row.names(.),fixed = T))


#Issues, 
#several cases with Local roads having far too high traffic, e.g., Road ID 78194
#freeways sometime have local road numbers, e.g., ID 42074
#Please add an intersection for Jurisdiction
#Redmond Fall City Road has a verrrryyy low ADT & Fall City Snoqualmie is zero
#chunk of fall city road disappears north of fall city
#huge drop in SR18 ADT at Covington way from 28,000 to 1700 per day
# more traffic on SR18 then I5?
#I90 west of mercer is 0 ADT?

#Please provide the shapefile rather than an excel export
#missing vehicle weigth
#we should add somethign for bridges

kc_roads_SM<-read_excel('inputs/KC_Street_Address_SummaryAttributes_20240223_v2.xlsx') %>%
  transmute(OBJECTID,
            FULLNAME,
            RoadSegmentID,
            KC_FCC=factor(KC_FCC,levels=c('L','C','M','P','F'),
                       labels=c('Local','Collector','Minor','Primary','Freeway')),
            Juris=Jurisdiction,
            TAZ,
            emme_linkID,
            ADT_PSRC=ADT_tveh_PSRC,
            MediumVehicle=MedVehicle_PSRC,
            HeavyVehicle=HeavyVehicle_PSRC,
            CurbLen_FT=ifelse(is.na(CurbLen_FT),0,CurbLen_FT),
            PctCurbed=CurbLen_FT/Shape_Leng*100,
            GutterLen_FT =ifelse(is.na(GutterLen_FT ),0,GutterLen_FT ),
            PctGuttered=GutterLen_FT/Shape_Leng*100,
            RdSkirtPctImp=RdSkirtPctImpervious*100,
             OpenConvey=Ditch|Swales|Streams,
             ClosedConvey=Pipe,
             Convey=OpenConvey|ClosedConvey,
             ConveyType=case_when(Pipe==1~'Piped',
                                  Swales==1~'Swale',
                                  Ditch==1~'Ditched',
                                  Streams==1~'Stream',
                                  T~'None') %>% factor(levels=c('None','Ditched','Swale','Stream','Piped')),
            across(PipeLen_FT:StreamLen_FT ,function(x) ifelse(is.na(x),0,as.numeric(x))),
            ConveyLen_FT=(PipeLen_FT +DitchLen_FT+SwaleLen_FT+StreamLen_FT),
            PctConveyed=ConveyLen_FT/Shape_Leng*100,
            PctConveyed=ifelse(PctConveyed>100,100,PctConveyed),
            BusTraffic =ifelse(is.na(BusTraffic ),0,BusTraffic ),
             RoadMiles=Shape_Leng/5280
         ) %>%
  filter(Juris %in% c('WSDOT','King County') ) %>%
  right_join(st_address %>% select(OBJECTID),.)

nrow(kc_roads_SM) #89309 (when not using filter)
nrow(st_address) #89313
#missing four roads...
# st_address %>%
#   filter(!(OBJECTID %in% kc_roads_SM$OBJECTID))
#OBJECID 1 - looks like a fake road? -1 for segiment ID, no name
#BOREN-36052
#BOSTON-38101
#22ND-17351
#  filter(Juris=="King County"|Juris=='WSDOT') %>%
  #no longer needed
  # left_join(emme_links %>% st_drop_geometry()%>% select(X.tveh,X.dyhtrk,X.dymtrk),
  #           multiple='first',
  #           by=c('ADT_PSRC'='X.tveh')) %>%
  #adjust to remove freeway numbers from load roads

  # mutate(ADT_PSRC=ifelse(KC_FCC=='Local'&ADT_PSRC>1000,
  #                        rnorm(length(which(KC_FCC=='Local'&ADT_PSRC>1000)),
  #                              mean(ADT_PSRC[KC_FCC=='Local'&ADT_PSRC<500]),
  #                             sd(ADT_PSRC[KC_FCC=='Local'&ADT_PSRC<500])),
  #                        ADT_PSRC),
  #        MediumVehicle=ifelse(KC_FCC=='Local'&X.dymtrk>50,
  #                                  median(X.dymtrk[KC_FCC=='Local'&X.dymtrk<50]),
  #                            X.dymtrk),
  #        HeavyVehicle=ifelse(KC_FCC=='Local'&X.dyhtrk>50,
  #                            median(X.dyhtrk[KC_FCC=='Local'&X.dyhtrk<50]),
  #                            X.dyhtrk))
#issue with Freeways having zero, let's articially up those to 150001
 # mutate(ADT_PSRC=ifelse(grepl('FWY|SR',FULLNAME)&ADT_PSRC<1000,runif(1,15001,30000),ADT_PSRC))
#add lookup for heavy vehicles from emme link
 # left_join(emme_links %>% st_drop_geometry() %>% select(X.tveh,X.dyhtrk) %>% distinct(),
  #          by=c('ADT_PSRC'='X.tveh'))


kc_roads_SM %>%
  st_drop_geometry() %>%
  group_by(KC_FCC) %>%
  summarise(n=n(),
            AveRoadLength=mean(RoadMiles),
            TotalRoadMiles=sum(RoadMiles),
            AveADT=mean(ADT_PSRC,na.rm=T),
            VehicleRoadMiles=sum(RoadMiles*ADT_PSRC,na.rm=T),
            AveMediumVehicles=mean(MediumVehicle,na.rm=T),
            HeavyMediumRoadMiles=sum(RoadMiles*MediumVehicle,na.rm=T),
            #HeavyVehicles=mean(FreightRank,na.rm=T),
            AveHeavyVehicles=mean(HeavyVehicle,na.rm=T),
            HeavyVehicleRoadMiles=sum(RoadMiles*HeavyVehicle,na.rm=T),
            RoadMilesConvey=sum(RoadMiles*Convey,na.rm=T),
            PctConveyed=RoadMilesConvey/TotalRoadMiles,
            nConvey=sum(Convey),
            PctConvey_Count=nConvey/n,
            RdSkirtPctImp=mean(RdSkirtPctImp,na.rm=T)) %>%
  write.csv('outputs/kc_wsdot_roads_summar.csv',row.names = F)

#length of each road segement
kc_roads_SM %>%
  ggplot(aes(RoadMiles,fill=KC_FCC))+
  geom_density(alpha=0.5)+
  theme_bw()+
  scale_x_log10(label=scales::number,breaks=c(0.001,.01,0.1,0.5,1,5,10),
                minor_breaks=NULL)


plot_dist_ADT_count<-kc_roads_SM %>%
  ggplot(aes(ADT_PSRC))+
  geom_density(alpha=.5)+
  geom_density(alpha=.5)+
  scale_x_log10('Average Daily Traffic',
                limits=c(10,200000),breaks=c(100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw()#+
#  geom_vline(xintercept=c(500,5000,15000))+
 # annotate('label',x=c(100,1000,8000,25000),y=1.5,label=c('Low','Mod.','High','V. High'))

plot_ADT_FCC<-kc_roads_SM %>%
  ggplot(aes(ADT_PSRC,fill=KC_FCC))+
  geom_density(alpha=.5)+
  geom_density(alpha=.5)+
  scale_x_log10('Average Daily Traffic',
                limits=c(10,200000),breaks=c(100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw()#+
 # geom_vline(xintercept=c(500,5000,15000))+
  #annotate('label',x=c(100,1000,8000,25000),y=1.5,label=c('Low','Mod.','High','V. High'))
ggsave('plots/adt_by_fcc.png',plot_ADT_FCC,scale=0.8,width=10,height=4.5)

plot_ADT_FCC_no_FW_locals<-kc_roads_SM %>%
  filter(KC_FCC %in% c('Collector','Minor','Primary','Freeway')) %>%
  ggplot(aes(ADT_PSRC,fill=KC_FCC))+
  geom_density(alpha=.5)+
  scale_x_log10('Average Daily Traffic',
                limits=c(10,200000),breaks=c(100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw()#+
 # geom_vline(xintercept=c(500,5000,15000))+
  #annotate('label',x=c(100,1000,8000,25000),y=1.5,label=c('Low','Mod.','High','V. High'))

kc_roads_SM %>%
  ggplot(aes(MediumVehicle))+
  geom_density(alpha=.5)+
  scale_x_log10('Medium Vehicle Count (per day)',
                limits=c(1,50000),breaks=c(10,50,100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw() #+
#geom_vline(xintercept = c(5,50,1000))+
# annotate('label',x=c(1,30,500,5000),y=0.5,label=c('Low','Mod.','High','V. High'))

plot_dist_med_freight_count<-kc_roads_SM %>%
  ggplot(aes(MediumVehicle,fill=KC_FCC))+
  geom_density(alpha=.5)+
  scale_x_log10('Medium Vehicle Count (per day)',
                limits=c(1,50000),breaks=c(10,50,100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw() #+
#geom_vline(xintercept = c(5,50,1000))+
# annotate('label',x=c(1,30,500,5000),y=0.7,label=c('Low','Mod.','High','V. High'))
ggsave('plots/freight_med_by_fcc.png',plot_dist_med_freight_count,scale=0.8,width=10,height=4.5)

kc_roads_SM %>%
  ggplot(aes(HeavyVehicle))+
  geom_density(alpha=.5)+
  scale_x_log10('Heavy Vehicle Count (per day)',
                limits=c(1,50000),breaks=c(10,50,100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw() #+
  #geom_vline(xintercept = c(5,50,1000))+
 # annotate('label',x=c(1,30,500,5000),y=0.5,label=c('Low','Mod.','High','V. High'))

plot_dist_freight_count<-kc_roads_SM %>%
  ggplot(aes(HeavyVehicle,fill=KC_FCC))+
  geom_density(alpha=.5)+
  scale_x_log10('Heavy Vehicle Count (per day)',
                limits=c(1,50000),breaks=c(10,50,100,500,1000,5000,10000,50000,100000,200000),minor_breaks=NULL,
                labels=scales::label_number(scale_cut = scales::cut_short_scale()))+
  theme_bw() #+
  #geom_vline(xintercept = c(5,50,1000))+
 # annotate('label',x=c(1,30,500,5000),y=0.7,label=c('Low','Mod.','High','V. High'))
ggsave('plots/freight_by_fcc.png',plot_dist_freight_count,scale=0.8,width=10,height=4.5)


#with emme link output
# emme_links %>%
#   ggplot(aes(X.dyhtrk))+
#   geom_density(alpha=.5)+
#   scale_x_log10()+
#   theme_bw() +
#   geom_vline(xintercept = c(5,50,1000))

plot_ADT_med_Freight<-kc_roads_SM %>%
  ggplot(aes(ADT_PSRC,MediumVehicle))+
  geom_point()+
  facet_wrap(~KC_FCC)+
  theme_bw()+
  scale_x_log10()+scale_y_log10()+
  geom_abline(slope=1)

plot_ADT_Freight<-kc_roads_SM %>%
  ggplot(aes(ADT_PSRC,HeavyVehicle))+
  geom_point()+
  facet_wrap(~KC_FCC)+
  theme_bw()+
  scale_x_log10()+scale_y_log10()+
  geom_abline(slope=1)

plot_med_Freight<-kc_roads_SM %>%
  ggplot(aes(MediumVehicle,HeavyVehicle))+
  geom_point()+
  facet_wrap(~KC_FCC)+
  theme_bw()+
  scale_x_log10()+scale_y_log10()+
  geom_abline(slope=1)

plot_ADT_Imp<-kc_roads_SM %>%
  ggplot(aes(ADT_PSRC,RdSkirtPctImp))+
  geom_point()+
  facet_wrap(~KC_FCC)+
  theme_bw()+
  scale_x_log10()

plot_Imp_FCC<-kc_roads_SM %>%
  ggplot(aes(RdSkirtPctImp,fill=KC_FCC))+
  geom_density(alpha=.5)+
  theme_bw()+
  xlab('Road Skirt Imperviousness (%)')
#  geom_vline(xintercept = c(.25,0.5))+
  # ggtitle("Roadway Connectedness")+
  # annotate('label',y=6,x=c(.75,0.375,.125),label=c('High\n(>50% imp & Piped)',
  #                                       'Moderate\n(>25% Imp\nOR\n>50% Imp &\nConveyance Present)',
  #                                       'Low\n(<25% Imp)'))
ggsave('plots/imperviousness_by__FCC.png',plot_Imp_FCC,scale=0.8,width=10,height=4.5)

plot_convey_FCC<-kc_roads_SM %>%
  ggplot(aes(PctConveyed,fill=KC_FCC))+
  geom_density(alpha=.5)+
  theme_bw()+
  xlab('Conveyance (%)')
ggsave('plots/conveyance_by_FCC.png',plot_convey_FCC,scale=0.8,width=10,height=4.5)

plot_convey_FCC<-kc_roads_SM %>%
  ggplot(aes(ConveyType))+
  geom_bar(stat='count')+
  scale_y_log10()+
  facet_wrap(~KC_FCC)+
  theme_bw()

plot_convey_imp<-kc_roads_SM %>%
  ggplot(aes(RdSkirtPctImp,fill=ConveyType))+
  geom_density(alpha=.5)+
  facet_wrap(~KC_FCC,scales = 'free_y')+
  theme_bw()
ggsave('plots/imperviousness_by_conveyance_FCC.png',plot_convey_imp,scale=0.8,width=10,height=4.5)



library(leaflet)

#alternative scoring could be done so that the bins aren't artificially lost
#e.g., (log10(Traffic*x*log10(HvyVeh))+log10(Connectedness)
kc_roads_score_alternative<-kc_roads_SM %>%
  transmute(OBJECTID,Juris,FULLNAME,RoadSegmentID,KC_FCC,
            RoadMiles,
            TrafficIntensity=ifelse(is.na(ADT_PSRC),1,ADT_PSRC),
            HeavyVehicleCount=ifelse(is.na(HeavyVehicle),1,HeavyVehicle),
            MediumVehicleCount=ifelse(is.na(MediumVehicle),1,MediumVehicle),
            GenScore=round(log10(TrafficIntensity+5*MediumVehicleCount+6.4*HeavyVehicleCount+1),2),
            GenScore=ifelse(is.na(GenScore),0,GenScore),
            RoadwaySkirtImperviousness=ifelse(is.na(RdSkirtPctImp),RdSkirtPctImp[-1],RdSkirtPctImp),
            RoadwayDrainage=ConveyType,
            PctConveyed,
            ImpScore=log10(((RoadwaySkirtImperviousness/100+0.01))^2),
            ConveyScore=log10(PctConveyed/100+.01),
            RoadwayConnectednessScore=round(ImpScore+ConveyScore,2),
            TotalScore=(GenScore+RoadwayConnectednessScore),
           TotalScore=ifelse(TotalScore<0,0,TotalScore)
  )

plot_ecdf_Scores<-kc_roads_score_alternative %>%
  ggplot(aes(TotalScore,col=KC_FCC,group=KC_FCC))+
  stat_ecdf()+
  theme_bw()
ggsave('plots/plot_ecdf_Scores.png',plot_ecdf_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_Scores<-kc_roads_score_alternative %>%
  ggplot(aes(x=KC_FCC,TotalScore,fill=KC_FCC,group=KC_FCC))+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(height = 0,shape=21,alpha=.5)+
  theme_bw()+
  scale_y_continuous('Screening Model Score',limits=c(0,6))+
  xlab('Road Classification')
ggsave('plots/plot_jitter_Scores.png',plot_jitter_Scores,scale=0.8,width=10,height=4.5)

plot_jitter_Scores_antiLog<-kc_roads_score_alternative %>%
  #filter(TotalScore>=7) %>%
  ggplot(aes(x=KC_FCC,10^TotalScore,fill=KC_FCC,group=KC_FCC))+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(height = 0,shape=21,alpha=.5)+
  theme_bw()+
  scale_y_continuous('Screening Model Score (antilog)',
                     labels=scales::label_log())+
  xlab('Road Classification')
ggsave('plots/plot_jitter_Scores_antiLog.png',plot_jitter_Scores_antiLog,scale=0.8,width=10,height=4.5)

top10_scores<-arrange(kc_roads_score_alternative,desc(TotalScore)) %>% st_drop_geometry()%>%slice(1:20) %>%
  mutate(RoadMiles=round(RoadMiles,1),
         across(c(TrafficIntensity:MediumVehicleCount,RoadwaySkirtImperviousness,PctConveyed),round)) %>%
  select(-ImpScore,-ConveyScore,-OBJECTID) 
top10_scores%>%
  write.csv('outputs/top10_roads.csv',row.names = F)

kc_roads_score_alternative %>%
  st_write('outputs/kc_roads_scored.shp',append = F)
zip('outputs/kc_roads_scored.zip',list.files('outputs','kc_roads_scored',full.names = T))

pal_alt<-colorNumeric('RdYlBu',0:6,reverse=T)
kc_roads_score_alternative %>%
#  filter(TotalScore>=5) %>%
  leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addPolylines(color=~pal_alt(TotalScore),popup = ~paste(FULLNAME,RoadSegmentID,KC_FCC,Juris,
                                                     paste('Total Score:',TotalScore),
                                                     paste('ADT:',round(TrafficIntensity)),
                                                     paste('Hvy Veh:',round(HeavyVehicleCount)),
                                                     paste('Med Veh:',round(MediumVehicleCount)),
                                                     paste('%Imp:',round(RoadwaySkirtImperviousness,0)),
                                                     paste('Drainage:',RoadwayDrainage),
                                                     paste('%Conveyed:',round(PctConveyed,0)),
                                                    # paste('Connectedness:',RoadwayConnectednessScore),
                                                     sep='<br>'),
               opacity = 1) %>%
  addLegend('bottomright',pal_alt,0:6,title='Screening Model\nScore')

#Plots for Presentation
#ADT distribution by KC_FCC
#Breakpoints for ADT classes

#Vehicle weight distribution by KC_FF and ADT Classes
#breakpoints for VW classes

#Roadway Skirt Imperviousness by KC_FCC and ADT classes
#Roadway Skirt Imperviousness and Conveyance type

#Map of Scores
#zoom in too neighborhood

#normalize by miles? how?

#cluster analysis
library(cluster)

cluster_matrix<-kc_roads_SM %>%
  st_drop_geometry() %>%
 # filter(KC_FCC!='Local') %>%
  transmute(RoadSegmentID,KC_FCC,
    ADT_PSRC=sqrt(ADT_PSRC),
         HeavyVehicle=sqrt(HeavyVehicle),
         RdSkirtPctImp=sqrt(RdSkirtPctImp),
         PctConveyed=sqrt(PctConveyed)) %>%
  na.omit() 
row.names(cluster_matrix)<-cluster_matrix$RoadSegmentID
cluster_fit<-cluster_matrix%>%
  select(-RoadSegmentID,-KC_FCC) %>%
  scale() %>%
  kmeans(5)

clusplot(cluster_matrix, 
         cluster_fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

kc_roads_SM %>%
  filter(grepl('STEVENS PASS',FULLNAME))

library(mclust)
#still using square roots from above
X<-cluster_matrix %>% select(-KC_FCC,-RoadSegmentID) 
class<-cluster_matrix$KC_FCC
clPairs(X,class)
BIC <- mclustBIC(X)
plot(BIC)
summary(BIC)

mod1 <- Mclust(X, x = BIC,G=9)
summary(mod1, parameters = TRUE)
plot(mod1,what='classification')
table(class, mod1$classification)

