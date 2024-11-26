library(dplyr)
library(readxl)
library(sf)

#select geodatabase holding output from Geospatial Harmonization
hec_gdb<-'\\\\herrera.local\\hecnet\\gis-k\\Projects\\Y2019\\19-07102-011\\Geodatabase\\GIS_Working\\WQBE6ppdq_20241007.gdb'

#Load in the harmonized spatial layer
hec_gis_export<-st_read(hec_gdb,
          layer = 'King_County_St_Addresses_6PPDQ_Metrics_20241008_v2')

#read in jurisdiction by areas (i.e., if a WSDOT road runs through the city, what city is it in?)
geo_sw_juris<-read_excel('O:\\proj\\Y2019\\19-07102-011\\GIS\\KC_Street_Address_Juris_SWJuris_20240924.xlsx') %>%
  distinct()

#missing jurisdictions from the aggregrated stormwater dataset
missing_sw_juris<-c('Carnation','Medina','Yarrow Point','Skykomish')


kc_roads_SM<-
  hec_gis_export %>%  
  #transform to WGS1984 coordinate system
  st_transform(4326) %>%
  #join with geographic stormwater jurisdictions
  left_join(geo_sw_juris) %>%
  transmute(FRADDL,FRADDR,TOADDL,TOADDR,FULLNAME,
            RoadSegmentID, #unique ID for every road segment
            KC_FCC=factor(KC_FCC,levels=c('L','C','M','P','F'),
                          labels=c('Local','Collector','Minor','Primary','Freeway')),
            SW_Juris, #geographic MS4 jurisdiction
            Juris=Jurisdiction, #road owner
            TAZ, #transportation analysis zone
            emme_linkID, #PSRC SoundCast model Link ID
            ADT_PSRC=ADT_tveh_PSRC, #average daily traffic (all vehicles) from PSRC Model
            MediumVehicle=MedVehicle_PSRC,#average daily traffic (medium-weight vehicles) from PSRC Model
            HeavyVehicle=HeavyVehicle_PSRC,#average daily traffic (heavy-weight vehicles) from PSRC Model
            CurbLen_FT=ifelse(is.na(CurbLen_FT),0,CurbLen_FT), #length of curb in feet
            PctCurbed=CurbLen_FT/2/Shape_Leng*100, #percent of road with curb
            GutterLen_FT =ifelse(is.na(GutterLen_FT ),0,GutterLen_FT ),#length of gutter in feet
            PctGuttered=GutterLen_FT/2/Shape_Leng*100,#percent of road with curb
            RdSkirtPctImp=RdSkirtPctImpervious*100, 
            #percent of road skirt buffer (6-meter on either side of road surface) that is impervious
            Grade,#road slope
            RdSkirtPctOverwater=100*as.numeric(ifelse(is.na(RdSkirtPctOverwater),0,RdSkirtPctOverwater)),
            #how much of the road skirt is over water?
            StreamWaterCrossing=ifelse(is.na(StreamWaterCrossing),0,StreamWaterCrossing),
            #does the road cross a stream or water body at any time? (wtr_crs wtr_bdy)
            isRamp=ifelse(is.na(isRamp),0,isRamp), #is the segement a Freeway Ramp
            isOverpass=ifelse(is.na(isOverpass),0,isOverpass),
            isUnderpass=ifelse(is.na(isUnderpass),0,isUnderpass),
            OpenConvey=Ditch|Swales|Streams,#does the road have an open conveyance structure present?
            ClosedConvey=Pipe, #does the road have an close conveyance structure present?
            Convey=OpenConvey|ClosedConvey, #any conveyance
            across(PipeLen_FT:StreamLen_FT ,function(x) ifelse(is.na(x),0,as.numeric(x))), #replace NAs with zero
            #Identify the conveyance type in a single column
            ConveyType=case_when(DitchLen_FT>0~'Ditched',
                                 PipeLen_FT>0~'Piped',
                                 SwaleLen_FT>0~'Swale',
                                 StreamLen_FT>0~'Stream',
                                 T~'None') %>% factor(levels=c('None','Ditched','Swale','Stream','Piped')),
            ConveyLen_FT=(PipeLen_FT +DitchLen_FT+SwaleLen_FT+StreamLen_FT), #total conveyance length
            #Where is stormwater data available? (WSDOT, King County or Seattle provided high quality data)
            SW_DATA_Available=(Juris=='WSDOT'&ConveyLen_FT>0)|SW_Juris %in% c('King County','Seattle'),
            #where is there stormwater from the aggregated source provided by King County GIS
            Aggr_SW_DATA_Available=!(SW_Juris %in% missing_sw_juris),
            #calculate percent conveyed
            PctConveyed=ConveyLen_FT/Shape_Leng*100,
            PctConveyed=ifelse(PctConveyed>100,100,PctConveyed), #top out at 100%
            PctConveyed=ifelse(SW_DATA_Available,PctConveyed,NA),
            #aggregated stormwater data,
            AggregatedSWLen_KC=ifelse(is.na(AggregatedSWLen_KC)&Aggr_SW_DATA_Available,0,AggregatedSWLen_KC),
            #use seattle, KC, and WSDOT data if there are data available
            AggregatedSWLen_KC=ifelse(SW_DATA_Available,ConveyLen_FT,AggregatedSWLen_KC),
            Aggr_PctConveyed=AggregatedSWLen_KC/Shape_Leng*100,
            Aggr_PctConveyed=ifelse(Aggr_PctConveyed >100,100,Aggr_PctConveyed),
            Aggr_PctConveyed=ifelse(Aggr_SW_DATA_Available,Aggr_PctConveyed,NA),
            #remove conveyance for WSDOT within other jurisdictions
            ConveyType=if_else(SW_DATA_Available,ConveyType,NA), 
            BusTraffic =ifelse(is.na(BusTraffic ),0,BusTraffic ), #bus route present?
            Shape_Length=Shape_Leng, #feet
            RoadMiles=Shape_Leng/5280
  )
