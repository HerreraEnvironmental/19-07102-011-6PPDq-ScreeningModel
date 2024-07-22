##all roads traffic
library(sf)
library(readxl)
library(dplyr)

st_address<-st_read(paste0("https://gisdata.kingcounty.gov/arcgis/rest/services/",
                           "OpenDataPortal/transportation__st_address_line/MapServer/108/query?outFields=*&where=1%3D1&f=geojson"))

st_address_attribute_summary<-read_excel('inputs/KC_Street_Address_SummaryAttributes_20240223_v2.xlsx') 

st_address_ADT<-st_address %>%
  select(OBJECTID:KC_FCC,Shape_Length) %>%
  mutate(across(FRADDL:TOADDR,as.numeric),
         FULLNAME=trimws(FULLNAME)) %>%
  left_join(st_address_attribute_summary%>% 
              select(FRADDL:FULLNAME,RoadSegmentID,ADT_tveh_PSRC,MedVehicle_PSRC ,HeavyVehicle_PSRC),
            by=c('FRADDL','FRADDR','TOADDL','TOADDR','FULLNAME'),
            multiple='first'
  )

st_write(st_address_ADT,'outputs/st_address_PSRC_ADT.shp')
