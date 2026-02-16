library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(sf)
library(jsonlite)
library(highcharter)
library(formattable)
library(readxl)
source("funciones.R")
source("graphs.R")

shp <- st_read("/Users/javierruizrubio/Desktop/fsp/SHP-Municipios/Municipios.shp")
# shp<-shp %>% filter(Estado=="México")
# shp1 <-shp %>% st_as_sf( )
# write_json(shp1, "morelos.json")

shp <- st_read("/Users/javierruizrubio/Desktop/fsp/SHP-Colonias") %>% st_transform( "+init=epsg:4326")
shp<-shp %>% filter(ST_NAME=="MEXICO" & MUN_NAME== "IXTAPALUCA") %>% 
  st_as_sf( ) %>% st_zm( drop = T, what = "ZM")

black_zone<-bind_rows( 
 shp %>% filter(SETT_NAME=="MORELOS"),
 shp %>% filter(SETT_NAME=="EL PORVENIR"),
 shp %>%  mutate(esta=case_when(grepl("OTILIO MONTAÑO", SETT_NAME)==T~1)) %>%  filter(esta==1),
 shp %>%  mutate(esta=case_when(grepl("CIVAC", SETT_NAME)==T~1)) %>%  filter(esta==1),
 shp %>% mutate(esta=case_when(grepl("TEJALPA", SETT_NAME)==T~1)) %>%  filter(esta==1))


shp_ixtapaluca <- st_read("/Users/javierruizrubio/Desktop/fsp/SHP-Municipios")%>% 
           st_transform( "+init=epsg:4326") %>% 
           filter(Estado=="México" & Municipios=="Ixtapaluca") %>%
  st_as_sf( )

 
del_map_jiu<-read_xlsx("/Users/javierruizrubio/Downloads/chalco e ixtapaluca.xlsx")
#PON LA DIRECCIÓN DEL ARCHIVO QUE DESCARGASTE
delitos<-read.csv("/Users/javierruizrubio/Downloads/Historico Indicadores Op final. VF_Enero-SPINS122217.xlsx", fileEncoding = "latin1")
names(delitos)[c(1,6)]<-c("años", "Bien juridico afectado")
delitos<-delitos %>% filter(Clave_Ent=="15")

delitos0<-delitos %>% select(años, Modalidad, c(10:21)) %>%  limpia_del()     
delitos_coa<-delitos %>% filter( Cve..Municipio=="15020") %>% select(años, Modalidad, c(10:21)) %>%  limpia_del()
delitos_tultepec<-delitos %>% filter( Cve..Municipio=="15108") %>% select(años, Modalidad, c(10:21)) %>%  limpia_del()
delitos_tultitlan<-delitos %>% filter( Cve..Municipio=="15109") %>% select(años, Modalidad, c(10:21)) %>%  limpia_del()

graph1<-delitos0 %>% group_by(mes) %>% summarise(`Promedio mensual`=round(sum(delitos)/30,0)) %>% 
        left_join(delitos_coa %>% group_by(mes) %>% summarise(`Coacalco de Berriozábal`=sum(delitos)) ) %>% 
        left_join(delitos_tultepec %>% group_by(mes) %>% summarise(`Tultepec`=sum(delitos)) ) %>% drop_na() %>% 
        left_join(delitos_tultitlan %>% group_by(mes) %>% summarise(`Tultitlán`=sum(delitos)) ) %>% drop_na() %>% 
        pivot_longer(cols =c(`Promedio mensual`:`Tultitlán`), names_to = c("mes1"), values_to = "delitos") %>% 
        g_line( "Incidentes", "Fecha")

titulo_1 <-paste("Delitos 2025: Total=", delitos0 %>%suma_del(),
                 ", Coacalco de Berriozábal:", delitos_coa %>%suma_del(),
                 ", Tultepec:", delitos_tultepec %>%suma_del(),
                 ", Tultitlán:", delitos_tultitlan %>%suma_del())

tabla1<-delitos %>% filter( años==2025) %>% 
  select(Municipio, años, Modalidad, c(10:21)) %>% 
  limpia_del() %>% group_by(Municipio) %>% 
  summarise(n=sum(delitos, na.rm=T)) %>% arrange(desc(n)) %>% 
  mutate(por= (n)/sum(n, na.rm = T)) %>% mutate(n=accounting(n, digits = 0L)) %>% 
  head(12) 
  colnames(tabla1) <- c("Municipio", "Delitos", "Porcentaje")
  tabla1<- formattable(tabla1, 
                 align = c("l","r", "r"),
                 list(`Municipio` = formatter("span", style = ~ style(color = "grey", font.weight = "bold", fontFamily = "Sans-serif")), 
                      `Delitos` = color_bar("#d9ebba"),    
                      `Porcentaje` = percent), table.attr = 'style="font-size: 20px; line-height = 2em; ";\"')

   mapa1<-delitos %>% filter( años==2025) %>% 
    select(Cve..Municipio, años, Modalidad, c(10:21)) %>% 
    limpia_del() %>% group_by(Cve..Municipio) %>% summarise(n=sum(delitos, na.rm=T)) %>% 
    mutate(Cve..Municipio=as.character(Cve..Municipio)) %>% 
    rename(CVEGEO="Cve..Municipio") %>% mapas(shp1)

   prom_mes_jiu<-delitos_coa %>% filter(años>=2023) %>% des_anos() 
   tabla2<- arregla_tabla(prom_mes_jiu)
   
   prom_mes_tultepec<-delitos_tultepec  %>% filter(años>=2023)%>% des_anos() 
   tabla3<- arregla_tabla(prom_mes_tultepec)
   
   prom_mes_tultitlan<-delitos_tultitlan  %>% filter(años>=2023)%>% des_anos() 
   tabla4<- arregla_tabla(prom_mes_tultitlan)
   
   coa<-tabla_comps("Coacalco", "15020", delitos)
   tultepec<-tabla_comps("Tultepec", "15108", delitos)
   tultitlan<-tabla_comps("Tultitlán", "15109", delitos)
   
library(leaflet)
library(leaflet.extras)
library(googlesheets4)   
library(htmltools)   
   puntos<-read_sheet("https://docs.google.com/spreadsheets/d/1fnEaW2HLgCwN9QsZF6H6GogZhSnSOfO1D8nG6d718AU/edit?gid=0#gid=0") 
   puntos<-puntos %>% tidyr::separate(`lat y log`, sep=",", into = c("lat", "lon")) %>% 
       mutate(lat=as.numeric(lat), lon=as.numeric(lon))
   
   shp <- st_read("/Users/javierruizrubio/Desktop/fsp/SHP-Colonias") %>% st_transform( "+init=epsg:4326")
   shp<-shp %>% filter(ST_NAME=="MEXICO" & (MUN_NAME== "COACALCO DE BERRIOZÁBAL" | MUN_NAME=="TULTEPEC" |  MUN_NAME=="TULTITLÁN") ) %>% 
     st_as_sf( ) %>% st_zm( drop = T, what = "ZM")
   
   black_zone<- shp %>% filter(SETT_NAME %in% c("FRACC LOS HEROES DE COACALCO", "EL POTRERO",
                                                "POTRERO POPULAR", "VILLA DE LAS FLORES", "VILLA DE LAS FLORES 1RA SECC",
                                                "SIERRA GUADALUPE","SARDAÑA", "AMPL LA SARDAÑA",
                                                "EL TESORO", "AMPL EL TESORO", "BENITO JUAREZ",
                                                "EL MIRADOR", "LOMAS DE TULTEPEC", "LA CANTERA",
                                                "SANTIAGO TEYAHUALCO", "SAN ANTONIO XAHUENTO", 
                                                "SAN ANTONIO XAHUENTO"))
   
   red_zone<-shp %>% filter(SETT_NAME %in% c("CONJUNTO HAB LOS PORTALES", "PORTALES", "CONJUNTO HAB LOS PORTALES"))
   
   mapa_2<-leaflet(width ="830px", height="520px") %>% 
     setView(-99.09481, 19.63637, 14 ) %>%
     addTiles() %>%  
     addCircles(lat=19.6328929, lng=-99.112371, radius = 2000, weight = 4, fillOpacity = 0, color = 'orange') %>% 
     addLegend(title = "Zonas", position = c("topright"),
               colors =  c("#000","#279e0e", "#a61717", "orange"),
               labels = c("Zonas alta incidencia", "Zonas propuestas", "Delitos reportados", "Radio 2km")) %>% 
     addMarkers( label ="San Pablo", lat=19.6328929, lng=-99.112371) %>% 
     addPolygons(data = shp_ixtapaluca, weight = 4, fillOpacity = 0, color = '#F135FF') %>% 
     addPolygons(data = black_zone, fillColor = '#000', fillOpacity = 0.7) %>% 
     addPolygons(data = red_zone,  fillColor = '#279e0e', fillOpacity = 0.7) %>% 
     addPolygons(data = shp, weight = 2, fillOpacity = 0, color = '#007bff',  label = ~SETT_NAME,
                 popup = paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Zona: ",shp$SETT_NAME,  "</div><br>",
                                "<b>Tipo de vivienda</b>: ", shp$SETT_TYPE,"<br>")) %>%
     addDrawToolbar(targetGroup='datos', position = "bottomleft",
                    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>% 
     addCircleMarkers(data = puntos, radius = 5,   lat =puntos$lat, lng = puntos$lon, opacity = .9, fill = TRUE,
                       fillOpacity = 0.8, color = "#a61717",
                      stroke = FALSE, labelOptions = labelOptions(noHide=T, offset = c(-3,0), textOnly = T),
                      popup = paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Zona: ", puntos$clasificacion,  "</div><br>",
                                      "<b>Nota</b>: ",  puntos$delito, "<br>",
                                      "<b>Fecha</b>: ", paste(puntos$fecha)))
     
     
   
   rmarkdown::render("press.Rmd", output_file = "press.html",
                     params = list(graph1=graph1, titulo_1=titulo_1, tabla1=tabla1, mapa1=mapa1,
                                   tabla2=tabla2, tabla3=tabla3, tabla4=tabla4, mapa_2=mapa_2))
   
              
            

   

        