library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(sf)
library(jsonlite)
library(highcharter)
library(formattable)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(geojsonsf)
source("funciones.R")
source("graphs.R")

shp <- st_read("/Users/javierruizrubio/Desktop/fsp/SHP-Municipios/Municipios.shp")%>% st_transform( "+init=epsg:4326")
# shp<-shp %>% filter(Estado=="México")
# shp1 <-shp %>%  st_as_sf( ) %>% st_zm( drop = T, what = "ZM")%>%  st_as_sf( ) %>% 
# write_json("ixt_chal.json")
shp1<-read_json("ixt_chal.json")

# shp <- st_read("/Users/javierruizrubio/Desktop/fsp/SHP-Colonias") %>% st_transform( "+init=epsg:4326")
# shp<-shp %>% filter(ST_NAME=="MEXICO" & MUN_NAME== "IXTAPALUCA") %>% 
#   st_as_sf() %>% st_zm( drop = T, what = "ZM")

black_zone<-bind_rows( 
 shp %>% filter(SETT_NAME=="MORELOS"),
 shp %>% filter(SETT_NAME=="EL PORVENIR"),
 shp %>%  mutate(esta=case_when(grepl("OTILIO MONTAÑO", SETT_NAME)==T~1)) %>%  filter(esta==1),
 shp %>%  mutate(esta=case_when(grepl("CIVAC", SETT_NAME)==T~1)) %>%  filter(esta==1),
 shp %>% mutate(esta=case_when(grepl("TEJALPA", SETT_NAME)==T~1)) %>%  filter(esta==1))


shp_ixtapaluca <- st_read("/Users/javierruizrubio/Desktop/fsp/SHP-Municipios")%>% 
           st_transform( "+init=epsg:4326") %>% 
           filter(Estado=="México" & (Municipios=="Ixtapaluca" | Municipios=="Chalco" |Municipios=="Valle de Chalco Solidaridad")) %>%
  st_as_sf( )


del_map_jiu<-read_xlsx("/Users/javierruizrubio/Downloads/chalco e ixtapaluca (1).xlsx")
#PON LA DIRECCIÓN DEL ARCHIVO QUE DESCARGASTE
delitos<-read.csv("/Users/javierruizrubio/Downloads/Municipal-Delitos-2015-2025_dic2025/Municipal-Delitos-2015-2025_dic2025.csv", fileEncoding = "latin1")
names(delitos)[c(1,6)]<-c("años", "Bien juridico afectado")
delitos<-delitos %>% filter(Clave_Ent=="15")

delitos0<-delitos %>% select(años, Modalidad, c(10:21)) %>%  limpia_del()     
delitos_ixt<-delitos %>% filter( Cve..Municipio=="15039") %>% select(años, Modalidad, c(10:21)) %>%  limpia_del()
delitos_chal<-delitos %>% filter( Cve..Municipio=="15025") %>% select(años, Modalidad, c(10:21)) %>%  limpia_del()
delitos_vchal<-delitos %>% filter( Cve..Municipio=="15122") %>% select(años, Modalidad, c(10:21)) %>%  limpia_del()

graph1<-delitos0 %>% group_by(mes) %>% summarise(`Promedio mensual`=round(sum(delitos)/30,0)) %>% 
        left_join(delitos_ixt %>% group_by(mes) %>% summarise(`Ixtapaluca`=sum(delitos)) ) %>% 
        left_join(delitos_chal %>% group_by(mes) %>% summarise(`Chalco`=sum(delitos)) ) %>% drop_na() %>% 
        left_join(delitos_vchal %>% group_by(mes) %>% summarise(`Valle de Chalco`=sum(delitos)) ) %>% drop_na() %>% 
        pivot_longer(cols =c(`Promedio mensual`:`Valle de Chalco`), names_to = c("mes1"), values_to = "delitos") %>% 
  g_lines0( "Incidentes", "Fecha")

titulo_1 <-paste("Delitos 2025: Total=", delitos0 %>%suma_del(),
                 ", Ixtapaluca:", delitos_ixt %>%suma_del(),
                 ", Chalco:", delitos_chal %>%suma_del(),
                 ", Valle de Chalco:", delitos_vchal %>%suma_del())

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
    mutate(Cve..Municipio=as.character(Cve..Municipio), n=as.numeric(n), Cve..Municipio=as.character(Cve..Municipio)) %>% 
    rename(CVEGEO="Cve..Municipio") %>% mapas(shp1)

   prom_mes_ixt<-delitos_ixt %>% filter(años>=2023) %>% des_anos() 
   tabla2<- arregla_tabla(prom_mes_ixt)
   
   prom_mes_chal<-delitos_chal  %>% filter(años>=2023)%>% des_anos() 
   tabla3<- arregla_tabla(prom_mes_chal)
   
   prom_mes_vchal<-delitos_vchal  %>% filter(años>=2023)%>% des_anos() 
   tabla4<- arregla_tabla(prom_mes_vchal)
   
   ixtapaluca<-tabla_comps("Ixtapaluca", "15039", delitos)
   chalco<-tabla_comps("Chalco", "15025", delitos)
   va_chalco<-tabla_comps("Valle de Chalco", "15122", delitos)
   

library(leaflet)
library(leaflet.extras)
library(googlesheets4)   
library(htmltools)  
library(readxl)   
   puntos<-read_xlsx("/Users/javierruizrubio/Downloads/chalco e ixtapaluca (1).xlsx", sheet = "Hoja1") 
   puntos<-puntos %>% 
       mutate(lat=as.numeric(lat), lon=as.numeric(lon),
              Fecha=as.Date(Fecha))
   
   shp <- st_read("/Users/javierruizrubio/Desktop/fsp/SHP-Colonias") %>% st_transform( "+init=epsg:4326")
   shp<-shp %>% filter(ST_NAME=="MEXICO" & (MUN_NAME== "IXTAPALUCA" | MUN_NAME=="CHALCO" |  MUN_NAME=="VALLE DE CHALCO SOLIDARIDAD") ) %>% 
     st_as_sf( ) %>% st_zm( drop = T, what = "ZM")
   
   black_zone<- shp %>% filter(SETT_NAME %in% c("NIÑOS HEROES", "COVANDONGA",
                                                "UNION DE GUADALUPE", "FRACC VILLAS DE CHALCO", "EL MOLINO",
                                                "JORGE JIMENEZ CANTU","ALFREDO DEL MAZO", "UNIDAD HAB CUATRO VIENTOS",
                                                "SAN JERONIMO CUATRO VIENTOS", "AMPL MORELOS", "GUADALUPANA",
                                                "SAN ISIDRO", "SANTA CRUZ", "AN MIGUEL XICO",
                                                "PROVIDENCIA", "MARIA ISABEL"))
   
   red_zone<-shp %>% filter(SETT_NAME %in% c("CONJUNTO HAB LOS PORTALES", "PORTALES", "CONJUNTO HAB LOS PORTALES"))
   
   mapa_2<-leaflet(width ="830px", height="520px") %>% 
     setView(-98.89255331, 19.26288483, 12) %>%
     addTiles() %>%  
     addCircles(lat=19.6328929, lng=-99.112371, radius = 2000, weight = 4, fillOpacity = 0, color = 'orange') %>% 
     addLegend(title = "Zonas", position = c("topright"),
               colors =  c("#000","#279e0e", "#a61717", "orange"),
               labels = c("Zonas alta incidencia", "Zonas propuestas", "Delitos reportados", "Radio 2km")) %>% 
     addMarkers( label ="F62", lat=19.31391481, lng=-98.88830659) %>% 
     addMarkers( label ="F66", lat=19.30039619, lng=-98.93929376) %>% 
     addMarkers( label ="F156", lat=19.26288483, lng=-98.89655331) %>% 
     addPolygons(data = shp_ixtapaluca, label=shp_ixtapaluca$Municipios, weight = 4, fillOpacity = 0, color = '#F135FF') %>% 
     addPolygons(data = black_zone, fillColor = '#000', fillOpacity = 0.7) %>% 
     #addPolygons(data = red_zone,  fillColor = '#279e0e', fillOpacity = 0.7) %>% 
     addPolygons(data = shp, weight = 2, fillOpacity = 0, color = '#007bff',  label = ~SETT_NAME,
                 popup = paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Zona: ",shp$SETT_NAME,  "</div><br>",
                                "<b>Tipo de vivienda</b>: ", shp$SETT_TYPE,"<br>")) %>%
     addDrawToolbar(targetGroup='datos', position = "bottomleft",
                    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>% 
     addCircleMarkers(data = puntos, radius = 5,   lat =puntos$lat, lng = puntos$lon, opacity = .9, fill = TRUE,
                       fillOpacity = 0.8, color = "#a61717",
                      stroke = FALSE, labelOptions = labelOptions(noHide=T, offset = c(-3,0), textOnly = T),
                      popup = paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Zona: ", puntos$Categoria,  "</div><br>",
                                      "<b>Nota</b>: ",  puntos$Noticia, "<br>",
                                      "<b>Fecha</b>: ", paste(puntos$Fecha)))
     
     
   
   rmarkdown::render("ixt.Rmd", output_file = "press.html",
                     params = list(graph1=graph1, titulo_1=titulo_1, tabla1=tabla1, mapa1=mapa1,
                                   tabla2=tabla2, tabla3=tabla3, tabla4=tabla4, mapa_2=mapa_2))
   
              
            