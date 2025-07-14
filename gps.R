library(httr)
library(jsonlite)

res <- VERB("GET", url = "https://web1ws.widetech.co/wsHistoryGetByPlate.asmx/HistoyDataLastLocationByUser_JSON?sLogin=WS_History_SANPABLO&sPassword=462D53414E5041424C4F2E475053")

tablas<-fromJSON(content(res, as='text'))
as2<-tablas[[1]][["Response"]][["Plate"]][["hst"]] %>% 
  mutate(Latitude=as.numeric(Latitude), Longitude=as.numeric(Longitude))

library(leaflet)
library(htmltools)

crea_icon_number<-function(number, colors){
  icons <- awesomeIcons(
    icon = 'ion-numeric',
    markerColor = colors,
    iconColor = 'white',
    text = number)
  return(icons)}

leaflet() %>% addTiles() %>% 
  addAwesomeMarkers(data =as , icon = crea_icon_number(as$`@id`, "blue"), 
                    lat = ~Latitude, lng = ~Longitude,  label = ~htmlEscape(PDI),
                    popup = paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Unidad ",as$`@id`,  "</div>")) %>% 
  addAwesomeMarkers(data =as1, icon = crea_icon_number(as1$`@id`, "red"), 
                    lat = ~Latitude, lng = ~Longitude,  label = ~htmlEscape(PDI),
                    popup = paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Unidad ",as1$`@id`,  "</div>")) %>% 
  addAwesomeMarkers(data =as2, icon = crea_icon_number(as2$`@id`, "orange"), 
                    lat = ~Latitude, lng = ~Longitude,  label = ~htmlEscape(PDI),
                    popup = paste0("<div style='font-size: 17px; font-weight:bold; text-align: center; background: white; padding: 4px; border-radius: 4px;'>","Unidad ",as2$`@id`,  "</div>"))
  






placas <- VERB("GET", url = "https://web1ws.widetech.co/wsHistoryGetByPlate.asmx/GetMobileList?sLogin=WS_History_SANPABLO&sPassword=462D53414E5041424C4F2E475053")

content(res, 'text')

xml_text(xml_find_all(placas, xpath = "//ITEM"))


xmlToDataFrame(nodes = getNodeSet(xmlRoot(placas), "//ITEM"))
