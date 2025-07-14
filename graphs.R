Sys.setlocale("LC_ALL", "es_ES.UTF-8")

colores<-c("#32AC90", "#A3A9AC","#E0BC4C", "#66b0f2", "#f266b0", "#ab83fe")

g_barras<-function(b, titulo, tooltip, rotacion, rx, ry){
  names(b)<-c("fecha", "estatus", "n") #titulo="as"; tooltip="prueba"; rotacion=0; rx=0; ry=0;
  b<-hchart(b,  fillOpacity = 0.3, type = "column" , stack = "Assets",  hcaes(x = fecha, y = n, group=estatus)) %>% 
    hc_plotOptions(column = list( stacking = "normal", showInLegend = TRUE, align = "center",
                                  dataLabels = list(enabled = T,  format = "{point.y}", align='left', x=rx, y=ry, rotation=rotacion,
                                                    style = list(fontFamily = "Titillium Web", fontSize = "0.9em", pointPadding= 0.1)))) %>% 
    hc_yAxis(gridLineWidth = 0, title = list(enabled = F, text = ""), 
             stackLabels = list(color = "#1E242B", x=rx, rotation=rotacion, fontWeight = "bold", enabled = TRUE),
             labels = list(enabled = TRUE,  style = list( fontFamily = "Titillium Web", color =  "#1E242B", fontSize = "1.2em"))) %>%
    hc_exporting(enabled = T) %>%   
    hc_xAxis(tickPixelInterval= 15, title= list(text=paste(" "), enable=F, margin = 1, align = "left",
                                                style = list(color = "#1E242B", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web")),
             labels = list(rotation=rotacion, style = list( fontFamily = "Titillium Web", color =  "#1E242B",  fontSize = ".95em"))) %>%     
    hc_chart(zoomType = "x") %>%  
    hc_legend(enabled=T, borderColor = "#5C666F8F1", borderWidth = 0,  align = "center",
              backgroundColor = "#5C666F8F1", itemHoverStyle = list(color = "#1E242B", fontSize = "1.1em"),
              itemStyle = list(fontFamily = "Titillium Web", color = "#1E242B", fontWeight="normal")) %>%
    hc_tooltip(crosshairs = F, shared = T, borderWidth = 3, useHTML = TRUE, backgroundColor = "#5C666F", fillOpacity = 1, borderRadius=15,
               sort = TRUE, style = list(align = "center",color = "#EEF2F5", fontFamily = "Titillium Web"),
               headerFormat = paste(tooltip, ": <b>{point.key}</b><br>"),
               footerFormat='</table>', valueDecimals= 0, useHTML = TRUE)
  return(b)}


g_barras_sc<-function(b, titulo, tooltip, rotacion, rx, ry){
  names(b)<-c("fecha", "estatus", "n") #titulo="as"; tooltip="prueba"; rotacion=0; rx=0; ry=0;
  b<-hchart(b,  fillOpacity = 0.3, type = "column" , stack = "Assets",  hcaes(x = fecha, y = n, group=estatus)) %>% 
    hc_plotOptions(column = list( stacking = "percent", showInLegend = TRUE, align = "center",
                                  dataLabels = list(enabled = T,  format = "{point.y} ({point.percentage:.1f}%)", align='left', x=rx, y=ry, rotation=rotacion,
                                                    style = list(fontFamily = "Titillium Web", fontSize = "0.9em", pointPadding= 0.1)))) %>% 
    hc_yAxis(gridLineWidth = 0, title = list(enabled = F, text = ""), 
             stackLabels = list(color = "#1E242B", x=rx, rotation=rotacion, fontWeight = "bold", enabled = TRUE),
             labels = list(enabled = TRUE,  style = list( fontFamily = "Titillium Web", color =  "#1E242B", fontSize = "1.2em"))) %>%
    hc_exporting(enabled = T) %>%   
    hc_xAxis(tickPixelInterval= 15, title= list(text=paste(" "), enable=F, margin = 1, align = "left",
                                                style = list(color = "#1E242B", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web")),
             labels = list(rotation=rotacion, style = list( fontFamily = "Titillium Web", color =  "#1E242B",  fontSize = "0.9em"))) %>%     
    hc_chart(zoomType = "x") %>%  
    hc_legend(enabled=T, borderColor = "#5C666F8F1", borderWidth = 1,  align = "center",
              backgroundColor = "#5C666F8F1", itemHoverStyle = list(color = "#1E242B", fontSize = "1.1em"),
              itemStyle = list(fontFamily = "Titillium Web", color = "#1E242B", fontWeight="normal")) %>%
    hc_tooltip(crosshairs = F, shared = T, borderWidth = 3, useHTML = TRUE,
               sort = TRUE, style = list(align = "center",color = "#EEF2F5", fontFamily = "Titillium Web"),
               headerFormat = paste(tooltip, ": <b>{point.key}</b><br>"),
               footerFormat='</table>', valueDecimals= 0, useHTML = TRUE)
  return(b)}


g_pie<-function(b, tooltip ){ #b=g6
  names(b)<-c("grupo", "n") #titulo="as"; tooltip="prueba"; rotacion=0; rx=0; ry=0;
  #colores1<-colores[1:length(unique(b$grupo))]
  b<- b %>% highcharter::hchart( type = "pie",  innerSize = '60%',   hcaes(x = grupo, y = n, color=colores[1:length(unique(grupo))])) %>% 
    hc_plotOptions(useHTML = TRUE, series = list( dataLabels =  list(enabled = T, distance= 5,  format = "<b>{point.name}</b><br>{point.y:,.0f} ({point.percentage:.1f}%)",
                                                                     style = list(color = "#5C666F", fontSize = "15px", fontFamily = "Titillium Web")))) %>% 
    #hc_title(text= titulo, margin = 14, align = "center",style = list(color = "#5C666F", useHTML = TRUE,  fontFamily = "Titillium Web"))%>%
    hc_chart(zoomType = "x") %>%   hc_exporting(enabled = T) %>% 
    hc_tooltip(useHTML = TRUE, style = list(fontSize = "20px", opacity = 1, align= "center", color = "#5C666F", fontFamily = "Titillium Web"),
               headerFormat = "",
               pointFormat = paste0("<div style='text-align: center;'> <b> ", paste0( tooltip, ""),  ": {point.name}</b> <br>{point.y:,.0f} ({point.percentage:.1f} %) <br> Total: {point.total:,.0f}</div>"),
               positioner = JS(
                 "function () {
        xp =  this.chart.chartWidth/2 - this.label.width/2
        yp =  this.chart.chartHeight/2 - this.label.height/2
        return { x: xp, y: yp }; }" ),
               shadow = FALSE,  borderWidth = 0, backgroundColor = "transparent", hideDelay = 1000) 
  return(b)}


g_line<-function(tabla, ytext, xtext){#tabla<- grafica_2, ytext="Cuentas", titulo="", xtext=""
  names(tabla)<-c("fecha", "grupo", "n")
  #colores<-colores[1:length(unique(tabla$grupo))]
  tabla<-tabla %>% hchart(type="line", hcaes(x=fecha, y=n, group=grupo), color=colores[1:length(unique(tabla$grupo))]) %>% 
    hc_yAxis(gridLineWidth = 0, title = list(enabled = T, text = ytext,
                                             style = list(color = "#5C666F", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web", fontSize = "1.2em")), 
             stackLabels = list(color = "#5C666F", y=10, fontWeight = "bold", enabled = TRUE),
             labels = list(enabled = TRUE, rotation=0, style = list( fontFamily = "Titillium Web", color =  "#5C666F", fontSize = "1.1em"))) %>%
    hc_exporting(enabled = T) %>%   
    hc_xAxis(title= list(text=xtext, style = list(color = "#5C666F", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web", fontSize = "1.2em")),
             labels = list(rotation=0, style = list( fontFamily = "Titillium Web", color =  "#5C666F",  fontSize = "1.1em"))) %>%     
    hc_chart(zoomType = "x") %>%  
    # hc_title(text= titulo, margin = 7, align = "center",
    #          style = list(color = "#5C666F", useHTML = TRUE,  fontFamily = "Titillium Web"))%>%
    hc_legend(enabled=T, borderWidth = 1, borderColor = "#EEF2F5", align = "center", itemHoverStyle = list(color = "#1E242B", fontSize = "1.1em"),
              itemStyle = list(fontFamily = "Titillium Web", color = "#5C666F", fontWeight="normal")) %>%
    hc_tooltip(crosshairs = F, shared = T, useHTML = TRUE, borderWidth = 1, backgroundColor = "#1E242B", fillOpacity = 0.85, borderRadius=15,
               sort = TRUE, style = list(align = "center",fontWeight="400", color = "#EEF2F5", fontFamily = "Titillium Web"),
               headerFormat = paste("Mes", ": <b>{point.key}</b><hr hr style='margin: 4px 0px 0px 5px;'>"),
               footerFormat='<table>', valueDecimals= 0, useHTML = TRUE)
  return(tabla)  }

mapas<-function(del, shp1){
  map<-highchart() %>%
    hc_add_series_map(map=shp1, del,
                      joinBy = "CVEGEO",
                      value = "n",
                      name = "Municipio") %>% 
    hc_tooltip(crosshairs = F, shared = T, borderWidth = 2, useHTML = TRUE, backgroundColor = "#5C666F", fillOpacity = 1, borderRadius=15,
               sort = TRUE, style = list(align = "center",color = "#EEF2F5", fontFamily = "Titillium Web"),
               headerFormat ="",
               pointFormat = "<b style='font-style: italic; font-size:15px;'>{point.Municipios}</b><hr style='margin: 4px 0px 0px 5px;'> Total de incidentes: <b>{point.value:,.0f}</b>",
               footerFormat='</table>', valueDecimals= 0, useHTML = TRUE) %>% 
    hc_mapNavigation(enabled = TRUE) %>%
    hc_colorAxis(minColor = "#d9ebba", maxColor = "#000022") %>%
    hc_plotOptions(
      series = list(  borderColor = "#fff", borderWidth = 2)) 
return(map)}