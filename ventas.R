library(highcharter)
library(dplyr)
library(readxl)
library(rlang)
library(gt)
library(lubridate)
library(stringr)
library(tidyr)

venta<- read_excel("~/Downloads/F100_venta por hora.xlsx", 
                                  sheet = "Hoja3")
names(venta)<-c(venta[1,])
venta<-venta[2: nrow(venta), c(-3)]
venta<-venta %>% 
  mutate(`Día natural`=as.Date(`Día natural`, format = "%d.%m.%Y"), 
       MXN=as.numeric(MXN),
       UN=as.numeric(UN),
       `NA`=as.numeric(`NA`), 
      `Hora de venta`=ifelse(`Hora de venta`=='#', 0, `Hora de venta`),
      des=case_when(`Día natural`>=as.Date("2025-06-09") & `Día natural`<=as.Date("2025-06-15")~ "Antes",
                    `Día natural`>=as.Date("2025-06-16") & `Día natural`<=as.Date("2025-06-22")~ "S1",
                    `Día natural`>=as.Date("2025-06-23") & `Día natural`<=as.Date("2025-06-29")~ "S2"),
      `Hora de venta`=ifelse(nchar(`Hora de venta`)==1, paste0('0', `Hora de venta`), `Hora de venta`),
      `Hora de venta`=hm(paste0(`Hora de venta`, ":00"))) 

resul0<-venta %>%
  filter(des=="Antes") %>% 
  group_by(`Hora de venta`) %>% 
  summarise(clientes=sum(`NA`, na.rm = T),
            monto=sum(MXN, na.rm = T),
            prod=sum(UN,na.rm = T)) %>% 
  mutate(des="Antes")


resul1<-venta %>%
  filter(des=="S1") %>% 
  group_by(`Hora de venta`) %>% 
  summarise(clientes=sum(`NA`, na.rm = T),
            monto=sum(MXN, na.rm = T),
            prod=sum(UN,na.rm = T)) %>% 
  mutate(des="S1")

resul2<-venta %>%
  filter(des=="S2") %>% 
  group_by(`Hora de venta`) %>% 
  summarise(clientes=sum(`NA`, na.rm = T),
            monto=sum(MXN, na.rm = T),
            prod=sum(UN,na.rm = T)) %>% 
  mutate(des="S2")


tabla<-resul0 %>% rbind(resul1) %>% rbind(resul2) %>% 
  mutate(`Hora de venta`=  as.numeric(sub("H.*", "", `Hora de venta`)) ) 
tabla[is.na(tabla)]<-0


grafica_linea<-function(tabla, labely, sel0){#sel2="clientes2"; sel1="clientes1";labely="clientes"
# paste0(sel2)
# paste0(sel1)  
  colors=c("#0786dc", "#ebc334", "#a3100e")
  highchart() %>%
  hc_chart(type = "line") %>%
    hc_add_series(tabla,  type="line", hcaes(`Hora de venta`, y= !!sym(sel0), group=des), color=colors) %>% 
  hc_yAxis(gridLineWidth = 0, title = list(enabled = T, text = labely,
                                           style = list(color = "#5C666F", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web", fontSize = "1.2em")), 
           stackLabels = list(color = "#5C666F", y=10, fontWeight = "bold", enabled = TRUE),
           labels = list(enabled = TRUE, rotation=0, style = list( fontFamily = "Titillium Web", color =  "#5C666F", fontSize = "1.1em"))) %>%
    hc_xAxis(
         title= list(text="Hora", style = list(color = "#5C666F", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web", fontSize = "1.2em")),
             labels = list(rotation=0, style = list( fontFamily = "Titillium Web", color =  "#5C666F",  fontSize = "1.1em")),
             plotBands = list(
               list(
                 from = 11, to = 13,
                 color = "#f2f9ff",
                 label = list(text = "11-13H", style = list( fontFamily = "Titillium Web", color = "#5C666F")) ),
               list(
                 from = 18, to = 20,
                 color = "#f2f9ff",
                 label = list(text = "18-20H", style = list( fontFamily = "Titillium Web", color = "#5C666F")) )) ) %>%     
    hc_chart(zoomType = "x") %>%  
    hc_legend( align = "left", verticalAlign = "bottom", y = 10, floating = TRUE,
             enabled=T, borderWidth = 0, borderRadius = 10, borderColor = "#EEF2F5", 
              itemHoverStyle = list(color = "#1E242B", fontSize = "13px"),
              itemStyle = list(fontFamily = "Titillium Web", color = "#5C666F", fontWeight="normal")) %>%
    hc_tooltip(crosshairs = F, shared = T, useHTML = TRUE, borderWidth = 1, backgroundColor = "#1E242B", fillOpacity = 0.85, borderRadius=15,
               sort = TRUE, style = list(align = "center",fontWeight="400", color = "#EEF2F5", fontFamily = "Titillium Web"),
               headerFormat = paste("Hora", ": <b>{point.key}</b><hr hr style='margin: 4px 0px 0px 5px;'>"),
               footerFormat='<table>', valueDecimals= 0, useHTML = TRUE)
} 



grafica_linea_tres<-function(tabla, labely){#sel2="clientes2"; sel1="clientes1";labely="clientes"
  colors=c("#0786dc", "#ebc334", "#a3100e")
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_add_series(tabla, type="line", hcaes(hr, y=n, group=un ), color=colors) %>% 
    hc_yAxis(gridLineWidth = 0, title = list(enabled = T, text = labely,
                                             style = list(color = "#5C666F", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web", fontSize = "1.2em")), 
             stackLabels = list(color = "#5C666F", y=10, fontWeight = "bold", enabled = TRUE),
             labels = list(enabled = TRUE, rotation=0, format = "{value}%",
                           style = list( fontFamily = "Titillium Web", color =  "#5C666F", fontSize = "1.1em"))) %>%
    hc_xAxis(
      title= list(text="Hora", style = list(color = "#5C666F", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web", fontSize = "1.2em")),
      labels = list(rotation=0, style = list( fontFamily = "Titillium Web", color =  "#5C666F",  fontSize = "1.1em")),
      plotBands = list(
        list(
          from = 11, to = 13,
          color = "#f2f9ff",
          label = list(text = "11-13H", style = list( fontFamily = "Titillium Web", color = "#5C666F")) ),
        list(
          from = 18, to = 20,
          color = "#f2f9ff",
          label = list(text = "18-20H", style = list( fontFamily = "Titillium Web", color = "#5C666F")) )) ) %>%     
    hc_chart(zoomType = "x") %>%  
    hc_legend( align = "left", verticalAlign = "bottom", y = 10, floating = TRUE,
               enabled=T, borderWidth = 0, borderRadius = 10, borderColor = "#EEF2F5", 
               itemHoverStyle = list(color = "#1E242B", fontSize = "13px"),
               itemStyle = list(fontFamily = "Titillium Web", color = "#5C666F", fontWeight="normal")) %>%
    hc_tooltip(crosshairs = F, shared = T, useHTML = TRUE, borderWidth = 1, backgroundColor = "#1E242B", fillOpacity = 0.85, borderRadius=15,
               sort = TRUE, style = list(align = "center",fontWeight="400", color = "#EEF2F5", fontFamily = "Titillium Web"),
               headerFormat = paste("Hora", ": <b>{point.key}</b><hr hr style='margin: 4px 0px 0px 5px;'>"),
               footerFormat='<table>', valueDecimals= 2, valueSuffix = "%", useHTML = TRUE)
} 


  ## grafica de productos
  graf1<-grafica_linea(tabla, "Clientes", "clientes")
  
  ## grafica de venta
  graf2<-grafica_linea(tabla, "Monto de venta", "monto")
  
  ## grafica de clientes
  graf3<-grafica_linea(tabla, "Productos", "prod")
  
  
  gf1<- tabla %>% 
             filter(des=="Antes") %>% 
             mutate(clientes=round(100*clientes/sum(clientes), 2),
                    monto=round(100*monto/sum(monto), 2), 
                    prod=round(100*prod/sum(prod), 2)) %>% 
             grafica_linea_tres("Antes del Proceso","clientes", "monto", "prod")
  
  
  gf2<- tabla %>% 
    filter(des=="S1") %>% 
    mutate(clientes=round(100*clientes/sum(clientes), 2),
           monto=round(100*monto/sum(monto), 2), 
           prod=round(100*prod/sum(prod), 2)) %>% 
    grafica_linea_tres("Semana 1","clientes", "monto", "prod")
  
  
  gf3<- tabla %>% 
    filter(des=="S2") %>% 
    mutate(clientes=round(100*clientes/sum(clientes), 2),
           monto=round(100*monto/sum(monto), 2), 
           prod=round(100*prod/sum(prod), 2)) %>% 
    grafica_linea_tres("Semana 2","clientes", "monto", "prod")
  
  
  resul1<-resul1 %>% mutate(producto_cliente= round(prod1/clientes1,1),
                    monto_cliente=round(monto1/clientes1,1)) %>% 
    mutate(`Hora de venta`=  as.numeric(sub("H.*", "", `Hora de venta`)) ) 
  resul1[is.na(resul1)]<-0
  
  resul2<-resul2 %>% mutate(producto_cliente= round(prod2/clientes2,1),
                            monto_cliente=round(monto2/clientes2,1)) %>% 
    mutate(`Hora de venta`=  as.numeric(sub("H.*", "", `Hora de venta`)) ) 
  resul2[is.na(resul2)]<-0
  

  resul1<- resul1 %>%
    mutate(
      eficiencia=scale(producto_cliente) +
        scale(monto_cliente) +
        scale(clientes1) )
   
  resul2<- resul2 %>%
    mutate(
      eficiencia=scale(producto_cliente) +
        scale(monto_cliente) +
        scale(clientes2) ) 
    
   
  highchart() %>% 
  hc_chart(type = "line") %>%
    hc_add_series(resul1, name="Con proceso", type = "line", color="#0786dc",  hcaes(`Hora de venta`, eficiencia)) %>%
    hc_add_series(resul2, name="Sin proceso", type = "line", color="#a3100e", hcaes(x = `Hora de venta`, y = eficiencia)) %>%
    hc_yAxis(gridLineWidth = 0, title = list(enabled = T, text = "Productos por cliente",
                                             style = list(color = "#5C666F", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web", fontSize = "1.2em")), 
             stackLabels = list(color = "#5C666F", y=10, fontWeight = "bold", enabled = TRUE),
             labels = list(enabled = TRUE, rotation=0, style = list( fontFamily = "Titillium Web", color =  "#5C666F", fontSize = "1.1em"))) %>%
    hc_xAxis(title= list(text="Hora", style = list(color = "#5C666F", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web", fontSize = "1.2em")),
             labels = list(rotation=0, style = list( fontFamily = "Titillium Web", color =  "#5C666F",  fontSize = "1.1em")),
             plotBands = list(
               list(
                 from = 11,
                 to = 13,
                 color = "#f2f9ff",
                 label = list(text = "11-13H", style = list( fontFamily = "Titillium Web", color = "#5C666F")) ),
               list(
                 from = 18,
                 to = 20,
                 color = "#f2f9ff",
                 label = list(text = "18-20H", style = list( fontFamily = "Titillium Web", color = "#5C666F")) )) ) %>%     
    hc_chart(zoomType = "x") %>%  
    # hc_title(text= titulo, margin = 7, align = "center",
    #          style = list(color = "#5C666F", useHTML = TRUE,  fontFamily = "Titillium Web"))%>%
    hc_legend( align = "left", verticalAlign = "bottom", y = 10, floating = TRUE,
               enabled=T, borderWidth = 0, borderRadius = 10, borderColor = "#EEF2F5", 
               itemHoverStyle = list(color = "#1E242B", fontSize = "13px"),
               itemStyle = list(fontFamily = "Titillium Web", color = "#5C666F", fontWeight="normal")) %>%
    hc_tooltip(crosshairs = F, shared = T, useHTML = TRUE, borderWidth = 1, backgroundColor = "#1E242B", fillOpacity = 0.85, borderRadius=15,
               sort = TRUE, style = list(align = "center",fontWeight="400", color = "#EEF2F5", fontFamily = "Titillium Web"),
               headerFormat = paste("Hora", ": <b>{point.key}</b><hr hr style='margin: 4px 0px 0px 5px;'>"),
               footerFormat='<table>', valueDecimals= 0, useHTML = TRUE)
  
  
  limpia_csv<-function(tabla){
    tabla<- tabla %>% 
      mutate(fecha=as.Date(substr(start, 1,10)),
             hr=as.numeric(substr(start, 12,13)),
             human=as.numeric(human),
             des=case_when(fecha>=as.Date("2025-06-09") & fecha<=as.Date("2025-06-15")~ "Antes",
                           fecha>=as.Date("2025-06-16") & fecha<=as.Date("2025-06-22")~ "S1",
                           fecha>=as.Date("2025-06-23") & fecha<=as.Date("2025-06-29")~ "S2"),
             hrs_sel=case_when(hr>=11 & hr<=13 ~ "Día (11-13H)",
                               hr>=18 & hr<=20 ~ "Tarde (18-20H)")) %>% 
             filter(is.na(des)==F & is.na(hrs_sel)==F) %>% 
             group_by(des, hrs_sel) %>% summarise(n=sum(human)) %>% 
          reshape2::dcast(hrs_sel~ des, value.var = 'n', sum) 
          tot<-c("Acumulado", sum(tabla$Antes), sum(tabla$S1), sum(tabla$S2))
          tabla<-tabla %>% 
            rbind(tot) %>% 
            mutate(
              Antes=as.numeric(Antes), S1=as.numeric(S1), S2=as.numeric(S2), 
              var1= paste0(round(100*( 1-(Antes/S1)), 1), '%'),
              var2= paste0(round(100*( 1-(Antes/S2)), 1), '%'),
              Antes=format(Antes, big.mark=","),
              S1=format(S1, big.mark=","),
              S2=format(S2, big.mark=",")) %>% 
            rename(Horario=hrs_sel, Var1=var1, Var2=var2)
     return(tabla)}  

  
  limpia_grafica_csv<-function(tabla){
    tabla<- tabla %>% 
      mutate(fecha=as.Date(substr(start, 1,10)),
             hr=as.numeric(substr(start, 12,13)),
             human=as.numeric(human),
             des=case_when(fecha>=as.Date("2025-06-09") & fecha<=as.Date("2025-06-15")~ "Antes",
                           fecha>=as.Date("2025-06-16") & fecha<=as.Date("2025-06-22")~ "S1",
                           fecha>=as.Date("2025-06-23") & fecha<=as.Date("2025-06-29")~ "S2"),
             hrs_sel=case_when(hr>=11 & hr<=13 ~ "Día",
                               hr>=18 & hr<=20 ~ "Tarde")) %>%
      filter(is.na(des)==F) %>% 
      group_by(des,hr) %>% summarise(n=sum(human)) %>% 
    return(tabla)}  
  
    
salida <- read.csv("/Users/javierruizrubio/Downloads/Conteo salida-Personas 29 ABRIL al 08 JULIO 2025.csv") %>% 
        limpia_csv() %>% formato("Salida de clientes")

salidas <- read.csv("/Users/javierruizrubio/Downloads/Conteo salida-Personas 29 ABRIL al 08 JULIO 2025.csv") %>% 
          limpia_grafica_csv() %>% grafica_0("Salida de clientes")

entrada <- read.csv("/Users/javierruizrubio/Downloads/Conteo entrada-Personas 29 ABRIL al 08 JULIO 2025.csv") %>% 
         limpia_csv() %>% formato("Entrada de clientes")

entradas <- read.csv("/Users/javierruizrubio/Downloads/Conteo entrada-Personas 29 ABRIL al 08 JULIO 2025.csv") %>% 
      limpia_grafica_csv() %>%  grafica_0("Entrada de clientes")

unifila <- read.csv("/Users/javierruizrubio/Downloads/Permanencia Area-Unifila-30 ABRIL al 03 JULIO 2025.csv") %>% 
       rename(human=max.human) %>% limpia_csv() %>% formato("Unifila")
  
unifilas <- read.csv("/Users/javierruizrubio/Downloads/Permanencia Area-Unifila-30 ABRIL al 03 JULIO 2025.csv") %>% 
  rename(human=max.human) %>% limpia_grafica_csv() %>%  grafica_0("Unifila")

union<-
read.csv("/Users/javierruizrubio/Downloads/Conteo salida-Personas 29 ABRIL al 08 JULIO 2025.csv") %>% 
  limpia_grafica_csv() %>% mutate(un="Salida") %>% 
rbind(  
read.csv("/Users/javierruizrubio/Downloads/Conteo entrada-Personas 29 ABRIL al 08 JULIO 2025.csv") %>% 
  limpia_grafica_csv() %>% mutate(un="Entrada")) %>% 
rbind(  
read.csv("/Users/javierruizrubio/Downloads/Permanencia Area-Unifila-30 ABRIL al 03 JULIO 2025.csv") %>% 
  rename(human=max.human) %>% limpia_grafica_csv() %>% mutate(un="Unifila") )

union %>%
  filter(des=="Antes") %>% 
  group_by(un) %>% 
  mutate(suma=sum(n), 
         n=round(100*(n/suma),2)) %>% 
  grafica_linea_tres("Antes del proceso")


union %>%
  filter(des=="S1") %>% 
  group_by(un) %>% 
  mutate(suma=sum(n), 
         n=round(100*(n/suma),2)) %>% 
  grafica_linea_tres("Semana 1")


union %>%
  filter(des=="S2") %>% 
  group_by(un) %>% 
  mutate(suma=sum(n), 
         n=round(100*(n/suma),2)) %>% 
  grafica_linea_tres("Semana 2")


venta<- read_excel("~/Downloads/F100_venta por hora.xlsx", 
                   sheet = "Hoja3")

names(venta)<-c(venta[1,])
venta<-venta[2: nrow(venta), c(-3)]

limpia_xls<-function(ventas,sel){
names(venta)[sel]<-"human"
tabla<-venta %>% 
  mutate(fecha=as.Date(`Día natural`, format = "%d.%m.%Y"), 
         human=as.numeric(human),
         hr=ifelse(`Hora de venta`=='#', 0, `Hora de venta`),
         des=case_when(fecha>=as.Date("2025-06-09") & fecha<=as.Date("2025-06-15")~ "Antes",
                       fecha>=as.Date("2025-06-16") & fecha<=as.Date("2025-06-22")~ "S1",
                       fecha>=as.Date("2025-06-23") & fecha<=as.Date("2025-06-29")~ "S2"),
         hr=as.numeric(hr),
         hrs_sel=case_when(hr>=11 & hr<=13 ~ "Día (11-13H)",
                           hr>=18 & hr<=20 ~ "Tarde (18-20H)")) %>% 
  filter(is.na(des)==F & is.na(hrs_sel)==F) %>%
  group_by(des, hrs_sel) %>% summarise(n=sum(human, na.rm=T)) %>% 
  reshape2::dcast(hrs_sel~ des, value.var = 'n', sum) 
  #tidyr::pivot_wider(names_from=des, values_from = c('productos', 'monto', 'clientes')) 
tot<-c("Acumulado", sum(tabla$Antes, na.rm=T), sum(tabla$S1, na.rm=T), sum(tabla$S2, na.rm=T))
tabla<-tabla %>% 
  rbind(tot) %>% 
  mutate(
    Antes=as.numeric(Antes), S1=as.numeric(S1), S2=as.numeric(S2), 
    var1= paste0(round(100*( 1-(Antes/S1)), 1), '%'),
    var2= paste0(round(100*( 1-(Antes/S2)), 1), '%'),
    Antes=format(Antes, big.mark=","),
    S1=format(S1, big.mark=","),
    S2=format(S2, big.mark=",")) %>% 
  rename(Horario=hrs_sel, Var1=var1, Var2=var2)
}

library(tidyr)

limpia_grafica_xls<-function(venta, sel){
  names(venta)[sel]<-"human"
  tabla<- venta %>% 
    mutate(fecha=as.Date(`Día natural`, format = "%d.%m.%Y"), 
           human=as.numeric(human),
           hr=ifelse(`Hora de venta`=='#', 0, `Hora de venta`),
           des=case_when(fecha>=as.Date("2025-06-09") & fecha<=as.Date("2025-06-15")~ "Antes",
                         fecha>=as.Date("2025-06-16") & fecha<=as.Date("2025-06-22")~ "S1",
                         fecha>=as.Date("2025-06-23") & fecha<=as.Date("2025-06-29")~ "S2"),
           hr=as.numeric(hr),
           hrs_sel=case_when(hr>=11 & hr<=13 ~ "Día (11-13H)",
                             hr>=18 & hr<=20 ~ "Tarde (18-20H)")) %>%
    filter(is.na(des)==F) %>% 
    group_by(des,hr) %>% summarise(n=sum(human)) %>% 
return(tabla)}  


productos <-limpia_xls(venta, 4) %>% formato("Productos")#33342d
product<-limpia_grafica_xls(venta, 4)%>% grafica_0("Productos")
monto<-limpia_xls(venta, 5) %>% formato("Monto")
mont<-limpia_grafica_xls(venta, 5)%>% grafica_0("Monto")
clientes<-limpia_xls(venta, 6) %>% formato("Clientes")
client<-limpia_grafica_xls(venta, 6)%>% grafica_0("Clientes")



formato<-function(df, titulo){
gt(df) %>% 
  tab_header(title=titulo, subtitle = '') %>%
  #tab_header(title = html(paste0("<div style='background-color: #32AC90; padding:5px; margin:0px; border-radius: 9px 9px 0px 0px;'>", paste(titulo),"</div>")), subtitle = subtitulo) %>%
  tab_style(style = list(cell_text( align = "center", color="#333", weight=600, font = "Titillium Web")), locations =  cells_column_labels(names(df)) ) %>%
  tab_style(style = list( cell_borders(color = c("#fff"), weight = px(2)), cell_text(color="#333", align = "center", weight = "bold", font = "Titillium Web", size=px(18))), locations = cells_body( columns = everything())) %>%
  tab_style(style = list(cell_borders(color = c("#fff"), weight = px(0)), cell_fill(color="#fff"), cell_text(color="#32AC90", align = "center", font = "Titillium Web")), locations = cells_source_notes()) %>%
  tab_style(style = cell_borders(sides = "bottom", color = "#fff", weight = px(0)), locations = cells_body()) %>%
  tab_style(style = list(cell_fill(color="#fff"), cell_borders(color = c("#fff"), weight = px(0)), cell_text( weight = "bold", color="#333", align = "center", font = "Titillium Web", size=px(23))), locations = cells_title("title") ) %>%
  tab_style(style = list(cell_fill(color="#fff"), cell_borders(color = c("#fff"), weight = px(0)), cell_text( color="#333", align = "center", font = "Titillium Web", size=px(21))), locations = cells_title("subtitle") ) %>%
  tab_style(style = list(cell_fill(color="#fff"), cell_borders(color = c("#32AC90"), sides = "top", weight = px(2)), cell_text( weight = "bold", color="#333", size=px(18), align = "center", font = "Titillium Web")), locations =  cells_column_labels(names(df)) ) %>%
  tab_style( style = list(cell_fill(color="#fff"), cell_text(color = "#333",  weight = "bold",align = "center", font = "Titillium Web")), locations = cells_body(columns=c(1))) %>% 
  tab_style( style = list(cell_fill(color = "#82c77b"), cell_text(color = "#333")), locations = cells_body(columns=c(2:(ncol(df))), rows=c(nrow(df) ))) %>% 
  tab_style( style = list(cell_fill(color = "#ddd"), cell_text(color = "#333")), locations = cells_body(columns=c(2:(ncol(df))), rows=c(1:2))) %>% 
  tab_style( style = list(cell_borders(color = c("#32AC90"), sides = "bottom", weight = px(2))), locations = cells_body(columns=c(1:(ncol(df))), rows=c(nrow(df) ))) %>% 
  #tab_style( style = list(cell_fill(color = "#8fdaff"), cell_text(color = "#ddd")), locations = cells_body(columns=c(2:(ncol(df)-1)), rows=c(nrow(df)))) %>% 
  tab_style( style = list(cell_fill(color = "#8fdaff"), cell_text(color = "#333")), locations = cells_body(columns=c(ncol(df)), rows=c(1:(nrow(df))))) %>%  
  tab_style( style = list(cell_fill(color = "#8fdaff"), cell_text(color = "#333")), locations = cells_body(columns=c(ncol(df)-1), rows=c(1:(nrow(df))))) %>%  
  tab_options(
    heading.border.bottom.color = "#fff",
    heading.border.lr.color ="#fff",
    table.border.top.color = "#fff",
    table.border.bottom.style = "#fff",
    column_labels.border.bottom.color = "#fff",
    table.border.bottom.color ="#fff",
    table.border.left.color ="#fff",
    table.border.right.color ="#fff",
    table.border.left.style ="#fff",
    table.border.right.style ="#fff",
    table.background.color = "#fff",
    page.margin.top = "0px",
    page.margin.bottom = "0px")}

grafica_0<-function(tabla, title){
  colors=c("#0786dc", "#a3100e", "#e0c31b")
  
  highchart() %>% 
    hc_chart(type = "line") %>%
    hc_add_series(tabla,  type = "line", color=colors,  hcaes(x=hr, y=n, group=des)) %>%
    hc_yAxis(gridLineWidth = 0, title = list(enabled = T, text = title,
                                             style = list(color = "#5C666F", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web", fontSize = "1.2em")), 
             stackLabels = list(color = "#5C666F", y=10, fontWeight = "bold", enabled = TRUE),
             labels = list(enabled = TRUE, rotation=0, style = list( fontFamily = "Titillium Web", color =  "#5C666F", fontSize = "1.1em"))) %>%
    hc_xAxis(title= list(text="Hora", style = list(color = "#5C666F", useHTML = TRUE, fontWeight = "bold", fontFamily = "Titillium Web", fontSize = "1.2em")),
             labels = list(rotation=0, style = list( fontFamily = "Titillium Web", color =  "#5C666F",  fontSize = "1.1em")),
             plotBands = list(
               list(
                 from = 11,
                 to = 13,
                 color = "#f2f9ff",
                 label = list(text = "11-13H", style = list( fontFamily = "Titillium Web", color = "#5C666F")) ),
               list(
                 from = 18,
                 to = 20,
                 color = "#f2f9ff",
                 label = list(text = "18-20H", style = list( fontFamily = "Titillium Web", color = "#5C666F")) )) ) %>%     
    hc_chart(zoomType = "x") %>%  
    # hc_title(text= titulo, margin = 7, align = "center",
    #          style = list(color = "#5C666F", useHTML = TRUE,  fontFamily = "Titillium Web"))%>%
    hc_legend( align = "left", verticalAlign = "bottom", y = 10, floating = TRUE,
               enabled=T, borderWidth = 0, borderRadius = 10, borderColor = "#EEF2F5", 
               itemHoverStyle = list(color = "#1E242B", fontSize = "13px"),
               itemStyle = list(fontFamily = "Titillium Web", color = "#5C666F", fontWeight="normal")) %>%
    hc_tooltip(crosshairs = F, shared = T, useHTML = TRUE, borderWidth = 1, backgroundColor = "#1E242B", fillOpacity = 0.85, borderRadius=15,
               sort = TRUE, style = list(align = "center",fontWeight="400", color = "#EEF2F5", fontFamily = "Titillium Web"),
               headerFormat = paste("Hora", ": <b>{point.key}</b><hr hr style='margin: 4px 0px 0px 5px;'>"),
               footerFormat='<table>', valueDecimals= 0, useHTML = TRUE)
}




venta_hra<-read_excel("/Users/javierruizrubio/Downloads/F100_venta JUNIO.xlsx",  sheet = "Hoja3")

names(venta_hra)<-c(venta_hra[1,])
venta_hra<-venta_hra[2:nrow(venta_hra),]
names(venta_hra)[3]<-"nombre_centro"

pba<-venta_hra %>% 
  mutate(`Día natural`=as.Date(`Día natural`, format = "%d.%m.%Y"), 
         MXN=as.numeric(MXN),
         UN=as.numeric(UN),
         `NA`=as.numeric(`NA`), 
         fecha=ymd_hms(paste(`Día natural`, `Fin cronom.(hora)`)),
         `Fin cronom.(hora)`=hms(`Fin cronom.(hora)`),
         des=case_when(fecha>=as.Date("2025-06-09") & fecha<=as.Date("2025-06-15")~ "Antes",
                       fecha>=as.Date("2025-06-16") & fecha<=as.Date("2025-06-22")~ "S1",
                       fecha>=as.Date("2025-06-23") & fecha<=as.Date("2025-06-29")~ "S2"),
         hr=as.numeric(sub("H.*", "",`Fin cronom.(hora)`)),
         hrs_sel=case_when(hr>=11 & hr<=13 ~ "Día (11-13H)",
                           hr>=18 & hr<=20 ~ "Tarde (18-20H)")) %>% 
  filter(is.na(des)==F & is.na(hrs_sel)==F) %>% 
  group_by(Centro, `Número de TPV`) %>% 
  mutate(tiempo_des = lag(fecha),
         tiempo_dif_new = round(as.numeric(difftime(fecha, tiempo_des, units = "secs"))/60,2)) %>% 
  filter(tiempo_dif_new>1 & tiempo_dif_new<30) 


as<-pba %>%   
group_by(hrs_sel, des) %>%
  summarise(atenciones=n(),
            Tiempo=round(mean(tiempo_dif_new),2),
            productos=round(mean(UN),2),
            monto=round(mean(MXN),2)) 
  

antes <- as[as$des == "Antes", 3:6]

comparar_con_antes <- function(des_name) {#des_name="S1"
  actual <- as[as$des == des_name, 3:6]
  diferencia <- actual - antes
  porcentaje <- data.frame(des=c(paste0(des_name, " - Antes (11-13H)"), paste0(des_name, " - Antes (18-20H)") )) %>% 
                cbind(
                round((diferencia/antes)*100, 2)) #%>% 
    # mutate(#across(-des, ~paste0(.x, "%")),
    #       across(-des, ~as.numeric(str_remove(.x, "%"))))
}

#"Comparación: Día (11-13H)"
#"Comparación: Tarde (18-20H)"
tabla_comparacion<-function(df, tit, esta){
  if(esta=="compa"){
    names(df)<-c("Comparación", "Atenciones", "Tiempo (Prom)", "Productos (Prom)", "Monto (Prom)")
  gts<- gt(df) %>%
      tab_style(
        style = cell_text(color = "#a30803"),
        locations = cells_body( columns = c("Atenciones"), rows = df$Atenciones < 0)) %>%
      tab_style(
        style = cell_text(color = "#186940"),
        locations = cells_body( columns = c("Atenciones"), rows = df$Atenciones > 0)) %>%
      tab_style(
        style = cell_text(color = "#186940"),
        locations = cells_body( columns = "Tiempo (Prom)", rows = df$`Tiempo (Prom)` < 0)) %>%
      tab_style(
        style = cell_text(color = "#a30803"),
        locations = cells_body( columns = c("Tiempo (Prom)"), rows = df$`Tiempo (Prom)` > 0)) %>%
      tab_style(
        style = cell_text(color = "#a30803"),
        locations = cells_body( columns = "Productos (Prom)", rows = df$`Productos (Prom)` < 0)) %>%
      tab_style(
        style = cell_text(color = "#186940"),
        locations = cells_body( columns = c("Productos (Prom)"), rows = df$`Productos (Prom)` > 0)) %>%
      tab_style(
        style = cell_text(color = "#a30803"),
        locations = cells_body( columns = "Monto (Prom)", rows = df$`Monto (Prom)` < 0 ) ) %>% 
      tab_style(
        style = cell_text(color = "#186940"),
        locations = cells_body( columns = c("Monto (Prom)"), rows = df$`Monto (Prom)` > 0)) %>%
      fmt_percent(columns = -Comparación, decimals = 2, scale_values = FALSE) 
  }else{
    names(df)<-c("Semana", "Atenciones", "Tiempo (Prom)", "Productos (Prom)", "Monto (Prom)")
    gts<- gt(df) 
  }
  gts %>% 
    tab_header(title=tit, subtitle = '') %>%
    tab_style(style = list(cell_text( align = "center", color="#333", weight=600, font = "Titillium Web")), locations =  cells_column_labels(names(df)) ) %>%
    tab_style(style = list( cell_borders(color = c("#fff"), weight = px(2)), cell_text( align = "center", weight = "bold", font = "Titillium Web", size=px(18))), locations = cells_body( columns = everything())) %>%
    tab_style(style = list(cell_borders(color = c("#fff"), weight = px(0)), cell_fill(color="#fff"), cell_text(color="#32AC90", align = "center", font = "Titillium Web")), locations = cells_source_notes()) %>%
    tab_style(style = cell_borders(sides = "bottom", color = "#fff", weight = px(0)), locations = cells_body()) %>%
    tab_style(style = list(cell_fill(color="#fff"), cell_borders(color = c("#fff"), weight = px(0)), cell_text( weight = "bold", color="#333", align = "center", font = "Titillium Web", size=px(23))), locations = cells_title("title") ) %>%
    tab_style(style = list(cell_fill(color="#fff"), cell_borders(color = c("#fff"), weight = px(0)), cell_text( color="#333", align = "center", font = "Titillium Web", size=px(21))), locations = cells_title("subtitle") ) %>%
    tab_style(style = list(cell_fill(color="#fff"), cell_borders(color = c("#32AC90"), sides = "top", weight = px(2)), cell_text( weight = "bold", color="#333", size=px(18), align = "center", font = "Titillium Web")), locations =  cells_column_labels(names(df)) ) %>%
    tab_style( style = list(cell_fill(color="#fff"), cell_text(color = "#333",  weight = "bold",align = "center", font = "Titillium Web")), locations = cells_body(columns=c(1))) %>% 
    tab_style( style = list(cell_fill(color = "#ddd")), locations = cells_body(columns=c(2:(ncol(df))), rows=c(1:nrow(df)))) %>% 
    tab_style( style = list(cell_borders(color = c("#32AC90"), sides = "bottom", weight = px(2))), locations = cells_body(columns=c(1:(ncol(df))), rows=c(nrow(df) ))) %>% 
    tab_options(
      heading.border.bottom.color = "#fff",
      heading.border.lr.color ="#fff",
      table.border.top.color = "#fff",
      table.border.bottom.style = "#fff",
      column_labels.border.bottom.color = "#fff",
      table.border.bottom.color ="#fff",
      table.border.left.color ="#fff",
      table.border.right.color ="#fff",
      table.border.left.style ="#fff",
      table.border.right.style ="#fff",
      table.background.color = "#fff",
      page.margin.top = "0px",
      page.margin.bottom = "0px")}

comparar_con_antes("S1") %>% tabla_comparacion("Comparación: Semana 1", "compa")
as[1:3,2:6]  %>% tabla_comparacion("Comparación: Rango (11-13H)", "")
comparar_con_antes("S2") %>% tabla_comparacion("Comparación: Semana 2", "compa")
as[4:6,2:6]  %>% tabla_comparacion("Comparación: Rango (18-20H)", "")




pba %>% group_by(des, hrs_sel) %>% summarise(n=sum(MXN, na.rm=T)) %>% 
  reshape2::dcast(hrs_sel~ des, value.var = 'n', sum) 
#tidyr::pivot_wider(names_from=des, values_from = c('productos', 'monto', 'clientes')) 
tot<-c("Acumulado", sum(tabla$Antes, na.rm=T), sum(tabla$S1, na.rm=T), sum(tabla$S2, na.rm=T))
tabla<-tabla %>% 
  rbind(tot) %>% 
  mutate(
    Antes=as.numeric(Antes), S1=as.numeric(S1), S2=as.numeric(S2), 
    var1= paste0(round(100*( 1-(Antes/S1)), 1), '%'),
    var2= paste0(round(100*( 1-(Antes/S2)), 1), '%'),
    Antes=format(Antes, big.mark=","),
    S1=format(S1, big.mark=","),
    S2=format(S2, big.mark=",")) %>% 
  rename(Horario=hrs_sel, Var1=var1, Var2=var2)


 

