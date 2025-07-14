limpia_del<-function(delitos){
  delitos<-delitos %>% 
    pivot_longer(cols =c(Enero:Diciembre),
                 names_to = c("mes"),
                 values_to = "delitos") %>% 
    mutate(mes=case_when(mes=="Enero"~"01-01",
                         mes=="Febrero"~"02-01",
                         mes=="Marzo"~"03-01",
                         mes=="Abril"~"04-01",
                         mes=="Mayo"~"05-01",
                         mes=="Junio"~"06-01",
                         mes=="Julio"~"07-01",
                         mes=="Agosto"~"08-01",
                         mes=="Septiembre"~"09-01",
                         mes=="Octubre"~"10-01",
                         mes=="Noviembre"~"11-01",
                         mes=="Diciembre"~"12-01")) %>% 
    mutate(mes=as.Date(paste(años, mes, sep="-")))   
  return(delitos)}

des_anos<-function(delitos){
  delitos<-delitos %>% group_by(Modalidad, años) %>% summarise(n=mean(delitos, na.rm=T)) %>% arrange(desc(n)) %>% 
    reshape2::dcast(Modalidad~años, value.var = "n", sum) %>% rowwise() %>%
    mutate(comp=comparativo(`2025`, `2024`)) %>% mutate(comp1=comparativo(`2024`, `2023`)) %>% 
    mutate(`2024`=accounting(`2024`, digits = 1L)) %>% 
    mutate(`2023`=accounting(`2023`, digits = 1L)) %>% 
    mutate(`2025`=accounting(`2025`, digits = 1L)) %>% 
    arrange(desc(`2025`))
  delitos=1:nrow(delitos) %>% data.frame()%>% bind_cols(delitos)
  names(delitos)[1]<-"Rank"
  delitos1<-delitos %>% head(13) 
  delitos<- bind_rows(delitos1,delitos[delitos$Modalidad=="Extorsi\xf3n",] ) %>% 
    mutate(Modalidad= case_when(Modalidad=="Da\xf1o a la propiedad"~"Daño a la propiedad",
                                Modalidad=="Otros delitos del Fuero Com\xfan"~"Otros delitos del Fuero Común",
                                Modalidad=="Extorsi\xf3n"~"Extorsión",
                                .default = as.character(Modalidad))) %>% 
  mutate(Modalidad=case_when(Modalidad=="Amenazas"~ "<b>Amenazas</b>",
                             Modalidad=="Robo de motocicleta Sin violencia"~"<b>Robo de motocicleta Sin violencia</b>",
                             Modalidad=="Robo de coche de 4 ruedas Con violencia"~"<b>Robo de coche de 4 ruedas Sin violencia</b>" ,
                             Modalidad=="Robo de coche de 4 ruedas Sin violencia"~"<b>Robo de coche de 4 ruedas Sin violencia</b>" , 
                             Modalidad=="Robo de motocicleta Con violencia"~"<b>Robo de motocicleta Con violencia</b>" ,
                             Modalidad=="Extorsión"~"<b>Extorsión</b>", .default = as.character(Modalidad)))
  names(delitos)[c(6:7)]<-c("Var año pasado", "Var periodo anterior")
  return(delitos)}

comparativo<-function(val_act, val_ant){
  if(val_act>0 & val_ant>0){
  com<-round(100*((val_act-val_ant)/val_ant),1)
  }else{com=NA}
  return(com)}

arregla_tabla<-function(tabla){
  tabla<-formattable(tabla, 
              align = c("l","l","r", "r", "r", "r", "r"),
              list(`Municipio` = formatter("span", style = ~ style(color = "grey", font.weight = "bold", fontFamily = "Sans-serif")), 
                   `2023` = color_bar("#d9ebba"),
                   `2024` = color_bar("#d9ebba"),
                   `2025` = color_bar("#d9ebba"),
                   `Var año pasado` = formatter("span", 
                                                style = ~ style(color = ifelse(`Var año pasado` <0, "green", "red")),                                    
                                                ~ icontext(sapply(`Var año pasado`, function(x) if (x <= 0) "chevron-down" else if (x > 0) "chevron-up" else ""), `Var año pasado`)),
                   `Var periodo anterior` = formatter("span", 
                                                      style = ~ style(color = ifelse(`Var periodo anterior` <0, "green", "red")),         
                                                      ~ icontext(sapply(`Var periodo anterior`, function(x) if (x <= 0) "chevron-down" else if (x > 0) "chevron-up" else ""), `Var periodo anterior`))),
              table.attr = 'style="font-size: 18px; line-height = 2em; ";\"')
return(tabla)}

suma_del<-function(delitos){
  delitos<-delitos %>%
    filter(años == "2024") %>%
    summarise(total_delitos = sum(delitos, na.rm = TRUE)) %>%
    pull(total_delitos) %>% format(big.mark=",")
return(delitos)}

