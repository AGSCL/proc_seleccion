library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(plotly)
library(anonymizer)

copiar_nombres <- function(x,row.names=FALSE,col.names=TRUE,dec=",",...) {
  if(class(ungroup(x))[1]=="tbl_df"){
    if(options()$OutDec=="."){
      options(OutDec = dec)
      write.table(format(data.frame(x)),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ".")
      return(x)
    } else {
      options(OutDec = ",")
      write.table(format(data.frame(x)),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ",")
      return(x)    
    }
  } else {
    if(options()$OutDec=="."){
      options(OutDec = dec)
      write.table(format(x),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ".")
      return(x)
    } else {
      options(OutDec = ",")
      write.table(format(x),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ",")
      return(x)       
    }
  }
}  

url <- "https://ddp.usach.cl/sites/depto-personas/files/HISTORIAL%20DE%20PROCESOS%20DE%20SELECCION%202013-2020.xlsx"
destfile <- "HISTORIAL_20DE_20PROCESOS_20DE_20SELECCION_202013_2020.xlsx"
curl::curl_download(url, destfile)
HISTORIAL_20DE_20PROCESOS_20DE_20SELECCION_202013_2020 <- read_excel(destfile)

part_1<-
  HISTORIAL_20DE_20PROCESOS_20DE_20SELECCION_202013_2020 %>% 
  dplyr::rename("cat"="HISTORIAL PROCESOS DE SELECCIÓN  2013-2020") %>% 
  dplyr::mutate(cat=stringr::str_trim(cat)) %>% 
  dplyr::mutate(cat=gsub("\\.$", "", cat)) %>% 
  dplyr::mutate(cat=gsub(";", ":", cat)) %>% 
  dplyr::mutate(cat=gsub("\\.", ":", cat)) %>%
  dplyr::mutate(upper_cnt=ifelse(!grepl("Lugar|Cargo|Fecha|eleccionad",cat),nchar(gsub("[:a-z:]","",cat))/nchar(cat),0)) %>% 
  dplyr::mutate(cat = tolower(cat)) %>% 
  dplyr::mutate(cat=gsub("seleccinado", "seleccionado", cat)) %>%
  dplyr::mutate(cat=gsub("selecciondo", "seleccionado", cat)) %>%
  dplyr::mutate(cat=gsub("estadao", "estado", cat)) %>% 
  dplyr::mutate(cat=gsub("cargdo", "cargo", cat)) %>% 
  dplyr::mutate(cat=gsub("postulacuión", "postulación", cat)) %>% 
  dplyr::mutate(cat=gsub("publciación", "postulación", cat)) %>% 
  dplyr::mutate(cat=gsub("publicación", "postulación", cat)) %>% 
  dplyr::mutate(cat=gsub("seleción", "selección:", cat)) %>% 
  dplyr::mutate(cat=gsub("seleccionadas", "seleccionado", cat)) %>% 
  dplyr::mutate(cat=gsub("seleccionada", "seleccionado", cat)) %>% 
  dplyr::mutate(cat=gsub("seleccionados", "seleccionado", cat)) %>% 
  dplyr::mutate(cat=gsub("selecionada", "seleccionado", cat)) %>% 
  dplyr::mutate(cat=gsub("selecionado", "seleccionado", cat)) %>%
  dplyr::mutate(cat=gsub("seleccionado ", "seleccionado", cat)) %>%
  dplyr::mutate(cat=gsub("fecha de postulación ", "fecha", cat)) %>% 
  dplyr::mutate(cat=gsub("fecha de postulación", "fecha", cat)) %>% 
  dplyr::mutate(cat=gsub("fecha postulación", "fecha", cat)) %>% 
  dplyr::mutate(cat=gsub("fecha de postulación ", "fecha", cat)) %>% 
  dplyr::mutate(cat=gsub("fecha de postulación", "fecha", cat)) %>% 
  dplyr::mutate(cat=gsub("unidad implementadora", "lugar", cat)) %>%
  dplyr::mutate(cat=gsub("proceso de selección ", "proceso de selección", cat)) %>%
  dplyr::mutate(cat=gsub("selección externo", "selección: externo", cat)) %>% 
  dplyr::mutate(cat=gsub("procesos", "proceso", cat)) %>% 
  dplyr::mutate(cat=gsub("lugar ", "lugar", cat)) %>% 
  dplyr::mutate(cat=gsub("estado ", "estado", cat)) %>% 
  
  dplyr::mutate(cat=dplyr::case_when(grepl("^ver",cat)~NA_character_,T~cat)) %>% 
  dplyr::mutate(cat=dplyr::case_when(nchar(cat)<=3~NA_character_,T~cat)) %>% 
  dplyr::mutate(cat_fwd=dplyr::lag(cat))


part_2<-
  data.table::data.table(part_1, keep.rownames = T)%>% 
#no tiene lespacio después para el siguiente proceso de selección
  #dplyr::mutate(rn=row_number()) %>%   
  dplyr::mutate(rn=as.numeric(rn)) %>% 
  dplyr::mutate(rn2=ifelse(grepl("mayerley pacheco",cat),rn,NA_real_)) %>% 
  filter(dplyr::case_when(is.na(cat_fwd) & is.na(cat)~F, #When y == "", x > 3
                   T ~ T) #Otherwise, x < 3
  ) %>% 
  dplyr::mutate(rn=row_number()) %>% 
  dplyr::mutate(rn2=ifelse(grepl("mayerley pacheco",cat),rn,NA_real_)) %>% 
  tibble::add_row(tibble::tibble_row(cat = NA_character_, upper_cnt=0, cat_fwd=NA_character_,rn=NA_real_,rn2=NA_real_),.before=max(.$rn2,na.rm=T)+1) %>%
  
  dplyr::mutate(cat_lead=dplyr::lag(cat)) %>%
  dplyr::mutate(cargo=ifelse(is.na(cat_lead),cat,NA_character_)) %>% 
  dplyr::select(-cat_fwd,-cat_lead) %>% 
  dplyr::mutate(cat=stringi::stri_trans_general(cat,"Latin-ASCII")) %>% 
  #dplyr::mutate(ifelse(cat=="proceso de seleccionexterno","proceso de seleccion:externo",cat)) %>% 
  dplyr::mutate(cat=gsub("seleccionexterno", "seleccion:externo", cat)) %>% 
  dplyr::mutate(cat=gsub("fecha de postulacion", "fecha", cat)) %>% 
  tidyr::separate(cat,c("1","2","3","4","5","6"),":") %>%
  dplyr::mutate(titulos=ifelse(is.na(cargo),`1`,NA_character_)) %>% 
  dplyr::mutate(rn=row_number()) #%>% 
  #dplyr::distinct(titulos,.keep_all=T) %>% 
  #View()

part_3<-
part_2 %>% 
  tidyr::unite(cat_info, `2`,`3`,`4`, na.rm = TRUE, sep = ',') %>% 
  dplyr::select(cargo, titulos, cat_info) %>% 
  dplyr::mutate(cargo2=cargo) %>% 
  tidyr::fill(cargo,.direction="down") %>% 
  dplyr::mutate(cat_info=stringr::str_trim(cat_info)) %>%
  dplyr::mutate(cat_info=ifelse(cat_info=="",NA_character_,cat_info)) %>% 
  dplyr::mutate(cargo2 = ifelse( is.na(cargo2), 0, 1), #remove NA
                 id_proc_sel = sprintf("%04d",cumsum(cargo2))) %>%
  dplyr::select(id_proc_sel,cargo, titulos,cat_info) 

part_4<-
  part_3%>% 
  dplyr::group_by(id_proc_sel) %>% 
  dplyr::mutate(n=n()) %>% 
  dplyr::ungroup() %>% 
  #dplyr::filter(n==6)
  #Los casos con 6 filas o les falta la fecha o les falta el estado
  #EL caso con 9 filas corresponde a un proceso con 2 seleccionados
  #El caso con 5 filas (1755) corresponde a un proceso que habría sido declarado desierto, o que se asumirá así 
  filter(dplyr::case_when(is.na(cat_info) & is.na(titulos)~F, #When y == "", x > 3
                          T ~ T) #Otherwise, x < 3
  ) %>%
  #fila 12711, repite la fecha de postulación, 1705
  #dplyr::filter(id_proc_sel=="1705")
  dplyr::mutate(comb=paste0(id_proc_sel,"_",titulos,"_",cat_info)) %>% 
  group_by(comb) %>% 
  dplyr::mutate(n2=row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n2==1) %>% 
  #caso con más de un seleccionade
  #dplyr::filter(id_proc_sel=="0070")
  dplyr::mutate(cat_info=dplyr::case_when(cat_info=="francisca miranda baez" & id_proc_sel=="0070"~"francisca miranda baez - gilberto godoy mendez",
                                          T~cat_info)) %>% 
  filter(dplyr::case_when(grepl("gilberto godoy mendez",cat_info) & id_proc_sel=="0070"~F, #When y == "", x > 3
                          T ~ T) #Otherwise, x < 3
  ) %>%
  dplyr::mutate(id=paste0(id_proc_sel,"__",cargo,"__",n)) %>% 
  dplyr::group_by(id) %>% 
  tidyr::pivot_wider(id_cols = c(id), names_from=titulos, values_from = cat_info) %>% 
  dplyr::ungroup()

part_5<-
  part_4 %>% 
  tidyr::separate(seleccionado,into=c("sel1","sel2","sel3"),sep="-") %>%
  dplyr::mutate_at(vars(c("sel1","sel2","sel3")),~stringr::str_trim(.)) %>% 
  dplyr::mutate(sel2=ifelse(nchar(sel2)<2,NA_character_,sel2)) %>% 
  tidyr::separate(id,c("id_pub","id_proc","n_filas"),"__")

part_6 <-
  part_5 %>% 
  dplyr::mutate(cargo=gsub("axuliar|axiliar", "auxiliar", cargo)) %>% 
  dplyr::mutate(cargo=gsub("11hrs", "11 hrs", cargo)) %>% 
  dplyr::mutate(cargo=gsub(",,", ",", cargo)) %>% 
  dplyr::mutate(cargo=gsub("19,6640", "19,664", cargo)) %>% 
  dplyr::mutate(cargo=gsub("15076", "15,076", cargo)) %>% 
  dplyr::mutate(cargo=gsub("n°", "", cargo)) %>% 
  
  dplyr::mutate(cargo=gsub("administrivo de apoyo", "administrativo", cargo)) %>% 
  dplyr::mutate(cargo=gsub("°", "", cargo)) %>% 
  dplyr::mutate(cargo=gsub("administrativo 20", "administrativo grado 20", cargo)) %>% 
  dplyr::mutate(cargo=gsub("administrativo 12", "administrativo grado 12", cargo)) %>% 
  dplyr::mutate(cargo=gsub(", ", " ", cargo)) %>% 
  dplyr::mutate(cargo=gsub(" ,", " ", cargo)) %>% 
  dplyr::mutate(cargo=gsub(" , ", " ", cargo)) %>%
  dplyr::mutate(cargo=gsub("  ", " ", cargo)) %>%
  dplyr::mutate(cargo=gsub("grado17", "grado 17", cargo)) %>%
  dplyr::mutate(cargo=gsub("tecnico 1/2 jornada vespertino grado 17", "tecnico grado 17 (1/2 jornada vespertina)", cargo)) %>%
  dplyr::mutate(cargo=gsub("tecnico grado 20 1/2 jornada", "tecnico grado 20 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("tecnico grado 20 al 19", "tecnico grado del 20 al 19", cargo)) %>%
  dplyr::mutate(cargo=gsub("tecnico grado 14 1/2 jornada", "tecnico grado 14 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("tecnico grado 14 al 12", "tecnico grado del 14 al 12", cargo)) %>%
  dplyr::mutate(cargo=gsub("tecnico 17", "tecnico grado 17", cargo)) %>%
  #dplyr::mutate(cargo=gsub("1/4 jornada", "(1/4 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("1/4 de jornada", "1/4 jornada", cargo)) %>%
  dplyr::mutate(cargo=gsub("6 1/4 jornada", "6 (1/4 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("13 1/2 jornada", "13 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("10 1/2 jornada", "10 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("13 1/4 jornada", "13 (1/4 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("08 al 06", "del 08 al 06", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional grado 6", "profesional grado 06", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional grado 8", "profesional grado 08", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional,grado 10 al 8", "profesional grado del 10 al 08", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional 12", "profesional grado 12", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional grado 10 al 8", "profesional grado del 10 al 08", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional,grado 10", "profesional grado 10", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional 1/2 jornada grado 13", "profesional grado 13 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional grado 10 media jornada", "profesional grado 10 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional grado 7", "profesional grado 07", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional 13", "profesional grado 13", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional 10", "profesional grado 10", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional por 11 o 22 horas ley nº 15 076", "11 o 22 hrs, ley 15,076", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional 14 media jornada", "profesional 14 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional grado 9", "profesional grado 09", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional grado10", "profesional grado 10", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional por 11 horas a contrata", "11 hrs, ley 15,076", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional por 22 horas a contrata", "22 hrs, ley 15,076", cargo)) %>%
  dplyr::mutate(cargo=gsub("15,076", "15076", cargo)) %>%
  dplyr::mutate(cargo=gsub("19,664", "19664", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional del grado del 08 al 06", "profesional grado del 08 al 06", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional grado 11 1/2 jornada", "profesional grado 11 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional grado 13 \\(media jornada\\)", "profesional grado 13 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional grado 13 media jornada", "profesional grado 13 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional 14 \\(1/2 jornada\\)", "profesional grado 14 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("profesional grado del 10 al 8", "profesional grado del 10 al 08", cargo)) %>%
  dplyr::mutate(cargo=gsub("auxiliar 21", "auxiliar grado 21", cargo)) %>%  
  dplyr::mutate(cargo=gsub("auxiliar 23", "auxiliar grado 23", cargo)) %>%  
  dplyr::mutate(cargo=gsub("auxiliar grado21", "auxiliar grado 21", cargo)) %>%  
  dplyr::mutate(cargo=gsub("\\,", "", cargo)) %>%
  
  dplyr::mutate(estamento= dplyr::case_when(grepl("profesional",cargo)~"profesional",
                                            grepl("ley",cargo)~"médicos y dentistas",
                                            grepl("tecnico",cargo)~"tecnico",
                                            grepl("auxiliar",cargo)~"auxiliar",
                                            grepl("admin",cargo)~"administrativo",
                                            T~NA_character_)) 
  #janitor::tabyl(estamento)
  #dplyr::filter(grepl("prof",estamento)) %>% distinct(cargo)

part_7 <-
  part_6 %>% 
  dplyr::mutate(`proceso de seleccion`=gsub("interno\\,", "interno", `proceso de seleccion`)) %>%
  #distinct(`proceso de seleccion`) %>% 
  dplyr::mutate(estado=ifelse(`proceso de seleccion`=="desierto","desierto",estado)) %>% 
  dplyr::mutate(`proceso de seleccion`=ifelse(`proceso de seleccion`=="desierto",NA_character_,`proceso de seleccion`)) %>% 
  #dplyr::filter(`proceso de seleccion`=="desierto")
  dplyr::mutate(estado=gsub("\\,|\\*", "", estado)) %>%
  dplyr::mutate(estado=gsub("finlizado","finalizado", estado)) %>%
  dplyr::mutate(estado=gsub("finalizados","finalizado", estado)) %>%
  dplyr::mutate(estado=gsub("finalizada","finalizado", estado)) %>%
  #dplyr::filter(estado=="claudia pizarro vera"|estado=="luciano leyton silva")
  dplyr::mutate(estado=stringr::str_trim(estado)) %>%
  dplyr::mutate(sel1=ifelse(estado=="claudia pizarro vera","claudia pizarro vera",sel1)) %>% 
  dplyr::mutate(sel1=ifelse(estado=="luciano leyton silva","luciano leyton silva",sel1)) %>% 
  dplyr::mutate(estado=gsub("claudia pizarro vera|luciano leyton silva","finalizado", estado))
  #distinct(estado)

part_8 <-
  part_7 %>% 
  dplyr::mutate(n_filas=as.numeric(n_filas)) %>% 
  #distinct(fecha)
  #janitor::tabyl(estamento)
  #dplyr::filter(estamento!="médicos y dentistas") %>% 
  #janitor::tabyl(cargo)
  dplyr::mutate(vesp=ifelse(grepl("vesp",cargo),1,0)) %>% 
  dplyr::mutate(cargo=gsub(" vespertina","", cargo)) %>%
  dplyr::mutate(cargo=gsub("\\(vespertino\\)","", cargo)) %>%
  dplyr::mutate(cargo=gsub("\\( vespertino\\)","", cargo)) %>%
  dplyr::mutate(cargo=gsub("auxiliar grado 21 1/2 jornada","auxiliar grado 21 (1/2 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("auxiliar grado 23 3/4 de jornada","auxiliar grado 23 (3/4 jornada)", cargo)) %>%
  dplyr::mutate(cargo=gsub("auxiliar grado 21 3/4 jornada","auxiliar grado 21 (3/4 jornada)", cargo)) %>% 
  
  dplyr::mutate(cargo=gsub("11 hrs ley 15076","ley 15076 11 hrs", cargo)) %>% 
  dplyr::mutate(cargo=gsub("11 hrs ley 19664","ley 19664 11 hrs", cargo)) %>% 
  dplyr::mutate(cargo=gsub("11 o 22 hrs ley 15076","ley 15076 11 o 22 hrs", cargo)) %>% 
  dplyr::mutate(cargo=gsub("22 hrs ley 15076","ley 15076 22 hrs", cargo)) %>% 
  dplyr::mutate(cargo=gsub("22 hrs ley 19664","ley 19664 22 hrs", cargo)) %>% 
  dplyr::mutate(cargo=gsub("33 hrs ley 15076","ley 15076 33 hrs", cargo)) %>% 
  dplyr::mutate(cargo=gsub("44 hrs ley 15076","ley 15076 44 hrs", cargo)) %>% 
  dplyr::mutate(cargo=gsub("44 hrs ley 19664","ley 19664 44 hrs", cargo)) %>% 
  dplyr::mutate(cargo=gsub("adminsitrativo","administrativo", cargo)) %>% 
  
  tidyr::separate(cargo,c("est","grado"),"grado ") %>% 
  tidyr::separate(grado,c("grado","jornada")," \\(") %>% 
  dplyr::mutate(jornada=gsub("\\)","", jornada)) %>% 
  dplyr::mutate(grado_n=as.numeric(grado)) %>% 
  dplyr::mutate(grado_n=ifelse(grado=="del 20 al 19",18,grado_n)) %>% 
  dplyr::mutate(grado_n=ifelse(grado=="del 17 al 15",16,grado_n)) %>% 
  dplyr::mutate(grado_n=ifelse(grado=="del 14 al 12",13,grado_n)) %>% 
  dplyr::mutate(grado_n=ifelse(grado=="del 10 al 08",9,grado_n)) %>% 
  dplyr::mutate(grado_n=ifelse(grado=="del 08 al 06",7,grado_n)) %>% 
  dplyr::select(-estamento)
  #distinct(cargo)
  
#nchar("del 07") #6
#ver los que no tienen "al"
#investigar el cargo y las separaciones
#identificar los distintos lugares
#proceso de selección, ver si el interno y el externo están bien especificados
#ver que hayan sólo los estados que corresponden
  

part_9 <-
  part_8 %>%  
  dplyr::mutate(cnt_del=stringr::str_count(fecha, "\\bdel\\b")) %>% 
  dplyr::mutate(cnt_de=stringr::str_count(fecha, "\\bde\\b")) %>% 
  #dplyr::filter(cnt_de<=2, cnt_del==1) %>%
  #dplyr::mutate(cargo=gsub("33 hrs ley 15076","ley 15076 33 hrs", cargo)) %>% 
  dplyr::mutate(fecha=ifelse(grepl("^[0-9]*$", substring(fecha, 1,1)),paste("del ",fecha,sep=" "),fecha)) %>% 
  dplyr::mutate(fecha=gsub("  "," ", fecha)) %>% 
  #dplyr::filter(grepl("^[^0-9]*$",fecha)) %>% 
  dplyr::mutate(fecha=gsub("^cerrado$",NA_character_, fecha))  
  #83 filas

part_10<-
part_9 %>% 
  dplyr::mutate(fecha=gsub("de 20","del 20", fecha)) %>% 
  dplyr::mutate(fecha=gsub("2105","2015", fecha)) %>% 
  dplyr::mutate(fecha=gsub("o 20","o del 20", fecha)) %>% 
  #dplyr::filter(grepl("\\,\\}",fecha)) %>% 
  dplyr::mutate(fecha=gsub(" hasta el "," al ", fecha)) %>% 
  dplyr::mutate(fecha=gsub("al al ","al ", fecha)) %>% 
  dplyr::mutate(fecha=gsub("\\,\\}","", fecha)) %>% 
  dplyr::mutate(fecha=gsub("\\,\\+","", fecha)) %>% 
  dplyr::mutate(fecha=gsub(" a las 12,00 hrs$","", fecha)) %>% 
  dplyr::mutate(fecha=gsub(" a las 14,00 hrs$","", fecha)) %>% 
  dplyr::mutate(fecha=gsub("\\, a las 17,00 hrs$","", fecha)) %>% 
  dplyr::mutate(fecha=gsub("\\, hasta las 17,00 hrs$","", fecha)) %>% 
  dplyr::mutate(fecha=gsub("\\, hasta las 13,00 hrs$","", fecha)) %>%  
  dplyr::mutate(fecha=gsub("\\, al mediodia$","", fecha)) %>%  
  dplyr::mutate(fecha=gsub(" abril 20"," abril del 20", fecha)) %>%  
  dplyr::mutate(fecha=gsub(" septiembre 2014"," septiembre del 2014 ", fecha)) %>%  
  #distinct(fecha) %>% View()
  tidyr::separate(fecha, into=c("fech_in","fech_ter"),sep= " al ") 
  #dplyr::mutate(fech_ter=as.Date(fech_ter,format="%d de %m del %Y"))

part_11<-
part_10 %>% 
  #195  -- date like %d de %B del %Y del 20 junio del 2019
  #228  -- date like %d de %B del %Y 23 abril del 2019    
  #273  -- date like %d de %B del %Y de 14 enero del 2018 
  #698  -- date like %d de %B del %Y 08 junio del 2017    
  #756  -- date like %d de %B del %Y 0 5de mayo del 2017  
  dplyr::mutate(fech_ter=gsub("del  20 de junio del 2019","20 de junio del 2019", fech_ter)) %>%  
  dplyr::mutate(fech_ter=gsub("de 14 enero"," 14 de enero", fech_ter)) %>%  
  dplyr::mutate(fech_ter=gsub("08 junio del","08 de junio del", fech_ter)) %>%  
  dplyr::mutate(fech_ter=gsub("0 5de mayo del 2017","05 de mayo del 2017", fech_ter)) %>%  
  dplyr::mutate(fech_ter=gsub("23 abril del 2019"," 23 de abril del 2019", fech_ter)) %>%  
  
  dplyr::mutate(fech_ter=gsub("del 20 junio del 2019","20 de junio del 2019", fech_ter)) %>%
  dplyr::mutate(fech_ter=gsub("01 junio del 2017","01 de junio del 2017", fech_ter)) %>%  
  dplyr::mutate(fech_ter=gsub("02 mayo del 2017","02 de mayo del 2017", fech_ter)) %>%  
  dplyr::mutate(fech_ter=gsub("10 de marzo de 2015","10 de marzo del 2015", fech_ter)) %>%  
  
  dplyr::mutate(fech_ter=gsub("01 junio del 2017","01 de junio del 2017", fech_ter)) %>%  
  dplyr::mutate(fech_ter=gsub("02 mayo del 2017","02 de mayo del 2017", fech_ter)) %>%  
  dplyr::mutate(fech_ter=gsub("10 de marzo de 2015","10 de marzo del 2015", fech_ter)) %>%  
  dplyr::mutate(fech_ter=gsub("03 noviembre del 2014","03 de noviembre del 2014", fech_ter)) %>%  
  dplyr::mutate(fech_ter=gsub("14 marzo del 2014","14 de marzo del 2014", fech_ter)) %>%  
  dplyr::mutate(fech_ter=gsub("12 marzo del 2014","12 de marzo del 2014", fech_ter)) %>%
  dplyr::mutate(fech_ter=readr::parse_date(fech_ter,
    format = "%d de %B del %Y",
    na = c("", "NA"),
    locale = readr::locale("es"),
    trim_ws = TRUE))# %>% #%>% distinct(fech_ter) 
#  "mauricio  rojas martinez"

#" a las 12,00 hrs"
#" a las 14,00 hrs"
#", a las 17,00 hrs"
#", hasta las 13,00 hrs"
#", hasta las 17,00 hrs"
#", hasta las 13,00 hrs"
#" hasta el "
#"al al "

#_#_#_#_#_#_#_#_#_#_
#Seleccionados


part_12 <-
  part_11 %>% 
  dplyr::mutate(sel2=dplyr::case_when(sel1=="victor soto valdivia y claudia orellana garrido"~"claudia orellana garrido",
                                      T~sel2)) %>% 
  dplyr::mutate(sel1=dplyr::case_when(sel1=="victor soto valdivia y claudia orellana garrido"~"victor soto valdivia",
                                      T~sel1)) %>% 
  dplyr::mutate(sel2=dplyr::case_when(sel1=="jose carrizo craig y reinaldo barrios jeria"~"jose carrizo craig",
                                      T~sel2)) %>% 
  dplyr::mutate(sel1=dplyr::case_when(sel1=="jose carrizo craig y reinaldo barrios jeria"~"reinaldo barrios jeria",
                                      T~sel1)) %>% 
  dplyr::mutate(sel1=gsub("aviles zapata, juan","juan aviles zapata", sel1)) 

library(fuzzyjoin); library(dplyr);

fuzzyjoin::stringdist_join(data.frame(name=part_12$sel1), data.frame(name=part_12$sel1), 
                by = "name",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = .25, #Maximum distance allowed for a match. Expressed either as integer, or as a fraction of the pattern length (will be replaced by the smallest integer not less than the corresponding fraction), or a list with possible components
                distance_col = "dist") %>%
  dplyr::filter(dist>0) %>% 
  arrange(dist) %>% 
  dplyr::mutate(name_conc1=paste0(name.x,"_",name.y)) %>% 
  dplyr::mutate(name_conc2=paste0(name.y,"_",name.x)) %>% 
  dplyr::group_by(name_conc1) %>% 
  dplyr::mutate(name_conc1_n=row_number()) %>% 
  dplyr::ungroup(name_conc1) %>% 
  dplyr::group_by(name_conc2) %>% 
  dplyr::mutate(name_conc2_n=row_number()) %>% 
  dplyr::ungroup(name_conc2) %>% 
  dplyr::filter(name_conc1_n==1, name_conc2_n==1) %>% 
  dplyr::mutate(name_conc2_lag=lag(name_conc2)) %>% 
  dplyr::filter(name_conc1!=name_conc2_lag) %>% 
  View()
# rene estay zorricueta  --- rene eduardo estay zorricueta ---0.20306513
# jaime arce lopez --- jaime antonio arce lopez  --- 0.173
# danilo ahumada castillo --- ahumada castillo danilo --- 0.1729
# mauricio valenzuela arancibia --- mauricio andres valenzuela arancibia --- 0.168
# maria cubillos castro --- maria paz cubillos castro --- 0.14
# ana maria leiva orellana --- ana maria leiva --- 0.125
# susana  cerda arancibia --- susana cerda arancibia ---.059
# mauricio rojas martinez --- mauricio  rojas martinez ---
# enzo borroni ricardo --- enzo borroni ricardi
# mabel matsumoto jimenez --- mabel matsumotu jimenez
# marta adriazola  moreno --- marta adriazola moreno

# veo 1218 casos para ver quiénes calzan


part_13 <-
part_12 %>% 
  dplyr::mutate(sel1=gsub("rene eduardo estay zorricueta","rene estay zorricueta", sel1)) %>%  
  dplyr::mutate(sel1=gsub("jaime antonio arce lopez","jaime arce lopez", sel1)) %>%  
  dplyr::mutate(sel1=gsub("ahumada castillo danilo","danilo ahumada castillo", sel1)) %>%  
  dplyr::mutate(sel1=gsub("mauricio andres valenzuela arancibia","mauricio valenzuela arancibia", sel1)) %>%  
  dplyr::mutate(sel1=gsub("maria paz cubillos castro","maria cubillos castro", sel1)) %>%  
  dplyr::mutate(sel1=gsub("susana  cerda arancibia","susana cerda arancibia", sel1)) %>%  
  dplyr::mutate(sel1=gsub("mauricio  rojas martinez","mauricio rojas martinez", sel1)) %>%  
  dplyr::mutate(sel1=gsub("enzo borroni ricardo","enzo borroni ricardi", sel1)) %>%  
  dplyr::mutate(sel1=gsub("mabel matsumotu jimenez","mabel matsumoto jimenez", sel1)) %>%  
  dplyr::mutate(sel1=gsub("marta adriazola  moreno","marta adriazola moreno", sel1)) %>% 
  dplyr::mutate(sel1=gsub("desierto",NA_character_, sel1)) %>% 
  dplyr::mutate(sel1=gsub("boin maraboli maria susana","susana boin maraboli", sel1)) %>% 
  dplyr::mutate(sel1=gsub("toledo gerardo","gerardo toledo villegas", sel1)) 
  #dplyr::filter(grepl("\\,",sel1)) %


fuzzyjoin::stringdist_join(data.frame(name=part_13$sel1), data.frame(name=part_13$sel2), 
                           by = "name",
                           mode = "left",
                           ignore_case = FALSE, 
                           method = "jw", 
                           max_dist = .25, #Maximum distance allowed for a match. Expressed either as integer, or as a fraction of the pattern length (will be replaced by the smallest integer not less than the corresponding fraction), or a list with possible components
                           distance_col = "dist") %>%
  dplyr::filter(dist>0) %>% 
  arrange(dist) %>% 
  dplyr::mutate(name_conc1=paste0(name.x,"_",name.y)) %>% 
  dplyr::mutate(name_conc2=paste0(name.y,"_",name.x)) %>% 
  dplyr::group_by(name_conc1) %>% 
  dplyr::mutate(name_conc1_n=row_number()) %>% 
  dplyr::ungroup(name_conc1) %>% 
  dplyr::group_by(name_conc2) %>% 
  dplyr::mutate(name_conc2_n=row_number()) %>% 
  dplyr::ungroup(name_conc2) %>% 
  dplyr::filter(name_conc1_n==1, name_conc2_n==1) %>% 
  dplyr::mutate(name_conc2_lag=lag(name_conc2)) %>% 
  dplyr::filter(name_conc1!=name_conc2_lag) %>% 
  View()

invisible("Separar ampliar la lista desde wide por proceso de selección, a una lista por seleccionado")

part_14 <-
part_13 %>% 
  tidyr::pivot_longer(cols=c("sel1","sel2"), names_to="seleccionado") %>% 
  dplyr::filter(dplyr::case_when(is.na(value) & seleccionado=="sel2"~F,
                                 T~T)) %>%
  dplyr::rename("id_seleccionado"="value") %>% 
  dplyr::select(id_seleccionado,fech_ter,everything()) %>%
  #dplyr::filter(estado!="finalizado",!is.na(id_seleccionado))
  dplyr::filter(!grepl("ley",est), estado=="finalizado") %>% 
  dplyr::mutate(sel1=gsub("gomez  soto sandra","gomez soto sandra", id_seleccionado))
  #group_by(id_seleccionado) %>%    summarise(n())
  #View()

invisible("Cuántas veces un sujeto gaba proceso de selección")

part_14 %>% group_by(id_seleccionado) %>% 
  summarise(n()) %>%  janitor::tabyl(`n()`) %>% 
  dplyr::ungroup()%>%
  dplyr::mutate(valid_percent=scales::percent(n/(sum(n)-sum(n[is.na(`n()`)])),2),
                valid_percent=ifelse(is.na(`n()`),NA,valid_percent),
                percent=scales::percent(n/(sum(n)),2)) %>% 
  dplyr::mutate(label=dplyr::case_when(`n()`==1~paste0("Selected in one\nprocess\n",percent,"(n=",n,")"),
                                       `n()`==2~paste0("Selected in two\nprocesses\n",percent,"(n=",n,")"),
                                       `n()`==3~paste0("Selected in three\nprocesses\n",percent,"(n=",n,")")
                                       )) %>% 
  plot_ly(labels = ~`n()`, values = ~n, type = 'pie',hole = 0.6,textposition = 'outside',textinfo = 'text', sort = FALSE,
          automargin = TRUE,
          text = ~label,
          direction = "clockwise",
          textfont = list(color = "black", size = 13),
          marker = list(colors = gray.colors(3))) %>%
  layout(yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T)) %>% 
  layout(showlegend = FALSE, list(size = 14,color = 'blue'))%>% 
  layout(title="People selected in \npersonnel selection processes\n ",font=list(
    family = "sans serif",
    size = 14,
    color = 'black')) %>% 
  layout(autosize = T, margin=list( l = 50, r = 50, b = 100, t = 250,  pad = 4))

invsible("4. Categorizar por sexo los nombres")

conversion_sex <- readxl::read_excel("conversion_sex.xlsx")

part_15 <-
  part_14 %>% 
  dplyr::left_join(conversion_sex,by=c("id_seleccionado"="names"))
  
#------------------------------------------------------Otra opción#------------
invsible("1. Ver cuántas veces se concreta un proceso de selección y si eso depende del año de término")

part_16 <-
part_13 %>% 
  dplyr::filter(grepl("\\[",id_proc)) %>% 
  dplyr::mutate(id_proc_usach=id_proc) %>% 
  tidyr::separate(id_proc_usach,c("id1","id2","id3"),sep="\\[") %>% 
  tidyr::separate(id2,c("id21","id22","id23"),sep="\\]") %>%
  dplyr::select(-id1, -id22, -id23,-id3) %>%
  dplyr::rename("cod_proc_sel"="id21") %>% 
  dplyr::filter(!grepl("s/f",cod_proc_sel)) %>% 
  dplyr::select(id_pub, id_proc,cod_proc_sel,everything())%>%
  dplyr::mutate(sel1=anonymizer::anonymize(sel1, .algo = "sha256", .seed = 2125)) %>% 
  dplyr::mutate(sel2=anonymizer::anonymize(sel2, .algo = "sha256", .seed = 2125)) %>% 
  dplyr::mutate(sel3=anonymizer::anonymize(sel3, .algo = "sha256", .seed = 2125))

#478 rows
part_16%>% group_by(cod_proc_sel) %>% 
  summarise(n_cod=n()) %>%
  dplyr::mutate(n_cod=ifelse(n_cod>3,4,n_cod)) %>% 
  janitor::tabyl(n_cod) %>% 
  dplyr::ungroup()%>%
  dplyr::mutate(valid_percent=scales::percent(n/(sum(n)-sum(n[is.na(n_cod)])),2),
                valid_percent=ifelse(is.na(n_cod),NA,valid_percent),
                percent=scales::percent(n/(sum(n)),2)) %>% 
  dplyr::mutate(label=dplyr::case_when(n_cod==1~paste0("Only appears one time\n",percent,"(n=",n,")"),
                                       n_cod==2~paste0("Appears two times\n",percent,"(n=",n,")"),
                                       n_cod==3~paste0("Appears three times\n",percent,"(n=",n,")"),
                                       n_cod>3~paste0("Appears more than 3 times\n",percent,"(n=",n,")")
  )) %>% 
  plot_ly(labels = ~n_cod, values = ~n, type = 'pie',hole = 0.6,textposition = 'outside',textinfo = 'text', sort = FALSE,
          automargin = TRUE,
          text = ~label,
          direction = "clockwise",
          textfont = list(color = "black", size = 13),
          marker = list(colors = gray.colors(4))) %>%
  layout(yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T)) %>% 
  layout(showlegend = FALSE, list(size = 14,color = 'blue'))%>% 
  layout(title="Selection processes\n and times they were published\n ",font=list(
    family = "sans serif",
    size = 14,
    color = 'black')) %>% 
  layout(autosize = T, margin=list( l = 50, r = 50, b = 100, t = 250,  pad = 4))

#---------------------------------DEFINIR REGLAS PARA VARIABLES DE TRANSICION
invsible("2. Ver si las que han ganado hasta 3 procesos de seleccion responden a un incremento de renta o solo a reubicación")

part_16zeroinf<-
part_16%>% group_by(cod_proc_sel) %>% 
  mutate(n_cod=n()) %>%
  ungroup() %>% 
  dplyr::mutate(n_cod=ifelse(n_cod>3,4,n_cod)-1) %>% 
  dplyr::mutate(year=format(fech_ter,format="%Y")) %>% 
  dplyr::mutate(status=ifelse(!is.na(sel1),1,0)) %>% 
  group_by(cod_proc_sel) %>%
  dplyr::mutate(diff_fech=as.numeric(lag(fech_ter)-fech_ter)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mumtate(max(fech_ter))

m1 <- pscl::zeroinfl(n_cod ~ year |  est,
               data = part_16zeroinf, dist = "negbin") #, EM=T error, model_count not found
summary(m1)
#we cannot reject the null hypothesis that the predictor (estamento) has zero effect on the
#zero-inflation component of the model.

modelNB <- glm.nb(formula = n_cod ~ year,
                  data    = part_16zeroinf)
summary(modelNB)


#-----------------------------------DEFINIR MODELOS MULTIESTADO#------------

invisible("Las trayectorias debieran ser: pasa a profesional, mejora grado")

invsible("3. De esas, calcular tiempo al evento")

PWP_GT <- coxph (Surv(diff_fech,URI_status)∼Mon_R + + cluster. (StudyID)+Strata (URI_Count),data = uri)
summary (PWP_GT)

