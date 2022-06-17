library(readxl)
library(tidyr)
library(dplyr)
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
  tidyr::pivot_wider(id_cols = c(id), names_from=titulos, values_from = cat_info)

part_5<-
part_4 %>% 
  tidyr::separate(seleccionado,into=c("sel1","sel2","sel3"),sep="-") %>%
  dplyr::mutate_at(vars(c("sel1","sel2","sel3")),~stringr::str_trim(.)) %>% 
  dplyr::mutate(sel2=ifelse(nchar(sel2)<2,NA_character_,sel2))
S

#nchar("del 07") #6
#ver los que no tienen "al"
#investigar el cargo y las separaciones
#identificar los distintos lugares
#proceso de selección, ver si el interno y el externo están bien especificados
#ver que hayan sólo los estados que corresponden
  
  