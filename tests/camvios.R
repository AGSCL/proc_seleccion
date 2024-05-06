part_1 %>% 
  slice(525:535) %>% View()


part_15 %>% 
  dplyr::filter(is.na(sex)) %>% View()
# [a438] administrativo/a de cobranzas
# [b021] técnico(a) especializado(a) de laboratorio ( bioterio)
# [b272-b273-b274] guardia de seguridad
# [a946-a947-a948] abogado(a) dirección jurídica
# [b015] secretario(a) c




part_3_2 %>% 
  dplyr::filter(grepl("0070|1755|2130|1608|1729|1798|2041|2228",id_proc_sel)) %>% View()

part_5 %>% 
  dplyr::filter(grepl("0070|1755|2130|1608|1729|1798|2041|2228",id_pub)) %>% View()

part_5 %>% 
  dplyr::filter(grepl("0070", id_pub))


part_3_2 %>% dplyr::filter(id_proc_sel %in% (dplyr::filter(part_15, is.na(fech_ter)) %>% distinct(id_pub) %>% pull(id_pub))) %>% View()


invisible("Con los cambios que hice, gente me quedo sin fecha de término")






invisible("Cargo donde va el lugar y viceversa")
part_3_2 %>% 
  dplyr::filter(grepl("2103|2174",id_proc_sel)) %>% View()

invisible("No se debiesen producir")
table(part_5$estado)
#luciano leyton silva 
#claudia pizarro vera 
