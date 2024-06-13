load("H:/Mi unidad/DOCUMENTOS/proyecto_proc_sel/__analisis.RData")
library(dplyr)
library(tidyr)

# > nrow(part_16)
# [1] 1031
# > nrow(part_15)
# [1] 1104
# > nrow(part_14)
# [1] 1104
# > nrow(part_6)
# [1] 2799
# > nrow(part_5)
# [1] 2799
# > nrow(part_4)
# [1] 2799
# 
summary(part_17$diff_bet_proc/365.25)