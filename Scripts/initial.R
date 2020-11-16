rais = read.csv2("C:/Users/pc/Documents/GitHub/rais-rn/RAIS_VINC_PUB_NORDESTE.txt")

library("dplyr")
library("stringr")

rn = rais %>% 
  filter(str_starts(Município, '24'))

str(rn)

write.csv2(rn, "C:/Users/pc/Documents/GitHub/rais-rn/rais_rn.txt")
