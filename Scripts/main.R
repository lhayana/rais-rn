library(dplyr)
library(ggplot2)
library(forcats)
library(plotly)
library(viridis)
library(geobr)

rn = read.csv2("C:/Users/pc/Documents/GitHub/rais-rn/Dados/rais_rn.txt")

rn$Bairros.SP = NULL
rn$Bairros.Fortaleza = NULL
rn$Bairros.RJ = NULL
rn$Regiões.Adm.DF = NULL

str(rn)

rn_ativos = filter(rn, Vínculo.Ativo.31.12 == 1)
pcd = filter(rn_ativos, rn_ativos$Ind.Portador.Defic == 1)

h = data.frame(tapply(rn_ativos$Sexo.Trabalhador==1, rn_ativos$Município, sum))
colnames(h) = c("Homens")
h = cbind(Município = rownames(h), h)
rownames(h) <- 1:nrow(h)

m = data.frame(tapply(rn_ativos$Sexo.Trabalhador==2, rn_ativos$Município, sum))
colnames(m) = c("Mulheres")
m = cbind(Município = rownames(m), m)
rownames(m) <- 1:nrow(m)

sexo_municipio = merge(h,m, all=T)
sexo_municipio['p_mullheres'] = (sexo_municipio$Mulheres/(sexo_municipio$Mulheres + sexo_municipio$Homens))

pcd$Tipo.Defic[pcd$Tipo.Defic == "1"] = "Física"
pcd$Tipo.Defic[pcd$Tipo.Defic == "2"] = "Auditiva"
pcd$Tipo.Defic[pcd$Tipo.Defic == "3"] = "Visual"
pcd$Tipo.Defic[pcd$Tipo.Defic == "4"] = "Mental"
pcd$Tipo.Defic[pcd$Tipo.Defic == "5"] = "Múltipla"
pcd$Tipo.Defic[pcd$Tipo.Defic == "6"] = "Reabilitado"

pcd$Tipo.Defic = factor(pcd$Tipo.Defic, levels=c("Física","Reabilitado",
                                               "Visual", "Auditiva",
                                               "Mental", "Múltipla"))
grafico_tipos_pcd =
  ggplot(pcd)+
  geom_bar(aes(Tipo.Defic),fill=heat.colors(6), stat='count')
  theme_bw()+
  ggtitle("Vínculos ativos por tipo de deficiência")

grafico_tipos_pcd = ggplotly(grafico_tipos_pcd)
grafico_tipos_pcd

geo_rn = read_municipality(code_muni="RN", year=2019)

ggplot() +
  geom_sf(data=geo_rn, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municipalities of RN, 2019", size=8) +
  theme_minimal()

ggplot() +
  geom_sf(data=geo_rn, aes(fill=sexo_municipio$p_mullheres)) +
  labs(subtitle="", size=8)+
  scale_fill_distiller(palette = "RdPu", name="Proporção de mulheres") +
  theme_minimal()+
  theme(axis.text.y=element_blank(), axis.text.x=element_blank())
