#Bibliotecas

library("PNADcIBGE")
require(survey)

# pnadc2021 <-  get_pnadc(2021,quarter=1, labels=TRUE, savedir="C:/Users/lhayana/Downloads/PNADC_2021")

names(pnadc2021)
names(pnadc2021$variables)

#Variáveis que vamos usar:
# - UF
# - UPA: Unidade Primária de Amostragem (UPA - Para identificação dos domicílios)
# - ESTRATO: Agrupamentos de seleção (Para identificação dos domicílios)
# - VD4001: Condição em relação à força de trabalho na semana de referência para pessoas de 14 anos ou mais de idade
# - VD4002: Condição de ocupação na semana de referência para pessoas de 14 anos ou mais de idade
# - V2009: Idade do morador naquela data
# - V2007: Sexo
# - VD4016: Rendimento mensal habitual do trabalho principal para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias no trabalho principal)
# - VD4009: Posição na ocupação e categoria do emprego do trabalho principal da semana de referência para pessoas de 14 anos ou mais de idade
# - V1028: Peso do domicílio e das pessoas

read = read_pnadc("C:/Users/lhayana/Downloads/PNADC_2021/PNADC_012021.txt", 
                        "C:/Users/lhayana/Downloads/PNADC_2021/input_PNADC_trimestral.txt", vars = NULL)

reduz <- c("UF", "UPA","Estrato","VD4001", "VD4002", "V2007", "V2009", "VD4009", "VD4016", "V1028")
reduz_pnad = read[reduz]

#Para trabalhar com as variáveis precisaremos transformar alguns de character em numéricas
reduz_pnad$UF = as.numeric(reduz_pnad$UF)
reduz_pnad$UPA = as.numeric(reduz_pnad$UPA)
reduz_pnad$Estrato = as.numeric(reduz_pnad$Estrato)
reduz_pnad$VD4001 = as.numeric(reduz_pnad$VD4001)
reduz_pnad$VD4002 = as.numeric(reduz_pnad$VD4002)
reduz_pnad$V2009 = as.numeric(reduz_pnad$V2009)
reduz_pnad$V2007 = as.numeric(reduz_pnad$V2007)
reduz_pnad$VD4009 = as.numeric(reduz_pnad$VD4009)
reduz_pnad$VD4016 = as.numeric(reduz_pnad$VD4016)
reduz_pnad$V1028 = as.numeric(reduz_pnad$V1028)

#Condição da força de trabalho
table(reduz_pnad$VD4001)
reduz_pnad$condat = factor(reduz_pnad$VD4001,labels=c('PEA', 'Inativos'))
100*prop.table(table(reduz_pnad$condat))

#Condição de ocupação
reduz_pnad$condocup = factor(reduz_pnad$VD4002,labels=c('Ocupadas', 'Desocupadas'))
100*prop.table(table(reduz_pnad$condocup))

#Idade
summary(reduz_pnad$V2009)
reduz_pnad$IDADECAT = factor(cut(reduz_pnad$V2009, breaks=c(14,20,25,30,35,40,45,50,55,60,65,112), labels=c("14-19","20-24","25-29","30-34","35-39","40-44","45-49", "50-54","55-59", "60-64","65+"),right=FALSE))
table(reduz_pnad$IDADECAT)
100*prop.table(table(reduz_pnad$IDADECAT))

#Sexo
table(reduz_pnad$V2007)
reduz_pnad$sexo = factor(reduz_pnad$V2007,labels=c('Homens', 'Mulheres'))
100*prop.table(table(reduz_pnad$sexo))

#Renda
summary(reduz_pnad$VD4016)

#Criando uma variável para trabalhadores sem/com carteira assinada 
reduz_pnad$trabcarteira[reduz_pnad$VD4009==2 | reduz_pnad$VD4009==4 | reduz_pnad$VD4009==6 | reduz_pnad$VD4009==9 | reduz_pnad$VD4009==10] <- "Sem carteira (informal)"
reduz_pnad$trabcarteira[reduz_pnad$VD4009==3 | reduz_pnad$VD4009==5 | reduz_pnad$VD4009==7 | reduz_pnad$VD4009==8 | reduz_pnad$VD4009==9]<- "com carteira/outro"
table(reduz_pnad$trabcarteira)

#Recompor o plano amostral

### O pacote survey fornece recursos (funções) para analisar dados de pesquisas 
### como a PNAD Contínua, pois leva em conta seu plano amostral complexo.

### Principais funções do pacote:
### - svymean: estima médias de variáveis contínuas para a população (ponderado
###            pelo peso pós-estratificação – V1028 no caso da PNAD Contínua trimestral),
###            além de possibilitar estimativas para razões entre contagem de subpopulações;
### - svyby: calcula estatísticas em subconjuntos a partir de recortes definidos
###          por fatores (ponderadas pelo peso pós-estratificação).


# Recompondo para o Brasil
sample.pnadc = svydesign(ids = ~UPA, strata = ~Estrato, weights = ~V1028, data = reduz_pnad , na.rm=TRUE, nest = TRUE)
# para evitar erro "has only one PSU at stage 1"
options(survey.lonely.psu = "adjust")

#Agora podemos fazer análises
 # Condição de atividade - Brasil
svymean(~factor(condat), design=sample.pnadc, na.rm=TRUE)

a = svymean(~factor(condat), design=sample.pnadc, na.rm=TRUE)
barplot(a*100, names.arg=c("PEA","Inativos"), col="purple", 
        ylim=c(0,100), main="Condição de atividade no Brasil.",
        sub="Fonte: PNAD, 1o tri 2021")

# Condição de ocupação - Brasil
b = svymean(~factor(condocup), design=sample.pnadc, na.rm=TRUE)
barplot(b*100, names.arg=c("Ocupadas","Desocupadas"), col="grey", 
        ylim=c(0,100), main="Condição de ocupação no Brasil",
        sub="Fonte: PNAD C, 1o tri 2021")

# Posição na ocupação  - Brasil
c = svymean(~factor(trabcarteira), design=sample.pnadc, na.rm=TRUE)
barplot(b*100, names.arg=c("Com carteira/outro","Sem carteira"), col="red", 
        ylim=c(0,100), main="Posição na ocupação, Brasil",
        sub="Fonte: PNAD C, 1o tri 2021",
        xlab="Sem carteira: setor privado+empreg.doméstico",
        font.main=1, font.lab=4, font.sub=4)

# Renda média por sexo
svyby(~VD4016, ~sexo, sample.pnadc, svymean, na.rm=TRUE)
svyboxplot(~VD4016~factor(sexo, labels=c('Homens','Mulheres')),
           design=sample.pnadc, xlab='Fonte: PNAD C, 1o tri 2021', ylab='R$', col="red",main="Boxplot - Rendimento mensal por sexo, Brasil", 
           font.main = 4, col.main = "black", cex.main = 0.8, outliers=TRUE,
           ylim =c (0, 8000))

### RN

RN = reduz_pnad[reduz_pnad$UF==24,]

#Recompondo plano amostral do RN
subamostra <- svydesign(ids = ~UPA, strata = ~Estrato, weights = ~V1028, data = RN, na.rm=TRUE, nest = TRUE)

#Condição de atividade - RN
svymean(~factor(condat), design=subamostra, na.rm=TRUE)

a<-svymean(~factor(condat), design=subamostra, na.rm=TRUE)
barplot(a*100, names.arg=c("PEA","Inativos"), col="purple", 
        ylim=c(0,100), main="Condição de atividade, RN",
        sub="Fonte: PNAD Contínua, 1o tri 2021")

