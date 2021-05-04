#selecionando diretorio
setwd("/Hitalo/Documents/case-fortBrasil/")

#requerindo pacotes
library(dplyr)
library(lubridate)
library(tsibble)

#importando base
dadosQ1 = read.table("Bases/Q1.txt",header = T)
colnames(dadosQ1) = tolower(colnames(dadosQ1))

#criando coluna ano mes
dadosQ1_ = dadosQ1 %>% dplyr::mutate(mes_vencimento = yearmonth(dt_vencimento))

# 1.1 - Qual o percentual de faturas emitidas por mês no qual os clientes não pagaram a fatura anterior? 
dadosQ1_ %>% dplyr::group_by(mes_vencimento) %>%
  dplyr::summarise(perc_fat = paste0(round((sum(ds_rolagem == "FX1")/dplyr::n())*100,2),"%")) %>% View()


# Cramer's V for nominal variables with more than 2 categories
tab <- table(sample(1:2, 30, TRUE), sample(1:3, 30, TRUE))
cramer(tab)

baseCompQ1_ = baseCompQ1 %>% mutate(default = case_when(ds_rolagem == 'FX1' ~ 1,TRUE ~ 0)) %>%
  select(default,qtd_faturas_ult_6m,vl_medio_fatura,qtd_faturas_ult_6m_fx1)

baseCompQ1_$default = as.factor(baseCompQ1_$default)

baseCompQ1_rmna = na.omit(baseCompQ1_)

dim(baseCompQ1_rmna)

dim(na.omit(baseCompQ1_rmna))

cramer(default ~ qtd_faturas_ult_6m, data = baseCompQ1_)

cramer(default ~ vl_medio_fatura, data = baseCompQ1_)

cramer(default ~ qtd_faturas_ult_6m_fx1, data = baseCompQ1_)

str(baseCompQ1_rmna)

str(baseCompQ1_rmna)


mod_qtd_faturas_ult_6m = stats::lm(formula = default ~ qtd_faturas_ult_6m,data = baseCompQ1_rmna)
summary(mod_qtd_faturas_ult_6m)$r.squared

mod_vl_medio_fatura = lm(formula = default ~  vl_medio_fatura,data = baseCompQ1_)
summary(mod_vl_medio_fatura)$r.squared

mod_qtd_faturas_ult_6m_fx1 = lm(formula = default ~  qtd_faturas_ult_6m_fx1,data = baseCompQ1_)
summary(mod_qtd_faturas_ult_6m_fx1)$r.squared

mod_geral = glm(formula = default ~.,data = baseCompQ1_,family = 'binomial')
summary(mod_geral)
exp(mod_geral$coefficients)
odds.ratio(mod_geral,level = 0.95)
1/0.86375

mod_geral = glm(formula = default ~ qtd_faturas_ult_6m + qtd_faturas_ult_6m_fx1,data = baseCompQ1_,family = 'binomial')
summary(mod_geral)
exp(mod_geral$coefficients)
odds.ratio(mod_geral,level = 0.95)

######## questao 2 ###################
######################################

dadosQ21 = read.table("Bases/Q21.txt",header = T)
colnames(dadosQ21) =  tolower(colnames(dadosQ21))
dadosQ21$dt_acordo = as.Date(dadosQ21$dt_acordo)

dadosQ22 = read.table("Bases/Q22.txt",header = T)
colnames(dadosQ22) =  tolower(colnames(dadosQ22))
dadosQ22$dt_acordo = as.Date(dadosQ22$dt_acordo)

dadosQ23 = read.table("Bases/Q23.txt",header = T)
colnames(dadosQ23) =  tolower(colnames(dadosQ23))
dadosQ23$dt_acordo = as.Date(dadosQ23$dt_acordo)

dadosQ24 = read.table("Bases/Q24.txt",header = T,sep = "\t")
colnames(dadosQ24) =  tolower(colnames(dadosQ24))
dadosQ24$dt_acordo = as.Date(dadosQ24$dt_acordo)

dadosQ25 = read.table("Bases/Q25.txt",header = T)
colnames(dadosQ25) =  tolower(colnames(dadosQ25))
dadosQ25$dt_acordo = as.Date(dadosQ25$dt_acordo)

baseCompQ2 = dadosQ21 %>% left_join(dadosQ22) %>%
  left_join(dadosQ23) %>%
  left_join(dadosQ24) %>%
  left_join(dadosQ25)

baseCompQ2_ = baseCompQ2 %>% dplyr::mutate(ano_mes = yearmonth(dt_acordo),
                                           faixa_atraso = case_when((nu_dias_atraso >=180 & nu_dias_atraso <= 240) ~ "180-240 dias",
                                                                    (nu_dias_atraso >240 & nu_dias_atraso <= 300) ~ "240-300 dias",
                                                                    (nu_dias_atraso >300 & nu_dias_atraso <= 360) ~ "300-360 dias",
                                                                    (nu_dias_atraso >360 & nu_dias_atraso <= 420) ~ "360-420 dias",
                                                                    (nu_dias_atraso >420 & nu_dias_atraso <= 480) ~ "420-480 dias",
                                                                    (nu_dias_atraso >480) ~ "480 dias-mais"))

write.table(baseCompQ2_,file = "Bases/Q2_comp.csv",quote = F,row.names = F,col.names = T,dec = ",",sep = ";")

baseCompQ2_ %>% dplyr::group_by(ano_mes) %>%
  dplyr::mutate(qtde_resposta = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ano_mes,faixa_atraso) %>%
  dplyr::summarise(perc_adesao = paste0(round((sum(resposta==1)/max(qtde_resposta))*100,2),"%")) %>%  View()


a = baseCompQ2_ %>% dplyr::group_by(ano_mes) %>%
  dplyr::mutate(qtde_resposta = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(faixa_atraso) %>%
  dplyr::summarise(perc_adesao = paste0(round((sum(resposta==1)/max(qtde_resposta))*100,2),"%"),
                   qtd_acionamento = n())

a = baseCompQ2_ %>% 
  dplyr::group_by(faixa_atraso) %>%
  dplyr::summarise(perc_adesao = paste0(round((sum(resposta==1)/n())*100,2),"%"),
                   qtd_acionamento = n())

ggplot(a) +
  aes(x = faixa_atraso, y = perc_adesao) +
  geom_tile(size = 1.2) +
  theme_minimal()

esquisse::esquisser(a)

################################################
#################### modelo ####################
################################################

## filtrando periodo do modelo #####
base_modelo = baseCompQ2_ %>% #dplyr::filter(faixa_atraso == "180-240 dias") %>%
  select(resposta,nu_dias_atraso:divida_atual,qtd_parcelamento_3m:qtd_acionamento_6m, -c(id_conta,dt_acordo,ano_mes, faixa_atraso))

#### Descritiva base ######



### analisando composicao da variavel resposta ####
table(base_modelo$resposta)

### percentual de acordos realizados ###
sum(base_modelo$resposta == 1)/nrow(base_modelo)

## transformando variavel resposta em fator ####

base_modelo$resposta = as.factor(base_modelo$resposta)

# retirando dados omissos da base

base_modelo_rmna = na.omit(base_modelo)

dim(base_modelo_rmna)

### percentual de acordos realizados depois de remover dados omissos ###
sum(base_modelo_rmna$resposta == 1)/nrow(base_modelo_rmna)

base_modelo_rmna_amostra_1 = base_modelo_rmna %>% dplyr::filter(resposta == 1)

base_modelo_rmna_amostra_0 = base_modelo_rmna %>% dplyr::filter(resposta == 0)

set.seed(404020)
base_modelo_rmna_amostra_0_ = base_modelo_rmna_amostra_0[sample(1:nrow(base_modelo_rmna_amostra_0),nrow(base_modelo_rmna_amostra_1)),]

base_modelo_rmna_amostra = rbind(base_modelo_rmna_amostra_1,
                                 base_modelo_rmna_amostra_0_)

set.seed(404020)
base_modelo_rmna_amostra_train = rbind(base_modelo_rmna_amostra_1[sample(1:nrow(base_modelo_rmna_amostra_1),nrow(base_modelo_rmna_amostra_1)*0.7),],
                                       base_modelo_rmna_amostra_0_[sample(1:nrow(base_modelo_rmna_amostra_0_),nrow(base_modelo_rmna_amostra_0_)*0.7),])

base_modelo_rmna_amostra_test = rbind(base_modelo_rmna_amostra_1[!(c(1:nrow(base_modelo_rmna_amostra_1)) %in% (sample(1:nrow(base_modelo_rmna_amostra_1),nrow(base_modelo_rmna_amostra_1)*0.7))),],
                                       base_modelo_rmna_amostra_0_[!(c(1:nrow(base_modelo_rmna_amostra_0_)) %in% (sample(1:nrow(base_modelo_rmna_amostra_0_),nrow(base_modelo_rmna_amostra_0_)*0.7))),])


#########################################
base_modelo_rmna_amostra$resposta = as.factor(base_modelo_rmna_amostra$resposta)
### vendo a relacao da variavel resposta com as variaveis explicativas induvidualmente

names_colunas = colnames(base_modelo_rmna_amostra)

for (i in 2:length(names_colunas)) {
  assign(paste0("mod_",names_colunas[i]), glm(formula = resposta ~ base_modelo_rmna_amostra[,i],data = base_modelo_rmna_amostra ,family = binomial(link="logit")))
  
  #assign(paste0("mod_",names_colunas[i]), lm(formula = as.numeric(resposta) ~ base_modelo_rmna_amostra[,i],data = base_modelo_rmna_amostra))
  
  a = summary(get(paste0("mod_",names_colunas[i])))
  
  base_testes =  data_frame(`Variáveis` = names_colunas[i],
                            Estimativa = round(a$coefficients[2,1],6),
                            `Erro-padrão` = round(a$coefficients[2,2],6),
                            `Estatística t` = round(a$coefficients[2,3],6),
                            `Valor-p` = round(a$coefficients[2,4],6),
                            Significante = ifelse(round(a$coefficients[2,4],6)>0.05,"Não","Sim"))
  
  if(i == 2){
    base_testes_fim = base_testes
  }else{
    base_testes_fim = rbind(base_testes_fim,base_testes)
  }

}

## selecionando variaevis de significante ####

variaveis_selec = base_testes_fim %>% dplyr::filter(Significante == "Sim") %>% dplyr::select(`Variáveis`)

base_modelo_mult = base_modelo_rmna_amostra %>% select(resposta,as.vector(variaveis_selec$Variáveis))

### gerando modelo multiplo #####

mod_mult = glm(formula = resposta ~ .,data = base_modelo_mult[,!(colnames(base_modelo_mult) %in% c("qtd_fx2_geral"))],family = binomial(link="logit"))
sum_modmult = summary(mod_mult)

base_modmult_fim = base_modelo_mult %>% dplyr::select(colnames(base_modelo_mult)[round(sum_modmult$coefficients[,4],6)<0.05])
library(car)
options("scipen"=100, "digits"=4)
sort(vif(mod_mult))
ld.vars <- attributes(alias(mod_mult)$Complete)$dimnames[[1]]

test_vif = glm(formula = resposta ~ .,data = base_modelo_mult[,!(colnames(base_modelo_mult) %in% c("qtd_extratos",
                                                                                                   "divida_atual",
                                                                                                   "qtd_parcelamento_6m",
                                                                                                   "qtd_fx1_6m"))],family = binomial(link="logit"))
sum_test_vif = summary(test_vif)
sort(vif(test_vif))

base_vif = base_modelo_mult[,!(colnames(base_modelo_mult) %in% c("qtd_extratos",
                                                                 "divida_atual",
                                                                 "qtd_parcelamento_6m",
                                                                 "qtd_fx1_6m"))]

base_modmult_fim_vif = base_vif %>% dplyr::select(resposta,colnames(base_vif)[round(sum_test_vif$coefficients[,4],6)<0.05])

select = subset(base_modmult_fim_vif,select = c(nu_dias_atraso,
                                                valor_creliq,
                                                qtd_parcelamento_12m,
                                                qtd_fx0_geral,
                                                qtd_fx1_geral,
                                                qtd_cpc_1m,
                                                qtd_cpc_6m,
                                                qtd_acionamento_3m,
                                                resposta))

library(bestglm)
bestmod = bestglm(Xy = select,family = binomial,IC = "AIC",
                  method = "exhaustive", TopModels = 10, RequireFullEnumerationQ = T)

summary.bestglm(bestmod)
summary.glm(bestmod$BestModel)

# tabela dos modelos testados
bestmod$BestModels

mod_mult_bestglm = glm(formula = resposta ~ .,data = base_modmult_fim_vif,family = binomial(link="logit"))
sum_modmult_bestglm = summary(mod_mult_bestglm)

#### teste hosmer e lemeshow ####
library(ResourceSelection)
hl = hoslem.test(mod_mult_bestglm$y,fitted(mod_mult_bestglm), g = 15)

round(cbind(hl$observed,hl$expected),2)

##### analise de residuos #####



fit.model = mod_mult_bestglm

save(fit.model,file = "Scripts/model.Rdata")

source("Scripts/diag_bino.R")

source("Scripts/envel_bino.R")

### curva roc #####
library(ROSE)
curva_roc = roc.curve(base_modmult_fim_vif$resposta,
          mod_mult_bestglm$fitted.values,
          ylab = "Taxa de verdadeiros positivos",
          xlab = "Taxa de falsos positivos")
curva_roc$auc

library(caret)

pdata <- predict(mod_mult_bestglm, newdata = base_modmult_fim_vif, type = "response")


confusionMatrix(table(data = as.numeric(pdata>0.5), reference = base_modmult_fim_vif$resposta),positive = '1')

################################################################################################################
################################ treino e teste ################################################################
################################################################################################################

base_modelo_rmna_amostra_train$resposta = as.factor(base_modelo_rmna_amostra_train$resposta)
### vendo a relacao da variavel resposta com as variaveis explicativas induvidualmente

names_colunas = colnames(base_modelo_rmna_amostra_train)

for (i in 2:length(names_colunas)) {
  assign(paste0("mod_",names_colunas[i]), glm(formula = resposta ~ base_modelo_rmna_amostra_train[,i],data = base_modelo_rmna_amostra_train ,family = binomial(link="logit")))
  
  #assign(paste0("mod_",names_colunas[i]), lm(formula = as.numeric(resposta) ~ base_modelo_rmna_amostra_train[,i],data = base_modelo_rmna_amostra_train))
  
  a = summary(get(paste0("mod_",names_colunas[i])))
  
  base_testes =  data_frame(`Variáveis` = names_colunas[i],
                            Estimativa = round(a$coefficients[2,1],6),
                            `Erro-padrão` = round(a$coefficients[2,2],6),
                            `Estatística z` = round(a$coefficients[2,3],6),
                            `Valor-p` = round(a$coefficients[2,4],6),
                            Significante = ifelse(round(a$coefficients[2,4],6)>0.05,"Não","Sim"))
  
  if(i == 2){
    base_testes_fim = base_testes
  }else{
    base_testes_fim = rbind(base_testes_fim,base_testes)
  }
  
}

save(base_testes_fim,file = "Scripts/teste_reg_log_sim.Rdata")

## selecionando variaevis de significante ####

variaveis_selec = base_testes_fim %>% dplyr::filter(Significante == "Sim") %>% dplyr::select(`Variáveis`)

base_modelo_mult = base_modelo_rmna_amostra_train %>% select(resposta,as.vector(variaveis_selec$Variáveis))

### gerando modelo multiplo #####

mod_mult = glm(formula = resposta ~ .,data = base_modelo_mult,family = binomial(link="logit"))
sum_modmult = summary(mod_mult)

base_modmult_fim = base_modelo_mult %>% dplyr::select(colnames(base_modelo_mult)[round(sum_modmult$coefficients[,4],6)<0.05])
library(car)
options("scipen"=100, "digits"=4)
sort(vif(mod_mult))
ld.vars <- attributes(alias(mod_mult)$Complete)$dimnames[[1]]

test_vif = glm(formula = resposta ~ .,data = base_modelo_mult[,!(colnames(base_modelo_mult) %in% c("divida_atual",
                                                                                                   "qtd_parcelamento_6m",
                                                                                                   "qtd_fx1_6m"))],family = binomial(link="logit"))
sum_test_vif = summary(test_vif)
sort(vif(test_vif))

base_vif = base_modelo_mult[,!(colnames(base_modelo_mult) %in% c("divida_atual",
                                                                 "qtd_parcelamento_6m",
                                                                 "qtd_fx1_6m"))]

base_modmult_fim_vif = base_vif %>% dplyr::select(resposta,colnames(base_vif)[round(sum_test_vif$coefficients[,4],6)<0.05])

select = subset(base_modmult_fim_vif,select = c(nu_dias_atraso,
                                                valor_creliq,
                                                qtd_parcelamento_12m,
                                                qtd_fx1_geral,
                                                qtd_cpc_1m,
                                                qtd_cpc_6m,
                                                resposta))

library(bestglm)
bestmod = bestglm(Xy = select,family = binomial,IC = "AIC",
                  method = "exhaustive", TopModels = 10, RequireFullEnumerationQ = T)

summary.bestglm(bestmod)
summary.glm(bestmod$BestModel)

# tabela dos modelos testados
bm = bestmod$BestModels
save(bm,file = "Scripts/bestglm.Rdata")


mod_mult_bestglm$call

mod_mult_bestglm = glm(formula = resposta ~ .,data = base_modmult_fim_vif,family = binomial(link="logit"))
sum_modmult_bestglm = summary(mod_mult_bestglm)

#### teste hosmer e lemeshow ####
library(ResourceSelection)
hl = hoslem.test(mod_mult_bestglm$y,fitted(mod_mult_bestglm), g = 15)

round(cbind(hl$observed,hl$expected),2)

##### analise de residuos #####



fit.model = mod_mult_bestglm

save(fit.model,file = "Scripts/model.Rdata")

### curva roc #####
library(ROSE)
curva_roc = roc.curve(base_modmult_fim_vif$resposta,
                      mod_mult_bestglm$fitted.values,
                      ylab = "Taxa de verdadeiros positivos",
                      xlab = "Taxa de falsos positivos")
curva_roc$auc

library(caret)

pdata_train <- predict(mod_mult_bestglm, newdata = base_modmult_fim_vif[,-1], type = "response")
cm = confusionMatrix(table(data = as.numeric(pdata_train>0.5), reference = base_modmult_fim_vif$resposta),positive = '1')
save(cm,file = "Scripts/cm_train.Rdata")

###### teste ############

test = base_modelo_rmna_amostra_test %>% dplyr::select(colnames(mod_mult_bestglm$model))

pdata_test <- predict(mod_mult_bestglm, newdata = test[,-1], type = "response")
confusionMatrix(table(data = as.numeric(pdata_test>0.5), reference = test$resposta),positive = '1')

ks.test(pdata_train,pdata_test)

