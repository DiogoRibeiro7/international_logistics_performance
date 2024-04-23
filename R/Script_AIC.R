#importar dados
library(readxl)
dados2023 <- read_excel("International_LPI_from_2007_to_2023.xlsx",
                        sheet = "2023")
View(dados2023)

dados_ACP <- subset(dados2023, select = -c("LPI Grouped Rank", "Customs Grouped Rank","Infrastructure Grouped Rank", "International Shipments Grouped Rank", "Logistics Competence and Quality Grouped Rank", "Timeliness Grouped Rank", "Tracking and Tracing Grouped Rank"))
#coluna 4: "Customs Score"
#6: "Infrastructure Score", 8"International Shipments Score",
#10"Logistics Competence and Quality Score",
#12"Timeliness Score", 14"Tracking and Tracing Score"))
View(dados_ACP)
mydata<-dados_ACP
#-------------------
#Pressupostos
#-------------------
#Normalidade multivarida
#-------------------------
#Mardia's Test
#𝐻_0:As variáveis seguem uma distribuição normal mutivariada
#install.packages("MVN")
library(MVN)
#install.packages("QuantPsyc")
library(QuantPsyc)
mult.norm(mydata)

# Beta-hat      kappa     p-val
# Skewness  2.496964 57.8463412 0.4069266
# Kurtosis 49.162767  0.6995755 0.4841924
#Uma vez que ambos os valores de p são maiores que 0,05,
#não rejeitamos a hipótese nula do teste.
#Temos portanto evidências para dizer que as 6
#variáveis no nosso conjunto de dados
# seguem uma distribuição normal multivariada.

#Energy Test
#install.packages("energy")
library(energy)

dim(mydata)
mvnorm.etest(mydata,R=100)
#como o valor de prova é p-value < 2.2e-16 inferior a 5%
#Não temos evidências estatísticas para considerar a
#normalidade multivarida

#2)Linearidade - As variaveis estao correlacionadas
#(o coeficiente de  correlacao linear, de Pearson, é significativo)

#matriz de correlacoes
#e níveis de significância
#install.packages("Hmisc")
library(Hmisc)
matrix_corr<-rcorr(as.matrix(mydata))
matrix_corr
#As variáveis têm todas correlações significativas

#esboço de correlações - parece haver correlações significativas
#install.packages("corrplot")
library(corrplot)
res <- cor(mydata)
corrplot(res, type = "upper", #order = "hclust",
         tl.col = "black", tl.srt = 45)

# Use ggcorrplot() to explore the correlation matrix
#representar graficamente a matrix de correla??es
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(res)


#3) Homocedasticidade - A variacia é homogenea para as variaveis
#variâncias
var(mydata)
sapply(mydata,var,na.rm=TRUE)
#Desvios-padrão
sapply(mydata,sd,na.rm=TRUE)
#ou
sqrt(var(mydata))
# Customs Score                   Infrastructure Score          International Shipments Score
# 0.6249638                              0.7207849                              0.5235184
# Logistics Competence and Quality Score                       Timeliness Score             Tracking and Tracing Score
# 0.6456369                              0.5650418                              0.6754888
#todos muito proximos de 1, o que sugere a homogeneidade dos dados

#podiam fazer-se testes de variância 2 a 2
#H0: as variancias sao iguais
#var.test(mydata$x1,mydata$x2)
#p-value = 0.3239>5% nao se rejeita a H0

###########################
#       Aplicar ACP       #
##########################

#Averiguar a adequabilidade da amostra para aplicar a ACP
#-------------_______________--------------------------------
#Kaiser-Meyer-Olkin factor adequacy
#install.packages("psych")
library(psych)
KMO(mydata)

# Kaiser-Meyer-Olkin factor adequacy
# Call: KMO(r = mydata)
# Overall MSA =  0.93
# MSA for each item =
# Customs Score    Infrastructure Score          International Shipments Score
# 0.91             0.90                         0.97
# Logistics Competence and Quality Score  0.93
# Timeliness Score             Tracking and Tracing Score
# 0.94                                   0.92
#KMO =Overall MSA =  0.93 Excelente

#ja e adequado usar AF/ACP para estas variaveis

#Se algumas das MSA forem inferiores a 0,5
#as variaveis devem ser retiradas da analise
#1 por 1 come,cando por retirar a que tiver menor MSA
#até nao haver nenhuma MSA <0,5

#eliminar X5, porque ? o que tem menor MSA individual
#View(mydata)
#mydata$x5<-NULL



#bartlett.test
#Teste de esfericidade de Bartlett
#H0 = a matriz de correla??o ? igual ? matriz identidade
bartlett.test(mydata)
#	Bartlett test of homogeneity of variances
#data:  mydata
#Bartlett's K-squared = 18.459, df = 5, p-value = 0.002423
# p-value <0.002423<<5%, logo rejeita-se H0,
#Ou seja, h? evid?ncia estat?tsica para dizer que
#a matriz de correla??es ? signitivamente diferente da matriz identidade
#justificando a utiliza??o desta t?cnica


#####################################
#  Pricipal Components Analysis     #
####################################
#on the given numeric data matrix
#returns the results as an object of class princomp.
?princomp
#just for numeric variables

fit <- princomp(mydata, cor=TRUE)#,scale = TRUE)
#neste caso n?o precisamos do scale porque
#havia homogeneidade dos dados

#imprimir resumo das compenentes
fit

summary(fit) # print variance accounted for fit
#From the output we can see that 92.4% of the variation in the dataset
#is explained by the first component alone, and 97.8% is explained by the first two components.

#Explorar os dados no fit
fit$sdev
#valores dos a11,a12,...., app
fit$loadings
#valores dos Ys para as 100 oservações
fit$scores

#juntar os scores à BD
mydata2<-cbind(mydata,fit$scores)
View(mydata2)

#Variancia é decescente nas componentes
sd(mydata2$Comp.1)
sd(mydata2$Comp.2)
sd(mydata2$Comp.3)
sd(mydata2$Comp.4)
sd(mydata2$Comp.5)

#variância sem qualquer ordem nas variaveis originais
sd(mydata$x1)
sd(mydata$x2)
sd(mydata$x3)
sd(mydata2$x4)
sd(mydata2$x6)

#este processo e um processo de
#feature selection - usualmente considera-se comp 1,
#a que tem maior variancia, ou seja a primeira

#Para fazer feature extration - considerar apenas algumas

#How much components to retain?
#--------Varios criterios-----------------------------
plot(fit,type="lines")
# scree plot com a reprsenta??o das vari?ncias
#das componentes
#1) Identificar o #cotovelo"
#este crit?rio sugere a reten??o de 1 componentes

#2) reter os componentes com valor proprio >1

#or npcs = 6-> n~de componentes a representar
screeplot(fit, type = "l", npcs = 6, main = "Screeplot of the first 6 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
#por este criterio so retinha 1 componente

#ou
#-------------
#nova matriz de correla??es
matriz<-cor(mydata)
matriz

# Calculate eigenvalues & eigenvectors
fit.cov <- matriz
fit.eigen <- eigen(fit.cov)
fit.eigen[1]
aux=fit.eigen[1]
#Determine Number of Factors to Extract
aux$values>1
#eigenvalues>1 =number of factors to retain
#neste caso 1

#3)Comulative variance proportion >80%
par(mar=c(2,2,2,2))

cumpro <- cumsum(fit$sdev^2 / sum(fit$sdev^2))
plot(cumpro[0:6], xlab = "PC #",
     ylab = "Amount of explained variance",
     main = "Cumulative variance plot")
abline(h = 0.8, col="blue", lty=5)
abline(v = 2, col="blue", lty=5)
legend("topleft", legend=c("Cut-off 80% @ PC1"),
       col=c("blue"), lty=5, cex=0.6)

#para uma vari?ncia superior a 80%
# o grafico sugere 1 fatores


names(fit)
#"sdev"     "loadings" "center"   "scale"    "n.obs"
# "scores"   "call"

loadings(fit) # pc loadings

fit$scores # the principal components
biplot(fit) #so para 2 componentes

##############################################
## Analise Fatorial - feature extration      #
##############################################

#Varimax Rotated Principal Components
library(psych)
?principal
fit2 <- principal(mydata, nfactors=1, rotate="varimax")
fit2

summary(fit2)
print(fit2,digits=7)

#                                          PC1        h2
# Customs Score                          0.9578735 0.9175216
# Infrastructure Score                   0.9643776 0.9300241
# International Shipments Score          0.9146187 0.8365274
# Logistics Competence and Quality Score 0.9728222 0.9463830
# Timeliness Score                       0.9387521 0.8812555
# Tracking and Tracing Score             0.9669542 0.9350004

#aqui vê-se o q o sr perguntou!!!
#Logistics Competence and Quality Score
#é o mais importante


fit2$communality #deviam ser todas >0,5
#quantidade da variância total da variável original explicada pelos fatores comuns


#valores das novas vari?veis
fit2$scores

cor(fit2$scores[,1],fit2$scores[,2])
#-4.388343e-16
#eliminamos a multicolinearidade
#criando 2 vari?veis "novas"- 2 fatores


####################################################
#  Fiabilidade da Medida ou Consist?ncia Interna   #
####################################################
#Alpha de Cronbach
#install.packages("ltm")
library(ltm)

#enter survey responses as a data frame
F1 <- data.frame(mydata$`Customs Score`,
                 mydata$`Infrastructure Score`,
                 mydata$`International Shipments Score`,
                 mydata$`Logistics Competence and Quality Score`,
                 mydata$`Timeliness Score`,
                 mydata$`Tracking and Tracing Score`)
#calculate Cronbach's Alpha
cronbach.alpha(F1)
#0.978

#Frequentist Individual Item Reliability Statistics
###################################################
#tirando "Customs Score"
F1_CS <- data.frame(mydata$`Infrastructure Score`,
                    mydata$`International Shipments Score`,
                    mydata$`Logistics Competence and Quality Score`,
                    mydata$`Timeliness Score`,
                    mydata$`Tracking and Tracing Score`)
#calculate Cronbach's Alpha
cronbach.alpha(F1_CS)
#0.972

#Tirando "Infrastructure Score",
F1_IS <- data.frame(mydata$`Customs Score`,
                    mydata$`International Shipments Score`,
                    mydata$`Logistics Competence and Quality Score`,
                    mydata$`Timeliness Score`,
                    mydata$`Tracking and Tracing Score`)
#calculate Cronbach's Alpha
cronbach.alpha(F1_IS)
#0.973

#tirando "International Shipments Score"
F1_ISS <- data.frame(mydata$`Customs Score`,
                     mydata$`Infrastructure Score`,
                     mydata$`Logistics Competence and Quality Score`,
                     mydata$`Timeliness Score`,
                     mydata$`Tracking and Tracing Score`)
#calculate Cronbach's Alpha
cronbach.alpha(F1_ISS)

#tirando "Logistics Competence and Quality Score",
F1_LCQS <- data.frame(mydata$`Customs Score`,
                      mydata$`Infrastructure Score`,
                      mydata$`International Shipments Score`,
                      mydata$`Timeliness Score`,
                      mydata$`Tracking and Tracing Score`)
#calculate Cronbach's Alpha
cronbach.alpha(F1_LCQS)

#tirando "Timeliness Score",
F1_TS <- data.frame(mydata$`Customs Score`,
                    mydata$`Infrastructure Score`,
                    mydata$`International Shipments Score`,
                    mydata$`Logistics Competence and Quality Score`,
                    mydata$`Tracking and Tracing Score`)
#calculate Cronbach's Alpha
cronbach.alpha(F1_TS)

#tirando"Tracking and Tracing Score"
F1_TTS <- data.frame(mydata$`Customs Score`,
                     mydata$`Infrastructure Score`,
                     mydata$`International Shipments Score`,
                     mydata$`Logistics Competence and Quality Score`,
                     mydata$`Timeliness Score`)
#calculate Cronbach's Alpha
cronbach.alpha(F1_TTS)
