#BRUGBARE DATAFRAMES
load("/Users/maikenbuje/Desktop/Mat5/cityhomes.Rda")

#Laver en dataframe med alle variable, baade raa data og logaritmetransformeret data.
data.uden.log.na <- data.frame(Pris_Salg = cityhomes$Pris_Salg, Areal_Bolig = cityhomes$Areal_Bolig, Areal_Grund = cityhomes$Areal_Grund, Antal_Rum = cityhomes$Ejd_AntalRum, Salgsmaaned = cityhomes$Salgsmaaned, Salgsaar = cityhomes$Salgsaar, Stor_Grund = cityhomes$StorGrund, Afstemningsomraade = cityhomes$Afstemningsomraade, Sogn = cityhomes$Sogn, Afstand_Skole = cityhomes$Dist_skole, Afstand_Raadhus = cityhomes$Dist_raadhus, Hus_Alder = cityhomes$Alder, Kommune = cityhomes$KommuneNavn, Velhavende = cityhomes$Velhavende, Trend = cityhomes$Trend)
data.na.alt <- data.frame(lPris_Salg = log(cityhomes$Pris_Salg), lAreal_Bolig = log(cityhomes$Areal_Bolig), lAreal_Grund = log(cityhomes$Areal_Grund), Antal_Rum = cityhomes$Ejd_AntalRum, Salgsmaaned = cityhomes$Salgsmaaned, Salgsaar = cityhomes$Salgsaar, Stor_Grund = cityhomes$StorGrund, Afstemningsomraade = cityhomes$Afstemningsomraade, Velhavende = cityhomes$Velhavende, Sogn = cityhomes$Sogn, Afstand_Skole = cityhomes$Dist_skole, Afstand_Raadhus =cityhomes$Dist_raadhus, Hus_Alder = cityhomes$Alder, Kommune = cityhomes$KommuneNavn, Trend = cityhomes$Trend)

#Fjerner de raekker, der indeholder NA'er.
data.uden.log <- na.omit(data.uden.log.na)
data.alt <- na.omit(data.na.alt)

#Laver en dataframe for hver af de fire kommuner, baade raa data og logaritmetransformeret data.
data_aalborg <- subset(data.alt, Kommune == "Aalborg")
data_aarhus <- subset(data.alt, Kommune == "Aarhus")
data_odense <- subset(data.alt, Kommune == "Odense")
data_koebenhavn <- subset(data.alt, Kommune == "koebenhavn")
data_aalborg.udenlog <- subset(data.uden.log, Kommune == "Aalborg")
data_aarhus.udenlog <- subset(data.uden.log, Kommune == "Aarhus")
data_odense.udenlog <- subset(data.uden.log, Kommune == "Odense")
data_koebenhavn.udenlog <- subset(data.uden.log, Kommune == "koebenhavn")

#Laver dataframes hvor kun to af kommunerne indgaar
data_aalborg_aarhus <- subset(data.alt, Kommune == "Aalborg" | Kommune == "Aarhus")
data_aalborg_odense <- subset(data.alt, Kommune == "Aalborg" | Kommune == "Odense")
data_aalborg_koebenhavn <- subset(data.alt, Kommune == "Aalborg" | Kommune == "koebenhavn")
data_aarhus_odense <- subset(data.alt, Kommune == "Aarhus" | Kommune == "Odense")
data_aarhus_koebenhavn <- subset(data.alt, Kommune == "Aarhus" | Kommune == "koebenhavn")
data_odense_koebenhavn <- subset(data.alt, Kommune == "Odense" | Kommune == "koebenhavn")

#UDREGNER KORRELATION MELLEM DE FORSKELLIGE VARIABLE OG TESTER FOR MULTIKOLINEARITET
#Korrelationsmatrix
cor(data.alt$lAreal_Bolig, data.alt$lAreal_Grund)
cor(data.alt$lAreal_Bolig, data.alt$Antal_Rum)
cor(data.alt$lAreal_Bolig, data.alt$Trend)
cor(data.alt$lAreal_Bolig, data.alt$Afstand_Skole)
cor(data.alt$lAreal_Bolig, data.alt$Afstand_Raadhus)
cor(data.alt$lAreal_Bolig, data.alt$Hus_Alder)
cor(data.alt$Antal_Rum, data.alt$lAreal_Grund)
cor(data.alt$Antal_Rum, data.alt$Trend)
cor(data.alt$Antal_Rum, data.alt$Afstand_Skole)
cor(data.alt$Antal_Rum, data.alt$Afstand_Raadhus)
cor(data.alt$Antal_Rum, data.alt$Hus_Alder)
cor(data.alt$Afstand_Skole, data.alt$lAreal_Grund)
cor(data.alt$Afstand_Skole, data.alt$Trend)
cor(data.alt$Afstand_Skole, data.alt$Afstand_Raadhus)
cor(data.alt$Afstand_Skole, data.alt$Hus_Alder)
cor(data.alt$lAreal_Grund, data.alt$Trend)
cor(data.alt$lAreal_Grund, data.alt$Afstand_Raadhus)
cor(data.alt$lAreal_Grund, data.alt$Hus_Alder)
cor(data.alt$Trend, data.alt$Afstand_Raadhus)
cor(data.alt$Trend, data.alt$Hus_Alder)
cor(data.alt$Afstand_Raadhus, data.alt$Hus_Alder)

#Laver en lineaer model for hver forklarende variabel ud fra de resterende forklarende variable, for at tjekke for multikolinearitet
Multikolin_lAreal_Bolig <- lm(lAreal_Bolig ~ lAreal_Grund + Antal_Rum + Salgsmaaned + Trend + Stor_Grund + Sogn + Afstand_Skole + Afstand_Raadhus + Hus_Alder, data = data.alt)
summary(Multikolin_lAreal_Bolig) #R2=0.64
Multikolin_lAreal_Grund <- lm(lAreal_Grund ~ lAreal_Bolig + Antal_Rum + Salgsmaaned + Trend + Stor_Grund + Sogn + Afstand_Skole + Afstand_Raadhus + Hus_Alder, data = data.alt)
summary(Multikolin_lAreal_Grund) #R2=0.53
Multikolin_Antal_Rum <- lm(Antal_Rum ~ lAreal_Bolig + lAreal_Grund + Salgsmaaned + Trend + Stor_Grund + Sogn + Afstand_Skole + Afstand_Raadhus + Hus_Alder, data = data.alt)
summary(Multikolin_Antal_Rum) #R2=0.58
Multikolin_Trend <- lm(Trend ~ lAreal_Bolig + lAreal_Grund + Antal_Rum + Salgsmaaned + Stor_Grund + Sogn + Afstand_Skole + Afstand_Raadhus + Hus_Alder, data = data.alt)
summary(Multikolin_Trend) #R2=0.10 
Mulyikolin_Afstand_Skole <- lm(Afstand_Skole ~ lAreal_Bolig + lAreal_Grund + Antal_Rum + Salgsmaaned + Trend + Stor_Grund + Sogn + Afstand_Raadhus + Hus_Alder, data = data.alt)
summary(Multikolin_Afstand_Skole) #R2=0.37
Multikolin_Afstand_Raadhus <- lm(Afstand_Raadhus ~ lAreal_Bolig + lAreal_Grund + Antal_Rum + Salgsmaaned + Trend + Stor_Grund + Sogn + Afstand_Skole + Hus_Alder, data = data.alt)
summary(Multikolin_Afstand_Raadhus) #R2=0.81
Multikolin_Hus_Alder <- lm(Hus_Alder ~ lAreal_Bolig + lAreal_Grund + Antal_Rum + Salgsmaaned + Trend + Stor_Grund + Sogn + Afstand_Skole + Afstand_Raadhus, data = data.alt)
summary(Multikolin_Hus_Alder) #R2=0.32


#REDUCERER MODELLEN MED ALLE VARIABLE UNDTAGEN VELHAVENDE, AFSTEMNINGSOMRAADE OG KOMMUNE
#Laver referencen til sognet Vollsmose
data.alt$Sogn <- factor(data.alt$Sogn, ordered = FALSE)
data.alt$Sogn <- relevel(data.alt$Sogn, ref = "Vollsmose")

#F-test for at reducere modellen
#Laver en lineaer model med alt undtagen afstemningsomraade, kommune og velhavende og tester derefter om de forskellige variable kan fjernes ved F-test
model_alle <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Antal_Rum + Salgsmaaned + Trend + Stor_Grund + Afstand_Skole + Afstand_Raadhus + Hus_Alder + Sogn, data = data.alt)
summary(model_alle)

model_uden_lAreal_Bolig <- lm(lPris_Salg ~ lAreal_Grund + Antal_Rum + Salgsmaaned + Trend + Stor_Grund + Afstand_Skole + Afstand_Raadhus + Hus_Alder + Sogn, data = data.alt)
p_lAreal_Bolig <- anova(model_alle, model_uden_lAreal_Bolig)
print(p_lAreal_Bolig) #kan ikke fjernes

model_uden_lAreal_Grund <- lm(lPris_Salg ~ lAreal_Bolig + Antal_Rum + Salgsmaaned + Trend + Stor_Grund + Afstand_Skole + Afstand_Raadhus + Hus_Alder +  Sogn, data = data.alt)
p_lAreal_Grund <- anova(model_alle, model_uden_lAreal_Grund)
print(p_lAreal_Grund) # kan ikke fjernes

model_uden_Antal_Rum <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Salgsmaaned + Trend + Stor_Grund + Afstand_Skole + Afstand_Raadhus + Hus_Alder +  Sogn, data = data.alt)
p_Antal_Rum <- anova(model_alle, model_uden_Antal_Rum)
print(p_Antal_Rum) # kan fjernes

model_uden_Salgsmaaned <-  lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund + Afstand_Skole + Afstand_Raadhus + Hus_Alder +  Sogn, data = data.alt)
p_Salgsmaaned <- anova(model_uden_Antal_Rum, model_uden_Salgsmaaned)
print(p_Salgsmaaned) # kan fjernes

model_uden_Trend <-  lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Stor_Grund + Afstand_Skole + Afstand_Raadhus + Hus_Alder +  Sogn, data = data.alt)
p_Trend <- anova(model_uden_Salgsmaaned, model_uden_Trend)
print(p_Trend) # kan ikke fjernes

model_uden_Storgrund <-  lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Afstand_Skole + Afstand_Raadhus + Hus_Alder +  Sogn, data = data.alt)
p_Storgrund <- anova(model_uden_Salgsmaaned, model_uden_Storgrund)
print(p_Storgrund) # kan ikke fjernes

model_uden_Sogn <-  lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Afstand_Skole + Afstand_Raadhus + Hus_Alder, data = data.alt)
p_Sogn <- anova(model_uden_Storgrund, model_uden_Sogn)
print(p_Sogn) # kan ikke fjernes

model_uden_Afstand_Skole <-  lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend +  Sogn + Afstand_Raadhus + Hus_Alder, data = data.alt)
p_Afstand_Skole <- anova(model_uden_Storgrund, model_uden_Afstand_Skole)
print(p_Afstand_Skole) # kan fjernes

model_uden_Afstand_Raadhus <-  lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend +  Sogn + Hus_Alder, data = data.alt)
p_Afstand_Raadhus <- anova(model_uden_Afstand_Skole, model_uden_Afstand_Raadhus)
print(p_Afstand_Raadhus) # kan ikke fjernes

model_uden_Hus_Alder <-  lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund +  Trend +  Sogn +  Afstand_Raadhus, data = data.alt)
p_Hus_Alder <- anova(model_uden_Afstand_Skole, model_uden_Hus_Alder)
print(p_Hus_Alder) #kan ikke fjernes

#Dermed faas den reducerede model
reduceret_model <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund +  Sogn + Afstand_Raadhus + Hus_Alder, data = data.alt)
summary(reduceret_model)

#AFVIGELSE MELLEM PRAEDIKTIONER FOR DEN REDUCEREDE MODEL OG DATA, SAMT PRAEDIKTION FOR EN NY OBSERVATION
#Laver praediktioner for den reducerede model og beregner afvigelsen mellem data og praediktionerne
praediktion_reduceret_model <- predict(reduceret_model)
print(praediktion_reduceret_model)
afvigelse_reduceret_model <- data.frame(afvigelse = data.alt$lPris_Salg-praediktion_reduceret_model)

#Forsoger at fjerne hus nr. 627 fra den reducerede model
data.uden627 <- data.alt[-c(627),]
reduceret_model.uden627 <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund +  Sogn + Afstand_Raadhus + Hus_Alder, data = data.uden627)
summary(reduceret_model.uden627) #Fjernes ikke

#Beregner en praediktion samt praediktionsinterval for den reducerede model
x_nplus1 <- data.frame(lAreal_Bolig=c(log(126)), lAreal_Grund=c(log(798)), Trend=c(12), Stor_Grund=c(0), Sogn=c("Hasseris"), Afstand_Raadhus=c(4), Hus_Alder=c(54))
praediktion_reduceret <- predict(reduceret_model, newdata=x_nplus1, interval="prediction")
print(exp(praediktion_reduceret)) #fit=3496226 lwr=2219965 upr=5506212


#UNDERSOGER UDVIKLING I KVADRATMETERPRIS KOMMUNERNE IMELLEM
#Laver en lineaer model for hver kommune for at kunne lave residualplots og dermed tjekke for varianshomogenitet
reduceret_model_aalborg <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Antal_Rum + Sogn + Trend + Afstand_Skole + Afstand_Raadhus + Hus_Alder, data=data_aalborg)
summary(reduceret_model_aalborg)
reduceret_model_aarhus <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Antal_Rum + Sogn + Trend + Afstand_Skole + Afstand_Raadhus + Hus_Alder, data=data_aarhus)
summary(reduceret_model_aarhus)
reduceret_model_odense <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Antal_Rum + Sogn + Trend + Afstand_Skole + Afstand_Raadhus + Hus_Alder, data=data_odense)
summary(reduceret_model_odense)
reduceret_model_koebenhavn <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Antal_Rum + Sogn + Trend + Afstand_Skole + Afstand_Raadhus + Hus_Alder, data=data_koebenhavn)
summary(reduceret_model_koebenhavn)

#Beregner gennemsnit og standardafvigelser for residualerne til modellerne for hver kommune
mean(reduceret_model_aalborg$residuals)
mean(reduceret_model_aarhus$residuals)
mean(reduceret_model_odense$residuals)
mean(reduceret_model_koebenhavn$residuals)
sd(reduceret_model_aalborg$residuals)
sd(reduceret_model_aarhus$residuals)
sd(reduceret_model_odense$residuals)
sd(reduceret_model_koebenhavn$residuals)

#Beregner den gennemsnitlige boligkvadratmeterpris og standardafvigelse for hver kommune pr. aar
kvmprisaalborg <- data.frame(kvmpris = data_aalborg.udenlog$Pris_Salg/data_aalborg.udenlog$Areal_Bolig, Salgsaar = data_aalborg.udenlog$Salgsaar) %>%
  group_by(Salgsaar) %>%
  summarise_at(vars(kvmpris),
               list(mean = mean,
                    sd = sd)) %>%
  as.data.frame()
kvmprisaalborg

kvmprisaarhus <- data.frame(kvmpris = data_aarhus.udenlog$Pris_Salg/data_aarhus.udenlog$Areal_Bolig, Salgsaar = data_aarhus.udenlog$Salgsaar) %>%
  group_by(Salgsaar) %>%
  summarise_at(vars(kvmpris),
               list(mean = mean,
                    sd = sd)) %>%
  as.data.frame()
kvmprisaarhus

kvmprisodense <- data.frame(kvmpris = data_odense.udenlog$Pris_Salg/data_odense.udenlog$Areal_Bolig, Salgsaar = data_odense.udenlog$Salgsaar) %>%
  group_by(Salgsaar) %>%
  summarise_at(vars(kvmpris),
               list(mean = mean,
                    sd = sd)) %>%
  as.data.frame()
kvmprisodense

kvmpriskoebenhavn <- data.frame(kvmpris = data_koebenhavn.udenlog$Pris_Salg/data_koebenhavn.udenlog$Areal_Bolig, Salgsaar = data_koebenhavn.udenlog$Salgsaar) %>%
  group_by(Salgsaar) %>%
  summarise_at(vars(kvmpris),
               list(mean = mean,
                    sd = sd)) %>%
  as.data.frame()
kvmpriskoebenhavn

#Laver en datafrane med de gennemsnitlige boligkvadratmeterpriser og standardafvigelser og plotter det
kvmpris <- data.frame(Salgsaar = kvmprisaalborg$Salgsaar, kvmprisall = kvmprisaalborg$mean, kvmprisaar = kvmprisaarhus$mean, kvmprisod = kvmprisodense$mean, kvmpriskbh = kvmpriskoebenhavn$mean)

#Test om udviklingen kvadratmeterprisen er signifikant forskellig i de fire kommuner
reduceret_model_kvmpris <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Trend + Velhavende + Afstand_Raadhus + Hus_Alder, data = data.alt)
summary(reduceret_model_KommuneTrend)
model_uden_KommuneTrend <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data.alt)
p_KommuneTrend <- anova(reduceret_model_kvmpris, model_uden_KommuneTrend)
print(p_KommuneTrend) #p<0.05 og den kan dermed ikke fjernes

#Tjekker om Kommune*Trend (betydning af stigning i kvadratmeterpris) kan fjernes ved F-test i modellerne hvor der kun indgaar to kommuner ad gangen
model_aalborg_aarhus_kvmpris <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Trend + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_aarhus)
model_aalborg_aarhus_uden_KommuneTrend <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_aarhus)
p_aalborg_aarhus_KommuneTrend <- anova(model_aalborg_aarhus_kvmpris, model_aalborg_aarhus_uden_KommuneTrend)
print(p_aalborg_aarhus_KommuneTrend) #p>0.05 og den kan dermed fjernes

model_aalborg_odense_kvmpris <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Trend + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_odense)
model_aalborg_odense_uden_KommuneTrend <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_odense)
p_aalborg_odense_KommuneTrend <- anova(model_aalborg_odense_kvmpris, model_aalborg_odense_uden_KommuneTrend)
print(p_aalborg_odense_KommuneTrend) #p<0.05 og den kan dermed ikke fjernes

model_aalborg_koebenhavn_kvmpris <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Trend + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_koebenhavn)
model_aalborg_koebenhavn_uden_KommuneTrend <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_koebenhavn)
p_aalborg_koebenhavn_KommuneTrend <- anova(model_aalborg_koebenhavn_kvmpris, model_aalborg_koebenhavn_uden_KommuneTrend)
print(p_aalborg_koebenhavn_KommuneTrend) #p<0.05 og den kan dermed ikke fjernes

model_aarhus_odense_kvmpris <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Trend + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_odense)
model_aarhus_odense_uden_KommuneTrend <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_odense)
p_aarhus_odense_KommuneTrend <- anova(model_aarhus_odense_kvmpris, model_aarhus_odense_uden_KommuneTrend)
print(p_aarhus_odense_KommuneTrend) #p<0.05 og den kan dermed ikke fjernes

model_aarhus_koebenhavn_kvmpris <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Trend + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_koebenhavn)
model_aarhus_koebenhavn_uden_KommuneTrend <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_koebenhavn)
p_aarhus_koebenhavn_KommuneTrend <- anova(model_aarhus_koebenhavn_kvmpris, model_aarhus_koebenhavn_uden_KommuneTrend)
print(p_aarhus_koebenhavn_KommuneTrend) #p<0.05 og den kan dermed ikke fjernes

model_odense_koebenhavn_kvmpris <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Trend + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_odense_koebenhavn)
model_odense_koebenhavn_uden_KommuneTrend <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_odense_koebenhavn)
p_odense_koebenhavn_KommuneTrend <- anova(model_odense_koebenhavn_kvmpris, model_odense_koebenhavn_uden_KommuneTrend)
print(p_odense_koebenhavn_KommuneTrend) #p>0.05 og den kan dermed fjernes

#UNDERSOGER BETYDNINGEN AF CORONA
#Test om Corona har en signifikant indvirkning paa husprisen. 
#Lav dataframe med en ekstra dummyvariabel der antager vaerdien 1 naar huset er solgt i aar 2020, 2021 eller 2022
data.alt_covid <- data.frame(lPris_Salg = data.alt$lPris_Salg, lAreal_Bolig = data.alt$lAreal_Bolig, lAreal_Grund = data.alt$lAreal_Grund, Antal_Rum = data.alt$Antal_Rum, Salgsmaaned = data.alt$Salgsmaaned, Salgsaar = data.alt$Salgsaar, Stor_Grund = data.alt$Stor_Grund, Afstemningsomraade = data.alt$Afstemningsomraade, Velhavende = data.alt$Velhavende, Sogn = data.alt$Sogn, Afstand_Skole = data.alt$Afstand_Skole, Afstand_Raadhus = data.alt$Afstand_Raadhus, Hus_Alder = data.alt$Hus_Alder, Kommune = data.alt$Kommune, Trend = data.alt$Trend, Covid = c(rep(0,863)))
data.alt_covid["Covid"][data.alt_covid["Salgsaar"] == "2020"] <- "1"
data.alt_covid["Covid"][data.alt_covid["Salgsaar"] == "2021"] <- "1"
data.alt_covid["Covid"][data.alt_covid["Salgsaar"] == "2022"] <- "1"
model_covid_test <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data.alt)
model_med_covid <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund +  Kommune + Velhavende + Afstand_Raadhus + Hus_Alder + Covid, data = data.alt_covid)

p_covid <- anova(model_covid_test, model_med_covid)
print(p_covid) #p>0.05 og den kan dermed fjernes

#Tjek om covid*kommune har signifikant betydning
model_covidkommune <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Kommune*Covid + Afstand_Raadhus + Hus_Alder, data = data.alt_covid)
summary(model_covidkommune)
p_covidkommune <- anova(model_covid_test, model_covidkommune)
print(p_covidkommune) #p>0.05 og den kan dermed fjernes

#UNDERSOGER BETYDNINGEN AF AFSTAND TIL SKOLE KOMMUNERNE IMELLEM
#Laver referencen til kommunen Aalborg
data.alt$Kommune <- factor(data.alt$Kommune, ordered = FALSE)
data.alt$Kommune <- relevel(data.alt$Kommune, ref = "Aalborg")

#Undersoger betydning af afstand til skole ganget med kommunerne
model_KommuneSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Kommune + Velhavende + Stor_Grund  + Afstand_Skole + Kommune*Afstand_Skole + Afstand_Raadhus + Hus_Alder, data = data.alt)
model_uden_KommuneSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Kommune + Velhavende + Stor_Grund + Afstand_Skole + Afstand_Raadhus + Hus_Alder, data = data.alt)
p_KommuneAfstand <- anova(model_KommuneSkole, model_uden_KommuneSkole)
print(p_KommuneAfstand) #p<0.05 og den kan dermed ikke fjernes

#tjekker om Kommune*Afstand_Skole kan fjernes ved F-test i modellerne hvor der kun indgaar to kommuner ad gangen
model_aalborg_aarhus_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_aarhus)
model_aalborg_aarhus_uden_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_aarhus)
p_aalborg_aarhus_KommuneSkole <- anova(model_aalborg_aarhus_KommuneSkole, model_aalborg_aarhus_uden_KommuneSkole)
print(p_aalborg_aarhus_KommuneSkole) #p>0.05 og den kan dermed fjernes

model_aalborg_odense_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_odense)
model_aalborg_odense_uden_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_odense)
p_aalborg_odense_KommuneSkole <- anova(model_aalborg_odense_KommuneSkole, model_aalborg_odense_uden_KommuneSkole)
print(p_aalborg_odense_KommuneSkole) #p>0.05 og den kan dermed fjernes

model_aalborg_koebenhavn_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_koebenhavn)
model_aalborg_koebenhavn_uden_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_koebenhavn)
p_aalborg_koebenhavn_KommuneSkole <- anova(model_aalborg_koebenhavn_KommuneSkole, model_aalborg_koebenhavn_uden_KommuneSkole)
print(p_aalborg_koebenhavn_KommuneSkole) #p<0.05 og den kan dermed ikke fjernes

model_aarhus_odense_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_odense)
model_aarhus_odense_uden_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_odense)
p_aarhus_odense_KommuneSkole <- anova(model_aarhus_odense_KommuneSkole, model_aarhus_odense_uden_KommuneSkole)
print(p_aarhus_odense_KommuneSkole) #p<0.05 og den kan dermed ikke fjernes

model_aarhus_koebenhavn_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_koebenhavn)
model_aarhus_koebenhavn_uden_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_koebenhavn)
p_aarhus_koebenhavn_KommuneSkole <- anova(model_aarhus_koebenhavn_KommuneSkole, model_aarhus_koebenhavn_uden_KommuneSkole)
print(p_aarhus_koebenhavn_KommuneSkole) #p<0.05 og den kan dermed ikke fjernes

model_odense_koebenhavn_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_odense_koebenhavn)
model_odense_koebenhavn_uden_KommuneSkole <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_odense_koebenhavn)
p_odense_koebenhavn_KommuneSkole <- anova(model_odense_koebenhavn_KommuneSkole, model_odense_koebenhavn_uden_KommuneSkole)
print(p_odense_koebenhavn_KommuneSkole) #p<0.05 og den kan dermed ikke fjernes

#Tjekker ift. velhavende og afstand til skole i alle kommunerne
model_VelhavendeSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Kommune + Stor_Grund + Afstand_Skole + Kommune*Afstand_Skole + Velhavende*Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data.alt)
model_uden_VelhavendeSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Kommune + Stor_Grund + Afstand_Skole + Velhavende + Kommune*Afstand_Skole + Afstand_Raadhus + Hus_Alder, data = data.alt)
p_VelhavendeSkole <- anova(model_VelhavendeSkole, model_uden_VelhavendeSkole)
print(p_VelhavendeSkole) #p<0.05 og den kan dermed ikke fjernes

model_aalborg_VelhavendeSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund + Afstand_Skole + Velhavende*Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg)
summary(model_aalborg_VelhavendeSkole)
model_aalborg_uden_VelhavendeSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund + Afstand_Skole  + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg)
p_aalborg_VelhavendeSkole <- anova(model_aalborg_VelhavendeSkole, model_aalborg_uden_VelhavendeSkole)
print(p_aalborg_VelhavendeSkole) #p>0.05 og den kan dermed fjernes

model_aarhus_VelhavendeSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund + Afstand_Skole + Velhavende*Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus)
summary(model_aarhus_VelhavendeSkole)
model_aarhus_uden_VelhavendeSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund + Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus)
p_aarhus_VelhavendeSkole <- anova(model_aarhus_VelhavendeSkole, model_aarhus_uden_VelhavendeSkole)
print(p_aarhus_VelhavendeSkole) #p<0.05 og den kan dermed ikke fjernes

model_odense_VelhavendeSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund + Afstand_Skole + Velhavende*Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_odense)
summary(model_odense_VelhavendeSkole)
model_odense_uden_VelhavendeSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund + Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_odense)
p_odense_VelhavendeSkole <- anova(model_odense_VelhavendeSkole, model_odense_uden_VelhavendeSkole)
print(p_odense_VelhavendeSkole) #p<0.05 og den kan dermed ikke fjernes

model_koebenhavn_VelhavendeSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund + Afstand_Skole + Velhavende*Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_koebenhavn)
summary(model_koebenhavn_VelhavendeSkole)
model_koebenhavn_uden_VelhavendeSkole <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Stor_Grund + Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_koebenhavn)
p_koebenhavn_VelhavendeSkole <- anova(model_koebenhavn_VelhavendeSkole, model_koebenhavn_uden_VelhavendeSkole)
print(p_koebenhavn_VelhavendeSkole) #p>0.05 og den kan dermed fjernes

#Beregner den gennemsnitlige huspris og standardafvigelse for afstand til skole i de fire kommuner
afstand_skole0.50_aalborg.na <- data.frame(Pris_Salg = data_aalborg.udenlog$Pris_Salg, Afstand_Skole = data_aalborg.udenlog$Afstand_Skole)
afstand_skole0.50_aalborg.na["Afstand_Skole"][afstand_skole0.50_aalborg.na["Afstand_Skole"] > "0.50"] <- NA
afstand_skole0.50_aalborg <- na.omit(afstand_skole0.50_aalborg.na)

afstand_skole1.00_aalborg.na <- data.frame(Pris_Salg = data_aalborg.udenlog$Pris_Salg, Afstand_Skole = data_aalborg.udenlog$Afstand_Skole)
afstand_skole1.00_aalborg.na["Afstand_Skole"][afstand_skole1.00_aalborg.na["Afstand_Skole"] < "0.50"] <- NA
afstand_skole1.00_aalborg.na["Afstand_Skole"][afstand_skole1.00_aalborg.na["Afstand_Skole"] > "1.00"] <- NA
afstand_skole1.00_aalborg <- na.omit(afstand_skole1.00_aalborg.na)

afstand_skole1.50_aalborg.na <- data.frame(Pris_Salg = data_aalborg.udenlog$Pris_Salg, Afstand_Skole = data_aalborg.udenlog$Afstand_Skole)
afstand_skole1.50_aalborg.na["Afstand_Skole"][afstand_skole1.50_aalborg.na["Afstand_Skole"] < "1.00"] <- NA
afstand_skole1.50_aalborg.na["Afstand_Skole"][afstand_skole1.50_aalborg.na["Afstand_Skole"] > "1.50"] <- NA
afstand_skole1.50_aalborg <- na.omit(afstand_skole1.50_aalborg.na)

afstand_skole2.00_aalborg.na <- data.frame(Pris_Salg = data_aalborg.udenlog$Pris_Salg, Afstand_Skole = data_aalborg.udenlog$Afstand_Skole)
afstand_skole2.00_aalborg.na["Afstand_Skole"][afstand_skole2.00_aalborg.na["Afstand_Skole"] < "1.50"] <- NA
afstand_skole2.00_aalborg.na["Afstand_Skole"][afstand_skole2.00_aalborg.na["Afstand_Skole"] > "2.00"] <- NA
afstand_skole2.00_aalborg <- na.omit(afstand_skole2.00_aalborg.na)

afstand_skole0.50_aarhus.na <- data.frame(Pris_Salg = data_aarhus.udenlog$Pris_Salg, Afstand_Skole = data_aarhus.udenlog$Afstand_Skole)
afstand_skole0.50_aarhus.na["Afstand_Skole"][afstand_skole0.50_aarhus.na["Afstand_Skole"] > "0.50"] <- NA
afstand_skole0.50_aarhus <- na.omit(afstand_skole0.50_aarhus.na)

afstand_skole1.00_aarhus.na <- data.frame(Pris_Salg = data_aarhus.udenlog$Pris_Salg, Afstand_Skole = data_aarhus.udenlog$Afstand_Skole)
afstand_skole1.00_aarhus.na["Afstand_Skole"][afstand_skole1.00_aarhus.na["Afstand_Skole"] < "0.50"] <- NA
afstand_skole1.00_aarhus.na["Afstand_Skole"][afstand_skole1.00_aarhus.na["Afstand_Skole"] > "1.00"] <- NA
afstand_skole1.00_aarhus <- na.omit(afstand_skole1.00_aarhus.na)

afstand_skole1.50_aarhus.na <- data.frame(Pris_Salg = data_aarhus.udenlog$Pris_Salg, Afstand_Skole = data_aarhus.udenlog$Afstand_Skole)
afstand_skole1.50_aarhus.na["Afstand_Skole"][afstand_skole1.50_aarhus.na["Afstand_Skole"] < "1.00"] <- NA
afstand_skole1.50_aarhus.na["Afstand_Skole"][afstand_skole1.50_aarhus.na["Afstand_Skole"] > "1.50"] <- NA
afstand_skole1.50_aarhus <- na.omit(afstand_skole1.50_aarhus.na)

afstand_skole2.00_aarhus.na <- data.frame(Pris_Salg = data_aarhus.udenlog$Pris_Salg, Afstand_Skole = data_aarhus.udenlog$Afstand_Skole)
afstand_skole2.00_aarhus.na["Afstand_Skole"][afstand_skole2.00_aarhus.na["Afstand_Skole"] < "1.50"] <- NA
afstand_skole2.00_aarhus.na["Afstand_Skole"][afstand_skole2.00_aarhus.na["Afstand_Skole"] > "2.00"] <- NA
afstand_skole2.00_aarhus <- na.omit(afstand_skole2.00_aarhus.na)

afstand_skole0.50_odense.na <- data.frame(Pris_Salg = data_odense.udenlog$Pris_Salg, Afstand_Skole = data_odense.udenlog$Afstand_Skole)
afstand_skole0.50_odense.na["Afstand_Skole"][afstand_skole0.50_odense.na["Afstand_Skole"] > "0.50"] <- NA
afstand_skole0.50_odense <- na.omit(afstand_skole0.50_odense.na)

afstand_skole1.00_odense.na <- data.frame(Pris_Salg = data_odense.udenlog$Pris_Salg, Afstand_Skole = data_odense.udenlog$Afstand_Skole)
afstand_skole1.00_odense.na["Afstand_Skole"][afstand_skole1.00_odense.na["Afstand_Skole"] < "0.50"] <- NA
afstand_skole1.00_odense.na["Afstand_Skole"][afstand_skole1.00_odense.na["Afstand_Skole"] > "1.00"] <- NA
afstand_skole1.00_odense <- na.omit(afstand_skole1.00_odense.na)

afstand_skole1.50_odense.na <- data.frame(Pris_Salg = data_odense.udenlog$Pris_Salg, Afstand_Skole = data_odense.udenlog$Afstand_Skole)
afstand_skole1.50_odense.na["Afstand_Skole"][afstand_skole1.50_odense.na["Afstand_Skole"] < "1.00"] <- NA
afstand_skole1.50_odense.na["Afstand_Skole"][afstand_skole1.50_odense.na["Afstand_Skole"] > "1.50"] <- NA
afstand_skole1.50_odense <- na.omit(afstand_skole1.50_odense.na)

afstand_skole2.00_odense.na <- data.frame(Pris_Salg = data_odense.udenlog$Pris_Salg, Afstand_Skole = data_odense.udenlog$Afstand_Skole)
afstand_skole2.00_odense.na["Afstand_Skole"][afstand_skole2.00_odense.na["Afstand_Skole"] < "1.50"] <- NA
afstand_skole2.00_odense.na["Afstand_Skole"][afstand_skole2.00_odense.na["Afstand_Skole"] > "2.00"] <- NA
afstand_skole2.00_odense <- na.omit(afstand_skole2.00_odense.na)

afstand_skole0.50_koebenhavn.na <- data.frame(Pris_Salg = data_koebenhavn.udenlog$Pris_Salg, Afstand_Skole = data_koebenhavn.udenlog$Afstand_Skole)
afstand_skole0.50_koebenhavn.na["Afstand_Skole"][afstand_skole0.50_koebenhavn.na["Afstand_Skole"] > "0.50"] <- NA
afstand_skole0.50_koebenhavn <- na.omit(afstand_skole0.50_koebenhavn.na)

afstand_skole1.00_koebenhavn.na <- data.frame(Pris_Salg = data_koebenhavn.udenlog$Pris_Salg, Afstand_Skole = data_koebenhavn.udenlog$Afstand_Skole)
afstand_skole1.00_koebenhavn.na["Afstand_Skole"][afstand_skole1.00_koebenhavn.na["Afstand_Skole"] < "0.50"] <- NA
afstand_skole1.00_koebenhavn.na["Afstand_Skole"][afstand_skole1.00_koebenhavn.na["Afstand_Skole"] > "1.00"] <- NA
afstand_skole1.00_koebenhavn <- na.omit(afstand_skole1.00_koebenhavn.na)

afstand_skole1.50_koebenhavn.na <- data.frame(Pris_Salg = data_koebenhavn.udenlog$Pris_Salg, Afstand_Skole = data_koebenhavn.udenlog$Afstand_Skole)
afstand_skole1.50_koebenhavn.na["Afstand_Skole"][afstand_skole1.50_koebenhavn.na["Afstand_Skole"] < "1.00"] <- NA
afstand_skole1.50_koebenhavn.na["Afstand_Skole"][afstand_skole1.50_koebenhavn.na["Afstand_Skole"] > "1.50"] <- NA
afstand_skole1.50_koebenhavn <- na.omit(afstand_skole1.50_koebenhavn.na)

afstand_skole2.00_koebenhavn.na <- data.frame(Pris_Salg = data_koebenhavn.udenlog$Pris_Salg, Afstand_Skole = data_koebenhavn.udenlog$Afstand_Skole)
afstand_skole2.00_koebenhavn.na["Afstand_Skole"][afstand_skole2.00_koebenhavn.na["Afstand_Skole"] < "1.50"] <- NA
afstand_skole2.00_koebenhavn.na["Afstand_Skole"][afstand_skole2.00_koebenhavn.na["Afstand_Skole"] > "2.00"] <- NA
afstand_skole2.00_koebenhavn <- na.omit(afstand_skole2.00_koebenhavn.na)

aalborg0.50 <- mean(afstand_skole0.50_aalborg$Pris_Salg)
aalborg1.00 <- mean(afstand_skole1.00_aalborg$Pris_Salg)
aalborg1.50 <- mean(afstand_skole1.50_aalborg$Pris_Salg)
aalborg2.00 <- mean(afstand_skole2.00_aalborg$Pris_Salg)

aarhus0.50 <- mean(afstand_skole0.50_aarhus$Pris_Salg)
aarhus1.00 <- mean(afstand_skole1.00_aarhus$Pris_Salg)
aarhus1.50 <- mean(afstand_skole1.50_aarhus$Pris_Salg)
aarhus2.00 <- mean(afstand_skole2.00_aarhus$Pris_Salg)

odense0.50 <- mean(afstand_skole0.50_odense$Pris_Salg)
odense1.00 <- mean(afstand_skole1.00_odense$Pris_Salg)
odense1.50 <- mean(afstand_skole1.50_odense$Pris_Salg)
odense2.00 <- mean(afstand_skole2.00_odense$Pris_Salg)

koebenhavn0.50 <- mean(afstand_skole0.50_koebenhavn$Pris_Salg)
koebenhavn1.00 <- mean(afstand_skole1.00_koebenhavn$Pris_Salg)
koebenhavn1.50 <- mean(afstand_skole1.50_koebenhavn$Pris_Salg)
koebenhavn2.00 <- mean(afstand_skole2.00_koebenhavn$Pris_Salg)

#Laver en dataframe med de gennemsnitlige huspriser alt efter hvor langt huset ligger fra en skole
afstand_skoledf <- data.frame(x = c("0.00-0.50", "0.50-1.00", "1.00-1.50", "1.50-2.00"), Aalborg = c(aalborg0.50, aalborg1.00, aalborg1.50, aalborg2.00), Aarhus = c(aarhus0.50, aarhus1.00, aarhus1.50, aarhus2.00), Odense = c(odense0.50, odense1.00, odense1.50, odense2.00), koebenhavn = c(koebenhavn0.50, koebenhavn1.00, koebenhavn1.50, koebenhavn2.00))


#UNDERSOGER BETYDNINGEN AF ANTAL RUM KOMMUNERNE IMELLEM
#Undersoger betydning af antal rum ganget med kommunerne
model_KommuneRum <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Kommune + Stor_Grund  + Afstand_Skole + Kommune*Antal_Rum + Velhavende + Afstand_Raadhus + Hus_Alder, data = data.alt)
summary(model_KommuneRum)
model_uden_KommuneRum <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Kommune + Stor_Grund + Afstand_Skole + Velhavende + Afstand_Raadhus + Hus_Alder, data = data.alt)
p_KommuneRum <- anova(model_KommuneRum, model_uden_KommuneRum)
print(p_KommuneRum) #p<0.05 og den kan dermed ikke fjernes

#tjekker om Kommune*Antal_Rum kan fjernes ved F-test i modellerne hvor der kun indgaar to kommuner ad gangen
red_model_aalborg_aarhus_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Antal_Rum + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_aarhus)
model_aalborg_aarhus_uden_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_aarhus)
p_aalborg_aarhus_KommuneRum <- anova(red_model_aalborg_aarhus_KommuneRum, model_aalborg_aarhus_uden_KommuneRum)
print(p_aalborg_aarhus_KommuneRum) #p<0.05 og den kan dermed ikke fjernes

red_model_aalborg_odense_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Antal_Rum + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_odense)
model_aalborg_odense_uden_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_odense)
p_aalborg_odense_KommuneRum <- anova(red_model_aalborg_odense_KommuneRum, model_aalborg_odense_uden_KommuneRum)
print(p_aalborg_odense_KommuneRum) #p<0.05 og den kan dermed ikke fjernes

red_model_aalborg_koebenhavn_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Antal_Rum + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_koebenhavn)
model_aalborg_koebenhavn_uden_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aalborg_koebenhavn)
p_aalborg_koebenhavn_KommuneRum <- anova(red_model_aalborg_koebenhavn_KommuneRum, model_aalborg_koebenhavn_uden_KommuneRum)
print(p_aalborg_koebenhavn_KommuneRum) #p<0.05 og den kan dermed ikke fjernes

red_model_aarhus_odense_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Antal_Rum + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_odense)
model_aarhus_odense_uden_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_odense)
p_aarhus_odense_KommuneRum <- anova(red_model_aarhus_odense_KommuneRum, model_aarhus_odense_uden_KommuneRum)
print(p_aarhus_odense_KommuneRum) #p<0.05 og den kan dermed ikke fjernes

red_model_aarhus_koebenhavn_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Antal_Rum + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_koebenhavn)
model_aarhus_koebenhavn_uden_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_aarhus_koebenhavn)
p_aarhus_koebenhavn_KommuneRum <- anova(red_model_aarhus_koebenhavn_KommuneRum, model_aarhus_koebenhavn_uden_KommuneRum)
print(p_aarhus_koebenhavn_KommuneRum) #p<0.05 og den kan dermed ikke fjernes

red_model_odense_koebenhavn_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Kommune + Stor_Grund + Kommune*Antal_Rum + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_odense_koebenhavn)
model_odense_koebenhavn_uden_KommuneRum <- lm(lPris_Salg-lAreal_Bolig ~ lAreal_Grund + Trend + Stor_Grund + Kommune + Velhavende + Afstand_Raadhus + Hus_Alder, data = data_odense_koebenhavn)
p_odense_koebenhavn_KommuneRum <- anova(red_model_odense_koebenhavn_KommuneRum, model_odense_koebenhavn_uden_KommuneRum)
print(p_odense_koebenhavn_KommuneRum) #p<0.05 og den kan dermed ikke fjernes

#Tjekker ift. velhavende og antal rum i alle kommunerne
red_model_VelhavendeRum <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Kommune + Stor_Grund + Afstand_Skole + Kommune*Antal_Rum + Velhavende*Antal_Rum + Velhavende 
                              + Afstand_Raadhus + Hus_Alder, data = data.alt)
model_uden_VelhavendeRum <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Trend + Kommune + Stor_Grund + Afstand_Skole + Velhavende + Kommune*Antal_Rum + Afstand_Raadhus + Hus_Alder, data = data.alt)
p_VelhavendeRum <- anova(red_model_VelhavendeRum, model_uden_VelhavendeRum)
print(p_VelhavendeRum) #p>0.05 og den kan dermed fjernes

#Beregner den gennemsnitlige huspris og standardafvigelse for hvert antal af rum i de fire kommuner
pris_antalrum_aalborg <- data.frame(Pris_Salg = data_aalborg.udenlog$Pris_Salg, Antal_Rum = data_aalborg.udenlog$Antal_Rum) %>%
  group_by(Antal_Rum) %>%
  summarise_at(vars(Pris_Salg),
               list(mean = mean)) %>%
  as.data.frame()
pris_antalrum_aalborg

pris_antalrum_aarhus <- data.frame(Pris_Salg = data_aarhus.udenlog$Pris_Salg, Antal_Rum = data_aarhus.udenlog$Antal_Rum) %>%
  group_by(Antal_Rum) %>%
  summarise_at(vars(Pris_Salg),
               list(mean = mean)) %>%
  as.data.frame()
pris_antalrum_aarhus

pris_antalrum_odense <- data.frame(Pris_Salg = data_odense.udenlog$Pris_Salg, Antal_Rum = data_odense.udenlog$Antal_Rum) %>%
  group_by(Antal_Rum) %>%
  summarise_at(vars(Pris_Salg),
               list(mean = mean)) %>%
  as.data.frame()
pris_antalrum_odense

pris_antalrum_koebenhavn <- data.frame(Pris_Salg = data_koebenhavn.udenlog$Pris_Salg, Antal_Rum = data_koebenhavn.udenlog$Antal_Rum) %>%
  group_by(Antal_Rum) %>%
  summarise_at(vars(Pris_Salg),
               list(mean = mean)) %>%
  as.data.frame()
pris_antalrum_koebenhavn

#Laver en dataframe med antal rum og den tilhorende gennemsnitlige pris for de fire kommuner
antalrum_aalborg <- data.frame(Antal_Rum = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), gnspris = c(2400000, 2154069, 2304939, 2923818, 3334984, 4158625, 4036250, NA, NA, NA, NA, NA,NA,NA,NA, NA))
antalrum_aarhus <- data.frame(Antal_Rum = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), gnspris = c(NA, 3310615, 4217010, 4777163, 5048206, 5161050, 7505357, 6116667, 4500000, 6200000, 9700000, 6998000,
                                                                                                  NA,NA,NA,NA))
antalrum_odense <- data.frame(Antal_Rum = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), gnspris = c(3450000, 2789444, 2188113, 3830079, 4151015, 4442014, 4927500, 6486429, 7522500, 6950000, 3295000, NA,NA,
                                                                                                  NA,NA, NA))
antalrum_koebenhavn <- data.frame(Antal_Rum = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), gnspris = c(2681250, 2627143, 2970526, 5819524, 6649030, 8562273, 15738889, 7516667, 16200000, 27500000, 12500000, 
                                                                                                     30500000,NA,NA,NA, 21000000))
df.antal_rum <- data.frame(Antal_Rum = antalrum_aalborg$Antal_Rum, gnsprisaalborg = antalrum_aalborg$gnspris, gnsprisaarhus = antalrum_aarhus$gnspris, gnsprisodense = antalrum_odense$gnspris, 
                           gnspriskoebenhavn = antalrum_koebenhavn$gnspris)

#FORSOGER AT OPTIMERE MODELLEN VED AT GANGE SOGN PAA ALLE VARIABLE OG REDUCERE MODELLEN
#Laver modellen, hvor der er ganget Sogn paa alt
model_alle_Sogn <- lm(lPris_Salg ~ Sogn*lAreal_Bolig + Sogn*lAreal_Grund + Sogn*Antal_Rum + Sogn*Salgsmaaned + Sogn*Trend + Sogn*Stor_Grund + Sogn*Afstand_Skole + Sogn*Afstand_Raadhus 
                      + Sogn*Hus_Alder, data = data.alt)
summary(model_alle_Sogn)
plot(model_alle_Sogn$residuals)

#Tester ved F-test hvilke variable der ikke er signifikante
model_Sogn_uden_lAreal_Bolig <- lm(lPris_Salg ~ Sogn*lAreal_Grund + Sogn*Antal_Rum + Sogn*Salgsmaaned + Sogn*Trend + Sogn*Stor_Grund + Sogn*Afstand_Skole + Sogn*Afstand_Raadhus + Sogn*Hus_Alder, data = data.alt)
p_Sogn_uden_lAreal_Bolig <- anova(model_alle_Sogn, model_Sogn_uden_lAreal_Bolig)
print(p_Sogn_uden_lAreal_Bolig) #p<0.05 og den kan dermed ikke fjernes

model_Sogn_uden_lAreal_Grund <- lm(lPris_Salg ~ Sogn*lAreal_Bolig + Sogn*Antal_Rum + Sogn*Salgsmaaned + Sogn*Trend + Sogn*Stor_Grund + Sogn*Afstand_Skole + Sogn*Afstand_Raadhus + Sogn*Hus_Alder, data = data.alt)
p_Sogn_uden_lAreal_Grund <- anova(model_alle_Sogn, model_Sogn_uden_lAreal_Grund)
print(p_Sogn_uden_lAreal_Grund) #p<0.05 og den kan dermed ikke fjernes

model_Sogn_uden_Antal_Rum <- lm(lPris_Salg ~ Sogn*lAreal_Bolig + Sogn*lAreal_Grund + Sogn*Salgsmaaned + Sogn*Trend + Sogn*Stor_Grund + Sogn*Afstand_Skole + Sogn*Afstand_Raadhus + Sogn*Hus_Alder, data = data.alt)
p_Sogn_uden_Antal_Rum <- anova(model_alle_Sogn, model_Sogn_uden_Antal_Rum)
print(p_Sogn_uden_Antal_Rum) #p<0.05 og den kan dermed ikke fjernes

model_Sogn_uden_Salgsmaaned <- lm(lPris_Salg ~ Sogn*lAreal_Bolig + Sogn*lAreal_Grund + Sogn*Antal_Rum + Sogn*Trend + Sogn*Stor_Grund + Sogn*Afstand_Skole + Sogn*Afstand_Raadhus + Sogn*Hus_Alder, data = data.alt)
p_Sogn_uden_Salgsmaaned <- anova(model_alle_Sogn, model_Sogn_uden_Salgsmaaned)
print(p_Sogn_uden_Salgsmaaned) #p>0.05 og den kan dermed fjernes

model_Sogn_uden_Trend <- lm(lPris_Salg ~ Sogn*lAreal_Bolig + Sogn*lAreal_Grund + Sogn*Antal_Rum + Sogn*Stor_Grund + Sogn*Afstand_Skole + Sogn*Afstand_Raadhus + Sogn*Hus_Alder, data = data.alt)
p_Sogn_uden_Trend <- anova(model_Sogn_uden_Salgsmaaned, model_Sogn_uden_Trend)
print(p_Sogn_uden_Trend) #p<0.05 og den kan dermed ikke fjernes

model_Sogn_uden_Stor_Grund <- lm(lPris_Salg ~ Sogn*lAreal_Bolig + Sogn*lAreal_Grund + Sogn*Antal_Rum + Sogn*Trend + Sogn*Afstand_Skole + Sogn*Afstand_Raadhus + Sogn*Hus_Alder, data = data.alt)
p_Sogn_uden_Stor_Grund <- anova(model_Sogn_uden_Salgsmaaned, model_Sogn_uden_Stor_Grund)
print(p_Sogn_uden_Stor_Grund) #p>0.05 og den kan dermed fjernes

model_Sogn_uden_Afstand_Skole <- lm(lPris_Salg ~ Sogn*lAreal_Bolig + Sogn*lAreal_Grund + Sogn*Antal_Rum + Sogn*Trend + Sogn*Afstand_Raadhus + Sogn*Hus_Alder, data = data.alt)
p_Sogn_uden_Afstand_Skole <- anova(model_Sogn_uden_Stor_Grund, model_Sogn_uden_Afstand_Skole)
print(p_Sogn_uden_Afstand_Skole) #p<0.05 og den kan dermed ikke fjernes

model_Sogn_uden_Afstand_Raadhus <- lm(lPris_Salg ~ Sogn*lAreal_Bolig + Sogn*lAreal_Grund + Sogn*Antal_Rum + Sogn*Trend + Sogn*Afstand_Skole + Sogn*Hus_Alder, data = data.alt)
p_Sogn_uden_Afstand_Raadhus <- anova(model_Sogn_uden_Stor_Grund, model_Sogn_uden_Afstand_Raadhus)
print(p_Sogn_uden_Afstand_Raadhus) #p<0.05 og den kan dermed ikke fjernes

model_Sogn_uden_Hus_Alder <- lm(lPris_Salg ~ Sogn*lAreal_Bolig + Sogn*lAreal_Grund + Sogn*Antal_Rum + Sogn*Trend + Sogn*Afstand_Skole + Sogn*Afstand_Raadhus, data = data.alt)
p_Sogn_uden_Hus_Alder <- anova(model_Sogn_uden_Stor_Grund, model_Sogn_uden_Hus_Alder)
print(p_Sogn_uden_Hus_Alder) #p<0.05 og den kan dermed ikke fjernes

optimeret_model <- lm(lPris_Salg ~ Sogn*lAreal_Bolig + Sogn*lAreal_Grund + Sogn*Antal_Rum  + Sogn*Trend + Sogn*Afstand_Skole + Sogn*Afstand_Raadhus + Sogn*Hus_Alder, data = data.alt)
summary(optimeret_model)

#AFVIGELSE MELLEM PRAEDIKTIONER FOR DEN OPTIMEREDE MODEL OG DATA, SAMT PRAEDIKTION FOR EN NY OBSERVATION
praediktion_optimeret_model <- predict(optimeret_model)
print(praediktion_optimeret_model)
afvigelse_optimeret_model <- data.frame(afvigelse = data.alt$lPris_Salg-praediktion_optimeret_model)

#Forsoger at fjerne hus nr. 627 fra den optimerede model
optimeret_model.uden627 <- lm(lPris_Salg ~ Sogn*lAreal_Bolig + Sogn*lAreal_Grund + Sogn*Antal_Rum + Sogn*Trend + Sogn*Afstand_Skole + Sogn*Afstand_Raadhus + Sogn*Hus_Alder, data = data.uden627)
summary(optimeret_model.uden627) #Fjernes ikke

#Laver praediktion og praediktionsinterval for den optimerede model
x_nplus1 <- data.frame(lAreal_Bolig=c(log(126)), lAreal_Grund=c(log(798)), Antal_Rum=c(4), Trend=c(12), Afstand_Skole=c(0.65), Sogn=c("Hasseris"), Afstand_Raadhus=c(4), Hus_Alder=c(54))
praediktion_optimeret_model <- predict(optimeret_model, newdata=x_nplus1, interval="prediction")
print(exp(praediktion_optimeret_model)) #fit=2852581 lwr=1871165 upr=4348744

#PLOTS
#Laver residualplots for modellen hvor der ikke er taget log og for modellen hvor der er taget log til husprisen for at tjekke for varianshomogenitet
model.uden.log <- lm(Pris_Salg ~ Areal_Bolig + Areal_Grund + Antal_Rum + Salgsmaaned + Stor_Grund + Afstand_Skole + Trend +  Sogn + Afstand_Raadhus + Hus_Alder, data = data.uden.log)
res.log = residuals(model.uden.log)
resplot_uden.log <- data.frame(fitted_values = model.uden.log$fitted.values, res = res.log)
ggplot(resplot_uden.log, aes(x=fitted_values, y=res)) + geom_point() + xlab("Fittede vaerdier") + ylab("Residualer") + geom_hline(yintercept=0)

model.med.log <- lm(lPris_Salg ~ lAreal_Bolig + lAreal_Grund + Antal_Rum + Stor_Grund + Salgsmaaned + Afstand_Skole + Trend + Sogn + Afstand_Raadhus + Hus_Alder, data = data.alt)
res = residuals(model.med.log)
resplot_med.log <- data.frame(fitted_values = model.med.log$fitted.values, res = res)
ggplot(resplot_med.log, aes(x=fitted_values, y=res)) + geom_point() + xlab("Fittede vaerdier") + ylab("Residualer") + geom_hline(yintercept=0)

#Scatterplots hvor de forskellige kontinuerte variable er paa x-aksen og huspris paa y-aksen
ggplot(data.uden.log, aes(x=Areal_Bolig, y=Pris_Salg)) +
  geom_point()+ 
  xlab("Boligareal")+
  ylab("Huspris")

ggplot(data.uden.log, aes(x=Areal_Grund, y=Pris_Salg)) +
  geom_point()+ 
  xlab("Grundareal")+
  ylab("Huspris")

ggplot(data.uden.log, aes(x=Afstand_Skole, y=Pris_Salg)) +
  geom_point()+ 
  xlab("Afstand til skole")+
  ylab("Huspris")

ggplot(data.uden.log, aes(x=Afstand_Raadhus, y=Pris_Salg)) +
  geom_point()+ 
  xlab("Afstand til raadhus")+
  ylab("Huspris")

ggplot(data.uden.log, aes(x=Hus_Alder, y=Pris_Salg)) +
  geom_point()+
  xlab("Husets alder")+
  ylab("Huspris")

#Boxplots hvor de forskellige diskrete variable er paa x-aksen og huspris paa y-aksen
data.uden.log$Antal_Rum <- factor(data.uden.log$Antal_Rum, ordered = FALSE)
ggplot(data.uden.log, aes(x=Antal_Rum, y=Pris_Salg, fill = Antal_Rum)) + 
  geom_boxplot() +
  xlab("Antal rum") +
  ylab("Huspris") +
  theme(legend.position="none") +
  scale_color_viridis_b()

ggplot(data.uden.log, aes(x=Salgsmaaned, y=Pris_Salg, fill=Salgsmaaned)) + 
  geom_boxplot(fill="#1F968BFF") +
  xlab("Salgsmaaned") +
  ylab("Huspris") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.position="none") 

data.uden.log$Trend <- factor(data.uden.log$Trend, ordered = FALSE)
ggplot(data.uden.log, aes(x=Trend, y=Pris_Salg, fill=Trend)) + 
  geom_boxplot() +
  xlab("Trend") +
  ylab("Huspris") +
  theme(legend.position="none") 

data.uden.log$Stor_Grund <- factor(data.uden.log$Stor_Grund, ordered = FALSE)
ggplot(data.uden.log, aes(x=Stor_Grund, y=Pris_Salg, fill=Stor_Grund)) + 
  geom_boxplot() +
  xlab("Stor grund") +
  ylab("Huspris") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("#1F968BFF","#B8DE29FF"))

ggplot(data.uden.log, aes(x=Sogn, y=Pris_Salg, fill=Sogn)) +
  geom_boxplot() +
  xlab("Sogn") +
  ylab("Huspris") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.position="none") +
  scale_x_discrete(limits=c("Norre Tranders", "Hasseris", "Vollsmose", "Thomas Kingos", "Gellerup", "Skaade", "Husumvold", "Sions")) +
  scale_fill_manual(values=c("#55C667FF", "#453781FF", "#FDE725FF" , "#453781FF", "#FDE725FF", "#55C667FF", "#2D708EFF", "#2D708EFF"))

ggplot(data.uden.log, aes(x=Kommune, y=Pris_Salg, fill=Kommune)) + 
  geom_boxplot() +
  xlab("Kommune") +
  ylab("Huspris") +
  theme(legend.position="none") +
  scale_x_discrete(limits=c("Aalborg", "Odense", "Aarhus","koebenhavn")) +
  scale_fill_manual(values=c("#DCE319FF", "#453781FF","#238A8DFF", "#39568CFF"))

#plot over afvigelsen mellem data og praediktionerne for den reducerede model
ggplot(afvigelse_reduceret_model, aes(x=afvigelse)) + geom_histogram(color="black", fill="white", bins=30) + xlab("Afvigelse mellem model og data") + ylab("Hyppighed")

#plot der illustrerer udviklingen i den gennemsnitlige kvadratmeterpris 
ggplot(data=kvmpris, aes(x=Salgsaar)) +
  geom_line(aes(y=kvmprisall), colour ="blue") + geom_point(aes(y=kvmprisall), colour = "blue") +
  geom_line(aes(y=kvmprisaar), colour = "red") + geom_point(aes(y=kvmprisaar), colour = "red") +
  geom_line(aes(y=kvmprisod), colour = "darkgreen") + geom_point(aes(y=kvmprisod), colour = "darkgreen") +
  geom_line(aes(y=kvmpriskbh), colour = "orange") + geom_point(aes(y=kvmpriskbh), colour = "orange") +
  xlab("Salgsaar") +
  ylab("Kvadratmeterpris, kr") +
  scale_x_continuous(name="Salgsaar", breaks = seq(2011,2022, by=1.0)) 

#Plotter histogrammer for residualerne for hver kommune, for at sammenligne om variansen er ens og et plot der viser normalkurverne for residualerne for hver kommune
ggplot(reduceret_model_aalborg, aes(x=reduceret_model_aalborg$residuals)) + geom_histogram(aes(y = after_stat(count / sum(count))), color="black", fill="white", bins=25) + xlim(-1,1) 
+ xlab("Residualer for Aalborgmodellen") + ylab("Frekvens")+ scale_y_continuous(labels = scales::percent) 
ggplot(reduceret_model_aarhus, aes(x=reduceret_model_aarhus$residuals)) + geom_histogram(aes(y = after_stat(count / sum(count))), color="black", fill="white", bins=25) + xlim(-1,1) 
+ xlab("Residualer for Aarhusmodellen") + ylab("Frekvens") + scale_y_continuous(labels = scales::percent) 
ggplot(reduceret_model_odense, aes(x=reduceret_model_odense$residuals)) + geom_histogram(aes(y = after_stat(count / sum(count))), color="black", fill="white", bins=25) + xlim(-1,1) 
+ xlab("Residualer for Odensemodellen") + ylab("Frekvens") + scale_y_continuous(labels = scales::percent) 
ggplot(reduceret_model_koebenhavn, aes(x=reduceret_model_koebenhavn$residuals)) + geom_histogram(aes(y = after_stat(count / sum(count))), color="black", fill="white", bins=25) + xlim(-1,1) 
+ xlab("Residualer for koebenhavnmodellen") + ylab("Frekvens") + scale_y_continuous(labels = scales::percent) 

ggplot() + stat_function(fun = dnorm, aes(), color = "blue", n = 310, args = list(mean = 2.322772e-17, sd = 0.2168396)) 
+ stat_function(fun = dnorm, aes(), color = "red", n = 177, args = list(mean = 8.654016e-18, sd = 0.2165281)) 
+ stat_function(fun = dnorm, aes(), color = "darkgreen", n = 262, args = list(mean = 6.797989e-18, sd = 0.2234215)) 
+ stat_function(fun = dnorm, aes(), color = "orange", n = 114, args = list(mean = -1.778829e-18, sd = 0.181795)) + xlim(-1,1) + ylab("") + scale_y_continuous(breaks = NULL) + xlab("Normalkurver")

#plot der illustrerer den gennemsnitlige huspris afhaengig af antal rum i de fire kommuner
ggplot(data=df.antal_rum, aes(x=Antal_Rum)) +
  geom_line(aes(y=gnsprisaalborg), colour ="blue") + geom_point(aes(y=gnsprisaalborg), colour = "blue") +
  geom_line(aes(y=gnsprisaarhus), colour = "red") + geom_point(aes(y=gnsprisaarhus), colour = "red") +
  geom_line(aes(y=gnsprisodense), colour = "darkgreen") + geom_point(aes(y=gnsprisodense), colour = "darkgreen") +
  geom_line(aes(y=gnspriskoebenhavn), colour = "orange") + geom_point(aes(y=gnspriskoebenhavn), colour = "orange") +
  xlab("Antal rum") +
  ylab("Huspris, kr") +
  scale_x_continuous(name="Antal rum", breaks = seq(1,17, by=1.0)) +
  ylim(0,32000000)

#plot der illustrerer udviklingen i den gennemsnitlige huspris ift. afstand til skole i de fire kommuner
ggplot(data=afstand_skoledf, aes(x=x)) +
  geom_line(aes(y=Aalborg, group=1), colour ="blue") + geom_point(aes(y=Aalborg), colour = "blue") +
  geom_line(aes(y=Aarhus, group=1), colour = "red") + geom_point(aes(y=Aarhus), colour = "red") +
  geom_line(aes(y=Odense, group=1), colour = "darkgreen") + geom_point(aes(y=Odense), colour = "darkgreen") +
  geom_line(aes(y=koebenhavn, group=1), colour = "orange") + geom_point(aes(y=koebenhavn), colour = "orange") +
  xlab("Afstand til skole") +
  ylab("Huspris, kr") 

#plot over afvigelsen mellem data og praediktionerne for den optimerede model
ggplot(afvigelse_Sogn, aes(x=afvigelse)) + geom_histogram(color="black", fill="white", bins=30) + xlab("Afvigelse mellem model og data") + ylab("Hyppighed")

#histogram der viser hvor mange huse der ligger ved de forskellige afstand til skole
ggplot(data.alt, aes(x=Afstand_Skole)) + geom_histogram(color="black", fill="white", bins=30) + xlab("Afstand til skole") + ylab("Hyppighed")




