# **********************************
#   Análisis tesis Maite Castañ
#     1. Gestión de datos
# **********************************

# Este script contiene la gestión de datos con los que se realiza el análisis estadístico de la tesis de Maite Castañ

# Los datos de los que se parte son:
# - Listado de pacientes qe conforman la cohorte de estudio
# - Datos de los pacientes recogidos durante el primer y segundo año de estudio
# - Datos de las exacerbaciones sufridas por los pacientes de la cohorte durante el primer y segundo año de estudio, así como el año previo, entre los meses de ????


# load packages ####

library(dplyr)
library(kableExtra)

# import data ####

pacients <- read.csv("data/raw/BASE DE DADES INICIAL DM I MPOC1.csv", sep=";")
primer <- read.csv("data/raw/BASE DE DADES PRIMER ANY.csv", sep=";")
segon <- read.csv("data/raw/Base de dades segon any 2.csv", sep=";")
respiratorio_pre <- read.csv("data/raw/RESPIRATORIO _EPOC_epidemiologia,_tesis_de_MC_any_anterior.csv", sep=";")
respiratorio1 <- read.csv("data/raw/RESPIRATORIO__EPOC_epidemiologia,_tesis_de_MC1r año (1).csv", sep=";", header=T)
respiratorio2 <- read.csv("data/raw/RESPIRATORIO__EPOC_epidemiologia,_tesis_de_MC_2020.csv", sep=";")


# cleaning raw data (pacientes) ####

names(pacients) <- c("cip", "sexe", "dn", "abs")
pacients$cip <- substr(pacients$cip, 1,13) # Se conservan únicamente los primeros 13 caracteres del campo CIP, ya que en algunos casos contiene más información

names(primer) <- c("cip", "exitus_1", "data_exitus_1", "sexe", "dn", "abs", "edat_1", "imc_1", "tabac_1", "hta_1", "ic_1", "irc_1", "enolic_1", "vacgrip_1", "datavacgrip_1", "vacpneumo_1", "pn23p_1", "pn23s_1", "pnc13_1", "hba1c_1", "espiro_1", "patro_1", "anyespiro_1", "fev1_1", "fev1fvc_1")
primer <- primer[primer$cip!="",] # Una fila no tenia CIP
primer$cip <- substr(primer$cip, 1,13)
primer$exitus_1 <- as.factor(primer$exitus_1)
primer$data_exitus_1 <- as.Date(primer$data_exitus_1, format="%d_%m_%Y")
primer$imc_1 <- gsub(",", ".", primer$imc_1)
primer$imc_1 <- as.numeric(primer$imc_1)
primer$tabac_1[primer$tabac_1=="Ex-fumador "] <- "Ex-fumador"
primer$tabac_1[primer$tabac_1=="Fumadora"] <- "Fumador"
primer$tabac_1[primer$tabac_1=="No fumador"] <- "No-fumador"
primer$tabac_1[primer$tabac_1==""] <- NA
primer$tabac_1 <- factor(primer$tabac_1, levels=c("No-fumador", "Ex-fumador", "Fumador"))
primer$hta_1[primer$hta_1=="NO" | primer$hta_1=="No "] <- "No"
primer$hta_1[primer$hta_1=="Si" | primer$hta_1=="Sí " | primer$hta_1=="Sï"] <- "Sí"
primer$hta_1[primer$hta_1==""] <- NA
primer$hta_1 <- factor(primer$hta_1)
primer$ic_1[primer$ic_1=="no" | primer$ic_1=="No "] <- "No"
primer$ic_1[primer$ic_1==""] <- NA
primer$ic_1 <- factor(primer$ic_1)
primer$irc_1[primer$irc_1=="no" | primer$irc_1=="NO" | primer$irc_1=="No "] <- "No"
primer$irc_1[primer$irc_1=="Sï" | primer$irc_1=="Sí "] <- "Sí"
primer$irc_1[primer$irc_1==""] <- NA
primer$irc_1 <- factor(primer$irc_1)
primer$enolic_1[primer$enolic_1=="baix risc"] <- "Baix risc"
primer$enolic_1[primer$enolic_1==""] <- NA
primer$enolic_1 <- factor(primer$enolic_1, levels=c("Abstemi", "Baix risc", "Alt risc"))
primer$vacgrip_1[primer$vacgrip_1=="NO" | primer$vacgrip_1=="No "] <- "No"
primer$vacgrip_1[primer$vacgrip_1=="Si" | primer$vacgrip_1=="Si " | primer$vacgrip_1=="Sí "] <- "Sí"
primer$vacgrip_1[primer$vacgrip_1==""] <- NA
primer$datavacgrip_1 <- substr(primer$datavacgrip_1, 1, 10)
primer$datavacgrip_1 <- as.Date(primer$datavacgrip_1, format="%d/%m/%Y")
primer$vacpneumo_1[primer$vacpneumo_1=="No "] <- "No"
primer$vacpneumo_1[primer$vacpneumo_1=="S" | primer$vacpneumo_1=="SÍ " | primer$vacpneumo_1=="Sí " | primer$vacpneumo_1=="Sí  " | primer$vacpneumo_1=="Sí (" | primer$vacpneumo_1==" Sí"] <- "Sí"
primer$vacpneumo_1[primer$vacpneumo_1==""] <- NA
primer$vacpneumo_1 <- factor(primer$vacpneumo_1)
primer$hba1c_1 <- gsub(",", ".", primer$hba1c_1)
primer$hba1c_1 <- as.numeric(primer$hba1c_1)
primer$fev1_1 <- gsub(",", ".", primer$fev1_1)
primer$fev1_1 <- as.numeric(primer$fev1_1)
primer$fev1fvc_1 <- gsub(",", ".", primer$fev1fvc_1)
primer$fev1fvc_1 <- as.numeric(primer$fev1fvc_1)
primer <- primer[-c(which(colnames(primer)=="abs"), which(colnames(primer)=="sexe"), which(colnames(primer)=="dn"))]


names(segon) <- c("cip", "exitus_2", "dataexitus_2", "sexe", "dn", "abs", "edat_2", "imc_2", "tabac_2", "hta_2", "ic_2", "irc_2", "enolic_2", "vacgrip_2", "datavacgrip_2", "vacpneumo_2", "pn23p_2", "pn23s_2", "pnc13_2", "hba1c_2", "espiro_2", "patro_2", "anyespiro_2", "fev1_2", "fev1fvc_2", "ansietat_2", "depressio_2", "charlson_2", "mrc_2", "bodex_2")
segon$cip <- substr(segon$cip, 1,13)
segon$exitus_2 <- substr(segon$exitus_2, 1,2)
segon$tabac_2[segon$tabac_2=="ex fumador" | segon$tabac_2=="exfumador"] <- "Ex-fumador"
segon$tabac_2[segon$tabac_2=="fumador"] <- "Fumador"
segon$tabac_2[segon$tabac_2=="no fuma" | segon$tabac_2=="no fumador" | segon$tabac_2=="No fumadora" | segon$tabac_2=="fumador passiu" | segon$tabac_2=="No fumador"] <- "No-fumador"
segon$tabac_2[segon$tabac_2==""] <- NA
segon$tabac_2 <- factor(segon$tabac_2, levels=c("No-fumador", "Ex-fumador", "Fumador"))
segon$hta_2[segon$hta_2=="no"] <- "No"
segon$hta_2[segon$hta_2=="si" | segon$hta_2=="sí" | segon$hta_2=="Sï"] <- "Sí"
segon$hta_2[segon$hta_2==""] <- NA
segon$hta_2 <- factor(segon$hta_2)
segon$ic_2[segon$ic_2=="S" | segon$ic_2=="si" | segon$ic_2=="sí"] <- "Sí"
segon$ic_2[segon$ic_2=="no"] <- "No"
segon$ic_2[segon$ic_2==""] <- NA
segon$ic_2 <- factor(segon$ic_2)
segon$irc_2[segon$irc_2=="N" | segon$irc_2=="no" | segon$irc_2=="NO"] <- "No"
segon$irc_2[segon$irc_2=="si" | segon$irc_2=="sí"] <- "Sí"
segon$irc_2[segon$irc_2==""] <- NA
segon$irc_2 <- factor(segon$irc_2)
segon$enolic_2[segon$enolic_2=="abstemi"] <- "Abstemi"
segon$enolic_2[segon$enolic_2=="baix risc"] <- "Baix risc"
segon$enolic_2[segon$enolic_2=="alcoholisme cronic" | segon$enolic_2=="alt risc" | segon$enolic_2=="de risc" | segon$enolic_2=="De risc"] <- "Alt risc"
segon$enolic_2[segon$enolic_2==""] <- NA
segon$enolic_2 <- factor(segon$enolic_2, levels=c("Abstemi", "Baix risc", "Alt risc"))
segon$vacgrip_2[segon$vacgrip_2=="si" | segon$vacgrip_2=="Si" | segon$vacgrip_2=="Sï"] <- "Sí"
segon$vacgrip_2[segon$vacgrip_2==""] <- NA
segon$vacgrip_2 <- factor(segon$vacgrip_2)
segon$datavacgrip_2 <- as.Date(segon$datavacgrip_2, format="%d/%m/%Y")
segon$vacpneumo_2[segon$vacpneumo_2=="no"] <- "No"
segon$vacpneumo_2[segon$vacpneumo_2=="Sï" | segon$vacpneumo_2=="Sí "] <- "Sí"
segon$vacpneumo_2[segon$vacpneumo_2==""] <- NA
segon$vacpneumo_2 <-factor(segon$vacpneumo_2)
segon$hba1c_2 <- gsub(",", ".", segon$hba1c_2)
segon$hba1c_2[segon$hba1c_2==31.45] <- NA
segon$hba1c_2 <- as.numeric(segon$hba1c_2)
segon$fev1_2 <- gsub("%", "", segon$fev1_2)
segon$fev1_2 <- gsub(",", ".", segon$fev1_2)
segon$fev1_2[segon$fev1_2==7100.00] <- 71.00
segon$fev1_2 <- as.numeric(segon$fev1_2)
segon$fev1fvc_2 <- gsub("%", "", segon$fev1fvc_2)
segon$fev1fvc_2 <- gsub(",", ".", segon$fev1fvc_2)
segon$fev1fvc_2 <- as.numeric(segon$fev1fvc_2)
segon <- segon[-c(which(colnames(segon)=="abs"), which(colnames(segon)=="sexe"), which(colnames(segon)=="dn"))]



# cleaning raw data (exacerbaciones) ####

# De los datos de exacerbaciones nos interesa el CIP y número de episodio para descartar episodios repetidos

respiratorio_pre <- respiratorio_pre[,c(3,4)]
names(respiratorio_pre) <- c("cip", "episodipre")
respiratorio1 <- respiratorio1[,c(2,4)]
names(respiratorio1) <- c("cip", "episodi1")
respiratorio2 <- respiratorio2[,c(3,4)]
names(respiratorio2) <- c("cip", "episodi2")

respiratorio1 <- unique(respiratorio1)
respiratorio2 <- unique(respiratorio2)
respiratorio_pre <- unique(respiratorio_pre)

respiratorio1$Episodios_1 <- 1
respiratorio2$Episodios_2 <- 1
respiratorio_pre$Episodios_pre <- 1

respiratorio1 <- respiratorio1 %>% group_by(cip) %>% summarise(Episodis_1 = sum(Episodios_1))
respiratorio2 <- respiratorio2 %>% group_by(cip) %>% summarise(Episodis_2 = sum(Episodios_2))
respiratorio_pre <- respiratorio_pre %>% group_by(cip) %>% summarise(Episodis_pre = sum(Episodios_pre))

respiratori_a <- merge(respiratorio_pre, respiratorio1, by="cip", all=T)
respiratori <- merge(respiratori_a, respiratorio2, by="cip", all=T)
rm(respiratori_a)


# merge data ####
# Nos quedamos con los datos de los 770 pacientes que hay en la cohorte

bd1 <- merge(pacients, primer, by="cip", all.x=T)
bd <- merge(bd1, segon, by="cip", all.x=T)
rm(bd1)



# cleaning processed data  ####

bd <- bd[!bd$cip=="GONÇ057072100",] # Elimino este paciente ya que no tiene datos
bd$fev1fvc_2 <- bd$fev1fvc_2/100

# Para las características basales que no esperamos que hayan cambiado con el tiempo, nos quedamos con el valor del primer año, si lo hay. Para el segundo año nos quedamos con las del segundo, si lo hay.
bd <- bd %>%  mutate(imc = coalesce(imc_1, imc_2), 
                     tabac = coalesce(tabac_1, tabac_2),
                     hta = coalesce(hta_1, hta_2),
                     ic = coalesce(ic_1, ic_2),
                     hta = coalesce(hta_1, hta_2),
                     ic = coalesce(ic_1, ic_2),
                     irc = coalesce(irc_1, irc_2),
                     enolic = coalesce(enolic_1, enolic_2),
                     hba1c = coalesce(hba1c_1, hba1c_2),
                     fev = coalesce(fev1_1, fev1_2),
                     fev1fvc = coalesce(fev1fvc_1, fev1fvc_2),
                     imc_segon = coalesce(imc_2, imc_1), 
                     tabac_segon = coalesce(tabac_2, tabac_1),
                     hta_segon = coalesce(hta_2, hta_1),
                     ic_segon = coalesce(ic_2, ic_1),
                     hta_segon = coalesce(hta_2, hta_1),
                     ic_segon = coalesce(ic_2, ic_1),
                     irc_segon = coalesce(irc_2, irc_1),
                     enolic_segon = coalesce(enolic_2, enolic_1),
                     hba1c_segon = coalesce(hba1c_2, hba1c_1),
                     fev_segon = coalesce(fev1_2, fev1_1),
                     fev1fvc_segon = coalesce(fev1fvc_2, fev1fvc_1)
                     
)



# Se unen los datos de exacerbaciones
df <- merge(bd, respiratori, by="cip", all.x=T)
rm(bd)

# Se definen nuevas variables
df$Episodis_1[is.na(df$Episodis_1)] <- 0
df$Episodis_2[is.na(df$Episodis_2)] <- 0
df$Episodis_pre[is.na(df$Episodis_pre)] <- 0

df$edades[bd$edat_1<65] <- "Edat1"
df$edades[bd$edat_1>=65 & bd$edat_1<75] <- "Edat2"
df$edades[bd$edat_1>=75] <- "Edat3"
df$edades <- factor(df$edades)

df$Episodis_tot <- df$Episodis_1 + df$Episodis_2

df$index_imc[df$imc<18.5] <- "Bajo peso"
df$index_imc[df$imc>=18.5 & df$imc<25] <- "Normopeso"
df$index_imc[df$imc>=25 & df$imc<30] <- "Sobrepeso"
df$index_imc[df$imc>=30] <- "Obesidad"
df$index_imc <- factor(df$index_imc, levels=c("Bajo peso", "Normopeso", "Sobrepeso", "Obesidad"))


df$GOLD <- NA
df$fev[df$fev>100] <- 100
df$GOLD[df$fev>=80] <- "GOLD 1"
df$GOLD[df$fev<80 & df$fev>=50] <- "GOLD 2"
df$GOLD[df$fev<50 & df$fev>=30] <- "GOLD 3"
df$GOLD[df$fev<30] <- "GOLD 4"
df$GOLD <- factor(df$GOLD)


df$GOLD_segon <- NA
df$fev_segon[df$fev_segon>100] <- 100
df$GOLD_segon[df$fev_segon>=80] <- "GOLD 1"
df$GOLD_segon[df$fev_segon<80 & df$fev_segon>=50] <- "GOLD 2"
df$GOLD_segon[df$fev_segon<50 & df$fev_segon>=30] <- "GOLD 3"
df$GOLD_segon[df$fev_segon<30] <- "GOLD 4"
df$GOLD_segon <- factor(df$GOLD_segon)

df$dep <- NA
df$dep[df$depressio_2>=8] <- "Si"
df$dep[df$depressio_2<8] <- "No"
table(df$dep)

df$ans <- NA
df$ans[df$ansietat_2>=8] <- "Si"
df$ans[df$ansietat_2<8] <- "No"
table(df$ans)

df$b1 <- NA
df$b1[df$imc>21] <- 0
df$b1[df$imc<=21] <- 1

df$o1 <- NA
df$o1[df$fev>=65] <- 0
df$o1[df$fev<=64 & df$fev>=50] <- 1
df$o1[df$fev<=49 & df$fev>=36] <- 2
df$o1[df$fev<=35] <- 3

df$d1 <- NA
df$d1[df$mrc_2==0 | df$mrc_2==1] <- 0
df$d1[df$mrc_2==2] <- 1
df$d1[df$mrc_2==3] <- 2
df$d1[df$mrc_2==4] <- 3

df$ex.1 <- NA
df$ex.1[df$Episodis_pre==0] <- 0
df$ex.1[df$Episodis_pre==1 | df$Episodis_pre==2] <- 1
df$ex.1[df$Episodis_pre>=3] <- 3

df$bodex <- df$b1 + df$o1 + df$d1 + df$ex.1
table(df$bodex)
df$n_bodex <- NA
df$n_bodex[df$bodex==0 | df$bodex==1 | df$bodex==2] <- "I. Leve"
df$n_bodex[df$bodex==3 | df$bodex==4] <- "II. Moderada"
df$n_bodex[df$bodex==5 | df$bodex==6] <- "III. Grave"
df$n_bodex[df$bodex==7 | df$bodex==8 | df$bodex==9] <- "IV. Muy grave"

df$b2 <- NA
df$b2[df$imc>21] <- 0
df$b2[df$imc<=21] <- 1

df$o2 <- NA
df$o2[df$fev>=65] <- 0
df$o2[df$fev<=64 & df$fev>=50] <- 1
df$o2[df$fev<=49 & df$fev>=36] <- 2
df$o2[df$fev<=35] <- 3

df$d2 <- NA
df$d2[df$mrc_2==0 | df$mrc_2==1] <- 0
df$d2[df$mrc_2==2] <- 1
df$d2[df$mrc_2==3] <- 2
df$d2[df$mrc_2==4] <- 3

df$ex.2 <- NA
df$ex.2[df$Episodis_1==0] <- 0
df$ex.2[df$Episodis_1==1 | df$Episodis_1==2] <- 1
df$ex.2[df$Episodis_1>=3] <- 3

df$bodex_segon <- df$b2 + df$o2 + df$d2 + df$ex.2
df$n_bodex_segon <- NA
df$n_bodex_segon[df$bodex_segon==0 | df$bodex_segon==1 | df$bodex_segon==2] <- "I. Leve"
df$n_bodex_segon[df$bodex_segon==3 | df$bodex_segon==4] <- "II. Moderada"
df$n_bodex_segon[df$bodex_segon==5 | df$bodex_segon==6] <- "III. Grave"
df$n_bodex_segon[df$bodex_segon==7 | df$bodex_segon==8 | df$bodex_segon==9] <- "IV. Muy grave"

df$abs[df$abs=="EAP Lleida 1 - Centre Històric-R.Ferran (A_LL)"] <- "ABS1"
df$abs[df$abs=="EAP LLEIDA-2 (CAP PRIMER DE MAIG)  (A_LL)"] <- "ABS2"
df$abs[df$abs=="EAP LLEIDA-3 (EIXAMPLE) (A_LL)"] <- "ABS3"
df$abs[df$abs=="EAP LLEIDA-4 (BALÀFIA-PARDINYES) (A_LL)"] <- "ABS4"
df$abs[df$abs=="EAP LLEIDA-5 (CAPPONT)  (A_LL)"] <- "ABS5"
df$abs[df$abs=="ABS LLEIDA-6 (BORDETA-MAGRANERS) (A_LL)"] <- "ABS6"
df$abs[df$abs=="EAP LLEIDA-7 - ONZE DE SETEMBRE (A_LL)"] <- "ABS7"

df$tbc <- df$tabac
df$tbc[df$tabac=="Ex-fumador"] <- "Fumador"
df$tbc <- droplevels(df$tbc)

df$exa1 <- NA
df$exa1[df$Episodis_1==0] <- "Sense exacerbacions"
df$exa1[df$Episodis_1==1 | df$Episodis_1==2] <- "1-2 exacerbacions"
df$exa1[df$Episodis_1>=3] <- "3 o més exacerbacions"
df$exa1 <- factor(df$exa1, levels=c("Sense exacerbacions", "1-2 exacerbacions", "3 o més exacerbacions"))

df$exa2 <- NA
df$exa2[df$Episodis_2==0] <- "Sense exacerbacions"
df$exa2[df$Episodis_2==1 | df$Episodis_2==2] <- "1-2 exacerbacions"
df$exa2[df$Episodis_2>=3] <- "3 o més exacerbacions"
df$exa2 <- factor(df$exa2, levels=c("Sense exacerbacions", "1-2 exacerbacions", "3 o més exacerbacions"))

df$ex1 <- NA
df$ex1[df$exa1=="Sense exacerbacions"] <- "Sense exacerbacions"
df$ex1[df$exa1=="1-2 exacerbacions" | df$exa1=="3 o més exacerbacions"] <- "Amb exacerbacions"
df$ex1 <- factor(df$ex1, levels=c("Sense exacerbacions", "Amb exacerbacions"))

df$ex2 <- NA
df$ex2[df$exa2=="Sense exacerbacions"] <- "Sense exacerbacions"
df$ex2[df$exa2=="1-2 exacerbacions" | df$exa2=="3 o més exacerbacions"] <- "Amb exacerbacions"
df$ex2 <- factor(df$ex2, levels=c("Sense exacerbacions", "Amb exacerbacions"))

df$exatot <- NA
df$exatot[df$Episodis_tot==0] <- "Sense exacerbacions"
df$exatot[df$Episodis_tot==1 | df$Episodis_tot==2] <- "1-2 exacerbacions"
df$exatot[df$Episodis_tot>=3] <- "3 o més exacerbacions"

df$exatot <- factor(df$exatot, levels=c("Sense exacerbacions", "1-2 exacerbacions", "3 o més exacerbacions"))


df$extot <- NA
df$extot[df$exatot=="Sense exacerbacions"] <- "Sense exacerbacions"
df$extot[df$exatot=="1-2 exacerbacions" | df$exatot=="3 o més exacerbacions"] <- "Amb exacerbacions"
df$extot <- factor(df$extot, levels=c("Sense exacerbacions", "Amb exacerbacions"))

df$edat_2[is.na(df$edat_2)] <- df$edat_1+1

any_1 <- df[df$exitus_1=="No",] # Tenim 717

any_1$GOLD <- factor(any_1$GOLD, levels=c("GOLD 1", "GOLD 2", "GOLD 3", "GOLD 4", "GOLD 3-4"))

any_1$GOLD[any_1$GOLD=="GOLD 3" | any_1$GOLD=="GOLD 4"] <- "GOLD 3-4"
any_1$GOLD <- factor(any_1$GOLD, levels=c("GOLD 1", "GOLD 2", "GOLD 3-4"))

any_11 <- subset(any_1, any_1$GOLD=="GOLD 1")
any_12 <- subset(any_1, any_1$GOLD=="GOLD 2")
any_13 <- subset(any_1, any_1$GOLD=="GOLD 3-4")
any_14 <- subset(any_1, any_1$GOLD=="GOLD 3-4")

any_1$exitus_2[is.na(any_1$exitus_2)] <- "No"  # Considero que si no diu res És pq no s'han mort
any_2 <- subset(any_1, any_1$exitus_2=="No") # Tenim 606 pacients

any_2$GOLD_segon <- factor(any_2$GOLD_segon, levels=c("GOLD 1", "GOLD 2", "GOLD 3", "GOLD 4", "GOLD 3-4"))

any_2$GOLD_segon[any_2$GOLD_segon=="GOLD 3" | any_2$GOLD_segon=="GOLD 4"] <- "GOLD 3-4"
any_2$GOLD_segon <- factor(any_2$GOLD_segon, levels=c("GOLD 1", "GOLD 2", "GOLD 3-4"))

any_21 <- subset(any_2, any_2$GOLD_segon=="GOLD 1")
any_22 <- subset(any_2, any_2$GOLD_segon=="GOLD 2")
any_23 <- subset(any_2, any_2$GOLD_segon=="GOLD 3-4")
any_24 <- subset(any_2, any_2$GOLD_segon=="GOLD 3-4")


