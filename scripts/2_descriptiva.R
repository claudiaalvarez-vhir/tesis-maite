# **********************************
#   Análisis tesis Maite Castañ
#     2. Descriptiva
# **********************************



gold1 <- subset(df, df$GOLD=="GOLD 1")
gold2 <- subset(df, df$GOLD=="GOLD 2")
gold3 <- subset(df, df$GOLD=="GOLD 3")
gold4 <- subset(df, df$GOLD=="GOLD 4")


media <- function(var){
  paste0(round(mean(var, na.rm=T),2), " (", round(sd(var, na.rm=T),2), ") [",round(min(var, na.rm = T), 2), "-", round(max(var, na.rm = T), 2), "]")
}

pct <- function(var, i){
  paste0(table(var)[i], " (", round(table(var)[i]/sum(!is.na(var))*100, 2), "%)")
}

chi <- function(var1, var2){
  round(chisq.test(var1,var2, simulate.p.value = T)$p.value, 3)
}

krus <- function(var1, var2){
  round(kruskal.test(var1, var2)$p.value, 3)
}



taula1 <- rbind(c("Variable", "nval", "GOLD1", "nval", "GOLD2", "nval", "GOLD3", "nval", "GOLD4", "nval", "Total", "p-valor"),
                c("", "", paste0("(n=", table(df$GOLD)[1], ")"), "", paste0("(n=", table(df$GOLD)[2], ")"),"", paste0("(n=", table(df$GOLD)[3], ")"), "", paste0("(n=", table(df$GOLD)[4], ")"), "",paste0("(n=", nrow(df), ")"),""),
                c("Edad", sum(!is.na(gold1$edat_1)), media(gold1$edat_1), sum(!is.na(gold2$edat_1)), media(gold2$edat_1), sum(!is.na(gold3$edat_1)), media(gold3$edat_1), sum(!is.na(gold4$edat_1)), media(gold4$edat_1), sum(!is.na(df$edat_1)), media(df$edat_1), krus(df$edat_1, df$GOLD)),
                c("Sexe", sum(!is.na(gold1$sexe)), "", sum(!is.na(gold2$sexe)), "", sum(!is.na(gold3$sexe)), "", sum(!is.na(gold4$sexe)), "", sum(!is.na(df$sexe)), "", chi(df$GOLD, df$sexe)),
                c("    Dona", "", pct(gold1$sexe, 1), "", pct(gold2$sexe, 1), "", pct(gold3$sexe, 1), "", pct(gold4$sexe, 1), "", pct(df$sexe, 1), ""),
                c("    Home", "", pct(gold1$sexe, 2), "", pct(gold2$sexe, 2), "", pct(gold3$sexe, 2), "", pct(gold4$sexe, 2), "", pct(df$sexe, 2), ""),
                c("ABS", sum(!is.na(gold1$abs)), "", sum(!is.na(gold2$abs)), "", sum(!is.na(gold3$abs)), "", sum(!is.na(gold4$abs)), "", sum(!is.na(df$abs)), "", chi(df$GOLD, df$abs)),
                c("    EAP Lleida 1 - Centre Històric-R.Ferran (A_LL)", "", pct(gold1$abs, 1), "", pct(gold2$abs, 1), "", pct(gold3$abs, 1), "", pct(gold4$abs, 1), "", pct(df$abs, 1), ""),
                c("    EAP LLEIDA-2 (CAP PRIMER DE MAIG)  (A_LL)", "", pct(gold1$abs, 2), "", pct(gold2$abs, 2), "", pct(gold3$abs, 2), "", pct(gold4$abs, 2), "", pct(df$abs, 2), ""),
                c("    EAP LLEIDA-3 (EIXAMPLE) (A_LL)", "", pct(gold1$abs, 3), "", pct(gold2$abs, 3), "", pct(gold3$abs, 3), "", pct(gold4$abs,3), "", pct(df$abs, 3), ""),
                c("    EAP LLEIDA-4 (BALÀFIA-PARDINYES) (A_LL)", "", pct(gold1$abs, 4), "", pct(gold2$abs, 4), "", pct(gold3$abs,4), "", pct(gold4$abs, 4), "", pct(df$abs, 4), ""),
                c("    EAP LLEIDA-5 (CAPPONT)  (A_LL)", "", pct(gold1$abs, 5), "", pct(gold2$abs, 5), "", pct(gold3$abs,5), "", pct(gold4$abs, 5), "", pct(df$abs, 5), ""),
                c("    ABS LLEIDA-6 (BORDETA-MAGRANERS) (A_LL)", "", pct(gold1$abs, 6), "", pct(gold2$abs, 6), "", pct(gold3$abs, 6), "", pct(gold4$abs, 6), "", pct(df$abs, 6), ""),
                c("    EAP LLEIDA-7 - ONZE DE SETEMBRE (A_LL)", "", pct(gold1$abs, 7), "", pct(gold2$abs, 7), "", pct(gold3$abs, 7), "", pct(gold4$abs, 7), "", pct(df$abs,7), ""),
                c("BMI", sum(!is.na(gold1$imc)), media(gold1$imc), sum(!is.na(gold2$imc)), media(gold2$imc), sum(!is.na(gold3$imc)), media(gold3$imc), sum(!is.na(gold4$imc)), media(gold4$imc), sum(!is.na(df$imc)), media(df$imc), krus(df$imc, df$GOLD)),
                c("","","", "", "", "", "", "", "", "", "",chi(df$GOLD, df$index_imc)),
                c("    Bajo peso", "", pct(gold1$index_imc, 1), "", pct(gold2$index_imc, 1), "", pct(gold3$index_imc, 1), "", pct(gold4$index_imc, 1), "", pct(df$index_imc, 1), ""),
                c("    Normopeso", "", pct(gold1$index_imc, 2), "", pct(gold2$index_imc, 2), "", pct(gold3$index_imc, 2), "", pct(gold4$index_imc, 2), "", pct(df$index_imc, 2), ""),
                c("    Sobrepeso", "", pct(gold1$index_imc, 3), "", pct(gold2$index_imc, 3), "", pct(gold3$index_imc, 3), "", pct(gold4$index_imc,3), "", pct(df$index_imc, 3), ""),
                c("    Obesidad", "", pct(gold1$index_imc, 4), "", pct(gold2$index_imc, 4), "", pct(gold3$index_imc,4), "", pct(gold4$index_imc, 4), "", pct(df$index_imc, 4), ""),
                c("Hábito tabáquico", sum(!is.na(gold1$tbc)), "", sum(!is.na(gold2$tbc)), "", sum(!is.na(gold3$tbc)), "", sum(!is.na(gold4$tbc)), "", sum(!is.na(df$tbc)), "", chi(df$GOLD, df$tbc)),
                c("    No fumador", "", pct(gold1$tbc, 1), "", pct(gold2$tbc, 1), "", pct(gold3$tbc, 1), "", pct(gold4$tbc, 1), "", pct(df$tbc, 1), ""),
                c("    Fumador + ex-fumador", "", pct(gold1$tbc, 2), "", pct(gold2$tbc, 2), "", pct(gold3$tbc, 2), "", pct(gold4$tbc, 2), "", pct(df$tbc, 2), ""),
                c("Depresión", sum(!is.na(gold1$dep)), "", sum(!is.na(gold2$dep)), "", sum(!is.na(gold3$dep)), "", sum(!is.na(gold4$dep)), "", sum(!is.na(df$dep)), "", chi(df$GOLD, df$dep)),
                c("    No", "", pct(gold1$dep, 1), "", pct(gold2$dep, 1), "", pct(gold3$dep, 1), "", pct(gold4$dep, 1), "", pct(df$dep, 1), ""),
                c("    Sí", "", pct(gold1$dep, 2), "", pct(gold2$dep, 2), "", pct(gold3$dep, 2), "", pct(gold4$dep, 2), "", pct(df$dep, 2), ""),
                c("Ansiedad", sum(!is.na(gold1$ans)), "", sum(!is.na(gold2$ans)), "", sum(!is.na(gold3$ans)), "", sum(!is.na(gold4$ans)), "", sum(!is.na(df$ans)), "", chi(df$GOLD, df$ans)),
                c("    No", "", pct(gold1$ans, 1), "", pct(gold2$ans, 1), "", pct(gold3$ans, 1), "", pct(gold4$ans, 1), "", pct(df$ans, 1), ""),
                c("    Sí", "", pct(gold1$ans, 2), "", pct(gold2$ans, 2), "", pct(gold3$ans, 2), "", pct(gold4$ans, 2), "", pct(df$ans, 2), ""),
                c("FEV", sum(!is.na(gold1$fev)), media(gold1$fev), sum(!is.na(gold2$fev)), media(gold2$fev), sum(!is.na(gold3$fev)), media(gold3$fev), sum(!is.na(gold4$fev)), media(gold4$fev), sum(!is.na(df$fev)), media(df$fev), krus(df$fev, df$GOLD)),
                c("FEV1.FVC", sum(!is.na(gold1$fev1fvc)), media(gold1$fev1fvc), sum(!is.na(gold2$fev1fvc)), media(gold2$fev1fvc), sum(!is.na(gold3$fev1fvc)), media(gold3$fev1fvc), sum(!is.na(gold4$fev1fvc)), media(gold4$fev1fvc), sum(!is.na(df$fev1fvc)), media(df$fev1fvc), krus(df$fev1fvc, df$GOLD)),
                c("Insuficiencia cardíaca", sum(!is.na(gold1$ic)), "", sum(!is.na(gold2$ic)), "", sum(!is.na(gold3$ic)), "", sum(!is.na(gold4$ic)), "", sum(!is.na(df$ic)), "", chi(df$GOLD, df$ic)),
                c("    No", "", pct(gold1$ic, 1), "", pct(gold2$ic, 1), "", pct(gold3$ic, 1), "", pct(gold4$ic, 1), "", pct(df$ic, 1), ""),
                c("    Sí", "", pct(gold1$ic, 2), "", pct(gold2$ic, 2), "", pct(gold3$ic, 2), "", pct(gold4$ic, 2), "", pct(df$ic, 2), ""),
                c("Insuficiencia renal crónica", sum(!is.na(gold1$irc)), "", sum(!is.na(gold2$irc)), "", sum(!is.na(gold3$irc)), "", sum(!is.na(gold4$irc)), "", sum(!is.na(df$irc)), "", chi(df$GOLD, df$irc)),
                c("    No", "", pct(gold1$irc, 1), "", pct(gold2$irc, 1), "", pct(gold3$irc, 1), "", pct(gold4$irc, 1), "", pct(df$irc, 1), ""),
                c("    Sí", "", pct(gold1$irc, 2), "", pct(gold2$irc, 2), "", pct(gold3$irc, 2), "", pct(gold4$irc, 2), "", pct(df$irc, 2), ""),
                c("Hipertensión arterial", sum(!is.na(gold1$hta)), "", sum(!is.na(gold2$hta)), "", sum(!is.na(gold3$hta)), "", sum(!is.na(gold4$hta)), "", sum(!is.na(df$hta)), "", chi(df$GOLD, df$hta)),
                c("    No", "", pct(gold1$hta, 1), "", pct(gold2$hta, 1), "", pct(gold3$hta, 1), "", pct(gold4$hta, 1), "", pct(df$hta, 1), ""),
                c("    Sí", "", pct(gold1$hta, 2), "", pct(gold2$hta, 2), "", pct(gold3$hta, 2), "", pct(gold4$hta, 2), "", pct(df$hta, 2), ""),
                c("Vacuna antigripal primer año", sum(!is.na(gold1$vacgrip_1)), "", sum(!is.na(gold2$vacgrip_1)), "", sum(!is.na(gold3$vacgrip_1)), "", sum(!is.na(gold4$vacgrip_1)), "", sum(!is.na(df$vacgrip_1)), "", chi(df$GOLD, df$vacgrip_1)),
                c("    No", "", pct(gold1$vacgrip_1, 1), "", pct(gold2$vacgrip_1, 1), "", pct(gold3$vacgrip_1, 1), "", pct(gold4$vacgrip_1, 1), "", pct(df$vacgrip_1, 1), ""),
                c("    Sí", "", pct(gold1$vacgrip_1, 2), "", pct(gold2$vacgrip_1, 2), "", pct(gold3$vacgrip_1, 2), "", pct(gold4$vacgrip_1, 2), "", pct(df$vacgrip_1, 2), ""),
                c("Vacuna antipneumocócica primer año", sum(!is.na(gold1$vacpneumo_1)), "", sum(!is.na(gold2$vacpneumo_1)), "", sum(!is.na(gold3$vacpneumo_1)), "", sum(!is.na(gold4$vacpneumo_1)), "", sum(!is.na(df$vacpneumo_1)), "", chi(df$GOLD, df$vacpneumo_1)),
                c("    No", "", pct(gold1$vacpneumo_1, 1), "", pct(gold2$vacpneumo_1, 1), "", pct(gold3$vacpneumo_1, 1), "", pct(gold4$vacpneumo_1, 1), "", pct(df$vacpneumo_1, 1), ""),
                c("    Sí", "", pct(gold1$vacpneumo_1, 2), "", pct(gold2$vacpneumo_1, 2), "", pct(gold3$vacpneumo_1, 2), "", pct(gold4$vacpneumo_1, 2), "", pct(df$vacpneumo_1, 2), ""),
                c("Vacuna antigripal segundo año", sum(!is.na(gold1$vacgrip_2)), "", sum(!is.na(gold2$vacgrip_2)), "", sum(!is.na(gold3$vacgrip_2)), "", sum(!is.na(gold4$vacgrip_2)), "", sum(!is.na(df$vacgrip_2)), "", chi(df$GOLD, df$vacgrip_2)),
                c("    No", "", pct(gold1$vacgrip_2, 1), "", pct(gold2$vacgrip_2, 1), "", pct(gold3$vacgrip_2, 1), "", pct(gold4$vacgrip_2, 1), "", pct(df$vacgrip_2, 1), ""),
                c("    Sí", "", pct(gold1$vacgrip_2, 2), "", pct(gold2$vacgrip_2, 2), "", pct(gold3$vacgrip_2, 2), "", pct(gold4$vacgrip_2, 2), "", pct(df$vacgrip_2, 2), ""),
                c("Vacuna antipneumocócica segundo año", sum(!is.na(gold1$vacpneumo_2)), "", sum(!is.na(gold2$vacpneumo_2)), "", sum(!is.na(gold3$vacpneumo_2)), "", sum(!is.na(gold4$vacpneumo_2)), "", sum(!is.na(df$vacpneumo_2)), "", chi(df$GOLD, df$vacpneumo_2)),
                c("    No", "", pct(gold1$vacpneumo_2, 1), "", pct(gold2$vacpneumo_2, 1), "", pct(gold3$vacpneumo_2, 1), "", pct(gold4$vacpneumo_2, 1), "", pct(df$vacpneumo_2, 1), ""),
                c("    Sí", "", pct(gold1$vacpneumo_2, 2), "", pct(gold2$vacpneumo_2, 2), "", pct(gold3$vacpneumo_2, 2), "", pct(gold4$vacpneumo_2, 2), "", pct(df$vacpneumo_2, 2), ""),
                c("Hábito enólico", sum(!is.na(gold1$enolic)), "", sum(!is.na(gold2$enolic)), "", sum(!is.na(gold3$enolic)), "", sum(!is.na(gold4$enolic)), "", sum(!is.na(df$enolic)), "", chi(df$GOLD, df$enolic)),
                c("    Abstemio", "", pct(gold1$enolic, 1), "", pct(gold2$enolic, 1), "", pct(gold3$enolic, 1), "", pct(gold4$enolic, 1), "", pct(df$enolic, 1), ""),
                c("    Bajo riesgo", "", pct(gold1$enolic, 2), "", pct(gold2$enolic, 2), "", pct(gold3$enolic, 2), "", pct(gold4$enolic, 2), "", pct(df$enolic, 2), ""),
                c("    Alto riesgo", "", pct(gold1$enolic, 3), "", pct(gold2$enolic, 3), "", pct(gold3$enolic, 3), "", pct(gold4$enolic,3), "", pct(df$enolic, 3), ""),
                c("Charlson", sum(!is.na(gold1$charlson_2)), media(gold1$charlson_2), sum(!is.na(gold2$charlson_2)), media(gold2$charlson_2), sum(!is.na(gold3$charlson_2)), media(gold3$charlson_2), sum(!is.na(gold4$charlson_2)), media(gold4$charlson_2), sum(!is.na(df$charlson_2)), media(df$charlson_2), krus(df$charlson_2, df$GOLD)),
                c("MRC", sum(!is.na(gold1$mrc_2)), "", sum(!is.na(gold2$mrc_2)), "", sum(!is.na(gold3$mrc_2)), "", sum(!is.na(gold4$mrc_2)), "", sum(!is.na(df$mrc_2)), "", chi(df$GOLD, df$mrc_2)),
                c("    0", "", pct(gold1$mrc_2, 1), "", pct(gold2$mrc_2, 1), "", pct(gold3$mrc_2, 1), "", pct(gold4$mrc_2, 1), "", pct(df$mrc_2, 1), ""),
                c("    1", "", pct(gold1$mrc_2, 2), "", pct(gold2$mrc_2, 2), "", pct(gold3$mrc_2, 2), "", pct(gold4$mrc_2, 2), "", pct(df$mrc_2, 2), ""),
                c("    2", "", pct(gold1$mrc_2, 3), "", pct(gold2$mrc_2, 3), "", pct(gold3$mrc_2, 3), "", pct(gold4$mrc_2,3), "", pct(df$mrc_2, 3), ""),
                c("    3", "", pct(gold1$mrc_2, 4), "", pct(gold2$mrc_2, 4), "", pct(gold3$mrc_2,4), "", pct(gold4$mrc_2, 4), "", pct(df$mrc_2, 4), ""),
                c("    4", "", pct(gold1$mrc_2, 5), "", pct(gold2$mrc_2, 5), "", pct(gold3$mrc_2,5), "", pct(gold4$mrc_2, 5), "", pct(df$mrc_2, 5), ""),
                c("Bodex", sum(!is.na(gold1$bodex)), media(gold1$bodex), sum(!is.na(gold2$bodex)), media(gold2$bodex), sum(!is.na(gold3$bodex)), media(gold3$bodex), sum(!is.na(gold4$bodex)), media(gold4$bodex), sum(!is.na(df$bodex)), media(df$bodex), krus(df$bodex, df$GOLD)),
                c("","","", "", "", "", "", "", "", "", "",chi(df$GOLD, df$n_bodex)),
                c("    Leve", "", pct(gold1$n_bodex, 1), "", pct(gold2$n_bodex, 1), "", pct(gold3$n_bodex, 1), "", pct(gold4$n_bodex, 1), "", pct(df$n_bodex, 1), ""),
                c("    Moderada", "", pct(gold1$n_bodex, 2), "", pct(gold2$n_bodex, 2), "", pct(gold3$n_bodex, 2), "", pct(gold4$n_bodex, 2), "", pct(df$n_bodex, 2), ""),
                c("    Grave", "", pct(gold1$n_bodex, 3), "", pct(gold2$n_bodex, 3), "", pct(gold3$n_bodex, 3), "", pct(gold4$n_bodex,3), "", pct(df$n_bodex, 3), ""),
                c("    Muy grave", "", pct(gold1$n_bodex, 4), "", pct(gold2$n_bodex, 4), "", pct(gold3$n_bodex,4), "", pct(gold4$n_bodex, 4), "", pct(df$n_bodex, 4), ""),
                c("HBA1C", sum(!is.na(gold1$hba1c)), media(gold1$hba1c), sum(!is.na(gold2$hba1c)), media(gold2$hba1c), sum(!is.na(gold3$hba1c)), media(gold3$hba1c), sum(!is.na(gold4$hba1c)), media(gold4$hba1c), sum(!is.na(df$hba1c)), media(df$hba1c), krus(df$hba1c, df$GOLD))
                
)

