
# 3.3. Análisis multivariante

# Selecció de les variables


a11 <- subset(any_1, any_1$exa1=="Sense exacerbacions" )
a12 <- subset(any_1, any_1$exa1=="1-2 exacerbacions" | any_1$exa1=="3 o més exacerbacions")
a13 <- subset(any_1, any_1$exa1=="1-2 exacerbacions" | any_1$exa1=="3 o més exacerbacions")



cont <- function(var1, var2, var3){
  c(sum(!is.na(var1)), paste0(round(mean(var1, na.rm=T),2), " (", round(sd(var1, na.rm=T),2), ") [",round(min(var1, na.rm = T), 2), "-", round(max(var1, na.rm = T), 2), "]"),
    sum(!is.na(var2)), paste0(round(mean(var2, na.rm=T),2), " (", round(sd(var2, na.rm=T),2), ") [",round(min(var2, na.rm = T), 2), "-", round(max(var2, na.rm = T), 2), "]"),
    sum(!is.na(var3)), paste0(round(mean(var3, na.rm=T),2), " (", round(sd(var3, na.rm=T),2), ") [",round(min(var3, na.rm = T), 2), "-", round(max(var3, na.rm = T), 2), "]"))
}



cat <- function(var1, var2, var3, i){
  c(sum(!is.na(var1)), paste0(table(var1)[i], " (", round(table(var1)[i]/sum(!is.na(var1))*100, 2), "%)"),
    sum(!is.na(var2)), paste0(table(var2)[i], " (", round(table(var2)[i]/sum(!is.na(var2))*100, 2), "%)"),
    sum(!is.na(var3)), paste0(table(var3)[i], " (", round(table(var3)[i]/sum(!is.na(var3))*100, 2), "%)"))
}

taulax_1 <- rbind(c("", "", "Cap exacerbació", "", "1-2 exacerbacions", "", "3 o més exacerbacions", "p-valor"),
                  c("Edat", cont(a11$edat_1, a12$edat_1, a13$edat_1), anv(any_1$edat_1, any_1$ex1)),
                  c("Sexe", "", "", "", "", "", "", chi(any_1$sexe, any_1$ex1)),
                  c("    Dona", cat(a11$sexe, a12$sexe, a13$sexe, 1), ""),
                  c("    Home", cat(a11$sexe, a12$sexe, a13$sexe, 2), ""),
                  c("ABS", "", "", "", "", "", "", chi(any_1$abs, any_1$ex1)),
                  c("    1", cat(a11$abs, a12$abs, a13$abs, 1), ""),
                  c("    2", cat(a11$abs, a12$abs, a13$abs, 2), ""),
                  c("    3", cat(a11$abs, a12$abs, a13$abs, 3), ""),
                  c("    4", cat(a11$abs, a12$abs, a13$abs, 4), ""),
                  c("    5", cat(a11$abs, a12$abs, a13$abs, 5), ""),
                  c("    6", cat(a11$abs, a12$abs, a13$abs, 6), ""),
                  c("    7", cat(a11$abs, a12$abs, a13$abs, 7), ""),
                  c("IMC", cont(a11$imc, a12$imc, a13$imc), anv(any_1$imc, any_1$ex1)),
                  c("", "", "", "", "", "", "", chi(any_1$index_imc, any_1$ex1)),
                  c("    Bajo peso", cat(a11$index_imc, a12$index_imc, a13$index_imc, 1), ""),
                  c("    Normopeso", cat(a11$index_imc, a12$index_imc, a13$index_imc, 2), ""),
                  c("    Sobrepeso", cat(a11$index_imc, a12$index_imc, a13$index_imc, 3), ""),
                  c("    Obesidad", cat(a11$index_imc, a12$index_imc, a13$index_imc, 4), ""),
                  c("Hábito tabáquico", "", "", "", "", "", "", chi(any_1$tabac, any_1$ex1)),
                  c("    No fumador", cat(a11$tabac, a12$tabac, a13$tabac, 1), ""),
                  c("    Fumador + ex-fumador", cat(a11$tabac, a12$tabac, a13$tabac, 2), ""),
                  c("HTA", "", "", "", "", "", "", chi(any_1$hta, any_1$ex1)),
                  c("    No", cat(a11$hta, a12$hta, a13$hta, 1), ""),
                  c("    Sí", cat(a11$hta, a12$hta, a13$hta, 2), ""),
                  c("IC", "", "", "", "", "", "", chi(any_1$ic, any_1$ex1)),
                  c("    No", cat(a11$ic, a12$ic, a13$ic, 1), ""),
                  c("    Sí", cat(a11$ic, a12$ic, a13$ic, 2), ""),
                  c("IRC", "", "", "", "", "", "", chi(any_1$irc, any_1$ex1)),
                  c("    No", cat(a11$irc, a12$irc, a13$irc, 1), ""),
                  c("    Sí", cat(a11$irc, a12$irc, a13$irc, 2), ""),
                  c("Hábito enólico", "", "", "", "", "", "", chi(any_1$enolic, any_1$ex1)),
                  c("    Abstemio", cat(a11$enolic, a12$enolic, a13$enolic, 1), ""),
                  c("    Bajo riesgo", cat(a11$enolic, a12$enolic, a13$enolic, 2), ""),
                  c("    Alto riesgo", cat(a11$enolic, a12$enolic, a13$enolic, 3), ""),
                  c("Vacuna gripe", "", "", "", "", "", "", chi(any_1$vacgrip_1, any_1$ex1)),
                  c("    No", cat(a11$vacgrip_1, a12$vacgrip_1, a13$vacgrip_1, 1), ""),
                  c("    Sí", cat(a11$vacgrip_1, a12$vacgrip_1, a13$vacgrip_1, 2), ""),
                  c("Vacuna pneumococo", "", "", "", "", "", "", chi(any_1$vacpneumo_1, any_1$ex1)),
                  c("    No", cat(a11$vacpneumo_1, a12$vacpneumo_1, a13$vacpneumo_1, 1), ""),
                  c("    Sí", cat(a11$vacpneumo_1, a12$vacpneumo_1, a13$vacpneumo_1, 2), ""),
                  c("Ansiedad", "", "", "", "", "", "", chi(any_1$ans, any_1$ex1)),
                  c("    No", cat(a11$ans, a12$ans, a13$ans, 1), ""),
                  c("    Sí", cat(a11$ans, a12$ans, a13$ans, 2), ""),
                  c("Depresión", "", "", "", "", "", "", chi(any_1$dep, any_1$ex1)),
                  c("    No", cat(a11$dep, a12$dep, a13$dep, 1), ""),
                  c("    Sí", cat(a11$dep, a12$dep, a13$dep, 2), ""),
                  c("Charlson", cont(a11$charlson_2, a12$charlson_2, a13$charlson_2), anv(any_1$charlson_2, any_1$ex1)),
                  c("MRC", "", "", "", "", "", "", chi(any_1$mrc_2, any_1$ex1)),
                  c("    0", cat(a11$mrc_2, a12$mrc_2, a13$mrc_2, 1), ""),
                  c("    1", cat(a11$mrc_2, a12$mrc_2, a13$mrc_2, 2), ""),                 
                  c("    2", cat(a11$mrc_2, a12$mrc_2, a13$mrc_2, 3), ""),
                  c("    3", cat(a11$mrc_2, a12$mrc_2, a13$mrc_2, 4), ""),                  
                  c("    4", cat(a11$mrc_2, a12$mrc_2, a13$mrc_2, 5), ""),
                  c("    Bodex", cont(a11$bodex, a12$bodex, a13$bodex), anv(any_1$bodex, any_1$ex1)),
                  c("", "", "", "", "", "", "", chi(any_1$n_bodex, any_1$ex1)),
                  c("    Leve", cat(a11$n_bodex, a12$n_bodex, a13$n_bodex, 1), ""),                  
                  c("    Moderada", cat(a11$n_bodex, a12$n_bodex, a13$n_bodex, 2), ""),
                  c("    Grave", cat(a11$n_bodex, a12$n_bodex, a13$n_bodex, 3), ""),                 
                  c("    Muy grave", cat(a11$n_bodex, a12$n_bodex, a13$n_bodex, 4), ""),
                  c("Número de exacerbaciones previas", cont(a11$Episodis_pre, a12$Episodis_pre, a13$Episodis_pre), anv(any_1$Episodis_pre, any_1$ex1)),
                  c("GOLD", "", "", "", "", "", "", chi(any_1$GOLD, any_1$ex1)),
                  c("    1", cat(a11$GOLD, a12$GOLD, a13$GOLD, 1), ""),                 
                  c("    2", cat(a11$GOLD, a12$GOLD, a13$GOLD, 2), ""),
                  c("    3", cat(a11$GOLD, a12$GOLD, a13$GOLD, 3), ""),                  
                  c("    4", cat(a11$GOLD, a12$GOLD, a13$GOLD, 4), ""),
                  c("HBA1C", cont(a11$hba1c, a12$hba1c, a13$hba1c), anv(any_1$hba1c, any_1$ex1))
                  
)

write.csv(taulax_1, "Resultados/taula_x1.csv")

a21 <- subset(any_2, any_2$exa2=="Sense exacerbacions" )
a22 <- subset(any_2, any_2$exa2=="1-2 exacerbacions" | any_2$exa2=="3 o més exacerbacions")
a23 <- subset(any_2, any_2$exa2=="1-2 exacerbacions" | any_2$exa2=="3 o més exacerbacions")





taulax_2 <- rbind(c("", "", "Cap exacerbació", "", "1-2 exacerbacions", "", "3 o més exacerbacions", "p-valor"),
                  c("Edat", cont(a21$edat_2, a22$edat_2, a23$edat_2), anv(any_2$edat_2, any_2$ex2)),
                  c("Sexe", "", "", "", "", "", "", chi(any_2$sexe, any_2$ex2)),
                  c("    Dona", cat(a21$sexe, a22$sexe, a23$sexe, 1), ""),
                  c("    Home", cat(a21$sexe, a22$sexe, a23$sexe, 2), ""),
                  c("ABS", "", "", "", "", "", "", chi(any_2$abs, any_2$ex2)),
                  c("    1", cat(a21$abs, a22$abs, a23$abs, 1), ""),
                  c("    2", cat(a21$abs, a22$abs, a23$abs, 2), ""),
                  c("    3", cat(a21$abs, a22$abs, a23$abs, 3), ""),
                  c("    4", cat(a21$abs, a22$abs, a23$abs, 4), ""),
                  c("    5", cat(a21$abs, a22$abs, a23$abs, 5), ""),
                  c("    6", cat(a21$abs, a22$abs, a23$abs, 6), ""),
                  c("    7", cat(a21$abs, a22$abs, a23$abs, 7), ""),
                  c("IMC", cont(a21$imc_segon, a22$imc_segon, a23$imc_segon), anv(any_2$imc_segon, any_2$ex2)),
                  c("", "", "", "", "", "", "", chi(any_2$index_imc, any_2$ex2)),
                  c("    Bajo peso", cat(a21$index_imc, a22$index_imc, a23$index_imc, 1), ""),
                  c("    Normopeso", cat(a21$index_imc, a22$index_imc, a23$index_imc, 2), ""),
                  c("    Sobrepeso", cat(a21$index_imc, a22$index_imc, a23$index_imc, 3), ""),
                  c("    Obesidad", cat(a21$index_imc, a22$index_imc, a23$index_imc, 4), ""),
                  c("Hábito tabáquico", "", "", "", "", "", "", chi(any_2$tabac_segon, any_2$ex2)),
                  c("    No fumador", cat(a21$tabac_segon, a22$tabac_segon, a23$tabac_segon, 1), ""),
                  c("    Fumador + ex-fumador", cat(a21$tabac_segon, a22$tabac_segon, a23$tabac_segon, 2), ""),
                  c("HTA", "", "", "", "", "", "", chi(any_2$hta_segon, any_2$ex2)),
                  c("    No", cat(a21$hta_segon, a22$hta_segon, a23$hta_segon, 1), ""),
                  c("    Sí", cat(a21$hta_segon, a22$hta_segon, a23$hta_segon, 2), ""),
                  c("IC", "", "", "", "", "", "", chi(any_2$ic_segon, any_2$ex2)),
                  c("    No", cat(a21$ic_segon, a22$ic_segon, a23$ic_segon, 1), ""),
                  c("    Sí", cat(a21$ic_segon, a22$ic_segon, a23$ic_segon, 2), ""),
                  c("IRC", "", "", "", "", "", "", chi(any_2$irc_segon, any_2$ex2)),
                  c("    No", cat(a21$irc_segon, a22$irc_segon, a23$irc_segon, 1), ""),
                  c("    Sí", cat(a21$irc_segon, a22$irc_segon, a23$irc_segon, 2), ""),
                  c("Hábito enólico", "", "", "", "", "", "", chi(any_2$enolic_segon, any_2$ex2)),
                  c("    Abstemio", cat(a21$enolic_segon, a22$enolic, a23$enolic, 1), ""),
                  c("    Bajo riesgo", cat(a21$enolic_segon, a22$enolic_segon, a23$enolic_segon, 2), ""),
                  c("    Alto riesgo", cat(a21$enolic_segon, a22$enolic_segon, a23$enolic_segon, 3), ""),
                  c("Vacuna gripe", "", "", "", "", "", "", chi(any_2$vacgrip_2, any_2$ex2)),
                  c("    No", cat(a21$vacgrip_2, a22$vacgrip_2, a23$vacgrip_2, 1), ""),
                  c("    Sí", cat(a21$vacgrip_2, a22$vacgrip_2, a23$vacgrip_2, 2), ""),
                  c("Vacuna pneumococo", "", "", "", "", "", "", chi(any_2$vacpneumo_2, any_2$ex2)),
                  c("    No", cat(a21$vacpneumo_2, a22$vacpneumo_2, a23$vacpneumo_2, 1), ""),
                  c("    Sí", cat(a21$vacpneumo_2, a22$vacpneumo_2, a23$vacpneumo_2, 2), ""),
                  c("Ansiedad", "", "", "", "", "", "", chi(any_2$ans, any_2$ex2)),
                  c("    No", cat(a21$ans, a22$ans, a23$ans, 1), ""),
                  c("    Sí", cat(a21$ans, a22$ans, a23$ans, 2), ""),
                  c("Depresión", "", "", "", "", "", "", chi(any_2$dep, any_2$ex2)),
                  c("    No", cat(a21$dep, a22$dep, a23$dep, 1), ""),
                  c("    Sí", cat(a21$dep, a22$dep, a23$dep, 2), ""),
                  c("Charlson", cont(a21$charlson_2, a22$charlson_2, a23$charlson_2), anv(any_2$charlson_2, any_2$ex2)),
                  c("MRC", "", "", "", "", "", "", chi(any_2$mrc_2, any_2$ex2)),
                  c("    0", cat(a21$mrc_2, a22$mrc_2, a23$mrc_2, 1), ""),
                  c("    1", cat(a21$mrc_2, a22$mrc_2, a23$mrc_2, 2), ""),                 
                  c("    2", cat(a21$mrc_2, a22$mrc_2, a23$mrc_2, 3), ""),
                  c("    3", cat(a21$mrc_2, a22$mrc_2, a23$mrc_2, 4), ""),                  
                  c("    4", cat(a21$mrc_2, a22$mrc_2, a23$mrc_2, 5), ""),
                  c("    Bodex", cont(a21$bodex_segon, a22$bodex_segon, a23$bodex_segon), anv(any_2$bodex_segon, any_2$ex2)),
                  c("", "", "", "", "", "", "", chi(any_2$n_bodex_segon, any_2$ex2)),
                  c("    Leve", cat(a21$n_bodex_segon, a22$n_bodex_segon, a23$n_bodex_segon, 1), ""),                  
                  c("    Moderada", cat(a21$n_bodex_segon, a22$n_bodex_segon, a23$n_bodex_segon, 2), ""),
                  c("    Grave", cat(a21$n_bodex_segon, a22$n_bodex_segon, a23$n_bodex_segon, 3), ""),                 
                  c("    Muy grave", cat(a21$n_bodex_segon, a22$n_bodex_segon, a23$n_bodex_segon, 4), ""),
                  c("Número de exacerbaciones previas", cont(a21$Episodis_pre, a22$Episodis_pre, a23$Episodis_pre), anv(any_2$Episodis_pre, any_2$ex2)),
                  c("GOLD", "", "", "", "", "", "", chi(any_2$GOLD_segon, any_2$ex2)),
                  c("    1", cat(a21$GOLD_segon, a22$GOLD_segon, a23$GOLD_segon, 1), ""),                 
                  c("    2", cat(a21$GOLD_segon, a22$GOLD_segon, a23$GOLD_segon, 2), ""),
                  c("    3", cat(a21$GOLD_segon, a22$GOLD_segon, a23$GOLD_segon, 3), ""),                  
                  c("    4", cat(a21$GOLD_segon, a22$GOLD_segon, a23$GOLD_segon, 4), ""),
                  c("HBA1C", cont(a21$hba1c_segon, a22$hba1c_segon, a23$hba1c_segon), anv(any_2$hba1c_segon, any_2$ex2))
                  
)


a31 <- subset(any_2, any_2$exatot=="Sense exacerbacions" )
a32 <- subset(any_2, any_2$exatot=="1-2 exacerbacions" | any_2$exatot=="3 o més exacerbacions")
a33 <- subset(any_2, any_2$exatot=="1-2 exacerbacions" | any_2$exatot=="3 o més exacerbacions")



taulax_3 <- rbind(c("", "", "Cap exacerbació", "", "1-2 exacerbacions", "", "3 o més exacerbacions", "p-valor"),
                  c("Edat", cont(a31$edat_1, a32$edat_1, a33$edat_1), anv(any_2$edat_1, any_2$extot)),
                  c("Sexe", "", "", "", "", "", "", chi(any_2$sexe, any_2$extot)),
                  c("    Dona", cat(a31$sexe, a32$sexe, a33$sexe, 1), ""),
                  c("    Home", cat(a31$sexe, a32$sexe, a33$sexe, 2), ""),
                  c("ABS", "", "", "", "", "", "", chi(any_2$abs, any_2$extot)),
                  c("    1", cat(a31$abs, a32$abs, a33$abs, 1), ""),
                  c("    2", cat(a31$abs, a32$abs, a33$abs, 2), ""),
                  c("    3", cat(a31$abs, a32$abs, a33$abs, 3), ""),
                  c("    4", cat(a31$abs, a32$abs, a33$abs, 4), ""),
                  c("    5", cat(a31$abs, a32$abs, a33$abs, 5), ""),
                  c("    6", cat(a31$abs, a32$abs, a33$abs, 6), ""),
                  c("    7", cat(a31$abs, a32$abs, a33$abs, 7), ""),
                  c("IMC", cont(a31$imc, a32$imc, a33$imc), anv(any_2$imc, any_2$extot)),
                  c("", "", "", "", "", "", "", chi(any_2$index_imc, any_2$extot)),
                  c("    Bajo peso", cat(a31$index_imc, a32$index_imc, a33$index_imc, 1), ""),
                  c("    Normopeso", cat(a31$index_imc, a32$index_imc, a33$index_imc, 2), ""),
                  c("    Sobrepeso", cat(a31$index_imc, a32$index_imc, a33$index_imc, 3), ""),
                  c("    Obesidad", cat(a31$index_imc, a32$index_imc, a33$index_imc, 4), ""),
                  c("Hábito tabáquico", "", "", "", "", "", "", chi(any_2$tabac, any_2$extot)),
                  c("    No fumador", cat(a31$tabac, a32$tabac, a33$tabac, 1), ""),
                  c("    Fumador + ex-fumador", cat(a31$tabac, a32$tabac, a33$tabac, 2), ""),
                  c("HTA", "", "", "", "", "", "", chi(any_2$hta, any_2$extot)),
                  c("    No", cat(a31$hta, a32$hta, a33$hta, 1), ""),
                  c("    Sí", cat(a31$hta, a32$hta, a33$hta, 2), ""),
                  c("IC", "", "", "", "", "", "", chi(any_2$ic, any_2$extot)),
                  c("    No", cat(a31$ic, a32$ic, a33$ic, 1), ""),
                  c("    Sí", cat(a31$ic, a32$ic, a33$ic, 2), ""),
                  c("IRC", "", "", "", "", "", "", chi(any_2$irc, any_2$extot)),
                  c("    No", cat(a31$irc, a32$irc, a33$irc, 1), ""),
                  c("    Sí", cat(a31$irc, a32$irc, a33$irc, 2), ""),
                  c("Hábito enólico", "", "", "", "", "", "", chi(any_2$enolic, any_2$extot)),
                  c("    Abstemio", cat(a31$enolic, a32$enolic, a33$enolic, 1), ""),
                  c("    Bajo riesgo", cat(a31$enolic, a32$enolic, a33$enolic, 2), ""),
                  c("    Alto riesgo", cat(a31$enolic, a32$enolic, a33$enolic, 3), ""),
                  c("Vacuna gripe", "", "", "", "", "", "", chi(any_2$vacgrip_1, any_2$extot)),
                  c("    No", cat(a31$vacgrip_1, a32$vacgrip_1, a33$vacgrip_1, 1), ""),
                  c("    Sí", cat(a31$vacgrip_1, a32$vacgrip_1, a33$vacgrip_1, 2), ""),
                  c("Vacuna pneumococo", "", "", "", "", "", "", chi(any_2$vacpneumo_1, any_2$extot)),
                  c("    No", cat(a31$vacpneumo_1, a32$vacpneumo_1, a33$vacpneumo_1, 1), ""),
                  c("    Sí", cat(a31$vacpneumo_1, a32$vacpneumo_1, a33$vacpneumo_1, 2), ""),
                  c("Vacuna gripe 2", "", "", "", "", "", "", chi(any_2$vacgrip_2, any_2$extot)),
                  c("    No", cat(a31$vacgrip_2, a32$vacgrip_2, a33$vacgrip_2, 1), ""),
                  c("    Sí", cat(a31$vacgrip_2, a32$vacgrip_2, a33$vacgrip_2, 2), ""),
                  c("Vacuna pneumococo 2", "", "", "", "", "", "", chi(any_2$vacpneumo_2, any_2$extot)),
                  c("    No", cat(a31$vacpneumo_2, a32$vacpneumo_2, a33$vacpneumo_2, 1), ""),
                  c("    Sí", cat(a31$vacpneumo_2, a32$vacpneumo_2, a33$vacpneumo_2, 2), ""),
                  c("Ansiedad", "", "", "", "", "", "", chi(any_2$ans, any_2$extot)),
                  c("    No", cat(a31$ans, a32$ans, a33$ans, 1), ""),
                  c("    Sí", cat(a31$ans, a32$ans, a33$ans, 2), ""),
                  c("Depresión", "", "", "", "", "", "", chi(any_2$dep, any_2$extot)),
                  c("    No", cat(a31$dep, a32$dep, a33$dep, 1), ""),
                  c("    Sí", cat(a31$dep, a32$dep, a33$dep, 2), ""),
                  c("Charlson", cont(a31$charlson_2, a32$charlson_2, a33$charlson_2), anv(any_2$charlson_2, any_2$extot)),
                  c("MRC", "", "", "", "", "", "", chi(any_2$mrc_2, any_2$extot)),
                  c("    0", cat(a31$mrc_2, a32$mrc_2, a33$mrc_2, 1), ""),
                  c("    1", cat(a31$mrc_2, a32$mrc_2, a33$mrc_2, 2), ""),                 
                  c("    2", cat(a31$mrc_2, a32$mrc_2, a33$mrc_2, 3), ""),
                  c("    3", cat(a31$mrc_2, a32$mrc_2, a33$mrc_2, 4), ""),                  
                  c("    4", cat(a31$mrc_2, a32$mrc_2, a33$mrc_2, 5), ""),
                  c("    Bodex", cont(a31$bodex, a32$bodex, a33$bodex), anv(any_2$bodex, any_2$extot)),
                  c("", "", "", "", "", "", "", chi(any_2$n_bodex, any_2$extot)),
                  c("    Leve", cat(a31$n_bodex, a32$n_bodex, a33$n_bodex, 1), ""),                  
                  c("    Moderada", cat(a31$n_bodex, a32$n_bodex, a33$n_bodex, 2), ""),
                  c("    Grave", cat(a31$n_bodex, a32$n_bodex, a33$n_bodex, 3), ""),                 
                  c("    Muy grave", cat(a31$n_bodex, a32$n_bodex, a33$n_bodex, 4), ""),
                  c("Número de exacerbaciones previas", cont(a31$Episodis_pre, a32$Episodis_pre, a33$Episodis_pre), anv(any_2$Episodis_pre, any_2$extot)),
                  c("GOLD", "", "", "", "", "", "", chi(any_2$GOLD, any_2$extot)),
                  c("    1", cat(a31$GOLD, a32$GOLD, a33$GOLD, 1), ""),                 
                  c("    2", cat(a31$GOLD, a32$GOLD, a33$GOLD, 2), ""),
                  c("    3", cat(a31$GOLD, a32$GOLD, a33$GOLD, 3), ""),                  
                  c("    4", cat(a31$GOLD, a32$GOLD, a33$GOLD, 4), ""),
                  c("HBA1C", cont(a31$hba1c, a32$hba1c, a33$hba1c), anv(any_2$hba1c, any_2$extot))
                  
)

# Primer any

any1_model <- any_1[c(2,4,7,55,56,14,78,79,49,95,70,76,84,14,53, 60)]
prova <- na.omit(any1_model)

mylogit1 <- glm(ex1 ~ edat_1 + sexe +  tabac + vacgrip_1 + ic + irc  + ans + dep + charlson_2 + bodex + Episodis_pre + fev1fvc + GOLD, data = prova, family = "binomial")


summary(mylogit1)

backwards = step(mylogit1)

m1 <- glm(ex1 ~ sexe + dep + charlson_2 + Episodis_pre + fev1fvc , data=any_1, family = "binomial")


fm1 <- glm(ex1 ~ fev1fvc + GOLD + edat_1 + sexe, data = prova, family = "binomial")


fm.step1 <- step(fm1, scope=list(upper = ~  edat_1 + sexe +  tabac + vacgrip_1 + ic + irc  + ans + dep + charlson_2 + bodex + Episodis_pre + fev1fvc + GOLD, 
                                 lower = ~  fev1fvc + GOLD + edat_1 + sexe), trace=FALSE)





summary(fm.step1)

round(exp(confint(m1)),3)

# Segon any

# edat hta ic irc enol vacgrip ans dep charlson bodex exp gold

any_2$fev1fvc_segon

any_2_model <- any_2[c(28,63,64,65,66,35,78,79,49,90,71,77,96,62, 2, 69)]

prova2 <- na.omit(any_2_model)
mylogit2 <- glm(ex2 ~ edat_2 + tabac_segon +hta_segon + ic_segon + irc_segon + enolic_segon + ans + dep + charlson_2 + vacgrip_2 + bodex_segon + Episodis_1 + GOLD_segon, data = prova2, family = "binomial")


summary(mylogit2)

fm2 <- glm(ex2 ~ edat_2 + sexe +  GOLD_segon + fev1fvc_segon, data = prova2, family = "binomial")


fm.step2 <- step(fm2, scope=list(upper = ~  sexe +edat_2 + tabac_segon +hta_segon + ic_segon + irc_segon + enolic_segon + ans + dep + charlson_2 + vacgrip_2 + bodex_segon + Episodis_1 + GOLD_segon + fev1fvc_segon, 
                                 lower = ~  edat_2 + sexe +  GOLD_segon + fev1fvc_segon), trace=FALSE)

summary(fm.step2)



round(exp(coef(m2)),3)
round(exp(confint(m2)),3)

# Període total
# edat abs imc (cat) ic irc vacgrip1 vacpenu2 ans charlson bodex nexp gold

any_3_model <- any_2[c(7,4,75,55,56,14,37,79,49,84,98,70,76,60,2)]

prova3 <- na.omit(any_3_model)
mylogit3 <- glm(extot ~ edat_1 + abs + index_imc + ic + irc + vacgrip_1 + vacpneumo_2 + ans + charlson_2 + bodex + Episodis_pre , data = prova3, family = "binomial")


summary(mylogit3)

fm3 <- glm(extot ~  edat_1+ sexe + GOLD + Episodis_pre + fev1fvc , data = prova3, family = "binomial")
summary(fm3)

fm.step3 <- step(fm3, scope=list(upper = ~  sexe + edat_1 + abs + index_imc + ic + irc + vacgrip_1 + vacpneumo_2 + ans + charlson_2 + bodex +  GOLD + Episodis_pre + fev1fvc, 
                                 lower = ~  sexe + edat_1 + GOLD + Episodis_pre + fev1fvc), trace=FALSE)

summary(fm.step3)

summary(m3)

step(fm3, scope=list(upper = ~  sexe + edat_1 + abs + index_imc + ic + irc + vacgrip_1 + vacpneumo_2 + ans + charlson_2 + bodex +  GOLD + Episodis_pre + fev1fvc, 
                     lower = ~  sexe + edat_1 + GOLD + Episodis_pre + fev1fvc))

round(exp(coef(m3)),3)
round(exp(confint(m3)),3)



coef1 <- cbind(round(exp(fm.step1$coefficients),3), round(exp(confint(fm.step1))[,1],3), round(exp(confint(fm.step1))[,2],3),   round(summary(fm.step1)$coefficients[,4],3))

coef2 <- cbind(round(exp(fm.step2$coefficients),3), round(exp(confint(fm.step2))[,1],3), round(exp(confint(fm.step2))[,2],3),   round(summary(fm.step2)$coefficients[,4],3))

coef3 <- cbind(round(exp(fm.step3$coefficients),3), round(exp(confint(fm.step3))[,1],3), round(exp(confint(fm.step3))[,2],3),   round(summary(fm.step3)$coefficients[,4],3))

