a <- primer

a <- merge(pacients, a, by="cip", all.x=T)
a <- a[!a$cip=="GONÇ057072100",]
a$any_naix <- as.numeric(substr(a$dn, 7, 10))
a$any_vac <- pmin(a$pn23p_1, a$pn23s_1, a$pnc13_1, na.rm=T)
a$any_vac[a$any_vac=="20000"] <- 2000
a$any_vac[a$any_vac=="20009"] <- 2009
a$edat_vac <- a$any_vac - a$any_naix
a$g_edat <- NA
a$g_edat[a$edat_vac <40] <- "Menys de 40"
a$g_edat[a$edat_vac >= 40 & a$edat_vac < 65] <- "Entre 40 i 64"
a$g_edat[a$edat_vac >= 65 & a$edat_vac < 80] <- "Entre 65 i 79"
a$g_edat[a$edat_vac >= 80] <- "Majors de 80"
a$g_edat[is.na(a$edat_vac) ] <- "No vacunats"
table(a$g_edat, useNA = "ifany")
novac <- subset(a, a$g_edat=="No vacunats")
a$majors <- 0
a$majors[a$edat_1>=65] <- 1
table(a$majors, a$g_edat)