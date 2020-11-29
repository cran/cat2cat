# pacman::p_load(tidyr,readr,foreign,ggplot2,dplyr,stringr,pdftools,broom,vroom)
#
# baza_final = readRDS("../kzis_base/baza_raw.Rds")
# set.seed(1234)
# bb = baza_final %>% rename(exp = staz, sex = plec, age = wiek , district = woj, employees = liczbaZat,
#                            multiplier = mnoznikk, id = id, year = Rok, code = zawod, edu = wyksz, salary = wyn_ogol_SUM ,
#                            parttime = etat_SUM, firmid = nrjed, sector = sekcja, contract = rodzaj_umowy) %>%
#   select(id,firmid,age,sex,edu,exp,district,parttime,salary,code,multiplier,year) %>%
#   mutate(year = as.integer(year), exp_age_r = runif(n(),0,4),
#          edu = as.integer(edu), sex = sex == "1", age = as.integer(age) + exp_age_r  , exp = exp + exp_age_r,
#          salary = salary + runif(n(),0,2000), code4 = substr(code, 1, 4), parttime = parttime/12,
#          district = as.integer(as.factor(district)) + 1L) %>% select(-exp_age_r)
# prop_s = 0.05
# occup = bb %>% filter(year %in% c(2008,2010)) %>% group_by(year,firmid) %>%
#   sample_frac(prop_s) %>% ungroup() %>% select(-firmid) %>%
#   mutate(id = 1:n(), multiplier = (multiplier+ runif(n(),0,3))*1/prop_s   )
# usethis::use_data(occup, overwrite  = T)
# # vroom::vroom_write(occup, 'occup.csv.bz2')
# # vroom::vroom('/home/maciej/Desktop/own_R_packages/cat2cat/occup.csv.bz2', col_types = 'iiliddccddicii')
#
# trans = haven::read_dta("~/Desktop/kzis/Data_Preproc/klucz.dta")
# colnames(trans) <- c('old','new')
# trans$old = substr(trans$old, 1 ,4)
# trans = unique(trans)
# usethis::use_data(trans, overwrite  = T)
#
# # ng = pdftools::pdf_text("~/Desktop/kzis/archive_pdf/KZiS_nowa.pdf")
# # nggg =  stringr::str_match_all(ng,"(\\d*)\\s*((?:[[:alpha:]]|[[:punct:]]|[[:space:]])*)")
# # ngggg = do.call(rbind,nggg)
# # ngggg = ngggg [9:nrow(ngggg),]  #remove intro rows
# # nazwy_grup_2010 = data.frame(kod=ngggg[,2],nazwa = stringr::str_to_title(trimws(gsub("\\s{2,10}","",gsub("\r\n|\\*","",ngggg[,3])))),stringsAsFactors = F)
# # nazwy_grup_2010 = nazwy_grup_2010[!nazwy_grup_2010$nazwa=="",]
# # write.csv(nazwy_grup_2010,"/home/maciej/Desktop/own_R_packages/catTOcat/R/nazwy_grup_2010.csv",row.names=FALSE)
# # nazwy_grup_2010 = read.csv("/home/maciej/Desktop/own_R_packages/catTOcat/R/nazwy_grup_2010.csv",stringsAsFactors = F)
# #
# # ng = pdftools::pdf_text("~/Desktop/kzis/archive_pdf/2004_ZALACZNIK-klasyfikacja.pdf")
# # ngg = ng[-c(1:9)]  #remove intro pages
# # nggg =  str_match_all(ngg,"(\\d*)\\s*((?:[[:alpha:]]|[[:punct:]]|[[:space:]])*)")
# # ngggg = do.call(rbind,nggg)
# # nazwy_grup_2008 = data.frame(kod=ngggg[,2],nazwa = str_to_title(trimws(gsub("\\s{2,10}","",gsub("\r\n|\\*","",ngggg[,3])))),stringsAsFactors = F)
# # nazwy_grup_2008 = nazwy_grup_2008[!nazwy_grup_2008$nazwa=="",]
# # write.csv(nazwy_grup_2008,"./R/nazwy_grup_2008.csv",row.names=FALSE)
# #
# # nazwy_grup_2008 = read.csv("~/Desktop/kzis/Data_Preproc/nazwy_grup_2008.csv",stringsAsFactors = F)
# #
# # setwd("C:\\Users\\user\\Desktop\\kzis\\DaneZ12_Bonus")
# #
# # jobn<-readChar("jobs.txt",file.info("jobs.txt")$size)
# # split_jobn<-strsplit(jobn,"\\s")
# # subn<-grep("[0-9]",(split_jobn[[1]]))
# # num<-split_jobn[[1]][subn]
# # subn2<-c(subn,(length(split_jobn[[1]])+1))
# # s1<-sapply(1:(length(subn2)-1),function(i) paste(split_jobn[[1]][((subn2[i])+1):((subn2[i+1])-1)],collapse=" "))
# # nazwy_grup_2010<-data.frame(kod=as.character(num),nazwa=as.character(s1),stringsAsFactors =FALSE)
# # Encoding(nazwy_grup_2010$nazwa) <- "latin-1"
# # from_replace = c("ę","ą","ż","ł","ó","ś","ć","ź")
# # to_replace = c("e","a","z","l","o","s","c","z")
# # for(i in seq_along(to_replace)){nazwy_grup_2010$nazwa <- gsub(from_replace[i],to_replace[i],nazwy_grup_2010$nazwa)}
