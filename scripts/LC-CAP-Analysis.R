setwd(dir = "D:/Personnal Files/Gilbert Forciers/CAP/")

library(tidyverse)
library(questionr)
library(dlookr)
library(GGally)
library(gtsummary)
library(readxl)
library(rio)
library(Hmisc)

# d1 <- rio::import(file = "Données enquêtes CAP  TAPE SK - R.xlsx")

d1 <- read_xlsx(path = "Données enquêtes CAP  TAPE SK - R.xlsx", sheet = "CAP",col_names = T)
d1 <- d1 %>% select(-n8,-n9,-n10,-n11)
df <- read_xlsx(path = "Données enquêtes CAP  TAPE SK - R.xlsx", sheet = "CAP-Data",col_names = T)
# df <- df %>% 
#   select(-starts_with("_"))


d1 %>% select(starts_with('C...')) %>% names()
d1 %>% select(starts_with('P...')) %>% names()
d1 %>% select(starts_with('A...')) %>% names()



# Position des variables
connaissance <-grep('^C...', colnames(d1))
attitude <- grep('^A...', colnames(d1))
pratique <- grep('^P...', colnames(d1))


con <-df[connaissance] %>% names()
att <- df[attitude] %>% names()
prat <- df[pratique] %>%  names()




con <- df %>% 
  select(all_of(con)) %>% 
  select(!where(is.character)) %>%
  names()


att <- df %>% 
  select(all_of(att)) %>% 
  select(!where(is.character)) %>%
  names()



prat <- df %>% 
  select(all_of(prat)) %>% 
  select(!where(is.character)) %>%
  names()

# Verifier le types des variables à mettre pour chaque section du CAP
df %>% select(all_of(con)) %>% str()
df %>% select(all_of(con)) %>% select(!where(is.character)) %>% names()
df %>% select(all_of(con)) %>% select(!where(is.character)) %>% str()

df %>% select(all_of(att)) %>% str()
df %>% select(all_of(att)) %>% select(where(is.character)) %>% names()
df %>% select(all_of(con)) %>% select(where(is.character)) %>% View()



# Calcul des cores
# df <- df %>% 
#   rowwise() %>% 
#   mutate(score_connaissance=sum((all_of(con)),na.rm = T),
#          score_attitude=sum(c(all_of(att)), na.rm = T),
#          score_pratique=sum(c(all_of(prat)),na.rm = T))
# 
# 
# df <- df %>% 
#   rowwise() %>% 
#   mutate(score_connaissance=sum((all_of(df %>% select(all_of(con)) %>% select(where(is.numeric)))),na.rm = T),
#          score_attitude=sum(c(all_of(att)), na.rm = T),
#          score_pratique=sum(c(all_of(prat)),na.rm = T))
# 
# 
# 
# df <- df %>% mutate(Sc_Connaiss=reduce(select(.,all_of(con)),`+`))
# 
# df <- df %>%
#   rowwise() %>%
#   mutate(Sc_Connaiss = reduce(select(., all_of(con)), ~ sum(., na.rm = TRUE)))




grep("^2. ",colnames(df))
df %>% select(all_of(grep("^2. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^2. ",colnames(df)))) %>% View()
df$connaissance1 <- ifelse(rowSums(df[,c(18, 19, 20,21,22)]) %>%  replace_na(0) >=2, 1,0)


df <- df %>% 
  mutate(
    connaissance2=
      `5. Connaissez-vous les 3 meilleures espèces animales domestiques de la région adaptéees aux conditions agro-climatiques ?`
  )


grep("^8. ",colnames(df))
df %>% select(all_of(grep("^8. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^8. ",colnames(df)))) %>% View()
df$connaissance3 <- ifelse(rowSums(df[,c(68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86)]) %>%  replace_na(0) >=5, 1,0)



# section 11
grep("^11.",colnames(df))
df %>% select(all_of(grep("^11.",colnames(df)))) %>% names()
df$connaissance4=ifelse(rowSums(df[,c(91,92,93,94,95,96,97,98,100)]) %>% replace_na(0)>=2,1,0)


grep("^19.",colnames(df))
df %>% select(all_of(grep("^19.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^19.",colnames(df)))) %>% View()
df$connaissance5=ifelse(rowSums(df[,c(120:124)]) %>% replace_na(0)>=2,1,0)


# df <- df %>%  mutate(connaissance_5a=connaissance_5a %>% replace_na(0),
#                      connaissance_6=ifelse(`23. Pour vous, que recouvre la diversité biologique, que l'on appelle aussi la nature ?/je ne sais pas`==1,0,1)
#                      )


grep("^27.1",colnames(df))
df %>% select(all_of(grep("^27.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^27.",colnames(df)))) %>% View()
df$connaissance6=ifelse(rowSums(df[,c(144, 145, 146, 147, 148, 149, 150 ,151)]) %>% replace_na(0)>=3,1,0)


grep("^28.1",colnames(df))
df %>% select(all_of(grep("^28.1.",colnames(df)))) %>% names()
df$connaissance7 <-ifelse(rowSums(df[,c(155,156,157,158, 159,160, 161)]) %>% replace_na(0)>=3,1,0)

grep("^30.1",colnames(df))
df %>% select(all_of(grep("^28.1.",colnames(df)))) %>% names()
df$connaissance8 <-ifelse(rowSums(df[,c(167, 168,169, 170, 171)]) %>% replace_na(0)>=3,1,0)



grep("^33",colnames(df))
df %>% select(all_of(grep("^33",colnames(df)))) %>% names()
df %>% select(all_of(grep("^33",colnames(df)))) %>% View()
df$connaissance9 <- ifelse(rowSums(df[,c(176, 177, 178, 179, 180, 181, 182, 183)]) %>% replace_na(0)>=3,1,0)

grep("^42",colnames(df))
df %>% select(all_of(grep("^42",colnames(df)))) %>% names()
df %>% select(all_of(grep("^42",colnames(df)))) %>% View()
df$connaissance10 <- ifelse(rowSums(df[,c(204, 205, 206, 207, 208)]) %>% replace_na(0)>=3,1,0)



grep("^46.1",colnames(df))
df %>% select(all_of(grep("^48",colnames(df)))) %>% names()
df %>% select(all_of(grep("^48",colnames(df)))) %>% View()

df$connaissance11 <- ifelse(df$`46.  Pouvez-vous citer 3 avantages d'un foyer amÃ©liorÃ© ?`==1 &
                               replace_na(rowSums(df[c(217, 218, 219, 220, 221)]),0)  >=3, 1,0)
  

grep("^48",colnames(df))
df %>% select(all_of(grep("^48",colnames(df)))) %>% names()
df %>% select(all_of(grep("^48",colnames(df)))) %>% View()
df$connaissance12 <- ifelse(rowSums(df[c(225,226,228)]) %>% replace_na(0)  >=1, 1,0)


grep("^52",colnames(df))
df %>% select(all_of(grep("^52",colnames(df)))) %>% names()
df %>% select(all_of(grep("^52",colnames(df)))) %>% View()
df$connaissance13 <- ifelse(rowSums(df[c(235, 236, 237)]) %>% replace_na(0)  >=3, 1,0)



grep("^58",colnames(df))
df %>% select(all_of(grep("^58",colnames(df)))) %>% names()
df %>% select(all_of(grep("^58",colnames(df)))) %>% View()
df$connaissance14 <- ifelse(rowSums(df[c(252,253,254,255,256,257,258, 259, 260, 261 ,262)]) %>% replace_na(0)  >=3, 1,0)


grep("^60",colnames(df))
df %>% select(all_of(grep("^60",colnames(df)))) %>% names()
df %>% select(all_of(grep("^60",colnames(df)))) %>% View()
df$connaissance15 <-ifelse(rowSums(df[c(266,267,268,269,271)])%>% replace_na(0) >=3,1,0)


grep("^63",colnames(df))
df %>% select(all_of(grep("^63",colnames(df)))) %>% names()
df %>% select(all_of(grep("^63",colnames(df)))) %>% View()
df$connaissance16 <-ifelse(rowSums(df[c(277, 278, 279, 280, 281, 282, 283, 284, 285)])%>% replace_na(0) >=4,1,0)


grep("^66",colnames(df))
df %>% select(all_of(grep("^66",colnames(df)))) %>% names()
df %>% select(all_of(grep("^66",colnames(df)))) %>% View()
df$connaissance17 <-ifelse(rowSums(df[c(290,291,292,293,294,295)])%>% replace_na(0) >=3,1,0)


grep("^72",colnames(df))
df %>% select(all_of(grep("^72",colnames(df)))) %>% names()
df %>% select(all_of(grep("^72",colnames(df)))) %>% View()
df$connaissance18 <-ifelse(rowSums(df[c(312, 313, 314, 315, 316, 317,319)])%>% replace_na(0) >=4,1,0)


grep("^75",colnames(df))
df %>% select(all_of(grep("^75",colnames(df)))) %>% names()
df %>% select(all_of(grep("^75",colnames(df)))) %>% View()
df$connaissance19 <-ifelse(rowSums(df[c(325, 326, 327, 328, 329)])%>% replace_na(0) >=3,1,0)


grep("^78",colnames(df))
df %>% select(all_of(grep("^78",colnames(df)))) %>% names()
df %>% select(all_of(grep("^78",colnames(df)))) %>% View()
df$connaissance20 <-ifelse(rowSums(df[c(337, 338, 339, 340, 341, 342, 343, 344)])%>% replace_na(0) >=3,1,0)



df <- df %>%
  rowwise() %>% 
  mutate(score_connaissance=
           sum(
             c(
               connaissance1, connaissance2 , connaissance3,connaissance5  ,
                 connaissance5 , connaissance6, connaissance7,
                 connaissance8 , connaissance9, connaissance10,connaissance11 ,
                 connaissance12,connaissance13,connaissance14 , connaissance15,
                 connaissance16, connaissance17,connaissance18, connaissance19,
                 connaissance20 )
                       )
         ) %>%
    ungroup()



## Remodélage des calculs : chemin long  --------------
## Attitude ------------
df %>% select(all_of(attitude)) %>% names()
att <- df %>% 
  select(all_of(attitude)) %>% 
  select(where(is.numeric)) %>%
  names()

grep("^3.",colnames(df))
df %>% select(all_of(grep("^3.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^3.",colnames(df)))) %>% View()
df$Attitude1 <-ifelse(df[c(24)] ==1,1,0)

grep("^6. ",colnames(df))
df %>% select(all_of(grep("^6. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^76.",colnames(df)))) %>% View()
df$Attitude2 <-ifelse(df[c(39)] ==1,1,0)

grep("^9. ",colnames(df))
df %>% select(all_of(grep("^9. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^9.",colnames(df)))) %>% View()
df$Attitude3 <-case_when(df[c(87)] ==1~1,
                         df[c(87)] ==0~0,
                         TRUE~NA )



grep("^12. ",colnames(df))
df %>% select(all_of(grep("^12. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^12. ",colnames(df)))) %>% View()
df$Attitude4 <-case_when(df[c(102)] ==1~1,
                         df[c(102)] ==0~0,
                         TRUE~NA )

grep("^17",colnames(df))
df %>% select(all_of(grep("^17",colnames(df)))) %>% names()
df %>% select(all_of(grep("^17",colnames(df)))) %>% View()
df$Attitude5 <-ifelse(df[c(107)] =="sait" & 
                        rowSums(df[c(109, 110, 111, 112, 113, 114, 115, 116)]) %>% replace_na(0)>=3 ,1,0)


grep("^18. ",colnames(df))
df %>% select(all_of(grep("^18. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^18.",colnames(df)))) %>% View()
df$Attitude6 <-ifelse(df[c(117)] == "Important",1,0)


grep("^20. ",colnames(df))
df %>% select(all_of(grep("^20. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^20",colnames(df)))) %>% View()
df$Attitude7 <-ifelse(df[c(126)] == 1,1,0)

grep("^24. ",colnames(df))
df %>% select(all_of(grep("^24. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^24",colnames(df)))) %>% View()
df$Attitude8 <-ifelse(df[c(138)] %in% c("important","Important"),1,0)

grep("^26. ",colnames(df))
df %>% select(all_of(grep("^26. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^26",colnames(df)))) %>% View()
df$Attitude9 <-ifelse(df[c(141)] %in% c( "D'accord","d_accord"),1,0)

grep("^34. ",colnames(df))
df %>% select(all_of(grep("^34",colnames(df)))) %>% names()
df %>% select(all_of(grep("^34",colnames(df)))) %>% View()
df$Attitude10 <-ifelse(df[c(184)]==1 ,1,0)


grep("^40. ",colnames(df))
df %>% select(all_of(grep("^40.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^40.",colnames(df)))) %>% View()
df$Attitude11 <-case_when(df[c(200)] %in% c("D'accord","d_accord")~0,
                         df[c(200)] %in% c("En dÃ©saccord" ,"en_d_saccord")~1,
                         TRUE~NA)

grep("^43. ",colnames(df))
df %>% select(all_of(grep("^43.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^43.",colnames(df)))) %>% View()
df$Attitude12 <-case_when(df[c(210)] ==1~1,
                         df[c(210)] ==0~0,
                         TRUE~NA)

grep("^49. ",colnames(df))
df %>% select(all_of(grep("^49.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^49.",colnames(df)))) %>% View()
df$Attitude13 <-ifelse(df[c(230)] %in% c("D'accord" ),1,0)

# grep("^49. ",colnames(df))
# df %>% select(all_of(grep("^49.",colnames(df)))) %>% names()
# df %>% select(all_of(grep("^49.",colnames(df)))) %>% View()
# df$Attitude13 <-ifelse(df[c(230)] %in% c("D'accord" ),1,0)


grep("^50.",colnames(df))
df %>% select(all_of(grep("^50.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^50",colnames(df)))) %>% View()
df$Attitude14 <-ifelse(df[c(231)] ==1 &
                        df[232] %in% c(
                           "A chat des semences" ,                                              
                           "Achat  de semence" ,                                                
                           "Achat d'engrais organique",                                         
                           "Achat de 2porcs" ,                                                  
                           "Achat de porc",                                                     
                           "Achat de semences",                                                 
                           "Achat des animaux Ã  elever \r\nActivitÃ©s champÃªtres" ,           
                           "Achat des betes",                                                   
                           "Achat des cobayes" ,                                                
                           "Achat des semences" ,                                               
                           "Achat des semences, et products veterinaires",                      
                           "Achat fumier"  ,                                                    
                           "Achat pesticide" ,                                                  
                           "Achat semence"  ,                                                   
                           "Achat semences" ,                                                   
                           "Achat semences et fumiers" ,                                        
                           "Achat semences,  labour",                                           
                           "Achatâ€™semences" ,                                                 
                           "Achete semences" ,                                                  
                           "ActivitÃ©s Agricole" ,                                              
                           "ActivitÃ©s champÃªtre" ,                                            
                           "ActivitÃ©s champÃªtre et scolarisations des enfants",               
                           "ActivitÃ©s champÃªtres"  ,                                          
                           "ActivitÃ©s diverses"  ,                                             
                           "Agri"  ,                                                            
                           "Agricole"  ,                                                        
                           "Agriculture"  ,                                                     
                           "Amelioration  des  fabrication  des3 engrains" ,                    
                           "Amelioration des  travaux  champettres" ,                           
                           "ChampÃªtre"   ,                                                     
                           "ChampÃªtre \r\nCommerciale\r\nEt autres" ,                          
                           "ChampÃªtre et petit commerce",                                      
                           "Commerce de manioc"  ,                                              
                           "Elevage" ,                                                          
                           "Emprunter dans le muso pour l,a hat du porc" ,                      
                           "Entretien,labour, semi et recolte."  ,                              
                           "Fabrication  des  a grains" ,                                       
                           "Fabrication  des insecticide"  ,                                    
                           "Fabrication  des produits phyto sanitaire"  ,                       
                           "Fabrication  et amelioration des produits  phytosanitaire" ,        
                           "La bour et sarclage"   ,                                            
                           "La bour, sÃ©mences,  sarclages"   ,                                 
                           "Labour" ,                                                           
                           "Labour  ,Sarclage , achat aliments des porcs"  ,                    
                           "Labour ,semis , entretien et recolte" ,                             
                           "Labour et Achat des semences" ,                                     
                           "Labour et achat fumier"  ,                                          
                           "Labour et Achat semences" ,                                         
                           "Labour et sarclage" ,                                               
                           "Labour et semis" ,                                                  
                           "Labour,  semis et recolte" ,                                        
                           "Labour, semis  et recolte" ,                                        
                           "Labour, semis et recolte"  ,                                        
                           "Labour, semis, sarclage et recolte" ,                               
                           "Labour, semis, Sarclage et recolte" ,                               
                           "Labour,semis  et recolte" ,                                         
                           "Labours"  ,                                                         
                           "Les activitÃ©s chÃ¢mpetres"   ,                                     
                           "Louage terrain et commerce"  ,                                      
                           "Mains d'oeuvre  et a hats des des  products  phytosanitaire" ,      
                           "Payer les gens  qui m,ont aide pour le labour" ,                    
                           "Petit commerce et achat des semences" ,                             
                           "Petites ActivitÃ©s commerciales et champÃªtres",                    
                           "Plantation dâ€™arbres"   ,                                          
                           "Pomme de terre"  ,                                                  
                           "Renforcer l,elevage"  ,                                             
                           "Sarclage"  ,                                                        
                           "Toutes les  activitÃ©s champetre"  ,                                
                           "Toutes les activitÃ©s champÃªtres" ,                                
                           "Toutes les activitÃ©s champÃªtres en generales" ,                   
                           "Travaux  champÃªtres,  Faire  scolariser  les  enfants"),
                      1,0)


grep("^51. ",colnames(df))
df %>% select(all_of(grep("^51.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^51. ",colnames(df)))) %>% View()
df$Attitude15 <-case_when(df[c(233)] %in% c("D'accord","d_accord" )~0,
                         df[c(233)] %in% c("En dÃ©saccord" ,"en_d_saccord")~1,
                         TRUE~NA)


grep("^54. ",colnames(df))
df %>% select(all_of(grep("^54.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^54. ",colnames(df)))) %>% View()
df$Attitude16 <-case_when(df[c(240)] %in% c("important","Important" )~1,
                         df[c(240)] %in% c("Pas important")~0,
                         TRUE~NA)


grep("^57. ",colnames(df))
df %>% select(all_of(grep("^57.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^57. ",colnames(df)))) %>% View()
df$Attitude17 <-ifelse(df[c(250)] ==1,1,0)
                       

grep("^59. ",colnames(df))
df %>% select(all_of(grep("^59.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^59. ",colnames(df)))) %>% View()
df$Attitude18 <-case_when(df[c(264)] %in% c( "1")~1,
                         df[c(264)] %in% c("non","NON")~0,
                         TRUE~NA)


grep("^61. ",colnames(df))
df %>% select(all_of(grep("^61",colnames(df)))) %>% names()
df %>% select(all_of(grep("^61 ",colnames(df)))) %>% View()
df$Attitude19 <-case_when(df[c(273)] %in% c( "1")~1,
                         df[c(273)] %in% c("non","NON")~0,
                         TRUE~NA)


grep("^64. ",colnames(df))
df %>% select(all_of(grep("^64",colnames(df)))) %>% names()
df %>% select(all_of(grep("^64 ",colnames(df)))) %>% View()
df$Attitude20 <-ifelse(df[c(287)]== "D'accord" | df[c(287)]==  "d_accord",1,0)
                        

grep("^67. ",colnames(df))
df %>% select(all_of(grep("^67",colnames(df)))) %>% names()
df %>% select(all_of(grep("^67 ",colnames(df)))) %>% View()
df$Attitude21 <-ifelse(df[c(297)]== "D'accord" | df[c(297)]==  "d_accord",1,0)


grep("^70. ",colnames(df))
df %>% select(all_of(grep("^70",colnames(df)))) %>% names()
df %>% select(all_of(grep("^70.",colnames(df)))) %>% View()
df$Attitude21 <-ifelse(df[c(308)]== "1",1,0)



grep("^73. ",colnames(df))
df %>% select(all_of(grep("^73",colnames(df)))) %>% names()
df %>% select(all_of(grep("^73.",colnames(df)))) %>% View()
df$Attitude23 <-ifelse(df[c(321)]== 1,1,0)


grep("^Attitude",colnames(df))
df %>% select(all_of(grep("^Attitude",colnames(df)))) %>% names()
df %>% select(all_of(grep("^Attitude",colnames(df)))) %>% View()
df <- df %>%
  rowwise() %>%
  mutate(
    score_attitude=sum(c(
       Attitude1,  Attitude2,  Attitude3,  Attitude4 , Attitude5,  Attitude6 ,
       Attitude7 , Attitude8 , Attitude9 , Attitude10 ,Attitude11, Attitude12,
       Attitude13, Attitude14, Attitude15, Attitude16 ,Attitude17 , Attitude18,
       Attitude19, Attitude20, Attitude21, Attitude23
    ), na.rm = T)
  )


## Pratique ------------
df %>% select(all_of(con)) %>% select(where(is.numeric))
prat <- df %>% 
  select(all_of(pratique)) %>% 
  select(where(is.numeric)) %>%
  names()

grep("^1. ",colnames(df))
df %>% select(all_of(grep("^1.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^1. ",colnames(df)))) %>% View()
df$prat1<-ifelse(df[c(15)] ==1 &
                 df[16] %nin% c("1","2","3","3\r\n" ),1,0)

 
grep("^4.1",colnames(df))
df %>% select(all_of(grep("^4.1",colnames(df)))) %>% names()
df %>% select(all_of(grep("^4.1",colnames(df)))) %>% View()
df$prat2<-ifelse( rowSums(df[c(27, 28, 29, 30, 31, 32, 33, 34, 35)]) %>% replace_na(0)>=3,
                  1,0)


grep("^Arbres agroforestiers :/",colnames(df))
df %>% select(all_of(grep("^Arbres agroforestiers :/",colnames(df)))) %>% names()
df %>% select(all_of(grep("^Arbres agroforestiers :/",colnames(df)))) %>% View()
df$prat3.1<-ifelse( rowSums(df[c(42, 43,44, 45, 46, 47, 48, 49, 50)]) %>% replace_na(0)> 1,
                  1,0)



grep("^Arbres fruitiers :/",colnames(df))
df %>% select(all_of(grep("^Arbres fruitiers :/",colnames(df)))) %>% names()
df %>% select(all_of(grep("^Arbres fruitiers :/",colnames(df)))) %>% View()
df$prat3.2<-ifelse( rowSums(df[c(52, 53, 54, 55, 56, 57, 58)]) %>% replace_na(0)> 1,
                    1,0)


grep("^7.2.",colnames(df))
df %>% select(all_of(grep("^7.2. Quels sont ces services ? /",colnames(df)))) %>% names()
df %>% select(all_of(grep("^7.2. Quels sont ces services ? /",colnames(df)))) %>% View()
df$prat4<-ifelse( rowSums(df[c( 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86)]) %>% replace_na(0)>=5,
                    1,0)


grep("^8.",colnames(df))
df %>% select(all_of(grep("^8. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^8. ",colnames(df)))) %>% View()
df$prat5<-ifelse( rowSums(df[c(63, 64, 65, 66)]) %>% replace_na(0)> 1,
                  1,0)


grep("^10.",colnames(df))
df %>% select(all_of(grep("^10. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^10.",colnames(df)))) %>% View()
df$prat6<-ifelse( df[88]==1,1,0)


grep("^15.",colnames(df))
df %>% select(all_of(grep("^15. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^15.",colnames(df)))) %>% View()
df$prat7<-ifelse( df[15]==1,1,0)


grep("^21.",colnames(df))
df %>% select(all_of(grep("^21. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^21.",colnames(df)))) %>% View()
df$prat8 <-ifelse( df[21]==1,1,0)

grep("^22.",colnames(df))
df %>% select(all_of(grep("^22. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^22.",colnames(df)))) %>% View()
df$prat9 <-ifelse( rowSums(df[c(129, 130,131)])>=2,1,0)

grep("^25.",colnames(df))
df %>% select(all_of(grep("^25. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^25.",colnames(df)))) %>% View()
df$prat10 <- ifelse(df[139]==1,1,0)


grep("^29.",colnames(df))
df %>% select(all_of(grep("^29. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^29.",colnames(df)))) %>% View()
df$prat11 <- ifelse(df[163]==1,1,0)


grep("^31.",colnames(df))
df %>% select(all_of(grep("^31. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^31.",colnames(df)))) %>% View()
df$prat12 <- ifelse(df[173] == "D'accord" | df[173]== "d_accord",1,0)

grep("^32.",colnames(df))
df %>% select(all_of(grep("^32. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^32.",colnames(df)))) %>% View()
df$prat13 <- ifelse(df[174] ==1,1,0)


grep("^35.",colnames(df))
df %>% select(all_of(grep("^35 ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^35.",colnames(df)))) %>% View()
df$prat14 <- ifelse(rowSums(df[c(186, 187, 188, 189)]) >=2,1,0)


grep("^38.",colnames(df))
df %>% select(all_of(grep("^38 ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^38.",colnames(df)))) %>% View()
df$prat15 <- ifelse(df[192] ==1,1,0)

grep("^41. ",colnames(df))
df %>% select(all_of(grep("^41. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^41.",colnames(df)))) %>% View()
df$prat16 <- ifelse(df[201] ==1,1,0)

grep("^45. ",colnames(df))
df %>% select(all_of(grep("^45. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^45.",colnames(df)))) %>% View()
df$prat17 <- ifelse(df[212] ==1,1,0)


grep("^50. ",colnames(df))
df %>% select(all_of(grep("^50. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^50.",colnames(df)))) %>% View()
df$prat17 <- ifelse(df[231] ==1,1,0)

grep("^53. ",colnames(df))
df %>% select(all_of(grep("^53. ",colnames(df)))) %>% names()
df %>% select(all_of(grep("^53. ",colnames(df)))) %>% View()
df$prat6 <- ifelse(df[239] ==1,1,0)

grep("^55.",colnames(df))
df %>% select(all_of(grep("^55.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^55.",colnames(df)))) %>% View()
df$prat18 <- ifelse(df[241] ==1,1,0)

grep("^62.",colnames(df))
df %>% select(all_of(grep("^62.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^62.",colnames(df)))) %>% View()
df$prat19 <- ifelse(df[274] ==1,1,0)


grep("^65.",colnames(df))
df %>% select(all_of(grep("^65.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^65.",colnames(df)))) %>% View()
df$prat20 <- ifelse(df[288] ==1,1,0)


grep("^68.",colnames(df))
df %>% select(all_of(grep("^68.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^68.",colnames(df)))) %>% View()
df$prat21 <- ifelse(rowSums(df[c(300, 301, 302, 303, 304)]) >=3,1,0)


grep("^71.",colnames(df))
df %>% select(all_of(grep("^71.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^71.",colnames(df)))) %>% View()
df$prat22 <- ifelse(df[309]==1,1,0)


grep("^74.",colnames(df))
df %>% select(all_of(grep("^74.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^74.",colnames(df)))) %>% View()
df$prat23 <- ifelse(df[322]==1,1,0)


grep("^76.",colnames(df))
df %>% select(all_of(grep("^76.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^76.",colnames(df)))) %>% View()
df$prat24 <- ifelse(df[332]==1,1,0)


grep("^77.",colnames(df))
df %>% select(all_of(grep("^77.",colnames(df)))) %>% names()
df %>% select(all_of(grep("^77.",colnames(df)))) %>% View()
df$prat25 <- ifelse(df[334]==1,1,0)



grep("^prat",colnames(df))
df %>% select(all_of(grep("^prat",colnames(df)))) %>% names()
df %>% select(all_of(grep("^prat",colnames(df)))) %>% View()


df <- df %>%
  rowwise() %>%
  mutate(
    score_pratique =sum(c(
       prat1,   prat2,   prat3.1, prat3.2, prat4,   prat5,   prat6,   prat7 , 
       prat8 ,  prat9 ,  prat10 , prat11,  prat12,  prat13,  prat14,  prat15 ,
       prat16,  prat17,  prat18,  prat19,  prat20,  prat21,  prat22,  prat23, 
       prat24,  prat25 
    ), na.rm = T)
  )




#  Connaissance  : sur 20
# Attitude : sur 23
# Pratique : sur 26 points
# CAP :  69 points
df$`Territoire :` %>% as.factor() %>% levels()
df <-df %>% 
  mutate(`Territoire :`= `Territoire :` %>% 
           fct_recode("kabare"="Kabare",
                      "kalehe"="Kalehe",
                      "walungu"="Walungu"
                      ))


df$`Niveau d'instruction :` %>% as_factor() %>% levels()


df <-df %>% 
  mutate(`Niveau d'instruction :`= `Niveau d'instruction :` %>% 
           fct_recode("ecole_primaire"="Ecole primaire",
                      "ecole_secondaire"="Ecole secondaire",
                      "aucun"="Aucun" 
           ))

df <- df %>% 
  mutate(SC= score_connaissance*100/20,
         SA= score_attitude*100/23,
         SP= score_pratique*100/26,
         
         CAP = sum(c(score_connaissance,score_attitude,score_pratique))*100/69
         )

## Synthèse des scores
df %>% 
  select(SC,SA,SP,CAP,`Territoire :`, `sexe :`, `Niveau d'instruction :`) %>% 
  tbl_summary(by=`Territoire :`,
              statistic = list(
                all_categorical()~"{p}%",
                all_continuous()~"{mean}"
              ),
              digits = everything()~1) %>% 
  add_overall() %>% 
  bold_labels() %>% 
  italicize_levels()


df <- df %>% 
  mutate(site="Sud-Kivu")


library(fmsb)
radar<-
  df %>% 
  dplyr::select(SC, SA, SP, CAP, site) %>%
  group_by(site) %>% 
  summarise(Caonnaissance=mean(SC,na.rm=T),
            Attitude=mean(SA,na.rm=T),
            Pratique=mean(SP,na.rm=T),
            CAP=mean(CAP,na.rm=T)
            )

radar <- rbind(rep(1,4), rep(0,4), radar %>% select(-site))
radar <- rbind(rep(100,4), rep(0,4), radar %>% select(-site))

radar
library(fmsb)
par(las = 0)
# radarchart(radar,axistype=1, 
#            #custom polygon
#            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
#            #custom the grid
#            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,.25), cglwd=0.1,
#            #custom labels
#            vlcex=0.6 ) 


radarchart(radar,axistype=1, 
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,20), cglwd=0.1,
           #custom labels
           vlcex=0.6 ) 

