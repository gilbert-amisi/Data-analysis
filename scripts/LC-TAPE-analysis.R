
setwd(dir = "D:/Personnal Files/Gilbert Forciers")

# chargement des packages --------
library(tidyverse)
library(dlookr)
library(GGally)
library(questionr)
library(janitor)
library(gt)
library(gtsummary)
library(readxl)

# Chargement des données  ----------



df <- read_xlsx("TAPE_to DRC_Louvain - Final locally validation.xlsx", sheet = "TAPE Data (2)")
df <- df %>% janitor::remove_empty(which = "cols")
df$location2 %>% freq(total = T)



df <- df %>% drop_na(location2)



df$Sex %>% freq(total = T)
xtabs(~Sex+location2, data = df)


df %>% 
  select(Sex, location2) %>% 
  tbl_summary(by=location2,
              statistic = list(all_categorical()~"{n}"),
              digits = everything()~1) %>% 
  add_overall(last=T)

library(Hmisc)
library(rpart)

# df$sexx <-  dlookr::imputate_na(.data=df, xvar=Sexe, yvar=netrev,method="mode")
# df$sexx <-  dlookr::imputate_na(.data=df, xvar=df$Sexe, yvar=df$location2,method="mode")




# df <- df %>% 
#   mutate(Sexx=imputate_na(df, Sexe, location2, "mode"))

# df <- df %>% 
#   mutate(Sexx=imputate_na(.data=df,xvar=Sexe, yvar=location2, "mode"))

# Imputation des valeurs manquantes pour le sexe 
df<- df %>% mutate(Sexe=as.factor(Sex) %>% 
                     fct_na_value_to_level(level =  "Missing"))
# df <-
#   df %>% 
#   mutate(sexx= case_when(Sexe=="Missing" & location2=="Kabare"~ "F",
#                          Sexe=="Missing" & location2=="Kalehe"~"M",
#                          Sexe=="Missing" & location2=="Walungu"~"F",
#                          TRUE~Sexe
#                          ) )



df %>% 
  select(Sexe, location2) %>% 
  tbl_summary(by=location2,
              statistic = list(all_categorical()~"{n}"),
              digits = everything()~1) %>% 
  add_overall(last=T)



df <- df %>% 
  rowwise() %>% 
  mutate(
    hdds=sum(c(grains, pulses, nuts, dairy, meat, eggs, darkgreen, darkyellow,
               otherveg, otherfruit), na.rm = T), 
    # sante_sol=sum(c(structure, compaction, depth, residues, color, water_ret, 
    #                 cover, erosion, invertebrates,microbio),na.rm =T ),
    sante_sol=mean(c(structure, compaction, depth, residues, color, water_ret, 
                    cover, erosion, invertebrates,microbio),na.rm =T ),
    
    # verifier si le ménage comprends les jeunes dans le ménage  pour cet index
    index_opportunite_emploi=mean(youth_employ,youth_emigr, na.rm = T), 
    # index_de_biodiversite=mean(GSI_overall,)
    index_de_biodiversite=mean(c(GSIndex_crops, GSIndex_animals,GSI_other), na.rm = T)
    )


df %>% select(index_de_biodiversite,GSIndex_crops, GSIndex_animals,GSI_other) %>% View()







df <- df %>% 
  rowwise() %>% 
  mutate(
    # Revenu=sum(c(crop_prodval,anpr_prodval, crop_revenue, 
    #   anim_revenue, cfp_revenue, farm_revenue), na.rm = T),
    Revenu=sum(c(crop_revenue,anim_revenue,anpr_revenue ,cfp_revenue), na.rm = T),
    # Revenu1=sum(
    #   c(crop_prodval,anpr_prodval), na.rm = T),
    # Revenu2=sum(
    #   c(crop_revenue, 
    #     anim_revenue, cfp_revenue), na.rm = T) 
    )



# Classification des indicateurs en catégories  --------

df <- df %>% 
  mutate(across(.cols=c(grains, pulses, nuts, dairy, meat, eggs, darkgreen, darkyellow,otherveg, otherfruit
),.fns=~replace_na(.,0)))


# df <- df %>% 
#   mutate(Revenu1=imputate_outlier(.data=df, xvar=Revenu, method="capping",
#                                   cap_ntiles = c(0.05,0.95),
#                                   no_attrs=TRUE))

df$Revenu1 <- imputate_outlier(.data = df, xvar = Revenu, method = "capping",
                               cap_ntiles = c(0.1,0.90),
                               no_attrs = TRUE)




df <- df %>% 
  mutate(hdds_cut = cut(hdds,
                        breaks = c(0, 5,7,10),
                        right = TRUE,
                        include.lowest = TRUE,
                        labels = c("Rouge", "Jaune", "Vert")),
         sante_sol_cut=cut(sante_sol,
                           breaks = c(0,2.5,3.5,5),
                           include.lowest = TRUE,
                           right = FALSE,
                           labels = c("Insouhaitable","Acceptable", "Souhaitable")),
         index_opportunite_emploi_cut=cut(index_opportunite_emploi,
                                          breaks = c(0,50,70,100),
                                          include.lowest = T,
                                          right = FALSE,
                                          labels = c("Insouhaitable","Acceptable", "Souhaitable")),
         index_de_biodiversite_cut=cut(index_de_biodiversite,
                                       breaks = c(0,50,70,100),
                                       include.lowest = T,
                                       right = FALSE,
                                      
                                       labels = c("Insouhaitable","Acceptable", "Souhaitable")),
         Revenu_rec=cut(Revenu,
                        breaks = c(0,600000,900000, 1200000,190000000),
                        include.lowest = TRUE,
                        right = FALSE,
                        dig.lab = 10),
         Revenu1_rec=cut(Revenu1,
                         breaks = c(0,800000,1600000,2400000,3200000,3675500),
                         include.lowest = T, dig.lab = 9, 
                         right = FALSE)
         
         )



theme_gtsummary_language(language = "fr",decimal.mark = ",",
                         big.mark = " ")

# Table de résultats  -----------
tab1 <-
  df %>% 
  select(Sexe, location2,
         hdds, hdds_cut,
         grains, pulses, nuts, dairy, meat, eggs, darkgreen, darkyellow,otherveg, otherfruit,
         sante_sol, sante_sol_cut,
         structure, compaction, depth, residues, color, water_ret, 
         cover, erosion, invertebrates,microbio,
         index_opportunite_emploi, index_opportunite_emploi_cut,
         index_de_biodiversite, index_de_biodiversite_cut,
         Revenu,Revenu_rec,
         Revenu1, Revenu1_rec) %>% 
  tbl_summary(by=location2,
              statistic = list(all_categorical()~"{p}%",
                               all_continuous()~"{mean}"),
              digits = everything()~1) %>% 
  add_overall(last=T) %>% 
  bold_labels() %>% 
  italicize_levels()


tab2 <-
  df %>% 
  select(Sexe, location2,
         hdds, hdds_cut,
         grains, pulses, nuts, dairy, meat, eggs, darkgreen, darkyellow, otherveg, otherfruit,
         sante_sol, sante_sol_cut,
         structure, compaction, depth, residues, color, water_ret, 
         cover, erosion, invertebrates,microbio,
         index_opportunite_emploi, index_opportunite_emploi_cut,
         index_de_biodiversite, index_de_biodiversite_cut,
         Revenu,Revenu_rec,
         Revenu1, Revenu1_rec) %>% 
  tbl_summary(by=Sexe,
              statistic = list(all_categorical()~"{p}%",
                               all_continuous()~"{mean}"),
              digits = everything()~1) %>% 
  add_overall(last=T) %>% 
  bold_labels() %>% 
  italicize_levels()
  



tbl_merge(tbls = list(tab1, tab2),
          tab_spanner = c("Territoire","Sexe"))





# df %>%  select(structure, compaction, depth, residues, color, water_ret, 
#                cover, erosion, invertebrates,microbio) %>% View()





# Strates
df <- df %>% 
  mutate(Sexe=Sexe %>% droplevels(),
         Sexe=Sexe %>% 
           fct_recode("Masculin"="M", "Féminin"="F")
           )

# Table de résultats
State_sexe <-
  df %>% 
  select(Sexe, location2,
         hdds, hdds_cut,
         grains, pulses, nuts, dairy, meat, eggs, darkgreen, darkyellow, otherveg, otherfruit,
         sante_sol, sante_sol_cut,
         structure, compaction, depth, residues, color, water_ret, 
         cover, erosion, invertebrates,microbio,
         index_opportunite_emploi, index_opportunite_emploi_cut,
         index_de_biodiversite, index_de_biodiversite_cut,
         Revenu,Revenu_rec,
         Revenu1, Revenu1_rec) %>% 
  tbl_strata(strata = location2,
             .tbl_fun = ~ .x %>% 
               tbl_summary(by=Sexe,
                           type=list(hdds~"continuous",
                                     index_opportunite_emploi~"continuous"),
                           statistic = list(all_categorical()~"{p}%",
                                            all_continuous()~"{mean}",
                                            all_continuous2()~"{mean}"),
              digits = everything()~1) %>% 
               add_overall(last=F) %>%
               bold_labels() %>% 
               italicize_levels()
             )
State_sexe
library(fmsb)
# Resultats CAETv --------------
radar<-
df %>% 
  dplyr::select(div_score, syn_score, eff_score, rec_score,res_score, 
         res_score2, res_score3, cultf_score, cocr_score,
         human_score,circ_score, respg_score,country ) %>%
  group_by(country) %>% 
  summarise(Diversity=mean(div_score,na.rm=T),
            Synergies=mean(syn_score,na.rm=T),
            Effisciency=mean(eff_score,na.rm=T),
            Recycling=mean(rec_score,na.rm=T),
            Resilience=mean(res_score3,na.rm=T),
            "Culture and food tradition"=mean(cultf_score,na.rm=T),
            "Circular and solidarity economy"=mean(cocr_score,na.rm=T),
            "Human and social values"=mean(human_score,na.rm=T),
            "Co-creation and sharing of knowledge"=mean(circ_score,na.rm=T),
            "Response governance"=mean(respg_score,na.rm=T),
            )

radar <- rbind(rep(100,10), rep(0,10), radar %>% select(-country))
radar
library(fmsb)
par(las = 0)
radarchart(radar,axistype=1, 
          #custom polygon
          pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
          #custom the grid
          cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.1,
          #custom labels
          vlcex=0.6 ) 

devtools::install_github("ricardo-bion/ggradar")





df %>% 
  select(div_score, syn_score, eff_score, rec_score,res_score, 
         res_score2, res_score3, cultf_score, cocr_score,
         human_score,circ_score, respg_score,country, location2) %>% 
  pivot_longer(cols = c(div_score, syn_score, eff_score, rec_score,res_score, 
                        res_score2, res_score3, cultf_score, cocr_score,
                        human_score,circ_score, respg_score),
               names_to = "Indicateur",values_to = "Valeur" ) %>% 
  group_by(Indicateur, location2) %>% 
  summarise(moyenne=mean(Valeur),
            medianne=median(Valeur),
            maximum=max(Valeur),
            minimum=min(Valeur)
  ) 
library(openxlsx)

df %>% 
  select(div_score, syn_score, eff_score, rec_score,res_score, 
         res_score2, res_score3, cultf_score, cocr_score,
         human_score,circ_score, respg_score,country, location2) %>% 
  pivot_longer(cols = c(div_score, syn_score, eff_score, rec_score,res_score, 
                        res_score2, res_score3, cultf_score, cocr_score,
                        human_score,circ_score, respg_score),
               names_to = "Indicateur",values_to = "Valeur" ) %>% 
  group_by(Indicateur) %>% 
  summarise(moyenne=mean(Valeur),
            medianne=median(Valeur),
            maximum=max(Valeur),
            minimum=min(Valeur)
            ) 

df %>% 
  select(div_score, syn_score, eff_score, rec_score,res_score, 
         res_score2, res_score3, cultf_score, cocr_score,
         human_score,circ_score, respg_score,country, location2) %>% 
  pivot_longer(cols = c(div_score, syn_score, eff_score, rec_score,res_score, 
                        res_score2, res_score3, cultf_score, cocr_score,
                        human_score,circ_score, respg_score),
               names_to = "Indicateur",values_to = "Valeur" ) %>% 
  group_by(Indicateur, location2) %>% 
  summarise(moyenne=mean(Valeur,na.rm=T),
            medianne=median(Valeur, na.rm=T),
            maximum=max(Valeur, na.rm = T),
            minimum=min(Valeur, na.rm = T)
  ) %>% 
  select(location2,Indicateur, moyenne) %>% 
  pivot_wider(names_from = location2,
              values_from = c(moyenne))
  
  
  



  
  
  