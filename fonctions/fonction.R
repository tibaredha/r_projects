con <- function(tbl){
  library("RMySQL")
  mysql <-  dbConnect(MySQL(),user="tibaredha",password="030570",dbname="framework",host="localhost")
  #options(max.print=1000000)
  query <- paste0("SELECT * FROM ", tbl)
  resultat <-  dbSendQuery(mysql,query)
  #df <- fetch(resultat,n=Inf)
  df <- fetch(resultat,n=-1)
  dbDisconnect(mysql)
  return(df)
}


# 0-setwd()& getwd() ----

# 1-fixe varaibles ----

dt1 <- "2020-01-01"
dt2 <- "2022-12-31"
wilaya <- "Djelfa"
subtitle <-  paste("Wilaya de ",wilaya," du ",format(as.Date(dt1),"%d-%m-%Y")," au ",format(as.Date(dt2),"%d-%m-%Y"))
caption  <-  paste("Source: Dr R.TIBA \n DSP Wilaya de ",wilaya," \n Tel : 0772718059")
dspdjelfa <- paste("DSP de ",wilaya)

# 2-load tables from mysql ----

chp <- con("chapitre")
chp <- chp %>% 
  select(IDCHAP,CHAP) %>% # view
  rename(CODECIM0=IDCHAP) # %>% view 

cim <- con("cim")
cim <- cim %>% 
  select(row_id,diag_cod,diag_nom) %>% # view
  rename(CODECIM=row_id) # %>% view 

prf <- con("profession")
prf <- prf %>% 
  select(id,Profession) %>%                     # view
  rename(n_profession=Profession,Profession=id) # %>% view 

str <- con("structure")
str <- str %>% 
  select(id,structure) %>% # view
  rename(STRUCTURED=id)    # %>%  view

srv <- con("servicedeces")
srv <- srv %>% 
  select(id,service) %>%   # view
  rename(SERVICEHOSPIT=id) # %>%  view

data <- con("deceshosp")
data <- data %>%
  select(DINS,DATENAISSANCE,WILAYAR,COMMUNER,LD,STRUCTURED,SERVICEHOSPIT,DUREEHOSPIT,SEX,Years,Days,Profession,CD,CODECIM0,CODECIM) %>% 
  left_join(str,by="STRUCTURED") %>% 
  left_join(srv,by="SERVICEHOSPIT") %>% 
  left_join(prf,by="Profession") %>%
  left_join(chp,by="CODECIM0") %>%
  left_join(cim,by="CODECIM") %>%
  mutate(DINS = as.Date(DINS),
         DATENAISSANCE=as.Date(DATENAISSANCE),
         LD   = as.factor(LD),
         structure  = as.factor(structure),
         service  = as.factor(service),
         n_profession = as.factor(n_profession),
         SEX  = as.factor(SEX),
         Profession = as.factor(Profession),
         CHAP = as.factor(CHAP),
         diag_cod = as.factor(diag_cod),
         CD = as.factor(CD)) %>% 
  set_variable_labels(DINS="Date du décès",
                      DATENAISSANCE="date de naissance",
                      WILAYAR="Wilaya de résidence",
                      COMMUNER="Commune de résidence",
                      LD="Lieux du décès",
                      structure="Structure sanitaire",
                      service="Service d'hospitalisation",
                      DUREEHOSPIT="Durée d'hospitalisation",
                      SEX="Sexe",
                      Years="Age (Année)",
                      Days="Age (Jours)",
                      n_profession="Profession",
                      CD="Cause du décès",
                      CHAP="Capitre CIM10",
                      diag_cod="Titre CIM10"
  ) %>%
  select(DINS,DATENAISSANCE,WILAYAR,COMMUNER,LD,structure,service,DUREEHOSPIT,SEX,Years,Days,n_profession,CD,CHAP,diag_cod) %>% 
  filter(DINS >= dt1 & DINS <= dt2,Years >=0 & Years <=100)
# save data
# write_csv(data, "data/data.csv")