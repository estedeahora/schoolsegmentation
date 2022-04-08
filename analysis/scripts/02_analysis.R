# Package -----------------------------------------------------------------

library(flextable)
library(officer)

library(tidyverse)
library(ggrepel)
library(patchwork)

library(FactoMineR)
library(factoextra)
library(missMDA)
library(fclust)
library(segregation)

# Load data ---------------------------------------------

if(!exists("AUX")) AUX <- list()

AUX$GRUPO <- read.csv(here::here("analysis/data/dimensions.csv")) |>
  mutate(CLASE = factor(CLASE, levels = c("s", "n")),
         DIM_LAB = factor(DIM_LAB, levels = unique(DIM_LAB) ) )

SCHOOLS <- read.csv(here::here("analysis/data/schools.csv")) |>
  mutate(across(.cols = c(LQ_PRI, LQ_SUP),
                .fns = ~factor(.x, levels = c( "Alto", "Medio", "Bajo"))),
         COOP = factor(COOP, levels = c("c/PJ","s/PJ",  "NO") ),

         SUBS = factor(SUBS, levels = c("Estatal", "NO", "SI") ),
         TIPO_HABITAT = factor(TIPO_HABITAT,
                               levels = c("Ciudad Central",
                                          "Centro Administrativo y de Negocios",
                                          "Residencial alto",
                                          "Residencial medio",
                                          "Residencial bajo",
                                          "Conjunto Habitacional",
                                          "Popular de Origen Informal")),
         SECTOR  = factor(SECTOR, levels = c("Estatal", "Privado"))
         )

SEC_aux <- SCHOOLS |> select(-starts_with("N_"))
SEC_seg <- SCHOOLS |>select(ID_s, starts_with("N_"))

AUX$SCHOOLS <- SCHOOLS
rm(SCHOOLS)

# Grupos ------------------------------------------------------------------

  GRUPO <- AUX$GRUPO |>
    arrange(ACTIVA, DIM_LAB, CLASE) |>
    count(ACTIVA, DIM_LAB, CLASE) |>
    mutate(GRUPO = case_when(ACTIVA == 1 ~ as.character(DIM_LAB),
                             CLASE == "s" ~ paste0(DIM_LAB, " (sup.)"),
                             CLASE == "n" ~ paste0(DIM_LAB, " (sup. cualit.)") ),
           id_g = 1:n())

# Impute missing values with PCA ------------------------

  # Active variables names
  varact <- AUX$GRUPO |>
    filter(Tipo == "x") |>
    select(VAR) |>
    simplify() |>
    unname()

  # Impute marker
  imputado <- is.na(SEC_aux[varact]) %>% apply(., 1, sum)

  # Imputation
  SEC_aux[varact] <- SEC_aux[varact] |>
    imputePCA(scale = T, ncp = 4) |>
    pluck("completeObs")

  AUX$IMPUTA <- data.frame(ID_s = SEC_aux$ID_s, IMPUTADO = imputado)

  rm(varact, imputado)

# MFA para segmentación --------------------------------------

  SEC_MFA <- SEC_aux |>
    select(-ID_s) |>
    MFA(group = GRUPO$n, ncp = 10,
        type = as.character(GRUPO$CLASE),
        name.group = GRUPO$GRUPO,
        num.group.sup = GRUPO$id_g[GRUPO$ACTIVA != 1],
        graph = F)

  rm(GRUPO)

# Armado de fuzzy cluster -----------------------------------------

SEC_CL <- Fclust(SEC_MFA$ind$coord[ , 1:4],
                 type = "gk",
                 k = 4, noise = T)

CL_ORDEN <- order(SEC_CL$H[, 1])
SEC_aux <- cbind(SEC_aux, cl = SEC_CL$clus[ , "Cluster"],
                 cl_p = SEC_CL$clus[ , "Membership degree"],
                 SEC_CL$U[ , CL_ORDEN]) %>%
  mutate(cl = factor(cl, levels = CL_ORDEN,
                     labels = 1:4) )

colnames(SEC_aux)[str_starts(colnames(SEC_aux), "Clus.")] <- paste0("Clus.", 1:4)

# Indices de calidad de los agrupamientos ---------------------------------
# el indice PC y MPC son mediciones de "difusidad" (fuzzy)
I <- Fclust.index(SEC_CL, alpha = 1)

# Descripción de clusters:
D <- SEC_aux %>%
  group_by(cl) %>%
  summarise(Cl.Size = n(),
            Cl.Student = sum(n),
            No.Asig = sum(cl_p <= 0.5),
            P.Asig = No.Asig/Cl.Size * 100,
            deg.Min = min(cl_p),
            deg.Max = max(cl_p),
            deg.Av = mean(cl_p))

# Distancia entre cluster:
H <- SEC_CL$H[CL_ORDEN, ]
rownames(H) <- paste0 ("Clus ", 1:4)
HD <- round(dist(H), 2)

# Comparacion entre Sector y cluster
CSec <- Fclust.compare(VC = SEC_aux$SECTOR,
                       SEC_aux |>
                         select(starts_with("Clus.")) ) %>%
  round(digits = 3)

# Tabla (Sub)Sector - cluster
t1 <- cbind(round(prop.table(xtabs(n ~ cl + SUBS, data = SEC_aux), 1) * 100, 1),
            Total = xtabs(n ~ cl, data = SEC_aux) )
t1 <- t1 %>%
  as_tibble(rownames = "Circuito") %>%
  mutate(Circuito = paste("Clúster", Circuito),
         Privado = NO + SI) %>%
  select(Circuito, Estatal, Privado, SI, NO, Total)

CLUSTER <- list(H = H, Index = I, Descrip = D,
                Distan = HD, Ind_Sec = CSec, Tab_Sec = t1)

rm(CL_ORDEN, CSec, I, D, H, HD, SEC_CL, t1)

# Descripción de los cluster ----------------------------------------------

dat <- SEC_aux %>%
  select(-ID_s, -SECTOR, -COOP, -LQ_PRI,
         -cl_p, -starts_with("Clus.")) |>
  select(cl, everything()) |>
  mutate(SUBS = factor(SUBS,
                       levels = c("Estatal", "SI", "NO"),
                       labels = c("Estatal", "Priv. Con Subsidio",
                                  "Priv. Independiente")))

# Generar descripción de variables
CL_DESC <- catdes(dat, 1)

# Agregar Nombres Human-readable a variables
for(i in c("test.chi2", "quanti.var")){
  CL_DESC[[i]] <- CL_DESC[[i]] %>%
    as_tibble(rownames = "VAR") %>%
    left_join(AUX$GRUPO |> select(VAR, IND_LAB),
              by = "VAR")
}

# Agregar Nombres Human-readable a descripción de
#   clúster por variables cuantitativas
for(i in 1:length(CL_DESC$quanti)){
  CL_DESC$quanti[[i]] <- CL_DESC$quanti[[i]] %>%
    as_tibble(rownames = "VAR") %>%
    left_join(AUX$GRUPO |> select(VAR, IND_LAB),
              by = "VAR")
}
names(CL_DESC$quanti) <- paste0("Clus.", 1:4)

# Agregar Nombres Human-readable a descripción de
#   clúster por variables cualitativas
for(i in 1:length(CL_DESC$category )){
  CL_DESC$category[[i]] <- CL_DESC$category[[i]] %>%
    as_tibble(rownames = "V") %>%
    mutate(VAR = str_split(V, "=", simplify = T)[ , 1],
           CAT = str_split(V, "=", simplify = T)[ , 2],
           CAT = case_when(CAT %in% c("FALSE", "NO") ~ "No",
                           CAT %in% c("TRUE", "SI") ~ "Sí",
                           T ~ CAT)  ) %>%
    left_join(AUX$GRUPO |> select(VAR, IND_LAB),
              by = "VAR") %>%
    filter(CAT != "NA" &
             !(str_starts(VAR, "T_" ) & CAT == "No") &
             !(str_starts(VAR, "TIT_" ) & CAT == "No") &
             !(str_starts(VAR, "CONTINUIDAD_" ) &
                 CAT == "No")) %>%
    mutate(IND_LAB = paste(IND_LAB, CAT, sep = ": "))
}
names(CL_DESC$category) <- paste0("Clus.", 1:4)

# Principales características por cluster
C_CUANTI <- C_CUALI <- data.frame()
for(i in 1:4){
  #cat(paste("\n", "Circuito", i, "\n"))
  c <- paste("Clúster", i)
  b <- CL_DESC$quanti[[i]]
  C_CUANTI[1:10, c] <- b[c(1:5, ((nrow(b)-4):nrow(b))), ]  %>%
    mutate(X_ = `Mean in category`,
           X_ = ifelse(str_starts(IND_LAB, "%"),
                       X_ * 100, X_),
           X_ = ifelse(X_ > 1, round(X_, 1), round(X_, 3)),
           IND_LAB = paste(IND_LAB,
                             round(X_, 3),
                             sep = ": ")) %>%
    select(IND_LAB)
  b <- CL_DESC$category[[i]]
  C_CUALI[1:10, c] <- b[c(1:5, ((nrow(b)-4):nrow(b))), ]  %>%
    mutate(IND_LAB = paste(IND_LAB, " (",
                             round(`Mod/Cla`, 1), "%)",
                             sep = "")) %>%
    select(IND_LAB)
}

SEC_MFA$CL_DESC <- CL_DESC

rm(dat, i, c, b, CL_DESC)

# Segregación -------------------------------------------------------------

s_aux <- SEC_seg |>
  rename_with(.cols = -ID_s,
              .fn = ~str_remove(.x, pattern = "N_") ) |>
  left_join(SEC_aux |> select(ID_s, SECTOR, cl),
            by = "ID_s") |>
  filter(!is.na(cl)) |>
  pivot_longer(cols = PRI:SUP, names_to = "group",
               values_to = "peso")

# Global = Between Seg + Within Seg
SEGRE <- list()

SEGRE$t_f <- cbind(I = c("Sólo Agrupamiento", "Sólo Sector",
                         "Agrupamiento + Sector"),
                   rbind(f_seg(db = s_aux, VAR = "cl"),
                         f_seg(db = s_aux, VAR = "SECTOR"),
                         f_seg(db = s_aux, VAR = c("cl", "SECTOR"))))  |>
  mutate(Prop_ENTRE = round(Between / Global * 100, 1),
         Prop_DENTRO = round(Within / Global * 100, 1)) %>%
  select(I, Prop_ENTRE, Prop_DENTRO)

# Local
SEGRE$t_l <- mutual_local(s_aux, group = "group", unit = "cl",
                          weight = "peso", se =F, wide = T) %>%
  select(cl, p, Inter = ls) %>%
  mutate(cl = paste("Clúster", cl),
         Intra = map_dbl(1:4,
                         \(x) mutual_total(data = s_aux[s_aux$cl == x, ],
                                           group = "group", unit = "ID_s",
                                           weight = "peso", se = F)$est[1] )
  )

rm(s_aux, SEC_seg)
