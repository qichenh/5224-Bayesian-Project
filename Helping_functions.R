# get_school_data ################################################
# the imput data is whole dataset, and school to be chosen
# author: "Qichen He, Shin Ah Oh, Yue Shen, Mo Zhou"

get_school_data = function(data_full, school_id){
  data <- data_full %>% 
    filter(COHORT == "(2) Younger") %>%    # only cohort(2) selected seventh grade student  
    select(CASENUM, SCHOOLID, STRATA, AB29, AMTHIRT, 
           CMTHIRT, EMTHIRT, GMTHIRT) %>%  # use unimputed math scores
    mutate(Gender_coded = recode(AB29, "(1) Female" = 1, "(2) Male" = 0)) %>%
    rename(Gender = AB29, g7_math_score = AMTHIRT, g8_math_score = CMTHIRT,
           g9_math_score = EMTHIRT, g10_math_score = GMTHIRT, school_ID = SCHOOLID,
           case_identifier = CASENUM) %>%
    separate(STRATA, c("country_region", "community_type"), "-") %>%
    mutate(country_region = lapply(strsplit(country_region, " "), `[[`, 2),
           school_ID = unlist(school_ID)) %>%
    as.matrix() %>%   # remove the imbedded column names 
    as.data.frame() %>%
    select(case_identifier, school_ID, country_region, 
           community_type, Gender, Gender_coded, g7_math_score, 
           g8_math_score, g9_math_score, g10_math_score) %>%
    filter(school_ID == school_id) %>% # reorder variables 
    unnest(cols = c(case_identifier, school_ID, community_type, Gender, Gender_coded,
                    g7_math_score, g8_math_score, g9_math_score, g10_math_score)) %>%
    pivot_longer(cols = c(g7_math_score, g8_math_score, 
                          g9_math_score, g10_math_score),
                 values_to = "score",
                 names_to = "grade") %>%
    dplyr::select(case_identifier, Gender_coded, grade, score) %>%
    mutate(grade = ifelse(grade == "g7_math_score", 0, 
                          ifelse(grade == "g8_math_score", 1,
                                 ifelse(grade == "g9_math_score", 2,3)))) %>%
    filter(!is.na(Gender_coded)) %>%
    mutate(Gender_coded = as.factor(Gender_coded))
}

##############################################################################
######################get_dat1_list###########################################
##############################################################################
get_dat1_list = function(data_model){
  Z_i = data_model%>%
    dplyr::group_by(case_identifier) %>% 
    dplyr::summarise(gender = unique(Gender_coded))
  Ni = length(unique(data_model$case_identifier))
  data <- list(Nt = length(data_model$score),
               Ni = Ni,
               cluster = rep(1:Ni, each = 4),
               Y_ti    = data_model$score,
               X_1ti   = data_model$grade,
               Z_i = c(Z_i$gender) - 1)
  return(data)
}
get_dat2_list = function(data_model){
  data = get_dat1_list(data_model)
  data$Z_2i = data$Z_i - mean(data$Z_i)
  return(data)
}


#############################################################################
#############################################################################
train_two_level = function(fileName, data, pars, 
                           chains = 3, 
                           iter = 2000, warmup = 1000, seed = 12345){
  stan_code <- readChar(fileName, file.info(fileName)$size)
  resStan <- stan(model_code = stan_code, data = data,#algorithm = "HMC",
                   chains = chains, iter = iter, warmup = warmup,seed = seed)
  return(resStan)
}

###########################################################################
###############test statistic##############################################
###########################################################################
Test <- function (y){
  mat = matrix(y, ncol = 4, byrow = T)
  x = mean(mat[,4] - mat[,1])
  return(x)
  
}
