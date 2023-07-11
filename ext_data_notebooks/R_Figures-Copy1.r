# -*- coding: utf-8 -*-
library(drc)
library(tidyverse)
library(patchwork)
library(ggrepel)
library(cowplot)
ggplot2::theme_set(theme_minimal())


# install.packages("drc")













fig1_ylim = c(0.825, 1.14)
#            Min                Max
#       Raw  0.857902419696555  1.13000518877101
# Ensembled  0.827244808392623  1.13000518877101




Figure1_Data_1Mod <- read.csv("../output/Figure1_Data_1Mod.csv")
Figure1_Data_1Mod %>% head()

# Figure1_Data_1Mod  <- Figure1_Data_1Mod[!is.na(Figure1_Data_1Mod$model), ]



standardize_dat <- function(M){
    M <- M %>% 
      mutate(model = 
               case_when(
                model == "knn" ~ "KNN",
                model == "rf" ~ "RF",
                model == "rnr" ~ "RNR",
                model == "svrl" ~ "SVR",
                model == "DNN-Con." ~ "DNN-CO",
                model == "DNN-Sim." ~ "DNN-SO",

                model == "lm" ~ "LM",            

                model == "BLUP" ~ "BLUP",
                model == "Training Mean" ~ "Mean"
    ))

    # order the facets
    M$model <- factor(M$model, levels = c(
      "Mean", "LM", "BLUP", "KNN", "RNR", "RF", "SVR", "DNN-CO", "DNN-SO"))

    M$model_class <- factor(M$model_class, levels = c("LM", "BLUP", "ML", "DNN"))
    # M$data_source <- factor(M$data_source, levels = c("G", "S", "W", "Multi"))
    return(M)    
}
Figure1_Data_1Mod <- standardize_dat(Figure1_Data_1Mod)


Figure1_Data_1Mod <- Figure1_Data_1Mod[!is.na(Figure1_Data_1Mod$model), ]

fig1_orig_mods <- ggplot(Figure1_Data_1Mod, aes(model, test_rmse))+
    geom_boxplot(aes(fill = model_class))+
    scale_fill_brewer(type = "qual", palette = "Set2")+
    labs(y = '')+
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = 'none')+
    coord_cartesian(ylim = fig1_ylim)
# fig1_orig_mods





temp <- read.csv("../output/Figure1_Data_Heatmap.csv")
# temp %>% head()



temp <- temp %>% 
    separate(Model1, c("model", "num"), "_") %>% 
    mutate(model = 
        case_when(
            model == "knn" ~ "KNN",
            model == "rf" ~ "RF",
            model == "rnr" ~ "RNR",
            model == "svrl" ~ "SVR",
            model == "cat" ~ "DNN-CO",
            model == "full" ~ "DNN-SO",

            model == "lm" ~ "LM",            

            model == "bglr" ~ "BLUP",
            model == "Training Mean" ~ "Mean" )) %>% 
#     mutate(model1 = model) %>% 
    unite(Model1, model:num,  sep = "_", remove = TRUE) %>% 
    separate(Model2, c("model", "num"), "_") %>% 
    mutate(model = 
        case_when(
            model == "knn" ~ "KNN",
            model == "rf" ~ "RF",
            model == "rnr" ~ "RNR",
            model == "svrl" ~ "SVR",
            model == "cat" ~ "DNN-CO",
            model == "full" ~ "DNN-SO",

            model == "lm" ~ "LM",            

            model == "bglr" ~ "BLUP",
            model == "Training Mean" ~ "Mean" )) %>% 
#     mutate(model2 = model) %>% 
    unite(Model2, model:num,  sep = "_", remove = TRUE)

# separate_wider_delim(Model1, "_", names = c("A", "B"))



mods <- c("Mean", "LM", "BLUP", "KNN", "RNR", "RF", "SVR", "DNN-CO", "DNN-SO")
reps <- 0:9
mod_reps <- paste(rep(mods, each = length(reps)),
                  rep(reps, time = length(mods)), sep = '_')

temp$Model1 <- factor(temp$Model1, levels = mod_reps)
temp$Model2 <- factor(temp$Model2, levels = mod_reps)



options(repr.plot.width=16, repr.plot.height=10)

add_lines_at = 0.5+(10*1:8)

fig1_heatmap <- ggplot(temp, aes(x = Model1, y = Model2, fill = RMSE))+
    geom_tile()+
    scale_fill_viridis_c()+
    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    geom_hline(yintercept = add_lines_at, color = 'white')+
    geom_vline(xintercept = add_lines_at, color = 'white')
# fig1_heatmap 

# are there any cases where a single model is better than all it's possible ensembles?
# diagonal < off

test_mod = 'LM_0'

single_mods <- unique(temp$Model1)

ens_pr_better <- map(single_mods, function(test_mod){
    single_mod_mask <- ((temp$Model1 == test_mod) & (temp$Model2 == test_mod))
    single_mod <- temp[single_mod_mask, 'RMSE']

    # only considering Model1 to 'shave' the matrix
    ens <- temp[(!single_mod_mask & (temp$Model1 == test_mod)) , ] 
    
    # % ens better
    return(mean(ens$RMSE < single_mod))    
}) %>% unlist()

ens_pr <- data.frame(single_mods, ens_pr_better)  %>% 
    mutate(ens_n_better = (79*ens_pr_better)) %>% 
    arrange(ens_pr_better)
ens_pr %>% head()
ens_pr %>% tail(n = 15)

# include table in supplement

ens_pr %>% head()

ens_pr['Model'] <- stringr::str_extract(ens_pr$single_mods, '\\D+') %>% stringr::str_remove('_')
ens_pr %>% head()



ens_pr %>% 
    summarise(
        ens_pr_better = mean(ens_pr_better),
        ens_n_better = mean(ens_n_better)
    )


# include table
ens_pr %>% 
    group_by(Model) %>% 
    summarise(
        ens_pr_better = mean(ens_pr_better),
        ens_n_better = mean(ens_n_better)
    ) %>% arrange(ens_pr_better)





temp['Model1'] <- stringr::str_extract(temp$Model1, '\\D+') %>% stringr::str_remove('_')
temp['Model2'] <- stringr::str_extract(temp$Model2, '\\D+') %>% stringr::str_remove('_')
temp['Models'] <- paste0(temp$Model1, '_', temp$Model2)



temp %>% head()

# set to same format as temp
Figure1_Data_1Mod_small <- Figure1_Data_1Mod %>% 
    rename(
        Models = model,
        RMSE = test_rmse) %>% 
    select(-data_source, #-model_class, 
           -replicate)

temp <- full_join(temp, Figure1_Data_1Mod_small)

# Deduplicate equivalent Models
out  <- list()
for(i in seq(1, length(mods))){
    for(j in seq(i, length(mods))){
        if(mods[i] == mods[j]){
            out[length(out)+1] <- mods[i]
        } else {
            out[length(out)+1] <- paste(mods[i], mods[j], sep = '_')
        }
    }
}
temp <- temp[temp$Models %in% out, ]







temp <- temp %>% 
    group_by(Models) %>% 
    mutate(AveRMSE = mean(RMSE)) %>% 
    ungroup() %>% 
    arrange(AveRMSE)
temp$Models <- factor(temp$Models, levels = unique(temp$Models))
temp %>% head()



fig1_ens <- ggplot(temp, aes(Models, RMSE))+
#     geom_vline(xintercept = unique(temp$Models), color = 'darkgray')+
#     geom_hline(yintercept = c(0.7+(0.05*1:10)), color = 'darkgray')+

    geom_boxplot(fill = 'lightgray', width=0.7)+
    geom_boxplot(data = Figure1_Data_1Mod_small, aes(fill = model_class, group = Models), width=0.7)+
    geom_boxplot(data = Figure1_Data_1Mod_small[Figure1_Data_1Mod_small$Models %in% c('KNN', 'RNR', 'RF'), ], color = '#8da0cb', width=0.7)+
    labs(x = "")+
    theme(axis.text.x = element_blank(), legend.position = 'none',
#           panel.grid.major = element_blank(), panel.grid.minor = element_blank()
         )+
    coord_cartesian(ylim = fig1_ylim)+
    scale_fill_brewer(type = "qual", palette = "Set2")

fig1_ens

# fig1_ens <- ggplot(temp, aes(Models, RMSE))+
#     geom_boxplot(fill = 'lightgray')+
#     labs(x = "")+
#     theme(axis.text.x = element_blank())+
#     coord_cartesian(ylim = fig1_ylim)
# fig1_ens



temp$Model1_class <- case_when(
    temp$Model1 %in% c('LM') ~ 'LM',
    temp$Model1 %in% c('BLUP') ~ 'BLUP',
    temp$Model1 %in% c('KNN', 'RNR', 'RF', 'SVR') ~ 'ML',
    temp$Model1 %in% c('DNN-CO', 'DNN-SO') ~ 'DNN'
    )

temp$Model2_class <- case_when(
    temp$Model2 %in% c('LM') ~ 'LM',
    temp$Model2 %in% c('BLUP') ~ 'BLUP',
    temp$Model2 %in% c('KNN', 'RNR', 'RF', 'SVR') ~ 'ML',
    temp$Model2 %in% c('DNN-CO', 'DNN-SO') ~ 'DNN'
    )

# if there's only one model (not ensembled) then it should be filled in for plotting
temp[is.na(temp$Model1), 'Model1_class'] <- temp[is.na(temp$Model1), 'model_class']
temp[is.na(temp$Model1), 'Model1'] <- temp[is.na(temp$Model1), 'Models']

temp[is.na(temp$Model2), 'Model2_class'] <- temp[is.na(temp$Model2), 'model_class']
temp[is.na(temp$Model2), 'Model2'] <- temp[is.na(temp$Model2), 'Models']

temp$Model1_class <- factor(temp$Model1_class, levels = c("LM", "BLUP", "ML", "DNN"))
temp$Model2_class <- factor(temp$Model2_class, levels = c("LM", "BLUP", "ML", "DNN"))

temp$Model1 <- factor(temp$Model1, levels = c("LM", "BLUP", "KNN", "RNR", "RF", "SVR", "DNN-CO", "DNN-SO"))

# temp[temp$Models == 'DNN-CO', ]

fig1_ens_members <- ggplot(temp, aes(x = Models))+
    geom_tile(aes(y = Model1, fill = Model1_class))+
    geom_tile(aes(y = Model2, fill = Model2_class))+
    # Add in fake grid
    geom_vline(xintercept = 0.5+1:35, color = 'gray')+
    geom_hline(yintercept = 0.5+1:7, color = 'gray')+
    labs(x = 'Models Combinations', y = 'Models')+
    scale_fill_brewer(type = "qual", palette = "Set2")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = 'none',
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fig1_ens_members

# layout <- "
# AAAAAAAB
# AAAAAAAB
# AAAAAAAB
# AAAAAAAB
# CCCCCCCE
# "

# options(repr.plot.width=16, repr.plot.height=10)

# (fig1_ens+fig1_orig_mods+fig1_ens_members)+plot_layout(design = layout)



layout <- "
A
A
A
A
B
"
options(repr.plot.width=16, repr.plot.height=10)

(fig1_ens+fig1_ens_members)+plot_layout(design = layout)

fig1_heatmap























temp <- read.csv("../output/SFigure1_Data_RMSE_Ens_Reps.csv")
temp1 <- temp
temp %>% head()

sfig1_ens_reps <- ggplot(temp, aes(x = n_mods, y = rmse, color = mod_group))+
    geom_jitter(width = 0.25, height = 0.0, alpha = 0.3)+
    geom_smooth(aes(fill = mod_group),      alpha = 0.3)+
    theme(legend.position = 'bottom')+
    lims(y = c(0.7, 1.2))
# sfig1_ens_reps

temp <- read.csv("../output/SFigure1_Data_RMSE_Ens_Mods.csv")
temp2 <- temp
temp %>% head()

sfig1_ens_mods <- ggplot(temp, aes(x = n_mods, y = rmse))+
    geom_jitter(width = 0.25, height = 0.0, alpha = 0.3)+
    geom_smooth(alpha = 0.3)+
    lims(y = c(0.7, 1.2))
# sfig1_ens_mods

temp <- read.csv("../output/SFigure1_Data_RMSE_Ens_SelectMods.csv")
temp %>% head()

sfig1_ens_selectmods <- ggplot(temp, aes(x = n_mods, y = rmse))+
    geom_jitter(width = 0.25, height = 0.0, alpha = 0.3)+
    geom_smooth(alpha = 0.3, color = 'red')+
    lims(y = c(0.7, 1.2))
# sfig1_ens_selectmods





(
(sfig1_ens_reps+geom_smooth(data = temp, alpha = 0.3, color = 'red')+lims(x = c(0, 10)))+
(sfig1_ens_mods+geom_smooth(data = temp, alpha = 0.3, color = 'red'))+
(sfig1_ens_selectmods)
)



temp <- read.csv("../output/SFigure1_Data_RMSE_Ens_AllMods.csv")


temp <- temp %>% 
    rename(model = Model1) %>% 
    mutate(model = 
        case_when(
            model == "knn" ~ "KNN",
            model == "rf" ~ "RF",
            model == "rnr" ~ "RNR",
            model == "svrl" ~ "SVR",
            model == "cat" ~ "DNN-CO",
            model == "full" ~ "DNN-SO",

            model == "lm" ~ "LM",            

            model == "bglr" ~ "BLUP",
            model == "Training Mean" ~ "Mean" )) %>% 
    rename(Model1 = model) %>% 
    rename(model = Model2) %>% 
    mutate(model = 
        case_when(
            model == "knn" ~ "KNN",
            model == "rf" ~ "RF",
            model == "rnr" ~ "RNR",
            model == "svrl" ~ "SVR",
            model == "cat" ~ "DNN-CO",
            model == "full" ~ "DNN-SO",

            model == "lm" ~ "LM",            

            model == "bglr" ~ "BLUP",
            model == "Training Mean" ~ "Mean" )) %>% 
    rename(Model2 = model)

temp$Model1 <- factor(temp$Model1, levels = c("LM", "BLUP", "KNN", "RNR", "RF", "SVR", "DNN-CO", "DNN-SO"))
temp$Model2 <- factor(temp$Model2, levels = c("LM", "BLUP", "KNN", "RNR", "RF", "SVR", "DNN-CO", "DNN-SO"))

temp %>% head()





temp$Model1 %>% unique

temp %>% head()

# mod1 = 'BLUP'
# mod2 = 'DNN-CO'

# dat <- temp[((temp$Model1 == mod1) & (temp$Model2 == mod2)), ]
# ggplot(dat, aes(x = n_mods, y = rmse))+
#     geom_point()


# dat$rmse <- dat$rmse * -1


# fm = drm(rmse ~ n_mods, 
#          data = dat, 
# #          fct = AR.3() # up and saturate
#          fct = EXD.3()# exponential decay
# #          fct = L.4()
# #         pmodels = list(~1, ~1)
#         )

# fm_predict <- data.frame(n_mods = seq(1, 20))
# fm_predict$rmse <- predict(fm, newdata = fm_predict)

# ggplot(dat, aes(x = n_mods, y = rmse))+
#     geom_point()+
#     geom_line(data = fm_predict, color = 'blue')



# target_model = 'BLUP'

# dat <- temp[((temp$Model1 == target_model) | (temp$Model2 == target_model)), ]
# dat$Comparison <- dat$Model1
# # Don't need to find special case where both are target model because this 
# # will only overwrite cases where that is not the case
# dat[dat$Model1 == target_model, 'Comparison'] <- dat[dat$Model1 == target_model, 'Model2']

# # make prediction df
# predict_df <- expand.grid(
#     rmse = NA,
#     n_mods = seq(min(dat$n_mods), max(dat$n_mods)),
#     Comparison = unique(dat$Comparison)    
# )

# fm = drm(rmse ~ n_mods, 
#          data = dat, Comparison,
#        # fct = AR.3() # up and saturate
#          fct = EXD.3(), # exponential decay
#          #  f(x) = c + (d − c)(exp(−x/e))
#          #  (c, d, e)
#          pmodels = list(
#              ~factor(Comparison), # y at t=inf
#              ~factor(Comparison), # rate
#              ~1
#          ) 
#         )


# predict_df$rmse <- predict(fm, newdata = predict_df)
# #         , se.fit = TRUE)

# # Apply grouping colors to match above
# predict_df$Comparison_class <- case_when(
#     predict_df$Comparison %in% c('LM') ~ 'LM',
#     predict_df$Comparison %in% c('BLUP') ~ 'BLUP',
#     predict_df$Comparison %in% c('KNN', 'RNR', 'RF', 'SVR') ~ 'ML',
#     predict_df$Comparison %in% c('DNN-CO', 'DNN-SO') ~ 'DNN'
#     )

# predict_df[is.na(predict_df$Comparison), 'Comparison_class'] <- predict_df[is.na(predict_df$Comparison), 'model_class']
# predict_df[is.na(predict_df$Comparison), 'Comparison'] <- predict_df[is.na(predict_df$Comparison), 'Models']

# predict_df$Comparison_class <- factor(predict_df$Comparison_class, levels = c("LM", "BLUP", "ML", "DNN"))

# mask_linetype1 = !(predict_df$Comparison %in% c("RF", "RNR", "SVR", "DNN-SO"))
# mask_linetype2 =  (predict_df$Comparison %in% c("RF","DNN-SO"))
# mask_linetype3 =  (predict_df$Comparison %in% c("RNR"))
# mask_linetype4 =  (predict_df$Comparison %in% c("SVR"))

# plt <- ggplot(predict_df, aes(x = n_mods, y = rmse, color  = Comparison_class, group = Comparison))+
#     geom_line(data = predict_df[mask_linetype1, ], linewidth = set_linesize, )+
#     geom_line(data = predict_df[mask_linetype2, ], linewidth = set_linesize, linetype = 'longdash')+
#     geom_line(data = predict_df[mask_linetype3, ], linewidth = set_linesize, linetype = 'dashed' )+
#     geom_line(data = predict_df[mask_linetype4, ], linewidth = set_linesize, linetype = 'dotdash')+
#     ggrepel::geom_label_repel(data =predict_df[predict_df$n_mods == max(predict_df$n_mods), ], 
#                               aes(label = Comparison, fill = Comparison_class), 
#                               color = "black", 
#                               nudge_x = length(unique(predict_df$n_mods))/40)+
#     scale_color_brewer(type = "qual", palette = "Set2")+
#     scale_fill_brewer(type = "qual", palette = "Set2")+
#     theme(legend.position = "None")+
#     labs(x = "Number of Models", y = "RMSE")   
# plt



# Extend to multiple 
fit_exd_for_model <- function(target_model = 'BLUP'){
    print(paste0("Comparing Performance: ", target_model))

dat <- temp[((temp$Model1 == target_model) | (temp$Model2 == target_model)), ]
dat$Comparison <- dat$Model1
# Don't need to find special case where both are target model because this 
# will only overwrite cases where that is not the case
dat[dat$Model1 == target_model, 'Comparison'] <- dat[dat$Model1 == target_model, 'Model2']

dat <- mutate(dat, Comparison = as.character(Comparison)) # explicitly setting comparison to chr instead of factor
# allows for correct leveling if a group is dropped.

check_sd <- dat %>% 
            group_by(Model1, Model2) %>% 
            summarise(sd = sd(rmse))




if(nrow(check_sd[check_sd$sd == 0, ]) == 0){
    fm = drm(rmse ~ n_mods, 
             data = left_join(select(check_sd[check_sd$sd > 0, ], -sd), dat), 
             Comparison,
           # fct = AR.3() # up and saturate
             fct = EXD.3(), # exponential decay
             #  f(x) = c + (d − c)(exp(−x/e))
             #  (c, d, e)
             pmodels = list(
                 ~factor(Comparison), # y at t=inf
                 ~factor(Comparison), # rate
                 ~1
             ) 
            )

    Comparison_ok_sd <- unlist(unique(select(ungroup(left_join(select(
        check_sd[check_sd$sd > 0, ], -sd), dat)), Comparison)))

    # make prediction df
    predict_df <- expand.grid(
        rmse = NA,
        n_mods = seq(min(dat$n_mods), max(dat$n_mods)),
        Comparison = Comparison_ok_sd    
    )

    predict_df$fit <- "EXD3"

    predict_df$rmse <- predict(fm, newdata = predict_df)



} else {
    exd_dat <- left_join(select(check_sd[check_sd$sd > 0, ], -sd), dat)
    fm = drm(rmse ~ n_mods, 
             data = exd_dat,  
             Comparison,
           # fct = AR.3() # up and saturate
             fct = EXD.3(), # exponential decay
             #  f(x) = c + (d − c)(exp(−x/e))
             #  (c, d, e)
             pmodels = list(
                 ~factor(Comparison), # y at t=inf
                 ~factor(Comparison), # rate
                 ~1
             ) 
            )

    fm_linear = lm(rmse ~ n_mods, 
                   data = left_join(select(check_sd[check_sd$sd == 0, ], -sd), dat))


    Comparison_ok_sd <- unlist(unique(select(ungroup(left_join(select(
        check_sd[check_sd$sd > 0, ], -sd), dat)), Comparison)))

    Comparison_0sd <- unlist(unique(select(ungroup(left_join(select(
        check_sd[check_sd$sd == 0, ], -sd), dat)), Comparison)))

    # make prediction df
    predict_df <- expand.grid(
        rmse = NA,
        n_mods = seq(min(dat$n_mods), max(dat$n_mods)),
        Comparison = Comparison_ok_sd    
    )

    predict_df_0sd <- expand.grid(
        rmse = NA,
        n_mods = seq(min(dat$n_mods), max(dat$n_mods)),
        Comparison = Comparison_0sd    
    )

    predict_df$fit <- "EXD3"
    predict_df_0sd$fit <- "Linear"

    predict_df$rmse <- predict(fm, newdata = predict_df)
    predict_df_0sd$rmse <- predict(fm_linear, newdata = predict_df_0sd)

    predict_df <- rbind(predict_df, predict_df_0sd)

}

# Apply grouping colors to match above
predict_df$Comparison_class <- case_when(
    predict_df$Comparison %in% c('LM') ~ 'LM',
    predict_df$Comparison %in% c('BLUP') ~ 'BLUP',
    predict_df$Comparison %in% c('KNN', 'RNR', 'RF', 'SVR') ~ 'ML',
    predict_df$Comparison %in% c('DNN-CO', 'DNN-SO') ~ 'DNN'
    )

predict_df[is.na(predict_df$Comparison), 'Comparison_class'] <- predict_df[is.na(predict_df$Comparison), 'model_class']
predict_df[is.na(predict_df$Comparison), 'Comparison'] <- predict_df[is.na(predict_df$Comparison), 'Models']

predict_df$Comparison_class <- factor(predict_df$Comparison_class, levels = c("LM", "BLUP", "ML", "DNN"))

return(list(
    predict_df = predict_df,
    fm = fm
))
}




plot_exd_for_model <- function(res, set_linesize = 1){
    predict_df <- res$predict_df
    
    mask_linetype1 = !(predict_df$Comparison %in% c("RF", "RNR", "SVR", "DNN-SO"))
    mask_linetype2 =  (predict_df$Comparison %in% c("RF","DNN-SO"))
    mask_linetype3 =  (predict_df$Comparison %in% c("RNR"))
    mask_linetype4 =  (predict_df$Comparison %in% c("SVR"))

    plt <- ggplot(predict_df, aes(x = n_mods, y = rmse, color  = Comparison_class, group = Comparison))+
        geom_line(data = predict_df[mask_linetype1, ], linewidth = set_linesize, )+
        geom_line(data = predict_df[mask_linetype2, ], linewidth = set_linesize, linetype = 'longdash')+
        geom_line(data = predict_df[mask_linetype3, ], linewidth = set_linesize, linetype = 'dashed' )+
        geom_line(data = predict_df[mask_linetype4, ], linewidth = set_linesize, linetype = 'dotdash')+
        ggrepel::geom_label_repel(data =predict_df[predict_df$n_mods == max(predict_df$n_mods), ], 
                                  aes(label = Comparison, fill = Comparison_class), 
                                  color = "black", 
                                  nudge_x = length(unique(predict_df$n_mods))/5)+
        scale_color_brewer(type = "qual", palette = "Set2")+
        scale_fill_brewer(type = "qual", palette = "Set2")+
        theme(legend.position = "None")+
        labs(x = "Number of Models", y = "RMSE")    
    return(plt)
}



mod_names <- c("LM", "BLUP", "KNN", "RNR", "RF", "SVR", "DNN-CO", "DNN-SO")

if(file.exists("./res_list.RDS")){
    res_list <- readRDS("./res_list.RDS")
} else {
    res_list <- map(mod_names, 
        function(e){fit_exd_for_model(target_model = e)})
    
    saveRDS(res_list, "./res_list.RDS")
}

# executed in 15m 9s







res_plt_list <- map(res_list, function(e){
    plot_exd_for_model(res = e, set_linesize = 1)})

# enforce matching limits
res_plt_list <- map(res_plt_list, function(e){
    return(
#         e+lims(y = c(min(temp$rmse), max(temp$rmse) )) 
        e+coord_cartesian(y = c(0.875, 1.1))+geom_hline(yintercept = 0.889879972458087) 
          )
})

cowplot::plot_grid(plotlist = res_plt_list, 
                   nrow = 1, 
                   labels = paste0(LETTERS[1:length(mod_names)], '. ', mod_names))

sum(8:1)

best_rmses <- unlist(map(res_list, function(e){min(e$predict_df[, 'rmse'])}))

best_rmses
min(best_rmses)





















# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------

# Extend to multiple 
fit_exd_for_model <- function(target_model = 'BLUP'){
    dat <- temp[((temp$Model1 == target_model) | (temp$Model2 == target_model)), ]
    dat$Comparison <- dat$Model1
    # Don't need to find special case where both are target model because this 
    # will only overwrite cases where that is not the case
    dat[dat$Model1 == target_model, 'Comparison'] <- dat[dat$Model1 == target_model, 'Model2']

    # make prediction df
    predict_df <- expand.grid(
        rmse = NA,
        n_mods = seq(min(dat$n_mods), max(dat$n_mods)),
        Comparison = unique(dat$Comparison)    
    )

    fm = drm(rmse ~ n_mods, 
             data = dat, Comparison,
           # fct = AR.3() # up and saturate
             fct = EXD.3(), # exponential decay
             #  f(x) = c + (d − c)(exp(−x/e))
             #  (c, d, e)
             pmodels = list(
                 ~factor(Comparison), # y at t=inf
                 ~factor(Comparison), # rate
                 ~1
             ) 
            )


    predict_df$rmse <- predict(fm, newdata = predict_df)
    #         , se.fit = TRUE)

    # Apply grouping colors to match above
    predict_df$Comparison_class <- case_when(
        predict_df$Comparison %in% c('LM') ~ 'LM',
        predict_df$Comparison %in% c('BLUP') ~ 'BLUP',
        predict_df$Comparison %in% c('KNN', 'RNR', 'RF', 'SVR') ~ 'ML',
        predict_df$Comparison %in% c('DNN-CO', 'DNN-SO') ~ 'DNN'
        )

    predict_df[is.na(predict_df$Comparison), 'Comparison_class'] <- predict_df[is.na(predict_df$Comparison), 'model_class']
    predict_df[is.na(predict_df$Comparison), 'Comparison'] <- predict_df[is.na(predict_df$Comparison), 'Models']

    predict_df$Comparison_class <- factor(predict_df$Comparison_class, levels = c("LM", "BLUP", "ML", "DNN"))

    return(list(
        predict_df = predict_df,
        fm = fm
    ))
}





res <- fit_exd_for_model(target_model = 'BLUP')




# predict_df <- res$predictions





plot_exd_for_model(res = fit_exd_for_model(target_model = 'LM'), 
                   set_linesize = 1)













# temp[temp$Models == 'DNN-CO', ]











if(sd(dat$rmse) == 0){
}

lm()



inmod = 'LM'

dat <- temp[((temp$Model1 == inmod) & (temp$Model2 == inmod)), ]

lowess_values <- lowess(dat$n_mods, dat$rmse, f = 1)
# plot(lowess_values, type = "l")

ggplot(dat, aes(x = n_mods, y = rmse, color = Model2, fill = Model2))+
    geom_point()+
    geom_line(aes(x = lowess_values$x, y = lowess_values$y))

str(lowess_values)

inmod = 'LM'

dat <- temp[((temp$Model1 == inmod) | (temp$Model2 == inmod)), ]
# dat
ggplot(dat, aes(x = n_mods, y = rmse, color = Model2, fill = Model2))+
    geom_smooth(method = 'loess')
#     geom_point()

inmod = 'BLUP'

dat <- temp[((temp$Model1 == inmod) | (temp$Model2 == inmod)), ]
# dat
ggplot(dat, aes(x = n_mods, y = rmse, color = Model2, fill = Model2))+
    geom_smooth(method = 'loess')
#     geom_point()







# # sfig1_ens_selectmods <- 

# ggplot(temp, aes(x = n_mods, y = rmse))+
#     geom_jitter(width = 0.25, height = 0.0, alpha = 0.3)+
#     geom_smooth(alpha = 0.3, color = 'red', method = 'lm')+
# #     lims(y = c(0.7, 1.2))+
#     facet_grid(Model1 ~ Model2)

# # sfig1_ens_selectmods

mods <- c("LM", "BLUP", "KNN", "RNR", "RF", "SVR", "DNN-CO", "DNN-SO")

plt_list <- list()

for(i in seq(1, length(mods))){
for(j in seq(i, length(mods))){
dat <- temp[((temp$Model1 == mods[i]) & (temp$Model2 == mods[j])), ]

# ggplot(dat[!is.na(dat$rmse), ], aes(x = n_mods, y = rmse))+
#     geom_jitter(width = 0.25, height = 0.0, alpha = 0.3)+
#     geom_smooth(alpha = 0.3, color = 'red', method = 'loess')

dat_stats <- dat %>% 
    group_by(n_mods, Model1, Model2) %>% 
    summarise(
        xbar = mean(rmse),
        q99 = quantile(rmse, c(.99)), 
        q90 = quantile(rmse, c(.9)),
        q10 = quantile(rmse, c(.1)), 
        q75 = quantile(rmse, c(.75)),
        q50 = quantile(rmse, c(.5)),
        q25 = quantile(rmse, c(.25)),
        q10 = quantile(rmse, c(.1)), 
        q01 = quantile(rmse, .01), 
    ) %>% ungroup()
# dat_stats

plt <- dat_stats %>% 
    ggplot()+
        geom_ribbon(aes(x = n_mods, ymin=q01, ymax=q99), fill="blue", alpha = 0.1)+
        geom_ribbon(aes(x = n_mods, ymin=q10, ymax=q90), fill="blue", alpha = 0.1)+
        geom_ribbon(aes(x = n_mods, ymin=q25, ymax=q75), fill="blue", alpha = 0.1)+
        # geom_jitter(data = dat[!is.na(dat$rmse), ], aes(x = n_mods, y = rmse), width = 0.2, height = 0.0, alpha = 0.1)
        geom_smooth(data = dat[!(dat$n_mods %in% c(min(dat$n_mods),
                                                   max(dat$n_mods)
                                                  )), ], 
                    aes(x = n_mods, y = rmse), 
                    alpha = 0.3, 
                    color = 'black', 
                    method = 'loess')+
        geom_point(data = dat[(dat$n_mods %in% c(min(dat$n_mods),
                                                   max(dat$n_mods)
                                                  )), ], 
                    aes(x = n_mods, y = rmse), 
                    color = 'black')
plt_list[[length(plt_list)+1]] <- plt
}    
}

options(repr.plot.width=16, repr.plot.height=10)



(plt_list[[1]]+plt_list[[2]]#+plt_list[[3]]+plt_list[[4]]+plt_list[[5]]+plt_list[[6]]+plt_list[[7]]+plt_list[[8]])
 )

length(plt_list)

# layout <- "
# AAAAAAAB
# AAAAAAAB
# AAAAAAAB
# AAAAAAAB
# CCCCCCCE
# "

# options(repr.plot.width=16, repr.plot.height=10)

# (fig1_ens+fig1_orig_mods+fig1_ens_members)+plot_layout(design = layout)












