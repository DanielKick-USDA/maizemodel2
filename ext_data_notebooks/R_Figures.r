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
    geom_boxplot(fill = 'lightgray', width=0.7)+
    geom_boxplot(data = Figure1_Data_1Mod_small, aes(fill = model_class, group = Models), width=0.7)+
    geom_boxplot(data = Figure1_Data_1Mod_small[Figure1_Data_1Mod_small$Models %in% c('KNN', 'RNR', 'RF'), ], color = '#8da0cb', width=0.7)+
    labs(x = "")+
    theme(axis.text.x = element_blank(), legend.position = 'none'
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
# fig1_ens_members

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

stacked_fig <- (fig1_ens+fig1_ens_members)+plot_layout(design = layout)
stacked_fig

ggsave(stacked_fig, filename = './boxplot_2mods.svg', width = 16, height = 10)
ggsave(stacked_fig, filename = './boxplot_2mods.pdf', width = 16, height = 10)

fig1_heatmap

ggsave(fig1_heatmap, filename = './heatmap_2mods.svg', width = 10, height = 10)
ggsave(fig1_heatmap, filename = './heatmap_2mods.pdf', width = 10, height = 10)





















# temp <- read.csv("../output/SFigure1_Data_RMSE_Ens_Reps.csv")
# temp1 <- temp
# temp %>% head()

# sfig1_ens_reps <- ggplot(temp, aes(x = n_mods, y = rmse, color = mod_group))+
#     geom_jitter(width = 0.25, height = 0.0, alpha = 0.3)+
#     geom_smooth(aes(fill = mod_group),      alpha = 0.3)+
#     theme(legend.position = 'bottom')+
#     lims(y = c(0.7, 1.2))
# # sfig1_ens_reps

# temp <- read.csv("../output/SFigure1_Data_RMSE_Ens_Mods.csv")
# temp2 <- temp
# temp %>% head()

# sfig1_ens_mods <- ggplot(temp, aes(x = n_mods, y = rmse))+
#     geom_jitter(width = 0.25, height = 0.0, alpha = 0.3)+
#     geom_smooth(alpha = 0.3)+
#     lims(y = c(0.7, 1.2))
# # sfig1_ens_mods

# temp <- read.csv("../output/SFigure1_Data_RMSE_Ens_SelectMods.csv")
# temp %>% head()

# sfig1_ens_selectmods <- ggplot(temp, aes(x = n_mods, y = rmse))+
#     geom_jitter(width = 0.25, height = 0.0, alpha = 0.3)+
#     geom_smooth(alpha = 0.3, color = 'red')+
#     lims(y = c(0.7, 1.2))
# # sfig1_ens_selectmods





# (
# (sfig1_ens_reps+geom_smooth(data = temp, alpha = 0.3, color = 'red')+lims(x = c(0, 10)))+
# (sfig1_ens_mods+geom_smooth(data = temp, alpha = 0.3, color = 'red'))+
# (sfig1_ens_selectmods)
# )

temp <- read.csv("../output/SFigure1_Data_RMSE_Ens_SelectMods_redo.csv")


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

# temp %>% head()



# temp %>% group_by(Model1, Model2, n_mods) %>% tally()

temp$Model1 %>% unique

# duplicate to make tidy wrt model1/model2
temp2 <- temp %>% 
    rename(Model3 = Model1) %>% 
    rename(Model1 = Model2) %>% 
    rename(Model2 = Model3)
# drop where Model1==Model2; that's already in temp
temp2 <- temp2[(temp2$Model1 != temp2$Model2), ]

temp <- full_join(temp, temp2)

# Quick check that all groups have the same number of observations:
temp %>% 
    group_by(Model1, Model2, ensemble, n_mods) %>% 
    tally()  %>% 
    ungroup() %>% 
    select(n) %>% 
    distinct()

temp %>% head()

# quick vis --are trends captured by EXD and linear models?

ggplot(temp, aes(n_mods, rmse, color = Model2))+
    geom_smooth()+
    facet_grid(ensemble ~ Model1)



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
fit_exd_for_model <- function(target_model = 'BLUP', ensemble_type = '', sd_threshold = 1e-10
){
    print(paste0("Comparing Performance: ", target_model))

    dat <- temp
    dat <- dat[(dat$ensemble == ensemble_type), ]
    dat <- dat[(dat$Model1 == target_model | dat$Model2 == target_model), ]
    dat$Comparison <- dat$Model1
    dat[dat$Model1 == target_model, 'Comparison'] <- dat[dat$Model1 == target_model, 'Model2']

    
    
#     dat <- temp[(((temp$Model1 == target_model) | (temp$Model2 == target_model)
#                 ) & (temp$ensemble == ensemble_type)), ]
#     dat$Comparison <- dat$Model1
#     # Don't need to find special case where both are target model because this 
#     # will only overwrite cases where that is not the case
#     dat[dat$Model1 == target_model, 'Comparison'] <- dat[dat$Model1 == target_model, 'Model2']

#     dat <- mutate(dat, Comparison = as.character(Comparison)) # explicitly setting comparison to chr instead of factor
    
    # allows for correct leveling if a group is dropped.

    check_sd <- dat %>% 
                group_by(Model1, Model2) %>% 
                summarise(sd = sd(rmse))


    if(nrow(check_sd[check_sd$sd <= sd_threshold, ]) == 0){
        fm = drm(rmse ~ n_mods, 
                 data = left_join(select(check_sd[check_sd$sd > sd_threshold, ], -sd), dat), 
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
        exd_dat <- left_join(select(check_sd[check_sd$sd > sd_threshold, ], -sd), dat)
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
                       data = left_join(select(check_sd[check_sd$sd <= sd_threshold, ], -sd), dat))


        Comparison_ok_sd <- unlist(unique(select(ungroup(left_join(select(
            check_sd[check_sd$sd > sd_threshold, ], -sd), dat)), Comparison)))

        Comparison_0sd <- unlist(unique(select(ungroup(left_join(select(
            check_sd[check_sd$sd <= sd_threshold, ], -sd), dat)), Comparison)))

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


plot_exd_for_model <- function(res, set_linesize = 1, set_linealpha = 0.5){
    predict_df <- res$predict_df
    
    mask_linetype1 = !(predict_df$Comparison %in% c("RF", "RNR", "SVR", "DNN-SO"))
    mask_linetype2 =  (predict_df$Comparison %in% c("RF","DNN-SO"))
    mask_linetype3 =  (predict_df$Comparison %in% c("RNR"))
    mask_linetype4 =  (predict_df$Comparison %in% c("SVR"))

    plt <- ggplot(predict_df, aes(x = n_mods, y = rmse, color  = Comparison_class, group = Comparison))+
        geom_line(data = predict_df[mask_linetype1, ], linewidth = set_linesize, alpha = set_linealpha )+
        geom_line(data = predict_df[mask_linetype2, ], linewidth = set_linesize, linetype = 'longdash', alpha = set_linealpha)+
        geom_line(data = predict_df[mask_linetype3, ], linewidth = set_linesize, linetype = 'dashed', alpha = set_linealpha )+
        geom_line(data = predict_df[mask_linetype4, ], linewidth = set_linesize, linetype = 'dotdash', alpha = set_linealpha)+
        ggrepel::geom_label_repel(data =predict_df[predict_df$n_mods == max(predict_df$n_mods), ], 
                                  aes(label = Comparison, fill = Comparison_class), 
                                  color = "black", 
                                  nudge_x = length(unique(predict_df$n_mods))/20)+
#                                   nudge_x = length(unique(predict_df$n_mods))/5)+
        scale_color_brewer(type = "qual", palette = "Set2")+
        scale_fill_brewer(type = "qual", palette = "Set2")+
        theme(legend.position = "None")+
        labs(x = "Number of Models", y = "RMSE")    
    return(plt)
}

mod_names <- c("LM", "BLUP", "KNN", "RNR", "RF", "SVR", "DNN-CO", "DNN-SO")
ensemble_types <- c('uniform_weights', 'uniform_by_type_weights', 'inv_std_weights', 
        'inv_var_weights', 'inv_rmse_weights')

# res_list <- map(mod_names, 
#         function(e){fit_exd_for_model(target_model = e)})fit_exd_for_model(target_model = e)

# Because this takes a long time to run (est ~ 1hr) I'm splitting it up and saving out multiple data objects
# this way any debugging can be done piecemeal

# mod_names
# for(ensemble_type in ensemble_types){
#     print(paste('Trying', ensemble_type))
#     for(e in mod_names){
#         print(e)
#         fit_exd_for_model(
#             target_model = e, 
#             ensemble_type = ensemble_type,
#             sd_threshold = 1e-3)
        
#     }
# } 

temp %>% head()
# FINDME I was working here trying to get the fitting to work for all models.
# why are there not all the levels of comparison seen?
# 'BLUP', 'RF', 'SVR', 'DNN-CO', 'DNN-SO'


# target_model = 'LM'
# ensemble_type = 'uniform_weights'
# sd_threshold = 1e-3


# exd_dat %>% 
#     ggplot(aes(x = n_mods, y = rmse, color = Comparison))+
#     geom_point()+
#     geom_smooth()







































# Because this takes a long time to run (est ~ 1hr) I'm splitting it up and saving out multiple data objects
# this way any debugging can be done piecemeal

# mod_names
for(ensemble_type in ensemble_types){
    save_path <- paste0("./res_list_ENS_",ensemble_type,".RDS")

    if(!file.exists(save_path)){
        print(paste('Trying', ensemble_type))
        res_list <- map(mod_names, function(e){
            fit_exd_for_model(
                target_model = e, 
                ensemble_type = ensemble_type,
                sd_threshold = 1e-3)
        })
        saveRDS(res_list, save_path)
    }
}




# temp <- map(ensemble_types, function(ensemble_type){
#     M <- readRDS( paste0('./res_list_ENS_', ensemble_type, '.RDS') )
#     temp <- do.call(rbind, map(M, function(e){e$predict_df}))
#     temp['ensemble'] <- ensemble_type
#     return(temp)    
# })

# temp <- do.call(rbind, temp)




mk_ensemble_composite_figure <- function(
    ensemble_type = 'uniform_weights',
    figure_y_min = 0, 
    figure_y_max = 10
){
    # Make individual two model trend plots
    res_list = readRDS( paste0("./res_list_ENS_",ensemble_type,".RDS"))

    res_plt_list <- map(res_list, function(e){
        plot_exd_for_model(res = e, set_linesize = 1, set_linealpha = 0.75)})

    # Pull in and work with ensemble using ANY models --------------------------
    if (TRUE){
        temp_all <- read.csv("../output/SFigure1_Data_RMSE_Ens_Any_n_Mods_redo.csv")

        temp_all <- temp_all[temp_all$ensemble == ensemble_type, ]
        # make prediction df
        predict_df <- expand.grid(
            n_mods = seq(min(temp_all$n_mods), max(temp_all$n_mods))
        )

        fm = drm(rmse ~ n_mods, 
                 data = temp_all, 
                 fct = EXD.3(), # exponential decay
                 #  f(x) = c + (d − c)(exp(−x/e))
                 #  (c, d, e)
                 pmodels = list(
                     ~1, # y at t=inf
                     ~1, # rate
                     ~1) )

        predict_df <- cbind(predict_df, 
                            rename(data.frame(predict(fm, 
                                                      newdata = predict_df, se.fit = TRUE)), 
                                   rmse = Prediction))

        sfig1_ens_mods_any <- ggplot(temp_all, aes(x = n_mods, y = rmse))+
            # geom_point(color = 'black', alpha = 0.3)+
            ggrastr::geom_point_rast(raster.dpi = 350, color = 'black', alpha = 0.3)+
            geom_line(data = predict_df, color = '#ffd92f', linewidth = 1)+
            scale_x_continuous(breaks= seq(0, 80, 10))+
            labs(x = 'Number of Models', y = 'RMSE')+
            coord_cartesian(y = c(figure_y_min, figure_y_max))+
            theme(panel.grid.minor = element_blank())    
    }
    
    # Pull in and work with ensemble using all data --------------------------
    if (TRUE){
        temp_all <- read.csv("../output/SFigure1_Data_RMSE_Ens_AllMods_redo.csv")

        temp_all <- temp_all[temp_all$ensemble == ensemble_type, ]
        # make prediction df
        predict_df <- expand.grid(
            n_mods = seq(min(temp_all$n_mods), max(temp_all$n_mods))
        )

        fm = drm(rmse ~ n_mods, 
                 data = temp_all, 
                 fct = EXD.3(), # exponential decay
                 #  f(x) = c + (d − c)(exp(−x/e))
                 #  (c, d, e)
                 pmodels = list(
                     ~1, # y at t=inf
                     ~1, # rate
                     ~1) )

        predict_df <- cbind(predict_df, 
                            rename(data.frame(predict(fm, 
                                                      newdata = predict_df, se.fit = TRUE)), 
                                   rmse = Prediction))

        sfig1_ens_mods <- ggplot(temp_all, aes(x = n_mods, y = rmse))+
            # geom_point(color = 'black', alpha = 0.3)+
            ggrastr::geom_point_rast(raster.dpi = 350, color = 'black', alpha = 0.3)+
            geom_line(data = predict_df, color = '#a6d854', linewidth = 1)+
            scale_x_continuous(breaks=c(0, 5, 10))+
            labs(x = 'Number of Models', y = 'RMSE')+
            coord_cartesian(x = c(0, 10),
                            y = c(figure_y_min, figure_y_max))+
            theme(panel.grid.minor = element_blank())    
    }
    
    
    
    
    

    # enforce matching limits
    res_plt_list <- map(res_plt_list, function(e){
        return(
            e+scale_x_continuous(breaks=c(0, 5, 10))+
            theme(panel.grid.minor = element_blank())+
            coord_cartesian(x = c(0, 10),
                            y = c(figure_y_min, figure_y_max))
    #         e+lims(y = c(min(temp$rmse), max(temp$rmse) )) 
    #         e+coord_cartesian(y = c(0.83, 1.3))+geom_hline(yintercept = 0.889879972458087) 
              )
    })

    

    multi_plt <- (
        sfig1_ens_mods_any+ggtitle(     paste0(LETTERS[1], '. ', 'Any Model'))+
           sfig1_ens_mods+labs(y = '')+theme(axis.text.y = element_blank())+ggtitle(paste0(LETTERS[1+1], '. ', 'Every Model'))+
        res_plt_list[[1]]+labs(y = '')+theme(axis.text.y = element_blank())+ggtitle(paste0(LETTERS[2+1], '. ', mod_names[1]))+
        res_plt_list[[2]]+labs(y = '')+theme(axis.text.y = element_blank())+ggtitle(paste0(LETTERS[3+1], '. ', mod_names[2]))+
        res_plt_list[[3]]+labs(y = '')+theme(axis.text.y = element_blank())+ggtitle(paste0(LETTERS[4+1], '. ', mod_names[3]))+
        res_plt_list[[4]]+labs(y = '')+theme(axis.text.y = element_blank())+ggtitle(paste0(LETTERS[5+1], '. ', mod_names[4]))+
        res_plt_list[[5]]+labs(y = '')+theme(axis.text.y = element_blank())+ggtitle(paste0(LETTERS[6+1], '. ', mod_names[5]))+
        res_plt_list[[6]]+labs(y = '')+theme(axis.text.y = element_blank())+ggtitle(paste0(LETTERS[7+1], '. ', mod_names[6]))+
        res_plt_list[[7]]+labs(y = '')+theme(axis.text.y = element_blank())+ggtitle(paste0(LETTERS[8+1], '. ', mod_names[7]))+
        res_plt_list[[8]]+labs(y = '')+theme(axis.text.y = element_blank())+ggtitle(paste0(LETTERS[9+1], '. ', mod_names[8])
    )+plot_layout(
        widths = c(2, 1,
                   1, 1, 1, 1, 1, 1, 1, 1)
    )
)
    return(multi_plt)
}





multi_plt <- mk_ensemble_composite_figure(
    ensemble_type = 'uniform_weights',
    figure_y_min = 0.84, 
    figure_y_max = 1.15)

ggsave(multi_plt, filename = './ExponentialDecay_uniform_weights.svg', width = 20, height = 10)
multi_plt



multi_plt <- mk_ensemble_composite_figure(
    ensemble_type = 'uniform_by_type_weights',
    figure_y_min = 0.84, 
    figure_y_max = 1.15)

ggsave(multi_plt, filename = './ExponentialDecay_uniform_by_type_weights.svg', width = 20, height = 10)

multi_plt <- mk_ensemble_composite_figure(
    ensemble_type = 'inv_std_weights',
    figure_y_min = 0.84, 
    figure_y_max = 1.15)

ggsave(multi_plt, filename = './ExponentialDecay_inv_std_weights.svg', width = 20, height = 10)

multi_plt <- mk_ensemble_composite_figure(
    ensemble_type = 'inv_var_weights',
    figure_y_min = 0.84, 
    figure_y_max = 1.15)

ggsave(multi_plt, filename = './ExponentialDecay_inv_var_weights.svg', width = 20, height = 10)

multi_plt <- mk_ensemble_composite_figure(
    ensemble_type = 'inv_rmse_weights',
    figure_y_min = 0.84, 
    figure_y_max = 1.15)

ggsave(multi_plt, filename = './ExponentialDecay_inv_rmse_weights.svg', width = 20, height = 10)



# make a summary table of the predicted values of each mix
mod_fit_table <- do.call(
    rbind, 
    map(ensemble_types, 
        function(ensemble_type){
            res_list = readRDS( paste0("./res_list_ENS_",ensemble_type,".RDS"))

            do.call(rbind, 
                    map(seq_along(res_list), 
                        function(i){
                            res <- res_list[[i]]
                            temp <- filter(res$predict_df, n_mods == 10)
                            temp['Model1'] <- mod_names[i]
                            temp['Ensemble'] <- ensemble_type
                            temp <- temp %>% rename(RMSE = rmse, Model2 = Comparison, Fit = fit)
                            temp <- temp[, c('Ensemble', 'Model1', 'Model2', 'Fit', 'RMSE')]
                            return(temp)    
                        }
                       )
                   )
        }
       )
)
write.csv(mod_fit_table, './ExponentialDecay_fit_summary_long.csv')


mod_fit_table_wide <- mod_fit_table %>% 
    select(-Fit) %>% 
    pivot_wider(
      names_from = Ensemble,
      values_from = RMSE
)

write.csv(mod_fit_table_wide, './ExponentialDecay_fit_summary_wide.csv')

ensemble_types
mod_names

mod_fit_table %>% head()

# mod_fit_table %>% 
#     ggplot(aes(x = Ensemble, y = RMSE, group = Model1))+
#     geom_line()+
#     facet_grid(Model2~.)

mod_fit_table_wide %>% 
    pivot_longer(
    cols = c('uniform_by_type_weights', 'inv_std_weights', 'inv_var_weights', 'inv_rmse_weights')) %>% 
    rename(Ensemble = name, RMSE = value) %>% 
    ggplot(aes(x = uniform_weights, y = RMSE, color = Ensemble))+
        geom_smooth(method = 'lm', se = F, alpha = 0.5)+
        geom_point()+
        coord_fixed()

mod_fit_table_wide

# install.packages("GGally")
GGally::ggpairs(mod_fit_table_wide)

# all_mods_figs_list <- map(ensemble_types, function(ensemble_type){
#     # Pull in and work with ensemble using all data
#     temp <- read.csv("../output/SFigure1_Data_RMSE_Ens_AllMods_redo.csv")


#     temp <- temp[temp$ensemble == ensemble_type, ]
#     # make prediction df
#     predict_df <- expand.grid(
#         n_mods = seq(min(temp$n_mods), max(temp$n_mods))
#     )
    
#     fm = drm(rmse ~ n_mods, 
#              data = temp, 
#              fct = EXD.3(), # exponential decay
#              #  f(x) = c + (d − c)(exp(−x/e))
#              #  (c, d, e)
#              pmodels = list(
#                  ~1, # y at t=inf
#                  ~1, # rate
#                  ~1
#              ) 
#             )

#     predict_df <- cbind(predict_df, 
#                         rename(data.frame(predict(fm, 
#                                                   newdata = predict_df, se.fit = TRUE)), 
#                                rmse = Prediction))

#     sfig1_ens_mods <- ggplot(temp, aes(x = n_mods, y = rmse))+
#         geom_jitter(width = 0.25, height = 0.0, alpha = 0.9)+
#         geom_line(data = predict_df, color = 'black', linewidth = 1)+
#         labs(x = 'Number of Models', y = 'RMSE')
#     return(sfig1_ens_mods)
# })

all_mods_figs_list[[1]]





# enforce matching limits
res_plt_list <- map(res_plt_list, function(e){
    return(
        e+scale_x_continuous(breaks=c(1, 5, 10))
#         e+lims(y = c(min(temp$rmse), max(temp$rmse) )) 
#         e+coord_cartesian(y = c(0.83, 1.3))+geom_hline(yintercept = 0.889879972458087) 
          )
})

# cowplot::plot_grid(plotlist = res_plt_list, 
#                    nrow = 1, 
#                    labels = paste0(LETTERS[1:length(mod_names)], '. ', mod_names))

best_rmses <- unlist(map(res_list, function(e){min(e$predict_df[, 'rmse'])}))

best_rmses
min(best_rmses)

# SFigure1_Data_RMSE_Ens_AllMods_redo.csv



# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

temp <- read.csv("../output/SFigure1_Data_RMSE_Ens_AllMods_redo.csv")
# temp %>% head()

# make prediction df
predict_df <- expand.grid(
#     rmse = NA,
    n_mods = seq(min(temp$n_mods), max(temp$n_mods))
)

# this model fails if n_models is too high
# pass     | fails
# 40 60 62 | 65 70 80
fm = drm(rmse ~ n_mods, 
         data = temp[temp$n_mods < 60, ], 
         fct = EXD.3(), # exponential decay
         #  f(x) = c + (d − c)(exp(−x/e))
         #  (c, d, e)
         pmodels = list(
             ~1, # y at t=inf
             ~1, # rate
             ~1
         ) 
        )

predict_df <- cbind(predict_df, 
                    rename(data.frame(predict(fm, 
                                              newdata = predict_df, se.fit = TRUE)), 
                           rmse = Prediction))

# temp_summary <- temp[temp$n_mods < 80, ] %>% 
#     group_by(n_mods) %>% 
#     summarize(rmse_sd = sd(rmse))
# temp_summary %>% head()

# temp <- full_join(temp, temp_summary)

sfig1_ens_mods <- ggplot(temp, aes(x = n_mods, y = rmse))+
    geom_jitter(width = 0.25, height = 0.0, alpha = 0.1)+
#     geom_ribbon(data = predict_df, aes(ymin=rmse-SE*sqrt(sum(temp$n_mods == 1)), 
#                                        ymax=rmse+SE*sqrt(sum(temp$n_mods == 1))), 
#                 fill = 'blue', alpha = 0.9)+
    geom_line(data = predict_df, color = 'white', linewidth = 1)+
    coord_cartesian(y = c(0.83, 1.3))+
    labs(x = 'Number of Models', y = 'RMSE')

sfig1_ens_mods

multi_plt <- sfig1_ens_mods+ggtitle(     paste0(LETTERS[1], '. ', 'All Models')
)+res_plt_list[[1]]+ggtitle(paste0(LETTERS[2], '. ', mod_names[1])
)+res_plt_list[[2]]+ggtitle(paste0(LETTERS[3], '. ', mod_names[2])
)+res_plt_list[[3]]+ggtitle(paste0(LETTERS[4], '. ', mod_names[3])
)+res_plt_list[[4]]+ggtitle(paste0(LETTERS[5], '. ', mod_names[4])
)+res_plt_list[[5]]+ggtitle(paste0(LETTERS[6], '. ', mod_names[5])
)+res_plt_list[[6]]+ggtitle(paste0(LETTERS[7], '. ', mod_names[6])
)+res_plt_list[[7]]+ggtitle(paste0(LETTERS[8], '. ', mod_names[7])
)+res_plt_list[[8]]+ggtitle(paste0(LETTERS[9], '. ', mod_names[8])
)+plot_layout(
    widths = c(2, 
               1, 1, 1, 1, 1, 1, 1, 1)
)#+ plot_annotation(tag_levels = 'A')

multi_plt

ggsave(multi_plt, filename = './ExponentialDecay.svg', width = 20, height = 10)
ggsave(multi_plt, filename = './ExponentialDecay.pdf', width = 20, height = 10)

# cowplot::plot_grid(plotlist = c(list(sfig1_ens_mods), res_plt_list), 
#                    nrow = 1, 
#                    labels = paste0(LETTERS[1:(length(mod_names))], '. ', mod_names))

# install.packages("svglite")









STOP HERE












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













# work with genetic algorithm output
PopHist <- read.csv('./genetic_algo_Population_History.csv')

PopHist %>% arrange(RMSE) %>% head(10)



# temp = select(PopHist, -X, -Iteration)
# # temp

# cowplot::plot_grid(plotlist = map(
#     c('ensemble', 'lm', 'bglr', 'knn', 'rf', 'rnr', 'svrl', 'full', 'cat'), 
#    function(e){
#        ggplot(temp, aes_string(x = e, y = 'RMSE'))+
#            geom_boxplot(aes_string(group = e))
#    })
#                    )





sum(choose(80, 1:80))














