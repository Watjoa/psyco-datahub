run_item_plot <- function(item_name,
                          out_dir = "plots_longitudinal",
                          width = 8,
                          height = 5) {
  
  message("Running item: ", item_name)
  
   cols <- item_name
  col_labels <- cols
  items <- cols

  make_wave_long <- function(df, wave_id, cols) {
    cols_here <- intersect(cols, names(df))
    if (length(cols_here) == 0) return(NULL)
    
    df %>%
      dplyr::select(email, weights, all_of(cols_here)) %>%   # <-- include weights
      mutate(wave = wave_id) %>%
      tidyr::pivot_longer(
        cols = all_of(cols_here),
        names_to = "item",
        values_to = "score"
      )
  }
  
  df_long <- bind_rows(
    make_wave_long(df2020begin,     1, cols),
    make_wave_long(df2020wave2,     2, cols),
    make_wave_long(df2020long,      3, cols),
    make_wave_long(dfwave3,         4, cols),
    make_wave_long(df2021long,      5, cols),
    make_wave_long(df2022postcovid, 6, cols),
    make_wave_long(df2022long,      7, cols),
    make_wave_long(df2024long,      8, cols),
    make_wave_long(df2025long,      9, cols)
  ) %>%
    filter(!is.na(score)) %>%                      # drop NAs per-item (not whole rows)
    mutate(item = factor(item, levels = cols))
  
  table(df_long$wave,df_long$item)
  
  maxwaves <- dim(df_long %>% dplyr::count(wave, name = "n_values"))[1]
  aantalwaves <- maxwaves-(maxwaves/2)
  
  emails_longitudinal <- df_long %>%
    dplyr::distinct(email, wave) %>%
    dplyr::count(email) %>%
    filter(n >= aantalwaves) %>%          # your "at least 6/9 waves" criterion (exactly 6 here)
    pull(email)
  
  df_long <- df_long %>%
    filter(email %in% emails_longitudinal)
  
  n_longitudinal <- n_distinct(df_long$email)
  n_longitudinal
  
  
  # ADD WEIGHTS
  
  dfsub <- na.omit(dfmot[,c("email","province")])
  df_long$province <- dfsub$province[match(df_long$email,dfsub$email)]
  df_long$province <- as.factor(df_long$province)
  #freq(df_long$province)
  #print(paste('the number of missing provinces is',length(unique(df_long[is.na(df_long$province),'email']))))
  #levels(df_long$province)
  levels(df_long$province) <- c('West-Vlaanderen',
                                'Oost-Vlaanderen',
                                'Antwerpen',
                                'Limburg',
                                'Vlaams-Brabant',
                                'Andere',
                                'Henegouwen',
                                'Namen',
                                'Luik',
                                'Luxemburg',
                                'Waals-Brabant',
                                'Brussels Hoofdstedelijk Gewest')
  df_long$region <- df_long$province
  levels(df_long$region) <- c('Flanders','Flanders','Flanders','Flanders','Flanders',NA,
                              'Wallonia','Wallonia','Wallonia','Wallonia','Wallonia',
                              'Brussels')
  df_long$region_w <- plyr::revalue(df_long$region, c("Flanders"="1","Wallonia"="2","Brussels"="3"))
  
  dfsub <- na.omit(dfmot[,c("email","gender")])
  df_long$gender <- dfsub$gender[match(df_long$email,dfsub$email)]
  df_long$gender <- as.factor(df_long$gender)
  #print(paste('the number of missing gender is',length(unique(df_long[is.na(df_long$gender),'email']))))
  
  #freq(df_long$gender)
  #levels(df_long$gender)
  levels(df_long$gender) <- c('Male','Female')
  df_long$gender_w <- as.factor(df_long$gender)
  levels(df_long$gender_w) <- c('Male','Female')
  df_long$gender_w <- plyr::revalue(df_long$gender_w, c("Female"="2", "Male"="1"))
  
  dfsub <- na.omit(dfmot[,c("email","age")])
  df_long$age <- dfsub$age[match(df_long$email,dfsub$email)]
  #freq(df_long$age)
  #print(paste('the number of missing age is',length(unique(df_long[is.na(df_long$age),'email']))))
  
  df_long$age_group <- cut(
    df_long$age,
    breaks = c(20, 30, 40, 50, 60, 70, Inf),
    right = FALSE,
    labels = c(
      "20–29",
      "30–39",
      "40–49",
      "50–59",
      "60–69",
      "70+"
    )
  )
  library(questionr)
  #freq(df_long$age_group)
  df_long$age_w <- plyr::revalue(df_long$age_group, c("20–29" = "1",
                                                      "30–39" = "2",
                                                      "40–49" = "3",
                                                      "50–59" = "4",
                                                      "60–69" = "5",
                                                      "70+" = "6"))
  df_long$waves <- as.factor(df_long$wave)
  
  
  
  # Make new dataframe
  weighting <- df_long
  
  # Summarize characteristics by waves
  weights <- weighting %>%
    dplyr::group_by(waves,gender_w,age_w,region_w) %>% 
    dplyr::summarise(n=n())
  weights <- na.omit(weights)
  
  weights <- weights %>% group_by(waves)  %>% mutate(Proportion=n/sum(n))
  
  weights$combo <- paste(weights$gender_w, weights$age_w,weights$region_w)
  
  weights$weights <- as.numeric('1')
  
  ### MALE - AGE GROUPS - FLANDERS
  weights[which(weights$combo == "1 1 1"),'weights'] <- 0.0431/weights[which(weights$combo == "1 1 1"),'Proportion']
  weights[which(weights$combo == "1 2 1"),'weights'] <- 0.0472/weights[which(weights$combo == "1 2 1"),'Proportion']
  weights[which(weights$combo == "1 3 1"),'weights'] <- 0.0474/weights[which(weights$combo == "1 3 1"),'Proportion']
  weights[which(weights$combo == "1 4 1"),'weights'] <- 0.0494/weights[which(weights$combo == "1 4 1"),'Proportion']
  weights[which(weights$combo == "1 5 1"),'weights'] <- 0.0478/weights[which(weights$combo == "1 5 1"),'Proportion']
  weights[which(weights$combo == "1 6 1"),'weights'] <- 0.0518/weights[which(weights$combo == "1 6 1"),'Proportion']
  
  ### FEMALE - AGE GROUPS - FLANDERS
  weights[which(weights$combo == "2 1 1"),'weights'] <- 0.0417/weights[which(weights$combo == "2 1 1"),'Proportion']
  weights[which(weights$combo == "2 2 1"),'weights'] <- 0.0473/weights[which(weights$combo == "2 2 1"),'Proportion']
  weights[which(weights$combo == "2 3 1"),'weights'] <- 0.0474/weights[which(weights$combo == "2 3 1"),'Proportion']
  weights[which(weights$combo == "2 4 1"),'weights'] <- 0.0484/weights[which(weights$combo == "2 4 1"),'Proportion']
  weights[which(weights$combo == "2 5 1"),'weights'] <- 0.0483/weights[which(weights$combo == "2 5 1"),'Proportion']
  weights[which(weights$combo == "2 6 1"),'weights'] <- 0.0647/weights[which(weights$combo == "2 6 1"),'Proportion']
  
  ### MALE - AGE GROUPS - WALLONIA
  weights[which(weights$combo == "1 1 2"),'weights'] <- 0.0245/weights[which(weights$combo == "1 1 2"),'Proportion']
  weights[which(weights$combo == "1 2 2"),'weights'] <- 0.0258/weights[which(weights$combo == "1 2 2"),'Proportion']
  weights[which(weights$combo == "1 3 2"),'weights'] <- 0.0253/weights[which(weights$combo == "1 3 2"),'Proportion']
  weights[which(weights$combo == "1 4 2"),'weights'] <- 0.0268/weights[which(weights$combo == "1 4 2"),'Proportion']
  weights[which(weights$combo == "1 5 2"),'weights'] <- 0.0242/weights[which(weights$combo == "1 5 2"),'Proportion']
  weights[which(weights$combo == "1 6 2"),'weights'] <- 0.0242/weights[which(weights$combo == "1 6 2"),'Proportion']
  
  ### FEMALE - AGE GROUPS - WALLONIA
  weights[which(weights$combo == "2 1 2"),'weights'] <- 0.0236/weights[which(weights$combo == "2 1 2"),'Proportion']
  weights[which(weights$combo == "2 2 2"),'weights'] <- 0.0258/weights[which(weights$combo == "2 2 2"),'Proportion']
  weights[which(weights$combo == "2 3 2"),'weights'] <- 0.0256/weights[which(weights$combo == "2 3 2"),'Proportion']
  weights[which(weights$combo == "2 4 2"),'weights'] <- 0.0269/weights[which(weights$combo == "2 4 2"),'Proportion']
  weights[which(weights$combo == "2 5 2"),'weights'] <- 0.0259/weights[which(weights$combo == "2 5 2"),'Proportion']
  weights[which(weights$combo == "2 6 2"),'weights'] <- 0.0332/weights[which(weights$combo == "2 6 2"),'Proportion']
  
  ### MALE - AGE GROUPS - BRUSSELS
  weights[which(weights$combo == "1 1 3"),'weights'] <- 0.0102/weights[which(weights$combo == "1 1 3"),'Proportion']
  weights[which(weights$combo == "1 2 3"),'weights'] <- 0.0110/weights[which(weights$combo == "1 2 3"),'Proportion']
  weights[which(weights$combo == "1 3 3"),'weights'] <- 0.0097/weights[which(weights$combo == "1 3 3"),'Proportion']
  weights[which(weights$combo == "1 4 3"),'weights'] <- 0.0085/weights[which(weights$combo == "1 4 3"),'Proportion']
  weights[which(weights$combo == "1 5 3"),'weights'] <- 0.0057/weights[which(weights$combo == "1 5 3"),'Proportion']
  weights[which(weights$combo == "1 6 3"),'weights'] <- 0.0051/weights[which(weights$combo == "1 6 3"),'Proportion']
  
  ### FEMALE - AGE GROUPS - BRUSSELS
  weights[which(weights$combo == "2 1 3"),'weights'] <- 0.0110/weights[which(weights$combo == "2 1 3"),'Proportion']
  weights[which(weights$combo == "2 2 3"),'weights'] <- 0.0112/weights[which(weights$combo == "2 2 3"),'Proportion']
  weights[which(weights$combo == "2 3 3"),'weights'] <- 0.0095/weights[which(weights$combo == "2 3 3"),'Proportion']
  weights[which(weights$combo == "2 4 3"),'weights'] <- 0.0080/weights[which(weights$combo == "2 4 3"),'Proportion']
  weights[which(weights$combo == "2 5 3"),'weights'] <- 0.0061/weights[which(weights$combo == "2 5 3"),'Proportion']
  weights[which(weights$combo == "2 6 3"),'weights'] <- 0.0077/weights[which(weights$combo == "2 6 3"),'Proportion']
  
  weights$combo3 <- as.factor(paste(weights$waves,weights$age_w, weights$gender_w,weights$region_w))
  
  df_long$combo_y2 <- as.factor(paste(df_long$wave, df_long$age_w, df_long$gender_w,df_long$region_w))
  
  df_long <- as.data.frame(df_long)
  df_long[,'weights'] <- weights$weights[match(df_long[,'combo_y2'], weights$combo3)]
  
  dfuniquek <- na.omit(unique(df_long[,c('email','age_group','gender','region','wave')]))
  minaantal <- 40
  tableregion <- as.data.frame(table(dfuniquek$wave,dfuniquek$region))
  colnames(tableregion) <- c('wave','region','Freq')
  tableregion$combo <- paste0(tableregion$wave,tableregion$region)
  removedregion <- tableregion[which(tableregion$Freq<minaantal),'combo']
  tableregion[which(tableregion$Freq<minaantal),'Freq'] <- 'NOWP'
  df_long$combo <- paste0(df_long$wave,df_long$region)
  df_long$Freq <- tableregion$Freq[match(df_long$combo,tableregion$combo)]
  df_long <- df_long[!(df_long$Freq %in% "NOWP"), ]
  
  tableage_group <- as.data.frame(table(dfuniquek$wave,dfuniquek$age_group))
  colnames(tableage_group) <- c('wave','age_group','Freq')
  tableage_group$combo <- paste(tableage_group$wave,tableage_group$age_group,sep="-")
  removedage <- tableage_group[which(tableage_group$Freq<minaantal),'combo']
  
  tableage_group[which(tableage_group$Freq<minaantal),'Freq'] <- 'NOWP'
  df_long$combo <- paste(df_long$wave,df_long$age_group,sep="-")
  df_long$Freq <- tableage_group$Freq[match(df_long$combo,tableage_group$combo)]
  df_long <- df_long[!(df_long$Freq %in% "NOWP"), ]
  
  tablegender <- as.data.frame(table(dfuniquek$wave,dfuniquek$gender))
  colnames(tablegender) <- c('wave','gender','Freq')
  tablegender$combo <- paste(tablegender$wave,tablegender$gender,sep="-")
  removedgender <- tablegender[which(tablegender$Freq<minaantal),'combo']
  tablegender[which(tablegender$Freq<minaantal),'Freq'] <- 'NOWP'
  df_long$combo <- paste(df_long$wave,df_long$gender,sep="-")
  df_long$Freq <- tablegender$Freq[match(df_long$combo,tablegender$combo)]
  df_long <- df_long[!(df_long$Freq %in% "NOWP"), ]
  
  dfsub <- df_long
  dfsub$group <- dfsub$waves
  dfsub <- dfsub[!is.na(dfsub$age_group),]
  dfsub <- dfsub[!is.na(dfsub$gender),]
  dfsub <- dfsub[!is.na(dfsub$region),]
  dfsub <- dfsub[!is.na(dfsub$group),]
  dfsub <- droplevels(dfsub)
  dfsub <- as.data.frame(dfsub)
  dim(dfsub)
  
  variableslist <- list(
    fig0 = c('age_group','gender','region'))
  names <- list(
    fig0 = c('Age group','Gender','Region'))
  
  pformat <- function(p.value){
    unclass(symnum(p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                   symbols = c("***", "**", "*", " ")))
  }
  
  plot_list = list()
  plot_list3 = list()
  sociodemo <- c()
  test <- c()
  testlabels <- c()
  
  for(j in 1:length(variableslist)) {
    i <- 1
    variables <- variableslist[[j]]
    
    sociodemo <- vector(mode = "list", length = length(variables)) # lengte aanpassen
    test <- vector(mode = "list", length = length(variables))
    
    for(i in 1:length(variables)) { ## tabellen worden gemaakt
      sociodemo[[i]] <- table(dfsub$group,dfsub[,variables[i]],exclude=NA)
    }
    
    
    for(i in 1:length(variables)) { ## chi squares worden berekend
      test[[i]] <- paste('χ2(',chisq.test(sociodemo[[i]])$parameter,') = ',
                         round(chisq.test(sociodemo[[i]])$statistic,2),', p = ',
                         round(chisq.test(sociodemo[[i]])$p.value,2),
                         pformat(chisq.test(sociodemo[[i]])$p.value),
                         sep="")
    }
    
    for(i in 1:length(variables)) { ## tabellen in percentages
      sociodemo[[i]] <- as.data.frame(round(prop.table(sociodemo[[i]],1)*100,2))
    }
    
    testlabels <- vector(mode = "list", length = length(variables))
    for(i in 1:length(variables)) { ## p waarde 0 wordt vervangen door <.001
      if(as.numeric(gsub("\\*", "", gsub(".*p =","",test[[i]]))) <.001){
        testlabels[[i]] <- gsub("p = 0","p = <.001",test[[i]])
      } else {
        testlabels[[i]] <- test[[i]]
      }
    }
    testlabels
    
    
    names(sociodemo)<- names[[j]]
    
    g<-list() # aanmaken van figuren
    a <- sociodemo
    for(i in 1:length(a)) {
      p = ggplot(reshape2::melt(a[[i]]), aes(as.factor(Var1), value, 
                                             fill=as.factor(Var2), 
                                             label=paste(round(value, 0),sep="") ))+
        geom_col()+geom_text(position=position_stack(vjust=0.5))+
        coord_flip()+scale_fill_brewer(type="seq", palette=1)+
        labs(x="", y="", title=names(a)[i], fill = "Categorieën: ",
             subtitle=testlabels[i]
        )+
        theme_hc(base_size = 10)+
        theme(legend.position='right')
      g[[i]] = p
      
    }
    
    for(i in 1:length(variables)) { ## er wordt gecheckt welke significant is of niet
      if(as.numeric(gsub("\\*", "", gsub(".*p =","",test[[i]]))) >.05){
        g[[i]] <- g[[i]] + scale_fill_brewer(type="seq",palette=6)
      }  
    }
    
    # figuur ----
    
    plot_list[[j]] <- ggarrange(plotlist=g, align = "hv",# totale plot
                                common.legend=FALSE,
                                ncol = 1, nrow = 3 # aantal rijen en kolommen
    )
  }
  
  
  mean_df <- df_long %>%
    dplyr::group_by(wave, item) %>%
    dplyr::summarise(
      sum_w = sum(weights[!is.na(score) & !is.na(weights)], na.rm = TRUE),
      mean_w = ifelse(sum_w > 0,
                      sum(score * weights, na.rm = TRUE) / sum_w,
                      NA_real_),
      n = sum(!is.na(score)),
      .groups = "drop"
    )%>%as.data.frame()
  
  items_with_lines <- mean_df %>%
    dplyr::count(item) %>%
    dplyr::filter(n >= 2) %>%   # at least two waves → line possible
    dplyr::pull(item)
  
  mean_df_lines <- mean_df %>%
    filter(item %in% items_with_lines)
  
  mean_df$item <- as.factor(mean_df$item)
  mean_df <- mean_df %>%
    mutate(item_label = col_labels[item])
  mean_df[which(mean_df$n<100),] <- NA
  
  mean_df_lines$item <- as.factor(mean_df_lines$item)
  mean_df_lines <- mean_df_lines %>%
    mutate(item_label = col_labels[item])
  
  mean_df_lines[which(mean_df_lines$n<100),] <- NA
  mean_df$wave <- as.factor(mean_df$wave)
  mean_df_lines$wave <- as.factor(mean_df_lines$wave)
  
  mean_df <- mean_df %>%
    mutate(wave = as.integer(as.character(wave))) %>%   # fix type
    complete(
      wave = 1:9,
      item,
      fill = list(
        sum_w = NA_real_,
        mean_w = NA_real_,
        n = NA_integer_,
        item_label = NA_character_
      )
    )
  
  mean_df_lines <- mean_df_lines %>%
    mutate(wave = as.integer(as.character(wave))) %>%   # fix type
    complete(
      wave = 1:9,
      item,
      fill = list(
        sum_w = NA_real_,
        mean_w = NA_real_,
        n = NA_integer_,
        item_label = NA_character_
      )
    )
  
  mean_df$wave <- as.numeric(mean_df$wave)
  mean_df_lines$wave <- as.numeric(mean_df_lines$wave)
  mean_df <- na.omit(mean_df)
  mean_df_lines <- na.omit(mean_df_lines)
  mean_df <- as.data.frame(mean_df)
  mean_df_lines <- as.data.frame(mean_df_lines)
  
  p <- ggplot(mean_df,
              aes(
                x = wave,
                y = mean_w,
                color = item,
                shape = item,
                group = item,
                text = paste0(
                  "Item: ", item_label,
                  "<br>Code: ", item,
                  "<br>Wave: ", wave,
                  "<br>Weighted mean: ", round(mean_w, 2),
                  "<br>N: ", n
                )
              )) +
    geom_line(
      data = mean_df_lines,
      linewidth = 0.8
    ) +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = 1:9,
      labels = c(
        'COVID<br>wave I','COVID<br>wave II','End of<br>2020',
        'COVID<br>wave III','End of<br>2021','Post-COVID',
        'End of<br>2022','End of<br>2024','End of<br>2025'
      )
    ) +
    scale_y_continuous(limits = c(1, 5), expand = expansion(mult = c(.02, .05))) +
    labs(
      title = paste("Longitudinal change –", item_name),
      caption = paste0("Weighted; total longitudinal N = ", n_longitudinal),
      x = NULL,
      y = "Mean score"
    ) +
    theme_minimal()
  
  ## =========================
  ## SAVE
  ## =========================
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  ggsave(
    filename = file.path(out_dir, paste0(item_name, ".png")),
    plot = p,
    width = width,
    height = height,
    dpi = 300
  )
  
  return(p)
}


cols_all <- c(
    
  "post_crisis1",
  "post_crisis2",
  "post_crisis3",
  "post_crisis4",
  "post_crisis5",
  "post_crisis6",
  "post_crisis7",
  "post_crisis8"
  )


plots <- lapply(cols_all, run_item_plot)
names(plots) <- cols_all



dfsub1 <- na.omit(dfmot[which(dfmot$months<'2020-06-01'),c('email','ER_1')])
dfsub2 <- na.omit(dfmot[which(dfmot$months=='2021-01-01'),c('email','ER_1')])

length(na.omit(match(dfsub1$email,dfsub2$email)))
