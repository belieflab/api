
rm(list=ls())

if(!require(Hmisc)) {install.packages("Hmisc")}; library(Hmisc)
if(!require(effsize)) {install.packages("effsize")}; library(effsize)
if(!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr)

# wizardry
source("api/dataRequest.R")
source("api/dataMerge.R")
source("api/dataFilter.R")

# get tasks
dataRequest("liwc")
dataRequest("social_prl")

# get qualtrics
# dataRequest("lshsr", "rgpts", "pss", "pdi40", "bdi", "bai", "social_network")
# dataRequest("lshsr", "rgpts", "pdi40", "bdi", "bai", "social_network")
dataRequest("lshsr")


lshsr_compact <- dataFilter(lshsr_clean, columns_of_interest = c("src_subject_id", "visit", "arm", "lshsr_total"), states = "complete")
lshsr_compact$visit <- as.factor(lshsr_compact$visit)
View(lshsr_compact)

pre_lshsr <- lshsr_compact[lshsr_compact$visit == 2, ]
pre_lshsr_prefix <- pre_lshsr %>%
  dplyr::rename_with(~ paste0("pre_", .), -src_subject_id & -visit & -arm) %>%
  dplyr::select(-visit)  # Remove the visit column
# View(pre_lshsr_prefix)

post_lshsr <- lshsr_compact[lshsr_compact$visit == 7, ]
post_lshsr_prefix <- post_lshsr %>%
  dplyr::rename_with(~ paste0("post_", .), -src_subject_id & -visit & -arm) %>%
  dplyr::select(-visit)  # Remove the visit column
# View(post_lshsr_prefix)

lshsr_wide <- dataMerge(pre_lshsr_prefix, post_lshsr_prefix)
View(lshsr_wide)

lshsStats= lshsr_compact %>%
  
  dplyr::group_by(visit) %>%
  
  dplyr::summarise(lshsr_mean=mean(lshsr_total, na.rm = TRUE),
                   
                   lshsr_se=((sd(lshsr_total, na.rm = TRUE))/sqrt(70))) # SING13085 no visit 7



lshsScore=ggplot(data=lshsStats, aes(x=visit, y=lshsr_mean, fill=visit)) +
  
  geom_bar(stat="identity",
           
           # position = position_dodge(),
           
           width=0.5) +
  
  geom_errorbar(aes(x=visit, ymin=lshsr_mean-lshsr_se,
                    
                    ymax=lshsr_mean+lshsr_se),
                
                width=0.2, colour="black", alpha=0.9, size=1, position=position_dodge(0.5)) +
  
  geom_point(data=lshsr_compact, aes(x=visit, y=lshsr_total),
             
             alpha=0.3, position=position_jitter()) +
  
  labs(x="Music", y="Hallucinations") +
  
  scale_x_discrete(labels=c('pre', 'post')) +
  
  scale_fill_manual(name="", values=c("#32CD32", "#9370DB")) #+
lshsScore

# theme_Publication()
##########LSHSR SING ONLY
lshsr_sing_only <- lshsr_compact %>% dplyr::filter(arm=="sing") 
View(lshsr_sing_only)

lshsStats= lshsr_sing_only %>%
  
  dplyr::group_by(visit) %>%
  
  dplyr::summarise(lshsr_mean=mean(lshsr_total, na.rm = TRUE),
                   
                   lshsr_se=((sd(lshsr_total, na.rm = TRUE))/sqrt(35)))

lshsScore = ggplot(data = lshsStats, aes(x = visit, y = lshsr_mean, fill = visit)) +
  
  geom_bar(stat = "identity", width = 0.5) +
  
  geom_errorbar(aes(x = visit, ymin = lshsr_mean - lshsr_se, ymax = lshsr_mean + lshsr_se),
                
                width = 0.2, colour = "black", alpha = 0.9, size = 1, position = position_dodge(0.5)) +
  
  geom_point(data = lshsr_sing_only, aes(x = visit, y = lshsr_total),
             
             alpha = 0.3, position = position_jitter()) +
  
  labs(x = "SING", y = "Hallucinations") +
  
  scale_x_discrete(labels = c('pre', 'post')) +
  
  scale_fill_manual(name = "", values = c("#32CD32", "#9370DB")) +
  
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        
        axis.title.y = element_text(size = 20, face = "bold"),
        
        axis.text.x = element_text(size = 16),
        
        axis.text.y = element_text(size = 16),
        
        legend.position = "none")
lshsScore

##########LSHSR KARAOKE ONLY
lshsr_karaoke_only <- lshsr_compact %>% dplyr::filter(arm=="karaoke") 
View(lshsr_karaoke_only)
length(unique(lshsr_karaoke_only$src_subject_id)) # 14

lshsStats= lshsr_karaoke_only %>%
  
  dplyr::group_by(visit) %>%
  
  dplyr::summarise(lshsr_mean=mean(lshsr_total, na.rm = TRUE),
                   
                   lshsr_se=((sd(lshsr_total, na.rm = TRUE))/sqrt(14)))

lshsScore = ggplot(data = lshsStats, aes(x = visit, y = lshsr_mean, fill = visit)) +
  
  geom_bar(stat = "identity", width = 0.5) +
  
  geom_errorbar(aes(x = visit, ymin = lshsr_mean - lshsr_se, ymax = lshsr_mean + lshsr_se),
                
                width = 0.2, colour = "black", alpha = 0.9, size = 1, position = position_dodge(0.5)) +
  
  geom_point(data = lshsr_karaoke_only, aes(x = visit, y = lshsr_total),
             
             alpha = 0.3, position = position_jitter()) +
  
  labs(x = "Karaoke", y = "Hallucinations") +
  
  scale_x_discrete(labels = c('pre', 'post')) +
  
  scale_fill_manual(name = "", values = c("#32CD32", "#9370DB")) +
  
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        
        axis.title.y = element_text(size = 20, face = "bold"),
        
        axis.text.x = element_text(size = 16),
        
        axis.text.y = element_text(size = 16),
        
        legend.position = "none")
lshsScore

##########LSHSR JUKEBOX ONLY
lshsr_jukebox_only <- lshsr_compact %>% dplyr::filter(arm=="jukebox") 
View(lshsr_jukebox_only)
length(unique(lshsr_jukebox_only$src_subject_id)) # 8

lshsStats= lshsr_jukebox_only %>%
  
  dplyr::group_by(visit) %>%
  
  dplyr::summarise(lshsr_mean=mean(lshsr_total, na.rm = TRUE),
                   
                   lshsr_se=((sd(lshsr_total, na.rm = TRUE))/sqrt(8)))

lshsScore = ggplot(data = lshsStats, aes(x = visit, y = lshsr_mean, fill = visit)) +
  
  geom_bar(stat = "identity", width = 0.5) +
  
  geom_errorbar(aes(x = visit, ymin = lshsr_mean - lshsr_se, ymax = lshsr_mean + lshsr_se),
                
                width = 0.2, colour = "black", alpha = 0.9, size = 1, position = position_dodge(0.5)) +
  
  geom_point(data = lshsr_jukebox_only, aes(x = visit, y = lshsr_total),
             
             alpha = 0.3, position = position_jitter()) +
  
  labs(x = "Jukebox", y = "Hallucinations") +
  
  scale_x_discrete(labels = c('pre', 'post')) +
  
  scale_fill_manual(name = "", values = c("#32CD32", "#9370DB")) +
  
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        
        axis.title.y = element_text(size = 20, face = "bold"),
        
        axis.text.x = element_text(size = 16),
        
        axis.text.y = element_text(size = 16),
        
        legend.position = "none")
lshsScore

##########LSHSR PLAYLIST ONLY
lshsr_playlist_only <- lshsr_compact %>% dplyr::filter(arm=="playlist") 
View(lshsr_playlist_only)
length(unique(lshsr_playlist_only$src_subject_id))

lshsStats= lshsr_playlist_only %>%
  
  dplyr::group_by(visit) %>%
  
  dplyr::summarise(lshsr_mean=mean(lshsr_total, na.rm = TRUE),
                   
                   lshsr_se=((sd(lshsr_total, na.rm = TRUE))/sqrt(14)))

lshsScore = ggplot(data = lshsStats, aes(x = visit, y = lshsr_mean, fill = visit)) +
  
  geom_bar(stat = "identity", width = 0.5) +
  
  geom_errorbar(aes(x = visit, ymin = lshsr_mean - lshsr_se, ymax = lshsr_mean + lshsr_se),
                
                width = 0.2, colour = "black", alpha = 0.9, size = 1, position = position_dodge(0.5)) +
  
  geom_point(data = lshsr_playlist_only, aes(x = visit, y = lshsr_total),
             
             alpha = 0.3, position = position_jitter()) +
  
  labs(x = "Playlist", y = "Hallucinations") +
  
  scale_x_discrete(labels = c('pre', 'post')) +
  
  scale_fill_manual(name = "", values = c("#32CD32", "#9370DB")) +
  
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        
        axis.title.y = element_text(size = 20, face = "bold"),
        
        axis.text.x = element_text(size = 16),
        
        axis.text.y = element_text(size = 16),
        
        legend.position = "none")
lshsScore


############ RGPTS


dataRequest("rgpts")
View(rgpts_clean)
rgpts_compact <- dataFilter(rgpts_clean, columns_of_interest = c("src_subject_id", "visit", "arm", "rgpts_total"), states = "complete")
rgpts_compact$visit <- as.factor(rgpts_compact$visit)
View(rgpts_compact)


pre_rgpts <- rgpts_compact[rgpts_compact$visit == 2, ]

pre_rgpts_prefix <- pre_rgpts %>%
  dplyr::rename_with(~ paste0("pre_", .), -src_subject_id & -visit & -arm) %>%
  dplyr::select(-visit)  # Remove the visit column
View(pre_rgpts_prefix)

post_rgpts <- rgpts_compact[rgpts_compact$visit == 7, ]
post_rgpts_postfix <- post_rgpts %>%
  dplyr::rename_with(~ paste0("post_", .), -src_subject_id & -visit & -arm) %>%
  dplyr::select(-visit)  # Remove the visit column
View(post_rgpts_postfix)


