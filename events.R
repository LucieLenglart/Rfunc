rm( list = ls() )
graphics.off() 

library(rstudioapi) 
dirs<-rstudioapi::getSourceEditorContext()$path
setwd(dirname(dirs))


library(dplyr)
library(ggplot2)
library(reshape)
library(lme4)


raw =read.delim("Raw\event\file.csv",sep = ',',header=T)
name = "sub-FXXXXXX"

events= raw[,c(2,3,4,5,23,29,30,33,45,50)]
trig =  as.numeric(na.omit(events$trigger_time))
events$trigger_time = trig

events$Mask_2.started = events$Mask_2.started - events$trigger_time
events$trial.started = events$trial.started - events$trigger_time
events$key_resp.started = events$key_resp.started - events$trigger_time
events$text_break.started = events$text_break.started - events$trigger_time

events = cbind (type = 0, events)
events$space[events$space == "BOUND"] = "BOU"
events$type = paste("stim",events$space , events$own , events$response, sep = "")

#copy events with masks
insert.df = transform(events, type = "mask")
insert.df$response = "n"
insert.df$Midbreak = "n"

compevent = rbind(events, insert.df)

n = nrow(events)
compevent = compevent[kronecker(1:n, c(-1, n), "+"), ]


# Réinitialiser les indices
rownames(compevent) <- NULL
events = compevent


for (i in 1:nrow(events)) {
  # Si la valeur dans la colonne "response" est "y", ajouter une ligne supplémentaire
  if (events$response[i] == "y") {
    nouvelle_ligne = data.frame(events[i,])
    nouvelle_ligne$type = "resp"
    nouvelle_ligne$response = "n"
    # Utiliser rbind pour ajouter la nouvelle ligne sous la ligne actuelle
    events = rbind(events[1:i, , drop = FALSE], nouvelle_ligne, events[(i+1):nrow(events), , drop = FALSE])
  }
}
# Réinitialiser les indices
rownames(events) <- NULL


for (i in 1:nrow(events)) {
  # Si la valeur dans la colonne "response" est "y", ajouter une ligne supplémentaire
  if (events$Midbreak[i] == "y") {
    nouvelle_ligne = data.frame(events[(i+1),])
    nouvelle_ligne$type = "break"
    nouvelle_ligne$Midbreak  = "n"
    # Utiliser rbind pour ajouter la nouvelle ligne sous la ligne actuelle
    events = rbind(events[1:i, , drop = FALSE], nouvelle_ligne, events[(i+1):nrow(events), , drop = FALSE])
  }
}
# Réinitialiser les indices
rownames(events) <- NULL

events2 = events


####--- organisation
events$onset = 0
events$duration = 0

events$onset[events$type == "mask"] = events$Mask_2.started[events$type == "mask"]
events$duration[events$type == "mask"] = events$jitter[events$type == "mask"]

events$onset[events$type == "resp"] = events$key_resp.started[events$type == "resp"]
events$duration[events$type == "resp"] = "1.2"

events$onset[events$type == "stimPPSOn" | events$type == "stimPPSOy" | events$type == "stimEPSOn" | events$type == "stimEPSOy" | events$type == "stimPPSSn" | events$type == "stimPPSSy"
             | events$type == "stimEPSSn" | events$type == "stimEPSSy" | events$type == "stimBOUOn" | events$type == "stimBOUOy"| 
               events$type == "stimBOUSn" | events$type == "stimBOUSy"] = events$trial.started[events$type == "stimPPSOn" | events$type == "stimPPSOy" | events$type == "stimEPSOn" | events$type == "stimEPSOy" | events$type == "stimPPSSn" | events$type == "stimPPSSy"
                                                                                               | events$type == "stimEPSSn" | events$type == "stimEPSSy" | events$type == "stimBOUOn" | events$type == "stimBOUOy"| 
                                                                                                 events$type == "stimBOUSn" | events$type == "stimBOUSy"]
events$duration[events$type == "stimPPSOn" | events$type == "stimPPSOy" | events$type == "stimEPSOn" | events$type == "stimEPSOy" | events$type == "stimPPSSn" | events$type == "stimPPSSy"
                 | events$type == "stimEPSSn" | events$type == "stimEPSSy" | events$type == "stimBOUOn" | events$type == "stimBOUOy"| 
                   events$type == "stimBOUSn" | events$type == "stimBOUSy"] = "2"

events$onset[events$type == "break"] = events$text_break.started[events$type == "break"]
events$duration[events$type == "break"] = "25"


finalevent = events[,c(12,13,1)]
finalevent$onset = round(finalevent$onset, digits = 3)
colnames(finalevent)
colnames(finalevent) = c("onset", "duration", "trial_type")

finalevent = head(finalevent, -1)


filename = paste(name,"_task-tarefa_events.tsv", sep = "") 
write.table(finalevent, file= filename, quote=FALSE, sep='\t', row.names = F)

