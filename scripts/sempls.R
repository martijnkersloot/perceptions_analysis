library(semPLS)

structural_model <- rbind(
  c("compatibility", "attitude"), 
  c("awareness", "perceived_usefulness"), 
  c("experienced_usefulness", "perceived_usefulness"), 
  c("perceived_usefulness", "attitude"), 
  c("perceived_ease_of_use", "perceived_usefulness"), 
  c("perceived_ease_of_use", "attitude"), 
  c("attitude", "intention_to_act"), 
  c("interpersonal_influence", "subjective_norm"), 
  c("external_influence", "subjective_norm"), 
  c("subjective_norm", "intention_to_act"), 
  c("self_efficacy", "perceived_behavioural_control"), 
  c("facilitating_conditions", "perceived_behavioural_control"), 
  c("perceived_behavioural_control", "intention_to_act"), 
  c("situational_normality", "institutional_trust"), 
  c("structural_assurance", "institutional_trust"), 
  c("institutional_trust", "intention_to_act"), 
  c("institutional_trust", "perceived_risk"), 
  c("perceived_risk", "intention_to_act"), 
  c("intention_to_act", "behavior")
)

colnames(structural_model) <- c("source", "target")

measurement_model <- rbind(
  c("compatibility", "compatibility_1"), 
  c("awareness", "awareness_1"), 
  c("awareness", "awareness_2"), 
  c("awareness", "awareness_3"), 
  c("awareness", "awareness_4"), 
  c("perceived_usefulness", "perceivedusefulness_1"), 
  c("perceived_usefulness", "perceivedusefulness_2"), 
  c("perceived_usefulness", "perceivedusefulness_3"), 
  c("perceived_usefulness", "perceivedusefulness_4"), 
  c("perceived_usefulness", "perceivedusefulness_5"), 
  c("perceived_usefulness", "perceivedusefulness_6"), 
  c("experienced_usefulness", "experiencedusefulness_1"), 
  c("experienced_usefulness", "experiencedusefulness_2"), 
  c("experienced_usefulness", "experiencedusefulness_3"), 
  c("experienced_usefulness", "experiencedusefulness_4"), 
  c("experienced_usefulness", "experiencedusefulness_5"), 
  c("experienced_usefulness", "experiencedusefulness_6"), 
  c("perceived_ease_of_use", "perceivedeaseofuse_1"), 
  c("attitude", "attitude_1"), 
  c("attitude", "attitude_2"), 
  c("attitude", "attitude_3"), 
  c("attitude", "attitude_4"), 
  c("interpersonal_influence", "interpersonalinfluence_1"), 
  c("interpersonal_influence", "interpersonalinfluence_2"), 
  c("interpersonal_influence", "interpersonalinfluence_3"), 
  c("interpersonal_influence", "interpersonalinfluence_4"), 
  c("external_influence", "externalinfluence_1"), 
  c("external_influence", "externalinfluence_2"), 
  c("subjective_norm", "subjectivenorm_1"), 
  c("self_efficacy", "selfefficacy_1"), 
  c("self_efficacy", "selfefficacy_2"), 
  c("self_efficacy", "selfefficacy_3"), 
  c("self_efficacy", "selfefficacy_4"), 
  c("self_efficacy", "selfefficacy_5"), 
  c("self_efficacy", "selfefficacy_6"), 
  c("self_efficacy", "selfefficacy_7"), 
  c("self_efficacy", "selfefficacy_8"), 
  c("self_efficacy", "selfefficacy_9"), 
  c("self_efficacy", "selfefficacy_10"), 
  c("facilitating_conditions", "facilitatingconditions_1"), 
  c("facilitating_conditions", "facilitatingconditions_2"), 
  c("facilitating_conditions", "facilitatingconditions_3"), 
  c("facilitating_conditions", "facilitatingconditions_4"), 
  c("perceived_behavioural_control", "perceivedbehavioralcontrol_1"), 
  c("situational_normality", "situationalnormality_1"), 
  c("structural_assurance", "structuralassurance_1"), 
  c("structural_assurance", "structuralassurance_2"), 
  c("structural_assurance", "structuralassurance_3"), 
  c("structural_assurance", "structuralassurance_4"), 
  c("perceived_risk", "perceivedrisk_1"), 
  c("perceived_risk", "perceivedrisk_2"), 
  c("intention_to_act", "intentiontoact_1"), 
  c("intention_to_act", "intentiontoact_2"), 
  c("intention_to_act", "intentiontoact_3"), 
  c("intention_to_act", "intentiontoact_4"), 
  c("behavior", "behavior_1"), 
  c("behavior", "behavior_2"), 
  c("behavior", "behavior_3"), 
  c("behavior", "behavior_4")
)

colnames(measurement_model) <- c("source", "target")

ECSI <- plsm(data = completed_survey_data, strucmod = structural_model, measuremod = measurement_model)




latent <- unique(as.vector(structural_model))

manifest <- sort(setdiff(as.vector(measurement_model), latent))

check <- manifest %in% colnames(completed_survey_data)

blocks <- block2(latent, manifest, measurement_model)

MVs <- NULL
mm <- NULL
for(i in names(blocks)){
  MVs <- append(MVs, blocks[[i]])
  
  if(attr(blocks[[i]], "mode") == "A"){
    
    print(cbind(i, blocks[[i]]))
    
    mm <- rbind(mm, (cbind(i, blocks[[i]])))
  }
  if(attr(blocks[[i]], "mode") == "B"){
    mm <- rbind(mm, (cbind(blocks[[i]], i)))
  }
}


block2 <-
  function(latent, manifest, measuremod){
    ln <- length(latent)        # number of latent variables
    colnames(measuremod) <- NULL
    blocks <- list()
    
    for (i in 1:ln){
      blocks[[i]] <- measuremod[c(which(measuremod[,1]==latent[i], which(measuremod[,2]==latent[i]))),]
      blocks[[i]] <- append(blocks[[i]],
                            measuremod[c(which(measuremod[,2]==latent[i], which(measuremod[,1]==latent[i]))),])
      blocks[[i]] <- sort(blocks[[i]][blocks[[i]] %in% manifest])
      
      # determine the mode ("A"=reflective, "B"=formative)
      if (all(blocks[[i]] %in% measuremod[,2])){
        attr(blocks[[i]], "mode") <- "A"
      }
      else if (all(blocks[[i]] %in% measuremod[,1])) {
        attr(blocks[[i]], "mode") <- "B"
      }
      else stop("A block must be either formative or reflective, not both!")
    }
    names(blocks) <- latent
    
    return(blocks)
  }
