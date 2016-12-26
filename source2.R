seg  <- seg.original

########## segment ændringer HUSK at den det højeste nr på listen skal være først og så fremdeles. Ellers ændres numrene nedad og du ødelægger andre klynger end dem du havde tænkt dig. 

#en række ændringer foretages smides ud af ledelsklyngen
seg$segment.list[[4]][[11]]  <-  c(5,21,4,18,7,19,23)
seg$segment.list[[3]][[33]] <-  c(7,19,23)
seg$segment.list[[4]][[14]] <- NULL
seg$segment.list[[4]][[8]] <- NULL
seg$segment.list[[4]][[6]] <- NULL

# kultur-delmarkedet disskeres så alle business-tingene ryger til hver sit sted, herrens arbejde vor herre bevares et besvær #hvorforbrugerjegtidpåsådannoget.com
seg$segment.list[[4]][[11]] <- NULL 

#fjernes fra business også 
seg$segment.list[[3]][[31]] <- NULL 

#split dekoration og arkitetarbejde op 
seg$segment.list[[2]][[73]] <- NULL 

#130 3471 Arbejde m. dekoration, design, illustration og indretning sættes sammen med kulturklyngen 
seg$segment.list[[3]][[37]] <- append(seg$segment.list[[2]][[6]],c(130)) # 78,54 %

#34: arkitet lægges sammen med 40: andet arktietarbejde 
seg$segment.list[[2]][[80]] <- c(34,40)
#34: arkitet lægges sammen med 40: andet arktietarbejde sammen med resten af klyngen som 40: andet arkitetarbejde tilhører 
seg$segment.list[[3]][[13]] <- append(seg$segment.list[[3]][[13]],c(34))
seg$segment.list[[4]][[1]] <- append(seg$segment.list[[4]][[1]],c(34)) #78,44 %

# sætter personpleje sammen med gruppe 2
seg$segment.list[[3]][[38]] <- append(seg$segment.list[[2]][[73]],c(159)) #78,38 %

seg$segment.list[[4]][[8]] <- append(seg$segment.list[[4]][[8]],c(194)) #78,55 %

seg$segment.list[[5]][[2]] <- append(seg$segment.list[[5]][[2]],c(214)) #78,70 %
seg$segment.list[[5]][[1]] <- append(seg$segment.list[[5]][[1]],c(217)) #79,22 %
seg$segment.list[[5]][[1]] <- append(seg$segment.list[[5]][[1]],c(223)) #79,65 %
seg$segment.list[[5]][[1]] <- append(seg$segment.list[[5]][[1]],c(260)) #80,57 %
# seg$segment.list[[3]][[3]] <- append(seg$segment.list[[3]][[3]],c(45)) # 80,33 %  læge 
# seg$segment.list[[3]][[18]] <- append(seg$segment.list[[3]][[18]],c(75)) # 80,00 %  præst 
seg$segment.list[[3]][[2]] <- append(seg$segment.list[[3]][[2]],c(93)) # 80,20 %  flypilot (med de to ovenstående: uden, 80,76 %)

# seg$segment.list[[4]][[7]] <- append(seg$segment.list[[4]][[7]],seg$segment.list[[2]][[24]]) # politiarbejde og brand, men fucker densiteten i 4.7 helt op 
# seg$segment.list[[4]][[8]] <- append(seg$segment.list[[4]][[8]],seg$segment.list[[2]][[40]]) #2.40 fucker densitet op
# seg$segment.list[[3]][[24]] <- append(seg$segment.list[[3]][[24]],seg$segment.list[[2]][[58]]) #fucker densitet op 






