################################################################################### # setup folders/files for Campus Labs new semester
# L:\mgt\FCQ\R_Code\campus_labs\CL_New_Sem_Setup.R - Vince Darcangelo, 12/19/22
# output: K:\IR\FCQ\Prod\22xx\AcademicTerm\term22xx_row1.csv" (import first)
# output: K:\IR\FCQ\Prod\22xx\AcademicTerm\term22xx.csv"
################################################################################### 
# set date and term
entrydt <- Sys.Date()
entrydt <- format(entrydt, format = "%Y%m%d")

# update vars
term_cd <- 2231

yr <- 2023

sem <- 
  "Spring"
# "Summer"
# "Fall"

bgdt <- 
  "-01-01T17:00:00-07:00" # spring
# "-05-01T17:00:00-07:00" # summer
# "-08-01T17:00:00-07:00" # fall

endt <- 
  "-05-31T17:00:00-07:00" # spring
# "-08-31T17:00:00-07:00" # summer
# "-12-31T17:00:00-07:00" # fall

###################################
# term22xx_row1.csv - import first!!!!!
aterm_row1 <- data.frame(
TermIdentifier = term_cd,
Name = paste(sem, yr, sep = " "),
BeginDate = paste0(yr,bgdt),
EndDate = paste0(yr,endt),
ParentIdentifier = "",
Type = "Semester"
)

write.csv(aterm_row1, paste0("K:\\IR\\FCQ\\Prod\\", term_cd, "\\AcademicTerm\\term", term_cd, "_row1.csv"), row.names = FALSE)

###################################
# term22xx.csv
aterm <- data.frame(
TermIdentifier = 
  c(paste0(term_cd, ":CUBLD:BLDR"),
    paste0(term_cd, ":CUBLD:BLD3"),
    paste0(term_cd, ":CUBLD:CEPS"),
    paste0(term_cd, ":CUDEN")),
Name = 
  c(paste(sem, yr, "CU Boulder main campus semester", sep = " "),
    paste(sem, yr, "CU Boulder distance semester", sep = " "),
    paste(sem, yr, "CU Boulder Continuing Education", sep = " "),
    paste(sem, yr, "CU Denver semester", sep = " ")),
BeginDate = paste0(yr,bgdt),
EndDate = paste0(yr,endt),
ParentIdentifier = term_cd,
Type = "Intersession"
)

write.csv(aterm, paste0("K:\\IR\\FCQ\\Prod\\", term_cd, "\\AcademicTerm\\term", term_cd, ".csv"), row.names = FALSE)
