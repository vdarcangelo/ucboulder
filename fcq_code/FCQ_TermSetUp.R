#########################################################################
# Set up new term for FCQs
# L:\mgt\FCQ\R_Code\course_audit\FCQ_TermSetUp.R - VDarcangelo 4/21/23
# Creates folders for new term
#########################################################################

# update term
new_term <- 2234
old_term <- 2231

# update semester info for CL AcademicTerm
new_sem <- 'Summer 2023'

# update beg_dt = spring: 01-01, summer: 05-01, fall: 08-01
beg_dt <- '2023-05-01T17:00:00-07:00'

# update end_dt = spring: 05-31, summer: 08-31, fall: 12-31
end_dt <- '2023-08-31T17:00:00-07:00'

#########################################################################

# create folder for past term audit files
dir.create(paste0('C:\\Users\\darcange\\OneDrive - UCB-O365\\CourseAudit\\Previous_Semesters\\',old_term))

# create folder for new term audit files (in L:\mgt\FCQ\CourseAudit)
dir.create(paste0('L:\\mgt\\FCQ\\CourseAudit\\',new_term))

# create folders for CL import files
dir.create(paste0('K:\\IR\\FCQ\\Prod\\',new_term))
dir.create(paste0('K:\\IR\\FCQ\\Prod\\',new_term,'\\AcademicTerm'))
dir.create(paste0('K:\\IR\\FCQ\\Prod\\',new_term,'\\Accounts'))
dir.create(paste0('K:\\IR\\FCQ\\Prod\\',new_term,'\\Course'))
dir.create(paste0('K:\\IR\\FCQ\\Prod\\',new_term,'\\Enrollment'))
dir.create(paste0('K:\\IR\\FCQ\\Prod\\',new_term,'\\Instructor'))
dir.create(paste0('K:\\IR\\FCQ\\Prod\\',new_term,'\\OrgUnit'))
dir.create(paste0('K:\\IR\\FCQ\\Prod\\',new_term,'\\Section'))
dir.create(paste0('K:\\IR\\FCQ\\Prod\\',new_term,'\\SectionAttribute'))

# create row1 file (in AcademicTerm) to set up CL
row1 <- data.frame(
  TermIdentifier = new_term,
  Name = new_sem,
  BeginDate = beg_dt,
  EndDate = end_dt,
  ParentIdentifier = '',
  Type = 'Semester')

# write file for CL import
write.csv(row1, paste0("K:\\IR\\FCQ\\Prod\\", new_term, "\\AcademicTerm\\term", new_term, "_row1.csv"), row.names = FALSE)

# create new term file (in AcademicTerm) to set up CL
row2 <- data.frame(
  TermIdentifier = c(paste0(new_term,':CUBLD:BLDR'), paste0(new_term,':CUBLD:BLD3'), paste0(new_term,':CUBLD:CEPS'), paste0(new_term,':CUDEN')),
  Name = c(paste0(new_sem,' CU Boulder main campus semester'), paste0(new_sem,' CU Boulder distance semester'), paste0(new_sem,' CU Boulder Continuing Education'), paste0(new_sem,' CU Denver semester')),
  BeginDate = beg_dt,
  EndDate = end_dt,
  ParentIdentifier = new_term,
  Type = 'Intersession')

# write file for CL import
write.csv(row2, paste0("K:\\IR\\FCQ\\Prod\\", new_term, "\\AcademicTerm\\term", new_term, ".csv"), row.names = FALSE)

# create folder for past term results in Campus Labs folder
dir.create(paste0('L:\\mgt\\FCQ\\CampusLabs\\Batch_Reports\\', old_term))
dir.create(paste0('L:\\mgt\\FCQ\\CampusLabs\\Batch_Reports\\', old_term, '\\AMC'))
dir.create(paste0('L:\\mgt\\FCQ\\CampusLabs\\Batch_Reports\\', old_term, '\\Boulder'))
dir.create(paste0('L:\\mgt\\FCQ\\CampusLabs\\Batch_Reports\\', old_term, '\\Denver'))
dir.create(paste0('L:\\mgt\\FCQ\\CampusLabs\\Response Exports\\', old_term))

# create folder for new term in CL data_files
dir.create(paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\',new_term))
