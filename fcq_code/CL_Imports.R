#########################################################################
# creating .csv files to import to Campus Labs for FCQ administration
# L:\mgt\FCQ\R_Code\campus_labs\CL_Imports.R - Vince Darcangelo, 10/24/22
# files generated in K:\IR\FCQ\Prod\22xx\ folders as csv files
#########################################################################

# reformat deptOrgID
clscu3$deptOrgID <- gsub("_",":",clscu3$deptOrgID)

# set term_cd
term_cd <- 2231

# update batch number: updated to batch 23 on 4/25 vd
sess_num <- '23'
batch <- paste0("batch", sess_num)

# filter for desired session
session <- clscu3 %>%
  # filter(adminDtTxt == "Apr 17-Apr 21")
  filter(SBJCT_CD == 'HIST' & CATALOG_NBR == '1025' & CLASS_SECTION_CD == '103')
# final session filters
  # filter(campus == "B3" & adminDtTxt == "Apr 24-May 02")
  # filter(campus == "CE" & adminDtTxt == "Apr 24-May 02")
  # filter(campus == "BD" & adminDtTxt == "Apr 24-May 04")
  # filter(campus == "BD" & adminDtTxt == "Apr 24-May 02")
  # filter(campus == "DN" & adminDtTxt == "Apr 24-May 02")
  # filter(campus == "MC" & adminDtTxt == "May 01-May 09")

# create list of variable names
#colvars <- as.list(colnames(clscu3))

#########################################################################
# inst account csv
instAcct <- session %>%
  ungroup() %>%
  select(instrConstituentID, instrFirstNm, instrLastNm, instrEmailAddr) %>%
  mutate(instrConstituentID = paste0(instrConstituentID, "@cu.edu")) %>%
  distinct()

colnames(instAcct) <- c("PersonIdentifier", "FirstName", "LastName", "Email")

# create file for CL import
write.csv(instAcct, paste0("K:\\IR\\FCQ\\Prod\\", term_cd, "\\Accounts\\instrAcct", term_cd, "_", batch, ".csv"), row.names = FALSE)

# create running cumulative file
instAcct_all <- instAcct %>%
  mutate(batch = sess_num)

write.csv(instAcct_all, paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\', term_cd, '\\instAcct_All.csv'), append = TRUE, row.names = FALSE)

# save to CL data_files
save(instAcct_all, file = paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\',term_cd,'\\instAcct_',batch,'.Rdata'))

#########################################################################
# stu account csv
# prep pers/cid/em_stu files for students
pers_stu <- pers
colnames(pers_stu) <- c("PERSON_SID", "stuPersonID", "stuNm", "stuLastNm", "stuFirstNm", "stuMiddleNm")

cid_stu <- cid
colnames(cid_stu) <- c("stuPersonID", "stuConstituentID")

em_stu <- em
colnames(em_stu) <- c("stuPersonID", "PREF_EMAIL", "BLD_EMAIL", "CONT_ED_EMAIL", "DEN_EMAIL")

# pull CLASS_SID from cx2 to match later
cx2 <- cx %>%
  select(CLASS_NUM, CLASS_SID)

# pull from PS_D_PERSON as studeth (~1 min)
studeth <- dbGetQuery(con,
  "SELECT PERSON_SID, PERSON_ID, DEATH_DT
  FROM PS_D_PERSON"
)

# filter by death_dt
studeth2 <- studeth %>%
  filter(DEATH_DT == '1900-01-01')

# pull from PS_F_CLASS_ENRLMT as stuenrl (~2 mins)
stuenrl <- dbGetQuery(con,
  "SELECT CLASS_SID, PERSON_SID, ENRLMT_STAT_SID, ENRLMT_DROP_DT_SID, ACAD_PROG_SID, GRADE_DT, CRSE_GRADE_OFF
  FROM PS_F_CLASS_ENRLMT"
)

# join session with cx2 on CLASS_NUM
stuAcct <- session %>%
  left_join(cx2, "CLASS_NUM") %>%
  left_join(stuenrl, "CLASS_SID") %>%
  left_join(pers_stu, "PERSON_SID") %>%
  left_join(cid_stu, "stuPersonID") %>%
  left_join(em_stu, "stuPersonID") %>%
  mutate(stuConstituentID = paste0(stuConstituentID, "@cu.edu"))

# reduce columns to those required by Campus Labs
stuAcct_import <- stuAcct %>%
  ungroup() %>%
  select(stuConstituentID, stuFirstNm, stuLastNm, PREF_EMAIL) %>%
  distinct()
colnames(stuAcct_import) <- c("PersonIdentifier", "FirstName", "LastName", "Email")

###XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX####
### MISSING EMAIL SEQUENCE
### error check -- do all the students have PREF_EMAIL?
emCheck <- stuAcct_import %>%
  filter(Email %in% c("","-"))
###XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX####

### if emCheck > 0, run this code, else skip to write.csv call
stuAcct3 <- stuAcct %>%
  select(stuConstituentID, stuFirstNm, stuLastNm, PREF_EMAIL, BLD_EMAIL, CONT_ED_EMAIL, DEN_EMAIL)

### pull PREF_EMAIL if exists, else pull campus email
stuAcct4 <- stuAcct3 %>%
  mutate(Email = case_when(
    PREF_EMAIL %in% c("","-") & INSTITUTION_CD == "CUBLD" ~ BLD_EMAIL,
    PREF_EMAIL %in% c("","-") & CAMPUS_CD == "CEPS" & CONT_ED_EMAIL != "-" ~ CONT_ED_EMAIL,
    PREF_EMAIL %in% c("","-") & CAMPUS_CD == "CEPS" & BLD_EMAIL != "-" ~ BLD_EMAIL,
    PREF_EMAIL %in% c("","-") & INSTITUTION_CD == "CUDEN" ~ DEN_EMAIL,
    TRUE ~ PREF_EMAIL
  ))

### error check for missing emails
stu_missing <- stuAcct4 %>%
  filter(PREF_EMAIL %in% c("","-"))

### if stu_missing == 0, else manually look up and add student email
stuAcct_import <- stuAcct4 %>%
  select(stuConstituentID, stuFirstNm, stuLastNm, Email)
colnames(stuAcct5) <- c("PersonIdentifier", "FirstName", "LastName", "Email")
###XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX####

# create file for CL import
write.csv(stuAcct_import, paste0("K:\\IR\\FCQ\\Prod\\", term_cd, "\\Accounts\\stuAcct", term_cd, "_", batch, ".csv"), row.names = FALSE)

# create running cumulative file
stuAcct_all <- stuAcct_import %>%
  mutate(batch = sess_num)

write.csv(stuAcct_all, paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\', term_cd, '\\stuAcct_All.csv'), append = TRUE, row.names = FALSE)

# save to CL data_files
save(stuAcct_all, file = paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\',term_cd,'\\stuAcct_',batch,'.Rdata'))

#########################################################################
# course csv
crsecsv <- session %>%
  ungroup() %>%
  select(SBJCT_CD, CATALOG_NBR, CRSE_LD, deptOrgID) %>%
  mutate(CourseIdentifier = paste(deptOrgID, SBJCT_CD, CATALOG_NBR, sep = "_")) %>%
  mutate(Type = case_when(
    CATALOG_NBR <= 4999 ~ "Undergraduate",
    CATALOG_NBR >= 5000 & SBJCT_CD != "LAWS" ~ "Graduate",
    TRUE ~ "Professional"
  )) %>%
  add_column(Credits = '', Description = '', CIPCode = '') %>%
  relocate(CourseIdentifier, .before = SBJCT_CD) %>%
  relocate(Credits, .after = "CRSE_LD") %>%
  distinct()

colnames(crsecsv) <- c("CourseIdentifier", "Subject", "Number", "Title", "Credits", "OrgUnitIdentifier", "Type", "Description", "CIPCode")

# create file for CL import
write.csv(crsecsv, paste0('K:\\IR\\FCQ\\Prod\\', term_cd, '\\Course\\Course', term_cd, '_', batch, '.csv'), row.names = FALSE)

# create running cumulative file
crse_all <- crsecsv %>%
  mutate(batch = sess_num)

write.csv(crse_all, paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\', term_cd, '\\Crse_All.csv'), append = TRUE, row.names = FALSE)

save(crse_all, file = paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\',term_cd,'\\Crse_',batch,'.Rdata'))

#########################################################################
# section csv
# format dates and times
session$mtgStartDt <- strptime(session$mtgStartDt, "%m/%d/%Y")
session$mtgEndDt <- strptime(session$mtgEndDt, "%m/%d/%Y")
sub(" MDT", "", session$mtgStartDt)
sub(" MDT", "", session$mtgEndDt)
session$nSD <- substr(session$mtgStartDt, 1, 10)
session$nED <- substr(session$mtgEndDt, 1, 10)

# create mtgStartDt, mtgEndDt, SectionIdentifier
session2 <- session %>%
  select(-c(mtgStartDt, mtgEndDt)) %>%
  mutate(mtgStartDt = nSD) %>%
  mutate(mtgEndDt = nED) %>%
  mutate(SectionIdentifier = paste(term_cd, deptOrgID, SBJCT_CD, CATALOG_NBR, CLASS_SECTION_CD, SSR_COMP_CD, sep = "_")) %>%
  select(-c(nSD, nED))

# xlist sequence: isolate sponsor sects
xlist <- session2 %>%
  filter(combStat == 'S') %>%
    mutate(CrossListingIdentifier = SectionIdentifier) %>%
  ungroup() %>%
  select(campus, spons_id, CrossListingIdentifier)

# join CrossListingIdentifier
session3 <- session2 %>%
  left_join(xlist, c("campus", "spons_id"))

sectcsv <- session3 %>%
  ungroup() %>%
  select(SBJCT_CD, CATALOG_NBR, CLASS_SECTION_CD, CRSE_LD, deptOrgID, SSR_COMP_CD, INSTITUTION_CD, CAMPUS_CD, mtgStartDt, mtgEndDt, INSTRCTN_MODE_CD, LOC_ID, SectionIdentifier, CrossListingIdentifier) %>%
  mutate(CrossListingIdentifier = case_when(
    is.na(CrossListingIdentifier) ~ '',
    TRUE ~ CrossListingIdentifier)) %>%
  mutate(TermIdentifier = case_when(
    CAMPUS_CD %in% c("BLD3", "BLDR", "CEPS") ~ paste(term_cd, INSTITUTION_CD, CAMPUS_CD, sep = ":"),
    TRUE ~ paste(term_cd, INSTITUTION_CD, sep = ":"))) %>%
  mutate(CourseIdentifier = paste(deptOrgID, SBJCT_CD, CATALOG_NBR, sep = "_")) %>%
  mutate(BeginDate = paste0(strftime(mtgStartDt, "%Y-%m-%d"), "T18:00:00-06:00")) %>%
  mutate(EndDate = paste0(strftime(mtgEndDt, "%Y-%m-%d"), "T18:00:00-06:00")) %>%
  mutate(DeliveryMode = case_when(
    INSTRCTN_MODE_CD %in% c("OL", "OS", "SO") ~ "ONLINE",
    INSTRCTN_MODE_CD %in% c("HY", "HN", "H1", "H2") ~ "HYBRID",
    TRUE ~ "FACE2FACE")) %>%
  add_column(Credits = '', Description = '', CIPCode = '') %>%
  select(SectionIdentifier, TermIdentifier, CourseIdentifier, SBJCT_CD, CATALOG_NBR, CLASS_SECTION_CD, BeginDate, EndDate, deptOrgID, CRSE_LD, Credits, DeliveryMode, LOC_ID, Description, CrossListingIdentifier) %>%
  distinct()

colnames(sectcsv) <- c("SectionIdentifier", "TermIdentifier", "CourseIdentifier", "Subject", "CourseNumber", "Number", "BeginDate", "EndDate", "OrgUnitIdentifier", "Title", "Credits", "DeliveryMode", "Location", "Description", "CrossListingIdentifier")

# create file for CL import
write.csv(sectcsv, paste0("K:\\IR\\FCQ\\Prod\\", term_cd, "\\Section\\Section", term_cd, "_", batch, ".csv"), row.names = FALSE)

# create running cumulative file
sect_all <- sectcsv %>%
  mutate(batch = sess_num)

write.csv(sect_all, paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\', term_cd, '\\Sect_All.csv'), append = TRUE, row.names = FALSE)

save(sect_all, file = paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\',term_cd,'\\Sect_',batch,'.Rdata'))

#########################################################################
# inst csv
instcsv <- session %>%
  ungroup() %>%
  select(instrConstituentID, deptOrgID, SBJCT_CD, CATALOG_NBR, CLASS_SECTION_CD, SSR_COMP_CD, instrFirstNm, instrLastNm, instrEmailAddr, INSTRCTR_ROLE_CD) %>%
  mutate(PersonIdentifier = paste0(instrConstituentID, "@cu.edu")) %>%
  mutate(SectionIdentifier = paste(term_cd, deptOrgID, SBJCT_CD, CATALOG_NBR, CLASS_SECTION_CD, SSR_COMP_CD, sep = "_")) %>%
  mutate(Role = case_when(
    INSTRCTR_ROLE_CD == "PI" ~ "Primary",
    INSTRCTR_ROLE_CD == "SI" ~ "Secondary",
    INSTRCTR_ROLE_CD == "TA" ~ "TeachingAssistant"
  )) %>%
  select(PersonIdentifier, SectionIdentifier, instrFirstNm, instrLastNm, instrEmailAddr, Role) %>%
  distinct() %>%
  mutate(instrFirstNm = case_when(
    instrFirstNm == '' ~ '.',
    TRUE ~ instrFirstNm)) %>%
  mutate(instrLastNm = case_when(
    instrLastNm == '' ~ '.',
    TRUE ~ instrLastNm))

colnames(instcsv) <- c("PersonIdentifier", "SectionIdentifier", "FirstName", "LastName", "Email", "Role")

# create file for CL import
write.csv(instcsv, paste0("K:\\IR\\FCQ\\Prod\\", term_cd, "\\Instructor\\instr", term_cd, "_", batch, ".csv"), row.names = FALSE)

# create running cumulative file
inst_all <- instcsv %>%
  mutate(batch = sess_num)

write.csv(inst_all, paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\', term_cd, '\\Inst_All.csv'), append = TRUE, row.names = FALSE)

save(inst_all, file = paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\',term_cd,'\\Inst_',batch,'.Rdata'))

#########################################################################
# stu enrl

# filter by drop dt and withdrawls
studrp2 <- stuenrl %>%
#  filter(ENRLMT_DROP_DT_SID == 19000101 & CRSE_GRADE_OFF != 'W')
  filter(ENRLMT_STAT_SID == 3)

# join studrp2 and studeth2
studrp3 <- studrp2 %>%
  left_join(studeth2, "PERSON_SID") %>%
  drop_na(ENRLMT_DROP_DT_SID, CRSE_GRADE_OFF, DEATH_DT)

# join studrp3 with stuAcct
drops <- stuAcct %>%
  left_join(studrp3, c("CLASS_SID", "PERSON_SID"))

# assign enrollment status
drops2 <- drops %>%
  mutate(Status = case_when(
    ENRLMT_STAT_SID.x == 3 ~ 'Enrolled',
    TRUE ~ 'Dropped'
  ))

# reduce to match columns
drops3 <- drops2 %>%
  select(CLASS_NUM, PERSON_SID, stuPersonID, instrPersonID, Status)

# create stuEnrl2 doc to match
stuEnrl2 <- stuAcct_import %>%
  left_join(stuAcct, c("PersonIdentifier" = "stuConstituentID")) %>%
  mutate(SectionIdentifier = paste(term_cd, deptOrgID, SBJCT_CD, CATALOG_NBR, CLASS_SECTION_CD, SSR_COMP_CD, sep = "_")) %>%
  left_join(sectcsv, "SectionIdentifier")

# join stuEnrl2 with drops3
stuenrl_cmbd <- stuEnrl2 %>%
  left_join(drops3, c("CLASS_NUM", "PERSON_SID", "stuPersonID", "instrPersonID"), suffix = c("", ".y")) %>% 
  select_at(
    vars(-ends_with(".y"))
)

# reduce to needed columns and format for CL import
stuenrl_cmbd2 <- stuenrl_cmbd %>%
  select(PersonIdentifier, SectionIdentifier, Status, FirstName, LastName, Email, mtgStartDt, mtgEndDt) %>%
  distinct() %>%
  mutate(mtgStartDt = format(mdy(mtgStartDt), "%Y-%m-%d")) %>%
  mutate(mtgEndDt = format(mdy(mtgEndDt), "%Y-%m-%d")) %>%
  mutate(BeginDate = paste0(mtgStartDt, "T18:00:00-06:00")) %>%
  mutate(EndDate = paste0(mtgEndDt, "T18:00:00-06:00")) %>%
  add_column(Credits = '', GradeOption = '', RegisteredDate = '', InitialCourseGrade = '', StatusChangeDate = '', FinalCourseGrade = '') %>%
  select(PersonIdentifier,SectionIdentifier,Status,FirstName,LastName,Email,Credits,GradeOption,RegisteredDate,BeginDate,EndDate,InitialCourseGrade,StatusChangeDate,FinalCourseGrade) %>%
  mutate(FirstName = case_when(
    is.na(FirstName) ~ '.',
    TRUE ~ FirstName)) %>%
  mutate(LastName = case_when(
    is.na(LastName) ~ '.',
    TRUE ~ LastName))

# NEED TO ACCT FOR DUPS AT THIS STAGE
# where PersonIdentifier and SectionIdentifier both match
# choose Status == 'Enrolled' and remove Status == 'Dropped'
stuenrl_cmbd3 <- stuenrl_cmbd2 %>%
  mutate(dupck = paste0(PersonIdentifier, SectionIdentifier)) %>%
  group_by(dupck) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  select(-dupck)

# remove DROPPED and keep ENROLLED
stuenrl_cmbd4 <- stuenrl_cmbd3 %>%
  filter(Status == 'Enrolled')

# remove dups
stuenrl_cmbd5 <- anti_join(stuenrl_cmbd2,stuenrl_cmbd3)

# add filtered rows
stuenrl_cmbd6 <- rbind(stuenrl_cmbd5,stuenrl_cmbd4)

# remove DROPPED and keep ENROLLED
stuenrl_cmbd6 <- stuenrl_cmbd6 %>%
  filter(Status == 'Enrolled')

# create file for CL import
write.csv(stuenrl_cmbd6, paste0("K:\\IR\\FCQ\\Prod\\", term_cd, "\\Enrollment\\stuEnrl", term_cd, "__", batch, ".csv"), row.names = FALSE)

# create running cumulative file
enrl_all <- stuenrl_cmbd6 %>%
  mutate(batch = sess_num)

write.csv(enrl_all, paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\', term_cd, '\\Enrl_All.csv'), append = TRUE, row.names = FALSE)

save(enrl_all, file = paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\',term_cd,'\\Enrl_',batch,'.Rdata'))

#########################################################################
# sect attr

# select cols from session to match AND to create Identifier
cl_attr <- clscu3 %>%
  ungroup() %>%
  filter(sect_attr != '') %>%
  mutate(Identifier = paste(TERM_CD, deptOrgID, SBJCT_CD, CATALOG_NBR, CLASS_SECTION_CD, crseSec_comp_cd, sep = "_")) %>%
  select(Identifier, sect_attr) %>%
  separate_rows(sect_attr, sep = ',') %>%
  rename(Key = sect_attr) %>%
  mutate(Value = Key)

sess_attr <- session %>%
  ungroup() %>%
  mutate(Identifier = paste(TERM_CD, deptOrgID, SBJCT_CD, CATALOG_NBR, CLASS_SECTION_CD, crseSec_comp_cd, sep = "_")) %>%
  left_join(cl_attr, by = 'Identifier') %>%
  distinct() %>%
  select(Identifier, Key, Value) %>%
  filter(!is.na(Key)) %>%
  distinct()

# import cumulative list from K:\ (only run after created)
# semqs <- read.csv(paste0("K:\\IR\\FCQ\\Prod\\", term_cd, "\\SectionAttribute\\secAttr", term_cd, ".csv"))

# combine current session qs with previous custom qs
# sectattr_total <- rbind(semqs, sess_attr)

# for record keeping/troubleshooting
write.csv(sess_attr, paste0("K:\\IR\\FCQ\\Prod\\", term_cd, "\\SectionAttribute\\secAttr", term_cd, "_", batch, ".csv"), row.names = FALSE)

# create file for CL import
write.csv(sess_attr, paste0("K:\\IR\\FCQ\\Prod\\", term_cd, "\\SectionAttribute\\secAttr", term_cd, ".csv"), append = TRUE, row.names = FALSE)

# create running cumulative file
sectattr_all <- sess_attr %>%
  mutate(batch = sess_num)

write.csv(sectattr_all, paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\', term_cd, '\\sectAttr_All.csv'), append = TRUE, row.names = FALSE)

save(sectattr_all, file = paste0('L:\\mgt\\FCQ\\CampusLabs\\Data_Files\\',term_cd,'\\sectAttr_All_', batch, '.Rdata'))
