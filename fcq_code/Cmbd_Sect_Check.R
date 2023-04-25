#########################################################################
# Find combined section errors for Registrar in advance of semester
# L:\mgt\FCQ\R_Code\hr_tools\Comb_Sect_Check.R - VDarcangelo 04/21/23
# Run 6-8 weeks before beginning of each semester
# Files ouput to L:\mgt\FCQ\Data_Requests\CombSect3Reg\
#   - BDProbCombSect_(sheetdate).xlsx
#   - CEProbCombSect_(sheetdate).xlsx
#   - DNProbCombSect_(sheetdate).xlsx
#   - CSProbCombSect_(sheetdate).xlsx
# Email generated files to respective Registrar Office emails:
#   - BD: academicscheduling@colorado.edu
#   - CE: cerecords@colorado.edu
#   - CS: tbarber@uccs.edu (Tracy Barber), aschwab@uccs.edu (Ann Schwab)
#   - DN: courses@ucdenver.edu
#########################################################################

setwd('L:\\mgt\\FCQ\\Data_Requests\\CombSect4Reg')
cusis_term <- 2234

#isiswhdb.ps_d_class 
# pull from PS_D_CLASS as c (~15 secs)
psd_class <- dbGetQuery(con,
  'SELECT CLASS_SID, TERM_CD, INSTITUTION_CD, CAMPUS_CD, SESSION_CD, CRSE_CD, CRSE_OFFER_NUM, SBJCT_CD, CATALOG_NBR, CLASS_SECTION_CD, CLASS_NUM, ENRL_TOT, SSR_COMP_CD, CANCEL_DATE, DATA_ORIGIN
    FROM PS_D_CLASS'
  )

comck1 <- psd_class %>%
  filter(TERM_CD == cusis_term & DATA_ORIGIN == 'S')

# pull from PS_CU_D_CLASS_ATTR as cattr (~6 secs)
p_cattr <- dbGetQuery(con,
  'SELECT CLASS_SID, DATA_ORIGIN, CRSE_ATTR_VALUE_CD
  FROM PS_CU_D_CLASS_ATTR'
)

comck2 <- p_cattr %>%
  filter(DATA_ORIGIN == 'S' & CRSE_ATTR_VALUE_CD %in% c('SPONSOR', 'NON-SPONSR')) %>%
  select(-DATA_ORIGIN)

cbd1 <- left_join(comck1, comck2, by = 'CLASS_SID')

# pull from PS_CU_D_SCTN_CMBND as cmbsec (~1 sec)
p_sectco <- dbGetQuery(con,
  'SELECT SCTN_CMBND_CD, SCTN_CMBND_LD, PERM_COMBINATION, COMBINATION_TYPE, TERM_CD, CLASS_NUM, DATA_ORIGIN, INSTITUTION_CD
  FROM PS_CU_D_SCTN_CMBND'
)

comck3 <- p_sectco %>%
  filter(TERM_CD == cusis_term & DATA_ORIGIN == 'S') %>%
  select(-c(TERM_CD, INSTITUTION_CD, DATA_ORIGIN))


cbd2 <- left_join(cbd1, comck3, by = 'CLASS_NUM')

cbd3 <- cbd2 %>%
  select(TERM_CD, INSTITUTION_CD, CAMPUS_CD, SESSION_CD, SBJCT_CD, CATALOG_NBR, CLASS_SECTION_CD, CLASS_NUM, CRSE_CD, CRSE_OFFER_NUM, SCTN_CMBND_CD, SCTN_CMBND_LD, PERM_COMBINATION, COMBINATION_TYPE, CRSE_ATTR_VALUE_CD, ENRL_TOT, SSR_COMP_CD, CANCEL_DATE) %>%
  filter(CANCEL_DATE <= '1900-01-01 00:00:00')

#########################################################################
# filter for errors
#########################################################################

# cerr01: Cmbnd sctn class not assgnd sp/nonsp attrib: 
cerr01 <- cbd3 %>%
  filter(is.na(CRSE_ATTR_VALUE_CD) & SCTN_CMBND_CD != '') %>%
  mutate(Data_Issue = 'Cmbnd sctn class not assgnd sp/nonsp attrib')

# cerr02: Sponsor class not in cmbnd sctn: 
cerr02 <- cbd3 %>%
  filter(CRSE_ATTR_VALUE_CD == 'SPONSOR' & is.na(SCTN_CMBND_CD)) %>%
  mutate(Data_Issue = 'Sponsor class not in cmbnd sctn')

# cerr03: Nonspnsr class not in cmbnd sctn: 
cerr03 <- cbd3 %>%
  filter(CRSE_ATTR_VALUE_CD == 'NON-SPONSR' & is.na(SCTN_CMBND_CD)) %>%
  mutate(Data_Issue = 'Nonspnsr class not in cmbnd sctn')

# cerr04: 2+ spnsr and 1+ nonspnsr: 
cbd3x <- cbd3 %>%
  filter(!is.na(CRSE_ATTR_VALUE_CD)) %>%
  filter(!is.na(SCTN_CMBND_CD)) %>%
  filter(CRSE_ATTR_VALUE_CD == 'SPONSOR') %>%
  group_by(SCTN_CMBND_CD, SCTN_CMBND_LD) %>%
  distinct() %>%
  filter(n() >= 2) %>%
  select(SCTN_CMBND_CD, SCTN_CMBND_LD) %>%
  distinct()

cerr04 <- cbd3 %>%
  filter(SCTN_CMBND_CD == cbd3x$SCTN_CMBND_CD & SCTN_CMBND_LD == cbd3x$SCTN_CMBND_LD) %>%
  mutate(Data_Issue = '2+ spnsr and 1+ nonspnsr')

# cerr05: Only spnsr in cmbnd sctn, no non-spnsr:
# cerr06: Only non-spnsr in cmbnd sctn, no spnsr: 
cbd3y <- cbd3 %>%
  filter(!is.na(CRSE_ATTR_VALUE_CD)) %>%
  filter(!is.na(SCTN_CMBND_CD)) %>%
  filter(CRSE_ATTR_VALUE_CD == 'SPONSOR') %>%
  group_by(SCTN_CMBND_CD, SCTN_CMBND_LD) %>%
  distinct() %>%
  select(SCTN_CMBND_CD, SCTN_CMBND_LD, CRSE_ATTR_VALUE_CD)

cbd3z <- cbd3 %>%
  filter(!is.na(CRSE_ATTR_VALUE_CD)) %>%
  filter(!is.na(SCTN_CMBND_CD)) %>%
  filter(CRSE_ATTR_VALUE_CD == 'NON-SPONSR') %>%
  group_by(SCTN_CMBND_CD, SCTN_CMBND_LD) %>%
  distinct() %>%
  select(SCTN_CMBND_CD, SCTN_CMBND_LD, CRSE_ATTR_VALUE_CD)

cbd4s <- anti_join(cbd3y, cbd3z, by = c('SCTN_CMBND_CD', 'SCTN_CMBND_LD'))

cbd4n <- anti_join(cbd3z, cbd3y, by = c('SCTN_CMBND_CD', 'SCTN_CMBND_LD'))

cerr05 <- cbd3 %>%
  filter(SCTN_CMBND_CD %in% cbd4s$SCTN_CMBND_CD & SCTN_CMBND_LD %in% cbd4s$SCTN_CMBND_LD & CRSE_ATTR_VALUE_CD %in% cbd4s$CRSE_ATTR_VALUE_CD) %>%
  mutate(Data_Issue = 'Only spnsr in cmbnd sctn, no non-spnsr')

cerr06 <- cbd3 %>%
  filter(SCTN_CMBND_CD %in% cbd4n$SCTN_CMBND_CD & SCTN_CMBND_LD %in% cbd4n$SCTN_CMBND_LD & CRSE_ATTR_VALUE_CD %in% cbd4n$CRSE_ATTR_VALUE_CD) %>%
  mutate(Data_Issue = 'Only non-spnsr in cmbnd sctn, no spnsr')

# combine all cerr files
cerr <- rbind(cerr01, cerr02, cerr03, cerr04, cerr05, cerr06)

# arrange and split for excel output
cerr <- cerr %>%
  select(Data_Issue, TERM_CD, INSTITUTION_CD, CAMPUS_CD, SESSION_CD, SCTN_CMBND_CD, SBJCT_CD, CATALOG_NBR, CLASS_SECTION_CD, CRSE_ATTR_VALUE_CD, SCTN_CMBND_LD, CLASS_NUM, CRSE_CD, CRSE_OFFER_NUM, SSR_COMP_CD, ENRL_TOT, PERM_COMBINATION, COMBINATION_TYPE)

colnames(cerr) <- c('Dataissue', 'Termn', 'Inst', 'Campus', 'Sessn', 'CombSecID', 'Subject', 'CatalogNbr', 'Sectn', 'Classattr', 'Combsectdescrip', 'ClassNbr', 'CrseID', 'OffrNbr', 'Compon', 'EnrlTot', 'Permcomb', 'Combtype')

#########################################################################
# generate combined files by campus
#########################################################################

# BD
bdcmbd <- cerr %>%
  filter(Campus == 'BLDR') %>%
  arrange(Subject, CatalogNbr, Sectn)

# CE
cecmbd <- cerr %>%
  filter(Campus == 'CEPS') %>%
  arrange(Subject, CatalogNbr, Sectn)

# DN
dncmbd <- cerr %>%
  filter(Inst == 'CUDEN') %>%
  arrange(Subject, CatalogNbr, Sectn)

# CS
cscmbd <- cerr %>%
  filter(Inst == 'CUSPG') %>%
  arrange(Subject, CatalogNbr, Sectn)

#########################################################################
# create styles for output
#########################################################################

# all class daily + backup
appenddt <- Sys.Date()
sheetdate <- format(appenddt, format = '%m%d%y')
sheetnm <- 'Comb Sect Issues'

# create style for top row
Heading <- createStyle(textDecoration = 'bold', fgFill = '#EDF2F9', border = 'TopBottomLeftRight')

#########################################################################
# create BOULDER workbook
reg_bd <- createWorkbook()
addWorksheet(reg_bd, sheetnm, gridLines = TRUE)
writeData(reg_bd, sheetnm, paste0('Combined section data issues for CU Boulder - as of ', sheetdate), startCol = 1, startRow = 1)
writeData(reg_bd, sheetnm, bdcmbd, startCol = 1, startRow = 3, withFilter = TRUE)

# freeze top row of table
freezePane(reg_bd, sheetnm, firstActiveRow = 4, firstActiveCol = 1)

# add style to header
addStyle(reg_bd, sheetnm, cols = 1:ncol(bdcmbd), rows = 3, style = Heading)

saveWorkbook(reg_bd, paste0('BDProbCombSect_', sheetdate, '.xlsx'), overwrite = TRUE)

#########################################################################
# create CEPS workbook
reg_ce <- createWorkbook()
addWorksheet(reg_ce, sheetnm, gridLines = TRUE)
writeData(reg_ce, sheetnm, paste0('Combined section data issues for CU Continuing Education - as of ', sheetdate), startCol = 1, startRow = 1)
writeData(reg_ce, sheetnm, cecmbd, startCol = 1, startRow = 3, withFilter = TRUE)

# freeze top row of table
freezePane(reg_ce, sheetnm, firstActiveRow = 4, firstActiveCol = 1)

# add style to header
addStyle(reg_ce, sheetnm, cols = 1:ncol(cecmbd), rows = 3, style = Heading)

saveWorkbook(reg_ce, paste0('CEProbCombSect_', sheetdate, '.xlsx'), overwrite = TRUE)

#########################################################################
# create DENVER workbook
reg_dn <- createWorkbook()
addWorksheet(reg_dn, sheetnm, gridLines = TRUE)
writeData(reg_dn, sheetnm, paste0('Combined section data issues for CU Denver - as of ', sheetdate), startCol = 1, startRow = 1)
writeData(reg_dn, sheetnm, dncmbd, startCol = 1, startRow = 3, withFilter = TRUE)

# freeze top row of table
freezePane(reg_dn, sheetnm, firstActiveRow = 4, firstActiveCol = 1)

# add style to header
addStyle(reg_dn, sheetnm, cols = 1:ncol(dncmbd), rows = 3, style = Heading)

saveWorkbook(reg_dn, paste0('DNProbCombSect_', sheetdate, '.xlsx'), overwrite = TRUE)

#########################################################################
# create COLOSPRINGS workbook
reg_cs <- createWorkbook()
addWorksheet(reg_cs, sheetnm, gridLines = TRUE)
writeData(reg_cs, sheetnm, paste0('Combined section data issues for CU Colorado Springs - as of ', sheetdate), startCol = 1, startRow = 1)
writeData(reg_cs, sheetnm, cscmbd, startCol = 1, startRow = 3, withFilter = TRUE)

# freeze top row of table
freezePane(reg_cs, sheetnm, firstActiveRow = 4, firstActiveCol = 1)

# add style to header
addStyle(reg_cs, sheetnm, cols = 1:ncol(cscmbd), rows = 3, style = Heading)

saveWorkbook(reg_cs, paste0('CSProbCombSect_', sheetdate, '.xlsx'), overwrite = TRUE)
