################################################################################### creating .csv file to import new departments to Campus Labs
# L:\mgt\FCQ\R_Code\campus_labs\CL_Dept_Create.R - Vince Darcangelo, 12/19/22
# output: K:\IR\FCQ\Prod\22xx\OrgUnit\CLOrgUnits_(date).csv" date format 20221219
################################################################################### 

# set date and term
entrydt <- Sys.Date()
entrydt <- format(entrydt, format = "%Y%m%d")

term_cd <- 2231

# set values of new dept (format for non-dept (e.g., division, school) below)
df <- data.frame(

# format: CUBLD:BLDR:ENES, CUBLD:CEPS:BBAC, CUDEN:ENGR:D-IWKS, CUDEN:MEDS:D-BCMG
OrgUnitIdentifier = "CUBLD:BLDR:LASC",

# format: CU Boulder deptnm (ENES), CU Continuing Ed deptnm (BBAC), CU Denver deptnm (IWKS), CU Denver deptnm (MEDS:D-BCMG)
Name = "CU Boulder Latin American and Latinx Studies Center (LASC)",

# format: ENES, CEPS:BBAC, D-IWKS, D-BCMG
Acronym = "LASC",

# format: CUBLD:BLDR, CEPS:CEPS, CUDEN:ENGR, CUDEN:CLAS, CUDEN:MEDS
ParentIdentifier = "CUBLD:BLDR",

# options: Division (campus), School (college), Department
Type = "Department"
)

write.csv(df, paste0("K:\\IR\\FCQ\\Prod\\", term_cd, "\\OrgUnit\\CLOrgUnits_", entrydt, ".csv"), row.names = FALSE)


################################################################################### 
# hierarchy in campus labs for reference

# Institution (University of Colorado) 
#   > Academic Affairs (level 1) 
#     > Division (level 2) 
#       > School (level 3)
#         > Department (level 4)

# formatting
# CU,	University of Colorado,	CU,	, Institution
# AA,	Academic Affairs,	AA,	CU,	Division
# CUBLD,	CU Boulder (CUBLD),	CUBLD,	AA,	Division
# CUDEN,	CU Denver (CUDEN),	CUDEN,	AA,	Division
# CUSPG,	CU Colorado Springs (CUSPG),	CUSPG,	AA,	Division
# CUBLD:BLDR,	CU Boulder main campus (BLDR),	CUBLD:BLDR,	CUBLD,	Division
# CUBLD:CEPS,	CU Continuing Education (CEPS),	CUBLD:CEPS,	CUBLD,	Division
# CUDEN:MEDS,	CU Denver Anschutz Graduate Studies (MEDS),	CUDEN:MEDS,	CUDEN,	School
