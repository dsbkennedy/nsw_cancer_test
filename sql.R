
pacman::p_load(sqldf,readxl,here)


raw <- here('raw_data.xlsx')
patient <- readxl::read_xlsx(raw, sheet='patient')
stay <- readxl::read_xlsx(raw, sheet='stay')


sqldf('SELECT patient.mrn, 
coalesce(patient."First Name", stay."First Name") AS "First Name",
coalesce(patient."Last Name", stay."Last Name") AS "Last Name",
patient.DOB, 
patient.Gender
      FROM patient  
      LEFT JOIN stay
      ON patient.mrn = stay.mrn')




