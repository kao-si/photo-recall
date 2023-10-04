

library(readxl)
library(tidyverse)
library(fixest)
library(modelsummary)


# Data Management ####


# Import data
df <- read_excel("News-Events-Study/Raw-Data.xlsx")

# Construct the measure of perceived temporal distance of each event
df <- df %>% mutate(
  ptdsm = rowMeans(select(., distsm1, distsm2), na.rm = TRUE),
  ptdfu = rowMeans(select(., distfu1, distfu2), na.rm = TRUE),
  ptdlv = rowMeans(select(., distlv1, distlv2), na.rm = TRUE),
  ptdrw = rowMeans(select(., distrw1, distrw2), na.rm = TRUE),
  ptdpg = rowMeans(select(., distpg1, distpg2), na.rm = TRUE),
  ptddt = rowMeans(select(., distdt1, distdt2), na.rm = TRUE)
)

# Construct Style of Processing scores

df <- df %>% mutate(
  # construct reverse scores
  rev_sp2 = 5 - sp2,
  rev_sp3 = 5 - sp3,
  rev_sp5 = 5 - sp5,
  rev_sp6 = 5 - sp6,
  rev_sp8 = 5 - sp8,
  rev_sp10 = 5 - sp10,
  rev_sp11 = 5 - sp11,
  rev_sp12 = 5 - sp12,
  rev_sp13 = 5 - sp13,
  rev_sp14 = 5 - sp14,
  rev_sp16 = 5 - sp16,
  rev_sp19 = 5 - sp19,
  rev_sp21 = 5 - sp21,
  rev_sp22 = 5 - sp22
)
  
df <- df %>% mutate(
  
  # construct overall SOP score
  sop = rowMeans(select(., sp1, sp4, sp7, sp9, sp15, sp17, sp18, sp20,
                        rev_sp2:rev_sp22), na.rm = TRUE),
  
  # construct score for SOP visual component
  sop_vis = rowMeans(select(., rev_sp2, rev_sp5, rev_sp8:rev_sp14,
                            rev_sp16, sp20, rev_sp22), na.rm = TRUE),
  
  # construct score for SOP verbal component
  sop_ver = rowMeans(select(., sp1, rev_sp3, sp4, rev_sp6, sp7, sp9, sp15, sp17,
                            sp18, rev_sp19, rev_sp21), na.rm = TRUE)
)

df$treat <- factor(df$treat, levels = c(0, 1),
                   labels = c("No Photo", "Photo"))

# Reshape data to long format
df1 <- df %>% 
  select(ResponseId, starts_with("year"), eassm:numawa, male, age, race, treat:ptddt, sop:sop_ver) %>%
  
  # first use pivot_longer()
  pivot_longer(
    
    # identify columns to pivot
    col = -c(ResponseId, numawa, male, age, race, treat, sop:sop_ver),
    
    # name the columns that store the values from the column names 
    names_to = c("attr", "event"),
    
    # define the pattern that partitions the column names into two parts
    # (.*) the first part should have zero or more characters
    # (..) the second part should have just 2 characters at the “$” end of the string
    names_pattern = "(.*)(..)$"
  )

# then use pivot_wider()
df_long <- df1 %>% pivot_wider(
  
  # identify the column whose values will supply the column names
  names_from = attr,
  
  # identify the column whose values will supply the column values
  values_from = value,
  
  # to error if the column names are duplicated
  names_repair = "check_unique"
)


# Analyses ####


## R1: Main Effect of Treatment ====

r1_1 <- feols(ptd ~ treat, 
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

# Add controls
r1_2 <- feols(ptd ~ treat + eas + fam + imp + viv + emo + val,
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

# Add Event FE
r1_3 <- feols(ptd ~ treat | event, 
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

# Add controls & Event FE
r1_4 <- feols(ptd ~ treat + eas + fam + imp + viv + emo + val | event,
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

msummary(list(r1_1, r1_2, r1_3, r1_4), stars = TRUE)

## R2: Moderating Effect of SOP ====

r2_1 <- feols(ptd ~ treat*sop,
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

# Add controls
r2_2 <- feols(ptd ~ treat*sop + eas + fam + imp + viv + emo + val,
             cluster = ~ ResponseId,
             filter(df_long, is.na(awa) == TRUE))

# Add Event FE
r2_3 <- feols(ptd ~ treat*sop | event, 
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

# Add controls & Event FE
r2_4 <- feols(ptd ~ treat*sop + eas + fam + imp + viv + emo + val | event,
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

msummary(list(r2_1, r2_2, r2_3, r2_4), stars = TRUE)

## R3: Moderating Effect of SOP Verbal Component ====

r3_1 <- feols(ptd ~ treat*sop_ver,
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

# Add controls
r3_2 <- feols(ptd ~ treat*sop_ver + eas + fam + imp + viv + emo + val,
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

# Add Event FE
r3_3 <- feols(ptd ~ treat*sop_ver | event, 
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

# Add controls & Event FE
r3_4 <- feols(ptd ~ treat*sop_ver + eas + fam + imp + viv + emo + val | event,
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

msummary(list(r3_1, r3_2, r3_3, r3_4), stars = TRUE)

## R4: Moderating Effect of SOP Visual Component ====

r4_1 <- feols(ptd ~ treat*sop_vis,
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

# Add controls
r4_2 <- feols(ptd ~ treat*sop_vis + eas + fam + imp + viv + emo + val,
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

# Add Event FE
r4_3 <- feols(ptd ~ treat*sop_vis | event, 
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

# Add controls & Event FE
r4_4 <- feols(ptd ~ treat*sop_vis + eas + fam + imp + viv + emo + val | event,
              cluster = ~ ResponseId,
              filter(df_long, is.na(awa) == TRUE))

msummary(list(r4_1, r4_2, r4_3, r4_4), stars = TRUE)