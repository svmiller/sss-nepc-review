library(tidyverse)
library(stevemisc)
setwd("~/nepc-review")

# I'm not sure if I have permission to distribute this...
Dat <- haven::read_dta("pfi_pu_pert.dta")

Dat %>%
  select(basmid, 
         # Main IVs...
         S16TYPE, scpubpri, schrtschl,
         # Dependent variables...
         fcschool, fcteachr, fcstds, fcorder, fcsupprt, seenjoy,
         # child race...
         cblack,chispan, chisprm, casian, camind, cwhite, cpaci,
         # child place of birth, child sex...
         cplcbrth, csex,
         # how are you related to this child?
         relation,
         # Census region, poverty level, minority in ZIP code, ZIP code type/classification
         cenreg, ZIP18PO2, ZIPBLHI2, ziplocl,
         # Questionnaire in English or SpanisH? Questionnaire on web or paper?
         englspanx, modecomp,
         # Classification of school? Elementary, middle/junior, etc...
         schlgrad,
         # Total school enrollment size
         S16NUMST,
         # Taking any class online?
         snetcrsx,
         # Grade level holy shit why is this a fixed effect when you already have school classification what the fuck
         grade,
         # Birth month, like what... and birth year, which, sure...
         cdobmm, cdobyy,
         # Language at home
         cspeakx,
         # Child has disability
         disabltyx, DISBLTY2X,
         # speech/language impairment, emotional disturbance, deafness, blindness, orthopedic, autism,
         hdspeechx, hddistrbx, hddeafimx, hdorthox, hdautismx,
         # PDD, ADD, specific learning disability, development delay, traumatic brain injury, other injury
         hdpddx,hdaddx,hdlearnx, hddelayx, hdtrbrain, hdotherx,
         # month started school, same school beginning of the year, consider other schools, first choice
         smvmth, ssamsc, sconsidr, S1STCHOI,
         # Attend school event: # of family school
         fssportx:fsfreq,
         # Told story: // seriously, why are these in the model.
         FOSTORY2X:fohistx,
         # Times eaten dinner
         fodinnerx,
         # family attended library:familyt attended sporting event
         folibrayx:fosprtevx,
         # how many people in the household, brothers:other relatives
         hhtotalxx:hhonrelsx,
         # languages at home
         hhenglish:hhothlang,
         # Parent 1 relation, sex, marital status, first language, place of birth, race/ethnic, educ, currently enrolled, employment, age
         P1REL:P1MRSTA, P1FRLNG, P1SPEAK, P1PLCBRTH, P1HISPAN:P1HISPRM, P1EDUC, P1ENRL, P1EMPL, P1MTHSWRK, P1AGE, intacc,
         # Second parent in household
         P2GUARD,
         # Internet access
         hvintspho:useintrnt,
         # TANF-Section 8
         hwelftan:HSECN8,
         # Household income, years at address, rented home
         ttlhhinc, yrsaddr, ownrnthb,
         # Other stuff
         sneighbrx
         ) %>% rename_all(tolower) %>%
  # Make anything that's a -1 or -9 an NA
  mutate_if(is.numeric, ~ifelse(0 > ., NA, .)) %>%
  # Make some functional DVs now.
  # Worth noting: satisfaction counts up with "dissatisfaction". Let's make it more intuitive.
  # Same intuition with "seenjoy", though that's an agree/disagree question (but on same scale)
  mutate_at(vars("fcschool", "fcteachr","fcstds","fcorder","fcsupprt","seenjoy"),
            ~dplyr::recode(., `1` = 4, `2` = 3, `3` = 2, `4` = 1)) %>%
  # Let's create some dummies too
  mutate_at(vars("fcschool", "fcteachr","fcstds","fcorder","fcsupprt","seenjoy"),
            .funs = list(d = ~ifelse(. >= 3, 1, 0))) %>%
  # Let's do some school type variables now.
  mutate(sc_public = ifelse(scpubpri == 4, 1, 0),
         sc_cath = ifelse(scpubpri == 1, 1, 0),
         sc_otherrelig = ifelse(scpubpri == 2, 1, 0),
         sc_privnr = ifelse(scpubpri == 3, 1, 0),
         sc_public = ifelse(scpubpri == 4, 1, 0),
         sc_charter = ifelse(schrtschl == 1, 1, 0),
         # If it's missing, it's most likely because the student is in a private school.
         sc_charter = ifelse(is.na(sc_charter) & !is.na(scpubpri), 0, sc_charter)) %>%
  # child race/ethnicity dummies
  # Convert them 2s to 0s.
  mutate_at(vars("cblack","chispan","chisprm","casian", "camind", "cwhite", "cpaci"),
            ~ifelse(. == 2, 0, 1)) %>%
  # Create a non-white variable.
  mutate(cnonwhite = ifelse(cwhite == 0, 1, 0)) %>%
  # Create c-female variable
  mutate(cfemale = ifelse(csex == 2, 1, 0)) %>%
  # Make the relation variable more intuitively a factor
  mutate(relationf = recode(relation, `1` = "Mother",
                            `2` = "Father",
                            `3` = "Aunt",
                            `4` = "Uncle",
                            `5` = "Grandmother",
                            `6` = "Grandfather",
                            `7` = "Parent's GF/BF/Partner",
                            `8` = "Other Relationship",
                            `9` = "Sibling"))  %>%
  # Make poverty level variable more intuitively a factor
  mutate(povlevel = recode(zip18po2,
                           `1` = "Less than 5%",
                           `2` = "5 to 9 Percent",
                           `3` = "10 to 19 Percent",
                           `4` = "20 Percent or More")) %>%
  # Make census region more of a factor
  mutate(cenregf = recode(cenreg,
                          `1` = "Northeast",
                          `2` = "South",
                          `3` = "Midwest",
                          `4` = "West")) %>%
  mutate(west = ifelse(cenreg == 4, 1, 0)) %>%
  # %Black/Hispanic in ZIP code
  mutate(pblackhispzip = recode(zipblhi2,
                                `1` = "Less than 6 Percent",
                                `2` = "6 to 15 Percent",
                                `3` = "16 to 40 Percent",
                                `4` = "41 Percent or More")) %>% 
  mutate(pblackhisp16m = ifelse(zipblhi2 >= 3, 1, 0)) %>%
  # ZIP code classification
  mutate(zipclass = recode(ziplocl,
                           `11` = "City - Large",
                           `12` = "City - Midsize",
                           `13` = "City - Small",
                           `21` = "Suburb - Large",
                           `22` = "Suburb - Midsize",
                           `23` = "Suburb - Small",
                           `31` = "Town - Fringe",
                           `32` = "Town - Distant",
                           `33` = "Town - Remote",
                           `41` = "Rural - Fringe",
                           `42` = "Rural - Distant",
                           `43` = "Rural - Remote")) %>%
  # Questionnaire in English? On the Web?
  mutate(q_eng = ifelse(englspanx == 1, 1, 0),
         q_web = ifelse(modecomp == 1, 1, 0)) %>%
  # School/Grade classification
  mutate(schlgradf = recode(schlgrad,
                            `1` = "Early Childhood Programs",
                            `2` = "Elementary School",
                            `3` = "Middle/Junior High School",
                            `4` = "High School",
                            `5` = "Combined Grade School")) %>% 
  # Child's current grade, which you already did this with the other variable
  # Why are you doing this...
  mutate(gradef = recode(grade,
                         `2` = "Full-time Kindergarten",
                         `3` = "Part-time Kindergarten",
                         `4` = "First Grade",
                         `5` = "Second Grade",
                         `6` = "Third Grade",
                         `7` = "Fourth Grade",
                         `8` = "Fifth Grade",
                         `9` = "Sixth Grade",
                         `10` = "Seventh Grade",
                         `11` = "Eighth Grade",
                         `12` = "Ninth Grade",
                         `13` = "Tenth Grade",
                         `14` = "Eleventh Grade",
                         `15` = "Twelfth Grade")) %>% 
  # Total school enrollment size, more factor-y 
  mutate(enrollment = recode(s16numst,
                             `1` = "Under 300",
                             `2` = "300 - 599",
                             `3` = "600 - 999",
                             `4` = "1,000 - 2,499",
                             `5` = "2,500 or More")) %>%
  mutate(enroll599l = ifelse(s16numst <= 2, 1, 0)) %>%
  mutate(onlineclasses = ifelse(snetcrsx == 1, 1, 0)) %>%
  # Month of birth, which... why...
  mutate(birthmonth = month.name[cdobmm],
         birthmonth = paste0(birthmonth, " Birthday")) %>% 
  # Where was child born?
  # Knowing the U.S. and the distribution of the data, let's just make one "born outside 50 US states/DC variable"
  mutate(cbornoutus = ifelse(cplcbrth == 1, 0, 1)) %>%
  # Language at home
  mutate(clanghome = recode(cspeakx,
                            `1` = "Child is Unable to Speak",
                            `2` = "English",
                            `3` = "Spanish",
                            `4` = "Other Language",
                            `5` = "English & Spanish Equally",
                            `6` = "English & Other Language Equally")) %>%
  # intellectual disability variables. Jesus....
  # Convert them 2s to 0s.
  mutate_at(vars("disabltyx","disblty2x","hdspeechx","hddistrbx", "hddeafimx",
                 "hdorthox", "hdautismx", "hdpddx", "hdaddx", "hdlearnx",
                 "hddelayx","hdtrbrain","hdotherx"),
            ~ifelse(. == 2, 0, 1)) %>%
  # Started school in what month? Which... why...
  mutate(monthstarted = month.name[smvmth],
         monthstarted = paste0("Started School in ", monthstarted)) %>% 
  # same school beginning of the year, considered other schools?
  mutate(consdrothrsch = ifelse(sconsidr == 1, 1, 0),
         sameschoolby = ifelse(ssamsc == 1, 1, 0)) %>%
  # Done family shit at the school or whatever this is
  mutate_at(vars("fssportx","fsvol","fsmtng","fsptmtng","fsatcnfn",
                 "fsfundrs","fscommte","fscounslr"),
            ~ifelse(. == 2, 0, 1))  %>% 
  mutate(famactivschool = fssportx + fsvol + fsmtng + fsptmtng + fsatcnfn + fsfundrs + fscommte + fscounslr,
         z_famactivschool = r2sd(famactivschool)) %>%
  # Family activities variables holy shit this is exhausting. 
  mutate_at(vars("fostory2x","focrafts","fogames","fobuildx","fosport",
                 "forespon","fohistx","folibrayx","fobookstx","foconcrtx",
                 "fomuseumx","fozoox","fogroupx","fosprtevx"),
            ~ifelse(. == 2, 0, 1)) %>%
  mutate(famactiv = fostory2x + focrafts + fogames + fobuildx + fosport +
           forespon + fohistx + folibrayx + fobookstx + foconcrtx +
           fomuseumx + fozoox + fogroupx + fosprtevx,
         z_famactiv = r2sd(famactiv)) %>%
  # Household languages
  mutate_at(vars("hhenglish","hhspanish","hhfrench","hhchinese","hhothlang"),
            ~ifelse(. == 2, 0, 1)) %>%
  # Parent relation variable
  mutate(p1relation = recode(p1rel,
                            `1` = "Biological Parent",
                            `2` = "Adoptive Parent",
                            `3` = "Stepparent",
                            `4` = "Foster Parent",
                            `5` = "Grandparent",
                            `6` = "Other Guardian")) %>%
  # p1female
  mutate(p1female = ifelse(p1sex == 2, 1, 0)) %>%
  # Parent marital status
  mutate(p1marital = recode(p1mrsta,
                            `1` = "Married",
                            `2` = "Widowed",
                            `3` = "Divorced",
                            `4` = "Separated",
                            `5` = "Never Married")) %>%
  # Parent first language
  mutate(p1firstlang = recode(p1frlng,
                            `1` = "English",
                            `2` = "Spanish",
                            `3` = "Other Language",
                            `4` = "English and Spanish Equally",
                            `5` = "English and Another Language")) %>%
  # Language most spoken
  mutate(p1lngmspoke = recode(p1speak,
                              `1` = "English",
                              `2` = "Spanish",
                              `3` = "Other Language",
                              `4` = "English and Spanish Equally",
                              `5` = "English and Another Language")) %>%
  # Parent place of birth
  mutate(p1bornoutus = ifelse(p1plcbrth == 1, 0, 1)) %>%
  # Parent race/ethnicity
  mutate_at(vars("p1hispan","p1amind","p1asian","p1black","p1paci","p1white","p1hisprm"),
            ~ifelse(. == 2, 0, 1))  %>%
  mutate(p1nonwhite = ifelse(p1white == 0, 1, 0)) %>%
  # Parent's educational status.
  # Let's do this: keep the ordinal nature of the original data, and create a four-year college grad dummy
  mutate(collegeed = ifelse(p1educ >= 7, 1, 0)) %>%
  mutate(p1inschool = ifelse(p1enrl == 1, 1, 0)) %>% 
  # Employment status
  # This variable is going to be collinear with all the other shit in the model.
  mutate(p1emplystat = recode(p1empl,
                             `1` = "Employed",
                             `2` = "Self-Employed",
                             `3` = "Unemployed/Out of Work",
                             `4` = "Full-Time Student",
                             `5` = "Stay at Home Parent",
                             `6` = "Retired",
                             `7` = "Disabled/Unable to Work")) %>%
  mutate(p1unemployed = ifelse(p1empl == 3, 1, 0)) %>%
  # Second parent in household
  # Again, this will also be collinear with all the other shit in the model.
  mutate(secondparent = ifelse(p2guard == 1, 1, 0)) %>%
  # Internet access
  mutate(internet = recode(intacc,
                           `1` = "Home and Cell Phone",
                           `2` = "Home Only",
                           `3` = "Cell Phone Only",
                           `4` = "No Internet Access"),
         nointernet = ifelse(intacc == 4, 1, 0)) %>%
  # Family received welfare items
  mutate_at(vars("hwelftan","hwelfst","hwic","hfoodst","hmedicaid","hchip","hsecn8"),
            ~ifelse(. == 2, 0, 1)) %>%
  mutate(recpublicasst = ifelse(hwelftan == 1 | hwelfst == 1 | hwic == 1 |
                                  hfoodst == 1 | hmedicaid == 1 | hchip == 1 |
                                  hsecn8 == 1, 1, 0)) %>%
  # Other stuff
  mutate(moveattend = ifelse(sneighbrx == 1, 1, 0),
         z_p1age = r2sd(p1age),
         z_hhinc = r2sd(ttlhhinc),
         z_fodinnerx = r2sd(fodinnerx),
         homeownr = ifelse(ownrnthb == 1, 1, 0),
         firstchoice = ifelse(s1stchoi == 1, 1, 0)) -> Data

saveRDS(Data, "data.rds")


library(brms)
orddvs = c("fcschool","fcteachr","fcstds","fcorder","fcsupprt", "seenjoy")

Ord = lapply(setNames(orddvs,orddvs), function(var) {
  form = paste(var, "~ sc_cath + sc_otherrelig + sc_privnr + sc_charter +
            z_p1age + p1female + collegeed + cfemale + 
            p1nonwhite + z_hhinc + cbornoutus + p1bornoutus + recpublicasst + 
            moveattend + consdrothrsch +
            z_famactiv + z_famactivschool + z_fodinnerx + 
            enroll599l + pblackhisp16m")
  brm(form, data=Data, family=cumulative("logit"))
})

saveRDS(Ord, "ord.rds")



bindvs = c("fcschool_d","fcteachr_d","fcstds_d","fcorder_d","fcsupprt_d", "seenjoy_d")

Bin = lapply(setNames(bindvs,bindvs), function(var) {
  form = paste(var, "~ sc_cath + sc_otherrelig + sc_privnr + sc_charter +
            z_p1age + p1female + collegeed + cfemale + 
            p1nonwhite + z_hhinc + cbornoutus + p1bornoutus + recpublicasst + 
            moveattend + consdrothrsch +
            z_famactiv + z_famactivschool + z_fodinnerx + 
            enroll599l + pblackhisp16m")
  brm(form, data=Data, family=binomial(link="logit"))
})

saveRDS(Bin, "bin.rds")

library(modelr)
data_grid(Data, sc_charter, .model=M1) %>%
  na.omit %>%
  mutate_at(vars(contains("z_")), list(~replace(., . != 0, 0))) -> newdat

newdat %>% 
  bind_rows(.,newdat[1, 1:ncol(.)]) %>%
  mutate(sc_cath = ifelse(row_number() %in% 3, 1, 0)) %>%
  bind_rows(.,newdat[1, 1:ncol(.)]) %>%
  mutate(sc_otherrelig = ifelse(row_number() %in% 4, 1, 0)) %>%
  bind_rows(.,newdat[1, 1:ncol(.)]) %>%
  mutate(sc_privnr = ifelse(row_number() %in% 5, 1, 0)) -> newdat

library(tidybayes)


sims_bin = tibble()

for(i in 1:length(Bin)) {
  newdat %>%
    add_fitted_draws(Bin[[i]]) %>%
    mean_qi(.value) %>%
    ungroup() %>%
    select(.row:ncol(.), sc_charter:sc_privnr) %>%
    mutate(dv = names(Bin[i])) -> Temp
  sims_bin = bind_rows(Temp, sims_bin)
    
}


sims_ord = tibble()

for(i in 1:length(Ord)) {
newdat %>%
  add_fitted_draws(Ord[[i]]) %>%
  mean_qi(.value) %>%
  ungroup() %>%
  select(.row:ncol(.), sc_charter:sc_privnr) %>%
  mutate(dv = names(Ord[i])) -> Temp
  sims_ord = bind_rows(Temp, sims_ord)
}

list("sims_bin" = sims_bin,
     "sims_ord" = sims_ord) -> Sims 

saveRDS(Sims,"data/sims.rds")
