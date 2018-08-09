
#### assemble data for linear probability models ####

# get data on state level unemployment rate
quandl_api_key('DLk9RQrfTVkD4UTKc7op')

df.state.UR.raw <-
    c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
      "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
      "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
      "UT", "VA", "VT", "WA", "WI", "WV", "WY") %>%
    paste0("UR") %>%
    tq_get(get = "economic.data", from = "1976-01-01", to = "2018-06-30")

df.state.UR <-
    df.state.UR.raw %>%
    rename(state.code = symbol,
           u3.rate = price) %>%
    mutate(date = 100*year(date) + month(date),
           state.code = str_sub(state.code, 1, 2)) %>%
    select(date, state.code, u3.rate)

# help wanted index
df.hwi <- read_tsv(paste0(mdir.cps, "data/HWI_index_updated2016m12.txt"), col_types = c("id"))

df.state.codes <- read_csv(paste0(mdir.cps, "data/state_codes.csv"), col_types = c("iicc")) %>%
    select(gestcen, state.code)


load(file = paste0(edir.cps, "merged_2m_all.Rdata"))

# df.lpm.data.occ <-
#     df.merged.2m.all.sample
#
# rm(df.merged.2m.all.sample)

df.lpm.data.occ <-
    # df.lpm.data.occ %>%
    df.merged.2m.all %>%
    filter(age.1 >= 16 & age.1 <= 75) %>%
    filter(!(occ1cat.1 %in% c("FRM","MIL") | occ1cat.2 %in% c("FRM","MIL"))) %>%
    select(period.1, gestcen, age.1, educ.1, female, white, married.1, lfs.1, occ1cat.1, udur.1, weight.1, lfs.2, occ1cat.2) %>%
    unite(status.1, lfs.1, occ1cat.1, sep = ".") %>%
    unite(status.2, lfs.2, occ1cat.2, sep = ".") %>%
    filter(!(status.1 == "E.X")) %>%
    left_join(df.state.codes, by = "gestcen") %>%
    left_join(df.state.UR, by = c("state.code", "period.1" = "date")) %>%
    left_join(df.hwi, by = c("period.1" = "date")) %>%
    mutate(# dependent variables
           y.E.RM  = (status.2 == "E.RM") %>% as.integer(),
           y.E.RC  = (status.2 == "E.RC") %>% as.integer(),
           y.E.NRM = (status.2 == "E.NRM") %>% as.integer(),
           y.E.NRC = (status.2 == "E.NRC") %>% as.integer(),
           y.U.RM  = (status.2 == "U.RM") %>% as.integer(),
           y.U.RC  = (status.2 == "U.RC") %>% as.integer(),
           y.U.NRM = (status.2 == "U.NRM") %>% as.integer(),
           y.U.NRC = (status.2 == "U.NRC") %>% as.integer(),
           y.I     = (status.2 == "I.X") %>% as.integer(),
           # age categories
           agecat.1 = case_when(age.1 %in% c(16:19) ~ 1L,
                                age.1 %in% c(20:24) ~ 2L,
                                age.1 %in% c(25:34) ~ 3L,
                                age.1 %in% c(35:44) ~ 4L,
                                age.1 %in% c(45:54) ~ 5L,
                                age.1 %in% c(55:64) ~ 6L,
                                age.1 %in% c(65:75) ~ 7L,
                                TRUE              ~ NA_integer_) %>% factor() %>% relevel(ref = 5),
           # education categories
           educat.1 = case_when(educ.1 == 1        ~ "hsd",
                                educ.1 %in% c(2:3) ~ "hsg",
                                educ.1 %in% c(4:5) ~ "clp") %>%factor() %>% relevel(ref = "hsg"),
           male = 1L - female,
           nwhite = 1L - white,
           # unemployment duration bins
           udurcat.1 = case_when(udur.1 %in% c(0:4)                ~ 1L,
                                 udur.1 %in% c(5:8)                ~ 2L,
                                 udur.1 %in% c(9:12)               ~ 3L,
                                 udur.1 %in% c(13:16)              ~ 4L,
                                 udur.1 %in% c(17:20)              ~ 5L,
                                 udur.1 %in% c(21:24)              ~ 6L,
                                 udur.1 %in% c(25:28)              ~ 7L,
                                 udur.1 %in% c(29:32)              ~ 8L,
                                 udur.1 %in% c(33:40)              ~ 9L,
                                 #udur.1 %in% c(41:68)              ~ 10L,
                                 #udur.1 >= 69                      ~ 11L
                                 #udur.1 %in% c(69:96)              ~ 11L,
                                 udur.1 %in% c(41:52)              ~ 10L,
                                 udur.1 %in% c(53:96)              ~ 11L,
                                 udur.1 >= 97                      ~ 12L,
                                 TRUE                              ~ NA_integer_) %>% as.factor(),
           # state
           state.code = state.code %>% as.factor(),
           # seasonal dummies
           month.1 = (period.1 %% 100)  %>% as.factor(),
           # recession/expansion id
           cycle.id = case_when(period.1 %in% c(197601:197912) ~ "E1",
                                period.1 %in% c(198001:198007) ~ "R1",
                                period.1 %in% c(198008:198106) ~ "E2",
                                period.1 %in% c(198107:198211) ~ "R2",
                                period.1 %in% c(198212:199006) ~ "E3",
                                period.1 %in% c(199007:199103) ~ "R3",
                                period.1 %in% c(199104:200102) ~ "E4",
                                period.1 %in% c(200103:200111) ~ "R4",
                                period.1 %in% c(200112:200711) ~ "E5",
                                period.1 %in% c(200712:200906) ~ "R5",
                                period.1 %in% c(200907:201212) ~ "E6",
                                TRUE                           ~ NA_character_)) %>%
    select(-c(gestcen, age.1, educ.1, female, white, udur.1, status.2))

rm(df.merged.2m.all)

df.lpm.data.occ <-
    bind_rows(df.lpm.data.occ,
              df.lpm.data.occ %>%
                  filter(period.1 >= 201301) %>%
                  mutate(cycle.id = "E6.since.2013"),
              df.lpm.data.occ %>%
                  filter(period.1 >= 200907) %>%
                  mutate(cycle.id = "E6.whole")) %>%
    filter(!is.na(cycle.id))

save(df.lpm.data.occ, file = paste0(edir.cps, "lpm_data_occ.Rdata"))
# load(file = paste0(edir.cps, "lpm_data_occ.Rdata"))



##### estimate linear probability models ####

# df.tmp <-
#     df.lpm.data.occ %>%
#     filter(str_sub(cycle.id,1,1) == "E") %>%
#     mutate(dep = list(occ1 = list("RM","RC")),
#            spec = modify_depth(dep, 2, ~formula(paste0("y.E.",.," ~ 1"))),
#            lpm.E = map2(data, spec, ~modify(.y, lm, data = .x))
#            )

# right hand side for linear probability models
# add: hwi, state.code, u3rate
spec.rhs <- c(U.E = "male + nwhite + married.1 + agecat.1 + educat.1 + udurcat.1 + month.1",
              E.U = "male + nwhite + married.1 + agecat.1 + educat.1 + month.1")

df.lpm.results.occ <-
    df.lpm.data.occ %>%
    nest(-cycle.id) %>%
    # filter(str_sub(cycle.id,1,1) == "E") %>%
    mutate(lpm.U.E.RM.RM = map(data, ~lm(paste("y.E.RM ~", spec.rhs["U.E"]), weight = weight.1, data = .x %>% filter(status.1 == "U.RM"))),
           lpm.U.E.RC.RC = map(data, ~lm(paste("y.E.RC ~", spec.rhs["U.E"]), weight = weight.1, data = .x %>% filter(status.1 == "U.RC"))),
           lpm.U.E.NRM.NRM = map(data, ~lm(paste("y.E.NRM ~", spec.rhs["U.E"]), weight = weight.1, data = .x %>% filter(status.1 == "U.NRM"))),
           lpm.U.E.NRC.NRC = map(data, ~lm(paste("y.E.NRC ~", spec.rhs["U.E"]), weight = weight.1, data = .x %>% filter(status.1 == "U.NRC"))),
           lpm.E.U.RM.RM = map(data, ~lm(paste("y.U.RM ~", spec.rhs["E.U"]), weight = weight.1, data = .x %>% filter(status.1 == "E.RM"))),
           lpm.E.U.RC.RC = map(data, ~lm(paste("y.U.RC ~", spec.rhs["E.U"]), weight = weight.1, data = .x %>% filter(status.1 == "E.RC"))),
           lpm.E.U.NRM.NRM = map(data, ~lm(paste("y.U.NRM ~", spec.rhs["E.U"]), weight = weight.1, data = .x %>% filter(status.1 == "E.NRM"))),
           lpm.E.U.NRC.NRC = map(data, ~lm(paste("y.U.NRC ~", spec.rhs["E.U"]), weight = weight.1, data = .x %>% filter(status.1 == "E.NRC")))) %>%
    select(-data)

# save(df.lpm.results.occ, file = paste0(edir.cps, "lpm_results_occ.Rdata"))



#### plot estimated coefficients with their confidence intervals ####

df.lpm.results.occ.coefs <-
    df.lpm.lresults.occ %>%
    gather(status.trans, lpm.model, starts_with("lpm.")) %>%
    mutate(status.trans = str_sub(status.trans, 5, -1),
           lpm.coefs = map(lpm.model, tidy, conf.int = TRUE)) %>%
    select(-lpm.model) %>%
    unnest() %>%
    mutate(term = if_else(str_sub(term, 1, 7) == "udurcat" & str_length(term) == 10, paste0(str_sub(term, 1, 9),"0",str_sub(term, 10, 10)), term))

save(df.lpm.results.occ.coefs, file = paste0(edir.cps, "lpm_results_occ_coefs.Rdata"))

df.lpm.results.occ.coefs %>%
    # filter(str_sub(cycle.id, 1, 1) == "E") %>%
    # filter(cycle.id != "E2") %>%
    # filter(!(cycle.id %in% c("E6", "E6.since.2013"))) %>%
    filter(cycle.id == "E1" | str_sub(cycle.id, 1, 1) == "R") %>%
    # filter(str_sub(status.trans, 1, 3) == "E.U") %>%
    filter(str_sub(status.trans, 1, 3) == "U.E") %>%
    filter(str_sub(term, 1, 5) != "month") %>%
    filter(str_sub(term, 1, 7) != "udurcat") %>%
    # filter(term %in% c("(Intercept)","married.1","nwhite.1","male.1")) %>%
    ggplot(aes(x =  estimate, y = cycle.id, xmin = conf.low, xmax = conf.high, col = cycle.id, height = 0.2)) +
        geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        facet_grid(term ~ status.trans, as.table = FALSE)

df.lpm.results.occ.coefs %>%
    # filter(str_sub(cycle.id, 1, 1) == "E") %>%
    # filter(cycle.id != "E2") %>%
    # filter(!(cycle.id %in% c("E6", "E6.since.2013"))) %>%
    filter(cycle.id == "E1" | str_sub(cycle.id, 1, 1) == "R") %>%
    # filter(str_sub(status.trans, 1, 3) == "E.U") %>%
    filter(str_sub(status.trans, 1, 3) == "U.E") %>%
    filter(str_sub(term, 1, 7) == "udurcat") %>%
    ggplot(aes(x =  estimate, y = cycle.id, xmin = conf.low, xmax = conf.high, col = cycle.id, height = 0.2)) +
        geom_point() +
        geom_errorbarh() +
        geom_vline(xintercept = 0, linetype = "dotted") +
        facet_grid(term ~ status.trans, as.table = FALSE)
