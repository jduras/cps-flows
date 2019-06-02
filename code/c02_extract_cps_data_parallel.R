
# extracts CPS data from raw ASCII data files


# get state names and codes, FIPS and Census
df_state_codes <-
    read_csv(str_c(mdir_cps, "data/state_codes.csv"), col_types = c("iicc")) %>%
    rename(GESTFIPS = gestfips,
           GESTCEN = gestcen) %>%
    select(GESTCEN, GESTFIPS)


# mapping of occupation codes to routine/non-routine and congnitive/manual groups
occ_grp <- list(
    Y1970 = list(NRC = c(1:100, 102:162, 165, 171, 173:216, 222:225, 230, 235:245, 321, 326, 363, 382, 426, 506, 801:802, 924, 926),
                 NRM = c(101, 505, 740, 755, 901:923, 925, 931:984),
                 RC  = c(220, 231:233, 260:285, 301:305, 310:320, 323:325, 330:362, 364:381, 383:395),
                 RM  = c(163:164, 170, 172, 221, 226, 401:425, 430:446, 452:504, 510:575, 601:624, 626:715, 750:751, 753:754, 760, 762:785),
                 FRM = c(450, 600, 625, 752, 761, 821:846),
                 MIL = c(580)),
    Y1980 = list(NRC = c(3:225, 228:229, 234:235, 473:476),
                 NRM = c(403:469, 486:487, 773),
                 RC  = c(243:389),
                 RM  = c(226:227, 233, 503:769, 774:799, 803:869, 873:889),
                 FRM = c(477:485, 488:499),
                 MIL = c(905)),
    Y2000 = list(NRC = c(10:3540),
                 NRM = c(3600:4650),
                 RC  = c(4700:5930),
                 RM  = c(6200:9750),
                 FRM = c(6000:6130),
                 MIL = c(9800:9840)),
    Y2010 = list(NRC = c(10:3540),
                 NRM = c(3600:4650),
                 RC  = c(4700:5940),
                 RM  = c(6200:9750),
                 FRM = c(6005:6130),
                 MIL = c(9800:9840)))


# extract CPS data from raw ASCII data files

# changes in CPS survey design
# see Nekarda (2009) NBER wp
# mark start.period end.period
#  0 197601 197712
#  1 197801 198112
#  2 198201 198212
#  3 198301 198312
#  4 198401 198506
#  5 198507 198512
#  6 198601 198812
#  7 198901 199112
#  8 199201 199312
#  9 199401 199403
# 10 199404 199505
# 11 199506 199508
# 12 199509 199712
# 13 199801 200212
# 14 200301 200404
# 15 200405 200507
# 16 200508 200612
# 17 200701 200712

tic()

message(str_c("Extracting relevant data from CPS ", tfst, " to ", tlst))

future_walk(
    .progress = TRUE,
    .x = tseq,
    .f = function(t = .x) {

        # message(str_c(" process ID: ", str_pad(Sys.getpid(), 6, "left"), "   date: ", t))

        if (t <= 197712) {
            df_cpsdata <-
                read_fwf(file = str_c(ddir_cps, "b", t),
                         col_positions = fwf_cols(HRINTSTA = 1, HRMIS = 2, hh0 = c(4, 8), hh1 = c(9, 12), GESTCEN = c(17, 18), hh2 = c(25, 26), PRUNEDUR = c(66, 67),
                                                  # PEIO1ICD = c(88, 90),
                                                  PEIO1OCD = c(91, 93),
                                                  PULINENO = c(94, 95), PRTAGE = c(97, 98), PRMARSTA = 99, PTDTRACE = 100, PESEX = 101,
                                                  HGA = c(103, 104), HGC = 105, PEMLR = 109, PWSSWGT = c(121, 132),
                                                  PRMJIND1 = c(215, 216)),
                         # PRMJOCC1 = c(225, 226), PRDTOCC1 = c(227, 228)),
                         col_types = cols(HRINTSTA = "i", HRMIS = "i", hh0 = "c", hh1 = "c", GESTCEN = "i", hh2 = "c", PRUNEDUR = "c",
                                          # PEIO1ICD = "c",
                                          PEIO1OCD = "c",
                                          PULINENO = "i", PRTAGE = "i", PRMARSTA = "i", PTDTRACE = "i", PESEX = "i",
                                          HGA = "i", HGC = "i", PEMLR = "i", PWSSWGT = "d",
                                          PRMJIND1 = "i")
                         # PRMJOCC1 = "i", PRDTOCC1 = "i")
                ) %>%
                mutate(# PEIO1ICD = PEIO1ICD %>% str_replace_all("---", NA_character_) %>% as.numeric(),
                    PEIO1OCD = PEIO1OCD %>% str_replace_all("---", NA_character_) %>% as.numeric(),
                    PRUNEDUR = PRUNEDUR %>% str_replace_all("--", NA_character_) %>% str_replace_all("-", "") %>% as.numeric(),
                    HID = str_c(hh0, hh1, hh2)) %>%
                select(-c(hh0, hh1, hh2))
        }

        if ((t >= 197801) && (t <= 198212)) {
            df_cpsdata <-
                read_fwf(file = str_c(ddir_cps, "b", t),
                         col_positions = fwf_cols(HRINTSTA = 1, HRMIS = 2, HID = c(4, 15), GESTCEN = c(17, 18), PRUNEDUR = c(66, 67),
                                                  # PEIO1ICD = c(88,90),
                                                  PEIO1OCD = c(91, 93),
                                                  PULINENO = c(94, 95), PRTAGE = c(97, 98), PRMARSTA = 99, PTDTRACE = 100, PESEX = 101,
                                                  HGA = c(103, 104), HGC = 105, PEMLR = 109, PWSSWGT = c(121, 132),
                                                  PRMJIND1 = c(215, 216)),
                         # PRMJOCC1 = c(225,226), PRDTOCC1 = c(227,228)),
                         col_types = cols(HRINTSTA = "i", HRMIS = "i", HID = "c", GESTCEN = "i", PRUNEDUR = "c",
                                          # PEIO1ICD = "c",
                                          PEIO1OCD = "c",
                                          PULINENO = "i", PRTAGE = "i", PRMARSTA = "i", PTDTRACE = "i", PESEX = "i",
                                          HGA = "i", HGC = "i", PEMLR = "i", PWSSWGT = "d",
                                          PRMJIND1 = "i")
                         #, PRMJOCC1 = "i", PRDTOCC1 = "i")
                ) %>%
                mutate(# PEIO1ICD = PEIO1ICD %>% str_replace_all("---", NA_character_) %>% as.numeric(),
                    PEIO1OCD = PEIO1OCD %>% str_replace_all("---", NA_character_) %>% as.numeric(),
                    PRUNEDUR = PRUNEDUR %>% str_replace_all("--", NA_character_) %>% str_replace_all("-", "") %>% as.numeric())
        }

        if ((t >= 198301) && (t <= 198312)) {
            df_cpsdata <-
                read_fwf(file = str_c(ddir_cps, "b", t),
                         col_positions = fwf_cols(  HRINTSTA = 1, HRMIS = 2, HID = c(4, 15), GESTCEN = c(17, 18), PRUNEDUR = c(66, 67),
                                                    PULINENO = c(94, 95), PRTAGE = c(97, 98), PRMARSTA = 99, PTDTRACE = 100, PESEX = 101,
                                                    HGA = c(103, 104), HGC = 105, PEMLR = 109, PWSSWGT = c(121, 132),
                                                    # PRMJOCC1 = c(516,517),
                                                    PRMJIND1 = c(518, 519),
                                                    # PRDTOCC1 = c(520,521), PEIO1ICD = c(524,526),
                                                    PEIO1OCD = c(527, 529)),
                         col_types = cols(HRINTSTA = "i", HRMIS = "i", HID = "c", GESTCEN = "i", PRUNEDUR = "c",
                                          PULINENO = "i", PRTAGE = "i", PRMARSTA = "i", PTDTRACE = "i", PESEX = "i",
                                          HGA = "i", HGC = "i", PEMLR = "i", PWSSWGT = "d",
                                          # PRMJOCC1 = "i",
                                          PRMJIND1 = "i",
                                          # PRDTOCC1 = "i", PEIO1ICD = "i",
                                          PEIO1OCD = "i")) %>%
                mutate(PRUNEDUR = PRUNEDUR %>% str_replace_all("--", NA_character_) %>% str_replace_all("-", "") %>% as.numeric())
        }

        if ((t >= 198401) && (t <= 198812)) {
            df_cpsdata <-
                read_fwf(file = str_c(ddir_cps, "b", t),
                         col_positions = fwf_cols(HRINTSTA = 1, HRMIS = 2, HID = c(4, 15), GESTCEN = c(17, 18), PRUNEDUR = c(66, 67),
                                                  PRTAGE = c(97, 98), PRMARSTA = 99, PTDTRACE = 100, PESEX = 101,
                                                  HGA = c(103, 104), HGC = 105, PEMLR = 109, PWSSWGT = c(121, 132),
                                                  # PRMJOCC1 = c(516,517),
                                                  PRMJIND1 = c(518, 519),
                                                  # PRDTOCC1 = c(520,521), PEIO1ICD = c(524,526),
                                                  PEIO1OCD = c(527, 529), PULINENO = c(541, 542)),
                         col_types = cols(HRINTSTA = "i", HRMIS = "i", HID = "c", GESTCEN = "i", PRUNEDUR = "c",
                                          PRTAGE = "i", PRMARSTA = "i", PTDTRACE = "i", PESEX = "i",
                                          HGA = "i", HGC = "i", PEMLR = "i", PWSSWGT = "d",
                                          # PRMJOCC1 = "i",
                                          PRMJIND1 = "i",
                                          # PRDTOCC1 = "i", PEIO1ICD = "i",
                                          PEIO1OCD = "i", PULINENO = "i")) %>%
                mutate(PRUNEDUR = PRUNEDUR %>% str_replace_all("--", NA_character_) %>% str_replace_all("-", "") %>% as.numeric())
        }

        if ((t >= 198901) && (t <= 199112)) {
            df_cpsdata <-
                read_fwf(file = str_c(ddir_cps, "b", t),
                         col_positions = fwf_cols(HRINTSTA = 69, HRMIS = 70, GESTCEN = c(114, 115), GESTFIPS = c(118, 119), HID = c(145, 156),
                                                  PULINENO = c(264, 265), PRTAGE = c(270, 271), PRMARSTA = 272, PESEX = 275,
                                                  HGA = c(277, 278), HGC = 279, PTDTRACE = 280, PRUNEDUR = c(304, 305),
                                                  # PEIO1ICD = c(310,312),
                                                  PEIO1OCD = c(313, 315), PEMLR = 348,
                                                  PRMJIND1 = c(366, 367),
                                                  # PRMJOCC1 = c(370,371), PRDTOCC1 = c(372,373),
                                                  DSCWK = 376, PWSSWGT = c(398, 405)),
                         col_types = cols(HRINTSTA = "i", HRMIS = "i", GESTCEN = "i", GESTFIPS = "i", HID = "c",
                                          PULINENO = "i", PRTAGE = "i", PRMARSTA = "i", PESEX = "i",
                                          HGA = "i", HGC = "i", PTDTRACE = "i", PRUNEDUR = "i",
                                          # PEIO1ICD = "i",
                                          PEIO1OCD = "i", PEMLR = "i",
                                          PRMJIND1 = "i",
                                          # PRMJOCC1 = "i", PRDTOCC1 = "i",
                                          DSCWK = "i", PWSSWGT = "d"))
        }

        if ((t >= 199201) && (t <= 199312)) {
            df_cpsdata <-
                read_fwf(file = str_c(ddir_cps, "b", t),
                         col_positions = fwf_cols(HRINTSTA = 69, HRMIS = 70, GESTCEN = c(114, 115), GESTFIPS = c(118, 119), HID = c(145, 156),
                                                  PULINENO = c(264, 265), PRTAGE = c(270, 271), PRMARSTA = 272, PESEX = 275,
                                                  PEEDUCA = c(277, 278), PTDTRACE = 280, PRUNEDUR = c(304, 305),
                                                  # PEIO1ICD = c(310,312),
                                                  PEIO1OCD = c(313, 315), PEMLR = 348,
                                                  PRMJIND1 = c(366, 367),
                                                  # PRMJOCC1 = c(370,371), PRDTOCC1 = c(372,373),
                                                  DSCWK = 376, PWSSWGT = c(398, 405)),
                         col_types = cols(HRINTSTA = "i", HRMIS = "i", GESTCEN = "i", GESTFIPS = "i", HID = "c",
                                          PULINENO = "i", PRTAGE = "i", PRMARSTA = "i", PESEX = "i",
                                          PEEDUCA = "i", PTDTRACE = "i", PRUNEDUR = "i",
                                          # PEIO1ICD = "i",
                                          PEIO1OCD = "i", PEMLR = "i",
                                          PRMJIND1 = "i",
                                          # PRMJOCC1 = "i", PRDTOCC1 = "i",
                                          DSCWK = "i", PWSSWGT = "d"))
        }

        if (t >= 199401) {
            if (t <= 199508) {
                df_cpsdata <-
                    read_fwf(file = str_c(ddir_cps, "b", t),
                             col_positions = fwf_cols(HRHHID = c(1, 12), HRINTSTA = c(57, 58), HRMIS = c(63, 64), HRSAMPLE = c(71, 74), HRSERSUF = c(75, 76), HUHHNUM = c(77, 78),
                                                      GESTCEN = c(91, 92), GESTFIPS = c(93, 94), PRTAGE = c(122, 123), PESEX = c(129, 130), PEEDUCA = c(137, 138), PTDTRACE = c(139, 140),
                                                      PULINENO = c(147, 148), PRMARSTA = c(159, 160), PEMLR = c(180, 181), PRDISC = c(389, 390), PRUNEDUR = c(407, 409),
                                                      # PEIO1ICD = c(436,438),
                                                      PEIO1OCD = c(439, 441),
                                                      # PEIO2OCD = c(449,451), PRDTOCC1 = c(476,477), PRDTOCC2 = c(478,479),
                                                      PRMJIND1 = c(482, 483),
                                                      # PRMJOCC1 = c(486,487), PRMJOCC2 = c(488,489),
                                                      PWSSWGT = c(613, 622)),
                             col_types = cols(HRHHID = "c", HRINTSTA = "i", HRMIS = "i", HRSAMPLE = "c", HRSERSUF = "c", HUHHNUM = "c",
                                              GESTCEN = "i", GESTFIPS = "i", PRTAGE = "i", PESEX = "i", PEEDUCA = "i", PTDTRACE = "i",
                                              PULINENO = "i", PRMARSTA = "i", PEMLR = "i", PRDISC = "i", PRUNEDUR = "i",
                                              # PEIO1ICD = "i",
                                              PEIO1OCD = "i",
                                              # PEIO2OCD = "i", PRDTOCC1 = "i", PRDTOCC2 = "i",
                                              PRMJIND1 = "i",
                                              # PRMJOCC1 = "i", PRMJOCC2 = "i",
                                              PWSSWGT = "d"))
            }
            if ((t >= 199509) && (t <= 199712)) {
                df_cpsdata <-
                    read_fwf(file = str_c(ddir_cps, "b", t),
                             col_positions = fwf_cols(HRHHID = c(1, 15), HRINTSTA = c(57, 58), HRMIS = c(63, 64), HRSAMPLE = c(71, 74), HRSERSUF = c(75, 76), HUHHNUM = c(77, 78),
                                                      GESTCEN = c(91, 92), GESTFIPS = c(93, 94), PRTAGE = c(122, 123), PESEX = c(129, 130), PEEDUCA = c(137, 138), PTDTRACE = c(139, 140),
                                                      PULINENO = c(147, 148), PRMARSTA = c(159, 160), PEMLR = c(180, 181), PRDISC = c(389, 390), PRUNEDUR = c(407, 409),
                                                      # PEIO1ICD = c(436,438),
                                                      PEIO1OCD = c(439, 441),
                                                      # PEIO2OCD = c(449,451), PRDTOCC1 = c(476,477), PRDTOCC2 = c(478,479),
                                                      PRMJIND1 = c(482, 483),
                                                      # PRMJOCC1 = c(486,487), PRMJOCC2 = c(488,489),
                                                      PWSSWGT = c(613, 622)),
                             col_types = cols(HRHHID = "c", HRINTSTA = "i", HRMIS = "i", HRSAMPLE = "c", HRSERSUF = "c", HUHHNUM = "c",
                                              GESTCEN = "i", GESTFIPS = "i", PRTAGE = "i", PESEX = "i", PEEDUCA = "i", PTDTRACE = "i",
                                              PULINENO = "i", PRMARSTA = "i", PEMLR = "i", PRDISC = "i", PRUNEDUR = "i",
                                              # PEIO1ICD = "i",
                                              PEIO1OCD = "i",
                                              # PEIO2OCD = "i", PRDTOCC1 = "i", PRDTOCC2 = "i",
                                              PRMJIND1 = "i",
                                              # PRMJOCC1 = "i", PRMJOCC2 = "i",
                                              PWSSWGT = "d"))
            }
            if ((t >= 199801) && (t <= 199912)) {
                df_cpsdata <-
                    read_fwf(file = str_c(ddir_cps, "b", t),
                             col_positions = fwf_cols(HRHHID = c(1, 15), HRINTSTA = c(57, 58), HRMIS = c(63, 64), HRSAMPLE = c(71, 74), HRSERSUF = c(75, 76), HUHHNUM = c(77, 78),
                                                      GESTCEN = c(91, 92), GESTFIPS = c(93, 94), PRTAGE = c(122, 123), PESEX = c(129, 130), PEEDUCA = c(137, 138), PTDTRACE = c(139, 140),
                                                      PULINENO = c(147, 148), PRMARSTA = c(159, 160), PEMLR = c(180, 181), PRDISC = c(389, 390), PRUNEDUR = c(407, 409),
                                                      # PEIO1ICD = c(436,438),
                                                      PEIO1OCD = c(439, 441),
                                                      # PEIO2OCD = c(449,451), PRDTOCC1 = c(476,477), PRDTOCC2 = c(478,479),
                                                      PRMJIND1 = c(482, 483),
                                                      # PRMJOCC1 = c(486,487), PRMJOCC2 = c(488,489),
                                                      PWSSWGT = c(613, 622), PWCMPWGT = c(846, 855)),
                             col_types = cols(HRHHID = "c", HRINTSTA = "i", HRMIS = "i", HRSAMPLE = "c", HRSERSUF = "c", HUHHNUM = "c",
                                              GESTCEN = "i", GESTFIPS = "i", PRTAGE = "i", PESEX = "i", PEEDUCA = "i", PTDTRACE = "i",
                                              PULINENO = "i", PRMARSTA = "i", PEMLR = "i", PRDISC = "i", PRUNEDUR = "i",
                                              # PEIO1ICD = "i",
                                              PEIO1OCD = "i",
                                              # PEIO2OCD = "i", PRDTOCC1 = "i", PRDTOCC2 = "i",
                                              PRMJIND1 = "i",
                                              #PRMJOCC1 = "i", PRMJOCC2 = "i",
                                              PWSSWGT = "d", PWCMPWGT = "d"))
            }
            if ((t >= 200001) && (t <= 200212)) {
                df_cpsdata_initial <-
                    read_fwf(file = str_c(ddir_cps, "b", t),
                             col_positions = fwf_cols(HRHHID = c(1, 15), HRINTSTA = c(57, 58), HRMIS = c(63, 64), HRSAMPLE = c(71, 74), HRSERSUF = c(75, 76), HUHHNUM = c(77, 78),
                                                      GESTCEN = c(91, 92), GESTFIPS = c(93, 94), PRTAGE = c(122, 123), PESEX = c(129, 130), PEEDUCA = c(137, 138), PTDTRACE = c(139, 140),
                                                      PULINENO = c(147, 148), PRMARSTA = c(159, 160), PEMLR = c(180, 181), PRDISC = c(389, 390), PRUNEDUR = c(407, 409),
                                                      # PEIO1ICD = c(436,438),
                                                      PEIO1OCD = c(439, 441),
                                                      # PEIO2OCD = c(449,451), PRDTOCC1 = c(476,477), PRDTOCC2 = c(478,479),
                                                      PRMJIND1 = c(482, 483),
                                                      # PRMJOCC1 = c(486,487), PRMJOCC2 = c(488,489),
                                                      PWSSWGT = c(613, 622), QSTNUM = c(815, 819), OCCURNUM = c(820, 821), PWCMPWGT = c(846, 855)),
                             col_types = cols(HRHHID = "c", HRINTSTA = "i", HRMIS = "i", HRSAMPLE = "c", HRSERSUF = "c", HUHHNUM = "c",
                                              GESTCEN = "i", GESTFIPS = "i", PRTAGE = "i", PESEX = "i", PEEDUCA = "i", PTDTRACE = "i",
                                              PULINENO = "i", PRMARSTA = "i", PEMLR = "i", PRDISC = "i", PRUNEDUR = "i",
                                              # PEIO1ICD = "i",
                                              PEIO1OCD = "i",
                                              # PEIO2OCD = "i", PRDTOCC1 = "i", PRDTOCC2 = "i",
                                              PRMJIND1 = "i",
                                              # PRMJOCC1 = "i", PRMJOCC2 = "i",
                                              PWSSWGT = "d", QSTNUM = "i", OCCURNUM = "i", PWCMPWGT = "d"))
                df_cpsdata_revised <-
                    read_fwf(file = str_c(ddir_cps, "revised/b", t, "revised"),
                             col_positions = fwf_cols(QSTNUM = c(1, 5), OCCURNUM = c(12, 13),
                                                      # NEIO1ICD = c(14,17),
                                                      NRDTIND1 = c(40, 41),
                                                      NRDTOCC1 = c(44, 45),
                                                      # NRDTOCC2 = c(46,47),
                                                      # NRMJIND1 = c(50,51), NRMJOCC1 = c(54,55), NRMJOCC2 = c(56,57), NRMJOCGR = c(58,59),
                                                      NWCMPWGT = c(74, 83), NWSSWGT = c(94, 103)),
                             col_types = cols(QSTNUM = "i", OCCURNUM = "i",
                                              # NEIO1ICD = "i",
                                              NRDTIND1 = "i",
                                              NRDTOCC1 = "i",
                                              # NRDTOCC2 = "i",
                                              # NRMJIND1 = "i", NRMJOCC1 = "i", NRMJOCC2 = "i", NRMJOCGR = "i",
                                              NWCMPWGT = "d", NWSSWGT = "d"))
                df_cpsdata <- inner_join(df_cpsdata_initial, df_cpsdata_revised, by = c("QSTNUM","OCCURNUM"))
            }
            if ((t >= 200301) && (t <= 200404)) {
                df_cpsdata <-
                    read_fwf(file = str_c(ddir_cps, "b", t),
                             col_positions = fwf_cols(HRHHID = c(1, 15), HRINTSTA = c(57, 58), HRMIS = c(63, 64), HRSAMPLE = c(71, 74), HRSERSUF = c(75, 76), HUHHNUM = c(77, 78),
                                                      GESTCEN = c(91, 92), GESTFIPS = c(93, 94), PRTAGE = c(122, 123), PESEX = c(129, 130), PEEDUCA = c(137, 138), PTDTRACE = c(139, 140),
                                                      PULINENO = c(147, 148), PRMARSTA = c(159, 160), PEMLR = c(180, 181), PRDISC = c(389, 390), PRUNEDUR = c(407, 409),
                                                      # PRDTOCC1 = c(476,477), PRDTOCC2 = c(478,479), PRMJOCC1 = c(486,487), PRMJOCC2 = c(488,489),
                                                      PWSSWGT = c(613, 622), PWCMPWGT = c(846, 855),
                                                      # PEIO1ICD = c(856,859),
                                                      PEIO1OCD = c(860, 863),
                                                      # PEIO2OCD = c(868,871),
                                                      PRIMIND1 = c(872, 873)),
                             col_types = cols(HRHHID = "c", HRINTSTA = "i", HRMIS = "i", HRSAMPLE = "c", HRSERSUF = "c", HUHHNUM = "c",
                                              GESTCEN = "i", GESTFIPS = "i", PRTAGE = "i", PESEX = "i", PEEDUCA = "i", PTDTRACE = "i",
                                              PULINENO = "i", PRMARSTA = "i", PEMLR = "i", PRDISC = "i", PRUNEDUR = "i",
                                              # PRDTOCC1 = "i, PRDTOCC2 = "i, PRMJOCC1 = "i", PRMJOCC2 = "i",
                                              PWSSWGT = "d", PWCMPWGT = "d",
                                              # PEIO1ICD = "i",
                                              PEIO1OCD = "i",
                                              # PEIO2OCD = "i,
                                              PRIMIND1 = "i"))
            }
            if ((t >= 200405) && (t <= 201312)) {
                df_cpsdata <-
                    read_fwf(file = str_c(ddir_cps, "b", t),
                             col_positions = fwf_cols(HRHHID = c(1, 15), HRINTSTA = c(57, 58), HRMIS = c(63, 64), HRSAMPLE = c(71, 72), HRSERSUF = c(73, 74), HUHHNUM = 75,
                                                      GESTCEN = c(91, 92), GESTFIPS = c(93, 94), PRTAGE = c(122, 123), PESEX = c(129, 130), PEEDUCA = c(137, 138), PTDTRACE = c(139, 140),
                                                      PULINENO = c(147, 148), PRMARSTA = c(159, 160), PEMLR = c(180, 181), PRDISC = c(389, 390), PRUNEDUR = c(407, 409),
                                                      # PRDTOCC1 = c(476,477), PRDTOCC2 = c(478,479), PRMJOCC1 = c(486,487), PRMJOCC2 = c(488,489),
                                                      PWSSWGT = c(613, 622), PWCMPWGT = c(846, 855),
                                                      # PEIO1ICD = c(856,859),
                                                      PEIO1OCD = c(860, 863),
                                                      # PEIO2OCD = c(868,871),
                                                      PRIMIND1 = c(872, 873)),
                             col_types = cols(HRHHID = "c", HRINTSTA = "i", HRMIS = "i", HRSAMPLE = "c", HRSERSUF = "c", HUHHNUM = "c",
                                              GESTCEN = "i", GESTFIPS = "i", PRTAGE = "i", PESEX = "i", PEEDUCA = "i", PTDTRACE = "i",
                                              PULINENO = "i", PRMARSTA = "i", PEMLR = "i", PRDISC = "i", PRUNEDUR = "i",
                                              # PRDTOCC1 = "i", PRDTOCC2 = "i", PRMJOCC1 = "i", PRMJOCC2 = "i",
                                              PWSSWGT = "d", PWCMPWGT = "d",
                                              # PEIO1ICD = "i",
                                              PEIO1OCD = "i",
                                              #, PEIO2OCD = "i",
                                              PRIMIND1 = "i"))
            }
            if (t >= 201401) {
                df_cpsdata <-
                    read_fwf(file = str_c(ddir_cps, "b", t),
                             col_positions = fwf_cols(HRHHID = c(1, 15), HRINTSTA = c(57, 58), HRMIS = c(63, 64), HRSAMPLE = c(71, 72), HRSERSUF = c(73, 74), HUHHNUM = 75,
                                                      GESTFIPS = c(93, 94), PRTAGE = c(122, 123), PESEX = c(129, 130), PEEDUCA = c(137, 138), PTDTRACE = c(139, 140),
                                                      PULINENO = c(147, 148), PRMARSTA = c(159, 160), PEMLR = c(180, 181), PRDISC = c(389, 390), PRUNEDUR = c(407, 409),
                                                      # PRDTOCC1 = c(476,477), PRDTOCC2 = c(478,479), PRMJOCC1 = c(486,487), PRMJOCC2 = c(488,489),
                                                      PWSSWGT = c(613, 622), PWCMPWGT = c(846, 855),
                                                      # PEIO1ICD = c(856,859),
                                                      PEIO1OCD = c(860, 863),
                                                      PRIMIND1 = c(872, 873)),
                             col_types = cols(HRHHID = "c", HRINTSTA = "i", HRMIS = "i", HRSAMPLE = "c", HRSERSUF = "c", HUHHNUM = "c",
                                              GESTFIPS = "i", PRTAGE = "i", PESEX = "i", PEEDUCA = "i", PTDTRACE = "i",
                                              PULINENO = "i", PRMARSTA = "i", PEMLR = "i", PRDISC = "i", PRUNEDUR = "i",
                                              # PRDTOCC1 = "i", PRDTOCC2 = "i", PRMJOCC1 = "i", PRMJOCC2 = "i",
                                              PWSSWGT = "d", PWCMPWGT = "d",
                                              # PEIO1ICD = "i",
                                              PEIO1OCD = "i",
                                              # , PEIO2OCD = "i",
                                              PRIMIND1 = "i"))
            }

            df_cpsdata %<>%
                mutate(period = t,
                       HRSAMPLE = gsub("[^\\d]+", "", HRSAMPLE, perl = TRUE),
                       HRSERSUF = if_else(period >= 200405, HRSERSUF, sprintf("%02d", match(HRSERSUF, LETTERS, nomatch = 0))),
                       HRHHID2 = str_c(HRSAMPLE, HRSERSUF, gsub("0", "", HUHHNUM)),
                       HID = str_c(sprintf("%02d", GESTFIPS), HRHHID, HRHHID2)) %>%
                select(-c(HRSAMPLE, HRSERSUF, HUHHNUM, HRHHID, HRHHID2))
        }

        # drop noninterview types A,B,C
        df_cpsdata %>%
            count(HRINTSTA) %>%
            mutate(percent = 100*n/sum(n))
        df_cpsdata %<>%
            filter(HRINTSTA == 1) %>%
            select(-HRINTSTA)

        # recode highest grade attended so that for 12th grade HGA = 12
        if (t <= 198812) df_cpsdata %<>% mutate(HGA = HGA - 1L)
        # subtract one year if the highest grade attended is not completed
        if (t <= 199112) df_cpsdata %<>% mutate(PEEDUCA = if_else(!is.na(HGC) & HGC == 2, HGA - 1L, HGA))

        # add GESTCEN
        if (t >= 201401) df_cpsdata %<>% left_join(df_state_codes, by = "GESTFIPS")

        # recode variables
        df_cpsdata %<>%
            mutate(period = t,
                   # age
                   age = PRTAGE,
                   # gender
                   female = (PESEX == 2) %>% as.numeric(),
                   # nwhite = (PTDTRACE != 1) %>% as.numeric(),
                   # see Nekarda (2009) NBER wp, section A.3.5 page 28-29
                   nwhite = case_when((period <= 198812) & (PTDTRACE != 1) ~ 1,
                                      (period >= 198901 & period <= 200212) & (PTDTRACE != 1) ~ 1,
                                      (period >= 200301 & period <= 201204) & (PTDTRACE %in% c(2:5,10:14,20:21)) ~ 1,
                                      (period >= 201205) & (PTDTRACE %in% c(2:5,10:15,22,25:26)) ~ 1,
                                      TRUE ~ 0),
                   white = case_when((period <= 198812) & (PTDTRACE == 1) ~ 1,
                                     (period >= 198901 & period <= 200212) & (PTDTRACE == 1) ~ 1,
                                     (period >= 200301 & period <= 201204) & (PTDTRACE %in% c(1,6:9,15:19)) ~ 1,
                                     (period >= 201205) & (PTDTRACE %in% c(1,6:9,16:21,23:24)) ~ 1,
                                     TRUE ~ 0),
                   black = case_when((period <= 198812) & (PTDTRACE == 2) ~ 1,
                                     (period >= 198901 & period <= 200212) & (PTDTRACE == 2) ~ 1,
                                     (period >= 200301 & period <= 201204) & (PTDTRACE %in% c(2,10:12)) ~ 1,
                                     (period >= 201205) & (PTDTRACE %in% c(2,10:12,22)) ~ 1,
                                     TRUE ~ 0),
                   # married = (PRMARSTA %in% c(1:3)) %>% as.numeric(),
                   married = case_when((period <= 198812) & (PRMARSTA %in% c(1:3)) ~ 1,
                                       (period >= 198901) & (PRMARSTA %in% c(1:3,6)) ~ 1,
                                       TRUE ~ 0),
                   # education
                   educ = if_else(period <= 199112,
                                  case_when(  PEEDUCA <= 11                  ~ 1L,
                                              PEEDUCA == 12                  ~ 2L,
                                              (PEEDUCA >= 13 & PEEDUCA <= 15) ~ 3L,
                                              (PEEDUCA >= 16 & PEEDUCA <= 17) ~ 4L,
                                              PEEDUCA >= 18                   ~ 5L,
                                              TRUE                           ~ NA_integer_),
                                  case_when(  PEEDUCA <= 37                  ~ 1L,
                                              (PEEDUCA >= 38 & PEEDUCA <= 39) ~ 2L,
                                              (PEEDUCA >= 40 & PEEDUCA <= 42) ~ 3L,
                                              PEEDUCA == 43                  ~ 4L,
                                              (PEEDUCA >= 44 & PEEDUCA <= 46) ~ 5L,
                                              TRUE                           ~ NA_integer_)),
                   # educ.f = factor(educ, labels = c("HSD", "HSG", "SMC", "CLG", "GTC")),
                   # labor force status PEMLR
                   lfs = case_when( PEMLR %in% c(1:2)              ~ "E",
                                    PEMLR == 3                     ~ "U",
                                    (PEMLR == 4 & period >= 198901) ~ "U",
                                    (PEMLR == 4 & period <  198901) ~ "I",
                                    PEMLR %in% c(5:7)              ~ "I",
                                    TRUE                           ~ "M"),
                   # duration of unemployment
                   udur = PRUNEDUR,
                   # occupation: non-routine cognitive, non-routine manual, routine cognitive, routine manual, farm, military
                   occ1cat_all = case_when(is.na(PEIO1OCD) ~ "X",

                                           (period <= 198212) & (PEIO1OCD %in% occ_grp$Y1970$NRC) ~ "NRC",
                                           (period <= 198212) & (PEIO1OCD %in% occ_grp$Y1970$NRM) ~ "NRM",
                                           (period <= 198212) & (PEIO1OCD %in% occ_grp$Y1970$RC)  ~ "RC",
                                           (period <= 198212) & (PEIO1OCD %in% occ_grp$Y1970$RM)  ~ "RM",
                                           (period <= 198212) & (PEIO1OCD %in% occ_grp$Y1970$FRM) ~ "FRM",
                                           (period <= 198212) & (PEIO1OCD %in% occ_grp$Y1970$MIL) ~ "MIL",

                                           (period >= 198301 & period <= 200212) & (PEIO1OCD %in% occ_grp$Y1980$NRC) ~ "NRC",
                                           (period >= 198301 & period <= 200212) & (PEIO1OCD %in% occ_grp$Y1980$NRM) ~ "NRM",
                                           (period >= 198301 & period <= 200212) & (PEIO1OCD %in% occ_grp$Y1980$RC)  ~ "RC",
                                           (period >= 198301 & period <= 200212) & (PEIO1OCD %in% occ_grp$Y1980$RM)  ~ "RM",
                                           (period >= 198301 & period <= 200212) & (PEIO1OCD %in% occ_grp$Y1980$FRM) ~ "FRM",
                                           (period >= 198301 & period <= 200212) & (PEIO1OCD %in% occ_grp$Y1980$MIL) ~ "MIL",

                                           (period >= 200301 & period <= 201012) & (PEIO1OCD %in% occ_grp$Y2000$NRC) ~ "NRC",
                                           (period >= 200301 & period <= 201012) & (PEIO1OCD %in% occ_grp$Y2000$NRM) ~ "NRM",
                                           (period >= 200301 & period <= 201012) & (PEIO1OCD %in% occ_grp$Y2000$RC)  ~ "RC",
                                           (period >= 200301 & period <= 201012) & (PEIO1OCD %in% occ_grp$Y2000$RM)  ~ "RM",
                                           (period >= 200301 & period <= 201012) & (PEIO1OCD %in% occ_grp$Y2000$FRM) ~ "FRM",
                                           (period >= 200301 & period <= 201012) & (PEIO1OCD %in% occ_grp$Y2000$MIL) ~ "MIL",

                                           (period >= 201101) & (PEIO1OCD %in% occ_grp$Y2010$NRC) ~ "NRC",
                                           (period >= 201101) & (PEIO1OCD %in% occ_grp$Y2010$NRM) ~ "NRM",
                                           (period >= 201101) & (PEIO1OCD %in% occ_grp$Y2010$RC)  ~ "RC",
                                           (period >= 201101) & (PEIO1OCD %in% occ_grp$Y2010$RM)  ~ "RM",
                                           (period >= 201101) & (PEIO1OCD %in% occ_grp$Y2010$FRM) ~ "FRM",
                                           (period >= 201101) & (PEIO1OCD %in% occ_grp$Y2010$MIL) ~ "MIL",

                                           TRUE                          ~ "X"),

                   occ1cat = if_else(lfs == "I", "X", occ1cat_all)
            )

        # industry: manufacturing, non-manufacturing
        if (t <= 199912)
            df_cpsdata %<>% mutate(ind1cat = case_when(PRMJIND1 %in% c(4,5)       ~ "MAN",
                                                       PRMJIND1 %in% c(1:3, 6:23) ~ "NONMAN",
                                                       TRUE                       ~ "X"))
        if (t >= 200001 & t <= 200212)
            df_cpsdata %<>% mutate(ind1cat = case_when(NRDTIND1 %in% c(4,5)        ~ "MAN",
                                                       NRDTIND1 %in% c(1:3, 6:22)  ~ "NONMAN",
                                                       TRUE                        ~ "X"))
        if (t >= 200301)
            df_cpsdata %<>% mutate(ind1cat = case_when(PRIMIND1 %in% c(4,5)        ~ "MAN",
                                                       PRIMIND1 %in% c(1:3, 6:22)  ~ "NONMAN",
                                                       TRUE                        ~ "X"))

        # weights
        if (t <= 199712) df_cpsdata %<>% mutate(weight = PWSSWGT)
        if (t >= 199801) df_cpsdata %<>% mutate(weight = PWCMPWGT)
        if (t >= 200001 & t <= 200212) df_cpsdata %<>% mutate(weight = NWCMPWGT)

        # change weights to thousands of person
        df_cpsdata %<>%
            filter(!is.na(weight)) %>%
            mutate(weight = if_else(period < 199401, weight/100000, weight/10000000))

        # keep only variables that are needed
        df_cpsdata %<>%
            rename(gestcen = GESTCEN,
                   hid = HID,
                   pid = PULINENO,
                   mis = HRMIS,) %>%
            # select(-GESTCEN,-PRTAGE,-PESEX,-PEEDUCA,-PEMLR,-PRUNEDUR,-PEIO1OCD)
            select(period, gestcen, hid, pid, mis, age, educ, female, white, black, married, lfs, udur, occ1cat, occ1cat_all, ind1cat, weight)

        save(df_cpsdata, file = str_c(edir_cps, "cpsb_", t, ".Rdata"))

        rm(df_cpsdata, df_cpsdata_initial, df_cpsdata_revised)
    }
)

toc()
