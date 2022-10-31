# TODO: Universal column checker. All functions need to be refactored to make
# then more succinct. Currenctly only used in RETOLmcg_Recalculator

check_columns <- function(dataset, columns) {
    #' Check presence of columns required for a calculation in a given dataset.
    #' Column names are case sensitive and error is thrown if not found.
    #' @param dataset :Required (FCT dataset to be checked)
    #' @param columns (Columns to be checked)
    #' @return dataset
    #' @examples
    for (column in columns) {
        if (column %in% names(dataset)) {

        } else {
            stop(
                paste0(
                    "Error: variable ",
                    column,
                    " not found, halting execution. Please fix your input data and try again"
                )
            )
        }
    }
}

# TODO: Documenting this function. But its works. # change some variables !
# SOP_std = (WATERg + PROCNTg + FAT_g_standardised + CHOAVLg + FIBTGg + ALCg
# +ASHg)
SOP_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c(
        "WATERg",
        "PROCNTg",
        "FAT_g_standardised",
        # Change FAT_g to FAT_g_standardised
        "CHOAVLg",
        "FIBTGg",
        "ALCg",
        "ASHg_std" # change ASHg to ASHg_std
    )
    for (column in columns) {
        if (column %in% names(dataset)) {

        } else {
            stop(
                paste0(
                    "Error: variable ",
                    column,
                    " not found, halting execution. Please fix your input data and try again"
                )
            )
        }
    }
    # Try the calculation
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            # ! Create a temp row with the number of NAs across the required
            # column
            mutate(temp = rowSums(is.na(
                dataset %>%
                    select(all_of(columns))
            ))) %>%
            rowwise() %>%
            # ! Check if all the rows are NA then output NA else do the
            # calculation and omit NAs
            mutate(SOP_std = ifelse(
                temp == length(columns),
                NA,
                sum(
                    WATERg,
                    PROCNTg,
                    FAT_g_standardised,
                    CHOAVLg,
                    FIBTGg,
                    ALCg,
                    ASHg_std,
                    na.rm = TRUE
                )
            )) %>%
            # ! remove the temp column
            select(-temp) %>%
            ungroup(),
        error = function(e) {
            print(
                paste0(
                    "Error : Required columns i.e. ",
                    columns,
                    " should be numeric. The SOP_std will not be calculated"
                )
            )
        }
    )
}

#  Carotene Eq.
# Weighted sum of the listed variables
# CARTBEQmcg_std <- 1 * CARTBmcg + 0.5 * CARTAmcg + 0.5 * CRYPXBmcg
# TODO: Create documentation. Function works. Most fileds
CARTBEQmcg_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c("CARTBmcg", "CARTAmcg", "CRYPXBmcg")
    for (column in columns) {
        if (column %in% names(dataset)) {

        } else {
            stop(
                paste0(
                    "Error: variable ",
                    column,
                    " not found, halting execution. Please fix your input data and try again"
                )
            )
        }
    }
    # Try the calculation
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            # ! Create a temp row with the number of NAs across the required
            # column
            mutate(temp = rowSums(is.na(
                dataset %>%
                    select(all_of(columns))
            ))) %>%
            rowwise() %>%
            # ! Check if all the rows are NA then output NA else do the
            # calculation and omit NAs
            mutate(comment = ifelse(is.na(comment), "", comment)) %>%
            # ! Replace comment NAs with blank so that we can concatenate
            # comments well.
            mutate(CARTBEQmcg_std = ifelse(
                temp == length(columns),
                NA,
                sum(1 * CARTBmcg, 0.5 * CARTAmcg, 0.5 * CRYPXBmcg, na.rm = TRUE)
            )) %>%
            mutate(comment = ifelse(
                temp == length(columns),
                comment,
                paste0(
                    comment,
                    " | CARTBEQmcg_std calculated from CARTBmcg, CARTAmcg and CRYPXBmcg"
                )
            )) %>%
            mutate(comment = ifelse(
                (
                    temp != length(columns) &
                        !is.na(CARTBmcg) &
                        is.na(CARTAmcg) & is.na(CRYPXBmcg)
                ),
                paste0(comment, " but only CARTB was used"),
                comment
            )) %>%
            # ! remove the temp column
            select(-temp) %>%
            ungroup(),
        error = function(e) {
            print("Error : Required columns not found i.e :")
            print(columns)
            print("The SOP_std will not be calculated")
        }
    )
}

#  Vitamin A, retinol activity eq.
# Weighted sum of the listed variables
# ! VITA_RAEmcg_std <- RETOLmcg + 1 / 12 * CARTBEQmcg_std
# TODO: Create documentation. Function works. Most fileds
VITA_RAEmcg_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c("RETOLmcg", "CARTBEQmcg_std")
    for (column in columns) {
        if (column %in% names(dataset)) {

        } else {
            stop(
                paste0(
                    "Error: variable ",
                    column,
                    " not found, halting execution. Please fix your input data and try again"
                )
            )
        }
    }
    # Try the calculation
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            # ! Create a temp row with the number of NAs across the required
            # column
            mutate(temp = rowSums(is.na(
                dataset %>%
                    select(all_of(columns))
            ))) %>%
            rowwise() %>%
            # ! Check if all the rows are NA then output NA else do the
            # calculation and omit NAs
            mutate(VITA_RAEmcg_std = ifelse(
                temp == length(columns), NA, sum(RETOLmcg, (1 / 12 * CARTBEQmcg_std), na.rm = TRUE)
            )) %>% # ! remove the temp column
            select(-temp) %>%
            ungroup(),
        error = function(e) {
            print("Error : Required columns not found i.e :")
            print(columns)
            print("The SOP_std will not be calculated")
        }
    )
}

# Vitamin A, retinol eq.
# Weighted sum of the listed variables
# ! VITAmcg_std <- RETOLmcg + 1 / 6 * CARTBEQmcg_std
# TODO: Create documentation. Function works. Most fileds
VITAmcg_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c("RETOLmcg", "CARTBEQmcg_std")
    for (column in columns) {
        if (column %in% names(dataset)) {

        } else {
            stop(
                paste0(
                    "Error: variable ",
                    column,
                    " not found, halting execution. Please fix your input data and try again"
                )
            )
        }
    }
    # Try the calculation
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            # ! Create a temp row with the number of NAs across the required
            # column
            mutate(temp = rowSums(is.na(
                dataset %>%
                    select(all_of(columns))
            ))) %>%
            rowwise() %>%
            # ! Check if all the rows are NA then output NA else do the
            # calculation and omit NAs
            mutate(VITAmcg_std = ifelse(
                temp == length(columns), NA, sum(RETOLmcg, (1 / 6 * CARTBEQmcg_std), na.rm = TRUE)
            )) %>%
            # ! remove the temp column
            select(-temp) %>%
            ungroup(),
        error = function(e) {
            print("Error : Required columns not found i.e :")
            print(columns)
            print("The SOP_std will not be calculated")
        }
    )
}


# Thiamin
# Variable combinations: In absence of THIAmg, use values of THIAHCLmg
# ! THIAmg_std = THIAmg OR THIAHCLmg
THIAmg_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c("THIAmg", "THIAHCLmg")
    for (column in columns) {
        if (column %in% names(dataset)) {

        } else {
            stop(
                paste0(
                    "Error: variable ",
                    column,
                    " not found, halting execution. Please fix your input data and try again"
                )
            )
        }
    }
    # Try the calculation
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            mutate(THIAmg_std = case_when(
                !is.na(THIAmg) ~ THIAmg,
                is.na(THIAmg) ~ THIAHCLmg
            )),
        error = function(e) {
            print("Error : Required columns not found i.e :")
            print(columns)
        }
    )
}

# # TODO: Function needs testing CHOAVLDFg.calculation <- function(WATERg, PROTg,
# FATg_standardised, FBGTg, ASHg, ALCg) { ALCg <- ALCg %>% replace_na(0)
# CHOAVLDFg_std <- 100 - (WATERg + PROTg + FATg_standardised + FBGTg + ASHg + ALCg)
# return(CHOAVLDFg_std) }
# ! Modified Lucia's function to perform additional checks and throw warnings
# TODO: Create documentation. Function works. Most fileds
CHOAVLDFg_std_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c(
        "WATERg",
        "PROCNTg",
        "FAT_g_standardised",
        "FIBTGg",
        "ASHg_std",
        "ALCg"
    )
    for (column in columns) {
        if (column %in% names(dataset)) {

        } else {
            stop(
                paste0(
                    "Error: variable ",
                    column,
                    " not found, halting execution. Please fix your input data and try again"
                )
            )
        }
    }
    # Try the calculation
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            # ! I the original function only ALCg had NAs replaced. Is this
            # right then? TODO: future dev should check food group before
            # changing NAs to Zero ! Create a temp row with the number of NAs
            # across the required column
            mutate(temp = rowSums(is.na(
                dataset %>%
                    select(all_of(columns))
            ))) %>%
            rowwise() %>%
            # ! Check if all the rows are NA then output NA else do the
            # calculation and omit NAs
            mutate(CHOAVLDFg_std = ifelse(
                temp == length(columns),
                NA,
                sum(
                    100,
                    -WATERg,
                    -PROCNTg,
                    -FAT_g_standardised,
                    -FIBTGg,
                    -ASHg_std,
                    -ALCg,
                    na.rm = TRUE
                )
            )) %>%
            # ! remove the temp column
            select(-temp) %>%
            ungroup(),
        error = function(e) {
            print("Error : Required columns not found i.e :")
            print(columns)
            print("The CHOAVLDFg_std will not be calculated")
        }
    )
}

# #! Is this required as well? #Working but introduced extra values - Eg. US19 =
# 818 instead of 808 nia.conversion <- function(x, var1 = "NIAEQmg", var2 =
# "TRPmg", var3 = "NIAmg"){ if(is.na(var3)){ var3 <- as.numeric(var1) -
# 1/60*as.numeric(var2) }else{var3} print(var3) } }
nia_conversion_creator <- function(dataset) {
    # Check presence of required columns
    columns <- c("NIAEQmg", "TRPmg", "NIAmg")
    for (column in columns) {
        if (column %in% names(dataset)) {

        } else {
            stop(
                paste0(
                    "Error: variable ",
                    column,
                    " not found, halting execution. Please fix your input data and try again"
                )
            )
        }
    }
    # Try the calculation
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            mutate(NIAmg_std = case_when(
                !is.na(NIAmg) ~ NIAmg,
                is.na(NIAmg) ~ (NIAEQmg - (1 / 60 * TRPmg))
            )),
        error = function(e) {
            print("Error : Required columns not found i.e :")
            print(columns)
        }
    )
}

# We need to generate a function to recalculate the values of RETOLmcg, when it
# is not provided. It could be used as well when calculating CARTBEQmcg. Retinol
# (RETOLmcg) can be re-calculated from Vitamin A: [(2*VITA_REAmcg) - VITAmcg] ,
# add comment: "RETOLmcg value re-calulated from VITA_REAmcg and VITAmcg".

RETOLmcg_Recalculator <- function(dataset) {
    # Check presence of required columns
    # TODO: Refactor all code to have the column checker as its own function.
    columns <- c("RETOLmcg", "VITA_RAEmcg", "VITAmcg", "CARTBEQmcg")
    check_columns(dataset = dataset, columns = columns)
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            rowwise() %>%
            mutate(comment = ifelse(is.na(comment), "", comment)) %>%
            # ! case 1
            mutate(RETOLmcg = ifelse((is.na(RETOLmcg) &
                !is.na(CARTBEQmcg) &
                !is.na(VITA_RAEmcg)),
            sum(VITA_RAEmcg, (-1 / 12 * CARTBEQmcg)),
            RETOLmcg
            )) %>%
            mutate(comment = ifelse((is.na(RETOLmcg) &
                !is.na(CARTBEQmcg) &
                !is.na(VITA_RAEmcg)),
            paste0(
                comment,
                " | RETOLmcg value re-calulated from VITA_RAEmcg and CARTBEQmcg "
            ),
            comment
            )) %>%
            # ! Case 2
            mutate(RETOLmcg = ifelse((
                is.na(RETOLmcg) &
                    is.na(CARTBEQmcg) &
                    !is.na(VITA_RAEmcg) & !is.na(VITAmcg)
            ),
            sum((2 * VITA_RAEmcg), VITAmcg),
            RETOLmcg
            )) %>%
            mutate(comment = ifelse((
                is.na(RETOLmcg) &
                    is.na(CARTBEQmcg) &
                    !is.na(VITA_RAEmcg) & !is.na(VITAmcg)
            ),
            paste0(
                comment,
                " | RETOLmcg value re-calulated from VITA_RAEmcg and VITAmcg "
            ),
            comment
            )) %>%
            ungroup(),
        error = function(e) {
            print(paste0(
                "Error: ",
                columns,
                " columns missing in dataset, halting calculation"
            ))
        }
    )
}


CARTBEQmcg_std_back_calculator_VITA_RAEmcg <- function(dataset) {
    # nolint
    # Check presence of required columns
    columns <- c("VITA_RAEmcg", "RETOLmcg")
    check_columns(dataset = dataset, columns = columns)
    # Try the calculation
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            # ! Create a temp row with the number of NAs across the required
            rowwise() %>%
            # ! Check if all the rows are NA then output NA else do the
            # calculation and omit NAs
            mutate(comment = ifelse(is.na(comment), "", comment)) %>%
            mutate(comment = ifelse((
                is.na(CARTBEQmcg_std) &
                    !is.na(VITA_RAEmcg) & !is.na(RETOLmcg)
            ),
            paste0(
                comment,
                " | CARTBEQmcg_std back calculated from VITA_RAEmcg and VITAmcg"
            ),
            comment
            )) %>%
            mutate(CARTBEQmcg_std = ifelse((
                is.na(CARTBEQmcg_std) &
                    !is.na(VITA_RAEmcg) & !is.na(RETOLmcg)
            ),
            sum(12 * VITA_RAEmcg, -12 * RETOLmcg),
            CARTBEQmcg_std
            )) %>%
            ungroup(),
        error = function(e) {
            print(paste0(
                "Error: ",
                columns,
                " columns missing in dataset, halting calculation"
            ))
        }
    )
}

# TODO: Remove since this function is now deprecated
# CARTBEQmcg_std_back_calculator_VITAmcg <- function(dataset) {
#     # Check presence of required columns
#     columns <- c("VITAmcg", "RETOLmcg")
#     for (column in columns) {
#         if (column %in% names(dataset)) {

#         } else {
#             stop(
#                 paste0(
#                     "Error: variable ",
#                     column,
#                     " not found, halting execution. Please fix your input data and try again"
#                 )
#             )
#         }
#     }
#     # Try the calculation
#     tryCatch(
#         dataset %>%
#             as_tibble() %>%
#             mutate_at(.vars = columns, .funs = as.numeric) %>%
#             # ! Create a temp row with the number of NAs across the required
#             # column
#             mutate(temp = rowSums(is.na(
#                 dataset %>%
#                     select(all_of(columns))
#             ))) %>%
#             rowwise() %>%
#             # ! Check if all the rows are NA then output NA else do the
#             # calculation and omit NAs
#             # ? is one comment for both sufficient or
#             # should we identify each component individually?
#             mutate(comment = ifelse(is.na(comment), "", comment)) %>%
#             # ! Replace comment NAs with blank so that we can concatenate
#             # comments well.
#             mutate(CARTBEQmcg_std = ifelse((is.na(CARTBEQmcg_std) &
#                 temp == length(columns)),
#             CARTBEQmcg_std,
#             sum(6 * VITAmcg, -6 * RETOLmcg, na.rm = TRUE)
#             )) %>%
#             mutate(comment = ifelse((is.na(CARTBEQmcg_std) &
#                 temp == length(columns)),
#             comment,
#             paste0(
#                 comment,
#                 " | CARTBEQmcg_std back calculated from VITAmcg and VITAmcg"
#             )
#             )) %>% # nolint
#             # ! remove the temp column
#             select(-temp) %>%
#             ungroup(),
#         error = function(e) {
#             print("Error : Required columns not found i.e :")
#             print(columns)
#             print("The SOP_std will not be calculated")
#         }
#     )
# }

# Imputes values of CARTBEQmcg into CARTBEQmcg_std when they are NAs.
CARTBEQmcg_std_imputer_with_CARTBEQmcg <-
    function(dataset) {
        columns <- c("CARTBEQmcg_std", "CARTBEQmcg")
        check_columns(dataset = dataset, columns = columns)
        # Try the calculation
        tryCatch(
            dataset %>%
                as_tibble() %>%
                mutate_at(.vars = columns, .funs = as.numeric) %>%
                rowwise() %>%
                mutate(comment = ifelse(is.na(comment), "", comment)) %>%
                mutate(comment = ifelse((is.na(CARTBEQmcg_std) &
                    !is.na(CARTBEQmcg)),
                paste0(
                    comment,
                    "| CARTBEQmcg_std imputed with CARTBEQmcg"
                ),
                comment
                )) %>%
                mutate(CARTBEQmcg_std = ifelse((is.na(CARTBEQmcg_std) &
                    !is.na(CARTBEQmcg)),
                CARTBEQmcg, # Impute with CARTBEQmcg
                CARTBEQmcg_std # Retain the CARTBEQmcg_std)
                )) %>%
                ungroup(),
            error = function(e) {
                print(paste0(
                    "Error: ",
                    columns,
                    " columns missing in dataset, halting calculation"
                ))
            }
        )
    }


# Handling Implausible values of git fetch origin cartbeqmcg
CARTBEQmcg_std_to_zero <- function(dataset) {
    dataset %>%
        as_tibble() %>%
        # mutate_at(.vars = columns, .funs = as.numeric) %>%
        rowwise() %>%
        mutate(comment = ifelse((CARTBEQmcg_std < 0), paste0(comment, "| Impausible value of CARTBEQmcg_std = ", CARTBEQmcg_std, " replaced with 0"), comment)) %>%
        mutate(CARTBEQmcg_std = ifelse(CARTBEQmcg_std < 0, 0, CARTBEQmcg_std)) %>%
        ungroup()
}
