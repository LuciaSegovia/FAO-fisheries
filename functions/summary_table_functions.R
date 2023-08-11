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



SOP_std_creator <- function(dataset) {
    #' @description Calculates SOP_std = (WATERg + PROCNTg + FAT_g_standardised + CHOAVLg + #` FIBTGg + ALCg +ASHg).
    #' Column names are case sensitive and error is thrown if not found.
    #' @param dataset :Required (FCT dataset to be checked)
    #' @param SOP_std Sum of Proximate in g per 100g EP as reported in the original FCT
    #' @param WATERg Water/ moisture content in g per 100g of EP
    #' @param PROCNTg Protein in g per 100g of EP, as reported in the original FCT and assumed to be calculated from nitrogen (NTg) content
    #' @param FAT_g_standardised fat content unknown method of calculation in g per 100g of EP
    #' @param CHOAVLDFg_std Available carbohydrates calculated by difference in g per 100g of EP
    #' @param FIBTGg Total dietary fibre by AOAC Prosky method expressed in g per 100g of EP
    #' @param ALCg Alcohol in g per 100g
    #' @param ASHg_std Ashes in g per 100g of EP
    #' @return Original FCT dataset with SOP_std column added
    #' @examples
    # Check presence of required columns
    columns <- c(
        "WATERg",
        "PROCNTg",
        "FAT_g_standardised",
        "CHOAVLDFg_std",
        "FIBTGg",
        "ALCg",
        "ASHg_std"
    )
    check_columns(dataset = dataset, columns = columns)
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            # ! Create a temp row with the count of NAs in the required columns
            mutate(temp = rowSums(is.na(
                dataset %>%
                    select(all_of(columns))
            ))) %>%
            # Rowwise allows for per row evaluations.
            rowwise() %>%
            # ! If all the rows are NA then output is NA.
            # ! Else do the calculation and omit NAs.
            mutate(SOP_std = ifelse(
                temp == length(columns),
                NA,
                sum(
                    WATERg,
                    PROCNTg,
                    FAT_g_standardised,
                    CHOAVLDFg_std,
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



CARTBEQmcg_std_creator <- function(dataset) {
    #' @description Calculates a weighted sum of CARTBEQmcg_std = (1 * CARTBmcg + 0.5 * CARTAmcg + 0.5 * CRYPXBmcg)
    #' @param CARTBEQmcg_std Beta-carotene equivalents, expressed in mcg per 100g of EP
    #' @param CARTBmcg Beta-carotene in mcg per 100g of EP
    #' @param CARTAmcg Alpha-carotene in mcg per 100g of EP
    #' @param CRYPXBmcg Beta-cryptoxanthin in mcg per 100g of EP
    #' @return dataset (Original FCT dataset with CARTBEQmcg_std column)
    # Check presence of required columns
    columns <- c("CARTBmcg", "CARTAmcg", "CRYPXBmcg")
    check_columns(dataset = dataset, columns = columns)
    # Try the calculation
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            # ! Create a temp row with the count of NAs in the required columns
            mutate(temp = rowSums(is.na(
                dataset %>%
                    select(all_of(columns))
            ))) %>%
            rowwise() %>%
            # ! Replace comments NAs with blank so that we can concatenate commentss well.
            mutate(comments = ifelse(is.na(comments), "", comments)) %>%
            # ! If all inputs to the CARTBEQmcg_std calculation are NA return NA
            # ! Else perform calculation ommiting NAs
            mutate(CARTBEQmcg_std = ifelse(
                temp == length(columns),
                NA,
                sum(1 * CARTBmcg, 0.5 * CARTAmcg, 0.5 * CRYPXBmcg, na.rm = TRUE)
            )) %>%
            # ! Use the same logic as above for comments appendage.
            mutate(comments = ifelse(
                temp == length(columns),
                comments,
                paste0(
                    comments,
                    " | CARTBEQmcg_std calculated from CARTBmcg, CARTAmcg and CRYPXBmcg"
                )
            )) %>%
            # ! Check which components of the calculation were used. If only CARTB was used. Append the comments to reflect that.
            mutate(comments = ifelse(
                (
                    temp != length(columns) &
                        !is.na(CARTBmcg) &
                        is.na(CARTAmcg) & is.na(CRYPXBmcg)
                ),
                paste0(comments, " but only CARTB was used"),
                comments
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



VITA_RAEmcg_std_creator <- function(dataset) {
    #' @title Vitamin A, retinol activity
    #' @description Calculates a wighted sum of VITA_RAEmcg_std - Vitamin A (Retinol Activity Eq. (RAE)) in mcg per 100g of EP using (RETOLmcg + 1 / 12 * CARTBEQmcg_std)
    #' @param dataset - FCT dataset
    #' @param VITA_RAEmcg_std - Vitamin A (Retinol Activity Eq. (RAE)) in mcg per 100g of EP
    #' @param RETOLmcg Retinol in mcg per 100g of EP
    #' @param CARTBEQmcg_std Beta-carotene equivalents, expressed in mcg per 100g of EP
    #' @return Original dataset with the added column
    columns <- c("RETOLmcg", "CARTBEQmcg_std")
    check_columns(dataset = dataset, columns = columns)
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
            # ! Check if all the rows are NA then output NA else do the calculation and omit NAs
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



VITAmcg_std_creator <- function(dataset) {
    #' @title Vitamin A, retinol calculator
    #' @description Calculates weighted sum of VITAmcg_std (Vitamin A (Retinol Eq. (RE) in mcg per 100g of EP) using the eq. VITAmcg_std = RETOLmcg + 1 / 6 * CARTBEQmcg_std
    #' @param VITAmcg_std Vitamin A (Retinol Eq. (RE) in mcg per 100g of EP
    #' @param RETOLmcg Retinol in mcg per 100g of EP
    #' @param CARTBEQmcg_std Beta-carotene equivalents, expressed in mcg per 100g of EP
    #' @return Original dataset with the added column

    columns <- c("RETOLmcg", "CARTBEQmcg_std")
    check_columns(dataset = dataset, columns = columns)
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
            # ! Check if all the rows are NA then output NA else do the calculation and omit NAs
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



THIAmg_std_creator <- function(dataset) {
    #' @title THIAmg_std_creator
    #' @description Thiamin variable combinations: In absence of THIAmg, use values of THIAHCLmg i.e. THIAmg_std = THIAmg OR THIAHCLmg
    #' @param THIAmg Thiamin, vitamin B1 analysed and expressed as thiamin in mg per 100g of EP
    #' @param THIAHCLmg Thiamin hydrochloride, vitamin B1 analysed and expressed as thiamin hydrochloride in mg per 100g of EP

    columns <- c("THIAmg", "THIAHCLmg")
    check_columns(dataset = dataset, columns = columns)
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



CHOAVLDFg_std_creator <- function(dataset) {
    #' @title CHOAVLDFg_std_creator
    #' @description Calculates CHOAVLDFg_std = (100 - (WATERg + PROTg + FATg_standardised + FBGTg + ASHg + ALCg)).
    #' Column names are case sensitive and error is thrown if not found.
    #' @param dataset :Required (FCT dataset to be checked)
    #' @param CHOAVLDFg_std Available carbohydrates calculated by difference
    #' @param WATERg Water/ moisture content in g per 100g of EP
    #' @param PROCNTg Protein in g per 100g of EP, as reported in the original FCT and assumed to be calculated from nitrogen (NTg) content
    #' @param FAT_g_standardised fat content unknown method of calculation in g per 100g of EP
    #' @param FIBTGg Total dietary fibre by AOAC Prosky method expressed in g per 100g of EP
    #' @param ALCg Alcohol in g per 100g
    #' @param ASHg_std Ashes in g per 100g of EP
    #' @return Original FCT dataset with SOP_std column added
    #' @examples

    columns <- c(
        "WATERg",
        "PROCNTg",
        "FAT_g_standardised",
        "FIBTGg",
        "ASHg_std",
        "ALCg"
    )
    check_columns(dataset = dataset, columns = columns)
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            # ! Create a temp row with a count of number of NAs in req columns
            mutate(temp = rowSums(is.na(
                dataset %>%
                    select(all_of(columns))
            ))) %>%
            rowwise() %>%
            # ! Check if all the rows are NA then output NA else do the calculation and omit NAs
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



nia_conversion_creator <- function(dataset) {
    #' @title nia_conversion_creator
    #' @description Calculates NIAmg_std if based on presence of NIAmg. i.e. NIAmg_std = case_when(!is.na(NIAmg) ~ NIAmg,is.na(NIAmg) ~ (NIAEQmg - (1 / 60 * TRPmg)))
    #' @param NIAmg_std Niacin was combined into the Niacin standardised variable (`NIAmg_std`)
    #' @param NIAEQmg Niacin equivalents, total. Preformed niacin plus niacin equivalents from tryptophan (TRP) in mg per 100g EP
    #' @param TRPmg Tryptophan in mg per 100g of EP (includes only L-tryptophan)
    #' @param NIAmg Niacin, prefrormed in mg per 100g EP
    #' @return Original FCT dataset with additional column

    columns <- c("NIAEQmg", "TRPmg", "NIAmg")
    check_columns(dataset = dataset, columns = columns)
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



RETOLmcg_calculator <- function(dataset) {
    #' @title RETOLmcg_Recalculator
    #' @description Recalculates the values of RETOLmcg(Retinol in mcg per 100g of EP), when it is not provided. Works in 2 cases and in both cases an associated comments is added indicating how the value was derived.
    #' @param RETOLmcg Retinol in mcg per 100g of EP
    #' @param VITA_RAEmcg Vitamin A (Retinol Activity Eq. (RAE) = mcg retinol + 1/12 mcg ß- carotene + 1/24 mcg other provitamin A carotenoids) in mcg per 100g of EP
    #' @param VITAmcg Vitamin A (Retinol Eq. (RE) = mcg retinol + 1/6 mcg ß- carotene + 1/12 mcg other pro-vitamin A carotenoids) in mcg per 100g of EP
    #' @param CARTBEQmcg Beta-carotene equivalents, is the sum of the beta-carotene + 1/2 quantity of other carotenoids with vitamin A activity, expressed in mcg per 100g of EP
    #' @return Original FCT with appended column

    columns <- c("RETOLmcg", "VITA_RAEmcg", "VITAmcg", "CARTBEQmcg")
    check_columns(dataset = dataset, columns = columns)
    tryCatch(
        dataset %>%
            as_tibble() %>%
            mutate_at(.vars = columns, .funs = as.numeric) %>%
            rowwise() %>%
            mutate(comments = ifelse(is.na(comments), "", comments)) %>%
            # ! case 1 If all three variables are not NA them calculate and add comment
            mutate(RETOLmcg = ifelse((is.na(RETOLmcg) &
                !is.na(CARTBEQmcg) &
                !is.na(VITA_RAEmcg)),
            sum(VITA_RAEmcg, (-1 / 12 * CARTBEQmcg)),
            RETOLmcg
            )) %>%
            mutate(comments = ifelse((is.na(RETOLmcg) &
                !is.na(CARTBEQmcg) &
                !is.na(VITA_RAEmcg)),
            paste0(
                comments,
                " | RETOLmcg value re-calulated from VITA_RAEmcg and CARTBEQmcg "
            ),
            comments
            )) %>%
            # ! Case 2: If RETOLmcg is not present and CARTBEQmcg also not present but both VITA_RAEmcg and VITAmcg are available then calculate with eq2 and add comments.
            mutate(RETOLmcg = ifelse((
                is.na(RETOLmcg) &
                    is.na(CARTBEQmcg) &
                    !is.na(VITA_RAEmcg) & !is.na(VITAmcg)
            ),
            sum((2 * VITA_RAEmcg), VITAmcg),
            RETOLmcg
            )) %>%
            mutate(comments = ifelse((
                is.na(RETOLmcg) &
                    is.na(CARTBEQmcg) &
                    !is.na(VITA_RAEmcg) & !is.na(VITAmcg)
            ),
            paste0(
                comments,
                " | RETOLmcg value re-calulated from VITA_RAEmcg and VITAmcg "
            ),
            comments
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


# TODO: As above to add 2-case (for VITAmcg)
CARTBEQmcg_backcalculator <- function(dataset) {
    #' @title CARTBEQmcg_std_back_calculator_VITA_RAEmcg
    #' @description Recalculates CARTBEQmcg_std when they are NAs. It is used after running `CARTBEQmcg_std_creator(dataset)` first. It does the back calculation using sum(12 * VITA_RAEmcg, -12 * RETOLmcg).
    #' @param CARTBEQmcg_std Beta-carotene equivalents, expressed in mcg per 100g of EP
    #' @param VITA_RAEmcg Vitamin A (Retinol Activity Eq. (RAE)) in mcg per 100g of EP
    #' @param RETOLmcg Retinol in mcg per 100g of EP
    #' @return Original FCT with added column

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
            mutate(comments = ifelse(is.na(comments), "", comments)) %>%
            mutate(comments = ifelse((
                is.na(CARTBEQmcg_std) &
                    !is.na(VITA_RAEmcg) & !is.na(RETOLmcg)
            ),
            paste0(
                comments,
                " | CARTBEQmcg_std back calculated from VITA_RAEmcg and VITAmcg"
            ),
            comments
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




CARTBEQmcg_std_imputer_with_CARTBEQmcg <-
    function(dataset) {
        #' @title CARTBEQmcg_std_imputer_with_CARTBEQmcg
        #' @description Imputes values of CARTBEQmcg into CARTBEQmcg_std when they are NAs. then adds a comments
        #' @param CARTBEQmcg_std Beta-carotene equivalents, expressed in mcg per 100g of EP
        #' @param CARTBEQmcg Beta-carotene equivalents, is the sum of the beta-carotene + 1/2 quantity of other carotenoids with vitamin A activity, expressed in mcg per 100g of EP
        #' @return Original FCT with the imputed values

        columns <- c("CARTBEQmcg_std", "CARTBEQmcg")
        check_columns(dataset = dataset, columns = columns)
        tryCatch(
            dataset %>%
                as_tibble() %>%
                mutate_at(.vars = columns, .funs = as.numeric) %>%
                rowwise() %>%
                mutate(comments = ifelse(is.na(comments), "", comments)) %>%
                mutate(comments = ifelse((is.na(CARTBEQmcg_std) &
                    !is.na(CARTBEQmcg)),
                paste0(
                    comments,
                    "| CARTBEQmcg_std imputed with CARTBEQmcg"
                ),
                comments
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




CARTBEQmcg_std_to_zero <- function(dataset) {
    #' @title CARTBEQmcg_std_to_zero
    #' @description Handling Implausible values of CARTBEQmcg_std (Beta-carotene equivalents, expressed in mcg per 100g of EP). i.e. if CARTBEQmcg_std < 0 replace it with zero and add comments.
    dataset %>%
        as_tibble() %>%
        rowwise() %>%
        mutate(comments = ifelse((CARTBEQmcg_std < 0), paste0(comments, "| Impausible value of CARTBEQmcg_std = ", CARTBEQmcg_std, " replaced with 0"), comments)) %>%
        mutate(CARTBEQmcg_std = ifelse(CARTBEQmcg_std < 0, 0, CARTBEQmcg_std)) %>%
        ungroup()
}


ASHDFg_calculator <- function(dataset) {
  #' @description Calculates ASHDFg = 100-(WATERg + PROCNTg + FAT_g_standardised + CHOAVLg + FIBTGg + ALCg).
  #' Column names are case sensitive and error is thrown if not found.
  #' @param dataset :Required (FCT dataset to be checked)
  #' @param ASHDFg Ash calculated by difference in g per 100g EP 
  #' @param WATERg Water/ moisture content in g per 100g of EP
  #' @param PROCNTg Protein in g per 100g of EP, as reported in the original FCT and assumed to be calculated from nitrogen (NTg) content
  #' @param FAT_g_standardised fat content compiled from all tagnames in g per 100g of EP
  #' @param CHOAVLg Available carbohydrates calculated by weight in g per 100g of EP
  #' @param FIBTGg Total dietary fibre by AOAC Prosky method expressed in g per 100g of EP
  #' @param ALCg Alcohol in g per 100g
  #' @return Original FCT dataset with SOP_std column added
  #' @examples
  # Check presence of required columns
  columns <- c(
    "WATERg",
    "PROCNTg",
    "FAT_g_standardised",
    "CHOAVLg",
    "FIBTGg",
    "ALCg"
    )
  check_columns(dataset = dataset, columns = columns)
  tryCatch(
    dataset %>%
      as_tibble() %>%
      mutate_at(.vars = columns, .funs = as.numeric) %>%
      # ! Create a temp row with the count of NAs in the required columns
      mutate(temp = rowSums(is.na(
        dataset %>%
          select(all_of(columns))
      ))) %>%
      # Rowwise allows for per row evaluations.
      rowwise() %>%
      # ! If all the rows are NA then output is NA.
      # ! Else do the calculation and omit NAs.
      mutate(ASHDFg = ifelse(
        temp == length(columns),
        NA,
        100-sum(
          WATERg,
          PROCNTg,
          FAT_g_standardised,
          CHOAVLg,
          FIBTGg,
          ALCg,
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
          " should be numeric. The ASHDFg will not be calculated"
        )
      )
    }
  )
}
