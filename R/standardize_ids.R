#' Standardizes IDs to match main data repository format
#'
#' @param df dataframe containing ID variable
#' @param var name of the ID variable
#'
#' @returns A dataframe with a new variable column "std_id" that contains the standardized IDs
#' @export
#'
#' @examples
#' sample_data <- as.data.frame(list(
#'   id = c("123"," 0 123","00-123","0123","000456789","``3456","3456"),
#'   name = c(
#'   "ABC Incorporated","AlphaBravoCharlie Inc.","abc inc","AlfaBravoCharlie Incorp",
#'   "DEF Company","ghi services","ghi sercives")
#'   ), stringsAsFactors = FALSE)
#'
#' standardize_ids(sample_data, id)
#'
standardize_ids <- function(df, var) {

  df |>
    dplyr::mutate(
      std_id = stringr::str_replace_all(
        {{ var }},
        "[^\\p{N}]",
        ""
      ),
      std_id = dplyr::if_else(
        nchar(.data$std_id) < 9,
        stringr::str_pad(.data$std_id, width = 9, pad = "0", side = "left"),
        .data$std_id
      )
    )
}
