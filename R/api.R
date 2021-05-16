
new_notion_db <- function(name =  character(),
                          secret_token = character(),
                          db_token = character()){
  stopifnot(is.character(name))
  stopifnot(is.character(secret_token))
  stopifnot(is.character(db_token))

  auth <- glue::glue("Bearer {secret_token}")
  header <- httr::add_headers("Authorization" = auth,
                              "Content-Type" = "application/json")

  structure(.Data = name,
            secret = secret_token,
            db = db_token,
            head = header,
            class = "notion_db")
}

validate_notion_db <- function(x){
  values <- unclass(x)
  db_token <- attr(x, "db")

  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }

  if (nchar(db_token) != 32) {
    stop(
      "The database token must be 32 characters long",
      call. = FALSE
    )
  }


  x
}

#' Title
#'
#' @param name
#' @param secret_token
#' @param db_token
#'
#' @return
#' @export
#'
#' @examples
notion_db <- function(name =  "db",
                      secret_token = character(),
                      db_token = character()) {

  validate_notion_db(new_notion_db(name, secret_token, db_token))
}

########### NOTION PAGES ##############
new_notion_page <- function(name =  character(),
                          secret_token = character(),
                          page_token = character()){
  stopifnot(is.character(name))
  stopifnot(is.character(secret_token))
  stopifnot(is.character(page_token))

  auth <- glue::glue("Bearer {secret_token}")
  header <- httr::add_headers("Authorization" = auth,
                              "Content-Type" = "application/json")

  structure(.Data = name,
            secret = secret_token,
            page = page_token,
            head = header,
            class = "notion_db")
}

validate_notion_page <- function(x){
  values <- unclass(x)
  page_token <- attr(x, "page")

  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }

  if (nchar(page_token) != 36) {
    stop(
      "The page token must be 36 characters long",
      call. = FALSE
    )
  }


  x
}

#' Title
#'
#' @param name
#' @param secret_token
#' @param page_token
#'
#' @return
#' @export
#'
#' @examples
notion_page <- function(name =  "page",
                      secret_token = character(),
                      page_token = character()) {

  validate_notion_page(new_notion_page(name, secret_token, page_token))
}

############ NOTION QUERIES ###############

#' Query a Notion Datbase
#'
#' @param db
#' @param query
#'
#' @return
#' @export
#'
#' @examples
notion_db_query <- function(db = notion_db(), query = character()) {
  db_token <- attr(db, "db")
  header <- attr(db, "head")
  address <- glue::glue("https://api.notion.com/v1/databases/{db_token}/query")

  query_db <- httr::POST(url = address,
                         header,
                         body = query)

  content <- httr::content(query_db)$results

  tags <- purrr::map(purrr::map(content, ~.x$properties$Tags$multi_select), ~unlist(.x, recursive = FALSE))
  tags <- unlist(purrr::map(purrr::map(tags, ~.x[names(.x) == "name"]), ~stringr::str_c(.x, collapse = ", ")))

  database <- data.frame(
    title = unlist(purrr::map(content, ~.x$properties$Name$title[[1]]$text$content)),
    created = unlist(purrr::map(content, ~.x$created_time)),
    id = unlist(purrr::map(content, ~.x$id)),
    tags = tags
  )

  structure(
    database,
    class = c("notion_db_query", "data.frame"),
    db_id = db_token
  )
}

### PAGES
#' Title
#'
#' @param page
#'
#' @return
#' @export
#'
#' @examples
notion_page_query <- function(page){

  page_token <- attr(page, "page")
  header <- attr(page, "head")

  address <- glue::glue("https://api.notion.com/v1/blocks/{page_token}/children")

  query_page <- httr::GET(url = address,
                          header)

  content <- httr::content(query_page)

  structure(
    content$results,
    class = "notion_page_query"
  )

}
