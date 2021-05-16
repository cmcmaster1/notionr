#' Convert RMarkdown Document to Notion API compatible JSON
#'
#' @param md_file A .Rmd file
#'
#' @return A list containing the
#' @export
#'
#' @examples
#'
read_md <- function(md_file){
  md_text <- readLines(md_file)

  title_loc <- min(
    which(
      grepl("^title:", md_text)
    )
  )
  # The title will always predictably start at the 8th character, after title:
  title <- gsub('"',
                '',
                substring(md_text[title_loc], 8))

  title <- title_block(title)

  md_start <- max(which(md_text == "---")) + 1
  md_end <- length(md_text)

  md_text <- md_text[md_start:md_end]
  md_text <- md_text[md_text != ""]

  code_blocks <- which(grepl("```", md_text))
  code_chunks <- split(code_blocks, ceiling(seq_along(code_blocks) / 2))

  code_chunks <- unlist(purrr::map(code_chunks, ~.[1]:.[2]), use.names = FALSE)
  code_chunks <- setdiff(code_chunks, code_blocks)
  h1_chunks <- which(stringr::str_detect(md_text, "^#(?!#)"))
  h2_chunks <- which(stringr::str_detect(md_text, "^##(?!#)"))
  h3_chunks <- which(stringr::str_detect(md_text, "^###(?!#)"))

  text_chunks <- (1:length(md_text))[-c(code_chunks, h1_chunks, h2_chunks, h3_chunks)]

  md_text <- stringr::str_remove_all(md_text, "^#+ ")

  for (i in 1:length(md_text)){
    if (i %in% code_chunks) {
      md_text[i] <- code_block(md_text[i])
    } else if(i %in% h1_chunks){
      md_text[i] <- h1_block(md_text[i])
    } else if(i %in% h2_chunks){
      md_text[i] <- h2_block(md_text[i])
    } else if(i %in% h3_chunks){
      md_text[i] <- h3_block(md_text[i])
    } else if(i %in% text_chunks){
      md_text[i] <- text_block(md_text[i])
    }
  }

  md_text <- md_text[-code_blocks]

  list(title, md_text)
}

new_notion_md <- function(md_file =  character()){
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

post_md <- function(db = notion_db(), md_file = character(), debug = FALSE){
  db_token <- attr(db, "db")
  parent <- glue::glue(
    '"parent": {
      "database_id": "<<db_id>>"
    }',
    .open = "<<",
    .close = ">>"
  )

  md_json <- read_md(md_file)

  json <- stringr::str_c(
    "{",
    stringr::str_c(
      parent,
      md_json[[1]],
      children(md_json[[2]]),
      sep = ",\n"
    ),
    "}"
  )

  post <- httr::POST(url = "https://api.notion.com/v1/pages",
             attr(db, "head"),
             body = json)

  if (debug == TRUE) {
    list(json, post)
  } else {
    post
  }
}

