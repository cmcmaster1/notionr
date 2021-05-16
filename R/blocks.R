#### NOTION BLOCKS #####

# properties blocks

#' Title
#'
#' @param title
#'
#' @return
#' @export
#'
#' @examples
title_block <- function(title){
  block <- glue::glue(
    '"properties": {
      "Name": {
        "type": "text",
        "id": "title",
        "title": [{
          "text": {"content": "<<title>>"}
        }]
      }
    }',
    .open = "<<",
    .close = ">>")

  block
}

# children blocks
## Text block
#' Title
#'
#' @param text
#'
#' @return
#' @export
#'
#' @examples
text_block <- function(text){
  block <- glue::glue(
    '{
      "object": "block",
      "type": "paragraph",
      "has_children": false,
      "paragraph": {
        "text":
          [{
            "type": "text",
            "text": {"content": "<<text>>"}
          }]
      }
    }',
    .open = "<<",
    .close = ">>")

  block
}

## Heading 1 block
#' Title
#'
#' @param h1
#'
#' @return
#' @export
#'
#' @examples
h1_block <- function(h1){
  block <- glue::glue(
    '{
      "object": "block",
      "type": "heading_1",
      "has_children": false,
      "heading_1": {
        "text":
          [{
            "type": "text",
            "text": {"content": "<<h1>>"}
          }]
      }
    }',
    .open = "<<",
    .close = ">>")

  block
}

## Heading 2 block
#' Title
#'
#' @param h2
#'
#' @return
#' @export
#'
#' @examples
h2_block <- function(h2){
  block <- glue::glue(
    '{
      "object": "block",
      "type": "heading_2",
      "has_children": false,
      "heading_2": {
        "text":
          [{
            "type": "text",
            "text": {"content": "<<h2>>"}
          }]
      }
    }',
    .open = "<<",
    .close = ">>")

  block
}

## Heading 3 block
#' Title
#'
#' @param h3
#'
#' @return
#' @export
#'
#' @examples
h3_block <- function(h3){
  block <- glue::glue(
    '{
      "object": "block",
      "type": "heading_3",
      "has_children": false,
      "heading_3": {
        "text":
          [{
            "type": "text",
            "text": {"content": "<<h3>>"}
          }]
      }
    }',
    .open = "<<",
    .close = ">>")

  block
}

## Code block
#' Title
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
code_block <- function(code){
  block <- glue::glue(
    '{
      "object": "block",
      "type": "paragraph",
      "has_children": false,
      "paragraph": {
        "text":
          [{
            "type": "text",
            "text": {"content": "<<code>>"},
            "annotations": {
              "code": true
            }
          }]
      }
    }',
    .open = "<<",
    .close = ">>")

  block
}

## Equation block
#' Title
#'
#' @param equation
#'
#' @return
#' @export
#'
#' @examples
equation_block <- function(equation){
  block <- glue::glue(
    '{
      "object": "block",
      "type": "paragraph",
      "has_children": false,
      "paragraph": {
        "text": [{"type": "equation", "equation": {"expression": "<<equation>>"}}]
        }
      }',
    .open = "<<",
    .close = ">>")

  block
}

## Join all children together
#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
children <- function(...){
  paste0('"children": [\n', paste0(c(...), collapse = ", \n"), '\n]')
}


