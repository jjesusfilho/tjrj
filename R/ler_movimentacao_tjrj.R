#' Ler movimentação do tjrj
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório se arquivos não forem informados
#'
#' @return tibble
#' @export
#'
ler_movimentacao_tjrj <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,"html",full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    x<-xml2::read_html(.x)

 processo <- stringr::str_extract(.x,"\\d{20}")

outros <- x %>%
  xml2::xml_find_all("//tr[@class='tipoMovimento']/td|//tr[@class='tipoMovimento']/td[2]/../following-sibling::tr") %>%
  xml2::xml_text() %>%
  stringr::str_replace("^\\s*$","@@") %>%
  stringr::str_c(collapse=";") %>%
  strsplit("@@",fixed=TRUE) %>%
  unlist() %>%
  stringr::str_subset("(?i)data") %>%
  purrr::map(~stringr::str_split(.x,";")) %>%
         purrr::flatten() %>%
   purrr::map(~stringr::str_subset(.x,"(:$|^$)",negate=TRUE)) %>%
   purrr::map(~stringr::str_subset(.x,":")) %>%
   purrr::map(~stringr::str_split(.x,":")) %>%
   purrr::map_dfr(~{
  valor1 <- .x[[1]][[2]]
  valor2 <- .x[[2]][[2]]
  tibble::tibble(valor1,valor2) %>%
    purrr::set_names(c("tipo_movimento","data")) %>%
    tibble::add_column(processo,.before=1)
})

},NULL))

}


