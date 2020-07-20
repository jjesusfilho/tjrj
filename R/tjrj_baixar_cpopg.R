#' Baixar cpopg do TJRJ
#'
#' @param processo Número do processo com ou sem separador
#' @param diretorio Diretório onde serão armazenados os htmls
#'
#' @return htmls baixados
#' @export
#'
tjrj_baixar_cpopg <- function(processo = NULL, diretorio = "."){


  if(rlang::is_empty(processo)){

    stop("Você deve fornecer um valor para o processo")

  }

  pb <- progress::progress_bar$new(total = length(processo))

  purrr::walk(processo, purrr::possibly(~{

    pb$tick()

    p1 <- stringr::str_remove_all(.x,"\\D")

    p2 <- abjutils::build_id(p1)

    url <-paste0("http://www4.tjrj.jus.br/consultaProcessoWebV2/consultaMov.do?v=2&numProcesso=",p2,"&acessoIP=internet&tipoUsuario=")

    arquivo <- file.path(diretorio,paste0(stringr::str_replace_all(Sys.Date(),"\\D","_"),"_",p1,".html"))

    xml2::read_html(url) %>%
      xml2::write_html(arquivo)

  },NULL))
}
