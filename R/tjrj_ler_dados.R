#' Ler dados do TJRJ
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se não informar os arquivos
#'
#' @return tibble
#' @export
#'
tjrj_ler_dados <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){


    arquivos <-list.files(diretorio, "html",full.names = T)
  }


   pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    processo <- stringr::str_extract(.x,"\\d{20}")

    x <- xml2::read_html(.x) %>%
      xml2::xml_find_all("//form[@name='formResultado']/table") %>%
      rvest::html_table(fill=T) %>%
      purrr::pluck(1)


    comarca <- x$X1 %>%
      stringr::str_extract("Regional.+|Comarca.+") %>%
      subset(!is.na(.))

    if (rlang::is_empty(comarca)){
      comarca <- NA_character_
    }

    vara <- x %>%
      dplyr::filter(stringr::str_detect(X1,!!comarca)) %>%
      dplyr::pull(X2)

    if (rlang::is_empty(vara)){
      vara <- NA_character_
    }

    # processo <- x$X1 %>%
    #   stringr::str_squish() %>%
    #   stringr::str_extract("(?<=Processo....)\\d+.+") %>%
    #   subset(!is.na(.))
    #
    # if (rlang::is_empty(processo)){
    #   processo <- NA_character_
    # }

    bairro <- x %>%
      dplyr::filter(stringr::str_detect(X1,"Bairro")) %>%
      dplyr::pull(X2)

    if (rlang::is_empty(bairro)){
      bairro <- NA_character_
    }

    classe <- x %>%
      dplyr::filter(stringr::str_detect(X1,"Classe")) %>%
      dplyr::pull(X2)

    if (rlang::is_empty(classe)){
      classe <- NA_character_
    }

    acao <- x %>%
      dplyr::filter(stringr::str_detect(X1,"^A\u00e7\u00e3o")) %>%
      dplyr::pull(X2)

    if (rlang::is_empty(acao)){
      acao <- NA_character_
    }

    assunto <- x %>%
      dplyr::filter(stringr::str_detect(X1,"^Assunto")) %>%
      dplyr::pull(X2)

    if (rlang::is_empty(assunto)){
      assunto <- NA_character_
    }

    autor <- x %>%
      dplyr::filter(stringr::str_detect(X1,"^Autor$")) %>%
      dplyr::pull(X2)

    if (rlang::is_empty(autor)){
      autor <- NA_character_
    }

    representante_legal <- x %>%
      dplyr::filter(stringr::str_detect(X1,"^(Representante.+|Advogad.+)")) %>%
      dplyr::pull(X2)

    if (rlang::is_empty(representante_legal)){
      representante_legal <- NA_character_
    }

    reu <- x %>%
      dplyr::filter(stringr::str_detect(X1,"^(R\u00e9u|Autor do Fato)")) %>%
      dplyr::pull(X2) %>%
      .[[1]]

    if (rlang::is_empty(reu)){
      reu <- NA_character_
    }

    processo_tj <- x %>%
      dplyr::filter(stringr::str_detect(X1,"Processo\\(s\\) no")) %>%
      dplyr::pull(X2)

    if (rlang::is_empty(processo)){
      processo_tj <- NA_character_
    }

    data_distribuicao <- x$X1 %>%
      stringr::str_subset("Distribu.do") %>%
      stringr::str_extract("\\d\\S+")

    if (rlang::is_empty(data_distribuicao)){
      data_distribuicao <- NA_character_
    }

    data_arquivamento <- x$X1 %>%
      stringr::str_subset("(?i)Arquivado") %>%
      stringr::str_extract("\\d{2}/.+")

    if (rlang::is_empty(data_arquivamento)){
      data_arquivamento <- NA_character_
    }

    nomes<-Hmisc::Cs(processo,comarca,vara,assunto,classe,acao,autor,representante_legal,reu,data_distribuicao,data_arquivamento)

    df <- list(processo,comarca,vara,assunto,classe,acao,autor,representante_legal,reu,data_distribuicao,data_arquivamento) %>%
      purrr::set_names(nomes)

    dplyr::bind_rows(df)

  },NULL))



}
