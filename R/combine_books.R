#  books = file.path(getwd(), list.files("inst/sample", ".csv", full.names = TRUE))

#' combine_books
#'
#' @param books a simple table
#' @param fmt a format, defaults to NULL
#'
#' @return dataframe
#' @export
combine_books <- function(books, fmt = NULL) {
  dat = NULL
  if(is.null(fmt)){
    bn = basename(books[1])
    fmt = paste0(".", stringr::str_split(bn, "\\.")[[1]][[2]])
  }
  try({
  n = length(books)
  #dat = NULL
  if(fmt == ".xls"){
    dat = readxl::read_excel(books[1], sheet = "Fieldbook")
  }
  if(fmt == ".xlsx"){
    dat = readxl::read_excel(books[1], sheet = "Fieldbook")
  }
  if(fmt == ".csv"){
    dat = utils::read.csv(books[1])
  }

  add_locality_name <- function(dat, book, fmt = ".xls") {
    if(fmt == ".csv"){
      fn = basename(book)
      fn = stringr::str_replace(fn, fmt, "")
    }
    if(fmt == ".xls"){
      fn = readxl::read_excel(book, sheet = "Minimal")[1, "Value"]
      fn = as.character(fn)
    }
    if(fmt == ".xlsx"){
      fn = readxl::read_excel(book, sheet = "Minimal")[2, "Value"]
      fn = as.character(fn)
    }

    cbind(Locality = fn, dat)
  }
  dat = add_locality_name(dat, books[1], fmt)

  fin = ""
  for(i in 2:n){
    #print(i)
    if(fmt == ".csv") fin = utils::read.csv(books[i])
    if(fmt == ".xls") fin = readxl::read_excel(books[i], sheet = "Fieldbook")
    if(fmt == ".xlsx") fin = readxl::read_excel(books[i], sheet = "Fieldbook")

    fin = add_locality_name(fin, books[i], fmt)
    dat = rbind(dat, fin)
  }
  })
  dat
}

