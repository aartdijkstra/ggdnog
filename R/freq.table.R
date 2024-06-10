#' Frequency table with proportions
#'
#' This function combines [table()] and [proportions()] to output a human-readable frequency table. `freq.table(x,y)` is essentially a shorthand for calling `table(x,y)` and `proportions(table(x,y))` consecutively.
#'
#' @param ... Elements to crosstabulate. This is analogous to the structure of table().
#' @param is.list Specifies if the supplied arguments in ... are a list and should be interpreted as a list of tables to be constructed.
#' @param is.table Specifies if the supplied arguments in ... are a tibble, and should have the first column interpreted as row names.
#' @param row.prop Specifies whether row proportions should be shown.
#' @param col.prop Specifies whether column proportions should be shown.
#' @param num.fmt Specifies the display style for the proportions in [sprintf()]-format. Default is .2f.
#'
#' @return A formatted matrix containing either 2 columns and n rows for a one-dimensional table, or m*2+1 columns and n*2+1 rows for a two-dimensional table.
#' @importFrom stats chisq.test
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' dataset = c(rep(1, 15), rep(2, 5))
#' freq.table(dataset)
freq.table = function (..., is.list=F, is.table=F, row.prop=T, col.prop=T, num.fmt=".2f") {
  if (is.table) {
    result = tibble::as_tibble(...)

    # eerste kolom wordt rijnamen, daarna omzetten naar matrix
    tmp = result
    result = as.matrix(result[,-1])
    rownames(result) = as.character(tmp[[1]])
  } else if (is.list) {
    vars = list(...)
    full.set = lapply(vars, freq.table)
    names(full.set) = sapply(..., deparse)

    return(full.set)
  }
  else result = table(...)
  if (all(dim(result) == 0)) return(data.frame(NA))
  ret = result
  if (length(dim(result)) == 1) {
    # geen rijen of kolommen, alleen een lijst
    prop = proportions(result)*100
    tmp = as.data.frame(matrix(nrow = length(ret)+1, ncol=2))
    tmp[1:(nrow(tmp)-1),seq(from=1,to=ncol(tmp)-1,by=2)] = ret
    tmp[1:(nrow(tmp)-1),seq(from=2,to=ncol(tmp),by=2)] = sprintf("%.2f%%", prop)
    ret = tmp
    ret[nrow(ret),1] = sum(ret[,1], na.rm=T)
    rownames(ret) = c(names(result), "Totaal")
    colnames(ret) = c("n", "Perc")
    return(ret)
  }

  # totalen bewaren voor later
  totals.row = rowSums(ret)
  totals.col = colSums(ret)

  if (row.prop) {
    prop = proportions(result, margin=2)*100
    tmp = as.data.frame(matrix(nrow = (nrow(result)*2)+1, ncol=ncol(ret)))
    tmp[seq(from=1,to=nrow(tmp)-2,by=2),] = ret
    tmp[seq(from=2,to=nrow(tmp)-1,by=2),] = sprintf(paste0("%", num.fmt, "%%"), prop)
    ret = tmp
  }
  if (col.prop) {
    prop = proportions(result, margin=1)*100
    tmp = as.data.frame(matrix(nrow = nrow(ret), ncol=(ncol(ret)*2)+1))
    tmp[,seq(from=1,to=ncol(tmp)-1,by=2)] = ret
    if (row.prop) {
      # als hierboven rijen zijn ingevoegd moeten die rijen worden overgeslagen
      # bij het invoeren van de nieuwe waardes
      # praktisch betekent dit een lege regel tussen iedere waarde
      tmp[seq(from=1,to=nrow(tmp)-1,by=2),seq(from=2,to=ncol(tmp),by=2)] = sprintf(paste0("%", num.fmt, "%%"), prop)
    }
    else {
      tmp[,seq(from=2,to=ncol(tmp)-1,by=2)] = sprintf(paste0("%", num.fmt, "%%"), prop)
    }
    ret = tmp
  }

  # col-/rownames pas aan het einde toevoegen; bij het aanmaken van tmp gaan die
  # telkens verloren
  if (row.prop) {
    rownames(ret) = sprintf("tmp%d", 1:nrow(ret))
    rownames(ret)[seq(from=1,to=nrow(ret)-2,by=2)] = rownames(result)
    rownames(ret)[seq(from=2,to=nrow(ret)-1,by=2)] = sprintf("P%d", 1:(nrow(ret)/2))
  }
  else {
    rownames(ret) = rownames(result)
  }
  if (col.prop) {
    colnames(ret)[seq(from=1,to=ncol(ret)-2,by=2)] = colnames(result)
    colnames(ret)[seq(from=2,to=ncol(ret)-1,by=2)] = sprintf("P%d", 1:(ncol(ret)/2))
  }
  else {
    colnames(ret) = colnames(result)
  }

  # totalen toevoegen
  ret[nrow(ret),seq(from=1,to=ncol(ret)-1,by=2)] = totals.col
  ret[nrow(ret),seq(from=2,to=ncol(ret)-1,by=2)] = sprintf(paste0("%", num.fmt, "%%"), proportions(totals.col)*100)
  rownames(ret)[nrow(ret)] = "Totaal"
  ret[seq(from=1,to=nrow(ret)-1,by=2),ncol(ret)] = totals.row
  ret[seq(from=2,to=nrow(ret)-1,by=2),ncol(ret)] = sprintf(paste0("%", num.fmt, "%%"), proportions(totals.row)*100)
  colnames(ret)[ncol(ret)] = "Totaal"

  test = suppressWarnings(stats::chisq.test(result))

  ret[nrow(ret),ncol(ret)] = sprintf("p = %0.3e", test$p.value)

  return(ret)
}
