#' Gathers useful statistics about the underlying data (mean, median, sd, counts, frequencies etc.).
#'
#' @description
#' This is a slightly extended version of [summary()], combined with the functionality of [aggregate()] - but human readable.
#' Factors, logicals and strings are reported up to a total of 15 different options, after which other levels are truncated in ascending order.
#'
#' @param data The data to summarize.
#' @param group A grouping variable or list, if desired. This can be a character (column name), number (column index), or list of equal length to the variable in data.
#'
#' @return A data frame or list containing the results.
#' @import stats
#' @export
#'
#' @examples
#' data = data.frame(weight = rnorm(100, mean=70, sd=15),
#'                   gender = c(rep("male", 50), rep("female", 50)))
#' describe(data, "gender")
#' describe(data$weight, data$gender)
describe = function (data, group=NULL) {
  if (is.null(data)) return(data.frame())

  if (is.null(group)) {
    # parse entire dataset
    if (is.data.frame(data)) {
      # return value is a list of data frames, one per variable
      ret = list()
      for (i in 1:ncol(data)) {
        ret = append(ret, setNames(list(describeVar(data[[i]], colnames(data)[i])), colnames(data)[i]))
      }
      return(ret)
    } else if (is.list(data) || is.vector(data)) {
      return(describeVar(data))
    } else {
      return(describeVar(data))
    }
  } else {
    if (is.data.frame(data)) {
      group_var = NULL
      if (is.character(group) && length(group) == 1) {
        # group is a variable name
        if (group %in% colnames(data))
          group_var = which(colnames(data) == group)
      } else if (is.numeric(group) && length(group) == 1) {
        # group is a column index
        if (ncol(data) <= group)
          group_var = group
      }

      group_values = NULL
      if (is.null(group_var)) {
        # if there is no grouping variable, the list in group must be equal in size to the data frame
        if (length(group) != nrow(data))
          stop("The grouping variable must be supplied as either a column name or column index, or a list of equal size to the number of rows in the data frame.")

        group_values = group
      } else {
        group_values = data[[group_var]]
      }

      # return value is a list of lists of data frames, one list per group, and then inside that list one list per variable
      ret = list()
      groups = sort(unique(group_values))
      for (g in groups) {
        ret[[g]] = list()
        for (i in 1:ncol(data)) {
          ret[[g]] = append(ret[[g]], setNames(list(describeVar(data[[i]][which(group_values == g)], paste0(colnames(data)[i], " - ", g))), colnames(data)[i]))
        }
      }
      return(ret)
    } else if (is.factor(data) || is.character(data)) {
      # if the first variable is a factor or character, then the result of describeVar() is a list of frequencies
      # combining several of these lists becomes unreadable, so we need to split them up

      # group must be a list of equal size
      if (!is.list(group) && !is.vector(group) && !is.factor(group))
        stop("The supplied grouping variable must be a list, vector or factor.")
      if (length(group) != length(data))
        stop("The size of the second argument does not match the size of the first.")

      # return value is a list of lists of data frames, one list per group, and then inside that list one list per variable
      ret = list()
      groups = sort(unique(group))
      for (g in groups) {
        ret[[g]] = describeVar(data[which(group == g)])
      }
      return(ret)
    } else {
      # group must be a list of equal size
      if (!is.list(group) && !is.vector(group) && !is.factor(group))
        stop("The supplied grouping variable must be a list, vector or factor.")
      if (length(group) != length(data))
        stop("The size of the second argument does not match the size of the first.")

      # since it's one data type, we don't need lists - stick to a (nicer) data frame
      ret = data.frame()
      groups = sort(unique(group))
      for (g in groups) {
        ret = rbind(ret, describeVar(data[which(group == g)], g))
      }
      ret = rbind(ret, describeVar(data, "Total"))

      return(ret)
    }
  }
}

# shorthand function to eliminate long labels
# row names would be difficult to read otherwise
trunc_str = function (labels) {
  if (!is.character(labels)) return(labels)
  ret = labels
  ret[which(nchar(labels) > 25)] = paste0(substr(labels[which(nchar(labels) > 25)], 1, 22), "...")
  return(ret)
}

describeVar = function (data, rowname=NULL) {
  if (length(data) == 0) {
    return("Empty variable")
  }

  if (is.list(data))
    data = unlist(data)

  if (is.numeric(data) || is.double(data) || is.Date(data)) {
    if (all(is.na(data))) {
      return(data.frame(n=sum(!is.na(data)),
                        mean=NA,
                        sd=NA,
                        median=NA,
                        min=NA,
                        max=NA,
                        q25=NA,
                        q75=NA,
                        missing=sum(is.na(data)),
                        row.names=rowname))
    }
    return(data.frame(n=sum(!is.na(data)),
                      mean=mean(data, na.rm=T),
                      sd=stats::sd(data, na.rm=T),
                      median=stats::median(data, na.rm=T),
                      min=min(data, na.rm=T),
                      max=max(data, na.rm=T),
                      q25=stats::quantile(data, .25, na.rm=T),
                      q75=stats::quantile(data, .75, na.rm=T),
                      missing=sum(is.na(data)),
                      row.names=rowname))
  } else if (is.character(data) || is.factor(data) || is.logical(data)) {
    unique_values = sort(unique(data[which(!is.na(data))]))
    if (length(unique_values) > 15) {
      counts = sort(table(data), decreasing=T)
      ret = matrix(nrow=19, ncol=2)
      rownames(ret) = c("Unique values", "n", "Missing", "---", trunc_str(names(counts)[1:14]), "... list truncated")
      colnames(ret) = c("n", "%")
      ret[1,] = c(length(unique_values), NA)
      ret[2,] = c(sum(!is.na(data)), sum(!is.na(data))/length(data)*100)
      ret[3,] = c(sum(is.na(data)), sum(is.na(data))/length(data)*100)
      if (all(is.na(data))) {
        return(ret[1:3,])
      }
      ret[5:18,1] = counts[1:14]
      ret[5:18,2] = prop.table(counts)[1:14]*100
      ret[19,1] = sum(counts[15:length(counts)])
      ret[19,2] = sum(prop.table(counts)[15:length(counts)]*100)
      return(ret)
    } else {
      ret = matrix(nrow=length(unique_values)+4, ncol=2)
      rownames(ret) = c("Unique values", "n", "Missing", "---", trunc_str(unique_values))
      colnames(ret) = c("n", "%")
      ret[1,] = c(length(unique_values), NA)
      ret[2,] = c(sum(!is.na(data)), sum(!is.na(data))/length(data)*100)
      ret[3,] = c(sum(is.na(data)), sum(is.na(data))/length(data)*100)
      if (all(is.na(data))) {
        return(ret[1:3,])
      }
      counts = table(data)
      if (length(counts) != 0) {
        ret[5:(4+length(unique_values)),1] = counts
        ret[5:(4+length(unique_values)),2] = prop.table(counts)*100
      }
      return(ret)
    }
  }
  warning(paste0("Unknown data type: ", typeof(data), " - variable skipped."))
}
