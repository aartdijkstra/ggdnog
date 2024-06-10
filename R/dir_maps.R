#' Find directory with map data
#'
#' This function scans the directory tree relative from the working directory to find a specific directory in Sharepoint.
#' Note that it searches for specific keywords unique to GGD NOG, so running it outside this environment will yield unexpected results.
#'
#' @return The directory containing the shapefiles, or NA if not found.
#' @importFrom stringr str_detect
#' @importFrom stringr str_locate
#' @importFrom stringr str_sub
#' @importFrom stringr str_extract
#' @export
#'
#' @examples
#' paste0(dir_maps(), "/PC4.shp")
dir_maps = function () {
  cur = path.expand(getwd())
  if (stringr::str_detect(cur, "OneDrive")) {
    pos = stringr::str_locate(cur, "OneDrive")
    search = paste0("(", stringr::str_sub(cur, end=pos[1,"end"]), "[^/\\\\]+)")
    onedrive = stringr::str_extract(cur, search)

    result = scan_dir(onedrive)
    if (!is.na(result)) {
      return(paste0(result, "/"))
    }
    return(result)
  } else {
    result = scan_dir(cur)
    if (!is.na(result)) {
      return(paste0(result, "/"))
    }
    return(result)
  }
}

# Recursively scans the current folder and all folders up the tree for the occurence of 'Servicecentrum', 'NOG W Openbare' and 'Kaarten'.
# Whenever a folder does not contain 'NOG W Openbare', the folder one level up is scanned, until top level is reached.
# If the folder does contain 'NOG W Openbare', that directory is checked for subdirectory called 'Kaarten'. If this exists, the location is returned.
# If the folder contains 'Servicecentrum', the function moves into that folder and checks. To prevent endless recursion, folders up the tree from this location are not checked.
scan_dir = function (path, recursive=T) {
  search = "NOG W Openbare"
  dirs = basename(list.dirs(path, recursive=F))
  if (sum(stringr::str_detect(dirs, search)) == 1) {
    # check if 'Kaarten' exists as a subdirectory
    subdirs = basename(list.dirs(paste0(path, "/", dirs[stringr::str_detect(dirs, search)]), recursive=F))
    if ("Kaarten" %in% subdirs) {
      return(paste0(path, "/", dirs[stringr::str_detect(dirs, search)], "/Kaarten"))
    }
  } else if (any(stringr::str_detect(dirs, "Servicecentrum")) && recursive) {
    dirs = dirs[stringr::str_detect(dirs, "Servicecentrum")]
    for (dir in dirs) {
      scan_result = scan_dir(paste0(path, "/", dir), F)
      if (!is.na(scan_result)) {
        return(scan_result)
      }
    }
    return(NA)
  }
  if (dirname(path) == path) {
    return(NA)
  }
  return(scan_dir(dirname(path)))
}
