#' Install the gruvboxr theme
#'
#' You'll need RStudio at least 1.2.x and if your RStudio
#' is installed to a default location on Windows or Linux,
#' you'll need to be running RStudio as
#' Administrator to install the menu theme files (Just required for install).
#'
#' You can elect not to install the menu theme files with `menus = FALSE`.
#'
#' @param menus if FALSE do not install the RStudio menu theme qss files.
#' @return nothing.
#' @export
install_theme <- function(menus = TRUE) {

  ## check RStudio API available
  if(!rstudioapi::isAvailable()) stop("gruvboxr must be installed from within RStudio.")

  ## check RStudio supports themes
  if(utils::compareVersion(as.character(rstudioapi::versionInfo()$version), "1.2.0") < 0)
    stop("You need RStudio 1.2 or greater to get theme support")

  ## check if menu theme already installed and uninstall
  if(gruvboxr_installed()){
    uninstall_theme()
  }

  ## add the theme
  gruvboxr_theme <- rstudioapi::addTheme(system.file(fs::path("resources","gruvboxr.rstheme"),
                                                     package = "gruvboxr"))

  ## add the cusom Qt css
  if (menus) activate_menu_theme()

  rstudioapi::applyTheme(gruvboxr_theme)
}

#' Uninstall the gruvboxr theme
#'
#' @return nothing.
#' @export
uninstall_theme <- function(){

  deactivate_menu_theme()
  rstudioapi::removeTheme("gruvboxr")

}


#' Activate gruvboxr styling in file menu.
#'
#' @return nothing.
#' @export
activate_menu_theme <- function() {

  ## Styling menus not supported on Mac or RStudio Server.
  if(host_os_is_mac() | is_rstudio_server()) return(NULL)

  if(file.exists(gnome_theme_dark_backup()) |
     file.exists(windows_theme_dark_backup())) {
      message("Rgruvboxr menu theme already activated. Deactivate first.")
      return(FALSE)
  }

  ## backup dark Qt themes
  file.copy(from = gnome_theme_dark(),
            to = gnome_theme_dark_backup())
  file.copy(from = windows_theme_dark(),
            to = windows_theme_dark_backup())

  ## replace with gruvboxr Qt themes
  file.copy(from = system.file(fs::path("resources","stylesheets","rstudio-gnome-dark.qss"),
                               package = "gruvboxr"),
            to = gnome_theme_dark(),
            overwrite = TRUE)
  file.copy(from = system.file(fs::path("resources","stylesheets","rstudio-windows-dark.qss"),
                               package = "gruvboxr"),
            to = windows_theme_dark(),
            overwrite = TRUE)
}

#' Deactivate gruvboxr style in file menu.
#'
#' @return nothing.
#' @export
deactivate_menu_theme <- function(){

  ## Styling menus not supported on Mac.
  if(host_os_is_mac()) return(NULL)

  if(!file.exists(gnome_theme_dark_backup()) |
     !file.exists(windows_theme_dark_backup())) {
    message("RStudio theme backups not found. gruvboxr already deactivated?")
    return(FALSE)
  }

  ## restore dark Qt themes
  file.copy(from = gnome_theme_dark_backup(),
            to = gnome_theme_dark(),
            overwrite = TRUE)
  file.copy(from = windows_theme_dark_backup(),
            to = windows_theme_dark(),
            overwrite = TRUE)

  ## delete backups
  unlink(gnome_theme_dark_backup())
  unlink(windows_theme_dark_backup())

  }

