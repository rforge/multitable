".onAttach"=function(libname, pkgname) {
    if (interactive() && exists('packageVersion') && exists('packageStartupMessage')) {
        packageStartupMessage('Welcome to multitable ',as.character(packageVersion("multitable")),'\nType vignette("multitable") or help("multitable") to get started')
    }
    invisible(NULL)
}
