shinylive::export(here::here(), here::here("docs"), package_cache = TRUE)

httpuv::runStaticServer(here::here("docs"))

