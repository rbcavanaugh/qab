shinylive::export(here::here(), here::here("docs"))

httpuv::runStaticServer(here::here("site/"))
