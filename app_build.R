# Este código es para hacer una build local del aplicativo web en html que usa
# github pages. SOLO DEBE EJECUTARSE UNA VEZ. Si se quiere correr la app en shiny,
# ir a myapp/app.R.

# Exporta la app (debe llamarse app.R) en myapp a docs
shinylive::export(appdir = "myapp", destdir = "docs")

# Para probar que funcione bien. Abre la app estática
httpuv::runStaticServer("docs/", port=8008)