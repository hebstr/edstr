str_app <- \(data,
             user,
             password,
             download,
             filename) {

  user_base <-
  data.frame(.user = user,
             .password = password)

  ui <-
  fluidPage(loginUI(id = "login"),
            tabsetPanel(id = "tabs",
                        tabPanel("concepts", reactableOutput("concepts")),
                        tabPanel("UF exclues", reactableOutput("uf_exclus")),
                        tabPanel("texte", reactableOutput("texte")),
                        tabPanel("télécharger",
                                 downloadButton("download",
                                                "télécharger la sélection",
                                                class = "btn-danger"))))

    server <- \(input, output, session) {

        credentials <-
        loginServer(id = "login",
                    data = user_base,
                    user_col = .user,
                    pwd_col = .password)

        updateTabsetPanel(session, "tabs")

        output$concepts <-
        renderReactable({
          req(credentials()$user_auth)
          data$output$cpts
        })

        output$uf_exclus <-
        renderReactable({
          req(credentials()$user_auth)
          data$output$exclus_uf
        })

        output$texte <-
        renderReactable({
          req(credentials()$user_auth)
          data$output$text
        })

        output$download <-
        downloadHandler(filename = \() glue("{filename}_{Sys.Date()}.csv"),
                        content = \(file)
                          write_csv(data$str$br[getReactableState("texte", "selected"), download], file))

    }

  shinyApp(ui, server)

}
