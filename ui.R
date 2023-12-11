library(shiny)
library(shinythemes)

shinyUI(fluidPage(
    theme = shinytheme("simplex"),
    titlePanel("Hypothesis Test"),
    br(),
    br(),
    br(),
    
    sidebarPanel(
        # SlidebarPanel for t-test
        conditionalPanel(condition = "$('li.active a').first().html()==='Tentang t-test'",
                         h2("Tentang T-test:"),
                         p("t-test adalah metode pengujian statistik untuk mengetahui apakah rata-rata dari dua sampel yang dibandingkan berbeda secara signifikan"),
                         p("Metode t-test hanya bisa digunakan ketika ingin membandingkan rata-rata dua kelompok dengan perbandingan berpasangan. Sehingga jika ternyata data yang ingin anda bandingkan lebih dari dua kelompok, Uji T tidak bisa anda terapkan."),
                         p("Uji-T mengasumsikan data sesuai prinsip berikut ini :
                           - Data bersifat independen (mandiri),
                           - Data kurang lebih akan terdistribusi normal,
                           - Jumlah varian pada uji-t bersifat homogenitas varians atau bernilai sama untuk tiap tiap data kelompok yang diuji (dibandingkan)"),
                         br(),
                         tags$a(href = "https://wikielektronika.com/uji-t-adalah/", "Untuk lebih detail nya silahkan klik disini"),
                         br(),
                         br(),
                         sliderInput('range',
                                     'Range yang anda inginkan :',
                                     min = -50,
                                     max = 50,
                                     value = c(-10,10)),
                         sliderInput('df',
                                     'Derajat Kebebasan :',
                                     min = 1,
                                     max = 50,
                                     value = 1)
        ),
        
        # SlidebarPanel for file upload tab
        conditionalPanel(condition = "$('li.active a').first().html()==='View Data'",
                         fileInput('file1', 'Pilih file CSV',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         tags$hr(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"')
        ),
        
        # SliderbarPanel for t-test tab
        conditionalPanel(condition = "$('li.active a').first().html()==='T-test'",
                         sliderInput("bins",
                                     "Numer of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 2
                         ),
                         radioButtons("sample",
                                      "Pilih t-test satu sampel atau dua sampel :",
                                      choices = c("One sample" = "oneSamp", 
                                                  "Two sample" = "twoSamp")),
                         selectInput("var1", 
                                     label = "Pilih variabel numerik",
                                     ""
                         ),
                         conditionalPanel(condition = "input.sample == 'twoSamp'",
                                          selectInput("var2", 
                                                      label = "Pilih variabel numerik",
                                                      ""
                                          ),
                                          radioButtons("varequal",
                                                       "Apakah kedua sampel memiliki variansi yang setara:",
                                                       choices = c("Yes" = "y",
                                                                   "No" = "n"))
                         ),
                         selectInput("tail",
                                     label = "Pilih alternative hipotesis:",
                                     choices = c("Equal" = "two.sided", 
                                                 "Less" = "less",
                                                 "Greater" = "greater")),
                         conditionalPanel(condition = "input.sample == 'oneSamp'",
                                          numericInput("test",
                                                       "Nilai mean yang ingin di tes",
                                                       value = 0
                                                       
                                          )
                         ),
                         numericInput("conf",
                                      label = "Pilih confidence level(selang kepercayaan):",
                                      value = 0.95,
                                      min = 0.8,
                                      max = 0.99),
                         helpText("Note: Mohon masukkan angka antara 0 hingga 1 dengan input numerik")
                         
        ),
        
        # SliderbarPanel for prop test tab
        conditionalPanel(condition = "$('li.active a').first().html()==='Proportion Test'",
                         sliderInput("bins",
                                     "Numer of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 2
                         ),
                         numericInput("x", 
                                      "Banyaknya sukses dari sampel",
                                      value = 1,
                                      min = 1
                         ),
                         numericInput("n", 
                                     "Banyaknya sampel",
                                     value = 1,
                                     min = 1
                         ),
                         numericInput("p", 
                                      "Probabilitas kesuksesan (proporsi dalam H0)",
                                      value = 0.1,
                                      min = 0.1,
                                      max = 0.95
                         ),
                         
                         
                         selectInput("tail",
                                     label = "Pilih alternative hipotesis:",
                                     choices = c("Equal" = "two.sided", 
                                                 "Less" = "less",
                                                 "Greater" = "greater")),
                         numericInput("conf",
                                      label = "Pilih confidence level(selang kepercayaan):",
                                      value = 0.95,
                                      min = 0.8,
                                      max = 0.99),
                         helpText("Note: Mohon masukkan angka antara 0 hingga 1 dengan input numerik"),
                         
                         radioButtons("correct",
                                      "Pilih TRUE jika menggunakan faktor koreksi, pilih FALSE jika tidak",
                                      choices = c("TRUE" = "y",
                                                  "FALSE" = "n"))
                         
        ),
        # SliderbarPanel for variance test tab
        conditionalPanel(condition = "$('li.active a').first().html()==='Variance Test'",
                         sliderInput("bins",
                                     "Numer of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 2
                         ),
                         selectInput("x2", 
                                     label = "Pilih variabel numerik",
                                     ""
                         ),
                         selectInput("y", 
                                     label = "Pilih variabel numerik",
                                     ""
                         ),
                         numericInput("p", 
                                      "Probabilitas kesuksesan (proporsi dalam H0)",
                                      value = 0
                         ),
                         
                         selectInput("tail",
                                     label = "Pilih alternative hipotesis:",
                                     choices = c("Equal" = "two.sided", 
                                                 "Less" = "less",
                                                 "Greater" = "greater")),
                         numericInput("conf",
                                      label = "Pilih confidence level(selang kepercayaan):",
                                      value = 0.95,
                                      min = 0.8,
                                      max = 0.99),
                         helpText("Note: Mohon masukkan angka antara 0 hingga 1 dengan input numerik"),
                         
                         
        ),
        
        
        
        
    ),
    mainPanel(
        tabsetPanel(
            tabPanel('Tentang t-test',
                     plotOutput('tplot')),
            tabPanel('View Data', 
                     fluidRow(column(10, offset = 1,
                                     h2("Summary Data"),
                                     verbatimTextOutput('disc'))),
                     
                     fluidRow(column(10, offset = 1,
                                     h2("Struktur Data"),
                                     verbatimTextOutput('str'))),
                     fluidRow(column(10, offset = 1,
                                     h2("Tabel Data"),
                                     tableOutput('contents')))      
            ),           
            tabPanel('T-test',
                     fluidRow(column(10, offset = 1,
                                     plotOutput('graph'))),
                     fluidRow(column(8, offset = 1,
                                     h2("Hipotesis t-test"),
                                     p("Nilai t nya adalah :"),
                                     textOutput('tvalue'),
                                     br(),
                                     p("Jika p-value < 0,05 maka H0 ditolak artinya terdapat pengaruh yang signifikan antara satu variabel independen terhadap variabel dependen, 
                                        dan jika p-value >= 0,05 maka H0 diterima artinya tidak ada pengaruh yang signifikan antara satu variabel independen terhadap variabel dependen"),
                                     textOutput('pvalue')))
            ),
            tabPanel('Proportion Test',
                     fluidRow(column(8, offset = 1,
                                     h2("Hipotesis prop-test"),
                                     p("Nilai p nya adalah :"),
                                     textOutput('propvalue'),
                                     br(),
                                     p("Jika p-value < 0,05 maka H0 ditolak artinya terdapat pengaruh yang signifikan antara satu variabel independen terhadap variabel dependen, 
                                        dan jika p-value >= 0,05 maka H0 diterima artinya tidak ada pengaruh yang signifikan antara satu variabel independen terhadap variabel dependen"),
                                     textOutput('pvalue2')))
        
            ),
            tabPanel('Variance Test',
                     fluidRow(column(8, offset = 1,
                                     h2("Hipotesis var-test"),
                                     p("Nilai F nya adalah :"),
                                     textOutput('fvalue'),
                                     br(),
                                     p("Jika p-value < 0,05 maka H0 ditolak artinya terdapat pengaruh yang signifikan antara satu variabel independen terhadap variabel dependen, 
                                        dan jika p-value >= 0,05 maka H0 diterima artinya tidak ada pengaruh yang signifikan antara satu variabel independen terhadap variabel dependen"),
                                     textOutput('pvalue3')))
            ),
            
            
        )
    )
))