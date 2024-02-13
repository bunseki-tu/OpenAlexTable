#
# OpenAlexTable
#
# OpenAlexのAPIを利用して複数機関の数字をまとめて取得してテーブル表示するツールです。
# OpenAlexのAPIおよびopenalexRについて手を動かして知ろうとしたらできあがりました。
# 誰かの何かの参考になるかもしれないので公開します。
# 
# 使い方
# Rstudioで開いてRun App押せば起動します。
# 色々と選択し，Calcボタンを押せば右に表が表示されます。
# 表は各種形式でダウンロード可能。
# 
# 
#
# OpenAlexのAPIのリミットは
#  max 100,000 calls every day, and also
#  max 10 requests every second.
# https://docs.openalex.org/how-to-use-the-api/rate-limits-and-authentication
# なのでSys.sleep(0.2)を挟むことで1秒あたりのリミットにはいかないようにしています
# 1日あたりのリミットについてはツール側で対応していないので，使用者の責任で使いすぎないようにしてください
#
#


library(tidyverse)
library(shiny)
library(DT)
library(openalexR)

# univname，univid，universitylistは外部ファイル読み取って作ることを考えたが，実験用としてR11だけ決め打ちで
univname = c("Hokkaido","Tohoku","Tsukuba","Tokyo","TokyoTech","Nagoya","Kyoto","Osaka","Kyushu","Keio","Waseda")
univid = c("I205349734","I201537933","I146399215","I74801974","I114531698","I60134161","I22299242","I98285908","I135598925","I203951103","I150744194")
universitylist <- list("Hokkaido", "Tohoku", "Tsukuba", "Tokyo", "TokyoTech", "Nagoya","Kyoto", "Osaka", "Kyushu", "Keio", "Waseda")

workstypelist <- list("article","book-chapter","dissertation","book","dataset","paratext","other","reference-entry","report","peer-review","standard","erratum","editorial","grant","letter")
indicatorlist <- c("OutputCount",
                   "InternationalCount",
                   "Education-CompanyCount",
                   "CoAuthInstCount",
                   "AuthCount",
                   "FounderCount",
                   "SDGsCount",
                   "OAstatusCount",
                   "CitationPercentilebyYear",
                   "ConceptCount")

#######################################
# methods

# 大学リストの突合
match_unividlist <- function(ulist){
  uidlist <- c()
  for (u in ulist){
    uid <- univid[match(u, univname)]
    uidlist <- c(uidlist, uid)
  }
  return(uidlist)
}

# 文献数カウント
openalex_OutputCount <- function(instname, instid, ys, mtypes){
  inst_pubcount <- oa_fetch(
    authorships.institutions.lineage = instid,
    publication_year = ys,
    type = mtypes,
    group_by = "publication_year"
  )
  Sys.sleep(0.2)
  inst_pubcount <- rename(inst_pubcount, !!as.character(instname) := count)
  return(inst_pubcount[,-2])
}

#　助成機関ごとのカウント
openalex_FounderCount <- function(instname, instid, ys, mtypes){
  inst_foundcount <- oa_fetch(
    authorships.institutions.lineage = instid,
    publication_year = ys,
    type = mtypes,
    group_by = "grants.funder"
  )
  Sys.sleep(0.2)
  inst_foundcount <- rename(inst_foundcount, !!as.character(instname) := count)
  return(inst_foundcount)
}

# 国際共著数カウント
openalex_InternationalCount <- function(instname, instid, ys, mtypes){
  inst_pubcount <- oa_fetch(
    authorships.institutions.lineage = instid,
    publication_year = ys,
    type = mtypes,
    countries_distinct_count = "2-",
    group_by = "publication_year"
  )
  Sys.sleep(0.2)
  inst_pubcount <- rename(inst_pubcount, !!as.character(instname) := count)
  return(inst_pubcount[,-2])
}

# 産学共著カウント
openalex_EduComCount <- function(instname, instid, ys, mtypes){
  inst_pubcount <- oa_fetch(
    authorships.institutions.lineage=instid,
    authorships.institutions.type="company",
    authorships.institutions.type="education",
    publication_year=ys,
    type=mtypes,
    group_by="publication_year"
  )
  Sys.sleep(0.2)
  inst_pubcount <- rename(inst_pubcount, !!as.character(instname) := count)
  return(inst_pubcount[,-2])
}

# SDGsカウント
openalex_SDGsCount <- function(instname, instid, ys, mtypes){
  inst_foundcount <- oa_fetch(
    authorships.institutions.lineage = instid,
    publication_year = ys,
    type = mtypes,
    group_by = "sustainable_development_goals.id"
  )
  Sys.sleep(0.2)
  inst_foundcount <- rename(inst_foundcount, !!as.character(instname) := count)
  return(inst_foundcount)
}

# 共著先機関カウント
openalex_CoAuthInstCount <- function(instname, instid, ys, mtypes){
  inst_foundcount <- oa_fetch(
    authorships.institutions.lineage = instid,
    publication_year = ys,
    type = mtypes,
    group_by = "authorships.institutions.lineage"
  )
  Sys.sleep(0.2)
  inst_foundcount <- rename(inst_foundcount, !!as.character(instname) := count)
  return(inst_foundcount)
}

# Conceptカウント
openalex_ConceptCount <- function(instname, instid, ys, mtypes){
  inst_foundcount <- oa_fetch(
    authorships.institutions.lineage = instid,
    publication_year = ys,
    type = mtypes,
    group_by = "concepts.id"
  )
  Sys.sleep(0.2)
  inst_foundcount <- rename(inst_foundcount, !!as.character(instname) := count)
  return(inst_foundcount)
}

# 著者数ごとのカウント
openalex_AuthCount <- function(instname, instid, ys, mtypes){
  inst_foundcount <- oa_fetch(
    authorships.institutions.lineage = instid,
    publication_year = ys,
    type = mtypes,
    group_by = "authors_count"
  )
  Sys.sleep(0.2)
  inst_foundcount <- rename(inst_foundcount, !!as.character(instname) := count)
  return(inst_foundcount)
}

# OAステータスカウント
openalex_OAstatusCount <- function(instname, instid, ys, mtypes){
  inst_foundcount <- oa_fetch(
    authorships.institutions.lineage = instid,
    publication_year = ys,
    type = mtypes,
    group_by = "open_access.oa_status"
  )
  Sys.sleep(0.2)
  inst_foundcount <- rename(inst_foundcount, !!as.character(instname) := count)
  return(inst_foundcount)
}

# 年ごとの被引用パーセンタイルカウント
openalex_CitationPercentilebyYear <- function(instname, instid, ys, mtypes){
  inst_foundcount <- oa_fetch(
    authorships.institutions.lineage = instid,
    publication_year = ys,
    type = mtypes,
    group_by = "cited_by_percentile_year.min"
  )
  Sys.sleep(0.2)
  inst_foundcount <- rename(inst_foundcount, !!as.character(instname) := count)
  return(inst_foundcount)
}


#################################################################################
# Define UI
ui <- fluidPage(

    # Application title Google検索して引っかからなかったのでこの名前に
    titlePanel("OpenAlexTable"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            #  どの指標の推移をみるのか
            selectInput('indicator', 'Indicator', choices = indicatorlist, selected = "OutputCount"),
            # どの機関の数をみるのか
            selectInput("univ", "University", universitylist, multiple = TRUE, selected = "Tohoku"),
            # どの期間の数をみるのか，とりあえず2000-2023にしている
            sliderInput("year", "Publication Year", 2000, 2023, value = c(2000, 2023),sep = ""),
            # どのタイプの数をみるのか
            selectInput('mworkstype', 'Works Type', choices = workstypelist, multiple = TRUE, selected = "article"),
            # ボタン
            actionButton("goButton", "Calc")
        ),

        # 色々表示するメインパネル
        mainPanel(
           # テーブルを表示する
           h3("Table"),
           textOutput("numt"),
           dataTableOutput("numtable")
        )
    )
)


#################################################################################
# Define server logic
server <- function(input, output){
  # inputs
  tmp <- reactiveValues(
    mtypes = "no"
  )

  observeEvent(input$goButton, {
    tmp$indicator <- input$indicator
    tmp$year1 <- input$year[1]
    tmp$year2 <- input$year[2]
    tmp$years <- paste(tmp$year1, "-", tmp$year2)
    tmp$instname <- input$univ
    tmp$instidlist <- match_unividlist(input$univ)
    tmp$mtypes <- paste(input$mworkstype, collapse = "|")

    if (tmp$mtypes == "no"){
      NULL
    }else{
      if (tmp$indicator == "OutputCount"){
        i <- 1
        for (uid in tmp$instidlist){
          result <- openalex_OutputCount(tmp$instname[i], uid, tmp$years, tmp$mtypes)
          if (i == 1){
            idata <- result
          }else{
            idata <- left_join(idata, result, by=c("key"="key"))
          }
          i <- i + 1
        }
      } else if (tmp$indicator == "FounderCount"){
        i <- 1
        for (uid in tmp$instidlist){
          result <- openalex_FounderCount(tmp$instname[i], uid, tmp$years, tmp$mtypes)
          if (i == 1){
            idata <- result
          }else{
            idata <- dplyr::full_join(idata, result, by=c("key","key_display_name"))
          }
          i <- i + 1
        }
      } else if (tmp$indicator == "InternationalCount"){
        i <- 1
        for (uid in tmp$instidlist){
          result <- openalex_InternationalCount(tmp$instname[i], uid, tmp$years, tmp$mtypes)
          if (i == 1){
            idata <- result
          }else{
            idata <- left_join(idata, result, by=c("key"="key"))
          }
          i <- i + 1
        }
      } else if (tmp$indicator == "Education-CompanyCount"){
        i <- 1
        for (uid in tmp$instidlist){
          result <- openalex_EduComCount(tmp$instname[i], uid, tmp$years, tmp$mtypes)
          if (i == 1){
            idata <- result
          }else{
            idata <- left_join(idata, result, by=c("key"="key"))
          }
          i <- i + 1
        }
      } else if (tmp$indicator == "SDGsCount"){
        i <- 1
        for (uid in tmp$instidlist){
          result <- openalex_SDGsCount(tmp$instname[i], uid, tmp$years, tmp$mtypes)
          if (i == 1){
            idata <- result
          }else{
            idata <- dplyr::full_join(idata, result, by=c("key","key_display_name"))
          }
          i <- i + 1
        }
      } else if (tmp$indicator == "CoAuthInstCount"){
        i <- 1
        for (uid in tmp$instidlist){
          result <- openalex_CoAuthInstCount(tmp$instname[i], uid, tmp$years, tmp$mtypes)
          if (i == 1){
            idata <- result
          }else{
            idata <- dplyr::full_join(idata, result, by=c("key","key_display_name"))
          }
          i <- i + 1
        }
      } else if (tmp$indicator == "ConceptCount"){
        i <- 1
        for (uid in tmp$instidlist){
          result <- openalex_ConceptCount(tmp$instname[i], uid, tmp$years, tmp$mtypes)
          if (i == 1){
            idata <- result
          }else{
            idata <- dplyr::full_join(idata, result, by=c("key","key_display_name"))
          }
          i <- i + 1
        }
      } else if (tmp$indicator == "AuthCount"){
        i <- 1
        for (uid in tmp$instidlist){
          result <- openalex_AuthCount(tmp$instname[i], uid, tmp$years, tmp$mtypes)
          if (i == 1){
            idata <- result
          }else{
            idata <- dplyr::full_join(idata, result, by=c("key","key_display_name"))
          }
          i <- i + 1
        }
      } else if (tmp$indicator == "OAstatusCount"){
        i <- 1
        for (uid in tmp$instidlist){
          result <- openalex_OAstatusCount(tmp$instname[i], uid, tmp$years, tmp$mtypes)
          if (i == 1){
            idata <- result
          }else{
            idata <- dplyr::full_join(idata, result, by=c("key","key_display_name"))
          }
          i <- i + 1
        }
      } else if (tmp$indicator == "CitationPercentilebyYear"){
        i <- 1
        for (uid in tmp$instidlist){
          result <- openalex_CitationPercentilebyYear(tmp$instname[i], uid, tmp$years, tmp$mtypes)
          if (i == 1){
            idata <- result
          }else{
            idata <- dplyr::full_join(idata, result, by=c("key","key_display_name"))
          }
          i <- i + 1
        }
      }
    }
    tmp$data <- idata
  })
  
  
  # ダウンロードボタン付きでテーブル表示
  output$numtable <- renderDataTable(
    datatable(
    tmp$data, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',pageLength = 1000,buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
    )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
