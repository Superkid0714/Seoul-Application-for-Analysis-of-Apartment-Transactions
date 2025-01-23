# 1. 데이터 준비
load("06_geodataframe/06_apt_price.rdata") # 실거래 데이터 불러오기
library(sf)
apt_price <- st_drop_geometry(apt_price) # 공간 정보 제거
apt_price$py_area <- round(apt_price$area/3.3,0) # 크기 변환 (m^2 -> 평)

# 2. 사용자 화면 구성
library(shiny)
library(ggpmisc)

ui <- fluidPage(
  #--# 타이틀 입력
  titlePanel("여러 지역 상관관계 비교"),
  fluidRow(
    #--# 선택 몌뉴 1 : 지역 선택
    column(6,
           selectInput(
             inputId = "region", # 입력 아이디
             label = "지역을 선택하세요", # 라벨
             unique(apt_price$addr_1), # 선택 범위
             multiple = TRUE)), # 복수 선택 옵션
    #--# 선택 몌뉴 2 : 크기 선택
    column(6,
          sliderInput(
            inputId = "range_py", # 입력 아이디
            label = "평수를 선택하세요", # 라벨
            min = 0, # 선택 범위 최솟값
            max = max(apt_price$py_area), # 선택 범위 최댓값
            value = c(0,30))), # 기본 선택 범위
    #--# 출력
    column(12,
           plotOutput(outputId="gu_Plot",height="600"))) # 차트 출력
  )

server <- function(input,output,session){
  #--# 반응식
  apt_sel = reactive({
    apt_sel = subset(apt_price,
                     addr_1 == unlist(strsplit(paste(input$region, collapse = ','),","))&
                       py_area >= input$range_py[1] &
                       py_area <= input$range_py[2])
    return(apt_sel)
  })
  #--# 지역별 회귀선 그리기
  output$gu_Plot <- renderPlot({
    if (nrow(apt_sel())==0) # 선택 전 오류 메시지 없애기
        return(NULL)
    ggplot(apt_sel(),aes(x=py_area, y=py, col="red")) + # 축 설정
      geom_point() +  # 플롯 유형 : 포인트
      geom_smooth(method="lm", col="blue") + # 회귀선
      facet_wrap(~addr_1, scale='free_y', ncol=3) +
      theme(legend.position = "none") + # 테마 설정
      xlab('크기(평)') + # X축 설정
      ylab('평당 가격(만원)') + # Y축 설정
      stat_poly_eq(aes(label=paste(..eq.label..)),
                   label.x="right",label.y="top",
                   formula = y ~ x, parse = TRUE, size = 5, col="black")
     })
    }

shinyApp(ui=ui,server=server)








