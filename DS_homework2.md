Simple document
================

DS_problem1 1.1read and clean the data

``` r
Transit_data=read_csv("C:/Users/10145/Desktop/DS project/p8105_hw2_zx2425/p8105_hw2_zx2425/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c"))
names(Transit_data)
```

    ##  [1] "Division"           "Line"               "Station Name"      
    ##  [4] "Station Latitude"   "Station Longitude"  "Route1"            
    ##  [7] "Route2"             "Route3"             "Route4"            
    ## [10] "Route5"             "Route6"             "Route7"            
    ## [13] "Route8"             "Route9"             "Route10"           
    ## [16] "Route11"            "Entrance Type"      "Entry"             
    ## [19] "Exit Only"          "Vending"            "Staffing"          
    ## [22] "Staff Hours"        "ADA"                "ADA Notes"         
    ## [25] "Free Crossover"     "North South Street" "East West Street"  
    ## [28] "Corner"             "Entrance Latitude"  "Entrance Longitude"
    ## [31] "Station Location"   "Entrance Location"

``` r
Transit_data=janitor::clean_names(Transit_data)
names(Transit_data)
```

    ##  [1] "division"           "line"               "station_name"      
    ##  [4] "station_latitude"   "station_longitude"  "route1"            
    ##  [7] "route2"             "route3"             "route4"            
    ## [10] "route5"             "route6"             "route7"            
    ## [13] "route8"             "route9"             "route10"           
    ## [16] "route11"            "entrance_type"      "entry"             
    ## [19] "exit_only"          "vending"            "staffing"          
    ## [22] "staff_hours"        "ada"                "ada_notes"         
    ## [25] "free_crossover"     "north_south_street" "east_west_street"  
    ## [28] "corner"             "entrance_latitude"  "entrance_longitude"
    ## [31] "station_location"   "entrance_location"

``` r
tail(Transit_data)
```

    ## # A tibble: 6 × 32
    ##   division line       stati…¹ stati…² stati…³ route1 route2 route3 route4 route5
    ##   <chr>    <chr>      <chr>     <dbl>   <dbl> <chr>  <chr>  <chr>  <chr>  <chr> 
    ## 1 IRT      White Pla… Simpso…    40.8   -73.9 2      5      <NA>   <NA>   <NA>  
    ## 2 IRT      White Pla… Wakefi…    40.9   -73.9 2      5      <NA>   <NA>   <NA>  
    ## 3 IRT      White Pla… Wakefi…    40.9   -73.9 2      5      <NA>   <NA>   <NA>  
    ## 4 IRT      White Pla… Wakefi…    40.9   -73.9 2      5      <NA>   <NA>   <NA>  
    ## 5 IRT      Flushing   34 St …    40.8   -74.0 7      <NA>   <NA>   <NA>   <NA>  
    ## 6 IRT      Flushing   34 St …    40.8   -74.0 7      <NA>   <NA>   <NA>   <NA>  
    ## # … with 22 more variables: route6 <chr>, route7 <chr>, route8 <chr>,
    ## #   route9 <chr>, route10 <chr>, route11 <chr>, entrance_type <chr>,
    ## #   entry <chr>, exit_only <chr>, vending <chr>, staffing <chr>,
    ## #   staff_hours <chr>, ada <lgl>, ada_notes <chr>, free_crossover <lgl>,
    ## #   north_south_street <chr>, east_west_street <chr>, corner <chr>,
    ## #   entrance_latitude <dbl>, entrance_longitude <dbl>, station_location <chr>,
    ## #   entrance_location <chr>, and abbreviated variable names ¹​station_name, …

``` r
data_c=select(Transit_data,line,station_name,station_latitude,station_longitude,route1:route11,entry,vending,entrance_type,ada)
```

DS_problem1 1.2 Convert the entry variable’s type

``` r
data_c=mutate(data_c,entry=factor(entry,levels=c("YES","NO")))
```

DS_problem1 1.3answer the questions

``` r
distincted_station=distinct(data_c,station_name, .keep_all = TRUE)
filter_data=filter(distincted_station, ada==TRUE)
#356 rows
filter_data2=filter(distincted_station, vending=="NO")
filter_data2
```

    ## # A tibble: 9 × 19
    ##   line  stati…¹ stati…² stati…³ route1 route2 route3 route4 route5 route6 route7
    ##   <chr> <chr>     <dbl>   <dbl> <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ## 1 6 Av… 7th Av     40.7   -74.0 F      <NA>   <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 2 Brig… West 8…    40.6   -74.0 F      Q      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 3 Broa… Cortla…    40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 4 Broa… Whiteh…    40.7   -74.0 R      1      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 5 Broa… South …    40.7   -74.0 R      1      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 6 Dyre… Bayche…    40.9   -73.8 5      <NA>   <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 7 Dyre… Gun Hi…    40.9   -73.8 5      <NA>   <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 8 Quee… Elmhur…    40.7   -73.9 M      R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 9 Rock… Aquedu…    40.7   -73.8 A      <NA>   <NA>   <NA>   <NA>   <NA>   <NA>  
    ## # … with 8 more variables: route8 <chr>, route9 <chr>, route10 <chr>,
    ## #   route11 <chr>, entry <fct>, vending <chr>, entrance_type <chr>, ada <lgl>,
    ## #   and abbreviated variable names ¹​station_name, ²​station_latitude,
    ## #   ³​station_longitude

1.3 the answer is: The original data set which called Transit_data
contains 32 variables. After standardizing the name of it, it including:
division,line, station_name, station_latitude, station_longitude,
route1, route2, route3, route4, route5, route6, route7, route8, route9,
route10, route11, entrance_type, entry, exit_only, vending, staffing,
staff_hours, ada, ada_notes, free_crossover, north_south_street,
east_west_street, corner, entrance_latitude, entrance_longitude,
station_location, entrance_location. The data set c is a cleaned dataset
of original data of problem1. This data set contain 19 variables
including
line,station_name,station_latitude,station_longitude,route1:route11,entry,vending,entrance_type,ada.
I have standarded the name of the variables and select target variable
to form a new dataset. it is 1868\*19. I think those steps are data
tidy.

``` r
filter_data2 %>% 
  filter(vending == "NO") %>% 
  pull(entry) %>% 
  mean
```

    ## Warning in mean.default(.): 参数不是数值也不是逻辑值：回覆NA

    ## [1] NA

``` r
filter_data2 %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 1 × 2
    ##   station_name       line    
    ##   <chr>              <chr>   
    ## 1 Aqueduct Racetrack Rockaway

``` r
filter_data2 %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 0 × 2
    ## # … with 2 variables: station_name <chr>, line <chr>

Explaination: At last, we need to make a code chunk to identify stations
that serve the A train, and to assess how many of these are ADA
compliant. As a first step, we tidy the data as alluded to previously;
that is, we convert `route` from wide to long format. After this step,
we can use tools from previous parts of the question (filtering to focus
on the A train, and on ADA compliance; selecting and using `distinct` to
obtain dataframes with the required stations in rows).

Problem2

Read and clean the dataset of Mr_Trash_Wheel, The results as following:

``` r
Mr_Trash_Wheel= 
    read_excel("C:/Users/10145/Desktop/DS project/p8105_hw2_zx2425/p8105_hw2_zx2425/Trash-Wheel-Collection-Totals-7-2020-2.xlsx",sheet = "Mr. Trash Wheel")%>% 
    janitor::clean_names() %>% 
    drop_na(dumpster) %>% 
    mutate(
      sports_balls = as.integer(round(sports_balls))
      )
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
Mr_Trash_Wheel
```

    ## # A tibble: 547 × 16
    ##    dumpster month year  date                weight_tons volume…¹ plast…² polys…³
    ##       <dbl> <chr> <chr> <dttm>                    <dbl>    <dbl>   <dbl>   <dbl>
    ##  1        1 May   2014  2014-05-16 00:00:00        4.31       18    1450    1820
    ##  2        2 May   2014  2014-05-16 00:00:00        2.74       13    1120    1030
    ##  3        3 May   2014  2014-05-16 00:00:00        3.45       15    2450    3100
    ##  4        4 May   2014  2014-05-17 00:00:00        3.1        15    2380    2730
    ##  5        5 May   2014  2014-05-17 00:00:00        4.06       18     980     870
    ##  6        6 May   2014  2014-05-20 00:00:00        2.71       13    1430    2140
    ##  7        7 May   2014  2014-05-21 00:00:00        1.91        8     910    1090
    ##  8        8 May   2014  2014-05-28 00:00:00        3.7        16    3580    4310
    ##  9        9 June  2014  2014-06-05 00:00:00        2.52       14    2400    2790
    ## 10       10 June  2014  2014-06-11 00:00:00        3.76       18    1340    1730
    ## # … with 537 more rows, 8 more variables: cigarette_butts <dbl>,
    ## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
    ## #   sports_balls <int>, homes_powered <dbl>, x15 <lgl>, x16 <lgl>, and
    ## #   abbreviated variable names ¹​volume_cubic_yards, ²​plastic_bottles,
    ## #   ³​polystyrene

Read and clean the dataset of Pro_Trash_Wheel, The results as following:

``` r
Pro_Trash_Wheel = 
    read_excel("C:/Users/10145/Desktop/DS project/p8105_hw2_zx2425/p8105_hw2_zx2425/Trash-Wheel-Collection-Totals-7-2020-2.xlsx",sheet = "Professor Trash Wheel") %>% 
    janitor::clean_names() %>% 
    drop_na(dumpster) %>% 
    mutate(
      dumpster=as.double(dumpster)
    )
Pro_Trash_Wheel
```

    ## # A tibble: 94 × 13
    ##    dumpster month     year date                weight_…¹ volum…² plast…³ polys…⁴
    ##       <dbl> <chr>    <dbl> <dttm>                  <dbl>   <dbl>   <dbl>   <dbl>
    ##  1        1 January   2017 2017-01-02 00:00:00      1.79      15    1950    6080
    ##  2        2 January   2017 2017-01-30 00:00:00      1.58      15    9540   11230
    ##  3        3 February  2017 2017-02-26 00:00:00      2.32      18    8350    9210
    ##  4        4 February  2017 2017-02-26 00:00:00      3.72      15    8590    1030
    ##  5        5 February  2017 2017-02-28 00:00:00      1.45      15    7830    9950
    ##  6        6 March     2017 2017-03-30 00:00:00      1.71      15    8210   10340
    ##  7        7 April     2017 2017-04-01 00:00:00      1.82      15    9830   11020
    ##  8        8 April     2017 2017-04-20 00:00:00      2.37      15    9240    8760
    ##  9        9 May       2017 2017-05-10 00:00:00      2.64      15    9540    8810
    ## 10       10 May       2017 2017-05-26 00:00:00      2.78      15    8230    7800
    ## # … with 84 more rows, 5 more variables: cigarette_butts <dbl>,
    ## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
    ## #   homes_powered <dbl>, and abbreviated variable names ¹​weight_tons,
    ## #   ²​volume_cubic_yards, ³​plastic_bottles, ⁴​polystyrene

``` r
Mr_Trash_Wheel=Mr_Trash_Wheel[ , -which(colnames(Mr_Trash_Wheel) %in% c("x15","x16"))]
Mr_Trash_Wheel= mutate(Mr_Trash_Wheel,name=as.character("Mr. Trash Wheel"))
Pro_Trash_Wheel=mutate(Pro_Trash_Wheel,name=as.character("Professor Trash Wheel"))
Mr_Trash_Wheel= mutate(Mr_Trash_Wheel,year=as.double(year))
Pro_Trash_Wheel= mutate(Pro_Trash_Wheel,year=as.double(year))
TOTAL= bind_rows(Mr_Trash_Wheel,Pro_Trash_Wheel)
```

``` r
sum(Pro_Trash_Wheel$weight_tons)
```

    ## [1] 190.12

``` r
filter_Mr_Trash_Wheel=dplyr::filter(Mr_Trash_Wheel,year==2020)
sum(filter_Mr_Trash_Wheel$sports_balls)
```

    ## [1] 856

Explaination: The total weight of trash collected by Professor Trash
Wheel is `190.12` The total number of sports balls collected by
Mr. Trash Wheel in 2020 is `856`

Problem 3

Problem3 Read three datasets:

``` r
unemployment=read_csv("C:/Users/10145/Desktop/DS project/p8105_hw2_zx2425/p8105_hw2_zx2425/fivethirtyeight_datasets/unemployment.csv")
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pol=read_csv("C:/Users/10145/Desktop/DS project/p8105_hw2_zx2425/p8105_hw2_zx2425/fivethirtyeight_datasets/pols-month.csv")
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp=read_csv("C:/Users/10145/Desktop/DS project/p8105_hw2_zx2425/p8105_hw2_zx2425/fivethirtyeight_datasets/snp.csv")
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

manipulate the pols_moth dataset

``` r
pol = separate(pol,mon, into= c("year","month",'day'),sep= "-")
pol=mutate(pol,
  month = recode(month, "01" = "January", "02" = "February", "03"="March", "04"="April","05"="May","06"="June", "07"="July", "08"="August","09"="September","10"="October","11"="November", "12"="December")
)
pol=mutate(pol,president=ifelse(prez_gop==1, "prez_gop",ifelse(prez_gop==0,"prez_dem","other") )
           )
```

manipulate the pols_moth dataset

``` r
pol=select(pol, -day)
pol=select(pol, -prez_gop)
pol=select(pol, -prez_dem)
```

manipulate the snp dataset

``` r
snp = separate(snp,date, into= c("month","day",'year'),sep= "/")
snp=mutate(snp, month = recode(month, "1" = "January", "2" = "February", "3"="March", "4"="April","5"="May","6"="June", "7"="July", "8"="August","9"="September","10"="October","11"="November", "12"="December")
)
snp=select(snp, -day)
snp=mutate(snp,year=as.integer(year))
```

translate the formate of the two datasets in order to combine them
together:

``` r
pol=mutate(pol,year=as.integer(year))
data_d=pivot_longer(unemployment,Jan:Dec,names_to="month",values_to="amount")
data_d= data_d %>%
  janitor::clean_names()
data_d=mutate(data_d,year=as.integer(year))
data_d=mutate(data_d,
  month = recode(month, "Jan" = "January", "Feb" = "February", "Mar"="March", "Apr"="April","May"="May","Jun"="June", "Jul"="July", "Aug"="August","Sep"="September","Oct"="October","Nov"="November", "Dec"="December")
)
snp=mutate(snp, year = ifelse(year<20,year + 2000,year + 1900))
snp=arrange(snp,year,month)
snp=relocate(snp, year, month,close)
```

jion them together

``` r
library(dplyr)
data_e=left_join(pol,snp,by=c("year","month"))
data_f=left_join(data_e,data_d,by=c("year","month"))
```

the content of the initial pols-month dataframe contains 9 variables,
including:mon prez_gop gov_gop sen_gop rep_gop prez_dem gov_dem sen_dem
rep_dem.822 multiply 9. After processing, they become to year month
gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president.822*9 The
range of the year of pols_moth dataframe is 1947-2015. the content of
the initial snp dataframe contains 2 variables, including:data close.
787*2 After processing, they become to month year close.787 multiply 3.
The range of the year of snp dataframe is 1950-2015.

the content of the combined dataframe：data_f contains 11 variables,
including:year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem
president close amount. 822\*11. The range of the combined dataframe
data_f is 1947-2015.
