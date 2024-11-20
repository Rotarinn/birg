# Tilraun fyrir birgðahald í R

library(qs)
library(tidyverse)
library(lubridate)

eval_parse <- function(x, y) {
  eval(parse(text = paste0(y, x)))
}

birgdir_base <- tibble(
  tegund = character(),
  vara = character(),
  magn = numeric(),
  eining = character(),
  dags = Date(),
  stadsetning = character()
)

birgdir <- qread("birgdir.qs")

dim_stadsetning <- c("heitt", "kalt", "frost")
dim_eining <- c("ltr", "gr", "kg", "pakki", "staukur")

birgdir <- birgdir |> add_row(
  tegund = "Mjólkurvara",
  vara = "Mjólk",
  magn = 1,
  eining = "ltr",
  dags = today() + days(0),
  stadsetning = "kalt"
)

tegund1 <- "pasta"
vara1 <- "rummo skrúfur"
magn1 <- 1
eining1 <- "pakki"
stadsetning1 <- "heitt"
dags1 <- today()

tegund2 <- "pasta"
vara2 <- "rummo slaufur"
magn2 <- 1
eining2 <- "pakki"
stadsetning2 <- "heitt"
dags2 <- today()

birgdir <- birgdir |> add_row(
  tegund = tegund1,
  vara = vara1,
  magn = magn1,
  eining = eining1,
  dags = dags1,
  stadsetning = stadsetning1
)

birgdir1 <- birgdir



map(1:2, function(x){
    tibble(
      tegund = eval_parse(x, "tegund"),
      vara = eval_parse(x, "vara"),
      magn = eval_parse(x, "magn"),
      eining = eval_parse(x, "eining"),
      dags = eval_parse(x, "dags"),
      stadsetning = eval_parse(x, "stadsetning")
    )
}) |> bind_rows(a = birgdir1, b = _)

map(1:2, function(x){
  eval_parse(x, "vara")
})

qsave(birgdir, "birgdir.qs")


birgdastada <- birgdir |> 
  group_by(vara, stadsetning) |> 
  summarise(
    magn = sum(magn),
    dags = min(dags)
  )
birgdastada

btest <- birgdir |> 
  mutate(hreyf = if_else(magn < 0, 0, 1)) |> 
  arrange(hreyf) |> 
  group_by(vara) |> 
  mutate(uppsafnad = cumsum(magn))

btest |> 
  filter(uppsafnad > 0) |> 
  summarise(dags = min(dags), magn = last(uppsafnad))

btest
reactable(
  btest |> 
    filter(uppsafnad > 0) |> 
    summarise(dags = min(dags), magn = last(uppsafnad))
  )
