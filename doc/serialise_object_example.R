library(tercen)
library(dplyr)

# remotes::install_github("tercen/tim")
library(tim)

ctx <- tercenCtx()

df_out <- ctx %>% 
  select(.y, .ci, .ri) %>% 
  group_by(.ci, .ri) %>%
  summarise(median = median(.y))

res <- tim::get_serialized_result(
  df = df_out, object = iris, object_name = "iris_object", ctx = ctx
)

ctx$save(res)
