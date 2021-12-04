library(tercen)
library(dplyr, warn.conflicts = FALSE)

ctx = tercenCtx()

ctx %>%
  select(.y, .ci, .ri) %>% 
  group_by(.ci, .ri) %>%
  summarise(mean = mean(.y)) %>%
  ctx$addNamespace() %>%
  ctx$save()
