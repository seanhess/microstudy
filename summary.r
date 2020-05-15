library(tidyverse)



run = function() {
  data = load(input_data)
  meta = load(input_meta)
  data = dropDuplicateTimes(data)
  rs = conditionResponses(data)
  combo = joinAll(data, rs, meta)
  save(combo, output)
}

input_data = "microstudy-data.csv"
input_meta = "microstudy-meta.csv"
output = "combined.csv"



load = function(inp) {
  read.csv(inp)
}

save = function(combo, output) {
  write.csv(combo, output, na="")
}



# takes the responses and returns a pattern 1222112
responsePattern = function(rs) {
  # filters out the NA
  rs[!is.na(rs)]
}


# takes the response pattern "", 122, 22, 211211 and produces the summary code
conditionSummary = function(ps) {
   Reduce(conditionSummaryR, ps, "", accumulate=FALSE)
}

conditionSummaryR = function(con, v) {
  if (con == "") {
    if (v == 1) {
      "S"
    } else {
      "I"
    }
  } else if (con == "S") {
    if (v == 1) {
      "S"
    } else {
      "SI"
    }
  } else if (con == "I") {
    if (v == 2) {
      "I"
    } else {
      "IS"
    }
  } else if (con == "SI") {
    if (v == 2) {
      "SI"
    } else {
      "X"
    }
  } else if (con == "IS") {
    if (v == 1) {
      "IS"
    } else {
      "X"
    }
  } else {
    "X"
  }

}

si = function(v) {
  as.factor(conditionSummary(responsePattern(v)))
}


# gives you the responses for a given condition for a given user?
conditionResponses = function(data) {
  data %>%
    group_by(RESP_ID) %>%
    summarize(
      Condition = Condition[1],
      GayMicro1SI = si(GayMicro1),
      GayMicro2SI = si(GayMicro2),
      GayMicro3SI = si(GayMicro3),
      MuslimMicro1SI = si(MuslimMicro1),
      MuslimMicro2SI = si(MuslimMicro2),
      MuslimMicro3SI = si(MuslimMicro3)
    )
    # arrange(Condition, RESP_ID)
}


# compute the mean of all the fields
subscale = function(meta, ...) {
  rowMeans(select(meta, ...), na.rm = TRUE)
}


# S, I, SI, IS, X, _

tally = function(fields, fn) {
  scores = map(fields, fn)
  pmap(scores, sum) %>% unlist()
}

sensitiveScore = Vectorize(function(si) {
  if (si == "S" | si == "IS") {
    1
  }
  else {
    0
  }
})

insensitiveScore = Vectorize(function(si) {
  if (si == "I" | si == "SI") {
    1
  } else {
    0
  }
})

# sumSensitiveScore = function(sis) {
#   a = sensitiveScore(sis)
# }


# select(rss, -Time)
joinAll = function(data, responses, meta) {
  data2 <- inner_join(data, responses)
  inner_join(data2, select(meta, -ID, -Condition))
}


dropDuplicateTimes = function(data) {
  mutate(data, Seconds = round(Time)) %>%
    distinct(RESP_ID, Condition, Seconds, .keep_all = TRUE) %>%
    select(ID, RESP_ID, Time, Seconds, everything())
}

