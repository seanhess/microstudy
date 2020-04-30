library(tidyverse)

input = "microstudy-data.csv"


loadData = function(inp) {
  read.csv(inp)
}

# rs: vector of responses 1, 2, or NA
# single user only, or this doesn't make sense
# Let's investigate this ourselves!
# a vector of responses, variable length, for each one




# takes the responses and returns a pattern 1222112
responsePattern = function(rs) {
  # filters out the NA
  rs[!is.na(rs)]
}


# takes the response pattern "", 122, 22, 211211 and produces the summary code
conditionSummary = function(ps) {
  s = Reduce(conditionSummaryR, ps, "", accumulate=FALSE)
  # print(ps, s)
  s
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
  # head(aggregate(MuslimMicro1 ~ RESP_ID, data, c)
  data %>%
    group_by(RESP_ID) %>%
    summarize(
      MuslimMicro1SI = si(MuslimMicro1),
      MuslimMicro2SI = si(MuslimMicro2),
      MuslimMicro3SI = si(MuslimMicro3),
      GayMicro1SI = si(GayMicro1),
      GayMicro2SI = si(GayMicro2),
      GayMicro3SI = si(GayMicro3)
    )
}

