# Function to make a vertical line on a Plotly scatterplot
make_vlines = function(x, color = "#000000") {
  l = list()
  for (x_intercept in x) {
    l = append(l, list(list(
                         type = "line",
                         x0 = x_intercept, x1 = x_intercept,
                         y0 = 0, y1 = 1, yref = "paper",
                         line = list(color = color)
                       )
                  )
        )
  }
  return(l)
}
