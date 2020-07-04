{
    d = $4
    if (d != "") {
        i += 1
        x[i] = d
    }

} END {
    for(i = 50; i <= length(x); i++)  {
      y[(i-0)] = x[i]
    }

    for(i in y) {
        sum_x += y[i]
    }

    N = length(y)

    m_x = sum_x / N

    for(i in y){
        sum_dx2 += (y[i] - m_x) ^ 2
    }


    print "Average:", m_x
    print "SD: ", sqrt(sum_dx2 / N)
}
