{
  x[NR] = $1
}
END{
  if(NR == 0) exit

  for(i in x){
    sum_x += x[i]
  }

  m_x = sum_x / NR

  for(i in x){
    sum_dx2 += (x[i] - m_x) ^ 2
  }

  print sqrt(sum_dx2 / NR)
}
