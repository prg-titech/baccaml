{
  sum += $1
}
END{
  if(NR == 0) exit

  print sum / NR
}
