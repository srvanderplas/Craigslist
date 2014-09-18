for (i in 1:5)
{
  system("(echo authenticate '\"password\"'; echo signal newnym; echo quit) | nc localhost 9051")
  Sys.sleep(6)
  ip <- scrape("http://ifconfig.me/ip")[[1]]  
  print(xmlValue(getNodeSet(ip, "//*/p")[[1]]))
  Sys.sleep(1)
}  
