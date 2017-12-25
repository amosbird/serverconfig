function httpproxy -d ""
  env http_proxy="http://172.26.133.192:1888/" $argv
end
