# google "www.google.com/search?q="
# google scholar "scholar.google.com/scholar?q="
function url -d "Open or search clipboard string" --argument-names 'engine'
  if test (count $argv) -ne 1
    echo "Usage: $_ <search engine string>"
    return 1
  end
  xclip -o | tr -d '\n' | read str
  if perl (dirname (status -f))/url.pl $str
    vivaldi $str
  else
    echo $str | perl -lpe 'BEGIN{$engine = shift @ARGV}   s/([^A-Za-z0-9])/sprintf("%%%02X", ord($1))/seg;$_ = "$engine$_"' $engine | xargs vivaldi
  end
end
