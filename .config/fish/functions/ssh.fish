function ssh --description 'alias ssh ssh -i ~/.ssh/id_rsa'
  command ssh -i ~/.ssh/id_rsa $argv
end
