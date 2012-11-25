watch('(.*)\.md') do |m|
  spawn("pandoc -s -f markdown -t html -o #{m[1]}.html #{m[0]}")
end
