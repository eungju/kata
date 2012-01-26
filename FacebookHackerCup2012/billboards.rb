def fit?(w, h, lines, font_size)
  (lines.length * font_size <= h) and ((lines.map { |e| e.length }).max * font_size <= w)
end

def word_wrap(s, w)
  lines = []
  line = words.pop
  while true
    if words.empty?
      lines.push(line)
      break
    end
    word = words.pop
    if (line.length + 1 + word.length) <= w
      line = line + " " + word
    else
      lines.push(line)
      line = word
    end
  end
  return lines
end

def solve(w, h, s)
  font_size = h
  while font_size > 0
    if fit?(w, h, word_wrap(s, w / font_size), font_size)
      return font_size
    end
    font_size -= 1
  end
  return 0
end

t = readline.to_i
(1..t).each { |i|
  w, h, s = readline.split(/ /, 3)
  fs = solve(w.to_i, h.to_i, s.strip)
  puts "Case \##{i}: #{fs}"
}
