package main

import (
  "bufio"
  "fmt"
  "io"
  "os"
  "sort"
  "strings"
)

type Dict map[string][]string

type Candidate struct {
  cand_word string
  score int
}

type CandidateSlice []Candidate

func (cands CandidateSlice) Len() int {
  return len(cands)
}

func (cands CandidateSlice) Less(i, j int) bool {
  return cands[i].score < cands[j].score
}

func (cands CandidateSlice) Swap(i, j int) {
  temp := cands[i]
  cands[i] = cands[j]
  cands[j] = temp
}

func LoadDict(filename string) Dict {
  dict := Dict{}
  file, _ := os.Open(filename)
  defer file.Close()
  r := bufio.NewReader(file)
  for {
    line, err := r.ReadString('\n')
    if err == io.EOF {
      break
    }
    word := strings.TrimRight(line, "\n")
    for i := 0; i < len(word); i++ {
      key := []byte(word)
      key[i] = '?'
      dict[string(key)] = append(dict[string(key)], word)
    }
  }
  return dict
}

func GetScore(word string, target string, fast_search bool) int {
  score := 0
  word_bytes := []byte(word)

  for i, _ := range word_bytes {
    if word_bytes[i] == target[i] {
      score++
    }
  }
  if fast_search {
    return -score
  }
  return score
}

func (dict *Dict) FindCandidates(word string, target string, fast_search bool) CandidateSlice {
  cands := CandidateSlice{}
  for i := 0; i < len(word); i++ {
    key := []byte(word)
    key[i] = '?'
    for _, cand_word := range (*dict)[string(key)] {
      if cand_word != word {
        cands = append(cands, Candidate{cand_word, GetScore(cand_word, target, fast_search)})
      }
    }
  }
  sort.Sort(cands)
  return cands
}

func IsExist(word string, route []string) bool {
  for _, route_word := range route {
    if route_word == word {
      return true
    }
  }
  return false
}

func (dict *Dict) FindRoute(word string, target string, route []string, fast_search bool) []string {
  if word == target {
    return route
  }
  cands := dict.FindCandidates(word, target, fast_search)
  for _, cand := range cands {
    cand_word := cand.cand_word
    if IsExist(cand_word, route) {
      continue
    }
    new_route := append(route, cand_word)
    final_route := dict.FindRoute(cand_word, target, new_route, fast_search)
    if final_route != nil {
      return final_route
    }
  }
  return nil
}

func main() {
  start := "damp"
  target := "like"
  dict := LoadDict("word4.txt")
  route := dict.FindRoute(start, target, []string{start}, true)
  fmt.Println(route)
  route = dict.FindRoute(start, target, []string{start}, false)
  fmt.Println(route)
}
