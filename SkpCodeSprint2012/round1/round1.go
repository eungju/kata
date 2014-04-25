package main

import "fmt"
import "os"
import "bufio"

type Direction struct {
	to int
	distance int
}

func main() {
	var graph [1000000][]Direction
	f, err := os.Open(os.Args[1])
	if err != nil {
	    fmt.Println("error opening file= ",err)
	    os.Exit(1)
	}
	r := bufio.NewReader(f)
	var prevFrom = 0
	var directions []Direction = make([]Direction, 0)
	for true {
		line, e := r.ReadString('\n')
		if e != nil {
			graph[prevFrom] = directions
			break;
		}
		var from, to, distance int
		fmt.Sscanf(line, "%d,%d,%d", &from, &to, &distance)
		if prevFrom != from {
			graph[prevFrom] = directions
			fmt.Printf("%d\n", prevFrom)
			directions = make([]Direction, 0)
			prevFrom = from
		}
		directions = append(directions, Direction{to, distance})
	}
}
