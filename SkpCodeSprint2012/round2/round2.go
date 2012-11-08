package main

import (
	"fmt"
	"os"
	"bufio"
	"regexp"
)

type Position struct {
	x, y int
}

type Generation struct {
	lifes map[Position]struct{}
}

func (self *Generation) Put(life Position) {
	self.lifes[life] = struct{}{}
}

func (self *Generation) Population() int {
	return len(self.lifes)
}

type Rule struct {
	birthCondition []int
	survivalCondition []int
}

func DigitsToInts(s string) []int {
	var result = make([]int, len(s))
	for i, r := range s {
		result[i] = int(r) - int('0')
	}
	return result;
}

func ParseRule(spec string) *Rule {
	p, _ := regexp.Compile(`^B(\d+)(/S(\d+))?$`)
	submatches := p.FindStringSubmatch(spec)
	return &Rule{DigitsToInts(submatches[1]), DigitsToInts(submatches[3])}
}

type Game struct {
	rows, cols int
	state *Generation
	setCost, nextCost int
	rule *Rule
}

func ReadGame(reader *bufio.Reader) *Game {
	var n, m, nc, cs, cn int
	var ruleSpec string
	line, _ := reader.ReadString('\n')
	fmt.Sscanf(line, "%d %d %d %d %d %s", &n, &m, &nc, &cs, &cn, &ruleSpec)
	var initial = new(Generation)
	initial.lifes = make(map[Position]struct{})
	for i := 0; i < nc; i++ {
		var x, y int
		line, _ := reader.ReadString('\n')
		fmt.Sscanf(line, "%d %d", &x, &y)
		initial.Put(Position{x, y})
	}
	return &Game{n, m, initial, cs, cn, ParseRule(ruleSpec)}
}

const (
	OPERATOR_NEXT = 0
	OPERATOR_SET = 1
)

type Operation struct {
	operator int
	pos Position
}

type Plan struct {
	operations []Operation
}

func (self *Plan) Add(operation Operation) {
	self.operations = append(self.operations, operation)
}

func (self *Plan) Print() {
	fmt.Println(len(self.operations))
	for _, o := range self.operations {
		if (o.operator == OPERATOR_SET) {
			fmt.Println("SET", o.pos.x, o.pos.y)
		} else {
			fmt.Println("NEXT")
		}
	}
}

func (self *Game) Cost(plan *Plan) int {
	var sum = 0
	for _, each := range plan.operations {
		if (each.operator == OPERATOR_SET) {
			sum += self.setCost
		} else {
			sum += self.nextCost
		}
	}
	return sum
}

func (self *Game) IsValid(pos Position) bool {
	return 0 <= pos.x && pos.x < self.cols && 0 <= pos.y && pos.y < self.rows
}

func (self *Game) Neighbors(pos Position) map[Position]struct{} {
	var result = make(map[Position]struct{})
	var d = []int{-1, 0, 1}
	for _, dx := range d {
		for _, dy := range d {
			var neighbor = Position{pos.x + dx, pos.y + dy}
			if (pos != neighbor && self.IsValid(neighbor)) {
				result[neighbor] = struct{}{}
			}
		}
	}
	return result
}

func (self *Game) NaiveSolution() *Plan {
	var result = new(Plan)
	for life, _ := range self.state.lifes {
		fmt.Println(self.Neighbors(life))
		result.Add(Operation{OPERATOR_SET, life})
	}
	return result
}

func main() {
	f, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println("error opening file= ",err)
		os.Exit(1)
	}
	r := bufio.NewReader(f)
	line, _ := r.ReadString('\n')
	var t int
	fmt.Sscanf(line, "%d", &t)
	for i := 0; i < t; i++ {
		//fmt.Printf("%d\n", i)
		var game = ReadGame(r)
		//fmt.Println("Population:", game.state.Population())
		//fmt.Println("Rule:", *game.rule)
		var solution = game.NaiveSolution()
		fmt.Println("Cost:", game.Cost(solution))
	}
}
