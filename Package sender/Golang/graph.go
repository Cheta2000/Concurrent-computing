package main

import (
	"errors"
	"math/rand"
	"time"
)

// tworzenie nowego wierzcholka, indeksy to kolejne liczby
func newNode(index int) *Node {
	s := Node{index: index, state: false,
		resp: make(chan *Package), occupied: make(chan bool), hunt: make(chan bool)}
	return &s
}

// generujemy graf
func generateGraph(n int, d int, b int) *Graph {
	g := Graph{rec: make(chan *Package)}
	// tworzymy nowe wierzcholki i dodajemy je do tablicy wierzcholkow grafu
	for i := 0; i < n; i++ {
		s := newNode(i)
		g.nodes = append(g.nodes, s)
		if g.edges == nil {
			g.edges = make(map[*Node][]*Node)
		}
		// dodajemy krawedz (i-1,i)
		if i > 0 {
			g.edges[g.nodes[i-1]] = append(g.edges[g.nodes[i-1]], s)
		}
	}
	// dodajemy d losowych krawedzi
	i := 0
	for i < d {
		check := true
		src, dest := randomEdges(n)
		m := g.edges[g.nodes[src]]
		// sprawdzamy czy nie istnieje juz taka krawedz
		for j := 0; j < len(m); j++ {
			if m[j].index == dest {
				check = false
				break
			}
		}
		if check {
			g.edges[g.nodes[src]] = append(g.edges[g.nodes[src]], g.nodes[dest])
			i++
		}
	}
	// dodajemy b krawedzi skierowanych
	i = 0
	for i < b {
		check := true
		src, dest := randomEgdes2(n)
		m := g.edges[g.nodes[src]]
		// sprawdzamy czy nie istnieje juz taka krawedz
		for j := 0; j < len(m); j++ {
			if m[j].index == dest {
				check = false
				break
			}
		}
		if check {
			g.edges[g.nodes[src]] = append(g.edges[g.nodes[src]], g.nodes[dest])
			i++
		}
	}
	return &g
}

// losowanie krawedzi(dwie liczby z zakresu indeksow wierzcholkow)
func randomEdges(n int) (int, int) {
	s1 := rand.NewSource(time.Now().UnixNano())
	r1 := rand.New(s1)
	rand1 := r1.Intn(n)
	for {
		if rand1 != n-1 {
			break
		}
		rand1 = r1.Intn(n)
	}
	rand2 := r1.Intn(n-rand1) + rand1
	for {
		if rand2 != rand1 {
			break
		}
		rand2 = r1.Intn(n-rand1) + rand1
	}
	return rand1, rand2
}

// losowanie krawedzi skierowanej
func randomEgdes2(n int) (int, int) {
	s1 := rand.NewSource(time.Now().UnixNano())
	r1 := rand.New(s1)
	rand1 := r1.Intn(n)
	for {
		if rand1 != 0 {
			break
		}
		rand1 = r1.Intn(n)
	}
	rand2 := r1.Intn(rand1)
	return rand1, rand2
}

// funkcja liczaca maksymalna liczbe skrotow w grafie
func calculateShortcuts(n int) (int, error) {
	if n <= 0 {
		return -1, errors.New("Size of graph must be positive")
	}
	max := 0
	for i := 0; i < n; i++ {
		max += n - i - 1
	}
	max -= n - 1
	return max, nil
}

// zwracanie losowej wartosci dla odpowiednich argumentow
func randomValue(nums ...int) int {
	s1 := rand.NewSource(time.Now().UnixNano())
	r1 := rand.New(s1)
	var random int
	if len(nums) == 1 {
		random = r1.Intn(nums[0])
	} else {
		random = r1.Intn(nums[0]) + nums[1]
	}
	return random
}

// mieszanie tablicy
func shuffleArray(array []*Node) {
	for i := len(array) - 1; i >= 0; i-- {
		rand := randomValue(i + 1)
		array[rand], array[i] = array[i], array[rand]
	}
}
