package main

import (
	"fmt"
	"math/rand"
	"time"
)

// tworzenie nowego wierzcholka, indeksy to kolejne liczby
func newNode(index int) *Node {
	s := Node{index: index, ask: make(chan NextHopInfo), read: make(chan ReadInfo), write: make(chan WriteInfo), receive: make(chan Package), queue: make(chan StandardPackage, 1000), receiveStandardPack: make(chan StandardPackage)}
	return &s
}

// generujemy graf
func generateGraph(n int, d int, k int) *Graph {
	g := Graph{}
	// tworzymy nowe wierzcholki i dodajemy je do tablicy wierzcholkow grafu
	for i := 0; i < n; i++ {
		s := newNode(i)
		g.nodes = append(g.nodes, s)
		if g.edges == nil {
			g.edges = make(map[*Node][]*Node)
		}
		// dodajemy krawedz (i-1,i) i (i,i-1)
		if i > 0 {
			g.edges[g.nodes[i-1]] = append(g.edges[g.nodes[i-1]], s)
			g.edges[g.nodes[i]] = append(g.edges[g.nodes[i]], g.nodes[i-1])
		}
	}
	// na poczatku ustawiamy wszystkie tablice routingu tak ze
	// koszt to roznica indeksow
	// nastepny wierzcholek to poprzedni lub nastepny
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			if i != j {
				var routing Routing
				if j < i {
					routing = Routing{g.nodes[i-1], i - j, true}
				} else {
					routing = Routing{g.nodes[i+1], j - i, true}
				}
				g.nodes[i].routingTable = append(g.nodes[i].routingTable, routing)
			} else {
				g.nodes[i].routingTable = append(g.nodes[i].routingTable, Routing{})
			}
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
			// zmianiamy tablice routingu
			// dla wierzcholkow z nowo dodanej krawedzi koszt podrozy miedzy nimi wynosi 1 oraz sa dla siebie nastepnymi wierzcholkami
			g.edges[g.nodes[src]] = append(g.edges[g.nodes[src]], g.nodes[dest])
			g.edges[g.nodes[dest]] = append(g.edges[g.nodes[dest]], g.nodes[src])
			g.nodes[src].routingTable[dest].nextHop = g.nodes[dest]
			g.nodes[src].routingTable[dest].cost = 1
			g.nodes[dest].routingTable[src].nextHop = g.nodes[src]
			g.nodes[dest].routingTable[src].cost = 1
			i++
		}
	}
	// losujemy hosty
	for i := 0; i < k; i++ {
		src := randomValue(n)
		host := Host{src, len(g.nodes[src].hosts), make(chan StandardPackage)}
		g.nodes[src].hosts = append(g.nodes[src].hosts, host)
		g.allHosts = append(g.allHosts, host)
	}
	return &g
}

// wypisywanie grafu
func printGraph(g *Graph) {
	fmt.Println("GRAPH:")
	for i := 0; i < len(g.nodes); i++ {
		result := g.nodes[i].toString()
		result += " -> "
		m := g.edges[g.nodes[i]]
		for j := 0; j < len(m); j++ {
			result += m[j].toString()
			result += " "
		}
		result += fmt.Sprintf("HOSTS: %d", len(g.nodes[i].hosts))
		fmt.Println(result)
	}
}

// wypisywanie wynikow
func printResults(g *Graph) {
	fmt.Println("Shortest paths:")
	for i := 0; i < len(g.nodes); i++ {
		result := "\nFrom " + g.nodes[i].toString()
		for j := 0; j < len(g.nodes); j++ {
			result += "\nto " + g.nodes[j].toString() + ": " + g.nodes[i].toString()
			routing := g.nodes[i].routingTable[j].nextHop
			for k := 0; k < g.nodes[i].routingTable[j].cost; k++ {
				result += fmt.Sprintf(" --> %d", routing.index)
				routing = routing.routingTable[j].nextHop
			}
			result += fmt.Sprintf(" COST: %d", g.nodes[i].routingTable[j].cost)
		}
		fmt.Println(result)
	}
}

// losowanie krawedzi(dwie liczby z zakresu indeksow wierzcholkow)
func randomEdges(n int) (int, int) {
	s1 := rand.NewSource(time.Now().UnixNano())
	r1 := rand.New(s1)
	rand1 := r1.Intn(n)
	rand2 := r1.Intn(n)
	for {
		if rand2 != rand1 {
			break
		}
		rand2 = r1.Intn(n)
	}
	return rand1, rand2
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
