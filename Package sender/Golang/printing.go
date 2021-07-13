package main

import "fmt"

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
		fmt.Println(result)
	}
}

// wypisywanie wynikow koncowych
func printResults(g *Graph, packs []*Package) {
	fmt.Println("RESULTS: ")
	fmt.Println("Nodes: ")
	for i := 0; i < len(g.nodes); i++ {
		n := g.nodes[i]
		result := n.toString()
		result += ": "
		for j := 0; j < len(n.packages); j++ {
			result += n.packages[j].toString()
			result += " "
		}
		result += "\nAverage steps: "
		if n.rounds > 0 {
			result += fmt.Sprintf("%.2f", float32(n.steps)/float32(n.rounds))
		} else {
			result += "0"
		}
		result += " Rounds: "
		result += fmt.Sprintf("%d", n.rounds)
		result += " Sum of packages: "
		result += fmt.Sprintf("%d", len(n.packages))
		fmt.Println(result)
	}
	fmt.Println("Packages: ")
	for i := 0; i < len(packs); i++ {
		pack := packs[i]
		result := pack.toString()
		result += ": "
		for j := 0; j < len(pack.nodes); j++ {
			result += pack.nodes[j].toString()
			result += " "
		}
		fmt.Println(result)
	}
}
