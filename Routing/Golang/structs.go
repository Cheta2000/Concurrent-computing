package main

import "fmt"

// graf zawiera tablice ze wskazaniami na wierzcholki, mape wskazanie na wierzcholek-tablica wskazan na wierzcholki oraz liste wszystkich hostow
type Graph struct {
	nodes    []*Node
	edges    map[*Node][]*Node
	allHosts []Host
}

// wierzcholek
type Node struct {
	// numer
	index int
	// tablica routingu
	routingTable []Routing
	// hosty routera
	hosts []Host
	// kanaly do czytania i zapisywania tablicy, pytania o nextHop, przesylania pakietow i kolejka pakietows
	read                chan ReadInfo
	write               chan WriteInfo
	ask                 chan NextHopInfo
	receive             chan Package
	queue               chan StandardPackage
	receiveStandardPack chan StandardPackage
}

// element tablicy routingu
type Routing struct {
	// nastepny wierzcholek
	nextHop *Node
	// koszt podrozy
	cost int
	// czy zostal zmieniony
	changed bool
}

// pakiet zawiera wierzcholek ktory go wyslal i tablice par
type Package struct {
	from  *Node
	pairs []Pair
}

type Pair struct {
	// numer wierzcholka wraz z kosztem podrozy do niego
	number int
	cost   int
}

// host
type Host struct {
	// router
	r int
	// index
	h int
	// kanal do odebierania pakietow
	destination chan StandardPackage
}

// pakiet miedzy hostami
type StandardPackage struct {
	// kto wysyla i do kogo
	sender   Host
	receiver Host
	// jakie routery przeszedl pakiet
	visited []*Node
}

// pytanie o routing table
type ReadInfo struct {
	// o ktory index pytamy
	index int
	// gdzie odpowiedziec
	resp chan int
}

// zapis do routing table
type WriteInfo struct {
	// gdzie zapisujemy
	index int
	// co zapisujemy
	newCost int
	newHop  *Node
	resp    chan bool
}

// pytanie o nextHop
type NextHopInfo struct {
	// o jaki wierzcholek pytamy
	index int
	// gdzie odpowiedziec
	resp chan int
}

// funkcja dla struktry node zwracajaca indeks wierzcholka jako napis
func (node *Node) toString() string {
	return fmt.Sprintf("%d", node.index)
}
