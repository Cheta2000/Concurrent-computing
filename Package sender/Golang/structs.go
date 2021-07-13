package main

import "fmt"

// graf zawiera tablice ze wskazaniami na wierzcholki oraz mape wskazanie nawierzcholek-tablica wskazan na wierzcholki
type Graph struct {
	nodes []*Node
	edges map[*Node][]*Node
	rec   chan *Package
}

// wierzcholek
type Node struct {
	// numer
	index int
	// stan- czy moze przyjac pakiet
	state bool
	// tablica pakietow ktore go odwiedzily
	packages []*Package
	// liczba krokow wykonana aby znalezc pustego sasiada
	steps int
	// liczba rund szukania pustego sasiada
	rounds int
	// kanal do wymiany pakietow
	resp chan *Package
	// kanal do informowania o stanie
	occupied chan bool
	// kanal dla klusownika
	hunt chan bool
}

//pakiet zawiera numer, tablice wskazan na wierzcholki ktore odwiedzil i swoj czas zycia
type Package struct {
	number int
	nodes  []*Node
	ttl    int
}

// funkcja dla struktury package zwracajaca nr pakietu jako napis
func (pack Package) toString() string {
	return fmt.Sprintf("%d", pack.number)
}

// funkcja dla struktry node zwracajaca indeks wierzcholka jako napis
func (node *Node) toString() string {
	return fmt.Sprintf("%d", node.index)
}
