package main

import (
	"fmt"
	"sync"
	"time"
)

// goroutine: wysylanie pakietow
func insertPackage(k int, node *Node, h int) {
	// wysylamy k pakietow
	for i := 1; i <= k; i++ {
		// tworzymy pakiet
		pack := Package{number: i, ttl: h}
		// losujemy czas i czekamy
		rand := randomValue(2000, 500)
		time.Sleep(time.Millisecond * time.Duration(rand))
		// gdy sie da wrzucamy do wierzcholka pakiet (jego adres w pamieci bo bedziemu mu zmieniac liste odwiedzonych wierzcholkow)
		node.resp <- &pack
	}
}

// goroutine: transfer pakietow miedzy wierzcholkami
func transferPackage(graph *Graph, node *Node, printer chan string, wg *sync.WaitGroup, packs *[]*Package) {
	// czy wierzcholek to ujscie
	last := false
	// czy w wierzcholku jest pulapka
	trap := false
	// sasiedzi wierzcholka
	neigh := graph.edges[node]
	// jesli to ujscie to dokladamy do jego sasiadow specjalny wierzcholek odbiorcy
	if node.index == len(graph.nodes)-1 {
		last = true
		recNode := newNode(len(graph.nodes))
		neigh = append(neigh, recNode)
	}
	// wykonujemy caly czas, nie wiemy ile pakietow przejdzie przez wierzcholek
	for {
		select {
		// gdy jest jakis pakiet do odebrania
		case pack := <-node.resp:
			// wierzcholek jest zajety
			node.state = true
			// wysylamy informacje ze pakiet jest w wierzcholku
			info := fmt.Sprintf("Package %d is in node %d", pack.number, node.index)
			printer <- info
			// dodajemy go do listy pakietow ktore odwiedzily wierzcholek
			node.packages = append(node.packages, pack)
			// pakiet dodaje wierzcholek do listy odwiedzonych
			pack.nodes = append(pack.nodes, node)
			// jesli zostala zastawiona pulapka
			if trap {
				// dodajemy pakiet do tablicy pakietow
				//*packs = append(*packs, pack)
				// wysylamy informacje
				info := fmt.Sprintf("Package %d was caught in trap in node %d", pack.number, node.index)
				printer <- info
				// zmiejszamy waitgroup bo pakiet zakonczyl podroz
				wg.Done()
				// usuwamy pulapke
				trap = false
			} else {
				// jesli pakiet nie moze juz podrozowac dalej
				if pack.ttl == 0 {
					// jesli jest w ostatnim wierzcholku
					if last {
						// wysylamy go do odbiorcy
						graph.rec <- pack
					} else {
						// pakiet umiera
						//*packs = append(*packs, pack)
						info := fmt.Sprintf("Package %d died in node %d", pack.number, node.index)
						printer <- info
						wg.Done()
					}
				} else {
					state := true
					// petla wykonuje sie az wierzcholek znajdzie pustego sasiada
					for state {
						// dodajemy runde
						node.rounds += 1
						// losujemy czas spania
						rand := randomValue(2000, 500)
						time.Sleep(time.Millisecond * time.Duration(rand))
						// shuffle tablicy
						shuffleArray(neigh)
						i := 0
						// sprawdzamy sasiadow
						for i < len(neigh) {
							// jesli sasiad to specjalny wierzcholek to wysylamy pakiet do odbiorcy
							if neigh[i].index == len(graph.nodes) {
								graph.rec <- pack
								state = false
								break
							} else {
								// wysylamy zapytanie o wierzcholek
								neigh[i].occupied <- false
								// otrzymujemy odpowiedz
								state = <-neigh[i].occupied
								// jesli wierzcholek nie jest zajety
								if !state {
									// zmniejszamy ttl pakietu i wysylamy go do wierzcholka
									pack.ttl--
									where := neigh[i]
									where.resp <- pack
									break
								}
								i++
							}
						}
						// dodajemy liczbe wykonanych sprawdzen sasiadow
						node.steps += i + 1
					}
				}
			}
			// po wszytskich akcjach wierzcholek jest pusty
			node.state = false
		// pulapka od klusownika
		case <-node.hunt:
			trap = true
		}
	}
}

// goroutine: odbieranie pakietow
func receivePackage(graph *Graph, printer chan string, wg *sync.WaitGroup, packs *[]*Package) {
	// nie wiemy ile pakietow odbierzemy
	for {
		// losujemy czas i czekamy
		rand := randomValue(2000, 500)
		time.Sleep(time.Millisecond * time.Duration(rand))
		// gdy bedzie cos do odebrania
		pack := <-graph.rec
		*packs = append(*packs, pack)
		info := fmt.Sprintf("Package %d was received", pack.number)
		printer <- info
		wg.Done()
	}
}

// goroutine: informacja o pacie
func pat(node *Node) {
	for {
		// gdy przyjdzie zapytanie
		<-node.occupied
		// jesli wierzcholek jest wolny
		if !node.state {
			// wysylamy informacje ze jest wolny i zmieniamy jego stan na zajety
			node.occupied <- false
			node.state = true
		} else {
			node.occupied <- true
		}
	}
}

// goroutine: klusownik
func hunter(g *Graph) {
	for {
		// losujemy czas spania
		randTime := randomValue(5000, 4000)
		time.Sleep(time.Millisecond * time.Duration(randTime))
		// losujemy wierzcholek
		rand := randomValue(len(g.nodes))
		// zastawiamy pulapke w wierzcholku
		g.nodes[rand].hunt <- true
	}
}
