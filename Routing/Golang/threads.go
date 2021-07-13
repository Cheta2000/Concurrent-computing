package main

import (
	"fmt"
	"time"
)

// goroutine sender
func sender(g *Graph, node *Node, printer chan string) {
	for {
		// czekamy
		rand := randomValue(2000, 500)
		time.Sleep(time.Millisecond * time.Duration(rand))
		// tworzymy pakiet
		pack := Package{from: node}
		// petla po routing table
		for i := 0; i < len(node.routingTable); i++ {
			if i != node.index {
				// tworzymy pytanie i je wysylamy
				readInfo := ReadInfo{i, make(chan int)}
				node.read <- readInfo
				// odpowiedz czy element zostal juz zmieniony
				cost := <-readInfo.resp
				// jesli element zostal zmieniony
				if cost > 0 {
					// dodajemy do pakietu pare z indeksem wierzcholka i kosztem podrozy do niego
					pair := Pair{i, cost}
					pack.pairs = append(pack.pairs, pair)
				}
			}
		}
		// jesli pakiet nie jest pusty
		if len(pack.pairs) > 0 {
			// wysylamy go do wszytskich sasiadow
			for _, element := range g.edges[node] {
				//info := fmt.Sprintf("Node %d send package to node %d", node.index, element.index)
				//printer <- info
				element.receive <- pack
			}
		}
	}
}

//goroutine receiver
func receiver(node *Node, printer chan string) {
	for {
		// odbieramy pakiet
		pack := <-node.receive
		//info := fmt.Sprintf("Node %d received package from node %d", node.index, pack.from.index)
		//printer <- info
		// pobieramy kazda pare
		for _, element := range pack.pairs {
			// wyliczamy nowy koszt
			newCost := element.cost + 1
			// jesli jest on mniejszy niz ten co mamy zapisany
			if newCost < node.routingTable[element.number].cost {
				// tworzymy nowe informacje do zapisania i je wysylamy
				writeInfo := WriteInfo{element.number, newCost, pack.from, make(chan bool)}
				node.write <- writeInfo
				<-writeInfo.resp
			}
		}
	}
}

func state(node *Node, printer chan string) {
	for {
		select {
		// gdy wierzcholek czyta
		case read := <-node.read:
			// pobieramy stan i wysylamy
			state := node.routingTable[read.index].changed
			// jesli stan byl true to go zmieniamy
			if state == true {
				//info := fmt.Sprintf("Setting change of routing table[%d] in node %d to false", read.index, node.index)
				//printer <- info
				node.routingTable[read.index].changed = false
				read.resp <- node.routingTable[read.index].cost
			} else {
				read.resp <- -1
			}
		// gdy wierzcholek zapisuje
		case write := <-node.write:
			info := fmt.Sprintf("Setting cost of routing table[%d] in node %d from %d to %d, nextHop from %d to %d, changed to true", write.index, node.index, node.routingTable[write.index].cost, write.newCost, node.routingTable[write.index].nextHop.index, write.newHop.index)
			printer <- info
			// zmianiamy parametry i usawiamy stan na true
			node.routingTable[write.index].cost = write.newCost
			node.routingTable[write.index].nextHop = write.newHop
			node.routingTable[write.index].changed = true
			write.resp <- true
		// gdy wierzcholek otrzymuje pytanie o nextHop
		case ask := <-node.ask:
			ask.resp <- node.routingTable[ask.index].nextHop.index
		}

	}
}

//goroutine printer
func print(printer chan string) {
	for {
		mess := <-printer
		fmt.Println(mess)
	}
}

// goroutine wrzucajaca pakiety do kolejki w wierzcholku
func queueSender(node *Node) {
	for {
		pack := <-node.receiveStandardPack
		node.queue <- pack
	}
}

// gouroutine wyciagajaca pakiety z kolejki i przetwarzajaca je
func queueReceiver(g *Graph, node *Node) {
	for {
		pack := <-node.queue
		// dodajemy wierzcholek do listy odwiedzonych routerow pakietu
		pack.visited = append(pack.visited, node)
		// jesli cel do ten router
		if pack.receiver.r == node.index {
			// wysylamy pakiet do odpowiedniego hosta
			node.hosts[pack.receiver.h].destination <- pack
		} else {
			// pytamy routing table gdzie wyslac pakiet aby najszybciej dosc do celu
			ask := NextHopInfo{pack.receiver.r, make(chan int)}
			node.ask <- ask
			nextHop := <-ask.resp
			// wysylamy pakiet do routera z odpowiedzi
			g.nodes[nextHop].receiveStandardPack <- pack
		}
	}

}

// host
func host(node *Node, host Host, g *Graph, printer chan string) {
	// losujemy hosta do ktorego wyslemy pakiet
	dest := randomValue(len(g.allHosts))
	// nie wysylamy do siebie
	for g.allHosts[dest] == host {
		dest = randomValue(len(g.allHosts))
	}
	destination := g.allHosts[dest]
	// tworzymy pakiet i wysylamy do naszego routera
	pack := StandardPackage{sender: host, receiver: destination}
	node.receiveStandardPack <- pack
	for {
		// gdy otrzymamy pakiet
		pack := <-host.destination
		info := fmt.Sprintf("Standard package from (%d,%d) was received by (%d,%d). I am (%d,%d). Route:", pack.sender.r, pack.sender.h, pack.receiver.r, pack.receiver.h, host.r, host.h)
		for i := 0; i < len(pack.visited); i++ {
			info += fmt.Sprintf(" %d", pack.visited[i].index)
		}
		printer <- info
		// czekamy
		rand := randomValue(1000, 200)
		time.Sleep(time.Millisecond * time.Duration(rand))
		// wysylamy pakiet to hosta od ktorego otrzymalismy pakiet
		newPack := StandardPackage{sender: host, receiver: g.nodes[pack.sender.r].hosts[pack.sender.h]}
		node.receiveStandardPack <- newPack
	}
}
