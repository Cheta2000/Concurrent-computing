package main

import (
	"fmt"
	"sync"
)

func main() {
	var n, d, k, b, h int
	fmt.Println("Input size of graph: ")
	fmt.Scan(&n)
	max, err := calculateShortcuts(n)
	if err != nil {
		fmt.Println("ERROR:", err)
		return
	}
	info := fmt.Sprintf("Input number of shortcuts (max=%d): ", max)
	fmt.Println(info)
	fmt.Scan(&d)
	if d < 0 {
		fmt.Println("ERROR. Number of shortcuts cannot be negative")
		return
	}
	if d > max {
		fmt.Println("ERROR. Too many shortcuts")
		return
	}
	max = n * (n - 1) / 2
	info = fmt.Sprintf("Input number of linked egdes (max=%d): ", max)
	fmt.Println(info)
	fmt.Scan(&b)
	if b < 0 {
		fmt.Println("ERROR. Number of linked edges cannot be negative")
		return
	}
	if b > max {
		fmt.Println("ERROR. Too many edges")
		return
	}
	g := generateGraph(n, d, b)
	printGraph(g)
	fmt.Println("Input number of packages to send: ")
	fmt.Scan(&k)
	if k <= 0 {
		fmt.Println("ERROR. Number of packages must be positive.")
		return
	}
	fmt.Println("Input package lifetime:")
	fmt.Scan(&h)
	if h <= 0 {
		fmt.Println("ERROR. Package lifetime must be positive")
		return
	}
	done := make(chan bool)
	printer := make(chan string, k)
	packs := []*Package{}
	var wg sync.WaitGroup
	wg.Add(k)
	go insertPackage(k, g.nodes[0], h)
	for i := 0; i < n; i++ {
		go transferPackage(g, g.nodes[i], printer, &wg, &packs)
		go pat(g.nodes[i])
	}
	go receivePackage(g, printer, &wg, &packs)
	go hunter(g)
	// goroutine wypisujaca na terminal
	go func() {
		for mess := range printer {
			fmt.Println(mess)
		}
		done <- true
	}()
	// czekamy na odebranie lub smierc wszystskich pakietow i zamykamy kanal do wypisywania
	go func() {
		wg.Wait()
		close(printer)
	}()
	<-done
	printResults(g, packs)
}
