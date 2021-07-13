package main

import (
	"fmt"
)

func main() {
	var n, d, k int
	fmt.Println("Input size of graph: ")
	fmt.Scan(&n)
	if n <= 0 {
		fmt.Println("ERROR. Size of graph must be positive")
		return
	}
	max := n*(n-1)/2 - n + 1
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
	fmt.Println("Input number of hosts: ")
	fmt.Scan(&k)
	if k < 0 {
		fmt.Println("ERROR. Number of hosts cannot be negative")
		return
	}
	g := generateGraph(n, d, k)
	printGraph(g)
	done := make(chan bool)
	printer := make(chan string, n*n*2)
	for i := 0; i < n; i++ {
		go sender(g, g.nodes[i], printer)
		go state(g.nodes[i], printer)
		go receiver(g.nodes[i], printer)
		go queueSender(g.nodes[i])
		go queueReceiver(g, g.nodes[i])
		for j := 0; j < len(g.nodes[i].hosts); j++ {
			go host(g.nodes[i], g.nodes[i].hosts[j], g, printer)
		}
	}
	go print(printer)
	<-done
}
