# Concurrent-computing
Some tasks from concurrent computing in Golang and Ada

PACKAGE SENDER

Zaimplementować program współbieżny symulujący system, w którym nadawca wysyła do odbiorcy autonomiczne pakiety podróżujące po acyklicznym grafie skierowanym G z jednym źródłem i z jednym ujściem. 
Wierzchołki grafu G są  indeksowane liczbami naturalnymi od 0 do n-1.
Wierzchołek 0 jest źródłem, a wierzchołek n-1  jest ujściem.
Dla każdego i, 0in-2, istnieje w grafie krawędź skierowana (i, i+1), co zapewnia, że z każdego wierzchołka jest jakaś ścieżka skierowana do ujścia.
Ponadto w grafie istnieje pewna liczba d  dodatkowych krawędzi postaci (j,k), gdzie 0j<kn-1 (tzw. skrótów).
Dla wierzchołka i, zbiorem następników i, N(i), nazywamy zbiór takich j, że istnieje krawędź (i,j).
Program ma działać następująco:
 Generowany jest graf G dla podanych parametrów n i d, gdzie d skrótów generowane jest w sposób losowy.
Graf G drukowany jest na terminalu tak aby przedstawić istniejące połączenia. (Zastanowić się nad tym jaki sposób prezentacji będzie najbardziej czytelny.)
Uruchamiana jest symulacja systemu przesyłania pakietów po grafie G.
System  przesyłania pakietów działa według następujących zasad:
W jednym wierzchołku może przebywać tylko jeden pakiet.
Co pewien losowy czas nadawca umieszcza w źródle (o ile jest ono puste) nowy pakiet indeksowany kolejną liczbą naturalną.
Co pewien losowy czas odbiorca odbiera z ujścia pakiet (o ile jest co odebrać).
Pakiet w wierzchołku i, po odczekaniu losowego czasu, wybiera losowo jeden wierzchołek j ze zbioru N(i) i czeka aż będzie mógł się do niego przemieścić. 
Gdy pakiet p dotrze do wierzchołka i drukowany jest komunikat:
"pakiet p jest w wierzchołku i"
i jednocześnie p dodaje i do swojej listy odwiedzonych wierzchołków oraz i dodaje p do swojej listy obsłużonych pakietów.
Gdy odbiorca odbierze pakiet p, drukuje komunikat:
"pakiet p został odebrany".     
Po nadaniu kpakietów, nadawca kończy nadawanie.
Gdy odbiorca odbierze ostatni (tj. k-ty)  pakiet, system kończy działanie i rozpoczyna się drukowanie raportów końcowych.
W raportach końcowych pojawią się dwa wykazy:
dla każdego wierzchołka, lista kolejno obsłużonych przez niego pakietów, 
dla każdego pakietu, lista odwiedzonych przez niego wierzchołków  (ścieżka od źródła do ujścia).
Zaimplementuj system tak, aby nadawca, odbiorca oraz każdy wierzchołek grafu były osobnymi wątkami współbieżnymi, które przekazują sobie pakiety przez narzędzia komunikacji między wątkami.
Zwróć uwagę, że komunikaty drukowane przez współbieżne wątki na terminalu mogą się przeplatać. Dlatego dodaj nowy wątek (serwer drukowania), który przyjmuje zlecenia drukowania komunikatów od wątków i niezwłocznie, ale sekwencyjnie, drukuje je na ekranie w osobnych liniach.  
Rozszerz system zaimplementowany w zadaniu z poprzedniej listy w taki sposób, aby można w nim dodać b  krawędzi skierowanych postaci (i,j), gdzie i>j, oraz ustalić parametr h, oznaczający czas życia pakietu rozumiany jako największa liczba jego transferów od wierzchołka do wierzchołka. W grafie mogą występować cykle, więc jeśli pakiet w h krokach nie dotrze do celu, to  drukowany jest komunikat o jego śmierci i znika z systemu. 
Program ma być uruchamiany z parametrami: n, d, b,  k, h, gdzie parametry n, d,  k oznaczają to samo co w zadaniu z poprzedniej listy, a parametry b, h mają takie znaczenie jak opisano wyżej.
Dodaj wątek kłusownika, który co pewien czas budzi się, kontaktuje się z wątkiem losowo wybranego wierzchołka i umieszcza w nim pułapkę na jeden pakiet.  Jeśli pakiet dotrze do wierzchołka z zastawioną pułapką, to drukowany jest komunikat, że wpadł on w pułapkę i pakiet znika z systemu wraz z pułapką, w którą wpadł. 
(Wskazówka: W wątku wierzchołka zastosuj konstrukcję select, aby mógł on obsługiwać zarówno zastawienie pułapki kłusownika, jak i odbieranie pakietów.)

ROUTING

Zakładamy, że mamy graf n wierzchołków, w którym krawędzie są nieskierowane. 
Krawędź między wierzchołkami i a j oznaczamy: {i,j}.
Listę sąsiadów wierzchołka i oznaczamy: N(i).
Podobnie jak w poprzednich zadaniach zakładamy, że w grafie istnieje ścieżka Hamiltona złożona z krawędzi postaci {v, v+1} (dzięki czemu graf jest spójny), oraz pewna liczba d dodatkowych krawędzi (skrótów). 
Należy zaimplementować wykonywanie protokołu routingu podobnego do znanego protokołu RIP, zgodnie z poniższymi wskazówkami.
Każdy wierzchołek i zawiera zmienną reprezentującą tzw. routing table (oznaczaną przez Ri), która dla każdego wierzchołka j, różnego od i, zawiera następujące dane:
Ri[j].nexthop - wierzchołek ze zbioru N(i) (tj. sąsiad i) leżący na najkrótszej, znanej wierzchołkowi i, ścieżce p od i do j, oraz
Ri[j].cost - długość tej ścieżki p.
Początkowo  każdy wierzchołek i zna swoich bezpośrednich sąsiadów N(i) i wie o istnieniu krawędzi postaci {v,v+1}. Zatem, 
dla jN(i),  początkowo Ri[j].cost=1  i  Ri[j].nexthop=j, a
dla jN(i), Ri[j].cost=|i-j|  oraz
Ri[j].nexthop=i+1, jeśli i<j, albo 
Ri[j].nexthop=i-1, jeśli j<i.
Ponadto, dla każdegoRi[j], istnieje flaga Ri[j].changed (początkowo ustawiona na true).
W każdym wierzchołku i działają dwa współbieżne wątki:
Senderi  oraz
Receiveri
Oba te wątki mają współbieżny dostęp do routing table Ri. W Go można zaimplementować Ri jako stateful goroutine a w Adzie jako zmienną protected.
Co pewien czas Senderi budzi się i jeśli istnieją jakieś j, gdzie Ri[j].changed=true, to tworzy pakiet z ofertą, do którego dodaje pary (j, Ri[j].cost) dla wszystkich takich j, ustawiając Ri[j].changedna false, a następnie wysyła ten pakiet do każdego swojego sąsiada z N(i).
Wątek Receiveri oczekuje na pakiet z ofertą od jakiegoś sąsiada z N(i). Gdy taki pakiet otrzymuje od jakiegoś sąsiada l, to dla każdej pary (j, costj)z takiego pakietu:
wylicza newcosti,j=1+costj,
jeśli newcosti,j<Ri[j].cost to ustawia nowe wartości:
Ri[j].cost=newcost,
Ri[j].nexthop=l,
Ri[j].changed=true,
Oba wątki drukują stosowne komunikaty o wysyłanych i otrzymywanych pakietach oraz zmianach w w routing table.
Zwróć uwagę aby Senderi nie blokował dostępu do Ri w czasie gdy rozsyła pakiet z ofertami do sąsiadów oraz tak zmieniał Ri[j].changed na false aby nie "zagłuszyć" żadnej nowej zmiany. 
Rozbudowujemy zadanie z listy 3:
Wierzchołki grafu, wykonujące protokół routingu, nazywamy routerami. 
Dodatkowo dodajemy nowy rodzaj elementów nazwanych hostami.
Każdy host może być podłączony do jednego routera.
Hosty podłączone do jednego routera są indeksowane kolejnymi liczbami od zera.
Host o indeksie h podłączony do routera r ma w sieci adres, który jest parą numerów (r,h).
Host może generować pakiety adresowane do innego hosta. Nazwijmy je pakietami standardowymi, w odróżnieniu od pakietów z ofertami przesyłanymi między routerami na potrzeby protokołu routingu. Taki pakiet standardowy zawiera:
adres nadawcy: (rs,hs),
adres odbiorcy:  (rd,hd) oraz
listę odwiedzonych routerów.
W każdym routerze r, oprócz wątków Senderr i  Receiverr, uruchomiony jest dodatkowy wątek Forwarderr, który zajmuje się przekazywaniem pakietów standardowych.  Działa on powtarzając następujące czynności:
Oczekuje na pakiet standardowy od
hosta podłączonego do r (nadawcy pakietu)  lub
wątku Forwarderj, sąsiedniego routera j, jN(r).
Gdy otrzyma taki pakiet p o adresie odbiorcy  (rd,hd), to:
Dopisuje swój indeks r do listy odwiedzonych routerów pakietu p.
Jeśli rd=r, to przesyła pakiet do (bezpośrednio podłączonego) hosta   (rd,hd) -- odbiorcy pakietu.
Jeśli rdr, to przesyła pakiet do wątku Forwardern sąsiedniego routera n, takiego, że Rr[rd].nexthop=n, gdzie Rrjest tablicą routingu routera r.
Każdy host (r,h) działa następująco:
Losuje sobie jakiś inny istniejący host (r',h') i nadaje pakiet o adresie odbiorcy:  (r',h'), przesyłając go do Forwarderr swojego routera r.
Następnie przechodzi do trybu, w którym powtarza następujące czynności:
Oczekuje na nadejście jakiegoś pakietu od innego hosta.
Gdy nadejdzie taki pakiet p od nadawcy (rs,hs), to:
Drukuje pakiet p (adresy nadawcy, odbiorcy i listę odwiedzonych routerów).
Po losowym opóźnieniu, nadaje pakiet o adresie odbiorcy (rs,hs). 
Po wygenerowaniu grafu połączeń, przed uruchomieniem działania systemu, wydrukować graf połączeń między routerami, zaznaczając przy każdym routerze liczbę podłączonych do niego hostów.
W czasie działania systemu, protokół routingu modyfikujący tablice routingu wykonuje się współbieżnie z przesyłaniem pakietów standardowych. 
Dobrać wyświetlanie informacji oraz opóźnienia w działaniach protokołu routingu i hostów, tak aby było widoczne, że kolejne pakiety standardowe przesyłane między daną parą hostów podróżują coraz krótszymi trasami.
Komunikaty dotyczące protokołu routingu ograniczyć tylko do drukowania tablicy routingu, gdy zostanie ona zmodyfikowana przez pakiet z ofertami.
POPRAWKA: Słusznie zwrócono mi uwagę, że między wątkami Forwarder może powstać deadlock. W związku z tym, należy rozbić ten wątek na dwa wątki:
jeden odbiera pakiety standardowe i umieszcza je w kolejce,
drugi pobiera pakiety z kolejki i usiłuje je przekazać dalej.
Można założyć, że kolejka pakietów jest nieograniczona.
