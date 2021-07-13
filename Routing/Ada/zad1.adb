with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Exceptions;            use Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure zad1 is

    --typy tablicy wierzcholkow i macierzy krawedzi o nieznanym rozmiarze
    type numArray is array (Natural range <>) of Natural;
    type edgesMatrix is array (Natural range <>, Natural range <>) of Natural;

    -- host zawiera router i swoj indeks
    type Host is record
        r : Natural;
        h : Natural;
    end record;

    type allHosts is array (Natural range <>) of Host;

    --graf zawiera tablice wierzcholkow i macierz krawedzi
    type Graph (nodesNumber : Natural; hostsNumber : Natural) is record
        nodes : numArray (1 .. nodesNumber);
        edges : edgesMatrix (1 .. nodesNumber, 1 .. nodesNumber);
        hosts : allHosts (1 .. hostsNumber);
    end record;

    --do przekazywania danych do wypisania
    type Previous is record
        cost    : Integer;
        nextHop : Natural;
    end record;

    --typ z mutexem na zapis, odczyt i pytanie o nextHop
    protected type Routing is
        procedure Write
           (newHop : Natural; newCost : Natural; control : in out Previous);
        procedure Read (state : in out Boolean; askCost : in out Natural);
        procedure Ask (hop : in out Natural);
    private
        nextHop : Natural;
        cost    : Natural := 10_000;
        changed : Boolean;
    end Routing;

    protected body Routing is
        procedure Write
           (newHop : Natural; newCost : Natural; control : in out Previous)
        is
        begin
            --jesli nowy koszt jest mniejszy niz poprzedni to zapisujemy
            if newCost < cost then
                control.cost    := cost;
                control.nextHop := nextHop;
                nextHop         := newHop;
                cost            := newCost;
                changed         := True;
            else
                control.cost := -1;
            end if;
        end Write;
        procedure Read (state : in out Boolean; askCost : in out Natural) is
        begin
            --informujemy o stanie
            state := changed;
            if state = True then
                changed := False;
            end if;
            askCost := cost;
        end Read;
        procedure Ask (hop : in out Natural) is
        begin
            --informujemy o nextHopie
            hop := nextHop;
        end Ask;
    end Routing;

    type routingArray1 is array (Natural range <>) of Routing;
    -- wskazanie na tablice
    type routingArray is access routingArray1;

    --wierzcholek zawiera swoj numer i routingtable
    type Node (nodesNumber : Natural) is record
        number       : Natural;
        routingTable : routingArray (1 .. nodesNumber);
    end record;

    --para do pakietu
    type Pair is record
        number : Natural;
        cost   : Natural;
    end record;

    type pairArray is array (Natural range <>) of Pair;

--pakiet zawiera dane od kogo zostal wyslany, tablice par i rozmiar tej tablicy
    type Pack (nodesNumber : Natural) is record
        from  : Natural;
        pairs : pairArray (1 .. nodesNumber);
        size  : Natural;
    end record;

--pakiet standardowy: host sendera i receivera i lista odwiedzonych routerow
    type StandardPackage (nodesNumber : Natural) is record
        sender   : Host;
        receiver : Host;
        visited  : numArray (1 .. nodesNumber);
    end record;

    --funkcja zwracajaca losowego inta z zakresu
    function randomInt (min : Integer; max : Integer) return Integer is
        subtype randomRange is Natural range min .. max;
        package R is new Ada.Numerics.Discrete_Random (randomRange);
        use R;
        dice : Generator;
    begin
        Reset (dice);
        return Random (dice);
    end randomInt;

    --funkcja zwracajaca losowego floata z zakresu
    function randomTime (min : Float; max : Float) return Float is
        use Ada.Numerics.Float_Random;
        dice : Generator;
    begin
        Reset (dice);
        return Random (dice) * max + min;
    end randomTime;

    --procedura zwraca tablice sasiadow wierzcholka
    procedure nodesArray
       (g       :        Graph; node : Natural; arr : in out numArray;
        counter : in out Natural)
    is
    begin
        for i in g.nodes'Range loop
            if g.edges (node, i) = 1 then
                counter       := counter + 1;
                arr (counter) := i;
            end if;
        end loop;
    end nodesArray;

    --procedura w ktorej uruchamiamy wszystkie taski
    procedure tasking (g : Graph; n : Natural) is

        type stdPck is new StandardPackage (n);

        --kolejka zawieracjaca standardowe pakiety
        package StandardPackage_Queue_Interfaces is new Ada.Containers
           .Synchronized_Queue_Interfaces
           (Element_Type => stdPck);

        package StandardPackage_Queues is new Ada.Containers
           .Unbounded_Synchronized_Queues
           (Queue_Interfaces => StandardPackage_Queue_Interfaces);

        queue : array (1 .. n) of StandardPackage_Queues.Queue;

        task type sender is
            entry run (currentNode : Node);
        end sender;

        task type receiver is
            entry run (currentNode : Node);
            entry receive (pck : Pack);
        end receiver;

        task printer is
            entry print (info : String);
        end printer;

        task type hostThread is
            entry run (currNode : Natural; index : Natural);
            entry receive (pck : stdPck);
        end hostThread;

        task type queueSender is
            entry run (currNode : Node);
            entry receive (pck : stdPck);
        end queueSender;

        task type queueReceiver is
            entry run (currNode : Node);
        end queueReceiver;

        sendTask         : array (1 .. n) of sender;
        receiveTask      : array (1 .. n) of receiver;
        queueSendTask    : array (1 .. n) of queueSender;
        queueReceiveTask : array (1 .. n) of queueReceiver;
        hosts            : array (1 .. g.hosts'Length) of hostThread;

        task body sender is
            currNode  : Node (n);
            nodesArr  : numArray (1 .. n);
            counter   : Natural := 0;
            pck       : Pack (n);
            pckPairs  : pairArray (1 .. n);
            index     : Natural := 0;
            pairToAdd : Pair;
            state     : Boolean := False;
            askCost   : Natural := 0;
        begin
            -- ustawiamy wierzcholek
            accept run (currentNode : Node) do
                currNode := currentNode;
            end run;
            pck.from := currNode.number;
            -- ustawiamy sasiadow
            nodesArray (g, currNode.number, nodesArr, counter);
            loop
                delay Duration (randomTime (0.5, 2.0));
                for i in Natural range 1 .. n loop
                    if i /= currNode.number then
                        --pytamy o konkretny element tablicy routingu
                        currNode.routingTable (i).Read (state, askCost);
                        --jesli jest juz zmieniony
                        if state then
                            -- tworzymy pare do pakietu
                            --printer.print("Setting change of routing table[" & i'img & " ] in node" & currNode.number'img & " to false");
                            index            := index + 1;
                            pairToAdd.number := i;
                            pairToAdd.cost   := askCost;
                            pckPairs (index) := pairToAdd;
                        end if;
                    end if;
                end loop;
                if index > 0 then
                    --wysylamy pakiety do wszystkich sasiadow
                    pck.pairs := pckPairs;
                    pck.size  := index;
                    index     := 0;
                    for i in Natural range 1 .. counter loop
                        --printer.print("Node" & currNode.number'img & " send package to node" & nodesArr(i)'img);
                        receiveTask (nodesArr (i)).receive (pck);
                    end loop;
                end if;
            end loop;
        end sender;

        task body receiver is
            currNode : Node (n);
            tmp      : Pack (n);
            newCost  : Natural;
            control  : Previous;
        begin

            accept run (currentNode : Node) do
                currNode := currentNode;
            end run;
            loop
                --gdy jest cos do odebrania
                accept receive (pck : Pack) do
                    tmp := pck;
                end receive;
                -- printer.print("Node" & currNode.number'img & " received package from node" & tmp.from'img);
                --przetawrzamy wszystkie pary z pakietu
                for i in Natural range 1 .. tmp.size loop
                    if tmp.pairs (i).number /= currNode.number then
                        --liczmy nowy koszt i probujemy zapisac
                        newCost := tmp.pairs (i).cost + 1;
                        currNode.routingTable (tmp.pairs (i).number).Write
                           (tmp.from, newCost, control);
                        if control.cost > 0 then
                            printer.print
                               ("Setting cost of routing table[" &
                                tmp.pairs (i).number'Img & " ] in node" &
                                currNode.number'Img & " from" &
                                control.cost'Img & " to" & newCost'Img &
                                ", nextHop from" & control.nextHop'Img &
                                " to" & tmp.from'Img & ", changed to true");
                        end if;
                    end if;
                end loop;
            end loop;

        end receiver;

        task body printer is
        begin
            loop
                accept print (info : String) do
                    Put_Line (info);
                end print;
            end loop;
        end printer;

        task body hostThread is
            currentNode : Natural;
            hostIndex   : Natural;
            tmp         : stdPck;
            newPck      : stdPck;
            rand        : Natural;
            myHost      : Host;
            recHost     : Host;
            info        : Unbounded_String;
            nodes       : numArray (1 .. n) := (others => 0);
        begin
            -- startujemy hosta z jego routerem i indeksem
            accept run (currNode : Natural; index : Natural) do
                currentNode := currNode;
                hostIndex   := index;
            end run;
            myHost := (currentNode, hostIndex);
            -- losujemy do kogo wyslemy pakiet
            rand := randomInt (1, g.hosts'Length);
            while g.hosts (rand).r = currentNode and
               g.hosts (rand).h = hostIndex
            loop
                rand := randomInt (1, g.hosts'Length);
            end loop;
            recHost := g.hosts (rand);
            -- tworzymy pakiey i wysylamy do swojego routera
            newPck :=
               (nodesNumber => n, sender => myHost, receiver => recHost,
                visited     => nodes);
            queueSendTask (currentNode).receive (newPck);
            loop
                -- odbieramy pakiey
                accept receive (pck : stdPck) do
                    tmp := pck;
                end receive;
                info := To_Unbounded_String ("");
                info :=
                   info & "Standard package from (" & tmp.sender.r'Img & "," &
                   tmp.sender.h'Img & " ) was received by (" &
                   tmp.receiver.r'Img & "," & tmp.receiver.h'Img &
                   " ). I am (" & myHost.r'Img & "," & myHost.h'Img &
                   " ). Route:";
                for i in Natural range 1 .. tmp.visited'Length loop
                    if tmp.visited (i) /= 0 then
                        info := info & tmp.visited (i)'Img;
                    end if;
                end loop;
                printer.print (Ada.Strings.Unbounded.To_String (info));
                -- czekamy i odpowiadamy pakietem
                delay Duration (randomTime (0.2, 1.0));
                recHost.r := tmp.sender.r;
                recHost.h := tmp.sender.h;
                newPck    :=
                   ((nodesNumber => n, sender => myHost, receiver => recHost,
                     visited     => nodes));
                queueSendTask (currentNode).receive (newPck);
            end loop;
        end hostThread;

        task body queueSender is
            currentNode : Node (n);
        begin
            accept run (currNode : Node) do
                currentNode := currNode;
            end run;
            loop
                --odbieramy pakiet i wrzucamy do kolejki
                accept receive (pck : stdPck) do
                    queue (currentNode.number).Enqueue (New_Item => pck);
                end receive;
            end loop;
        end queueSender;

        task body queueReceiver is
            pck         : stdPck;
            currentNode : Node (n);
            hop         : Natural := 0;
        begin
            accept run (currNode : Node) do
                currentNode := currNode;
            end run;
            loop
                -- odbieramy pakiet z kolejki
                queue (currentNode.number).Dequeue (Element => pck);
                -- dodajemy router do listy odwiedzonych
                for i in Natural range 1 .. pck.visited'Length loop
                    if pck.visited (i) = 0 then
                        pck.visited (i) := currentNode.number;
                        exit;
                    end if;
                end loop;
                -- gdy jestesmy routerem docelowym
                if pck.receiver.r = currentNode.number then
                    for i in Natural range 1 .. g.hosts'Length loop
                        -- szukamy hosta i wysylamy do niego pakiet
                        if g.hosts (i).r = pck.receiver.r and
                           g.hosts (i).h = pck.receiver.h
                        then
                            hosts (i).receive (pck);
                            exit;
                        end if;
                    end loop;
                else
                    -- pytamy gdzie musimy isc aby w najkrotszej znanej drodze dojsc do routera docelowego i wysyamy tam
                    currentNode.routingTable (pck.receiver.r).Ask (hop);
                    queueSendTask (hop).receive (pck);
                end if;
            end loop;
        end queueReceiver;

        nodes   : array (1 .. n) of Node (n);
        control : Previous;
    begin
        --tworzenie routingtable dla kazdego wierzcholka
        for i in Natural range 1 .. n loop
            declare
                routingTable : routingArray := new routingArray1 (1 .. n);
            begin
                for j in Natural range 1 .. n loop
                    if i /= j then
                        if g.edges (i, j) = 1 then
                            routingTable (j).Write (j, 1, control);
                        else
                            if j < i then
                                routingTable (j).Write (i - 1, i - j, control);
                            else
                                routingTable (j).Write (i + 1, j - i, control);
                            end if;
                        end if;
                    end if;
                end loop;
                nodes (i).number       := i;
                nodes (i).routingTable := routingTable;
            end;
        end loop;
        -- odpalamy wszytskie hosty
        for i in Natural range 1 .. g.hosts'Length loop
            hosts (i).run (g.hosts (i).r, g.hosts (i).h);
        end loop;
        --odpalamy taski z ich wierzcholkami
        for i in Natural range 1 .. n loop
            sendTask (i).run (nodes (i));
            receiveTask (i).run (nodes (i));
            queueSendTask (i).run (nodes (i));
            queueReceiveTask (i).run (nodes (i));
        end loop;
    end tasking;

    --procedura dodajaca losowe krawedzie zgodne z warunkami zadania
    procedure randomEdges (g : in out Graph; d : Natural) is
        i : Natural := 0;
    begin
        while i < d loop
            declare
                edge1 : Natural;
                edge2 : Natural;
            begin
                edge1 := randomInt (1, g.nodes'Length);
                loop
                    edge2 := randomInt (1, g.nodes'Length);
                    exit when edge2 /= edge1;
                end loop;
                if (g.edges (edge1, edge2) /= 1) then
                    g.edges (edge1, edge2) := 1;
                    g.edges (edge2, edge1) := 1;
                    i                      := i + 1;
                end if;
            end;
        end loop;
    end randomEdges;

    -- procedura dodajaca loswe hosty
    procedure randomHosts (n : Natural; hosts : in out allHosts; k : Natural)
    is
        tmp : numArray (1 .. n) := (others => 0);
    begin
        for i in Natural range 1 .. k loop
            declare
                random  : Natural := randomInt (1, n);
                newHost : Host;
            begin
                newHost      := (random, tmp (random));
                tmp (random) := tmp (random) + 1;
                hosts (i)    := newHost;
                null;
            end;
        end loop;
    end randomHosts;

    --funkcja generujaca graf
    function generateGraph
       (n : Positive; d : Natural; k : Natural) return Graph
    is
        nodes : numArray (1 .. n);
        edges : edgesMatrix (1 .. n, 1 .. n);
        hosts : allHosts (1 .. k);
        g     : Graph (n, k);
    begin
        for i in Natural range 1 .. n loop
            nodes (i) := i;
        end loop;
        for i in Natural range 1 .. n loop
            for j in Natural range 1 .. n loop
                if j = i + 1 or j = i - 1 then
                    edges (i, j) := 1;
                else
                    edges (i, j) := 0;
                end if;
            end loop;
        end loop;
        randomHosts (n, hosts, k);
        g :=
           (nodesNumber => n, hostsNumber => k, nodes => nodes, edges => edges,
            hosts       => hosts);
        randomEdges (g, d);
        return g;
    end generateGraph;

    --procedura wypisujaca graf
    procedure printGraph (g : Graph) is
        counter : Natural := 0;
    begin
        Put_Line ("Graph: ");
        for i in g.nodes'Range loop
            Put (g.nodes (i)'Image & " =>");
            for j in g.edges'Range loop
                if g.edges (i, j) /= 0 then
                    Put (j'Image & " ");
                end if;
            end loop;
            counter := 0;
            for k in g.hosts'Range loop
                if g.hosts (k).r = g.nodes (i) then
                    counter := counter + 1;
                end if;
            end loop;
            Put ("Hosts:" & counter'Img);
            New_Line;
        end loop;
    end printGraph;

    --procedura pobierajaca parametry, tworzaca graf i odpalajaca taski
    procedure start is
        n   : Positive;
        d   : Natural;
        k   : Natural;
        max : Natural;
    begin
        Put_Line ("Input size of graph:");
        Get (n);
        max := n * (n - 1) / 2 - n + 1;
        Put_Line ("Input number of shortcuts (max=" & max'Image & "):");
        Get (d);
        if max < d then
            raise INVALID_SHORTCUTS;
        end if;
        Put_Line ("Input number of hosts:");
        Get (k);
        declare
            g : Graph (n, k) := generateGraph (n, d, k);
        begin
            printGraph (g);
            tasking (g, n);
        end;
    exception
        when Constraint_Error =>
            Put_Line ("Number must be positive");
        when Data_Error =>
            Put_Line ("Insert number");
        when INVALID_SHORTCUTS =>
            Put_Line ("Too many shortcuts");
    end start;

begin
    start;
end zad1;
