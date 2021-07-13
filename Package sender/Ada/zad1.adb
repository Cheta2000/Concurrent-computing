with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Exceptions;          use Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

procedure zad1 is

    --typy tablicy wierzcholkow i macierzy krawedzi o nieznanym rozmiarze
    type numArray is array (Natural range <>) of Natural;
    type edgesMatrix is array (Natural range <>, Natural range <>) of Natural;

    --graf zawiera tablice wierzcholkow i macierz krawedzi
    type Graph (nodesNumber : Natural) is record
        nodes : numArray (1 .. nodesNumber);
        edges : edgesMatrix (1 .. nodesNumber, 1 .. nodesNumber);
    end record;

    --wierzcholek zawiera swoj numer, tablice z numerami pakietow ktore w nim byly oraz stan-czy jest w nim pakiet czy nie
    type Node (packsNumber : Natural) is record
        number   : Natural;
        packages : numArray (1 .. packsNumber);
        state    : Boolean;
    end record;

    --pakiet zawiera swoj numer, tablice z numerami wierzcholkow w ktorych byl oraz swoj czas zycia
    type Pack (nodesNumber : Natural) is record
        number : Natural;
        nodes  : numArray (1 .. nodesNumber);
        ttl    : Natural;
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

    --funkcja zwracajaca losowy wezel do ktorego mozna sie dostac z wezla node
    function next (g : Graph; node : Natural) return Natural is
        count : Natural := 0;
    begin
        --liczymy ile jest mozliwosci
        for i in g.nodes'Range loop
            if g.edges (node, i) = 1 then
                count := count + 1;
            end if;
        end loop;
        declare
            check : Natural := 0;
        begin
            --losujemy z zakresu 1..ilosc mozliwosci
            count := randomInt (1, count);
            --wybieramy wylosowana mozliwosc
            for i in g.nodes'Range loop
                if g.edges (node, i) = 1 then
                    check := check + 1;
                    if count = check then
                        return i;
                    end if;
                end if;
            end loop;
        end;
        return 0;
    end next;

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

    --procedura miesza tablice
    procedure shuffleArray (arr : in out numArray) is
        rand : Integer;
        tmp  : Natural;
    begin
        for i in reverse arr'Range loop
            rand       := randomInt (1, i);
            tmp        := arr (i);
            arr (i)    := arr (rand);
            arr (rand) := tmp;
        end loop;
    end shuffleArray;

    --procedura w ktorej uruchamiamy wszystkie taski
    procedure tasking (g : Graph; k : Natural; n : Natural; h : Natural) is

        --prodecent pakietow
        task insertPackage;

        --watek wierzcholka
        task type transferPackage is
            entry run (number : Natural);
            entry receive (pck : Pack);
            entry answer (state : Boolean);
            entry hunt;
        end transferPackage;

        --odbiorca pakietow
        task receivePackage is
            entry last (pck : Pack);
            entry died;
        end receivePackage;

        --serwer drukowania
        task printer is
            entry go (info : String);
        end printer;

        --watek wierzcholka do komunikacji czy jest w pacie
        task type pat is
            entry run (number : Natural);
            entry question (node : Natural);
        end pat;

        --watek klusownika
        task hunter is
            entry finish;
        end hunter;

        --tablica watkow wierzcholka, tworzymy watek dla kazdego wierzcholka
        nodesTransfer : array (1 .. n) of transferPackage;

        patChecking : array (1 .. n) of pat;

        --tablica wierzcholkow
        reportNode : array (1 .. n) of Node (k * h);

        --tablica pakietow
        reportPack : array (1 .. k) of Pack (h);

        task body insertPackage is
            --pakiet
            pck : Pack (h);
            --pusta tablica wierzcholkow
            nodes : numArray (1 .. h) := (others => 0);

        begin

            --wysylamy k pakietow
            for i in Natural range 1 .. k loop

                --tworzymy pakiet o numerze i i pustej tablicy wierzcholkow
                pck :=
                   (nodesNumber => h, number => i, nodes => nodes, ttl => h);
                --czekamy losowy czas
                delay Duration (randomTime (0.5, 1.5));
                --wysylamy pakiet do pierwszego wierzcholka
                nodesTransfer (1).receive (pck);
            end loop;
        end insertPackage;

        task body transferPackage is
            --pakiet
            tmp : Pack (h);
            --nr wierzcholka
            currentNode : Natural;
            --pusta tablica pakietow
            packs : numArray (1 .. k * h) := (others => 0);
            --licznik pakietow
            i : Natural := 0;
            --tablica sasiadow
            nodesArr : numArray (1 .. n);
            --licznik sasiadow
            counter : Natural := 0;
            --czy jest pulapka
            trap : Boolean := False;
        begin
            --czekamy az watek dostanie numer swojego wierzcholka
            accept run (number : Natural) do
                currentNode := number;
            end run;
            --do tablicy wierzcholkow dodajemy ten wierzcholek
            reportNode (currentNode) :=
               (packsNumber => k * h, number => currentNode, packages => packs,
                state       => False);
            --wypelniamy tablice sasiadow
            nodesArray (g, currentNode, nodesArr, counter);
--jesli to ostatni wierzcholek to ma tez dodatkowego sasiada: odbior pakietu
            if currentNode = n then
                counter := counter + 1;
            end if;

            loop
                select
                    --gdy przyjdzie pakiet
                    accept receive (pck : Pack) do
                        --pobieramy pakiet
                        tmp := pck;
                    end receive;
                    --wierzcholek jest zajety
                    reportNode (currentNode).state := True;
                    printer.go
                       ("Package" & tmp.number'Image & " is in node" &
                        currentNode'Image);
                    i := i + 1;
                    --dodajemy pakiet do tablicy pakietow wierzcholka
                    packs (i) := tmp.number;
                    --akutalizujemy tablice wierzcholkow
                    reportNode (currentNode).packages := packs;
--szukamy wolnej pozycji w tablicy wierzcholkow pakietu i dodajemy wierzcholek
                    for i in tmp.nodes'Range loop
                        if tmp.nodes (i) = 0 then
                            tmp.nodes (i) := currentNode;
                            exit;
                        end if;
                    end loop;
                    --jesli jest pulapka
                    if trap = True then
                --informujemy odbiorce ze pakiet umarl zeby na niego nie czekal
                        receivePackage.died;
                        printer.go
                           ("Package" & tmp.number'Image &
                            " was caught in trap in node" & currentNode'Image);
                        --usuwamy pulapke
                        trap := False;
                    else
                        --jesli pakiet nie moze juz dalej podrozowac
                        if tmp.ttl = 0 then
                            --jesli to ostatni wierzcholek
                            if currentNode = n then
                                --dajemy pakiet do odbioru
                                receivePackage.last (tmp);
                            else
                                --pakiet umiera
                                receivePackage.died;
                                printer.go
                                   ("Package" & tmp.number'Image &
                                    " died in node" & currentNode'Image);
                            end if;
                        else
                            declare
                                --zmienna mowiaca czy jest pat
                                occ : Boolean := True;
                            begin
                                while occ = True loop
                                    --czekamy losowy czas
                                    delay Duration (randomTime (0.5, 1.5));
                                    --mieszamy tablice
                                    shuffleArray (nodesArr (1 .. counter));
                                    for i in 1 .. counter loop
                                        --jesli pakiet idzie do odbiorcy
                                        if nodesArr (i) = 0 then
                                            receivePackage.last (tmp);
                                            occ := False;
                                            exit;
                                        else
                                --sprawdzamy czy wybrany wierzcholek jest wolny
                                            patChecking (nodesArr (i)).question
                                               (currentNode);
                                            accept answer (state : Boolean) do
                                                occ := state;
                                            end answer;
                                            --jesli jest wolny
                                            if occ = False then
                                    --pakiet idzie dalej i zmniejszamy mu ttl
                                                tmp.ttl := tmp.ttl - 1;
                                                nodesTransfer (nodesArr (i))
                                                   .receive
                                                   (tmp);
                                                exit;
                                            end if;
                                        end if;
                                    end loop;
                                end loop;
                            end;
                        end if;
                    end if;
                    --po wszytskich akcjach wierzcholek jest pusty
                    reportNode (currentNode).state := False;
                or
                    --klusownik zastawia pulapke
                    accept hunt;
                    trap := True;
                or
                    --koniec
                    terminate;
                end select;

            end loop;
        end transferPackage;

        task body receivePackage is
            --pakiet
            tmp     : Pack (h);
            counter : Natural := 0;
        begin
            --odbieramy k pakietow
            for i in Natural range 1 .. k loop
                select
                    --gdy poczekamy losowy czas i jest cos do odebrania
                    delay Duration (randomTime (0.5, 1.5));
                    accept last (pck : Pack) do
                        tmp := pck;
                    end last;
                    printer.go
                       ("Package" & tmp.number'Image & " was received");
                    --do tablicy pakietow dodajemy pakiet
                    counter              := counter + 1;
                    reportPack (counter) := tmp;
                    --gdy pakiet umarl
                or
                    accept died;
                end select;

            end loop;
            hunter.finish;
            --gdy odbierzemy ostatni pakiet wypisujemy wyniki
            Put_Line ("RESULTS:");
            Put_Line ("Edges:");
            for i in reportNode'Range loop
                Put (reportNode (i).number'Image & ":");
                for j in Natural range 1 .. k loop
                    if reportNode (i).packages (j) = 0 then
                        exit;
                    end if;
                    Put (reportNode (i).packages (j)'Image & " ");
                end loop;
                New_Line;
            end loop;
            Put_Line ("Packages:");
            for i in 1 .. counter loop
                Put (reportPack (i).number'Image & ":");
                for j in Natural range 1 .. n loop
                    if reportPack (i).nodes (j) = 0 then
                        exit;
                    end if;
                    Put (reportPack (i).nodes (j)'Image & " ");
                end loop;
                New_Line;
            end loop;
        end receivePackage;

        -- printer
        task body printer is
        begin
            loop
                select
                    accept go (info : String) do
                        Put_Line (info);
                    end go;
                or
                    terminate;
                end select;
            end loop;
        end printer;

        --pat
        task body pat is
            --ten wierzcholek
            currentNode : Natural;
            --pytajacy wierzcholek
            askingNode : Natural;
        begin
            accept run (number : Natural) do
                currentNode := number;
            end run;
            loop
                select
                    --gdy przyjdzie pytanie
                    accept question (node : Natural) do
                        askingNode := node;
                    end question;
            --jesli wierzcholek jest wolny to odpowiadamy i ustawiamy na zajety
                    if reportNode (currentNode).state = False then
                        reportNode (currentNode).state := True;
                        nodesTransfer (askingNode).answer (False);

                    else
                        nodesTransfer (askingNode).answer (True);
                    end if;
                or
                    terminate;
                end select;
            end loop;
        end pat;

        --hunter
        task body hunter is
            rand : Natural;
        begin
            loop
                select
                    --jesli przyjdzie polecenie konca
                    accept finish;
                    exit;

                or
                    --jesli minie losowy czas zastawiamy pulapke
                    delay Duration (randomTime (4.0, 5.0));
                    rand := randomInt (1, n);
                    nodesTransfer (rand).hunt;
                end select;
            end loop;
        end hunter;

    begin
        --odpalamy taski wierzcholkow z kolejnymi numerami
        for i in Natural range 1 .. n loop
            nodesTransfer (i).run (i);
            patChecking (i).run (i);
        end loop;
    end tasking;

    --funkcja liczaca maksymalna ilosc skrotow w grafie
    function calculateShortcuts (n : Positive) return Natural is
        max : Natural := 0;
    begin
        for i in Natural range 1 .. n loop
            max := max + n - i;
        end loop;
        max := max - n + 1;
        return max;
    end calculateShortcuts;

    --procedura dodajaca losowe krawedzie zgodne z warunkami zadania
    procedure randomEdges (g : in out Graph; d : Natural) is
        i : Natural := 0;
    begin
        while i < d loop
            declare
                edge1 : Natural;
                edge2 : Natural;
            begin
                loop
                    edge1 := randomInt (1, g.nodes'Length);
                    exit when edge1 /= g.nodes'Last;
                end loop;
                edge2 := randomInt (edge1 + 1, g.nodes'Length);
                if (g.edges (edge1, edge2) /= 1) then
                    g.edges (edge1, edge2) := 1;
                    i                      := i + 1;
                end if;
            end;
        end loop;
    end randomEdges;

    --procedura dodajaca losowe krawedzie skierowane
    procedure randomEdges2 (g : in out Graph; b : Natural) is
        i : Natural := 0;
    begin
        while i < b loop
            declare
                edge1 : Natural;
                edge2 : Natural;
            begin
                loop
                    edge1 := randomInt (1, g.nodes'Length);
                    exit when edge1 /= 1;
                end loop;
                edge2 := randomInt (1, edge1 - 1);
                if (g.edges (edge1, edge2) /= 1) then
                    g.edges (edge1, edge2) := 1;
                    i                      := i + 1;
                end if;
            end;
        end loop;
    end randomEdges2;

    --funkcja generujaca graf
    function generateGraph
       (n : Positive; d : Natural; b : Natural) return Graph
    is
        nodes : numArray (1 .. n);
        edges : edgesMatrix (1 .. n, 1 .. n);
        g     : Graph (n);
    begin
        for i in Natural range 1 .. n loop
            nodes (i) := i;
        end loop;
        for i in Natural range 1 .. n loop
            for j in Natural range 1 .. n loop
                if j = i + 1 then
                    edges (i, j) := 1;
                else
                    edges (i, j) := 0;
                end if;
            end loop;
        end loop;
        g := (nodesNumber => n, nodes => nodes, edges => edges);
        randomEdges (g, d);
        randomEdges2 (g, b);
        return g;
    end generateGraph;

    --procedura wypisujaca graf
    procedure printGraph (g : Graph) is
    begin
        Put_Line ("Graph: ");
        for i in g.nodes'Range loop
            Put (g.nodes (i)'Image & " =>");
            for j in g.edges'Range loop
                if g.edges (i, j) /= 0 then
                    Put (j'Image & " ");
                end if;
            end loop;
            New_Line;
        end loop;
    end printGraph;

    --procedura pobierajaca parametry, tworzaca graf i odpalajaca taski
    procedure start is
        n   : Positive;
        d   : Natural;
        k   : Natural;
        b   : Natural;
        h   : Natural;
        max : Natural;
    begin
        Put_Line ("Input size of graph:");
        Get (n);
        max := calculateShortcuts (n);
        Put_Line ("Input number of shortcuts (max=" & max'Image & "):");
        Get (d);
        if max < d then
            raise INVALID_SHORTCUTS;
        end if;
        max := n * (n - 1) / 2;
        Put_Line ("Input number of linked edges (max=" & max'Image & "):");
        Get (b);
        if max < b then
            raise INVALID_SHORTCUTS;
        end if;
        declare
            g : Graph (n) := generateGraph (n, d, b);
        begin

            printGraph (g);
            Put_Line ("Input number of packages to send:");
            Get (k);
            Put_Line ("Input package lifetime: ");
            Get (h);
            tasking (g, k, n, h);
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
