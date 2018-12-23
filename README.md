# sokoban-5

W moim rozwiązaniu do rysowania użyłem wytycznych ze strony  [sokoban level format](http://www.sokobano.de/wiki/index.php?title=Level_format)

Wybrałem *"potencjalnie wydajniejszą i bliższą ideom programowania funkcyjnego reprezentację"* funkcji Picture

Wczytywanie wejścia wykonuje za pomocą funkcji getChar<br>
Efektem ubocznym tego jest to, że naciskanie strzałek resetuje grę

Funkcjonalności wymienione niżej:
 - ekran startowy (opuszczany przez naciśnięcie spacji)
 - ekran końcowy (wraz z liczbą wykonanych ruchów)
 - cofanie ruchu (naciśnięcie klawisza u)
 - resetowanie gry (naciśnięcie klawisza esc)
 - przechodzenie między poziomami (naciśnięcie klawisza 1)
 
Pozostały takie jak w poprzednich iteracjach zadania.

Pisząc korzystałem z narzędzia stack<br>
Aby uruchomic grę można wykonać komendę "stack run sokoban-exe"<br>
