type Factor = (String,Int)
type Accion = Persona -> Persona
type Condicion = Factor -> Bool

data Persona = Persona { nombre :: String, dinero :: Float, suerte :: Int, factores :: [Factor] } deriving(Show)

nico = Persona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)]
maiu = Persona "Maiu" 100.0 42 [("inteligencia",55), ("paciencia",50)]


-- EJERCICIO 1
suerteTotal :: Persona -> Int
suerteTotal unaPersona 
 | (existeAmuleto.factores) unaPersona = (*) (suerte unaPersona) ((valorDelAmuleto.esAmuleto.factores) unaPersona)
 | otherwise = suerte unaPersona

existeAmuleto :: [Factor] -> Bool
existeAmuleto = (/=0).length.esAmuleto     --no pude hacerlo con funcion 'elem'

esAmuleto :: [Factor] -> [Factor]
esAmuleto = filter esUnAmuleto

esUnAmuleto :: Factor -> Bool
esUnAmuleto (_,0) = False
esUnAmuleto ("amuleto",_) = True
esUnAmuleto (_,_) = False

valorDelAmuleto :: [Factor] -> Int
valorDelAmuleto = snd.head


-- EJERCICIO 2
data Juego = Juego { nombreJuego :: String, cuantoDineroGanaria :: (Float->Float), criterios :: [Persona->Bool] }

ruleta = Juego "ruleta" funcionRuleta [(suerteTotalMayorA 80)]
maquinita = Juego "maquinita" (funcionMaquinita 20 {-jackpot-}) [(suerteTotalMayorA 95), tenerPaciencia]    --jackpot no tiene valor numerico?

funcionRuleta :: Float -> Float
funcionRuleta apuesta = (*) 37 apuesta

funcionMaquinita :: Float -> Float -> Float
funcionMaquinita = (+)

suerteTotalMayorA :: Int -> Persona -> Bool
suerteTotalMayorA numeroCondicionante = ((>numeroCondicionante).suerteTotal)

tenerPaciencia :: Persona -> Bool
tenerPaciencia = existe.filter esPaciencia.factores

esPaciencia :: Factor -> Bool
esPaciencia (_,0) = False
esPaciencia ("paciencia",_) = True
esPaciencia (_,_) = False

existe :: [Factor] -> Bool
existe = (/=0).length


-- EJERCICIO 3
ganaUnJugador :: Persona -> Juego -> Bool
ganaUnJugador unaPersona unJuego = all (==True) (listaCriteriosAplicados unaPersona unJuego)

listaCriteriosAplicados :: Persona -> Juego -> [Bool]
listaCriteriosAplicados unaPersona = (map (\criterio -> criterio unaPersona).criterios)


-- EJERCICIO 4
dineroConseguible :: Persona -> Float -> [Juego] -> Float
dineroConseguible unaPersona dineroApostado listaJuegos 
 | noGanaNingunJuego unaPersona listaJuegos = dineroApostado
 | otherwise = dineroConseguible unaPersona (nuevaApuesta dineroApostado unaPersona listaJuegos) listaJuegos

listaJuegosFiltrados :: Persona -> [Juego] -> [Juego]
listaJuegosFiltrados unaPersona listaJuegos = filter (ganaUnJugador unaPersona) listaJuegos

nuevaApuesta :: Float -> Persona -> [Juego] -> Float
nuevaApuesta dineroApostado unaPersona listaJuegos = sum (todosLosJuegosJugados dineroApostado (listaJuegosFiltrados unaPersona listaJuegos))

todosLosJuegosJugados :: Float -> [Juego] -> [Float]
todosLosJuegosJugados dineroApostado listaJuegosGanables = map (\funcion -> funcion dineroApostado) (listaFuncionesDineroSegunJuego listaJuegosGanables)

listaFuncionesDineroSegunJuego :: [Juego] -> [(Float->Float)]
listaFuncionesDineroSegunJuego = map cuantoDineroGanaria


-- EJERCICIO 5
noPuedenGanarNingunJuego ::  [Juego] -> [Persona] -> [String]
noPuedenGanarNingunJuego listaJuegos listaJugadores = map nombre (listaJugadoresNoGananNingunJuego listaJuegos listaJugadores)

listaJugadoresNoGananNingunJuego :: [Juego] -> [Persona] -> [Persona]
listaJugadoresNoGananNingunJuego listaJuegos = filter (\unaPersona -> noGanaNingunJuego unaPersona listaJuegos)

noGanaNingunJuego :: Persona -> [Juego] -> Bool
noGanaNingunJuego unaPersona listaJuegos = all (==False) (map (\unJuego -> ganaUnJugador unaPersona unJuego ) listaJuegos)


-- EJERCICIO 6
apostarCantidad :: Float -> Juego -> Persona -> Persona
apostarCantidad dineroApostado unJuego unaPersona = jugar unJuego dineroApostado (disminuirSaldo dineroApostado unaPersona)

disminuirSaldo :: Float -> Persona -> Persona
disminuirSaldo dineroApostado unaPersona = unaPersona { dinero = dinero unaPersona - dineroApostado}

jugar :: Juego -> Float -> Persona -> Persona
jugar unJuego dineroApostado unaPersona
 | ganaUnJugador unaPersona unJuego = unaPersona { dinero = (+) ((cuantoDineroGanaria unJuego) dineroApostado) (dinero unaPersona) }
 | otherwise = unaPersona