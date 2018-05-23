type Factor = (String,Int)
type Accion = Persona -> Persona
type Condicion = Factor -> Bool

data Persona = Persona { nombre :: String, dinero :: Float, suerte :: Int, factores :: [Factor] } deriving(Show)

nico = Persona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)]
maiu = Persona "Maiu" 100.0 42 [("inteligencia",55), ("paciencia",50)]

-- EJERCICIO 1
suerteTotal :: Persona -> Int
suerteTotal unaPersona 
 | (verificarListaVaciaYValor.esAmuleto.factores) unaPersona = (*) (suerte unaPersona) ((valorDelAmuleto.esAmuleto.factores) unaPersona)
 | otherwise = suerte unaPersona

esAmuleto :: [Factor] -> [Factor]
esAmuleto = filter esUnAmuleto

esUnAmuleto :: Factor -> Bool
esUnAmuleto ("amuleto",_) = True
esUnAmuleto (_,_) = False

verificarListaVaciaYValor :: [Factor] -> Bool --lo hice de esta manera para reutilizar en otros puntos la funcion verificarValorAmuleto
verificarListaVaciaYValor listaAmuleto = verificarListaVacia listaAmuleto && verificarValorAmuleto listaAmuleto

verificarListaVacia :: [Factor] -> Bool
verificarListaVacia = (/=0).length

verificarValorAmuleto :: [Factor] -> Bool
verificarValorAmuleto = (/=0).valorDelAmuleto

valorDelAmuleto :: [Factor] -> Int
valorDelAmuleto = snd.head

-- EJERCICIO 2