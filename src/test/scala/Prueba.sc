import Anagrama._

val palabra = "paraguas"
val palabraLista = palabra.toList
val agrupadas = palabraLista.groupBy(_.charValue())
agrupadas.values
val agrupalist = agrupadas.toList


val umbrella = lOcPal("paraguas")


val pal1 = "probemos"
val pal2 = "con"
val pal3 = "esta"
val pal4 = "frase"

val frase = List(pal1, pal2, pal3, pal4)
val palabrasListas = frase map((ls:Palabra)=>ls.toList)
val ocurrenciasFrase = palabrasListas.flatten

val probemosConEstaFrase = lOcFra(frase)

val diccionario = diccionarioPorOcurrencias


val pal5 = "probemos"
val pal6 = "con"
val pal7 = "con"
val pal8 = "frase"
val frase2 = List(List('a', 'a', 'a'), List('a', 'b', 'a'), List('a', 'a', 'a'), List('a', 'c', 'a'))

val cy = diccionarioPorOcurrencias
for {
  n <- cy
} yield n._2

combinaciones(lOcPal("cy"))
complemento(lOcPal("ccao"), lOcPal("yo"))


anagramasDePalabras("moco")