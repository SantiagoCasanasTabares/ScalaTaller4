object Anagrama {

  type Palabra = String
  type Frase = List[Palabra]
  type Ocurrencias = List[(Char, Int)]
  val diccionario : List[Palabra] = List( "cosas" , "como" , "yo" , "y" , "ocasos" , "cayo" , "mocosos" , "roca" , "moco" , "sos")


  /**
   * Recibe una palabra como string, lo convierte a lista de carcateres para luego aplicar empaquetar y así contar
   * cuantas veces se repite cada letra en una palabra
   * @param palabra
   * @return ocurrencias -> lista( lista(caracter, veces que se repite))
   */
  def lOcPal(palabra: Palabra):Ocurrencias = {
    val ocurrenciasPal = (for {
      n <- palabra
   }yield (n, palabra.count((x) => x==n))).toList

    ocurrenciasPal.distinct
  }
  /**
   * Recibe una frase, que es una lista de palabras, vuelve esas palabras listas de caracteres, luego las empaqueta y
   * cuenta cuantas veces se repite cada letra de la frase.
   * @param frase -> lista[palabras]
   * @return ocurrencias -> lista( lista(caracter, veces que se repite))
   */
  def lOcFra(frase: Frase):Ocurrencias = {
    lOcPal(frase reduceLeft((x,y)=>x+y))
  }

  /**
   * Evaluacion peresoza para crear un hashMap de listas de palabras ascociadas pr su mismo tipo de ocurrencia.
   */
  lazy val diccionarioPorOcurrencias:Map[Ocurrencias, List[Palabra]] = {
    diccionario.groupBy((pal:Palabra)=>lOcPal(pal).sorted)
  }


  /**
   * Funcin que recibe una palabra y busca en el diccionario las palabras que tienes sus mismas ocurrencias
   * @param palabra
   * @return
   */
  def anagramasDePalabras(palabra: Palabra): List[Palabra] = {

    def anagramAux(ocurrencias: Ocurrencias): List[Palabra] = {
    val ocurrenciaPal = (for {
      n <- diccionarioPorOcurrencias
      if (n._1==ocurrencias)
    } yield n._2)
      ocurrenciaPal.flatten
    }.toList

    anagramAux(lOcPal(palabra).sorted)

  }

  /**
   * Funcion que devuuelve todas las posibles combinaciones de una lista de ocurrencias
   * @param lOcurrencias
   * @return
   */
  def combinaciones(lOcurrencias: Ocurrencias):List[Ocurrencias]={
    val comb = (for {
      n <- lOcurrencias
      f <- lOcurrencias
      j <- 1 to n._2
      i <- 1 to f._2
      if n._1 !=  f._1
    }yield (List():+(n._1,j):+(f._1,i)).sorted).distinct

    val comb2 = (for{
      x<-lOcurrencias
      i<- 1 to x._2
    }yield List():+(x._1,i))

    List.concat(comb,comb2):+List()
  }

  def complemento(lOcurrencias: Ocurrencias, slOcurrencias: Ocurrencias):Ocurrencias = slOcurrencias match {
    case Nil => lOcurrencias
    case y::ys =>
      if (y._1 == lOcurrencias.head._1) {
        if (y._2 == lOcurrencias.head._2) complemento(lOcurrencias.tail, ys)
        else (y._1, lOcurrencias.head._2-y._2) :: complemento(lOcurrencias.tail, ys)
      } else lOcurrencias.head :: complemento(lOcurrencias.tail, ys)
  }


}
